#include "zmq.h"
#include "erl_nif.h"
#include <string.h>
#include <sys/queue.h>
#include <stdio.h>

static ErlNifResourceType* ezmq_nif_resource_context;
static ErlNifResourceType* ezmq_nif_resource_socket;

typedef struct _ezmq_context {
  void * context;
  void * ipc_socket;
  char * ipc_socket_name;
  int running;
  ErlNifCond * cond;
  ErlNifMutex * mutex;
  ErlNifTid polling_tid;
} ezmq_context;

typedef struct _ezmq_socket {
  void * socket;
  ezmq_context * context;
} ezmq_socket;

typedef struct _ezmq_recv {
  ErlNifEnv * env;
  ERL_NIF_TERM ref;
  int flags;
  ErlNifPid pid;
  void * socket;
  TAILQ_ENTRY(_ezmq_recv) recvs;
} ezmq_recv;
TAILQ_HEAD(recvs_head, _ezmq_recv);

// Prototypes
#define NIF(name) ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

NIF(ezmq_nif_context);
NIF(ezmq_nif_socket);
NIF(ezmq_nif_bind);
NIF(ezmq_nif_connect);
NIF(ezmq_nif_setsockopt);
NIF(ezmq_nif_getsockopt);
NIF(ezmq_nif_send);
NIF(ezmq_nif_brecv);
NIF(ezmq_nif_recv);
NIF(ezmq_nif_close);
NIF(ezmq_nif_term);

static ErlNifFunc nif_funcs[] =
{
  {"context", 1, ezmq_nif_context},
  {"socket", 2, ezmq_nif_socket},
  {"bind", 2, ezmq_nif_bind},
  {"connect", 2, ezmq_nif_connect},
  {"setsockopt", 3, ezmq_nif_setsockopt},
  {"getsockopt", 2, ezmq_nif_getsockopt},
  {"send", 3, ezmq_nif_send},
  {"brecv", 2, ezmq_nif_brecv},
  {"recv", 2, ezmq_nif_recv},
  {"close", 1, ezmq_nif_close},
  {"term", 1, ezmq_nif_term}
};

void * polling_thread(void * handle);
NIF(ezmq_nif_context)
{
  int _threads;

  if (!enif_get_int(env, argv[0], &_threads)) {
    return enif_make_badarg(env);
  }

  ezmq_context * handle = enif_alloc_resource(ezmq_nif_resource_context,
                                     sizeof(ezmq_context));

  handle->context = zmq_init(_threads);

  char socket_id[64];
  sprintf(socket_id, "inproc://ezmq-%ld", (long int) handle);
  handle->ipc_socket_name = strdup(socket_id);

  handle->ipc_socket = zmq_socket(handle->context, ZMQ_PUSH);
  zmq_bind(handle->ipc_socket,socket_id);

  handle->running = 0;
  handle->mutex = enif_mutex_create("ezmq_context_mutex");
  handle->cond = enif_cond_create("ezmq_context_cond");

  enif_mutex_lock(handle->mutex);
  int err;
  if ((err = enif_thread_create("ezmq_polling_thread", &handle->polling_tid,
                                polling_thread, handle, NULL))) {
    enif_mutex_unlock(handle->mutex);
    enif_mutex_destroy(handle->mutex);
    enif_cond_destroy(handle->cond);
    zmq_close(handle->ipc_socket);
    free(handle->ipc_socket_name);
    zmq_term(handle->context);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                 enif_make_int(env, err));
  }
  while (handle->running == 0) {
    enif_cond_wait(handle->cond, handle->mutex);
  }
  enif_mutex_unlock(handle->mutex);

  ERL_NIF_TERM result = enif_make_resource(env, handle);
  enif_release_resource(handle);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

NIF(ezmq_nif_socket)
{
  ezmq_context * ctx;
  int _type;

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_context, (void **) &ctx)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &_type)) {
    return enif_make_badarg(env);
  }
  
  if (!ctx->running) {
    return enif_make_badarg(env);
  }

  ezmq_socket * handle = enif_alloc_resource(ezmq_nif_resource_socket,
                                             sizeof(ezmq_socket));

  handle->context = ctx;
  handle->socket = zmq_socket(ctx->context, _type);

  ERL_NIF_TERM result = enif_make_resource(env, handle);
  enif_release_resource(handle);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

NIF(ezmq_nif_bind)
{
  ezmq_socket * socket;
  unsigned _endpoint_length;
  char * _endpoint;

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_socket, (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_list_length(env, argv[1], &_endpoint_length)) {
    return enif_make_badarg(env);
  }

  _endpoint = (char *) malloc(_endpoint_length + 1);

  if (!enif_get_string(env, argv[1], _endpoint, _endpoint_length + 1, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  int error;

  if (!(error = zmq_bind(socket->socket, _endpoint))) {
    free(_endpoint);
    return enif_make_atom(env, "ok");
  } else {
    free(_endpoint);
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
  }
}

NIF(ezmq_nif_connect)
{
  ezmq_socket * socket;
  unsigned _endpoint_length;
  char * _endpoint;

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_socket, (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_list_length(env, argv[1], &_endpoint_length)) {
    return enif_make_badarg(env);
  }

  _endpoint = (char *) malloc(_endpoint_length + 1);

  if (!enif_get_string(env, argv[1], _endpoint, _endpoint_length + 1, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  int error;

  if (!(error = zmq_connect(socket->socket, _endpoint))) {
    free(_endpoint);
    return enif_make_atom(env, "ok");
  } else {
    free(_endpoint);
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
  }
}

NIF(ezmq_nif_setsockopt)
{
  ezmq_socket * socket;
  int _option_name;
  ErlNifUInt64 _uint64;
  ErlNifSInt64 _int64;
  ErlNifBinary _bin;
  int _int;
  void *_option_value;
  size_t _option_len = 8; // 64 bit

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_socket,
                                       (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &_option_name)) {
    return enif_make_badarg(env);
  }

  switch (_option_name) {
    case ZMQ_HWM: // uint64_t
    case ZMQ_AFFINITY:
    case ZMQ_SNDBUF:
    case ZMQ_RCVBUF:
        if (!enif_get_uint64(env, argv[2], &_uint64)) {
            goto badarg;
        }
        _option_value = &_uint64;
        break;
    case ZMQ_SWAP: // int64_t
    case ZMQ_RATE:
    case ZMQ_RECOVERY_IVL:
    case ZMQ_MCAST_LOOP:
        if (!enif_get_int64(env, argv[2], &_int64)) {
            goto badarg;
        }
        _option_value = &_int64;
        break;
    case ZMQ_IDENTITY: // binary
    case ZMQ_SUBSCRIBE:
    case ZMQ_UNSUBSCRIBE:
        if (!enif_inspect_iolist_as_binary(env, argv[2], &_bin)) {
            goto badarg;
        }
        _option_value = _bin.data;
        _option_len = _bin.size;
        break;
    case ZMQ_LINGER: // int
    case ZMQ_RECONNECT_IVL:
    case ZMQ_BACKLOG:
        if (!enif_get_int(env, argv[1], &_int)) {
            goto badarg;
        }
        _option_value = &_int;
        _option_len = sizeof(int);
        break;
    default:
        goto badarg;
  }

  if (zmq_setsockopt(socket->socket, _option_name,
                     _option_value, _option_len)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                 enif_make_int(env, zmq_errno()));
  }
  return enif_make_atom(env, "ok");

badarg:
  return enif_make_badarg(env);
}

NIF(ezmq_nif_getsockopt)
{
  ezmq_socket * socket;
  int _option_name;
  ErlNifBinary _bin;
  int64_t _option_value_64;
  int64_t _option_value_u64;
  char _option_value[255];
  int _option_value_int;
  size_t _option_len = 8; // 64 bit

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_socket,
                                       (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &_option_name)) {
    return enif_make_badarg(env);
  }

  switch(_option_name) {
    case ZMQ_RCVMORE: // int64_t
    case ZMQ_SWAP:
    case ZMQ_RATE:
    case ZMQ_RECOVERY_IVL:
    case ZMQ_RECOVERY_IVL_MSEC:
    case ZMQ_MCAST_LOOP:
      if (zmq_getsockopt(socket->socket, _option_name,
                         &_option_value_64, &_option_len)) {
        goto error;
      }
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                   enif_make_int64(env, _option_value_64));
    case ZMQ_HWM: // uint64_t
    case ZMQ_AFFINITY:
    case ZMQ_SNDBUF:
    case ZMQ_RCVBUF:
      if (zmq_getsockopt(socket->socket, _option_name,
                         &_option_value_u64, &_option_len)) {
        goto error;
      }
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                   enif_make_uint64(env, _option_value_u64));
    case ZMQ_IDENTITY: // binary
      if (zmq_getsockopt(socket->socket, _option_name,
                         _option_value, &_option_len)) {
        goto error;
      }
      enif_alloc_binary(_option_len, &_bin);
      memcpy(_bin.data, _option_value, _option_len);
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                   enif_make_binary(env, &_bin));
    case ZMQ_TYPE: // int
    case ZMQ_LINGER:
    case ZMQ_RECONNECT_IVL:
    case ZMQ_RECONNECT_IVL_MAX:
    case ZMQ_BACKLOG:
    case ZMQ_FD: // FIXME: ZMQ_FD returns SOCKET on Windows
      if (zmq_getsockopt(socket->socket, _option_name,
                         &_option_value_int, &_option_len)) {
        goto error;
      }
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                   enif_make_int(env, _option_value_int));
    default:
      return enif_make_badarg(env);
  }
error:
  return enif_make_tuple2(env, enif_make_atom(env, "error"),
                               enif_make_int(env, zmq_errno()));
}

NIF(ezmq_nif_send)
{
  ezmq_socket * socket;
  int _flags;
  ErlNifBinary _bin;

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_socket, (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_inspect_iolist_as_binary(env, argv[1], &_bin)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[2], &_flags)) {
    return enif_make_badarg(env);
  }

  int error;
  zmq_msg_t msg;

  if ((error = zmq_msg_init_size(&msg, _bin.size))) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
  }

  memcpy(zmq_msg_data(&msg), _bin.data, _bin.size);

  if ((error = zmq_send(socket->socket, &msg, _flags))) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
  }

  zmq_msg_close(&msg);

  return enif_make_atom(env, "ok");

}

int brecv(zmq_msg_t * msg, ezmq_socket * socket, int flags) {
  int error;
  if ((error = zmq_msg_init(msg))) {
    return zmq_errno();
  }

  if ((error = zmq_recv(socket->socket, msg, flags))) {
    return zmq_errno();
  }

  return 0;
}

NIF(ezmq_nif_brecv)
{
  ezmq_socket * socket;
  int _flags;

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_socket, (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &_flags)) {
    return enif_make_badarg(env);
  }

  int error;
  zmq_msg_t msg;

  if ((error = brecv(&msg, socket, _flags))) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, error));
  }

  ErlNifBinary bin;
  enif_alloc_binary(zmq_msg_size(&msg), &bin);
  memcpy(bin.data, zmq_msg_data(&msg), zmq_msg_size(&msg));

  zmq_msg_close(&msg);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &bin));
}

NIF(ezmq_nif_recv)
{

  ezmq_recv recv;
  ezmq_socket * socket;

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_socket,
                                       (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &recv.flags)) {
    return enif_make_badarg(env);
  }

  enif_self(env, &recv.pid);

  int error;
  zmq_msg_t msg;

  // try brecv with noblock

  error = brecv(&msg, socket, ZMQ_NOBLOCK);

  if (error == EAGAIN) { // if nothing is there, hand it off to the receiver thread
    if (recv.flags & ZMQ_NOBLOCK) {
      goto out;
    }
    recv.env = enif_alloc_env();
    recv.ref = enif_make_ref(recv.env);
    recv.socket = socket->socket;

    if ((error = zmq_msg_init_size(&msg, sizeof(ezmq_recv)))) {
        goto q_err;
    }

    memcpy(zmq_msg_data(&msg), &recv, sizeof(ezmq_recv));

    if ((error = zmq_send(socket->context->ipc_socket, &msg, 0))) {
        goto q_err;
    }

    zmq_msg_close(&msg);

    return enif_make_copy(env, recv.ref);
q_err:
    enif_free_env(recv.env);
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                 enif_make_int(env, zmq_errno()));
  } else if (error == 0) { // return result immediately
    ErlNifBinary bin;
    enif_alloc_binary(zmq_msg_size(&msg), &bin);
    memcpy(bin.data, zmq_msg_data(&msg), zmq_msg_size(&msg));

    zmq_msg_close(&msg);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                 enif_make_binary(env, &bin));
  }
out:
  return enif_make_tuple2(env, enif_make_atom(env, "error"),
                               enif_make_int(env, error));
}

void * polling_thread(void * handle)
{
  ezmq_context * ctx = (ezmq_context *) handle;

  struct recvs_head * recvs_queue;
  recvs_queue = malloc(sizeof(struct recvs_head));
  TAILQ_INIT(recvs_queue);

  void *ipc_socket = zmq_socket(ctx->context, ZMQ_PULL);
  zmq_connect(ipc_socket,ctx->ipc_socket_name);

  int nreaders = 1;
  enif_mutex_lock(ctx->mutex);
  ctx->running = 1;
  enif_cond_signal(ctx->cond);
  enif_mutex_unlock(ctx->mutex);

  while (ctx->running) {
    int i;
    zmq_msg_t msg;
    ezmq_recv *r, *rtmp;

    zmq_pollitem_t *items = calloc(nreaders, sizeof(zmq_pollitem_t));
    items[0].socket = ipc_socket;
    items[0].events = ZMQ_POLLIN;

    for (i = 1, r = recvs_queue->tqh_first;
         r != NULL; r = r->recvs.tqe_next, i++) {
      items[i].socket = r->socket;
      items[i].events = ZMQ_POLLIN;
    }

    zmq_poll(items, nreaders, -1);

    for (i = 1, r = recvs_queue->tqh_first;
         r && ((rtmp = r->recvs.tqe_next), 1); r = rtmp, i++) {
      if (items[i].revents & ZMQ_POLLIN) {
        nreaders--;
        ErlNifBinary bin;

        zmq_msg_init(&msg);
        zmq_recv(r->socket, &msg, r->flags);

        enif_alloc_binary(zmq_msg_size(&msg), &bin);
        memcpy(bin.data, zmq_msg_data(&msg), zmq_msg_size(&msg));

        zmq_msg_close(&msg);

        enif_send(NULL, &r->pid, r->env,
                  enif_make_tuple2(r->env,
                                   enif_make_copy(r->env, r->ref),
                                   enif_make_binary(r->env, &bin)));
        enif_free_env(r->env);
        TAILQ_REMOVE(recvs_queue, r, recvs);
        free(r);
      }
    }
    if (items[0].revents & ZMQ_POLLIN) {
      zmq_msg_init(&msg);
      if (!zmq_recv(items[0].socket, &msg, 0)) {
        ezmq_recv * recv = (ezmq_recv *) zmq_msg_data(&msg);
        if (recv->env == NULL) {
          ctx->running = 0;
          goto out;
        }
        nreaders++;
        ezmq_recv * r = malloc(sizeof(ezmq_recv));
        memcpy(r, recv, sizeof(ezmq_recv));
        TAILQ_INSERT_TAIL(recvs_queue, r, recvs);
      }
out:
      zmq_msg_close(&msg);
    }
    free(items);
  }
  // cleanup reader's queue
  ezmq_recv * r;
  while ((r = recvs_queue->tqh_first) != NULL) {
      TAILQ_REMOVE(recvs_queue, recvs_queue->tqh_first, recvs);
      free(r);
  }
  free(recvs_queue);
  zmq_close(ipc_socket);
  return NULL;
}

NIF(ezmq_nif_close)
{

  ezmq_socket * socket;

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_socket, (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (-1 == zmq_close(socket->socket)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
  } else {
    return enif_make_atom(env, "ok");
  }
}

NIF(ezmq_nif_term) 
{
  ezmq_context * ctx;

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_context, (void **) &ctx)) {
    return enif_make_badarg(env);
  }

  zmq_msg_t msg;
  ezmq_recv recv;
  recv.env = NULL;
  zmq_msg_init_size(&msg, sizeof(ezmq_recv));
  memcpy(zmq_msg_data(&msg), &recv, sizeof(ezmq_recv));
  zmq_send(ctx->ipc_socket, &msg, ZMQ_NOBLOCK);
  zmq_msg_close(&msg);
  enif_thread_join(ctx->polling_tid, NULL);
  zmq_close(ctx->ipc_socket);
  free(ctx->ipc_socket_name);
  enif_mutex_destroy(ctx->mutex);
  enif_cond_destroy(ctx->cond);

  if (-1 == zmq_term(ctx->context)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
  } else {
    return enif_make_atom(env, "ok");
  }
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ezmq_nif_resource_context =
    enif_open_resource_type(env, "ezmq_nif",
                            "ezmq_nif_resource_context",
                            NULL,
                            ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                            0);
  ezmq_nif_resource_socket =
    enif_open_resource_type(env, "ezmq_nif",
                            "ezmq_nif_resource_socket",
                            NULL,
                            ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                            0);
  return 0;
}

static void on_unload(ErlNifEnv* env, void* priv_data) {
}

ERL_NIF_INIT(ezmq_nif, nif_funcs, &on_load, NULL, NULL, &on_unload);

