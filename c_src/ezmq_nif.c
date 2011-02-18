#include "zmq.h"
#include "erl_nif.h"
#include <string.h>
#include <stdio.h>

static ErlNifResourceType* ezmq_nif_resource_context;
static ErlNifResourceType* ezmq_nif_resource_socket;

typedef struct _ezmq_context {
  void * context;
} ezmq_context;

typedef struct _ezmq_socket {
  void * socket;

  void * server;
  void * client;

  int run_receiver;
  ErlNifTid receiver_tid;
} ezmq_socket;

typedef struct _ezmq_recv {
  ErlNifEnv * env;
  ERL_NIF_TERM ref;
  int flags;
  ErlNifPid pid;
} ezmq_recv;

static void * zmq_internal_context;

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
  {"recv", 2, ezmq_nif_recv}
};

NIF(ezmq_nif_context)
{
  int _threads;

  if (!enif_get_int(env, argv[0], &_threads)) {
    return enif_make_badarg(env);
  }

  ezmq_context * handle = enif_alloc_resource(ezmq_nif_resource_context,
                                     sizeof(ezmq_context));

  handle->context = zmq_init(_threads);

  ERL_NIF_TERM result = enif_make_resource(env, handle);
  enif_release_resource(handle);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

void * receiver_thread(void * handle); // fwd
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

  ezmq_socket * handle = enif_alloc_resource(ezmq_nif_resource_socket,
                                             sizeof(ezmq_socket));


  handle->socket = zmq_socket(ctx->context, _type);

  char socket_id[64];
  sprintf(socket_id, "inproc://ezmq-internal-queue-%ld", (long int) handle->socket);

  handle->server = zmq_socket(zmq_internal_context, ZMQ_PUSH);
  zmq_bind(handle->server,socket_id);

  handle->client = zmq_socket(zmq_internal_context, ZMQ_PULL);
  zmq_connect(handle->client,socket_id);

  handle->run_receiver = 1;
  enif_thread_create("ezmq_receiver_thread", &handle->receiver_tid, receiver_thread, 
                     handle, NULL);

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
  void *_option_value;
  size_t _option_len = 8; // 64 bit

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_socket, (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &_option_name)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_uint64(env, argv[2], &_uint64)) {
    if (!enif_get_int64(env, argv[2], &_int64)) {
        if (!enif_inspect_iolist_as_binary(env, argv[2], &_bin)) {
          return enif_make_badarg(env);
        }  else {
          _option_value = _bin.data;
          _option_len = _bin.size;
        }
    } else {
         _option_value = &_int64;
    }
  } else {
    _option_value = &_uint64;
  }

  int error;

  if (!(error = zmq_setsockopt(socket->socket, _option_name, _option_value, _option_len))) {
    return enif_make_atom(env, "ok");
  } else {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
  }
}

NIF(ezmq_nif_getsockopt)
{
  ezmq_socket * socket;
  int _option_name;
  ErlNifBinary _bin;
  int64_t _option_value_64;
  int64_t _option_value_u64;
  char _option_value[255];
  size_t _option_len = 8; // 64 bit

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_socket, (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &_option_name)) {
    return enif_make_badarg(env);
  }
  int error;
  
  switch(_option_name) {
  case ZMQ_RCVMORE:
  case ZMQ_SWAP:
  case ZMQ_RATE:
  case ZMQ_RECOVERY_IVL:
  case ZMQ_MCAST_LOOP:
  case ZMQ_FD:
    // int64_t
    if (!(error = zmq_getsockopt(socket->socket, _option_name, &_option_value_64, &_option_len))) {
      return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_int64(env, _option_value_64));
    } else {
      return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
    }
    break;
  case ZMQ_HWM:
  case ZMQ_AFFINITY:
  case ZMQ_SNDBUF:
  case ZMQ_RCVBUF:
    // uint64_t
    if (!(error = zmq_getsockopt(socket->socket, _option_name, &_option_value_u64, &_option_len))) {
      return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_uint64(env, _option_value_u64));
    } else {
      return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
    }
    break;
  case ZMQ_SUBSCRIBE:
  case ZMQ_UNSUBSCRIBE:
  case ZMQ_IDENTITY:
    // binary
    if (!(error = zmq_getsockopt(socket->socket, _option_name, _option_value, &_option_len))) {
      enif_alloc_binary(_option_len, &_bin);
      memcpy(_bin.data, _option_value, _option_len);
      return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &_bin));
    }  else {
      return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
    }
    break;
  }
  
  return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "unknown"));
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
  
  if ((error = zmq_msg_close(&msg))) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
  }

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
  
  if ((error = zmq_msg_close(&msg))) {
    enif_release_binary(&bin);
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
  }

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &bin));
}

NIF(ezmq_nif_recv)
{

  ezmq_recv recv;
  ezmq_socket * socket;

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_socket, (void **) &socket)) {
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
    recv.env = enif_alloc_env();
    recv.ref = enif_make_ref(recv.env);
    
    if ((error = zmq_msg_init_size(&msg, sizeof(ezmq_recv)))) {
      return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
    }
    
    memcpy(zmq_msg_data(&msg), &recv, sizeof(ezmq_recv));
    
    if ((error = zmq_send(socket->server, &msg, ZMQ_NOBLOCK))) {
      return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
    }
    
    if ((error = zmq_msg_close(&msg))) {
      return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
    }
    
    return enif_make_copy(env, recv.ref);
  } else if (error == 0) { // return result immediately
    ErlNifBinary bin;
    enif_alloc_binary(zmq_msg_size(&msg), &bin);
    memcpy(bin.data, zmq_msg_data(&msg), zmq_msg_size(&msg));
    
    if ((error = zmq_msg_close(&msg))) {
      enif_release_binary(&bin);
      return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
    }

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_binary(env, &bin));
  } else {
      return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, error));
  }

}

//

void rcvmsg(ErlNifBinary * bin, zmq_msg_t * rmsg, ezmq_socket * socket, int flags) {
  zmq_msg_init(rmsg);
  zmq_recv(socket->socket, rmsg, flags);
  
  enif_alloc_binary(zmq_msg_size(rmsg), bin);
  memcpy(bin->data, zmq_msg_data(rmsg), zmq_msg_size(rmsg));
  
  zmq_msg_close(rmsg);
}


void * receiver_thread(void * handle) {
  ezmq_socket * socket = (ezmq_socket *) handle;
  zmq_msg_t msg;
  zmq_msg_t rmsg;
  ezmq_recv * recv;
  ErlNifEnv * env = enif_alloc_env();

  while (socket->run_receiver) {
    zmq_msg_init(&msg);
    if (!zmq_recv(socket->client, &msg, 0)) {
      recv = (ezmq_recv *) zmq_msg_data(&msg);
      
      ErlNifBinary bin;
      
      rcvmsg(&bin, &rmsg, socket, recv->flags);
      
      enif_send(NULL, &recv->pid, env, enif_make_tuple2(env, enif_make_copy(env, recv->ref),
                                                          enif_make_binary(env, &bin)));
      enif_free(recv->env);
    }
    zmq_msg_close(&msg);
  }
  enif_free(env);
  return NULL;
}

static void ezmq_nif_resource_context_cleanup(ErlNifEnv* env, void* arg)
{

  zmq_term(((ezmq_context *)arg)->context);
}

static void ezmq_nif_resource_socket_cleanup(ErlNifEnv* env, void* arg)
{
  ezmq_socket * socket = (ezmq_socket *)arg;

  socket->run_receiver = 0;
  enif_thread_join(socket->receiver_tid, NULL);

  zmq_close(socket->server);
  zmq_close(socket->client);

  zmq_close(socket->socket);
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ezmq_nif_resource_context = enif_open_resource_type(env, "ezmq_nif",
                                                      "ezmq_nif_resource_context",
                                                      &ezmq_nif_resource_context_cleanup,
                                                      ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                                      0);
  ezmq_nif_resource_socket = enif_open_resource_type(env, "ezmq_nif",
                                                      "ezmq_nif_resource_socket",
                                                      &ezmq_nif_resource_socket_cleanup,
                                                      ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                                      0);

  zmq_internal_context = zmq_init(1);
  return 0;
}

static void on_unload(ErlNifEnv* env, void* priv_data) {
  zmq_term(zmq_internal_context);
}


ERL_NIF_INIT(ezmq_nif, nif_funcs, &on_load, NULL, NULL, &on_unload);
