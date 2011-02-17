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
} ezmq_socket;

// Prototypes
#define NIF(name) ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

NIF(ezmq_nif_context);
NIF(ezmq_nif_socket);
NIF(ezmq_nif_bind);
NIF(ezmq_nif_connect);
NIF(ezmq_nif_send);
NIF(ezmq_nif_brecv);


static ErlNifFunc nif_funcs[] =
{
  {"context", 1, ezmq_nif_context},
  {"socket", 2, ezmq_nif_socket},
  {"bind", 2, ezmq_nif_bind},
  {"connect", 2, ezmq_nif_connect},
  {"send", 3, ezmq_nif_send},
  {"brecv", 2, ezmq_nif_brecv}
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

NIF(ezmq_nif_send)
{
  ezmq_socket * socket;
  int _flags;
  ErlNifBinary _bin;

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_socket, (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_inspect_binary(env, argv[1], &_bin)) {
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

NIF(ezmq_nif_brecv)
{
  ezmq_socket * socket;
  int _flags;

  if (!enif_get_resource(env, argv[0], ezmq_nif_resource_socket, (void **) &socket)) {
    printf("0");fflush(0);
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &_flags)) {
    printf("1");fflush(0);

    return enif_make_badarg(env);
  }

  int error;
  zmq_msg_t msg;

  if ((error = zmq_msg_init(&msg))) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
  }

  if ((error = zmq_recv(socket->socket, &msg, _flags))) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
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

static void ezmq_nif_resource_context_cleanup(ErlNifEnv* env, void* arg)
{
  zmq_term(((ezmq_context *)arg)->context);
}

static void ezmq_nif_resource_socket_cleanup(ErlNifEnv* env, void* arg)
{
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

  return 0;
}

ERL_NIF_INIT(ezmq_nif, nif_funcs, &on_load, NULL, NULL, NULL);
