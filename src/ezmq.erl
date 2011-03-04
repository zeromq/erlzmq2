-module(ezmq).
%% @headerfile "ezmq.hrl"
-include_lib("ezmq.hrl").
-export([context/0, context/1, socket/2, bind/2, connect/2, send/2, send/3,
         brecv/1, brecv/2, recv/1, recv/2, setsockopt/3, getsockopt/2,
         close/1, term/1, term/2]).
-export_type([ezmq_socket/0, ezmq_context/0]).

%% @equiv context(1)
%% @spec context() -> {ok, ezmq_context()} | ezmq_error()
-spec context() -> {ok, ezmq_context()} | ezmq_error().
context() ->
    context(1).

%% @doc Create a new ezmq context with the specified number of io threads.
%% <br />
%% If the context can be created an 'ok' tuple containing an
%% {@type ezmq_context()} handle to the created context is returned;
%% if not, it returns an 'error' tuple with an {@type ezmq_type_error()}
%% describing the error.
%% <br />
%% The context must be later cleaned up calling {@link ezmq:term/1. term/1}
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq-init">zmq_init</a></i>
%% @end
%% @spec context(pos_integer()) -> {ok, ezmq_context()} | ezmq_error()
-spec context(Threads :: pos_integer()) -> {ok, ezmq_context()} | ezmq_error().

context(Threads) when is_integer(Threads) ->
    ezmq_nif:context(Threads).


%% @doc Create a socket.
%% <br />
%% This functions creates a socket of the given
%% {@link ezmq_socket_type(). type} and associates it with the given
%% {@link ezmq_context(). context}.
%% <br />
%% If the socket can be created an 'ok' tuple containing a
%% {@type ezmq_socket()} handle to the created socket is returned;
%% if not, it returns an {@type ezmq_error()} describing the error.<br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_socket">zmq_socket</a>.</i>
%% @end
%% @spec socket(ezmq_context(), ezmq_socket_type()) -> {ok, ezmq_socket()} | ezmq_error()
-spec socket(Context :: ezmq_context(), Type :: ezmq_socket_type()) -> {ok, ezmq_socket()} | ezmq_error().

socket(Context, Type) ->
    ezmq_nif:socket(Context, socket_type(Type)).

%% @doc Accept connections on a socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_bind">zmq_bind</a>.</i>
%% @end
%% @spec bind(ezmq_socket(), ezmq_endpoint()) -> ok | ezmq_error()
-spec bind(Socket :: ezmq_socket(), Endpoint :: ezmq_endpoint()) -> ok | ezmq_error().

bind(Socket, Endpoint) ->
    ezmq_result(ezmq_nif:bind(Socket, Endpoint)).

%% @doc Connect a socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_connect">zmq_connect</a>.</i>
%% @end
%% @spec connect(ezmq_socket(), ezmq_endpoint()) -> ok | ezmq_error()
-spec connect(Socket :: ezmq_socket(), Endpoint :: ezmq_endpoint()) -> ok | ezmq_error().

connect(Socket, Endpoint) ->
    ezmq_result(ezmq_nif:connect(Socket, Endpoint)).

%% @equiv send(Socket, Msg, [])
%% @spec send(ezmq_socket(), ezmq_data()) -> ok | ezmq_error()
-spec send(Socket :: ezmq_socket(), Data :: ezmq_data()) -> ok | ezmq_error().

send(Socket, Binary) ->
    send(Socket, Binary, []).

%% @doc Send a message on a socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_send">zmq_send</a>.</i>
%% @end
%% @spec send(ezma_socket(), ezmq_data(), ezmq_send_recv_flags()) -> ok | ezmq_error()
-spec send(Socket :: ezmq_socket(), Data :: ezmq_data(), Flags :: ezmq_send_recv_flags()) -> ok | ezmq_error().

send(Socket, Binary, Flags) when is_list(Flags) ->
    ezmq_result(ezmq_nif:send(Socket, Binary, sendrecv_flags(Flags))).

%% @equiv brecv(Socket, 0)
%% @spec brecv(ezmq_socket()) -> {ok, ezmq_data()} | ezmq_error()
-spec brecv(Socket :: ezmq_socket()) -> {ok, ezmq_data()} | ezmq_error().

brecv(Socket) ->
    ezmq_result(brecv(Socket, [])).

%% @doc Receive a message from a socket in a blocking way.
%% This function can block the current VM scheduler. <b>DO NOT USE IT UNLESS YOU REALLY KNOW WHAT YOU ARE DOING</b>.
%% @end
%% @spec brecv(ezmq_socket(), ezmq_send_recv_flags()) -> {ok, ezmq_data()} | ezmq_error()
-spec brecv(Socket :: ezmq_socket(), Flags :: ezmq_send_recv_flags()) -> {ok, ezmq_data()} | ezmq_error().

brecv(Socket, Flags) when is_list(Flags) ->
   ezmq_result( ezmq_nif:brecv(Socket, sendrecv_flags(Flags))).


%% @equiv recv(Socket, 0)
%% @spec recv(ezmq_socket()) -> {ok, ezmq_data()} | ezmq_error()
-spec recv(Socket :: ezmq_socket()) -> {ok, ezmq_data()} | ezmq_error().

recv(Socket) ->
    recv(Socket, []).

%% @doc Receive a message from a socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_recv">zmq_recv</a>.</i>
%% @end
%% @spec recv(ezmq_socket(), ezmq_send_recv_flags()) -> {ok, ezmq_data()} | ezmq_error()
-spec recv(Socket :: ezmq_socket(), Flags :: ezmq_send_recv_flags()) -> {ok, ezmq_data()} | ezmq_error() | {error, timeout, reference()}.

recv(Socket, Flags) when is_list(Flags) ->
    case ezmq_nif:recv(Socket, sendrecv_flags(Flags)) of
        Ref when is_reference(Ref) ->
            Timeout = proplists:get_value(timeout, Flags, infinity),
            receive
                {Ref, Result} ->
                    {ok, Result}
            after Timeout ->
                    {error, timeout, Ref}
            end;
        Result ->
            ezmq_result(Result)
    end.

%% @doc Set an {@link ezmq_sockopt(). option} associated with a socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_setsockopt">zmq_setsockopt</a>.</i>
%% @end
%% @spec setsockopt(ezmq_socket(), ezmq_sockopt(), ezmq_sockopt_value()) -> ok | ezmq_error()
-spec setsockopt(Socket :: ezmq_socket(), Name :: ezmq_sockopt(), ezmq_sockopt_value()) -> ok | ezmq_error().

setsockopt(Socket, Name, Value) ->
    ezmq_result(ezmq_nif:setsockopt(Socket, option_name(Name), Value)).

%% @doc Get an {@link ezmq_sockopt(). option} associated with a socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_getsockopt">zmq_getsockopt</a>.</i>
%% @end
%% @spec getsockopt(ezmq_socket(), ezmq_sockopt()) -> {ok, ezmq_sockopt_value()} | ezmq_error()
-spec getsockopt(Socket :: ezmq_socket(), Name :: ezmq_sockopt()) -> {ok, ezmq_sockopt_value()} | ezmq_error().

getsockopt(Socket, Name) ->
    ezmq_result(ezmq_nif:getsockopt(Socket, option_name(Name))).


%% @doc Close the given socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_close">zmq_close</a>.</i>
%% @end
%% @spec close(ezmq_socket()) -> ok | ezmq_error()
-spec close(Socket :: ezmq_socket()) -> ok | ezmq_error().

close(Socket) ->
    ezmq_result(ezmq_nif:close(Socket)).

%% @equiv term(Context, infinity)
%% @spec term(ezmq_context()) -> ok | ezmq_error()
-spec term(Context :: ezmq_context()) -> ok | ezmq_error().

term(Context) ->
    term(Context, infinity).


%% @doc Terminate the given context waiting up to Timeout ms.
%% <br />
%% This function should be called after all sockets associated with
%% the given context have been closed.<br />
%% If not it will block the given Timeout amount of time.
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_term">zmq_term</a>.</i>
%% @end
%% @spec term(ezmq_context(), timeout()) -> ok | ezmq_error()
-spec term(Context :: ezmq_context(), Timeout :: timeout()) -> ok | ezmq_error() | {error, timeout, reference()}.

term(Context, Timeout) ->
    case ezmq_nif:term(Context) of
        Ref when is_reference(Ref) ->
            receive
                {Ref, Result} ->
                    Result
            after Timeout ->
                    {error, timeout, Ref}
            end;
        Result ->
            ezmq_result(Result)
    end.


%% Private

-spec socket_type(Type :: ezmq_socket_type()) -> integer().

socket_type(pair) ->
    ?'ZMQ_PAIR';
socket_type(pub) ->
    ?'ZMQ_PUB';
socket_type(sub) ->
    ?'ZMQ_SUB';
socket_type(req) ->
    ?'ZMQ_REQ';
socket_type(rep) ->
    ?'ZMQ_REP';
socket_type(xreq) ->
    ?'ZMQ_XREQ';
socket_type(xrep) ->
    ?'ZMQ_XREP';
socket_type(pull) ->
    ?'ZMQ_PULL';
socket_type(push) ->
    ?'ZMQ_PUSH';
socket_type(xpub) ->
    ?'ZMQ_XPUB';
socket_type(xsub) ->
    ?'ZMQ_XSUB'.

-spec sendrecv_flags(Flags :: ezmq_send_recv_flags()) -> integer().

sendrecv_flags([]) ->
    0;
sendrecv_flags([{timeout,_}]) ->
    0;
sendrecv_flags([noblock|Rest]) ->
    ?'ZMQ_NOBLOCK' bor sendrecv_flags(Rest);
sendrecv_flags([sndmore|Rest]) ->
    ?'ZMQ_SNDMORE' bor sendrecv_flags(Rest).

-spec option_name(Name :: ezmq_sockopt()) -> integer().

option_name(hwm) ->
    ?'ZMQ_HWM';
option_name(swap) ->
    ?'ZMQ_SWAP';
option_name(affinity) ->
    ?'ZMQ_AFFINITY';
option_name(identity) ->
    ?'ZMQ_IDENTITY';
option_name(subscribe) ->
    ?'ZMQ_SUBSCRIBE';
option_name(unsubscribe) ->
    ?'ZMQ_UNSUBSCRIBE';
option_name(rate) ->
    ?'ZMQ_RATE';
option_name(recovery_ivl) ->
    ?'ZMQ_RECOVERY_IVL';
option_name(mcast_loop) ->
    ?'ZMQ_MCAST_LOOP';
option_name(sndbuf) ->
    ?'ZMQ_SNDBUF';
option_name(rcvbuf) ->
    ?'ZMQ_RCVBUF';
option_name(rcvmore) ->
    ?'ZMQ_RCVMORE';
option_name(fd) ->
    ?'ZMQ_FD';
option_name(events) ->
    ?'ZMQ_EVENTS';
option_name(linger) ->
    ?'ZMQ_LINGER';
option_name(reconnect_ivl) ->
    ?'ZMQ_RECONNECT_IVL';
option_name(backlog) ->
    ?'ZMQ_BACKLOG';
option_name(recovery_ivl_msec) ->
    ?'ZMQ_RECOVERY_IVL_MSEC';
option_name(reconnect_ivl_max) ->
    ?'ZMQ_RECONNECT_IVL_MAX'.


-spec ezmq_result(ok) -> ok;
                 ({ok, Value :: term()}) -> Value :: term();
                 ({error, Value :: atom()}) -> Value :: atom();
                 ({error, integer()}) -> {error, ezmq_error_type()};
                 ({error, ezmq, integer()}) -> {error, ezmq_error_type()}.

ezmq_result(ok) ->
    ok;
ezmq_result({ok, _} = Result) ->
    Result;
ezmq_result({error, Code} = Error) when is_atom(Code) ->
    Error;
ezmq_result({error, Code}) when is_integer(Code) andalso Code > 156384712 ->
    ezmq_result({error, ezmq, Code - 156384712});
ezmq_result({error, ezmq, 1}) ->
    {error, enotsup};
ezmq_result({error, ezmq, 2}) ->
    {error, eprotonosupport};
ezmq_result({error, ezmq, 3}) ->
    {error, enobufs};
ezmq_result({error, ezmq, 4}) ->
    {error, enetdown};
ezmq_result({error, ezmq, 5}) ->
    {error, eaddrinuse};
ezmq_result({error, ezmq, 6}) ->
    {error, eaddrnotavail};
ezmq_result({error, ezmq, 7}) ->
    {error, econnrefused};
ezmq_result({error, ezmq, 8}) ->
    {error, einprogress};
ezmq_result({error, ezmq, 51}) ->
    {error, efsm};
ezmq_result({error, ezmq, 52}) ->
    {error, enocompatproto};
ezmq_result({error, ezmq, 53}) ->
    {error, eterm};
ezmq_result({error, ezmq, 54}) ->
    {error, emthread};

%% errno
ezmq_result({error, 1}) ->
    {error, eperm};
ezmq_result({error, 2}) ->
    {error, enoent};
ezmq_result({error, 3}) ->
    {error, esrch};
ezmq_result({error, 4}) ->
    {error, eintr};
ezmq_result({error, 5}) ->
    {error, eio};
ezmq_result({error, 7}) ->
    {error, enxio};
ezmq_result({error, 8}) ->
    {error, eperm};
ezmq_result({error, 9}) ->
    {error, ebadf};
ezmq_result({error, 10}) ->
    {error, echild};
ezmq_result({error, 11}) ->
    {error, edeadlk};
ezmq_result({error, 12}) ->
    {error, enomem};
ezmq_result({error, 13}) ->
    {error, eacces};
ezmq_result({error, 14}) ->
    {error, efault};
ezmq_result({error, 15}) ->
    {error, enotblk};
ezmq_result({error, 16}) ->
    {error, ebusy};
ezmq_result({error, 17}) ->
    {error, eexist};
ezmq_result({error, 18}) ->
    {error, exdev};
ezmq_result({error, 19}) ->
    {error, enodev};
ezmq_result({error, 20}) ->
    {error, enotdir};
ezmq_result({error, 21}) ->
    {error, eisdir};
ezmq_result({error, 22}) ->
    {error, einval};
ezmq_result({error, 23}) ->
    {error, enfile};
ezmq_result({error, 24}) ->
    {error, emfile};
ezmq_result({error, 25}) ->
    {error, enotty};
ezmq_result({error, 26}) ->
    {error, etxtbsy};
ezmq_result({error, 27}) ->
    {error, efbig};
ezmq_result({error, 28}) ->
    {error, enospc};
ezmq_result({error, 29}) ->
    {error, espipe};
ezmq_result({error, 30}) ->
    {error, erofs};
ezmq_result({error, 31}) ->
    {error, emlink};
ezmq_result({error, 32}) ->
    {error, epipe};
ezmq_result({error, 35}) ->
    {error, eagain};
ezmq_result({error, 36}) ->
    {error, einprogress};
ezmq_result({error, 37}) ->
    {error, ealready};
ezmq_result({error, 38}) ->
    {error, enotsock};
ezmq_result({error, 39}) ->
    {error, edestaddrreq};
ezmq_result({error, 40}) ->
    {error, emsgsize};
ezmq_result({error, 41}) ->
    {error, eprototype};
ezmq_result({error, 42}) ->
    {error, enoprotoopt};
ezmq_result({error, 43}) ->
    {error, eprotonosupport};
ezmq_result({error, 44}) ->
    {error, esocktnosupport};
ezmq_result({error, 45}) ->
    {error, enotsup};
ezmq_result({error, 46}) ->
    {error, epfnosupport};
ezmq_result({error, 47}) ->
    {error, eafnosupport};
ezmq_result({error, 48}) ->
    {error, eaddrinuse};
ezmq_result({error, 49}) ->
    {error, eaddrnotavail};
ezmq_result({error, 50}) ->
    {error, enetdown};
ezmq_result({error, 51}) ->
    {error, enetunreach};
ezmq_result({error, 52}) ->
    {error, enetreset};
ezmq_result({error, 53}) ->
    {error, econnaborted};
ezmq_result({error, 54}) ->
    {error, econnreset};
ezmq_result({error, 55}) ->
    {error, enobufs};
ezmq_result({error, 56}) ->
    {error, eisconn};
ezmq_result({error, 57}) ->
    {error, enotconn};
ezmq_result({error, 58}) ->
    {error, eshutdown};
ezmq_result({error, 59}) ->
    {error, etoomanyrefs};
ezmq_result({error, 60}) ->
    {error, etimedout};
ezmq_result({error, 61}) ->
    {error, econnrefused};
ezmq_result({error, 62}) ->
    {error, eloop};
ezmq_result({error, 63}) ->
    {error, enametoolong};

ezmq_result({error, N}) ->
    {error, {unknown, N}}.
