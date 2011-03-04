-module(ezmq).
-include_lib("ezmq.hrl").
-export([context/0, context/1, socket/2, bind/2, connect/2, send/2, send/3, brecv/1, brecv/2, recv/1, recv/2, setsockopt/3, getsockopt/2, close/1, term/1]).
-export_type([ezmq_socket/0, ezmq_context/0]).

-spec context() -> ezmq_context().

context() ->
    context(1).

-spec context(Threads :: pos_integer()) -> ezmq_context().
                     
context(Threads) when is_integer(Threads) ->
    ezmq_nif:context(Threads).

-spec socket(Context :: ezmq_context(), Type :: ezmq_socket_type()) -> ezmq_socket().
                    
socket(Context, Type) ->
    ezmq_nif:socket(Context, socket_type(Type)).

-spec bind(Socket :: ezmq_socket(), Endpoint :: ezmq_endpoint()) -> ok | ezmq_error().
                  
bind(Socket, Endpoint) ->
    ezmq_nif:bind(Socket, Endpoint).

-spec connect(Socket :: ezmq_socket(), Endpoint :: ezmq_endpoint()) -> ok | ezmq_error().
                     
connect(Socket, Endpoint) ->
    ezmq_nif:connect(Socket, Endpoint).

-spec send(Socket :: ezmq_socket(), Data :: ezmq_data()) -> ok | ezmq_error().
                     
send(Socket, Binary) ->
    send(Socket, Binary, []).

-spec send(Socket :: ezmq_socket(), Data :: ezmq_data(), Flags :: ezmq_send_recv_flags()) -> ok | ezmq_error().

send(Socket, Binary, Flags) when is_list(Flags) ->
    ezmq_nif:send(Socket, Binary, sendrecv_flags(Flags)).

-spec brecv(Socket :: ezmq_socket()) -> {ok, ezmq_data()} | ezmq_error().
                   
brecv(Socket) ->
    brecv(Socket, []).

-spec brecv(Socket :: ezmq_socket(), Flags :: ezmq_send_recv_flags()) -> {ok, ezmq_data()} | ezmq_error().

brecv(Socket, Flags) when is_list(Flags) ->
    ezmq_nif:brecv(Socket, sendrecv_flags(Flags)).

-spec recv(Socket :: ezmq_socket()) -> {ok, ezmq_data()} | ezmq_error().

recv(Socket) ->
    recv(Socket, []).

-spec recv(Socket :: ezmq_socket(), Flags :: ezmq_send_recv_flags()) -> {ok, ezmq_data()} | ezmq_error().

recv(Socket, Flags) when is_list(Flags) ->
    case ezmq_nif:recv(Socket, sendrecv_flags(Flags)) of
        Ref when is_reference(Ref) ->
            Timeout = proplists:get_value(timeout, Flags, infinity),
            receive
                {Ref, Result} ->
                    {ok, Result}
            after Timeout ->
                    {error, timeout}
            end;
        Result ->
            Result
    end.

-spec setsockopt(Socket :: ezmq_socket(), Name :: ezmq_sockopt(), ezmq_sockopt_value()) -> ok | ezmq_error().
                        
setsockopt(Socket, Name, Value) ->
    ezmq_nif:setsockopt(Socket, option_name(Name), Value).

-spec getsockopt(Socket :: ezmq_socket(), Name :: ezmq_sockopt()) -> {ok, ezmq_sockopt_value()} | ezmq_error().
                        
getsockopt(Socket, Name) ->
    ezmq_nif:getsockopt(Socket, option_name(Name)).

-spec close(Socket :: ezmq_socket()) -> ok | ezmq_error().
                   
close(Socket) ->
    ezmq_nif:close(Socket).

-spec term(Context :: ezmq_context()) -> ok | ezmq_error().

term(Context) ->
    ezmq_nif:term(Context).

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

