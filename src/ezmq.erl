-module(ezmq).
-include_lib("ezmq.hrl").
-export([context/0, context/1, socket/2, bind/2, connect/2, send/2, send/3, brecv/1, brecv/2, recv/1, recv/2, setsockopt/3, getsockopt/2]).

context() ->
    context(1).

context(Threads) when is_integer(Threads) ->
    ezmq_nif:context(Threads).

socket(Context, Type) ->
    ezmq_nif:socket(Context, socket_type(Type)).

bind(Socket, Endpoint) ->
    ezmq_nif:bind(Socket, Endpoint).

connect(Socket, Endpoint) ->
    ezmq_nif:connect(Socket, Endpoint).

send(Socket, Binary) ->
    send(Socket, Binary, []).

send(Socket, Binary, Flags) when is_list(Flags) ->
    ezmq_nif:send(Socket, Binary, sendrecv_flags(Flags)).

brecv(Socket) ->
    brecv(Socket, []).

brecv(Socket, Flags) when is_list(Flags) ->
    ezmq_nif:brecv(Socket, sendrecv_flags(Flags)).

recv(Socket) ->
    recv(Socket, []).

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

setsockopt(Socket, Name, Value) ->
    ezmq_nif:setsockopt(Socket, option_name(Name), Value).

getsockopt(Socket, Name) ->
    ezmq_nif:getsockopt(Socket, option_name(Name)).

%% Private

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

sendrecv_flags([]) ->
    0;
sendrecv_flags([{timeout,_}]) ->
    0;
sendrecv_flags([noblock|Rest]) ->
    ?'ZMQ_NOBLOCK' bor sendrecv_flags(Rest);
sendrecv_flags([sndmore|Rest]) ->
    ?'ZMQ_SNDMORE' bor sendrecv_flags(Rest).

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
option_name(fd) ->
    ?'ZMQ_FD'.
