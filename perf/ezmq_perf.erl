-module(ezmq_perf).
-export([recv_loop/2, send_loop/3]).


recv_loop(0, _) ->
    ok;
recv_loop(N, S) ->
    ezmq:recv(S),
    recv_loop(N-1, S).

send_loop(0, _, _) ->
    ok;
send_loop(N, S, M) ->
    ezmq:send(S, M),
    send_loop(N-1, S, M).
