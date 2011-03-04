-module(ezmq_test).
-include_lib("eunit/include/eunit.hrl").
-export([worker/2]).

hwm_test() ->
    {ok, C} = ezmq:context(),
    {ok, S1} = ezmq:socket(C, pull),
    {ok, S2} = ezmq:socket(C, push),

    ok = ezmq:setsockopt(S2, linger, 0),
    ok = ezmq:setsockopt(S2, hwm, 5),

    ok = ezmq:bind(S1, "tcp://127.0.0.1:5858"),
    ok = ezmq:connect(S2, "tcp://127.0.0.1:5858"),

    ok = hwm_loop(10, S2),

    ?assertMatch({ok, <<"test">>}, ezmq:recv(S1)),
    ?assertMatch(ok, ezmq:send(S2, <<"test">>)).

hwm_loop(0, _S) ->
    ok;
hwm_loop(N, S) when N > 5 ->
    ?assertMatch(ok, ezmq:send(S, <<"test">>, [noblock])),
    hwm_loop(N-1, S);
hwm_loop(N, S) ->
    ?assertMatch({error, _} ,ezmq:send(S, <<"test">>, [noblock])),
    hwm_loop(N-1, S).


pair_inproc_test() ->
    basic_tests("inproc://tester", pair, pair).

pair_ipc_test() ->
    basic_tests("ipc:///tmp/tester", pair, pair).

pair_tcp_test() ->
    basic_tests("tcp://127.0.0.1:5555", pair, pair).

reqrep_inproc_test() ->
    basic_tests("inproc://test", req, rep).

reqrep_ipc_test() ->
    basic_tests("ipc:///tmp/tester", req, rep).

reqrep_tcp_test() ->
    basic_tests("tcp://127.0.0.1:5556", req, rep).

shutdown_stress_test() ->
    ?assertMatch(ok, shutdown_stress_loop(10)).

shutdown_stress_loop(0) ->
    ok;
shutdown_stress_loop(N) ->
    {ok, C} = ezmq:context(7),
    {ok, S1} = ezmq:socket(C, rep),
    ?assertMatch(ok, shutdown_stress_worker_loop(100, C)),
    ?assertMatch(ok, join_procs(100)),
    ?assertMatch(ok, ezmq:close(S1)),
    ?assertMatch(ok, ezmq:term(C)),
    shutdown_stress_loop(N-1).

shutdown_no_blocking_test() ->
    {ok, C} = ezmq:context(),
    {ok, S} = ezmq:socket(C, pub),
    ezmq:close(S),
    ?assertEqual(ok, ezmq:term(C, 500)).

shutdown_blocking_test() ->
    {ok, C} = ezmq:context(),
    {ok, _S} = ezmq:socket(C, pub),
    ?assertMatch({error, timeout, _}, ezmq:term(C, 500)).

shutdown_blocking_unblocking_test() ->
    {ok, C} = ezmq:context(),
    {ok, S} = ezmq:socket(C, pub),
    V = ezmq:term(C, 500),
    ?assertMatch({error, timeout, _}, V),
    {error, timeout, Ref} = V,
    ezmq:close(S),
    receive 
        {Ref, ok} ->
            ok
    end.

join_procs(0) ->
    ok;
join_procs(N) ->
    receive
        proc_end ->
            join_procs(N-1)
    end.

shutdown_stress_worker_loop(0, _) ->
    ok;
shutdown_stress_worker_loop(N, C) ->
    {ok, S2} = ezmq:socket(C, sub),
    spawn(?MODULE, worker, [self(), S2]),
    shutdown_stress_worker_loop(N-1, C).

worker(Pid, S) ->
    ?assertMatch(ok, ezmq:connect(S, "tcp://127.0.0.1:5557")),
    ?assertMatch(ok, ezmq:close(S)),
    Pid ! proc_end.

create_bound_pair(Ctx, Type1, Type2, Transport) ->
    {ok, S1} = ezmq:socket(Ctx, Type1),
    {ok, S2} = ezmq:socket(Ctx, Type2),
    ok = ezmq:bind(S1, Transport),
    ok = ezmq:connect(S2, Transport),
    {S1, S2}.

ping_pong({S1, S2}, Msg) ->
    ok = ezmq:send(S1, Msg),
    ?assertMatch({ok, Msg}, ezmq:recv(S2)),
    ok = ezmq:send(S2, Msg).

basic_tests(Transport, Type1, Type2) ->
    {ok, C} = ezmq:context(1),
    {S1, S2} = create_bound_pair(C, Type1, Type2, Transport),
    ping_pong({S1, S2}, <<"XXX">>).

