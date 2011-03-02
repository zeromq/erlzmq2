#! /usr/bin/env escript
%%! -smp enable -pa ebin -pa perf
%-mode(compile).

main([BindTo,MessageSizeStr,MessageCountStr]) ->
    {MessageSize, _} = string:to_integer(MessageSizeStr),
    {MessageCount, _} = string:to_integer(MessageCountStr),
    {ok, Context} = ezmq:context(),
    {ok, Socket} = ezmq:socket(Context, sub),
    ok = ezmq:setsockopt(Socket,subscribe, <<>>),
    ok = ezmq:bind(Socket, BindTo),
    ezmq:recv(Socket),
    Start = now(),
    ezmq_perf:recv_loop(MessageCount-1, Socket),
    Elapsed = timer:now_diff(now(), Start),

    Throughput = MessageCount / Elapsed * 1000000,
    Megabits = Throughput * MessageSize * 8 / 1000000,

    io:format("message size: ~p [B]~n"
              "message count: ~p~n"
              "mean throughput: ~p [msg/s]~n"
              "mean throughput: ~p [Mb/s]~n",
              [MessageSize, MessageCount, Throughput, Megabits]).

