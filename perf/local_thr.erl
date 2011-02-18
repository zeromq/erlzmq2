#! /usr/bin/env escript
%%! -smp enable -pa ebin

main([ConnectTo,MessageSizeStr,MessageCountStr]) ->
    {MessageSize, _} = string:to_integer(MessageSizeStr),
    {MessageCount, _} = string:to_integer(MessageCountStr),
    {ok, Context} = ezmq:context(),
    {ok, Socket} = ezmq:socket(Context, sub),
    ok = ezmq:setsockopt(Socket,subscribe, <<>>),
    ok = ezmq:connect(Socket, ConnectTo),
    % use an extra message to synchronize
    {ok, <<"start">>} = ezmq:brecv(Socket),
    {Elapsed, _} = timer:tc(fun () ->
        [ {ok, _} = ezmq:brecv(Socket) || _I <- lists:seq(1,MessageCount) ]
        end,[]),

    Throughput = MessageCount / (Elapsed / 1000000),
    Megabits = Throughput * MessageSize * 8 / 1000000,

    io:format("message size: ~p [B]~n"
              "message count: ~p~n"
              "mean throughput: ~p [msg/s]~n"
              "mean throughput: ~p [Mbit/s]~n",
              [MessageSize, MessageCount, Throughput, Megabits]).
