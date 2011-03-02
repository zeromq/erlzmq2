#! /usr/bin/env escript
%%! -smp enable -pa ebin -pa perf

main([ConnectTo,MessageSizeStr,MessageCountStr]) ->
    {MessageSize, _} = string:to_integer(MessageSizeStr),
    {MessageCount, _} = string:to_integer(MessageCountStr),
    {ok, Context} = ezmq:context(1),
    {ok, Socket} = ezmq:socket(Context,pub),
    ezmq:connect(Socket, ConnectTo),
    Msg = list_to_binary(lists:duplicate(MessageSize, 0)),
    ezmq_perf:send_loop(MessageCount, Socket, Msg).

