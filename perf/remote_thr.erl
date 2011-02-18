#! /usr/bin/env escript
%%! -smp enable -pa ebin

main([BindTo,MessageSizeStr,MessageCountStr]) ->
    {MessageSize, _} = string:to_integer(MessageSizeStr),
    {MessageCount, _} = string:to_integer(MessageCountStr),
    {ok, Context} = ezmq:context(1),
    {ok, Socket} = ezmq:socket(Context,pub),
    ezmq:bind(Socket, BindTo),
    io:get_line("Press enter when subscriber's ready"),
    ezmq:send(Socket, <<"start">>),
    Msg = list_to_binary(lists:duplicate(MessageSize, 0)),
    [ ezmq:send(Socket, Msg) || _I <- lists:seq(1, MessageCount) ].
