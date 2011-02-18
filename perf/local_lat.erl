#! /usr/bin/env escript
%%! -smp enable -pa ebin

main([BindTo,MessageSizeStr,RoundtripCountStr]) ->
    {MessageSize, _} = string:to_integer(MessageSizeStr),
    {RoundtripCount, _} = string:to_integer(RoundtripCountStr),
    {ok, Context} = ezmq:context(),
    {ok, Socket} = ezmq:socket(Context, rep),
    ok = ezmq:bind(Socket, BindTo),
    Msg = list_to_binary(lists:duplicate(MessageSize, 0)),
    Do = fun() ->
            {ok, RMsg} = ezmq:brecv(Socket),
            RMsg = Msg,
            ezmq:send(Socket, Msg)
        end,
    [ Do() || _I <- lists:seq(1,RoundtripCount) ].

