#! /usr/bin/env escript
%%! -smp enable -pa ebin

main([ConnectTo,MessageSizeStr,RoundtripCountStr]) ->
    {MessageSize, _} = string:to_integer(MessageSizeStr),
    {RoundtripCount, _} = string:to_integer(RoundtripCountStr),
    {ok, Context} = ezmq:context(),
    {ok, Socket} = ezmq:socket(Context, req),
    ok = ezmq:connect(Socket, ConnectTo),
    Msg = list_to_binary(lists:duplicate(MessageSize, 0)),
    Do = fun() ->
            ezmq:send(Socket, Msg),
            {ok, Msg} = ezmq:brecv(Socket)
        end,

    {Elapsed, _} = timer:tc(fun () ->
                [ Do() || _I <- lists:seq(1,RoundtripCount) ]
        end,[]),

    Latency = Elapsed / (RoundtripCount * 2),

    io:format("message size: ~p [B]~n"
              "roundtrip count: ~p~n"
              "average latency: ~p [us]~n",
              [MessageSize, RoundtripCount, Latency]),
   ezmq:close(Socket),
   ezmq:term(Context).
