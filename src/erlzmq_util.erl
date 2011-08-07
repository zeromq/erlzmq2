-module(erlzmq_util).

-export([dump/1]).

%%--------------------------------------------------------------------
%% @doc Reads available messages from Socket, printing them to stdout.
%% @spec dump(Socket) -> any()
%% Socket = erlzmq_socket()
%% @end
%%--------------------------------------------------------------------

dump(Socket) ->
    {ok, Msg} = erlzmq:recv(Socket),
    io:format("----------------------------------------~n"),
    dump_msg(Msg, Socket).

%%--------------------------------------------------------------------
%% @doc Print a socket message, including subsequent parts.
%% @spec dump_msg(Msg, Socket) -> ok
%% @end
%%--------------------------------------------------------------------

dump_msg(Msg, Socket) ->
    io:format("[~3..0B] ", [size(Msg)]),
    Str = binary_to_list(Msg),
    case io_lib:printable_list(Str) of
        true -> io:format(Str);
        false -> io:format(bin_to_hex(Msg))
    end,
    io:format("~n"),
    case erlzmq:getsockopt(Socket, rcvmore) of
        {ok, true} ->
            {ok, Next} = erlzmq:recv(Socket),
            dump_msg(Next, Socket);
        {ok, false} ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Convert a binary to a hex string.
%% @spec bin_to_hex(binary()) -> list()
%% @end
%%--------------------------------------------------------------------

bin_to_hex(B) when is_binary(B) ->
    lists:flatten(lists:map(fun int_to_hex/1, binary_to_list(B))).

%%--------------------------------------------------------------------
%% @doc Convert an int to a two char hex string.
%% @spec int_to_hex(integer()) -> string()
%% @end
%%--------------------------------------------------------------------

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

%%--------------------------------------------------------------------
%% @doc Converts an integer to a hex char.
%% @spec hex(integer()) -> char()
%% @end
%%--------------------------------------------------------------------

hex(N) when N < 10 -> $0 + N;
hex(N) when N >= 10, N < 16 -> $a + (N - 10).
