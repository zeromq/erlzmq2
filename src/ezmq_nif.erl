-module(ezmq_nif).

-export([context/1, socket/2, bind/2, connect/2, send/3, brecv/2]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    case code:which(ezmq_nif) of
        Filename when is_list(Filename) ->
            erlang:load_nif(filename:join([filename:dirname(Filename),"../priv/ezmq_drv"]), []);
        Err ->
            Err
    end.

context(_Threads) ->
    error(not_loaded).

socket(_Context, _Type) ->
    error(not_loaded).

bind(_Socket, _Endpoint) ->
    error(not_loaded).

connect(_Socket, _Endpoint) ->
    error(not_loaded).

send(_Socket, _Binary, _Flags) ->
    error(not_loaded).

brecv(_Socket, _Flags) ->
    error(not_loaded).
