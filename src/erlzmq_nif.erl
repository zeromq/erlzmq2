%% @hidden
-module(erlzmq_nif).

-export([context/1,
         socket/4,
         bind/2,
         connect/2,
         send/3,
         recv/2,
         setsockopt/3,
         getsockopt/2,
         close/1,
         term/1,
         version/0]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                _ ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end,
    erlang:load_nif(SoName, 0).


context(_Threads) ->
    erlang:nif_error(not_loaded).

socket(_Context, _Type, _Active, _ActivePid) ->
    erlang:nif_error(not_loaded).

bind(_Socket, _Endpoint) ->
    erlang:nif_error(not_loaded).

connect(_Socket, _Endpoint) ->
    erlang:nif_error(not_loaded).

send(_Socket, _Binary, _Flags) ->
    erlang:nif_error(not_loaded).

recv(_Socket, _Flags) ->
    erlang:nif_error(not_loaded).

setsockopt(_Socket, _OptionName, _OptionValue) ->
    erlang:nif_error(not_loaded).

getsockopt(_Socket, _OptionName) ->
    erlang:nif_error(not_loaded).

close(_Socket) ->
    erlang:nif_error(not_loaded).

term(_Context) ->
    erlang:nif_error(not_loaded).

version() ->
    erlang:nif_error(not_loaded).
