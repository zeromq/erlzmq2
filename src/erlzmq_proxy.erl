-module(erlzmq_proxy).
-export([create/2]).


%%---------------------------------------------------
%% @doc A proxy implemented in Erlang.
%% 
%% Frontend and Backend must be sockets in default, 
%% i.e. non-active, mode
%%
%% @spec create(Frontend, Backend) -> any()
%% Frontend = erlzmq_socket()
%% Backend = erlzmq_socket()
%% @end
%%---------------------------------------------------

create(Frontend, Backend) ->
    case erlzmq:recv(Backend, [dontwait]) of
        {error, eagain} -> ok;
        {ok, MsgB} ->
            case erlzmq:getsockopt(Backend, rcvmore) of
                {ok, true} -> erlzmq:send(Frontend, MsgB, [sndmore]);
                {ok, false} -> erlzmq:send(Frontend, MsgB)
            end
    end,
    case erlzmq:recv(Frontend, [dontwait]) of
        {error, eagain} -> ok;
        {ok, MsgF} ->
            case erlzmq:getsockopt(Frontend, rcvmore) of
                {ok, true} -> erlzmq:send(Backend, MsgF, [sndmore]);
                {ok, false} -> erlzmq:send(Backend, MsgF)
            end
    end,
    receive
        shutdown -> 
            ok
    after 
        0 ->
            create(Frontend, Backend)
    end.
