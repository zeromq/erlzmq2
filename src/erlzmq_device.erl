-module(erlzmq_device).

-export([queue/2]).

-import(proplists, [get_bool/2]).

%%--------------------------------------------------------------------
%% @doc A queue device implemented in Erlang.
%%
%% Frontend and Backend must be sockets in active mode.
%%
%% This function will not return.
%%
%% @spec queue(Frontend, Backend) -> any()
%% Frontend = erlzmq_socket()
%% Backend = erlzmq_socket()
%% @end
%%--------------------------------------------------------------------

queue(Frontend, Backend) ->
    receive
        {zmq, Frontend, Msg, Flags} ->
            Parts = lists:reverse(queue_recv_acc(Frontend, Flags, [Msg])),
            queue_send(Backend, Parts),
            queue(Frontend, Backend);
        {zmq, Backend, Msg, Flags} ->
            Parts = lists:reverse(queue_recv_acc(Backend, Flags, [Msg])),
            queue_send(Frontend, Parts),
            queue(Frontend, Backend);
        {shutdown} ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Accumulates messages from Socket.
%% @spec queue_recv_acc(Socket, Flags, Acc0) -> Acc
%% @end
%%--------------------------------------------------------------------

queue_recv_acc(Socket, Flags0, Acc) ->
    case get_bool(rcvmore, Flags0) of
        true ->
            receive
                {zmq, Socket, Msg, Flags} ->
                    queue_recv_acc(Socket, Flags, [Msg|Acc])
            end;
        false -> Acc
    end.

%%--------------------------------------------------------------------
%% @doc Sends a multipart message to Out.
%% @spec queue_send(erlzmq_socket(), Parts) -> ok
%% @end
%%--------------------------------------------------------------------

queue_send(Out, [LastPart]) ->
    ok = erlzmq:send(Out, LastPart);
queue_send(Out, [Part|Rest]) ->
    ok = erlzmq:send(Out, Part, [sndmore]),
    queue_send(Out, Rest).
