-module(erlzmq_parts).

-export([new/0, part_in/2]).

new() -> [].

part_in({zmq, Socket, Part, [rcvmore]}, Parts) ->
    {rcvmore, add_part(Socket, Part, Parts)};
part_in({zmq, Socket, Part, []}, Parts) ->
    {SocketParts, NewParts} =
        find_socket_parts(Socket, add_part(Socket, Part, Parts)),
    {msg, Socket, SocketParts, NewParts}.

add_part(Socket, Part, Parts) ->
    [{Socket, Part}|Parts].

find_socket_parts(Socket, Parts) ->
    find_socket_parts_acc(Socket, Parts, [], []).

find_socket_parts_acc(_Socket, [], SocketParts, NewParts) ->
    {SocketParts, lists:reverse(NewParts)};
find_socket_parts_acc(Socket, [{Socket, Part}|Rest], SocketParts, NewParts) ->
    find_socket_parts_acc(Socket, Rest, [Part|SocketParts], NewParts);
find_socket_parts_acc(Socket, [OtherPart|Rest], SocketParts, NewParts) ->
    find_socket_parts_acc(Socket, Rest, SocketParts, [OtherPart|NewParts]).

