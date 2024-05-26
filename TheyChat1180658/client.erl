-module(client).
-export([start/1, stop_client/1, add_remote/1, send_msg/3, get_host_name/0]).

start(Client) -> 
    Router = list_to_atom("router@" ++ get_host_name()),
    register(Client, spawn(fun() -> loop(Router) end)).

add_remote(Server) ->
    ServerNode = list_to_atom(lists:concat([Server, "@", get_host_name()])),
    net_adm:ping(ServerNode).

get_host_name() ->
    {ok, Hostname} = inet:gethostname(),
    Hostname.

send_msg(Client, Server, Message) -> 
    add_remote(Server),
    Client ! {send, Server, Message}.

stop_client(Client) -> 
    Client ! {stop_client}.

loop(Router) -> 
    receive
        {send, Server, Message} -> 
            case erpc:call(Router, router, get_server, [Server]) of
                {ok, Pid} ->
                    io:format("Server ~p located~n", [Server]),
                    Pid ! {self(), Message},
                    io:format("Message ~p sent to server ~p at PID ~p~n", [Message, Server, Pid]),
                    loop(Router);
                {error, Problem} ->
                    io:format("Problem discovered: ~p on Server: ~p~n", [Problem, Server]),
                    loop(Router)
            end;
        {Server, server_disconnect} ->
            io:format("Client ~p leaving from Server: ~p~n",[self(),Server]);
        {Server, _} ->
            io:format("I'm the server ~p~n", [Server])
    end.

