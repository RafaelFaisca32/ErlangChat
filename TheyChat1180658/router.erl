-module(router).
-export([start/0, register_server/2, server_down/1, get_server/1, find_server/2]).

start() ->
    register(router, spawn(fun() -> init() end)).

register_server(Server, Pid) ->
    router ! {register_server, Server, Pid}.

server_down(Pid) ->
    router ! {server_down, Pid}.

get_server(Server) ->
    io:format("Received request to get server ~p~n", [Server]),
    router ! {get_server, Server, self()},
    receive
        {info, Pid} ->
            io:format("Found server ~p with PID ~p~n", [Server, Pid]),
            {ok, Pid};
        {error, Problem} ->
            io:format("Error getting server ~p: ~p~n", [Server, Problem]),
            {error, Problem}
    end.

init()->
    ListServers = [],
    loop(ListServers).

loop(ListServers) ->
    receive
        {register_server, Server, Pid} ->
            io:format("Creating server ~p with PID ~p~n", [Server, Pid]),
            UpdatedListServers = [{Pid, Server} | ListServers],
            io:format("Current list of servers after registration: ~p~n", [UpdatedListServers]),
            loop(UpdatedListServers);
        {'DOWN', _, process, Pid, _} ->
            io:format("Server process down: ~p~n", [Pid]),
            UpdatedListServers = lists:keydelete(Pid, 1, ListServers),
            io:format("Updated list of servers after removal: ~p~n", [UpdatedListServers]),
            loop(UpdatedListServers);
        {get_server, Server, From} ->
            io:format("Received request to get server ~p from ~p~n", [Server, From]),
            case find_server(Server, ListServers) of
                {ok, Pid} ->
                    From ! {info, Pid},
                    loop(ListServers);
                false ->
                    io:format("Server ~p was not found~n", [Server]),
                    From ! {error, "Server not found"},
                    loop(ListServers)
            end
    end.

find_server(_, []) ->
    false; % Server not found
find_server(Server, [{Pid, ServerName} | _]) when Server == ServerName ->
    {ok, Pid}; % Found the server
find_server(Server, [_ | Rest]) ->
    find_server(Server, Rest). % Continue searching the rest of the list