-module(router).
-export([start/1, register_server/2, server_down/1, get_server/1, find_server/2, start_monitor/0]).

start(UpdatedListServers) ->
    register(router, spawn(fun() -> loop(UpdatedListServers) end)).

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

loop(ListServers) ->
    receive
        {register_server, Server, Pid} ->
            io:format("Creating server ~p with PID ~p~n", [Server, Pid]),
            case lists:member({Pid, Server}, ListServers) of
                true -> 
                    io:format("Not a new server: ~p~n", [{Pid, Server}]),
                    loop(ListServers);
                false -> 
                    io:format("Added a new server: ~p~n", [{Pid, Server}]),
                    PidRouterMonitor = whereis(router_monitor),
                    PidRouterMonitor ! {updateList,  [{Pid, Server} | ListServers]},
                    loop([{Pid, Server} | ListServers])
                end;
        {server_down, Pid} ->
            io:format("Server downed by Server Monitor with Pid ~p~n", [Pid]),
            UpdatedListServers = lists:keydelete(Pid, 1, ListServers),
            io:format("Updated list of servers after server_down request: ~p~n", [UpdatedListServers]),
            PidRouterMonitor = whereis(router_monitor),
            PidRouterMonitor ! {updateList, UpdatedListServers},
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

start_monitor() ->
    register(router_monitor, spawn(fun() -> monitor_loop(router, []) end)). %starts server monitor

monitor_loop(Router, ListServers) ->
    Pid = whereis(Router),
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            io:format("Router ~p is down: ~p~n", [Router, Reason]),
            restart(ListServers),
            monitor_loop(Router, ListServers);
        {updateList, UpdatedListServers} ->
            io:format("Server list in the monitor: ~p~n", [UpdatedListServers]),
            monitor_loop(Router,UpdatedListServers)
    end.

restart(UpdatedListServers) ->
    io:format("Restarting router"),
    router:start(UpdatedListServers).

find_server(_, []) ->
    false; % Server not found
find_server(Server, [{Pid, ServerName} | _]) when Server == ServerName ->
    {ok, Pid}; % Found the server
find_server(Server, [_ | Rest]) ->
    find_server(Server, Rest). % Continue searching the rest of the list