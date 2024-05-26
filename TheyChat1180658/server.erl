-module(server).
-export([start/2, init/2, createServer/2, start_monitor/1, monitor_loop/2, restart/2,get_host_name/0]).

start(Server, ListClients) -> 
    register(Server, spawn(fun() -> init(Server, ListClients) end)). %starts normal behaviour of the server

get_host_name() ->
    {ok, Hostname} = inet:gethostname(),
    Hostname.

init(Server, ListClients) ->
    Router = list_to_atom("router@" ++ get_host_name()),
    createServer(Server,Router),
    loop(ListClients).

add_client(Client, ListClients) ->
    case lists:member(Client, ListClients) of
        true -> 
            io:format("Not a new client: ~p~n", [Client]),
            ListClients; % Client is already in the list, do not add
        false -> 
            io:format("Added a new client: ~p~n", [Client]),
            PidServerMonitor = whereis(server_monitor),
            PidServerMonitor ! {new_client,  [Client | ListClients]},
            [Client | ListClients] % Add new client to the list
    end.

remove_client(Client, ListClients) ->
    io:format("Removed the client: ~p~n", [Client]),
    UpdatedList = lists:delete(Client, ListClients),
    PidServerMonitor = whereis(server_monitor),
    PidServerMonitor ! {removed_client, UpdatedList},
    UpdatedList. % Remove client from the list

createServer(Server, Router) ->
    net_adm:ping(Router),
    Pid = whereis(Server),  % Pid of the new Server
    erpc:call(Router, router, register_server, [Server, Pid]). % tcp calls to certain router methods

loop(ListClients) -> 
    receive
        {From, stop} -> 
            io:format("Received from ~p message to stop!~n", [From]),
            UpdatedListClients = remove_client(From, ListClients),
            io:format("CurrentClientList: ~p~n", [UpdatedListClients]),
            From ! {self(), server_disconnect},
            loop(UpdatedListClients);
        {From, Msg} -> 
            UpdatedListClients = add_client(From, ListClients),
            io:format("CurrentClientList~p~n", [UpdatedListClients]),
            io:format("Received ~p: ~p~n", [From, Msg]),
            io:format("Sending reply...~n"), 
            From ! {self(), happy_to_receive_your_message},
            loop(UpdatedListClients)
    end.

start_monitor(Server) ->
    register(server_monitor, spawn(fun() -> monitor_loop(Server, []) end)). %starts server monitor

monitor_loop(Server, ListClients) ->
    Pid = whereis(Server),
    Ref = erlang:monitor(process, Pid),
    receive
        {removed_client, UpdatedList} ->
            io:format("Client list in the monitor: ~p~n", [UpdatedList]),
            monitor_loop(Server, UpdatedList);
        {new_client,  [Client | ListClients]} ->
            io:format("Client list in the monitor: ~p~n", [[Client | ListClients]]),
            monitor_loop(Server, [Client | ListClients]);
        {'DOWN', Ref, process, Pid, Reason} ->
            io:format("Server ~p is down: ~p~n", [Server, Reason]),
            Router = list_to_atom("router@" ++ get_host_name()),
            erpc:call(Router, router, server_down, [Pid]), %removes the server from the router list since it doesn't exist anymore
            restart(Server, ListClients),
            monitor_loop(Server, ListClients)
    end.

restart(Server, ListClients) ->
    io:format("Restarting server ~p~n", [Server]),
    server:start(Server, ListClients).