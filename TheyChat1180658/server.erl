-module(server).
-export([start/1, init/1, createServer/2, start_monitor/1, monitor_loop/1, restart/1,get_host_name/0]).

start(Server) -> 
    register(Server, spawn(fun() -> init(Server) end)). %starts normal behaviour of the server

get_host_name() ->
    {ok, Hostname} = inet:gethostname(),
    Hostname.

init(Server) ->
    Router = list_to_atom("router@" ++ get_host_name()),
    createServer(Server,Router),
    loop().

createServer(Server, Router) ->
    net_adm:ping(Router),
    Pid = whereis(Server),  % Pid of the new Server
    erpc:call(Router, router, register_server, [Server, Pid]). % tcp calls to certain router methods

loop() -> 
    receive
        {From, stop} -> 
            io:format("Received from ~p message to stop!~n", [From]),
            From ! {self(), server_disconnect};
        {From, Msg} -> 
            io:format("Received ~p: ~p~n", [From, Msg]),
            io:format("Sending reply...~n"), 
            From ! {self(), happy_to_receive_your_message},
            loop();
        AnyOtherMsg ->
            io:format("Unknown message: ~p~n", [AnyOtherMsg]),
            loop()
    end.

start_monitor(Server) ->
    spawn(fun() -> monitor_loop(Server) end). %starts server monitor

monitor_loop(Server) ->
    Pid = whereis(Server),
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            io:format("Server ~p is down: ~p~n", [Server, Reason]),
            Router = list_to_atom("router@" ++ get_host_name()),
            erpc:call(Router, router, server_down, [Pid]), %removes the server from the router list since it doesn't exist anymore
            restart(Server),
            monitor_loop(Server)
    end.

restart(Server) ->
    io:format("Restarting server ~p~n", [Server]),
    server:start(Server).