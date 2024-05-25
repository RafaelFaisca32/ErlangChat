-module(server).
-export([start/1, init/1, createServer/2]).

start(Server) -> 
    register(Server, spawn(fun() -> init(Server) end)).

init(Server) ->
    {ok, Hostname} = inet:gethostname(),
    Router = list_to_atom("router@" ++ Hostname),
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
