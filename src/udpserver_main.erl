-module (udpserver_main).

-export ([start_link/0]).


start_link() ->
    io:format("udpserver_main:start_link~n"),
    % Start UDP Server
    % spawn(udpserver_listener, start_link, []),

    % Start Kafka Producer
    % spawn(udpserver_kafka, start_link, []),

    ok.
