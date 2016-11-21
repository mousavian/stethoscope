-module (udpserver_listener).

-export ([start/0, handler/2]).

-define(PORT, 12345).


start() ->
    spawn_link(udpserver_kafka, start, []),

    {ok, Socket} = gen_udp:open(?PORT, [binary, {active, false}]),
    io:format("UDP server is started on port ~p~n", [?PORT]),

    % increase socket buffer size to 50MB
    inet:setopts(Socket, [{recbuf, 50*1024*1024}]),

    loop(Socket).


loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {udp, Socket, Host, _Port, Bin} ->
            spawn(?MODULE, handler, [Host, Bin]),
            loop(Socket)
    end.


handler(_Host, Bin) ->
    udpserver_kafka:produce(Bin).
