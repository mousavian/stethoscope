-module (udpserver_listener).

-export ([start/0, handler/4]).


start() ->
    spawn_link(udpserver_kafka, start, []),
    init_pcap(),
    loop().


init_pcap() ->
    Interface = os:getenv("INTERFACE_TO_SNIFF"),
    Promiscuous = true,
    Snaplen = 65535,
    Options = [
            {interface, Interface},
            {promiscuous, Promiscuous},
            {snaplen, Snaplen}
        ],
    {ok, Pid} = epcap:start_link(Options),

    error_logger:info_report("PCAP initiated"),
    Pid.


loop() ->
    receive
        {packet, DataLink, Time, Length, Data} ->
            spawn(?MODULE, handler, [DataLink, Time, Length, Data]),
            loop()
    end.


handler(DataLink, _Time, _Length, Data) ->
    ProtocolPort = packet_utils:get_protocol(DataLink, Data),
    case ProtocolPort of
        {tcp, Port} ->
            udpserver_kafka:produce({<<Port>>, Data});
        {udp, Port} ->
            udpserver_kafka:produce({<<Port>>, Data});
        _ ->
            ok
    end.
