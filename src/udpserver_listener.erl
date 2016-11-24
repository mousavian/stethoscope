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
            {filter, "udp or tcp"},
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


handler(DataLink, Time, Length, Data) ->
    PCAP = packet_utils:convert_to_pcap_format(Time, Length, Data),
    udpserver_kafka:produce({<<DataLink>>, PCAP}).
