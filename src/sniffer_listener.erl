-module (sniffer_listener).

-export ([start/0, handler/4]).


start() ->
    spawn_link(sniffer_kafka, start, []),
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


handler(_DataLink, _Time, _Length, Data) ->
    sniffer_kafka:produce(Data).
