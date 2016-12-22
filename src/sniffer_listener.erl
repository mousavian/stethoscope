-module (sniffer_listener).

-export ([start/0, handler/5]).


start() ->
    {_, Channel} = sniffer_rabbitmq:start(),
    init_pcap(),
    loop(Channel).


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


loop(Channel) ->
    receive
        {packet, DataLink, Time, Length, Data} ->
            spawn(?MODULE, handler, [Channel, DataLink, Time, Length, Data]),
            loop(Channel)
    end.


handler(Channel, _DataLink, _Time, _Length, Data) ->
    sniffer_rabbitmq:produce(Channel, Data).
