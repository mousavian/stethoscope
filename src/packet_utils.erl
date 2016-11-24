-module (packet_utils).

-export ([stringify/2, get_protocol/2, convert_to_pcap_format/3]).

-define(is_print(C), C >= $\s, C =< $~).
-define(MAGIC, 16#a1b2c3d4).
-define(SNAPLEN, 65535).
-define(GMT_TO_LOCALTIME, 65535).
-define(SIGFIGS, 0).
-define(VERSION_MAJOR, 2).
-define(VERSION_MINOR, 4).

-include_lib("pkt/include/pkt.hrl").


get_protocol(DLT, Data) ->
    Packet = decode(DLT, Data),
    extract_protocol(Packet).


stringify(DLT, Data) ->
    Packet = decode(DLT, Data),
    Headers = header(Packet),
    Headers.


decode(DLT, Data) ->
    case pkt:decode(pkt:dlt(DLT), Data) of
        {ok, {Headers, Payload}} ->
            Headers ++ [Payload];
        {error, SoFar, _Failed} ->
            SoFar
    end.


extract_protocol(Packet) ->
    extract_protocol(continue, Packet).

extract_protocol(done, Protocol) ->
    Protocol;
extract_protocol(continue, [#ether{}|Rest]) ->
    extract_protocol(continue, Rest);
extract_protocol(continue, [#ipv4{}|Rest]) ->
    extract_protocol(continue, Rest);
extract_protocol(continue, [#ipv6{}|Rest]) ->
    extract_protocol(done, Rest);
extract_protocol(continue, [#tcp{dport = Dport}|_]) ->
    extract_protocol(done, {tcp, Dport});
extract_protocol(continue, [#udp{dport = Dport}|_]) ->
    extract_protocol(done, {udp, Dport});
extract_protocol(continue, [#icmp{}|_]) ->
    extract_protocol(done, icmp);
extract_protocol(continue, [#icmp6{}|_]) ->
    extract_protocol(done, icmp6);
extract_protocol(continue, [#arp{}|_]) ->
    extract_protocol(done, arp);
extract_protocol(continue, [#sctp{}|_]) ->
    extract_protocol(done, sctp);
extract_protocol(continue, [#igmp{}|_]) ->
    extract_protocol(done, igmp);
extract_protocol(continue, [#'802.1q'{}|_]) ->
    extract_protocol(done, '802.1q');
extract_protocol(continue, [#gre{}|_]) ->
    extract_protocol(done, gre);
extract_protocol(continue, [#igmp_group{}|_]) ->
    extract_protocol(done, igmp_group);
extract_protocol(continue, [#llc{}|_]) ->
    extract_protocol(done, llc);
extract_protocol(continue, [#lldp{}|_]) ->
    extract_protocol(done, lldp);
extract_protocol(continue, [#mpls{}|_]) ->
    extract_protocol(done, mpls);
extract_protocol(continue, [#vrrp{}|_]) ->
    extract_protocol(done, vrrp);
extract_protocol(continue, [#null{}|_]) ->
    extract_protocol(done, null);
extract_protocol(continue, [_Hdr|_Rest]) ->
    extract_protocol(done, unkown).















header(Payload) ->
    header(Payload, []).

header([], Acc) ->
    lists:reverse(Acc);


header([#ether{shost = Shost, dhost = Dhost}|Rest], Acc) ->
    header(Rest, [{ether, [{source_macaddr, ether_addr(Shost)},
                    {destination_macaddr, ether_addr(Dhost)}]}|Acc]);


header([#ipv4{saddr = Saddr, daddr = Daddr, p = Proto}|Rest], Acc) ->
    header(Rest, [{ipv4, [{protocol, pkt:proto(Proto)},
                    {source_address, inet_parse:ntoa(Saddr)},
                    {destination_address, inet_parse:ntoa(Daddr)}]}|Acc]);


header([#ipv6{saddr = Saddr, daddr = Daddr, next = Proto}|Rest], Acc) ->
    header(Rest, [{ipv6, [{protocol, pkt:proto(Proto)},
                    {source_address, inet_parse:ntoa(Saddr)},
                    {destination_address, inet_parse:ntoa(Daddr)}]}|Acc]);



header([#tcp{sport = Sport, dport = Dport, ackno = Ackno, seqno = Seqno,
            win = Win, cwr = CWR, ece = ECE, urg = URG, ack = ACK, psh = PSH,
            rst = RST, syn = SYN, fin = FIN}|Rest], Acc) ->
    Flags = [ F || {F,V} <- [{cwr, CWR}, {ece, ECE}, {urg, URG}, {ack, ACK},
                   {psh, PSH}, {rst, RST}, {syn, SYN}, {fin, FIN} ], V =:= 1 ],
    header(Rest, [{tcp, [{source_port, Sport}, {destination_port, Dport},
                    {flags, Flags}, {seq, Seqno}, {ack, Ackno}, {win, Win}]}|Acc]);


header([#udp{sport = Sport, dport = Dport, ulen = Ulen}|Rest], Acc) ->
    header(Rest, [{udp, [{source_port, Sport}, {destination_port, Dport},
                    {ulen, Ulen}]}|Acc]);


header([#icmp{type = Type, code = Code}|Rest], Acc) ->
    header(Rest, [{icmp, [{type, Type}, {code, Code}]}|Acc]);


header([#icmp6{type = Type, code = Code}|Rest], Acc) ->
    header(Rest, [{icmp6, [{type, Type}, {code, Code}]}|Acc]);


header([Hdr|Rest], Acc) when is_tuple(Hdr) ->
    header(Rest, [{header, Hdr}|Acc]);


header([Payload|Rest], Acc) when is_binary(Payload) ->
    header(Rest, [{payload, to_ascii(Payload)},
            {payload_size, byte_size(Payload)}|Acc]).




to_ascii(Bin) when is_binary(Bin) ->
    [ to_ascii(C) || <<C:8>> <= Bin ];
to_ascii(C) when ?is_print(C) -> C;
to_ascii(_) -> $..

to_hex(Bin) when is_binary(Bin) ->
    [ integer_to_list(N, 16) || <<N:8>> <= Bin ].

ether_addr(MAC) ->
    string:join(to_hex(MAC), ":").

timestamp(Now) when is_tuple(Now) ->
    iso_8601_fmt(calendar:now_to_local_time(Now)).

iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
            [Year, Month, Day, Hour, Min, Sec])).







network(null) -> 0;
network(ethernet) -> 1;
network(mtp2) -> 140;
network(mtp3) -> 141;
network(sccp) -> 142;
network(ipv4) -> 228;
network(ipv6) -> 229.

convert_to_pcap_format({MegaSecs, Secs, _}, Len, Binary) ->
    Network = network(ethernet),
    Header = <<?MAGIC:32, ?VERSION_MAJOR:16, ?VERSION_MINOR:16,
        ?GMT_TO_LOCALTIME:32, ?SIGFIGS:32, ?SNAPLEN:32, Network:32>>,

    Timestamp = MegaSecs * 1000000 + Secs,
    Timestamp_us = (Timestamp rem 1000) * 1000,
    <<Header/binary, Timestamp:32, Timestamp_us:32, Len:32, Len:32, Binary/binary>>.
