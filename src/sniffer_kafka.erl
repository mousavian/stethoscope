-module (sniffer_kafka).

-export ([start/0, produce/1]).

-define (KAFKA_HOST, "kafka.dev").
-define (KAFKA_PORT, 9092).
-define (RAW_PACKETS_TOPIC, <<"raw_packets">>).
-define (DECODED_LAYERS_TOPIC, <<"decoded_layers">>).

start() ->
    application:set_env(ekaf, ekaf_bootstrap_broker, {?KAFKA_HOST, ?KAFKA_PORT}),

    {ok, _} = application:ensure_all_started(ekaf),

    error_logger:info_report("Kafka Connected"),
    ok.


produce(Data) ->
    ekaf:produce_sync(?RAW_PACKETS_TOPIC, Data).
