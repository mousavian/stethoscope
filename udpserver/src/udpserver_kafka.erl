-module (udpserver_kafka).

-export ([start/0, produce/1]).

-define (KAFKA_HOST, "kafka.dev").
-define (KAFKA_PORT, 9092).
-define (TOPIC, <<"foobar">>).

start() ->
    ok = application:load(ekaf),

    application:set_env(ekaf, ekaf_bootstrap_broker, {?KAFKA_HOST, ?KAFKA_PORT}),

    {ok, _} = application:ensure_all_started(ekaf),

    ok.


produce(Data) ->
    ekaf:produce_sync(?TOPIC, Data).
