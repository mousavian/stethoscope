-module (sniffer_rabbitmq).

-export ([start/0, stop/2, produce/2]).

-define (RABBITMQ_HOST, "kafka.dev").
-define (RABBITMQ_PORT, 5672).
-define (RABBITMQ_USERNAME, <<"ngnrem">>).
-define (RABBITMQ_PASSWORD, <<"merngn">>).
-define (RAW_PACKETS_TOPIC, <<"raw_packets">>).
-define (DECODED_LAYERS_TOPIC, <<"decoded_layers">>).

-include_lib("amqp_client/include/amqp_client.hrl").

%% rabbitmqctl add_user ngnrem merngn
%% rabbitmqctl set_user_tags ngnrem administrator
%% rabbitmqctl set_permissions -p / ngnrem ".*" ".*" ".*"

start() ->
    %% Start a network connection
    {ok, Connection} = amqp_connection:start(
        #amqp_params_network{
            host=?RABBITMQ_HOST,
            port=?RABBITMQ_PORT,
            username=?RABBITMQ_USERNAME,
            password=?RABBITMQ_PASSWORD}),

    %% Open a channel on the connection
    {ok, Channel} = amqp_connection:open_channel(Connection),

    %% Declare a queue
    Declare = #'queue.declare'{queue=?RAW_PACKETS_TOPIC, durable=true},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),

    error_logger:info_report("RabbitMQ Connected"),

    {Connection, Channel}.


stop(Connection, Channel) ->
    %% Close the channel
    amqp_channel:close(Channel),
    %% Close the connection
    amqp_connection:close(Connection),
    ok.

produce(Channel, Data) ->
    %% Publish a message
    Publish = #'basic.publish'{exchange = <<>>, routing_key = ?RAW_PACKETS_TOPIC},
    Props = #'P_basic'{delivery_mode = 2}, %% persistent message
    amqp_channel:cast(Channel, Publish, #amqp_msg{props = Props, payload = Data}).
