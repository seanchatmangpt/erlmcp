%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin Exporter Behavior - Export to external systems
%%%
%%% Exporters send data to external systems:
%%% - S3, GCS object storage
%%% - Kafka, RabbitMQ message queues
%%% - Elasticsearch, databases
%%% - Custom HTTP endpoints
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(erlmcp_plugin_s3_exporter).
%%% -behaviour(erlmcp_plugin_exporter).
%%%
%%% -export([init/1, export/2, get_config_schema/0, metadata/0]).
%%%
%%% metadata() ->
%%%     #{name => <<"s3_exporter">>,
%%%       version => <<"1.0.0">>,
%%%       type => exporter,
%%%       description => <<"Export to S3">>}.
%%%
%%% init(Opts) ->
%%%     Bucket = maps:get(bucket, Opts),
%%%     {ok, #{bucket => Bucket}}.
%%%
%%% export(Data, State) ->
%%%     %% Upload to S3
%%%     {ok, #{url => <<"s3://bucket/key">>}, State}.
%%%
%%% get_config_schema() ->
%%%     #{type => object,
%%%       properties => #{bucket => #{type => string}}}.
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_exporter).

%% Re-export plugin behavior
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    erlmcp_plugin:behaviour_info(callbacks) ++
    [
        {export, 2},
        {get_config_schema, 0}
    ];
behaviour_info(_) ->
    undefined.
