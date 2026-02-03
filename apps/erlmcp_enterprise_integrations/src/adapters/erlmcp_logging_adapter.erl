%% @doc Enterprise Logging Adapter
%% Integrates with logging platforms (ELK Stack, Graylog, Sumo Logic)
-module(erlmcp_logging_adapter).

-behaviour(gen_server).

-export([start_link/0, send_log/3, query_logs/3, setup_index/2,
         create_pipeline/2, get_stats/1, rotate_logs/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type provider() :: elk | graylog | sumologic.
-type log_level() :: emergency | alert | critical | error | warning | notice | info | debug.
-type log_format() :: json | text | xml | syslog.
-type log_data() :: map().
-type query() :: map().

-record(state, {
    provider :: provider(),
    config :: map(),
    connection :: pid() | undefined,
    index :: binary(),
    pipelines :: map(),
    metrics :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Send a log entry to the logging platform
-spec send_log(binary(), log_level(), log_data()) -> {ok, binary()} | {error, term()}.
send_log(Index, LogLevel, LogData) ->
    gen_server:call(?MODULE, {send_log, Index, LogLevel, LogData}).

%% Query logs from the platform
-spec query_logs(binary(), query(), map()) -> {ok, [log_data()]} | {error, term()}.
query_logs(Index, Query, Context) ->
    gen_server:call(?MODULE, {query_logs, Index, Query, Context}).

%% Set up a new index
-spec setup_index(binary(), map()) -> {ok, binary()} | {error, term()}.
setup_index(IndexName, Config) ->
    gen_server:call(?MODULE, {setup_index, IndexName, Config}).

%% Create a log processing pipeline
-spec create_pipeline(binary(), map()) -> {ok, binary()} | {error, term()}.
create_pipeline(PipelineName, Config) ->
    gen_server:call(?MODULE, {create_pipeline, PipelineName, Config}).

%% Get logging platform statistics
-spec get_stats(binary()) -> {ok, map()} | {error, term()}.
get_stats(Index) ->
    gen_server:call(?MODULE, {get_stats, Index}).

%% Rotate logs based on retention policy
-spec rotate_logs(binary(), map()) -> {ok, binary()} | {error, term()}.
rotate_logs(Index, Policy) ->
    gen_server:call(?MODULE, {rotate_logs, Index, Policy}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize state with configuration
    Config = load_config(),
    Provider = maps:get(provider, Config, undefined),

    State = #state{
        provider = Provider,
        config = Config,
        connection = undefined,
        index = <<"logs">>,
        pipelines = #{},
        metrics = #{}
    },

    %% Initialize provider-specific connection
    case Provider of
        elk ->
            elk_connection:start(State);
        graylog ->
            graylog_connection:start(State);
        sumologic ->
            sumologic_connection:start(State);
        _ ->
            throw({invalid_provider, Provider})
    end,

    {ok, State}.

handle_call({send_log, Index, LogLevel, LogData}, _From, State) ->
    try
        %% Send log based on provider
        case State#state.provider of
            elk -> send_elk_log(Index, LogLevel, LogData, State);
            graylog -> send_graylog_log(Index, LogLevel, LogData, State);
            sumologic -> send_sumologic_log(Index, LogLevel, LogData, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to send log: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({query_logs, Index, Query, Context}, _From, State) ->
    try
        %% Query logs based on provider
        case State#state.provider of
            elk -> query_elk_logs(Index, Query, Context, State);
            graylog -> query_graylog_logs(Index, Query, Context, State);
            sumologic -> query_sumologic_logs(Index, Query, Context, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to query logs: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({setup_index, IndexName, Config}, _From, State) ->
    try
        %% Set up index based on provider
        case State#state.provider of
            elk -> setup_elk_index(IndexName, Config, State);
            graylog -> setup_graylog_index(IndexName, Config, State);
            sumologic -> setup_sumologic_index(IndexName, Config, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to set up index: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({create_pipeline, PipelineName, Config}, _From, State) ->
    try
        %% Create pipeline based on provider
        case State#state.provider of
            elk -> create_elk_pipeline(PipelineName, Config, State);
            graylog -> create_graylog_pipeline(PipelineName, Config, State);
            sumologic -> create_sumologic_pipeline(PipelineName, Config, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to create pipeline: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({get_stats, Index}, _From, State) ->
    try
        %% Get statistics based on provider
        case State#state.provider of
            elk -> get_elk_stats(Index, State);
            graylog -> get_graylog_stats(Index, State);
            sumologic -> get_sumologic_stats(Index, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to get stats: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({rotate_logs, Index, Policy}, _From, State) ->
    try
        %% Rotate logs based on provider
        case State#state.provider of
            elk -> rotate_elk_logs(Index, Policy, State);
            graylog -> rotate_graylog_logs(Index, Policy, State);
            sumologic -> rotate_sumologic_logs(Index, Policy, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to rotate logs: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Clean up resources
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

load_config() ->
    %% Load logging provider configuration
    case application:get_env(erlmcp_enterprise_integrations, logging_config) of
        undefined -> default_logging_config();
        {ok, Config} -> Config
    end.

default_logging_config() ->
    #{
        provider => elk,
        endpoint => "http://localhost:9200",
        username => undefined,
        password => undefined,
        cluster_name => "elasticsearch",
        index_pattern => "erlmcp-*",
        timeout => 30000
    }.

%% ELK Stack Integration
send_elk_log(Index, LogLevel, LogData, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/" ++ binary_to_list(Index) ++ "/_doc",
    Headers = elk_headers(State),
    LogDocument = elk_log_document(LogLevel, LogData),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(LogDocument)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"_id">>, Response)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {log_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

query_elk_logs(Index, Query, Context, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/" ++
               binary_to_list(Index) ++ "/_search",
    Headers = elk_headers(State),
    SearchQuery = elk_search_query(Query, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(SearchQuery)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            Hits = proplists:get_value(<<"hits">>, Response, #{}),
            {ok, proplists:get_value(<<"hits">>, Hits, [])};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {query_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

setup_elk_index(IndexName, Config, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/" ++ binary_to_list(IndexName),
    Headers = elk_headers(State),
    IndexSettings = elk_index_settings(Config, State),

    case httpc:request(put, {Endpoint, Headers, "application/json", jsx:encode(IndexSettings)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"_id">>, Response)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {index_setup_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

create_elk_pipeline(PipelineName, Config, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++
               "/_ingest/pipeline/" ++ binary_to_list(PipelineName),
    Headers = elk_headers(State),
    PipelineDef = elk_pipeline_definition(Config, State),

    case httpc:request(put, {Endpoint, Headers, "application/json", jsx:encode(PipelineDef)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, PipelineName};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {pipeline_creation_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

get_elk_stats(Index, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/" ++
               binary_to_list(Index) ++ "/_stats",
    Headers = elk_headers(State),

    case httpc:request(get, {Endpoint, Headers},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(ResponseBody, [{labels, binary}]};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {stats_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

rotate_elk_logs(Index, Policy, State) ->
    %% Create rollover index
    RolloverEndpoint = maps:get(endpoint, State#state.config) ++
                       "/" ++ binary_to_list(Index) ++ "/_rollover",
    Headers = elk_headers(State),
    RolloverBody = elk_rollover_config(Policy, State),

    case httpc:request(post, {RolloverEndpoint, Headers, "application/json", jsx:encode(RolloverBody)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"_new_index">>, Response)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {rotation_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

%% Graylog Integration
send_graylog_log(Index, LogLevel, LogData, State) ->
    %% Graylog uses GELF protocol
    Endpoint = "http://localhost:12201", % Graylog GELF endpoint
    GraylogEvent = graylog_gelf_message(LogLevel, LogData, Index),

    case httpc:request(post, {Endpoint, [], "application/json", jsx:encode(GraylogEvent)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, <<"gelf_sent">>};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {log_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

query_graylog_logs(Index, Query, Context, State) ->
    GraylogEndpoint = maps:get(endpoint, State#state.config) ++ "/api/search/queries",
    Headers = graylog_headers(State),
    QueryConfig = graylog_query_config(Query, Context, Index),

    case httpc:request(post, {GraylogEndpoint, Headers, "application/json", jsx:encode(QueryConfig)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"messages">>, Response, [])};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {query_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

setup_graylog_index(IndexName, Config, State) ->
    GraylogEndpoint = maps:get(endpoint, State#state.config) ++
                     "/api/indexsets",
    Headers = graylog_headers(State),
    IndexSetConfig = graylog_indexset_config(IndexName, Config, State),

    case httpc:request(post, {GraylogEndpoint, Headers, "application/json", jsx:encode(IndexSetConfig)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"id">>, Response)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {index_setup_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

create_graylog_pipeline(PipelineName, Config, State) ->
    GraylogEndpoint = maps:get(endpoint, State#state.config) ++
                     "/api/pipelines",
    Headers = graylog_headers(State),
    PipelineConfig = graylog_pipeline_config(PipelineName, Config, State),

    case httpc:request(post, {GraylogEndpoint, Headers, "application/json", jsx:encode(PipelineConfig)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"id">>, Response)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {pipeline_creation_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

get_graylog_stats(Index, State) ->
    GraylogEndpoint = maps:get(endpoint, State#state.config) ++
                     "/api/system/metrics",
    Headers = graylog_headers(State),
    MetricQuery = #{<<"metrics">> => [<<"index_*">>]},

    case httpc:request(post, {GraylogEndpoint, Headers, "application/json", jsx:encode(MetricQuery)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(ResponseBody, [{labels, binary}]};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {stats_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

rotate_graylog_logs(Index, Policy, State) ->
    %% Graylog uses index sets for retention policies
    GraylogEndpoint = maps:get(endpoint, State#state.config) ++
                     "/api/indexsets/" ++ binary_to_list(Index) ++
                     "/rotate",
    Headers = graylog_headers(State),

    case httpc:request(post, {GraylogEndpoint, Headers, "application/json", <<>>},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, <<"rotated">>};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {rotation_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

%% Sumo Logic Integration
send_sumologic_log(Index, LogLevel, LogData, State) ->
    Endpoint = "https://api.sumologic.com/api/v1/collectors/" ++
               maps:get(collector_id, State#state.config) ++ "/logs",
    Headers = sumologic_headers(State),
    SumoEvent = sumologic_log_event(LogLevel, LogData, Index),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(SumoEvent)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, <<"logs_sent">>};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {log_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

query_sumologic_logs(Index, Query, Context, State) ->
    Endpoint = "https://api.sumologic.com/api/v1/logs/search",
    Headers = sumologic_headers(State),
    SearchQuery = sumologic_search_query(Query, Context, Index),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(SearchQuery)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"messages">>, Response, [])};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {query_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

setup_sumologic_index(IndexName, Config, State) ->
    SumoEndpoint = "https://api.sumologic.com/api/v1/indexes",
    Headers = sumologic_headers(State),
    IndexConfig = sumologic_index_config(IndexName, Config, State),

    case httpc:request(post, {SumoEndpoint, Headers, "application/json", jsx:encode(IndexConfig)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"id">>, Response)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {index_setup_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

create_sumologic_pipeline(PipelineName, Config, State) ->
    %% Sumo Logic doesn't have pipelines like ELK
    %% Use source category and parsing rules instead
    SumoEndpoint = "https://api.sumologic.com/api/v1/collectors",
    Headers = sumologic_headers(State),
    SourceConfig = sumologic_source_config(PipelineName, Config, State),

    case httpc:request(post, {SumoEndpoint, Headers, "application/json", jsx:encode(SourceConfig)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"id">>, Response)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {source_creation_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

get_sumologic_stats(Index, State) ->
    SumoEndpoint = "https://api.sumologic.com/api/v1/indexes/" ++
                   binary_to_list(Index) ++ "/stats",
    Headers = sumologic_headers(State),
    TimeRange = proplists:get_value(time_range, State#state.config, 3600),

    case httpc:request(get, {SumoEndpoint, Headers ++ [{"time_range", TimeRange}]},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(ResponseBody, [{labels, binary}]};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {stats_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

rotate_sumologic_logs(Index, Policy, State) ->
    SumoEndpoint = "https://api.sumologic.com/api/v1/indexes/" ++
                   binary_to_list(Index) ++ "/rotate",
    Headers = sumologic_headers(State),

    case httpc:request(post, {SumoEndpoint, Headers, "application/json", <<>>},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, <<"rotated">>};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {rotation_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

%% Helper Functions
elk_headers(State) =>
    case maps:get(username, State#state.config) of
        undefined ->
            #{<<"Content-Type">> => <<"application/json">>};
        Username ->
            Auth = base64:encode(<<Username/binary, ":", (maps:get(password, State#state.config))/binary>>),
            #{<<"Content-Type">> => <<"application/json">>,
              <<"Authorization">> => <<"Basic ", Auth/binary>>}
    end.

elk_log_document(LogLevel, LogData) =>
    #{
        timestamp => erlang:system_time(millisecond),
        level => LogLevel,
        host => list_to_binary(atom_to_list(node())),
        message => jsx:encode(LogData),
        metadata => #{
            module => ?MODULE,
            application => erlmcp
        }
    }.

elk_search_query(Query, Context) =>
    #{
        query => proplists:get_value(query, Query, "*"),
        filter => proplists:get_value(filter, Query, []),
        sort => [{<<"timestamp">>, <<"desc">>}],
        from => proplists:get_value(from, Query, 0),
        size => proplists:get_value(size, Query, 100),
        aggs => proplists:get_value(aggregations, Query, #{}),
        post_filter => proplists:get_value(post_filter, Query, #{}),
        highlight => proplists:get_value(highlight, Query, #{}),
        explain => proplists:get_value(explain, Context, false)
    }.

elk_index_settings(Config, State) =>
    #{
        settings => #{
            number_of_shards => proplists:get_value(shards, Config, 5),
            number_of_replicas => proplists:get_value(replicas, Config, 1),
            refresh_interval => proplists:get_value(refresh_interval, Config, <<"1s">>),
            analysis => proplists:get_value(analysis, Config, #{
                analyzer => #{
                    default => #{
                        tokenizer => standard,
                        filter => [lowercase, stop]
                    }
                }
            })
        },
        mappings => #{
            dynamic => proplists:get_value(dynamic, Config, true),
            properties => proplists:get_value(properties, Config, #{
                timestamp => #{
                    type => date
                },
                level => #{
                    type => keyword
                },
                message => #{
                    type => text,
                    analyzer => standard
                },
                host => #{
                    type => keyword
                }
            })
        }
    }.

elk_pipeline_definition(Config, State) =>
    #{
        description => proplists:get_value(description, Config, "erlmcp logging pipeline"),
        processors => proplists:get_value(processors, Config, [
            #{
                set => #{
                    field => "timestamp",
                    value => proplists:get_value(timestamp_field, Config, "@timestamp")
                }
            },
            #{
                set => #{
                    field => "node",
                    value => list_to_binary(atom_to_list(node()))
                }
            }
        ]),
        on_failure => proplists:get_value(on_failure, Config, #{
            process => [
                #{
                    set => #{
                        field => "pipeline_failure",
                        value => true
                    }
                }
            ]
        })
    }.

elk_rollover_config(Policy, State) =>
    #{
        conditions => proplists:get_value(conditions, Policy, [
            #{
                max_age => proplists:get_value(max_age, Policy, <<"7d">>)
            },
            #{
                max_docs => proplists:get_value(max_docs, Policy, 5000000)
            }
        ]),
        settings => #{
            index => #{
                number_of_shards => proplists:get_value(shards, Policy, 5),
                number_of_replicas => proplists:get_value(replicas, Policy, 1)
            }
        }
    }.

graylog_headers(State) =>
    case maps:get(username, State#state.config) of
        undefined ->
            #{<<"Content-Type">> => <<"application/json">>};
        Username ->
            Auth = base64:encode(<<Username/binary, ":", (maps:get(password, State#state.config))/binary>>),
            #{<<"Content-Type">> => <<"application/json">>,
              <<"Authorization">> => <<"Basic ", Auth/binary>>}
    end.

graylog_gelf_message(LogLevel, LogData, Index) =>
    #{
        version => "1.1",
        host => list_to_binary(atom_to_list(node())),
        timestamp => erlang:system_time(second),
        level => graylog_log_level(LogLevel),
        message => jsx:encode(LogData),
        facility => "erlmcp",
        file => "erlmcp_logging_adapter.erl",
        full_message => jsx:encode(LogData),
        index => Index
    }.

graylog_log_level(emergency) -> 0;
graylog_log_level(alert) -> 1;
graylog_log_level(critical) -> 2;
graylog_log_level(error) -> 3;
graylog_log_level(warning) -> 4;
graylog_log_level(notice) -> 5;
graylog_log_level(info) -> 6;
graylog_log_level(debug) -> 7.

graylog_query_config(Query, Context, Index) =>
    #{
        query => proplists:get_value(query, Query, "*"),
        filter => proplists:get_value(filter, Query, #{
            "index" => Index
        }),
        sort => [{"timestamp", "desc"}],
        limit => proplists:get_value(limit, Query, 100),
        offset => proplists:get_value(offset, Query, 0),
        fields => proplists:get_value(fields, Query, []),
        timerange => proplists:get_value(timerange, Context, {
            "type", "relative",
            "range", 300
        }),
        query_id => generate_uuid()
    }.

graylog_indexset_config(IndexName, Config, State) =>
    #{
        title => IndexName,
        index_prefix => binary_to_list(IndexName) ++ "-",
        rotation_strategy => proplists:get_value(rotation_strategy, Config, "time"),
        rotation_strategy_time => proplists:get_value(rotation_interval, Config, 86400),
        rotation_strategy_count => proplists:get_value(rotation_count, Config, 5),
        retention_strategy => proplists:get_value(retention_strategy, Config, "delete"),
        retention_strategy_count => proplists:get_value(retention_count, Config, 1000),
        retention_strategy_time => proplists:get_value(retention_time, Config, 604800)
    }.

graylog_pipeline_config(PipelineName, Config, State) =>
    #{
        title => PipelineName,
        description => proplists:get_value(description, Config, "erlmcp pipeline"),
        rules => proplists:get_value(rules, Config, []),
        execution_order => proplists:get_value(execution_order, Config, 0),
        match => proplists:get_value(match, Config, [])
    }.

sumologic_headers(State) =>
    #{
        <<"Authorization">> => <<"Bearer ", (maps:get(api_key, State#state.config))/binary>>,
        <<"Content-Type">> => <<"application/json">>,
        <<"Accept">> => <<"application/json">>
    }.

sumologic_log_event(LogLevel, LogData, Index) =>
    #{
        timestamp => erlang:system_time(millisecond),
        source => "erlmcp",
        source_category => binary_to_list(Index),
        message => jsx:encode(LogData),
        log_level => LogLevel,
        hostname => list_to_binary(atom_to_list(node()))
    }.

sumologic_search_query(Query, Context, Index) =>
    #{
        query => proplists:get_value(query, Query, "*"),
        fromTime => proplists:get_value(from, Context, erlang:system_time(millisecond) - 3600000),
        toTime => proplists:get_value(to, Context, erlang:system_time(millisecond)),
        timeZone => proplists:get_value(timezone, Context, "UTC"),
        limit => proplists:get_value(limit, Query, 100),
        offset => proplists:get_value(offset, Query, 0),
        fields => proplists:get_value(fields, Query, []),
        fieldSelectionEnabled => true
    }.

sumologic_index_config(IndexName, Config, State) =>
    #{
        name => IndexName,
        description => proplists:get_value(description, Config, "erlmcp index"),
        retentionDays => proplists:get_value(retention_days, Config, 30),
        hotTierDays => proplists:get_value(hot_tier_days, Config, 7),
        coldTierDays => proplists:get_value(cold_tier_days, Config, 23),
        enableDeduplication => proplists:get_value(deduplication, Config, false),
        deduplicationRules => proplists:get_value(deduplication_rules, Config, [])
    }.

sumologic_source_config(SourceName, Config, State) =>
    #{
        name => SourceName,
        description => proplists:get_value(description, Config, "erlmcp source"),
        category => binary_to_list(proplists:get_value(category, Config, "erlmcp/logs")),
        source => binary_to_list(proplists:get_value(source, Config, "erlmcp")),
        scanInterval => proplists:get_value(scan_interval, Config, 300),
        pathExpression => proplists:get_value(path_expression, Config, "*"),
        contentType => proplists:get_value(content_type, Config, "auto"),
        blacklistedPaths => proplists:get_value(blacklisted_paths, Config, []),
        encoding => proplists:get_value(encoding, Config, "UTF-8")
    }.

generate_uuid() ->
    binary_to_list(erlang:ref_to_list(make_ref())).