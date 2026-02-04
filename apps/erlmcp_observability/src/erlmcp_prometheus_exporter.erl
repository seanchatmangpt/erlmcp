%%%-------------------------------------------------------------------
%%% @doc
%%% Prometheus metrics exporter for erlmcp v3
%%% Implements Prometheus exposition format with proper metric types
%%% and support for remote write configuration
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_prometheus_exporter).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         register_collector/2,
         export_metrics/1,
         get_scrape_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 9090).
-define(DEFAULT_PATH, "/metrics").
-define(METRIC_CACHE, erlmcp_prometheus_cache).
-define(SCRAPE_CACHE, erlmcp_scrape_cache).
-define(MAX_CACHE_SIZE, 10000).

-include("erlmcp_observability.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start the Prometheus exporter with default configuration
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(#{
        port => ?DEFAULT_PORT,
        path => ?DEFAULT_PATH,
        max_cache_size => ?MAX_CACHE_SIZE,
        enable_metrics => true,
        enable_remote_write => false
    }).

%%--------------------------------------------------------------------
%% @doc
%% Start the Prometheus exporter with custom configuration
%% @end
%%--------------------------------------------------------------------
start_link(Config) when is_map(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%%--------------------------------------------------------------------
%% @doc
%% Register a new metric collector
%% @end
%%--------------------------------------------------------------------
register_collector(Name, Collector) when is_binary(Name) ->
    gen_server:call(?SERVER, {register_collector, Name, Collector}).

%%--------------------------------------------------------------------
%% @doc
%% Export metrics in Prometheus format
%% @end
%%--------------------------------------------------------------------
export_metrics(OutputFormat) ->
    gen_server:call(?SERVER, {export_metrics, OutputFormat}).

%%--------------------------------------------------------------------
%% @doc
%% Get scrape statistics
%% @end
%%--------------------------------------------------------------------
get_scrape_stats() ->
    gen_server:call(?SERVER, get_scrape_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Config) ->
    %% Initialize metric cache
    ets:new(?METRIC_CACHE, [set, public, named_table, {write_concurrency, true}]),
    ets:new(?SCRAPE_CACHE, [set, public, named_table, {write_concurrency, true}]),

    %% Start HTTP server
    Port = maps:get(port, Config, ?DEFAULT_PORT),
    Path = maps:get(path, Config, ?DEFAULT_PATH),

    %% Start cowboy server
    Dispatch = cowboy_router:compile([
        {'_', [
            {Path, prometheus_handler, []},
            {'_', not_found_handler, []}
        ]}
    ]),

    {ok, _Pid} = cowboy:start_clear(prometheus_listener,
        #{num_acceptors => 10},
        #{
            env => #{dispatch => Dispatch},
            protocol_options => #{idle_timeout => 30000}
        }
    ),

    %% Start remote write if enabled
    RemoteWriteConfig = maps:get(remote_write_config, Config, #{}),
    RemoteWritePid = case maps:get(enable_remote_write, Config, false) of
        true -> start_remote_writer(RemoteWriteConfig);
        false -> undefined
    end,

    %% Schedule periodic metric collection
    erlang:send_after(5000, self(), collect_metrics),

    State = #{
        port => Port,
        path => Path,
        max_cache_size => maps:get(max_cache_size, Config, ?MAX_CACHE_SIZE),
        enable_metrics => maps:get(enable_metrics, Config, true),
        collectors => maps:new(),
        remote_writer => RemoteWritePid,
        last_scrape => undefined,
        scrape_count => 0,
        error_count => 0,
        config => Config
    },

    {ok, State}.

handle_call({register_collector, Name, Collector}, _From, State) ->
    %% Register new collector
    Registered = maps:put(Name, Collector, State#{collectors => maps:put(Name, Collector, State#{collectors => maps:put(Name, Collector, State#{collectors => maps:new()})})}),
    {reply, ok, Registered};

handle_call({export_metrics, prometheus}, _From, State) ->
    Exported = export_prometheus_format(State),
    {reply, Exported, State};

handle_call({export_metrics, json}, _From, State) ->
    Exported = export_json_format(State),
    {reply, Exported, State};

handle_call(get_scrape_stats, _From, State) ->
    Stats = #{
        last_scrape => maps:get(last_scrape, State),
        scrape_count => maps:get(scrape_count, State),
        error_count => maps:get(error_count, State),
        collectors_count => map_size(maps:get(collectors, State)),
        cache_size => ets:info(?METRIC_CACHE, size)
    },
    {reply, Stats, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_metrics, State) ->
    collect_all_metrics(State),
    erlang:send_after(5000, self(), collect_metrics),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Stop HTTP server
    cowboy:stop(prometheus_listener),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Start remote write writer
start_remote_writer(Config) ->
    RemoteConfig = #{
        endpoint => maps:get(endpoint, Config),
        auth_token => maps:get(auth_token, Config),
        timeout => maps:get(timeout, Config, 10000),
        retry_count => maps:get(retry_count, Config, 3),
        batch_size => maps:get(batch_size, Config, 1000)
    },
    spawn_link(fun() -> remote_writer_loop(RemoteConfig, []) end).

remote_writer_loop(Config, Queue) ->
    receive
        {remote_write, Data} ->
            NewQueue = [Data | Queue],
            BatchSize = maps:get(batch_size, Config, 100),
            case length(NewQueue) >= BatchSize of
                true ->
                    send_batch(Config, lists:reverse(NewQueue)),
                    remote_writer_loop(Config, []);
                false ->
                    remote_writer_loop(Config, NewQueue)
            end
    after 60000 ->
        %% Flush queue periodically
        case Queue of
            [] -> ok;
            _ -> send_batch(Config, lists:reverse(Queue))
        end,
        remote_writer_loop(Config, [])
    end.

send_batch(Config, Data) ->
    case send_to_prometheus(Config, Data) of
        ok -> ok;
        Error -> io:format("Remote write error: ~p~n", [Error])
    end.

send_to_prometheus(Config, Data) ->
    %% Implementation for sending to Prometheus remote write
    %% This would use HTTP POST to the remote write endpoint
    %% For now, just log the data
    io:format("Sending batch to remote write: ~p~n", [length(Data)]),
    ok.

%% Collect all metrics
collect_all_metrics(State) ->
    case maps:get(enable_metrics, State) of
        true ->
            Collectors = maps:keys(maps:get(collectors, State)),
            lists:foreach(fun(Collector) ->
                collect_metric_collector(Collector, State)
            end, Collectors);
        false ->
            ok
    end.

collect_metric_collector(Collector, State) ->
    try
        Metrics = call_collector(Collector, State),
        cache_metrics(Collector, Metrics, State),
        update_scrape_stats(State, ok)
    catch
        Error:Reason ->
            io:format("Collector error: ~p:~p~n", [Error, Reason]),
            update_scrape_stats(State, {error, Error, Reason})
    end.

call_collector(Collector, State) ->
    case maps:get(Collector, State#{collectors => maps:get(collectors, State)}) of
        F when is_function(F, 0) ->
            F();
        {M, F} when is_atom(M), is_atom(F) ->
            M:F();
        {M, F, Args} when is_atom(M), is_atom(F), is_list(Args) ->
            apply(M, F, Args)
    end.

cache_metrics(Collector, Metrics, State) ->
    %% Clean cache if needed
    CacheSize = ets:info(?METRIC_CACHE, size),
    MaxSize = maps:get(max_cache_size, State, 10000),
    if
        CacheSize > MaxSize ->
            %% Remove oldest entries
            remove_oldest_metrics(MaxSize div 2);
        true ->
            ok
    end,

    %% Store metrics
    Now = erlang:system_time(millisecond),
    ets:insert(?METRIC_CACHE, {Collector, Now, Metrics}).

remove_oldest_metrics(TargetSize) ->
    %% Get oldest metrics and remove them
    Oldest = get_oldest_metrics(TargetSize),
    lists:foreach(fun({Key, _, _}) ->
        ets:delete(?METRIC_CACHE, Key)
    end, Oldest).

get_oldest_metrics(N) ->
    ets:foldl(fun({Key, Time, Metrics}, Acc) ->
        if
            length(Acc) < N ->
                [{Key, Time, Metrics} | Acc];
            true ->
                Acc
        end
    end, [], ?METRIC_CACHE).

export_prometheus_format(State) ->
    %% Generate Prometheus format output
    Header = [
        "# HELP erlmcp_info Information about erlmcp installation\n",
        "# TYPE erlmcp_info gauge\n",
        case ?ERLMCP_VERSION of
        Version when is_list(Version) ->
            "erlmcp_info{version=\"" ++ Version ++ "\"} 1\n";
        _ ->
            "erlmcp_info{version=\"unknown\"} 1\n"
    end
    ],

    MetricLines = ets:foldl(fun({_, _, Metrics}, Acc) ->
        case format_metrics(Metrics) of
            {help_line, Help} -> [Help | Acc];
            {type_line, Type} -> [Type | Acc];
            {metric_lines, Lines} -> Lines ++ Acc
        end
    end, [], ?METRIC_CACHE),

    Header ++ MetricLines.

format_metrics(Metrics) ->
    %% Format metrics according to Prometheus spec
    case Metrics of
        #{type := Type, name := Name, help := Help, values := Values} ->
            HelpLine = "# HELP " ++ binary_to_list(Name) ++ " " ++ binary_to_list(Help) ++ "\n",
            TypeLine = "# TYPE " ++ binary_to_list(Name) ++ " " ++ atom_to_list(Type) ++ "\n",
            MetricLines = format_metric_values(Name, Type, Values),
            {metric_lines, [HelpLine, TypeLine | MetricLines]};
        _ -> {metric_lines, []}
    end.

format_metric_values(Name, Type, Values) ->
    lists:map(fun({Tags, Value}) ->
        LabelStr = format_metric_labels(Tags),
        case Type of
            counter ->
                io_lib:format("~s~s ~.2f\n", [binary_to_list(Name), LabelStr, Value]);
            gauge ->
                io_lib:format("~s~s ~.2f\n", [binary_to_list(Name), LabelStr, Value]);
            histogram ->
                %% Format histogram buckets
                format_histogram_metrics(Name, Tags, Value)
        end
    end, Values).

format_metric_labels(Tags) ->
    LabelPairs = maps:fold(fun(Key, Value, Acc) ->
        case Key of
            <<"__name__">> -> Acc;
            _ -> Acc ++ [binary_to_list(Key), binary_to_list(Value)]
        end
    end, [], Tags),

    case LabelPairs of
        [] -> "";
        _ -> "{" ++ string:join(LabelPairs, ",") ++ "}"
    end.

format_histogram_metrics(Name, Tags, Value) ->
    %% Format histogram metrics
    case Value of
        #{buckets := Buckets, sum := Sum, count := Count} ->
            Lines = lists:map(fun({Bound, Count}) ->
                LabelStr = format_metric_labels(maps:put(<<"le">>, Bound, Tags)),
                io_lib:format("~s_bucket~s ~p\n", [binary_to_list(Name), LabelStr, Count])
            end, Buckets),
            CountLine = io_lib:format("~s_count~s ~p\n", [binary_to_list(Name), format_metric_labels(Tags), Count]),
            SumLine = io_lib:format("~s_sum~s ~.2f\n", [binary_to_list(Name), format_metric_labels(Tags), Sum]),
            Lines ++ [CountLine, SumLine]
    end.

export_json_format(State) ->
    %% Export as JSON for API endpoints
    Metrics = ets:foldl(fun({Collector, Timestamp, Data}, Acc) ->
        [#{collector => Collector,
           timestamp => Timestamp,
           metrics => Data} | Acc]
    end, [], ?METRIC_CACHE),

    jsx:encode(#{
        metrics => Metrics,
        timestamp => erlang:system_time(millisecond),
        metadata => #{version => ?ERLMCP_VERSION}
    }).

update_scrape_stats(State, Result) ->
    Now = erlang:system_time(millisecond),
    NewCount = maps:get(scrape_count, State) + 1,
    NewErrorCount = case Result of
        ok -> maps:get(error_count, State);
        _ -> maps:get(error_count, State) + 1
    end,

    %% Update scrape cache
    ets:insert(?SCRAPE_CACHE, {Now, Result}),

    State#{
        last_scrape => Now,
        scrape_count => NewCount,
        error_count => NewErrorCount
    }.

%%====================================================================
%% HTTP Handlers
%%====================================================================

%% Prometheus metrics handler
prometheus_handler(Req, State) ->
    try
        %% Get export format from request
        Format = case cowboy_req:header(<<"accept">>, Req) of
                    <<"application/json">> -> json;
                    _ -> prometheus
                end,

        %% Export metrics
        Exported = export_metrics(Format),

        %% Return response
        cowboy_req:reply(200,
            #{
                <<"content-type">> => case Format of
                    prometheus -> <<"text/plain; version=0.0.4">>;
                    json -> <<"application/json">>
                end,
                <<"cache-control">> => <<"no-cache">>
            },
            Exported,
            Req
        )
    catch
        Error:Reason ->
            cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{
                    error => atom_to_binary(Error),
                    reason => atom_to_binary(Reason)
                }),
                Req
            )
    end.

%% Not found handler
not_found_handler(Req, State) ->
    cowboy_req:reply(404,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{
            error => "not_found",
            path => cowboy_req:path(Req)
        }),
        Req
    ).