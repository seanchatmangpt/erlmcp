-module(erlmcp_batch_processor).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([process_batch/1]).
-export([configure/2]).
-export([get_config/0]).
-export([get_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_MAX_BATCH_SIZE, 100).
-define(DEFAULT_BATCH_TIMEOUT, 1000).
-define(DEFAULT_MAX_CONCURRENCY, 10).

-record(config, {
    max_batch_size = ?DEFAULT_MAX_BATCH_SIZE,
    batch_timeout = ?DEFAULT_BATCH_TIMEOUT,
    max_concurrency = ?DEFAULT_MAX_CONCURRENCY
}).

-record(metrics, {
    batches_processed = 0,
    items_processed = 0,
    errors = 0,
    last_batch_time = 0
}).

-record(state, {
    config = #config{},
    metrics = #metrics{},
    pending = [],
    processing = 0
}).

-type batch_request() :: [map()].
-type batch_result() :: #{success => [map()], errors => [{map(), term()}]}.
-type config_key() :: max_batch_size | batch_timeout | max_concurrency.
-type config_value() :: pos_integer().

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec process_batch(batch_request()) -> {ok, batch_result()} | {error, term()}.
process_batch([]) ->
    {error, empty_batch};
process_batch(Batch) when is_list(Batch) ->
    gen_server:call(?SERVER, {process_batch, Batch}, infinity).

-spec configure(config_key(), config_value()) -> ok | {error, term()}.
configure(Key, Value) when is_atom(Key), is_integer(Value), Value > 0 ->
    gen_server:call(?SERVER, {configure, Key, Value}).

-spec get_config() -> #{max_batch_size => pos_integer(),
                        batch_timeout => pos_integer(),
                        max_concurrency => pos_integer()}.
get_config() ->
    gen_server:call(?SERVER, get_config).

-spec get_metrics() -> #{batches_processed => non_neg_integer(),
                         items_processed => non_neg_integer(),
                         errors => non_neg_integer(),
                         last_batch_time => non_neg_integer()}.
get_metrics() ->
    gen_server:call(?SERVER, get_metrics).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call({process_batch, Batch}, _From, State) ->
    #state{config = Config, metrics = Metrics0} = State,
    StartTime = erlang:monotonic_time(millisecond),
    TraceContext = erlmcp_observability:trace_span(<<"batch_processor_process">>, #{
        batch_size => length(Batch),
        max_size => Config#config.max_batch_size
    }),

    % Record batch processing start
    erlmcp_observability:log(<<"batch_processing_started">>, #{
        module => ?MODULE,
        batch_size => length(Batch),
        max_batch_size => Config#config.max_batch_size
    }, TraceContext),

    % Record request metrics
    erlmcp_observability:counter(<<"batch_processor_requests_total">>, #{
        service => ?MODULE
    }),
    erlmcp_observability:histogram_observe(<<"batch_processor_batch_size">>, length(Batch)),

    % Validate batch size
    MaxSize = Config#config.max_batch_size,
    case length(Batch) of
        N when N > MaxSize ->
            Duration = erlang:monotonic_time(millisecond) - StartTime,

            % Record error metrics
            erlmcp_observability:counter(<<"batch_processor_requests_total">>, #{
                service => ?MODULE,
                status => "error"
            }),
            erlmcp_observability:histogram_observe(<<"batch_processor_duration_ms">>, Duration),
            erlmcp_observability:counter(<<"batch_processor_errors_total">>, #{
                type => "validation",
                error => "batch_too_large"
            }),

            erlmcp_observability:log(<<"batch_processing_failed">>, #{
                error => batch_too_large,
                batch_size => N,
                max_size => MaxSize,
                duration => Duration
            }, TraceContext),
            erlmcp_observability:trace_span(<<"batch_result">>, #{
                result => error,
                reason => batch_too_large,
                duration => Duration
            }, TraceContext),
            {reply, {error, {batch_too_large, N, MaxSize}}, State};
        _ ->
            % Process the batch
            {Results, Metrics1} = process_batch_items(Batch, Metrics0),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = EndTime - StartTime,

            % Update metrics
            Metrics2 = Metrics1#metrics{
                batches_processed = Metrics1#metrics.batches_processed + 1,
                last_batch_time = EndTime - StartTime,
                items_processed = Metrics1#metrics.items_processed + length(Batch)
            },

            % Record metrics
            erlmcp_observability:counter(<<"batch_processor_batches_total">>, #{
                batch_size => length(Batch)
            }),
            erlmcp_observability:histogram_observe(<<"batch_processor_duration_ms">>, Duration),
            erlmcp_observability:histogram_observe(<<"batch_processor_items_per_batch">>, length(Batch)),

            % Update active connections gauge
            erlmcp_observability:gauge_set(<<"batch_processor_active_connections">>, #{
                service => ?MODULE
            }, 1),

            % Log completion
            SuccessCount = length(maps:get(success, Results, [])),
            ErrorCount = length(maps:get(errors, Results, [])),

            % Record success/failure metrics
            case ErrorCount of
                0 ->
                    erlmcp_observability:counter(<<"batch_processor_requests_total">>, #{
                        service => ?MODULE,
                        status => "success"
                    });
                _ ->
                    erlmcp_observability:counter(<<"batch_processor_requests_total">>, #{
                        service => ?MODULE,
                        status => "partial_success"
                    })
            end,

            erlmcp_observability:log(<<"batch_processing_completed">>, #{
                batch_size => length(Batch),
                success_count => SuccessCount,
                error_count => ErrorCount,
                duration => Duration
            }, TraceContext),
            erlmcp_observability:trace_span(<<"batch_result">>, #{
                result => success,
                success_count => SuccessCount,
                error_count => ErrorCount,
                duration => Duration
            }, TraceContext),

            {reply, {ok, Results}, State#state{metrics = Metrics2}}
    end;

handle_call({configure, Key, Value}, _From, State) ->
    #state{config = Config} = State,
    try
        NewConfig = update_config(Config, Key, Value),
        {reply, ok, State#state{config = NewConfig}}
    catch
        _:Error -> {reply, {error, Error}, State}
    end;

handle_call(get_config, _From, State) ->
    #state{config = Config} = State,
    ConfigMap = #{
        max_batch_size => Config#config.max_batch_size,
        batch_timeout => Config#config.batch_timeout,
        max_concurrency => Config#config.max_concurrency
    },
    {reply, ConfigMap, State};

handle_call(get_metrics, _From, State) ->
    #state{metrics = Metrics} = State,
    MetricsMap = #{
        batches_processed => Metrics#metrics.batches_processed,
        items_processed => Metrics#metrics.items_processed,
        errors => Metrics#metrics.errors,
        last_batch_time => Metrics#metrics.last_batch_time
    },
    {reply, MetricsMap, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

process_batch_items(Batch, Metrics) ->
    Successes = [],
    Errors = [],

    lists:foldl(
        fun(Item, {Succ, Err, Met}) ->
            case process_item(Item) of
                {ok, Result} ->
                    {[Result | Succ], Err, Met#metrics{items_processed = Met#metrics.items_processed + 1}};
                {error, Reason} ->
                    {Succ, [{Item, Reason} | Err], Met#metrics{errors = Met#metrics.errors + 1}}
            end
        end,
        {Successes, Errors, Metrics},
        Batch
    ),

    Result = #{
        success => lists:reverse(Successes),
        errors => lists:reverse(Errors)
    },
    {Result, Metrics}.

process_item(Item) when is_map(Item) ->
    % Simulate item processing (e.g., tool invocation, resource fetch)
    try
        Result = #{
            jsonrpc => <<"2.0">>,
            id => maps:get(id, Item, null),
            result => #{processed => true}
        },
        {ok, Result}
    catch
        _:Error -> {error, {process_failed, Error}}
    end;
process_item(_Item) ->
    {error, invalid_item}.

update_config(Config, max_batch_size, Value) when Value > 0, Value =< 1000 ->
    Config#config{max_batch_size = Value};
update_config(Config, batch_timeout, Value) when Value > 0, Value =< 60000 ->
    Config#config{batch_timeout = Value};
update_config(Config, max_concurrency, Value) when Value > 0, Value =< 100 ->
    Config#config{max_concurrency = Value};
update_config(_, Key, _) ->
    error({invalid_config, Key}).
