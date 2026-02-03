%%%-------------------------------------------------------------------
%%% @doc
%%% Synthetic Transaction Monitor for erlmcp
%%%
%%% Implements synthetic transaction monitoring to proactively detect
%%% issues by simulating real user workflows and monitoring their
%%% performance and success rates.
%%%
%%% Features:
%%% - Configurable synthetic transactions
%%% - Multi-endpoint monitoring
%%% - Performance threshold detection
%%% - Automated alerting
%%% - Historical tracking
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_synthetic_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         add_transaction/1, remove_transaction/1,
         run_all_transactions/0, run_transaction/1,
         get_transaction_results/0, get_transaction_history/1,
         set_thresholds/1, get_thresholds/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Records
-record(transaction_config, {
    name :: binary(),
    endpoint :: binary(),
    method :: get | post | put | delete,
    headers :: map(),
    body :: binary(),
    expected_status :: integer(),
    timeout :: pos_integer(),
    interval :: pos_integer(),
    enabled :: boolean(),
    tags :: [binary()]
}).

-record.transaction_result, {
    name :: binary(),
    timestamp :: integer(),
    duration :: integer(),
    status :: success | failure | timeout,
    http_status :: integer(),
    error :: binary(),
    metadata :: map()
}).

-record(state, {
    transactions :: #{binary() => #transaction_config{}},
    history :: queue:queue(#transaction_result{}),
    thresholds :: map(),
    timer_ref :: reference() | undefined,
    running :: boolean()
}).

-define(DEFAULT_INTERVAL, 30000).  % 30 seconds
-define(HISTORY_SIZE, 1000).
-define(DEFAULT_TIMEOUT, 5000).
-define(DEFAULT_EXPECTED_STATUS, 200).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the synthetic transaction monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start with configuration
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Add a synthetic transaction configuration
-spec add_transaction(map()) -> ok.
add_transaction(Config) ->
    gen_server:call(?MODULE, {add_transaction, Config}).

%% @doc Remove a synthetic transaction
-spec remove_transaction(binary()) -> ok.
remove_transaction(Name) ->
    gen_server:call(?MODULE, {remove_transaction, Name}).

%% @doc Run all enabled transactions once
-spec run_all_transactions() -> ok.
run_all_transactions() ->
    gen_server:cast(?MODULE, run_all_transactions).

%% @doc Run a specific transaction
-spec run_transaction(binary()) -> ok.
run_transaction(Name) ->
    gen_server:cast(?MODULE, {run_transaction, Name}).

%% @doc Get all transaction results
-spec get_transaction_results() -> [#transaction_result{}].
get_transaction_results() ->
    gen_server:call(?MODULE, get_transaction_results).

%% @doc Get transaction history for a specific transaction
-spec get_transaction_history(binary()) -> [#transaction_result{}].
get_transaction_history(Name) ->
    gen_server:call(?MODULE, {get_transaction_history, Name}).

%% @doc Set monitoring thresholds
-spec set_thresholds(map()) -> ok.
set_thresholds(Thresholds) ->
    gen_server:call(?MODULE, {set_thresholds, Thresholds}).

%% @doc Get current thresholds
-spec get_thresholds() -> map().
get_thresholds() ->
    gen_server:call(?MODULE, get_thresholds).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Config) ->
    process_flag(trap_exit, true),

    %% Initialize default thresholds
    DefaultThresholds = #{
        latency_p95 => 5000,  % 5 seconds
        latency_p99 => 10000, % 10 seconds
        error_rate => 0.05,   % 5%
        success_rate => 0.95, % 95%
        timeout_rate => 0.01  % 1%
    },

    %% Load transactions from config
    InitialTransactions = load_transactions(Config),
    Transactions = lists:foldl(fun(T, Acc) ->
        maps:put(T#transaction_config.name, T, Acc)
    end, #{}, InitialTransactions),

    %% Initialize state
    State = #state{
        transactions = Transactions,
        history = queue:new(),
        thresholds = maps:merge(DefaultThresholds, maps:get(thresholds, Config, #{})),
        timer_ref = undefined,
        running = maps:get(auto_run, Config, true)
    },

    %% Start monitoring if enabled
    if State#state.running ->
        {ok, start_monitoring(State)};
    true ->
        {ok, State}
    end.

handle_call({add_transaction, Config}, _From, State) ->
    %% Create transaction record
    Transaction = create_transaction_record(Config),
    Transactions = maps:put(Transaction#transaction_config.name, Transaction, State#state.transactions),

    ?LOG_INFO("Added synthetic transaction: ~s", [Transaction#transaction_config.name]),

    {reply, ok, State#state{transactions = Transactions}};

handle_call({remove_transaction, Name}, _From, State) ->
    Transactions = maps:remove(Name, State#state.transactions),

    ?LOG_INFO("Removed synthetic transaction: ~s", [Name]),

    {reply, ok, State#state{transactions = Transactions}};

handle_call(get_transaction_results, _From, State) ->
    %% Get all results from history
    Results = queue:to_list(State#state.history),
    {reply, Results, State};

handle_call({get_transaction_history, Name}, _From, State) ->
    %% Filter results for specific transaction
    Results = queue:to_list(State#state.history),
    Filtered = lists:filter(fun(R) -> R#transaction_result.name =:= Name end, Results),
    {reply, Filtered, State};

handle_call({set_thresholds, Thresholds}, _From, State) ->
    MergedThresholds = maps:merge(State#state.thresholds, Thresholds),
    {reply, ok, State#state{thresholds = MergedThresholds}};

handle_call(get_thresholds, _From, State) ->
    {reply, State#state.thresholds, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(run_all_transactions, State) ->
    %% Run all enabled transactions
    EnabledTransactions = [T || T <- maps:values(State#state.transactions),
                              T#transaction_config.enabled],
    lists:foreach(fun(Transaction) ->
        spawn_monitor(fun() -> execute_transaction(Transaction) end)
    end, EnabledTransactions),
    {noreply, State};

handle_cast({run_transaction, Name}, State) ->
    %% Run specific transaction
    case maps:get(Name, State#state.transactions, undefined) of
        undefined ->
            ?LOG_WARNING("Transaction not found: ~s", [Name]);
        Transaction ->
            spawn_monitor(fun() -> execute_transaction(Transaction) end)
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, run_transactions}, State) ->
    %% Run scheduled transactions
    NewState = run_scheduled_transactions(State),
    {noreply, NewState};

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    %% Handle process termination
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel timer if running
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Create transaction record from config
create_transaction_record(Config) ->
    #transaction_config{
        name = maps:get(name, Config),
        endpoint = maps:get(endpoint, Config),
        method = maps:get(method, Config, get),
        headers = maps:get(headers, Config, #{}),
        body = maps:get(body, Config, <<>>),
        expected_status = maps:get(expected_status, Config, ?DEFAULT_EXPECTED_STATUS),
        timeout = maps:get(timeout, Config, ?DEFAULT_TIMEOUT),
        interval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
        enabled = maps:get(enabled, Config, true),
        tags = maps:get(tags, Config, [])
    }.

%% @doc Load transactions from configuration
load_transactions(Config) ->
    %% This would typically load from config file
    case maps:get(transactions, Config, []) of
        Transactions when is_list(Transactions) ->
            [create_transaction_record(T) || T <- Transactions];
        _ ->
            []
    end.

%% @doc Start monitoring timer
start_monitoring(State) ->
    %% Schedule first run immediately
    Self = self(),
    erlang:send_after(0, Self, run_transactions),

    %% Schedule regular runs
    Interval = min([T#transaction_config.interval || T <- maps:values(State#state.transactions)]),
    TimerRef = erlang:send_after(Interval, Self, run_transactions),

    State#state{timer_ref = TimerRef, running = true}.

%% @doc Run scheduled transactions
run_scheduled_transactions(State) ->
    %% Find the next smallest interval
    Intervals = [T#transaction_config.interval || T <- maps:values(State#state.transactions)],
    NextInterval = lists:min(Intervals),

    %% Update timer
    TimerRef = erlang:send_after(NextInterval, self(), run_transactions),

    %% Run all enabled transactions
    EnabledTransactions = [T || T <- maps:values(State#state.transactions),
                              T#transaction_config.enabled],
    lists:foreach(fun(Transaction) ->
        spawn_monitor(fun() -> execute_transaction(Transaction) end)
    end, EnabledTransactions),

    State#state{timer_ref = TimerRef}.

%% @doc Execute a synthetic transaction
execute_transaction(Transaction) ->
    StartTime = erlang:monotonic_time(millisecond),

    try
        %% Build HTTP request
        Url = Transaction#transaction_config.endpoint,
        Method = Transaction#transaction_config.method,
        Headers = format_headers(Transaction#transaction_config.headers),
        Body = Transaction#transaction_config.body,
        Timeout = Transaction#transaction_config.timeout,

        %% Execute request
        Response = httpc:request(Method, {Url, Headers}, [{timeout, Timeout}], [], []),
        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        %% Process response
        Result = process_response(Response, Transaction, Duration),

        %% Store result
        gen_server:call(?MODULE, {store_result, Result})

    catch
        Class:Reason:Stacktrace ->
            EndTime = erlang:monotonic_time(millisecond),
            Duration = EndTime - StartTime,

            %% Create failure result
            Result = #transaction_result{
                name = Transaction#transaction_config.name,
                timestamp = StartTime,
                duration = Duration,
                status = failure,
                http_status = 0,
                error = format_error(Class, Reason),
                metadata = #{stacktrace => Stacktrace}
            },

            %% Store result
            gen_server:call(?MODULE, {store_result, Result})
    end.

%% @doc Process HTTP response
process_response({ok, {{Status, _}, Headers, Body}}, Transaction, Duration) ->
    %% Check if status matches expected
    ExpectedStatus = Transaction#transaction_config.expected_status,
    StatusMatch = Status =:= ExpectedStatus,

    ResultStatus =
        if StatusMatch -> success;
           true -> failure
        end,

    #transaction_result{
        name = Transaction#transaction_config.name,
        timestamp = erlang:system_time(millisecond),
        duration = Duration,
        status = ResultStatus,
        http_status = Status,
        error = if StatusMatch -> <<>>; true -> format_status_error(Status) end,
        metadata = #{headers => Headers, body => Body}
    };

process_response({error, Reason}, Transaction, Duration) ->
    Result = #transaction_result{
        name = Transaction#transaction_config.name,
        timestamp = erlang:system_time(millisecond),
        duration = Duration,
        status = failure,
        http_status = 0,
        error = format_error_info(Reason),
        metadata = #{}
    },

    %% Check for timeout
    Timeout = Transaction#transaction_config.timeout,
    if Duration >= Timeout ->
        Result#transaction_result{status = timeout};
       true ->
            Result
    end.

%% @doc Store transaction result
store_result(Result, State) ->
    %% Add to history queue
    NewHistory = queue:in(Result, State#state.history),

    %% Trim history to size limit
    TrimmedHistory = trim_history(NewHistory, ?HISTORY_SIZE),

    %% Check for threshold violations
    check_threshold_violations(Result),

    State#state{history = TrimmedHistory}.

%% @doc Trim history queue to size limit
trim_history(Queue, MaxSize) ->
    case queue:len(Queue) > MaxSize of
        true -> queue:drop(Queue);
        false -> Queue
    end.

%% @doc Check for threshold violations
check_threshold_violations(Result) ->
    %% This would typically send alerts
    ?LOG_DEBUG("Transaction result: ~p", [Result]),

    %% Check latency thresholds
    Thresholds = gen_server:call(?MODULE, get_thresholds),

    case Result#transaction_result.status of
        timeout ->
            TimeoutRate = calculate_timeout_rate(Result#transaction_result.name),
            if TimeoutRate > maps:get(timeout_rate, Thresholds) ->
                   send_alert(timeout_high, Result);
               true ->
                   ok
            end;
        failure ->
            ErrorRate = calculate_error_rate(Result#transaction_result.name),
            if ErrorRate > maps:get(error_rate, Thresholds) ->
                   send_alert(error_rate_high, Result);
               true ->
                   ok
            end;
        success ->
            Latency = Result#transaction_result.duration,
            if Latency > maps:get(latency_p99, Thresholds) ->
                   send_alert(latency_high, Result);
               true ->
                   ok
            end
    end.

%% @doc Calculate timeout rate for a transaction
calculate_timeout_rate(Name) ->
    Results = get_transaction_history(Name),
    Total = length(Results),
    Timeouts = lists:filter(fun(R) -> R#transaction_result.status =:= timeout end, Results),
    case Total of
        0 -> 0.0;
        _ -> length(Timeouts) / Total
    end.

%% @doc Calculate error rate for a transaction
calculate_error_rate(Name) ->
    Results = get_transaction_history(Name),
    Total = length(Results),
    Errors = lists:filter(fun(R) -> R#transaction_result.status =:= failure end, Results),
    case Total of
        0 -> 0.0;
        _ -> length(Errors) / Total
    end.

%% @doc Send alert for threshold violation
send_alert(AlertType, Result) ->
    %% This would integrate with alert manager
    Alert = #{
        type => AlertType,
        transaction => Result#transaction_result.name,
        severity => "warning",
        message => io_lib:format("Threshold violated for ~s", [Result#transaction_result.name]),
        timestamp => erlang:system_time(millisecond),
        result => Result
    },

    ?LOG_WARNING("Synthetic alert: ~p", [Alert]).

%% @doc Format HTTP headers
format_headers(Headers) when is_map(Headers) ->
    maps:to_list(Headers);
format_headers(Headers) ->
    Headers.

%% @doc format error message
format_error(Class, Reason) ->
    iolist_to_binary(io_lib:format("~p: ~p", [Class, Reason])).

%% @doc format error information
format_error_info(Reason) ->
    case Reason of
        timeout -> <<"Request timeout">>;
        {failed, _} -> <<"HTTP request failed">>;
        {options, invalid_option} -> <<"Invalid option">>;
        _ -> format_error(error, Reason)
    end.

%% @doc format status error
format_status_error(Status) ->
    case Status of
        400 -> <<"Bad Request">>;
        401 -> <<"Unauthorized">>;
        403 -> <<"Forbidden">>;
        404 -> <<"Not Found">>;
        429 -> <<"Too Many Requests">>;
        500 -> <<"Internal Server Error">>;
        502 -> <<"Bad Gateway">>;
        503 -> <<"Service Unavailable">>;
        504 -> <<"Gateway Timeout">>;
        _ -> io_lib:format("HTTP ~p", [Status])
    end.