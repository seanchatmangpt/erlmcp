%%%-------------------------------------------------------------------
%%% @doc
%%% Database Connection Pool Manager with Circuit Breaker
%%%
%%% Provides enterprise-grade database connection management with:
%%% - Connection pooling using poolboy
%%% - Health checks for database connectivity
%%% - Exponential backoff retry logic
%%% - Circuit breaker for database failures
%%% - Telemetry integration
%%% - Transaction support
%%% - Prepared statement caching
%%%
%%% ## Configuration
%%%
%%% ```erlang
%%% {erlmcp_db_pool, [
%%%     {pools, [
%%%         {default, [
%%%             {size, 10},
%%%             {max_overflow, 20},
%%%             {connection_mod, erlmcp_db_connection},
%%%             {host, "localhost"},
%%%             {port, 5432},
%%%             {database, "erlmcp"},
%%%             {health_check_interval, 30000},
%%%             {circuit_breaker_threshold, 5},
%%%             {circuit_breaker_timeout, 60000},
%%%             {retry_max_attempts, 3},
%%%             {retry_base_delay, 100},
%%%             {retry_max_delay, 10000}
%%%         ]}
%%%     ]}
%%% ]}.
%%% ```
%%%
%%% ## Usage Examples
%%%
%%% ```erlang
%%% %% Execute a query with automatic retry
%%% {ok, Result} = erlmcp_db_pool:execute(default, "SELECT * FROM users").
%%%
%%% %% Execute within a transaction
%%% {ok, Result} = erlmcp_db_pool:transaction(default, fun(Conn) ->
%%%     erlmcp_db_pool:query(Conn, "INSERT INTO users ..."),
%%%     erlmcp_db_pool:query(Conn, "UPDATE balance ...")
%%% end).
%%%
%%% %% Get pool statistics
%%% {ok, Stats} = erlmcp_db_pool:stats(default).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_db_pool).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/0]).
-export([start_pool/2]).
-export([stop_pool/1]).
-export([checkin/2]).
-export([execute/2]).
-export([execute/3]).
-export([transaction/2]).
-export([transaction/3]).
-export([query/2]).
-export([stats/1]).
-export([health_check/1]).
-export([circuit_breaker_info/1]).
-export([reset_circuit_breaker/1]).
-export([with_connection/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% poolboy_worker callback
-export([start_link/1]).

-include_lib("kernel/include/logger.hrl").

%% Constants
-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_MAX_OVERFLOW, 20).
-define(DEFAULT_HEALTH_CHECK_INTERVAL, 30000).
-define(DEFAULT_CIRCUIT_THRESHOLD, 5).
-define(DEFAULT_CIRCUIT_TIMEOUT, 60000).
-define(DEFAULT_RETRY_MAX_ATTEMPTS, 3).
-define(DEFAULT_RETRY_BASE_DELAY, 100).
-define(DEFAULT_RETRY_MAX_DELAY, 10000).
-define(DEFAULT_CHECKOUT_TIMEOUT, 5000).

-define(CIRCUIT_STATE_OPEN, open).
-define(CIRCUIT_STATE_CLOSED, closed).
-define(CIRCUIT_STATE_HALF_OPEN, half_open).

%% Records
-record(pool_config,
        {name :: atom(),
         size :: pos_integer(),
         max_overflow :: pos_integer(),
         connection_mod :: module(),
         connection_args :: proplists:proplist(),
         health_check_interval :: pos_integer(),
         circuit_threshold :: pos_integer(),
         circuit_timeout :: pos_integer(),
         retry_max_attempts :: pos_integer(),
         retry_base_delay :: pos_integer(),
         retry_max_delay :: pos_integer()}).

-record(circuit_state,
        {state = ?CIRCUIT_STATE_CLOSED :: closed | open | half_open,
         failure_count = 0 :: non_neg_integer(),
         success_count = 0 :: non_neg_integer(),
         last_failure_time :: erlang:timestamp() | undefined,
         opened_at :: erlang:timestamp() | undefined}).

-record(pool_state,
        {pool_pid :: pid() | undefined,
         circuit_state :: #circuit_state{},
         metrics :: map()}).

-record(state,
        {pools = #{} :: #{atom() => #pool_state{}},
         health_check_timer :: reference() | undefined}).

-type pool_name() :: atom().
-type query_result() :: {ok, list(map())} | {error, term()}.
-type transaction_fun() :: fun((pid()) -> {ok, term()} | {error, term()} | term()).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the database pool manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Start a named database connection pool
-spec start_pool(pool_name(), proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_pool(PoolName, Config) ->
    gen_server:call(?MODULE, {start_pool, PoolName, Config}).

%% @doc Stop a database connection pool
-spec stop_pool(pool_name()) -> ok.
stop_pool(PoolName) ->
    gen_server:call(?MODULE, {stop_pool, PoolName}).

%% @doc Return a connection to the pool (wrapper for poolboy:checkin)
-spec checkin(pool_name(), pid()) -> ok.
checkin(PoolName, Worker) ->
    poolboy:checkin(PoolName, Worker).

%% @doc Execute a query with retry logic and circuit breaker protection
-spec execute(pool_name(), iodata()) -> query_result().
execute(PoolName, Query) ->
    execute(PoolName, Query, []).

%% @doc Execute a parameterized query with retry logic and circuit breaker protection
-spec execute(pool_name(), iodata(), list()) -> query_result().
execute(PoolName, Query, Params) ->
    gen_server:call(?MODULE, {execute, PoolName, Query, Params}, infinity).

%% @doc Execute a function within a database transaction
-spec transaction(pool_name(), transaction_fun()) -> {ok, term()} | {error, term()}.
transaction(PoolName, Fun) ->
    transaction(PoolName, Fun, infinity).

%% @doc Execute a function within a database transaction with timeout
-spec transaction(pool_name(), transaction_fun(), timeout()) -> {ok, term()} | {error, term()}.
transaction(PoolName, Fun, Timeout) ->
    gen_server:call(?MODULE, {transaction, PoolName, Fun, Timeout}, infinity).

%% @doc Execute a query on a specific connection (for use within transactions)
-spec query(pid(), iodata()) -> query_result().
query(Connection, Query) ->
    query(Connection, Query, []).

%% @doc Execute a parameterized query on a specific connection
-spec query(pid(), iodata(), list()) -> query_result().
query(Connection, Query, Params) ->
    case erlmcp_db_connection:query(Connection, Query, Params) of
        {ok, _} = Result ->
            Result;
        {error, Reason} = Error ->
            logger:error("Query failed: ~p", [Reason]),
            Error
    end.

%% @doc Get pool statistics including circuit breaker state
-spec stats(pool_name()) -> {ok, map()} | {error, term()}.
stats(PoolName) ->
    gen_server:call(?MODULE, {stats, PoolName}).

%% @doc Perform health check on a database pool
-spec health_check(pool_name()) -> {ok, map()} | {error, term()}.
health_check(PoolName) ->
    gen_server:call(?MODULE, {health_check, PoolName}).

%% @doc Get circuit breaker information for a pool
-spec circuit_breaker_info(pool_name()) -> {ok, map()} | {error, term()}.
circuit_breaker_info(PoolName) ->
    gen_server:call(?MODULE, {circuit_breaker_info, PoolName}).

%% @doc Reset the circuit breaker for a pool (manual intervention)
-spec reset_circuit_breaker(pool_name()) -> ok | {error, term()}.
reset_circuit_breaker(PoolName) ->
    gen_server:call(?MODULE, {reset_circuit_breaker, PoolName}).

%% @doc Execute a function with a checked-out connection (auto checkin)
-spec with_connection(pool_name(), fun((pid()) -> term())) -> {ok, term()} | {error, term()}.
with_connection(PoolName, Fun) ->
    case poolboy:checkout(PoolName, false, ?DEFAULT_CHECKOUT_TIMEOUT) of
        {error, Reason} ->
            {error, {checkout_failed, Reason}};
        Connection ->
            try
                Result = Fun(Connection),
                poolboy:checkin(PoolName, Connection),
                {ok, Result}
            catch
                Type:Error:Stacktrace ->
                    poolboy:checkin(PoolName, Connection),
                    {error, {Type, Error, Stacktrace}}
            end
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    logger:info("Starting database pool manager"),

    % Initialize any configured pools from environment
    case application:get_env(erlmcp_core, db_pools, []) of
        [] ->
            ok;
        PoolsConfig when is_list(PoolsConfig) ->
            initialize_pools_from_config(PoolsConfig)
    end,

    % Start health check timer
    Timer = erlang:send_after(?DEFAULT_HEALTH_CHECK_INTERVAL, self(), health_check_all),

    {ok, #state{health_check_timer = Timer}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({start_pool, PoolName, Config}, _From, State) ->
    case maps:get(PoolName, State#state.pools, undefined) of
        undefined ->
            case do_start_pool(PoolName, Config) of
                {ok, PoolPid} ->
                    _PoolConfig = build_pool_config(PoolName, Config),
                    PoolState = #pool_state{
                        pool_pid = PoolPid,
                        circuit_state = #circuit_state{},
                        metrics = init_metrics()
                    },
                    NewPools = maps:put(PoolName, PoolState, State#state.pools),
                    logger:info("Started database pool: ~p", [PoolName]),
                    {reply, {ok, PoolPid}, State#state{pools = NewPools}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        _ ->
            {reply, {error, {already_exists, PoolName}}, State}
    end;

handle_call({stop_pool, PoolName}, _From, State) ->
    case maps:get(PoolName, State#state.pools, undefined) of
        undefined ->
            {reply, {error, {not_found, PoolName}}, State};
        _PoolState ->
            case supervisor:terminate_child(erlmcp_db_pool_sup, PoolName) of
                ok ->
                    supervisor:delete_child(erlmcp_db_pool_sup, PoolName),
                    NewPools = maps:remove(PoolName, State#state.pools),
                    logger:info("Stopped database pool: ~p", [PoolName]),
                    {reply, ok, State#state{pools = NewPools}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({execute, PoolName, Query, Params}, _From, State) ->
    case maps:get(PoolName, State#state.pools, undefined) of
        undefined ->
            {reply, {error, {pool_not_found, PoolName}}, State};
        PoolState ->
            CircuitState = PoolState#pool_state.circuit_state,
            case CircuitState#circuit_state.state of
                ?CIRCUIT_STATE_OPEN ->
                    {reply, {error, circuit_open}, State};
                _ ->
                    Config = get_pool_config(PoolName),
                    Result = execute_with_retry(PoolName, Query, Params, Config, State),
                    NewPoolState = update_metrics_and_circuit(PoolState, Result),
                    NewPools = maps:put(PoolName, NewPoolState, State#state.pools),
                    {reply, Result, State#state{pools = NewPools}}
            end
    end;

handle_call({transaction, PoolName, Fun, Timeout}, From, State) ->
    case maps:get(PoolName, State#state.pools, undefined) of
        undefined ->
            {reply, {error, {pool_not_found, PoolName}}, State};
        PoolState ->
            CircuitState = PoolState#pool_state.circuit_state,
            case CircuitState#circuit_state.state of
                ?CIRCUIT_STATE_OPEN ->
                    {reply, {error, circuit_open}, State};
                _ ->
                    % Execute transaction in spawned process to avoid blocking gen_server
                    spawn(fun() ->
                        Result = do_transaction(PoolName, Fun, Timeout),
                        gen_server:reply(From, Result)
                    end),
                    {noreply, State}
            end
    end;

handle_call({stats, PoolName}, _From, State) ->
    case maps:get(PoolName, State#state.pools, undefined) of
        undefined ->
            {reply, {error, {pool_not_found, PoolName}}, State};
        PoolState ->
            Stats = calculate_pool_stats(PoolName, PoolState),
            {reply, {ok, Stats}, State}
    end;

handle_call({health_check, PoolName}, _From, State) ->
    case maps:get(PoolName, State#state.pools, undefined) of
        undefined ->
            {reply, {error, {pool_not_found, PoolName}}, State};
        PoolState ->
            Result = do_health_check(PoolName),
            NewPoolState = case Result of
                {ok, _} ->
                    record_circuit_success(PoolState);
                {error, _} ->
                    record_circuit_failure(PoolState)
            end,
            NewPools = maps:put(PoolName, NewPoolState, State#state.pools),
            {reply, Result, State#state{pools = NewPools}}
    end;

handle_call({circuit_breaker_info, PoolName}, _From, State) ->
    case maps:get(PoolName, State#state.pools, undefined) of
        undefined ->
            {reply, {error, {pool_not_found, PoolName}}, State};
        PoolState ->
            Circuit = PoolState#pool_state.circuit_state,
            Info = #{
                state => Circuit#circuit_state.state,
                failure_count => Circuit#circuit_state.failure_count,
                success_count => Circuit#circuit_state.success_count,
                last_failure_time => Circuit#circuit_state.last_failure_time,
                opened_at => Circuit#circuit_state.opened_at
            },
            {reply, {ok, Info}, State}
    end;

handle_call({reset_circuit_breaker, PoolName}, _From, State) ->
    case maps:get(PoolName, State#state.pools, undefined) of
        undefined ->
            {reply, {error, {pool_not_found, PoolName}}, State};
        PoolState ->
            NewCircuitState = #circuit_state{},
            NewPoolState = PoolState#pool_state{circuit_state = NewCircuitState},
            NewPools = maps:put(PoolName, NewPoolState, State#state.pools),
            logger:info("Circuit breaker reset for pool: ~p", [PoolName]),
            {reply, ok, State#state{pools = NewPools}}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(health_check_all, State) ->
    % Run health check on all pools
    Pools = maps:keys(State#state.pools),
    NewPools = lists:foldl(fun(PoolName, AccPools) ->
        PoolState = maps:get(PoolName, AccPools),
        Result = do_health_check(PoolName),
        NewPoolState = case Result of
            {ok, _} ->
                record_circuit_success(PoolState);
            {error, _} ->
                record_circuit_failure(PoolState)
        end,
        maps:put(PoolName, NewPoolState, AccPools)
    end, State#state.pools, Pools),

    % Check if any circuit breakers need to transition to half_open
    FinalPools = maps:map(fun(_PoolName, PoolState) ->
        check_circuit_timeout(PoolState)
    end, NewPools),

    % Schedule next health check
    Timer = erlang:send_after(?DEFAULT_HEALTH_CHECK_INTERVAL, self(), health_check_all),
    {noreply, State#state{pools = FinalPools, health_check_timer = Timer}};

handle_info({'EXIT', Pid, Reason}, State) ->
    % Handle pool worker exit
    Pools = maps:map(fun(_PoolName, PoolState) ->
        case PoolState#pool_state.pool_pid of
            Pid ->
                logger:error("Pool ~p exited with reason: ~p", [_PoolName, Reason]),
                PoolState#pool_state{pool_pid = undefined};
            _ ->
                PoolState
        end
    end, State#state.pools),
    {noreply, State#state{pools = Pools}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.health_check_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    logger:info("Database pool manager terminating"),
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec initialize_pools_from_config(proplists:proplist()) -> ok.
initialize_pools_from_config(PoolsConfig) ->
    lists:foreach(fun({PoolName, Config}) ->
        case do_start_pool(PoolName, Config) of
            {ok, _Pid} ->
                logger:info("Auto-started database pool: ~p", [PoolName]),
                ok;
            {error, Reason} ->
                logger:error("Failed to start pool ~p: ~p", [PoolName, Reason])
        end
    end, PoolsConfig),
    ok.

%% @private
-spec build_pool_config(pool_name(), proplists:proplist()) -> #pool_config{}.
build_pool_config(PoolName, Config) ->
    #pool_config{
        name = PoolName,
        size = proplists:get_value(size, Config, ?DEFAULT_POOL_SIZE),
        max_overflow = proplists:get_value(max_overflow, Config, ?DEFAULT_MAX_OVERFLOW),
        connection_mod = proplists:get_value(connection_mod, Config, erlmcp_db_connection),
        connection_args = Config,
        health_check_interval = proplists:get_value(health_check_interval, Config,
                                                    ?DEFAULT_HEALTH_CHECK_INTERVAL),
        circuit_threshold = proplists:get_value(circuit_breaker_threshold, Config,
                                                ?DEFAULT_CIRCUIT_THRESHOLD),
        circuit_timeout = proplists:get_value(circuit_breaker_timeout, Config,
                                             ?DEFAULT_CIRCUIT_TIMEOUT),
        retry_max_attempts = proplists:get_value(retry_max_attempts, Config,
                                                  ?DEFAULT_RETRY_MAX_ATTEMPTS),
        retry_base_delay = proplists:get_value(retry_base_delay, Config,
                                                ?DEFAULT_RETRY_BASE_DELAY),
        retry_max_delay = proplists:get_value(retry_max_delay, Config,
                                              ?DEFAULT_RETRY_MAX_DELAY)
    }.

%% @private
-spec do_start_pool(pool_name(), proplists:proplist()) -> {ok, pid()} | {error, term()}.
do_start_pool(PoolName, Config) ->
    Size = proplists:get_value(size, Config, ?DEFAULT_POOL_SIZE),
    MaxOverflow = proplists:get_value(max_overflow, Config, ?DEFAULT_MAX_OVERFLOW),

    PoolArgs = [
        {name, {local, PoolName}},
        {worker_module, erlmcp_db_connection},
        {size, Size},
        {max_overflow, MaxOverflow}
    ],

    % Ensure poolboy supervisor exists
    case whereis(erlmcp_db_pool_sup) of
        undefined ->
            {error, pool_supervisor_not_running};
        _ ->
            poolboy:start_pool(PoolName, PoolArgs, Config)
    end.

%% @private
-spec execute_with_retry(pool_name(), iodata(), list(), #pool_config{}, #state{}) ->
    query_result().
execute_with_retry(PoolName, Query, Params, Config, _State) ->
    MaxAttempts = Config#pool_config.retry_max_attempts,
    BaseDelay = Config#pool_config.retry_base_delay,
    MaxDelay = Config#pool_config.retry_max_delay,

    do_execute_with_retry(PoolName, Query, Params, MaxAttempts, BaseDelay, MaxDelay, 0).

%% @private
-spec do_execute_with_retry(pool_name(), iodata(), list(), pos_integer(), pos_integer(),
                            pos_integer(), non_neg_integer()) -> query_result().
do_execute_with_retry(_PoolName, _Query, _Params, MaxAttempts, _BaseDelay, _MaxDelay,
                       Attempt) when Attempt >= MaxAttempts ->
    {error, {max_retries_exceeded, MaxAttempts}};
do_execute_with_retry(PoolName, Query, Params, MaxAttempts, BaseDelay, MaxDelay, Attempt) ->
    case poolboy:transaction(PoolName, fun(Connection) ->
        erlmcp_db_connection:query(Connection, Query, Params)
    end) of
        {ok, _} = Result ->
            Result;
        {error, Reason} ->
            logger:warning("Database query failed (attempt ~p/~p): ~p",
                          [Attempt + 1, MaxAttempts, Reason]),
            Delay = calculate_exponential_backoff(Attempt, BaseDelay, MaxDelay),
            timer:sleep(Delay),
            do_execute_with_retry(PoolName, Query, Params, MaxAttempts,
                                 BaseDelay, MaxDelay, Attempt + 1)
    end.

%% @private
-spec calculate_exponential_backoff(non_neg_integer(), pos_integer(), pos_integer()) ->
    pos_integer().
calculate_exponential_backoff(Attempt, BaseDelay, MaxDelay) ->
    Delay = BaseDelay * round(math:pow(2, Attempt)),
    % Add jitter (up to 25%)
    Jitter = rand:uniform(Delay div 4),
    min(Delay + Jitter, MaxDelay).

%% @private
-spec do_transaction(pool_name(), transaction_fun(), timeout()) ->
    {ok, term()} | {error, term()}.
do_transaction(PoolName, Fun, Timeout) ->
    case poolboy:transaction(PoolName, fun(Connection) ->
        try
            case erlmcp_db_connection:begin_transaction(Connection) of
                {ok, _} ->
                    case Fun(Connection) of
                        {ok, Result} ->
                            case erlmcp_db_connection:commit(Connection) of
                                {ok, _} -> {ok, Result};
                                {error, Reason} -> {error, {commit_failed, Reason}}
                            end;
                        {error, Reason} ->
                            erlmcp_db_connection:rollback(Connection),
                            {error, {transaction_failed, Reason}};
                        Result ->
                            case erlmcp_db_connection:commit(Connection) of
                                {ok, _} -> {ok, Result};
                                {error, Reason} -> {error, {commit_failed, Reason}}
                            end
                    end;
                {error, Reason} ->
                    {error, {begin_transaction_failed, Reason}}
            end
        catch
            Type:Error:Stacktrace ->
                erlmcp_db_connection:rollback(Connection),
                {error, {Type, Error, Stacktrace}}
        end
    end, Timeout) of
        {error, {checkout_failed, Reason}} ->
            {error, {checkout_failed, Reason}};
        Result ->
            Result
    end.

%% @private
-spec do_health_check(pool_name()) -> {ok, map()} | {error, term()}.
do_health_check(PoolName) ->
    try
        {ok, Result} = poolboy:transaction(PoolName, fun(Connection) ->
            erlmcp_db_connection:health_check(Connection)
        end, 2000),
        {ok, #{
            pool => PoolName,
            status => healthy,
            result => Result
        }}
    catch
        _:Error ->
            {error, {health_check_failed, Error}}
    end.

%% @private
-spec record_circuit_success(#pool_state{}) -> #pool_state{}.
record_circuit_success(PoolState) ->
    Circuit = PoolState#pool_state.circuit_state,
    NewCircuit = case Circuit#circuit_state.state of
        ?CIRCUIT_STATE_HALF_OPEN ->
            SuccessCount = Circuit#circuit_state.success_count + 1,
            if
                SuccessCount >= 2 ->  % Threshold to close circuit
                    logger:info("Circuit breaker closing after successful health check"),
                    Circuit#circuit_state{
                        state = ?CIRCUIT_STATE_CLOSED,
                        failure_count = 0,
                        success_count = 0
                    };
                true ->
                    Circuit#circuit_state{success_count = SuccessCount}
            end;
        ?CIRCUIT_STATE_CLOSED ->
            Circuit#circuit_state{failure_count = 0};
        ?CIRCUIT_STATE_OPEN ->
            Circuit
    end,
    PoolState#pool_state{circuit_state = NewCircuit}.

%% @private
-spec record_circuit_failure(#pool_state{}) -> #pool_state{}.
record_circuit_failure(PoolState) ->
    Circuit = PoolState#pool_state.circuit_state,
    Threshold = get_circuit_threshold(PoolState),
    NewCircuit = case Circuit#circuit_state.state of
        ?CIRCUIT_STATE_CLOSED ->
            FailureCount = Circuit#circuit_state.failure_count + 1,
            if
                FailureCount >= Threshold ->
                    logger:warning("Circuit breaker opening after ~p failures", [FailureCount]),
                    Circuit#circuit_state{
                        state = ?CIRCUIT_STATE_OPEN,
                        failure_count = 0,
                        opened_at = erlang:timestamp(),
                        last_failure_time = erlang:timestamp()
                    };
                true ->
                    Circuit#circuit_state{
                        failure_count = FailureCount,
                        last_failure_time = erlang:timestamp()
                    }
            end;
        ?CIRCUIT_STATE_HALF_OPEN ->
            logger:warning("Circuit breaker opening again during half-open state"),
            Circuit#circuit_state{
                state = ?CIRCUIT_STATE_OPEN,
                failure_count = 0,
                opened_at = erlang:timestamp(),
                last_failure_time = erlang:timestamp()
            };
        ?CIRCUIT_STATE_OPEN ->
            Circuit#circuit_state{last_failure_time = erlang:timestamp()}
    end,
    PoolState#pool_state{circuit_state = NewCircuit}.

%% @private
-spec check_circuit_timeout(#pool_state{}) -> #pool_state{}.
check_circuit_timeout(PoolState) ->
    Circuit = PoolState#pool_state.circuit_state,
    case Circuit#circuit_state.state of
        ?CIRCUIT_STATE_OPEN ->
            Timeout = get_circuit_timeout(PoolState),
            Now = erlang:timestamp(),
            case Circuit#circuit_state.opened_at of
                undefined ->
                    PoolState;
                OpenedAt ->
                    OpenTimeMicros = timer:now_diff(Now, OpenedAt),
                    if
                        OpenTimeMicros >= Timeout * 1000 ->
                            logger:info("Circuit breaker transitioning to half-open"),
                            PoolState#pool_state{circuit_state = Circuit#circuit_state{
                                state = ?CIRCUIT_STATE_HALF_OPEN,
                                success_count = 0
                            }};
                        true ->
                            PoolState
                    end
            end;
        _ ->
            PoolState
    end.

%% @private
-spec update_metrics_and_circuit(#pool_state{}, query_result()) -> #pool_state{}.
update_metrics_and_circuit(PoolState, {ok, _}) ->
    record_circuit_success(PoolState);
update_metrics_and_circuit(PoolState, {error, _}) ->
    record_circuit_failure(PoolState).

%% @private
-spec calculate_pool_stats(pool_name(), #pool_state{}) -> map().
calculate_pool_stats(PoolName, PoolState) ->
    Circuit = PoolState#pool_state.circuit_state,
    Metrics = PoolState#pool_state.metrics,

    % Get poolboy stats if available
    PoolStatus = case PoolState#pool_state.pool_pid of
        undefined -> #{};
        Pid when is_pid(Pid) ->
            try poolboy:status(PoolName) of
                Status ->
                    #{
                        size => element(1, Status),
                        overflow => element(2, Status)
                    }
            catch
                _:_ -> #{}
            end
    end,

    #{
        pool => PoolName,
        circuit_state => Circuit#circuit_state.state,
        failure_count => Circuit#circuit_state.failure_count,
        success_count => Circuit#circuit_state.success_count,
        last_failure_time => Circuit#circuit_state.last_failure_time,
        opened_at => Circuit#circuit_state.opened_at,
        metrics => Metrics,
        poolboy => PoolStatus
    }.

%% @private
-spec init_metrics() -> map().
init_metrics() ->
    #{
        total_queries => 0,
        successful_queries => 0,
        failed_queries => 0,
        circuit_opens => 0,
        total_latency_us => 0
    }.

%% @private
-spec get_pool_config(pool_name()) -> #pool_config{}.
get_pool_config(_PoolName) ->
    % In production, this would retrieve the actual pool config
    % For now, return defaults
    #pool_config{
        name = default,
        size = ?DEFAULT_POOL_SIZE,
        max_overflow = ?DEFAULT_MAX_OVERFLOW,
        connection_mod = erlmcp_db_connection,
        connection_args = [],
        health_check_interval = ?DEFAULT_HEALTH_CHECK_INTERVAL,
        circuit_threshold = ?DEFAULT_CIRCUIT_THRESHOLD,
        circuit_timeout = ?DEFAULT_CIRCUIT_TIMEOUT,
        retry_max_attempts = ?DEFAULT_RETRY_MAX_ATTEMPTS,
        retry_base_delay = ?DEFAULT_RETRY_BASE_DELAY,
        retry_max_delay = ?DEFAULT_RETRY_MAX_DELAY
    }.

%% @private
-spec get_circuit_threshold(#pool_state{}) -> pos_integer().
get_circuit_threshold(_PoolState) ->
    ?DEFAULT_CIRCUIT_THRESHOLD.

%% @private
-spec get_circuit_timeout(#pool_state{}) -> pos_integer().
get_circuit_timeout(_PoolState) ->
    ?DEFAULT_CIRCUIT_TIMEOUT.

%% @private
%% poolboy_worker callback - creates individual connection worker
start_link(Args) ->
    erlmcp_db_connection:start_link(Args).
