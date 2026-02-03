%%%-------------------------------------------------------------------
%%% @doc
%%% Workflow timeout and retry policies for erlmcp v3
%%% Manages execution policies with circuit breaker and backoff strategies.
%%%
%%% == Features ==
%%% - Configurable timeout strategies
%%% - Exponential backoff retry
%%% - Circuit breaker pattern
%%% - Dead letter queue
%%% - Policy evaluation engine
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_workflow_policy).

-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([start_link/0, start_link/1,
         define_policy/2, get_policy/1,
         evaluate_policy/2, evaluate_task_policy/3,
         register_timeout/2, register_retry/2,
         register_circuit_breaker/2,
         check_circuit_breaker/1,
         reset_circuit_breaker/1,
         get_policy_stats/1,
         list_policies/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Type definitions
-type policy_id() :: binary().
-type workflow_id() :: binary().
-type task_id() :: binary().

-record(timeout_policy,
        {id :: policy_id(),
         default_timeout = 30000 :: pos_integer(),
         max_timeout = 300000 :: pos_integer(),
         per_task_timeouts = #{} :: #{task_id() => pos_integer()}}).

-record(retry_policy,
        {id :: policy_id(),
         max_attempts = 3 :: pos_integer(),
         backoff = exponential :: exponential | fixed,
         base_delay = 1000 :: pos_integer(),
         max_delay = 60000 :: pos_integer(),
         retryable_errors = [] :: [term()]}).

-record(circuit_breaker_state,
        {id :: policy_id(),
         state = closed :: closed | open | half_open,
         failure_count = 0 :: non_neg_integer(),
         success_count = 0 :: non_neg_integer(),
         failure_threshold = 5 :: pos_integer(),
         success_threshold = 2 :: pos_integer(),
         timeout = 60000 :: pos_integer(),
         last_failure_time :: integer() | undefined,
         opened_at :: integer() | undefined}).

-record(policy_stats,
        {policy_id :: policy_id(),
         total_evaluations = 0 :: non_neg_integer(),
         allowed_executions = 0 :: non_neg_integer(),
         rejected_executions = 0 :: non_neg_integer(),
         timeouts = 0 :: non_neg_integer(),
         retries = 0 :: non_neg_integer()}).

-record(state,
        {timeout_policies = #{} :: #{policy_id() => #timeout_policy{}},
         retry_policies = #{} :: #{policy_id() => #retry_policy{}},
         circuit_breakers = #{} :: #{policy_id() => #circuit_breaker_state{}},
         policy_stats = #{} :: #{policy_id() => #policy_stats{}},
         dead_letter_queue = [] :: list()}).

-define(DEFAULT_TIMEOUT, 30000).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(_Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec define_policy(binary(), map()) -> {ok, policy_id()} | {error, term()}.
define_policy(PolicyType, PolicyConfig) ->
    gen_server:call(?MODULE, {define_policy, PolicyType, PolicyConfig}, ?DEFAULT_TIMEOUT).

-spec get_policy(policy_id()) -> {ok, map()} | {error, not_found}.
get_policy(PolicyId) ->
    gen_server:call(?MODULE, {get_policy, PolicyId}, ?DEFAULT_TIMEOUT).

-spec evaluate_policy(policy_id(), map()) -> {ok, map()} | {error, term()}.
evaluate_policy(PolicyId, Context) ->
    gen_server:call(?MODULE, {evaluate_policy, PolicyId, Context}, ?DEFAULT_TIMEOUT).

-spec evaluate_task_policy(task_id(), policy_id(), map()) -> {ok, map()} | {error, term()}.
evaluate_task_policy(TaskId, PolicyId, Context) ->
    gen_server:call(?MODULE, {evaluate_task_policy, TaskId, PolicyId, Context}, ?DEFAULT_TIMEOUT).

-spec register_timeout(policy_id(), map()) -> {ok, policy_id()} | {error, term()}.
register_timeout(PolicyId, Config) ->
    gen_server:call(?MODULE, {register_timeout, PolicyId, Config}, ?DEFAULT_TIMEOUT).

-spec register_retry(policy_id(), map()) -> {ok, policy_id()} | {error, term()}.
register_retry(PolicyId, Config) ->
    gen_server:call(?MODULE, {register_retry, PolicyId, Config}, ?DEFAULT_TIMEOUT).

-spec register_circuit_breaker(policy_id(), map()) -> {ok, policy_id()} | {error, term()}.
register_circuit_breaker(PolicyId, Config) ->
    gen_server:call(?MODULE, {register_circuit_breaker, PolicyId, Config}, ?DEFAULT_TIMEOUT).

-spec check_circuit_breaker(policy_id()) -> {ok, closed | open | half_open} | {error, not_found}.
check_circuit_breaker(PolicyId) ->
    gen_server:call(?MODULE, {check_circuit_breaker, PolicyId}, ?DEFAULT_TIMEOUT).

-spec reset_circuit_breaker(policy_id()) -> ok | {error, not_found}.
reset_circuit_breaker(PolicyId) ->
    gen_server:call(?MODULE, {reset_circuit_breaker, PolicyId}, ?DEFAULT_TIMEOUT).

-spec get_policy_stats(policy_id()) -> {ok, map()} | {error, not_found}.
get_policy_stats(PolicyId) ->
    gen_server:call(?MODULE, {get_policy_stats, PolicyId}, ?DEFAULT_TIMEOUT).

-spec list_policies() -> {ok, [policy_id()]}.
list_policies() ->
    gen_server:call(?MODULE, list_policies, ?DEFAULT_TIMEOUT).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    logger:info("Initializing workflow policy engine"),
    {ok, #state{}}.

handle_call({define_policy, <<"timeout">>, Config}, _From, State) ->
    PolicyId = maps:get(id, Config, generate_policy_id()),
    TimeoutPolicy = #timeout_policy{
        id = PolicyId,
        default_timeout = maps:get(default_timeout, Config, 30000),
        max_timeout = maps:get(max_timeout, Config, 300000),
        per_task_timeouts = maps:get(per_task_timeouts, Config, #{})
    },
    NewTimeoutPolicies = maps:put(PolicyId, TimeoutPolicy, State#state.timeout_policies),
    {reply, {ok, PolicyId}, State#state{timeout_policies = NewTimeoutPolicies}};

handle_call({define_policy, <<"retry">>, Config}, _From, State) ->
    PolicyId = maps:get(id, Config, generate_policy_id()),
    RetryPolicy = #retry_policy{
        id = PolicyId,
        max_attempts = maps:get(max_attempts, Config, 3),
        backoff = maps:get(backoff, Config, exponential),
        base_delay = maps:get(base_delay, Config, 1000),
        max_delay = maps:get(max_delay, Config, 60000),
        retryable_errors = maps:get(retryable_errors, Config, [])
    },
    NewRetryPolicies = maps:put(PolicyId, RetryPolicy, State#state.retry_policies),
    {reply, {ok, PolicyId}, State#state{retry_policies = NewRetryPolicies}};

handle_call({define_policy, <<"circuit_breaker">>, Config}, _From, State) ->
    PolicyId = maps:get(id, Config, generate_policy_id()),
    CircuitBreaker = #circuit_breaker_state{
        id = PolicyId,
        failure_threshold = maps:get(failure_threshold, Config, 5),
        success_threshold = maps:get(success_threshold, Config, 2),
        timeout = maps:get(timeout, Config, 60000)
    },
    NewCircuitBreakers = maps:put(PolicyId, CircuitBreaker, State#state.circuit_breakers),
    {reply, {ok, PolicyId}, State#state{circuit_breakers = NewCircuitBreakers}};

handle_call({get_policy, PolicyId}, _From, State) ->
    case maps:get(PolicyId, State#state.timeout_policies, undefined) of
        #timeout_policy{} = Policy ->
            {reply, {ok, timeout_policy_to_map(Policy)}, State};
        undefined ->
            case maps:get(PolicyId, State#state.retry_policies, undefined) of
                #retry_policy{} = Policy ->
                    {reply, {ok, retry_policy_to_map(Policy)}, State};
                undefined ->
                    case maps:get(PolicyId, State#state.circuit_breakers, undefined) of
                        #circuit_breaker_state{} = Policy ->
                            {reply, {ok, circuit_breaker_to_map(Policy)}, State};
                        undefined ->
                            {reply, {error, not_found}, State}
                    end
            end
    end;

handle_call({evaluate_policy, PolicyId, Context}, _From, State) ->
    case maps:get(PolicyId, State#state.timeout_policies, undefined) of
        #timeout_policy{} = Policy ->
            Timeout = evaluate_timeout(Policy, Context),
            NewStats = update_stats(PolicyId, allowed, State#state.policy_stats),
            {reply, {ok, #{timeout => Timeout}}, State#state{policy_stats = NewStats}};
        undefined ->
            {reply, {error, not_found}, State}
    end;

handle_call({evaluate_task_policy, TaskId, PolicyId, Context}, _From, State) ->
    case maps:get(PolicyId, State#state.retry_policies, undefined) of
        #retry_policy{} = Policy ->
            case should_retry(Policy, Context) of
                {true, Delay} ->
                    NewStats = update_stats(PolicyId, retry, State#state.policy_stats),
                    {reply, {ok, #{retry => true, delay => Delay}}, State#state{policy_stats = NewStats}};
                false ->
                    NewStats = update_stats(PolicyId, rejected, State#state.policy_stats),
                    {reply, {error, max_retries_exceeded}, State#state{policy_stats = NewStats}}
            end;
        undefined ->
            {reply, {error, not_found}, State}
    end;

handle_call({register_timeout, PolicyId, Config}, _From, State) ->
    TimeoutPolicy = #timeout_policy{
        id = PolicyId,
        default_timeout = maps:get(default_timeout, Config, 30000),
        max_timeout = maps:get(max_timeout, Config, 300000),
        per_task_timeouts = maps:get(per_task_timeouts, Config, #{})
    },
    NewTimeoutPolicies = maps:put(PolicyId, TimeoutPolicy, State#state.timeout_policies),
    {reply, {ok, PolicyId}, State#state{timeout_policies = NewTimeoutPolicies}};

handle_call({register_retry, PolicyId, Config}, _From, State) ->
    RetryPolicy = #retry_policy{
        id = PolicyId,
        max_attempts = maps:get(max_attempts, Config, 3),
        backoff = maps:get(backoff, Config, exponential),
        base_delay = maps:get(base_delay, Config, 1000),
        max_delay = maps:get(max_delay, Config, 60000),
        retryable_errors = maps:get(retryable_errors, Config, [])
    },
    NewRetryPolicies = maps:put(PolicyId, RetryPolicy, State#state.retry_policies),
    {reply, {ok, PolicyId}, State#state{retry_policies = NewRetryPolicies}};

handle_call({register_circuit_breaker, PolicyId, Config}, _From, State) ->
    CircuitBreaker = #circuit_breaker_state{
        id = PolicyId,
        failure_threshold = maps:get(failure_threshold, Config, 5),
        success_threshold = maps:get(success_threshold, Config, 2),
        timeout = maps:get(timeout, Config, 60000)
    },
    NewCircuitBreakers = maps:put(PolicyId, CircuitBreaker, State#state.circuit_breakers),
    {reply, {ok, PolicyId}, State#state{circuit_breakers = NewCircuitBreakers}};

handle_call({check_circuit_breaker, PolicyId}, _From, State) ->
    case maps:get(PolicyId, State#state.circuit_breakers, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #circuit_breaker_state{state = CBState} ->
            {reply, {ok, CBState}, State}
    end;

handle_call({reset_circuit_breaker, PolicyId}, _From, State) ->
    case maps:get(PolicyId, State#state.circuit_breakers, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        CircuitBreaker ->
            ResetBreaker = CircuitBreaker#circuit_breaker_state{
                state = closed,
                failure_count = 0,
                success_count = 0,
                last_failure_time = undefined,
                opened_at = undefined
            },
            NewCircuitBreakers = maps:put(PolicyId, ResetBreaker, State#state.circuit_breakers),
            {reply, ok, State#state{circuit_breakers = NewCircuitBreakers}}
    end;

handle_call({get_policy_stats, PolicyId}, _From, State) ->
    case maps:get(PolicyId, State#state.policy_stats, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #policy_stats{} = Stats ->
            StatsMap = #{
                policy_id => Stats#policy_stats.policy_id,
                total_evaluations => Stats#policy_stats.total_evaluations,
                allowed_executions => Stats#policy_stats.allowed_executions,
                rejected_executions => Stats#policy_stats.rejected_executions,
                timeouts => Stats#policy_stats.timeouts,
                retries => Stats#policy_stats.retries
            },
            {reply, {ok, StatsMap}, State}
    end;

handle_call(list_policies, _From, State) ->
    TimeoutIds = maps:keys(State#state.timeout_policies),
    RetryIds = maps:keys(State#state.retry_policies),
    CircuitBreakerIds = maps:keys(State#state.circuit_breakers),
    AllPolicies = lists:usort(TimeoutIds ++ RetryIds ++ CircuitBreakerIds),
    {reply, {ok, AllPolicies}, State};

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

-spec generate_policy_id() -> binary().
generate_policy_id() ->
    <<Id:64>> = crypto:strong_rand_bytes(8),
    <<"policy_", (integer_to_binary(Id))/binary>>.

-spec evaluate_timeout(#timeout_policy{}, map()) -> pos_integer().
evaluate_timeout(#timeout_policy{default_timeout = Default, per_task_timeouts = PerTask}, Context) ->
    TaskId = maps:get(task_id, Context, undefined),
    case TaskId of
        undefined -> Default;
        _ ->
            case maps:get(TaskId, PerTask, undefined) of
                undefined -> Default;
                Timeout -> Timeout
            end
    end.

-spec should_retry(#retry_policy{}, map()) -> {true, pos_integer()} | false.
should_retry(#retry_policy{max_attempts = MaxAttempts, backoff = Backoff,
                           base_delay = BaseDelay, max_delay = MaxDelay,
                           retryable_errors = RetryableErrors}, Context) ->
    Attempt = maps:get(attempt, 0, Context),
    Error = maps:get(error, undefined, Context),

    case Attempt < MaxAttempts of
        false ->
            false;
        true ->
            case is_error_retryable(Error, RetryableErrors) of
                true ->
                    Delay = calculate_backoff(Attempt + 1, BaseDelay, MaxDelay, Backoff),
                    {true, Delay};
                false ->
                    false
            end
    end.

-spec is_error_retryable(term(), [term()]) -> boolean().
is_error_retryable(_Error, []) -> true;
is_error_retryable(Error, RetryableErrors) ->
    lists:any(fun(Pattern) ->
        case Pattern of
            Error -> true;
            _ when is_tuple(Pattern) -> Error =:= Pattern;
            _ -> false
        end
    end, RetryableErrors).

-spec calculate_backoff(pos_integer(), pos_integer(), pos_integer(), exponential | fixed) ->
                          pos_integer().
calculate_backoff(Attempt, BaseDelay, MaxDelay, exponential) ->
    min(BaseDelay * round(math:pow(2, Attempt - 1)), MaxDelay);
calculate_backoff(_Attempt, BaseDelay, MaxDelay, fixed) ->
    min(BaseDelay, MaxDelay).

-spec update_stats(policy_id(), allowed | rejected | retry | timeout,
                   #{policy_id() => #policy_stats{}}) -> #{policy_id() => #policy_stats{}}.
update_stats(PolicyId, Result, Stats) ->
    case maps:get(PolicyId, Stats, undefined) of
        undefined ->
            NewStats = #policy_stats{policy_id = PolicyId},
            update_stats_record(NewStats, Result);
        #policy_stats{} = S ->
            UpdatedStats = update_stats_record(S, Result),
            maps:put(PolicyId, UpdatedStats, Stats)
    end.

-spec update_stats_record(#policy_stats{}, allowed | rejected | retry | timeout) -> #policy_stats{}.
update_stats_record(Stats, allowed) ->
    Stats#policy_stats{
        total_evaluations = Stats#policy_stats.total_evaluations + 1,
        allowed_executions = Stats#policy_stats.allowed_executions + 1
    };
update_stats_record(Stats, rejected) ->
    Stats#policy_stats{
        total_evaluations = Stats#policy_stats.total_evaluations + 1,
        rejected_executions = Stats#policy_stats.rejected_executions + 1
    };
update_stats_record(Stats, retry) ->
    Stats#policy_stats{
        total_evaluations = Stats#policy_stats.total_evaluations + 1,
        retries = Stats#policy_stats.retries + 1
    };
update_stats_record(Stats, timeout) ->
    Stats#policy_stats{
        total_evaluations = Stats#policy_stats.total_evaluations + 1,
        timeouts = Stats#policy_stats.timeouts + 1
    }.

-spec timeout_policy_to_map(#timeout_policy{}) -> map().
timeout_policy_to_map(Policy) ->
    #{
        type => timeout,
        id => Policy#timeout_policy.id,
        default_timeout => Policy#timeout_policy.default_timeout,
        max_timeout => Policy#timeout_policy.max_timeout,
        per_task_timeouts => Policy#timeout_policy.per_task_timeouts
    }.

-spec retry_policy_to_map(#retry_policy{}) -> map().
retry_policy_to_map(Policy) ->
    #{
        type => retry,
        id => Policy#retry_policy.id,
        max_attempts => Policy#retry_policy.max_attempts,
        backoff => Policy#retry_policy.backoff,
        base_delay => Policy#retry_policy.base_delay,
        max_delay => Policy#retry_policy.max_delay,
        retryable_errors => Policy#retry_policy.retryable_errors
    }.

-spec circuit_breaker_to_map(#circuit_breaker_state{}) -> map().
circuit_breaker_to_map(State) ->
    #{
        type => circuit_breaker,
        id => State#circuit_breaker_state.id,
        state => State#circuit_breaker_state.state,
        failure_count => State#circuit_breaker_state.failure_count,
        success_count => State#circuit_breaker_state.success_count,
        failure_threshold => State#circuit_breaker_state.failure_threshold,
        success_threshold => State#circuit_breaker_state.success_threshold,
        timeout => State#circuit_breaker_state.timeout
    }.
