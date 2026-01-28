%%%-------------------------------------------------------------------
%%% @doc
%%% Lifecycle Manager: TTL Management, Subscription Limits, and Resource Cleanup
%%%
%%% This module manages the lifecycle of transient resources:
%%% - Subscriptions with TTL expiry and automatic cleanup
%%% - Task cleanup and progress token management
%%% - Subscription count limits per connection/tenant
%%% - Memory leak prevention through periodic cleanup
%%% - Metrics collection for monitoring
%%%
%%% Architecture:
%%% 1. Per-Server TTL Tracking: Each subscription/task stores creation time
%%% 2. Periodic Cleanup: Spawned on init, runs every check_interval_ms (default: 30s)
%%% 3. Limit Enforcement: Rejects new subscriptions if at limit
%%% 4. Metrics: Tracks cleanup events, expirations, and rejections
%%%
%%% Configuration (in sys.config):
%%% {erlmcp, [
%%%   {subscription_ttl_ms, 3600000},      % 1 hour default
%%%   {task_ttl_ms, 3600000},             % 1 hour default
%%%   {max_subscriptions_per_server, 10000}, % Hard limit
%%%   {cleanup_interval_ms, 30000},        % Check every 30s
%%%   {cleanup_batch_size, 1000}          % Process 1000 at a time
%%% ]}.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_lifecycle_manager).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API
-export([
    start_link/1,
    register_subscription/4,
    unregister_subscription/3,
    register_task/4,
    unregister_task/3,
    get_subscription_count/2,
    get_task_count/2,
    get_metrics/1,
    reset_metrics/1,
    stop/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types (subscription_key and task_key are internal)

-record(subscription, {
    uri :: binary(),
    subscriber :: pid(),
    created_at :: integer(),  % milliseconds since epoch
    ttl_ms :: integer()
}).

-record(task, {
    task_id :: binary(),
    created_at :: integer(),
    ttl_ms :: integer(),
    progress_token :: binary() | undefined
}).

-record(state, {
    server_id :: server_id(),
    subscriptions = #{} :: map(),
    tasks = #{} :: map(),
    subscription_count = 0 :: non_neg_integer(),
    task_count = 0 :: non_neg_integer(),
    max_subscriptions :: pos_integer(),
    subscription_ttl_ms :: pos_integer(),
    task_ttl_ms :: pos_integer(),
    cleanup_interval_ms :: pos_integer(),
    cleanup_batch_size :: pos_integer(),
    cleanup_ref :: reference() | undefined,
    metrics = #{} :: map()
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(server_id()) -> {ok, pid()} | {error, term()}.
start_link(ServerId) ->
    gen_server:start_link(?MODULE, [ServerId], []).

-spec register_subscription(pid(), binary(), pid(), pos_integer()) ->
    ok | {error, limit_exceeded}.
register_subscription(ManagerPid, Uri, Subscriber, TtlMs) when
    is_pid(ManagerPid), is_binary(Uri), is_pid(Subscriber), is_integer(TtlMs), TtlMs > 0
->
    gen_server:call(ManagerPid, {register_subscription, Uri, Subscriber, TtlMs}).

-spec unregister_subscription(pid(), binary(), pid()) -> ok.
unregister_subscription(ManagerPid, Uri, Subscriber) when
    is_pid(ManagerPid), is_binary(Uri), is_pid(Subscriber)
->
    gen_server:call(ManagerPid, {unregister_subscription, Uri, Subscriber}).

-spec register_task(pid(), binary(), pos_integer(), binary() | undefined) ->
    ok | {error, limit_exceeded}.
register_task(ManagerPid, TaskId, TtlMs, ProgressToken) when
    is_pid(ManagerPid), is_binary(TaskId), is_integer(TtlMs), TtlMs > 0,
    (ProgressToken =:= undefined orelse is_binary(ProgressToken))
->
    gen_server:call(ManagerPid, {register_task, TaskId, TtlMs, ProgressToken}).

-spec unregister_task(pid(), binary(), binary() | undefined) -> ok.
unregister_task(ManagerPid, TaskId, ProgressToken) when
    is_pid(ManagerPid), is_binary(TaskId),
    (ProgressToken =:= undefined orelse is_binary(ProgressToken))
->
    gen_server:call(ManagerPid, {unregister_task, TaskId, ProgressToken}).

-spec get_subscription_count(pid(), binary()) -> non_neg_integer().
get_subscription_count(ManagerPid, Uri) when is_pid(ManagerPid), is_binary(Uri) ->
    gen_server:call(ManagerPid, {get_subscription_count, Uri}).

-spec get_task_count(pid(), binary()) -> non_neg_integer().
get_task_count(ManagerPid, TaskId) when is_pid(ManagerPid), is_binary(TaskId) ->
    gen_server:call(ManagerPid, {get_task_count, TaskId}).

-spec get_metrics(pid()) -> map().
get_metrics(ManagerPid) when is_pid(ManagerPid) ->
    gen_server:call(ManagerPid, get_metrics).

-spec reset_metrics(pid()) -> ok.
reset_metrics(ManagerPid) when is_pid(ManagerPid) ->
    gen_server:call(ManagerPid, reset_metrics).

-spec stop(pid()) -> ok.
stop(ManagerPid) when is_pid(ManagerPid) ->
    gen_server:stop(ManagerPid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([server_id()]) -> {ok, state()}.
init([ServerId]) ->
    process_flag(trap_exit, true),

    MaxSubs = application:get_env(erlmcp, max_subscriptions_per_server, 10000),
    SubTtl = application:get_env(erlmcp, subscription_ttl_ms, 3600000),
    TaskTtl = application:get_env(erlmcp, task_ttl_ms, 3600000),
    CleanupInterval = application:get_env(erlmcp, cleanup_interval_ms, 30000),
    CleanupBatch = application:get_env(erlmcp, cleanup_batch_size, 1000),

    logger:info("Starting lifecycle manager for server ~p (max_subs=~p, sub_ttl=~pms, task_ttl=~pms)",
        [ServerId, MaxSubs, SubTtl, TaskTtl]),

    State = #state{
        server_id = ServerId,
        max_subscriptions = MaxSubs,
        subscription_ttl_ms = SubTtl,
        task_ttl_ms = TaskTtl,
        cleanup_interval_ms = CleanupInterval,
        cleanup_batch_size = CleanupBatch,
        metrics = #{
            subscriptions_created => 0,
            subscriptions_expired => 0,
            subscriptions_removed => 0,
            subscriptions_rejected => 0,
            tasks_created => 0,
            tasks_expired => 0,
            tasks_removed => 0,
            total_cleanup_runs => 0,
            last_cleanup_duration_ms => 0,
            last_cleanup_at => 0
        }
    },

    % Schedule first cleanup
    CleanupRef = erlang:send_after(CleanupInterval, self(), cleanup),

    {ok, State#state{cleanup_ref = CleanupRef}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.

handle_call({register_subscription, Uri, Subscriber, TtlMs}, _From, State) ->
    Key = {uri, Uri, Subscriber},
    case State#state.subscription_count >= State#state.max_subscriptions of
        true ->
            Metrics = State#state.metrics,
            NewMetrics = Metrics#{subscriptions_rejected => maps:get(subscriptions_rejected, Metrics, 0) + 1 },
            logger:warning("Subscription limit exceeded (~p/~p): rejecting ~p on ~s",
                [State#state.subscription_count, State#state.max_subscriptions, Subscriber, Uri]),
            {reply, {error, limit_exceeded}, State#state{metrics = NewMetrics}};
        false ->
            Subscription = #subscription{
                uri = Uri,
                subscriber = Subscriber,
                created_at = erlang:system_time(millisecond),
                ttl_ms = TtlMs
            },
            NewSubs = maps:put(Key, Subscription, State#state.subscriptions),
            Metrics = State#state.metrics,
            NewMetrics = Metrics#{subscriptions_created => maps:get(subscriptions_created, Metrics, 0) + 1},
            NewState = State#state{
                subscriptions = NewSubs,
                subscription_count = State#state.subscription_count + 1,
                metrics = NewMetrics
            },
            logger:debug("Registered subscription: ~p on ~s (ttl=~pms)", [Subscriber, Uri, TtlMs]),
            {reply, ok, NewState}
    end;

handle_call({unregister_subscription, Uri, Subscriber}, _From, State) ->
    Key = {uri, Uri, Subscriber},
    case maps:is_key(Key, State#state.subscriptions) of
        true ->
            NewSubs = maps:remove(Key, State#state.subscriptions),
            Metrics = State#state.metrics,
            NewMetrics = Metrics#{subscriptions_removed => maps:get(subscriptions_removed, Metrics, 0) + 1},
            NewState = State#state{
                subscriptions = NewSubs,
                subscription_count = max(0, State#state.subscription_count - 1),
                metrics = NewMetrics
            },
            logger:debug("Unregistered subscription: ~p on ~s", [Subscriber, Uri]),
            {reply, ok, NewState};
        false ->
            {reply, ok, State}
    end;

handle_call({register_task, TaskId, TtlMs, ProgressToken}, _From, State) ->
    Key = {task, TaskId},
    case maps:is_key(Key, State#state.tasks) of
        true ->
            logger:warning("Task ~s already registered", [TaskId]),
            {reply, ok, State};
        false ->
            Task = #task{
                task_id = TaskId,
                created_at = erlang:system_time(millisecond),
                ttl_ms = TtlMs,
                progress_token = ProgressToken
            },
            NewTasks = maps:put(Key, Task, State#state.tasks),
            Metrics = State#state.metrics,
            NewMetrics = Metrics#{tasks_created => maps:get(tasks_created, Metrics, 0) + 1},
            NewState = State#state{
                tasks = NewTasks,
                task_count = State#state.task_count + 1,
                metrics = NewMetrics
            },
            logger:debug("Registered task: ~s (ttl=~pms)", [TaskId, TtlMs]),
            {reply, ok, NewState}
    end;

handle_call({unregister_task, TaskId, _ProgressToken}, _From, State) ->
    Key = {task, TaskId},
    case maps:is_key(Key, State#state.tasks) of
        true ->
            NewTasks = maps:remove(Key, State#state.tasks),
            Metrics = State#state.metrics,
            NewMetrics = Metrics#{tasks_removed => maps:get(tasks_removed, Metrics, 0) + 1},
            NewState = State#state{
                tasks = NewTasks,
                task_count = max(0, State#state.task_count - 1),
                metrics = NewMetrics
            },
            logger:debug("Unregistered task: ~s", [TaskId]),
            {reply, ok, NewState};
        false ->
            {reply, ok, State}
    end;

handle_call({get_subscription_count, Uri}, _From, State) ->
    Count = maps:size(maps:filter(fun
        ({uri, U, _}, _) -> U =:= Uri;
        (_, _) -> false
    end, State#state.subscriptions)),
    {reply, Count, State};

handle_call({get_task_count, _TaskId}, _From, State) ->
    {reply, State#state.task_count, State};

handle_call(get_metrics, _From, State) ->
    BaseMetrics = State#state.metrics,
    Metrics = BaseMetrics#{
        current_subscriptions => State#state.subscription_count,
        current_tasks => State#state.task_count
    },
    {reply, Metrics, State};

handle_call(reset_metrics, _From, State) ->
    NewMetrics = #{
        subscriptions_created => 0,
        subscriptions_expired => 0,
        subscriptions_removed => 0,
        subscriptions_rejected => 0,
        tasks_created => 0,
        tasks_expired => 0,
        tasks_removed => 0,
        total_cleanup_runs => 0,
        last_cleanup_duration_ms => 0,
        last_cleanup_at => 0
    },
    logger:info("Reset metrics for server ~p", [State#state.server_id]),
    {reply, ok, State#state{metrics = NewMetrics}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info(cleanup, State) ->
    StartTime = erlang:system_time(millisecond),

    % Run cleanup
    NewState = cleanup_expired_resources(State),

    % Calculate cleanup duration
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    % Update metrics
    Metrics = NewState#state.metrics,
    UpdatedMetrics = Metrics#{
        total_cleanup_runs => maps:get(total_cleanup_runs, Metrics, 0) + 1,
        last_cleanup_duration_ms => Duration,
        last_cleanup_at => EndTime
    },

    % Schedule next cleanup
    CleanupRef = erlang:send_after(NewState#state.cleanup_interval_ms, self(), cleanup),

    logger:debug("Cleanup run completed for server ~p: ~pms, subs=~p, tasks=~p",
        [State#state.server_id, Duration, NewState#state.subscription_count, NewState#state.task_count]),

    {noreply, NewState#state{cleanup_ref = CleanupRef, metrics = UpdatedMetrics}};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    % Handle linked process termination
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
    logger:info("Lifecycle manager for server ~p terminating: ~p", [State#state.server_id, Reason]),
    case State#state.cleanup_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec cleanup_expired_resources(state()) -> state().
cleanup_expired_resources(State) ->
    Now = erlang:system_time(millisecond),

    % Cleanup expired subscriptions
    {NewSubs, ExpiredSubCount} = cleanup_subscriptions(State#state.subscriptions, Now, State#state.cleanup_batch_size, 0),

    % Cleanup expired tasks
    {NewTasks, ExpiredTaskCount} = cleanup_tasks(State#state.tasks, Now, State#state.cleanup_batch_size, 0),

    % Update metrics
    Metrics = State#state.metrics,
    UpdatedMetrics = Metrics#{
        subscriptions_expired => maps:get(subscriptions_expired, Metrics, 0) + ExpiredSubCount,
        tasks_expired => maps:get(tasks_expired, Metrics, 0) + ExpiredTaskCount
    },

    NewSubCount = maps:size(NewSubs),
    NewTaskCount = maps:size(NewTasks),

    logger:info("Cleanup: expired ~p subscriptions, ~p tasks (remaining: ~p subs, ~p tasks)",
        [ExpiredSubCount, ExpiredTaskCount, NewSubCount, NewTaskCount]),

    State#state{
        subscriptions = NewSubs,
        subscription_count = NewSubCount,
        tasks = NewTasks,
        task_count = NewTaskCount,
        metrics = UpdatedMetrics
    }.

-spec cleanup_subscriptions(map(), integer(), pos_integer(), non_neg_integer()) ->
    {map(), non_neg_integer()}.
cleanup_subscriptions(Subscriptions, _Now, BatchSize, Count) when Count >= BatchSize ->
    {Subscriptions, Count};
cleanup_subscriptions(Subscriptions, Now, BatchSize, Count) ->
    case maps:iterator(Subscriptions) of
        Iter when Iter =:= undefined ->
            {Subscriptions, Count};
        Iter ->
            case maps:next(Iter) of
                none ->
                    {Subscriptions, Count};
                {Key, #subscription{created_at = CreatedAt, ttl_ms = TtlMs}, _NextIter} ->
                    case Now - CreatedAt >= TtlMs of
                        true ->
                            % Expired - remove it
                            cleanup_subscriptions(
                                maps:remove(Key, Subscriptions),
                                Now,
                                BatchSize,
                                Count + 1
                            );
                        false ->
                            % Not expired - continue
                            cleanup_subscriptions(
                                Subscriptions,
                                Now,
                                BatchSize,
                                Count
                            )
                    end
            end
    end.

-spec cleanup_tasks(map(), integer(), pos_integer(), non_neg_integer()) ->
    {map(), non_neg_integer()}.
cleanup_tasks(Tasks, _Now, BatchSize, Count) when Count >= BatchSize ->
    {Tasks, Count};
cleanup_tasks(Tasks, Now, BatchSize, Count) ->
    case maps:iterator(Tasks) of
        Iter when Iter =:= undefined ->
            {Tasks, Count};
        Iter ->
            case maps:next(Iter) of
                none ->
                    {Tasks, Count};
                {Key, #task{created_at = CreatedAt, ttl_ms = TtlMs}, _NextIter} ->
                    case Now - CreatedAt >= TtlMs of
                        true ->
                            % Expired - remove it
                            cleanup_tasks(
                                maps:remove(Key, Tasks),
                                Now,
                                BatchSize,
                                Count + 1
                            );
                        false ->
                            % Not expired - continue
                            cleanup_tasks(
                                Tasks,
                                Now,
                                BatchSize,
                                Count
                            )
                    end
            end
    end.
