-module(erlmcp_queue_limits).

-behaviour(gen_server).

%%% ====================================================================
%%% Deterministic Overload Behavior - Per-Role Queue Limits
%%% ====================================================================
%%%
%%% Implements mailbox capacity limits per process role to provide
%%% deterministic backpressure instead of unbounded queue growth.
%%%
%%% Role-based limits:
%%%   - session:        10,000 pending requests
%%%   - sse_stream:      5,000 pending notifications
%%%   - task_worker:     1,000 pending tasks
%%%   - tool_executor:     500 concurrent executions
%%%   - transport:       2,000 pending messages
%%%   - default:         1,000 pending messages
%%%
%%% Behavior on capacity:
%%%   - Returns 429 (Too Many Requests) instead of queuing
%%%   - Logs shedding event with severity and metrics
%%%   - Enables per-role tuning via application config
%%%
%%% Integration:
%%%   - Call check_capacity/2 before queuing work
%%%   - Returns ok | {error, {capacity_exceeded, Stats}}
%%%   - Supports dynamic limit adjustment
%%% ====================================================================

-include("erlmcp.hrl").

%% API exports
-export([start_link/0, check_capacity/2, record_enqueue/2, record_dequeue/2, get_queue_depth/1,
         get_queue_depth/2, set_limit/2, get_limit/1, get_all_limits/0, get_role_stats/1,
         get_all_stats/0, reset_stats/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Role types
-type role() :: session | sse_stream | task_worker | tool_executor | transport | default.
-type process_role() :: {role(), pid()} | {role(), atom()}.
%% Limit configuration
-type limit_config() ::
    #{max_queue_depth := non_neg_integer(),
      warning_threshold => float(),
      alert_threshold => float()}.

                                   % 0.0-1.0, default 0.8
     % 0.0-1.0, default 0.9

%% Role statistics
-type role_stats() ::
    #{role := role(),
      current_depth := non_neg_integer(),
      max_depth := non_neg_integer(),
      total_enqueued := non_neg_integer(),
      total_dequeued := non_neg_integer(),
      total_rejected := non_neg_integer(),
      capacity_exceeded_count := non_neg_integer(),
      last_capacity_exceeded => integer() | undefined}.

-export_type([role/0, process_role/0, limit_config/0, role_stats/0]).

%% Default limits per role
-define(DEFAULT_LIMITS,
        #{session => 10000,
          sse_stream => 5000,
          task_worker => 1000,
          tool_executor => 500,
          transport => 2000,
          default => 1000}).
%% Default thresholds
-define(DEFAULT_WARNING_THRESHOLD, 0.8).   % 80% capacity
-define(DEFAULT_ALERT_THRESHOLD, 0.9).     % 90% capacity

%% State record
-record(state,
        {limits = ?DEFAULT_LIMITS :: #{role() => non_neg_integer()},
         queue_depths = #{} :: #{process_role() => non_neg_integer()},
         stats = #{} :: #{role() => role_stats()},
         monitors = #{} :: #{reference() => process_role()}}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Check if process has capacity to accept more work
%% Returns ok if below limit, {error, {capacity_exceeded, Stats}} otherwise
-spec check_capacity(role(), pid() | atom()) -> ok | {error, {capacity_exceeded, role_stats()}}.
check_capacity(Role, ProcId) ->
    gen_server:call(?MODULE, {check_capacity, Role, ProcId}, 1000).

%% @doc Record message enqueue (increment queue depth)
-spec record_enqueue(role(), pid() | atom()) -> ok.
record_enqueue(Role, ProcId) ->
    gen_server:cast(?MODULE, {record_enqueue, Role, ProcId}).

%% @doc Record message dequeue (decrement queue depth)
-spec record_dequeue(role(), pid() | atom()) -> ok.
record_dequeue(Role, ProcId) ->
    gen_server:cast(?MODULE, {record_dequeue, Role, ProcId}).

%% @doc Get current queue depth for a specific process
-spec get_queue_depth(pid() | atom()) -> non_neg_integer() | {error, not_found}.
get_queue_depth(ProcId) ->
    gen_server:call(?MODULE, {get_queue_depth, ProcId}).

%% @doc Get current queue depth for a specific role and process
-spec get_queue_depth(role(), pid() | atom()) -> non_neg_integer().
get_queue_depth(Role, ProcId) ->
    gen_server:call(?MODULE, {get_queue_depth, Role, ProcId}).

%% @doc Set queue limit for a role
-spec set_limit(role(), non_neg_integer()) -> ok.
set_limit(Role, Limit) when is_integer(Limit), Limit >= 0 ->
    gen_server:call(?MODULE, {set_limit, Role, Limit}).

%% @doc Get queue limit for a role
-spec get_limit(role()) -> non_neg_integer().
get_limit(Role) ->
    gen_server:call(?MODULE, {get_limit, Role}).

%% @doc Get all configured limits
-spec get_all_limits() -> #{role() => non_neg_integer()}.
get_all_limits() ->
    gen_server:call(?MODULE, get_all_limits).

%% @doc Get statistics for a specific role
-spec get_role_stats(role()) -> role_stats() | {error, not_found}.
get_role_stats(Role) ->
    gen_server:call(?MODULE, {get_role_stats, Role}).

%% @doc Get statistics for all roles
-spec get_all_stats() -> #{role() => role_stats()}.
get_all_stats() ->
    gen_server:call(?MODULE, get_all_stats).

%% @doc Reset statistics for a role
-spec reset_stats(role()) -> ok.
reset_stats(Role) ->
    gen_server:call(?MODULE, {reset_stats, Role}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    %% Load custom limits from application config
    CustomLimits = application:get_env(erlmcp_core, queue_limits, #{}),
    Limits = maps:merge(?DEFAULT_LIMITS, CustomLimits),

    %% Initialize stats for each role
    Stats = maps:from_list([{Role, init_role_stats(Role)} || Role <- maps:keys(Limits)]),

    {ok, #state{limits = Limits, stats = Stats}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call({check_capacity, Role, ProcId}, _From, State) ->
    ProcRole = {Role, ProcId},
    CurrentDepth = maps:get(ProcRole, State#state.queue_depths, 0),
    Limit = maps:get(Role, State#state.limits, maps:get(default, State#state.limits)),

    case CurrentDepth >= Limit of
        true ->
            %% Capacity exceeded - record rejection
            NewStats = record_rejection(Role, State#state.stats),
            RoleStats = maps:get(Role, NewStats, init_role_stats(Role)),
            logger:warning("Queue capacity exceeded for ~p:~p (depth: ~p, limit: ~p)",
                           [Role, ProcId, CurrentDepth, Limit]),
            {reply, {error, {capacity_exceeded, RoleStats}}, State#state{stats = NewStats}};
        false ->
            %% Check warning threshold
            WarningThreshold = Limit * ?DEFAULT_WARNING_THRESHOLD,
            AlertThreshold = Limit * ?DEFAULT_ALERT_THRESHOLD,

            if CurrentDepth >= AlertThreshold ->
                   logger:warning("Queue depth at ~p% for ~p:~p (~p/~p)",
                                  [round(CurrentDepth / Limit * 100),
                                   Role,
                                   ProcId,
                                   CurrentDepth,
                                   Limit]);
               CurrentDepth >= WarningThreshold ->
                   logger:info("Queue depth at ~p% for ~p:~p (~p/~p)",
                               [round(CurrentDepth / Limit * 100),
                                Role,
                                ProcId,
                                CurrentDepth,
                                Limit]);
               true ->
                   ok
            end,

            {reply, ok, State}
    end;
handle_call({get_queue_depth, ProcId}, _From, State) ->
    %% Find queue depth for any role with this ProcId
    Result =
        maps:fold(fun ({_Role, Pid}, Depth, Acc) when Pid =:= ProcId ->
                          case Acc of
                              {error, not_found} ->
                                  Depth;
                              Current ->
                                  Current + Depth
                          end;
                      (_, _, Acc) ->
                          Acc
                  end,
                  {error, not_found},
                  State#state.queue_depths),
    {reply, Result, State};
handle_call({get_queue_depth, Role, ProcId}, _From, State) ->
    Depth = maps:get({Role, ProcId}, State#state.queue_depths, 0),
    {reply, Depth, State};
handle_call({set_limit, Role, Limit}, _From, State) ->
    NewLimits = maps:put(Role, Limit, State#state.limits),
    logger:info("Updated queue limit for ~p: ~p", [Role, Limit]),
    {reply, ok, State#state{limits = NewLimits}};
handle_call({get_limit, Role}, _From, State) ->
    Limit = maps:get(Role, State#state.limits, maps:get(default, State#state.limits)),
    {reply, Limit, State};
handle_call(get_all_limits, _From, State) ->
    {reply, State#state.limits, State};
handle_call({get_role_stats, Role}, _From, State) ->
    case maps:get(Role, State#state.stats, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Stats ->
            {reply, Stats, State}
    end;
handle_call(get_all_stats, _From, State) ->
    {reply, State#state.stats, State};
handle_call({reset_stats, Role}, _From, State) ->
    NewStats = maps:put(Role, init_role_stats(Role), State#state.stats),
    {reply, ok, State#state{stats = NewStats}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({record_enqueue, Role, ProcId}, State) ->
    ProcRole = {Role, ProcId},
    CurrentDepth = maps:get(ProcRole, State#state.queue_depths, 0),
    NewDepth = CurrentDepth + 1,
    NewQueueDepths = maps:put(ProcRole, NewDepth, State#state.queue_depths),

    %% Update statistics
    NewStats = update_enqueue_stats(Role, NewDepth, State#state.stats),

    %% Monitor process if not already monitored
    NewMonitors =
        case is_pid(ProcId) of
            true ->
                case lists:member(ProcRole, maps:values(State#state.monitors)) of
                    false ->
                        MonitorRef = monitor(process, ProcId),
                        maps:put(MonitorRef, ProcRole, State#state.monitors);
                    true ->
                        State#state.monitors
                end;
            false ->
                State#state.monitors
        end,

    {noreply,
     State#state{queue_depths = NewQueueDepths,
                 stats = NewStats,
                 monitors = NewMonitors}};
handle_cast({record_dequeue, Role, ProcId}, State) ->
    ProcRole = {Role, ProcId},
    CurrentDepth = maps:get(ProcRole, State#state.queue_depths, 0),
    NewDepth = max(0, CurrentDepth - 1),
    NewQueueDepths = maps:put(ProcRole, NewDepth, State#state.queue_depths),

    %% Update statistics
    NewStats = update_dequeue_stats(Role, State#state.stats),

    {noreply, State#state{queue_depths = NewQueueDepths, stats = NewStats}};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    %% Clean up queue depth tracking for terminated process
    case maps:get(MonitorRef, State#state.monitors, undefined) of
        undefined ->
            {noreply, State};
        ProcRole ->
            NewQueueDepths = maps:remove(ProcRole, State#state.queue_depths),
            NewMonitors = maps:remove(MonitorRef, State#state.monitors),
            {noreply, State#state{queue_depths = NewQueueDepths, monitors = NewMonitors}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{monitors = Monitors}) ->
    %% Clean up all monitors
    maps:foreach(fun(MonitorRef, _ProcRole) -> demonitor(MonitorRef, [flush]) end, Monitors),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Initialize statistics for a role
init_role_stats(Role) ->
    #{role => Role,
      current_depth => 0,
      max_depth => 0,
      total_enqueued => 0,
      total_dequeued => 0,
      total_rejected => 0,
      capacity_exceeded_count => 0,
      last_capacity_exceeded => undefined}.

%% @doc Update enqueue statistics
update_enqueue_stats(Role, NewDepth, Stats) ->
    RoleStats = maps:get(Role, Stats, init_role_stats(Role)),
    UpdatedStats =
        RoleStats#{current_depth => NewDepth,
                   max_depth => max(maps:get(max_depth, RoleStats), NewDepth),
                   total_enqueued => maps:get(total_enqueued, RoleStats) + 1},
    maps:put(Role, UpdatedStats, Stats).

%% @doc Update dequeue statistics
update_dequeue_stats(Role, Stats) ->
    RoleStats = maps:get(Role, Stats, init_role_stats(Role)),
    UpdatedStats =
        RoleStats#{total_dequeued => maps:get(total_dequeued, RoleStats) + 1,
                   current_depth => max(0, maps:get(current_depth, RoleStats) - 1)},
    maps:put(Role, UpdatedStats, Stats).

%% @doc Record capacity rejection
record_rejection(Role, Stats) ->
    RoleStats = maps:get(Role, Stats, init_role_stats(Role)),
    UpdatedStats =
        RoleStats#{total_rejected => maps:get(total_rejected, RoleStats) + 1,
                   capacity_exceeded_count => maps:get(capacity_exceeded_count, RoleStats) + 1,
                   last_capacity_exceeded => erlang:system_time(millisecond)},
    maps:put(Role, UpdatedStats, Stats).
