%%%-------------------------------------------------------------------
%%% @doc
%%% Session Failover and Recovery Manager
%%%
%%% Handles automatic failover when nodes become unavailable:
%%% - Monitors replica node health
%%% - Detects node failures within 5 seconds
%%% - Promotes secondary nodes to primary
%%% - Restores session state from replicated data
%%% - Coordinates failover across the cluster
%%% - Tracks failover metrics and recovery time
%%%
%%% Recovery sequence:
%%% 1. Health check fails on primary node
%%% 2. Secondary node detects failure
%%% 3. Secondary loads replicated sessions into its ETS
%%% 4. Secondary becomes primary and accepts writes
%%% 5. Failback to original primary when recovered
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_failover).
-behaviour(gen_server).

%% Public API
-export([
    start_link/1,
    get_failover_status/0,
    trigger_manual_failover/1,
    monitor_node/1,
    unmonitor_node/1,
    get_recovery_time/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").
-include("erlmcp.hrl").

%% Constants
-define(SERVER, ?MODULE).
-define(HEALTH_CHECK_INTERVAL_MS, 1000).
-define(HEALTH_CHECK_TIMEOUT_MS, 2000).
-define(FAILURE_THRESHOLD, 3).  % Fail after 3 consecutive failures
-define(RECOVERY_ATTEMPT_INTERVAL_MS, 10000).
-define(SESSION_REPLICATOR, erlmcp_session_replicator).

%% State record
-record(failover_state, {
    primary_node :: node(),
    secondary_nodes = [] :: [node()],
    node_health = #{} :: map(),  % node => {consecutive_failures, last_check_time}
    failover_in_progress = false :: boolean(),
    last_failover_time = 0 :: integer(),
    recovery_start_time = 0 :: integer(),
    failover_count = 0 :: non_neg_integer(),
    health_check_timers = #{} :: map(),  % node => timer_ref
    recovery_timers = #{} :: map()  % node => timer_ref
}).

%%===================================================================
%% Public API
%%===================================================================

-spec start_link([node()]) -> {ok, pid()} | {error, term()}.
start_link(Nodes) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Nodes], []).

-spec get_failover_status() -> map().
get_failover_status() ->
    gen_server:call(?SERVER, get_failover_status, 5000).

-spec trigger_manual_failover(node()) -> ok | {error, term()}.
trigger_manual_failover(Node) ->
    gen_server:call(?SERVER, {manual_failover, Node}, 10000).

-spec monitor_node(node()) -> ok.
monitor_node(Node) ->
    gen_server:cast(?SERVER, {monitor_node, Node}).

-spec unmonitor_node(node()) -> ok.
unmonitor_node(Node) ->
    gen_server:cast(?SERVER, {unmonitor_node, Node}).

-spec get_recovery_time() -> non_neg_integer().
get_recovery_time() ->
    gen_server:call(?SERVER, get_recovery_time, 5000).

%%===================================================================
%% gen_server Callbacks
%%===================================================================

init([Nodes]) ->
    logger:info("Initializing failover manager with nodes: ~w", [Nodes]),

    PrimaryNode = node(),
    SecondaryNodes = lists:delete(PrimaryNode, Nodes),

    State = #failover_state{
        primary_node = PrimaryNode,
        secondary_nodes = SecondaryNodes,
        node_health = initialize_node_health(Nodes)
    },

    %% Start health checks for all nodes
    NewState = start_health_checks(State),

    {ok, NewState}.

handle_call(get_failover_status, _From, State) ->
    Status = #{
        primary_node => State#failover_state.primary_node,
        secondary_nodes => State#failover_state.secondary_nodes,
        failover_in_progress => State#failover_state.failover_in_progress,
        last_failover_time => State#failover_state.last_failover_time,
        failover_count => State#failover_state.failover_count,
        recovery_start_time => State#failover_state.recovery_start_time,
        node_health => State#failover_state.node_health
    },
    {reply, Status, State};

handle_call({manual_failover, TargetNode}, _From, State) ->
    case State#failover_state.failover_in_progress of
        true ->
            {reply, {error, failover_in_progress}, State};
        false ->
            logger:warning("Manual failover to ~p requested", [TargetNode]),
            NewState = execute_failover(TargetNode, State),
            {reply, ok, NewState}
    end;

handle_call(get_recovery_time, _From, State) ->
    RecoveryTime = case State#failover_state.recovery_start_time of
        0 -> 0;
        Start -> erlang:system_time(millisecond) - Start
    end,
    {reply, RecoveryTime, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({monitor_node, Node}, State) ->
    NewHealth = maps:put(Node, {0, 0}, State#failover_state.node_health),
    NewState = State#failover_state{node_health = NewHealth},
    FinalState = start_health_check_for_node(Node, NewState),
    {noreply, FinalState};

handle_cast({unmonitor_node, Node}, State) ->
    %% Cancel timer if exists
    NewTimers = case maps:get(Node, State#failover_state.health_check_timers, undefined) of
        undefined -> State#failover_state.health_check_timers;
        Timer ->
            erlang:cancel_timer(Timer),
            maps:remove(Node, State#failover_state.health_check_timers)
    end,
    NewHealth = maps:remove(Node, State#failover_state.node_health),
    {noreply, State#failover_state{
        health_check_timers = NewTimers,
        node_health = NewHealth
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({health_check, Node}, State) ->
    NewState = check_node_health(Node, State),
    {noreply, NewState};

handle_info({recovery_attempt, Node}, State) ->
    logger:info("Attempting recovery of node ~p", [Node]),
    case is_node_healthy(Node) of
        true ->
            logger:info("Node ~p recovered, scheduling failback", [Node]),
            NewState = schedule_failback(Node, State),
            {noreply, NewState};
        false ->
            %% Reschedule recovery attempt
            Timer = erlang:send_after(?RECOVERY_ATTEMPT_INTERVAL_MS, self(), {recovery_attempt, Node}),
            NewTimers = maps:put(Node, Timer, State#failover_state.recovery_timers),
            {noreply, State#failover_state{recovery_timers = NewTimers}}
    end;

handle_info({failover_timeout, Node}, State) ->
    logger:error("Failover timeout for node ~p", [Node]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel all timers
    maps:foreach(fun(_Node, Timer) ->
        erlang:cancel_timer(Timer)
    end, State#failover_state.health_check_timers),

    maps:foreach(fun(_Node, Timer) ->
        erlang:cancel_timer(Timer)
    end, State#failover_state.recovery_timers),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal Functions
%%===================================================================

%% @doc Initialize node health tracking
-spec initialize_node_health([node()]) -> map().
initialize_node_health(Nodes) ->
    maps:from_list([{Node, {0, erlang:system_time(millisecond)}} || Node <- Nodes]).

%% @doc Start health checks for all nodes
-spec start_health_checks(#failover_state{}) -> #failover_state{}.
start_health_checks(State) ->
    AllNodes = [State#failover_state.primary_node | State#failover_state.secondary_nodes],
    Timers = maps:from_list([
        {Node, erlang:send_after(?HEALTH_CHECK_INTERVAL_MS, self(), {health_check, Node})}
        || Node <- AllNodes
    ]),
    State#failover_state{health_check_timers = Timers}.

%% @doc Start health check for a single node
-spec start_health_check_for_node(node(), #failover_state{}) -> #failover_state{}.
start_health_check_for_node(Node, State) ->
    Timer = erlang:send_after(?HEALTH_CHECK_INTERVAL_MS, self(), {health_check, Node}),
    NewTimers = maps:put(Node, Timer, State#failover_state.health_check_timers),
    State#failover_state{health_check_timers = NewTimers}.

%% @doc Check if a node is healthy
-spec is_node_healthy(node()) -> boolean().
is_node_healthy(Node) ->
    case rpc:call(Node, erlang, node, [], ?HEALTH_CHECK_TIMEOUT_MS) of
        Node -> true;
        _ -> false
    end.

%% @doc Check node health and handle failures
-spec check_node_health(node(), #failover_state{}) -> #failover_state{}.
check_node_health(Node, State) ->
    case is_node_healthy(Node) of
        true ->
            %% Node is healthy - reset failure count
            NewHealth = maps:put(Node, {0, erlang:system_time(millisecond)}, State#failover_state.node_health),
            reschedule_health_check(Node, State#failover_state{node_health = NewHealth});

        false ->
            %% Node failed - increment failure count
            {FailureCount, _LastCheckTime} = maps:get(Node, State#failover_state.node_health, {0, 0}),
            NewFailureCount = FailureCount + 1,
            NewHealth = maps:put(Node, {NewFailureCount, erlang:system_time(millisecond)}, State#failover_state.node_health),

            case NewFailureCount >= ?FAILURE_THRESHOLD of
                true ->
                    logger:warning("Node ~p detected as failed after ~p failures", [Node, NewFailureCount]),
                    %% Trigger failover
                    NewState = State#failover_state{node_health = NewHealth},
                    FinalState = execute_failover(Node, NewState),
                    reschedule_health_check(Node, FinalState);

                false ->
                    logger:warning("Node ~p health check failed (~p/~p)", [Node, NewFailureCount, ?FAILURE_THRESHOLD]),
                    reschedule_health_check(Node, State#failover_state{node_health = NewHealth})
            end
    end.

%% @doc Reschedule health check for a node
-spec reschedule_health_check(node(), #failover_state{}) -> #failover_state{}.
reschedule_health_check(Node, State) ->
    %% Cancel existing timer if any
    case maps:get(Node, State#failover_state.health_check_timers, undefined) of
        undefined -> ok;
        OldTimer -> erlang:cancel_timer(OldTimer)
    end,

    %% Schedule new timer
    Timer = erlang:send_after(?HEALTH_CHECK_INTERVAL_MS, self(), {health_check, Node}),
    NewTimers = maps:put(Node, Timer, State#failover_state.health_check_timers),
    State#failover_state{health_check_timers = NewTimers}.

%% @doc Execute failover to a target node
-spec execute_failover(node(), #failover_state{}) -> #failover_state{}.
execute_failover(FailedNode, State) ->
    logger:critical("Executing failover due to node failure: ~p", [FailedNode]),

    case State#failover_state.secondary_nodes of
        [] ->
            logger:error("No secondary nodes available for failover"),
            State;

        SecondaryNodes ->
            %% Select first healthy secondary node
            TargetNode = select_failover_target(SecondaryNodes),

            case TargetNode of
                undefined ->
                    logger:error("No healthy secondary nodes available"),
                    State;

                _ ->
                    logger:warning("Promoting ~p to primary (failed node: ~p)", [TargetNode, FailedNode]),

                    %% Promote target node to primary
                    case rpc:call(TargetNode, ?SESSION_REPLICATOR, promote_to_primary, [], 5000) of
                        ok ->
                            %% Update state
                            NewPrimary = TargetNode,
                            NewSecondaries = lists:delete(TargetNode, State#failover_state.secondary_nodes) ++ [FailedNode],

                            %% Start recovery tracking
                            RecoveryTimer = erlang:send_after(
                                ?RECOVERY_ATTEMPT_INTERVAL_MS,
                                self(),
                                {recovery_attempt, FailedNode}
                            ),
                            NewRecoveryTimers = maps:put(FailedNode, RecoveryTimer, State#failover_state.recovery_timers),

                            NewState = State#failover_state{
                                primary_node = NewPrimary,
                                secondary_nodes = NewSecondaries,
                                failover_in_progress = false,
                                last_failover_time = erlang:system_time(millisecond),
                                recovery_start_time = erlang:system_time(millisecond),
                                failover_count = State#failover_state.failover_count + 1,
                                recovery_timers = NewRecoveryTimers
                            },

                            logger:info("Failover completed successfully", []),
                            NewState;

                        Error ->
                            logger:error("Failed to promote secondary node: ~p", [Error]),
                            State
                    end
            end
    end.

%% @doc Select a healthy secondary node for failover
-spec select_failover_target([node()]) -> node() | undefined.
select_failover_target([]) ->
    undefined;
select_failover_target(Nodes) ->
    %% Find first healthy node
    case lists:dropwhile(fun(Node) ->
        not is_node_healthy(Node)
    end, Nodes) of
        [] -> undefined;
        [TargetNode | _] -> TargetNode
    end.

%% @doc Schedule failback to original primary when it recovers
-spec schedule_failback(node(), #failover_state{}) -> #failover_state{}.
schedule_failback(RecoveredNode, State) ->
    %% Only failback if recovered node was original primary
    case RecoveredNode =:= State#failover_state.primary_node of
        false ->
            State;
        true ->
            logger:info("Scheduling failback to original primary: ~p", [RecoveredNode]),
            %% Promote original primary back to primary
            case rpc:call(RecoveredNode, ?SESSION_REPLICATOR, promote_to_primary, [], 5000) of
                ok ->
                    NewSecondaries = lists:delete(State#failover_state.primary_node, State#failover_state.secondary_nodes),
                    State#failover_state{
                        primary_node = RecoveredNode,
                        secondary_nodes = [node() | NewSecondaries],
                        recovery_start_time = 0
                    };
                Error ->
                    logger:error("Failback failed: ~p", [Error]),
                    State
            end
    end.
