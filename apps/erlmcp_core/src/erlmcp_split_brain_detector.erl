-module(erlmcp_split_brain_detector).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    get_partition_status/0,
    force_resolution/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State record
-record(split_brain_state, {
    strategy = winner_takes_all :: winner_takes_all | oldest_node | configured_master,
    master_node :: node() | undefined,
    partition_detected = false :: boolean(),
    last_check :: integer() | undefined,
    check_interval = 30000 :: pos_integer(),
    check_ref :: reference() | undefined
}).

-type state() :: #split_brain_state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_partition_status() -> #{
    partition_detected => boolean(),
    strategy => atom(),
    master_node => node() | undefined,
    last_check => integer() | undefined
}.
get_partition_status() ->
    gen_server:call(?MODULE, get_partition_status).

-spec force_resolution() -> ok.
force_resolution() ->
    gen_server:cast(?MODULE, force_resolution).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    Strategy = application:get_env(erlmcp_core, split_brain_strategy, winner_takes_all),
    MasterNode = application:get_env(erlmcp_core, master_node, undefined),
    CheckInterval = application:get_env(erlmcp_core, split_brain_check_interval, 30000),

    State = #split_brain_state{
        strategy = Strategy,
        master_node = MasterNode,
        check_interval = CheckInterval
    },

    %% Schedule periodic partition check
    CheckRef = erlang:send_after(CheckInterval, self(), check_partition),

    logger:info("Split-brain detector started (strategy: ~p)", [Strategy]),
    {ok, State#split_brain_state{check_ref = CheckRef}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.

handle_call(get_partition_status, _From, State) ->
    Status = #{
        partition_detected => State#split_brain_state.partition_detected,
        strategy => State#split_brain_state.strategy,
        master_node => State#split_brain_state.master_node,
        last_check => State#split_brain_state.last_check
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast(force_resolution, State) ->
    NewState = detect_and_resolve_partition(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info(check_partition, State) ->
    %% Periodic partition detection
    NewState = detect_and_resolve_partition(State),

    %% Schedule next check
    CheckRef = erlang:send_after(State#split_brain_state.check_interval, self(), check_partition),
    {noreply, NewState#split_brain_state{check_ref = CheckRef}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% Cancel periodic check
    case State#split_brain_state.check_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    logger:info("Split-brain detector terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec detect_and_resolve_partition(state()) -> state().
detect_and_resolve_partition(State) ->
    Now = erlang:system_time(second),
    ClusterNodes = application:get_env(erlmcp_core, cluster_nodes, []),
    ConnectedNodes = nodes(),

    %% Check if we can see all expected nodes
    MissingNodes = ClusterNodes -- ConnectedNodes,

    case MissingNodes of
        [] ->
            %% All nodes connected - no partition
            case State#split_brain_state.partition_detected of
                true ->
                    logger:info("Network partition resolved - all nodes reconnected");
                false ->
                    ok
            end,
            State#split_brain_state{
                partition_detected = false,
                last_check = Now
            };
        _ ->
            %% Network partition detected
            case State#split_brain_state.partition_detected of
                false ->
                    logger:warning("Network partition detected! Missing nodes: ~p", [MissingNodes]),
                    resolve_partition(State#split_brain_state.strategy, MissingNodes, State);
                true ->
                    %% Already in partition mode, just update last_check
                    ok
            end,
            State#split_brain_state{
                partition_detected = true,
                last_check = Now
            }
    end.

-spec resolve_partition(winner_takes_all | oldest_node | configured_master, [node()], state()) -> state().
resolve_partition(winner_takes_all, MissingNodes, State) ->
    %% Strategy: Node with most connections wins
    AllNodes = [node() | nodes()],
    ConnectedCount = length(AllNodes),

    logger:info("Split-brain resolution (winner_takes_all): This partition has ~p nodes, missing ~p nodes",
               [ConnectedCount, length(MissingNodes)]),

    %% Check if we're the larger partition
    TotalNodes = length(application:get_env(erlmcp_core, cluster_nodes, [])) + 1, % +1 for self
    case ConnectedCount > (TotalNodes div 2) of
        true ->
            logger:info("This partition has majority (~p/~p) - remaining master", [ConnectedCount, TotalNodes]),
            State;
        false ->
            logger:warning("This partition is minority (~p/~p) - should stop accepting writes", [ConnectedCount, TotalNodes]),
            State
    end;

resolve_partition(oldest_node, _MissingNodes, State) ->
    %% Strategy: Oldest node (by node creation time) wins
    AllNodes = [node() | nodes()],
    OldestNode = lists:min(AllNodes),

    case node() =:= OldestNode of
        true ->
            logger:info("Split-brain resolution (oldest_node): This node (~p) is oldest - remaining master", [node()]),
            State;
        false ->
            logger:warning("Split-brain resolution (oldest_node): Node ~p is oldest - this node should defer", [OldestNode]),
            State
    end;

resolve_partition(configured_master, _MissingNodes, State) ->
    %% Strategy: Pre-configured master node wins
    case State#split_brain_state.master_node of
        undefined ->
            logger:error("Split-brain resolution (configured_master): No master node configured!"),
            State;
        MasterNode ->
            case node() =:= MasterNode of
                true ->
                    logger:info("Split-brain resolution (configured_master): This node (~p) is configured master", [node()]),
                    State;
                false ->
                    AllNodes = [node() | nodes()],
                    case lists:member(MasterNode, AllNodes) of
                        true ->
                            logger:info("Split-brain resolution (configured_master): Master node ~p is in this partition", [MasterNode]),
                            State;
                        false ->
                            logger:warning("Split-brain resolution (configured_master): Master node ~p is NOT in this partition - should stop", [MasterNode]),
                            State
                    end
            end
    end.
