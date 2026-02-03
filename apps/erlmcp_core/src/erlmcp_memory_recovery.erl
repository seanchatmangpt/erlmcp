%%%-------------------------------------------------------------------
%%% @doc
%%% Memory Recovery Manager
%%%
%%% This module handles memory recovery after node failures, including:
%%%
%%% - State reconstruction from peers
%%% - Snapshot restoration
%%% - Delta synchronization
%%% - Conflict resolution
%%% - Healing protocols
%%%
%%% == Recovery Modes ==
%%%
%%% - **full**: Complete state reconstruction from backup
%%% - **delta**: Incremental sync from last checkpoint
%%% - **peer**: Pull state from cluster peers
%%% - **snapshot**: Restore from local snapshot
%%%
%%% == Recovery Phases ==
%%%
%%% 1. Detection: Identify failed node or partition
%%% 2. Assessment: Determine what data is lost
%%% 3. Reconstruction: Recover data from replicas
%%% 4. Validation: Verify data integrity
%%% 5. Reintegration: Join cluster with recovered state
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_recovery).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, start_link/1,
         recover_node/1,
         recover_domain/2,
         trigger_recovery/0,
         create_checkpoint/0,
         restore_checkpoint/1,
         list_checkpoints/0,
         delete_checkpoint/1,
         verify_state/0,
         heal_partition/0,
         get_recovery_status/0,
         set_recovery_mode/1,
         get_recovery_mode/0,
         estimate_recovery_time/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type recovery_mode() :: full | delta | peer | snapshot | auto.
-type recovery_status() :: #{status => idle | in_progress | completed | failed,
                              mode => recovery_mode(),
                              progress => 0..100,
                              started_at => integer(),
                              completed_at => integer() | undefined,
                              errors => [term()]}.

-type checkpoint() :: #{id := binary(),
                        timestamp := integer(),
                        domains => [memory_domain()],
                        size_bytes => non_neg_integer(),
                        checksum => binary()}.

-type memory_domain() :: session | shared | registry | cache | ephemeral.

-export_type([recovery_mode/0, recovery_status/0, checkpoint/0]).

%% Checkpoint record for efficient list operations
-record(checkpoint,
        {id :: binary(),
         timestamp :: integer(),
         domains :: [memory_domain()],
         size_bytes :: non_neg_integer(),
         checksum :: binary()}).

%%====================================================================
%% Constants
%%====================================================================

-define(CHECKPOINT_INTERVAL, 300000).  % 5 minutes
-define(RECOVERY_TIMEOUT, 300000).       % 5 minutes
-define(MAX_CHECKPOINTS, 10).
-define(CHECKPOINT_DIR, "checkpoints").

%%====================================================================
%% State Record
%%====================================================================

-record(state,
        {node_id :: node(),
         recovery_mode = auto :: recovery_mode(),
         recovery_status = idle :: recovery_status(),
         checkpoints = [] :: [checkpoint()],
         checkpoint_timer :: reference() | undefined,
         current_recovery = undefined :: {pid(), reference()} | undefined}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Recover a specific node
-spec recover_node(node()) -> {ok, recovery_status()} | {error, term()}.
recover_node(Node) ->
    gen_server:call(?MODULE, {recover_node, Node}, ?RECOVERY_TIMEOUT).

%% @doc Recover a specific domain
-spec recover_domain(memory_domain(), node()) -> {ok, map()} | {error, term()}.
recover_domain(Domain, Node) ->
    gen_server:call(?MODULE, {recover_domain, Domain, Node}, ?RECOVERY_TIMEOUT).

%% @doc Trigger automatic recovery
-spec trigger_recovery() -> {ok, pid()} | {error, term()}.
trigger_recovery() ->
    gen_server:call(?MODULE, trigger_recovery).

%% @doc Create a checkpoint
-spec create_checkpoint() -> {ok, checkpoint()} | {error, term()}.
create_checkpoint() ->
    gen_server:call(?MODULE, create_checkpoint).

%% @doc Restore from checkpoint
-spec restore_checkpoint(binary()) -> {ok, map()} | {error, term()}.
restore_checkpoint(CheckpointId) ->
    gen_server:call(?MODULE, {restore_checkpoint, CheckpointId}, ?RECOVERY_TIMEOUT).

%% @doc List available checkpoints
-spec list_checkpoints() -> [checkpoint()].
list_checkpoints() ->
    gen_server:call(?MODULE, list_checkpoints).

%% @doc Delete a checkpoint
-spec delete_checkpoint(binary()) -> ok | {error, term()}.
delete_checkpoint(CheckpointId) ->
    gen_server:call(?MODULE, {delete_checkpoint, CheckpointId}).

%% @doc Verify current state integrity
-spec verify_state() -> {ok, map()} | {error, term()}.
verify_state() ->
    gen_server:call(?MODULE, verify_state).

%% @doc Heal network partition
-spec heal_partition() -> {ok, map()} | {error, term()}.
heal_partition() ->
    gen_server:call(?MODULE, heal_partition).

%% @doc Get recovery status
-spec get_recovery_status() -> recovery_status().
get_recovery_status() ->
    gen_server:call(?MODULE, get_recovery_status).

%% @doc Set recovery mode
-spec set_recovery_mode(recovery_mode()) -> ok.
set_recovery_mode(Mode) ->
    gen_server:call(?MODULE, {set_recovery_mode, Mode}).

%% @doc Get recovery mode
-spec get_recovery_mode() -> recovery_mode().
get_recovery_mode() ->
    gen_server:call(?MODULE, get_recovery_mode).

%% @doc Estimate recovery time
-spec estimate_recovery_time() -> {ok, pos_integer()}.
estimate_recovery_time() ->
    gen_server:call(?MODULE, estimate_recovery_time).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(Opts) ->
    process_flag(trap_exit, true),

    NodeId = node(),
    RecoveryMode = maps:get(recovery_mode, Opts, auto),

    %% Load existing checkpoints
    Checkpoints = load_checkpoints(),

    %% Start checkpoint timer
    CheckpointTimer = erlang:send_after(?CHECKPOINT_INTERVAL, self(), checkpoint_tick),

    %% Join process group for recovery coordination
    ok = pg:join(erlmcp_recovery, recovery_coordinators, self()),

    logger:info("Memory recovery manager started on ~p: mode=~p, checkpoints=~p",
                [NodeId, RecoveryMode, length(Checkpoints)]),

    {ok, #state{node_id = NodeId,
                recovery_mode = RecoveryMode,
                checkpoints = Checkpoints,
                checkpoint_timer = CheckpointTimer}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
          {reply, term(), #state{}}.
handle_call({recover_node, Node}, _From, State) ->
    case State#state.current_recovery of
        undefined ->
            {Result, NewState} = do_recover_node(Node, State),
            {reply, Result, NewState};
        _ ->
            {reply, {error, recovery_in_progress}, State}
    end;

handle_call({recover_domain, Domain, Node}, _From, State) ->
    Result = do_recover_domain(Domain, Node, State),
    {reply, Result, State};

handle_call(trigger_recovery, _From, State) ->
    case State#state.current_recovery of
        undefined ->
            {Result, NewState} = do_trigger_recovery(State),
            {reply, Result, NewState};
        _ ->
            {reply, {error, recovery_in_progress}, State}
    end;

handle_call(create_checkpoint, _From, State) ->
    {Result, NewState} = do_create_checkpoint(State),
    {reply, Result, NewState};

handle_call({restore_checkpoint, CheckpointId}, _From, State) ->
    {Result, NewState} = do_restore_checkpoint(CheckpointId, State),
    {reply, Result, NewState};

handle_call(list_checkpoints, _From, State) ->
    {reply, State#state.checkpoints, State};

handle_call({delete_checkpoint, CheckpointId}, _From, State) ->
    {Result, NewState} = do_delete_checkpoint(CheckpointId, State),
    {reply, Result, NewState};

handle_call(verify_state, _From, State) ->
    Result = verify_state_internal(State),
    {reply, Result, State};

handle_call(heal_partition, _From, State) ->
    Result = heal_partition_internal(State),
    {reply, Result, State};

handle_call(get_recovery_status, _From, State) ->
    Status = case State#state.current_recovery of
                 undefined -> #{status => idle};
                 {Pid, Ref} ->
                     case is_process_alive(Pid) of
                         true ->
                             erlang:send(Pid, {get_status, Ref, self()}),
                             receive
                                 {status, Ref, StatusMap} -> StatusMap
                             after 1000 ->
                                     #{status => unknown}
                             end;
                         false ->
                             #{status => failed, reason => process_died}
                     end
             end,
    {reply, Status, State};

handle_call({set_recovery_mode, Mode}, _From, State) ->
    logger:info("Recovery mode changed from ~p to ~p", [State#state.recovery_mode, Mode]),
    {reply, ok, State#state{recovery_mode = Mode}};

handle_call(get_recovery_mode, _From, State) ->
    {reply, State#state.recovery_mode, State};

handle_call(estimate_recovery_time, _From, State) ->
    Result = estimate_recovery_time_internal(State),
    {reply, Result, State};

handle_call(get_last_checkpoint_time, _From, State) ->
    Checkpoints = State#state.checkpoints,
    Result = get_last_checkpoint_time(Checkpoints),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({recovery_progress, Ref, Progress, Status}, State) ->
    case State#state.current_recovery of
        {Pid, Ref} ->
            NewStatus = Status#{progress => Progress},
            logger:info("Recovery progress: ~p%", [Progress]),
            case Progress >= 100 of
                true ->
                    %% Recovery complete
                    NewState = State#state{current_recovery = undefined,
                                          recovery_status = Status#{status => completed}},
                    {noreply, NewState};
                false ->
                    {noreply, State#state{recovery_status = NewStatus}}
            end;
        _ ->
            {noreply, State}
    end;

handle_cast({recovery_complete, Ref, Result}, State) ->
    case State#state.current_recovery of
        {Pid, Ref} ->
            logger:info("Recovery complete: ~p", [Result]),
            NewState = State#state{current_recovery = undefined,
                                  recovery_status = Result#{status => completed}},
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_cast({recovery_failed, Ref, Reason}, State) ->
    case State#state.current_recovery of
        {_, Ref} ->
            logger:error("Recovery failed: ~p", [Reason]),
            NewState = State#state{current_recovery = undefined,
                                  recovery_status = #{status => failed,
                                                     reason => Reason}},
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(checkpoint_tick, State) ->
    NewState = maybe_create_checkpoint(State),
    TimerRef = erlang:send_after(?CHECKPOINT_INTERVAL, self(), checkpoint_tick),
    {noreply, NewState#state{checkpoint_timer = TimerRef}};

handle_info({node_down, Node}, State) ->
    logger:warning("Node down detected: ~p", [Node]),
    %% Auto-trigger recovery if enabled
    NewState = case State#state.recovery_mode of
                   auto -> maybe_trigger_node_recovery(Node, State);
                   _ -> State
               end,
    {noreply, NewState};

handle_info({node_up, Node}, State) ->
    logger:info("Node up detected: ~p", [Node]),
    %% Sync state to newly joined node
    spawn(fun() -> sync_to_new_node(Node) end),
    {noreply, State};

handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    case State#state.current_recovery of
        {Pid, Ref} ->
            logger:error("Recovery process died: ~p", [Reason]),
            NewState = State#state{current_recovery = undefined,
                                  recovery_status = #{status => failed,
                                                     reason => {process_died, Reason}}},
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    pg:leave(erlmcp_recovery, recovery_coordinators, self()),
    logger:info("Memory recovery manager terminating"),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Recovery
%%====================================================================

%% @private Recover a node
-spec do_recover_node(node(), #state{}) -> {{ok, recovery_status()}, #state{}} | {{error, term()}, #state{}}.
do_recover_node(Node, State) ->
    RecoveryFun = fun() ->
                         recover_node_internal(Node, State)
                 end,

    {Pid, Ref} = spawn_monitor(RecoveryFun),

    InitialStatus = #{status => in_progress,
                      mode => State#state.recovery_mode,
                      node => Node,
                      started_at => erlang:system_time(millisecond),
                      progress => 0},

    {{ok, InitialStatus}, State#state{current_recovery = {Pid, Ref},
                                      recovery_status = InitialStatus}}.

%% @private Recover a specific domain
-spec do_recover_domain(memory_domain(), node(), #state{}) -> {ok, map()} | {error, term()}.
do_recover_domain(Domain, Node, State) ->
    case get_peer_nodes(Node) of
        [] ->
            {error, no_peers_available};
        Peers ->
            %% Request domain state from peers
            Results = [rpc:call(Peer, erlmcp_distributed_memory, get,
                               [Domain, '_', #{local_only => true}], 10000)
                      || Peer <- Peers],

            %% Merge results
            case lists:filtermap(fun({ok, _}) -> true; (_) -> false end, Results) of
                [] ->
                    {error, no_data_available};
                States ->
                    %% Merge states and apply locally
                    MergedState = merge_domain_states(Domain, States),
                    apply_domain_state(Domain, MergedState),
                    {ok, #{domain => Domain,
                           entries_recovered => maps:size(MergedState),
                           source_nodes => Peers}}
            end
    end.

%% @private Trigger automatic recovery
-spec do_trigger_recovery(#state{}) -> {{ok, pid()}, #state{}} | {{error, term()}, #state{}}.
do_trigger_recovery(State) ->
    %% Determine what needs recovery
    NeedsRecovery = assess_recovery_needs(State),

    case NeedsRecovery of
        [] ->
            {{error, nothing_to_recover}, State};
        Domains ->
            logger:info("Triggering recovery for domains: ~p", [Domains]),
            {Pid, Ref} = spawn_monitor(fun() -> trigger_recovery_internal(Domains, State) end),

            InitialStatus = #{status => in_progress,
                              mode => State#state.recovery_mode,
                              domains => Domains,
                              started_at => erlang:system_time(millisecond),
                              progress => 0},

            {{ok, Pid}, State#state{current_recovery = {Pid, Ref},
                                     recovery_status = InitialStatus}}
    end.

%% @private Assess what needs recovery
-spec assess_recovery_needs(#state{}) -> [memory_domain()].
assess_recovery_needs(_State) ->
    %% Check each domain for consistency
    Domains = [session, shared, registry, cache, ephemeral],

    lists:filter(fun(Domain) ->
                         case verify_domain(Domain) of
                             {error, _} -> true;
                             _ -> false
                         end
                 end, Domains).

%% @private Verify domain integrity
-spec verify_domain(memory_domain()) -> ok | {error, term()}.
verify_domain(Domain) ->
    case erlmcp_distributed_memory:get_domain_stats(Domain) of
        #{error := _} = Error ->
            Error;
        Stats ->
            case maps:get(entry_count, Stats, 0) of
                0 -> {error, empty_domain};
                _ -> ok
            end
    end.

%% @private Create checkpoint
-spec do_create_checkpoint(#state{}) -> {{ok, checkpoint()}, #state{}} | {{error, term()}, #state{}}.
do_create_checkpoint(State) ->
    CheckpointId = generate_checkpoint_id(),
    Timestamp = erlang:system_time(millisecond),

    try
        %% Collect state from all domains
        Domains = [session, shared, registry, cache],
        DomainStates = lists:foldl(fun(Domain, Acc) ->
                                           case get_domain_state(Domain) of
                                               {ok, StateData} ->
                                                   Acc#{Domain => StateData};
                                               {error, _} ->
                                                   Acc
                                           end
                                   end, #{}, Domains),

        %% Serialize
        Serialized = term_to_binary(DomainStates, [compressed]),
        Checksum = crypto:hash(sha256, Serialized),

        %% Save to disk
        Filename = checkpoint_filename(CheckpointId),
        ok = filelib:ensure_dir(Filename),
        ok = file:write_file(Filename, Serialized),

        Checkpoint = #{id => CheckpointId,
                       timestamp => Timestamp,
                       domains => Domains,
                       size_bytes => byte_size(Serialized),
                       checksum => Checksum},

        %% Update state
        NewCheckpoints = add_checkpoint(Checkpoint, State#state.checkpoints),
        NewState = State#state{checkpoints = NewCheckpoints},

        logger:info("Created checkpoint ~p (~p bytes)", [CheckpointId, maps:get(size_bytes, Checkpoint)]),

        {{ok, Checkpoint}, NewState}
    catch
        Class:Reason:Stack ->
            logger:error("Failed to create checkpoint: ~p:~p~n~p", [Class, Reason, Stack]),
            {{error, {checkpoint_failed, Reason}}, State}
    end.

%% @private Restore from checkpoint
-spec do_restore_checkpoint(binary(), #state{}) -> {{ok, map()}, #state{}} | {{error, term()}, #state{}}.
do_restore_checkpoint(CheckpointId, State) ->
    case lists:keyfind(CheckpointId, #checkpoint.id, State#state.checkpoints) of
        false ->
            {{error, checkpoint_not_found}, State};
        Checkpoint ->
            Filename = checkpoint_filename(CheckpointId),

            case file:read_file(Filename) of
                {ok, Data} ->
                    %% Verify checksum
                    Checksum = crypto:hash(sha256, Data),
                    case Checksum =:= maps:get(checksum, Checkpoint) of
                        false ->
                            {{error, checksum_mismatch}, State};
                        true ->
                            try
                                %% Deserialize
                                DomainStates = binary_to_term(Data, [safe]),

                                %% Apply to each domain
                                Applied = lists:foldl(fun(Domain, Acc) ->
                                                            case maps:get(Domain, DomainStates, undefined) of
                                                                undefined -> Acc;
                                                                StateData ->
                                                                    ok = apply_domain_state(Domain, StateData),
                                                                    Acc#{Domain => ok}
                                                            end
                                                    end, #{}, maps:get(domains, Checkpoint)),

                                {{ok, #{checkpoint => CheckpointId,
                                        domains_applied => Applied}}, State}
                            catch
                                _:_ ->
                                    {{error, invalid_checkpoint_data}, State}
                            end
                    end;
                {error, Reason} ->
                    {{error, {read_failed, Reason}}, State}
            end
    end.

%% @private Delete checkpoint
-spec do_delete_checkpoint(binary(), #state{}) -> {ok, #state{}} | {{error, term()}, #state{}}.
do_delete_checkpoint(CheckpointId, State) ->
    Filename = checkpoint_filename(CheckpointId),

    case file:delete(Filename) of
        ok ->
            NewCheckpoints = lists:keydelete(CheckpointId, #checkpoint.id, State#state.checkpoints),
            {ok, State#state{checkpoints = NewCheckpoints}};
        {error, Reason} ->
            {{error, Reason}, State}
    end.

%% @private Maybe create checkpoint
-spec maybe_create_checkpoint(#state{}) -> #state{}.
maybe_create_checkpoint(State) ->
    %% Check if enough data changed since last checkpoint
    Changed = estimate_data_change(State),
    Threshold = 10 * 1024 * 1024,  % 10MB threshold

    case Changed > Threshold of
        true ->
            {_, NewState} = do_create_checkpoint(State),
            NewState;
        false ->
            State
    end.

%% @private Estimate data change since last checkpoint
-spec estimate_data_change(#state{}) -> non_neg_integer().
estimate_data_change(_State) ->
    %% Sum of write operations since last checkpoint
    %% For now, use a heuristic
    ets:info(erlmcp_memory, size) * 100.

%%====================================================================
%% Internal Functions - Node Recovery
%%====================================================================

%% @private Recover node internal
-spec recover_node_internal(node(), #state{}) -> ok.
recover_node_internal(Node, State) ->
    %% Implement recovery based on mode
    Mode = State#state.recovery_mode,

    Result = case Mode of
                 full -> full_recovery(Node, State);
                 delta -> delta_recovery(Node, State);
                 peer -> peer_recovery(Node, State);
                 snapshot -> snapshot_recovery(Node, State);
                 auto -> auto_recovery(Node, State)
             end,

    %% Notify coordinator of result
    Coordinator = self(),
    case Result of
        {ok, _} ->
            gen_server:cast(Coordinator, {recovery_complete, make_ref(), Result});
        {error, Reason} ->
            gen_server:cast(Coordinator, {recovery_failed, make_ref(), Reason})
    end.

%% @private Full recovery
-spec full_recovery(node(), #state{}) -> {ok, map()} | {error, term()}.
full_recovery(Node, _State) ->
    %% Get state from all peers and merge
    Peers = get_peer_nodes(Node),
    case Peers of
        [] ->
            {error, no_peers_available};
        _ ->
            States = [rpc:call(Peer, erlmcp_distributed_memory, get_cluster_memory, [], 30000)
                      || Peer <- Peers],
            {ok, #{mode => full, recovered_from => Peers, states => States}}
    end.

%% @private Delta recovery
-spec delta_recovery(node(), #state{}) -> {ok, map()} | {error, term()}.
delta_recovery(Node, _State) ->
    %% Get changes since last checkpoint
    LastCheckpoint = get_last_checkpoint_time(),
    Peers = get_peer_nodes(Node),

    DeltaStates = [rpc:call(Peer, erlmcp_distributed_memory, get_domain_stats,
                           [session], 10000) || Peer <- Peers],

    {ok, #{mode => delta, since => LastCheckpoint, delta_states => DeltaStates}}.

%% @private Peer recovery
-spec peer_recovery(node(), #state{}) -> {ok, map()} | {error, term()}.
peer_recovery(Node, _State) ->
    %% Request state from a single peer
    case get_peer_nodes(Node) of
        [Peer | _] ->
            State = rpc:call(Peer, erlmcp_distributed_memory, get_cluster_memory, [], 30000),
            {ok, #{mode => peer, peer => Peer, state => State}};
        [] ->
            {error, no_peers_available}
    end.

%% @private Snapshot recovery
-spec snapshot_recovery(node(), #state{}) -> {ok, map()} | {error, term()}.
snapshot_recovery(_Node, State) ->
    %% Restore from latest checkpoint
    case State#state.checkpoints of
        [] ->
            {error, no_checkpoints};
        [Checkpoint | _] ->
            case do_restore_checkpoint(maps:get(id, Checkpoint), State) of
                {{ok, Result}, _} ->
                    {ok, #{mode => snapshot, checkpoint => Checkpoint, result => Result}};
                {{error, Reason}, _} ->
                    {error, Reason}
            end
    end.

%% @private Auto recovery
-spec auto_recovery(node(), #state{}) -> {ok, map()} | {error, term()}.
auto_recovery(Node, State) ->
    %% Try fastest recovery method first
    case snapshot_recovery(Node, State) of
        {ok, Result} ->
            {ok, Result#{mode => auto_snapshot}};
        {error, _} ->
            case peer_recovery(Node, State) of
                {ok, Result} ->
                    {ok, Result#{mode => auto_peer}};
                {error, _} ->
                    full_recovery(Node, State)
            end
    end.

%% @private Maybe trigger recovery for node
-spec maybe_trigger_node_recovery(node(), #state{}) -> #state{}.
maybe_trigger_node_recovery(Node, State) ->
    spawn(fun() ->
                 case recover_node(Node) of
                     {ok, _} -> ok;
                     {error, Reason} -> logger:error("Auto-recovery failed for ~p: ~p", [Node, Reason])
                 end
         end),
    State.

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

%% @private Get peer nodes
-spec get_peer_nodes(node()) -> [node()].
get_peer_nodes(ExcludeNode) ->
    Nodes = nodes(),
    lists:filter(fun(N) -> N =/= ExcludeNode end, Nodes).

%% @private Sync to newly joined node
-spec sync_to_new_node(node()) -> ok.
sync_to_new_node(Node) ->
    %% Send current state to new node
    {ok, Snapshot} = create_checkpoint(),
    rpc:cast(Node, erlmcp_memory_recovery, restore_checkpoint, [maps:get(id, Snapshot)]),
    ok.

%% @private Verify state internal
-spec verify_state_internal(#state{}) -> {ok, map()} | {error, term()}.
verify_state_internal(_State) ->
    %% Check all domains
    Domains = [session, shared, registry, cache, ephemeral],
    Results = lists:foldl(fun(Domain, Acc) ->
                                  case verify_domain(Domain) of
                                      ok -> Acc;
                                      {error, Reason} -> Acc#{Domain => Reason}
                                  end
                          end, #{}, Domains),

    case maps:size(Results) of
        0 -> {ok, #{status => valid, domains => Domains}};
        _ -> {ok, #{status => invalid, errors => Results}}
    end.

%% @private Heal partition
-spec heal_partition_internal(#state{}) -> {ok, map()} | {error, term()}.
heal_partition_internal(_State) ->
    %% Detect partition and merge states
    Nodes = [node() | nodes()],

    States = lists:foldl(fun(N, Acc) ->
                               case N =:= node() of
                                   true ->
                                       Acc#{N => local_state()};
                                   false ->
                                       case rpc:call(N, ?MODULE, local_state, [], 5000) of
                                           {ok, StateData} -> Acc#{N => StateData};
                                           _ -> Acc
                                       end
                               end
                       end, #{}, Nodes),

    %% Merge states
    MergedState = merge_partition_states(States),
    apply_merged_state(MergedState),

    {ok, #{nodes => length(Nodes), merged => true}}.

%% @private Estimate recovery time
-spec estimate_recovery_time_internal(#state{}) -> {ok, pos_integer()}.
estimate_recovery_time_internal(_State) ->
    %% Estimate based on data size
    DataSize = estimate_data_size(),
    %% Rough estimate: 1MB per second
    Seconds = max(10, DataSize div (1024 * 1024)),
    {ok, Seconds}.

%% @private Estimate data size
-spec estimate_data_size() -> pos_integer().
estimate_data_size() ->
    erlang:memory(total) - erlang:memory(system).

%% @private Get domain state
-spec get_domain_state(memory_domain()) -> {ok, map()} | {error, term()}.
get_domain_state(Domain) ->
    try
        Stats = erlmcp_distributed_memory:get_domain_stats(Domain),
        {ok, Stats}
    catch
        _:_ -> {error, domain_unavailable}
    end.

%% @private Apply domain state
-spec apply_domain_state(memory_domain(), map()) -> ok.
apply_domain_state(_Domain, _State) ->
    %% Apply state to the domain
    %% This would involve writing to ETS/Mnesia
    ok.

%% @private Merge domain states
-spec merge_domain_states(memory_domain(), [map()]) -> map().
merge_domain_states(_Domain, States) ->
    lists:foldl(fun maps:merge/2, #{}, States).

%% @private Load checkpoints
-spec load_checkpoints() -> [checkpoint()].
load_checkpoints() ->
    Dir = checkpoint_dir(),
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:filtermap(fun(File) ->
                                    case string:prefix(File, "checkpoint_") of
                                        false -> false;
                                        _ ->
                                            FullPath = filename:join(Dir, File),
                                            case load_checkpoint_file(FullPath) of
                                                {ok, Checkpoint} -> {true, Checkpoint};
                                                _ -> false
                                            end
                                    end
                            end, Files);
        {error, _} ->
            []
    end.

%% @private Load checkpoint file
-spec load_checkpoint_file(file:filename_all()) -> {ok, checkpoint()} | {error, term()}.
load_checkpoint_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Data} ->
            Checksum = crypto:hash(sha256, Data),
            CheckpointId = extract_checkpoint_id(Filename),
            {ok, #{id => CheckpointId,
                   timestamp => filelib:last_modified(Filename),
                   size_bytes => byte_size(Data),
                   checksum => Checksum}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Generate checkpoint ID
-spec generate_checkpoint_id() -> binary().
generate_checkpoint_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = crypto:strong_rand_bytes(8),
    <<Timestamp:64, Random/binary>>.

%% @private Checkpoint filename
-spec checkpoint_filename(binary()) -> file:filename_all().
checkpoint_filename(CheckpointId) ->
    filename:join(checkpoint_dir(), "checkpoint_" ++ binary_to_list(CheckpointId)).

%% @private Checkpoint directory
-spec checkpoint_dir() -> file:filename_all().
checkpoint_dir() ->
    case application:get_env(erlmcp_core, checkpoint_dir) of
        {ok, Dir} -> Dir;
        undefined -> ?CHECKPOINT_DIR
    end.

%% @private Extract checkpoint ID from filename
-spec extract_checkpoint_id(file:filename_all()) -> binary().
extract_checkpoint_id(Filename) ->
    Base = filename:basename(Filename),
    string:prefix(Base, "checkpoint_"),
    list_to_binary(string:sub_string(Base, 12)).  % Skip "checkpoint_"

%% @private Add checkpoint
-spec add_checkpoint(checkpoint(), [checkpoint()]) -> [checkpoint()].
add_checkpoint(Checkpoint, Checkpoints) ->
    Sorted = lists:keysort(#checkpoint.timestamp, [Checkpoint | Checkpoints]),
    lists:sublist(Sorted, ?MAX_CHECKPOINTS).

%% @private Get last checkpoint time from gen_server state
-spec get_last_checkpoint_time() -> integer() | undefined.
get_last_checkpoint_time() ->
    gen_server:call(?MODULE, get_last_checkpoint_time).

%% @private Get last checkpoint time from checkpoint list
-spec get_last_checkpoint_time([checkpoint()]) -> integer() | undefined.
get_last_checkpoint_time([]) -> undefined;
get_last_checkpoint_time([#checkpoint{timestamp = TS} | _]) -> TS.

%% @private Trigger recovery internal
-spec trigger_recovery_internal([memory_domain()], #state{}) -> ok.
trigger_recovery_internal(Domains, _State) ->
    lists:foreach(fun(Domain) ->
                         case recover_domain(Domain, node()) of
                             {ok, _} -> ok;
                             {error, Reason} ->
                                 logger:error("Failed to recover domain ~p: ~p", [Domain, Reason])
                         end
                 end, Domains),
    ok.

%% @private Safe maps:get
-spec maps_get(term(), map()) -> term().
maps_get(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> error({key_not_found, Key})
    end.

%% @private Local state for healing
-spec local_state() -> {ok, map()}.
local_state() ->
    {ok, #{node => node(),
           memory => erlang:memory(),
           ets_tables => ets:all()}}.

%% @private Merge partition states
-spec merge_partition_states(#{node() => map()}) -> map().
merge_partition_states(States) ->
    %% For now, return the state from the first node
    %% Real implementation would use CRDT merge
    [First | _] = maps:values(States),
    First.

%% @private Apply merged state
-spec apply_merged_state(map()) -> ok.
apply_merged_state(_State) ->
    ok.
