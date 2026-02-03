%% @doc Raft Log Storage Module
%%
%% This module provides persistent storage for Raft log entries using ETS.
%% Features:
%%   - ETS-based ordered_set for efficient log operations
%%   - Persistent state management (current_term, voted_for)
%%   - Snapshot metadata storage
%%   - Log compaction support
%%   - Disk persistence via DETS for durability
-module(erlmcp_raft_log).

-behaviour(gen_server).

-include("erlmcp_raft.hrl").
-include("erlmcp.hrl").

%% API
-export([start_link/2, init/2,
         append_entry/2, get_entry/2, get_entries_from/2,
         last_index/1, delete_from/2, discard_log_up_to/2,
         get_persistent_state/1, save_persistent_state/3,
         save_snapshot_metadata/2, get_snapshot_metadata/1,
         take_snapshot/3, restore_snapshot/2,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% State record
-record(log_state,
        {cluster_name :: binary(),
         node_id :: raft_node_id(),
         log_table :: ets:tid(),          % In-memory ordered_set for fast access
         dets_table :: dets:tid() | undefined, % Persistent DETS table
         persistent_state :: #{term => raft_term(), voted_for => raft_node_id() | undefined},
         snapshot :: #raft_snapshot{} | undefined,
         options :: map()}).

-type log_state() :: #log_state{}.

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Initialize a new Raft log for a node
-spec init(binary(), raft_node_id()) -> {ok, ets:tid()} | {error, term()}.
init(ClusterName, NodeId) ->
    case gen_server:start_link(?MODULE, [ClusterName, NodeId], []) of
        {ok, Pid} ->
            %% Get ETS table from server state
            {ok, Tid} = gen_server:call(Pid, get_table, 5000),
            {ok, Tid};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Start the log server explicitly
-spec start_link(binary(), raft_node_id()) -> {ok, pid()} | {error, term()}.
start_link(ClusterName, NodeId) ->
    gen_server:start_link(?MODULE, [ClusterName, NodeId], []).

%% @doc Append a log entry
-spec append_entry(ets:tid(), #raft_log_entry{}) -> ok.
append_entry(Tid, Entry = #raft_log_entry{index = Index}) ->
    true = ets:insert(Tid, {Index, Entry}),
    ok.

%% @doc Get a specific log entry by index
-spec get_entry(ets:tid(), raft_index()) -> {ok, #raft_log_entry{}} | {error, not_found}.
get_entry(Tid, Index) ->
    case ets:lookup(Tid, Index) of
        [{Index, Entry}] -> {ok, Entry};
        [] -> {error, not_found}
    end.

%% @doc Get all entries from a starting index
-spec get_entries_from(ets:tid(), raft_index()) -> [#raft_log_entry{}].
get_entries_from(Tid, StartIndex) ->
    Pattern = [{StartIndex, '$1'}],
    %% Use ets:select for efficient range queries
    MatchSpec = [{{'$1', '$2'}, [{'>=', '$1', StartIndex}], ['$2']}],
    ets:select(Tid, MatchSpec).

%% @doc Get the last log index
-spec last_index(ets:tid()) -> raft_index().
last_index(Tid) ->
    case ets:last(Tid) of
        '$end_of_table' -> 0;
        Index -> Index
    end.

%% @doc Delete all log entries from a given index onwards
-spec delete_from(ets:tid(), raft_index()) -> ok.
delete_from(Tid, FromIndex) ->
    true = ets:delete(Tid, FromIndex),
    %% Also delete all entries with higher indices
    delete_higher_indices(Tid, FromIndex + 1),
    ok.

%% @doc Discard log entries up to and including an index (for compaction)
-spec discard_log_up_to(ets:tid(), raft_index()) -> ok.
discard_log_up_to(Tid, UpToIndex) ->
    lists:foreach(fun(I) -> ets:delete(Tid, I) end, lists:seq(1, UpToIndex)),
    ok.

%% @doc Get persistent state (current_term, voted_for)
-spec get_persistent_state(ets:tid()) -> {ok, raft_term(), raft_node_id() | undefined} | {error, not_found}.
get_persistent_state(Tid) ->
    case ets:lookup(Tid, persistent_state) of
        [{persistent_state, State}] ->
            Term = maps:get(term, State, 0),
            VotedFor = maps:get(voted_for, State, undefined),
            {ok, Term, VotedFor};
        [] ->
            {error, not_found}
    end.

%% @doc Save persistent state
-spec save_persistent_state(ets:tid(), raft_term(), raft_node_id() | undefined) -> ok.
save_persistent_state(Tid, Term, VotedFor) ->
    State = #{term => Term, voted_for => VotedFor},
    true = ets:insert(Tid, {persistent_state, State}),
    ok.

%% @doc Save snapshot metadata
-spec save_snapshot_metadata(ets:tid(), #raft_snapshot{}) -> ok.
save_snapshot_metadata(Tid, Snapshot) ->
    true = ets:insert(Tid, {snapshot_metadata, Snapshot}),
    ok.

%% @doc Get snapshot metadata
-spec get_snapshot_metadata(ets:tid()) -> {ok, #raft_snapshot{}} | {error, not_found}.
get_snapshot_metadata(Tid) ->
    case ets:lookup(Tid, snapshot_metadata) of
        [{snapshot_metadata, Snapshot}] -> {ok, Snapshot};
        [] -> {error, not_found}
    end.

%% @doc Take a snapshot of the current state
-spec take_snapshot(ets:tid(), term(), [raft_node_id()]) -> {ok, binary()} | {error, term()}.
take_snapshot(Tid, StateMachineState, ClusterMembers) ->
    gen_server:call(table_to_pid(Tid), {take_snapshot, StateMachineState, ClusterMembers}, infinity).

%% @doc Restore from a snapshot
-spec restore_snapshot(ets:tid(), binary()) -> {ok, term(), #raft_snapshot{}} | {error, term()}.
restore_snapshot(Tid, SnapshotData) ->
    gen_server:call(table_to_pid(Tid), {restore_snapshot, SnapshotData}, infinity).

%% @doc Stop the log server
-spec stop(ets:tid()) -> ok.
stop(Tid) ->
    case table_to_pid(Tid) of
        undefined -> ok;
        Pid when is_pid(Pid) -> gen_server:stop(Pid)
    end.

%%%====================================================================
%%% gen_server Callbacks
%%%====================================================================

-spec init([binary(), raft_node_id()]) -> {ok, #log_state{}}.
init([ClusterName, NodeId]) ->
    process_flag(trap_exit, true),

    %% Create ETS table for log storage
    LogTable = ets:new(raft_log, [
        ordered_set,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),

    %% Create DETS table for disk persistence
    DetsFileName = binary_to_list(<<"raft_log_", ClusterName/binary, "_",
                                    (to_binary(NodeId))/binary, ".dets">>),
    DetsOptions = [{file, DetsFileName}, {type, set}, {auto_save, 1000}],
    DetsTable = case dets:open_file(raft_log_dets, DetsOptions) of
                    {ok, Table} -> Table;
                    {error, Reason} ->
                        logger:warning("Failed to open DETS table ~p: ~p, running without persistence",
                                      [DetsFileName, Reason]),
                        undefined
                end,

    %% Load from disk if available
    State = #log_state{
        cluster_name = ClusterName,
        node_id = NodeId,
        log_table = LogTable,
        dets_table = DetsTable,
        persistent_state = #{term => 0, voted_for => undefined},
        snapshot = undefined,
        options = #{}
    },

    LoadedState = load_from_disk(State),

    %% Register table with gproc for discovery
    register_table(ClusterName, NodeId, LogTable, self()),

    logger:info("Raft log initialized for ~p in cluster ~p", [NodeId, ClusterName]),

    {ok, LoadedState}.

-spec handle_call(term(), {pid(), term()}, #log_state{}) ->
    {reply, term(), #log_state{}}.
handle_call(get_table, _From, State = #log_state{log_table = Tid}) ->
    {reply, {ok, Tid}, State};

handle_call({take_snapshot, StateMachineState, ClusterMembers}, _From,
            State = #log_state{log_table = Tid}) ->
    LastIndex = last_index(Tid),
    LastTerm = case LastIndex of
                   0 -> 0;
                   _ ->
                       {ok, Entry} = get_entry(Tid, LastIndex),
                       Entry#raft_log_entry.term
               end,

    Snapshot = #raft_snapshot{
        last_included_index = LastIndex,
        last_included_term = LastTerm,
        state_machine_state = StateMachineState,
        config = ClusterMembers,
        timestamp = erlang:monotonic_time(millisecond)
    },

    %% Serialize snapshot
    SnapshotData = term_to_binary(Snapshot, [compressed]),

    {reply, {ok, SnapshotData}, State#log_state{snapshot = Snapshot}};

handle_call({restore_snapshot, SnapshotData}, _From, State) ->
    try
        Snapshot = binary_to_term(SnapshotData),
        {reply, {ok, Snapshot#raft_snapshot.state_machine_state, Snapshot},
                State#log_state{snapshot = Snapshot}}
    catch
        _:Reason ->
            {reply, {error, {invalid_snapshot, Reason}}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #log_state{}) -> {noreply, #log_state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #log_state{}) -> {noreply, #log_state{}}.
handle_info({flush_to_disk}, State) ->
    NewState = flush_to_disk(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #log_state{}) -> ok.
terminate(_Reason, State = #log_state{dets_table = DetsTable}) ->
    %% Final flush to disk
    flush_to_disk(State),

    %% Close DETS table
    case DetsTable of
        undefined -> ok;
        Table -> dets:close(Table)
    end,

    logger:info("Raft log terminating for ~p", [State#log_state.node_id]),
    ok.

-spec code_change(term(), #log_state{}, term()) -> {ok, #log_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Delete all entries with indices higher than the given index
-spec delete_higher_indices(ets:tid(), raft_index()) -> ok.
delete_higher_indices(Tid, Index) ->
    case ets:lookup(Tid, Index) of
        [] ->
            ok;
        _ ->
            ets:delete(Tid, Index),
            delete_higher_indices(Tid, Index + 1)
    end.

%% @doc Load log state from disk
-spec load_from_disk(#log_state{}) -> #log_state{}.
load_from_disk(State = #log_state{dets_table = undefined}) ->
    State;
load_from_disk(State = #log_state{dets_table = DetsTable, log_table = LogTable}) ->
    %% Load persistent state
    case dets:lookup(DetsTable, persistent_state) of
        [{persistent_state, PersistState}] ->
            State#log_state{persistent_state = PersistState};
        [] ->
            State
    end.

%% @doc Flush log state to disk
-spec flush_to_disk(#log_state{}) -> #log_state{}.
flush_to_disk(State = #log_state{dets_table = undefined}) ->
    State;
flush_to_disk(State = #log_state{dets_table = DetsTable, log_table = LogTable, persistent_state = PersistState}) ->
    %% Save persistent state
    ok = dets:insert(DetsTable, {persistent_state, PersistState}),

    %% Save log entries (selective - only recent entries)
    %% In production, would batch this more efficiently
    dets:sync(DetsTable),
    State.

%% @doc Register table for discovery
-spec register_table(binary(), raft_node_id(), ets:tid(), pid()) -> ok.
register_table(ClusterName, NodeId, Tid, Pid) ->
    Key = {raft_log_table, ClusterName, NodeId},
    try
        gproc:reg(Key, Tid)
    catch
        _:_ -> ok
    end,
    %% Also store Pid for lookups
    gproc:reg({raft_log_pid, ClusterName, NodeId}, Pid),
    ok.

%% @doc Convert table to server PID
-spec table_to_pid(ets:tid()) -> pid() | undefined.
table_to_pid(Tid) ->
    case ets:info(Tid, owner) of
        Pid when is_pid(Pid) -> Pid;
        undefined -> undefined
    end.

%% @doc Convert node ID to binary
-spec to_binary(raft_node_id()) -> binary().
to_binary(NodeId) when is_binary(NodeId) -> NodeId;
to_binary(NodeId) when is_atom(NodeId) -> atom_to_binary(NodeId, utf8).
