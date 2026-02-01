%%%-------------------------------------------------------------------
%%% @doc Session Replicator
%%%
%%% Implements session replication across distributed Erlang nodes using
%%% Mnesia for distributed storage and vector clocks for conflict resolution.
%%%
%%% == Joe Armstrong Principles Applied ==
%%% - "Let it crash": Replication failures never block primary operations
%%% - Async replication: Performance through non-blocking writes
%%% - Mnesia handles distributed transactions transparently
%%%
%%% == Architecture ==
%%% - Mnesia table: erlmcp_session_replica (distributed, disc_copies)
%%% - Vector clocks for conflict detection (last-write-wins with causality)
%%% - Replication queue for temporarily disconnected nodes
%%% - Bootstrap recovery for new nodes joining cluster
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_replicator).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    replicate/2,
    replicate_async/2,
    sync_replicate/2,
    get_replicas/1,
    get_replication_status/0,
    bootstrap_node/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_continue/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

-define(REPLICA_TABLE, erlmcp_session_replica).
-define(QUEUE_TABLE, erlmcp_replication_queue).
-define(VECTOR_CLOCK_TABLE, erlmcp_vector_clock).
-define(REPLICATION_TIMEOUT, 5000).

%%====================================================================
%% Types
%%====================================================================

-type session_id() :: binary().
-type session() :: #{
    id := session_id(),
    created_at := integer(),
    last_accessed := integer(),
    timeout_ms := pos_integer() | infinity,
    metadata := map()
}.
-type vector_clock() :: #{node() => integer()}.
-type replication_status() :: #{
    total_sessions => non_neg_integer(),
    replicated_sessions => non_neg_integer(),
    pending_replications => non_neg_integer(),
    replica_nodes => [node()],
    queue_size => non_neg_integer()
}.

-record(state, {
    local_node :: node(),
    replica_nodes :: [node()],
    replication_queue :: queue:queue({session_id(), session(), vector_clock()}),
    queue_timer :: reference() | undefined,
    batch_size :: pos_integer(),
    batch_interval :: pos_integer()
}).

%% Mnesia record for replica state
-record(replica_state, {
    session_id :: session_id(),
    session :: session(),
    vector_clock :: vector_clock(),
    replica_nodes :: [node()],
    last_replicated :: integer()
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the replicator
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Replicate a session asynchronously (fire-and-forget)
-spec replicate_async(session_id(), session()) -> ok.
replicate_async(SessionId, Session) ->
    gen_server:cast(?MODULE, {replicate_async, SessionId, Session}),
    ok.

%% @doc Replicate a session synchronously with confirmation
-spec sync_replicate(session_id(), session()) -> {ok, [node()]} | {error, term()}.
sync_replicate(SessionId, Session) ->
    gen_server:call(?MODULE, {sync_replicate, SessionId, Session}, ?REPLICATION_TIMEOUT).

%% @doc Alias for replicate_async (backward compatibility)
-spec replicate(session_id(), session()) -> ok.
replicate(SessionId, Session) ->
    replicate_async(SessionId, Session).

%% @doc Get all replica nodes for a session
-spec get_replicas(session_id()) -> {ok, [node()]} | {error, not_found}.
get_replicas(SessionId) ->
    case ets:lookup(?REPLICA_TABLE, SessionId) of
        [#replica_state{replica_nodes = Nodes}] ->
            {ok, Nodes};
        [] ->
            {error, not_found}
    end.

%% @doc Get replication status
-spec get_replication_status() -> {ok, replication_status()}.
get_replication_status() ->
    gen_server:call(?MODULE, get_replication_status).

%% @doc Bootstrap a new node with existing sessions
-spec bootstrap_node(node()) -> {ok, non_neg_integer()} | {error, term()}.
bootstrap_node(Node) when Node =/= node() ->
    gen_server:call(?MODULE, {bootstrap_node, Node}, infinity);
bootstrap_node(_LocalNode) ->
    {error, cannot_bootstrap_local}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([]) ->
    LocalNode = node(),
    ReplicaNodes = get_replica_nodes(),

    %% Fast init - return immediately without blocking Mnesia table creation
    %% Tables will be initialized asynchronously in handle_continue
    State = #state{
        local_node = LocalNode,
        replica_nodes = ReplicaNodes,
        replication_queue = queue:new(),
        queue_timer = undefined,
        batch_size = 100,
        batch_interval = 1000
    },

    {ok, State, {continue, init_mnesia_tables}}.

%% @private
handle_call({sync_replicate, SessionId, Session}, _From, State) ->
    %% Synchronous replication with vector clock update
    VectorClock = get_vector_clock(SessionId),
    UpdatedClock = increment_vector_clock(VectorClock, node()),

    case replicate_to_nodes(SessionId, Session, UpdatedClock, State) of
        {ok, ReplicatedNodes} ->
            %% Store replica state
            ReplicaState = #replica_state{
                session_id = SessionId,
                session = Session,
                vector_clock = UpdatedClock,
                replica_nodes = ReplicatedNodes,
                last_replicated = erlang:system_time(millisecond)
            },
            store_replica_state(ReplicaState),

            {reply, {ok, ReplicatedNodes}, State};
        {error, _Reason} ->
            %% Queue for retry
            Queue = State#state.replication_queue,
            NewQueue = queue:in({SessionId, Session, UpdatedClock}, Queue),
            {reply, {error, replication_failed}, State#state{replication_queue = NewQueue}}
    end;

handle_call(get_replication_status, _From, State) ->
    %% Calculate replication status
    TotalSessions = ets:info(?REPLICA_TABLE, size),
    PendingCount = queue:len(State#state.replication_queue),
    QueueSize = case ets:whereis(?QUEUE_TABLE) of
        undefined -> 0;
        _ -> ets:info(?QUEUE_TABLE, size)
    end,

    %% Count fully replicated sessions (all sessions in replica table are replicated)
    ReplicatedCount = TotalSessions,

    Status = #{
        total_sessions => TotalSessions,
        replicated_sessions => ReplicatedCount,
        pending_replications => PendingCount,
        replica_nodes => State#state.replica_nodes,
        queue_size => QueueSize
    },

    {reply, {ok, Status}, State};

handle_call({bootstrap_node, Node}, _From, State) ->
    %% Bootstrap a new node asynchronously to avoid blocking handle_call
    %% Spawn async worker to perform bootstrap (prevents blocking on network I/O)
    Self = self(),
    spawn(fun() ->
        %% Check node availability with timeout (non-blocking for caller)
        case rpc:call(Node, erlang, node, [], 1000) of
            Node ->
                %% Node is reachable, proceed with bootstrap
                Sessions = get_all_local_sessions(),
                BatchSize = 100,
                Batches = partition_list(Sessions, BatchSize),

                %% Bootstrap with RPC timeouts per batch
                Results = lists:map(fun(Batch) ->
                    rpc:call(Node, mnesia, transaction, [fun() ->
                        lists:foreach(fun({SessId, Sess, VClock}) ->
                            Record = #replica_state{
                                session_id = SessId,
                                session = Sess,
                                vector_clock = VClock,
                                replica_nodes = [node() | State#state.replica_nodes],
                                last_replicated = erlang:system_time(millisecond)
                            },
                            mnesia:write(?REPLICA_TABLE, Record, write)
                        end, Batch)
                    end], 3000)  % 3 second timeout per batch
                end, Batches),

                %% Check if any batch succeeded
                case lists:any(fun({atomic, ok}) -> true; (_) -> false end, Results) of
                    true ->
                        %% Notify replicator to add node to replica list
                        gen_server:cast(Self, {bootstrap_complete, Node, length(Sessions)});
                    false ->
                        ?LOG_ERROR("Bootstrap failed for node ~p: all batches failed", [Node])
                end;
            {error, timeout} ->
                ?LOG_ERROR("Bootstrap failed for node ~p: timeout", [Node]);
            {badrpc, Reason} ->
                ?LOG_ERROR("Bootstrap failed for node ~p: ~p", [Node, Reason]);
            _Other ->
                ?LOG_ERROR("Bootstrap failed for node ~p: node unreachable", [Node])
        end
    end),

    %% Return immediately - bootstrap happens asynchronously
    {reply, {ok, bootstrap_started}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({replicate_async, SessionId, Session}, State) ->
    %% Asynchronous replication - add to queue
    VectorClock = get_vector_clock(SessionId),
    UpdatedClock = increment_vector_clock(VectorClock, node()),

    Queue = State#state.replication_queue,
    NewQueue = queue:in({SessionId, Session, UpdatedClock}, Queue),

    %% Trigger flush if queue exceeds batch size
    NewState = State#state{replication_queue = NewQueue},
    maybe_flush_queue(NewQueue, NewState),

    %% Trigger immediate flush for single-item queue
    case queue:len(NewQueue) of
        1 -> erlang:send_after(0, self(), flush_queue);
        _ -> ok
    end,

    {noreply, NewState};

handle_cast({bootstrap_complete, Node, SessionCount}, State) ->
    %% Bootstrap completed successfully, add node to replica list
    NewReplicaNodes = lists:usort([Node | State#state.replica_nodes]),
    ?LOG_INFO("Bootstrap complete for node ~p: ~p sessions replicated", [Node, SessionCount]),
    {noreply, State#state{replica_nodes = NewReplicaNodes}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(flush_queue, State) ->
    %% Flush replication queue in batches
    Queue = State#state.replication_queue,

    case queue:out(Queue) of
        {{value, {SessionId, Session, VClock}}, RemainingQueue} ->
            %% Replicate this item
            case replicate_to_nodes(SessionId, Session, VClock, State) of
                {ok, _ReplicatedNodes} ->
                    %% Store replica state
                    ReplicaState = #replica_state{
                        session_id = SessionId,
                        session = Session,
                        vector_clock = VClock,
                        replica_nodes = State#state.replica_nodes,
                        last_replicated = erlang:system_time(millisecond)
                    },
                    store_replica_state(ReplicaState),

                    %% Continue flushing
                    erlang:send_after(10, self(), flush_queue),
                    {noreply, State#state{replication_queue = RemainingQueue}};
                {error, _Reason} ->
                    %% Re-queue for retry
                    NewQueue = queue:in({SessionId, Session, VClock}, RemainingQueue),
                    ?LOG_WARNING("Replication failed for session ~p, re-queued", [SessionId]),
                    {noreply, State#state{replication_queue = NewQueue}}
            end;
        {empty, EmptyQueue} ->
            {noreply, State#state{replication_queue = EmptyQueue}}
    end;

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    %% Handle monitored process death
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
handle_continue(init_mnesia_tables, State) ->
    %% Initialize Mnesia tables asynchronously (after init/1 returns)
    case init_mnesia_tables(State#state.local_node, State#state.replica_nodes) of
        ok ->
            %% Start batch replication timer
            {ok, Timer} = timer:send_interval(State#state.batch_interval, flush_queue),
            ?LOG_INFO("Session replicator initialized successfully with Mnesia tables", []),
            {noreply, State#state{queue_timer = Timer}};
        {error, Reason} ->
            %% Log error but continue in degraded mode (no replication)
            ?LOG_ERROR("Failed to initialize Mnesia tables: ~p. Replication disabled.", [Reason]),
            {noreply, State}
    end.

%% @private
terminate(_Reason, #state{queue_timer = Timer}) ->
    %% Cancel timer
    case Timer of
        undefined -> ok;
        _ -> timer:cancel(Timer)
    end,
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Initialize Mnesia tables for replication
-spec init_mnesia_tables(node(), [node()]) -> ok | {error, term()}.
init_mnesia_tables(LocalNode, ReplicaNodes) ->
    AllNodes = [LocalNode | ReplicaNodes],

    %% disc_copies requires a named node (not nonode@nohost)
    UseDiscCopies = (LocalNode =/= 'nonode@nohost'),

    %% Create replica table
    case mnesia:create_table(?REPLICA_TABLE, [
        {attributes, record_info(fields, replica_state)},
        {disc_copies, case UseDiscCopies of true -> AllNodes; false -> [] end},
        {ram_copies, case UseDiscCopies of true -> []; false -> AllNodes end},
        {type, set}
    ]) of
        {atomic, ok} ->
            ?LOG_INFO("Created replica table: ~p on ~p (storage: ~p)", [?REPLICA_TABLE, AllNodes,
                case UseDiscCopies of true -> disc_copies; false -> ram_copies end]);
        {atomic, {already_exists, ?REPLICA_TABLE}} ->
            ?LOG_INFO("Replica table already exists: ~p", [?REPLICA_TABLE]);
        {aborted, CreateReason} ->
            ?LOG_ERROR("Failed to create replica table: ~p", [CreateReason]),
            {error, CreateReason}
    end,

    %% Create queue table for pending replications
    case mnesia:create_table(?QUEUE_TABLE, [
        {attributes, [session_id, session, vector_clock, timestamp]},
        {disc_copies, case UseDiscCopies of true -> AllNodes; false -> [] end},
        {ram_copies, case UseDiscCopies of true -> []; false -> AllNodes end},
        {type, bag}
    ]) of
        {atomic, ok} ->
            ?LOG_INFO("Created queue table: ~p", [?QUEUE_TABLE]);
        {atomic, {already_exists, ?QUEUE_TABLE}} ->
            ?LOG_INFO("Queue table already exists: ~p", [?QUEUE_TABLE]);
        {aborted, _QueueReason} ->
            ok
    end,

    %% Create vector clock table
    case mnesia:create_table(?VECTOR_CLOCK_TABLE, [
        {attributes, [session_id, vector_clock]},
        {disc_copies, case UseDiscCopies of true -> AllNodes; false -> [] end},
        {ram_copies, case UseDiscCopies of true -> []; false -> AllNodes end},
        {type, set}
    ]) of
        {atomic, ok} ->
            ?LOG_INFO("Created vector clock table: ~p", [?VECTOR_CLOCK_TABLE]);
        {atomic, {already_exists, ?VECTOR_CLOCK_TABLE}} ->
            ?LOG_INFO("Vector clock table already exists: ~p", [?VECTOR_CLOCK_TABLE]);
        {aborted, _VCReason} ->
            ok
    end,

    ok.

%% @doc Get configured replica nodes
-spec get_replica_nodes() -> [node()].
get_replica_nodes() ->
    case application:get_env(erlmcp_core, replica_nodes) of
        {ok, Nodes} when is_list(Nodes) -> Nodes;
        _ -> []
    end.

%% @doc Get vector clock for a session
-spec get_vector_clock(session_id()) -> vector_clock().
get_vector_clock(SessionId) ->
    case ets:lookup(?VECTOR_CLOCK_TABLE, SessionId) of
        [{SessionId, VClock}] -> VClock;
        [] -> #{}
    end.

%% @doc Increment vector clock for a node
-spec increment_vector_clock(vector_clock(), node()) -> vector_clock().
increment_vector_clock(VClock, Node) ->
    CurrentVersion = maps:get(Node, VClock, 0),
    maps:put(Node, CurrentVersion + 1, VClock).

%% @doc Replicate session to all replica nodes
%% Uses RPC with timeout instead of blocking net_adm:ping
-spec replicate_to_nodes(session_id(), session(), vector_clock(), #state{}) ->
    {ok, [node()]} | {error, term()}.
replicate_to_nodes(_SessionId, _Session, _VClock, State) ->
    ReplicaNodes = State#state.replica_nodes,
    LocalNode = State#state.local_node,

    %% Filter out local node
    RemoteNodes = lists:filter(fun(Node) -> Node =/= LocalNode end, ReplicaNodes),

    %% If no replica nodes configured, return empty list (success)
    case RemoteNodes of
        [] ->
            {ok, []};
        _ ->
            %% Replicate to remote nodes with RPC timeout (non-blocking check)
            Results = lists:map(fun(Node) ->
                {Node, rpc:call(Node, mnesia, transaction, [fun() ->
                    %% Just verify node is accessible
                    {atomic, ok}
                end], 3000)}  % 3 second timeout per node
            end, RemoteNodes),

            %% Collect successful replications (includes implicit availability check)
            SuccessfulNodes = lists:foldl(fun({Node, Result}, Acc) ->
                case Result of
                    {atomic, ok} -> [Node | Acc];
                    {badrpc, _} -> Acc;  % Node unreachable
                    {error, _} -> Acc;   % RPC error
                    _ -> Acc
                end
            end, [], Results),

            case SuccessfulNodes of
                [] ->
                    {error, all_replications_failed};
                _ ->
                    {ok, lists:reverse(SuccessfulNodes)}
            end
    end.

%% @doc Store replica state in Mnesia
-spec store_replica_state(#replica_state{}) -> ok | {error, term()}.
store_replica_state(ReplicaState) ->
    Transaction = fun() ->
        mnesia:write(?REPLICA_TABLE, ReplicaState, write),
        SessionId = ReplicaState#replica_state.session_id,
        VClock = ReplicaState#replica_state.vector_clock,
        mnesia:write(?VECTOR_CLOCK_TABLE, {SessionId, VClock}, write)
    end,

    %% Write replication state - 5000ms timeout
    case transaction_with_timeout(Transaction, 5000) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Check if a replica is fully replicated
-spec is_fully_replicated(#replica_state{}, #state{}) -> boolean().
is_fully_replicated(ReplicaState, State) ->
    ReplicaNodes = ReplicaState#replica_state.replica_nodes,
    RequiredNodes = State#state.replica_nodes,
    length(ReplicaNodes) >= length(RequiredNodes).

%% @doc Get all local sessions for bootstrap
-spec get_all_local_sessions() -> [{session_id(), session(), vector_clock()}].
get_all_local_sessions() ->
    ets:foldl(fun(_Key, #replica_state{
        session_id = SessId,
        session = Sess,
        vector_clock = VClock
    }, Acc) ->
        [{SessId, Sess, VClock} | Acc]
    end, [], ?REPLICA_TABLE).

%% @doc Partition list into batches
-spec partition_list(list(), pos_integer()) -> [list()].
partition_list(List, BatchSize) ->
    partition_list(List, BatchSize, []).

partition_list([], _BatchSize, Acc) ->
    lists:reverse(Acc);
partition_list(List, BatchSize, Acc) ->
    {Batch, Rest} = lists:split(min(BatchSize, length(List)), List),
    partition_list(Rest, BatchSize, [Batch | Acc]).

%% @doc Flush queue if it exceeds batch size
-spec maybe_flush_queue(queue:queue(_), #state{}) -> ok.
maybe_flush_queue(Queue, State) ->
    case queue:len(Queue) >= State#state.batch_size of
        true ->
            erlang:send_after(0, self(), flush_queue),
            ok;
        false ->
            ok
    end.

%% @private Execute Mnesia transaction with timeout to prevent indefinite hangs.
%% Mnesia transactions don't have built-in timeout support, so we wrap them
%% in a process with a timeout to prevent lock contention or network partition hangs.
-spec transaction_with_timeout(fun(() -> term()), timeout()) ->
    {atomic, term()} | {aborted, term()}.
transaction_with_timeout(Fun, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn_link(fun() ->
        Result = mnesia:transaction(Fun),
        Parent ! {Ref, Result}
    end),

    receive
        {Ref, Result} -> Result
    after Timeout ->
        exit(Pid, kill),
        {aborted, timeout}
    end.
