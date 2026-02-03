-ifndef(ERLMCP_RAFT_HRL).

-define(ERLMCP_RAFT_HRL, 1).

%%% ====================================================================
%%% Raft Consensus Protocol Definitions for erlmcp v3
%%% ====================================================================
%%%
%%% Based on the Raft consensus algorithm:
%%%   - Leader Election: Randomized election timeouts prevent split votes
%%%   - Log Replication: Leader replicates entries to followers via AppendEntries
%%%   - Safety: Election restriction prevents uncommitted entries from winning elections
%%%   - Cluster Membership Changes: Joint consensus for safe reconfiguration
%%%   - Log Compaction: Snapshotting to bound log size
%%%
%%% Reference: Diego Ongaro and John Ousterhout, "In Search of an Understandable
%%% Consensus Algorithm", USENIX ATC 2014.
%%%
%%% Implementation Notes:
%%%   - Uses OTP 28+ features: priority messages for Raft RPCs
%%%   - ETS-based log storage for persistence
%%%   - Split-brain prevention via quorum enforcement
%%%   - Network partition tolerance via leader lease
%%% ====================================================================

%%% ====================================================================
%%% Raft Server States
%%% ====================================================================

-type raft_state() :: follower | candidate | leader.
-type raft_node_id() :: atom() | binary().
-type raft_term() :: non_neg_integer().
-type raft_index() :: pos_integer().

%%% ====================================================================
%%% Raft Log Entries
%%% ====================================================================

%% Log entry types
-type raft_entry_type() ::
    config_change      % Cluster membership change
    | command          % Normal state machine command
    | noop             % No-op for leadership assertion
    | snapshot.        % Snapshot marker

%% Log entry record
-record(raft_log_entry,
        {index :: raft_index(),                % Log entry position (1-based)
         term :: raft_term(),                  % Term when entry was created
         type :: raft_entry_type(),            % Entry type
         command :: term(),                    % Command to apply to state machine
         timestamp :: integer()}).             % Monotonic timestamp for debugging

-type raft_log_entry() :: #raft_log_entry{}.

%%% ====================================================================
%%% Raft RPC Messages
%%% ====================================================================

%% RequestVote RPC - Used during leader election
-record(raft_request_vote,
        {term :: raft_term(),                  % Candidate's term
         candidate_id :: raft_node_id(),       % Candidate requesting vote
         last_log_index :: raft_index() | 0,   % Index of candidate's last log entry
         last_log_term :: raft_term()}).       % Term of candidate's last log entry

-type raft_request_vote() :: #raft_request_vote{}.

%% RequestVote Response
-record(raft_request_vote_response,
        {term :: raft_term(),                  % Current term (for candidate to update)
         vote_granted :: boolean(),            % True means candidate received vote
         reason :: term()}).                   % Why vote was granted/denied (for debugging)

-type raft_request_vote_response() :: #raft_request_vote_response{}.

%% AppendEntries RPC - Used for heartbeats and log replication
-record(raft_append_entries,
        {term :: raft_term(),                  % Leader's term
         leader_id :: raft_node_id(),          % Leader's identity
         prev_log_index :: raft_index() | 0,   % Index of log entry preceding new ones
         prev_log_term :: raft_term(),         % Term of prev_log_index entry
         entries :: [raft_log_entry()],        % Log entries to replicate (empty for heartbeat)
         leader_commit :: raft_index() | 0}).  % Leader's commit index

-type raft_append_entries() :: #raft_append_entries{}.

%% AppendEntries Response
-record(raft_append_entries_response,
        {term :: raft_term(),                  % Current term (for leader to update)
         success :: boolean(),                 % True if follower had entry matching prev_log_index
         match_index :: raft_index() | 0,      % Index up to which entries match
         reason :: term()}).                   % Why append succeeded/failed (for debugging)

-type raft_append_entries_response() :: #raft_append_entries_response{}.

%% InstallSnapshot RPC - Used to send snapshots to followers
-record(raft_install_snapshot,
        {term :: raft_term(),                  % Leader's term
         leader_id :: raft_node_id(),          % Leader's identity
         last_included_index :: raft_index(),  % Index of last log entry in snapshot
         last_included_term :: raft_term(),    % Term of last_included_index
         snapshot_data :: binary(),            % Raw snapshot data
         snapshot_size :: non_neg_integer()}).  % Size of snapshot data in bytes

-type raft_install_snapshot() :: #raft_install_snapshot{}.

%% InstallSnapshot Response
-record(raft_install_snapshot_response,
        {term :: raft_term()}).                % Current term (for leader to update)

-type raft_install_snapshot_response() :: #raft_install_snapshot_response{}.

%% AddServer RPC - Used for cluster membership changes (joint consensus)
-record(raft_add_server,
        {server_id :: raft_node_id(),          % New server to add
         config_index :: raft_index()}).       % Log index of this config change

-type raft_add_server() :: #raft_add_server{}.

%% RemoveServer RPC - Used for cluster membership changes
-record(raft_remove_server,
        {server_id :: raft_node_id(),          % Server to remove
         config_index :: raft_index()}).       % Log index of this config change

-type raft_remove_server() :: #raft_remove_server{}.

%%% ====================================================================
%%% Raft Timeout Constants
%%% ====================================================================

%% Election timeout bounds (randomized per follower)
%% These prevent split votes by staggering elections
-define(RAFT_ELECTION_TIMEOUT_MIN, 150).       % Minimum election timeout (ms)
-define(RAFT_ELECTION_TIMEOUT_MAX, 300).       % Maximum election timeout (ms)

%% Heartbeat interval - how often leaders send AppendEntries (empty)
-define(RAFT_HEARTBEAT_INTERVAL, 50).          % Leader heartbeat interval (ms)

%% RPC timeouts
-define(RAFT_RPC_TIMEOUT, 100).                % Default RPC timeout (ms)
-define(RAFT_SNAPSHOT_TIMEOUT, 5000).          % InstallSnapshot timeout (ms)

%% Snapshot thresholds
-define(RAFT_SNAPSHOT_THRESHOLD, 10000).       % Log entries before snapshot
-define(RAFT_SNAPSHOT_MARGIN, 1000).           % Entries to keep after snapshot

%%% ====================================================================
%%% Raft Server State Record
%%% ====================================================================

-record(raft_server_state,
        {%% Persistent state (written to log before responding to RPCs)
         current_term :: raft_term(),          % Latest term server has seen
         voted_for :: raft_node_id() | undefined, % Candidate that received vote in current term
         log :: ets:tid(),                     % Raft log (ETS table)

         %% Volatile state on all servers
         commit_index :: raft_index() | 0,     % Index of highest log entry known to be committed
         last_applied :: raft_index() | 0,     % Index of highest log entry applied to state machine

         %% Volatile state on leaders (re-initialized after election)
         next_index :: #{raft_node_id() => raft_index()}, % For each server, index of next log entry
         match_index :: #{raft_node_id() => raft_index()}, % For each server, index of highest log entry

         %% Cluster membership
         peers :: [raft_node_id()],            % Other nodes in the cluster
         cluster_members :: [raft_node_id()],  % All nodes including self
         leader_id :: raft_node_id() | undefined, % Current cluster leader

         %% State machine
         state_machine :: module(),            % State machine module (callback)
         state_machine_state :: term(),        % Current state machine state

         %% Server state
         server_state :: raft_state(),         % follower | candidate | leader
         node_id :: raft_node_id(),            % This node's identifier

         %% Elections
         election_timer_ref :: reference() | undefined,
         votes_received :: sets:set(raft_node_id()),

         %% Heartbeat
         heartbeat_timer_ref :: reference() | undefined,

         %% Snapshotting
         snapshot_in_progress :: boolean(),
         snapshot_threshold :: pos_integer(),

         %% Leader lease (optimization)
         leader_lease_until :: integer() | undefined, % Monotonic time when lease expires

         %% Network layer
         transport :: module(),                % Transport module for RPC sending

         %% Metrics
         metrics :: raft_metrics()}).

-type raft_server_state() :: #raft_server_state{}.

%%% ====================================================================
%%% Raft Metrics Record
%%% ====================================================================

-record(raft_metrics,
        {elections_won = 0 :: non_neg_integer(),
         elections_lost = 0 :: non_neg_integer(),
         terms_served = 0 :: non_neg_integer(),
         log_entries_committed = 0 :: non_neg_integer(),
         log_entries_appended = 0 :: non_neg_integer(),
         snapshots_taken = 0 :: non_neg_integer(),
         snapshots_installed = 0 :: non_neg_integer(),
         rpcs_sent = 0 :: non_neg_integer(),
         rpcs_received = 0 :: non_neg_integer(),
         rpc_timeouts = 0 :: non_neg_integer(),
         split_brain_prevented = 0 :: non_neg_integer()}).

-type raft_metrics() :: #raft_metrics{}.

%%% ====================================================================
%%% Raft Configuration Change Record
%%% ====================================================================

-record(raft_config_change,
        {type :: add_server | remove_server,
         server_id :: raft_node_id(),
         old_cluster :: [raft_node_id()],       % Cluster before change
         new_cluster :: [raft_node_id()],       % Cluster after change
         config_index :: raft_index()}).        % Log index of this config entry

-type raft_config_change() :: #raft_config_change{}.

%%% ====================================================================
%%% Raft Snapshot Record
%%% ====================================================================

-record(raft_snapshot,
        {last_included_index :: raft_index(),
         last_included_term :: raft_term(),
         state_machine_state :: term(),
         config :: [raft_node_id()],
         timestamp :: integer()}).

-type raft_snapshot() :: #raft_snapshot{}.

%%% ====================================================================
%%% Raft State Machine Callback Behavior
%%% ====================================================================

%% State machine callback module specification
-callback init(Args :: term()) -> {ok, State :: term()} | {error, term()}.

-callback apply(Command :: term(), State :: term()) ->
    {ok, NewState :: term(), Result :: term()} | {error, term()}.

-callback snapshot(State :: term()) -> {ok, Binary :: binary()} | {error, term()}.

-callback restore(Binary :: binary()) -> {ok, State :: term()} | {error, term()}.

-callback get_state(State :: term()) -> map().

-type raft_state_machine() :: module().

%%% ====================================================================
%%% Raft Transport Callback Behavior
%%% ====================================================================

%% Transport callback module specification for network abstraction
-callback send_rpc(To :: raft_node_id(), RPC :: term()) -> ok | {error, term()}.

-callback broadcast(Peers :: [raft_node_id()], RPC :: term()) -> ok.

-type raft_transport() :: module().

%%% ====================================================================
%%% Raft Error Reasons
%%% ====================================================================

-type raft_error() ::
    {not_leader, raft_node_id()} |           % Not the leader, here is the leader
    {timeout, term()} |                       % Operation timed out
    {election_in_progress, raft_term()} |     % Election in progress
    {snapshot_in_progress, raft_index()} |    % Snapshot installation in progress
    {network_partition, [raft_node_id()]} |   % Network partition detected
    {config_change_in_progress, raft_index()} | % Membership change in progress
    {term_mismatch, raft_term(), raft_term()} | % Term mismatch (expected, actual)
    {log_inconsistent, raft_index(), raft_term()} | % Log inconsistency
    {quorum_lost, raft_node_id()} |           % Lost quorum, leader is...
    shutdown.                                 % Server shutting down

%%% ====================================================================
%%% Raft Client API Types
%%% ====================================================================

-type raft_write_result() ::
    {ok, term()} |                            % Command succeeded, result from state machine
    {error, raft_error()}.                    % Command failed

-type raft_read_result() ::
    {ok, term()} |                            % Read succeeded
    {error, raft_error()}.                    % Read failed

-type raft_status() ::
    #{state => raft_state(),
      term => raft_term(),
      leader => raft_node_id() | undefined,
      commit_index => raft_index(),
      last_applied => raft_index(),
      cluster_size => non_neg_integer(),
      is_leader => boolean()}.

%%% ====================================================================
%%% Convenience Macros
%%% ====================================================================

%% Check if server is leader
-define(is_leader(State), State#raft_server_state.server_state =:= leader).

%% Check if server is follower
-define(is_follower(State), State#raft_server_state.server_state =:= follower).

%% Check if server is candidate
-define(is_candidate(State), State#raft_server_state.server_state =:= candidate).

%% Calculate quorum size from cluster size
-define(quorum_size(ClusterSize), (ClusterSize div 2) + 1).

%% Current cluster size from state
-define(cluster_size(State), length(State#raft_server_state.cluster_members)).

%% Quorum size from state
-define(quorum(State), ?quorum_size(?cluster_size(State))).

%% Check if term is up-to-date (for election restriction)
-define(is_log_up_to_date(OurLastIndex, OurLastTerm, TheirLastIndex, TheirLastTerm),
    TheirLastTerm > OurLastTerm orelse
    (TheirLastTerm =:= OurLastTerm andalso TheirLastIndex >= OurLastIndex)).

%% Generate election timeout with jitter
-define(election_timeout(),
    rand:uniform(?RAFT_ELECTION_TIMEOUT_MAX - ?RAFT_ELECTION_TIMEOUT_MIN) +
    ?RAFT_ELECTION_TIMEOUT_MIN).

%% Convert node to/from atom for gproc compatibility
-define(to_atom(NodeId),
    case NodeId of
        N when is_atom(N) -> N;
        N when is_binary(N) -> binary_to_existing_atom(N, utf8)
    end).

-define(to_binary(NodeId),
    case NodeId of
        N when is_binary(N) -> N;
        N when is_atom(N) -> atom_to_binary(N, utf8)
    end).

%%% ====================================================================
%%% Raft Gproc Keys for Distributed Coordination
%%% ====================================================================

%% Leader registration key (used for leader discovery)
-define(RAFT_LEADER_KEY(ClusterName),
    {n, g, {raft_leader, ClusterName}}).

%% Cluster member registry key
-define(RAFT_MEMBER_KEY(ClusterName, NodeId),
    {n, g, {raft_member, ClusterName, NodeId}}).

%% Request key (for request tracking across cluster)
-define(RAFT_REQUEST_KEY(RequestId),
    {n, g, {raft_request, RequestId}}).

%%% ====================================================================
%%% Export Types
%%% ====================================================================

-export_type([
    raft_state/0,
    raft_node_id/0,
    raft_term/0,
    raft_index/0,
    raft_entry_type/0,
    raft_log_entry/0,
    raft_request_vote/0,
    raft_request_vote_response/0,
    raft_append_entries/0,
    raft_append_entries_response/0,
    raft_install_snapshot/0,
    raft_install_snapshot_response/0,
    raft_server_state/0,
    raft_metrics/0,
    raft_config_change/0,
    raft_snapshot/0,
    raft_error/0,
    raft_write_result/0,
    raft_read_result/0,
    raft_status/0,
    raft_state_machine/0,
    raft_transport/0
]).

-endif.
