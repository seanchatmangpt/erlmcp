%%%-------------------------------------------------------------------
%%% @doc EUnit tests for minimal Raft consensus (leader election only)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_raft_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

raft_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Single node election", fun test_single_node_election/0},
         {"3-node quorum election", fun test_three_node_quorum/0},
         {"Leader detection via is_leader", fun test_leader_detection/0},
         {"Heartbeat with live leader", fun test_heartbeat_live_leader/0},
         {"Heartbeat with dead leader", fun test_heartbeat_dead_leader/0}
     ]}.

setup() ->
    % Ensure distributed Erlang is started
    case net_kernel:start([test_node, shortnames]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} ->
            error_logger:error_msg("Failed to start net_kernel: ~p", [Reason]),
            ok
    end.

cleanup(_) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc Test 1: Single node election
%% Single node should always win election (quorum of 1)
test_single_node_election() ->
    % Single node cluster (just self)
    Result = erlmcp_flow_raft:start_election([]),

    % Should succeed with self as leader
    ?assertMatch({ok, _}, Result),
    {ok, Leader} = Result,
    ?assertEqual(node(), Leader),

    % Verify is_leader returns true for self
    ?assert(erlmcp_flow_raft:is_leader(node())),
    ?assert(erlmcp_flow_raft:is_leader(self())).

%% @doc Test 2: 3-node quorum
%% With 3 nodes, quorum is 2. Election should succeed with 2+ votes.
%% Since we can't easily spawn real distributed nodes in unit test,
%% we simulate by checking the quorum logic with self + 2 fake nodes
test_three_node_quorum() ->
    % In a real distributed system, these would be actual nodes
    % For unit test, we test with self which will vote yes
    FakeNodes = ['node1@localhost', 'node2@localhost'],

    % Start election (will only get 1 vote - from self)
    % This tests the quorum calculation logic
    Result = erlmcp_flow_raft:start_election(FakeNodes),

    % With 3 total nodes (self + 2 fake), quorum is 2
    % We only get 1 vote (self), so election should fail
    ?assertMatch({error, no_quorum}, Result),

    % Test with empty list should also fail
    ?assertMatch({error, no_quorum}, erlmcp_flow_raft:start_election([])).

%% @doc Test 3: Leader detection
%% Test is_leader/1 function with different inputs
test_leader_detection() ->
    CurrentNode = node(),

    % Current node should be detected as leader
    ?assert(erlmcp_flow_raft:is_leader(CurrentNode)),

    % Self process should be detected as leader (on current node)
    ?assert(erlmcp_flow_raft:is_leader(self())),

    % Different node should not be leader
    OtherNode = 'other@localhost',
    ?assertNot(erlmcp_flow_raft:is_leader(OtherNode)),

    % Test with current node explicitly
    ?assert(erlmcp_flow_raft:is_leader(CurrentNode)).

%% @doc Test 4: Heartbeat with live leader
%% Heartbeat to self (which is always reachable) should succeed
test_heartbeat_live_leader() ->
    Leader = node(),
    Nodes = [Leader],

    % Heartbeat to self should succeed
    Result = erlmcp_flow_raft:heartbeat(Leader, Nodes),
    ?assertEqual(ok, Result),

    % Test with empty nodes list (leader not in list but is self)
    Result2 = erlmcp_flow_raft:heartbeat(Leader, []),
    ?assertEqual(ok, Result2).

%% @doc Test 5: Heartbeat with dead/unreachable leader
%% Heartbeat to unreachable node should return leader_dead
test_heartbeat_dead_leader() ->
    DeadLeader = 'dead_node@localhost',
    Nodes = [DeadLeader, 'node2@localhost'],

    % Heartbeat to unreachable node should fail
    Result = erlmcp_flow_raft:heartbeat(DeadLeader, Nodes),
    ?assertMatch({error, leader_dead}, Result),

    % Test with leader not in nodes list and not self
    Result2 = erlmcp_flow_raft:heartbeat(DeadLeader, ['other@localhost']),
    ?assertMatch({error, leader_dead}, Result2).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Note: In a full integration test suite, we would use Common Test
%% with actual distributed Erlang nodes. These EUnit tests verify
%% the core logic with minimal setup.
