%%%-------------------------------------------------------------------
%%% @doc erlmcp_distributed_session_tests — EUnit tests for session failover (T019)
%%%
%%% Tests the handle_membership_event/2 `member_dead` path which was a stub.
%%% We test the behaviour directly via the gen_server's membership_event
%%% handle_info path.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_distributed_session_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Failover tests (T019)
%%====================================================================

failover_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"Session on dead node is updated in replica map",
       fun test_session_replica_updated_after_failover/0},
      {"Session on live node is unaffected by unrelated node death",
       fun test_session_on_live_node_unaffected/0}]}.

%%====================================================================
%% Setup / cleanup
%%====================================================================

setup() ->
    %% Start the distributed session manager without session manager dep.
    %% We test via handle_info membership events directly.
    {ok, DsPid} = erlmcp_distributed_session:start_link(#{
        sync_interval => 60000
    }),
    DsPid.

cleanup(DsPid) ->
    gen_server:stop(DsPid, normal, 5000),
    timer:sleep(50),
    ok.

%%====================================================================
%% Individual test functions
%%====================================================================

%% When the primary node for a session dies the replica_state should
%% be updated so the first replica becomes the new primary.
test_session_replica_updated_after_failover() ->
    DeadNode   = dead_failover_node@nohost,
    ReplicaNode = replica_failover_node@nohost,
    SessionId   = <<"t019-session-failover">>,

    %% Inject a replica entry with DeadNode as primary via sys:replace_state.
    %% The state record is: {state, local_node, replication_factor, sync_interval,
    %%                        sync_ref, replicas, pending_sync}
    %% Position 6 (1-indexed) = replicas map.
    ReplicaEntry = make_replica_state(SessionId, DeadNode, [ReplicaNode]),
    sys:replace_state(erlmcp_distributed_session, fun(S) ->
        OldReplicas = element(6, S),
        setelement(6, S, maps:put(SessionId, ReplicaEntry, OldReplicas))
    end),

    %% Trigger the member_dead event
    erlmcp_distributed_session ! {membership_event, {member_dead, DeadNode}},
    timer:sleep(200),

    %% After failover the session should still exist in the replica map
    %% but now with ReplicaNode as primary.
    S = sys:get_state(erlmcp_distributed_session),
    Replicas = element(6, S),
    ?assert(maps:is_key(SessionId, Replicas),
            "Session should still exist in replica map after failover"),
    Replica = maps:get(SessionId, Replicas),
    %% replica_state tuple: {replica_state, session_id, primary_node, ...}
    NewPrimary = element(3, Replica),
    ?assertEqual(ReplicaNode, NewPrimary,
                 "Primary should be promoted to first replica after dead-node event").

%% A session whose primary is NOT the dead node should not be touched.
test_session_on_live_node_unaffected() ->
    DeadNode   = dead_other_node@nohost,
    LiveNode   = live_other_node@nohost,
    ReplicaNode = replica_other_node@nohost,
    SessionId   = <<"t019-session-live">>,

    ReplicaEntry = make_replica_state(SessionId, LiveNode, [ReplicaNode]),
    sys:replace_state(erlmcp_distributed_session, fun(S) ->
        OldReplicas = element(6, S),
        setelement(6, S, maps:put(SessionId, ReplicaEntry, OldReplicas))
    end),

    %% Trigger death of an unrelated node
    erlmcp_distributed_session ! {membership_event, {member_dead, DeadNode}},
    timer:sleep(200),

    %% Primary should still be LiveNode
    S = sys:get_state(erlmcp_distributed_session),
    Replicas = element(6, S),
    Replica = maps:get(SessionId, Replicas, undefined),
    ?assertNotEqual(undefined, Replica),
    NewPrimary = element(3, Replica),
    ?assertEqual(LiveNode, NewPrimary,
                 "Session on live node must not be affected by unrelated node death").

%%====================================================================
%% Helpers
%%====================================================================

%% Build a #replica_state{} tuple.
%% Record fields: {replica_state, session_id, primary_node, replica_nodes,
%%                 last_sync, version}
make_replica_state(SessionId, PrimaryNode, ReplicaNodes) ->
    {replica_state, SessionId, PrimaryNode, ReplicaNodes,
     erlang:system_time(millisecond), 1}.
