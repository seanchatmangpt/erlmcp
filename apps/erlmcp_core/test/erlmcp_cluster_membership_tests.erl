%%%-------------------------------------------------------------------
%%% @doc erlmcp_cluster_membership_tests — EUnit tests for gossip merge (T018)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cluster_membership_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Gossip merge tests (T018)
%%====================================================================

gossip_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"Newer gossip updates local state", fun test_newer_gossip_updates_state/0},
      {"Older gossip is ignored",          fun test_older_gossip_ignored/0}]}.

%%====================================================================
%% Setup / cleanup
%%====================================================================

setup() ->
    %% Start membership server; use a unique name to avoid clashes with
    %% other suites running in the same node.
    {ok, Pid} = erlmcp_cluster_membership:start_link(#{
        heartbeat_interval => 60000,   %% long — we don't want timers firing
        failure_timeout    => 60000,
        suspect_timeout    => 60000,
        sync_interval      => 60000
    }),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid, normal, 5000),
    timer:sleep(50),
    ok.

%%====================================================================
%% Individual test functions
%%====================================================================

%% Gossip carrying a node with a higher incarnation should be merged in.
test_newer_gossip_updates_state() ->
    RemoteNode = gossip_remote_node@nohost,

    %% Before gossip: remote node is not known
    Members0 = erlmcp_cluster_membership:get_members(),
    ?assertEqual(false, maps:is_key(RemoteNode, Members0)),

    %% Build a gossip_state record with RemoteNode at incarnation 5.
    %% We use the gen_server cast interface so we don't need the record syntax.
    GossipState = build_gossip_state(RemoteNode, alive, 5),
    gen_server:cast(erlmcp_cluster_membership, {gossip, node(), GossipState}),

    %% Give the gen_server time to process
    timer:sleep(100),

    %% Remote node should now appear in members
    Members1 = erlmcp_cluster_membership:get_members(),
    ?assert(maps:is_key(RemoteNode, Members1),
            "Remote node should be visible after receiving newer gossip").

%% Gossip carrying a node with a lower incarnation than our local view
%% should NOT overwrite our existing entry.
test_older_gossip_ignored() ->
    RemoteNode = gossip_stale_node@nohost,

    %% Seed the server with incarnation 10 for this node via a higher-incarnation gossip
    GossipStateNew = build_gossip_state(RemoteNode, alive, 10),
    gen_server:cast(erlmcp_cluster_membership, {gossip, node(), GossipStateNew}),
    timer:sleep(100),

    %% Now send stale gossip at incarnation 2
    GossipStateOld = build_gossip_state(RemoteNode, dead, 2),
    gen_server:cast(erlmcp_cluster_membership, {gossip, node(), GossipStateOld}),
    timer:sleep(100),

    %% Node should still show alive (incarnation 10 wins)
    View = erlmcp_cluster_membership:get_membership_view(),
    NodeView = maps:get(RemoteNode, maps:get(members, View, #{}), undefined),
    ?assertNotEqual(undefined, NodeView),
    ?assertEqual(alive, maps:get(status, NodeView),
                 "Stale gossip must not overwrite newer local state").

%%====================================================================
%% Helper: construct a gossip_state-shaped term that handle_gossip accepts.
%% We use a tagged tuple that mirrors the #gossip_state{} record so we
%% don't need to copy the record definition here.
%%====================================================================

%% Build a gossip_state record term.  Because record definitions are local to
%% the source module we construct the tuple directly.
%%   #gossip_state{members = Members, incarnation = Inc}
%% The record tag is 'gossip_state', field order matches the -record() in
%% erlmcp_cluster_membership.erl.
build_gossip_state(Node, Status, Incarnation) ->
    Member = build_member(Node, Status, Incarnation),
    Members = #{Node => Member},
    %% gossip_state record: {gossip_state, Members, Incarnation}
    {gossip_state, Members, Incarnation}.

%% Build a member record term.
%% #member{node, status, incarnation, last_heartbeat, suspect_count, suspect_start}
build_member(Node, Status, Incarnation) ->
    {member, Node, Status, Incarnation,
     erlang:system_time(millisecond), 0, undefined}.
