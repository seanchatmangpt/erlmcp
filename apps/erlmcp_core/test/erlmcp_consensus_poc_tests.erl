-module(erlmcp_consensus_poc_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

consensus_basic_test() ->
    %% Start 3 nodes
    {ok, Node1} = erlmcp_consensus_poc:start_link(node1),
    {ok, Node2} = erlmcp_consensus_poc:start_link(node2),
    {ok, Node3} = erlmcp_consensus_poc:start_link(node3),
    timer:sleep(200), %% Let election settle

    %% Verify one leader, two followers
    Nodes = [Node1, Node2, Node3],
    Statuses = [erlmcp_consensus_poc:get_status(N) || N <- Nodes],
    Roles = [maps:get(role, S) || S <- Statuses],

    ?assertEqual(1, length([R || R <- Roles, R =:= leader])),
    ?assertEqual(2, length([R || R <- Roles, R =:= follower])),

    %% Cleanup
    [erlmcp_consensus_poc:stop(N) || N <- Nodes].

tool_execution_test() ->
    %% Start 2 nodes
    {ok, Node1} = erlmcp_consensus_poc:start_link(node1),
    {ok, Node2} = erlmcp_consensus_poc:start_link(node2),
    timer:sleep(200),

    %% Execute tool from both nodes
    {ok, Result1} = erlmcp_consensus_poc:execute_tool(Node1, <<"add">>, #{<<"a">> => 5, <<"b">> => 3}),
    {ok, Result2} = erlmcp_consensus_poc:execute_tool(Node2, <<"multiply">>, #{<<"x">> => 4, <<"y">> => 2}),

    %% Verify results
    ?assertEqual(8, Result1),
    ?assertEqual(8, Result2),

    %% Verify executions happened on leader
    Status1 = erlmcp_consensus_poc:get_status(Node1),
    Status2 = erlmcp_consensus_poc:get_status(Node2),

    TotalExecutions = maps:get(execution_count, Status1) + maps:get(execution_count, Status2),
    ?assertEqual(2, TotalExecutions),

    %% Cleanup
    erlmcp_consensus_poc:stop(Node1),
    erlmcp_consensus_poc:stop(Node2).

leader_failover_test() ->
    %% Start 3 nodes
    {ok, Node1} = erlmcp_consensus_poc:start_link(node1),
    {ok, Node2} = erlmcp_consensus_poc:start_link(node2),
    {ok, Node3} = erlmcp_consensus_poc:start_link(node3),
    timer:sleep(200),

    %% Find leader
    Nodes = [Node1, Node2, Node3],
    Statuses = [erlmcp_consensus_poc:get_status(N) || N <- Nodes],
    LeaderNode = lists:foldl(fun({N, S}, Acc) ->
        case maps:get(role, S) of
            leader -> N;
            _ -> Acc
        end
    end, undefined, lists:zip(Nodes, Statuses)),

    ?assert(LeaderNode =/= undefined),

    %% Kill leader
    erlmcp_consensus_poc:stop(LeaderNode),
    timer:sleep(300), %% Wait for re-election

    %% Verify new leader elected
    Remaining = lists:delete(LeaderNode, Nodes),
    NewStatuses = [erlmcp_consensus_poc:get_status(N) || N <- Remaining],
    NewRoles = [maps:get(role, S) || S <- NewStatuses],

    ?assertEqual(1, length([R || R <- NewRoles, R =:= leader])),

    %% Verify can still execute tools
    [FirstRemaining | _] = Remaining,
    {ok, Result} = erlmcp_consensus_poc:execute_tool(FirstRemaining, <<"add">>, #{<<"a">> => 10, <<"b">> => 20}),
    ?assertEqual(30, Result),

    %% Cleanup
    [erlmcp_consensus_poc:stop(N) || N <- Remaining].

exactly_once_semantics_test() ->
    %% Start 3 nodes
    {ok, Node1} = erlmcp_consensus_poc:start_link(node1),
    {ok, Node2} = erlmcp_consensus_poc:start_link(node2),
    {ok, Node3} = erlmcp_consensus_poc:start_link(node3),
    timer:sleep(200),

    %% Execute multiple tools from different nodes
    Nodes = [Node1, Node2, Node3],
    [erlmcp_consensus_poc:execute_tool(Node1, <<"add">>, #{<<"a">> => I, <<"b">> => I}) || I <- lists:seq(1, 5)],
    [erlmcp_consensus_poc:execute_tool(Node2, <<"multiply">>, #{<<"x">> => I, <<"y">> => I}) || I <- lists:seq(1, 3)],
    [erlmcp_consensus_poc:execute_tool(Node3, <<"subtract">>, #{<<"a">> => 10, <<"b">> => I}) || I <- lists:seq(1, 4)],
    timer:sleep(100),

    %% Verify all executions are on leader only
    Statuses = [erlmcp_consensus_poc:get_status(N) || N <- Nodes],
    ExecutionCounts = [maps:get(execution_count, S) || S <- Statuses],
    TotalExecutions = lists:sum(ExecutionCounts),

    %% Exactly 12 executions (5 + 3 + 4)
    ?assertEqual(12, TotalExecutions),

    %% All executions on one node (the leader)
    ?assert(lists:member(12, ExecutionCounts)),
    ?assertEqual(2, length([C || C <- ExecutionCounts, C =:= 0])),

    %% Cleanup
    [erlmcp_consensus_poc:stop(N) || N <- Nodes].
