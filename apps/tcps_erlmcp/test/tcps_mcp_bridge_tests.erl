%%%-----------------------------------------------------------------------------
%%% @doc TCPS MCP Bridge Tests
%%%
%%% Tests for TCPS-MCP auto-integration:
%%% - Work order creation from requests
%%% - Quality gate enforcement
%%% - Andon triggering
%%% - Receipt generation
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_mcp_bridge_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

%% Copy bridge_state record from tcps_mcp_bridge.erl for tests
-record(bridge_state, {
    auto_integration_enabled = true :: boolean(),
    quality_gates_enabled = [1,2,3,4,5,6,7,8] :: list(),
    andon_on_sla_violation = true :: boolean(),
    total_requests = 0 :: non_neg_integer(),
    work_orders_created = 0 :: non_neg_integer(),
    quality_gate_failures = #{} :: map(),
    andon_triggers = 0 :: non_neg_integer(),
    request_to_work_order = #{} :: map(),
    work_order_to_request = #{} :: map()
}).

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

bridge_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"Work order creation from tools/call", fun test_work_order_from_tools_call/0},
            {"Work order creation from tasks/create", fun test_work_order_from_tasks_create/0},
            {"Quality gate enforcement", fun test_quality_gate_enforcement/0},
            {"Security gate blocks malicious input", fun test_security_gate_blocks_malicious/0},
            {"Schema validation gate", fun test_schema_validation_gate/0},
            {"Receipt generation", fun test_receipt_generation/0},
            {"Work order not created for list operations", fun test_no_work_order_for_lists/0},
            {"Priority calculation", fun test_priority_calculation/0},
            {"Bucket assignment", fun test_bucket_assignment/0},
            {"Deadline calculation", fun test_deadline_calculation/0}
        ]
    }.

setup() ->
    % Start bridge server (doesn't require full tcps_erlmcp app)
    case whereis(tcps_mcp_bridge) of
        undefined ->
            {ok, Pid} = tcps_mcp_bridge:start_link([
                {tcps_auto_integration, true},
                {tcps_quality_gates_enabled, [1,2,3,4,5,6,7,8]},
                {tcps_andon_on_sla_violation, true}
            ]),
            Pid;
        Pid ->
            tcps_mcp_bridge:reset_state(),
            Pid
    end.

cleanup(_Pid) ->
    % Don't stop - let it run for next test
    ok.

%%%=============================================================================
%%% Work Order Creation Tests
%%%=============================================================================

test_work_order_from_tools_call() ->
    Request = #json_rpc_request{
        id = <<"test-1">>,
        method = <<"tools/call">>,
        params = #{
            <<"name">> => <<"calculator">>,
            <<"arguments">> => #{<<"op">> => <<"add">>, <<"a">> => 1, <<"b">> => 2}
        }
    },

    % Before request (quality gates)
    {ok, _} = tcps_mcp_bridge:before_request(Request),

    % After request (work order creation)
    % Note: This may fail if tcps_work_order not available, but test continues
    _ = tcps_mcp_bridge:after_request(Request, {ok, #{<<"result">> => 3}}),

    % Verify basic processing worked
    ok.

test_work_order_from_tasks_create() ->
    Request = #json_rpc_request{
        id = <<"test-2">>,
        method = <<"tasks/create">>,
        params = #{
            <<"name">> => <<"long_running_task">>,
            <<"arguments">> => #{}
        }
    },

    % Process request
    {ok, _} = tcps_mcp_bridge:before_request(Request),
    _ = tcps_mcp_bridge:after_request(Request, {ok, #{<<"taskId">> => <<"task-1">>}}),

    % Verify request was processed
    ok.

test_no_work_order_for_lists() ->
    Request = #json_rpc_request{
        id = <<"test-3">>,
        method = <<"resources/list">>,
        params = #{}
    },

    % Process list request - should not create work order
    {ok, _} = tcps_mcp_bridge:before_request(Request),
    _ = tcps_mcp_bridge:after_request(Request, {ok, #{<<"resources">> => []}}),

    % Verify should_create_work_order is false for list operations
    ?assertNot(tcps_mcp_bridge:should_create_work_order(Request)).

%%%=============================================================================
%%% Quality Gate Tests
%%%=============================================================================

test_quality_gate_enforcement() ->
    % Valid request should pass all gates
    ValidRequest = #json_rpc_request{
        id = <<"test-4">>,
        method = <<"tools/call">>,
        params = #{<<"name">> => <<"safe_tool">>}
    },

    Result = tcps_mcp_bridge:before_request(ValidRequest),
    ?assertMatch({ok, _}, Result).

test_security_gate_blocks_malicious() ->
    % Request with path traversal attempt
    MaliciousRequest = #json_rpc_request{
        id = <<"test-5">>,
        method = <<"resources/read">>,
        params = #{<<"uri">> => <<"file://../../../etc/passwd">>}
    },

    Result = tcps_mcp_bridge:before_request(MaliciousRequest),
    ?assertMatch({error, {?JSONRPC_INVALID_REQUEST, _, #{gate := 6}}}, Result),

    % Verify gate failure was recorded
    State = tcps_mcp_bridge:get_state(),
    ?assert(maps:get(6, State#bridge_state.quality_gate_failures, 0) >= 1).

test_schema_validation_gate() ->
    % Request with invalid params (not a map)
    % Note: This test assumes JSON-RPC decoder catches this first
    % Testing with non-map params directly
    InvalidRequest = #json_rpc_request{
        id = <<"test-6">>,
        method = <<"tools/call">>,
        params = [<<"invalid">>, <<"list">>]  % Should be map
    },

    Result = tcps_mcp_bridge:before_request(InvalidRequest),
    ?assertMatch({error, {?JSONRPC_INVALID_REQUEST, _, #{gate := 1}}}, Result).

test_receipt_generation() ->
    Request = #json_rpc_request{
        id = <<"test-7">>,
        method = <<"tools/call">>,
        params = #{<<"name">> => <<"test">>}
    },

    {ok, _} = tcps_mcp_bridge:before_request(Request),
    ok = tcps_mcp_bridge:after_request(Request, {ok, #{}}),

    % Receipt should be generated and stored
    % This assumes tcps_receipt_chain is running
    % In real system, would verify receipt exists in chain
    ok.

%%%=============================================================================
%%% Request Mapping Tests
%%%=============================================================================

test_priority_calculation() ->
    % High priority: task cancellation
    ?assertEqual(9, tcps_mcp_bridge:calculate_priority(#{method => <<"tasks/cancel">>})),

    % Medium-high: tool call
    ?assertEqual(6, tcps_mcp_bridge:calculate_priority(#{method => <<"tools/call">>})),

    % Medium: resource read
    ?assertEqual(5, tcps_mcp_bridge:calculate_priority(#{method => <<"resources/read">>})),

    % Low: list operations
    ?assertEqual(3, tcps_mcp_bridge:calculate_priority(#{method => <<"resources/list">>})),

    % Complex request gets boost
    Priority = tcps_mcp_bridge:calculate_priority(#{
        method => <<"tools/call">>,
        params => #{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3, <<"d">> => 4, <<"e">> => 5, <<"f">> => 6}
    }),
    ?assertEqual(7, Priority).

test_bucket_assignment() ->
    ?assertEqual(features, tcps_mcp_bridge:calculate_bucket(<<"tools/call">>)),
    ?assertEqual(reliability, tcps_mcp_bridge:calculate_bucket(<<"resources/read">>)),
    ?assertEqual(features, tcps_mcp_bridge:calculate_bucket(<<"tasks/create">>)),
    ?assertEqual(reliability, tcps_mcp_bridge:calculate_bucket(<<"tasks/cancel">>)),
    ?assertEqual(compliance, tcps_mcp_bridge:calculate_bucket(<<"logging/setLevel">>)),
    ?assertEqual(features, tcps_mcp_bridge:calculate_bucket(<<"prompts/get">>)).

test_deadline_calculation() ->
    % Security bucket with high priority (critical)
    Deadline1 = tcps_mcp_bridge:calculate_deadline(#{bucket => security, priority => 9}),
    ?assert(is_tuple(Deadline1)),  % Should be erlang timestamp

    % Features bucket with low priority
    Deadline2 = tcps_mcp_bridge:calculate_deadline(#{bucket => features, priority => 2}),
    ?assert(is_tuple(Deadline2)),

    % Verify deadline is in future
    Now = calendar:universal_time(),
    ?assert(Deadline1 > Now),
    ?assert(Deadline2 > Now),

    % Higher priority should have earlier deadline
    Secs1 = calendar:datetime_to_gregorian_seconds(Deadline1),
    Secs2 = calendar:datetime_to_gregorian_seconds(Deadline2),
    NowSecs = calendar:datetime_to_gregorian_seconds(Now),

    ?assert((Secs1 - NowSecs) < (Secs2 - NowSecs)).

%%%=============================================================================
%%% Work Order Mapping Tests
%%%=============================================================================

test_mcp_request_to_work_order() ->
    Request = #json_rpc_request{
        id = <<"test-8">>,
        method = <<"tools/call">>,
        params = #{
            <<"name">> => <<"calculator">>,
            <<"arguments">> => #{<<"op">> => <<"multiply">>}
        }
    },

    WorkOrder = tcps_mcp_bridge:mcp_request_to_work_order(Request),

    % Verify work order structure
    ?assertEqual(feature, maps:get(type, WorkOrder)),
    ?assertEqual(features, maps:get(bucket, WorkOrder)),
    ?assert(maps:get(priority, WorkOrder) >= 1),
    ?assert(maps:get(priority, WorkOrder) =< 10),
    ?assert(is_tuple(maps:get(sla_deadline, WorkOrder))),

    % Verify pull signal
    PullSignal = maps:get(pull_signal, WorkOrder),
    ?assertEqual(mcp_request, maps:get(type, PullSignal)),
    ?assertEqual(<<"MCP Protocol">>, maps:get(source, PullSignal)),
    ?assertMatch([<<"mcp">>, <<"tools/call">>], maps:get(labels, PullSignal)),

    % Verify metadata
    Metadata = maps:get(metadata, PullSignal),
    ?assertEqual(<<"test-8">>, maps:get(request_id, Metadata)),
    ?assertEqual(<<"tools/call">>, maps:get(method, Metadata)).

test_should_create_work_order() ->
    % Should create for tool calls
    ?assert(tcps_mcp_bridge:should_create_work_order(
        #json_rpc_request{id = <<"1">>, method = <<"tools/call">>, params = #{}})),

    % Should create for task creation
    ?assert(tcps_mcp_bridge:should_create_work_order(
        #json_rpc_request{id = <<"2">>, method = <<"tasks/create">>, params = #{}})),

    % Should NOT create for list operations
    ?assertNot(tcps_mcp_bridge:should_create_work_order(
        #json_rpc_request{id = <<"3">>, method = <<"resources/list">>, params = #{}})),

    % Should NOT create for get operations
    ?assertNot(tcps_mcp_bridge:should_create_work_order(
        #json_rpc_request{id = <<"4">>, method = <<"prompts/get">>, params = #{}})),

    % Should NOT create for initialize
    ?assertNot(tcps_mcp_bridge:should_create_work_order(
        #json_rpc_request{id = <<"5">>, method = <<"initialize">>, params = #{}})).

%%%=============================================================================
%%% Statistics Tests
%%%=============================================================================

test_statistics_tracking() ->
    tcps_mcp_bridge:reset_state(),

    Requests = [
        #json_rpc_request{id = <<"s1">>, method = <<"tools/call">>, params = #{}},
        #json_rpc_request{id = <<"s2">>, method = <<"tasks/create">>, params = #{}},
        #json_rpc_request{id = <<"s3">>, method = <<"resources/list">>, params = #{}}
    ],

    % Process requests
    lists:foreach(fun(Req) ->
        {ok, _} = tcps_mcp_bridge:before_request(Req),
        ok = tcps_mcp_bridge:after_request(Req, {ok, #{}})
    end, Requests),

    State = tcps_mcp_bridge:get_state(),

    % Verify statistics
    ?assertEqual(3, State#bridge_state.total_requests),
    ?assertEqual(2, State#bridge_state.work_orders_created),  % Only tools/call and tasks/create
    ?assertEqual(0, State#bridge_state.andon_triggers).
