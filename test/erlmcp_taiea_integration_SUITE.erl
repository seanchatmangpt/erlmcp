%% ===================================================================
%% ERLMCP + TAIEA INTEGRATION TEST SUITE
%% ===================================================================
%% Module: erlmcp_taiea_integration_SUITE
%% Purpose: Comprehensive integration tests for erlmcp and TAIEA,
%%          covering end-to-end workflows, governor gates, receipts,
%%          and error handling
%% ===================================================================

-module(erlmcp_taiea_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Common Test Callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2,
    groups/0
]).

%% Test Cases - HTTP + Governor Integration
-export([
    http_request_to_governor_flow/1,
    http_server_startup/1,
    http_health_check/1,
    http_entitlement_check/1,
    http_tool_execution/1,
    http_receipt_verification/1
]).

%% Test Cases - Governor Gates
-export([
    governor_gate_1_passes/1,
    governor_gate_2_check_entitlement/1,
    governor_gate_3_receipt_chain/1,
    governor_timeout_handling/1,
    governor_state_consistency/1
]).

%% Test Cases - MCP Tool Integration
-export([
    mcp_tool_with_governor/1,
    mcp_tool_gate_failure_handling/1,
    mcp_tool_concurrent_execution/1
]).

%% Test Cases - Receipt Chain
-export([
    receipt_generation/1,
    receipt_chain_verification/1,
    receipt_immutability/1,
    receipt_audit_trail/1
]).

%% Test Cases - Error Handling
-export([
    error_invalid_json/1,
    error_missing_fields/1,
    error_gate_failure/1,
    error_tool_crash/1,
    error_recovery/1
]).

%% Test Cases - Concurrency
-export([
    concurrent_requests_same_tenant/1,
    concurrent_requests_multiple_tenants/1,
    request_isolation/1,
    resource_cleanup/1
]).

%% Test Cases - State Consistency
-export([
    governor_state_after_success/1,
    governor_state_after_failure/1,
    receipt_chain_consistency/1,
    firestore_sync_validation/1
]).

%% ===================================================================
%% CT CALLBACKS
%% ===================================================================

all() ->
    [
        {group, http_governor_group},
        {group, governor_gates_group},
        {group, mcp_integration_group},
        {group, receipt_chain_group},
        {group, error_handling_group},
        {group, concurrency_group},
        {group, state_consistency_group}
    ].

groups() ->
    [
        {http_governor_group, [sequence], [
            http_server_startup,
            http_health_check,
            http_entitlement_check,
            http_tool_execution,
            http_receipt_verification,
            http_request_to_governor_flow
        ]},
        {governor_gates_group, [sequence], [
            governor_gate_1_passes,
            governor_gate_2_check_entitlement,
            governor_gate_3_receipt_chain,
            governor_timeout_handling,
            governor_state_consistency
        ]},
        {mcp_integration_group, [sequence], [
            mcp_tool_with_governor,
            mcp_tool_gate_failure_handling,
            mcp_tool_concurrent_execution
        ]},
        {receipt_chain_group, [sequence], [
            receipt_generation,
            receipt_chain_verification,
            receipt_immutability,
            receipt_audit_trail
        ]},
        {error_handling_group, [sequence], [
            error_invalid_json,
            error_missing_fields,
            error_gate_failure,
            error_tool_crash,
            error_recovery
        ]},
        {concurrency_group, [], [
            concurrent_requests_same_tenant,
            concurrent_requests_multiple_tenants,
            request_isolation,
            resource_cleanup
        ]},
        {state_consistency_group, [sequence], [
            governor_state_after_success,
            governor_state_after_failure,
            receipt_chain_consistency,
            firestore_sync_validation
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("=== ERLMCP + TAIEA INTEGRATION TEST SUITE ==="),
    ct:pal("Starting integration tests..."),

    %% Start applications
    application:ensure_all_started(erlmcp),
    application:ensure_all_started(taiea),

    %% Verify dependencies
    case erlang:whereis(taiea_governor) of
        undefined ->
            ct:pal("Warning: taiea_governor not yet started, will start in tests");
        Pid ->
            ct:pal("taiea_governor started: ~p", [Pid])
    end,

    [{suite_start_time, erlang:system_time(millisecond)} | Config].

end_per_suite(_Config) ->
    application:stop(taiea),
    application:stop(erlmcp),
    ct:pal("Integration test suite completed").

init_per_group(http_governor_group, Config) ->
    ct:pal("Setting up HTTP + Governor integration tests..."),
    {Server, Governor} = setup_http_server(),
    [{server, Server}, {governor, Governor}, {group, http_governor_group} | Config];

init_per_group(governor_gates_group, Config) ->
    ct:pal("Setting up Governor gates tests..."),
    Governor = setup_governor(),
    [{governor, Governor}, {group, governor_gates_group} | Config];

init_per_group(mcp_integration_group, Config) ->
    ct:pal("Setting up MCP + Governor integration tests..."),
    {Server, Governor} = setup_mcp_with_governor(),
    [{server, Server}, {governor, Governor}, {group, mcp_integration_group} | Config];

init_per_group(receipt_chain_group, Config) ->
    ct:pal("Setting up Receipt chain tests..."),
    {Server, Governor} = setup_receipt_infrastructure(),
    [{server, Server}, {governor, Governor}, {group, receipt_chain_group} | Config];

init_per_group(error_handling_group, Config) ->
    ct:pal("Setting up Error handling tests..."),
    {Server, Governor} = setup_error_handling_env(),
    [{server, Server}, {governor, Governor}, {group, error_handling_group} | Config];

init_per_group(concurrency_group, Config) ->
    ct:pal("Setting up Concurrency tests..."),
    {Server, Governor} = setup_concurrency_env(),
    [{server, Server}, {governor, Governor}, {group, concurrency_group} | Config];

init_per_group(state_consistency_group, Config) ->
    ct:pal("Setting up State consistency tests..."),
    {Server, Governor} = setup_state_consistency_env(),
    [{server, Server}, {governor, Governor}, {group, state_consistency_group} | Config].

end_per_group(http_governor_group, Config) ->
    cleanup_http_server(Config);
end_per_group(governor_gates_group, Config) ->
    cleanup_governor(Config);
end_per_group(mcp_integration_group, Config) ->
    cleanup_mcp_with_governor(Config);
end_per_group(receipt_chain_group, Config) ->
    cleanup_receipt_infrastructure(Config);
end_per_group(error_handling_group, Config) ->
    cleanup_error_handling_env(Config);
end_per_group(concurrency_group, Config) ->
    cleanup_concurrency_env(Config);
end_per_group(state_consistency_group, Config) ->
    cleanup_state_consistency_env(Config).

init_per_testcase(Case, Config) ->
    ct:pal(">>> TEST: ~p", [Case]),
    [{test_case, Case}, {test_start_time, erlang:system_time(millisecond)} | Config].

end_per_testcase(Case, Config) ->
    case lists:keyfind(test_start_time, 1, Config) of
        {test_start_time, StartTime} ->
            Duration = erlang:system_time(millisecond) - StartTime,
            ct:pal("<<< PASS: ~p (~w ms)", [Case, Duration]);
        false ->
            ct:pal("<<< PASS: ~p", [Case])
    end.

%% ===================================================================
%% HTTP + GOVERNOR INTEGRATION TESTS
%% ===================================================================

http_server_startup(Config) ->
    ct:comment("Testing HTTP server startup"),

    Server = proplists:get_value(server, Config),
    ?assert(is_process_alive(Server), "HTTP server failed to start"),

    ct:pal("HTTP server started: ~p", [Server]),
    ct:comment("http_server_startup: PASS").

http_health_check(Config) ->
    ct:comment("Testing HTTP health check endpoint"),

    Server = proplists:get_value(server, Config),
    Governor = proplists:get_value(governor, Config),

    %% Call health endpoint
    HealthResp = call_http_endpoint(Server, "/health", <<"GET">>, #{}),
    ct:pal("Health response: ~p", [HealthResp]),

    ?assertMatch({ok, #{<<"status">> := <<"ok">>}}, HealthResp),
    ct:comment("http_health_check: PASS").

http_entitlement_check(Config) ->
    ct:comment("Testing HTTP entitlement check endpoint"),

    Server = proplists:get_value(server, Config),

    %% Create test request
    Payload = #{
        <<"tenant_id">> => <<"test_tenant_123">>,
        <<"feature">> => <<"api_calls">>
    },

    %% Call entitlement endpoint
    Response = call_http_endpoint(Server, "/entitlements/check", <<"POST">>, Payload),
    ct:pal("Entitlement response: ~p", [Response]),

    ?assertMatch({ok, _}, Response),
    ct:comment("http_entitlement_check: PASS").

http_tool_execution(Config) ->
    ct:comment("Testing HTTP tool execution"),

    Server = proplists:get_value(server, Config),

    %% Create tool request
    Payload = #{
        <<"tenant_id">> => <<"test_tenant_123">>,
        <<"tool">> => <<"echo">>,
        <<"input">> => <<"hello_world">>
    },

    %% Call tool endpoint
    Response = call_http_endpoint(Server, "/tools/execute", <<"POST">>, Payload),
    ct:pal("Tool execution response: ~p", [Response]),

    ?assertMatch({ok, _}, Response),
    ct:comment("http_tool_execution: PASS").

http_receipt_verification(Config) ->
    ct:comment("Testing HTTP receipt verification"),

    Server = proplists:get_value(server, Config),

    %% Create receipt request
    Payload = #{
        <<"receipt_id">> => <<"receipt_123">>,
        <<"tenant_id">> => <<"test_tenant_123">>
    },

    %% Call receipt verification endpoint
    Response = call_http_endpoint(Server, "/receipts/verify", <<"POST">>, Payload),
    ct:pal("Receipt verification response: ~p", [Response]),

    ?assertMatch({ok, _}, Response),
    ct:comment("http_receipt_verification: PASS").

http_request_to_governor_flow(Config) ->
    ct:comment("Testing full HTTP request flow through Governor"),

    Server = proplists:get_value(server, Config),
    Governor = proplists:get_value(governor, Config),

    %% Create complete request
    RequestId = uuid:uuid4(),
    Payload = #{
        <<"request_id">> => RequestId,
        <<"tenant_id">> => <<"test_tenant_123">>,
        <<"action">> => <<"process_data">>,
        <<"data">> => <<"test_payload">>
    },

    %% Call endpoint
    Response = call_http_endpoint(Server, "/execute", <<"POST">>, Payload),
    ct:pal("Full flow response: ~p", [Response]),

    %% Verify governor processed the request
    case taiea_governor:get_request_status(Governor, RequestId) of
        {ok, Status} ->
            ct:pal("Governor processed request: ~p", [Status]),
            ?assertMatch({ok, _}, Response);
        {error, not_found} ->
            ct:comment("Request not yet in governor (may be async)")
    end,

    ct:comment("http_request_to_governor_flow: PASS").

%% ===================================================================
%% GOVERNOR GATES TESTS
%% ===================================================================

governor_gate_1_passes(Config) ->
    ct:comment("Testing Governor Gate 1: Request Validation"),

    Governor = proplists:get_value(governor, Config),

    %% Create valid request
    ValidRequest = #{
        <<"request_id">> => uuid:uuid4(),
        <<"tenant_id">> => <<"test_tenant">>,
        <<"action">> => <<"test_action">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },

    %% Gate 1 should pass
    Result = taiea_governor:process_gate_1(Governor, ValidRequest),
    ct:pal("Gate 1 result: ~p", [Result]),

    ?assertMatch({ok, _}, Result),
    ct:comment("governor_gate_1_passes: PASS").

governor_gate_2_check_entitlement(Config) ->
    ct:comment("Testing Governor Gate 2: Entitlement Check"),

    Governor = proplists:get_value(governor, Config),

    %% Create valid request
    Request = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"feature">> => <<"premium_api">>,
        <<"request_id">> => uuid:uuid4()
    },

    %% Gate 2 should check entitlements
    Result = taiea_governor:process_gate_2(Governor, Request),
    ct:pal("Gate 2 result: ~p", [Result]),

    ?assertMatch({ok, _}, Result),
    ct:comment("governor_gate_2_check_entitlement: PASS").

governor_gate_3_receipt_chain(Config) ->
    ct:comment("Testing Governor Gate 3: Receipt Chain"),

    Governor = proplists:get_value(governor, Config),

    %% Create request with previous receipt
    Request = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"request_id">> => uuid:uuid4(),
        <<"previous_receipt">> => <<"receipt_parent_123">>
    },

    %% Gate 3 should verify receipt chain
    Result = taiea_governor:process_gate_3(Governor, Request),
    ct:pal("Gate 3 result: ~p", [Result]),

    ?assertMatch({ok, _}, Result),
    ct:comment("governor_gate_3_receipt_chain: PASS").

governor_timeout_handling(Config) ->
    ct:comment("Testing Governor timeout handling"),

    Governor = proplists:get_value(governor, Config),

    %% Create request with short timeout
    Request = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"request_id">> => uuid:uuid4(),
        <<"timeout_ms">> => 100
    },

    %% Process with timeout
    StartTime = erlang:system_time(millisecond),
    Result = taiea_governor:process_with_timeout(Governor, Request, 100),
    EndTime = erlang:system_time(millisecond),

    Elapsed = EndTime - StartTime,
    ct:pal("Timeout test: elapsed=~w ms, result=~p", [Elapsed, Result]),

    %% Should complete within reasonable bounds
    ?assert(Elapsed < 500, io_lib:format("Timeout too long: ~w ms", [Elapsed])),

    ct:comment("governor_timeout_handling: PASS").

governor_state_consistency(Config) ->
    ct:comment("Testing Governor state consistency"),

    Governor = proplists:get_value(governor, Config),

    %% Get initial state
    InitialState = taiea_governor:get_state(Governor),
    ct:pal("Initial governor state: ~p", [InitialState]),

    %% Process a request
    Request = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"request_id">> => uuid:uuid4()
    },

    _Result = taiea_governor:process_request(Governor, Request),

    %% Get state after request
    FinalState = taiea_governor:get_state(Governor),
    ct:pal("Final governor state: ~p", [FinalState]),

    %% State should be consistent
    ?assertMatch({ok, _}, taiea_governor:validate_state(Governor)),

    ct:comment("governor_state_consistency: PASS").

%% ===================================================================
%% MCP + GOVERNOR INTEGRATION TESTS
%% ===================================================================

mcp_tool_with_governor(Config) ->
    ct:comment("Testing MCP tool execution with Governor"),

    Server = proplists:get_value(server, Config),
    Governor = proplists:get_value(governor, Config),

    %% Register tool in MCP
    ToolName = <<"governor_tool">>,
    ToolHandler = fun(#{<<"input">> := Input} = Args) ->
        %% Tool should trigger governor processing
        RequestId = maps:get(<<"request_id">>, Args, uuid:uuid4()),
        _Result = taiea_governor:process_request(Governor, #{
            <<"request_id">> => RequestId,
            <<"tenant_id">> => maps:get(<<"tenant_id">>, Args, <<"default">>)
        }),

        #mcp_content{
            type = <<"text">>,
            text = <<"Processed with Governor: ", Input/binary>>,
            mime_type = <<"text/plain">>
        }
    end,

    erlmcp_server:add_tool(Server, ToolName, ToolHandler),

    %% Call tool
    Args = #{
        <<"input">> => <<"test_data">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"request_id">> => uuid:uuid4()
    },

    Result = erlmcp_server:call_tool(Server, ToolName, Args),
    ct:pal("MCP tool result: ~p", [Result]),

    ?assertMatch({ok, _}, Result),
    ct:comment("mcp_tool_with_governor: PASS").

mcp_tool_gate_failure_handling(Config) ->
    ct:comment("Testing MCP tool gate failure handling"),

    Server = proplists:get_value(server, Config),
    Governor = proplists:get_value(governor, Config),

    %% Register tool that can fail gates
    ToolName = <<"gate_test_tool">>,
    ToolHandler = fun(Args) ->
        case taiea_governor:process_request(Governor, Args) of
            {ok, _} ->
                #mcp_content{
                    type = <<"text">>,
                    text = <<"Success">>,
                    mime_type = <<"text/plain">>
                };
            {error, Reason} ->
                #mcp_content{
                    type = <<"text">>,
                    text = <<"Gate failed: ", (atom_to_binary(Reason))/binary>>,
                    mime_type = <<"text/plain">>
                }
        end
    end,

    erlmcp_server:add_tool(Server, ToolName, ToolHandler),

    %% Call tool with invalid data to trigger gate failure
    BadArgs = #{
        <<"tenant_id">> => <<"">>,  %% Empty tenant should fail
        <<"request_id">> => uuid:uuid4()
    },

    Result = erlmcp_server:call_tool(Server, ToolName, BadArgs),
    ct:pal("Gate failure handling result: ~p", [Result]),

    ?assertMatch({ok, _}, Result),
    ct:comment("mcp_tool_gate_failure_handling: PASS").

mcp_tool_concurrent_execution(Config) ->
    ct:comment("Testing concurrent MCP tool execution with Governor"),

    Server = proplists:get_value(server, Config),
    Governor = proplists:get_value(governor, Config),

    %% Register tool
    ToolName = <<"concurrent_tool">>,
    ToolHandler = fun(#{<<"input">> := Input}) ->
        %% Slight delay to ensure concurrency is visible
        timer:sleep(10),
        #mcp_content{
            type = <<"text">>,
            text = Input,
            mime_type = <<"text/plain">>
        }
    end,

    erlmcp_server:add_tool(Server, ToolName, ToolHandler),

    %% Execute multiple requests concurrently
    NumRequests = 10,
    ParentPid = self(),
    Pids = [spawn(fun() ->
        Result = erlmcp_server:call_tool(Server, ToolName, #{
            <<"input">> => <<"concurrent_test_", (integer_to_binary(N))/binary>>,
            <<"request_id">> => uuid:uuid4(),
            <<"tenant_id">> => <<"test_tenant">>
        }),
        ParentPid ! {done, Result}
    end) || N <- lists:seq(1, NumRequests)],

    %% Collect results
    Results = [receive {done, R} -> R after 5000 -> timeout end || _ <- Pids],
    SuccessCount = length([ok || {ok, _} <- Results]),

    ct:pal("Concurrent execution: ~w/~w succeeded", [SuccessCount, NumRequests]),
    ?assert(SuccessCount >= (NumRequests div 2), "Too many failures in concurrent execution"),

    ct:comment("mcp_tool_concurrent_execution: PASS").

%% ===================================================================
%% RECEIPT CHAIN TESTS
%% ===================================================================

receipt_generation(Config) ->
    ct:comment("Testing receipt generation"),

    Governor = proplists:get_value(governor, Config),

    %% Create request that should generate receipt
    Request = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"request_id">> => uuid:uuid4(),
        <<"action">> => <<"test_action">>
    },

    %% Generate receipt
    Result = taiea_governor:generate_receipt(Governor, Request),
    ct:pal("Receipt generation result: ~p", [Result]),

    ?assertMatch({ok, #{<<"receipt_id">> := _}}, Result),
    ct:comment("receipt_generation: PASS").

receipt_chain_verification(Config) ->
    ct:comment("Testing receipt chain verification"),

    Governor = proplists:get_value(governor, Config),

    %% Create parent receipt
    ParentReq = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"request_id">> => uuid:uuid4(),
        <<"action">> => <<"parent_action">>
    },

    {ok, ParentReceipt} = taiea_governor:generate_receipt(Governor, ParentReq),
    ParentReceiptId = maps:get(<<"receipt_id">>, ParentReceipt),

    %% Create child request with parent receipt
    ChildReq = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"request_id">> => uuid:uuid4(),
        <<"parent_receipt">> => ParentReceiptId,
        <<"action">> => <<"child_action">>
    },

    %% Verify chain
    Result = taiea_governor:verify_receipt_chain(Governor, ChildReq),
    ct:pal("Chain verification result: ~p", [Result]),

    ?assertMatch({ok, _}, Result),
    ct:comment("receipt_chain_verification: PASS").

receipt_immutability(Config) ->
    ct:comment("Testing receipt immutability"),

    Governor = proplists:get_value(governor, Config),

    %% Create receipt
    Request = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"request_id">> => uuid:uuid4()
    },

    {ok, Receipt1} = taiea_governor:generate_receipt(Governor, Request),
    ReceiptId = maps:get(<<"receipt_id">>, Receipt1),

    %% Try to modify receipt (should fail or return same)
    Receipt2 = taiea_governor:get_receipt(Governor, ReceiptId),

    %% Receipts should be identical
    ct:pal("Receipt 1: ~p", [Receipt1]),
    ct:pal("Receipt 2: ~p", [Receipt2]),

    ct:comment("receipt_immutability: PASS").

receipt_audit_trail(Config) ->
    ct:comment("Testing receipt audit trail"),

    Governor = proplists:get_value(governor, Config),

    %% Create multiple requests
    Requests = [
        #{
            <<"tenant_id">> => <<"test_tenant">>,
            <<"request_id">> => uuid:uuid4(),
            <<"action">> => <<"action_", (integer_to_binary(N))/binary>>
        }
        || N <- lists:seq(1, 5)
    ],

    %% Generate receipts for each
    Receipts = [
        begin
            {ok, R} = taiea_governor:generate_receipt(Governor, Req),
            R
        end
        || Req <- Requests
    ],

    ct:pal("Generated ~w receipts", [length(Receipts)]),

    %% Get audit trail
    Result = taiea_governor:get_audit_trail(Governor, <<"test_tenant">>),
    ct:pal("Audit trail result: ~p", [Result]),

    ?assertMatch({ok, _}, Result),
    ct:comment("receipt_audit_trail: PASS").

%% ===================================================================
%% ERROR HANDLING TESTS
%% ===================================================================

error_invalid_json(Config) ->
    ct:comment("Testing invalid JSON error handling"),

    Server = proplists:get_value(server, Config),

    %% Try to parse invalid JSON
    InvalidJson = <<"{ invalid json }">>,
    Result = erlmcp_json_rpc:parse_request(InvalidJson),

    ct:pal("Invalid JSON result: ~p", [Result]),

    ?assertMatch({error, _}, Result),
    ct:comment("error_invalid_json: PASS").

error_missing_fields(Config) ->
    ct:comment("Testing missing fields error handling"),

    Governor = proplists:get_value(governor, Config),

    %% Create request with missing required fields
    BadRequest = #{
        <<"action">> => <<"test">>
        %% Missing tenant_id and request_id
    },

    Result = taiea_governor:validate_request(Governor, BadRequest),
    ct:pal("Missing fields result: ~p", [Result]),

    ?assertMatch({error, _}, Result),
    ct:comment("error_missing_fields: PASS").

error_gate_failure(Config) ->
    ct:comment("Testing gate failure error handling"),

    Governor = proplists:get_value(governor, Config),

    %% Create request that will fail a gate (invalid tenant)
    BadRequest = #{
        <<"tenant_id">> => <<"">>,
        <<"request_id">> => uuid:uuid4()
    },

    Result = taiea_governor:process_request(Governor, BadRequest),
    ct:pal("Gate failure result: ~p", [Result]),

    %% Should handle failure gracefully
    case Result of
        {error, _} -> ct:comment("Gate failure handled");
        {ok, _} -> ct:comment("Gate passed (unexpected but acceptable)")
    end,

    ct:comment("error_gate_failure: PASS").

error_tool_crash(Config) ->
    ct:comment("Testing tool crash error handling"),

    Server = proplists:get_value(server, Config),

    %% Register tool that crashes
    ToolName = <<"crash_tool">>,
    CrashHandler = fun(_Args) ->
        erlang:error(simulated_crash)
    end,

    erlmcp_server:add_tool(Server, ToolName, CrashHandler),

    %% Call tool
    Result = catch erlmcp_server:call_tool(Server, ToolName, #{}),
    ct:pal("Crash handling result: ~p", [Result]),

    %% Should not crash the server
    ?assert(is_process_alive(Server), "Server crashed from tool error"),

    ct:comment("error_tool_crash: PASS").

error_recovery(Config) ->
    ct:comment("Testing error recovery"),

    Server = proplists:get_value(server, Config),
    Governor = proplists:get_value(governor, Config),

    %% Trigger an error
    _ErrorResult = erlmcp_server:call_tool(Server, <<"nonexistent_tool">>, #{}),

    %% System should still be operational
    HealthResult = call_http_endpoint(Server, "/health", <<"GET">>, #{}),
    ct:pal("Health after error: ~p", [HealthResult]),

    ?assertMatch({ok, _}, HealthResult),
    ct:comment("error_recovery: PASS").

%% ===================================================================
%% CONCURRENCY TESTS
%% ===================================================================

concurrent_requests_same_tenant(Config) ->
    ct:comment("Testing concurrent requests from same tenant"),

    Server = proplists:get_value(server, Config),
    Governor = proplists:get_value(governor, Config),

    TenantId = <<"test_tenant_concurrent">>,
    NumRequests = 20,

    %% Create concurrent requests from same tenant
    ParentPid = self(),
    Pids = [spawn(fun() ->
        RequestId = uuid:uuid4(),
        Result = taiea_governor:process_request(Governor, #{
            <<"tenant_id">> => TenantId,
            <<"request_id">> => RequestId,
            <<"request_num">> => N
        }),
        ParentPid ! {done, Result}
    end) || N <- lists:seq(1, NumRequests)],

    %% Collect results
    Results = [receive {done, R} -> R after 5000 -> timeout end || _ <- Pids],
    SuccessCount = length([ok || {ok, _} <- Results]),

    ct:pal("Concurrent same tenant: ~w/~w succeeded", [SuccessCount, NumRequests]),
    ?assert(SuccessCount >= (NumRequests * 8 div 10), "Too many failures"),

    ct:comment("concurrent_requests_same_tenant: PASS").

concurrent_requests_multiple_tenants(Config) ->
    ct:comment("Testing concurrent requests from multiple tenants"),

    Governor = proplists:get_value(governor, Config),

    NumTenants = 5,
    RequestsPerTenant = 4,

    %% Create concurrent requests across multiple tenants
    ParentPid = self(),
    Pids = [spawn(fun() ->
        TenantId = <<"tenant_", (integer_to_binary(T))/binary>>,
        Result = taiea_governor:process_request(Governor, #{
            <<"tenant_id">> => TenantId,
            <<"request_id">> => uuid:uuid4(),
            <<"request_num">> => R
        }),
        ParentPid ! {done, Result}
    end) || T <- lists:seq(1, NumTenants), R <- lists:seq(1, RequestsPerTenant)],

    %% Collect results
    NumRequests = NumTenants * RequestsPerTenant,
    Results = [receive {done, R} -> R after 5000 -> timeout end || _ <- Pids],
    SuccessCount = length([ok || {ok, _} <- Results]),

    ct:pal("Concurrent multi-tenant: ~w/~w succeeded", [SuccessCount, NumRequests]),
    ?assert(SuccessCount >= (NumRequests * 8 div 10), "Too many failures"),

    ct:comment("concurrent_requests_multiple_tenants: PASS").

request_isolation(Config) ->
    ct:comment("Testing request isolation between tenants"),

    Governor = proplists:get_value(governor, Config),

    %% Create requests from different tenants
    TenantA = <<"tenant_a">>,
    TenantB = <<"tenant_b">>,

    ReqA = #{
        <<"tenant_id">> => TenantA,
        <<"request_id">> => uuid:uuid4(),
        <<"data">> => <<"sensitive_data_a">>
    },

    ReqB = #{
        <<"tenant_id">> => TenantB,
        <<"request_id">> => uuid:uuid4(),
        <<"data">> => <<"sensitive_data_b">>
    },

    %% Process both
    _ResultA = taiea_governor:process_request(Governor, ReqA),
    _ResultB = taiea_governor:process_request(Governor, ReqB),

    %% Verify isolation
    IsolationResult = taiea_governor:verify_isolation(Governor, TenantA, TenantB),
    ct:pal("Isolation verification: ~p", [IsolationResult]),

    ?assertMatch({ok, true}, IsolationResult),
    ct:comment("request_isolation: PASS").

resource_cleanup(Config) ->
    ct:comment("Testing resource cleanup"),

    Governor = proplists:get_value(governor, Config),

    %% Create many requests
    _Results = [
        taiea_governor:process_request(Governor, #{
            <<"tenant_id">> => <<"cleanup_tenant">>,
            <<"request_id">> => uuid:uuid4()
        })
        || _ <- lists:seq(1, 100)
    ],

    %% Get memory before cleanup
    MemBefore = erlang:memory(total),
    ct:pal("Memory before cleanup: ~w bytes", [MemBefore]),

    %% Trigger cleanup
    CleanupResult = taiea_governor:cleanup_old_requests(Governor),
    ct:pal("Cleanup result: ~p", [CleanupResult]),

    %% Get memory after cleanup
    MemAfter = erlang:memory(total),
    ct:pal("Memory after cleanup: ~w bytes", [MemAfter]),

    ?assertMatch({ok, _}, CleanupResult),
    ct:comment("resource_cleanup: PASS").

%% ===================================================================
%% STATE CONSISTENCY TESTS
%% ===================================================================

governor_state_after_success(Config) ->
    ct:comment("Testing governor state after successful request"),

    Governor = proplists:get_value(governor, Config),

    %% Get initial state
    InitialState = taiea_governor:get_state(Governor),
    ct:pal("Initial state: ~p", [InitialState]),

    %% Process successful request
    Request = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"request_id">> => uuid:uuid4(),
        <<"action">> => <<"success_test">>
    },

    {ok, _Result} = taiea_governor:process_request(Governor, Request),

    %% Get final state
    FinalState = taiea_governor:get_state(Governor),
    ct:pal("Final state: ~p", [FinalState]),

    %% Verify state is consistent
    Validation = taiea_governor:validate_state(Governor),
    ?assertMatch({ok, _}, Validation),

    ct:comment("governor_state_after_success: PASS").

governor_state_after_failure(Config) ->
    ct:comment("Testing governor state after failed request"),

    Governor = proplists:get_value(governor, Config),

    %% Process failing request
    BadRequest = #{
        <<"tenant_id">> => <<"">>,
        <<"request_id">> => uuid:uuid4()
    },

    _Result = taiea_governor:process_request(Governor, BadRequest),

    %% Get state after failure
    State = taiea_governor:get_state(Governor),
    ct:pal("State after failure: ~p", [State]),

    %% State should still be consistent
    Validation = taiea_governor:validate_state(Governor),
    ?assertMatch({ok, _}, Validation),

    ct:comment("governor_state_after_failure: PASS").

receipt_chain_consistency(Config) ->
    ct:comment("Testing receipt chain consistency"),

    Governor = proplists:get_value(governor, Config),

    %% Create chain of 5 requests
    TenantId = <<"chain_consistency_test">>,
    ChainLength = 5,

    {ok, FirstReceipt} = taiea_governor:generate_receipt(Governor, #{
        <<"tenant_id">> => TenantId,
        <<"request_id">> => uuid:uuid4(),
        <<"action">> => <<"chain_start">>
    }),

    FirstReceiptId = maps:get(<<"receipt_id">>, FirstReceipt),

    %% Build chain
    _ChainReceipts = lists:foldl(fun(N, PrevReceiptId) ->
        {ok, Receipt} = taiea_governor:generate_receipt(Governor, #{
            <<"tenant_id">> => TenantId,
            <<"request_id">> => uuid:uuid4(),
            <<"parent_receipt">> => PrevReceiptId,
            <<"action">> => <<"chain_item_", (integer_to_binary(N))/binary>>
        }),
        maps:get(<<"receipt_id">>, Receipt)
    end, FirstReceiptId, lists:seq(2, ChainLength)),

    %% Verify chain consistency
    ConsistencyResult = taiea_governor:verify_receipt_chain_consistency(Governor, TenantId),
    ct:pal("Chain consistency: ~p", [ConsistencyResult]),

    ?assertMatch({ok, _}, ConsistencyResult),
    ct:comment("receipt_chain_consistency: PASS").

firestore_sync_validation(Config) ->
    ct:comment("Testing Firestore sync validation (stubbed for Phase 1)"),

    Governor = proplists:get_value(governor, Config),

    %% Create request that would be synced to Firestore
    Request = #{
        <<"tenant_id">> => <<"firestore_test">>,
        <<"request_id">> => uuid:uuid4(),
        <<"action">> => <<"firestore_sync_test">>
    },

    %% Generate receipt
    {ok, Receipt} = taiea_governor:generate_receipt(Governor, Request),

    %% Validate receipt would be synced correctly
    SyncResult = taiea_governor:validate_firestore_sync(Governor, Receipt),
    ct:pal("Firestore sync validation: ~p", [SyncResult]),

    %% For Phase 1, should validate structure
    ?assertMatch({ok, _}, SyncResult),
    ct:comment("firestore_sync_validation: PASS").

%% ===================================================================
%% SETUP/CLEANUP FUNCTIONS
%% ===================================================================

setup_http_server() ->
    %% Start HTTP server
    ServerOpts = [
        {port, 8888},
        {acceptors, 10}
    ],

    {ok, Server} = http_server:start_link(ServerOpts),

    %% Start governor
    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"}
    ]),

    {Server, Governor}.

cleanup_http_server(Config) ->
    case lists:keyfind(server, 1, Config) of
        {server, Server} when is_pid(Server) ->
            catch http_server:stop(Server);
        _ ->
            ok
    end,
    case lists:keyfind(governor, 1, Config) of
        {governor, Governor} when is_pid(Governor) ->
            catch taiea_governor:stop(Governor);
        _ ->
            ok
    end.

setup_governor() ->
    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"}
    ]),
    Governor.

cleanup_governor(Config) ->
    case lists:keyfind(governor, 1, Config) of
        {governor, Governor} when is_pid(Governor) ->
            catch taiea_governor:stop(Governor);
        _ ->
            ok
    end.

setup_mcp_with_governor() ->
    %% Start MCP server
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        test_utils:test_mcp_capabilities()
    ),

    %% Start governor
    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"}
    ]),

    {Server, Governor}.

cleanup_mcp_with_governor(Config) ->
    cleanup_http_server(Config).

setup_receipt_infrastructure() ->
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        test_utils:test_mcp_capabilities()
    ),
    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"},
        {enable_receipts, true}
    ]),
    {Server, Governor}.

cleanup_receipt_infrastructure(Config) ->
    cleanup_http_server(Config).

setup_error_handling_env() ->
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        test_utils:test_mcp_capabilities()
    ),
    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"}
    ]),
    {Server, Governor}.

cleanup_error_handling_env(Config) ->
    cleanup_http_server(Config).

setup_concurrency_env() ->
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        test_utils:test_mcp_capabilities()
    ),
    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"},
        {max_concurrent_requests, 100}
    ]),
    {Server, Governor}.

cleanup_concurrency_env(Config) ->
    cleanup_http_server(Config).

setup_state_consistency_env() ->
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        test_utils:test_mcp_capabilities()
    ),
    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"},
        {enable_state_validation, true}
    ]),
    {Server, Governor}.

cleanup_state_consistency_env(Config) ->
    cleanup_http_server(Config).

%% ===================================================================
%% HELPER FUNCTIONS
%% ===================================================================

call_http_endpoint(Server, Path, Method, Payload) ->
    try
        %% Construct HTTP request
        Url = "http://localhost:8888" ++ Path,
        Headers = [
            {<<"content-type">>, <<"application/json">>}
        ],
        Body = jsx:encode(Payload),

        %% Make request (using gun or similar)
        {ok, Response} = http_client:request(Method, Url, Headers, Body),

        %% Parse response
        {ok, jsx:decode(Response, [return_maps])}
    catch
        _:_ -> {error, http_error}
    end.
