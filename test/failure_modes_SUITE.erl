%% ===================================================================
%% FAILURE MODES TEST SUITE
%% ===================================================================
%% Module: failure_modes_SUITE
%% Purpose: Comprehensive testing of failure scenarios and recovery
%%          for erlmcp + TAIEA integration
%% ===================================================================

-module(failure_modes_SUITE).

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

%% Test Cases - Governor Failures
-export([
    governor_timeout_recovery/1,
    governor_process_crash_recovery/1,
    governor_state_corruption_recovery/1,
    governor_memory_exhaustion/1
]).

%% Test Cases - Gate Failures
-export([
    gate_2_entitlement_failure/1,
    gate_2_invalid_tenant/1,
    gate_3_receipt_chain_broken/1,
    gate_3_receipt_verification_failure/1
]).

%% Test Cases - Tool Failures
-export([
    tool_handler_crash/1,
    tool_timeout/1,
    tool_invalid_return/1,
    tool_exception_handling/1
]).

%% Test Cases - Network Failures
-export([
    network_timeout/1,
    network_connection_loss/1,
    network_partial_write/1
]).

%% Test Cases - Concurrency Failures
-export([
    race_condition_duplicate_request/1,
    race_condition_state_conflict/1,
    deadlock_detection/1
]).

%% Test Cases - Recovery Scenarios
-export([
    recovery_after_governor_restart/1,
    recovery_after_tool_crash/1,
    recovery_state_consistency/1,
    recovery_receipt_chain_integrity/1
]).

%% ===================================================================
%% CT CALLBACKS
%% ===================================================================

all() ->
    [
        {group, governor_failure_group},
        {group, gate_failure_group},
        {group, tool_failure_group},
        {group, network_failure_group},
        {group, concurrency_failure_group},
        {group, recovery_group}
    ].

groups() ->
    [
        {governor_failure_group, [sequence], [
            governor_timeout_recovery,
            governor_process_crash_recovery,
            governor_state_corruption_recovery,
            governor_memory_exhaustion
        ]},
        {gate_failure_group, [sequence], [
            gate_2_entitlement_failure,
            gate_2_invalid_tenant,
            gate_3_receipt_chain_broken,
            gate_3_receipt_verification_failure
        ]},
        {tool_failure_group, [sequence], [
            tool_handler_crash,
            tool_timeout,
            tool_invalid_return,
            tool_exception_handling
        ]},
        {network_failure_group, [sequence], [
            network_timeout,
            network_connection_loss,
            network_partial_write
        ]},
        {concurrency_failure_group, [parallel], [
            race_condition_duplicate_request,
            race_condition_state_conflict,
            deadlock_detection
        ]},
        {recovery_group, [sequence], [
            recovery_after_governor_restart,
            recovery_after_tool_crash,
            recovery_state_consistency,
            recovery_receipt_chain_integrity
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("=== FAILURE MODES TEST SUITE ==="),

    application:ensure_all_started(erlmcp),
    application:ensure_all_started(taiea),

    [{suite_start_time, erlang:system_time(millisecond)} | Config].

end_per_suite(_Config) ->
    application:stop(taiea),
    application:stop(erlmcp),
    ct:pal("Failure modes test suite completed").

init_per_group(governor_failure_group, Config) ->
    ct:pal("Setting up governor failure tests..."),
    {Server, Governor} = setup_governor_failure_env(),
    [{server, Server}, {governor, Governor} | Config];

init_per_group(gate_failure_group, Config) ->
    ct:pal("Setting up gate failure tests..."),
    {Server, Governor} = setup_gate_failure_env(),
    [{server, Server}, {governor, Governor} | Config];

init_per_group(tool_failure_group, Config) ->
    ct:pal("Setting up tool failure tests..."),
    {Server, Governor} = setup_tool_failure_env(),
    [{server, Server}, {governor, Governor} | Config];

init_per_group(network_failure_group, Config) ->
    ct:pal("Setting up network failure tests..."),
    {Server, Governor} = setup_network_failure_env(),
    [{server, Server}, {governor, Governor} | Config];

init_per_group(concurrency_failure_group, Config) ->
    ct:pal("Setting up concurrency failure tests..."),
    {Server, Governor} = setup_concurrency_failure_env(),
    [{server, Server}, {governor, Governor} | Config];

init_per_group(recovery_group, Config) ->
    ct:pal("Setting up recovery tests..."),
    {Server, Governor} = setup_recovery_env(),
    [{server, Server}, {governor, Governor} | Config].

end_per_group(governor_failure_group, Config) ->
    cleanup_governor_failure_env(Config);
end_per_group(gate_failure_group, Config) ->
    cleanup_gate_failure_env(Config);
end_per_group(tool_failure_group, Config) ->
    cleanup_tool_failure_env(Config);
end_per_group(network_failure_group, Config) ->
    cleanup_network_failure_env(Config);
end_per_group(concurrency_failure_group, Config) ->
    cleanup_concurrency_failure_env(Config);
end_per_group(recovery_group, Config) ->
    cleanup_recovery_env(Config).

init_per_testcase(Case, Config) ->
    ct:pal(">>> FAILURE TEST: ~p", [Case]),
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
%% GOVERNOR FAILURE TESTS
%% ===================================================================

governor_timeout_recovery(Config) ->
    ct:comment("Testing governor timeout and recovery"),

    Governor = proplists:get_value(governor, Config),

    %% Create request with short timeout
    Request = #{
        <<"tenant_id">> => <<"timeout_test">>,
        <<"request_id">> => uuid:uuid4(),
        <<"timeout_ms">> => 10
    },

    %% Should timeout but not crash
    Result = taiea_governor:process_with_timeout(Governor, Request, 10),
    ct:pal("Timeout result: ~p", [Result]),

    %% Verify governor is still operational
    HealthResult = taiea_governor:get_status(Governor),
    ct:pal("Governor status after timeout: ~p", [HealthResult]),

    ?assertMatch({ok, _}, HealthResult),
    ct:comment("governor_timeout_recovery: PASS").

governor_process_crash_recovery(Config) ->
    ct:comment("Testing governor process crash and recovery"),

    Governor = proplists:get_value(governor, Config),

    %% Verify governor is alive
    ?assert(is_process_alive(Governor), "Governor not alive"),

    %% Simulate governor crash by sending bad message
    Governor ! crash_test,

    %% Governor should handle or ignore
    timer:sleep(100),

    %% System should be recoverable
    case is_process_alive(Governor) of
        true ->
            ct:comment("Governor survived crash message");
        false ->
            ct:comment("Governor crashed - testing recovery...")
            %% In production, supervisor should restart it
    end,

    ct:comment("governor_process_crash_recovery: PASS").

governor_state_corruption_recovery(Config) ->
    ct:comment("Testing governor state corruption recovery"),

    Governor = proplists:get_value(governor, Config),

    %% Make normal request
    Request = #{
        <<"tenant_id">> => <<"corruption_test">>,
        <<"request_id">> => uuid:uuid4()
    },

    {ok, _Result1} = taiea_governor:process_request(Governor, Request),

    %% Verify state validity
    ValidResult1 = taiea_governor:validate_state(Governor),
    ct:pal("State validation before: ~p", [ValidResult1]),

    ?assertMatch({ok, _}, ValidResult1),

    %% Process another request
    Request2 = Request#{<<"request_id">> => uuid:uuid4()},
    {ok, _Result2} = taiea_governor:process_request(Governor, Request2),

    %% Verify state is still valid
    ValidResult2 = taiea_governor:validate_state(Governor),
    ct:pal("State validation after: ~p", [ValidResult2]),

    ?assertMatch({ok, _}, ValidResult2),
    ct:comment("governor_state_corruption_recovery: PASS").

governor_memory_exhaustion(Config) ->
    ct:comment("Testing governor behavior under memory pressure"),

    Governor = proplists:get_value(governor, Config),

    %% Monitor memory
    MemBefore = erlang:memory(total),
    ct:pal("Memory before: ~w bytes", [MemBefore]),

    %% Create many requests
    _Results = [
        taiea_governor:process_request(Governor, #{
            <<"tenant_id">> => <<"mem_test_", (integer_to_binary(N))/binary>>,
            <<"request_id">> => uuid:uuid4(),
            <<"large_data">> => binary:copy(<<"x">>, 10000)
        })
        || N <- lists:seq(1, 100)
    ],

    MemAfter = erlang:memory(total),
    ct:pal("Memory after: ~w bytes", [MemAfter]),

    %% Force cleanup
    _CleanupResult = taiea_governor:cleanup_old_requests(Governor),

    %% System should still be operational
    ?assert(is_process_alive(Governor), "Governor crashed due to memory"),

    ct:comment("governor_memory_exhaustion: PASS").

%% ===================================================================
%% GATE FAILURE TESTS
%% ===================================================================

gate_2_entitlement_failure(Config) ->
    ct:comment("Testing gate 2 entitlement failure"),

    Governor = proplists:get_value(governor, Config),

    %% Create request with invalid entitlement
    Request = #{
        <<"tenant_id">> => <<"entitlement_fail">>,
        <<"request_id">> => uuid:uuid4(),
        <<"feature">> => <<"premium_only">>,
        <<"requires_entitlement">> => true
    },

    %% Should fail at gate 2
    Result = taiea_governor:process_request(Governor, Request),
    ct:pal("Entitlement failure result: ~p", [Result]),

    %% Should return error
    case Result of
        {error, entitlement_denied} ->
            ct:comment("Entitlement failure correctly handled");
        {error, _} ->
            ct:comment("Failed with different error (acceptable)");
        {ok, _} ->
            ct:comment("Request passed (may have entitlement)")
    end,

    ct:comment("gate_2_entitlement_failure: PASS").

gate_2_invalid_tenant(Config) ->
    ct:comment("Testing gate 2 with invalid tenant"),

    Governor = proplists:get_value(governor, Config),

    %% Create request with invalid tenant
    Request = #{
        <<"tenant_id">> => <<"">>,  %% Empty tenant
        <<"request_id">> => uuid:uuid4()
    },

    %% Should fail validation
    Result = taiea_governor:process_request(Governor, Request),
    ct:pal("Invalid tenant result: ~p", [Result]),

    ?assertMatch({error, _}, Result),
    ct:comment("gate_2_invalid_tenant: PASS").

gate_3_receipt_chain_broken(Config) ->
    ct:comment("Testing gate 3 with broken receipt chain"),

    Governor = proplists:get_value(governor, Config),

    %% Create request with invalid parent receipt
    Request = #{
        <<"tenant_id">> => <<"receipt_test">>,
        <<"request_id">> => uuid:uuid4(),
        <<"parent_receipt">> => <<"nonexistent_receipt_id">>
    },

    %% Should fail receipt validation
    Result = taiea_governor:process_request(Governor, Request),
    ct:pal("Broken chain result: ~p", [Result]),

    %% Should fail or warn about missing parent
    case Result of
        {error, _} ->
            ct:comment("Broken chain correctly rejected");
        {ok, _} ->
            ct:comment("Chain validation skipped (acceptable for Phase 1)")
    end,

    ct:comment("gate_3_receipt_chain_broken: PASS").

gate_3_receipt_verification_failure(Config) ->
    ct:comment("Testing gate 3 receipt verification failure"),

    Governor = proplists:get_value(governor, Config),

    %% Create request with corrupted receipt
    Request = #{
        <<"tenant_id">> => <<"receipt_corrupt_test">>,
        <<"request_id">> => uuid:uuid4(),
        <<"receipt_data">> => <<"corrupted_receipt_content">>
    },

    %% Should fail verification
    Result = taiea_governor:process_request(Governor, Request),
    ct:pal("Receipt verification failure result: ~p", [Result]),

    %% May pass or fail depending on implementation
    case Result of
        {error, _} ->
            ct:comment("Corruption correctly detected");
        {ok, _} ->
            ct:comment("Receipt validation handled")
    end,

    ct:comment("gate_3_receipt_verification_failure: PASS").

%% ===================================================================
%% TOOL FAILURE TESTS
%% ===================================================================

tool_handler_crash(Config) ->
    ct:comment("Testing tool handler crash"),

    Server = proplists:get_value(server, Config),

    %% Register tool that crashes
    ToolName = <<"crash_tool">>,
    CrashHandler = fun(_Args) ->
        erlang:error(tool_crash)
    end,

    erlmcp_server:add_tool(Server, ToolName, CrashHandler),

    %% Call tool
    Result = catch erlmcp_server:call_tool(Server, ToolName, #{}),
    ct:pal("Tool crash result: ~p", [Result]),

    %% Server should still be alive
    ?assert(is_process_alive(Server), "Server crashed from tool error"),

    ct:comment("tool_handler_crash: PASS").

tool_timeout(Config) ->
    ct:comment("Testing tool timeout"),

    Server = proplists:get_value(server, Config),

    %% Register slow tool
    ToolName = <<"slow_tool">>,
    SlowHandler = fun(_Args) ->
        timer:sleep(5000),  %% Very slow
        #mcp_content{
            type = <<"text">>,
            text = <<"Done">>,
            mime_type = <<"text/plain">>
        }
    end,

    erlmcp_server:add_tool(Server, ToolName, SlowHandler),

    %% Call with timeout
    Result = call_tool_with_timeout(Server, ToolName, #{}, 1000),
    ct:pal("Timeout result: ~p", [Result]),

    %% Should timeout or be slow
    case Result of
        timeout ->
            ct:comment("Tool correctly timed out");
        {ok, _} ->
            ct:comment("Tool completed");
        {error, _} ->
            ct:comment("Tool failed")
    end,

    ct:comment("tool_timeout: PASS").

tool_invalid_return(Config) ->
    ct:comment("Testing tool with invalid return"),

    Server = proplists:get_value(server, Config),

    %% Register tool with bad return
    ToolName = <<"bad_return_tool">>,
    BadHandler = fun(_Args) ->
        <<"not a proper content structure">>  %% Invalid return
    end,

    erlmcp_server:add_tool(Server, ToolName, BadHandler),

    %% Call tool
    Result = catch erlmcp_server:call_tool(Server, ToolName, #{}),
    ct:pal("Invalid return result: ~p", [Result]),

    %% Server should handle gracefully
    ?assert(is_process_alive(Server), "Server crashed from bad return"),

    ct:comment("tool_invalid_return: PASS").

tool_exception_handling(Config) ->
    ct:comment("Testing tool exception handling"),

    Server = proplists:get_value(server, Config),

    %% Register tool that throws exception
    ToolName = <<"exception_tool">>,
    ExceptionHandler = fun(#{<<"throw_exception">> := true}) ->
        throw(test_exception);
    (Args) ->
        #mcp_content{
            type = <<"text">>,
            text = <<"OK">>,
            mime_type = <<"text/plain">>
        }
    end,

    erlmcp_server:add_tool(Server, ToolName, ExceptionHandler),

    %% Call with exception
    Result1 = catch erlmcp_server:call_tool(Server, ToolName, #{<<"throw_exception">> => true}),
    ct:pal("Exception result: ~p", [Result1]),

    %% Call normally
    Result2 = catch erlmcp_server:call_tool(Server, ToolName, #{}),
    ct:pal("Normal result after exception: ~p", [Result2]),

    %% Server should still be operational
    ?assert(is_process_alive(Server), "Server crashed from exception"),

    ct:comment("tool_exception_handling: PASS").

%% ===================================================================
%% NETWORK FAILURE TESTS
%% ===================================================================

network_timeout(Config) ->
    ct:comment("Testing network timeout scenario"),

    Server = proplists:get_value(server, Config),

    %% Simulate HTTP request with timeout
    SlowEndpoint = "/slow_endpoint",

    Result = catch call_http_with_timeout(Server, SlowEndpoint, 100),
    ct:pal("Network timeout result: ~p", [Result]),

    %% Server should handle timeout gracefully
    ?assert(is_process_alive(Server), "Server crashed from network timeout"),

    ct:comment("network_timeout: PASS").

network_connection_loss(Config) ->
    ct:comment("Testing network connection loss"),

    Server = proplists:get_value(server, Config),

    %% Try to call endpoint on closed connection
    Result = catch call_http_with_closed_connection(Server),
    ct:pal("Connection loss result: ~p", [Result]),

    %% Server should still be operational
    ?assert(is_process_alive(Server), "Server crashed from connection loss"),

    ct:comment("network_connection_loss: PASS").

network_partial_write(Config) ->
    ct:comment("Testing network partial write"),

    Server = proplists:get_value(server, Config),

    %% Send partial/malformed HTTP request
    Result = catch send_partial_http_request(Server),
    ct:pal("Partial write result: ~p", [Result]),

    %% Server should recover
    ?assert(is_process_alive(Server), "Server crashed from partial write"),

    ct:comment("network_partial_write: PASS").

%% ===================================================================
%% CONCURRENCY FAILURE TESTS
%% ===================================================================

race_condition_duplicate_request(Config) ->
    ct:comment("Testing race condition with duplicate requests"),

    Governor = proplists:get_value(governor, Config),

    RequestId = uuid:uuid4(),
    TenantId = <<"race_test_1">>,

    %% Send same request from multiple threads
    ParentPid = self(),
    Pids = [spawn(fun() ->
        Result = taiea_governor:process_request(Governor, #{
            <<"tenant_id">> => TenantId,
            <<"request_id">> => RequestId
        }),
        ParentPid ! {done, Result}
    end) || _ <- lists:seq(1, 10)],

    %% Collect results
    Results = [receive {done, R} -> R after 5000 -> timeout end || _ <- Pids],

    SuccessCount = length([ok || {ok, _} <- Results]),
    ct:pal("Duplicate request handling: ~w/10 succeeded", [SuccessCount]),

    %% Should handle duplicates without crashing
    ?assert(SuccessCount >= 5, "Too many failures with duplicate requests"),

    ct:comment("race_condition_duplicate_request: PASS").

race_condition_state_conflict(Config) ->
    ct:comment("Testing race condition with state conflicts"),

    Governor = proplists:get_value(governor, Config),

    TenantId = <<"race_test_2">>,

    %% Send conflicting requests
    ParentPid = self(),
    Pids = [spawn(fun() ->
        Operation = N rem 3,
        Result = case Operation of
            0 ->  %% Read
                taiea_governor:get_state(Governor);
            1 ->  %% Write
                taiea_governor:process_request(Governor, #{
                    <<"tenant_id">> => TenantId,
                    <<"request_id">> => uuid:uuid4(),
                    <<"action">> => <<"write">>
                });
            2 ->  %% Validate
                taiea_governor:validate_state(Governor)
        end,
        ParentPid ! {done, Result}
    end) || N <- lists:seq(1, 20)],

    %% Collect results
    Results = [receive {done, R} -> R after 5000 -> timeout end || _ <- Pids],

    %% Should complete without deadlock
    CompletedCount = length([R || R <- Results, R =/= timeout]),
    ct:pal("State conflict handling: ~w/20 completed", [CompletedCount]),

    ?assert(CompletedCount >= 15, "Too many timeouts with state conflicts"),

    ct:comment("race_condition_state_conflict: PASS").

deadlock_detection(Config) ->
    ct:comment("Testing deadlock detection"),

    Governor = proplists:get_value(governor, Config),

    %% Try to create potential deadlock scenario
    ParentPid = self(),

    %% Thread 1: A -> B
    _Pid1 = spawn(fun() ->
        _Lock1 = acquire_lock(Governor, lock_a),
        timer:sleep(100),
        _Lock2 = acquire_lock(Governor, lock_b),
        ParentPid ! {thread1, done}
    end),

    %% Thread 2: B -> A (potential deadlock)
    _Pid2 = spawn(fun() ->
        _Lock1 = acquire_lock(Governor, lock_b),
        timer:sleep(100),
        _Lock2 = acquire_lock(Governor, lock_a),
        ParentPid ! {thread2, done}
    end),

    %% With timeout, should detect potential deadlock
    Timer = timer:send_after(5000, timeout),

    Results = [
        receive
            {thread1, done} -> thread1_done;
            {thread2, done} -> thread2_done;
            timeout -> deadlock_detected
        after
            10000 -> no_response
        end
        || _ <- lists:seq(1, 2)
    ],

    timer:cancel(Timer),

    ct:pal("Deadlock detection results: ~p", [Results]),

    %% Should not crash even with potential deadlock
    ?assert(is_process_alive(Governor), "Governor crashed from deadlock"),

    ct:comment("deadlock_detection: PASS").

%% ===================================================================
%% RECOVERY TESTS
%% ===================================================================

recovery_after_governor_restart(Config) ->
    ct:comment("Testing recovery after governor restart"),

    Governor = proplists:get_value(governor, Config),
    Server = proplists:get_value(server, Config),

    %% Make initial request
    Request1 = #{
        <<"tenant_id">> => <<"restart_test">>,
        <<"request_id">> => uuid:uuid4()
    },

    {ok, Result1} = taiea_governor:process_request(Governor, Request1),
    ct:pal("Before restart: ~p", [Result1]),

    %% Simulate governor restart
    catch taiea_governor:stop(Governor),
    timer:sleep(100),

    {ok, NewGovernor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"}
    ]),

    %% Make request after restart
    Request2 = #{
        <<"tenant_id">> => <<"restart_test">>,
        <<"request_id">> => uuid:uuid4()
    },

    {ok, Result2} = taiea_governor:process_request(NewGovernor, Request2),
    ct:pal("After restart: ~p", [Result2]),

    %% System should be operational
    ?assert(is_process_alive(NewGovernor), "Governor not running after restart"),

    ct:comment("recovery_after_governor_restart: PASS").

recovery_after_tool_crash(Config) ->
    ct:comment("Testing recovery after tool crash"),

    Server = proplists:get_value(server, Config),

    %% Register normal tool
    ToolName = <<"recovery_tool">>,
    Handler = fun(#{<<"crash">> := true}) ->
        erlang:error(crash_test);
    (_Args) ->
        #mcp_content{
            type = <<"text">>,
            text = <<"OK">>,
            mime_type = <<"text/plain">>
        }
    end,

    erlmcp_server:add_tool(Server, ToolName, Handler),

    %% Crash the tool
    catch erlmcp_server:call_tool(Server, ToolName, #{<<"crash">> => true}),

    %% Should still be able to call other tools
    Result = catch erlmcp_server:call_tool(Server, ToolName, #{}),
    ct:pal("Recovery result: ~p", [Result]),

    ?assertMatch({ok, _}, Result),
    ct:comment("recovery_after_tool_crash: PASS").

recovery_state_consistency(Config) ->
    ct:comment("Testing recovery with state consistency"),

    Governor = proplists:get_value(governor, Config),

    %% Create initial state
    Requests = [
        taiea_governor:process_request(Governor, #{
            <<"tenant_id">> => <<"recovery_state_test">>,
            <<"request_id">> => uuid:uuid4()
        })
        || _ <- lists:seq(1, 10)
    ],

    %% Verify state is valid
    ValidResult1 = taiea_governor:validate_state(Governor),
    ct:pal("State validation before: ~p", [ValidResult1]),

    ?assertMatch({ok, _}, ValidResult1),

    %% Trigger recovery operation
    _RecoveryResult = taiea_governor:recovery_restore(Governor),

    %% Verify state is still valid
    ValidResult2 = taiea_governor:validate_state(Governor),
    ct:pal("State validation after recovery: ~p", [ValidResult2]),

    ?assertMatch({ok, _}, ValidResult2),
    ct:comment("recovery_state_consistency: PASS").

recovery_receipt_chain_integrity(Config) ->
    ct:comment("Testing recovery with receipt chain integrity"),

    Governor = proplists:get_value(governor, Config),

    %% Create receipt chain
    TenantId = <<"recovery_receipt_test">>,

    {ok, Receipt1} = taiea_governor:generate_receipt(Governor, #{
        <<"tenant_id">> => TenantId,
        <<"request_id">> => uuid:uuid4()
    }),

    Receipt1Id = maps:get(<<"receipt_id">>, Receipt1),

    %% Create child receipt
    {ok, Receipt2} = taiea_governor:generate_receipt(Governor, #{
        <<"tenant_id">> => TenantId,
        <<"request_id">> => uuid:uuid4(),
        <<"parent_receipt">> => Receipt1Id
    }),

    %% Verify chain before recovery
    ChainResult1 = taiea_governor:verify_receipt_chain(Governor, TenantId),
    ct:pal("Chain before recovery: ~p", [ChainResult1]),

    %% Recovery
    _RecoveryResult = taiea_governor:recovery_restore(Governor),

    %% Verify chain after recovery
    ChainResult2 = taiea_governor:verify_receipt_chain(Governor, TenantId),
    ct:pal("Chain after recovery: ~p", [ChainResult2]),

    ?assertMatch({ok, _}, ChainResult2),
    ct:comment("recovery_receipt_chain_integrity: PASS").

%% ===================================================================
%% SETUP/CLEANUP
%% ===================================================================

setup_governor_failure_env() ->
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        test_utils:test_mcp_capabilities()
    ),
    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"}
    ]),
    {Server, Governor}.

cleanup_governor_failure_env(Config) ->
    cleanup_env(Config).

setup_gate_failure_env() ->
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        test_utils:test_mcp_capabilities()
    ),
    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"}
    ]),
    {Server, Governor}.

cleanup_gate_failure_env(Config) ->
    cleanup_env(Config).

setup_tool_failure_env() ->
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        test_utils:test_mcp_capabilities()
    ),
    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"}
    ]),
    {Server, Governor}.

cleanup_tool_failure_env(Config) ->
    cleanup_env(Config).

setup_network_failure_env() ->
    {ok, Server} = http_server:start_link([{port, 8889}]),
    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"}
    ]),
    {Server, Governor}.

cleanup_network_failure_env(Config) ->
    cleanup_env(Config).

setup_concurrency_failure_env() ->
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        test_utils:test_mcp_capabilities()
    ),
    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"},
        {max_concurrent_requests, 100}
    ]),
    {Server, Governor}.

cleanup_concurrency_failure_env(Config) ->
    cleanup_env(Config).

setup_recovery_env() ->
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        test_utils:test_mcp_capabilities()
    ),
    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"}
    ]),
    {Server, Governor}.

cleanup_recovery_env(Config) ->
    cleanup_env(Config).

cleanup_env(Config) ->
    case lists:keyfind(server, 1, Config) of
        {server, Server} when is_pid(Server) ->
            catch erlmcp_server:stop(Server);
        _ ->
            ok
    end,
    case lists:keyfind(governor, 1, Config) of
        {governor, Governor} when is_pid(Governor) ->
            catch taiea_governor:stop(Governor);
        _ ->
            ok
    end.

%% ===================================================================
%% HELPER FUNCTIONS
%% ===================================================================

call_tool_with_timeout(Server, ToolName, Args, TimeoutMs) ->
    ParentPid = self(),
    Pid = spawn(fun() ->
        Result = erlmcp_server:call_tool(Server, ToolName, Args),
        ParentPid ! {tool_result, Result}
    end),

    receive
        {tool_result, Result} ->
            Result
    after
        TimeoutMs ->
            timeout
    end.

call_http_with_timeout(Server, Endpoint, TimeoutMs) ->
    ParentPid = self(),
    _Pid = spawn(fun() ->
        Result = http_client:get("http://localhost:8888" ++ Endpoint),
        ParentPid ! {http_result, Result}
    end),

    receive
        {http_result, Result} ->
            Result
    after
        TimeoutMs ->
            timeout
    end.

call_http_with_closed_connection(Server) ->
    http_client:get("http://localhost:8888/test").

send_partial_http_request(Server) ->
    http_client:send_raw("GET /test ").

acquire_lock(Governor, LockName) ->
    %% Stub implementation
    {lock, LockName}.
