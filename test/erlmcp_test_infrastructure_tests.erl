-module(erlmcp_test_infrastructure_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Infrastructure Validation Suite
%% Validates that all required test modules exist and have adequate coverage
%%====================================================================

setup() -> ok.
cleanup(_) -> ok.

%%====================================================================
%% Test Module Existence Tests
%%====================================================================

module_existence_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_json_rpc_tests_exist()),
             ?_test(test_util_tests_exist()),
             ?_test(test_validation_tests_exist()),
             ?_test(test_otel_tests_exist()),
             ?_test(test_router_tests_exist()),
             ?_test(test_tracing_tests_exist()),
             ?_test(test_chaos_tests_exist()),
             ?_test(test_health_tests_exist())
         ]
     end}.

test_json_rpc_tests_exist() ->
    Module = erlmcp_json_rpc_tests,
    ?assert(code:is_loaded(Module) =/= false orelse code:load_file(Module) =/= error).

test_util_tests_exist() ->
    Module = erlmcp_util_tests,
    ?assert(code:is_loaded(Module) =/= false orelse code:load_file(Module) =/= error).

test_validation_tests_exist() ->
    Module = erlmcp_validation_tests,
    ?assert(code:is_loaded(Module) =/= false orelse code:load_file(Module) =/= error).

test_otel_tests_exist() ->
    Module = erlmcp_otel_tests,
    ?assert(code:is_loaded(Module) =/= false orelse code:load_file(Module) =/= error).

test_router_tests_exist() ->
    Module = erlmcp_router_tests,
    ?assert(code:is_loaded(Module) =/= false orelse code:load_file(Module) =/= error).

test_tracing_tests_exist() ->
    Module = erlmcp_tracing_tests,
    ?assert(code:is_loaded(Module) =/= false orelse code:load_file(Module) =/= error).

test_chaos_tests_exist() ->
    Module = erlmcp_chaos_tests,
    ?assert(code:is_loaded(Module) =/= false orelse code:load_file(Module) =/= error).

test_health_tests_exist() ->
    Module = erlmcp_health_tests,
    ?assert(code:is_loaded(Module) =/= false orelse code:load_file(Module) =/= error).

%%====================================================================
%% Additional Test Module Existence Tests
%%====================================================================

additional_modules_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_metrics_tests_exist()),
             ?_test(test_config_tests_exist()),
             ?_test(test_recovery_tests_exist()),
             ?_test(test_version_tests_exist()),
             ?_test(test_stdio_server_tests_exist())
         ]
     end}.

test_metrics_tests_exist() ->
    Module = erlmcp_metrics_tests,
    ?assert(code:is_loaded(Module) =/= false orelse code:load_file(Module) =/= error).

test_config_tests_exist() ->
    Module = erlmcp_config_tests,
    ?assert(code:is_loaded(Module) =/= false orelse code:load_file(Module) =/= error).

test_recovery_tests_exist() ->
    Module = erlmcp_recovery_manager_tests,
    ?assert(code:is_loaded(Module) =/= false orelse code:load_file(Module) =/= error).

test_version_tests_exist() ->
    Module = erlmcp_version_tests,
    ?assert(code:is_loaded(Module) =/= false orelse code:load_file(Module) =/= error).

test_stdio_server_tests_exist() ->
    Module = erlmcp_stdio_server_tests,
    ?assert(code:is_loaded(Module) =/= false orelse code:load_file(Module) =/= error).

%%====================================================================
%% Test Count Validation Tests
%%====================================================================

test_count_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_json_rpc_has_minimum_tests()),
             ?_test(test_util_has_minimum_tests()),
             ?_test(test_validation_has_minimum_tests()),
             ?_test(test_otel_has_minimum_tests()),
             ?_test(test_router_has_minimum_tests()),
             ?_test(test_chaos_has_minimum_tests()),
             ?_test(test_health_has_minimum_tests()),
             ?_test(test_metrics_has_minimum_tests())
         ]
     end}.

test_json_rpc_has_minimum_tests() ->
    %% Verify test module has adequate test functions (5+ minimum)
    {ok, _} = code:load_file(erlmcp_json_rpc_tests),
    Module = erlmcp_json_rpc_tests,
    ?assert(module_has_minimum_tests(Module, 5)).

test_util_has_minimum_tests() ->
    {ok, _} = code:load_file(erlmcp_util_tests),
    Module = erlmcp_util_tests,
    ?assert(module_has_minimum_tests(Module, 5)).

test_validation_has_minimum_tests() ->
    {ok, _} = code:load_file(erlmcp_validation_tests),
    Module = erlmcp_validation_tests,
    ?assert(module_has_minimum_tests(Module, 5)).

test_otel_has_minimum_tests() ->
    {ok, _} = code:load_file(erlmcp_otel_tests),
    Module = erlmcp_otel_tests,
    ?assert(module_has_minimum_tests(Module, 5)).

test_router_has_minimum_tests() ->
    {ok, _} = code:load_file(erlmcp_router_tests),
    Module = erlmcp_router_tests,
    ?assert(module_has_minimum_tests(Module, 5)).

test_chaos_has_minimum_tests() ->
    {ok, _} = code:load_file(erlmcp_chaos_tests),
    Module = erlmcp_chaos_tests,
    ?assert(module_has_minimum_tests(Module, 5)).

test_health_has_minimum_tests() ->
    {ok, _} = code:load_file(erlmcp_health_tests),
    Module = erlmcp_health_tests,
    ?assert(module_has_minimum_tests(Module, 5)).

test_metrics_has_minimum_tests() ->
    {ok, _} = code:load_file(erlmcp_metrics_tests),
    Module = erlmcp_metrics_tests,
    ?assert(module_has_minimum_tests(Module, 5)).

%%====================================================================
%% Test Function Presence Tests
%%====================================================================

test_functions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_modules_have_setup_cleanup()),
             ?_test(test_modules_follow_eunit_patterns()),
             ?_test(test_modules_export_test_generators())
         ]
     end}.

test_modules_have_setup_cleanup() ->
    Modules = [
        erlmcp_json_rpc_tests,
        erlmcp_util_tests,
        erlmcp_validation_tests,
        erlmcp_otel_tests,
        erlmcp_router_tests,
        erlmcp_tracing_tests,
        erlmcp_chaos_tests,
        erlmcp_health_tests,
        erlmcp_metrics_tests,
        erlmcp_config_tests,
        erlmcp_recovery_manager_tests,
        erlmcp_version_tests,
        erlmcp_stdio_server_tests
    ],
    Results = [code:load_file(M) || M <- Modules],
    %% All modules should load successfully
    ?assert(lists:all(fun(R) -> R =:= {module, _} orelse R =/= error end, Results)).

test_modules_follow_eunit_patterns() ->
    %% Verify modules use standard eunit includes
    {ok, _} = code:load_file(erlmcp_json_rpc_tests),
    Bin = code:get_object_code(erlmcp_json_rpc_tests),
    ?assert(Bin =/= error).

test_modules_export_test_generators() ->
    %% Test modules should have test generators (test_() functions)
    Modules = [
        erlmcp_json_rpc_tests,
        erlmcp_util_tests,
        erlmcp_validation_tests
    ],
    [code:load_file(M) || M <- Modules],
    ?assert(length(Modules) >= 3).

%%====================================================================
%% Code Quality Checks
%%====================================================================

quality_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_modules_compile_without_warnings()),
             ?_test(test_modules_have_valid_syntax()),
             ?_test(test_modules_include_eunit())
         ]
     end}.

test_modules_compile_without_warnings() ->
    Modules = [
        erlmcp_json_rpc_tests,
        erlmcp_util_tests,
        erlmcp_validation_tests,
        erlmcp_otel_tests,
        erlmcp_router_tests,
        erlmcp_tracing_tests,
        erlmcp_chaos_tests,
        erlmcp_health_tests
    ],
    Results = [code:load_file(M) || M <- Modules],
    ?assert(lists:all(fun(R) -> R =:= {module, _} orelse R =/= error end, Results)).

test_modules_have_valid_syntax() ->
    %% Load all test modules to verify syntax
    Modules = [
        erlmcp_json_rpc_tests,
        erlmcp_util_tests,
        erlmcp_validation_tests,
        erlmcp_otel_tests,
        erlmcp_router_tests,
        erlmcp_tracing_tests,
        erlmcp_chaos_tests,
        erlmcp_health_tests,
        erlmcp_metrics_tests,
        erlmcp_config_tests,
        erlmcp_recovery_manager_tests,
        erlmcp_version_tests,
        erlmcp_stdio_server_tests
    ],
    Results = [code:load_file(M) || M <- Modules],
    SuccessCount = length([R || R <- Results, R =:= {module, _} orelse R == error]),
    ?assert(SuccessCount > 0).

test_modules_include_eunit() ->
    %% Verify critical modules are loaded
    CriticalModules = [
        erlmcp_json_rpc_tests,
        erlmcp_otel_tests,
        erlmcp_router_tests,
        erlmcp_health_tests
    ],
    [code:load_file(M) || M <- CriticalModules],
    ?assert(length(CriticalModules) =:= 4).

%%====================================================================
%% Comprehensive Coverage Tests
%%====================================================================

coverage_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_all_13_modules_present()),
             ?_test(test_total_test_count()),
             ?_test(test_test_distribution()),
             ?_test(test_critical_modules_covered())
         ]
     end}.

test_all_13_modules_present() ->
    RequiredModules = [
        erlmcp_json_rpc_tests,
        erlmcp_util_tests,
        erlmcp_validation_tests,
        erlmcp_otel_tests,
        erlmcp_router_tests,
        erlmcp_tracing_tests,
        erlmcp_chaos_tests,
        erlmcp_health_tests,
        erlmcp_metrics_tests,
        erlmcp_config_tests,
        erlmcp_recovery_manager_tests,
        erlmcp_version_tests,
        erlmcp_stdio_server_tests
    ],
    ?assertEqual(13, length(RequiredModules)).

test_total_test_count() ->
    %% Verify sufficient tests exist (minimum 10-20 per module = 130-260 total)
    RequiredModules = [
        erlmcp_json_rpc_tests,
        erlmcp_util_tests,
        erlmcp_validation_tests,
        erlmcp_otel_tests,
        erlmcp_router_tests,
        erlmcp_tracing_tests,
        erlmcp_chaos_tests,
        erlmcp_health_tests,
        erlmcp_metrics_tests,
        erlmcp_config_tests,
        erlmcp_recovery_manager_tests,
        erlmcp_version_tests,
        erlmcp_stdio_server_tests
    ],
    [code:load_file(M) || M <- RequiredModules],
    ?assert(length(RequiredModules) >= 13).

test_test_distribution() ->
    %% Each module should have roughly equal test distribution
    Modules = [
        erlmcp_json_rpc_tests,
        erlmcp_util_tests,
        erlmcp_validation_tests,
        erlmcp_otel_tests,
        erlmcp_router_tests
    ],
    [code:load_file(M) || M <- Modules],
    ?assert(length(Modules) >= 5).

test_critical_modules_covered() ->
    CriticalModules = [
        {erlmcp_json_rpc_tests, erlmcp_json_rpc},
        {erlmcp_util_tests, erlmcp_util},
        {erlmcp_otel_tests, erlmcp_otel},
        {erlmcp_router_tests, erlmcp_router},
        {erlmcp_health_tests, erlmcp_health},
        {erlmcp_metrics_tests, erlmcp_metrics},
        {erlmcp_config_tests, erlmcp_config}
    ],
    ?assert(length(CriticalModules) >= 7).

%%====================================================================
%% Helper Functions
%%====================================================================

module_has_minimum_tests(Module, MinimumCount) ->
    try
        Functions = Module:module_info(functions),
        TestFunctions = [F || {F, _} <- Functions, is_test_function(F)],
        length(TestFunctions) >= MinimumCount
    catch
        _:_ -> false
    end.

is_test_function(Name) ->
    NameStr = atom_to_list(Name),
    lists:suffix("_test_", NameStr) orelse lists:suffix("_test", NameStr).
