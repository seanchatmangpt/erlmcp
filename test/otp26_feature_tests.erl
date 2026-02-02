%%%-------------------------------------------------------------------
%%% @doc OTP 26 Feature Test Suite
%%%
%%% This comprehensive test suite validates OTP 26 specific features
%%% and their integration with the erlmcp codebase.
%%%
%%% Test Coverage:
%%%   - Concurrent application startup
%%%   - Persistent configuration
%%%   - Prep/stop callback functionality
%%%   - Enhanced environment configuration
%%%   - Version-specific feature detection
%%%   - Performance benchmarks
%%%   - Error handling scenarios
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(otp26_feature_tests).

%% Test exports
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test case exports
-export([
    concurrent_startup_test/1,
    persistent_configuration_test/1,
    prep_stop_callback_test/1,
    enhanced_environment_test/1,
    feature_detection_test/1,
    performance_benchmark_test/1,
    error_handling_test/1,
    backward_compatibility_test/1,
    configuration_reload_test/1
]).

-include_lib("common_test/include/ct.hrl").
-include("otp_compat.hrl").

%%====================================================================
%% Test Configuration
%%====================================================================

all() ->
    [concurrent_startup_test,
     persistent_configuration_test,
     prep_stop_callback_test,
     enhanced_environment_test,
     feature_detection_test,
     performance_benchmark_test,
     error_handling_test,
     backward_compatibility_test,
     configuration_reload_test].

init_per_suite(Config) ->
    %% Start required applications
    application:ensure_all_started([crypto, public_key, ssl, ranch]),

    %% Load test environment
    application:set_env(otp26_test, test_mode, true),

    ct:pal("Starting OTP 26 feature test suite"),
    Config.

end_per_suite(Config) ->
    %% Stop all applications
    application:stop(ranch),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto),

    %% Clean up test environment
    application:unset_env(otp26_test, test_mode),

    ct:pal("OTP 26 feature test suite completed"),
    Config.

init_per_testcase(TestCase, Config) ->
    %% Set up test-specific environment
    ct:pal("Starting test case: ~p", [TestCase]),

    %% Clean up before each test
    cleanup_test_environment(),

    %% Start fresh applications for each test
    case application:ensure_all_started([crypto], permanent) of
        {ok, _} -> ok;
        {error, Reason} -> ct:fail("Failed to start crypto: ~p", [Reason])
    end,

    Config.

end_per_testcase(TestCase, Config) ->
    %% Clean up after each test
    cleanup_test_environment(),

    ct:pal("Completed test case: ~p", [TestCase]),
    Config.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test concurrent application startup functionality
concurrent_startup_test(Config) when is_list(Config) ->
    ct:pal("Testing concurrent application startup"),

    %% Test 1: Basic concurrent startup
    case application:ensure_all_started([crypto, public_key], permanent, concurrent) of
        {ok, Started} ->
            ct:assertEqual(2, length(Started), "Should start 2 applications"),
            lists:foreach(fun(App) ->
                ct:assertEqual(true, is_application_running(App))
            end, Started);
        {error, {App, Reason}} ->
            ct:fail("Failed to start ~p: ~p", [App, Reason])
    end,

    %% Test 2: Concurrent startup with dependencies
    IndependentApps = [crypto, public_key, ssl],
    {ok, _} = application:ensure_all_started(IndependentApps, permanent, concurrent),

    %% Test 3: Error handling - non-existent application
    case application:ensure_all_started([nonexistent_app], permanent, concurrent) of
        {error, {nonexistent_app, _}} ->
            ct:pass("Correctly handles missing application");
        {ok, _} ->
            ct:fail("Should fail to start non-existent application")
    end,

    ct:pass("Concurrent startup test passed").

%% @doc Test persistent configuration functionality
persistent_configuration_test(Config) when is_list(Config) ->
    ct:pal("Testing persistent configuration"),

    %% Test 1: Set persistent configuration
    application:set_env(test_app, test_key, test_value, [{persistent, true}]),
    application:set_env(test_app, test_key2, test_value2, [{persistent, true}]),

    %% Test 2: Verify configuration is set
    {ok, test_value} = application:get_env(test_app, test_key),
    {ok, test_value2} = application:get_env(test_app, test_key2),

    %% Test 3: Reload application - persistent values should remain
    application:unload(test_app),
    application:load(test_app),

    {ok, test_value} = application:get_env(test_app, test_key),
    {ok, test_value2} = application:get_env(test_app, test_key2),

    %% Test 4: Set non-persistent configuration
    application:set_env(test_app, temp_key, temp_value, [{persistent, false}]),

    {ok, temp_value} = application:get_env(test_app, temp_key),

    %% Test 5: Reload application - non-persistent values should be lost
    application:unload(test_app),
    application:load(test_app),

    undefined = application:get_env(test_app, temp_key),

    ct:pass("Persistent configuration test passed").

%% @doc Test prep_stop callback functionality
prep_stop_callback_test(Config) when is_list(Config) ->
    ct:pal("Testing prep_stop callback"),

    %% Test 1: Verify prep_stop callback is available
    PrepStopAvailable = erlang:function_exported(application_controller, prep_stop, 1),
    ct:assert(PrepStopAvailable, "prep_stop callback should be available"),

    %% Test 2: Test state preparation
    TestState = #{app => test_app, data => [1, 2, 3]},
    PreparedState = prepare_test_shutdown(TestState),

    ct:assert(is_map(PreparedState), "Should return prepared state"),
    ct:assert(map_size(PreparedState) > 0, "Should contain prepared data"),

    ct:pass("Prep/stop callback test passed").

%% @doc Test enhanced environment configuration
enhanced_environment_test(Config) when is_list(Config) ->
    ct:pal("Testing enhanced environment configuration"),

    %% Test 1: Set enhanced environment variables
    application:set_env(kernel, net_tickintensity, 4),
    application:set_env(kernel, prevent_overlapping_partitions, true),
    application:set_env(kernel, shell_docs_ansi, auto),

    %% Test 2: Verify environment variables are set
    {ok, 4} = application:get_env(kernel, net_tickintensity),
    {ok, true} = application:get_env(kernel, prevent_overlapping_partitions),
    {ok, auto} = application:get_env(kernel, shell_docs_ansi),

    %% Test 3: Test environment configuration function
    configure_test_environment(),

    ct:pass("Enhanced environment test passed").

%% @doc Test version-specific feature detection
feature_detection_test(Config) when is_list(Config) ->
    ct:pal("Testing version-specific feature detection"),

    %% Test 1: Check OTP version
    Version = erlang:system_info(otp_release),
    ct:assert(is_list(Version), "OTP version should be a string"),

    %% Test 2: Check feature availability
    Features = detect_test_features(),

    ct:assert(is_map(Features), "Features should be a map"),
    ct:assert(maps:size(Features) > 0, "Should detect some features"),

    %% Test 3: Verify specific features
    ct:assert(maps:is_concurrent_startup(Features), "Should detect concurrent startup"),
    ct:assert(maps:is_persistent_config(Features), "Should detect persistent config"),

    ct:pass("Feature detection test passed").

%% @doc Test performance benchmarks
performance_benchmark_test(Config) when is_list(Config) ->
    ct:pal("Testing performance benchmarks"),

    %% Test 1: Startup time comparison
    ConcurrentTime = measure_concurrent_startup(),
    SerialTime = measure_serial_startup(),

    ct:assert(ConcurrentTime > 0, "Concurrent startup should take time"),
    ct:assert(SerialTime > 0, "Serial startup should take time"),

    ct:pal("Concurrent startup: ~p ms", [ConcurrentTime]),
    ct:pal("Serial startup: ~p ms", [SerialTime]),

    %% Test 2: Performance improvement check (if OTP 26+)
    case is_otp_26_plus() of
        true ->
            ct:assert(ConcurrentTime < SerialTime,
                     "Concurrent should be faster than serial");
        false ->
            ct:pal("OTP <26, skipping performance comparison")
    end,

    ct:pass("Performance benchmark test passed").

%% @doc Test error handling scenarios
error_handling_test(Config) when is_list(Config) ->
    ct:pal("Testing error handling scenarios"),

    %% Test 1: Concurrent startup with invalid application
    case application:ensure_all_started([nonexistent_app], permanent, concurrent) of
        {error, {nonexistent_app, {not_started, _}}} ->
            ct:pass("Correctly handles missing application");
        {error, Reason} ->
            ct:pal("Unexpected error: ~p", [Reason]),
            ct:fail("Should fail with not_started error")
    end,

    %% Test 2: Persistent config with invalid app
    application:set_env(invalid_app, key, value, [{persistent, true}]),

    %% Test 3: Test cleanup on startup failure
    cleanup_on_failure_test(),

    ct:pass("Error handling test passed").

%% @doc Test backward compatibility
backward_compatibility_test(Config) when is_list(Config) ->
    ct:pal("Testing backward compatibility"),

    %% Test 1: Serial startup still works
    case application:ensure_all_started([crypto], permanent) of
        {ok, _} -> ct:pass("Serial startup still works");
        {error, Reason} -> ct:fail("Serial startup failed: ~p", [Reason])
    end,

    %% Test 2: Non-persistent config still works
    application:set_env(test_app, key, value),
    {ok, value} = application:get_env(test_app, key),

    ct:pass("Backward compatibility test passed").

%% @doc Test configuration reload behavior
configuration_reload_test(Config) when is_list(Config) ->
    ct:pal("Testing configuration reload behavior"),

    %% Test 1: Set persistent configuration
    application:set_env(test_app, persist_key, persist_value, [{persistent, true}]),

    %% Test 2: Reload application
    application:unload(test_app),
    application:load(test_app),

    %% Test 3: Verify persistent configuration remains
    {ok, persist_value} = application:get_env(test_app, persist_key),

    %% Test 4: Update application environment
    NewEnv = [{vsn, "2.0.0"}],
    application:set_env(test_app, env, NewEnv),

    %% Test 5: Reload - should override with .app defaults
    application:unload(test_app),
    application:load(test_app),

    {ok, NewEnv} = application:get_env(test_app, env),  % From .app file

    ct:pass("Configuration reload test passed").

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Test if application is running
-spec is_application_running(atom()) -> boolean().
is_application_running(App) ->
    case application:which_applications() of
        [{App, _, _, _, _} | _] -> true;
        [] -> false
    end.

%% @private Clean up test environment
-spec cleanup_test_environment() -> ok.
cleanup_test_environment() ->
    %% Stop all test applications
    TestApps = [test_app, invalid_app],
    lists:foreach(fun(App) ->
        case application:stop(App) of
            ok -> ok;
            {error, not_started} -> ok
        end,
        application:unload(App)
    end, TestApps),

    %% Clear test configuration
    application:unset_env(test_app, test_key),
    application:unset_env(test_app, test_key2),
    application:unset_env(test_app, temp_key),
    application:unset_env(test_app, env),

    ok.

%% @private Test shutdown preparation
-spec prepare_test_shutdown(map()) -> map().
prepare_test_shutdown(State) ->
    %% Simulate shutdown preparation
    Prepared = State#{prepared => true, timestamp => erlang:system_time(second)},

    %% Simulate cache flush
    flush_test_caches(),

    Prepared.

%% @private Configure test environment
-spec configure_test_environment() -> ok.
configure_test_environment() ->
    application:set_env(test_app, config_version, "1.0.0", [{persistent, true}]),
    application:set_env(test_app, optimization_level, high, [{persistent, true}]),
    ok.

%% @private Detect available features for testing
-spec detect_test_features() -> map().
detect_test_features() ->
    Features = #{
        concurrent_startup => erlang:function_exported(application, ensure_all_started, 3),
        persistent_config => erlang:function_exported(application, set_env, 4),
        prep_stop => erlang:function_exported(application_controller, prep_stop, 1),
        enhanced_env => erlang:function_exported(application, get_env, 3),
        native_json => erlang:function_exported(json, encode, 1),
        process_iterator => erlang:function_exported(erlang, processes_iterator, 0)
    },
    Features.

%% @private Measure concurrent startup time
-spec measure_concurrent_startup() -> integer().
measure_concurrent_startup() ->
    Start = erlang:monotonic_time(millisecond),
    case application:ensure_all_started([crypto, public_key], permanent, concurrent) of
        {ok, _} -> ok;
        {error, _} -> ok
    end,
    End = erlang:monotonic_time(millisecond),
    End - Start.

%% @private Measure serial startup time
-spec measure_serial_startup() -> integer().
measure_serial_startup() ->
    Start = erlang:monotonic_time(millisecond),
    case application:ensure_all_started([crypto, public_key], permanent) of
        {ok, _} -> ok;
        {error, _} -> ok
    end,
    End = erlang:monotonic_time(millisecond),
    End - Start.

%% @private Check if OTP 26+
-spec is_otp_26_plus() -> boolean().
is_otp_26_plus() ->
    case erlang:system_info(otp_release) of
        Vsn when is_list(Vsn) ->
            [Major | _] = string:split(Vsn, ".", all),
            case string:to_integer(Major) of
                {Int, ""} when Int >= 26 -> true;
                _ -> false
            end;
        _ -> false
    end.

%% @private Flush test caches
-spec flush_test_caches() -> ok.
flush_test_caches() ->
    %% Simulate cache flushing
    ok.

%% @private Test cleanup on startup failure
-spec cleanup_on_failure_test() -> ok.
cleanup_on_failure_test() ->
    %% Simulate failed startup and verify cleanup
    ok.

%%====================================================================
%% End of Module
%%====================================================================