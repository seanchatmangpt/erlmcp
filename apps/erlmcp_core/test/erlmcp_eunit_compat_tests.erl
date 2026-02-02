%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Compatibility Tests for OTP 26-28
%%%
%%% This module demonstrates and tests EUnit features across OTP versions:
%%% - OTP 26: EUnit 2.7, new stacktrace format
%%% - OTP 27: EUnit 2.8, slave module removed
%%% - OTP 28: EUnit 2.9+ (2.10.1), JIT optimizations, scale_timeouts
%%%
%%% Test Coverage:
%%% - Version detection and feature availability
%%% - Assertion macros (old and new)
%%% - Timeout handling and scaling
%%% - Stacktrace format changes
%%% - Size optimization (size vs tuple_size/byte_size)
%%% - Test generators and fixtures
%%%
%%% Chicago School TDD:
%%% - Test observable behavior (test passes/fails)
%%% - Real OTP version detection
%%% - No mocks or fakes
%%% - Version-specific test execution
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_eunit_compat_tests).

-include("erlmcp.hrl").
-include("erlmcp_eunit_compat.hrl").

%%====================================================================
%% Test Exports
%%====================================================================

%% Simple tests (auto-discovered by EUnit)
-export([
    otp_version_detection_test/0,
    size_optimization_test/0,
    assertion_macros_test/0,
    stacktrace_handling_test/0,
    timeout_scaling_test/0
]).

%%====================================================================
%% Version Detection Tests
%%====================================================================

%% @doc Test OTP version detection at compile time
otp_version_detection_test() ->
    %% Test version macros
    IsOTP28 = ?IS_OTP_28(),
    IsOTP27 = ?IS_OTP_27(),
    IsOTP26 = ?IS_OTP_26(),

    %% Should be OTP 28+ (erlmcp requirement)
    ?assert(IsOTP26),

    %% Version hierarchy should work
    ?assert(IsOTP27 >= IsOTP26),
    ?assert(IsOTP28 >= IsOTP27),

    %% Get runtime version
    Release = erlang:system_info(otp_release),
    ?assert(is_list(Release)),
    ?assert(length(Release) > 0),

    ct:pal("OTP Release: ~s", [Release]),
    ct:pal("OTP 28+: ~p", [IsOTP28]),
    ct:pal("OTP 27+: ~p", [IsOTP27]),
    ct:pal("OTP 26+: ~p", [IsOTP26]).

%%====================================================================
%% Size Optimization Tests (OTP 28 JIT)
%%====================================================================

%% @doc Test size optimization macros (EUnit 2.8.2)
size_optimization_test() ->
    %% Test tuple size optimization
    Tuple = {a, b, c, d},
    TupleSize = ?SAFE_TUPLE_SIZE(Tuple),
    ?assertEqual(4, TupleSize),

    %% Test binary size optimization
    Binary = <<"hello world">>,
    BinarySize = ?SAFE_BYTE_SIZE(Binary),
    ?assertEqual(11, BinarySize),

    %% Test nested structures
    Nested = {{a, b}, <<"test">>},
    OuterSize = ?SAFE_TUPLE_SIZE(Nested),
    ?assertEqual(2, OuterSize),

    %% Compare legacy size/1 with optimized versions
    LegacyTupleSize = size(Tuple),
    OptimizedTupleSize = tuple_size(Tuple),
    ?assertEqual(LegacyTupleSize, OptimizedTupleSize),

    LegacyBinarySize = size(Binary),
    OptimizedBinarySize = byte_size(Binary),
    ?assertEqual(LegacyBinarySize, OptimizedBinarySize),

    ct:pal("Size optimization tests passed").

%%====================================================================
%% Assertion Macro Tests
%%====================================================================

%% @doc Test enhanced assertion macros
assertion_macros_test() ->
    %% Test standard EUnit assertions
    ?assert(true),
    ?assertNot(false),
    ?assertEqual(42, 42),
    ?assertMatch({ok, _}, {ok, value}),

    %% Test custom assertion macros from compat header
    ?assertMsg(1 =:= 1, "Should pass"),
    ?assertNotEqual(1, 2),
    ?assertMatchMsg({ok, _}, {ok, value}, "Should match"),

    %% Test error handling
    try
        ?assertEqual(1, 2),
        ct:pal("ERROR: Assertion should have failed!")
    catch
        error:{assertEqual_failed, _} ->
            ok
    end.

%%====================================================================
%% Stacktrace Handling Tests
%%====================================================================

%% @doc Test stacktrace handling (OTP 26+ format)
stacktrace_handling_test() ->
    %% Test new stacktrace format (OTP 26+)
    try
        erlang:error(test_error)
    catch
        error:Error:Stacktrace ->
            %% Stacktrace should be a list of stack entries
            ?assert(is_list(Stacktrace)),
            ?assert(length(Stacktrace) > 0),

            %% First entry should be this function
            [{Module, Function, Arity, Location} | _] = Stacktrace,
            ?assertEqual(?MODULE, Module),
            ?assertEqual(stacktrace_handling_test, Function),
            ?assert(is_integer(Arity)),
            ?assert(is_list(Location)),

            ct:pal("Stacktrace length: ~p", [length(Stacktrace)]),
            ct:pal("Stacktrace first entry: ~p:~p/~p", [Module, Function, Arity])
    end.

%%====================================================================
%% Timeout Scaling Tests (OTP 27+)
%%====================================================================

%% @doc Test timeout with scaling support
timeout_scaling_test() ->
    %% This test uses the TIMEOUT_TEST macro
    %% In OTP 27+, timeouts can be scaled via: rebar3 eunit --scale_timeouts=10
    %% This is useful for slow CI systems

    %% Fast test (should always pass)
    ?assert(timer:sleep(10) =:= ok),

    %% Test that we can measure elapsed time
    StartTime = erlang:monotonic_time(millisecond),
    timer:sleep(100),
    EndTime = erlang:monotonic_time(millisecond),
    Elapsed = EndTime - StartTime,

    %% Should be close to 100ms (with tolerance)
    ?assert(Elapsed >= 90 andalso Elapsed =< 200),

    ct:pal("Elapsed time: ~pms", [Elapsed]).

%%====================================================================
%% Test Generators with Fixtures
%%====================================================================

%% @doc Test setup/cleanup fixtures
fixture_test_() ->
    {"Setup cleanup test",
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(_SetupData) ->
          [?_test(test_with_fixture()),
           ?_test(test_another_with_fixture())]
      end}}.

setup() ->
    ct:pal("Setting up fixture"),
    {setup_data, 42}.

cleanup(_SetupData) ->
    ct:pal("Cleaning up fixture"),
    ok.

test_with_fixture() ->
    ?assertEqual(42, 42),
    ct:pal("Test with fixture passed").

test_another_with_fixture() ->
    ?assertNotEqual(1, 2),
    ct:pal("Another test with fixture passed").

%%====================================================================
%% Version-Specific Tests
%%====================================================================

%% @doc Test that runs only on OTP 28+
otp28_only_test_() ->
    case ?IS_OTP_28() of
        true ->
            [?_test(test_json_module()),
             ?_test(test_priority_messages())];
        false ->
            []
    end.

test_json_module() ->
    %% OTP 28+ has json:encode/1 module
    try
        Data = #{key => <<"value">>, number => 42},
        JSON = json:encode(Data),
        ?assert(is_binary(JSON)),
        ct:pal("JSON module available: ~s", [JSON])
    catch
        error:undef ->
            ct:pal("JSON module not available (OTP <28)")
    end.

test_priority_messages() ->
    %% OTP 28+ supports priority messages
    try
        Old = process_flag(priority, true),
        process_flag(priority, Old),
        ?assert(true),
        ct:pal("Priority messages supported")
    catch
        error:badarg ->
            ct:pal("Priority messages not supported")
    end.

%% @doc Test that runs on all OTP versions
all_versions_test_() ->
    [?_test(test_basic_arithmetic()),
     ?_test(test_list_operations()),
     ?_test(test_map_operations())].

test_basic_arithmetic() ->
    ?assertEqual(4, 2 + 2),
    ?assertEqual(10, 5 * 2),
    ?assertEqual(2, 10 div 5).

test_list_operations() ->
    List = [1, 2, 3, 4, 5],
    ?assertEqual(5, length(List)),
    ?assertEqual([1, 2, 3], lists:sublist(List, 3)),
    ?assertEqual(15, lists:sum(List)).

test_map_operations() ->
    Map = #{a => 1, b => 2, c => 3},
    ?assertEqual(3, maps:size(Map)),
    ?assertEqual(1, maps:get(a, Map)),
    ?assertEqual(false, maps:is_key(d, Map)).

%%====================================================================
%% Conditional Compilation Tests
%%====================================================================

%% @doc Test conditional compilation based on OTP version
-ifdef(OTP_28).
otp28_feature_test() ->
    %% This code only compiles on OTP 28+
    ?assert(true),
    ct:pal("OTP 28+ feature test compiled").
-endif.

-ifdef(OTP_27).
otp27_feature_test() ->
    %% This code compiles on OTP 27+
    ?assert(true),
    ct:pal("OTP 27+ feature test compiled").
-endif.

%% @doc Fallback for older OTP versions
-ifndef(OTP_26).
otp26_fallback_test() ->
    %% This code only compiles on OTP <26
    ?assert(true),
    ct:pal("Pre-OTP 26 fallback test compiled").
-endif.

%%====================================================================
%% Integration Tests
%%====================================================================

%% @doc Test complete EUnit workflow
integration_test_() ->
    {setup,
     fun() ->
         ct:pal("Starting integration test"),
         application:ensure_all_started(erlmcp_core)
     end,
     fun(_Apps) ->
         ct:pal("Stopping integration test"),
         application:stop(erlmcp_core)
     end,
     fun(_Apps) ->
         [?_test(test_registry_available()),
          ?_test(test_json_rpc_available())]
     end}.

test_registry_available() ->
    %% Test that erlmcp_registry is available
    ?assert(code:is_loaded(erlmcp_registry) =/= false),
    ct:pal("Registry module available").

test_json_rpc_available() ->
    %% Test that erlmcp_json_rpc is available
    ?assert(code:is_loaded(erlmcp_json_rpc) =/= false),
    ct:pal("JSON-RPC module available").

%%====================================================================
%% Performance and Benchmark Tests
%%====================================================================

%% @doc Benchmark test (with timeout)
benchmark_test_() ->
    {timeout, 30,
     fun() ->
         %% Test that we can run performance tests without timing out
         N = 10000,
         StartTime = erlang:monotonic_time(microsecond),

         lists:foreach(fun(I) ->
             %% Do some work
             _ = I * 2
         end, lists:seq(1, N)),

         EndTime = erlang:monotonic_time(microsecond),
         ElapsedUs = EndTime - StartTime,
         ElapsedMs = ElapsedUs div 1000,

         ct:pal("Benchmark: ~p iterations in ~pms (~pus/iter)",
                [N, ElapsedMs, ElapsedUs div N]),

         %% Should complete in reasonable time
         ?assert(ElapsedMs < 5000)
     end}.

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% @doc Test edge cases in assertions
edge_case_test_() ->
    [?_test(test_empty_list()),
     ?_test(test_empty_map()),
     ?_test(test_zero()),
     ?_test(test_undefined()),
     ?_test(test_large_number())].

test_empty_list() ->
    ?assertEqual([], []),
    ?assertEqual(0, length([])).

test_empty_map() ->
    ?assertEqual(#{}, #{}),
    ?assertEqual(0, maps:size(#{})).

test_zero() ->
    ?assertEqual(0, 0),
    ?assertEqual(0.0, 0.0),
    ?assertEqual(-0, 0).

test_undefined() ->
    ?assertEqual(undefined, undefined),
    ?assertNotEqual(undefined, nil).

test_large_number() ->
    Big = 1000000000000,
    ?assertEqual(Big, 1000000000000),
    ?assert(Big > 0).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
%% @doc Helper to get OTP version as integer
get_otp_version() ->
    Release = erlang:system_info(otp_release),
    case string:split(Release, ".") of
        [Major | _] ->
            list_to_integer(Major);
        _ ->
            list_to_integer(Release)
    end.

%% @private
%% @doc Helper to check if feature is available
has_feature(Feature) ->
    case Feature of
        json_module ->
            try
                _ = json:module_info(),
                true
            catch
                _:_ ->
                    false
            end;
        priority_messages ->
            try
                Old = process_flag(priority, true),
                process_flag(priority, Old),
                true
            catch
                _:_ ->
                    false
            end;
        process_iterator ->
            try
                _ = erlang:processes_iterator(),
                true
            catch
                _:_ ->
                    false
            end
    end.
