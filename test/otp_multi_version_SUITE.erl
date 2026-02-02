%%%-------------------------------------------------------------------
%%% @doc
%%% Multi-OTP Version Test Suite
%%%
%%% This suite tests the multi-OTP version support for erlmcp,
%%% ensuring compatibility across OTP 26-28 with proper fallbacks
%%% and optimizations.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(otp_multi_version_SUITE).

%% Test callbacks
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
%% Test cases
-export([otp_version_detection/1, feature_detection/1, runtime_adaptation/1,
         conditional_compilation/1, configuration_migration/1, graceful_degradation/1,
         performance_optimization/1, error_handling/1, backward_compatibility/1,
         forward_compatibility/1, memory_efficiency/1, json_handling/1, process_enumeration/1,
         priority_messages/1]).

-include_lib("common_test/include/ct.hrl").

-include("otp_compat.hrl").

%%====================================================================
%% Test Configuration
%%====================================================================

all() ->
    [otp_version_detection,
     feature_detection,
     runtime_adaptation,
     conditional_compilation,
     configuration_migration,
     graceful_degradation,
     performance_optimization,
     error_handling,
     backward_compatibility,
     forward_compatibility,
     memory_efficiency,
     json_handling,
     process_enumeration,
     priority_messages].

init_per_suite(Config) ->
    %% Ensure modules are compiled
    case code:ensure_loaded(erlmcp_version_detector) of
        {module, _} ->
            ok;
        {error, _} ->
            ct:fail("Failed to load erlmcp_version_detector")
    end,

    case code:ensure_loaded(erlmcp_feature_detector) of
        {module, _} ->
            ok;
        {error, _} ->
            ct:fail("Failed to load erlmcp_feature_detector")
    end,

    case code:ensure_loaded(erlmcp_runtime_adapter) of
        {module, _} ->
            ok;
        {error, _} ->
            ct:fail("Failed to load erlmcp_runtime_adapter")
    end,

    %% Get current OTP version
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(),

    ct:pal("Starting multi-OTP test suite - OTP Version: ~p, Support Level: ~p",
           [Version, SupportLevel]),

    %% Set up test environment
    application:ensure_all_started(erlmcp),

    %% Configure for testing
    application:set_env(erlmcp, test_mode, true),
    application:set_env(erlmcp, optimization_level, test),

    Config.

end_per_suite(Config) ->
    %% Clean up
    application:stop(erlmcp),
    application:stop(kernel),
    application:stop(sasl),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),

    %% Configure test-specific settings
    TestConfig = get_test_config(TestCase),
    application:set_env(erlmcp, test_case, TestCase),

    %% Start monitoring for performance tests
    case TestCase of
        performance_optimization ->
            start_performance_monitoring();
        memory_efficiency ->
            start_memory_monitoring();
        _ ->
            ok
    end,

    Config.

end_per_testcase(TestCase, Config) ->
    %% Clean up test-specific settings
    application:unset_env(erlmcp, test_case),

    %% Stop monitoring
    case TestCase of
        performance_optimization ->
            stop_performance_monitoring();
        memory_efficiency ->
            stop_memory_monitoring();
        _ ->
            ok
    end,

    ct:pal("Completed test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

otp_version_detection(_Config) ->
    %% Test version detection
    Version = erlmcp_version_detector:otp_version(),
    VersionString = erlmcp_version_detector:otp_version_string(),

    ct:assert(is_tuple(Version), "Version should be a tuple"),
    ct:assert_equal(3, tuple_size(Version), "Version tuple should have 3 elements"),

    ct:assert(is_list(VersionString), "Version string should be a list"),

    %% Test support level
    SupportLevel = erlmcp_version_detector:get_support_level(),
    ct:assert(
        list:member(SupportLevel, [unsupported, legacy, stable, recommended]),
        "Invalid support level"),

    %% Test version comparison
    IsOTP28Plus = erlmcp_version_detector:is_version_at_least(Version, {28, 0, 0}),
    IsOTP26Plus = erlmcp_version_detector:is_version_at_least(Version, {26, 0, 0}),

    ct:assert(IsOTP26Plus, "Should be OTP 26+"),
    case Version of
        {28, _, _} ->
            ct:assert(IsOTP28Plus, "Should be OTP 28+");
        _ ->
            ct:assert_not(IsOTP28Plus, "Should not be OTP 28+")
    end,

    %% Test optimal settings
    OptimalSettings = erlmcp_version_detector:get_optimal_settings(),
    ct:assert(is_map(OptimalSettings), "Optimal settings should be a map"),

    ok.

feature_detection(_Config) ->
    %% Test feature detection
    FeatureFlags = erlmcp_feature_detector:detect_features(),
    ct:assert(is_list(FeatureFlags), "Feature flags should be a list"),

    %% Test specific features
    NativeJson = erlmcp_feature_detector:is_feature_available(native_json),
    ProcessIterator = erlmcp_feature_detector:is_feature_available(process_iterator),
    PriorityMessages = erlmcp_feature_detector:is_feature_available(priority_messages),

    ct:assert(is_boolean(NativeJson), "Native JSON flag should be boolean"),
    ct:assert(is_boolean(ProcessIterator), "Process iterator flag should be boolean"),
    ct:assert(is_boolean(PriorityMessages), "Priority messages flag should be boolean"),

    %% Test configuration based on version
    case erlmcp_version_detector:get_support_level() of
        unsupported ->
            ct:assert_not(NativeJson, "Native JSON should not be available on unsupported");
        legacy ->
            ct:assert_not(NativeJson, "Native JSON should not be available on legacy");
        stable ->
            ct:assert(NativeJson, "Native JSON should be available on stable");
        recommended ->
            ct:assert(NativeJson, "Native JSON should be available on recommended"),
            ct:assert(ProcessIterator, "Process iterator should be available on recommended"),
            ct:assert(PriorityMessages, "Priority messages should be available on recommended")
    end,

    %% Test warning system
    erlmcp_feature_detector:warn_unsupported_features(),

    ok.

runtime_adaptation(_Config) ->
    %% Test runtime adaptation
    Version = erlmcp_version_detector:otp_version(),

    %% Test adaptation
    ok = erlmcp_runtime_adapter:adapt_to_otp_version(Version),

    %% Test performance optimization
    OptimizationLevel = erlmcp_version_detector:get_support_level(),
    ok = erlmcp_runtime_adapter:optimize_for_performance(OptimizationLevel),

    %% Test feature gap handling
    FeatureFlags = erlmcp_feature_detector:get_feature_flags(),
    ok = erlmcp_runtime_adapter:handle_feature_gaps(FeatureFlags),

    %% Test resource allocation
    PoolSize = erlmcp_runtime_adapter:get_optimal_pool_size(),
    Timeout = erlmcp_runtime_adapter:get_optimal_timeout(),
    BatchSize = erlmcp_runtime_adapter:get_optimal_batch_size(),

    ct:assert(is_integer(PoolSize), "Pool size should be integer"),
    ct:assert(PoolSize > 0, "Pool size should be positive"),
    ct:assert(is_integer(Timeout), "Timeout should be integer"),
    ct:assert(Timeout > 0, "Timeout should be positive"),
    ct:assert(is_integer(BatchSize), "Batch size should be integer"),
    ct:assert(BatchSize > 0, "Batch size should be positive"),

    %% Test monitoring
    ok = erlmcp_runtime_adapter:monitor_performance(),

    ok.

conditional_compilation(_Config) ->
    %% Test conditional compilation macros
    CompileFeatures = get_compile_features(),

    %% Test that macros expand correctly
    case erlmcp_version_detector:get_support_level() of
        legacy ->
            ct:assert_not(?HAVE_PROCESS_ITERATOR, "Process iterator should not be available");
        recommended ->
            ct:assert(?HAVE_PROCESS_ITERATOR, "Process iterator should be available"),
            ct:assert(?HAVE_PRIORITY_MESSAGES, "Priority messages should be available");
        _ ->
            ok
    end,

    %% Test JSON handling
    TestMap = #{test => "value", number => 42},
    JsonEncoded = ?JSON_ENCODE(TestMap),
    JsonDecoded = ?JSON_DECODE(JsonEncoded),

    ct:assert(is_binary(JsonEncoded), "JSON should be encoded to binary"),
    ct:assert(is_map(JsonDecoded), "JSON should be decoded to map"),
    ct:assert_equal(TestMap, JsonDecoded, "Decoded JSON should match original"),

    %% Test safe macros
    ProcessCount = ?SAFE_PROCESS_COUNT(),
    ProcessList = ?SAFE_PROCESSES(),

    ct:assert(is_integer(ProcessCount), "Process count should be integer"),
    ct:assert(ProcessCount >= 0, "Process count should be non-negative"),
    ct:assert(is_list(ProcessList), "Process list should be a list"),

    %% Test priority macros
    ?SET_PRIORITY_HIGH(),
    ?SEND_PRIORITY(self(), test_message),

    ok.

configuration_migration(_Config) ->
    %% Test configuration migration
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(),

    %% Test version-specific configuration
    Config = erlmcp_runtime_adapter:configure_for_version(Version),
    ct:assert(is_list(Config), "Configuration should be a list"),

    %% Test feature-based configuration
    FeatureFlags = erlmcp_feature_detector:get_feature_flags(),
    ok = erlmcp_runtime_adapter:handle_feature_gaps(FeatureFlags),

    %% Test optimization levels
    Conservative = erlmcp_runtime_adapter:optimize_for_performance(conservative),
    Balanced = erlmcp_runtime_adapter:optimize_for_performance(balanced),
    Optimal = erlmcp_runtime_adapter:optimize_for_performance(optimal),

    ct:assert(is_tuple(Conservative), "Conservative optimization should return"),
    ct:assert(is_tuple(Balanced), "Balanced optimization should return"),
    ct:assert(is_tuple(Optimal), "Optimal optimization should return"),

    ok.

graceful_degradation(_Config) ->
    %% Test graceful degradation when features are missing
    FeatureFlags = [{native_json, false}, {process_iterator, false}, {priority_messages, false}],

    %% Test JSON fallback
    TestMap = #{test => "value"},
    FallbackJson = jsx:encode(TestMap),
    ct:assert(is_binary(FallbackJson), "Fallback JSON should be binary"),

    %% Test process enumeration fallback
    ProcessCount = erlang:system_info(process_count),
    ct:assert(is_integer(ProcessCount), "Process count should be integer"),

    %% Test message fallback
    ok = erlang:send(self(), test_message, [nosuspend]),
    receive
        test_message ->
            ok
    after 1000 ->
        ct:fail("Message send failed")
    end,

    %% Test feature gap handling
    ok = erlmcp_runtime_adapter:handle_feature_gaps(FeatureFlags),

    ok.

performance_optimization(_Config) ->
    %% Test performance optimizations
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(),

    %% Test memory optimization
    InitialMemory = erlang:memory(total),
    ProcessCountBefore = erlang:system_info(process_count),

    %% Create some test processes
    TestPids = [spawn(fun() -> timer:sleep(1000) end) || _ <- lists:seq(1, 100)],

    %% Measure process enumeration performance
    StartTime = erlang:monotonic_time(millisecond),
    ProcessList = ?SAFE_PROCESSES(),
    EndTime = erlang:monotonic_time(millisecond),

    ProcessEnumerationTime = EndTime - StartTime,
    ct:pal("Process enumeration took ~p ms for ~p processes",
           [ProcessEnumerationTime, length(ProcessList)]),

    %% Clean up test processes
    [exit(Pid, normal) || Pid <- TestPids],

    %% Test JSON performance
    TestData = #{data => lists:seq(1, 1000)},
    JsonStart = erlang:monotonic_time(millisecond),
    _JsonEncoded = ?JSON_ENCODE(TestData),
    JsonEnd = erlang:monotonic_time(millisecond),

    JsonTime = JsonEnd - JsonStart,
    ct:pal("JSON encoding took ~p ms", [JsonTime]),

    %% Validate performance expectations
    case SupportLevel of
        recommended ->
            ct:assert(ProcessEnumerationTime < 100, "Fast process enumeration expected");
        stable ->
            ct:assert(ProcessEnumerationTime < 200, "Moderate process enumeration expected");
        legacy ->
            ct:assert(ProcessEnumerationTime < 500, "Slower process enumeration expected");
        _ ->
            ok
    end,

    ok.

error_handling(_Config) ->
    %% Test error handling for unsupported versions
    %% This should not crash but should log warnings
    erlmcp_version_detector:is_otp_supported(),

    %% Test invalid feature requests
    case erlmcp_feature_detector:is_feature_available(invalid_feature) of
        false ->
            ok;
        _ ->
            ct:fail("Invalid feature should return false")
    end,

    %% Test version comparison edge cases
    Greater = erlmcp_version_detector:compare_versions({28, 1, 0}, {28, 0, 0}),
    Equal = erlmcp_version_detector:compare_versions({27, 0, 0}, {27, 0, 0}),
    Less = erlmcp_version_detector:compare_versions({26, 0, 0}, {27, 0, 0}),

    ct:assert_equal(gt, Greater, "Version comparison should work"),
    ct:assert_equal(eq, Equal, "Version comparison should work"),
    ct:assert_equal(lt, Less, "Version comparison should work"),

    ok.

backward_compatibility(_Config) ->
    %% Test that erlmcp works correctly on older OTP versions
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(),

    %% Test basic functionality works on all versions
    TestMap = #{test => "value"},
    JsonData = ?JSON_ENCODE(TestMap),
    ct:assert(is_binary(JsonData), "JSON encoding should work"),

    %% Test basic process handling
    TestPid =
        spawn(fun() ->
                 receive after 1000 ->
                     ok
                 end
              end),
    ?SEND_PRIORITY(TestPid, test_message),

    %% Verify message delivery
    TestPid ! {self(), test_response},
    receive
        {test_pid, _} ->
            ok
    after 1000 ->
        ct:fail("Message delivery failed")
    end,

    %% Clean up
    exit(TestPid, normal),

    ok.

forward_compatibility(_Config) ->
    %% Test that erlmcp works correctly on newer OTP versions
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(),

    %% Test that future OTP versions are handled gracefully
    case SupportLevel of
        recommended ->
            %% All features should be available
            ct:assert(?HAVE_PROCESS_ITERATOR, "Process iterator should be available"),
            ct:assert(?HAVE_PRIORITY_MESSAGES, "Priority messages should be available"),
            ct:assert(?HAVE_NATIVE_JSON, "Native JSON should be available");
        _ ->
            %% For older versions, ensure graceful degradation
            ok
    end,

    %% Test that version detection works with future versions
    IsVersionAtLeast = erlmcp_version_detector:is_version_at_least(Version, {29, 0, 0}),
    ct:assert(is_boolean(IsVersionAtLeast), "Version comparison should work"),

    ok.

memory_efficiency(_Config) ->
    %% Test memory efficiency improvements
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(),

    %% Measure memory usage
    InitialMemory = erlang:memory(total),

    %% Test process enumeration memory efficiency
    ProcessCount = ?SAFE_PROCESS_COUNT(),
    ct:pal("Current process count: ~p", [ProcessCount]),

    %% Test process iterator if available
    case SupportLevel of
        recommended ->
            %% Process iterator should use O(1) memory
            Iterator = erlang:processes_iterator(),
            Count = erlmcp_otp_compat:count_processes_iterator(Iterator, 0),
            ct:assert_equal(ProcessCount, Count, "Process count should match");
        _ ->
            %% Legacy process enumeration
            ct:pal("Using legacy process enumeration")
    end,

    %% Measure memory after operations
    FinalMemory = erlang:memory(total),
    MemoryDelta = FinalMemory - InitialMemory,

    ct:pal("Memory delta: ~p bytes", [MemoryDelta]),

    %% Validate memory efficiency
    case SupportLevel of
        recommended ->
            ct:assert(MemoryDelta < 1000000, "Memory increase should be minimal");
        legacy ->
            ct:assert(MemoryDelta < 5000000, "Legacy mode should still be reasonable");
        _ ->
            ok
    end,

    ok.

json_handling(_Config) ->
    %% Test JSON handling across different OTP versions
    TestMap =
        #{string => "test",
          number => 42,
          boolean => true,
          array => [1, 2, 3],
          nested => #{inner => "value"}},

    %% Test JSON encoding
    JsonEncoded = ?JSON_ENCODE(TestMap),
    ct:assert(is_binary(JsonEncoded), "JSON should be encoded to binary"),

    %% Test JSON decoding
    JsonDecoded = ?JSON_DECODE(JsonEncoded),
    ct:assert(is_map(JsonDecoded), "JSON should be decoded to map"),
    ct:assert_equal(TestMap, JsonDecoded, "Decoded JSON should match original"),

    %% Test safe JSON encoding
    SafeEncoded = ?JSON_ENCODE_SAFE(TestMap),
    SafeDecoded = ?JSON_DECODE_SAFE(SafeEncoded),
    ct:assert_equal(TestMap, SafeDecoded, "Safe JSON should match"),

    %% Test edge cases
    EmptyMap = #{},
    EmptyJson = ?JSON_ENCODE(EmptyMap),
    EmptyDecoded = ?JSON_DECODE(EmptyJson),
    ct:assert_equal(EmptyMap, EmptyDecoded, "Empty map should work"),

    ok.

process_enumeration(_Config) ->
    %% Test process enumeration methods
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(),

    %% Test safe process count
    ProcessCount = ?SAFE_PROCESS_COUNT(),
    ct:assert(is_integer(ProcessCount), "Process count should be integer"),
    ct:assert(ProcessCount >= 0, "Process count should be non-negative"),

    %% Test safe process list
    ProcessList = ?SAFE_PROCESSES(),
    ct:assert(is_list(ProcessList), "Process list should be a list"),
    ct:assert_equal(ProcessCount, length(ProcessList), "Count should match list length"),

    %% Test process iterator if available
    case SupportLevel of
        recommended ->
            Iterator = erlang:processes_iterator(),
            IteratorCount = count_with_iterator(Iterator),
            ct:assert_equal(ProcessCount, IteratorCount, "Iterator count should match");
        _ ->
            ct:pal("Process iterator not available on this version")
    end,

    ok.

priority_messages(_Config) ->
    %% Test priority message handling
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(),

    %% Test priority macro handling
    ?SET_PRIORITY_HIGH(),

    %% Test priority message sending
    TestPid =
        spawn(fun() ->
                 receive
                     {normal, Msg} ->
                         ct:pal("Normal message: ~p", [Msg]);
                     {priority, Msg} ->
                         ct:pal("Priority message: ~p", [Msg])
                 after 2000 ->
                     ct:fail("No message received")
                 end
              end),

    %% Send both normal and priority messages
    ?SEND_PRIORITY(TestPid, {priority, "test"}),
    erlang:send(TestPid, {normal, "test"}, [nosuspend]),

    %% Wait for message processing
    timer:sleep(100),

    %% Clean up
    exit(TestPid, normal),

    %% Test that priority macros are safe on older versions
    case SupportLevel of
        legacy ->
            ct:pal("Priority messages gracefully degrade on legacy version");
        stable ->
            ct:pal("Priority messages gracefully degrade on stable version");
        recommended ->
            ct:pal("Priority messages fully supported on recommended version");
        _ ->
            ok
    end,

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

get_test_config(TestCase) ->
    case TestCase of
        performance_optimization ->
            #{monitoring => true, iterations => 100};
        memory_efficiency ->
            #{monitoring => true, processes => 1000};
        _ ->
            #{}
    end.

get_compile_features() ->
    %% This would normally be generated by the compiler
    case erlmcp_version_detector:get_support_level() of
        legacy ->
            [{native_json, false}, {process_iterator, false}];
        recommended ->
            [{native_json, true}, {process_iterator, true}];
        _ ->
            [{native_json, false}, {process_iterator, false}]
    end.

count_with_iterator(Iterator) ->
    count_with_iterator(Iterator, 0).

count_with_iterator(Iterator, Acc) ->
    case erlang:process_next(Iterator) of
        {_Pid, NewIterator} ->
            count_with_iterator(NewIterator, Acc + 1);
        none ->
            Acc
    end.

start_performance_monitoring() ->
    %% Start performance monitoring
    erlmcp_runtime_adapter:monitor_performance().

stop_performance_monitoring() ->
    %% Stop performance monitoring
    ok.

start_memory_monitoring() ->
    %% Start memory monitoring
    application:set_env(erlmcp, memory_monitoring, true).

stop_memory_monitoring() ->
    %% Stop memory monitoring
    application:set_env(erlmcp, memory_monitoring, false).
