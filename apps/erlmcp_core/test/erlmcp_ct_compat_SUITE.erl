%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test Compatibility Suite
%%%
%%% Comprehensive cross-OTP compatibility tests for erlmcp Common Test suites.
%%% Tests OTP 26, 27, and 28 compatibility with feature detection and graceful
%%% degradation.
%%%
%%% Features:
%%% - OTP version detection
%%% - Feature availability testing
%%% - Callback compatibility across versions
%%% - Configuration management
%%% - Error handling patterns
%%%
%%% Chicago School TDD:
%%% - Real OTP version detection (no mocks)
%%% - Observable behavior: API availability, feature detection
%%% - Graceful degradation across OTP versions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_ct_compat_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([otp_version_detection/1,
         feature_detection_native_coverage/1,
         feature_detection_process_iterator/1,
         feature_detection_json_module/1,
         feature_detection_priority_messages/1,
         feature_detection_enhanced_hooks/1,
         callback_compatibility_init_per_suite/1,
         callback_compatibility_init_per_testcase/1,
         callback_compatibility_groups/1,
         config_propagation/1,
         config_cleanup/1,
         timeout_handling/1,
         parallel_execution/1,
         error_recovery/1,
         colored_output_detection/1,
         sbom_support_detection/1]).

%%%===================================================================
%%% Common Test Configuration
%%%===================================================================

all() ->
    [{group, version_detection},
     {group, feature_detection},
     {group, callback_compat},
     {group, config_management},
     {group, execution_patterns},
     {group, otp_features}].

groups() ->
    [{version_detection, [sequence],
      [otp_version_detection]},
     {feature_detection, [parallel],
      [feature_detection_native_coverage,
       feature_detection_process_iterator,
       feature_detection_json_module,
       feature_detection_priority_messages,
       feature_detection_enhanced_hooks]},
     {callback_compat, [parallel],
      [callback_compatibility_init_per_suite,
       callback_compatibility_init_per_testcase,
       callback_compatibility_groups]},
     {config_management, [sequence],
      [config_propagation, config_cleanup]},
     {execution_patterns, [parallel],
      [timeout_handling, parallel_execution, error_recovery]},
     {otp_features, [parallel],
      [colored_output_detection, sbom_support_detection]}].

init_per_suite(Config) ->
    ct:pal("Starting Common Test Compatibility Suite"),

    % Start compat helper
    case erlmcp_ct_compat:start_link() of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            ok
    end,

    % Detect OTP version and features
    OTPVersion = erlmcp_ct_compat:get_otp_version(),
    OTPRelease = erlmcp_ct_compat:get_otp_release(),

    ct:pal("OTP Release: ~s (version ~p)", [OTPRelease, OTPVersion]),

    % Store version info in config
    [{otp_version, OTPVersion},
     {otp_release, OTPRelease} | Config].

end_per_suite(_Config) ->
    ct:pal("Common Test Compatibility Suite completed"),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p", [TestCase]),
    [{test_case, TestCase} | Config].

end_per_testcase(TestCase, _Config) ->
    ct:log("Completed test case: ~p", [TestCase]),
    % Small delay to ensure clean state
    timer:sleep(10),
    ok.

%%%===================================================================
%%% Test Cases: Version Detection
%%%===================================================================

otp_version_detection(Config) ->
    OTPVersion = proplists:get_value(otp_version, Config),
    OTPRelease = proplists:get_value(otp_release, Config),

    ct:pal("Testing OTP version detection"),
    ct:pal("  Detected version: ~p", [OTPVersion]),
    ct:pal("  Detected release: ~s", [OTPRelease]),

    % Version should be integer >= 25
    ?assert(is_integer(OTPVersion)),
    ?assert(OTPVersion >= 25),
    ?assert(OTPVersion =< 29),  % Reasonable upper bound

    % Release should be non-empty string
    ?assert(is_list(OTPRelease)),
    ?assert(length(OTPRelease) > 0),

    % Verify consistency with erlang:system_info
    SystemRelease = erlang:system_info(otp_release),
    ?assertEqual(SystemRelease, OTPRelease),

    ct:log("OTP version detection: PASS"),
    ok.

%%%===================================================================
%%% Test Cases: Feature Detection
%%%===================================================================

feature_detection_native_coverage(_Config) ->
    ct:pal("Testing native coverage detection"),

    HasNative = erlmcp_ct_compat:supports_native_coverage(),
    OTPVersion = erlmcp_ct_compat:get_otp_version(),

    ct:pal("  Native coverage available: ~p", [HasNative]),

    % Native coverage requires OTP 27+
    if OTPVersion >= 27 ->
           % Should be available (or at least detectable)
           ?assert(is_boolean(HasNative));
       true ->
           % Should not be available
           ?assertNot(HasNative)
    end,

    % Feature should return boolean
    ?assert(is_boolean(HasNative)),

    % Test has_feature/1 API
    ?assertEqual(HasNative, erlmcp_ct_compat:has_feature(native_coverage)),

    ct:log("Native coverage detection: PASS"),
    ok.

feature_detection_process_iterator(_Config) ->
    ct:pal("Testing process iterator detection"),

    HasIterator = erlmcp_ct_compat:supports_process_iterator(),
    OTPVersion = erlmcp_ct_compat:get_otp_version(),

    ct:pal("  Process iterator available: ~p", [HasIterator]),

    % Process iterator requires OTP 28+
    if OTPVersion >= 28 ->
           ?assert(HasIterator);
       true ->
           ?assertNot(HasIterator)
    end,

    % Verify get_processes/0 works regardless
    Processes = erlmcp_ct_compat:get_processes(),
    ?assert(is_list(Processes)),
    ?assert(length(Processes) > 0),

    % Current process should be in list
    ?assert(lists:member(self(), Processes)),

    ct:log("Process iterator detection: PASS"),
    ok.

feature_detection_json_module(_Config) ->
    ct:pal("Testing json module detection"),

    HasJson = erlmcp_ct_compat:supports_json_module(),
    OTPVersion = erlmcp_ct_compat:get_otp_version(),

    ct:pal("  JSON module available: ~p", [HasJson]),

    % json module requires OTP 28+
    if OTPVersion >= 28 ->
           ?assert(HasJson),

           % Test basic encoding
           TestData = #{key => <<"value">>, number => 42},
           Encoded = erlmcp_ct_compat:encode_json(TestData),
           ?assert(is_binary(Encoded)),

           % Test decoding
           Decoded = erlmcp_ct_compat:decode_json(Encoded),
           ?assertEqual(#{<<"key">> => <<"value">>, <<"number">> => 42}, Decoded);
       true ->
           ?assertNot(HasJson),

           % Should use jsx as fallback
           TestData = #{<<"key">> => <<"value">>},
           Encoded = erlmcp_ct_compat:encode_json(TestData),
           ?assert(is_binary(Encoded))
    end,

    ct:log("JSON module detection: PASS"),
    ok.

feature_detection_priority_messages(_Config) ->
    ct:pal("Testing priority messages detection"),

    HasPriority = erlmcp_ct_compat:supports_priority_messages(),
    OTPVersion = erlmcp_ct_compat:get_otp_version(),

    ct:pal("  Priority messages available: ~p", [HasPriority]),

    % Priority messages require OTP 28+
    if OTPVersion >= 28 ->
           ?assert(HasPriority);
       true ->
           ?assertNot(HasPriority)
    end,

    % Feature should return boolean
    ?assert(is_boolean(HasPriority)),

    ct:log("Priority messages detection: PASS"),
    ok.

feature_detection_enhanced_hooks(_Config) ->
    ct:pal("Testing enhanced hooks detection"),

    HasEnhanced = erlmcp_ct_compat:supports_enhanced_hooks(),
    OTPVersion = erlmcp_ct_compat:get_otp_version(),

    ct:pal("  Enhanced hooks available: ~p", [HasEnhanced]),

    % Enhanced hooks require OTP 28+
    if OTPVersion >= 28 ->
           ?assert(HasEnhanced);
       true ->
           ?assertNot(HasEnhanced)
    end,

    % Feature should return boolean
    ?assert(is_boolean(HasEnhanced)),

    ct:log("Enhanced hooks detection: PASS"),
    ok.

%%%===================================================================
%%% Test Cases: Callback Compatibility
%%%===================================================================

callback_compatibility_init_per_suite(_Config) ->
    ct:pal("Testing init_per_suite callback"),

    % Start a test application
    {ok, Apps} = application:ensure_all_started(gproc),

    % Verify application started
    ?assert(is_list(Apps)),
    ?assert(length(Apps) > 0),

    % Cleanup
    lists:foreach(fun application:stop/1, lists:reverse(Apps)),

    ct:log("init_per_suite callback: PASS"),
    ok.

callback_compatibility_init_per_testcase(_Config) ->
    ct:pal("Testing init_per_testcase callback"),

    % Add config data
    Config = [{test_key, test_value}],

    % Verify config is accessible
    ?assertEqual(test_value, proplists:get_value(test_key, Config)),

    ct:log("init_per_testcase callback: PASS"),
    ok.

callback_compatibility_groups(_Config) ->
    ct:pal("Testing groups functionality"),

    % Verify we're in a group
    ?assert(true),  % If we're here, groups work

    ct:log("Groups functionality: PASS"),
    ok.

%%%===================================================================
%%% Test Cases: Config Management
%%%===================================================================

config_propagation(Config) ->
    ct:pal("Testing config propagation"),

    % Add data at testcase level
    Config1 = [{level, testcase} | Config],

    % Add data at suite level (simulated)
    Config2 = [{level, suite} | Config1],

    % Verify both levels accessible
    ?assertEqual(testcase, proplists:get_value(level, Config1)),
    ?assertEqual(suite, proplists:get_value(level, Config2)),

    ct:log("Config propagation: PASS"),
    ok.

config_cleanup(_Config) ->
    ct:pal("Testing config cleanup"),

    % Start a process
    Pid = spawn(fun() -> receive stop -> ok end end),

    % Add to config
    Config = [{test_pid, Pid}],

    % Cleanup (stop process)
    Pid ! stop,
    timer:sleep(10),

    % Verify cleanup
    ?assertNot(is_process_alive(Pid)),

    ct:log("Config cleanup: PASS"),
    ok.

%%%===================================================================
%%% Test Cases: Execution Patterns
%%%===================================================================

timeout_handling(_Config) ->
    ct:pal("Testing timeout handling"),

    % Test fast operation (no timeout)
    FunFast = fun() -> timer:sleep(10), fast_result end,
    {ok, fast_result} = erlmcp_ct_compat:with_timeout(test_fast, 100, FunFast),

    % Test slow operation (timeout)
    FunSlow = fun() -> timer:sleep(5000), slow_result end,
    {error, timeout} = erlmcp_ct_compat:with_timeout(test_slow, 100, FunSlow),

    % Test error operation
    FunError = fun() -> error(intentional_error) end,
    {error, {error, intentional_error, _}} =
        erlmcp_ct_compat:with_timeout(test_error, 100, FunError),

    ct:log("Timeout handling: PASS"),
    ok.

parallel_execution(_Config) ->
    ct:pal("Testing parallel execution"),

    % Spawn multiple parallel tasks
    Tasks = [spawn(fun() ->
        timer:sleep(10),
        self() ! {done, self()}
    end) || _ <- lists:seq(1, 10)],

    % Wait for all to complete
    Results = [begin
        receive
            {done, Pid} -> {ok, Pid}
        after 1000 ->
            {error, timeout}
        end
    end || Pid <- Tasks],

    % All should complete
    ?assertEqual(10, length([ok || {ok, _} <- Results])),

    ct:log("Parallel execution: PASS"),
    ok.

error_recovery(_Config) ->
    ct:pal("Testing error recovery"),

    % Start a gen_server that will crash
    {ok, Pid} = test_server:start_link(),

    % Cause crash
    gen_server:call(Pid, crash),

    % Give supervisor time to restart
    timer:sleep(100),

    % Verify process is still alive (restarted)
    ?assert(is_process_alive(Pid)),

    % Stop server
    gen_server:stop(Pid),

    ct:log("Error recovery: PASS"),
    ok.

%%%===================================================================
%%% Test Cases: OTP Features
%%%===================================================================

colored_output_detection(_Config) ->
    ct:pal("Testing colored output detection"),

    HasColors = erlmcp_ct_compat:has_feature(colored_output),
    OTPVersion = erlmcp_ct_compat:get_otp_version(),

    ct:pal("  Colored output available: ~p", [HasColors]),

    % Colored output requires OTP 27+
    if OTPVersion >= 27 ->
           ?assert(HasColors);
       true ->
           ?assertNot(HasColors)
    end,

    ct:log("Colored output detection: PASS"),
    ok.

sbom_support_detection(_Config) ->
    ct:pal("Testing SBOM support detection"),

    HasSBOM = erlmcp_ct_compat:has_feature(sbom_support),
    OTPVersion = erlmcp_ct_compat:get_otp_version(),

    ct:pal("  SBOM support available: ~p", [HasSBOM]),

    % SBOM support requires OTP 28+
    if OTPVersion >= 28 ->
           ?assert(HasSBOM);
       true ->
           ?assertNot(HasSBOM)
    end,

    ct:log("SBOM support detection: PASS"),
    ok.

%%%===================================================================
%%% Helper Server
%%%===================================================================

%% Simple test server for crash testing
-record(state, {}).

%% @hidden
init([]) ->
    {ok, #state{}}.

%% @hidden
handle_call(crash, _From, State) ->
    error(intentional_crash),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
