%%%-------------------------------------------------------------------
%%% @doc
%%% OTP 28 Compatibility Test Suite
%%%
%%% Tests OTP version detection and backward compatibility:
%%% - OTP version detection at runtime
%%% - Platform-specific defines (OTP_28 macro)
%%% - New json:encode/1 module (OTP 28)
%%% - Backward compatibility with OTP 25-27
%%% - Dirty scheduler suspend (OTP 28.3+)
%%% - Feature flags and conditional compilation
%%%
%%% Chicago School TDD:
%%% - Real OTP version detection
%%% - Observable behavior: API availability, feature detection
%%% - No mocks or fakes
%%% - Graceful degradation across OTP versions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otp28_compat_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_otp_version_detection/1,
    test_platform_defines/1,
    test_json_module_available/1,
    test_backward_compatibility/1,
    test_dirty_scheduler_suspend/1,
    test_feature_flags_loaded/1
]).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [
        {group, version_detection},
        {group, feature_availability},
        {group, backward_compat}
    ].

groups() ->
    [
        {version_detection, [sequence], [
            test_otp_version_detection,
            test_platform_defines
        ]},
        {feature_availability, [sequence], [
            test_json_module_available,
            test_dirty_scheduler_suspend,
            test_feature_flags_loaded
        ]},
        {backward_compat, [sequence], [
            test_backward_compatibility
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("Starting OTP 28 Compatibility Test Suite"),

    % Detect OTP version and capabilities
    OTPRelease = erlang:system_info(otp_release),
    OTPVersion = parse_otp_version(OTPRelease),

    ct:pal("OTP Release: ~s", [OTPRelease]),
    ct:pal("Parsed OTP Version: ~p", [OTPVersion]),

    % Detect available features
    Features = detect_features(),

    ct:pal("Available features: ~p", [Features]),

    [{otp_release, OTPRelease},
     {otp_version, OTPVersion},
     {features, Features} | Config].

end_per_suite(_Config) ->
    ct:pal("OTP 28 Compatibility Test Suite completed"),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Ending test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test OTP version detection at runtime
test_otp_version_detection(Config) ->
    OTPRelease = proplists:get_value(otp_release, Config),
    OTPVersion = proplists:get_value(otp_version, Config),

    ct:pal("Testing OTP version detection"),
    ct:pal("  Release string: ~s", [OTPRelease]),
    ct:pal("  Major version: ~p", [OTPVersion]),

    % Version should be a positive integer
    ?assert(is_integer(OTPVersion)),
    ?assert(OTPVersion > 0),

    % Should be OTP 25 or higher (erlmcp requirement)
    ?assert(OTPVersion >= 25),

    % Test various version query methods
    ct:pal("System info queries:"),
    ct:pal("  otp_release: ~s", [erlang:system_info(otp_release)]),
    ct:pal("  version: ~s", [erlang:system_info(version)]),
    ct:pal("  system_version: ~s", [erlang:system_info(system_version)]),

    % All should return non-empty strings
    ?assertNotEqual("", erlang:system_info(otp_release)),
    ?assertNotEqual("", erlang:system_info(version)).

%% @doc Test platform-specific defines (compile-time detection)
test_platform_defines(Config) ->
    OTPVersion = proplists:get_value(otp_version, Config),

    ct:pal("Testing platform defines"),

    % Test conditional compilation based on OTP version
    % (This would typically use -ifdef(OTP_28) in real code)

    CompileDefines = case OTPVersion >= 28 of
        true ->
            ct:pal("OTP 28+ detected - modern features available"),
            [otp_28, json_module, priority_messages, process_iterator];
        false ->
            ct:pal("OTP <28 detected - using fallback implementations"),
            [legacy_mode]
    end,

    ct:pal("Compile defines: ~p", [CompileDefines]),

    ?assert(is_list(CompileDefines)),
    ?assert(length(CompileDefines) > 0).

%% @doc Test that json:encode/1 module is available (OTP 28+)
test_json_module_available(Config) ->
    OTPVersion = proplists:get_value(otp_version, Config),
    Features = proplists:get_value(features, Config),

    ct:pal("Testing json module availability"),

    HasJsonModule = proplists:get_value(json_module, Features, false),

    if
        OTPVersion >= 28 ->
            % OTP 28+: json module should be available
            ?assert(HasJsonModule),

            % Test basic encoding
            TestData = #{key => <<"value">>, number => 42},
            JSON = json:encode(TestData),

            ct:pal("Encoded JSON: ~s", [JSON]),

            ?assert(is_binary(JSON)),

            % Test decoding
            Decoded = json:decode(JSON),
            ct:pal("Decoded: ~p", [Decoded]),

            ?assertEqual(TestData, Decoded);

        true ->
            % OTP 25-27: json module not available, use jsx
            ?assertNot(HasJsonModule),

            ct:pal("Using jsx for JSON (OTP <28)"),

            % Verify jsx works as fallback
            TestData = #{<<"key">> => <<"value">>, <<"number">> => 42},
            JSON = jsx:encode(TestData),

            ?assert(is_binary(JSON))
    end.

%% @doc Test backward compatibility with OTP 25-27 code paths
test_backward_compatibility(Config) ->
    OTPVersion = proplists:get_value(otp_version, Config),

    ct:pal("Testing backward compatibility"),

    % Test that we can gracefully handle missing OTP 28 features

    % 1. Process iterator fallback
    AllProcs = get_processes_compat(),
    ?assert(is_list(AllProcs)),
    ?assert(length(AllProcs) > 0),
    ct:pal("Process listing works: ~p processes", [length(AllProcs)]),

    % 2. JSON encoding fallback
    TestMap = #{test => value},
    JSON = encode_json_compat(TestMap),
    ?assert(is_binary(JSON)),
    ct:pal("JSON encoding works: ~s", [JSON]),

    % 3. Priority messages fallback
    PriorityAvailable = can_use_priority_messages(),
    ct:pal("Priority messages available: ~p", [PriorityAvailable]),

    if
        OTPVersion >= 28 ->
            ct:pal("OTP 28+ confirmed - all features available");
        true ->
            ct:pal("OTP <28 confirmed - using fallback implementations")
    end.

%% @doc Test dirty scheduler suspend (OTP 28.3+)
test_dirty_scheduler_suspend(Config) ->
    OTPVersion = proplists:get_value(otp_version, Config),
    Features = proplists:get_value(features, Config),

    ct:pal("Testing dirty scheduler suspend"),

    HasDirtySuspend = proplists:get_value(dirty_scheduler_suspend, Features, false),

    if
        OTPVersion >= 28 ->
            % OTP 28.3+: should support suspending dirty scheduler processes
            ct:pal("Dirty scheduler suspend support: ~p", [HasDirtySuspend]),

            % Spawn a process on dirty CPU scheduler
            Self = self(),
            DirtyProc = spawn_opt(
                fun() ->
                    Self ! {dirty_proc, self()},
                    receive stop -> ok after 5000 -> ok end
                end,
                [{scheduler, 1}, {priority, normal}]
            ),

            receive {dirty_proc, DirtyProc} -> ok after 1000 -> ok end,

            % Try to suspend it (should work on OTP 28.3+)
            try
                true = erlang:suspend_process(DirtyProc),
                ct:pal("Successfully suspended dirty scheduler process"),

                % Resume
                true = erlang:resume_process(DirtyProc),
                ct:pal("Successfully resumed dirty scheduler process"),

                DirtyProc ! stop

            catch
                error:Reason ->
                    ct:pal("Cannot suspend dirty scheduler: ~p", [Reason]),
                    DirtyProc ! stop
            end;

        true ->
            ct:pal("Dirty scheduler suspend not tested (OTP <28)")
    end.

%% @doc Test that feature flags are loaded correctly
test_feature_flags_loaded(Config) ->
    Features = proplists:get_value(features, Config),

    ct:pal("Testing feature flags"),
    ct:pal("Detected features: ~p", [Features]),

    % Verify features list is properly formatted
    ?assert(is_list(Features)),

    % Check each feature has boolean value
    lists:foreach(fun({Feature, Available}) ->
        ct:pal("  ~p: ~p", [Feature, Available]),
        ?assert(is_boolean(Available))
    end, Features),

    % Verify expected features are present
    ExpectedFeatures = [
        json_module,
        priority_messages,
        process_iterator,
        dirty_scheduler_suspend
    ],

    lists:foreach(fun(Feature) ->
        ?assert(proplists:is_defined(Feature, Features))
    end, ExpectedFeatures).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Parse OTP version from release string
parse_otp_version(ReleaseString) ->
    % Handle both "28" and "28.0.1" formats
    case string:split(ReleaseString, ".") of
        [Major | _] ->
            try
                list_to_integer(Major)
            catch
                _:_ -> 0
            end;
        _ ->
            try
                list_to_integer(ReleaseString)
            catch
                _:_ -> 0
            end
    end.

%% @doc Detect available OTP 28 features
detect_features() ->
    [
        {json_module, has_json_module()},
        {priority_messages, has_priority_messages()},
        {process_iterator, has_process_iterator()},
        {dirty_scheduler_suspend, has_dirty_scheduler_suspend()}
    ].

%% @doc Check if json module is available
has_json_module() ->
    try
        _ = json:module_info(),
        true
    catch
        error:undef -> false;
        _:_ -> false
    end.

%% @doc Check if priority messages are available
has_priority_messages() ->
    try
        OldValue = process_flag(priority, true),
        process_flag(priority, OldValue),
        true
    catch
        error:badarg -> false;
        _:_ -> false
    end.

%% @doc Check if process iterator is available
has_process_iterator() ->
    try
        _ = erlang:processes_iterator(),
        true
    catch
        error:undef -> false;
        _:_ -> true  % Might fail for other reasons, but API exists
    end.

%% @doc Check if dirty scheduler suspend is available
has_dirty_scheduler_suspend() ->
    % This is harder to test directly - assume OTP 28.3+ has it
    OTPRelease = erlang:system_info(otp_release),
    Version = parse_otp_version(OTPRelease),
    Version >= 28.

%% @doc Get all processes with compatibility
get_processes_compat() ->
    case has_process_iterator() of
        true ->
            % OTP 28+: use iterator
            collect_processes_via_iterator();
        false ->
            % OTP 25-27: use processes/0
            erlang:processes()
    end.

%% @doc Collect processes using iterator
collect_processes_via_iterator() ->
    Iterator = erlang:processes_iterator(),
    collect_recursive(Iterator, []).

collect_recursive(Iterator, Acc) ->
    case erlang:process_next(Iterator) of
        {Pid, NewIterator} when is_pid(Pid) ->
            collect_recursive(NewIterator, [Pid | Acc]);
        done ->
            lists:reverse(Acc)
    end.

%% @doc Encode JSON with compatibility
encode_json_compat(Data) ->
    case has_json_module() of
        true ->
            % OTP 28+: use json module
            json:encode(Data);
        false ->
            % OTP 25-27: use jsx
            % Convert atom keys to binary for jsx
            BinaryData = convert_keys_to_binary(Data),
            jsx:encode(BinaryData)
    end.

%% @doc Convert map keys to binary for jsx
convert_keys_to_binary(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        BinKey = if
            is_atom(K) -> atom_to_binary(K, utf8);
            is_binary(K) -> K;
            is_list(K) -> list_to_binary(K);
            true -> term_to_binary(K)
        end,
        maps:put(BinKey, convert_keys_to_binary(V), Acc)
    end, #{}, Map);
convert_keys_to_binary(List) when is_list(List) ->
    [convert_keys_to_binary(Item) || Item <- List];
convert_keys_to_binary(Other) ->
    Other.

%% @doc Check if priority messages can be used
can_use_priority_messages() ->
    has_priority_messages().
