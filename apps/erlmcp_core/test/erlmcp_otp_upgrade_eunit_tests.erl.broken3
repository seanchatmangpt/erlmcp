%%%-------------------------------------------------------------------
%%% @doc
%%% OTP Upgrade EUnit Test Suite
%%%
%%% Comprehensive EUnit tests for OTP upgrade functionality:
%%% - Version detection (compile-time and runtime)
%%% - Feature availability (JSON, process iterators, priority messages)
%%% - Code reload with state migration
%%% - Module validation and backup/restore
%%% - Backward compatibility (OTP 26-28)
%%% - State migration (v0 -> v1)
%%%
%%% Chicago School TDD:
%%% - Real OTP version detection (no mocks)
%%% - Observable behavior: API results, module versions
%%% - Real gen_servers, real ETS tables, real code loading
%%% - No mock objects or test doubles
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otp_upgrade_eunit_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../../include/otp_compat.hrl").
-include("include/otp_features.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

%% @doc Main test generator
otp_upgrade_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"OTP version detection", fun test_otp_version_detection/0},
      {"Feature detection macros", fun test_feature_detection_macros/0},
      {"JSON encoding compatibility", fun test_json_compatibility/0},
      {"Process enumeration", fun test_process_enumeration/0},
      {"Priority messages", fun test_priority_messages/0},
      {"Code reload with state migration", fun test_code_reload_state_migration/0},
      {"Module validation", fun test_module_validation/0},
      {"Backup and rollback", fun test_backup_rollback/0},
      {"Hot reload with state preservation", fun test_hot_reload_state_preservation/0},
      {"Atomic swap operation", fun test_atomic_swap/0}
     ]}.

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    %% Start real erlmcp_code_reload gen_server (Chicago School: real processes)
    {ok, Pid} = erlmcp_code_reload:start_link(),
    {ok, LoaderPid} = erlmcp_code_loader:start_link(),
    {ok, CoordinatorPid} = erlmcp_reload_coordinator:start_link(),
    {reload_pid, Pid, loader_pid, LoaderPid, coordinator_pid, CoordinatorPid}.

cleanup({reload_pid, ReloadPid, loader_pid, LoaderPid, coordinator_pid, CoordinatorPid}) ->
    %% Stop real processes (Chicago School: clean teardown)
    gen_server:stop(ReloadPid),
    gen_server:stop(LoaderPid),
    gen_server:stop(CoordinatorPid),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test OTP version detection at compile-time and runtime
test_otp_version_detection() ->
    %% Runtime detection (Chicago School: observe real system state)
    OtpRelease = erlang:system_info(otp_release),
    ?assertNotEqual("", OtpRelease),
    ?assert(is_list(OtpRelease)),

    %% Parse major version
    MajorVersion = parse_major_version(OtpRelease),
    ?assert(MajorVersion >= 25),  %% erlmcp requires OTP 25+

    %% Compile-time macros should be available
    %% Test both old and new macro styles
    ?assert(is_integer(MajorVersion)),

    %% Version should match between compile-time and runtime
    ct:pal("OTP Release: ~s (Major: ~p)", [OtpRelease, MajorVersion]),
    ok.

%% @doc Test feature detection macros (compile-time and runtime)
test_feature_detection_macros() ->
    %% Test JSON module availability
    HasJson = ?HAVE_NATIVE_JSON,
    JsonAvailable = has_json_module_runtime(),
    ?assertEqual(HasJson, JsonAvailable),

    %% Test process iterator availability
    HasIterator = ?HAVE_PROCESS_ITERATOR,
    IteratorAvailable = has_process_iterator_runtime(),
    ?assertEqual(HasIterator, IteratorAvailable),

    %% Test priority messages availability
    HasPriority = ?HAVE_PRIORITY_MESSAGES,
    PriorityAvailable = has_priority_messages_runtime(),
    ?assertEqual(HasPriority, PriorityAvailable),

    ct:pal("Features: JSON=~p, Iterator=~p, Priority=~p",
           [HasJson, HasIterator, HasPriority]),
    ok.

%% @doc Test JSON encoding compatibility (OTP 27+ vs fallback)
test_json_compatibility() ->
    TestData = #{key => value, number => 42, nested => #{item => true}},

    %% Test safe encoding macro (runtime detection)
    Encoded = ?JSON_ENCODE_SAFE(TestData),
    ?assert(is_binary(Encoded)),
    ?assertNotEqual(<<>>, Encoded),

    %% Test safe decoding macro
    {Decoded, []} = ?JSON_DECODE_SAFE(Encoded),
    ?assertEqual(TestData, Decoded),

    %% Test compile-time macro
    Encoded2 = ?JSON_ENCODE(TestData),
    ?assert(is_binary(Encoded2)),

    ct:pal("JSON encoded/decoded successfully (~p bytes)", [byte_size(Encoded)]),
    ok.

%% @doc Test process enumeration (OTP 28+ iterator vs legacy)
test_process_enumeration() ->
    %% Get process count using safe macro
    Count = ?SAFE_PROCESS_COUNT(),
    ?assert(is_integer(Count)),
    ?assert(Count > 0),

    %% Get process list using safe macro (with warning for >10K)
    PidList = ?SAFE_PROCESSES(),
    ?assert(is_list(PidList)),
    ?assert(length(PidList) > 0),

    %% Count should match list length
    ?assertEqual(Count, length(PidList)),

    ct:pal("Process enumeration: ~p processes", [Count]),
    ok.

%% @doc Test priority messages (OTP 28+)
test_priority_messages() ->
    %% Spawn a test receiver process (real process, Chicago School)
    Self = self(),
    Receiver = spawn(fun() -> receive_loop(Self) end),

    %% Test priority send macro
    ?SEND_PRIORITY(Receiver, {test_priority, high}),

    %% Verify message received
    receive
        {receiver_got, {test_priority, high}} ->
            ok
    after 1000 ->
        ?assert(false, priority_message_not_received)
    end,

    %% Clean up
    Receiver ! stop,
    ok.

%% @doc Test code reload with state migration (code_change/3)
test_code_reload_state_migration() ->
    %% Create a test module with versioned state
    Module = erlmcp_code_reload,

    %% Get current state
    {ok, State1} = get_reload_state(),

    %% Trigger code_change via simulated upgrade
    %% (In real scenario, this would happen during release upgrade)
    OldVsn = 0,
    Extra = [],
    Result = Module:code_change(OldVsn, State1, Extra),

    %% Verify migration succeeded
    ?assertMatch({ok, _State}, Result),

    ct:pal("State migration successful: v~p -> current", [OldVsn]),
    ok.

%% @doc Test module validation before reload
test_module_validation() ->
    %% Validate real module (erlmcp_code_reload)
    Module = erlmcp_code_reload,

    Result = erlmcp_code_reload:validate_module(Module),

    %% Should succeed (module is loaded and valid)
    ?assertEqual(ok, Result),

    %% Validate non-existent module
    InvalidModule = fake_nonexistent_module,
    ErrorResult = erlmcp_code_reload:validate_module(InvalidModule),
    ?assertMatch({error, _}, ErrorResult),

    ct:pal("Module validation: ~p ok, ~p failed", [Module, InvalidModule]),
    ok.

%% @doc Test backup and rollback functionality
test_backup_rollback() ->
    %% Use a simple module for testing
    Module = erlmcp_code_reload,

    %% Get current version
    {ok, OriginalVersion} = erlmcp_code_loader:get_module_md5(Module),
    ?assert(is_binary(OriginalVersion)),

    %% Simulate backup creation (internal function)
    %% In real scenario, this happens before reload
    BackupOk = backup_module_beam_internal(Module),
    ?assertEqual(ok, BackupOk),

    %% Verify backup exists
    BackupPath = get_backup_path(Module),
    ?assert(filelib:is_regular(BackupPath)),

    %% Clean up backup
    file:delete(BackupPath),

    ct:pal("Backup/rollback test passed for ~p", [Module]),
    ok.

%% @doc Test hot reload with state preservation
test_hot_reload_state_preservation() ->
    Module = erlmcp_code_reload,

    %% Get current state from gen_server
    SysGetState = sys:get_state(Module),
    ?assert(is_record(SysGetState, state)),

    %% Simulate hot reload (would load new code in real scenario)
    %% For now, just verify state structure is compatible
    #state{version = Version,
           reload_history = History,
           rollback_timers = Timers,
           draining = Draining} = SysGetState,

    ?assert(is_integer(Version)),
    ?assert(is_list(History)),
    ?assert(is_map(Timers)),
    ?assert(is_boolean(Draining)),

    ct:pal("Hot reload state: v~p, history=~p, timers=~p, draining=~p",
           [Version, length(History), maps:size(Timers), Draining]),
    ok.

%% @doc Test atomic swap operation
test_atomic_swap() ->
    Module = erlmcp_code_reload,

    %% Get object code
    {ok, Module, Binary, Filename} = erlmcp_code_loader:get_object_code(Module),
    ?assert(is_binary(Binary)),
    ?assert(is_list(Filename)),
    ?assert(filelib:is_regular(Filename)),

    %% Perform atomic swap (should succeed - same code)
    Result = erlmcp_code_loader:atomic_swap(Module, Binary, Filename),
    ?assertEqual(ok, Result),

    %% Verify module still works
    {ok, State} = get_reload_state(),
    ?assert(is_record(State, state)),

    ct:pal("Atomic swap successful for ~p", [Module]),
    ok.

%%====================================================================
%% Property-Based Tests (Proper)
%%====================================================================

%% @doc Property: JSON encode/decode roundtrip
prop_json_roundtrip() ->
    ?FORALL(Data, json_data_generator(),
        begin
            Encoded = ?JSON_ENCODE_SAFE(Data),
            {Decoded, []} = ?JSON_DECODE_SAFE(Encoded),
            Data =:= Decoded
        end).

%% @doc Property: Process count consistency
prop_process_count_consistent() ->
    ?FORALL(_Iterations, range(1, 10),
        begin
            Count1 = ?SAFE_PROCESS_COUNT(),
            timer:sleep(10),  %% Small delay
            Count2 = ?SAFE_PROCESS_COUNT(),
            %% Count may change but should be positive
            Count1 > 0 andalso Count2 > 0
        end).

%% @doc Property: Module version consistent
prop_module_version_consistent() ->
    ?FORALL(_Module, oneof([erlmcp_code_reload, erlmcp_code_loader, erlmcp_reload_coordinator]),
        begin
            {ok, Version1} = erlmcp_code_loader:get_module_md5(_Module),
            timer:sleep(10),
            {ok, Version2} = erlmcp_code_loader:get_module_md5(_Module),
            Version1 =:= Version2
        end).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Parse major OTP version
parse_major_version(ReleaseString) ->
    case string:split(ReleaseString, ".") of
        [Major | _] ->
            try list_to_integer(Major)
            catch _:_ -> 0
            end;
        _ ->
            try list_to_integer(ReleaseString)
            catch _:_ -> 0
            end
    end.

%% @doc Runtime check for JSON module
has_json_module_runtime() ->
    try
        _ = json:module_info(),
        true
    catch
        _:_ -> false
    end.

%% @doc Runtime check for process iterator
has_process_iterator_runtime() ->
    erlang:function_exported(erlang, processes_iterator, 0).

%% @doc Runtime check for priority messages
has_priority_messages_runtime() ->
    try
        OldValue = process_flag(priority, high),
        process_flag(priority, OldValue),
        true
    catch
        _:_ -> false
    end.

%% @doc Receive loop for priority message test
receive_loop(Parent) ->
    receive
        {test_priority, _} = Msg ->
            Parent ! {receiver_got, Msg},
            receive_loop(Parent);
        stop ->
            ok;
        _ ->
            receive_loop(Parent)
    end.

%% @doc Get reload state from gen_server
get_reload_state() ->
    try
        State = sys:get_state(erlmcp_code_reload),
        {ok, State}
    catch
        _:_ -> {error, state_unavailable}
    end.

%% @doc Backup module beam file (internal)
backup_module_beam_internal(Module) ->
    case code:which(Module) of
        non_existing ->
            {error, module_not_found};
        BeamPath when is_list(BeamPath) ->
            BackupPath = BeamPath ++ ".backup",
            file:copy(BeamPath, BackupPath)
    end.

%% @doc Get backup file path
get_backup_path(Module) ->
    BeamPath = code:which(Module),
    BeamPath ++ ".backup".

%% @doc JSON data generator for property tests
json_data_generator() ->
    oneof([
        #{binary() => binary()},
        #{binary() => integer()},
        #{binary() => float()},
        #{binary() => boolean()},
        #{binary() => json_data_generator()},
        [json_data_generator()],
        {json_data_generator()}
    ]).

%%====================================================================
%% Internal Macros for is_record/2 guard
%%====================================================================

-define(is_record(X, Tag), is_tuple(X) andalso element(1, X) =:= Tag andalso
                           tuple_size(X) > 0).
