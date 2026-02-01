-module(erlmcp_code_reload_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    % Start reload system
    {ok, _Pid} = erlmcp_reload_sup:start_link(),
    ok.

cleanup(_) ->
    % Stop reload system - use supervisor shutdown
    case whereis(erlmcp_reload_sup) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid, normal, 5000)
    end,
    % Ensure processes are cleaned up
    timer:sleep(100),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

code_reload_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"Reload system starts and stops", fun test_reload_system_lifecycle/0},
      {"Successful module reload", fun test_successful_reload/0},
      {"Reload with validation failure", fun test_validation_failure/0},
      {"Reload with rollback", fun test_rollback_on_failure/0},
      {"Connection draining during reload", fun test_connection_draining/0},
      {"Multiple module reload", fun test_multiple_module_reload/0},
      {"Reload history tracking", fun test_reload_history/0},
      {"Module version retrieval", fun test_module_version_retrieval/0}]}.

%%====================================================================
%% Individual Tests
%%====================================================================

test_reload_system_lifecycle() ->
    % Verify reload supervisor is running
    ?assertNotEqual(undefined, whereis(erlmcp_reload_sup)),
    % Verify code reload gen_server is running
    ?assertNotEqual(undefined, whereis(erlmcp_code_reload)),
    % Verify graceful drain service is running
    ?assertNotEqual(undefined, whereis(erlmcp_graceful_drain)).

test_successful_reload() ->
    % Test reloading a simple module
    Module = erlmcp_json_rpc,  % Use existing module

    % Validate module first
    ?assertEqual(ok, erlmcp_code_reload:validate_module(Module)),

    % Perform reload with minimal options (disable expensive checks)
    Opts = #{drain_connections => false, rollback_window_s => 1},

    Result = erlmcp_code_reload:reload_module(Module, Opts),

    % Should succeed
    ?assertMatch({ok, _OldVsn, _NewVsn}, Result),

    % Module should still be loaded
    ?assertMatch({file, _}, code:is_loaded(Module)).

test_validation_failure() ->
    % Test validation of non-existent module
    FakeModule = fake_nonexistent_module,

    Result = erlmcp_code_reload:validate_module(FakeModule),

    % Should fail with module_not_loaded
    ?assertEqual({error, module_not_loaded}, Result).

test_rollback_on_failure() ->
    % This test verifies rollback mechanism when smoke tests fail
    Module = erlmcp_json_rpc,

    % Get current version (stored for potential rollback verification)
    _OldVsn = get_module_version(Module),

    % Perform reload with smoke test that will fail
    FailingSmokeTest = fun() -> {error, intentional_failure} end,

    Opts =
        #{drain_connections => false,
          smoke_tests => [FailingSmokeTest],
          rollback_window_s => 1},

    Result = erlmcp_code_reload:reload_module(Module, Opts),

    % Should fail with smoke test error
    ?assertMatch({error, {smoke_test_failed, _}}, Result),

    % Module should still be functional
    ?assertMatch({file, _}, code:is_loaded(Module)),

    % Version should still exist (not undefined)
    CurrentVsn = get_module_version(Module),
    ?assertNotEqual(undefined, CurrentVsn).

test_connection_draining() ->
    Module = erlmcp_json_rpc,

    % Check drain service is available
    DrainPid = whereis(erlmcp_graceful_drain),
    ?assert(is_pid(DrainPid)),

    % Initially not draining
    ?assertEqual(false, erlmcp_graceful_drain:is_draining(Module)),

    % Test draining
    ?assertEqual(ok, erlmcp_graceful_drain:drain_module(Module, 1000)),

    % Module should be draining
    ?assertEqual(true, erlmcp_graceful_drain:is_draining(Module)),

    % Resume module
    ?assertEqual(ok, erlmcp_graceful_drain:resume_module(Module)),

    % Should no longer be draining
    ?assertEqual(false, erlmcp_graceful_drain:is_draining(Module)).

test_multiple_module_reload() ->
    % Test reloading multiple modules in dependency order
    Modules = [erlmcp_json_rpc, erlmcp_registry],

    Opts = #{drain_connections => false, rollback_window_s => 1},

    Results = erlmcp_code_reload:reload_modules(Modules, Opts),

    % Should have results for each module
    ?assertEqual(length(Modules), length(Results)),

    % All should succeed
    lists:foreach(fun({Mod, Result}) ->
                     ?assertMatch({ok, _OldVsn, _NewVsn}, Result),
                     ?assertMatch({file, _}, code:is_loaded(Mod))
                  end,
                  Results).

test_reload_history() ->
    Module = erlmcp_json_rpc,

    % Get initial history
    InitialHistory = erlmcp_code_reload:get_reload_history(),
    InitialCount = length(InitialHistory),

    % Perform a reload
    Opts = #{drain_connections => false, rollback_window_s => 1},

    {ok, _OldVsn, _NewVsn} = erlmcp_code_reload:reload_module(Module, Opts),

    % Check history updated
    NewHistory = erlmcp_code_reload:get_reload_history(),
    ?assertEqual(InitialCount + 1, length(NewHistory)),

    % Latest entry should be for our module
    [Latest | _] = NewHistory,
    ?assertEqual(Module, maps:get(module, Latest)),
    ?assertEqual(ok, maps:get(result, Latest)).

test_module_version_retrieval() ->
    % Test version retrieval for a loaded module
    Module = erlmcp_json_rpc,

    % Module should be loaded
    ?assertMatch({file, _}, code:is_loaded(Module)),

    % Should get a version (not undefined)
    Version = get_module_version(Module),
    ?assertNotEqual(undefined, Version),

    % Test with non-existent module
    FakeModule = fake_nonexistent_module,
    ?assertEqual(undefined, get_module_version(FakeModule)).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Get module version, handling cover_compiled modules
get_module_version(Module) ->
    case code:which(Module) of
        non_existing ->
            undefined;
        BeamPath when is_list(BeamPath) ->
            % Check if this is a cover_compiled path
            case string:find(BeamPath, "cover_compiled") of
                nomatch ->
                    case beam_lib:version(BeamPath) of
                        {ok, {Module, Version}} ->
                            Version;
                        _ ->
                            undefined
                    end;
                _ ->
                    % For cover_compiled modules, try to get the original beam path
                    case code:get_object_code(Module) of
                        {_Module, _Binary, Filename} when is_list(Filename) ->
                            case beam_lib:version(Filename) of
                                {ok, {Module, Version}} ->
                                    Version;
                                _ ->
                                    undefined
                            end;
                        _ ->
                            undefined
                    end
            end
    end.
