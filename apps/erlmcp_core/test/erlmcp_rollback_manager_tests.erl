-module(erlmcp_rollback_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test fixtures
-define(TEST_MODULE, test_rollback_module).
-define(TEST_VERSION_1, <<"v1.0.0">>).
-define(TEST_VERSION_2, <<"v1.1.0">>).
-define(TEST_VERSION_3, <<"v1.2.0">>).

%%%====================================================================
%%% Test Setup and Teardown
%%%====================================================================

rollback_manager_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"save and retrieve version", fun test_save_version/0},
      {"multiple version history", fun test_version_history/0},
      {"rollback one step", fun test_rollback_one_step/0},
      {"rollback multiple steps", fun test_rollback_multiple_steps/0},
      {"rollback to specific version", fun test_rollback_to_version/0},
      {"version history limit", fun test_version_history_limit/0},
      {"get current version", fun test_get_current_version/0},
      {"clear history", fun test_clear_history/0},
      {"rollback info", fun test_rollback_info/0},
      {"rollback not enough history", fun test_rollback_not_enough_history/0},
      {"rollback module not found", fun test_rollback_module_not_found/0}
     ]}.

setup() ->
    %% Start rollback manager
    {ok, Pid} = erlmcp_rollback_manager:start_link(),
    Pid.

cleanup(_Pid) ->
    %% Stop rollback manager
    gen_server:stop(erlmcp_rollback_manager),
    ok.

%%%====================================================================
%%% Test Cases
%%%====================================================================

%% @doc Test saving and retrieving a single version
test_save_version() ->
    %% Create a test module
    create_test_module(),

    %% Save version
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_1)),

    %% Verify current version
    ?assertEqual({ok, ?TEST_VERSION_1}, erlmcp_rollback_manager:get_current_version(?TEST_MODULE)),

    %% Cleanup
    cleanup_test_module().

%% @doc test multiple version history
test_version_history() ->
    create_test_module(),

    %% Save multiple versions
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_1)),
    timer:sleep(10),  %% Ensure different timestamps
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_2)),
    timer:sleep(10),
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_3)),

    %% Get version history
    History = erlmcp_rollback_manager:version_history(?TEST_MODULE),

    %% Should have 3 versions, most recent first
    ?assertEqual(3, length(History)),
    ?assertEqual(?TEST_VERSION_3, element(1, lists:nth(1, History))),
    ?assertEqual(?TEST_VERSION_2, element(1, lists:nth(2, History))),
    ?assertEqual(?TEST_VERSION_1, element(1, lists:nth(3, History))),

    cleanup_test_module().

%% @doc test rollback one step
test_rollback_one_step() ->
    create_test_module(),

    %% Save versions
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_1)),
    timer:sleep(10),
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_2)),

    %% Rollback one step (should go back to v1)
    ?assertEqual(ok, erlmcp_rollback_manager:rollback(?TEST_MODULE, 1)),

    %% Verify current version is v1
    ?assertEqual({ok, ?TEST_VERSION_1}, erlmcp_rollback_manager:get_current_version(?TEST_MODULE)),

    cleanup_test_module().

%% @doc test rollback multiple steps
test_rollback_multiple_steps() ->
    create_test_module(),

    %% Save versions
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_1)),
    timer:sleep(10),
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_2)),
    timer:sleep(10),
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_3)),

    %% Rollback 2 steps (should go back to v1)
    ?assertEqual(ok, erlmcp_rollback_manager:rollback(?TEST_MODULE, 2)),

    %% Verify current version is v1
    ?assertEqual({ok, ?TEST_VERSION_1}, erlmcp_rollback_manager:get_current_version(?TEST_MODULE)),

    cleanup_test_module().

%% @doc test rollback to specific version
test_rollback_to_version() ->
    create_test_module(),

    %% Save versions
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_1)),
    timer:sleep(10),
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_2)),
    timer:sleep(10),
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_3)),

    %% Rollback to v2
    ?assertEqual(ok, erlmcp_rollback_manager:rollback_to_version(?TEST_MODULE, ?TEST_VERSION_2)),

    %% Verify current version is v2
    ?assertEqual({ok, ?TEST_VERSION_2}, erlmcp_rollback_manager:get_current_version(?TEST_MODULE)),

    cleanup_test_module().

%% @doc test version history limit (default 10 versions)
test_version_history_limit() ->
    create_test_module(),

    %% Save more than max versions (default max is 10)
    MaxVersions = 10,
    lists:foreach(fun(N) ->
        Version = list_to_binary(io_lib:format("v~p.0.0", [N])),
        ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, Version)),
        timer:sleep(5)
    end, lists:seq(1, MaxVersions + 5)),

    %% Get history - should only have max versions
    History = erlmcp_rollback_manager:version_history(?TEST_MODULE),
    ?assertEqual(MaxVersions, length(History)),

    cleanup_test_module().

%% @doc test get current version
test_get_current_version() ->
    create_test_module(),

    %% No version saved initially
    ?assertEqual({error, not_found}, erlmcp_rollback_manager:get_current_version(?TEST_MODULE)),

    %% Save version
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_1)),

    %% Should get current version
    ?assertEqual({ok, ?TEST_VERSION_1}, erlmcp_rollback_manager:get_current_version(?TEST_MODULE)),

    cleanup_test_module().

%% @doc test clear history
test_clear_history() ->
    create_test_module(),

    %% Save versions
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_1)),
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_2)),

    %% Clear history
    ?assertEqual(ok, erlmcp_rollback_manager:clear_history(?TEST_MODULE)),

    %% Verify history is cleared
    ?assertEqual([], erlmcp_rollback_manager:version_history(?TEST_MODULE)),
    ?assertEqual({error, not_found}, erlmcp_rollback_manager:get_current_version(?TEST_MODULE)),

    cleanup_test_module().

%% @doc test rollback info
test_rollback_info() ->
    create_test_module(),

    %% Save versions
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_1)),
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_2)),

    %% Get rollback info
    Info = erlmcp_rollback_manager:rollback_info(),

    %% Verify info structure
    ?assert(maps:is_key(total_versions, Info)),
    ?assert(maps:is_key(module_count, Info)),
    ?assert(maps:is_key(max_versions_per_module, Info)),
    ?assert(maps:is_key(version_counts, Info)),
    ?assert(maps:is_key(current_versions, Info)),

    ?assertEqual(2, maps:get(total_versions, Info)),
    ?assertEqual(1, maps:get(module_count, Info)),
    ?assertEqual(10, maps:get(max_versions_per_module, Info)),

    cleanup_test_module().

%% @doc test rollback with not enough history
test_rollback_not_enough_history() ->
    create_test_module(),

    %% Save only one version
    ?assertEqual(ok, erlmcp_rollback_manager:save_version(?TEST_MODULE, ?TEST_VERSION_1)),

    %% Try to rollback 2 steps (should fail)
    ?assertEqual({error, not_enough_history}, erlmcp_rollback_manager:rollback(?TEST_MODULE, 2)),

    cleanup_test_module().

%% @doc test rollback for module not found
test_rollback_module_not_found() ->
    %% Try to rollback non-existent module
    ?assertEqual({error, module_not_found}, erlmcp_rollback_manager:rollback(nonexistent_module, 1)).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Create a test module for rollback testing
create_test_module() ->
    %% Create a simple test module
    Code = "
        -module(test_rollback_module).

        -export([test_func/0]).

        test_func() ->
            hello.
    ",

    %% Compile and load the module
    case compile:forms(Code, [binary]) of
        {ok, ModuleName, Binary} ->
            case code:load_binary(ModuleName, "test_rollback_module.erl", Binary) of
                {module, ModuleName} ->
                    ok;
                {error, Reason} ->
                    error({load_failed, Reason})
            end;
        {error, Reason, _} ->
            error({compile_failed, Reason})
    end.

%% @doc Cleanup test module
cleanup_test_module() ->
    %% Clear history
    erlmcp_rollback_manager:clear_history(?TEST_MODULE),

    %% Purge and delete module
    code:purge(?TEST_MODULE),
    code:delete(?TEST_MODULE),
    ok.
