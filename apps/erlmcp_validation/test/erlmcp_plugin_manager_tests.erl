%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin Manager Test Suite (EUnit)
%%%
%%% Tests for erlmcp_plugin_manager module - Plugin loading and execution
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real plugin system with hot-loading
%%% - NO mocks, real plugin processes and supervision
%%% - State-based verification (plugin state, execution results)
%%%
%%% Coverage Target: ≥85%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_manager_tests).
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

plugin_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Initialize plugin manager", fun test_init_plugin_manager/0},
      {"Load plugin", fun test_load_plugin/0},
      {"Unload plugin", fun test_unload_plugin/0},
      {"List loaded plugins", fun test_list_plugins/0},
      {"Execute plugin command", fun test_execute_plugin/0},
      {"Plugin hot-reload", fun test_hot_reload_plugin/0},
      {"Plugin dependencies", fun test_plugin_dependencies/0},
      {"Plugin isolation", fun test_plugin_isolation/0},
      {"Plugin crash recovery", fun test_plugin_crash_recovery/0},
      {"Plugin versioning", fun test_plugin_versioning/0},
      {"Plugin configuration", fun test_plugin_configuration/0},
      {"Plugin hooks", fun test_plugin_hooks/0},
      {"Plugin discovery", fun test_plugin_discovery/0},
      {"Plugin validation", fun test_plugin_validation/0},
      {"Plugin sandboxing", fun test_plugin_sandboxing/0}
     ]}.

setup() ->
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_Args) ->
    ok.

%%%====================================================================
%%% Initialization Tests
%%%====================================================================

test_init_plugin_manager() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    ?assert(is_pid(Manager)),
    {ok, State} = erlmcp_plugin_manager:get_state(Manager),
    ?assertMatch(#{plugins := #{}}, State),
    ok = erlmcp_plugin_manager:stop(Manager).

%%%====================================================================
%%% Plugin Loading Tests
%%%====================================================================

test_load_plugin() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    PluginPath = "/tmp/test_plugin.erl",
    ok = create_test_plugin(PluginPath),
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath),
    ?assert(is_binary(PluginId)),
    {ok, Loaded} = erlmcp_plugin_manager:is_loaded(Manager, PluginId),
    ?assert(Loaded),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(PluginPath).

test_unload_plugin() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    PluginPath = "/tmp/test_plugin2.erl",
    ok = create_test_plugin(PluginPath),
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath),
    ok = erlmcp_plugin_manager:unload_plugin(Manager, PluginId),
    {ok, Loaded} = erlmcp_plugin_manager:is_loaded(Manager, PluginId),
    ?assertNot(Loaded),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(PluginPath).

test_list_plugins() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    PluginPath1 = "/tmp/plugin1.erl",
    PluginPath2 = "/tmp/plugin2.erl",
    ok = create_test_plugin(PluginPath1),
    ok = create_test_plugin(PluginPath2),
    {ok, Id1} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath1),
    {ok, Id2} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath2),
    {ok, Plugins} = erlmcp_plugin_manager:list_plugins(Manager),
    ?assertEqual(2, length(Plugins)),
    ?assert(lists:member(Id1, Plugins)),
    ?assert(lists:member(Id2, Plugins)),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(PluginPath1),
    file:delete(PluginPath2).

%%%====================================================================
%%% Plugin Execution Tests
%%%====================================================================

test_execute_plugin() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    PluginPath = "/tmp/exec_plugin.erl",
    ok = create_test_plugin(PluginPath),
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath),
    {ok, Result} = erlmcp_plugin_manager:execute(Manager, PluginId, "test_command", #{}),
    ?assertMatch(#{status := success}, Result),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(PluginPath).

%%%====================================================================
%%% Hot Reload Tests
%%%====================================================================

test_hot_reload_plugin() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    PluginPath = "/tmp/hot_reload_plugin.erl",
    ok = create_test_plugin(PluginPath),
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath),
    timer:sleep(100),
    ok = modify_test_plugin(PluginPath),
    ok = erlmcp_plugin_manager:reload_plugin(Manager, PluginId),
    {ok, Version} = erlmcp_plugin_manager:get_plugin_version(Manager, PluginId),
    ?assert(Version > 1),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(PluginPath).

%%%====================================================================
%%% Dependency Tests
%%%====================================================================

test_plugin_dependencies() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    DepPlugin = "/tmp/dep_plugin.erl",
    MainPlugin = "/tmp/main_plugin.erl",
    ok = create_test_plugin(DepPlugin),
    ok = create_plugin_with_deps(MainPlugin, [DepPlugin]),
    {ok, DepId} = erlmcp_plugin_manager:load_plugin(Manager, DepPlugin),
    {ok, MainId} = erlmcp_plugin_manager:load_plugin(Manager, MainPlugin),
    {ok, Deps} = erlmcp_plugin_manager:get_dependencies(Manager, MainId),
    ?assert(lists:member(DepId, Deps)),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(DepPlugin),
    file:delete(MainPlugin).

%%%====================================================================
%%% Isolation Tests
%%%====================================================================

test_plugin_isolation() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    Plugin1 = "/tmp/isolated1.erl",
    Plugin2 = "/tmp/isolated2.erl",
    ok = create_test_plugin(Plugin1),
    ok = create_test_plugin(Plugin2),
    {ok, Id1} = erlmcp_plugin_manager:load_plugin(Manager, Plugin1),
    {ok, Id2} = erlmcp_plugin_manager:load_plugin(Manager, Plugin2),
    ok = erlmcp_plugin_manager:set_plugin_data(Manager, Id1, key, value1),
    ok = erlmcp_plugin_manager:set_plugin_data(Manager, Id2, key, value2),
    {ok, Value1} = erlmcp_plugin_manager:get_plugin_data(Manager, Id1, key),
    {ok, Value2} = erlmcp_plugin_manager:get_plugin_data(Manager, Id2, key),
    ?assertEqual(value1, Value1),
    ?assertEqual(value2, Value2),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(Plugin1),
    file:delete(Plugin2).

%%%====================================================================
%%% Crash Recovery Tests
%%%====================================================================

test_plugin_crash_recovery() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    PluginPath = "/tmp/crash_plugin.erl",
    ok = create_test_plugin(PluginPath),
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath),
    {ok, Pid} = erlmcp_plugin_manager:get_plugin_pid(Manager, PluginId),
    exit(Pid, kill),
    timer:sleep(200),
    {ok, NewPid} = erlmcp_plugin_manager:get_plugin_pid(Manager, PluginId),
    ?assert(NewPid =/= Pid),
    ?assert(is_process_alive(NewPid)),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(PluginPath).

%%%====================================================================
%%% Versioning Tests
%%%====================================================================

test_plugin_versioning() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    PluginPath = "/tmp/versioned_plugin.erl",
    ok = create_versioned_plugin(PluginPath, "1.0.0"),
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath),
    {ok, Version} = erlmcp_plugin_manager:get_plugin_version(Manager, PluginId),
    ?assertEqual(<<"1.0.0">>, Version),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(PluginPath).

%%%====================================================================
%%% Configuration Tests
%%%====================================================================

test_plugin_configuration() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    PluginPath = "/tmp/config_plugin.erl",
    ok = create_test_plugin(PluginPath),
    Config = #{setting1 => value1, setting2 => 42},
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath, Config),
    {ok, LoadedConfig} = erlmcp_plugin_manager:get_plugin_config(Manager, PluginId),
    ?assertMatch(#{setting1 := value1}, LoadedConfig),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(PluginPath).

%%%====================================================================
%%% Hook Tests
%%%====================================================================

test_plugin_hooks() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    PluginPath = "/tmp/hook_plugin.erl",
    ok = create_plugin_with_hooks(PluginPath),
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath),
    ok = erlmcp_plugin_manager:trigger_hook(Manager, pre_command, #{}),
    {ok, HooksCalled} = erlmcp_plugin_manager:get_hooks_called(Manager, PluginId),
    ?assert(lists:member(pre_command, HooksCalled)),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(PluginPath).

%%%====================================================================
%%% Discovery Tests
%%%====================================================================

test_plugin_discovery() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    PluginDir = "/tmp/plugins/",
    ok = filelib:ensure_dir(PluginDir),
    ok = create_test_plugin(PluginDir ++ "plugin1.erl"),
    ok = create_test_plugin(PluginDir ++ "plugin2.erl"),
    {ok, Discovered} = erlmcp_plugin_manager:discover_plugins(Manager, PluginDir),
    ?assertEqual(2, length(Discovered)),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(PluginDir ++ "plugin1.erl"),
    file:delete(PluginDir ++ "plugin2.erl"),
    file:del_dir(PluginDir).

%%%====================================================================
%%% Validation Tests
%%%====================================================================

test_plugin_validation() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    InvalidPlugin = "/tmp/invalid_plugin.erl",
    ok = file:write_file(InvalidPlugin, <<"invalid erlang code">>),
    Result = erlmcp_plugin_manager:load_plugin(Manager, InvalidPlugin),
    ?assertMatch({error, {compilation_failed, _}}, Result),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(InvalidPlugin).

%%%====================================================================
%%% Sandboxing Tests
%%%====================================================================

test_plugin_sandboxing() ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(#{sandbox => true}),
    PluginPath = "/tmp/sandboxed_plugin.erl",
    ok = create_test_plugin(PluginPath),
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath),
    Result = erlmcp_plugin_manager:execute(Manager, PluginId, "file_write", #{path => "/etc/passwd"}),
    ?assertMatch({error, permission_denied}, Result),
    ok = erlmcp_plugin_manager:stop(Manager),
    file:delete(PluginPath).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

create_test_plugin(Path) ->
    Code = <<"-module(test_plugin).
-export([init/1, execute/2]).

init(_Config) -> {ok, #{}}.
execute(_Command, _Args) -> {ok, #{status => success}}.
">>,
    file:write_file(Path, Code).

create_plugin_with_deps(Path, Deps) ->
    Code = io_lib:format("-module(main_plugin).
-export([init/1, execute/2, dependencies/0]).

dependencies() -> ~p.
init(_Config) -> {ok, #{}}.
execute(_Command, _Args) -> {ok, #{status => success}}.
", [Deps]),
    file:write_file(Path, Code).

create_versioned_plugin(Path, Version) ->
    Code = io_lib:format("-module(versioned_plugin).
-export([init/1, execute/2, version/0]).

version() -> ~p.
init(_Config) -> {ok, #{}}.
execute(_Command, _Args) -> {ok, #{status => success}}.
", [Version]),
    file:write_file(Path, Code).

create_plugin_with_hooks(Path) ->
    Code = <<"-module(hook_plugin).
-export([init/1, execute/2, hooks/0, handle_hook/2]).

hooks() -> [pre_command, post_command].
init(_Config) -> {ok, #{hooks_called => []}}.
execute(_Command, _Args) -> {ok, #{status => success}}.
handle_hook(Hook, State) -> {ok, State#{hooks_called => [Hook | maps:get(hooks_called, State)]}}.
">>,
    file:write_file(Path, Code).

modify_test_plugin(Path) ->
    Code = <<"-module(test_plugin).
-export([init/1, execute/2]).

init(_Config) -> {ok, #{version => 2}}.
execute(_Command, _Args) -> {ok, #{status => success, version => 2}}.
">>,
    file:write_file(Path, Code).
