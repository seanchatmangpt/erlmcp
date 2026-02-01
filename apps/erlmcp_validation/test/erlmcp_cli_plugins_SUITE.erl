%%%-------------------------------------------------------------------
%%% @doc erlmcp_cli_plugins_SUITE - Plugin System Integration Tests
%%%
%%% Full plugin loading, execution, and lifecycle integration tests
%%%
%%% Chicago School TDD:
%%% - Real plugin processes
%%% - Real supervision trees
%%% - Full plugin lifecycle
%%% - NO mocks
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_plugins_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%%====================================================================
%%% CT Callbacks
%%%====================================================================

all() ->
    [
     full_plugin_lifecycle_test,
     plugin_hot_reload_test,
     plugin_dependency_chain_test,
     plugin_crash_and_recovery_test,
     multiple_plugins_test,
     plugin_isolation_test,
     plugin_hooks_integration_test,
     plugin_configuration_test,
     plugin_discovery_and_load_test,
     plugin_versioning_test
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    PluginDir = "/tmp/test_plugins/",
    filelib:ensure_dir(PluginDir),
    [{plugin_dir, PluginDir} | Config].

end_per_suite(Config) ->
    PluginDir = ?config(plugin_dir, Config),
    [file:delete(F) || F <- filelib:wildcard(PluginDir ++ "*.erl")],
    file:del_dir(PluginDir),
    application:stop(erlmcp),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%====================================================================
%%% Test Cases
%%%====================================================================

full_plugin_lifecycle_test(Config) ->
    PluginDir = ?config(plugin_dir, Config),
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    
    %% Create and load plugin
    PluginPath = PluginDir ++ "lifecycle_plugin.erl",
    ok = create_plugin(PluginPath, "lifecycle_plugin"),
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath),
    
    %% Verify loaded
    {ok, true} = erlmcp_plugin_manager:is_loaded(Manager, PluginId),
    
    %% Execute plugin command
    {ok, Result} = erlmcp_plugin_manager:execute(Manager, PluginId, "test", #{}),
    #{status := success} = Result,
    
    %% Unload plugin
    ok = erlmcp_plugin_manager:unload_plugin(Manager, PluginId),
    {ok, false} = erlmcp_plugin_manager:is_loaded(Manager, PluginId),
    
    ok = erlmcp_plugin_manager:stop(Manager).

plugin_hot_reload_test(Config) ->
    PluginDir = ?config(plugin_dir, Config),
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    
    %% Load initial version
    PluginPath = PluginDir ++ "hot_reload_plugin.erl",
    ok = create_plugin_v1(PluginPath),
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath),
    {ok, V1Result} = erlmcp_plugin_manager:execute(Manager, PluginId, "version", #{}),
    
    %% Modify plugin
    timer:sleep(100),
    ok = create_plugin_v2(PluginPath),
    
    %% Hot reload
    ok = erlmcp_plugin_manager:reload_plugin(Manager, PluginId),
    
    %% Execute new version
    {ok, V2Result} = erlmcp_plugin_manager:execute(Manager, PluginId, "version", #{}),
    
    %% Verify different versions
    true = V1Result =/= V2Result,
    
    ok = erlmcp_plugin_manager:stop(Manager).

plugin_dependency_chain_test(Config) ->
    PluginDir = ?config(plugin_dir, Config),
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    
    %% Create dependency chain: C depends on B, B depends on A
    PluginA = PluginDir ++ "plugin_a.erl",
    PluginB = PluginDir ++ "plugin_b.erl",
    PluginC = PluginDir ++ "plugin_c.erl",
    
    ok = create_plugin(PluginA, "plugin_a"),
    ok = create_plugin_with_dep(PluginB, "plugin_b", "plugin_a"),
    ok = create_plugin_with_dep(PluginC, "plugin_c", "plugin_b"),
    
    %% Load in correct order
    {ok, IdA} = erlmcp_plugin_manager:load_plugin(Manager, PluginA),
    {ok, IdB} = erlmcp_plugin_manager:load_plugin(Manager, PluginB),
    {ok, IdC} = erlmcp_plugin_manager:load_plugin(Manager, PluginC),
    
    %% Verify dependency resolution
    {ok, DepsC} = erlmcp_plugin_manager:get_dependencies(Manager, IdC),
    true = lists:member(IdB, DepsC),
    
    ok = erlmcp_plugin_manager:stop(Manager).

plugin_crash_and_recovery_test(Config) ->
    PluginDir = ?config(plugin_dir, Config),
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    
    %% Load plugin
    PluginPath = PluginDir ++ "crash_plugin.erl",
    ok = create_plugin(PluginPath, "crash_plugin"),
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath),
    
    %% Get plugin process
    {ok, Pid1} = erlmcp_plugin_manager:get_plugin_pid(Manager, PluginId),
    
    %% Crash plugin
    exit(Pid1, kill),
    timer:sleep(200),
    
    %% Verify restarted by supervisor
    {ok, Pid2} = erlmcp_plugin_manager:get_plugin_pid(Manager, PluginId),
    true = Pid1 =/= Pid2,
    true = is_process_alive(Pid2),
    
    ok = erlmcp_plugin_manager:stop(Manager).

multiple_plugins_test(Config) ->
    PluginDir = ?config(plugin_dir, Config),
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    
    %% Load 10 plugins
    Plugins = [begin
        Path = PluginDir ++ io_lib:format("plugin_~p.erl", [N]),
        ok = create_plugin(Path, io_lib:format("plugin_~p", [N])),
        {ok, Id} = erlmcp_plugin_manager:load_plugin(Manager, Path),
        Id
    end || N <- lists:seq(1, 10)],
    
    %% Verify all loaded
    {ok, LoadedPlugins} = erlmcp_plugin_manager:list_plugins(Manager),
    10 = length(LoadedPlugins),
    
    %% Execute all plugins concurrently
    [spawn(fun() ->
        {ok, _} = erlmcp_plugin_manager:execute(Manager, Id, "test", #{})
    end) || Id <- Plugins],
    
    timer:sleep(500),
    
    ok = erlmcp_plugin_manager:stop(Manager).

plugin_isolation_test(Config) ->
    PluginDir = ?config(plugin_dir, Config),
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    
    %% Load two plugins
    Plugin1 = PluginDir ++ "isolated1.erl",
    Plugin2 = PluginDir ++ "isolated2.erl",
    ok = create_plugin(Plugin1, "isolated1"),
    ok = create_plugin(Plugin2, "isolated2"),
    
    {ok, Id1} = erlmcp_plugin_manager:load_plugin(Manager, Plugin1),
    {ok, Id2} = erlmcp_plugin_manager:load_plugin(Manager, Plugin2),
    
    %% Set different state in each plugin
    ok = erlmcp_plugin_manager:set_plugin_state(Manager, Id1, #{data => value1}),
    ok = erlmcp_plugin_manager:set_plugin_state(Manager, Id2, #{data => value2}),
    
    %% Verify isolation
    {ok, State1} = erlmcp_plugin_manager:get_plugin_state(Manager, Id1),
    {ok, State2} = erlmcp_plugin_manager:get_plugin_state(Manager, Id2),
    
    #{data := value1} = State1,
    #{data := value2} = State2,
    
    ok = erlmcp_plugin_manager:stop(Manager).

plugin_hooks_integration_test(Config) ->
    PluginDir = ?config(plugin_dir, Config),
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    
    %% Load plugin with hooks
    PluginPath = PluginDir ++ "hooks_plugin.erl",
    ok = create_plugin_with_hooks(PluginPath),
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath),
    
    %% Trigger hook
    ok = erlmcp_plugin_manager:trigger_hook(Manager, pre_command, #{cmd => "test"}),
    
    %% Verify hook was called
    {ok, HooksCalled} = erlmcp_plugin_manager:get_hooks_called(Manager, PluginId),
    true = lists:member(pre_command, HooksCalled),
    
    ok = erlmcp_plugin_manager:stop(Manager).

plugin_configuration_test(Config) ->
    PluginDir = ?config(plugin_dir, Config),
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    
    %% Load plugin with configuration
    PluginPath = PluginDir ++ "config_plugin.erl",
    ok = create_plugin(PluginPath, "config_plugin"),
    
    PluginConfig = #{
        setting1 => <<"value1">>,
        setting2 => 42
    },
    
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath, PluginConfig),
    
    %% Verify configuration passed to plugin
    {ok, LoadedConfig} = erlmcp_plugin_manager:get_plugin_config(Manager, PluginId),
    #{setting1 := <<"value1">>, setting2 := 42} = LoadedConfig,
    
    ok = erlmcp_plugin_manager:stop(Manager).

plugin_discovery_and_load_test(Config) ->
    PluginDir = ?config(plugin_dir, Config),
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    
    %% Create multiple plugins in directory
    [ok = create_plugin(PluginDir ++ io_lib:format("disco_~p.erl", [N]), 
                        io_lib:format("disco_~p", [N]))
     || N <- lists:seq(1, 5)],
    
    %% Discover plugins
    {ok, Discovered} = erlmcp_plugin_manager:discover_plugins(Manager, PluginDir),
    true = length(Discovered) >= 5,
    
    %% Load all discovered plugins
    {ok, LoadedIds} = erlmcp_plugin_manager:load_discovered(Manager),
    true = length(LoadedIds) >= 5,
    
    ok = erlmcp_plugin_manager:stop(Manager).

plugin_versioning_test(Config) ->
    PluginDir = ?config(plugin_dir, Config),
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    
    %% Load plugin with version
    PluginPath = PluginDir ++ "versioned.erl",
    ok = create_versioned_plugin(PluginPath, "1.2.3"),
    
    {ok, PluginId} = erlmcp_plugin_manager:load_plugin(Manager, PluginPath),
    {ok, Version} = erlmcp_plugin_manager:get_plugin_version(Manager, PluginId),
    
    <<"1.2.3">> = Version,
    
    ok = erlmcp_plugin_manager:stop(Manager).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

create_plugin(Path, ModuleName) ->
    Code = io_lib:format("-module(~s).
-export([init/1, execute/2]).

init(_Config) -> {ok, #{}}.
execute(_Command, _Args) -> {ok, #{status => success}}.
", [ModuleName]),
    file:write_file(Path, list_to_binary(Code)).

create_plugin_v1(Path) ->
    Code = <<"-module(hot_reload_plugin).
-export([init/1, execute/2]).

init(_Config) -> {ok, #{version => 1}}.
execute(version, _Args) -> {ok, #{version => 1}}.
">>,
    file:write_file(Path, Code).

create_plugin_v2(Path) ->
    Code = <<"-module(hot_reload_plugin).
-export([init/1, execute/2]).

init(_Config) -> {ok, #{version => 2}}.
execute(version, _Args) -> {ok, #{version => 2}}.
">>,
    file:write_file(Path, Code).

create_plugin_with_dep(Path, ModuleName, DepName) ->
    Code = io_lib:format("-module(~s).
-export([init/1, execute/2, dependencies/0]).

dependencies() -> [~s].
init(_Config) -> {ok, #{}}.
execute(_Command, _Args) -> {ok, #{status => success}}.
", [ModuleName, DepName]),
    file:write_file(Path, list_to_binary(Code)).

create_plugin_with_hooks(Path) ->
    Code = <<"-module(hooks_plugin).
-export([init/1, execute/2, hooks/0, handle_hook/2]).

hooks() -> [pre_command, post_command].
init(_Config) -> {ok, #{hooks_called => []}}.
execute(_Command, _Args) -> {ok, #{status => success}}.
handle_hook(Hook, State) -> 
    {ok, State#{hooks_called => [Hook | maps:get(hooks_called, State)]}}.
">>,
    file:write_file(Path, Code).

create_versioned_plugin(Path, Version) ->
    Code = io_lib:format("-module(versioned).
-export([init/1, execute/2, version/0]).

version() -> <<\"~s\">>.
init(_Config) -> {ok, #{}}.
execute(_Command, _Args) -> {ok, #{status => success}}.
", [Version]),
    file:write_file(Path, list_to_binary(Code)).
