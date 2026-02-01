%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin System EUnit Tests
%%%
%%% Tests for the plugin architecture following Chicago TDD principles.
%%% Uses real processes (no mocks).
%%%
%%% Coverage:
%%% - Plugin loading/unloading
%%% - Plugin discovery
%%% - Plugin registry
%%% - Plugin hooks execution
%%% - Error handling and isolation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

plugin_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_plugin_loader_validation/1,
      fun test_plugin_registry_registration/1,
      fun test_plugin_manager_lifecycle/1,
      fun test_plugin_worker_isolation/1,
      fun test_plugin_discovery/1,
      fun test_plugin_hooks/1]}.

setup() ->
    %% Start required applications
    application:ensure_all_started(gproc),
    application:ensure_all_started(erlmcp_core),

    %% Start plugin supervisor
    {ok, Pid} = erlmcp_plugin_sup:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop plugin supervisor
    case erlang:is_process_alive(Pid) of
        true ->
            exit(Pid, shutdown);
        false ->
            ok
    end,

    %% Clean up any remaining processes
    timer:sleep(100).

%%====================================================================
%% Test Cases
%%====================================================================

test_plugin_loader_validation(_) ->
    [?_test(begin
                %% Create a mock plugin module for testing
                MockPlugin = create_mock_plugin(),

                %% Validate plugin
                Result = erlmcp_plugin_loader:validate_plugin(MockPlugin),
                ?assertEqual(ok, Result),

                %% Clean up
                cleanup_mock_plugin(MockPlugin)
            end),
     ?_test(begin
                %% Test plugin with missing metadata
                InvalidPlugin = create_invalid_plugin_no_metadata(),

                %% Should fail validation
                Result = erlmcp_plugin_loader:validate_plugin(InvalidPlugin),
                ?assertMatch({error, _}, Result),

                %% Clean up
                cleanup_mock_plugin(InvalidPlugin)
            end)].

test_plugin_registry_registration(_) ->
    [?_test(begin
                %% Create mock plugin
                Module = create_mock_plugin(),
                Metadata = Module:metadata(),

                %% Start a mock worker
                {ok, WorkerPid} = erlmcp_plugin_worker:start_link(Module, #{}),

                %% Register plugin
                ok = erlmcp_plugin_registry:register_plugin(Module, Metadata, WorkerPid),

                %% Verify registration
                ?assertEqual(true, erlmcp_plugin_registry:plugin_exists(Module)),

                %% Get plugin info
                {ok, PluginInfo} = erlmcp_plugin_registry:get_plugin(Module),
                ?assertEqual(Module, maps:get(module, PluginInfo)),

                %% Unregister
                ok = erlmcp_plugin_registry:unregister_plugin(Module),
                ?assertEqual(false, erlmcp_plugin_registry:plugin_exists(Module)),

                %% Clean up
                erlmcp_plugin_worker:stop(WorkerPid),
                cleanup_mock_plugin(Module)
            end),
     ?_test(begin
                %% Test listing plugins by type
                Module1 = create_mock_formatter_plugin(),
                Module2 = create_mock_validator_plugin(),

                {ok, Worker1} = erlmcp_plugin_worker:start_link(Module1, #{}),
                {ok, Worker2} = erlmcp_plugin_worker:start_link(Module2, #{}),

                ok = erlmcp_plugin_registry:register_plugin(Module1, Module1:metadata(), Worker1),
                ok = erlmcp_plugin_registry:register_plugin(Module2, Module2:metadata(), Worker2),

                %% List formatters
                {ok, Formatters} = erlmcp_plugin_registry:list_plugins_by_type(formatter),
                ?assertEqual(1, length(Formatters)),

                %% List validators
                {ok, Validators} = erlmcp_plugin_registry:list_plugins_by_type(validator),
                ?assertEqual(1, length(Validators)),

                %% Clean up
                erlmcp_plugin_registry:unregister_plugin(Module1),
                erlmcp_plugin_registry:unregister_plugin(Module2),
                erlmcp_plugin_worker:stop(Worker1),
                erlmcp_plugin_worker:stop(Worker2),
                cleanup_mock_plugin(Module1),
                cleanup_mock_plugin(Module2)
            end)].

test_plugin_manager_lifecycle(_) ->
    [?_test(begin
                %% Create and load plugin
                Module = create_mock_plugin(),

                %% Add plugin to code path
                ensure_plugin_in_path(Module),

                %% Load plugin via manager
                {ok, WorkerPid} = erlmcp_plugin_manager:load_plugin(Module, #{}),
                ?assert(erlang:is_process_alive(WorkerPid)),

                %% Verify it's in loaded plugins
                {ok, LoadedPlugins} = erlmcp_plugin_manager:list_loaded_plugins(),
                ?assert(lists:member(Module, LoadedPlugins)),

                %% Unload plugin
                ok = erlmcp_plugin_manager:unload_plugin(Module),

                %% Verify it's no longer loaded
                {ok, LoadedPlugins2} = erlmcp_plugin_manager:list_loaded_plugins(),
                ?assertNot(lists:member(Module, LoadedPlugins2)),

                %% Clean up
                cleanup_mock_plugin(Module)
            end)].

test_plugin_worker_isolation(_) ->
    [?_test(begin
                %% Create a plugin that crashes
                CrashingModule = create_crashing_plugin(),
                ensure_plugin_in_path(CrashingModule),

                %% Load plugin
                {ok, WorkerPid} = erlmcp_plugin_manager:load_plugin(CrashingModule, #{}),

                %% Call plugin function that crashes
                Result = erlmcp_plugin_worker:call_function(WorkerPid, execute, [crash]),

                %% Should return error, not crash
                ?assertMatch({error, _}, Result),

                %% Manager should still be alive
                ?assert(erlang:is_process_alive(whereis(erlmcp_plugin_manager))),

                %% Clean up
                cleanup_mock_plugin(CrashingModule)
            end)].

test_plugin_discovery(_) ->
    [?_test(begin
                %% Get plugin paths
                Paths = erlmcp_plugin_loader:get_plugin_paths(),
                ?assert(is_list(Paths)),
                ?assert(length(Paths) > 0)
            end),
     ?_test(begin
                %% Discover plugins (may find none, that's OK)
                Result = erlmcp_plugin_loader:discover_plugins(),
                ?assertMatch({ok, _}, Result)
            end)].

test_plugin_hooks(_) ->
    [?_test(begin
                %% Create middleware plugin
                Module = create_mock_middleware_plugin(),
                ensure_plugin_in_path(Module),

                %% Load plugin
                {ok, _WorkerPid} = erlmcp_plugin_manager:load_plugin(Module, #{}),

                %% Execute pre-command hook
                Request = #{command => test, args => []},
                {ok, ModifiedRequest} = erlmcp_plugin_manager:execute_pre_command_hooks(Request),

                %% Should have been modified by middleware
                ?assertMatch(#{command := test, middleware_pre := true}, ModifiedRequest),

                %% Execute post-command hook
                Response = #{result => success},
                {ok, ModifiedResponse} =
                    erlmcp_plugin_manager:execute_post_command_hooks(Request, Response),

                %% Should have been modified by middleware
                ?assertMatch(#{result := success, middleware_post := true}, ModifiedResponse),

                %% Clean up
                erlmcp_plugin_manager:unload_plugin(Module),
                cleanup_mock_plugin(Module)
            end)].

%%====================================================================
%% Test Helpers - Mock Plugins
%%====================================================================

create_mock_plugin() ->
    Module = erlmcp_plugin_test_mock,
    Code = mock_plugin_code(Module, formatter),
    compile_and_load(Module, Code),
    Module.

create_mock_formatter_plugin() ->
    Module = erlmcp_plugin_test_formatter,
    Code = mock_plugin_code(Module, formatter),
    compile_and_load(Module, Code),
    Module.

create_mock_validator_plugin() ->
    Module = erlmcp_plugin_test_validator,
    Code = mock_plugin_code(Module, validator),
    compile_and_load(Module, Code),
    Module.

create_mock_middleware_plugin() ->
    Module = erlmcp_plugin_test_middleware,
    Code = mock_middleware_code(Module),
    compile_and_load(Module, Code),
    Module.

create_crashing_plugin() ->
    Module = erlmcp_plugin_test_crash,
    Code = mock_crashing_code(Module),
    compile_and_load(Module, Code),
    Module.

create_invalid_plugin_no_metadata() ->
    Module = erlmcp_plugin_test_invalid,
    Code = "-module(erlmcp_plugin_test_invalid).\n-export([init/1]).\ninit(_) -> {ok, #{}}.\n",
    compile_and_load(Module, Code),
    Module.

mock_plugin_code(Module, Type) ->
    io_lib:format("-module(~s).\n"
                  "-behaviour(erlmcp_plugin_~s).\n"
                  "-export([metadata/0, init/1, ~s]).\n"
                  "metadata() -> #{name => <<\"test\">>, version => <<\"1.0.0\">>, type => ~s, description => <<\"test\">>}.\n"
                  "init(_) -> {ok, #{}}.\n"
                  "~s",
                  [Module, Type, get_exports(Type), Type, get_functions(Type)]).

mock_middleware_code(Module) ->
    io_lib:format("-module(~s).\n"
                  "-behaviour(erlmcp_plugin_middleware).\n"
                  "-export([metadata/0, init/1, pre_execute/2, post_execute/2]).\n"
                  "metadata() -> #{name => <<\"middleware_test\">>, version => <<\"1.0.0\">>, type => middleware, description => <<\"test\">>}.\n"
                  "init(_) -> {ok, #{}}.\n"
                  "pre_execute(Request, State) -> {ok, Request#{middleware_pre => true}, State}.\n"
                  "post_execute(Response, State) -> {ok, Response#{middleware_post => true}, State}.\n",
                  [Module]).

mock_crashing_code(Module) ->
    io_lib:format("-module(~s).\n"
                  "-behaviour(erlmcp_plugin_command).\n"
                  "-export([metadata/0, init/1, execute/2, help/0]).\n"
                  "metadata() -> #{name => <<\"crash\">>, version => <<\"1.0.0\">>, type => command, description => <<\"crash\">>}.\n"
                  "init(_) -> {ok, #{}}.\n"
                  "execute(crash, _State) -> error(intentional_crash);\n"
                  "execute(_, State) -> {ok, ok, State}.\n"
                  "help() -> <<\"crash\">>.\n",
                  [Module]).

get_exports(formatter) ->
    "format/2, supports_format/0";
get_exports(validator) ->
    "validate/2, get_schema/0";
get_exports(exporter) ->
    "export/2, get_config_schema/0";
get_exports(command) ->
    "execute/2, help/0";
get_exports(_) ->
    "".

get_functions(formatter) ->
    "format(Data, State) -> {ok, Data, State}.\n"
    "supports_format() -> test.\n";
get_functions(validator) ->
    "validate(Data, State) -> {ok, #{status => passed}, State}.\n"
    "get_schema() -> #{}.\n";
get_functions(exporter) ->
    "export(Data, State) -> {ok, #{exported => true}, State}.\n"
    "get_config_schema() -> #{}.\n";
get_functions(command) ->
    "execute(Args, State) -> {ok, Args, State}.\n"
    "help() -> <<\"help\">>.\n";
get_functions(_) ->
    "".

compile_and_load(Module, Code) ->
    %% Write to temporary file
    TempFile = "/tmp/" ++ atom_to_list(Module) ++ ".erl",
    file:write_file(TempFile, Code),

    %% Compile
    {ok, Module, Binary} = compile:file(TempFile, [binary, return_errors]),

    %% Load
    code:purge(Module),
    {module, Module} = code:load_binary(Module, TempFile, Binary),

    %% Clean up temp file
    file:delete(TempFile),

    ok.

cleanup_mock_plugin(Module) ->
    code:purge(Module),
    code:delete(Module),
    ok.

ensure_plugin_in_path(_Module) ->
    %% Plugin is already loaded by compile_and_load
    ok.
