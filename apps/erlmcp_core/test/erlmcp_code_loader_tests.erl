-module(erlmcp_code_loader_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    {ok, _Pid} = erlmcp_code_loader:start_link(),
    ok.

cleanup(_) ->
    case whereis(erlmcp_code_loader) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid, normal, 5000)
    end,
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

code_loader_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"Safe load valid module", fun test_safe_load_valid_module/0},
      {"Safe load invalid module", fun test_safe_load_invalid_module/0},
      {"Hot reload with code_change", fun test_hot_reload_with_code_change/0},
      {"Hot reload without code_change", fun test_hot_reload_without_code_change/0},
      {"Prepare and commit reload", fun test_prepare_commit_reload/0},
      {"Get module version", fun test_get_module_version/0},
      {"Validate module", fun test_validate_module/0},
      {"Get module MD5", fun test_get_module_md5/0},
      {"Get object code", fun test_get_object_code/0},
      {"Atomic swap success", fun test_atomic_swap_success/0},
      {"Atomic swap rollback", fun test_atomic_swap_rollback/0}]}.

%%====================================================================
%% Individual Tests
%%====================================================================

test_safe_load_valid_module() ->
    %% Load a real existing module
    Module = erlmcp_json_rpc,

    Result = erlmcp_code_loader:safe_load(Module),

    ?assertEqual(ok, Result),

    %% Verify module is loaded
    ?assertMatch({file, _}, code:is_loaded(Module)).

test_safe_load_invalid_module() ->
    %% Try to load non-existent module
    Module = fake_nonexistent_module,

    Result = erlmcp_code_loader:safe_load(Module),

    ?assertMatch({error, _}, Result).

test_hot_reload_with_code_change() ->
    %% Test hot reload with a module that has code_change/2
    Module = erlmcp_code_reload,  % Has code_change/2

    %% Create mock state
    State = #{test => "data"},

    Result = erlmcp_code_loader:hot_reload(Module, State),

    ?assertEqual(ok, Result),

    %% Module should still be loaded
    ?assertMatch({file, _}, code:is_loaded(Module)).

test_hot_reload_without_code_change() ->
    %% Test hot reload with a module without code_change/2
    Module = erlmcp_json_rpc,

    State = #{test => "data"},

    Result = erlmcp_code_loader:hot_reload(Module, State),

    ?assertEqual(ok, Result),

    %% Module should still be loaded
    ?assertMatch({file, _}, code:is_loaded(Module)).

test_prepare_commit_reload() ->
    Module = erlmcp_json_rpc,

    %% Prepare reload
    {ok, Version} = erlmcp_code_loader:prepare_reload(Module),

    ?assert(is_binary(Version)),
    ?assert(byte_size(Version) > 0),

    %% Commit reload
    Result = erlmcp_code_loader:commit_reload(Module, Version),

    ?assertEqual(ok, Result).

test_get_module_version() ->
    Module = erlmcp_json_rpc,

    Result = erlmcp_code_loader:get_module_version(Module),

    ?assertMatch({ok, _Version}, Result),

    %% Version should be binary
    {ok, Version} = Result,
    ?assert(is_binary(Version)).

test_validate_module() ->
    %% Valid module
    ValidModule = erlmcp_json_rpc,
    ?assertEqual(ok, erlmcp_code_loader:validate_module(ValidModule)),

    %% Invalid module
    InvalidModule = fake_module,
    Result = erlmcp_code_loader:validate_module(InvalidModule),
    ?assertMatch({error, _}, Result).

test_get_module_md5() ->
    Module = erlmcp_json_rpc,

    Result = erlmcp_code_loader:get_module_md5(Module),

    ?assertMatch({ok, _MD5}, Result),

    %% MD5 should be 16 bytes (32 hex chars)
    {ok, MD5} = Result,
    ?assertEqual(32, byte_size(MD5)).

test_get_object_code() ->
    Module = erlmcp_json_rpc,

    Result = erlmcp_code_loader:get_object_code(Module),

    ?assertMatch({ok, Module, _Binary, _Filename}, Result),

    %% Binary should be present
    {ok, _Module, Binary, _Filename} = Result,
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0).

test_atomic_swap_success() ->
    Module = erlmcp_json_rpc,

    %% Get current object code
    {ok, _Module, Binary, Filename} = erlmcp_code_loader:get_object_code(Module),

    %% Perform atomic swap with same code (should succeed)
    Result = erlmcp_code_loader:atomic_swap(Module, Binary, Filename),

    ?assertEqual(ok, Result),

    %% Module should still be loaded
    ?assertMatch({file, _}, code:is_loaded(Module)).

test_atomic_swap_rollback() ->
    Module = erlmcp_json_rpc,

    %% Get current object code
    {ok, _Module, Binary, Filename} = erlmcp_code_loader:get_object_code(Module),

    %% Create invalid binary
    InvalidBinary = <<0, 1, 2, 3>>,

    %% Perform atomic swap with invalid code (should rollback)
    Result = erlmcp_code_loader:atomic_swap(Module, InvalidBinary, Filename),

    ?assertMatch({error, _}, Result),

    %% Module should still be functional (rolled back)
    ?assertMatch({file, _}, code:is_loaded(Module)).

%%====================================================================
%% Property-Based Tests
%%====================================================================

prop_safe_load_idempotent() ->
    ?FORALL(Module, elements([erlmcp_json_rpc, erlmcp_registry]),
            begin
                %% Load twice - should be idempotent
                ok =:= erlmcp_code_loader:safe_load(Module) andalso
                ok =:= erlmcp_code_loader:safe_load(Module)
            end).

prop_version_consistent() ->
    ?FORALL(Module, elements([erlmcp_json_rpc, erlmcp_registry]),
            begin
                {ok, Version1} = erlmcp_code_loader:get_module_version(Module),
                {ok, Version2} = erlmcp_code_loader:get_module_md5(Module),
                Version1 =:= Version2
            end).

%%====================================================================
%% Helper Functions
%%====================================================================

get_module_version(Module) ->
    case erlmcp_code_loader:get_module_version(Module) of
        {ok, Version} -> Version;
        {error, _} -> undefined
    end.
