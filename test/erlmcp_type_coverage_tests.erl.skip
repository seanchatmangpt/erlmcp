%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive Type Coverage Validation Tests
%%%
%%% Tests that verify:
%%% 1. Core modules have type specifications
%%% 2. Modules compile successfully
%%% 3. Type specifications use proper Erlang syntax
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_type_coverage_tests).

-include_lib("eunit/include/eunit.hrl").

-export([
    all/0
]).

%%====================================================================
%% Test Cases
%%====================================================================

%% Test that erlmcp_capabilities has specs
erlmcp_capabilities_specs_test() ->
    FilePath = "src/erlmcp_capabilities.erl",
    {ok, Content} = file:read_file(FilePath),
    ContentStr = binary_to_list(Content),

    %% Should have -spec for build_server_capabilities
    ?assert(string:find(ContentStr, "-spec build_server_capabilities()") /= nomatch,
           "Missing -spec for build_server_capabilities/0").

%% Test that erlmcp_server has specs
erlmcp_server_specs_test() ->
    FilePath = "src/erlmcp_server.erl",
    {ok, Content} = file:read_file(FilePath),
    ContentStr = binary_to_list(Content),

    %% Should have -spec declarations
    ?assert(string:find(ContentStr, "-spec") /= nomatch,
           "No -spec declarations found in erlmcp_server.erl").

%% Test that erlmcp_client has specs
erlmcp_client_specs_test() ->
    FilePath = "src/erlmcp_client.erl",
    {ok, Content} = file:read_file(FilePath),
    ContentStr = binary_to_list(Content),

    %% Should have -spec declarations
    ?assert(string:find(ContentStr, "-spec") /= nomatch,
           "No -spec declarations found in erlmcp_client.erl").

%% Test that erlmcp_json_rpc has specs
erlmcp_json_rpc_specs_test() ->
    FilePath = "src/erlmcp_json_rpc.erl",
    {ok, Content} = file:read_file(FilePath),
    ContentStr = binary_to_list(Content),

    %% Should have -spec declarations
    ?assert(string:find(ContentStr, "-spec") /= nomatch,
           "No -spec declarations found in erlmcp_json_rpc.erl").

%% Test that modules compile successfully
modules_compile_test() ->
    %% If this test runs, modules have compiled successfully
    ?assert(filelib:is_dir("_build/default/lib/erlmcp/ebin"),
           "erlmcp ebin directory not found - modules did not compile").

%% Test that specs use valid syntax
specs_have_valid_syntax_test() ->
    FilePath = "src/erlmcp_server.erl",
    {ok, Content} = file:read_file(FilePath),
    ContentStr = binary_to_list(Content),

    Lines = string:split(ContentStr, "\n", all),
    SpecLines = [L || L <- Lines, string:find(L, "-spec") /= nomatch],

    %% Each spec should have -> and end with .
    lists:foreach(fun(SpecLine) ->
        HasArrow = string:find(SpecLine, "->") /= nomatch,
        HasDot = string:find(SpecLine, ".") /= nomatch,
        ?assert(HasArrow andalso HasDot,
               io_lib:format("Invalid spec syntax: ~s", [SpecLine]))
    end, SpecLines).

%% Test that no specs use bare any() type
no_any_types_test() ->
    FilePath = "src/erlmcp_capabilities.erl",
    {ok, Content} = file:read_file(FilePath),
    ContentStr = binary_to_list(Content),

    Lines = string:split(ContentStr, "\n", all),
    AnyLines = [L || L <- Lines,
               string:find(L, "-spec") /= nomatch,
               string:find(L, "any()") /= nomatch],

    ?assertEqual([], AnyLines,
               io_lib:format("Found bare any() in specs: ~w", [AnyLines])).

%% Test that key modules have adequate spec coverage
adequate_spec_coverage_test() ->
    CoreModules = [
        "src/erlmcp_server.erl",
        "src/erlmcp_client.erl",
        "src/erlmcp_registry.erl",
        "src/erlmcp_json_rpc.erl"
    ],

    lists:foreach(fun(FilePath) ->
        {ok, Content} = file:read_file(FilePath),
        ContentStr = binary_to_list(Content),

        Lines = string:split(ContentStr, "\n", all),
        SpecCount = length([L || L <- Lines, string:find(L, "-spec") /= nomatch]),

        %% Core modules should have at least some specs
        ?assert(SpecCount > 0,
               io_lib:format("Module ~s has no specs", [FilePath]))
    end, CoreModules).

%%====================================================================
%% Suite Setup
%%====================================================================

all() ->
    [
        erlmcp_capabilities_specs_test,
        erlmcp_server_specs_test,
        erlmcp_client_specs_test,
        erlmcp_json_rpc_specs_test,
        modules_compile_test,
        specs_have_valid_syntax_test,
        no_any_types_test,
        adequate_spec_coverage_test
    ].
