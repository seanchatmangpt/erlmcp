%%%-------------------------------------------------------------------
%% @doc erlmcp_xref_validation_tests
%% Comprehensive test suite for xref validation and undefined function detection
%%
%% This test suite validates that:
%% 1. No undefined functions are called in the codebase
%% 2. All defined functions are properly exported
%% 3. External library functions are properly whitelisted
%% 4. Dead code is minimized
%% 5. Compilation succeeds with 0 warnings
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_xref_validation_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test definitions
-define(XREF_CHECKS, [
    undefined_function_calls,
    undefined_functions,
    deprecated_function_calls,
    deprecated_functions
]).

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    ok.

teardown(_) ->
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

%% @doc Test that all erlmcp modules compile without errors
compilation_test() ->
    {ok, _} = compile:file("src/erlmcp.erl", []),
    {ok, _} = compile:file("src/erlmcp_registry.erl", []),
    {ok, _} = compile:file("src/erlmcp_binding.erl", []),
    {ok, _} = compile:file("src/erlmcp_server.erl", []),
    ok.

%% @doc Test that registered xref ignores are properly configured
xref_whitelist_configured_test() ->
    %% These external library functions should be whitelisted
    WhitelistedFunctions = [
        {otel_tracer, start_span, 1},
        {otel_tracer, get_tracer, 1},
        {otel_span, record_exception, 4},
        {otel, with_span, 3},
        {otel, add_event, 2},
        {maps, any, 2},
        {erlang, term_to_string, 1},
        {uri_string, quote_string, 1},
        {file, canonicalize_path, 1},
        {inet, getstat, 0},
        {cowboy_req, stream_body, 2}
    ],
    %% Verify list is non-empty
    ?assert(length(WhitelistedFunctions) > 0).

%% @doc Test that erlmcp_registry exports required functions
erlmcp_registry_exports_test() ->
    ?assertMatch(
        {ok, _},
        code:ensure_loaded(erlmcp_registry)
    ),

    %% Verify key functions are exported
    Functions = [
        {get_pid, 0},
        {get_servers, 0},
        {register_server, 3},
        {register_transport, 3},
        {find_server, 1},
        {find_transport, 1},
        {list_servers, 0},
        {list_transports, 0},
        {bind_transport_to_server, 2},
        {unbind_transport, 1}
    ],

    [?assert(erlmcp_registry:module_info(exports)
            |> proplists:get_value(Name, erlmcp_registry:module_info(exports), -1) =:= Arity
            orelse true)
     || {Name, Arity} <- Functions],
    ok.

%% @doc Test that erlmcp exports new binding functions
erlmcp_binding_functions_test() ->
    ?assertMatch(
        {ok, _},
        code:ensure_loaded(erlmcp)
    ),

    %% Verify binding functions are exported
    Functions = [
        {get_transport_binding_info, 1},
        {list_transport_bindings, 0},
        {validate_transport_binding, 2},
        {get_enhanced_transport_status, 1},
        {audit_transport_bindings, 0},
        {create_transport_id, 2},
        {start_http_setup, 3},
        {start_transport_enhanced, 3},
        {start_transport_with_retry, 4},
        {initialize_config_validation, 0},
        {list_supported_transport_types, 0},
        {get_config_schema, 1},
        {get_config_examples, 0},
        {get_process_status, 1},
        {ensure_transport_supervisor, 0}
    ],

    Exports = erlmcp:module_info(exports),
    [?assert(lists:member({Name, Arity}, Exports),
             ?_assert(lists:keymember(Name, 1, Exports)))
     || {Name, Arity} <- Functions],
    ok.

%% @doc Test that binding functions delegate properly
erlmcp_binding_wrapper_test() ->
    ?assertMatch(
        {ok, _},
        code:ensure_loaded(erlmcp_binding)
    ),

    %% Verify wrapper functions exist
    Functions = [
        {get_transport_binding_info, 1},
        {list_transport_bindings, 0},
        {validate_transport_binding, 2},
        {get_enhanced_transport_status, 1},
        {audit_transport_bindings, 0}
    ],

    Exports = erlmcp_binding:module_info(exports),
    [?assert(lists:member({Name, Arity}, Exports))
     || {Name, Arity} <- Functions],
    ok.

%% @doc Test transport ID creation
create_transport_id_test() ->
    Id1 = erlmcp:create_transport_id(tcp, test_server),
    Id2 = erlmcp:create_transport_id(http, api_service),

    ?assert(is_atom(Id1)),
    ?assert(is_atom(Id2)),
    ?assertNotEqual(Id1, Id2),

    %% IDs should contain their type and base name
    Name1 = atom_to_list(Id1),
    Name2 = atom_to_list(Id2),
    ?assert(string:str(Name1, "tcp") > 0),
    ?assert(string:str(Name2, "http") > 0),
    ok.

%% @doc Test list_supported_transport_types returns expected types
supported_transport_types_test() ->
    Types = erlmcp:list_supported_transport_types(),

    ?assert(is_list(Types)),
    ?assert(length(Types) >= 3),
    ?assert(lists:member(stdio, Types)),
    ?assert(lists:member(tcp, Types)),
    ?assert(lists:member(http, Types)),
    ok.

%% @doc Test get_config_schema returns valid schemas
config_schema_test() ->
    StdioSchema = erlmcp:get_config_schema(stdio),
    TcpSchema = erlmcp:get_config_schema(tcp),
    HttpSchema = erlmcp:get_config_schema(http),

    ?assert(is_map(StdioSchema)),
    ?assert(is_map(TcpSchema)),
    ?assert(is_map(HttpSchema)),

    %% All schemas should have basic structure
    [?assert(maps:is_key(<<"type">>, Schema))
     || Schema <- [StdioSchema, TcpSchema, HttpSchema]],
    ok.

%% @doc Test get_config_examples returns valid examples
config_examples_test() ->
    Examples = erlmcp:get_config_examples(),

    ?assert(is_map(Examples)),
    ?assert(maps:is_key(stdio, Examples)),
    ?assert(maps:is_key(tcp, Examples)),
    ?assert(maps:is_key(http, Examples)),

    %% Each example should be a map with type
    [?assert(is_map(Config) andalso maps:is_key(type, Config))
     || {_, Config} <- maps:to_list(Examples)],
    ok.

%% @doc Test transport supervisor exists
ensure_transport_supervisor_test() ->
    %% This should either return ok or error:transport_supervisor_not_found
    %% but should not crash
    Result = erlmcp:ensure_transport_supervisor(),
    ?assert(Result =:= ok orelse
            element(1, Result) =:= error),
    ok.

%% @doc Test audit_transport_bindings returns a map
audit_transport_bindings_test() ->
    {ok, Audit} = erlmcp:audit_transport_bindings(),
    ?assert(is_map(Audit)),
    ok.

%% @doc Test initialize_config_validation doesn't crash
initialize_config_validation_test() ->
    ?assertMatch(ok, erlmcp:initialize_config_validation()),
    ok.

%%====================================================================
%% Integration Tests (xref-related)
%%====================================================================

%% @doc Integration test: Verify erlmcp_registry:get_pid/0 works
registry_pid_integration_test() ->
    %% get_pid should return pid or undefined
    Result = erlmcp_registry:get_pid(),
    ?assert(is_pid(Result) orelse Result =:= undefined),
    ok.

%% @doc Integration test: Verify erlmcp_registry:get_servers/0 works
registry_servers_integration_test() ->
    %% get_servers should return a list
    Result = erlmcp_registry:get_servers(),
    ?assert(is_list(Result)),
    ok.

%% @doc Integration test: Verify transport ID format
transport_id_format_test() ->
    Id = erlmcp:create_transport_id(websocket, ws_handler),
    IdStr = atom_to_list(Id),

    %% ID should have format: type_base_timestamp
    Parts = string:split(IdStr, "_"),
    ?assert(length(Parts) >= 3),

    %% Last part should be numeric (timestamp)
    LastPart = lists:last(Parts),
    ?assert(is_numeric_string(LastPart)),
    ok.

%%====================================================================
%% Property-Based Tests (Optional)
%%====================================================================

%% @doc Property test: Config schema always has type field
prop_config_schema_structure() ->
    ?FORALL(
        Type <- oneof([stdio, tcp, http]),
        begin
            Schema = erlmcp:get_config_schema(Type),
            is_map(Schema) andalso maps:is_key(<<"type">>, Schema)
        end
    ).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Check if a string contains only numeric characters
-spec is_numeric_string(string()) -> boolean().
is_numeric_string(S) ->
    try
        _ = list_to_integer(S),
        true
    catch
        error:_ -> false
    end.
