%% @doc Configuration Validation System Demo
%% Demonstrates the comprehensive configuration validation features in erlmcp
%% 
%% This demo showcases:
%% - Schema-based validation for all transport types
%% - Helpful error messages with suggestions
%% - Configuration examples and documentation
%% - Field-level validation
%% - Error formatting and debugging

-module(config_validation_demo).

-export([run/0, demo_all/0, demo_stdio/0, demo_tcp/0, demo_http/0, 
         demo_errors/0, demo_schemas/0]).

%% @doc Run complete configuration validation demonstration
run() ->
    io:format("~n~n"),
    io:format("===================================================================~n"),
    io:format("           ERLMCP CONFIGURATION VALIDATION DEMO~n"),
    io:format("===================================================================~n~n"),
    
    io:format("This demonstration showcases the comprehensive configuration~n"),
    io:format("validation system implemented in Step 6 of Phase 3.~n~n"),
    
    % Ensure erlmcp application is available
    code:add_patha("_build/default/lib/erlmcp/ebin"),
    
    demo_stdio(),
    demo_tcp(),
    demo_http(),
    demo_schemas(),
    demo_errors(),
    
    io:format("===================================================================~n"),
    io:format("                    DEMO COMPLETED SUCCESSFULLY~n"),
    io:format("===================================================================~n~n").

%% @doc Demo all features in sequence
demo_all() ->
    run().

%% @doc Demonstrate STDIO transport validation
demo_stdio() ->
    io:format("-------------------------------------------------------------------~n"),
    io:format("1. STDIO TRANSPORT VALIDATION~n"),
    io:format("-------------------------------------------------------------------~n"),
    
    % Valid minimal config
    io:format("Testing valid minimal STDIO config:~n"),
    ValidMinimal = #{type => stdio},
    io:format("  Config: ~p~n", [ValidMinimal]),
    case erlmcp:validate_transport_config(ValidMinimal) of
        ok -> io:format("  ✓ Validation passed~n");
        Error -> io:format("  ✗ Validation failed: ~p~n", [Error])
    end,
    
    % Valid full config
    io:format("~nTesting valid full STDIO config:~n"),
    ValidFull = #{type => stdio, server_id => demo_server, test_mode => true, buffer_size => 4096},
    io:format("  Config: ~p~n", [ValidFull]),
    case erlmcp:validate_transport_config(ValidFull) of
        ok -> io:format("  ✓ Validation passed~n");
        Error -> io:format("  ✗ Validation failed: ~p~n", [Error])
    end,
    
    % Invalid config
    io:format("~nTesting invalid STDIO config (bad test_mode):~n"),
    Invalid = #{type => stdio, test_mode => \"not_boolean\"},
    io:format("  Config: ~p~n", [Invalid]),
    case erlmcp:validate_transport_config(Invalid) of
        ok -> io:format("  ✗ Validation should have failed~n");
        {error, {validation_error, ErrorType, Field, Message}} ->
            io:format("  ✓ Validation correctly failed~n"),
            io:format("    Error Type: ~p~n", [ErrorType]),
            io:format("    Field: ~p~n", [Field]),
            io:format("    Message: ~s~n", [Message])
    end,
    io:format("~n").

%% @doc Demonstrate TCP transport validation
demo_tcp() ->
    io:format("-------------------------------------------------------------------~n"),
    io:format("2. TCP TRANSPORT VALIDATION~n"),
    io:format("-------------------------------------------------------------------~n"),
    
    % Valid config
    io:format("Testing valid TCP config:~n"),
    Valid = #{type => tcp, host => \"localhost\", port => 8080, keepalive => true},
    io:format("  Config: ~p~n", [Valid]),
    case erlmcp:validate_transport_config(Valid) of
        ok -> io:format("  ✓ Validation passed~n");
        Error -> io:format("  ✗ Validation failed: ~p~n", [Error])
    end,
    
    % Invalid port
    io:format("~nTesting invalid TCP config (bad port):~n"),
    InvalidPort = #{type => tcp, host => \"localhost\", port => 70000},
    io:format("  Config: ~p~n", [InvalidPort]),
    case erlmcp:validate_transport_config(InvalidPort) of
        ok -> io:format("  ✗ Validation should have failed~n");
        {error, {validation_error, ErrorType, Field, Message}} ->
            io:format("  ✓ Validation correctly failed~n"),
            io:format("    Error Type: ~p~n", [ErrorType]),
            io:format("    Field: ~p~n", [Field]),
            io:format("    Message: ~s~n", [Message])
    end,
    
    % Missing required field
    io:format("~nTesting TCP config with missing host:~n"),
    MissingHost = #{type => tcp, port => 8080},
    io:format("  Config: ~p~n", [MissingHost]),
    case erlmcp:validate_transport_config(MissingHost) of
        ok -> io:format("  ✗ Validation should have failed~n");
        {error, {validation_error, ErrorType, Field, Message}} ->
            io:format("  ✓ Validation correctly failed~n"),
            io:format("    Error Type: ~p~n", [ErrorType]),
            io:format("    Field: ~p~n", [Field]),
            io:format("    Message: ~s~n", [Message])
    end,
    io:format("~n").

%% @doc Demonstrate HTTP transport validation
demo_http() ->
    io:format("-------------------------------------------------------------------~n"),
    io:format("3. HTTP TRANSPORT VALIDATION~n"),
    io:format("-------------------------------------------------------------------~n"),
    
    % Valid config
    io:format("Testing valid HTTP config:~n"),
    Valid = #{
        type => http, 
        url => \"https://api.example.com/mcp\",
        method => post,
        headers => #{<<\"Content-Type\">> => <<\"application/json\">>},
        timeout => 30000
    },
    io:format("  Config: ~p~n", [Valid]),
    case erlmcp:validate_transport_config(Valid) of
        ok -> io:format("  ✓ Validation passed~n");
        Error -> io:format("  ✗ Validation failed: ~p~n", [Error])
    end,
    
    % Invalid URL
    io:format("~nTesting invalid HTTP config (bad URL):~n"),
    InvalidUrl = #{type => http, url => \"not-a-valid-url\"},
    io:format("  Config: ~p~n", [InvalidUrl]),
    case erlmcp:validate_transport_config(InvalidUrl) of
        ok -> io:format("  ✗ Validation should have failed~n");
        {error, {validation_error, ErrorType, Field, Message}} ->
            io:format("  ✓ Validation correctly failed~n"),
            io:format("    Error Type: ~p~n", [ErrorType]),
            io:format("    Field: ~p~n", [Field]),
            io:format("    Message: ~s~n", [Message])
    end,
    
    % Invalid CORS config
    io:format("~nTesting HTTP config with invalid CORS:~n"),
    InvalidCors = #{type => http, url => \"https://api.example.com\", cors => \"invalid\"},
    io:format("  Config: ~p~n", [InvalidCors]),
    case erlmcp:validate_transport_config(InvalidCors) of
        ok -> io:format("  ✗ Validation should have failed~n");
        {error, {validation_error, ErrorType, Field, Message}} ->
            io:format("  ✓ Validation correctly failed~n"),
            io:format("    Error Type: ~p~n", [ErrorType]),
            io:format("    Field: ~p~n", [Field]),
            io:format("    Message: ~s~n", [Message])
    end,
    io:format("~n").

%% @doc Demonstrate schema access and field validation
demo_schemas() ->
    io:format("-------------------------------------------------------------------~n"),
    io:format("4. SCHEMA ACCESS AND FIELD VALIDATION~n"),
    io:format("-------------------------------------------------------------------~n"),
    
    % List supported transport types
    io:format("Supported transport types:~n"),
    Types = erlmcp:list_supported_transport_types(),
    io:format("  ~p~n", [Types]),
    
    % Get schema for each type
    lists:foreach(fun(Type) ->
        io:format("~nSchema for ~p transport:~n", [Type]),
        case erlmcp:get_config_schema(Type) of
            {ok, Schema} ->
                RequiredFields = maps:get(required_fields, Schema),
                OptionalFields = maps:get(optional_fields, Schema),
                Description = maps:get(description, Schema),
                io:format("  Description: ~s~n", [Description]),
                io:format("  Required fields: ~p~n", [RequiredFields]),
                io:format("  Optional fields: ~p~n", [OptionalFields]);
            Error ->
                io:format("  Error getting schema: ~p~n", [Error])
        end
    end, Types),
    
    % Configuration examples
    io:format("~nConfiguration examples:~n"),
    Examples = erlmcp:get_config_examples(),
    maps:fold(fun(Type, Config, _) ->
        io:format("  ~p: ~p~n", [Type, Config])
    end, ok, Examples),
    
    % Individual field validation
    io:format("~nIndividual field validation examples:~n"),
    FieldTests = [
        {tcp, port, 8080, \"valid port\"},
        {tcp, port, -1, \"invalid port\"},
        {http, method, get, \"valid method\"},
        {http, method, invalid, \"invalid method\"},
        {stdio, test_mode, true, \"valid test_mode\"},
        {stdio, test_mode, \"invalid\", \"invalid test_mode\"}
    ],
    
    lists:foreach(fun({Type, Field, Value, Description}) ->
        io:format("  Testing ~s (~p:~p = ~p): ", [Description, Type, Field, Value]),
        case erlmcp:validate_config_field(Type, Field, Value) of
            ok -> io:format("✓ valid~n");
            {error, _} -> io:format("✗ invalid~n")
        end
    end, FieldTests),
    io:format("~n").

%% @doc Demonstrate comprehensive error handling
demo_errors() ->
    io:format("-------------------------------------------------------------------~n"),
    io:format("5. ERROR HANDLING AND MESSAGES~n"),
    io:format("-------------------------------------------------------------------~n"),
    
    ErrorCases = [
        {\"Missing transport type\", #{}},
        {\"Unknown transport type\", #{type => unknown_type}},
        {\"Unknown field\", #{type => stdio, unknown_field => value}},
        {\"Missing required field\", #{type => tcp, port => 8080}},
        {\"Invalid field type\", #{type => stdio, test_mode => \"not_boolean\"}},
        {\"Out of range value\", #{type => tcp, host => \"localhost\", port => 99999}},
        {\"Invalid URL format\", #{type => http, url => \"not-a-url\"}}
    ],
    
    lists:foreach(fun({Description, Config}) ->
        io:format("Testing ~s:~n", [Description]),
        io:format("  Config: ~p~n", [Config]),
        case erlmcp:validate_transport_config(Config) of
            ok -> 
                io:format("  ✗ Should have failed~n");
            {error, {validation_error, ErrorType, Field, Message}} ->
                io:format("  ✓ Correctly failed with detailed error:~n"),
                io:format("    Error Type: ~p~n", [ErrorType]),
                io:format("    Field: ~p~n", [Field]),
                io:format("    Message: ~s~n", [Message]);
            {error, Other} ->
                io:format("  ✓ Failed with error: ~p~n", [Other])
        end,
        io:format("~n")
    end, ErrorCases),
    io:format("~n").