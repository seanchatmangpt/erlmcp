%% @doc Simple Configuration Validation Demo
%% Demonstrates the configuration validation system without Unicode characters

-module(simple_config_demo).

-export([run/0]).

%% @doc Run configuration validation demonstration
run() ->
    io:format("~n=== ERLMCP CONFIGURATION VALIDATION DEMO ===~n~n"),
    
    % Ensure erlmcp is available
    code:add_patha("_build/default/lib/erlmcp/ebin"),
    
    demo_valid_configs(),
    demo_invalid_configs(),
    demo_schema_access(),
    
    io:format("=== DEMO COMPLETED ===~n~n").

demo_valid_configs() ->
    io:format("1. Testing Valid Configurations:~n"),
    
    % STDIO config
    StdioConfig = #{type => stdio, server_id => demo_server},
    io:format("  STDIO config: ~p~n", [StdioConfig]),
    Result1 = erlmcp:validate_transport_config(StdioConfig),
    io:format("    Result: ~p~n", [Result1]),
    
    % TCP config
    TcpConfig = #{type => tcp, host => "localhost", port => 8080},
    io:format("  TCP config: ~p~n", [TcpConfig]),
    Result2 = erlmcp:validate_transport_config(TcpConfig),
    io:format("    Result: ~p~n", [Result2]),
    
    % HTTP config
    HttpConfig = #{type => http, url => "https://api.example.com"},
    io:format("  HTTP config: ~p~n", [HttpConfig]),
    Result3 = erlmcp:validate_transport_config(HttpConfig),
    io:format("    Result: ~p~n", [Result3]),
    
    io:format("~n").

demo_invalid_configs() ->
    io:format("2. Testing Invalid Configurations:~n"),
    
    % Missing type
    NoType = #{},
    io:format("  No type config: ~p~n", [NoType]),
    Result1 = erlmcp:validate_transport_config(NoType),
    io:format("    Result: ~p~n", [Result1]),
    
    % Invalid port
    BadPort = #{type => tcp, host => "localhost", port => -1},
    io:format("  Bad port config: ~p~n", [BadPort]),
    Result2 = erlmcp:validate_transport_config(BadPort),
    io:format("    Result: ~p~n", [Result2]),
    
    % Invalid URL
    BadUrl = #{type => http, url => "not-a-url"},
    io:format("  Bad URL config: ~p~n", [BadUrl]),
    Result3 = erlmcp:validate_transport_config(BadUrl),
    io:format("    Result: ~p~n", [Result3]),
    
    io:format("~n").

demo_schema_access() ->
    io:format("3. Schema Access:~n"),
    
    % List supported types
    Types = erlmcp:list_supported_transport_types(),
    io:format("  Supported types: ~p~n", [Types]),
    
    % Get examples
    Examples = erlmcp:get_config_examples(),
    io:format("  Examples: ~p~n", [Examples]),
    
    % Get schema for STDIO
    {ok, StdioSchema} = erlmcp:get_config_schema(stdio),
    RequiredFields = maps:get(required_fields, StdioSchema),
    OptionalFields = maps:get(optional_fields, StdioSchema),
    io:format("  STDIO required fields: ~p~n", [RequiredFields]),
    io:format("  STDIO optional fields: ~p~n", [OptionalFields]),
    
    % Test field validation
    FieldResult1 = erlmcp:validate_config_field(tcp, port, 8080),
    io:format("  validate_config_field(tcp, port, 8080): ~p~n", [FieldResult1]),
    
    FieldResult2 = erlmcp:validate_config_field(tcp, port, -1),
    io:format("  validate_config_field(tcp, port, -1): ~p~n", [FieldResult2]),
    
    io:format("~n").