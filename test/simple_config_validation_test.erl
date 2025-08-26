%% @doc Simple configuration validation test
%% Tests the basic functionality of the new configuration validation system

-module(simple_config_validation_test).

-include_lib("eunit/include/eunit.hrl").

%% Test the basic validation functions directly
config_validation_test_() ->
    [
        {"STDIO config validation", fun test_stdio_validation/0},
        {"TCP config validation", fun test_tcp_validation/0},
        {"HTTP config validation", fun test_http_validation/0},
        {"Schema retrieval", fun test_schema_retrieval/0},
        {"Config examples", fun test_config_examples/0},
        {"Error handling", fun test_error_handling/0}
    ].

test_stdio_validation() ->
    % Test valid STDIO config
    ValidConfig = #{type => stdio},
    ?assertEqual(ok, erlmcp:validate_transport_config(ValidConfig)),
    
    % Test invalid STDIO config
    InvalidConfig = #{type => stdio, test_mode => "not_boolean"},
    Result = erlmcp:validate_transport_config(InvalidConfig),
    ?assertMatch({error, {validation_error, invalid_field_value, test_mode, _}}, Result).

test_tcp_validation() ->
    % Test valid TCP config
    ValidConfig = #{type => tcp, host => "localhost", port => 8080},
    ?assertEqual(ok, erlmcp:validate_transport_config(ValidConfig)),
    
    % Test invalid TCP config - bad port
    InvalidConfig = #{type => tcp, host => "localhost", port => -1},
    Result = erlmcp:validate_transport_config(InvalidConfig),
    ?assertMatch({error, {validation_error, invalid_field_value, port, _}}, Result).

test_http_validation() ->
    % Test valid HTTP config
    ValidConfig = #{type => http, url => "https://api.example.com"},
    ?assertEqual(ok, erlmcp:validate_transport_config(ValidConfig)),
    
    % Test invalid HTTP config - bad URL
    InvalidConfig = #{type => http, url => "not-a-url"},
    Result = erlmcp:validate_transport_config(InvalidConfig),
    ?assertMatch({error, {validation_error, invalid_field_value, url, _}}, Result).

test_schema_retrieval() ->
    % Test schema retrieval
    {ok, StdioSchema} = erlmcp:get_config_schema(stdio),
    ?assert(is_map(StdioSchema)),
    ?assert(maps:is_key(required_fields, StdioSchema)),
    
    {ok, TcpSchema} = erlmcp:get_config_schema(tcp),
    ?assert(is_map(TcpSchema)),
    
    {ok, HttpSchema} = erlmcp:get_config_schema(http),
    ?assert(is_map(HttpSchema)),
    
    % Test unknown schema
    ?assertMatch({error, {unknown_transport_type, unknown}}, 
                 erlmcp:get_config_schema(unknown)).

test_config_examples() ->
    Examples = erlmcp:get_config_examples(),
    ?assert(is_map(Examples)),
    ?assert(maps:is_key(stdio, Examples)),
    ?assert(maps:is_key(tcp, Examples)),
    ?assert(maps:is_key(http, Examples)),
    
    % Test that examples are valid
    StdioExample = maps:get(stdio, Examples),
    ?assertEqual(ok, erlmcp:validate_transport_config(StdioExample)),
    
    TcpExample = maps:get(tcp, Examples),
    ?assertEqual(ok, erlmcp:validate_transport_config(TcpExample)),
    
    HttpExample = maps:get(http, Examples),
    ?assertEqual(ok, erlmcp:validate_transport_config(HttpExample)).

test_error_handling() ->
    % Test missing type
    Result1 = erlmcp:validate_transport_config(#{}),
    ?assertMatch({error, {validation_error, missing_required_field, type, _}}, Result1),
    
    % Test unknown transport type
    Result2 = erlmcp:validate_transport_config(#{type => unknown}),
    ?assertMatch({error, {validation_error, unknown_transport_type, unknown, _}}, Result2),
    
    % Test unknown field
    Result3 = erlmcp:validate_transport_config(#{type => stdio, bad_field => value}),
    ?assertMatch({error, {validation_error, unknown_field, bad_field, _}}, Result3).