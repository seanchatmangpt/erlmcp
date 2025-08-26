%% @doc Simple test to verify enhanced validation works
-module(erlmcp_enhanced_validation_test).

-export([test_validation/0, test_enhanced_api/0]).

test_validation() ->
    io:format("Testing transport validation module...~n"),
    
    % Test STDIO validation
    StdioResult = erlmcp_transport_validation:validate_transport_config(stdio, #{buffer_size => 1024}),
    io:format("STDIO validation result: ~p~n", [StdioResult]),
    
    % Test TCP validation with missing port
    TcpResult = erlmcp_transport_validation:validate_transport_config(tcp, #{host => "localhost"}),
    io:format("TCP validation (missing port) result: ~p~n", [TcpResult]),
    
    % Test TCP validation with valid config
    TcpValidResult = erlmcp_transport_validation:validate_transport_config(tcp, #{host => "localhost", port => 8080}),
    io:format("TCP validation (valid) result: ~p~n", [TcpValidResult]),
    
    % Test HTTP validation with invalid URL
    HttpResult = erlmcp_transport_validation:validate_transport_config(http, #{url => "invalid-url"}),
    io:format("HTTP validation (invalid URL) result: ~p~n", [HttpResult]),
    
    % Test HTTP validation with valid config
    HttpValidResult = erlmcp_transport_validation:validate_transport_config(http, #{url => "http://localhost:8000/mcp"}),
    io:format("HTTP validation (valid) result: ~p~n", [HttpValidResult]),
    
    ok.

test_enhanced_api() ->
    io:format("Testing enhanced API error handling...~n"),
    
    % Test error formatting
    ValidationError = {validation_error, missing_field, port, "Port is required"},
    FormattedError = erlmcp:format_validation_error(test_transport, tcp, ValidationError),
    io:format("Formatted validation error: ~p~n", [FormattedError]),
    
    % Test transport error formatting
    TransportError = {port_in_use, 8080},
    FormattedTransportError = erlmcp:format_transport_error(test_transport, tcp, start_failed, TransportError),
    io:format("Formatted transport error: ~p~n", [FormattedTransportError]),
    
    % Test configuration examples
    Examples = erlmcp:get_config_examples(),
    io:format("Configuration examples available for: ~p~n", [maps:keys(Examples)]),
    
    % Test supported transport types
    SupportedTypes = erlmcp:list_supported_transport_types(),
    io:format("Supported transport types: ~p~n", [SupportedTypes]),
    
    ok.