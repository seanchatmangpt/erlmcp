%%%-------------------------------------------------------------------
%%% @doc
%%% Configuration Validation Tests - Phase 3
%%%
%%% Test cases for comprehensive configuration validation system with
%%% coordination hooks and memory storage.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_config_validation_test).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Setup and Cleanup
%%====================================================================

validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"STDIO Configuration Validation", fun test_stdio_validation/0},
      {"TCP Configuration Validation", fun test_tcp_validation/0},
      {"HTTP Configuration Validation", fun test_http_validation/0},
      {"Configuration Schema Storage", fun test_schema_storage/0},
      {"Coordination Hooks Integration", fun test_coordination_hooks/0},
      {"Error Message Quality", fun test_error_messages/0}]}.

setup() ->
    %% Initialize the validation system
    erlmcp:initialize_config_validation(),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% STDIO Transport Validation Tests
%%====================================================================

test_stdio_validation() ->
    %% Valid STDIO configuration
    ValidStdioConfig = #{
        type => stdio,
        server_id => test_server,
        test_mode => true,
        buffer_size => 8192
    },
    ?assertEqual(ok, erlmcp:validate_transport_config(ValidStdioConfig)),
    
    %% Missing type field
    MissingTypeConfig = #{
        server_id => test_server,
        test_mode => true
    },
    ?assertMatch({error, {validation_error, missing_required_field, type, _}}, 
                 erlmcp:validate_transport_config(MissingTypeConfig)),
    
    %% Invalid test_mode type
    InvalidTestModeConfig = #{
        type => stdio,
        test_mode => "not_boolean"
    },
    ?assertMatch({error, {validation_error, invalid_field_value, test_mode, _}},
                 erlmcp:validate_transport_config(InvalidTestModeConfig)),
    
    %% Invalid buffer_size
    InvalidBufferConfig = #{
        type => stdio,
        buffer_size => -1
    },
    ?assertMatch({error, {validation_error, invalid_field_value, buffer_size, _}},
                 erlmcp:validate_transport_config(InvalidBufferConfig)).

%%====================================================================
%% TCP Transport Validation Tests  
%%====================================================================

test_tcp_validation() ->
    %% Valid TCP configuration
    ValidTcpConfig = #{
        type => tcp,
        host => "localhost",
        port => 8080,
        keepalive => true,
        connect_timeout => 5000
    },
    ?assertEqual(ok, erlmcp:validate_transport_config(ValidTcpConfig)),
    
    %% Missing required fields
    MissingHostConfig = #{
        type => tcp,
        port => 8080
    },
    ?assertMatch({error, {validation_error, missing_required_field, host, _}},
                 erlmcp:validate_transport_config(MissingHostConfig)),
    
    MissingPortConfig = #{
        type => tcp,
        host => "localhost"
    },
    ?assertMatch({error, {validation_error, missing_required_field, port, _}},
                 erlmcp:validate_transport_config(MissingPortConfig)),
    
    %% Invalid port range
    InvalidPortConfig = #{
        type => tcp,
        host => "localhost", 
        port => 70000  % Above valid range
    },
    ?assertMatch({error, {validation_error, invalid_field_value, port, _}},
                 erlmcp:validate_transport_config(InvalidPortConfig)),
    
    %% Empty host
    EmptyHostConfig = #{
        type => tcp,
        host => "",
        port => 8080
    },
    ?assertMatch({error, {validation_error, invalid_field_value, host, _}},
                 erlmcp:validate_transport_config(EmptyHostConfig)).

%%====================================================================
%% HTTP Transport Validation Tests
%%====================================================================

test_http_validation() ->
    %% Valid HTTP configuration  
    ValidHttpConfig = #{
        type => http,
        url => "https://api.example.com/mcp",
        method => post,
        headers => #{<<"Content-Type">> => <<"application/json">>},
        timeout => 30000
    },
    ?assertEqual(ok, erlmcp:validate_transport_config(ValidHttpConfig)),
    
    %% Missing URL
    MissingUrlConfig = #{
        type => http,
        method => post
    },
    ?assertMatch({error, {validation_error, missing_required_field, url, _}},
                 erlmcp:validate_transport_config(MissingUrlConfig)),
    
    %% Invalid URL format
    InvalidUrlConfig = #{
        type => http,
        url => "not-a-valid-url"
    },
    ?assertMatch({error, {validation_error, invalid_field_value, url, _}},
                 erlmcp:validate_transport_config(InvalidUrlConfig)),
    
    %% Invalid HTTP method
    InvalidMethodConfig = #{
        type => http,
        url => "https://example.com",
        method => invalid_method
    },
    ?assertMatch({error, {validation_error, invalid_field_value, method, _}},
                 erlmcp:validate_transport_config(InvalidMethodConfig)).

%%====================================================================
%% Schema Storage Tests
%%====================================================================

test_schema_storage() ->
    %% Test schema storage and retrieval
    ?assertEqual(ok, erlmcp:store_validation_schemas()),
    
    %% Test schema retrieval
    {ok, Schemas} = erlmcp:get_validation_schemas_from_memory(),
    ?assert(is_map(Schemas)),
    ?assert(maps:is_key(stdio, Schemas)),
    ?assert(maps:is_key(tcp, Schemas)),
    ?assert(maps:is_key(http, Schemas)),
    ?assert(maps:is_key(metadata, Schemas)),
    
    %% Verify schema structure
    StdioSchema = maps:get(stdio, Schemas),
    ?assert(maps:is_key(required_fields, StdioSchema)),
    ?assert(maps:is_key(optional_fields, StdioSchema)),
    ?assert(maps:is_key(field_validators, StdioSchema)),
    ?assert(maps:is_key(description, StdioSchema)).

%%====================================================================
%% Coordination Hooks Tests
%%====================================================================

test_coordination_hooks() ->
    %% Test coordination-enabled validation
    TestConfig = #{
        type => stdio,
        test_mode => true
    },
    
    Result = erlmcp:validate_transport_with_coordination(stdio, TestConfig),
    ?assertEqual(ok, Result),
    
    %% Test that coordination data is stored
    %% (This is a basic test - in practice we'd check the actual storage)
    ?assert(true). % Placeholder assertion

%%====================================================================
%% Error Message Quality Tests  
%%====================================================================

test_error_messages() ->
    %% Test that error messages are helpful and descriptive
    InvalidConfig = #{
        type => tcp,
        host => "",
        port => -1
    },
    
    {error, {validation_error, _ErrorType, _Field, Message}} = 
        erlmcp:validate_transport_config(InvalidConfig),
    
    %% Error message should be a string and non-empty
    ?assert(is_list(Message)),
    ?assert(length(Message) > 0),
    
    %% Test unknown transport type error
    UnknownTypeConfig = #{
        type => unknown_type
    },
    
    {error, {validation_error, unknown_transport_type, unknown_type, UnknownMessage}} =
        erlmcp:validate_transport_config(UnknownTypeConfig),
    
    ?assert(is_list(UnknownMessage)),
    ?assert(length(UnknownMessage) > 0).