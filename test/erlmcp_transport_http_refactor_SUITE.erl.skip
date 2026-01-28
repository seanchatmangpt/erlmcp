%%%-------------------------------------------------------------------
%%% @doc
%%% Test suite for the refactored HTTP transport implementation
%%%
%%% Tests the simplified gen_server pattern with registry integration
%%% and standard behavior callbacks.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_http_refactor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% CT callbacks
-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_behavior_init_1/1,
    test_behavior_init_2/1,
    test_standard_callbacks/1,
    test_registry_integration/1,
    test_error_handling/1,
    test_state_management/1,
    test_transport_info/1,
    test_http_client_functionality/1,
    test_configuration_validation/1,
    test_timeout_handling/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [
        {group, behavior_compliance},
        {group, registry_integration},
        {group, error_handling},
        {group, http_functionality}
    ].

groups() ->
    [
        {behavior_compliance, [parallel], [
            test_behavior_init_1,
            test_behavior_init_2,
            test_standard_callbacks,
            test_state_management,
            test_transport_info
        ]},
        {registry_integration, [sequence], [
            test_registry_integration
        ]},
        {error_handling, [parallel], [
            test_error_handling,
            test_configuration_validation,
            test_timeout_handling
        ]},
        {http_functionality, [parallel], [
            test_http_client_functionality
        ]}
    ].

init_per_suite(Config) ->
    % Start necessary applications
    application:start(inets),
    application:start(ssl),
    Config.

end_per_suite(_Config) ->
    application:stop(ssl),
    application:stop(inets),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test the new init/1 behavior callback
test_behavior_init_1(_Config) ->
    TransportId = http_test_transport_1,
    Config = #{
        transport_id => TransportId,
        url => "https://example.com/mcp",
        timeout => 5000
    },
    
    % Test successful initialization
    {ok, State} = erlmcp_transport_http:init(Config),
    
    % Verify state structure
    ?assertMatch(#{transport_id := TransportId}, 
                 erlmcp_transport_http:get_info(State)),
    
    % Clean up
    ok = erlmcp_transport_http:close(State).

%% @doc Test the legacy init/2 behavior callback
test_behavior_init_2(_Config) ->
    TransportId = http_test_transport_2,
    Config = #{
        url => "https://example.com/mcp",
        timeout => 5000
    },
    
    % Test successful initialization
    {ok, State} = erlmcp_transport_http:init(TransportId, Config),
    
    % Verify state structure
    Info = erlmcp_transport_http:get_info(State),
    ?assertEqual(TransportId, maps:get(transport_id, Info)),
    ?assertEqual(http, maps:get(type, Info)),
    
    % Clean up
    ok = erlmcp_transport_http:close(State).

%% @doc Test all standard behavior callbacks are implemented
test_standard_callbacks(_Config) ->
    % Check that all required callbacks are exported
    Exports = erlmcp_transport_http:module_info(exports),
    
    ?assert(lists:member({init, 1}, Exports)),
    ?assert(lists:member({init, 2}, Exports)),
    ?assert(lists:member({send, 2}, Exports)),
    ?assert(lists:member({close, 1}, Exports)),
    ?assert(lists:member({get_info, 1}, Exports)),
    ?assert(lists:member({handle_transport_call, 2}, Exports)).

%% @doc Test registry integration using behavior helpers
test_registry_integration(_Config) ->
    % Note: This test assumes a mock registry or tests without actual registry
    TransportId = http_test_registry,
    Config = #{
        transport_id => TransportId,
        url => "https://example.com/mcp",
        timeout => 5000
    },
    
    % Initialize transport (should attempt registry registration)
    {ok, State} = erlmcp_transport_http:init(Config),
    
    % Verify transport info includes registry-related fields
    Info = erlmcp_transport_http:get_info(State),
    ?assertEqual(http, maps:get(type, Info)),
    ?assert(maps:is_key(capabilities, Info)),
    ?assert(lists:member(client, maps:get(capabilities, Info))),
    
    % Clean up (should attempt registry unregistration)
    ok = erlmcp_transport_http:close(State).

%% @doc Test error handling and state tracking
test_error_handling(_Config) ->
    TransportId = http_test_error,
    Config = #{
        transport_id => TransportId,
        url => "https://nonexistent.invalid.domain.test/mcp",
        timeout => 1000  % Short timeout for faster test
    },
    
    {ok, State} = erlmcp_transport_http:init(Config),
    
    % Test sending to non-existent server should fail
    {error, _Reason} = erlmcp_transport_http:send(State, <<"{\"test\": \"data\"}">>),
    
    % Verify error is tracked in state
    Info = erlmcp_transport_http:get_info(State),
    ?assertEqual(error, maps:get(connection_state, Info)),
    ?assert(maps:is_key(last_error, Info)),
    
    ok = erlmcp_transport_http:close(State).

%% @doc Test state management and simplified record
test_state_management(_Config) ->
    TransportId = http_test_state,
    Config = #{
        transport_id => TransportId,
        url => "https://example.com/mcp",
        timeout => 5000
    },
    
    {ok, State} = erlmcp_transport_http:init(Config),
    Info = erlmcp_transport_http:get_info(State),
    
    % Verify all required state fields are present
    ?assert(maps:is_key(transport_id, Info)),
    ?assert(maps:is_key(type, Info)),
    ?assert(maps:is_key(connection_state, Info)),
    ?assert(maps:is_key(statistics, Info)),
    ?assert(maps:is_key(started_at, Info)),
    ?assert(maps:is_key(last_activity, Info)),
    
    % Verify connection_state is properly set
    ?assertEqual(connected, maps:get(connection_state, Info)),
    
    ok = erlmcp_transport_http:close(State).

%% @doc Test transport info completeness
test_transport_info(_Config) ->
    TransportId = http_test_info,
    Config = #{
        transport_id => TransportId,
        url => "https://example.com/mcp",
        timeout => 5000,
        headers => [{"X-Custom", "test"}]
    },
    
    {ok, State} = erlmcp_transport_http:init(Config),
    Info = erlmcp_transport_http:get_info(State),
    
    % Verify transport-specific fields
    ?assertEqual(http, maps:get(type, Info)),
    ?assertEqual("https://example.com/mcp", maps:get(peer, Info)),
    ?assertEqual(<<"1.0.0">>, maps:get(version, Info)),
    
    % Verify capabilities
    Capabilities = maps:get(capabilities, Info),
    ?assert(lists:member(client, Capabilities)),
    ?assert(lists:member(json_rpc, Capabilities)),
    ?assert(lists:member(ssl, Capabilities)),
    
    % Verify statistics structure
    Stats = maps:get(statistics, Info),
    ?assert(maps:is_key(messages_sent, Stats)),
    ?assert(maps:is_key(messages_received, Stats)),
    ?assert(maps:is_key(errors, Stats)),
    
    ok = erlmcp_transport_http:close(State).

%% @doc Test HTTP client functionality (without actual HTTP calls)
test_http_client_functionality(_Config) ->
    TransportId = http_test_client,
    Config = #{
        transport_id => TransportId,
        url => "https://httpbin.org/post",  % Use httpbin for testing if available
        timeout => 10000
    },
    
    {ok, State} = erlmcp_transport_http:init(Config),
    
    % Test transport-specific calls
    {reply, {ok, Url}, _NewState} = 
        erlmcp_transport_http:handle_transport_call(get_url, State),
    ?assertEqual("https://httpbin.org/post", Url),
    
    % Test header setting
    NewHeaders = [{"X-Test", "value"}],
    {reply, ok, _NewState2} = 
        erlmcp_transport_http:handle_transport_call({set_headers, NewHeaders}, State),
    
    ok = erlmcp_transport_http:close(State).

%% @doc Test configuration validation
test_configuration_validation(_Config) ->
    % Test missing URL
    Config1 = #{transport_id => test_transport},
    {error, missing_url} = erlmcp_transport_http:init(Config1),
    
    % Test invalid URL
    Config2 = #{
        transport_id => test_transport,
        url => "invalid-url"
    },
    {error, invalid_url_format} = erlmcp_transport_http:init(Config2),
    
    % Test invalid timeout
    Config3 = #{
        transport_id => test_transport,
        url => "https://example.com/mcp",
        timeout => -1
    },
    {error, {invalid_timeout, -1}} = erlmcp_transport_http:init(Config3).

%% @doc Test timeout handling
test_timeout_handling(_Config) ->
    TransportId = http_test_timeout,
    Config = #{
        transport_id => TransportId,
        url => "https://example.com/mcp",
        timeout => 5000,
        connect_timeout => 2000
    },
    
    {ok, State} = erlmcp_transport_http:init(Config),
    
    % Test timeout setting via transport call
    {reply, ok, NewState} = 
        erlmcp_transport_http:handle_transport_call({set_timeout, 10000}, State),
    
    % Verify the timeout was updated (indirectly through state)
    ?assertNotEqual(State, NewState),
    
    ok = erlmcp_transport_http:close(NewState).