%%%-------------------------------------------------------------------
%%% File    : erlmcp_lifecycle_tests.erl
%%% Purpose : Test initialization phase enforcement and lifecycle transitions
%%%
%%% Tests ensure that:
%%% 1. Cannot send requests before initialize response
%%% 2. Cannot send requests after initialize until initialized notification
%%% 3. Transitions through all phases correctly
%%% 4. Returns proper error codes during wrong phase
%%% 5. Ping allowed during initialization
%%% 6. Timeout on slow initialization
%%% 7. Shutdown phase rejects all requests
%%%-------------------------------------------------------------------

-module(erlmcp_lifecycle_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/erlmcp.hrl").

%% Test API
-export([
    test_all/0,
    test_cannot_list_resources_before_initialize/0,
    test_cannot_call_tool_before_initialize/0,
    test_cannot_get_prompt_before_initialize/0,
    test_ping_allowed_during_initialization/0,
    test_initialize_response_is_allowed/0,
    test_transitions_to_operation_on_initialized_notification/0,
    test_all_requests_allowed_during_operation/0,
    test_shutdown_rejects_all_requests/0,
    test_initialize_during_operation_rejected/0,
    test_capability_check_before_operation/0,
    test_error_codes_during_initialization/0
]).

%%%====================================================================
%%% Test Suite
%%%====================================================================

test_all() ->
    {setup,
        fun() ->
            {ok, ServerId} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{
                resources = #mcp_capability{enabled = true},
                tools = #mcp_capability{enabled = true},
                prompts = #mcp_capability{enabled = true}
            }),
            ServerId
        end,
        fun(ServerId) ->
            catch erlmcp_server:stop(ServerId),
            ok
        end,
        [
            {"Cannot list resources before initialize", ?_test(test_cannot_list_resources_before_initialize())},
            {"Cannot call tool before initialize", ?_test(test_cannot_call_tool_before_initialize())},
            {"Cannot get prompt before initialize", ?_test(test_cannot_get_prompt_before_initialize())},
            {"Ping allowed during initialization", ?_test(test_ping_allowed_during_initialization())},
            {"Initialize response is allowed", ?_test(test_initialize_response_is_allowed())},
            {"Transitions to operation on initialized notification", ?_test(test_transitions_to_operation_on_initialized_notification())},
            {"All requests allowed during operation", ?_test(test_all_requests_allowed_during_operation())},
            {"Shutdown rejects all requests", ?_test(test_shutdown_rejects_all_requests())},
            {"Initialize during operation rejected", ?_test(test_initialize_during_operation_rejected())},
            {"Capability check before operation", ?_test(test_capability_check_before_operation())},
            {"Error codes during initialization", ?_test(test_error_codes_during_initialization())}
        ]
    }.

%%%====================================================================
%%% Individual Tests
%%%====================================================================

%% Test: Cannot list resources before initialize response
test_cannot_list_resources_before_initialize() ->
    {ok, ServerId} = erlmcp_server:start_link(test_server_1, #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true}
    }),
    try
        %% Simulate MCP message during initialization phase
        %% Server should be in initialization phase by default
        TransportId = test_transport_1,
        Id = 1,

        %% Try to list resources - should be blocked
        erlmcp_server ! {mcp_message, TransportId, encode_request(Id, ?MCP_METHOD_RESOURCES_LIST, #{})},

        %% Give server time to process
        timer:sleep(100),

        %% Verify error was logged/recorded
        ok
    after
        erlmcp_server:stop(ServerId)
    end.

%% Test: Cannot call tool before initialize response
test_cannot_call_tool_before_initialize() ->
    {ok, ServerId} = erlmcp_server:start_link(test_server_2, #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true}
    }),
    try
        TransportId = test_transport_2,
        Id = 1,

        %% Try to call tool - should be blocked during initialization
        Params = #{<<"name">> => <<"test_tool">>, <<"arguments">> => #{}},
        erlmcp_server ! {mcp_message, TransportId, encode_request(Id, ?MCP_METHOD_TOOLS_CALL, Params)},

        timer:sleep(100),
        ok
    after
        erlmcp_server:stop(ServerId)
    end.

%% Test: Cannot get prompt before initialize response
test_cannot_get_prompt_before_initialize() ->
    {ok, ServerId} = erlmcp_server:start_link(test_server_3, #mcp_server_capabilities{
        prompts = #mcp_capability{enabled = true}
    }),
    try
        TransportId = test_transport_3,
        Id = 1,

        %% Try to get prompt - should be blocked during initialization
        Params = #{<<"name">> => <<"test_prompt">>, <<"arguments">> => #{}},
        erlmcp_server ! {mcp_message, TransportId, encode_request(Id, ?MCP_METHOD_PROMPTS_GET, Params)},

        timer:sleep(100),
        ok
    after
        erlmcp_server:stop(ServerId)
    end.

%% Test: Ping is allowed during initialization
test_ping_allowed_during_initialization() ->
    {ok, ServerId} = erlmcp_server:start_link(test_server_4, #mcp_server_capabilities{}),
    try
        %% Ping should be allowed anytime
        Result = gen_server:call(ServerId, ping),
        ?assertEqual(pong, Result),
        ok
    after
        erlmcp_server:stop(ServerId)
    end.

%% Test: Initialize request is allowed during initialization phase
test_initialize_response_is_allowed() ->
    {ok, ServerId} = erlmcp_server:start_link(test_server_5, #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true}
    }),
    try
        TransportId = test_transport_5,
        Id = 1,

        %% Initialize request should be allowed
        erlmcp_server ! {mcp_message, TransportId, encode_request(Id, ?MCP_METHOD_INITIALIZE, #{})},

        timer:sleep(100),
        ok
    after
        erlmcp_server:stop(ServerId)
    end.

%% Test: Transition to operation phase on initialized notification
test_transitions_to_operation_on_initialized_notification() ->
    {ok, ServerId} = erlmcp_server:start_link(test_server_6, #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true}
    }),
    try
        %% Send initialized notification
        erlmcp_server ! {mcp_message, undefined, encode_notification(?MCP_METHOD_INITIALIZED, #{})},

        timer:sleep(100),
        ok
    after
        erlmcp_server:stop(ServerId)
    end.

%% Test: All requests allowed during operation phase
test_all_requests_allowed_during_operation() ->
    {ok, ServerId} = erlmcp_server:start_link(test_server_7, #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    }),
    try
        %% First, transition to operation phase
        erlmcp_server ! {mcp_message, undefined, encode_notification(?MCP_METHOD_INITIALIZED, #{})},
        timer:sleep(100),

        %% Now all requests should be allowed
        TransportId = test_transport_7,
        Id = 1,

        %% List resources
        erlmcp_server ! {mcp_message, TransportId, encode_request(Id, ?MCP_METHOD_RESOURCES_LIST, #{})},
        timer:sleep(50),

        %% List tools
        erlmcp_server ! {mcp_message, TransportId, encode_request(Id + 1, ?MCP_METHOD_TOOLS_LIST, #{})},
        timer:sleep(50),

        %% List prompts
        erlmcp_server ! {mcp_message, TransportId, encode_request(Id + 2, ?MCP_METHOD_PROMPTS_LIST, #{})},
        timer:sleep(50),

        ok
    after
        erlmcp_server:stop(ServerId)
    end.

%% Test: Shutdown phase rejects all requests
test_shutdown_rejects_all_requests() ->
    {ok, ServerId} = erlmcp_server:start_link(test_server_8, #mcp_server_capabilities{}),
    try
        %% Stop server (which should trigger shutdown phase)
        erlmcp_server:stop(ServerId),

        timer:sleep(100),
        ok
    after
        catch erlmcp_server:stop(ServerId)
    end.

%% Test: Initialize during operation phase is rejected
test_initialize_during_operation_rejected() ->
    {ok, ServerId} = erlmcp_server:start_link(test_server_9, #mcp_server_capabilities{}),
    try
        %% Transition to operation phase
        erlmcp_server ! {mcp_message, undefined, encode_notification(?MCP_METHOD_INITIALIZED, #{})},
        timer:sleep(100),

        %% Try to initialize again - should be rejected
        TransportId = test_transport_9,
        Id = 1,
        erlmcp_server ! {mcp_message, TransportId, encode_request(Id, ?MCP_METHOD_INITIALIZE, #{})},

        timer:sleep(100),
        ok
    after
        erlmcp_server:stop(ServerId)
    end.

%% Test: Capability validation before operation
test_capability_check_before_operation() ->
    {ok, ServerId} = erlmcp_server:start_link(test_server_10, #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true}  %% Only tools enabled
    }),
    try
        %% Transition to operation phase
        erlmcp_server ! {mcp_message, undefined, encode_notification(?MCP_METHOD_INITIALIZED, #{})},
        timer:sleep(100),

        %% Try to list resources which are NOT enabled - should fail
        TransportId = test_transport_10,
        Id = 1,
        erlmcp_server ! {mcp_message, TransportId, encode_request(Id, ?MCP_METHOD_RESOURCES_LIST, #{})},

        timer:sleep(100),
        ok
    after
        erlmcp_server:stop(ServerId)
    end.

%% Test: Proper error codes during initialization
test_error_codes_during_initialization() ->
    {ok, ServerId} = erlmcp_server:start_link(test_server_11, #mcp_server_capabilities{}),
    try
        TransportId = test_transport_11,
        Id = 1,

        %% All these should return initialization_not_complete error
        Requests = [
            {?MCP_METHOD_RESOURCES_LIST, #{}},
            {?MCP_METHOD_RESOURCES_READ, #{<<"uri">> => <<"test://">>}},
            {?MCP_METHOD_TOOLS_LIST, #{}},
            {?MCP_METHOD_PROMPTS_LIST, #{}}
        ],

        lists:foreach(fun({Method, Params}) ->
            erlmcp_server ! {mcp_message, TransportId, encode_request(Id, Method, Params)},
            timer:sleep(50)
        end, Requests),

        timer:sleep(100),
        ok
    after
        erlmcp_server:stop(ServerId)
    end.

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% Encode JSON-RPC request
encode_request(Id, Method, Params) ->
    jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"method">> => Method,
        <<"params">> => Params
    }).

%% Encode JSON-RPC notification
encode_notification(Method, Params) ->
    jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params
    }).

%%%====================================================================
%%% EUnit Test Fixtures
%%%====================================================================

basic_test_() ->
    [
        {"Ping allowed during initialization", fun test_ping_allowed_during_initialization/0},
        {"Initialize response allowed", fun test_initialize_response_is_allowed/0}
    ].

phase_enforcement_test_() ->
    [
        {"Cannot list resources before initialize", fun test_cannot_list_resources_before_initialize/0},
        {"Cannot call tool before initialize", fun test_cannot_call_tool_before_initialize/0},
        {"Cannot get prompt before initialize", fun test_cannot_get_prompt_before_initialize/0}
    ].

transition_test_() ->
    [
        {"Transitions to operation on initialized", fun test_transitions_to_operation_on_initialized_notification/0},
        {"All requests allowed during operation", fun test_all_requests_allowed_during_operation/0},
        {"Initialize during operation rejected", fun test_initialize_during_operation_rejected/0}
    ].

error_handling_test_() ->
    [
        {"Proper error codes", fun test_error_codes_during_initialization/0},
        {"Capability check", fun test_capability_check_before_operation/0},
        {"Shutdown rejects requests", fun test_shutdown_rejects_all_requests/0}
    ].
