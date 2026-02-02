%% @doc Comprehensive coverage tests for erlmcp_message_handler
%% Tests all public API functions to achieve 80%+ coverage
%% Chicago School TDD: State-based verification, real processes
-module(erlmcp_message_handler_coverage_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

message_handler_test_() ->
    {setup,
     fun setup_handler/0,
     fun cleanup_handler/1,
     fun run_handler_tests/1}.

setup_handler() ->
    % Create a minimal server state
    #mcp_server_state{
        server_id = test_server,
        phase = initialized,
        capabilities = #mcp_server_capabilities{
            resources = #mcp_resources_capability{},
            tools = #mcp_tools_capability{},
            prompts = #mcp_prompts_capability{}
        },
        resources = #{},
        tools = #{},
        prompts = #{}
    }.

cleanup_handler(_State) ->
    ok.

run_handler_tests(_State) ->
    [
     {"Initialize request", fun test_handle_initialize/0},
     {"Initialized notification", fun test_handle_initialized/0},
     {"Ping request", fun test_handle_ping/0},
     {"Resources list request", fun test_handle_resources_list/0},
     {"Tools list request", fun test_handle_tools_list/0},
     {"Prompts list request", fun test_handle_prompts_list/0},
     {"Tool call request", fun test_handle_call_tool/0},
     {"Get prompt request", fun test_handle_get_prompt/0},
     {"Read resource request", fun test_handle_read_resource/0},
     {"Unknown method request", fun test_handle_unknown_method/0},
     {"Unknown notification", fun test_handle_unknown_notification/0}
    ].

%%%====================================================================
%%% Initialize Handler Tests
%%%====================================================================

test_handle_initialize() ->
    State = #mcp_server_state{
        server_id = test_server,
        phase = initialization
    },

    {Method, _NewState} = erlmcp_message_handler:handle_initialize(#{}, State),
    ?assertEqual(<<"initialize">>, Method).

%%%====================================================================
%%% Initialized Notification Tests
%%%====================================================================

test_handle_initialized() ->
    State = #mcp_server_state{
        server_id = test_server,
        phase = initialization,
        initialized = false
    },

    {ok, NewState} = erlmcp_message_handler:handle_initialized(#{}, State),
    ?assertEqual(true, NewState#mcp_server_state.initialized).

%%%====================================================================
%%% Ping Handler Tests
%%%====================================================================

test_handle_ping() ->
    State = #mcp_server_state{
        server_id = test_server,
        phase = initialized
    },

    {ok, NewState} = erlmcp_message_handler:handle_ping(#{}, State),
    ?assertEqual(State, NewState).

%%%====================================================================
%%% Resources List Handler Tests
%%%====================================================================

test_handle_resources_list() ->
    Resources = #{
        <<"resource1">> => {#mcp_resource{uri = <<"file://test1">>}, fun(_) -> ok end},
        <<"resource2">> => {#mcp_resource{uri = <<"file://test2">>}, fun(_) -> ok end}
    },

    State = #mcp_server_state{
        server_id = test_server,
        phase = initialized,
        resources = Resources
    },

    {Method, _NewState} = erlmcp_message_handler:handle_resources_list(#{}, State),
    ?assertEqual(<<"resources">>, Method).

%%%====================================================================
%%% Tools List Handler Tests
%%%====================================================================

test_handle_tools_list() ->
    Tools = #{
        <<"tool1">> => {#mcp_tool{name = <<"tool1">>}, fun(_) -> ok end, #{}},
        <<"tool2">> => {#mcp_tool{name = <<"tool2">>}, fun(_) -> ok end, #{}}
    },

    State = #mcp_server_state{
        server_id = test_server,
        phase = initialized,
        tools = Tools
    },

    {Method, _NewState} = erlmcp_message_handler:handle_tools_list(#{}, State),
    ?assertEqual(<<"tools">>, Method).

%%%====================================================================
%%% Prompts List Handler Tests
%%%====================================================================

test_handle_prompts_list() ->
    Prompts = #{
        <<"prompt1">> => {#mcp_prompt{name = <<"prompt1">>}, fun(_) -> ok end},
        <<"prompt2">> => {#mcp_prompt{name = <<"prompt2">>}, fun(_) -> ok end}
    },

    State = #mcp_server_state{
        server_id = test_server,
        phase = initialized,
        prompts = Prompts
    },

    {Method, _NewState} = erlmcp_message_handler:handle_prompts_list(#{}, State),
    ?assertEqual(<<"prompts">>, Method).

%%%====================================================================
%%% Tool Call Handler Tests
%%%====================================================================

test_handle_call_tool() ->
    State = #mcp_server_state{
        server_id = test_server,
        phase = initialized,
        tools = #{}
    },

    {Method, _NewState} = erlmcp_message_handler:handle_call_tool(#{}, State),
    ?assertEqual(<<"tool_result">>, Method).

%%%====================================================================
%%% Get Prompt Handler Tests
%%%====================================================================

test_handle_get_prompt() ->
    State = #mcp_server_state{
        server_id = test_server,
        phase = initialized,
        prompts = #{}
    },

    {Method, _NewState} = erlmcp_message_handler:handle_get_prompt(#{}, State),
    ?assertEqual(<<"prompt_content">>, Method).

%%%====================================================================
%%% Read Resource Handler Tests
%%%====================================================================

test_handle_read_resource() ->
    State = #mcp_server_state{
        server_id = test_server,
        phase = initialized,
        resources = #{}
    },

    {Method, _NewState} = erlmcp_message_handler:handle_read_resource(#{}, State),
    ?assertEqual(<<"resource_content">>, Method).

%%%====================================================================
%%% Unknown Method Handler Tests
%%%====================================================================

test_handle_unknown_method() ->
    State = #mcp_server_state{
        server_id = test_server,
        phase = initialized
    },

    % Process message with unknown method
    {reply, Json, _NewState} = erlmcp_message_handler:process_message(
        <<"transport1">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"unknown/method\",\"params\":{}}">>,
        State
    ),

    % Should return method not found error
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(1, maps:get(<<"id">>, Decoded)),
    ?assert(maps:is_key(<<"error">>, Decoded)).

%%%====================================================================
%%% Unknown Notification Handler Tests
%%%====================================================================

test_handle_unknown_notification() ->
    State = #mcp_server_state{
        server_id = test_server,
        phase = initialized
    },

    % Process message with unknown notification
    {ok, NewState} = erlmcp_message_handler:process_message(
        <<"transport1">>,
        <<"{\"jsonrpc\":\"2.0\",\"method\":\"unknown/notification\",\"params\":{}}">>,
        State
    ),

    ?assertEqual(State, NewState).

%%%====================================================================
%%% Process Message Integration Tests
%%%====================================================================

process_message_request_test() ->
    State = #mcp_server_state{
        server_id = test_server,
        phase = initialized,
        tools = #{<<"test_tool">> => {#mcp_tool{name = <<"test_tool">>}, fun(_) -> ok end, #{}}}
    },

    % Process a valid tools/list request
    {reply, Json, _NewState} = erlmcp_message_handler:process_message(
        <<"transport1">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\",\"params\":{}}">>,
        State
    ),

    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(1, maps:get(<<"id">>, Decoded)),
    ?assert(maps:is_key(<<"result">>, Decoded)).

process_message_notification_test() ->
    State = #mcp_server_state{
        server_id = test_server,
        phase = initialization,
        initialized = false
    },

    % Process initialized notification
    {ok, NewState} = erlmcp_message_handler:process_message(
        <<"transport1">>,
        <<"{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\",\"params\":{}}">>,
        State
    ),

    ?assertEqual(true, NewState#mcp_server_state.initialized).

process_message_error_test() ->
    State = #mcp_server_state{
        server_id = test_server,
        phase = initialized
    },

    % Process invalid JSON
    {error, Reason} = erlmcp_message_handler:process_message(
        <<"transport1">>,
        <<"invalid json">>,
        State
    ),

    ?assertMatch({parse_error, _}, Reason).
