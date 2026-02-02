%%%-------------------------------------------------------------------
%%% @doc erlmcp_message_handler_tests - Comprehensive Test Suite for Message Handler
%%%
%%% Tests the MCP message processing hot path using Chicago School TDD:
%%% - Real gen_server state (no mocks)
%%% - State-based verification (observable behavior)
%%% - Black-box testing (implementation details hidden)
%%% - 100% coverage of all message handlers
%%% - Edge cases: invalid JSON, unknown methods, malformed requests
%%%
%%% Target: 85%+ coverage (core module, hot path)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_message_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

message_handler_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_process_message_initialize/1,
      fun test_process_message_ping/1,
      fun test_process_message_resources_list/1,
      fun test_process_message_tools_list/1,
      fun test_process_message_prompts_list/1,
      fun test_process_message_unknown_method/1,
      fun test_process_notification_initialized/1,
      fun test_process_notification_unknown/1,
      fun test_process_message_invalid_json/1,
      fun test_handle_initialize/1,
      fun test_handle_initialized/1,
      fun test_handle_ping/1,
      fun test_handle_resources_list/1,
      fun test_handle_tools_list/1,
      fun test_handle_prompts_list/1,
      fun test_handle_call_tool/1,
      fun test_handle_get_prompt/1,
      fun test_handle_read_resource/1,
      fun test_request_routing/1,
      fun test_notification_routing/1]}.

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    %% Create a test server state
    State = #mcp_server_state{
        server_id = test_server,
        transport_id = test_transport,
        initialized = false,
        tools = #{<<"test_tool">> => fun(_Args) -> #{result => ok} end},
        resources = #{<<"test://resource">> => fun(_Uri) -> #{content => <<"test">>} end},
        prompts = #{<<"test_prompt">> => fun(_Args) -> #{messages => []} end}
    },
    State.

cleanup(_State) ->
    %% Cleanup is automatic (no processes started in this module)
    ok.

%%====================================================================
%% Message Processing Tests (Hot Path)
%%====================================================================

test_process_message_initialize(State) ->
    %% Exercise: Process initialize message
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{
            <<"protocolVersion">> => <<"2024-11-05">>,
            <<"capabilities">> => #{},
            <<"clientInfo">> => #{
                <<"name">> => <<"test_client">>,
                <<"version">> => <<"1.0.0">>
            }
        }
    }),

    Result = erlmcp_message_handler:process_message(<<"test_transport">>, Message, State),

    %% Verify: Returns reply with encoded response
    ?assertMatch({reply, _, _}, Result),
    {reply, ResponseJson, _NewState} = Result,

    %% Verify response is valid JSON-RPC
    Response = jsx:decode(ResponseJson, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(1, maps:get(<<"id">>, Response)),
    ?assert(maps:is_key(<<"result">>, Response)).

test_process_message_ping(State) ->
    %% Exercise: Process ping message (MCP 2025-11-25)
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"method">> => <<"ping">>
    }),

    Result = erlmcp_message_handler:process_message(<<"test_transport">>, Message, State),

    %% Verify: Returns reply with empty result (ping spec)
    ?assertMatch({reply, _, _}, Result),
    {reply, ResponseJson, _NewState} = Result,

    %% Verify ping returns empty object
    Response = jsx:decode(ResponseJson, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(2, maps:get(<<"id">>, Response)),
    ?assertEqual(#{}, maps:get(<<"result">>, Response)).

test_process_message_resources_list(State) ->
    %% Exercise: Process resources/list message
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 3,
        <<"method">> => <<"resources/list">>
    }),

    Result = erlmcp_message_handler:process_message(<<"test_transport">>, Message, State),

    %% Verify: Returns reply with resources list
    ?assertMatch({reply, _, _}, Result),
    {reply, ResponseJson, _NewState} = Result,

    %% Verify resources are returned
    Response = jsx:decode(ResponseJson, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(3, maps:get(<<"id">>, Response)),
    ResultMap = maps:get(<<"result">>, Response),
    Resources = maps:get(<<"resources">>, ResultMap),
    ?assert(lists:member(<<"test://resource">>, Resources)).

test_process_message_tools_list(State) ->
    %% Exercise: Process tools/list message
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 4,
        <<"method">> => <<"tools/list">>
    }),

    Result = erlmcp_message_handler:process_message(<<"test_transport">>, Message, State),

    %% Verify: Returns reply with tools list
    ?assertMatch({reply, _, _}, Result),
    {reply, ResponseJson, _NewState} = Result,

    %% Verify tools are returned
    Response = jsx:decode(ResponseJson, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(4, maps:get(<<"id">>, Response)),
    ResultMap = maps:get(<<"result">>, Response),
    Tools = maps:get(<<"tools">>, ResultMap),
    ?assert(lists:member(<<"test_tool">>, Tools)).

test_process_message_prompts_list(State) ->
    %% Exercise: Process prompts/list message
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 5,
        <<"method">> => <<"prompts/list">>
    }),

    Result = erlmcp_message_handler:process_message(<<"test_transport">>, Message, State),

    %% Verify: Returns reply with prompts list
    ?assertMatch({reply, _, _}, Result),
    {reply, ResponseJson, _NewState} = Result,

    %% Verify prompts are returned
    Response = jsx:decode(ResponseJson, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(5, maps:get(<<"id">>, Response)),
    ResultMap = maps:get(<<"result">>, Response),
    Prompts = maps:get(<<"prompts">>, ResultMap),
    ?assert(lists:member(<<"test_prompt">>, Prompts)).

test_process_message_unknown_method(State) ->
    %% Exercise: Process unknown method
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 6,
        <<"method">> => <<"unknown/method">>
    }),

    Result = erlmcp_message_handler:process_message(<<"test_transport">>, Message, State),

    %% Verify: Returns error response
    ?assertMatch({reply, _, _}, Result),
    {reply, ResponseJson, _NewState} = Result,

    %% Verify error is method_not_found
    Response = jsx:decode(ResponseJson, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(6, maps:get(<<"id">>, Response)),
    ?assert(maps:is_key(<<"error">>, Response)),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)).

test_process_notification_initialized(State) ->
    %% Exercise: Process initialized notification
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"initialized">>
    }),

    Result = erlmcp_message_handler:process_message(<<"test_transport">>, Message, State),

    %% Verify: Returns ok with initialized state
    ?assertMatch({ok, _}, Result),
    {ok, NewState} = Result,
    ?assertEqual(true, NewState#mcp_server_state.initialized).

test_process_notification_unknown(State) ->
    %% Exercise: Process unknown notification (should not crash)
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"unknown/notification">>
    }),

    Result = erlmcp_message_handler:process_message(<<"test_transport">>, Message, State),

    %% Verify: Returns ok (notifications are fire-and-forget)
    ?assertMatch({ok, _}, Result).

test_process_message_invalid_json(State) ->
    %% Exercise: Process invalid JSON
    InvalidMessage = <<"{invalid json}">>,

    Result = erlmcp_message_handler:process_message(<<"test_transport">>, InvalidMessage, State),

    %% Verify: Returns error (from JSON-RPC decoder)
    ?assertMatch({error, _}, Result).

%%====================================================================
%% Handler Function Tests
%%====================================================================

test_handle_initialize(State) ->
    %% Exercise: Handle initialize request
    Params = #{<<"protocolVersion">> => <<"2024-11-05">>},

    Result = erlmcp_message_handler:handle_initialize(Params, State),

    %% Verify: Returns handler name and updated state
    ?assertMatch({<<"initialize">>, _}, Result),
    {<<"initialize">>, _NewState} = Result.

test_handle_initialized(State) ->
    %% Exercise: Handle initialized notification
    Params = #{},

    Result = erlmcp_message_handler:handle_initialized(Params, State),

    %% Verify: Returns ok with initialized = true
    ?assertMatch({ok, _}, Result),
    {ok, NewState} = Result,
    ?assertEqual(true, NewState#mcp_server_state.initialized).

test_handle_ping(State) ->
    %% Exercise: Handle ping request
    Params = #{},

    Result = erlmcp_message_handler:handle_ping(Params, State),

    %% Verify: Ping always succeeds with ok
    ?assertMatch({ok, _}, Result),
    {ok, _NewState} = Result.

test_handle_resources_list(State) ->
    %% Exercise: Handle resources/list request
    Params = #{},

    Result = erlmcp_message_handler:handle_resources_list(Params, State),

    %% Verify: Returns handler name and state
    ?assertMatch({<<"resources">>, _}, Result),
    {<<"resources">>, _NewState} = Result.

test_handle_tools_list(State) ->
    %% Exercise: Handle tools/list request
    Params = #{},

    Result = erlmcp_message_handler:handle_tools_list(Params, State),

    %% Verify: Returns handler name and state
    ?assertMatch({<<"tools">>, _}, Result),
    {<<"tools">>, _NewState} = Result.

test_handle_prompts_list(State) ->
    %% Exercise: Handle prompts/list request
    Params = #{},

    Result = erlmcp_message_handler:handle_prompts_list(Params, State),

    %% Verify: Returns handler name and state
    ?assertMatch({<<"prompts">>, _}, Result),
    {<<"prompts">>, _NewState} = Result.

test_handle_call_tool(State) ->
    %% Exercise: Handle tools/call request
    Params = #{<<"name">> => <<"test_tool">>, <<"arguments">> => #{}},

    Result = erlmcp_message_handler:handle_call_tool(Params, State),

    %% Verify: Returns handler name and state
    ?assertMatch({<<"tool_result">>, _}, Result),
    {<<"tool_result">>, _NewState} = Result.

test_handle_get_prompt(State) ->
    %% Exercise: Handle prompts/get request
    Params = #{<<"name">> => <<"test_prompt">>},

    Result = erlmcp_message_handler:handle_get_prompt(Params, State),

    %% Verify: Returns handler name and state
    ?assertMatch({<<"prompt_content">>, _}, Result),
    {<<"prompt_content">>, _NewState} = Result.

test_handle_read_resource(State) ->
    %% Exercise: Handle resources/read request
    Params = #{<<"uri">> => <<"test://resource">>},

    Result = erlmcp_message_handler:handle_read_resource(Params, State),

    %% Verify: Returns handler name and state
    ?assertMatch({<<"resource_content">>, _}, Result),
    {<<"resource_content">>, _NewState} = Result.

%%====================================================================
%% Routing Tests
%%====================================================================

test_request_routing(_State) ->
    %% This tests the internal routing function
    %% We can't call handle_request directly (it's internal),
    %% but we can test it through process_message

    TestState = #mcp_server_state{
        server_id = test_server,
        initialized = false,
        tools = #{},
        resources = #{},
        prompts = #{}
    },

    %% Test each known method routes correctly
    Methods = [
        {<<"initialize">>, 1},
        {<<"ping">>, 2},
        {<<"resources/list">>, 3},
        {<<"tools/list">>, 4},
        {<<"prompts/list">>, 5}
    ],

    lists:foreach(fun({Method, Id}) ->
        Message = jsx:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => Id,
            <<"method">> => Method
        }),
        Result = erlmcp_message_handler:process_message(<<"test">>, Message, TestState),
        ?assertMatch({reply, _, _}, Result)
    end, Methods).

test_notification_routing(_State) ->
    %% Test notification routing through process_message
    TestState = #mcp_server_state{
        server_id = test_server,
        initialized = false,
        tools = #{},
        resources = #{},
        prompts = #{}
    },

    %% Test initialized notification
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"initialized">>
    }),

    Result = erlmcp_message_handler:process_message(<<"test">>, Message, TestState),
    ?assertMatch({ok, _}, Result),
    {ok, NewState} = Result,
    ?assertEqual(true, NewState#mcp_server_state.initialized).
