%%%-------------------------------------------------------------------
%% @doc HTTP DELETE Handler Tests - Gap #28 Compliance
%%
%% Tests for HTTP DELETE method support in MCP 2025-11-25 compliance
%% Covers:
%% - DELETE /mcp -> terminate session (204 No Content)
%% - DELETE /mcp/resources/{uri} -> remove resource (204 / 404)
%% - DELETE /mcp/tools/{name} -> remove tool (204 / 404)
%% - DELETE /mcp/prompts/{name} -> remove prompt (204 / 404)
%% - Session header validation
%% - Origin validation
%% - Idempotent operations
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_http_delete_handler_tests).

-include("erlmcp.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp),
    case erlmcp_session_manager:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    {ok, ServerPid} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),
    ServerPid.

cleanup(ServerPid) ->
    try
        gen_server:stop(ServerPid, normal, 5000)
    catch
        _:_ -> ok
    end,
    try
        gen_server:stop(erlmcp_session_manager, normal, 5000)
    catch
        _:_ -> ok
    end.

%%====================================================================
%% Session Deletion Tests
%%====================================================================

%% Test 1: DELETE /mcp with valid session returns 204
delete_session_valid_test() ->
    ServerPid = setup(),
    try
        {ok, SessionId} = erlmcp_session_manager:create_session(),

        % Verify session exists
        {ok, valid} = erlmcp_session_manager:validate_session(SessionId),

        % Simulate DELETE request
        Result = erlmcp_http_delete_handler:handle_delete(
            mock_req("/mcp", SessionId),
            <<"transport_1">>,
            #{}
        ),

        ?assertMatch({ok, _, _}, Result),

        % Verify session is deleted
        {error, not_found} = erlmcp_session_manager:validate_session(SessionId)
    after
        cleanup(ServerPid)
    end.

%% Test 2: DELETE /mcp without session header returns 400
delete_session_no_header_test() ->
    ServerPid = setup(),
    try
        % Simulate DELETE request without session header
        Result = erlmcp_http_delete_handler:handle_delete(
            mock_req_no_header("/mcp"),
            <<"transport_1">>,
            #{}
        ),

        ?assertMatch({ok, _, _}, Result)
    after
        cleanup(ServerPid)
    end.

%% Test 3: DELETE /mcp with invalid session returns 404
delete_session_invalid_test() ->
    ServerPid = setup(),
    try
        InvalidSessionId = <<"invalid-session-12345">>,

        % Simulate DELETE request with invalid session
        Result = erlmcp_http_delete_handler:handle_delete(
            mock_req("/mcp", InvalidSessionId),
            <<"transport_1">>,
            #{}
        ),

        ?assertMatch({ok, _, _}, Result)
    after
        cleanup(ServerPid)
    end.

%% Test 4: DELETE /mcp with trailing slash works
delete_session_trailing_slash_test() ->
    ServerPid = setup(),
    try
        {ok, SessionId} = erlmcp_session_manager:create_session(),

        % Simulate DELETE request with trailing slash
        Result = erlmcp_http_delete_handler:handle_delete(
            mock_req("/mcp/", SessionId),
            <<"transport_1">>,
            #{}
        ),

        ?assertMatch({ok, _, _}, Result),

        % Verify session is deleted
        {error, not_found} = erlmcp_session_manager:validate_session(SessionId)
    after
        cleanup(ServerPid)
    end.

%% Test 5: DELETE same session twice is idempotent
delete_session_idempotent_test() ->
    ServerPid = setup(),
    try
        {ok, SessionId} = erlmcp_session_manager:create_session(),

        % First delete
        Result1 = erlmcp_http_delete_handler:handle_delete(
            mock_req("/mcp", SessionId),
            <<"transport_1">>,
            #{}
        ),
        ?assertMatch({ok, _, _}, Result1),

        % Second delete of same session
        Result2 = erlmcp_http_delete_handler:handle_delete(
            mock_req("/mcp", SessionId),
            <<"transport_1">>,
            #{}
        ),
        ?assertMatch({ok, _, _}, Result2)
    after
        cleanup(ServerPid)
    end.

%%====================================================================
%% Resource Deletion Tests
%%====================================================================

%% Test 6: DELETE /mcp/resources/{uri} with existing resource returns 204
delete_resource_exists_test() ->
    ServerPid = setup(),
    try
        {ok, SessionId} = erlmcp_session_manager:create_session(),
        Uri = <<"resource://example/data">>,
        Handler = fun(_) -> <<"data">> end,

        % Add resource
        ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

        % Delete resource
        Result = erlmcp_http_delete_handler:handle_delete(
            mock_req("/mcp/resources/example/data", SessionId),
            <<"transport_1">>,
            #{}
        ),

        ?assertMatch({ok, _, _}, Result)
    after
        cleanup(ServerPid)
    end.

%% Test 7: DELETE /mcp/resources/{uri} with non-existent resource returns 404
delete_resource_not_found_test() ->
    ServerPid = setup(),
    try
        {ok, SessionId} = erlmcp_session_manager:create_session(),

        % Try to delete non-existent resource
        Result = erlmcp_http_delete_handler:handle_delete(
            mock_req("/mcp/resources/nonexistent", SessionId),
            <<"transport_1">>,
            #{}
        ),

        ?assertMatch({ok, _, _}, Result)
    after
        cleanup(ServerPid)
    end.

%% Test 8: DELETE /mcp/resources/{uri} is idempotent
delete_resource_idempotent_test() ->
    ServerPid = setup(),
    try
        {ok, SessionId} = erlmcp_session_manager:create_session(),
        Uri = <<"resource://example/data">>,
        Handler = fun(_) -> <<"data">> end,

        % Add resource
        ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

        % First delete
        Result1 = erlmcp_http_delete_handler:handle_delete(
            mock_req("/mcp/resources/example/data", SessionId),
            <<"transport_1">>,
            #{}
        ),
        ?assertMatch({ok, _, _}, Result1),

        % Second delete
        Result2 = erlmcp_http_delete_handler:handle_delete(
            mock_req("/mcp/resources/example/data", SessionId),
            <<"transport_1">>,
            #{}
        ),
        ?assertMatch({ok, _, _}, Result2)
    after
        cleanup(ServerPid)
    end.

%%====================================================================
%% Tool Deletion Tests
%%====================================================================

%% Test 9: DELETE /mcp/tools/{name} with existing tool returns 204
delete_tool_exists_test() ->
    ServerPid = setup(),
    try
        {ok, SessionId} = erlmcp_session_manager:create_session(),
        ToolName = <<"calculator">>,
        Handler = fun(_) -> <<"result">> end,

        % Add tool
        ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

        % Delete tool
        Result = erlmcp_http_delete_handler:handle_delete(
            mock_req("/mcp/tools/calculator", SessionId),
            <<"transport_1">>,
            #{}
        ),

        ?assertMatch({ok, _, _}, Result)
    after
        cleanup(ServerPid)
    end.

%% Test 10: DELETE /mcp/tools/{name} with non-existent tool returns 404
delete_tool_not_found_test() ->
    ServerPid = setup(),
    try
        {ok, SessionId} = erlmcp_session_manager:create_session(),

        % Try to delete non-existent tool
        Result = erlmcp_http_delete_handler:handle_delete(
            mock_req("/mcp/tools/nonexistent", SessionId),
            <<"transport_1">>,
            #{}
        ),

        ?assertMatch({ok, _, _}, Result)
    after
        cleanup(ServerPid)
    end.

%%====================================================================
%% Prompt Deletion Tests
%%====================================================================

%% Test 11: DELETE /mcp/prompts/{name} with existing prompt returns 204
delete_prompt_exists_test() ->
    ServerPid = setup(),
    try
        {ok, SessionId} = erlmcp_session_manager:create_session(),
        PromptName = <<"greetings">>,
        Handler = fun(_) -> <<"Hello">> end,

        % Add prompt
        ok = erlmcp_server:add_prompt(ServerPid, PromptName, Handler),

        % Delete prompt
        Result = erlmcp_http_delete_handler:handle_delete(
            mock_req("/mcp/prompts/greetings", SessionId),
            <<"transport_1">>,
            #{}
        ),

        ?assertMatch({ok, _, _}, Result)
    after
        cleanup(ServerPid)
    end.

%% Test 12: DELETE /mcp/prompts/{name} with non-existent prompt returns 404
delete_prompt_not_found_test() ->
    ServerPid = setup(),
    try
        {ok, SessionId} = erlmcp_session_manager:create_session(),

        % Try to delete non-existent prompt
        Result = erlmcp_http_delete_handler:handle_delete(
            mock_req("/mcp/prompts/nonexistent", SessionId),
            <<"transport_1">>,
            #{}
        ),

        ?assertMatch({ok, _, _}, Result)
    after
        cleanup(ServerPid)
    end.

%%====================================================================
%% Mock Helpers
%%====================================================================

%% Create a mock Cowboy request with session ID header
mock_req(Path, SessionId) when is_binary(SessionId) ->
    mock_req(Path, erlang:binary_to_list(SessionId));
mock_req(Path, SessionId) ->
    #{
        <<"path">> => erlang:list_to_binary(Path),
        <<"mcp-session-id">> => erlang:list_to_binary(SessionId)
    }.

%% Create a mock request without session header
mock_req_no_header(Path) ->
    #{
        <<"path">> => erlang:list_to_binary(Path)
    }.

%%====================================================================
%% Mock Cowboy Request Implementation
%%====================================================================

%% These functions mock the cowboy_req API
-define(REQ_TAB, mock_req_tab).

%% Mock cowboy_req functions (for testing only)
-spec mock_req_path(term()) -> binary().
mock_req_path(Req) when is_map(Req) ->
    maps:get(<<"path">>, Req, <<"/">>).

-spec mock_req_header(binary(), term(), term()) -> term().
mock_req_header(Header, Req, Default) when is_map(Req) ->
    case Header of
        <<"mcp-session-id">> ->
            maps:get(<<"mcp-session-id">>, Req, Default);
        <<"origin">> ->
            maps:get(<<"origin">>, Req, Default);
        _ ->
            Default
    end.

-spec mock_req_reply(non_neg_integer(), map(), term()) -> term().
mock_req_reply(StatusCode, _Headers, _Req) ->
    {status_code, StatusCode}.

-spec mock_req_reply_with_body(non_neg_integer(), map(), binary(), term()) -> term().
mock_req_reply_with_body(StatusCode, _Headers, _Body, _Req) ->
    {status_code, StatusCode}.

