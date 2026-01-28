%%%-------------------------------------------------------------------
%% @doc HTTP DELETE Handler Integration Tests - Gap #28
%%
%% Integration tests for HTTP DELETE method support
%% Tests the complete flow: session creation -> resource/tool/prompt addition -> deletion
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_http_delete_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

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
%% Session Management Tests
%%====================================================================

%% Test 1: Create and delete session
session_create_and_delete_test() ->
    ServerPid = setup(),
    try
        % Create session
        {ok, SessionId} = erlmcp_session_manager:create_session(),
        ?assert(is_binary(SessionId)),
        ?assert(byte_size(SessionId) > 0),

        % Validate session exists
        {ok, valid} = erlmcp_session_manager:validate_session(SessionId),

        % Delete session
        ok = erlmcp_session_manager:delete_session(SessionId),

        % Verify session is deleted
        {error, not_found} = erlmcp_session_manager:validate_session(SessionId)
    after
        cleanup(ServerPid)
    end.

%% Test 2: Multiple session creation and independent deletion
multiple_sessions_independent_delete_test() ->
    ServerPid = setup(),
    try
        % Create multiple sessions
        {ok, Session1} = erlmcp_session_manager:create_session(),
        {ok, Session2} = erlmcp_session_manager:create_session(),
        {ok, Session3} = erlmcp_session_manager:create_session(),

        ?assert(Session1 =/= Session2),
        ?assert(Session2 =/= Session3),

        % Delete middle session
        ok = erlmcp_session_manager:delete_session(Session2),

        % Verify only Session2 is deleted
        {ok, valid} = erlmcp_session_manager:validate_session(Session1),
        {error, not_found} = erlmcp_session_manager:validate_session(Session2),
        {ok, valid} = erlmcp_session_manager:validate_session(Session3),

        % Delete remaining sessions
        ok = erlmcp_session_manager:delete_session(Session1),
        ok = erlmcp_session_manager:delete_session(Session3),

        {error, not_found} = erlmcp_session_manager:validate_session(Session1),
        {error, not_found} = erlmcp_session_manager:validate_session(Session3)
    after
        cleanup(ServerPid)
    end.

%%====================================================================
%% Resource Deletion Tests
%%====================================================================

%% Test 3: Add and delete resource
resource_add_and_delete_test() ->
    ServerPid = setup(),
    try
        Uri = <<"resource://test/data">>,
        Handler = fun(_) -> <<"test data">> end,

        % Add resource
        ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

        % Delete resource
        ok = erlmcp_server:delete_resource(ServerPid, Uri),

        % Verify deletion - attempting to delete again should return error
        {error, not_found} = erlmcp_server:delete_resource(ServerPid, Uri)
    after
        cleanup(ServerPid)
    end.

%% Test 4: Delete non-existent resource returns error
resource_delete_nonexistent_test() ->
    ServerPid = setup(),
    try
        Uri = <<"resource://nonexistent">>,

        % Try to delete non-existent resource
        Result = erlmcp_server:delete_resource(ServerPid, Uri),
        ?assertEqual({error, not_found}, Result)
    after
        cleanup(ServerPid)
    end.

%% Test 5: Multiple resources - selective deletion
resource_selective_deletion_test() ->
    ServerPid = setup(),
    try
        Uri1 = <<"resource://test/data1">>,
        Uri2 = <<"resource://test/data2">>,
        Uri3 = <<"resource://test/data3">>,
        Handler = fun(_) -> <<"data">> end,

        % Add multiple resources
        ok = erlmcp_server:add_resource(ServerPid, Uri1, Handler),
        ok = erlmcp_server:add_resource(ServerPid, Uri2, Handler),
        ok = erlmcp_server:add_resource(ServerPid, Uri3, Handler),

        % Delete middle resource
        ok = erlmcp_server:delete_resource(ServerPid, Uri2),

        % Verify deletion
        {error, not_found} = erlmcp_server:delete_resource(ServerPid, Uri2),

        % Verify others still can be deleted
        ok = erlmcp_server:delete_resource(ServerPid, Uri1),
        ok = erlmcp_server:delete_resource(ServerPid, Uri3)
    after
        cleanup(ServerPid)
    end.

%%====================================================================
%% Tool Deletion Tests
%%====================================================================

%% Test 6: Add and delete tool
tool_add_and_delete_test() ->
    ServerPid = setup(),
    try
        ToolName = <<"calculator">>,
        Handler = fun(_) -> <<"result">> end,

        % Add tool
        ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

        % Delete tool
        ok = erlmcp_server:delete_tool(ServerPid, ToolName),

        % Verify deletion
        {error, not_found} = erlmcp_server:delete_tool(ServerPid, ToolName)
    after
        cleanup(ServerPid)
    end.

%% Test 7: Delete non-existent tool returns error
tool_delete_nonexistent_test() ->
    ServerPid = setup(),
    try
        ToolName = <<"nonexistent_tool">>,

        % Try to delete non-existent tool
        Result = erlmcp_server:delete_tool(ServerPid, ToolName),
        ?assertEqual({error, not_found}, Result)
    after
        cleanup(ServerPid)
    end.

%% Test 8: Multiple tools - selective deletion
tool_selective_deletion_test() ->
    ServerPid = setup(),
    try
        Tool1 = <<"add">>,
        Tool2 = <<"subtract">>,
        Tool3 = <<"multiply">>,
        Handler = fun(_) -> <<"result">> end,

        % Add tools
        ok = erlmcp_server:add_tool(ServerPid, Tool1, Handler),
        ok = erlmcp_server:add_tool(ServerPid, Tool2, Handler),
        ok = erlmcp_server:add_tool(ServerPid, Tool3, Handler),

        % Delete middle tool
        ok = erlmcp_server:delete_tool(ServerPid, Tool2),

        % Verify deletion
        {error, not_found} = erlmcp_server:delete_tool(ServerPid, Tool2),

        % Verify others still exist
        ok = erlmcp_server:delete_tool(ServerPid, Tool1),
        ok = erlmcp_server:delete_tool(ServerPid, Tool3)
    after
        cleanup(ServerPid)
    end.

%%====================================================================
%% Prompt Deletion Tests
%%====================================================================

%% Test 9: Add and delete prompt
prompt_add_and_delete_test() ->
    ServerPid = setup(),
    try
        PromptName = <<"greeting">>,
        Handler = fun(_) -> <<"Hello!">> end,

        % Add prompt
        ok = erlmcp_server:add_prompt(ServerPid, PromptName, Handler),

        % Delete prompt
        ok = erlmcp_server:delete_prompt(ServerPid, PromptName),

        % Verify deletion
        {error, not_found} = erlmcp_server:delete_prompt(ServerPid, PromptName)
    after
        cleanup(ServerPid)
    end.

%% Test 10: Delete non-existent prompt returns error
prompt_delete_nonexistent_test() ->
    ServerPid = setup(),
    try
        PromptName = <<"nonexistent_prompt">>,

        % Try to delete non-existent prompt
        Result = erlmcp_server:delete_prompt(ServerPid, PromptName),
        ?assertEqual({error, not_found}, Result)
    after
        cleanup(ServerPid)
    end.

%%====================================================================
%% Mixed Operations Tests
%%====================================================================

%% Test 11: Add resources, tools, and prompts, then delete all
mixed_operations_add_and_delete_test() ->
    ServerPid = setup(),
    try
        Handler = fun(_) -> <<"data">> end,

        % Add mixed types
        ok = erlmcp_server:add_resource(ServerPid, <<"resource://test">>, Handler),
        ok = erlmcp_server:add_tool(ServerPid, <<"tool1">>, Handler),
        ok = erlmcp_server:add_prompt(ServerPid, <<"prompt1">>, Handler),

        % Delete all
        ok = erlmcp_server:delete_resource(ServerPid, <<"resource://test">>),
        ok = erlmcp_server:delete_tool(ServerPid, <<"tool1">>),
        ok = erlmcp_server:delete_prompt(ServerPid, <<"prompt1">>),

        % Verify all deleted
        {error, not_found} = erlmcp_server:delete_resource(ServerPid, <<"resource://test">>),
        {error, not_found} = erlmcp_server:delete_tool(ServerPid, <<"tool1">>),
        {error, not_found} = erlmcp_server:delete_prompt(ServerPid, <<"prompt1">>)
    after
        cleanup(ServerPid)
    end.

%% Test 12: Delete handler module exports are accessible
delete_handler_exports_test() ->
    % Verify module exists and exports are callable
    Exports = erlmcp_http_delete_handler:module_info(exports),

    % Check for key exports
    ?assert(lists:member({handle_delete, 3}, Exports)),
    ?assert(lists:member({handle_delete_by_path, 5}, Exports)),
    ?assert(lists:member({handle_delete_session_termination, 4}, Exports)),
    ?assert(lists:member({handle_delete_resource, 5}, Exports)),
    ?assert(lists:member({handle_delete_tool, 5}, Exports)),
    ?assert(lists:member({handle_delete_prompt, 5}, Exports))
.

%%====================================================================
%% Idempotency Tests
%%====================================================================

%% Test 13: Deleting same resource multiple times is safe
idempotent_resource_deletion_test() ->
    ServerPid = setup(),
    try
        Uri = <<"resource://idempotent">>,
        Handler = fun(_) -> <<"data">> end,

        % Add resource
        ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

        % Delete resource multiple times
        ok = erlmcp_server:delete_resource(ServerPid, Uri),
        {error, not_found} = erlmcp_server:delete_resource(ServerPid, Uri),
        {error, not_found} = erlmcp_server:delete_resource(ServerPid, Uri)
    after
        cleanup(ServerPid)
    end.

%% Test 14: Deleting same tool multiple times is safe
idempotent_tool_deletion_test() ->
    ServerPid = setup(),
    try
        ToolName = <<"idempotent_tool">>,
        Handler = fun(_) -> <<"result">> end,

        % Add tool
        ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

        % Delete tool multiple times
        ok = erlmcp_server:delete_tool(ServerPid, ToolName),
        {error, not_found} = erlmcp_server:delete_tool(ServerPid, ToolName),
        {error, not_found} = erlmcp_server:delete_tool(ServerPid, ToolName)
    after
        cleanup(ServerPid)
    end.

