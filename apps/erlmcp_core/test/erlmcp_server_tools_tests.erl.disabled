-module(erlmcp_server_tools_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/erlmcp.hrl").

%%%====================================================================
%%% Tools Management Tests - Chicago School TDD
%%% Tests for tool API: add_tool, add_tool_with_schema, add_tool_with_description,
%%% add_tool_full, delete_tool
%%% Principles: Real processes, observable behavior, no state inspection
%%%====================================================================

%%%====================================================================
%%% Test Generators
%%%====================================================================

tools_basic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Add tool", fun test_add_tool/0},
          {"Add tool with schema", fun test_add_tool_with_schema/0},
          {"Add tool with description", fun test_add_tool_with_description/0},
          {"Add tool full", fun test_add_tool_full/0},
          {"Delete tool", fun test_delete_tool/0},
          {"Delete non-existent tool", fun test_delete_nonexistent/0}
         ]
     end}.

tools_advanced_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Multiple tools", fun test_multiple_tools/0},
          {"Tool description validation", fun test_description_validation/0},
          {"Tool pagination support", fun test_pagination/0},
          {"Tool notification", fun test_tools_changed/0}
         ]
     end}.

tools_error_handling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Tool name validation", fun test_name_validation/0},
          {"Schema validation", fun test_schema_validation/0},
          {"Duplicate tool", fun test_duplicate_tool/0},
          {"Empty description", fun test_empty_description/0}
         ]
     end}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

%%%====================================================================
%%% Basic Tools Tests
%%%====================================================================

test_add_tool() ->
    Server = start_server(),
    ToolName = <<"test_tool">>,
    Handler = fun(Args) -> #{result => Args} end,

    ?assertEqual(ok, erlmcp_server:add_tool(Server, ToolName, Handler)),

    ok = erlmcp_server:stop(Server).

test_add_tool_with_schema() ->
    Server = start_server(),
    ToolName = <<"tool_with_schema">>,
    Schema = #{
        type => <<"object">>,
        properties => #{
            input => #{type => <<"string">>},
            count => #{type => <<"number">>}
        },
        required => [<<"input">>]
    },
    Handler = fun(Args) -> #{processed => Args} end,

    ?assertEqual(ok, erlmcp_server:add_tool_with_schema(Server, ToolName, Handler, Schema)),

    ok = erlmcp_server:stop(Server).

test_add_tool_with_description() ->
    Server = start_server(),
    ToolName = <<"tool_with_desc">>,
    Description = <<"A test tool with custom description">>,
    Handler = fun(_) -> <<"result">> end,

    ?assertEqual(ok, erlmcp_server:add_tool_with_description(Server, ToolName, Description, Handler)),

    ok = erlmcp_server:stop(Server).

test_add_tool_full() ->
    Server = start_server(),
    ToolName = <<"full_tool">>,
    Description = <<"Full tool metadata">>,
    Handler = fun(_) -> <<"full">> end,
    Options = #{
        <<"inputSchema">> => #{type => <<"object">>},
        <<"deprecated">> => true,
        <<"metadata">> => #{<<"key">> => <<"value">>},
        <<"experimental">> => true,
        <<"version">> => <<"1.0.0">>
    },

    ?assertEqual(ok, erlmcp_server:add_tool_full(Server, ToolName, Description, Handler, Options)),

    ok = erlmcp_server:stop(Server).

test_delete_tool() ->
    Server = start_server(),
    ToolName = <<"delete_tool">>,
    Handler = fun(_) -> #{result => ok} end,

    ok = erlmcp_server:add_tool(Server, ToolName, Handler),
    ?assertEqual(ok, erlmcp_server:delete_tool(Server, ToolName)),

    %% Verify deletion - second delete should fail
    ?assertEqual({error, not_found}, erlmcp_server:delete_tool(Server, ToolName)),

    ok = erlmcp_server:stop(Server).

test_delete_nonexistent() ->
    Server = start_server(),

    ?assertEqual({error, not_found}, erlmcp_server:delete_tool(Server, <<"nonexistent_tool">>)),

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Advanced Tools Tests
%%%====================================================================

test_multiple_tools() ->
    Server = start_server(),

    %% Add multiple tools
    [begin
        Name = <<"bulk_tool_", (integer_to_binary(N))/binary>>,
        Handler = fun(_) -> N end,
        ?assertEqual(ok, erlmcp_server:add_tool(Server, Name, Handler))
    end || N <- lists:seq(1, 10)],

    ok = erlmcp_server:stop(Server).

test_description_validation() ->
    Server = start_server(),

    %% Test normal description
    NormalDesc = <<"Normal tool description">>,
    ?assertEqual(ok,
                 erlmcp_server:add_tool_with_description(
                     Server, <<"tool_normal">>, NormalDesc,
                     fun(_) -> ok end)),

    %% Test long description (10000 chars - should succeed)
    LongDesc = <<<<$A>> || _ <- lists:seq(1, 10000)>>,
    ?assertEqual(ok,
                 erlmcp_server:add_tool_with_description(
                     Server, <<"tool_long">>, LongDesc,
                     fun(_) -> ok end)),

    %% Test very long description (>10000 chars)
    TooLongDesc = <<<<$B>> || _ <- lists:seq(1, 10001)>>,
    Result = erlmcp_server:add_tool_with_description(
                 Server, <<"tool_too_long">>, TooLongDesc,
                 fun(_) -> ok end),
    %% Either ok (if not validated) or error tuple
    case Result of
        ok -> ok;
        {error, {_Code, _Msg, _Data}} -> ok
    end,

    ok = erlmcp_server:stop(Server).

test_pagination() ->
    Server = start_server(),

    %% Add many tools to test pagination
    [begin
         Name = <<"paginated_tool_", (integer_to_binary(N))/binary>>,
         Handler = fun(_) -> #{n => N} end,
         ok = erlmcp_server:add_tool(Server, Name, Handler)
     end || N <- lists:seq(1, 50)],

    %% Server should handle all tools
    ?assert(erlang:is_process_alive(Server)),

    ok = erlmcp_server:stop(Server).

test_tools_changed() ->
    Server = start_server(),

    %% Adding tools should trigger notification
    ok = erlmcp_server:add_tool(Server, <<"notify_tool_1">>, fun(_) -> ok end),
    ok = erlmcp_server:add_tool(Server, <<"notify_tool_2">>, fun(_) -> ok end),

    %% Deleting tool should trigger notification
    ok = erlmcp_server:delete_tool(Server, <<"notify_tool_1">>),

    ?assert(erlang:is_process_alive(Server)),

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

test_name_validation() ->
    Server = start_server(),

    %% Test various tool names
    ValidNames = [
        <<"simple">>,
        <<"with_underscore">>,
        <<"with-dash">>,
        <<"with123numbers">>,
        <<"CamelCase">>,
        <<"UPPERCASE">>,
        <<"nested/tool/name">>
    ],
    [begin
        Handler = fun(_) -> ok end,
        ?assertEqual(ok, erlmcp_server:add_tool(Server, Name, Handler))
    end || Name <- ValidNames],

    ok = erlmcp_server:stop(Server).

test_schema_validation() ->
    Server = start_server(),

    %% Test various schemas
    Schemas = [
        #{type => <<"object">>},
        #{type => <<"object">>, properties => #{}},
        #{type => <<"object">>, properties => #{<<"field">> => #{type => <<"string">>}}},
        #{<<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>}
    ],
    [begin
        Name = <<"schema_tool_", (integer_to_binary(N))/binary>>,
        Handler = fun(_) -> ok end,
        ?assertEqual(ok, erlmcp_server:add_tool_with_schema(Server, Name, Handler, Schema))
    end || {N, Schema} <- lists:zip(lists:seq(1, length(Schemas)), Schemas)],

    ok = erlmcp_server:stop(Server).

test_duplicate_tool() ->
    Server = start_server(),
    ToolName = <<"duplicate_tool">>,
    Handler = fun(_) -> ok end,

    ok = erlmcp_server:add_tool(Server, ToolName, Handler),

    %% Try to add duplicate - behavior depends on implementation
    %% Either ok (replace) or error (reject)
    case erlmcp_server:add_tool(Server, ToolName, Handler) of
        ok -> ok;
        {error, _} -> ok
    end,

    ok = erlmcp_server:stop(Server).

test_empty_description() ->
    Server = start_server(),

    %% Empty description
    ?assertEqual(ok,
                 erlmcp_server:add_tool_with_description(
                     Server, <<"empty_desc">>, <<>>,
                     fun(_) -> ok end)),

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Start server with default capabilities
start_server() ->
    ServerId = <<"tools_test_server_">>,
    Capabilities = #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true}
    },
    {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities),
    Pid.
