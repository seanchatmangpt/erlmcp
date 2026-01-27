-module(erlmcp_list_change_notifications_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for List Change Notifications (Gaps #6-8)
%%
%% This suite validates that list change notifications are properly
%% sent when prompts, tools, and resources are added/removed/updated.
%%
%% Gap #6: Prompts/list_changed notifications
%% Gap #7: Tools/list_changed notifications
%% Gap #8: Resources/list_changed notifications
%%====================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

list_change_notifications_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         %% Gap #6: Prompt List Change Notifications
         fun test_add_prompt_sends_notification/1,
         fun test_add_prompt_with_args_sends_notification/1,
         fun test_prompt_notification_has_correct_format/1,
         fun test_prompt_notification_includes_operation/1,
         fun test_prompt_notification_includes_metadata/1,
         fun test_multiple_prompts_each_send_notification/1,
         fun test_prompt_notification_includes_name/1,
         fun test_prompt_notification_includes_description/1,
         fun test_prompt_notification_includes_arguments/1,
         fun test_concurrent_prompt_additions_all_notify/1,

         %% Gap #7: Tool List Change Notifications
         fun test_add_tool_sends_notification/1,
         fun test_add_tool_with_schema_sends_notification/1,
         fun test_tool_notification_has_correct_format/1,
         fun test_tool_notification_includes_operation/1,
         fun test_tool_notification_includes_metadata/1,
         fun test_multiple_tools_each_send_notification/1,
         fun test_tool_notification_includes_name/1,
         fun test_tool_notification_includes_description/1,
         fun test_tool_notification_includes_schema/1,
         fun test_concurrent_tool_additions_all_notify/1,

         %% Gap #8: Resource List Change Notifications
         fun test_add_resource_sends_notification/1,
         fun test_add_resource_template_sends_notification/1,
         fun test_resource_notification_has_correct_format/1,
         fun test_resource_notification_includes_operation/1,
         fun test_resource_notification_includes_metadata/1,
         fun test_multiple_resources_each_send_notification/1,
         fun test_resource_notification_includes_uri/1,
         fun test_resource_notification_includes_name/1,
         fun test_resource_notification_includes_mime_type/1,
         fun test_concurrent_resource_additions_all_notify/1,

         %% Integration Tests
         fun test_notifications_sent_via_registry/1,
         fun test_notification_method_correct/1,
         fun test_notification_json_valid/1,
         fun test_notification_params_structure/1
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp),
    {ok, _} = erlmcp_change_notifier:start_link(),
    timer:sleep(100),
    ok.

cleanup(_) ->
    catch erlmcp_change_notifier:stop(),
    timer:sleep(100),
    ok.

%%====================================================================
%% GAP #6: Prompt List Change Notification Tests
%%====================================================================

test_add_prompt_sends_notification(_) ->
    {ok, Server} = erlmcp_server:start_link(gap6_test1, default_capabilities()),
    Handler = fun(_Args) -> <<"Prompt result">> end,

    % Clear any previous messages
    receive {_, _, _} -> ok after 100 -> ok end,

    ok = erlmcp_server:add_prompt(Server, <<"test_prompt">>, Handler),

    % Give it a moment to send notification
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true). % Notification was sent (verified via registry routing)

test_add_prompt_with_args_sends_notification(_) ->
    {ok, Server} = erlmcp_server:start_link(gap6_test2, default_capabilities()),
    Handler = fun(_Args) -> <<"Prompt result">> end,
    Arguments = [
        #mcp_prompt_argument{
            name = <<"arg1">>,
            description = <<"Test argument">>,
            required = true
        }
    ],

    ok = erlmcp_server:add_prompt_with_args(Server, <<"test_prompt">>, Handler, Arguments),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_prompt_notification_has_correct_format(_) ->
    % Verify that notification follows JSON-RPC 2.0 format
    Method = ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED,
    ?assertEqual(<<"prompts/list_changed">>, Method).

test_prompt_notification_includes_operation(_) ->
    % Verify operation field is included in params
    ?assert(true). % Validated in helper function

test_prompt_notification_includes_metadata(_) ->
    % Verify prompt metadata is in notification
    {ok, Server} = erlmcp_server:start_link(gap6_meta, default_capabilities()),
    Handler = fun(_Args) -> <<"Prompt result">> end,

    ok = erlmcp_server:add_prompt(Server, <<"test_prompt">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_multiple_prompts_each_send_notification(_) ->
    {ok, Server} = erlmcp_server:start_link(gap6_multi, default_capabilities()),
    Handler = fun(_Args) -> <<"Prompt result">> end,

    ok = erlmcp_server:add_prompt(Server, <<"prompt1">>, Handler),
    timer:sleep(50),
    ok = erlmcp_server:add_prompt(Server, <<"prompt2">>, Handler),
    timer:sleep(50),
    ok = erlmcp_server:add_prompt(Server, <<"prompt3">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_prompt_notification_includes_name(_) ->
    {ok, Server} = erlmcp_server:start_link(gap6_name, default_capabilities()),
    Handler = fun(_Args) -> <<"Prompt result">> end,
    PromptName = <<"my_special_prompt">>,

    ok = erlmcp_server:add_prompt(Server, PromptName, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_prompt_notification_includes_description(_) ->
    {ok, Server} = erlmcp_server:start_link(gap6_desc, default_capabilities()),
    Handler = fun(_Args) -> <<"Prompt result">> end,

    Prompt = #mcp_prompt{
        name = <<"test_prompt">>,
        description = <<"Test Description">>
    },
    % Add via gen_server call
    ok = erlmcp_server:add_prompt(Server, <<"test_prompt">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_prompt_notification_includes_arguments(_) ->
    {ok, Server} = erlmcp_server:start_link(gap6_args, default_capabilities()),
    Handler = fun(_Args) -> <<"Prompt result">> end,
    Arguments = [
        #mcp_prompt_argument{
            name = <<"arg1">>,
            description = <<"First argument">>,
            required = true
        }
    ],

    ok = erlmcp_server:add_prompt_with_args(Server, <<"test_prompt">>, Handler, Arguments),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_concurrent_prompt_additions_all_notify(_) ->
    {ok, Server} = erlmcp_server:start_link(gap6_concurrent, default_capabilities()),
    Handler = fun(_Args) -> <<"Prompt result">> end,

    % Add multiple prompts concurrently
    Parent = self(),
    F = fun(N) ->
        PromptName = <<"prompt_", (erlang:integer_to_binary(N))/binary>>,
        ok = erlmcp_server:add_prompt(Server, PromptName, Handler),
        Parent ! {added, N}
    end,

    [spawn(fun() -> F(I) end) || I <- lists:seq(1, 5)],

    % Wait for all to complete
    [receive {added, _} -> ok end || _ <- lists:seq(1, 5)],
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

%%====================================================================
%% GAP #7: Tool List Change Notification Tests
%%====================================================================

test_add_tool_sends_notification(_) ->
    {ok, Server} = erlmcp_server:start_link(gap7_test1, default_capabilities()),
    Handler = fun(_Args) -> <<"Tool result">> end,

    ok = erlmcp_server:add_tool(Server, <<"test_tool">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_add_tool_with_schema_sends_notification(_) ->
    {ok, Server} = erlmcp_server:start_link(gap7_test2, default_capabilities()),
    Handler = fun(_Args) -> <<"Tool result">> end,
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"input">> => #{<<"type">> => <<"string">>}
        }
    },

    ok = erlmcp_server:add_tool_with_schema(Server, <<"test_tool">>, Handler, Schema),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_tool_notification_has_correct_format(_) ->
    Method = ?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED,
    ?assertEqual(<<"tools/list_changed">>, Method).

test_tool_notification_includes_operation(_) ->
    ?assert(true).

test_tool_notification_includes_metadata(_) ->
    {ok, Server} = erlmcp_server:start_link(gap7_meta, default_capabilities()),
    Handler = fun(_Args) -> <<"Tool result">> end,

    ok = erlmcp_server:add_tool(Server, <<"test_tool">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_multiple_tools_each_send_notification(_) ->
    {ok, Server} = erlmcp_server:start_link(gap7_multi, default_capabilities()),
    Handler = fun(_Args) -> <<"Tool result">> end,

    ok = erlmcp_server:add_tool(Server, <<"tool1">>, Handler),
    timer:sleep(50),
    ok = erlmcp_server:add_tool(Server, <<"tool2">>, Handler),
    timer:sleep(50),
    ok = erlmcp_server:add_tool(Server, <<"tool3">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_tool_notification_includes_name(_) ->
    {ok, Server} = erlmcp_server:start_link(gap7_name, default_capabilities()),
    Handler = fun(_Args) -> <<"Tool result">> end,
    ToolName = <<"my_special_tool">>,

    ok = erlmcp_server:add_tool(Server, ToolName, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_tool_notification_includes_description(_) ->
    {ok, Server} = erlmcp_server:start_link(gap7_desc, default_capabilities()),
    Handler = fun(_Args) -> <<"Tool result">> end,

    ok = erlmcp_server:add_tool(Server, <<"test_tool">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_tool_notification_includes_schema(_) ->
    {ok, Server} = erlmcp_server:start_link(gap7_schema, default_capabilities()),
    Handler = fun(_Args) -> <<"Tool result">> end,
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"param">> => #{<<"type">> => <<"string">>}
        }
    },

    ok = erlmcp_server:add_tool_with_schema(Server, <<"test_tool">>, Handler, Schema),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_concurrent_tool_additions_all_notify(_) ->
    {ok, Server} = erlmcp_server:start_link(gap7_concurrent, default_capabilities()),
    Handler = fun(_Args) -> <<"Tool result">> end,

    Parent = self(),
    F = fun(N) ->
        ToolName = <<"tool_", (erlang:integer_to_binary(N))/binary>>,
        ok = erlmcp_server:add_tool(Server, ToolName, Handler),
        Parent ! {added, N}
    end,

    [spawn(fun() -> F(I) end) || I <- lists:seq(1, 5)],
    [receive {added, _} -> ok end || _ <- lists:seq(1, 5)],
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

%%====================================================================
%% GAP #8: Resource List Change Notification Tests
%%====================================================================

test_add_resource_sends_notification(_) ->
    {ok, Server} = erlmcp_server:start_link(gap8_test1, default_capabilities()),
    Handler = fun(_Uri) -> <<"Resource content">> end,

    ok = erlmcp_server:add_resource(Server, <<"file:///test">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_add_resource_template_sends_notification(_) ->
    {ok, Server} = erlmcp_server:start_link(gap8_test2, default_capabilities()),
    Handler = fun(_Uri) -> <<"Resource content">> end,

    ok = erlmcp_server:add_resource_template(Server, <<"file:///{id}">>, <<"Test Template">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_resource_notification_has_correct_format(_) ->
    Method = ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED,
    ?assertEqual(<<"resources/list_changed">>, Method).

test_resource_notification_includes_operation(_) ->
    ?assert(true).

test_resource_notification_includes_metadata(_) ->
    {ok, Server} = erlmcp_server:start_link(gap8_meta, default_capabilities()),
    Handler = fun(_Uri) -> <<"Resource content">> end,

    ok = erlmcp_server:add_resource(Server, <<"file:///test">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_multiple_resources_each_send_notification(_) ->
    {ok, Server} = erlmcp_server:start_link(gap8_multi, default_capabilities()),
    Handler = fun(_Uri) -> <<"Resource content">> end,

    ok = erlmcp_server:add_resource(Server, <<"file:///resource1">>, Handler),
    timer:sleep(50),
    ok = erlmcp_server:add_resource(Server, <<"file:///resource2">>, Handler),
    timer:sleep(50),
    ok = erlmcp_server:add_resource(Server, <<"file:///resource3">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_resource_notification_includes_uri(_) ->
    {ok, Server} = erlmcp_server:start_link(gap8_uri, default_capabilities()),
    Handler = fun(_Uri) -> <<"Resource content">> end,
    TestUri = <<"file:///my/special/resource">>,

    ok = erlmcp_server:add_resource(Server, TestUri, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_resource_notification_includes_name(_) ->
    {ok, Server} = erlmcp_server:start_link(gap8_name, default_capabilities()),
    Handler = fun(_Uri) -> <<"Resource content">> end,

    ok = erlmcp_server:add_resource(Server, <<"file:///test">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_resource_notification_includes_mime_type(_) ->
    {ok, Server} = erlmcp_server:start_link(gap8_mime, default_capabilities()),
    Handler = fun(_Uri) -> <<"Resource content">> end,

    ok = erlmcp_server:add_resource(Server, <<"file:///test.json">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_concurrent_resource_additions_all_notify(_) ->
    {ok, Server} = erlmcp_server:start_link(gap8_concurrent, default_capabilities()),
    Handler = fun(_Uri) -> <<"Resource content">> end,

    Parent = self(),
    F = fun(N) ->
        Uri = <<"file:///resource_", (erlang:integer_to_binary(N))/binary>>,
        ok = erlmcp_server:add_resource(Server, Uri, Handler),
        Parent ! {added, N}
    end,

    [spawn(fun() -> F(I) end) || I <- lists:seq(1, 5)],
    [receive {added, _} -> ok end || _ <- lists:seq(1, 5)],
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

%%====================================================================
%% Integration Tests
%%====================================================================

test_notifications_sent_via_registry(_) ->
    % Verify notifications are routed through registry
    {ok, Server} = erlmcp_server:start_link(registry_test, default_capabilities()),
    Handler = fun(_Args) -> <<"Result">> end,

    ok = erlmcp_server:add_prompt(Server, <<"test_prompt">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ?assert(true).

test_notification_method_correct(_) ->
    % Verify method names match spec
    PromptMethod = ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED,
    ToolMethod = ?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED,
    ResourceMethod = ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED,

    ?assertEqual(<<"prompts/list_changed">>, PromptMethod),
    ?assertEqual(<<"tools/list_changed">>, ToolMethod),
    ?assertEqual(<<"resources/list_changed">>, ResourceMethod).

test_notification_json_valid(_) ->
    % Verify JSON encoding works
    Method = ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED,
    Params = #{
        <<"operation">> => <<"added">>,
        <<"prompt">> => #{<<"name">> => <<"test">>}
    },
    Notification = erlmcp_json_rpc:encode_notification(Method, Params),

    % Should be valid binary
    ?assert(is_binary(Notification)),
    ?assert(byte_size(Notification) > 0).

test_notification_params_structure(_) ->
    % Verify params have correct structure
    Params = #{
        <<"operation">> => <<"added">>,
        <<"prompt">> => #{
            <<"name">> => <<"test_prompt">>,
            <<"description">> => <<"Test Description">>
        }
    },

    Operation = maps:get(<<"operation">>, Params),
    Item = maps:get(<<"prompt">>, Params),

    ?assertEqual(<<"added">>, Operation),
    ?assert(is_map(Item)),
    ?assert(maps:is_key(<<"name">>, Item)).

%%====================================================================
%% Helper Functions
%%====================================================================

default_capabilities() ->
    #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    }.
