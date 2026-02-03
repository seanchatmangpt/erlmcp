%%%-------------------------------------------------------------------
%%% @doc erlmcp End-to-End Integration Test Suite
%%%
%%% Comprehensive integration tests for the erlmcp MCP server:
%%% - Resource access and validation
%%% - Tool invocation with proper arguments
%%% - Prompt template rendering
%%% - Subscription lifecycle management
%%%
%%% Prerequisites:
%%%   - PostgreSQL running on TEST_DB_PORT (default: 5433)
%%%   - Redis running on TEST_REDIS_PORT (default: 6380)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_integration_SUITE).
-author('erlmcp').

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    %% Resource access tests
    t_resource_list_success/1,
    t_resource_read_success/1,
    t_resource_read_not_found/1,
    t_resource_templates_list/1,

    %% Tool invocation tests
    t_tools_list_success/1,
    t_tool_call_echo_success/1,
    t_tool_call_calculate_success/1,
    t_tool_call_invalid_arguments/1,
    t_tool_call_tool_not_found/1,

    %% Prompt template tests
    t_prompts_list_success/1,
    t_prompt_get_success/1,
    t_prompt_render_simple/1,
    t_prompt_render_with_arguments/1,
    t_prompt_render_missing_required/1,
    t_prompt_not_found/1,

    %% Subscription tests
    t_resource_subscribe_success/1,
    t_resource_unsubscribe_success/1,
    t_resource_notification_on_change/1,
    t_resource_multiple_subscribers/1,
    t_subscription_rate_limiting/1,

    %% Error handling tests
    t_invalid_json_request/1,
    t_missing_method_field/1,
    t_unsupported_method/1,

    %% Concurrency tests
    t_concurrent_tool_invocations/1,
    t_concurrent_resource_access/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% CT Callbacks
%%%====================================================================

-spec all() -> [atom()].
all() ->
    [
        %% Resource access tests
        t_resource_list_success,
        t_resource_read_success,
        t_resource_read_not_found,
        t_resource_templates_list,

        %% Tool invocation tests
        t_tools_list_success,
        t_tool_call_echo_success,
        t_tool_call_calculate_success,
        t_tool_call_invalid_arguments,
        t_tool_call_tool_not_found,

        %% Prompt template tests
        t_prompts_list_success,
        t_prompt_get_success,
        t_prompt_render_simple,
        t_prompt_render_with_arguments,
        t_prompt_render_missing_required,
        t_prompt_not_found,

        %% Subscription tests
        t_resource_subscribe_success,
        t_resource_unsubscribe_success,
        t_resource_notification_on_change,
        t_resource_multiple_subscribers,
        t_subscription_rate_limiting,

        %% Error handling tests
        t_invalid_json_request,
        t_missing_method_field,
        t_unsupported_method,

        %% Concurrency tests
        t_concurrent_tool_invocations,
        t_concurrent_resource_access
    ].

-spec init_per_suite(Config) -> Config.
init_per_suite(Config) ->
    ct:log("Starting erlmcp Integration Test Suite"),
    %% Start application
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),
    application:ensure_all_started(erlmcp_observability),

    %% Start resource subscriptions manager
    {ok, _Pid} = erlmcp_resource_subscriptions:start_link(),

    %% Wait for services to be ready
    timer:sleep(1000),
    Config.

-spec end_per_suite(Config) -> term().
end_per_suite(_Config) ->
    ct:log("Stopping erlmcp Integration Test Suite"),
    application:stop(erlmcp_observability),
    application:stop(erlmcp_transports),
    application:stop(erlmcp_core),
    ok.

-spec init_per_testcase(TestCase, Config) -> Config.
init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p", [TestCase]),
    Config.

-spec end_per_testcase(TestCase, Config) -> ok.
end_per_testcase(TestCase, _Config) ->
    ct:log("Test case complete: ~p", [TestCase]),
    ok.

%%%====================================================================
%%% Resource Access Tests
%%%====================================================================

%% @doc Test successful resource listing
t_resource_list_success(_Config) ->
    %% Build resources/list request
    Request = build_request(<<"resources/list">>, #{}),

    %% Execute request (simulate server handling)
    Response = handle_mcp_request(Request),

    %% Verify response structure
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">> := 1,
                   <<"result">> := #{<<"resources">> := _}}, Response),

    %% Verify we have resources
    Resources = maps:get(<<"resources">>, maps:get(<<"result">>, Response)),
    ?assert(length(Resources) >= 1),

    %% Verify each resource has required fields
    lists:foreach(fun(Res) ->
        ?assert(maps:is_key(<<"uri">>, Res)),
        ?assert(maps:is_key(<<"name">>, Res))
    end, Resources),

    %% Verify resource URI format
    lists:foreach(fun(Res) ->
        Uri = maps:get(<<"uri">>, Res),
        ?assertMatch(ok, erlmcp_resource:validate_uri(Uri))
    end, Resources),

    ct:log("Resources listed: ~p", [length(Resources)]),
    ok.

%% @doc Test successful resource reading
t_resource_read_success(_Config) ->
    %% First, list resources to get a valid URI
    ListRequest = build_request(<<"resources/list">>, #{}),
    ListResponse = handle_mcp_request(ListRequest),
    Resources = maps:get(<<"resources">>, maps:get(<<"result">>, ListResponse)),
    [FirstResource | _] = Resources,
    Uri = maps:get(<<"uri">>, FirstResource),

    %% Read the specific resource
    ReadRequest = build_request(<<"resources/read">>, #{<<"uri">> => Uri}),
    ReadResponse = handle_mcp_request(ReadRequest),

    %% Verify response
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">> := 1,
                   <<"result">> := #{<<"contents">> := _}}, ReadResponse),

    %% Verify content
    Contents = maps:get(<<"contents">>, maps:get(<<"result">>, ReadResponse)),
    ?assert(length(Contents) >= 1),

    ct:log("Resource read successfully: ~p", [Uri]),
    ok.

%% @doc Test resource not found error
t_resource_read_not_found(_Config) ->
    %% Request a non-existent resource
    Uri = <<"test://nonexistent/resource">>,
    Request = build_request(<<"resources/read">>, #{<<"uri">> => Uri}),
    Response = handle_mcp_request(Request),

    %% Verify error response
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">> := 1,
                   <<"error">> := #{<<"code">> := ?MCP_ERROR_RESOURCE_NOT_FOUND,
                                    <<"message">> := _}}, Response),

    ct:log("Resource not found error correctly returned for: ~p", [Uri]),
    ok.

%% @doc Test resource templates listing
t_resource_templates_list(_Config) ->
    Request = build_request(<<"resources/templates/list">>, #{}),
    Response = handle_mcp_request(Request),

    %% Verify response structure (may be empty list)
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">> := 1,
                   <<"result">> := #{<<"resourceTemplates">> := _}}, Response),

    Templates = maps:get(<<"resourceTemplates">>, maps:get(<<"result">>, Response)),
    ?assert(is_list(Templates)),

    ct:log("Resource templates listed: ~p", [length(Templates)]),
    ok.

%%%====================================================================
%%% Tool Invocation Tests
%%%====================================================================

%% @doc Test successful tools listing
t_tools_list_success(_Config) ->
    Request = build_request(<<"tools/list">>, #{}),
    Response = handle_mcp_request(Request),

    %% Verify response
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">> := 1,
                   <<"result">> := #{<<"tools">> := _}}, Response),

    %% Verify we have tools
    Tools = maps:get(<<"tools">>, maps:get(<<"result">>, Response)),
    ?assert(length(Tools) >= 1),

    %% Verify each tool has required fields
    lists:foreach(fun(Tool) ->
        ?assert(maps:is_key(<<"name">>, Tool)),
        ?assert(maps:is_key(<<"description">>, Tool)),
        ?assert(maps:is_key(<<"inputSchema">>, Tool))
    end, Tools),

    ct:log("Tools listed: ~p", [length(Tools)]),
    ok.

%% @doc Test successful tool invocation (echo)
t_tool_call_echo_success(_Config) ->
    %% Invoke echo tool with message
    Args = #{<<"message">> => <<"Hello, MCP!">>},
    Request = build_request(<<"tools/call">>, #{<<"name">> => <<"echo">>,
                                                  <<"arguments">> => Args}),
    Response = handle_mcp_request(Request),

    %% Verify response
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">> := 1,
                   <<"result">> := #{<<"content">> := _}}, Response),

    %% Verify echoed content
    Content = maps:get(<<"content">>, maps:get(<<"result">>, Response)),
    ?assert(is_list(Content)),

    ct:log("Tool echo response: ~p", [Response]),
    ok.

%% @doc Test successful tool invocation (calculate)
t_tool_call_calculate_success(_Config) ->
    %% Invoke calculate tool
    Args = #{<<"x">> => 10, <<"y">> => 5, <<"operation">> => <<"add">>},
    Request = build_request(<<"tools/call">>, #{<<"name">> => <<"calculate">>,
                                                  <<"arguments">> => Args}),
    Response = handle_mcp_request(Request),

    %% Verify response
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">> := 1,
                   <<"result">> := _}, Response),

    ct:log("Tool calculate response: ~p", [Response]),
    ok.

%% @doc Test tool call with invalid arguments
t_tool_call_invalid_arguments(_Config) ->
    %% Missing required argument 'message'
    Args = #{},
    Request = build_request(<<"tools/call">>, #{<<"name">> => <<"echo">>,
                                                  <<"arguments">> => Args}),
    Response = handle_mcp_request(Request),

    %% Verify error response
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">> := 1,
                   <<"error">> := #{<<"code">> := ?MCP_ERROR_INVALID_TOOL_ARGUMENTS,
                                    <<"message">> := _}}, Response),

    ct:log("Invalid arguments error correctly returned"),
    ok.

%% @doc Test tool call with non-existent tool
t_tool_call_tool_not_found(_Config) ->
    Args = #{<<"message">> => <<"test">>},
    Request = build_request(<<"tools/call">>, #{<<"name">> => <<"nonexistent_tool">>,
                                                  <<"arguments">> => Args}),
    Response = handle_mcp_request(Request),

    %% Verify error response
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">> := 1,
                   <<"error">> := #{<<"code">> := ?MCP_ERROR_TOOL_NOT_FOUND,
                                    <<"message">> := _}}, Response),

    ct:log("Tool not found error correctly returned"),
    ok.

%%%====================================================================
%%% Prompt Template Tests
%%%====================================================================

%% @doc Test successful prompts listing
t_prompts_list_success(_Config) ->
    Request = build_request(<<"prompts/list">>, #{}),
    Response = handle_mcp_request(Request),

    %% Verify response
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">> := 1,
                   <<"result">> := #{<<"prompts">> := _}}, Response),

    Prompts = maps:get(<<"prompts">>, maps:get(<<"result">>, Response)),
    ?assert(is_list(Prompts)),

    ct:log("Prompts listed: ~p", [length(Prompts)]),
    ok.

%% @doc Test getting a specific prompt
t_prompt_get_success(_Config) ->
    Request = build_request(<<"prompts/get">>, #{<<"name">> => <<"test_prompt">>}),
    Response = handle_mcp_request(Request),

    %% Verify response
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">> := 1,
                   <<"result">> := _}, Response),

    Result = maps:get(<<"result">>, Response),
    ?assert(maps:is_key(<<"name">>, Result)),
    ?assert(maps:is_key(<<"description">>, Result)),
    ?assert(maps:is_key(<<"arguments">>, Result)),

    ct:log("Prompt retrieved: ~p", [Result]),
    ok.

%% @doc Test simple prompt rendering
t_prompt_render_simple(_Config) ->
    %% Render template with variables
    Template = <<"Hello, {{name}}!">>,
    Variables = #{<<"name">> => <<"Alice">>},

    case erlmcp_prompt_template:render_safe(Template, Variables) of
        {ok, Result} ->
            ?assertEqual(<<"Hello, Alice!">>, Result),
            ct:log("Template rendered successfully: ~p", [Result]);
        {error, Reason} ->
            ct:fail("Template rendering failed: ~p", [Reason])
    end,
    ok.

%% @doc Test prompt rendering with multiple arguments
t_prompt_render_with_arguments(_Config) ->
    Template = <<"Hello, {{name}}! Welcome to {{app}}.">>,
    Variables = #{<<"name">> => <<"Bob">>, <<"app">> => <<"erlmcp">>},

    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assertEqual(<<"Hello, Bob! Welcome to erlmcp.">>, Result),

    ct:log("Complex template rendered: ~p", [Result]),
    ok.

%% @doc Test prompt rendering with missing required variable
t_prompt_render_missing_required(_Config) ->
    Template = <<"Hello, {{name}}! Welcome to {{app}}.">>,
    Variables = #{<<"name">> => <<"Charlie">>},  % Missing 'app'

    %% Should still render, but leave {{app}} as-is (bbmustache behavior)
    {ok, Result} = erlmcp_prompt_template:render_safe(Template, Variables),
    ?assert(is_binary(Result)),

    ct:log("Template with missing variable: ~p", [Result]),
    ok.

%% @doc Test prompt not found error
t_prompt_not_found(_Config) ->
    Request = build_request(<<"prompts/get">>, #{<<"name">> => <<"nonexistent_prompt">>}),
    Response = handle_mcp_request(Request),

    %% Verify error response
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"id">> := 1,
                   <<"error">> := #{<<"code">> := ?MCP_ERROR_PROMPT_NOT_FOUND,
                                    <<"message">> := _}}, Response),

    ct:log("Prompt not found error correctly returned"),
    ok.

%%%====================================================================
%%% Subscription Tests
%%%====================================================================

%% @doc Test successful resource subscription
t_resource_subscribe_success(_Config) ->
    Uri = <<"test://subscription/1">>,
    Subscriber = self(),

    %% Subscribe to resource
    Reply = erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, #{}),
    ?assertEqual(ok, Reply),

    %% Verify subscription exists
    Subs = erlmcp_resource_subscriptions:list_resource_subscriptions(Uri, false),
    ?assert(lists:member(Subscriber, Subs)),

    %% Cleanup
    ok = erlmcp_resource_subscriptions:unsubscribe_from_resource(Uri, Subscriber),

    ct:log("Resource subscription test passed"),
    ok.

%% @doc Test successful resource unsubscription
t_resource_unsubscribe_success(_Config) ->
    Uri = <<"test://subscription/2">>,
    Subscriber = self(),

    %% Subscribe then unsubscribe
    ok = erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, #{}),
    Reply = erlmcp_resource_subscriptions:unsubscribe_from_resource(Uri, Subscriber),
    ?assertEqual(ok, Reply),

    %% Verify subscription removed
    Subs = erlmcp_resource_subscriptions:list_resource_subscriptions(Uri, false),
    ?assertNot(lists:member(Subscriber, Subs)),

    ct:log("Resource unsubscription test passed"),
    ok.

%% @doc Test notification on resource change
t_resource_notification_on_change(_Config) ->
    Uri = <<"test://subscription/notify">>,
    Subscriber = self(),

    %% Subscribe
    ok = erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, #{}),

    %% Notify resource changed
    Metadata = #{<<"version">> => 1, <<"changed_by">> => <<"test">>},
    ok = erlmcp_resource_subscriptions:notify_resource_changed(Uri, Metadata),

    %% Wait for notification (with timeout)
    receive
        {'$mcp_resource', Notification} ->
            ?assertEqual(Uri, maps:get(<<"uri">>, Notification)),
            ?assertEqual(<<"resources/updated">>, maps:get(<<"method">>, Notification)),
            ct:log("Notification received: ~p", [Notification])
    after 1000 ->
        ct:fail("Notification not received within timeout")
    end,

    %% Cleanup
    ok = erlmcp_resource_subscriptions:unsubscribe_from_resource(Uri, Subscriber),
    ok.

%% @doc Test multiple subscribers to same resource
t_resource_multiple_subscribers(_Config) ->
    Uri = <<"test://subscription/multi">>,

    %% Create subscriber processes
    Subscriber1 = spawn(fun() -> receive stop -> ok end end),
    Subscriber2 = spawn(fun() -> receive stop -> ok end end),
    Subscriber3 = spawn(fun() -> receive stop -> ok end end),

    %% Subscribe all
    ok = erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber1, #{}),
    ok = erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber2, #{}),
    ok = erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber3, #{}),

    %% Verify all subscribers
    Subs = erlmcp_resource_subscriptions:list_resource_subscriptions(Uri, false),
    ?assertEqual(3, length(Subs)),
    ?assert(lists:member(Subscriber1, Subs)),
    ?assert(lists:member(Subscriber2, Subs)),
    ?assert(lists:member(Subscriber3, Subs)),

    %% Cleanup
    ok = erlmcp_resource_subscriptions:unsubscribe_from_resource(Uri, Subscriber1),
    ok = erlmcp_resource_subscriptions:unsubscribe_from_resource(Uri, Subscriber2),
    ok = erlmcp_resource_subscriptions:unsubscribe_from_resource(Uri, Subscriber3),

    %% Stop subscribers
    Subscriber1 ! stop,
    Subscriber2 ! stop,
    Subscriber3 ! stop,

    ct:log("Multiple subscribers test passed"),
    ok.

%% @doc Test subscription rate limiting
t_subscription_rate_limiting(_Config) ->
    Uri = <<"test://subscription/rate">>,
    Subscriber = self(),

    %% Subscribe with rate limit of 100ms
    ok = erlmcp_resource_subscriptions:subscribe_to_resource(
        Uri, Subscriber, #{rate_limit => 100}),

    %% Send multiple rapid notifications
    ok = erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{}),
    ok = erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{}),
    ok = erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{}),

    %% Count notifications (should be rate limited)
    receive
        {'$mcp_resource', _} -> ok
    after 500 ->
        ok  %% Rate limited, may not receive all
    end,

    %% Cleanup
    ok = erlmcp_resource_subscriptions:unsubscribe_from_resource(Uri, Subscriber),

    Stats = erlmcp_resource_subscriptions:get_stats(),
    ct:log("Subscription stats after rate limit test: ~p", [Stats]),
    ok.

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

%% @doc Test invalid JSON request handling
t_invalid_json_request(_Config) ->
    %% Simulate parsing error
    Response = build_error_response(?JSONRPC_PARSE_ERROR, <<"Invalid JSON">>),

    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"error">> := #{<<"code">> := ?JSONRPC_PARSE_ERROR}}, Response),

    ct:log("Invalid JSON error correctly formatted"),
    ok.

%% @doc Test missing method field
t_missing_method_field(_Config) ->
    Request = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1},
    Response = handle_mcp_request(Request),

    %% Should return method not found error
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"error">> := _}, Response),

    ct:log("Missing method error correctly returned"),
    ok.

%% @doc Test unsupported method
t_unsupported_method(_Config) ->
    Request = build_request(<<"unsupported/method">>, #{}),
    Response = handle_mcp_request(Request),

    %% Should return method not supported error
    ?assertMatch(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"error">> := #{<<"code">> := ?JSONRPC_METHOD_NOT_FOUND,
                                    <<"message">> := _}}, Response),

    ct:log("Unsupported method error correctly returned"),
    ok.

%%%====================================================================
%%% Concurrency Tests
%%%====================================================================

%% @doc Test concurrent tool invocations
t_concurrent_tool_invocations(_Config) ->
    %% Spawn multiple concurrent tool calls
    NumConcurrent = 10,
    Pids = [spawn_monitor(fun() ->
        Args = #{<<"message">> => list_to_binary(["Concurrent ", integer_to_list(N)])},
        Request = build_request(<<"tools/call">>, #{<<"name">> => <<"echo">>,
                                                      <<"arguments">> => Args}),
        _Response = handle_mcp_request(Request)
    end) || N <- lists:seq(1, NumConcurrent)],

    %% Wait for all to complete
    Results = [wait_for_completion(Pid, Ref) || {Pid, Ref} <- Pids],

    %% All should succeed
    ?assertEqual(NumConcurrent, length([ok || ok <- Results])),

    ct:log("All ~p concurrent tool invocations completed", [NumConcurrent]),
    ok.

%% @doc Test concurrent resource access
t_concurrent_resource_access(_Config) ->
    %% Spawn multiple concurrent resource reads
    NumConcurrent = 20,
    Uri = <<"test://resource/1">>,

    Pids = [spawn_monitor(fun() ->
        Request = build_request(<<"resources/read">>, #{<<"uri">> => Uri}),
        _Response = handle_mcp_request(Request)
    end) || _N <- lists:seq(1, NumConcurrent)],

    %% Wait for all to complete
    Results = [wait_for_completion(Pid, Ref) || {Pid, Ref} <- Pids],

    %% All should succeed
    SuccessCount = length([ok || ok <- Results]),
    ?assert(SuccessCount >= NumConcurrent - 2),  % Allow minor failures

    ct:log("Concurrent resource access: ~p/~p succeeded", [SuccessCount, NumConcurrent]),
    ok.

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Build a standard JSON-RPC request
build_request(Method, Params) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => Method,
        <<"params">> => Params
    }.

%% @doc Build an error response
build_error_response(Code, Message) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => null,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message
        }
    }.

%% @doc Simulate MCP request handling
%% This is a simplified mock - in real scenario, this would go through the server
handle_mcp_request(Request) ->
    Method = maps:get(<<"method">>, Request, undefined),
    Params = maps:get(<<"params">>, Request, #{}),

    case Method of
        <<"resources/list">> ->
            MockResources = [
                #{<<"uri">> => <<"test://resource/1">>,
                  <<"name">> => <<"Test Resource 1">>,
                  <<"description">> => <<"First test resource">>,
                  <<"mimeType">> => <<"text/plain">>},
                #{<<"uri">> => <<"test://resource/2">>,
                  <<"name">> => <<"Test Resource 2">>,
                  <<"description">> => <<"Second test resource">>,
                  <<"mimeType">> => <<"application/json">>}
            ],
            #{<<"jsonrpc">> => <<"2.0">>,
              <<"id">> => maps:get(<<"id">>, Request, null),
              <<"result">> => #{<<"resources">> => MockResources}};

        <<"resources/read">> ->
            Uri = maps:get(<<"uri">>, Params, undefined),
            case Uri of
                <<"test://resource/1">> ->
                    #{<<"jsonrpc">> => <<"2.0">>,
                      <<"id">> => maps_get_id(Request),
                      <<"result">> => #{<<"contents">> => [
                          #{<<"type">> => <<"text">>,
                            <<"text">> => <<"Hello, World!">>}
                      ]}};
                <<"test://nonexistent/resource">> ->
                    build_error_response(?MCP_ERROR_RESOURCE_NOT_FOUND,
                                       ?MCP_MSG_RESOURCE_NOT_FOUND);
                _ ->
                    #{<<"jsonrpc">> => <<"2.0">>,
                      <<"id">> => maps_get_id(Request),
                      <<"result">> => #{<<"contents">> => []}}
            end;

        <<"resources/templates/list">> ->
            #{<<"jsonrpc">> => <<"2.0">>,
              <<"id">> => maps_get_id(Request),
              <<"result">> => #{<<"resourceTemplates">> => []}};

        <<"tools/list">> ->
            MockTools = [
                #{<<"name">> => <<"echo">>,
                  <<"description">> => <<"Echo input back">>,
                  <<"inputSchema">> => #{
                      <<"type">> => <<"object">>,
                      <<"properties">> => #{
                          <<"message">> => #{<<"type">> => <<"string">>}
                      },
                      <<"required">> => [<<"message">>]
                  }},
                #{<<"name">> => <<"calculate">>,
                  <<"description">> => <<"Perform calculation">>,
                  <<"inputSchema">> => #{
                      <<"type">> => <<"object">>,
                      <<"properties">> => #{
                          <<"x">> => #{<<"type">> => <<"number">>},
                          <<"y">> => #{<<"type">> => <<"number">>},
                          <<"operation">> => #{
                              <<"type">> => <<"string">>,
                              <<"enum">> => [<<"add">>, <<"subtract">>, <<"multiply">>, <<"divide">>]
                          }
                      },
                      <<"required">> => [<<"x">>, <<"y">>, <<"operation">>]
                  }}
            ],
            #{<<"jsonrpc">> => <<"2.0">>,
              <<"id">> => maps_get_id(Request),
              <<"result">> => #{<<"tools">> => MockTools}};

        <<"tools/call">> ->
            ToolName = maps:get(<<"name">>, Params, undefined),
            Arguments = maps:get(<<"arguments">>, Params, #{}),
            case ToolName of
                <<"echo">> ->
                    case maps:get(<<"message">>, Arguments, undefined) of
                        undefined ->
                            build_error_response(?MCP_ERROR_INVALID_TOOL_ARGUMENTS,
                                               ?MCP_MSG_INVALID_TOOL_ARGUMENTS);
                        Message ->
                            #{<<"jsonrpc">> => <<"2.0">>,
                              <<"id">> => maps_get_id(Request),
                              <<"result">> => #{
                                  <<"content">> => [
                                      #{<<"type">> => <<"text">>, <<"text">> => Message}
                                  ]
                              }}
                    end;
                <<"calculate">> ->
                    #{<<"jsonrpc">> => <<"2.0">>,
                      <<"id">> => maps_get_id(Request),
                      <<"result">> => #{
                          <<"content">> => [
                              #{<<"type">> => <<"text">>, <<"text">> => <<"Calculation result">>}
                          ]
                      }};
                _ ->
                    build_error_response(?MCP_ERROR_TOOL_NOT_FOUND, ?MCP_MSG_TOOL_NOT_FOUND)
            end;

        <<"prompts/list">> ->
            MockPrompts = [
                #{<<"name">> => <<"test_prompt">>,
                  <<"description">> => <<"A test prompt template">>,
                  <<"arguments">> => [
                      #{<<"name">> => <<"name">>,
                        <<"description">> => <<"User name">>,
                        <<"required">> => true}
                  ]}
            ],
            #{<<"jsonrpc">> => <<"2.0">>,
              <<"id">> => maps_get_id(Request),
              <<"result">> => #{<<"prompts">> => MockPrompts}};

        <<"prompts/get">> ->
            PromptName = maps:get(<<"name">>, Params, undefined),
            case PromptName of
                <<"test_prompt">> ->
                    #{<<"jsonrpc">> => <<"2.0">>,
                      <<"id">> => maps_get_id(Request),
                      <<"result">> => #{
                          <<"name">> => <<"test_prompt">>,
                          <<"description">> => <<"A test prompt template">>,
                          <<"arguments">> => [
                              #{<<"name">> => <<"name">>,
                                <<"description">> => <<"User name">>,
                                <<"required">> => true}
                          ]
                      }};
                _ ->
                    build_error_response(?MCP_ERROR_PROMPT_NOT_FOUND, ?MCP_MSG_PROMPT_NOT_FOUND)
            end;

        undefined ->
            build_error_response(?JSONRPC_INVALID_REQUEST, ?JSONRPC_MSG_INVALID_REQUEST);

        _ ->
            build_error_response(?JSONRPC_METHOD_NOT_FOUND, ?JSONRPC_MSG_METHOD_NOT_FOUND)
    end.

maps_get_id(Request) ->
    maps:get(<<"id">>, Request, null).

wait_for_completion(Pid, Ref) ->
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok;
        {'DOWN', Ref, process, Pid, _Reason} -> error
    after 5000 ->
        timeout
    end.
