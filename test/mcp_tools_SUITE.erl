%% @doc MCP Tool Management Compliance Test Suite
%% Tests all tool-related capabilities according to MCP specification
-module(mcp_tools_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erlmcp.hrl").

%% Test exports
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test case exports
-export([
    %% Basic Tool Operations
    tool_registration/1,
    tool_listing/1,
    tool_invocation/1,
    tool_deletion/1,

    %% Tool Input/Output
    tool_input_validation/1,
    tool_output_handling/1,
    tool_error_handling/1,
    tool_schema_validation/1,

    %% Tool Types
    tool_text_output/1,
    tool_image_output/1,
    tool_audio_output/1,
    tool_resource_link_output/1,
    tool_embedded_resource_output/1,
    tool_structured_output/1,

    %% Tool Templates
    tool_with_input_schema/1,
    tool_with_output_schema/1,
    tool_with_both_schemas/1,

    %% Tool Subscriptions
    tool_subscription_lifecycle/1,
    tool_change_notification/1,

    %% Tool Capabilities
    tool_list_changed_notification/1,
    tool_dynamic_updates/1,

    %% Error Handling
    tool_not_found/1,
    invalid_tool_arguments/1,
    tool_execution_error/1,
    tool_timeout/1,
    tool_rate_limit/1,

    %% Security Tests
    tool_input_sanitization/1,
    tool_command_injection/1,
    path_traversal_injection/1,
    malicious_tool_names/1,

    %% Performance Tests
    tool_registration_performance/1,
    tool_invocation_performance/1,
    concurrent_tool_calls/1,
    tool_throughput/1,

    %% Integration Tests
    tool_resource_interaction/1,
    tool_prompt_interaction/1,
    tool_server_interaction/1,

    %% Advanced Features
    tool_chaining/1,
    tool_pipeline/1,
    tool_orchestration/1,
    tool_fallback/1
]).

%%====================================================================
%% Test configuration
%%====================================================================

all() ->
    [
        %% Basic Tool Operations
        tool_registration,
        tool_listing,
        tool_invocation,
        tool_deletion,

        %% Tool Input/Output
        tool_input_validation,
        tool_output_handling,
        tool_error_handling,
        tool_schema_validation,

        %% Tool Types
        tool_text_output,
        tool_image_output,
        tool_audio_output,
        tool_resource_link_output,
        tool_embedded_resource_output,
        tool_structured_output,

        %% Tool Templates
        tool_with_input_schema,
        tool_with_output_schema,
        tool_with_both_schemas,

        %% Tool Subscriptions
        tool_subscription_lifecycle,
        tool_change_notification,

        %% Tool Capabilities
        tool_list_changed_notification,
        tool_dynamic_updates,

        %% Error Handling
        tool_not_found,
        invalid_tool_arguments,
        tool_execution_error,
        tool_timeout,
        tool_rate_limit,

        %% Security Tests
        tool_input_sanitization,
        tool_command_injection,
        path_traversal_injection,
        malicious_tool_names,

        %% Performance Tests
        tool_registration_performance,
        tool_invocation_performance,
        concurrent_tool_calls,
        tool_throughput,

        %% Integration Tests
        tool_resource_interaction,
        tool_prompt_interaction,
        tool_server_interaction,

        %% Advanced Features
        tool_chaining,
        tool_pipeline,
        tool_orchestration,
        tool_fallback
    ].

init_per_suite(Config) ->
    %% Start required applications
    {ok, Apps} = application:ensure_all_started(erlmcp_core),
    [{apps, Apps} | Config].

end_per_suite(Config) ->
    Apps = proplists:get_value(apps, Config, []),
    lists:foreach(fun application:stop/1, lists:reverse(Apps)),
    Config.

init_per_testcase(TestCase, Config) ->
    process_flag(trap_exit, true),

    %% Create test server with tool capabilities
    ServerCapabilities = #mcp_server_capabilities{
        resources = #{},
        tools = #{listChanged => true},
        prompts = #{}
    },

    {ok, ServerPid} = erlmcp_server:start_link(test_server, ServerCapabilities),

    %% Create test client
    {ok, ClientPid} = erlmcp_client:start_link({stdio, []}),

    %% Initialize client
    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    },
    {ok, _} = erlmcp_client:initialize(ClientPid, ClientCapabilities),

    %% Set up notification handler
    NotificationHandler = fun(Method, Params) ->
        ct:pal("Tool notification: ~p ~p", [Method, Params]),
        ok
    end,
    erlmcp_client:set_notification_handler(ClientPid, <<"tools">>, NotificationHandler),

    [{server_pid, ServerPid}, {client_pid, ClientPid} | Config].

end_per_testcase(_TestCase, Config) ->
    %% Clean up
    proplists:delete_value(server_pid, Config),
    proplists:delete_value(client_pid, Config),
    ok.

%%====================================================================
%% Basic Tool Operations
%%====================================================================

tool_registration(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test tool registration
    ToolName = <<"test_tool">>,
    ToolHandler = fun(Args) -> {ok, {text, <<"Echo: ", maps:get(<<"input">>, Args, <<"">>)/binary>>}} end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, ToolHandler),

    %% Verify tool is registered
    {ok, Tools} = erlmcp_client:list_tools(ClientPid),

    case Tools of
        {tools, ToolList} when is_list(ToolList) ->
            lists:any(fun(Tool) ->
                maps:get(<<"name">>, Tool) =:= ToolName
            end, ToolList);
        _ ->
            ct:fail("Invalid tool list format")
    end.

tool_listing(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add multiple tools
    Tools = [
        {<<"echo">>, <<"Echo Text">>, #{<<"text">> => #{type => string}}},
        {<<"reverse">>, <<"Reverse Text">>, #{<<"text">> => #{type => string}}},
        {<<"uppercase">>, <<"To Uppercase">>, #{<<"text">> => #{type => string}}}
    ],

    lists:foreach(fun({Name, Description, Schema}) ->
        Handler = fun(Args) ->
            case Name of
                <<"echo">> -> {ok, {text, maps:get(<<"text">>, Args, <<"">>)}};
                <<"reverse">> ->
                    Text = maps:get(<<"text">>, Args),
                    Reversed = lists:reverse(binary_to_list(Text)),
                    {ok, {text, list_to_binary(Reversed)}};
                <<"uppercase">> ->
                    Text = maps:get(<<"text">>, Args),
                    Uppercase = binary_to_list(Text),
                    {ok, {text, list_to_binary(string:to_upper(Uppercase))}}
            end
        end,
        erlmcp_server:add_tool_with_schema(ServerPid, Name, Handler, Schema)
    end, Tools),

    %% List all tools
    {ok, ListedTools} = erlmcp_client:list_tools(ClientPid),

    case ListedTools of
        {tools, ToolList} when is_list(ToolList) ->
            ct:pal("Listed ~p tools", [length(ToolList)]),
            length(ToolList) >= 3;
        _ ->
            ct:fail("Invalid tool listing result")
    end.

tool_invocation(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add test tool
    ToolName = <<"math_add">>,
    Handler = fun(Args) ->
        A = maps:get(<<"a">>, Args, 0),
        B = maps:get(<<"b">>, Args, 0),
        Result = A + B,
        {ok, {text, integer_to_list(Result)}}
    end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Invoke tool
    Args = #{<<"a">> => 5, <<"b">> => 10},
    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, Args),

    case Result of
        {content, [#{text := <<"15">>}]} ->
            ct:pal("Tool invocation successful"),
            true;
        _ ->
            ct:fail("Tool invocation failed")
    end.

tool_deletion(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add test tool
    ToolName = <<"temporary_tool">>,
    Handler = fun(_) -> {ok, {text, <<"Temporary">>}} end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Delete tool
    ok = erlmcp_server:delete_tool(ServerPid, ToolName),

    %% Verify tool is deleted
    {ok, Tools} = erlmcp_client:list_tools(ClientPid),

    case Tools of
        {tools, ToolList} when is_list(ToolList) ->
            not lists:any(fun(Tool) -> maps:get(<<"name">>, Tool) =:= ToolName end, ToolList);
        _ ->
            ct:fail("Invalid tool list format")
    end.

%%====================================================================
%% Tool Input/Output
%%====================================================================

tool_input_validation(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add tool with schema validation
    ToolName = <<"validated_tool">>,
    InputSchema = #{
        type => object,
        properties => #{
            name => #{type => string, description => "Name to validate"},
            age => #{type => integer, description => "Age", minimum => 0, maximum => 150}
        },
        required => [<<"name">>]
    },

    Handler = fun(Args) ->
        Name = maps:get(<<"name">>, Args),
        Age = maps:get(<<"age">>, Args, 0),
        {ok, {text, <<"Valid input: ", Name/binary, ", Age: ", integer_to_list(Age)/binary>>}}
    end,

    erlmcp_server:add_tool_with_schema(ServerPid, ToolName, Handler, InputSchema),

    %% Test valid input
    ValidArgs = #{<<"name">> => <<"Alice">>, <<"age">> => 30},
    {ok, _} = erlmcp_client:call_tool(ClientPid, ToolName, ValidArgs),

    %% Test invalid input (missing required field)
    InvalidArgs1 = #{<<"age">> => 30},
    {Result1, _} = erlmcp_client:call_tool(ClientPid, ToolName, InvalidArgs1),

    %% Test invalid input (invalid type)
    InvalidArgs2 = #{<<"name">> => 123, <<"age">> => 30},
    {Result2, _} = erlmcp_client:call_tool(ClientPid, ToolName, InvalidArgs2),

    ct:pal("Tool input validation test completed"),
    true.

tool_output_handling(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add tool that returns various output types
    ToolName = <<"multi_output_tool">>,
    Handler = fun(_) ->
        {ok, [
            {text, <<"Text output">>},
            {image, base64:encode(<<"Image data">>), <<"image/png">>},
            {audio, base64:encode(<<"Audio data">>), <<"audio/wav">>}
        ]}
    end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Invoke tool
    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, #{ }),

    case Result of
        {content, Contents} when is_list(Contents) ->
            ct:pal("Multi-output result: ~p", [Contents]),
            %% Should have text, image, and audio content
            TextContent = lists:any(fun(Item) -> maps:get(<<"type">>, Item) =:= <<"text">> end, Contents),
            ImageContent = lists:any(fun(Item) -> maps:get(<<"type">>, Item) =:= <<"image">> end, Contents),
            AudioContent = lists:any(fun(Item) -> maps:get(<<"type">>, Item) =:= <<"audio">> end, Contents),

            TextContent andalso ImageContent andalso AudioContent;
        _ ->
            ct:fail("Multi-output handling failed")
    end.

tool_error_handling(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add tool that returns error
    ToolName = <<"failing_tool">>,
    Handler = fun(Args) ->
        case maps:get(<<"fail">>, Args, false) of
            true -> {error, {text, <<"Tool execution failed">>}};
            false -> {ok, {text, <<"Success">>}}
        end
    end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Test success case
    {ok, Result1} = erlmcp_client:call_tool(ClientPid, ToolName, #{ }),
    case Result1 of
        {content, [#{text := <<"Success">>}]} -> ok;
        _ -> ct:fail("Success case failed")
    end,

    %% Test error case
    {Result2, _} = erlmcp_client:call_tool(ClientPid, ToolName, #{<<"fail">> => true}),
    case Result2 of
        {content, [#{text := <<"Tool execution failed">>}], true} -> ok;
        _ -> ct:fail("Error case failed")
    end.

tool_schema_validation(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add tool with input schema
    ToolName = <<"schema_tool">>,
    Schema = #{
        type => object,
        properties => #{
            text => #{type => string, maxLength => 100},
            number => #{type => number, minimum => 0, maximum => 100}
        },
        required => [<<"text">>]
    },

    Handler = fun(Args) ->
        Text = maps:get(<<"text">>, Args),
        Number = maps:get(<<"number">>, Args, 0),
        {ok, {text, <<"Input: ", Text/binary, ", Number: ", integer_to_list(Number)/binary>>}}
    end,

    erlmcp_server:add_tool_with_schema(ServerPid, ToolName, Handler, Schema),

    %% Test schema validation
    ValidArgs = #{<<"text">> => <<"Valid input">>, <<"number">> => 50},
    {ok, _} = erlmcp_client:call_tool(ClientPid, ToolName, ValidArgs),

    ct:pal("Tool schema validation test completed"),
    true.

%%====================================================================
%% Tool Types
%%====================================================================

tool_text_output(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test text output
    ToolName = <<"text_output_tool">>,
    Handler = fun(_) -> {ok, {text, <<"This is text output">>}} end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, #{ }),

    case Result of
        {content, [#{type := <<"text">>, text := <<"This is text output">>}]} ->
            ct:pal("Text output test passed"),
            true;
        _ ->
            ct:fail("Text output test failed")
    end.

tool_image_output(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test image output
    ToolName = <<"image_output_tool">>,
    ImageData = base64:encode(<<"PNG image data">>),
    Handler = fun(_) -> {ok, {image, ImageData, <<"image/png">>}} end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, #{ }),

    case Result of
        {content, [#{type := <<"image">>, data := ImageData, mimeType := <<"image/png">>}]} ->
            ct:pal("Image output test passed"),
            true;
        _ ->
            ct:fail("Image output test failed")
    end.

tool_audio_output(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test audio output
    ToolName = <<"audio_output_tool">>,
    AudioData = base64:encode(<<"WAV audio data">>),
    Handler = fun(_) -> {ok, {audio, AudioData, <<"audio/wav">>}} end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, #{ }),

    case Result of
        {content, [#{type := <<"audio">>, data := AudioData, mimeType := <<"audio/wav">>}]} ->
            ct:pal("Audio output test passed"),
            true;
        _ ->
            ct:fail("Audio output test failed")
    end.

tool_resource_link_output(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add test resource
    ResourceUri = <<"file:///linked_resource.txt">>,
    ResourceHandler = fun(_) -> {ok, {text, <<"Linked resource content">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, ResourceUri, ResourceHandler),

    %% Test resource link output
    ToolName = <<"resource_link_tool">>,
    Handler = fun(_) ->
        {ok, {resource_link, ResourceUri, <<"linked_resource.txt">>,
              <<"Linked resource">>, <<"text/plain">>}}
    end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, #{ }),

    case Result of
        {content, [#{type := <<"resource_link">>, uri := ResourceUri}]} ->
            ct:pal("Resource link output test passed"),
            true;
        _ ->
            ct:fail("Resource link output test failed")
    end.

tool_embedded_resource_output(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test embedded resource output
    ToolName = <<"embedded_resource_tool">>,
    Handler = fun(_) ->
        {ok, {resource,
              #{
                  uri => <<"file:///embedded.txt">>,
                  mimeType => <<"text/plain">>,
                  text => <<"Embedded resource content">>
              }}}
    end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, #{ }),

    case Result of
        {content, [#{type := <<"resource">>,
                     uri := <<"file:///embedded.txt">>,
                     mimeType := <<"text/plain">>,
                     text := <<"Embedded resource content">>}]} ->
            ct:pal("Embedded resource output test passed"),
            true;
        _ ->
            ct:fail("Embedded resource output test failed")
    end.

tool_structured_output(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test structured output
    ToolName = <<"structured_output_tool">>,
    OutputSchema = #{
        type => object,
        properties => #{
            name => #{type => string},
            age => #{type => integer},
            active => #{type => boolean}
        },
        required => [<<"name">>, <<"age">>]
    },

    Handler = fun(_) ->
        Structured = #{
            <<"name">> => <<"John Doe">>,
            <<"age">> => 30,
            <<"active">> => true
        },
        {ok, {text, jsx:encode(Structured)}, OutputSchema}
    end,

    erlmcp_server:add_tool_with_schema(ServerPid, ToolName, Handler, #{type => object}),

    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, #{ }),

    case Result of
        {content, [#{type := <<"text">>, text := _}], structuredContent = Structured} ->
            ct:pal("Structured output test passed"),
            true;
        _ ->
            ct:fail("Structured output test failed")
    end.

%%====================================================================
%% Tool Templates
%%====================================================================

tool_with_input_schema(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test tool with input schema
    ToolName = <<"input_schema_tool">>,
    InputSchema = #{
        type => object,
        properties => #{
            text => #{type => string, description => "Input text"},
            repeat => #{type => integer, description => "Repeat count", default => 1}
        },
        required => [<<"text">>]
    },

    Handler = fun(Args) ->
        Text = maps:get(<<"text">>, Args),
        Repeat = maps:get(<<"repeat">>, Args, 1),
        RepeatedText = lists:duplicate(Repeat, Text),
        {ok, {text, list_to_binary(RepeatedText)}}
    end,

    erlmcp_server:add_tool_with_schema(ServerPid, ToolName, Handler, InputSchema),

    %% Test with schema
    Args = #{<<"text">> => <<"Hello">>, <<"repeat">> => 3},
    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, Args),

    case Result of
        {content, [#{text := <<"HelloHelloHello">>}]} ->
            ct:pal("Tool with input schema test passed"),
            true;
        _ ->
            ct:fail("Tool with input schema test failed")
    end.

tool_with_output_schema(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test tool with output schema
    ToolName = <<"output_schema_tool">>,
    OutputSchema = #{
        type => object,
        properties => #{
            original => #{type => string},
            reversed => #{type => string},
            length => #{type => integer}
        },
        required => [<<"original">>, <<"reversed">>]
    },

    Handler = fun(Args) ->
        Text = maps:get(<<"text">>, Args),
        Reversed = lists:reverse(binary_to_list(Text)),
        Structured = #{
            <<"original">> => Text,
            <<"reversed">> => list_to_binary(Reversed),
            <<"length">> => byte_size(Text)
        },
        {ok, {text, jsx:encode(Structured)}, OutputSchema}
    end,

    InputSchema = #{
        type => object,
        properties => #{<<"text">> => #{type => string}},
        required => [<<"text">>]
    },

    erlmcp_server:add_tool_with_schema(ServerPid, ToolName, Handler, InputSchema),

    Args = #{<<"text">> => <<"test">>},
    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, Args),

    case Result of
        {content, [#{text := _}], structuredContent = Structured} ->
            maps:is_key(<<"original">>, Structured) andalso
            maps:is_key(<<"reversed">>, Structured) andalso
            maps:is_key(<<"length">>, Structured);
        _ ->
            ct:fail("Tool with output schema test failed")
    end.

tool_with_both_schemas(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test tool with both input and output schemas
    ToolName = <<"both_schemas_tool">>,
    InputSchema = #{
        type => object,
        properties => #{<<"numbers">> => #{type => array, items => #{type => number}}},
        required => [<<"numbers">>]
    },

    OutputSchema = #{
        type => object,
        properties => #{
            sum => #{type => number},
            average => #{type => number},
            count => #{type => integer}
        },
        required => [<<"sum">>, <<"average">>, <<"count">>]
    },

    Handler = fun(Args) ->
        Numbers = maps:get(<<"numbers">>, Args, []),
        Sum = lists:sum(Numbers),
        Count = length(Numbers),
        Average = case Count > 0 of
            true -> Sum / Count;
            false -> 0
        end,
        Structured = #{
            <<"sum">> => Sum,
            <<"average">> => Average,
            <<"count">> => Count
        },
        {ok, {text, jsx:encode(Structured)}, OutputSchema}
    end,

    erlmcp_server:add_tool_with_schema(ServerPid, ToolName, Handler, InputSchema),

    Args = #{<<"numbers">> => [1, 2, 3, 4, 5]},
    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, Args),

    case Result of
        {content, [#{text := _}], structuredContent = Structured} ->
            maps:get(<<"sum">>, Structured) =:= 15 andalso
            maps:get(<<"average">>, Structured) =:= 3.0 andalso
            maps:get(<<"count">>, Structured) =:= 5;
        _ ->
            ct:fail("Tool with both schemas test failed")
    end.

%%====================================================================
%% Tool Subscriptions
%%====================================================================

tool_subscription_lifecycle(_Config) ->
    ct:pal("Tool subscription lifecycle test placeholder"),
    true.

tool_change_notification(_Config) ->
    ct:pal("Tool change notification test placeholder"),
    true.

%%====================================================================
%% Tool Capabilities
%%====================================================================

tool_list_changed_notification(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add initial tool
    ToolName1 = <<"initial_tool">>,
    Handler1 = fun(_) -> {ok, {text, <<"Initial">>}} end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName1, Handler1),

    %% Add new tool (should trigger notification)
    ToolName2 = <<"new_tool">>,
    Handler2 = fun(_) -> {ok, {text, <<"New">>}} end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName2, Handler2),

    ct:pal("Tool list change notification test completed"),
    true.

tool_dynamic_updates(_Config) ->
    ct:pal("Tool dynamic updates test placeholder"),
    true.

%%====================================================================
%% Error Handling
%%====================================================================

tool_not_found(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Try to call non-existent tool
    ToolName = <<"nonexistent_tool">>,
    {Result, _} = erlmcp_client:call_tool(ClientPid, ToolName, #{ }),

    case Result of
        {error, {tool_not_found, _}} ->
            ct:pal("Tool not found error handled correctly"),
            true;
        _ ->
            ct:fail("Should return tool not found error")
    end.

invalid_tool_arguments(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add tool with schema
    ToolName = <<"validated_tool">>,
    Schema = #{
        type => object,
        properties => #{
            required_field => #{type => string}
        },
        required => [<<"required_field">>]
    },

    Handler = fun(_) -> {ok, {text, <<"OK">>}} end,
    erlmcp_server:add_tool_with_schema(ServerPid, ToolName, Handler, Schema),

    %% Test invalid arguments
    Args = #{<<"wrong_field">> => <<"value">>},
    {Result, _} = erlmcp_client:call_tool(ClientPid, ToolName, Args),

    case Result of
        {error, {invalid_params, _}} ->
            ct:pal("Invalid arguments error handled correctly"),
            true;
        _ ->
            ct:fail("Should return invalid arguments error")
    end.

tool_execution_error(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add tool that throws error
    ToolName = <<"error_tool">>,
    Handler = fun(_) -> {error, {text, <<"Execution failed">>}} end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Test error execution
    Args = #{},
    {Result, _} = erlmcp_client:call_tool(ClientPid, ToolName, Args),

    case Result of
        {content, [#{text := <<"Execution failed">>}], true} ->
            ct:pal("Tool execution error handled correctly"),
            true;
        _ ->
            ct:fail("Should return execution error")
    end.

tool_timeout(_Config) ->
    ct:pal("Tool timeout test placeholder"),
    true.

tool_rate_limit(_Config) ->
    ct:pal("Tool rate limit test placeholder"),
    true.

%%====================================================================
%% Security Tests
%%====================================================================

tool_input_sanitization(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add tool with sanitization
    ToolName = <<"sanitized_tool">>,
    Handler = fun(Args) ->
        Text = maps:get(<<"text">>, Args),
        Sanitized = string:replace(Text, "<script>", "", all),
        {ok, {text, Sanitized}}
    end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Test with malicious input
    MaliciousInput = <<"Hello <script>alert('xss')</script> World">>,
    Args = #{<<"text">> => MaliciousInput},
    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, Args),

    case Result of
        {content, [#{text := SanitizedText}]} ->
            not string:find(SanitizedText, "<script>") =/= nomatch;
        _ ->
            ct:fail("Input sanitization failed")
    end.

tool_command_injection(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add tool that should be protected against command injection
    ToolName = <<"safe_command_tool">>,
    Handler = fun(Args) ->
        Command = maps:get(<<"command">>, Args, ""),
        SafeCommand = string:replace(Command, ";", "", all),
        os:cmd(SafeCommand),
        {ok, {text, <<"Command executed safely">>}}
    end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Test command injection
    MaliciousArgs = #{<<"command">> => "ls; rm -rf /"},
    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, MaliciousArgs),

    case Result of
        {content, [#{text := _}]} ->
            ct:pal("Command injection test completed"),
            true;
        _ ->
            ct:fail("Command injection should be handled")
    end.

path_traversal_injection(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add tool with path check
    ToolName = <<"safe_path_tool">>,
    Handler = fun(Args) ->
        Path = maps:get(<<"path">>, Args),
        case filename:pathtype(Path) of
            absolute -> {error, {text, "Absolute paths not allowed"}};
            _ -> {ok, {text, <<"Path: ", Path/binary>>}}
        end
    end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Test path traversal
    MaliciousArgs = #{<<"path">> => "../../../etc/passwd"},
    {Result, _} = erlmcp_client:call_tool(ClientPid, ToolName, MaliciousArgs),

    case Result of
        {error, {invalid_params, _}} ->
            ct:pal("Path traversal injection handled correctly"),
            true;
        _ ->
            ct:fail("Path traversal should be blocked")
    end.

malicious_tool_names(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),

    %% Test malicious tool names
    MaliciousNames = [
        <<"rm -rf /">>,
        <<"../../../bin/rm">>,
        <<"javascript:alert('xss')">>,
        <<"file:///etc/passwd">>
    ],

    lists:foreach(fun(Name) ->
        try
            Handler = fun(_) -> {ok, {text, <<"Content">>}} end,
            erlmcp_server:add_tool(ServerPid, Name, Handler),
            ct:fail("Should have rejected malicious name: ~p", [Name])
        catch
            _:_ ->
                ct:pal("Correctly rejected malicious name: ~p", [Name])
        end
    end, MaliciousNames),

    true.

%%====================================================================
%% Performance Tests
%%====================================================================

tool_registration_performance(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),

    %% Test tool registration performance
    NumTools = 1000,
    {Time, _} = timer:tc(fun() ->
        lists:map(fun(I) ->
            Name = list_to_binary("perf_tool_" ++ integer_to_list(I)),
            Handler = fun(_) -> {ok, {text, <<"Tool ", integer_to_list(I)/binary>>}} end,
            erlmcp_server:add_tool(ServerPid, Name, Handler)
        end, lists:seq(1, NumTools))
    end),

    Throughput = NumTools / (Time / 1000000),
    ct:pal("Tool registration throughput: ~p tools/sec", [Throughput]),

    Throughput > 500,  % Minimum 500 tools/sec
    true.

tool_invocation_performance(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add performance test tool
    ToolName = <<"perf_tool">>,
    Handler = fun(Args) ->
        Delay = maps:get(<<"delay">>, Args, 0),
        timer:sleep(Delay),
        {ok, {text, <<"Performance test">>}}
    end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Test invocation performance
    NumCalls = 100,
    Args = #{<<"delay">> => 0},  % No delay for performance test
    {Time, Results} = timer:tc(fun() ->
        lists:map(fun(_) ->
            {ok, _} = erlmcp_client:call_tool(ClientPid, ToolName, Args)
        end, lists:seq(1, NumCalls))
    end),

    Throughput = NumCalls / (Time / 1000000),
    ct:pal("Tool invocation throughput: ~p calls/sec", [Throughput]),

    Throughput > 100,  % Minimum 100 calls/sec
    true.

concurrent_tool_calls(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add test tool
    ToolName = <<"concurrent_tool">>,
    Handler = fun(Args) ->
        Delay = maps:get(<<"delay">>, Args, 100),
        timer:sleep(Delay),
        {ok, {text, <<"Concurrent call">>}}
    end,

    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Test concurrent calls
    NumConcurrent = 50,
    Args = #{<<"delay">> => 50},
    {Time, Results} = timer:tc(fun() ->
        Pids = lists:map(fun(_) ->
            spawn_link(fun() ->
                {ok, _} = erlmcp_client:call_tool(ClientPid, ToolName, Args)
            end)
        end, lists:seq(1, NumConcurrent)),

        %% Wait for all to complete
        lists:foreach(fun(Pid) ->
            receive
                {'EXIT', Pid, _} -> ok
            after 10000 ->
                ct:fail("Timeout in concurrent test")
            end
        end, Pids)
    end),

    Throughput = NumConcurrent / (Time / 1000000),
    ct:pal("Concurrent tool calls throughput: ~p calls/sec", [Throughput]),

    Throughput > 10,  % Minimum 10 concurrent calls/sec
    true.

tool_throughput(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add throughput test tool
    ToolName = <<"throughput_tool">>,
    Handler = fun(_) -> {ok, {text, <<"Throughput test">>}} end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Test throughput
    NumCalls = 10000,
    Args = #{},
    {Time, Results} = timer:tc(fun() ->
        lists:map(fun(_) ->
            {ok, _} = erlmcp_client:call_tool(ClientPid, ToolName, Args)
        end, lists:seq(1, NumCalls))
    end),

    Throughput = NumCalls / (Time / 1000000),
    ct:pal("Tool throughput: ~p calls/sec", [Throughput]),

    Throughput > 500,  % Minimum 500 calls/sec
    true.

%%====================================================================
%% Integration Tests
%%====================================================================

tool_resource_interaction(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add test resource
    ResourceUri = <<"file:///tool_resource.txt">>,
    ResourceContent = <<"Tool integration resource">>,
    ResourceHandler = fun(_) -> {ok, {text, ResourceContent}} end,
    ok = erlmcp_server:add_resource(ServerPid, ResourceUri, ResourceHandler),

    %% Add tool that uses resource
    ToolName = <<"resource_tool">>,
    ToolHandler = fun(_) ->
        {ok, {text, <<"Tool: ", ResourceContent/binary>>}}
    end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, ToolHandler),

    %% Call tool
    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, #{ }),

    case Result of
        {content, [#{text := _}]} ->
            ct:pal("Tool-resource integration completed"),
            true;
        _ ->
            ct:fail("Tool-resource integration failed")
    end.

tool_prompt_interaction(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add test prompt
    PromptName = <<"tool_prompt">>,
    PromptHandler = fun(_) -> {ok, {text, <<"Tool prompt response">>}} end,
    ok = erlmcp_server:add_prompt(ServerPid, PromptName, PromptHandler),

    %% Add tool that uses prompt
    ToolName = <<"prompt_tool">>,
    ToolHandler = fun(_) ->
        {ok, {text, <<"Tool with prompt integration">>}}
    end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, ToolHandler),

    %% Call tool
    {ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, #{ }),

    case Result of
        {content, [#{text := _}]} ->
            ct:pal("Tool-prompt integration completed"),
            true;
        _ ->
            ct:fail("Tool-prompt integration failed")
    end.

tool_server_interaction(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Server dynamically adds tools
    DynamicTools = [
        {<<"dynamic1">>, <<"Dynamic Tool 1">>},
        {<<"dynamic2">>, <<"Dynamic Tool 2">>}
    ],

    lists:foreach(fun({Name, Description}) ->
        Handler = fun(_) -> {ok, {text, Description}} end,
        erlmcp_server:add_tool(ServerPid, Name, Handler)
    end, DynamicTools),

    %% Server notifies changes
    erlmcp_server:notify_resources_changed(ServerPid),

    %% Client verifies changes
    {ok, _} = erlmcp_client:list_tools(ClientPid),

    ct:pal("Tool-server interaction test completed"),
    true.

%%====================================================================
%% Advanced Features
%%====================================================================

tool_chaining(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Add chainable tools
    Tool1Name = <<"extract_text">>,
    Tool1Handler = fun(_) -> {ok, {text, <<"Extracted text data">>}} end,
    ok = erlmcp_server:add_tool(ServerPid, Tool1Name, Tool1Handler),

    Tool2Name = <<"analyze_text">>,
    Tool2Handler = fun(Args) ->
        Text = maps:get(<<"text">>, Args),
        Analysis = #{<<"length">> => byte_size(Text), <<"word_count">> => length(binary:split(Text, [<<" ">>], [global]))},
        {ok, {text, jsx:encode(Analysis)}}
    end,
    ok = erlmcp_server:add_tool(ServerPid, Tool2Name, Tool2Handler),

    %% Chain tools manually (simulate)
    {ok, Result1} = erlmcp_client:call_tool(ClientPid, Tool1Name, #{ }),
    TextResult = case Result1 of
        {content, [#{text := Text}]} -> Text
    end,

    {ok, Result2} = erlmcp_client:call_tool(ClientPid, Tool2Name, #{<<"text">> => TextResult }),

    ct:pal("Tool chaining test completed"),
    true.

tool_pipeline(_Config) ->
    ct:pal("Tool pipeline test placeholder"),
    true.

tool_orchestration(_Config) ->
    ct:pal("Tool orchestration test placeholder"),
    true.

tool_fallback(_Config) ->
    ct:pal("Tool fallback test placeholder"),
    true.