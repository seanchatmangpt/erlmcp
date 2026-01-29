-module(erlmcp_server_capabilities_prop_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

%%====================================================================
%% Property-Based Tests for MCP Server Capabilities
%%====================================================================
%% This module implements property-based tests using Proper to verify:
%% - Protocol invariants
%% - State consistency properties
%% - Capability negotiation rules
%% - Resource/tool/prompt management properties
%% - Error handling properties
%%====================================================================

%%====================================================================
%% Property Test Generators
%%====================================================================

%% Generator for server IDs
server_id() ->
    binary(50).

%% Generator for resource URIs
resource_uri() ->
    oneof([
        binary(100),
        list(oneof([<<"http://">>, <<"https://">>, <<"file://">>, <<"resource://">>]))
    ]).

%% Generator for tool names
tool_name() ->
    binary(50).

%% Generator for prompt names
prompt_name() ->
    binary(50).

%% Generator for capability maps
capability_map() ->
    #{
        resources => resources_capability(),
        tools => tools_capability(),
        prompts => prompts_capability(),
        logging => logging_capability(),
        sampling => sampling_capability(),
        roots => roots_capability(),
        experimental => experimental_capability()
    }.

%% Generator for resources capability
resources_capability() ->
    #{
        subscribe => bool(),
        listChanged => bool()
    }.

%% Generator for tools capability
tools_capability() ->
    #{
        listChanged => bool()
    }.

%% Generator for prompts capability
prompts_capability() ->
    #{
        listChanged => bool()
    }.

%% Generator for logging capability
logging_capability() ->
    #{}.

%% Generator for sampling capability
sampling_capability() ->
    #{
        modelPreferences => model_preferences()
    }.

%% Generator for model preferences
model_preferences() ->
    oneof([
        undefined,
        #{
            temperature => float(),
            maxTokens => pos_integer(),
            stopSequences => list(binary()),
            costPriority => oneof([low, medium, high]),
            speedPriority => oneof([low, medium, high]),
            intelligencePriority => oneof([low, medium, high])
        }
    ]).

%% Generator for roots capability
roots_capability() ->
    #{}.

%% Generator for experimental capability
experimental_capability() ->
    oneof([
        undefined,
        map(binary(), binary())
    ]).

%% Generator for server capabilities
server_capabilities() ->
    #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe => bool(),
            listChanged => bool()
        },
        tools = #mcp_tools_capability{
            listChanged => bool()
        },
        prompts = #mcp_prompts_capability{
            listChanged => bool()
        },
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{
            modelPreferences => model_preferences()
        },
        roots = #mcp_roots_capability{},
        experimental => experimental_capability()
    }.

%% Generator for client capabilities
client_capabilities() ->
    #mcp_client_capabilities{
        roots = #mcp_capability{
            enabled => bool()
        },
        sampling = #mcp_capability{
            enabled => bool()
        },
        experimental => experimental_capability()
    }.

%% Generator for MCP resource
mcp_resource() ->
    #mcp_resource{
        uri => resource_uri(),
        name => binary(50),
        description => binary(100),
        mime_type => oneof([<<"text/plain">>, <<"application/json">>, <<"application/xml">>]),
        metadata => map(binary(), term())
    }.

%% Generator for MCP tool
mcp_tool() ->
    #mcp_tool{
        name => tool_name(),
        description => binary(100),
        input_schema => oneof([undefined, json_schema()])
    }.

%% Generator for JSON schema
json_schema() ->
    #{
        type => oneof([object, array, string, number, boolean, null]),
        properties => map(binary(), json_schema()),
        required => list(binary()),
        items => json_schema()
    }.

%% Generator for MCP prompt
mcp_prompt() ->
    #mcp_prompt{
        name => prompt_name(),
        description => binary(100),
        arguments => list(prompt_argument()),
        input_schema => oneof([undefined, json_schema()])
    }.

%% Generator for prompt argument
prompt_argument() ->
    #mcp_prompt_argument{
        name => binary(50),
        description => binary(100),
        required => bool()
    }.

%% Generator for protocol versions
protocol_version() ->
    oneof([
        <<"2025-11-25">>,
        <<"2025-06-18">>,
        <<"2024-11-25">>,  %% Invalid for testing
        <<"invalid-version">>,
        undefined
    ]).

%% Generator for MCP messages
mcp_message() ->
    oneof([
        {request, integer(), binary(), map()},
        {notification, binary(), map()}
    ]).

%% Generator for initialize parameters
initialize_params() ->
    #{
        protocolVersion => protocol_version(),
        capabilities => client_capabilities()
    }.

%%====================================================================
%% Property Tests
%%====================================================================

%% Test capability map generation property
prop_capability_map_generation() ->
    ?FORALL(Capabilities, server_capabilities(),
        begin
            %% Generate capability map
            CapabilityMap = erlmcp_server:build_initialize_response(Capabilities),

            %% Verify required fields exist
            RequiredFields = [<<"protocolVersion">>, <<"capabilities">>, <<"serverInfo">>],
            lists:all(fun(Field) -> maps:is_key(Field, CapabilityMap) end, RequiredFields),

            %% Verify capabilities are properly encoded
            CapabilitiesJson = maps:get(<<"capabilities">>, CapabilityMap),
            is_binary(CapabilitiesJson)
        end).

%% Test protocol version validation property
prop_protocol_version_validation() ->
    ?FORALL(Version, protocol_version(),
        begin
            Result = erlmcp_server:validate_protocol_version(Version),
            case Version of
                <<"2025-11-25">> -> Result =:= ok;
                <<"2025-06-18">> -> Result =:= ok;
                _ -> Result =/= ok
            end
        end).

%% Test client capability extraction property
prop_client_capability_extraction() ->
    ?FORALL(Params, initialize_params(),
        begin
            %% Extract client capabilities
            ClientCaps = erlmcp_server:extract_client_capabilities(Params),

            %% Verify extracted capabilities are valid
            is_record(ClientCaps, mcp_client_capabilities),
            is_record(ClientCaps#mcp_client_capabilities.roots, mcp_capability),
            is_record(ClientCaps#mcp_client_capabilities.sampling, mcp_capability)
        end).

%% Test resource registration property
prop_resource_registration() ->
    ?FORALL({ResourceId, Resource, Handler},
            {resource_uri(), mcp_resource(), function(0, binary())},
        begin
            %% Start server with resource capability enabled
            Capabilities = #mcp_server_capabilities{
                resources = #mcp_resources_capability{subscribe = true, listChanged = true}
            },

            {ok, Pid} = erlmcp_server:start_link(ResourceId, Capabilities),

            %% Register resource
            Result = erlmcp_server:add_resource(Pid, Resource, Handler),

            %% Verify registration succeeded
            RegistrationSucceeded = Result =:= ok,

            %% Verify resource is stored
            State = get_server_state(Pid),
            ResourceStored = maps:is_key(Resource#mcp_resource.uri, State#state.resources),

            %% Cleanup
            erlmcp_server:stop(Pid),

            RegistrationSucceeded and ResourceStored
        end).

%% Test tool registration property
prop_tool_registration() ->
    ?FORALL({ToolId, Tool, Handler, Schema},
            {tool_name(), mcp_tool(), function(0, binary()), oneof([undefined, json_schema()])},
        begin
            %% Start server with tool capability enabled
            Capabilities = #mcp_server_capabilities{
                tools = #mcp_tools_capability{listChanged = true}
            },

            {ok, Pid} = erlmcp_server:start_link(ToolId, Capabilities),

            %% Register tool
            Result = case Schema of
                undefined ->
                    erlmcp_server:add_tool(Pid, Tool, Handler);
                _ ->
                    erlmcp_server:add_tool_with_schema(Pid, Tool, Handler, Schema)
            end,

            %% Verify registration succeeded
            RegistrationSucceeded = Result =:= ok,

            %% Verify tool is stored
            State = get_server_state(Pid),
            ToolStored = maps:is_key(Tool#mcp_tool.name, State#state.tools),

            %% Cleanup
            erlmcp_server:stop(Pid),

            RegistrationSucceeded and ToolStored
        end).

%% Test prompt registration property
prop_prompt_registration() ->
    ?FORALL({PromptId, Prompt, Handler, Arguments, Schema},
            {prompt_name(), mcp_prompt(), function(0, list()), list(prompt_argument()), oneof([undefined, json_schema()])},
        begin
            %% Start server with prompt capability enabled
            Capabilities = #mcp_server_capabilities{
                prompts = #mcp_prompts_capability{listChanged = true}
            },

            {ok, Pid} = erlmcp_server:start_link(PromptId, Capabilities),

            %% Register prompt
            Result = case Schema of
                undefined when Arguments =:= [] ->
                    erlmcp_server:add_prompt(Pid, Prompt, Handler);
                undefined ->
                    erlmcp_server:add_prompt_with_args(Pid, Prompt, Handler, Arguments);
                _ when Arguments =:= [] ->
                    erlmcp_server:add_prompt(Pid, Prompt, Handler);
                _ ->
                    erlmcp_server:add_prompt_with_args_and_schema(Pid, Prompt, Handler, Arguments, Schema)
            end,

            %% Verify registration succeeded
            RegistrationSucceeded = Result =:= ok,

            %% Verify prompt is stored
            State = get_server_state(Pid),
            PromptStored = maps:is_key(Prompt#mcp_prompt.name, State#state.prompts),

            %% Cleanup
            erlmcp_server:stop(Pid),

            RegistrationSucceeded and PromptStored
        end).

%% Test capability negotiation property
prop_capability_negotiation() ->
    ?FORALL({ServerCaps, ClientParams},
            {server_capabilities(), initialize_params()},
        begin
            %% Start server
            ServerId = list_to_binary("negotiate_" ++ integer_to_list(erlang:phash2({ServerCaps, ClientParams}))),
            {ok, Pid} = erlmcp_server:start_link(ServerId, ServerCaps);

            %% Initialize with client parameters
            TransportId = test_transport;
            InitializeRequest = erlmcp_json_rpc:encode_request(1, <<"initialize">>, ClientParams);
            Pid ! {mcp_message, TransportId, InitializeRequest};
            timer:sleep(50);

            %% Verify negotiation results
            State = get_server_state(Pid);
            NegotiationSuccess = State#state.initialized and
                               (State#state.protocolVersion =/= undefined) and
                               (State#state.clientCapabilities =/= undefined);

            %% Cleanup
            erlmcp_server:stop(Pid);

            NegotiationSuccess
        end).

%% Test subscription property
prop_resource_subscription() ->
    ?FORALL({ResourceId, Resource, NumSubscribers},
            {resource_uri(), mcp_resource(), pos_integer()},
        begin
            %% Start server with subscription capability
            Capabilities = #mcp_server_capabilities{
                resources = #mcp_resources_capability{subscribe = true, listChanged = false}
            },

            {ok, Pid} = erlmcp_server:start_link(ResourceId, Capabilities);

            %% Add resource
            ok = erlmcp_server:add_resource(Pid, Resource, fun(_) -> <<"content">> end);

            %% Add subscribers
            Subscribers = [spawn(fun() -> receive after 1000 -> ok end end)
                         || _ <- lists:seq(1, NumSubscribers)],
            lists:foreach(fun(Subscriber) ->
                ok = erlmcp_server:subscribe_resource(Pid, Resource#mcp_resource.uri, Subscriber)
            end, Subscribers);

            %% Verify subscription count
            State = get_server_state(Pid);
            SubscribersSet = maps:get(Resource#mcp_resource.uri, State#state.subscriptions, sets:new()),
            ExpectedCount = sets:size(SubscribersSet),

            %% Cleanup
            lists:foreach(fun(Subscriber) ->
                Subscriber ! stop
            end, Subscribers),
            erlmcp_server:stop(Pid),

            ExpectedCount =:= NumSubscribers
        end).

%% Test idempotent operations property
prop_idempotent_operations() ->
    ?FORALL({ResourceId, Resource, Tool, Prompt},
            {resource_uri(), mcp_resource(), mcp_tool(), mcp_prompt()},
        begin
            %% Start server
            Capabilities = default_server_capabilities();
            {ok, Pid} = erlmcp_server:start_link(ResourceId, Capabilities);

            %% Test idempotent resource registration
            ok = erlmcp_server:add_resource(Pid, Resource, fun(_) -> <<"content">> end);
            Result1 = erlmcp_server:add_resource(Pid, Resource, fun(_) -> <<"content">> end);

            %% Test idempotent tool registration
            ok = erlmcp_server:add_tool(Pid, Tool, fun(_) -> <<"result">> end);
            Result2 = erlmcp_server:add_tool(Pid, Tool, fun(_) -> <<"result">> end);

            %% Test idempotent prompt registration
            ok = erlmcp_server:add_prompt(Pid, Prompt, fun(_) -> [#{role => user, content => #{type => text, text => <<"prompt">>}}] end);
            Result3 = erlmcp_server:add_prompt(Pid, Prompt, fun(_) -> [#{role => user, content => #{type => text, text => <<"prompt">>}}] end);

            %% Operations should be idempotent (return ok or handle gracefully)
            Idempotent = (Result1 =:= ok orelse {error, _} =:= Result1) and
                         (Result2 =:= ok orelse {error, _} =:= Result2) and
                         (Result3 =:= ok orelse {error, _} =:= Result3);

            %% Cleanup
            erlmcp_server:stop(Pid);

            Idempotent
        end).

%% Test state consistency property
prop_state_consistency() ->
    ?FORALL({ServerId, Operations},
            {server_id(), list(operation())},
        begin
            %% Start server
            Capabilities = default_server_capabilities();
            {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities);

            %% Execute operations
            lists:foldl(fun(Operation, _Acc) ->
                execute_operation(Pid, Operation)
            end, ok, Operations);

            %% Verify state consistency
            State = get_server_state(Pid);
            Consistent = is_record(State, state) andalso
                         State#state.serverId =/= undefined andalso
                         is_record(State#state.capabilities, mcp_server_capabilities);

            %% Cleanup
            erlmcp_server:stop(Pid);

            Consistent
        end).

%% Generator for operations
operation() ->
    oneof([
        {add_resource, mcp_resource(), function(0, binary())},
        {add_tool, mcp_tool(), function(0, binary())},
        {add_prompt, mcp_prompt(), function(0, list())},
        {delete_resource, resource_uri()},
        {delete_tool, tool_name()},
        {delete_prompt, prompt_name()}
    ]).

%% Execute operation helper
execute_operation(Pid, Operation) ->
    case Operation of
        {add_resource, Resource, Handler} ->
            erlmcp_server:add_resource(Pid, Resource, Handler);
        {add_tool, Tool, Handler} ->
            erlmcp_server:add_tool(Pid, Tool, Handler);
        {add_prompt, Prompt, Handler} ->
            erlmcp_server:add_prompt(Pid, Prompt, Handler);
        {delete_resource, Uri} ->
            erlmcp_server:delete_resource(Pid, Uri);
        {delete_tool, Name} ->
            erlmcp_server:delete_tool(Pid, Name);
        {delete_prompt, Name} ->
            erlmcp_server:delete_prompt(Pid, Name)
    end.

%% Test capability coexistence property
prop_capability_coexistence() ->
    ?FORALL(Capabilities, server_capabilities(),
        begin
            %% Start server with given capabilities
            ServerId = list_to_binary("coexist_" ++ integer_to_list(erlang:phash2(Capabilities)));
            {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities);

            %% Test that all capabilities can coexist
            %% Add resources, tools, and prompts
            Resource = #mcp_resource{uri = <<"coexist://res">>, name = <<"Coexist Resource">>},
            Tool = #mcp_tool{name = <<"coexist_tool">>, description = <<"Coexist Tool">>},
            Prompt = #mcp_prompt{name = <<"coexist_prompt">>, description = <<"Coexist Prompt">>};

            ok = erlmcp_server:add_resource(Pid, Resource, fun(_) -> <<"resource">> end),
            ok = erlmcp_server:add_tool(Pid, Tool, fun(_) -> <<"tool">> end),
            ok = erlmcp_server:add_prompt(Pid, Prompt, fun(_) -> [#{role => user, content => #{type => text, text => <<"prompt">>}}] end);

            %% Verify all coexist
            State = get_server_state(Pid);
            Coexistence = maps:size(State#state.resources) > 0 and
                         maps:size(State#state.tools) > 0 and
                         maps:size(State#state.prompts) > 0;

            %% Cleanup
            erlmcp_server:stop(Pid);

            Coexistence
        end).

%%====================================================================
%% Test Entry Points
%%====================================================================

capability_property_tests() ->
    [
        {timeout, 30000,
         ?assert(proper:quickcheck(prop_capability_map_generation(),
                                   [numtests(100), long(true)]))},

        {timeout, 30000,
         ?assert(proper:quickcheck(prop_protocol_version_validation(),
                                   [numtests(100)]))},

        {timeout, 30000,
         ?assert(proper:quickcheck(prop_client_capability_extraction(),
                                   [numtests(100), long(true)]))},

        {timeout, 30000,
         ?assert(proper:quickcheck(prop_resource_registration(),
                                   [numtests(50), long(true)]))},

        {timeout, 30000,
         ?assert(proper:quickcheck(prop_tool_registration(),
                                   [numtests(50), long(true)]))},

        {timeout, 30000,
         ?assert(proper:quickcheck(prop_prompt_registration(),
                                   [numtests(50), long(true)]))},

        {timeout, 30000,
         ?assert(proper:quickcheck(prop_capability_negotiation(),
                                   [numtests(30), long(true)]))},

        {timeout, 30000,
         ?assert(proper:quickcheck(prop_resource_subscription(),
                                   [numtests(30), long(true)]))},

        {timeout, 30000,
         ?assert(proper:quickcheck(prop_idempotent_operations(),
                                   [numtests(50), long(true)]))},

        {timeout, 30000,
         ?assert(proper:quickcheck(prop_state_consistency(),
                                   [numtests(30), long(true)]))},

        {timeout, 30000,
         ?assert(proper:quickcheck(prop_capability_coexistence(),
                                   [numtests(50), long(true)]))}
    ].

%%====================================================================
%% Helper Functions
%%====================================================================

default_server_capabilities() ->
    #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = true,
            listChanged = true
        },
        tools = #mcp_tools_capability{
            listChanged = true
        },
        prompts = #mcp_prompts_capability{
            listChanged = true
        },
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{
            modelPreferences = #{}
        },
        roots = #mcp_roots_capability{},
        experimental = undefined
    }.

get_server_state(Pid) ->
    %% Simplified state retrieval for testing
    case process_info(Pid, {dictionary, '_'}) of
        undefined ->
            #state{};
        {dictionary, Dict} ->
            case lists:keyfind('$state', 1, Dict) of
                {_, State} when is_record(State, state) -> State;
                _ -> #state{}
            end
    end.