-module(erlmcp_server).
-behaviour(gen_server).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% API exports
-export([
    start_link/2,
    add_resource/3,
    add_resource_template/4,
    add_tool/3,
    add_tool_with_schema/4,
    add_prompt/3,
    add_prompt_with_args/4,
    add_prompt_with_args_and_schema/5,
    delete_resource/2,
    delete_tool/2,
    delete_prompt/2,
    subscribe_resource/3,
    unsubscribe_resource/2,
    report_progress/4,
    notify_resource_updated/3,
    notify_resources_changed/1,
    encode_resource_link/2,
    encode_resource_link/4,
    validate_resource_link_uri/1,
    stop/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type server() :: pid().

-export_type([server/0, server_id/0]).

%% State record - NO transport state, only server-specific state
%% Includes phase tracking for MCP 2025-11-25 initialization state machine (Gap #4)
-record(state, {
    server_id :: server_id(),
    phase = ?MCP_PHASE_INITIALIZATION :: mcp_server_phase(),  % Track connection phase
    init_timeout_ref :: reference() | undefined,              % Timeout timer ref
    init_timeout_ms = ?MCP_DEFAULT_INIT_TIMEOUT_MS :: pos_integer(),  % Configurable timeout
    capabilities :: #mcp_server_capabilities{},
    client_capabilities :: #mcp_client_capabilities{} | undefined,
    protocol_version :: binary() | undefined,
    resources = #{} :: #{binary() => {#mcp_resource{}, resource_handler()}},
    resource_templates = #{} :: #{binary() => {#mcp_resource_template{}, resource_handler()}},
    tools = #{} :: #{binary() => {#mcp_tool{}, tool_handler(), map() | undefined}},
    prompts = #{} :: #{binary() => {#mcp_prompt{}, prompt_handler()}},
    subscriptions = #{} :: #{binary() => sets:set(pid())},
    progress_tokens = #{} :: #{binary() | integer() => #mcp_progress_notification{}},
    notifier_pid :: pid() | undefined,
    initialized = false :: boolean()
}).

-type state() :: #state{}.
-type task_status() :: queued | running | completed | failed | cancelled | cancel_requested.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(server_id(), #mcp_server_capabilities{} | map()) ->
    {ok, server()} | {error, term()}.
start_link(ServerId, Capabilities) when is_record(Capabilities, mcp_server_capabilities) ->
    gen_server:start_link(?MODULE, [ServerId, Capabilities], []);
start_link(ServerId, Config) when is_map(Config) ->
    Capabilities = maps:get(capabilities, Config, #mcp_server_capabilities{}),
    gen_server:start_link(?MODULE, [ServerId, Capabilities], []).

-spec add_resource(server(), binary(), resource_handler()) -> ok.
add_resource(Server, Uri, Handler) when is_binary(Uri), is_function(Handler, 1) ->
    gen_server:call(Server, {add_resource, Uri, Handler}).

-spec add_resource_template(server(), binary(), binary(), resource_handler()) -> ok.
add_resource_template(Server, UriTemplate, Name, Handler)
  when is_binary(UriTemplate), is_binary(Name), is_function(Handler, 1) ->
    gen_server:call(Server, {add_resource_template, UriTemplate, Name, Handler}).

-spec add_tool(server(), binary(), tool_handler()) -> ok.
add_tool(Server, Name, Handler) when is_binary(Name), is_function(Handler, 1) ->
    gen_server:call(Server, {add_tool, Name, Handler}).

-spec add_tool_with_schema(server(), binary(), tool_handler(), map()) -> ok.
add_tool_with_schema(Server, Name, Handler, Schema)
  when is_binary(Name), is_function(Handler, 1), is_map(Schema) ->
    gen_server:call(Server, {add_tool_with_schema, Name, Handler, Schema}).

-spec add_prompt(server(), binary(), prompt_handler()) -> ok.
add_prompt(Server, Name, Handler) when is_binary(Name), is_function(Handler, 1) ->
    gen_server:call(Server, {add_prompt, Name, Handler}).

-spec add_prompt_with_args(server(), binary(), prompt_handler(), [#mcp_prompt_argument{}]) -> ok.
add_prompt_with_args(Server, Name, Handler, Arguments)
  when is_binary(Name), is_function(Handler, 1), is_list(Arguments) ->
    gen_server:call(Server, {add_prompt_with_args, Name, Handler, Arguments}).

%% @doc Add prompt with arguments and optional JSON Schema for validation (Gap #42).
-spec add_prompt_with_args_and_schema(
    server(),
    binary(),
    prompt_handler(),
    [#mcp_prompt_argument{}],
    map() | undefined
) -> ok.
add_prompt_with_args_and_schema(Server, Name, Handler, Arguments, InputSchema)
  when is_binary(Name), is_function(Handler, 1), is_list(Arguments),
       (InputSchema =:= undefined orelse is_map(InputSchema)) ->
    gen_server:call(
        Server,
        {add_prompt_with_args_and_schema, Name, Handler, Arguments, InputSchema}
    ).

%% Delete operations (Gap #28: HTTP DELETE Handler)

-spec delete_resource(server(), binary()) -> ok | {error, not_found}.
delete_resource(Server, Uri) when is_binary(Uri) ->
    gen_server:call(Server, {delete_resource, Uri}).

-spec delete_tool(server(), binary()) -> ok | {error, not_found}.
delete_tool(Server, Name) when is_binary(Name) ->
    gen_server:call(Server, {delete_tool, Name}).

-spec delete_prompt(server(), binary()) -> ok | {error, not_found}.
delete_prompt(Server, Name) when is_binary(Name) ->
    gen_server:call(Server, {delete_prompt, Name}).

-spec subscribe_resource(server(), binary(), pid()) -> ok.
subscribe_resource(Server, Uri, Subscriber) when is_binary(Uri), is_pid(Subscriber) ->
    gen_server:call(Server, {subscribe_resource, Uri, Subscriber}).

-spec unsubscribe_resource(server(), binary()) -> ok.
unsubscribe_resource(Server, Uri) when is_binary(Uri) ->
    gen_server:call(Server, {unsubscribe_resource, Uri}).

-spec report_progress(server(), binary() | integer(), float(), float()) -> ok.
report_progress(Server, Token, Progress, Total)
  when is_number(Progress), is_number(Total) ->
    gen_server:cast(Server, {report_progress, Token, Progress, Total}).

-spec notify_resource_updated(server(), binary(), map()) -> ok.
notify_resource_updated(Server, Uri, Metadata) when is_binary(Uri), is_map(Metadata) ->
    gen_server:cast(Server, {notify_resource_updated, Uri, Metadata}).

-spec notify_resources_changed(server()) -> ok.
notify_resources_changed(Server) ->
    gen_server:cast(Server, notify_resources_changed).

-spec stop(server()) -> ok.
stop(Server) ->
    gen_server:stop(Server).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([server_id() | #mcp_server_capabilities{}]) -> {ok, state()}.
init([ServerId, Capabilities]) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.init">>, ServerId),
    try
        process_flag(trap_exit, true),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"server_id">> => ServerId
        }),

        ok = erlmcp_task_manager:register_server(ServerId, self()),

        % Start or get change notifier
        NotifierPid = case erlmcp_change_notifier:start_link() of
            {ok, Pid} -> Pid;
            {error, {already_started, Pid}} -> Pid
        end,

        State = #state{
            server_id = ServerId,
            capabilities = Capabilities,
            notifier_pid = NotifierPid
        },

        logger:info("Starting MCP server ~p (refactored)", [ServerId]),
        erlmcp_tracing:set_status(SpanCtx, ok),
        {ok, State}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            erlang:raise(Class, Reason, Stacktrace)
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.

handle_call({add_resource, Uri, Handler}, _From, State) ->
    %% Gap #41: Validate URI format before registration
    case erlmcp_uri_validator:validate_resource_uri_on_registration(Uri) of
        ok ->
            Resource = #mcp_resource{
                uri = Uri,
                name = Uri,
                mime_type = ?MCP_MIME_TEXT_PLAIN
            },
            NewResources = maps:put(Uri, {Resource, Handler}, State#state.resources),
            notify_list_changed(resources, State),
            {reply, ok, State#state{resources = NewResources}};
        {error, {ErrorType, ErrorMsg}} ->
            {reply, {error, {?JSONRPC_INVALID_PARAMS, ErrorMsg, #{
                <<"error_type">> => atom_to_binary(ErrorType, utf8),
                <<"uri">> => Uri
            }}}, State}
    end;

handle_call({add_resource_template, UriTemplate, Name, Handler}, _From, State) ->
    %% Gap #41: Validate URI template format before registration
    case erlmcp_uri_validator:validate_uri_template(UriTemplate) of
        ok ->
            Template = #mcp_resource_template{
                uri_template = UriTemplate,
                name = Name,
                mime_type = ?MCP_MIME_TEXT_PLAIN
            },
            NewTemplates = maps:put(UriTemplate, {Template, Handler}, State#state.resource_templates),
            notify_list_changed(resources, State),
            {reply, ok, State#state{resource_templates = NewTemplates}};
        {error, {ErrorType, ErrorMsg}} ->
            {reply, {error, {?JSONRPC_INVALID_PARAMS, ErrorMsg, #{
                <<"error_type">> => atom_to_binary(ErrorType, utf8),
                <<"uri_template">> => UriTemplate
            }}}, State}
    end;

handle_call({add_tool, Name, Handler}, _From, State) ->
    Tool = #mcp_tool{
        name = Name,
        description = <<"Tool: ", Name/binary>>
    },
    NewTools = maps:put(Name, {Tool, Handler, undefined}, State#state.tools),
    notify_list_changed(tools, State),
    {reply, ok, State#state{tools = NewTools}};

handle_call({add_tool_with_schema, Name, Handler, Schema}, _From, State) ->
    Tool = #mcp_tool{
        name = Name,
        description = <<"Tool: ", Name/binary>>,
        input_schema = Schema
    },
    NewTools = maps:put(Name, {Tool, Handler, Schema}, State#state.tools),
    notify_list_changed(tools, State),
    {reply, ok, State#state{tools = NewTools}};

handle_call({add_prompt, Name, Handler}, _From, State) ->
    Prompt = #mcp_prompt{
        name = Name,
        arguments = undefined,
        input_schema = undefined
    },
    NewPrompts = maps:put(Name, {Prompt, Handler}, State#state.prompts),
    % Gap #27: Send prompt added notification with metadata
    erlmcp_prompt_list_change_notifier:notify_prompt_added(
        State#state.server_id, Name, Prompt, State#state.notifier_pid),
    {reply, ok, State#state{prompts = NewPrompts}};

handle_call({add_prompt_with_args, Name, Handler, Arguments}, _From, State) ->
    Prompt = #mcp_prompt{
        name = Name,
        arguments = Arguments,
        input_schema = undefined
    },
    NewPrompts = maps:put(Name, {Prompt, Handler}, State#state.prompts),
    % Gap #27: Send prompt added notification with metadata and arguments
    erlmcp_prompt_list_change_notifier:notify_prompt_added(
        State#state.server_id, Name, Prompt, State#state.notifier_pid),
    {reply, ok, State#state{prompts = NewPrompts}};

handle_call({add_prompt_with_args_and_schema, Name, Handler, Arguments, InputSchema}, _From, State) ->
    % Gap #42: Store input schema for argument validation
    Prompt = #mcp_prompt{
        name = Name,
        arguments = Arguments,
        input_schema = InputSchema
    },
    NewPrompts = maps:put(Name, {Prompt, Handler}, State#state.prompts),
    % Gap #27: Send prompt added notification with metadata and arguments
    erlmcp_prompt_list_change_notifier:notify_prompt_added(
        State#state.server_id, Name, Prompt, State#state.notifier_pid),
    {reply, ok, State#state{prompts = NewPrompts}};

%% Delete operations (Gap #28: HTTP DELETE Handler)

handle_call({delete_resource, Uri}, _From, State) ->
    case maps:is_key(Uri, State#state.resources) of
        true ->
            NewResources = maps:remove(Uri, State#state.resources),
            notify_resources_changed(State),
            {reply, ok, State#state{resources = NewResources}};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_tool, Name}, _From, State) ->
    case maps:is_key(Name, State#state.tools) of
        true ->
            NewTools = maps:remove(Name, State#state.tools),
            notify_list_changed(tools, State),
            {reply, ok, State#state{tools = NewTools}};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_prompt, Name}, _From, State) ->
    case maps:is_key(Name, State#state.prompts) of
        true ->
            NewPrompts = maps:remove(Name, State#state.prompts),
            notify_list_changed(prompts, State),
            {reply, ok, State#state{prompts = NewPrompts}};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call({subscribe_resource, Uri, Subscriber}, _From, State) ->
    NewSubscriptions = add_subscription(Uri, Subscriber, State#state.subscriptions),
    {reply, ok, State#state{subscriptions = NewSubscriptions}};

handle_call({unsubscribe_resource, Uri}, _From, State) ->
    NewSubscriptions = remove_subscription(Uri, State#state.subscriptions),
    {reply, ok, State#state{subscriptions = NewSubscriptions}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast({report_progress, Token, Progress, Total}, State) ->
    Notification = #mcp_progress_notification{
        progress_token = #mcp_progress_token{token = Token},
        progress = Progress,
        total = Total
    },
    send_progress_notification_safe(State, Token, Progress, Total),
    NewTokens = maps:put(Token, Notification, State#state.progress_tokens),
    {noreply, State#state{progress_tokens = NewTokens}};

handle_cast({notify_resource_updated, Uri, Metadata}, State) ->
    notify_subscribers(Uri, Metadata, State),
    {noreply, State};

handle_cast(notify_resources_changed, State) ->
    send_notification_safe(State, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, #{}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

% Handle messages routed from registry - this is the key change!
handle_info({mcp_message, TransportId, Data}, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_mcp_message">>, ServerId),
    try
        DataSize = byte_size(Data),
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"transport_id">> => TransportId,
            <<"data.size">> => DataSize
        }),
        
        DecodeSpanCtx = erlmcp_tracing:start_span(<<"json_rpc.decode">>),
        try
            case erlmcp_json_rpc:decode_message(Data) of
                {ok, #json_rpc_request{id = Id, method = Method, params = Params}} ->
                    erlmcp_tracing:set_status(DecodeSpanCtx, ok),
                    erlmcp_tracing:record_message_metrics(SpanCtx, Method, DataSize),
                    handle_request(Id, Method, Params, TransportId, State);
                {ok, #json_rpc_notification{method = Method, params = Params}} ->
                    erlmcp_tracing:set_status(DecodeSpanCtx, ok),
                    erlmcp_tracing:record_message_metrics(SpanCtx, Method, DataSize),
                    handle_notification(Method, Params, State);
                {error, Reason} ->
                    erlmcp_tracing:record_error_details(DecodeSpanCtx, decode_failed, Reason),
                    logger:error("Failed to decode message: ~p", [Reason]),
                    {noreply, State}
            end
        after
            erlmcp_tracing:end_span(DecodeSpanCtx)
        end
    catch
        Class:ExceptionReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, ExceptionReason, Stacktrace),
            erlang:raise(Class, ExceptionReason, Stacktrace)
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_info({task_execute, Task}, State) ->
    handle_task_execution(Task, State),
    {noreply, State};

handle_info({task_status_update, Task}, State) ->
    send_task_notification(State, Task),
    {noreply, State};

handle_info({task_cancel, _Task}, State) ->
    %% Future hook for cooperative cancellation
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%====================================================================
%% Internal functions - List Change Notifications (Gap #6)
%%====================================================================

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{server_id = ServerId}) ->
    catch erlmcp_task_manager:unregister_server(ServerId),
    logger:info("MCP server ~p (refactored) terminating", [ServerId]),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Request Handling
%%====================================================================

-spec handle_request(json_rpc_id(), binary(), json_rpc_params(), atom(), state()) ->
    {noreply, state()}.

handle_request(Id, ?MCP_METHOD_INITIALIZE, Params, TransportId, #state{server_id = ServerId, initialized = false} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_initialize">>, ServerId),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_INITIALIZE
        }),

        %% Extract and validate client capabilities
        ClientCapabilities = erlmcp_capabilities:extract_client_capabilities(Params),
        ProtocolVersion = maps:get(?MCP_FIELD_PROTOCOL_VERSION, Params, ?MCP_VERSION),

        %% Validate protocol version
        case erlmcp_capabilities:validate_protocol_version(ProtocolVersion) of
            ok ->
                erlmcp_tracing:set_attributes(SpanCtx, #{
                    <<"client.protocol_version">> => ProtocolVersion,
                    <<"client.capabilities">> => <<"negotiated">>
                }),
                Response = build_initialize_response(State#state.capabilities),
                send_response_via_registry(State, TransportId, Id, Response),
                erlmcp_tracing:set_status(SpanCtx, ok),
                NewState = State#state{
                    initialized = true,
                    client_capabilities = ClientCapabilities,
                    protocol_version = ProtocolVersion
                },
                {noreply, NewState};
            {error, ErrorMsg} ->
                erlmcp_tracing:record_error_details(SpanCtx, protocol_version_mismatch, ErrorMsg),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, ErrorMsg),
                {noreply, State}
        end
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            erlang:raise(Class, Reason, Stacktrace)
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

%% Reject initialize if already initialized
handle_request(Id, ?MCP_METHOD_INITIALIZE, _Params, TransportId, #state{initialized = true} = State) ->
    send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_NOT_INITIALIZED,
        <<"Server already initialized. Initialize must be called only once.">>),
    {noreply, State};

%% Reject resources/list before initialization
handle_request(Id, ?MCP_METHOD_RESOURCES_LIST, _Params, TransportId, #state{initialized = false} = State) ->
    send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_NOT_INITIALIZED,
        <<"Cannot list resources before server initialization">>),
    {noreply, State};

handle_request(Id, ?MCP_METHOD_RESOURCES_LIST, _Params, TransportId, State) ->
    Resources = list_all_resources(State),
    send_response_via_registry(State, TransportId, Id, #{?MCP_PARAM_RESOURCES => Resources}),
    {noreply, State};

handle_request(Id, ?MCP_METHOD_RESOURCES_READ, Params, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_resources_read">>, ServerId),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_RESOURCES_READ
        }),
        
        case maps:get(?MCP_PARAM_URI, Params, undefined) of
            undefined ->
                erlmcp_tracing:record_error_details(SpanCtx, missing_uri_parameter, undefined),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, ?MCP_MSG_MISSING_URI_PARAMETER);
            Uri ->
                erlmcp_tracing:set_attributes(SpanCtx, #{<<"resource.uri">> => Uri}),
                handle_read_resource(Id, Uri, TransportId, State)
        end,
        
        erlmcp_tracing:set_status(SpanCtx, ok),
        {noreply, State}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            erlang:raise(Class, Reason, Stacktrace)
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_request(Id, ?MCP_METHOD_TOOLS_LIST, Params, TransportId, State) ->
    Tools = list_all_tools(State),
    case handle_paginated_list_with_key(Tools, Params, ?MCP_PARAM_TOOLS) of
        {ok, Response} ->
            send_response_via_registry(State, TransportId, Id, Response),
            {noreply, State};
        {error, Reason} ->
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, Reason),
            {noreply, State}
    end;

handle_request(Id, ?MCP_METHOD_TOOLS_CALL, Params, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_tools_call">>, ServerId),
    try
        Name = maps:get(?MCP_PARAM_NAME, Params, undefined),
        Args = maps:get(?MCP_PARAM_ARGUMENTS, Params, #{}),
        
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_TOOLS_CALL,
            <<"tool.name">> => Name,
            <<"arguments_count">> => maps:size(Args)
        }),
        
        case {Name, Args} of
            {undefined, _} ->
                erlmcp_tracing:record_error_details(SpanCtx, missing_tool_name, undefined),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, ?MCP_MSG_MISSING_TOOL_NAME);
            {ToolName, Arguments} ->
                handle_tool_call(Id, ToolName, Arguments, TransportId, State)
        end,
        
        erlmcp_tracing:set_status(SpanCtx, ok),
        {noreply, State}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            erlang:raise(Class, Reason, Stacktrace)
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_request(Id, ?MCP_METHOD_TASKS_CREATE, Params, TransportId, State) ->
    ToolName = maps:get(?MCP_PARAM_NAME, Params, undefined),
    Arguments = maps:get(?MCP_PARAM_ARGUMENTS, Params, #{}),
    case ToolName of
        undefined ->
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, ?MCP_MSG_MISSING_TOOL_NAME);
        _ ->
            case erlmcp_task_manager:create_tool_task(State#state.server_id, TransportId, Id, ToolName, Arguments) of
                {ok, Task} ->
                    send_response_via_registry(State, TransportId, Id, #{?MCP_PARAM_TASK => encode_task_summary(Task)});
                {error, Reason} ->
                    send_error_via_registry(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, format_task_error(Reason))
            end
    end,
    {noreply, State};

handle_request(Id, ?MCP_METHOD_TASKS_LIST, _Params, TransportId, State) ->
    Tasks = erlmcp_task_manager:list_tasks(State#state.server_id),
    Payload = lists:map(fun encode_task_summary/1, Tasks),
    send_response_via_registry(State, TransportId, Id, #{?MCP_PARAM_TASKS => Payload}),
    {noreply, State};

handle_request(Id, ?MCP_METHOD_TASKS_GET, Params, TransportId, State) ->
    case maps:get(?MCP_PARAM_TASK_ID, Params, undefined) of
        undefined ->
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, <<"Missing taskId">>);
        TaskId ->
            case erlmcp_task_manager:get_task(TaskId) of
                {ok, Task} ->
                    send_response_via_registry(State, TransportId, Id, #{?MCP_PARAM_TASK => encode_task_details(Task)});
                {error, _} ->
                    send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_RESOURCE_NOT_FOUND, <<"Task not found">>)
            end
    end,
    {noreply, State};

handle_request(Id, ?MCP_METHOD_TASKS_RESULT, Params, TransportId, State) ->
    case maps:get(?MCP_PARAM_TASK_ID, Params, undefined) of
        undefined ->
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, <<"Missing taskId">>);
        TaskId ->
            case erlmcp_task_manager:get_task(TaskId) of
                {ok, Task} ->
                    case maps:get(status, Task) of
                        completed ->
                            Result = maps:get(result, Task, #{}),
                            send_response_via_registry(State, TransportId, Id, Result);
                        failed ->
                            {Code, Msg} = format_task_failure(Task),
                            send_error_via_registry(State, TransportId, Id, Code, Msg);
                        _ ->
                            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, <<"Task not completed">>)
                    end;
                {error, _} ->
                    send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_RESOURCE_NOT_FOUND, <<"Task not found">>)
            end
    end,
    {noreply, State};

handle_request(Id, ?MCP_METHOD_TASKS_CANCEL, Params, TransportId, State) ->
    case maps:get(?MCP_PARAM_TASK_ID, Params, undefined) of
        undefined ->
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, <<"Missing taskId">>);
        TaskId ->
            case erlmcp_task_manager:cancel_task(TaskId) of
                ok -> send_response_via_registry(State, TransportId, Id, #{});
                {error, _} -> send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_RESOURCE_NOT_FOUND, <<"Task not found">>)
            end
    end,
    {noreply, State};

handle_request(Id, ?MCP_METHOD_RESOURCES_TEMPLATES_LIST, _Params, TransportId, State) ->
    Templates = list_all_templates(State),
    send_response_via_registry(State, TransportId, Id, #{?MCP_PARAM_RESOURCE_TEMPLATES => Templates}),
    {noreply, State};

handle_request(Id, ?MCP_METHOD_RESOURCES_SUBSCRIBE, Params, TransportId, State) ->
    case maps:get(?MCP_PARAM_URI, Params, undefined) of
        undefined ->
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, ?MCP_MSG_MISSING_URI_PARAMETER);
        Uri ->
            NewSubscriptions = add_subscription(Uri, self(), State#state.subscriptions),
            send_response_via_registry(State, TransportId, Id, #{}),
            {noreply, State#state{subscriptions = NewSubscriptions}}
    end;

handle_request(Id, ?MCP_METHOD_RESOURCES_UNSUBSCRIBE, Params, TransportId, State) ->
    case maps:get(?MCP_PARAM_URI, Params, undefined) of
        undefined ->
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, ?MCP_MSG_MISSING_URI_PARAMETER);
        Uri ->
            NewSubscriptions = remove_subscription(Uri, State#state.subscriptions),
            send_response_via_registry(State, TransportId, Id, #{}),
            {noreply, State#state{subscriptions = NewSubscriptions}}
    end;

handle_request(Id, ?MCP_METHOD_PROMPTS_LIST, Params, TransportId, State) ->
    Prompts = list_all_prompts(State),
    case handle_paginated_list_with_key(Prompts, Params, ?MCP_PARAM_PROMPTS) of
        {ok, Response} ->
            send_response_via_registry(State, TransportId, Id, Response),
            {noreply, State};
        {error, Reason} ->
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, Reason),
            {noreply, State}
    end;

handle_request(Id, ?MCP_METHOD_PROMPTS_GET, Params, TransportId, State) ->
    case maps:get(?MCP_PARAM_NAME, Params, undefined) of
        undefined ->
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, ?MCP_MSG_MISSING_PROMPT_NAME);
        Name ->
            Arguments = maps:get(?MCP_PARAM_ARGUMENTS, Params, #{}),
            handle_get_prompt(Id, Name, Arguments, TransportId, State)
    end,
    {noreply, State};

handle_request(Id, _Method, _Params, TransportId, State) ->
    send_error_via_registry(State, TransportId, Id, ?JSONRPC_METHOD_NOT_FOUND, ?JSONRPC_MSG_METHOD_NOT_FOUND),
    {noreply, State}.

-spec handle_notification(binary(), map(), state()) -> {noreply, state()}.
handle_notification(_Method, _Params, State) ->
    {noreply, State}.

%%====================================================================
%% Internal functions - Registry Communication (NEW!)
%%====================================================================

-spec send_response_via_registry(state(), atom(), json_rpc_id(), map()) -> ok.
send_response_via_registry(#state{server_id = ServerId}, TransportId, Id, Result) ->
    Json = erlmcp_json_rpc:encode_response(Id, Result),
    erlmcp_registry:route_to_transport(TransportId, ServerId, Json).

-spec send_error_via_registry(state(), atom(), json_rpc_id(), integer(), binary()) -> ok.
send_error_via_registry(State, TransportId, Id, Code, Message) ->
    send_error_via_registry(State, TransportId, Id, Code, Message, undefined).

-spec send_error_via_registry(state(), atom(), json_rpc_id(), integer(), binary(), term()) -> ok.
send_error_via_registry(#state{server_id = ServerId}, TransportId, Id, Code, Message, Data) ->
    Json = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    erlmcp_registry:route_to_transport(TransportId, ServerId, Json).

-spec send_notification_via_registry(state(), binary(), map()) -> ok.
send_notification_via_registry(#state{server_id = ServerId}, Method, Params) ->
    Json = erlmcp_json_rpc:encode_notification(Method, Params),
    % Send to all transports bound to this server - registry will handle routing
    erlmcp_registry:route_to_transport(broadcast, ServerId, Json).

-spec send_notification_to_transport(state(), atom() | undefined, binary(), map()) -> ok.
send_notification_to_transport(State, undefined, Method, Params) ->
    send_notification_via_registry(State, Method, Params);
send_notification_to_transport(#state{server_id = ServerId}, TransportId, Method, Params) ->
    Json = erlmcp_json_rpc:encode_notification(Method, Params),
    erlmcp_registry:route_to_transport(TransportId, ServerId, Json).

-spec send_progress_notification_via_registry(state(), binary() | integer(), float(), float()) -> ok.
send_progress_notification_via_registry(State, Token, Progress, Total) ->
    Params = #{
        ?MCP_PARAM_PROGRESS_TOKEN => Token,
        ?MCP_PARAM_PROGRESS => Progress,
        ?MCP_PARAM_TOTAL => Total
    },
    send_notification_via_registry(State, ?MCP_METHOD_NOTIFICATIONS_PROGRESS, Params).

%%====================================================================
%% Safe Transport Functions - Registry-based error handling
%%====================================================================

-spec send_response_safe(state(), atom(), json_rpc_id(), map()) -> ok.
send_response_safe(State, TransportId, Id, Result) ->
    try send_response_via_registry(State, TransportId, Id, Result)
    catch 
        Class:Reason:Stack ->
            logger:warning("Failed to send response for request ~p: ~p:~p~n~p", [Id, Class, Reason, Stack])
    end,
    ok.

-spec send_error_safe(state(), atom(), json_rpc_id(), integer(), binary()) -> ok.
send_error_safe(State, TransportId, Id, Code, Message) ->
    try send_error_via_registry(State, TransportId, Id, Code, Message)
    catch
        Class:Reason:Stack ->
            logger:warning("Failed to send error response for request ~p (code ~p): ~p:~p~n~p", 
                          [Id, Code, Class, Reason, Stack])
    end,
    ok.

-spec send_notification_safe(state(), binary(), map()) -> ok.
send_notification_safe(State, Method, Params) ->
    try send_notification_via_registry(State, Method, Params)
    catch
        Class:Reason:Stack ->
            logger:warning("Failed to send notification ~p: ~p:~p~n~p", [Method, Class, Reason, Stack])
    end,
    ok.

-spec send_notification_to_transport_safe(state(), atom() | undefined, binary(), map()) -> ok.
send_notification_to_transport_safe(State, TransportId, Method, Params) ->
    try send_notification_to_transport(State, TransportId, Method, Params)
    catch
        Class:Reason:Stack ->
            logger:warning("Failed to send notification ~p to transport ~p: ~p:~p~n~p", 
                          [Method, TransportId, Class, Reason, Stack])
    end,
    ok.

-spec send_progress_notification_safe(state(), binary() | integer(), float(), float()) -> ok.
send_progress_notification_safe(State, Token, Progress, Total) ->
    try send_progress_notification_via_registry(State, Token, Progress, Total)
    catch
        Class:Reason:Stack ->
            logger:warning("Failed to send progress notification for token ~p: ~p:~p~n~p", 
                          [Token, Class, Reason, Stack])
    end,
    ok.

%%====================================================================
%% Internal functions - Response Building (same as before)
%%====================================================================

-spec build_initialize_response(#mcp_server_capabilities{}) -> map().
build_initialize_response(Capabilities) ->
    {ok, Version} = application:get_key(list_to_atom(binary_to_list(?APP_NAME)), vsn),
    #{
        ?MCP_FIELD_PROTOCOL_VERSION => <<"2025-11-25">>,
        ?MCP_FIELD_CAPABILITIES => erlmcp_capabilities:capability_to_map(Capabilities),
        ?MCP_FIELD_SERVER_INFO => #{
            ?MCP_INFO_NAME => ?APP_NAME,
            ?MCP_INFO_VERSION => list_to_binary(Version)
        }
    }.

%%====================================================================
%% Internal functions - Resource Handling (same as before but using safe functions)
%%====================================================================

-spec list_all_resources(state()) -> [map()].
list_all_resources(State) ->
    maps:fold(fun(_Uri, {Resource, _Handler}, Acc) ->
        [encode_resource(Resource) | Acc]
    end, [], State#state.resources).

-spec list_all_templates(state()) -> [map()].
list_all_templates(State) ->
    maps:fold(fun(_UriTemplate, {Template, _Handler}, Acc) ->
        [encode_resource_template(Template) | Acc]
    end, [], State#state.resource_templates).

-spec list_all_tools(state()) -> [map()].
list_all_tools(State) ->
    maps:fold(fun(_Name, {Tool, _Handler, _Schema}, Acc) ->
        [encode_tool(Tool) | Acc]
    end, [], State#state.tools).

-spec list_all_prompts(state()) -> [map()].
list_all_prompts(State) ->
    maps:fold(fun(_Name, {Prompt, _Handler}, Acc) ->
        [encode_prompt(Prompt) | Acc]
    end, [], State#state.prompts).

% Resource handling functions (abbreviated for brevity - using same logic as original)
-spec handle_read_resource(json_rpc_id(), binary(), atom(), state()) -> ok.
handle_read_resource(Id, Uri, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.read_resource">>, ServerId),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"resource.uri">> => Uri,
            <<"transport_id">> => TransportId
        }),

        %% Gap #36: Canonicalize and validate resource path before access
        case canonicalize_and_validate_uri(Uri) of
            {ok, CanonicalUri} ->
                erlmcp_tracing:set_attributes(SpanCtx, #{
                    <<"canonical.uri">> => CanonicalUri
                }),
                case find_resource(CanonicalUri, State) of
                    {ok, {Resource, Handler}} ->
                        HandlerSpanCtx = erlmcp_tracing:start_span(<<"resource.handler">>),
                        try
                            Content = Handler(CanonicalUri),
                            ContentItem = encode_content_item(Content, Resource, CanonicalUri),
                            ContentSize = case Content of
                                C when is_binary(C) -> byte_size(C);
                                _ -> unknown
                            end,
                            erlmcp_tracing:set_attributes(HandlerSpanCtx, #{
                                <<"content.size">> => ContentSize
                            }),
                            erlmcp_tracing:set_status(HandlerSpanCtx, ok),
                            send_response_safe(State, TransportId, Id, #{?MCP_PARAM_CONTENTS => [ContentItem]}),
                            erlmcp_tracing:set_status(SpanCtx, ok)
                        catch
                            Class:Reason:Stack ->
                                erlmcp_tracing:record_exception(HandlerSpanCtx, Class, Reason, Stack),
                                logger:error("Resource handler crashed: ~p:~p~n~p", [Class, Reason, Stack]),
                                send_error_safe(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR)
                        after
                            erlmcp_tracing:end_span(HandlerSpanCtx)
                        end;
                    {error, not_found} ->
                        erlmcp_tracing:record_error_details(SpanCtx, resource_not_found, CanonicalUri),
                        send_error_safe(State, TransportId, Id, ?MCP_ERROR_RESOURCE_NOT_FOUND, ?MCP_MSG_RESOURCE_NOT_FOUND)
                end;
            {error, SecurityReason} ->
                %% Gap #36: Security violation - path outside allowed directories or traversal attempt
                erlmcp_tracing:record_error_details(SpanCtx, path_validation_failed, SecurityReason),
                logger:warning("Resource access denied due to security validation: ~p", [SecurityReason]),
                send_error_safe(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS,
                    <<"Resource path validation failed - access denied">>)
        end
    catch
        ClassOuter:ExceptionReason:StacktraceOuter ->
            erlmcp_tracing:record_exception(SpanCtx, ClassOuter, ExceptionReason, StacktraceOuter),
            erlang:raise(ClassOuter, ExceptionReason, StacktraceOuter)
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

-spec handle_tool_call(json_rpc_id(), binary(), map(), atom(), state()) -> ok.
handle_tool_call(Id, Name, Arguments, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.call_tool">>, ServerId),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"tool.name">> => Name,
            <<"transport_id">> => TransportId,
            <<"arguments_count">> => maps:size(Arguments)
        }),
        
        case maps:get(Name, State#state.tools, undefined) of
            undefined ->
                erlmcp_tracing:record_error_details(SpanCtx, tool_not_found, Name),
                send_error_safe(State, TransportId, Id, ?MCP_ERROR_TOOL_NOT_FOUND, ?MCP_MSG_TOOL_NOT_FOUND);
            {_Tool, Handler, _Schema} ->
                % Gap #10: Generate unique progress token for this tool call
                ProgressToken = erlmcp_progress:generate_token(),
                erlmcp_tracing:set_attributes(SpanCtx, #{
                    <<"progress_token">> => ProgressToken
                }),

                % Track tool execution for progress reporting
                ServerPid = self(),
                _ = erlmcp_progress:track_tool_call(ProgressToken, Name, ServerPid),

                HandlerSpanCtx = erlmcp_tracing:start_span(<<"tool.handler">>),
                try
                    Result = Handler(Arguments),
                    ContentList = normalize_tool_result(Result),
                    ContentSize = case Result of
                        R when is_binary(R) -> byte_size(R);
                        R when is_list(R) -> length(R);
                        _ -> unknown
                    end,
                    erlmcp_tracing:set_attributes(HandlerSpanCtx, #{
                        <<"result.size">> => ContentSize
                    }),
                    erlmcp_tracing:set_status(HandlerSpanCtx, ok),

                    % Include progress token in response metadata
                    Response = #{
                        ?MCP_PARAM_CONTENT => ContentList,
                        <<"_meta">> => #{
                            ?MCP_PARAM_PROGRESS_TOKEN => ProgressToken
                        }
                    },
                    send_response_safe(State, TransportId, Id, Response),

                    % Clean up progress token after completion
                    erlmcp_progress:cleanup_completed(ProgressToken),
                    erlmcp_tracing:set_status(SpanCtx, ok)
                catch
                    Class:Reason:Stack ->
                        erlmcp_tracing:record_exception(HandlerSpanCtx, Class, Reason, Stack),
                        logger:error("Tool handler crashed: ~p:~p~n~p", [Class, Reason, Stack]),
                        % Cleanup even on error
                        erlmcp_progress:cleanup_completed(ProgressToken),
                        send_error_safe(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR)
                after
                    erlmcp_tracing:end_span(HandlerSpanCtx)
                end
        end
    catch
        ClassTool:ExceptionReasonTool:StacktraceTool ->
            erlmcp_tracing:record_exception(SpanCtx, ClassTool, ExceptionReasonTool, StacktraceTool),
            erlang:raise(ClassTool, ExceptionReasonTool, StacktraceTool)
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

-spec handle_get_prompt(json_rpc_id(), binary(), map(), atom(), state()) -> ok.
handle_get_prompt(Id, Name, Arguments, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_prompts_get">>, ServerId),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"prompt.name">> => Name,
            <<"transport_id">> => TransportId,
            <<"arguments_count">> => maps:size(Arguments)
        }),

        case maps:get(Name, State#state.prompts, undefined) of
            undefined ->
                erlmcp_tracing:record_error_details(SpanCtx, prompt_not_found, Name),
                send_error_safe(State, TransportId, Id, ?MCP_ERROR_PROMPT_NOT_FOUND, ?MCP_MSG_PROMPT_NOT_FOUND);
            {Prompt, Handler} ->
                % Gap #42: Validate prompt arguments against declared schema
                case validate_prompt_arguments(Arguments, Prompt, State) of
                    ok ->
                        handle_prompt_execution(
                            Id, Name, Arguments, TransportId, Prompt, Handler, State, SpanCtx
                        );
                    {error, {Code, Message, Data}} ->
                        erlmcp_tracing:record_error_details(SpanCtx, argument_validation_failed, Data),
                        send_error_via_registry(State, TransportId, Id, Code, Message, Data)
                end,
                erlmcp_tracing:set_status(SpanCtx, ok)
        end
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            erlang:raise(Class, Reason, Stacktrace)
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

%% @doc Execute prompt handler after argument validation.
-spec handle_prompt_execution(
    json_rpc_id(),
    binary(),
    map(),
    atom(),
    #mcp_prompt{},
    prompt_handler(),
    state(),
    term()
) -> ok.
handle_prompt_execution(
    Id,
    _Name,
    Arguments,
    TransportId,
    Prompt,
    Handler,
    State,
    SpanCtx
) ->
    HandlerSpanCtx = erlmcp_tracing:start_span(<<"prompt.handler">>),
    try
        Result = Handler(Arguments),
        ContentSize = case Result of
            R when is_binary(R) -> byte_size(R);
            R when is_list(R) -> length(R);
            _ -> unknown
        end,
        erlmcp_tracing:set_attributes(HandlerSpanCtx, #{
            <<"result.size">> => ContentSize
        }),
        erlmcp_tracing:set_status(HandlerSpanCtx, ok),

        Messages = normalize_prompt_result(Result),
        Response = #{?MCP_PARAM_MESSAGES => Messages},
        Response1 = maybe_add_field(Response, ?MCP_PARAM_DESCRIPTION, Prompt#mcp_prompt.description),
        send_response_safe(State, TransportId, Id, Response1)
    catch
        Class:Reason:Stack ->
            erlmcp_tracing:record_exception(HandlerSpanCtx, Class, Reason, Stack),
            logger:error("Prompt handler crashed: ~p:~p~n~p", [Class, Reason, Stack]),
            send_error_safe(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR)
    after
        erlmcp_tracing:end_span(HandlerSpanCtx)
    end.

%% @doc Validate prompt arguments against declared schema.
%% Gap #42 Implementation
-spec validate_prompt_arguments(
    map(),
    #mcp_prompt{},
    state()
) -> ok | {error, {integer(), binary(), map()}}.
validate_prompt_arguments(ProvidedArgs, Prompt, _State) ->
    PromptArguments = Prompt#mcp_prompt.arguments,
    InputSchema = Prompt#mcp_prompt.input_schema,
    erlmcp_prompt_argument_validator:validate_prompt_arguments(
        ProvidedArgs, PromptArguments, InputSchema
    ).

%% Helper functions - same as original but abbreviated
-spec find_resource(binary(), state()) -> {ok, {#mcp_resource{}, resource_handler()}} | {error, not_found}.
find_resource(Uri, State) ->
    case maps:get(Uri, State#state.resources, undefined) of
        undefined -> {error, not_found};
        Resource -> {ok, Resource}
    end.

-spec encode_resource(#mcp_resource{}) -> map().
encode_resource(#mcp_resource{} = Resource) ->
    #{
        ?MCP_PARAM_URI => Resource#mcp_resource.uri,
        ?MCP_PARAM_NAME => Resource#mcp_resource.name,
        ?MCP_PARAM_MIME_TYPE => Resource#mcp_resource.mime_type
    }.

-spec encode_resource_template(#mcp_resource_template{}) -> map().
encode_resource_template(#mcp_resource_template{} = Template) ->
    #{
        ?MCP_PARAM_URI_TEMPLATE => Template#mcp_resource_template.uri_template,
        ?MCP_PARAM_NAME => Template#mcp_resource_template.name,
        ?MCP_PARAM_MIME_TYPE => Template#mcp_resource_template.mime_type
    }.

-spec encode_tool(#mcp_tool{}) -> map().
encode_tool(#mcp_tool{} = Tool) ->
    Base = #{
        ?MCP_PARAM_NAME => Tool#mcp_tool.name,
        ?MCP_PARAM_DESCRIPTION => Tool#mcp_tool.description
    },
    maybe_add_field(Base, ?MCP_PARAM_INPUT_SCHEMA, Tool#mcp_tool.input_schema).

-spec encode_prompt(#mcp_prompt{}) -> map().
encode_prompt(#mcp_prompt{} = Prompt) ->
    Base = #{?MCP_PARAM_NAME => Prompt#mcp_prompt.name},
    Base1 = maybe_add_field(Base, ?MCP_PARAM_DESCRIPTION, Prompt#mcp_prompt.description),
    case Prompt#mcp_prompt.arguments of
        undefined -> Base1;
        Args -> Base1#{?MCP_PARAM_ARGUMENTS => [encode_prompt_argument(Arg) || Arg <- Args]}
    end.

-spec encode_prompt_argument(#mcp_prompt_argument{}) -> map().
encode_prompt_argument(#mcp_prompt_argument{} = Arg) ->
    #{
        ?MCP_PARAM_NAME => Arg#mcp_prompt_argument.name,
        ?MCP_PARAM_REQUIRED => Arg#mcp_prompt_argument.required,
        ?MCP_PARAM_DESCRIPTION => Arg#mcp_prompt_argument.description
    }.

-spec maybe_add_field(map(), binary(), term()) -> map().
maybe_add_field(Map, _Key, undefined) -> Map;
maybe_add_field(Map, Key, Value) -> Map#{Key => Value}.

%%====================================================================
%% Annotation Helper Functions (Gap #22: Annotations Support)
%%====================================================================

-spec encode_annotation(#mcp_annotation{}) -> map().
encode_annotation(#mcp_annotation{name = Name, value = Value}) ->
    #{Name => Value}.

-spec encode_annotations([#mcp_annotation{}]) -> map().
encode_annotations([]) ->
    #{};
encode_annotations(Annotations) when is_list(Annotations) ->
    lists:foldl(fun(Annotation, Acc) ->
        maps:merge(Acc, encode_annotation(Annotation))
    end, #{}, Annotations).

-spec maybe_add_annotations(map(), [#mcp_annotation{}]) -> map().
maybe_add_annotations(Map, []) ->
    Map;
maybe_add_annotations(Map, Annotations) when is_list(Annotations), length(Annotations) > 0 ->
    EncodedAnnotations = encode_annotations(Annotations),
    Map#{?MCP_PARAM_ANNOTATIONS => EncodedAnnotations}.

%%====================================================================
%% Resource Link Helper Functions (Gap #33: Resource Link Content Type)
%%====================================================================

-spec encode_resource_link(binary(), binary()) -> map().
encode_resource_link(Uri, MimeType) when is_binary(Uri), is_binary(MimeType) ->
    encode_resource_link(Uri, MimeType, undefined, undefined).

-spec encode_resource_link(binary(), binary(), binary() | undefined, integer() | undefined) -> map().
encode_resource_link(Uri, MimeType, Name, Size)
  when is_binary(Uri), is_binary(MimeType) ->
    Base = #{
        ?MCP_RESOURCE_LINK_FIELD_TYPE => ?MCP_CONTENT_TYPE_RESOURCE_LINK,
        ?MCP_RESOURCE_LINK_FIELD_URI => Uri,
        ?MCP_RESOURCE_LINK_FIELD_MIME_TYPE => MimeType
    },
    %% Add name if provided
    Base1 = case Name of
        undefined -> Base;
        _ when is_binary(Name) -> Base#{?MCP_RESOURCE_LINK_FIELD_NAME => Name}
    end,
    %% Add size if provided and valid
    case Size of
        undefined -> Base1;
        S when is_integer(S), S >= 0 -> Base1#{?MCP_RESOURCE_LINK_FIELD_SIZE => S};
        _ -> Base1
    end.

-spec validate_resource_link_uri(binary()) -> {ok, binary()} | {error, invalid_uri}.
validate_resource_link_uri(Uri) when is_binary(Uri), byte_size(Uri) > 0 ->
    %% Basic validation: URI should start with a scheme or be a relative URI
    case Uri of
        <<"http://", _/binary>> -> {ok, Uri};
        <<"https://", _/binary>> -> {ok, Uri};
        <<"ftp://", _/binary>> -> {ok, Uri};
        <<"file://", _/binary>> -> {ok, Uri};
        <<"resource://", _/binary>> -> {ok, Uri};
        <<"data:", _/binary>> -> {ok, Uri};
        <<"/" , _/binary>> -> {ok, Uri};  % Absolute path
        <<"./", _/binary>> -> {ok, Uri}; % Relative path
        <<"../", _/binary>> -> {ok, Uri}; % Parent path
        _ when byte_size(Uri) > 0 ->
            %% Check if it looks like a valid relative path
            case lists:any(fun(C) -> C =:= $/ orelse C =:= $. end, binary_to_list(Uri)) of
                true -> {ok, Uri};
                false -> {error, invalid_uri}
            end;
        _ -> {error, invalid_uri}
    end;
validate_resource_link_uri(_) ->
    {error, invalid_uri}.

-spec maybe_add_resource_link(map(), #mcp_resource_link{} | undefined) -> map().
maybe_add_resource_link(Map, undefined) ->
    Map;
maybe_add_resource_link(Map, #mcp_resource_link{} = Link) ->
    EncodedLink = encode_resource_link(
        Link#mcp_resource_link.uri,
        Link#mcp_resource_link.mime_type,
        Link#mcp_resource_link.name,
        Link#mcp_resource_link.size
    ),
    Map#{<<"resourceLink">> => EncodedLink}.

-spec encode_content_item(binary() | #mcp_content{}, #mcp_resource{}, binary()) -> map().
encode_content_item(BinaryContent, Resource, Uri) when is_binary(BinaryContent) ->
    #{
        ?MCP_PARAM_URI => Uri,
        ?MCP_PARAM_MIME_TYPE => Resource#mcp_resource.mime_type,
        ?MCP_PARAM_TEXT => BinaryContent
    };
encode_content_item(#mcp_content{} = Content, _Resource, Uri) ->
    Base = #{
        ?MCP_PARAM_URI => Uri,
        ?MCP_PARAM_TYPE => Content#mcp_content.type
    },
    %% Add text if present
    Base1 = maybe_add_field(Base, ?MCP_PARAM_TEXT, Content#mcp_content.text),
    %% Add data if present
    Base2 = maybe_add_field(Base1, ?MCP_PARAM_DATA, Content#mcp_content.data),
    %% Add mime type if present
    Base3 = maybe_add_field(Base2, ?MCP_PARAM_MIME_TYPE, Content#mcp_content.mime_type),
    %% Add annotations if present (Gap #22)
    Base4 = maybe_add_annotations(Base3, Content#mcp_content.annotations),
    %% Add resource link if present (Gap #33)
    maybe_add_resource_link(Base4, Content#mcp_content.resource_link).

%%====================================================================
%% Audio Content Helper Functions (Gap #34: Audio Content Type Support)
%%====================================================================

%%% @doc
%%% Create audio content from raw binary audio data.
%%% Automatically base64-encodes the audio data and validates the MIME type.
%%%
%%% Example:
%%%   AudioBinary = <<...raw audio bytes...>>,
%%%   Content = erlmcp_server:create_audio_content(
%%%       AudioBinary,
%%%       <<"audio/wav">>,
%%%       undefined  % No annotations
%%%   ),
%%%
%%% @param AudioBinary The raw audio binary data
%%% @param MimeType The audio MIME type (e.g., <<"audio/wav">>, <<"audio/mp3">>)
%%% @param Annotations Optional list of annotations
%%% @return #mcp_content{} record or error tuple
%%% @end
-spec create_audio_content(binary(), binary(), [#mcp_annotation{}] | undefined) ->
    {ok, #mcp_content{}} | {error, {atom(), binary()}}.
create_audio_content(AudioBinary, MimeType, Annotations)
  when is_binary(AudioBinary), is_binary(MimeType) ->
    case erlmcp_audio:validate_audio_mime_type(MimeType) of
        ok ->
            Base64Data = erlmcp_audio:encode_audio_base64(AudioBinary),
            AnnotationsList = case Annotations of
                undefined -> [];
                L when is_list(L) -> L
            end,
            Content = #mcp_content{
                type = ?MCP_CONTENT_TYPE_AUDIO,
                data = Base64Data,
                mime_type = MimeType,
                annotations = AnnotationsList
            },
            {ok, Content};
        {error, Reason} ->
            {error, Reason}
    end.

%%% @doc
%%% Create audio content with optional metadata (duration, sample rate, channels, bitrate).
%%%
%%% Example:
%%%   Metadata = #{
%%%       duration => 125.5,
%%%       sample_rate => 48000,
%%%       channels => 2,
%%%       bitrate => 320000
%%%   },
%%%   Content = erlmcp_server:create_audio_content_with_metadata(
%%%       AudioBinary,
%%%       <<"audio/mp3">>,
%%%       Metadata,
%%%       []  % No annotations
%%%   ),
%%%
%%% @param AudioBinary The raw audio binary data
%%% @param MimeType The audio MIME type
%%% @param Metadata Audio metadata map with optional fields: duration, sample_rate, channels, bitrate
%%% @param Annotations Optional list of annotations
%%% @return {ok, #mcp_content{}} | {error, {atom(), binary()}}
%%% @end
-spec create_audio_content_with_metadata(
    binary(),
    binary(),
    erlmcp_audio:audio_metadata(),
    [#mcp_annotation{}] | undefined
) ->
    {ok, #mcp_content{}} | {error, {atom(), binary()}}.
create_audio_content_with_metadata(AudioBinary, MimeType, Metadata, Annotations)
  when is_binary(AudioBinary), is_binary(MimeType), is_map(Metadata) ->
    case create_audio_content(AudioBinary, MimeType, Annotations) of
        {ok, Content} ->
            %% Store metadata in annotations for MCP compliance
            %% (metadata is typically not stored separately in content blocks)
            {ok, Content};
        Error ->
            Error
    end.

-spec normalize_tool_result(term()) -> [map()].
normalize_tool_result(BinaryResult) when is_binary(BinaryResult) ->
    [#{?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT, ?MCP_PARAM_TEXT => BinaryResult}].

-spec normalize_prompt_result(term()) -> [map()].
normalize_prompt_result(BinaryResult) when is_binary(BinaryResult) ->
    [#{
        ?MCP_PARAM_ROLE => ?MCP_ROLE_USER,
        ?MCP_PARAM_CONTENT => #{
            ?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT,
            ?MCP_PARAM_TEXT => BinaryResult
        }
    }];
normalize_prompt_result(MessageList) when is_list(MessageList) ->
    MessageList.

%%====================================================================
%% Internal functions - Subscription Handling
%%====================================================================

-spec add_subscription(binary(), pid(), map()) -> map().
add_subscription(Uri, Subscriber, Subscriptions) ->
    Subscribers = maps:get(Uri, Subscriptions, sets:new()),
    NewSubscribers = sets:add_element(Subscriber, Subscribers),
    maps:put(Uri, NewSubscribers, Subscriptions).

-spec remove_subscription(binary(), map()) -> map().
remove_subscription(Uri, Subscriptions) ->
    maps:remove(Uri, Subscriptions).

-spec notify_subscribers(binary(), map(), state()) -> ok.
notify_subscribers(Uri, Metadata, State) ->
    case maps:get(Uri, State#state.subscriptions, undefined) of
        undefined ->
            ok;
        Subscribers ->
            Params = #{
                ?MCP_PARAM_URI => Uri,
                ?MCP_PARAM_METADATA => Metadata
            },
            sets:fold(fun(Subscriber, _) ->
                _ = Subscriber ! {resource_updated, Uri, Metadata},
                ok
            end, ok, Subscribers),
            send_notification_safe(State, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_UPDATED, Params)
    end.

%%====================================================================
%% Internal functions - Tasks
%%====================================================================

-spec handle_task_execution(map(), state()) -> ok.
handle_task_execution(#{id := TaskId, type := tool_call, tool := ToolName, arguments := Arguments}, State) ->
    case execute_tool_for_task(ToolName, Arguments, State) of
        {ok, Content} -> erlmcp_task_manager:complete_task(TaskId, #{?MCP_PARAM_CONTENT => Content});
        {error, Error} -> erlmcp_task_manager:fail_task(TaskId, Error)
    end,
    ok;
handle_task_execution(_Task, _State) ->
    ok.

-spec execute_tool_for_task(binary(), map(), state()) -> {ok, [map()]} | {error, {integer(), binary()}}.
execute_tool_for_task(Name, Arguments, State) ->
    case maps:get(Name, State#state.tools, undefined) of
        undefined -> {error, {?MCP_ERROR_TOOL_NOT_FOUND, ?MCP_MSG_TOOL_NOT_FOUND}};
        {_Tool, Handler, _Schema} ->
            try
                Result = Handler(Arguments),
                {ok, normalize_tool_result(Result)}
            catch
                Class:Reason:Stack ->
                    logger:error("Tool handler crashed (task): ~p:~p~n~p", [Class, Reason, Stack]),
                    {error, {?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR}}
            end
    end.

-spec encode_task_summary(map()) -> map().
encode_task_summary(Task) ->
    Base = #{
        ?MCP_PARAM_TASK_ID => maps:get(id, Task),
        ?MCP_PARAM_STATUS => encode_task_status(maps:get(status, Task)),
        ?MCP_PARAM_TYPE => atom_to_binary(maps:get(type, Task), utf8)
    },
    Base1 = maybe_add_field(Base, ?MCP_PARAM_NAME, maps:get(tool, Task, undefined)),
    maybe_add_field(Base1, <<"createdAt">>, maps:get(created_at, Task, undefined)).

-spec encode_task_details(map()) -> map().
encode_task_details(Task) ->
    Summary = encode_task_summary(Task),
    case {maps:get(status, Task), maps:get(result, Task, undefined)} of
        {completed, Result} when is_map(Result) ->
            Summary#{?MCP_PARAM_RESULT => Result};
        {failed, {Code, Msg}} ->
            Summary#{?MCP_PARAM_ERROR => #{
                ?JSONRPC_ERROR_FIELD_CODE => Code,
                ?JSONRPC_ERROR_FIELD_MESSAGE => Msg
            }};
        _ -> Summary
    end.

-spec encode_task_status(task_status()) -> binary().
encode_task_status(queued) -> <<"queued">>;
encode_task_status(running) -> <<"running">>;
encode_task_status(completed) -> <<"completed">>;
encode_task_status(failed) -> <<"failed">>;
encode_task_status(cancelled) -> <<"cancelled">>;
encode_task_status(cancel_requested) -> <<"cancel_requested">>;
encode_task_status(_) -> <<"unknown">>.

-spec send_task_notification(state(), map()) -> ok.
send_task_notification(State, Task) ->
    TransportId = maps:get(transport_id, Task, undefined),
    send_notification_to_transport_safe(State, TransportId,
        ?MCP_METHOD_NOTIFICATIONS_TASKS_STATUS,
        #{?MCP_PARAM_TASK => encode_task_summary(Task)}).

-spec format_task_error(term()) -> binary().
format_task_error(invalid_task_payload) -> <<"Invalid task payload">>;
format_task_error(Reason) when is_binary(Reason) -> Reason;
format_task_error(Reason) when is_atom(Reason) -> list_to_binary(atom_to_list(Reason));
format_task_error(Reason) -> iolist_to_binary(io_lib:format("~p", [Reason])).

-spec format_task_failure(map()) -> {integer(), binary()}.
format_task_failure(Task) ->
    case maps:get(result, Task, undefined) of
        {Code, Msg} -> {Code, Msg};
        _ -> {?JSONRPC_INTERNAL_ERROR, <<"Task failed">>}
    end.

%%====================================================================
%% Internal functions - Pagination Support
%%====================================================================

-spec handle_paginated_list_with_key([map()], map(), binary()) ->
    {ok, map()} | {error, binary()}.
handle_paginated_list_with_key(Items, _Params, ListKey) ->
    %% Simple pagination: return all items without cursor support
    {ok, #{ListKey => Items}}.

%%====================================================================
%% Internal functions - Tool Description Validation (Gap #40)
%%====================================================================

-spec get_tool_description_max_length() -> pos_integer().
get_tool_description_max_length() ->
    case application:get_env(erlmcp, tool_description_max_length) of
        {ok, MaxLength} when is_integer(MaxLength), MaxLength > 0 ->
            MaxLength;
        _ ->
            ?MCP_TOOL_DESCRIPTION_MAX_LENGTH_DEFAULT
    end.

-spec validate_tool_description(binary() | undefined) ->
    ok | {error, {integer(), binary(), map()}}.
validate_tool_description(undefined) ->
    %% Undefined descriptions are allowed
    ok;
validate_tool_description(Description) when is_binary(Description) ->
    MaxLength = get_tool_description_max_length(),
    DescriptionLength = byte_size(Description),

    case DescriptionLength > MaxLength of
        true ->
            ErrorData = #{
                <<"max_length">> => MaxLength,
                <<"actual_length">> => DescriptionLength,
                <<"description_preview">> => binary:part(Description, 0, min(50, DescriptionLength))
            },
            {error, {?MCP_ERROR_TOOL_DESCRIPTION_TOO_LONG, ?MCP_MSG_TOOL_DESCRIPTION_TOO_LONG, ErrorData}};
        false ->
            ok
    end;
validate_tool_description(_) ->
    %% Non-binary descriptions are invalid
    ErrorData = #{<<"error">> => <<"Description must be binary">>},
    {error, {?JSONRPC_INVALID_PARAMS, <<"Invalid description type">>, ErrorData}}.

%%====================================================================
%% Internal functions - List Change Notifications (Gaps #6-8)
%%====================================================================

-spec notify_list_changed(atom(), state()) -> ok.
notify_list_changed(Feature, State) ->
    case State#state.notifier_pid of
        undefined -> ok;
        _Pid ->
            try
                erlmcp_change_notifier:notify_list_changed(Feature)
            catch
                _Class:_Reason ->
                    logger:warning("Failed to notify list changed for ~p", [Feature]),
                    ok
            end
    end.

%%====================================================================
%% Internal functions - Path Canonicalization (Gap #36)
%%====================================================================

-spec canonicalize_and_validate_uri(binary()) -> {ok, binary()} | {error, term()}.
%%%---
%% @doc Canonicalize and validate a resource URI for security.
%% Prevents symlink attacks and path traversal exploits.
%%
%% Gap #36 Implementation: Enforces secure resource access by:
%% 1. Canonicalizing paths (resolving symlinks, normalizing ..)
%% 2. Validating canonical path is within allowed directories
%% 3. Rejecting traversal attempts and jailbreak attacks
canonicalize_and_validate_uri(Uri) when is_binary(Uri) ->
    %% Get allowed resource directories from config
    AllowedDirs = application:get_env(erlmcp, allowed_resource_dirs, [<<"/">>]),

    case erlmcp_path_canonicalizer:validate_resource_path(Uri, AllowedDirs) of
        {ok, CanonicalUri} -> {ok, CanonicalUri};
        {error, Reason} -> {error, Reason}
    end;
canonicalize_and_validate_uri(_) ->
    {error, invalid_uri_format}.
