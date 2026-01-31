-module(erlmcp_server).
-behaviour(gen_server).

-include("erlmcp.hrl").
%% TODO: Add opentelemetry_api dependency when telemetry is enabled
%% -include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% API exports
-export([
    start_link/2,
    add_resource/3,
    add_resource_template/4,
    add_tool/3,
    add_tool_with_description/4,
    add_tool_with_schema/4,
    add_tool_full/5,
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
    register_notification_handler/3,
    unregister_notification_handler/2,
    unregister_all_handlers/1,
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
    initialized = false :: boolean(),
    last_tools_notification :: integer() | undefined,  % Rate limiting: timestamp of last tools/list_changed notification
    roots = #{} :: map(),  % Track roots state for change detection
    notification_handlers = #{} :: #{binary() => {pid(), reference()}}  % Notification method -> {HandlerPid, MonitorRef}
}).

-type state() :: #state{}.

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

%% @doc Add tool with custom description (max 10000 characters).
%% Validates description length before adding tool.
-spec add_tool_with_description(server(), binary(), binary(), tool_handler()) -> ok.
add_tool_with_description(Server, Name, Description, Handler)
  when is_binary(Name), is_binary(Description), is_function(Handler, 1) ->
    gen_server:call(Server, {add_tool_with_description, Name, Description, Handler}).

%% @doc Add tool with full metadata including description, schema, and deprecated flag.
%% Description is required and must be <= 10000 characters.
%% InputSchema is optional JSON Schema for argument validation.
%% Deprecated marks the tool as deprecated (default: false).
-spec add_tool_full(server(), binary(), binary(), tool_handler(), map()) -> ok.
add_tool_full(Server, Name, Description, Handler, Options)
  when is_binary(Name), is_binary(Description), is_function(Handler, 1), is_map(Options) ->
    gen_server:call(Server, {add_tool_full, Name, Description, Handler, Options}).

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

%% @doc Register a handler for a specific notification method.
%% The handler process will receive {mcp_notification, Method, Params} messages
%% when notifications of this type are received.
%% HandlerPid is monitored and will be automatically unregistered if it dies.
-spec register_notification_handler(server(), binary(), pid()) -> ok.
register_notification_handler(Server, Method, HandlerPid) when is_binary(Method), is_pid(HandlerPid) ->
    gen_server:call(Server, {register_notification_handler, Method, HandlerPid}).

%% @doc Unregister a handler for a specific notification method.
-spec unregister_notification_handler(server(), binary()) -> ok.
unregister_notification_handler(Server, Method) when is_binary(Method) ->
    gen_server:call(Server, {unregister_notification_handler, Method}).

%% @doc Unregister all notification handlers for the calling process.
%% Useful for cleanup when a client disconnects.
-spec unregister_all_handlers(server()) -> ok.
unregister_all_handlers(Server) ->
    gen_server:call(Server, unregister_all_handlers).

-spec stop(server()) -> ok.
stop(Server) ->
    gen_server:stop(Server).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([server_id() | #mcp_server_capabilities{}]) -> {ok, state()}.
init([ServerId, Capabilities]) ->
    process_flag(trap_exit, true),

    % OTP Pattern: NEVER block init/1 - use async cast initialization
    % Trigger async initialization via cast to avoid blocking supervisor
    gen_server:cast(self(), async_init),

    % Start periodic GC for each server (Gap #10)
    start_periodic_gc(),

    State = #state{
        server_id = ServerId,
        capabilities = Capabilities,
        notifier_pid = undefined,  % Will be set in async_init
        initialized = false        % Will be set to true after async_init completes
    },

    logger:info("Starting MCP server ~p with capabilities: ~p (async init pending)", [ServerId, Capabilities]),
    {ok, State}.

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
    notify_tools_changed(State),
    {reply, ok, State#state{tools = NewTools}};

handle_call({add_tool_with_schema, Name, Handler, Schema}, _From, State) ->
    Tool = #mcp_tool{
        name = Name,
        description = <<"Tool: ", Name/binary>>,
        input_schema = Schema
    },
    NewTools = maps:put(Name, {Tool, Handler, Schema}, State#state.tools),
    notify_tools_changed(State),
    {reply, ok, State#state{tools = NewTools}};

handle_call({add_tool_with_description, Name, Description, Handler}, _From, State) ->
    Tool = #mcp_tool{
        name = Name,
        description = Description
    },
    NewTools = maps:put(Name, {Tool, Handler, undefined}, State#state.tools),
    notify_tools_changed(State),
    {reply, ok, State#state{tools = NewTools}};

handle_call({add_tool_full, Name, Description, Handler, Options}, _From, State) ->
    InputSchema = maps:get(<<"inputSchema">>, Options, undefined),
    Deprecated = maps:get(<<"deprecated">>, Options, false),
    Metadata = maps:get(<<"metadata">>, Options, undefined),
    Experimental = maps:get(<<"experimental">>, Options, undefined),
    Version = maps:get(<<"version">>, Options, undefined),

    Tool = #mcp_tool{
        name = Name,
        description = Description,
        input_schema = InputSchema,
        deprecated = Deprecated,
        metadata = Metadata,
        experimental = Experimental,
        version = Version
    },
    NewTools = maps:put(Name, {Tool, Handler, InputSchema}, State#state.tools),
    notify_tools_changed(State),
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
            %% Notify about list changed inline
            _ = notify_list_changed(resources, State),
            {reply, ok, State#state{resources = NewResources}};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_tool, Name}, _From, State) ->
    case maps:is_key(Name, State#state.tools) of
        true ->
            NewTools = maps:remove(Name, State#state.tools),
            notify_tools_changed(State),
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
    % Subscribe via resource subscriptions manager if available
    % Otherwise fall back to local tracking
    case erlang:whereis(erlmcp_resource_subscriptions) of
        undefined ->
            % Resource subscriptions manager not running - use local tracking only
            logger:debug("Resource subscriptions manager not available, using local tracking"),
            NewSubscriptions = add_subscription(Uri, Subscriber, State#state.subscriptions),
            {reply, ok, State#state{subscriptions = NewSubscriptions}};
        _Pid ->
            % Resource subscriptions manager running - use it
            case erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, #{}) of
                ok ->
                    % Track subscription count in server state for cleanup
                    NewSubscriptions = add_subscription(Uri, Subscriber, State#state.subscriptions),
                    {reply, ok, State#state{subscriptions = NewSubscriptions}};
                {error, Reason} ->
                    logger:warning("Failed to subscribe to resource ~p: ~p", [Uri, Reason]),
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({unsubscribe_resource, Uri}, From, State) ->
    % Remove the caller's subscription
    CallerPid = element(1, From),

    % Unsubscribe via resource subscriptions manager if available
    case erlang:whereis(erlmcp_resource_subscriptions) of
        undefined ->
            % Resource subscriptions manager not running - clean local state only
            NewSubscriptions = remove_subscription(Uri, CallerPid, State#state.subscriptions),
            {reply, ok, State#state{subscriptions = NewSubscriptions}};
        _Pid ->
            case erlmcp_resource_subscriptions:unsubscribe_from_resource(Uri, CallerPid) of
                ok ->
                    NewSubscriptions = remove_subscription(Uri, CallerPid, State#state.subscriptions),
                    {reply, ok, State#state{subscriptions = NewSubscriptions}};
                {error, not_found} ->
                    % Already unsubscribed or never subscribed - still clean local state
                    NewSubscriptions = remove_subscription(Uri, CallerPid, State#state.subscriptions),
                    {reply, ok, State#state{subscriptions = NewSubscriptions}}
            end
    end;

handle_call({register_notification_handler, Method, HandlerPid}, _From, State) ->
    % Monitor the handler process for automatic cleanup
    case maps:get(Method, State#state.notification_handlers, undefined) of
        undefined ->
            % New handler registration
            Ref = monitor(process, HandlerPid),
            NewHandlers = maps:put(Method, {HandlerPid, Ref}, State#state.notification_handlers),
            logger:info("Registered notification handler for ~p: ~p", [Method, HandlerPid]),
            {reply, ok, State#state{notification_handlers = NewHandlers}};
        {_ExistingPid, _ExistingRef} ->
            % Handler already registered for this method
            {reply, {error, already_registered}, State}
    end;

handle_call({unregister_notification_handler, Method}, _From, State) ->
    case maps:get(Method, State#state.notification_handlers, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        {_HandlerPid, Ref} ->
            % Demonitor the process
            demonitor(Ref, [flush]),
            NewHandlers = maps:remove(Method, State#state.notification_handlers),
            logger:info("Unregistered notification handler for ~p", [Method]),
            {reply, ok, State#state{notification_handlers = NewHandlers}}
    end;

handle_call(unregister_all_handlers, {CallerPid, _Tag}, State) ->
    % Remove all handlers registered by the caller
    NewHandlers = maps:filter(fun(_Method, {HandlerPid, Ref}) ->
        if
            HandlerPid =:= CallerPid ->
                demonitor(Ref, [flush]),
                false;
            true ->
                true
        end
    end, State#state.notification_handlers),
    {reply, ok, State#state{notification_handlers = NewHandlers}};

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
    % Notify via resource subscriptions manager if available
    case erlang:whereis(erlmcp_resource_subscriptions) of
        undefined ->
            % Resource subscriptions manager not running - use local notification only
            logger:debug("Resource subscriptions manager not available, using local notification"),
            notify_subscribers(Uri, Metadata, State);
        _Pid ->
            erlmcp_resource_subscriptions:notify_resource_changed(Uri, Metadata),
            % Also notify local subscribers for backward compatibility
            notify_subscribers(Uri, Metadata, State)
    end,
    {noreply, State};

handle_cast(notify_resources_changed, State) ->
    send_notification_safe(State, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, #{}),
    {noreply, State};

handle_cast(notify_tools_changed, State) ->
    NewState = maybe_send_tools_list_changed(State),
    {noreply, NewState};

%% @doc Async initialization handler - performs blocking operations after init/1 returns.
%% OTP Pattern: This prevents blocking the supervisor tree during process startup.
%% Called via gen_server:cast(self(), async_init) from init/1.
handle_cast(async_init, #state{server_id = ServerId, initialized = false} = State) ->
    logger:debug("MCP server ~p: Starting async initialization", [ServerId]),

    % Start or get change notifier (this is the blocking operation moved from init/1)
    NotifierPid = case erlmcp_change_notifier:start_link() of
        {ok, Pid} ->
            logger:debug("MCP server ~p: Started new change notifier ~p", [ServerId, Pid]),
            Pid;
        {error, {already_started, Pid}} ->
            logger:debug("MCP server ~p: Using existing change notifier ~p", [ServerId, Pid]),
            Pid
    end,

    % Monitor the notifier process
    _MonitorRef = erlang:monitor(process, NotifierPid),

    NewState = State#state{
        notifier_pid = NotifierPid,
        initialized = true
    },

    logger:info("MCP server ~p: Async initialization complete, notifier_pid=~p", [ServerId, NotifierPid]),
    {noreply, NewState};

%% @doc Ignore duplicate async_init if already initialized (defensive programming)
handle_cast(async_init, #state{initialized = true} = State) ->
    logger:warning("MCP server ~p: Ignoring duplicate async_init (already initialized)", [State#state.server_id]),
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

handle_info(force_gc, #state{server_id = ServerId} = State) ->
    %% Periodic garbage collection (Gap #10)
    Before = erlang:memory(total),
    _ = garbage_collect(),
    After = erlang:memory(total),
    Freed = Before - After,

    logger:debug("Server ~p: Periodic GC freed ~p bytes (~.2f MB)", [
        ServerId,
        Freed,
        Freed / (1024 * 1024)
    ]),

    %% Reschedule periodic GC
    start_periodic_gc(),
    {noreply, State};

% Handle handler process death - automatic cleanup
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    % Find and remove the handler with this monitor reference
    NewHandlers = maps:filter(fun(_Method, {_HandlerPid, HandlerRef}) ->
        HandlerRef =/= Ref
    end, State#state.notification_handlers),
    case maps:size(State#state.notification_handlers) - maps:size(NewHandlers) of
        0 ->
            % No handler was removed (Ref not found)
            {noreply, State};
        _ ->
            % At least one handler was removed
            logger:info("Automatically unregistered dead notification handler (ref: ~p)", [Ref]),
            {noreply, State#state{notification_handlers = NewHandlers}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%====================================================================
%% Internal functions - List Change Notifications (Gap #6)
%%====================================================================

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{server_id = ServerId, subscriptions = Subscriptions}) ->
    logger:info("MCP server ~p (refactored) terminating", [ServerId]),

    % Cleanup all resource subscriptions for this server
    % The resource_subscriptions manager monitors subscriber processes
    % and will automatically cleanup when the server process dies
    case erlang:whereis(erlmcp_resource_subscriptions) of
        undefined ->
            % Resource subscriptions manager not running - no cleanup needed
            ok;
        _Pid ->
            maps:foreach(fun(Uri, Subscriber) ->
                logger:debug("Server ~p: Cleaning up subscription to ~p for ~p", [ServerId, Uri, Subscriber]),
                % Best effort cleanup - ignore errors since process is terminating
                case erlmcp_resource_subscriptions:unsubscribe_from_resource(Uri, Subscriber) of
                    ok -> ok;
                    {error, not_found} -> ok  % Already cleaned up
                end
            end, Subscriptions)
    end,

    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Request Handling
%%====================================================================

-spec handle_request(json_rpc_id(), binary(), json_rpc_params(), atom(), state()) ->
    {noreply, state()}.

%% STRICT INITIALIZATION ENFORCEMENT (P0 Security)
%% All non-initialize requests rejected before initialization completes
%% This enforces the MCP 2025-11-25 initialization state machine (Gap #4)

%% Initialize request - only allowed in initialization phase
handle_request(Id, ?MCP_METHOD_INITIALIZE, Params, TransportId, #state{server_id = ServerId, initialized = false} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_initialize">>, ServerId),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_INITIALIZE,
            <<"phase">> => <<"initialization">>,
            <<"phase_enforcement">> => <<"strict">>
        }),

        %% Validate client info first (required field)
        case validate_client_info(Params) of
            ok ->
                ok;
            {error, ClientInfoError} ->
                erlmcp_tracing:record_error_details(SpanCtx, invalid_client_info, ClientInfoError),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, ClientInfoError),
                throw({client_info_error, ClientInfoError})
        end,

        %% Extract and validate client capabilities
        ClientCapabilities = erlmcp_capabilities:extract_client_capabilities(Params),
        ProtocolVersion = maps:get(?MCP_FIELD_PROTOCOL_VERSION, Params, ?MCP_VERSION),

        %% Validate protocol version against supported versions
        case erlmcp_capabilities:validate_protocol_version(ProtocolVersion) of
            ok ->
                erlmcp_tracing:set_attributes(SpanCtx, #{
                    <<"client.protocol_version">> => ProtocolVersion,
                    <<"client.capabilities">> => <<"negotiated">>
                }),

                %% Negotiate capabilities - validate requested against server capabilities
                NegotiatedCapabilities = erlmcp_capabilities:negotiate_capabilities(
                    ClientCapabilities,
                    State#state.capabilities
                ),

                %% Build and send initialize response with negotiated capabilities
                Response = build_initialize_response(NegotiatedCapabilities),
                send_response_via_registry(State, TransportId, Id, Response),
                erlmcp_tracing:set_status(SpanCtx, ok),

                %% Send notifications/initialized after successful initialize
                %% This is REQUIRED by MCP 2025-11-25 specification (Gap P0-1)
                send_notification_via_registry(State, ?MCP_METHOD_INITIALIZED, #{}),

                %% Transition to initialized phase
                NewState = State#state{
                    initialized = true,
                    client_capabilities = ClientCapabilities,
                    protocol_version = ProtocolVersion,
                    phase = ?MCP_PHASE_INITIALIZED,
                    capabilities = NegotiatedCapabilities
                },
                {noreply, NewState};
            {error, ErrorMsg} ->
                erlmcp_tracing:record_error_details(SpanCtx, protocol_version_mismatch, ErrorMsg),
                send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_UNSUPPORTED_PROTOCOL_VERSION, ErrorMsg),
                {noreply, State}
        end
    catch
        {client_info_error, _Reason} ->
            {noreply, State};
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            erlang:raise(Class, Reason, Stacktrace)
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

%% P0 SECURITY: Reject double initialization strictly
%% MCP spec: "Initialize must be called only once per connection"
handle_request(Id, ?MCP_METHOD_INITIALIZE, _Params, TransportId, #state{initialized = true} = State) ->
    erlmcp_tracing:log("PROTOCOL_VIOLATION: Double initialize attempt on already initialized connection", [
        {request_id, Id},
        {violation_type, double_initialize},
        {severity, critical}
    ]),
    send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_NOT_INITIALIZED,
        <<"Server already initialized. Initialize must be called only once.">>),
    {noreply, State};

%% P0 SECURITY: Reject ALL non-initialize RPC requests before initialization
%% This is critical for protocol safety - prevents any operation until handshake completes
handle_request(Id, Method, _Params, TransportId, #state{initialized = false} = State) ->
    erlmcp_tracing:log("PROTOCOL_VIOLATION: RPC before initialization", [
        {request_id, Id},
        {method, Method},
        {violation_type, pre_init_rpc},
        {severity, critical}
    ]),
    send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_NOT_INITIALIZED,
        <<"Cannot execute operation before server initialization. Call initialize first.">>),
    {noreply, State};

%% Resources/list (and all other operations) now protected by pre-init check above
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
                %% Check for async task execution (MCP 2025-11-25)
                TaskParams = maps:get(<<"task">>, Params, undefined),
                case TaskParams of
                    undefined ->
                        %% Synchronous execution
                        handle_tool_call(Id, ToolName, Arguments, TransportId, State);
                    _ when is_map(TaskParams) ->
                        %% Async task execution
                        handle_tool_call_async(Id, ToolName, Arguments, TaskParams, TransportId, State, SpanCtx)
                end
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

%% Task management endpoints - MCP 2025-11-25 specification
%% These endpoints allow async task execution with progress tracking

handle_request(Id, ?MCP_METHOD_TASKS_CREATE, Params, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_tasks_create">>, ServerId),
    %% Capture server PID before try block for use in task operations
    ServerPid = self(),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_TASKS_CREATE
        }),

        case validate_task_create_params(Params) of
            {ok, Action, Metadata, Options} ->
                %% Add server PID to metadata for notifications
                MetadataWithPid = Metadata#{<<"serverPid">> => ServerPid},
                case erlmcp_tasks:create(ServerPid, Action, MetadataWithPid, Options) of
                    {ok, TaskId} ->
                        %% Get the created task details
                        case erlmcp_tasks:get(ServerPid, TaskId) of
                            {ok, TaskMap} ->
                                erlmcp_tracing:set_status(SpanCtx, ok),
                                Response = #{
                                    ?MCP_PARAM_TASK => TaskMap
                                },
                                send_response_via_registry(State, TransportId, Id, Response);
                            {error, _} ->
                                %% Return basic task info if get fails
                                Response = #{
                                    ?MCP_PARAM_TASK => #{
                                        ?MCP_PARAM_TASK_ID => TaskId,
                                        ?MCP_PARAM_STATUS => <<"pending">>
                                    }
                                },
                                send_response_via_registry(State, TransportId, Id, Response)
                        end;
                    {error, CreateReason} ->
                        erlmcp_tracing:record_error_details(SpanCtx, task_create_failed, CreateReason),
                        send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_TASK_FAILED,
                            io_lib:format("Failed to create task: ~p", [CreateReason]))
                end;
            {error, {Code, Message}} ->
                erlmcp_tracing:record_error_details(SpanCtx, invalid_params, Message),
                send_error_via_registry(State, TransportId, Id, Code, Message)
        end,
        {noreply, State}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_request(Id, ?MCP_METHOD_TASKS_LIST, Params, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_tasks_list">>, ServerId),
    ServerPid = self(),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_TASKS_LIST
        }),

        Cursor = maps:get(?MCP_PARAM_CURSOR, Params, undefined),
        Limit = maps:get(<<"limit">>, Params, 100),

        case erlmcp_tasks:list_tasks(ServerPid, Cursor, Limit) of
            {ok, #{tasks := Tasks} = Result} ->
                erlmcp_tracing:set_status(SpanCtx, ok),
                send_response_via_registry(State, TransportId, Id, Result);
            {error, ListReason} ->
                erlmcp_tracing:record_error_details(SpanCtx, task_list_failed, ListReason),
                send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_TASK_FAILED,
                    io_lib:format("Failed to list tasks: ~p", [ListReason]))
        end,
        {noreply, State}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_request(Id, ?MCP_METHOD_TASKS_GET, Params, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_tasks_get">>, ServerId),
    ServerPid = self(),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_TASKS_GET
        }),

        case maps:get(?MCP_PARAM_TASK_ID, Params, undefined) of
            undefined ->
                erlmcp_tracing:record_error_details(SpanCtx, missing_task_id, undefined),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS,
                    ?MCP_MSG_TASK_NOT_FOUND);
            TaskId ->
                case erlmcp_tasks:get(ServerPid, TaskId) of
                    {ok, TaskMap} ->
                        erlmcp_tracing:set_status(SpanCtx, ok),
                        Response = #{
                            ?MCP_PARAM_TASK => TaskMap
                        },
                        send_response_via_registry(State, TransportId, Id, Response);
                    {error, _GetReason} ->
                        erlmcp_tracing:record_error_details(SpanCtx, task_not_found, TaskId),
                        send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_TASK_NOT_FOUND,
                            ?MCP_MSG_TASK_NOT_FOUND)
                end
        end,
        {noreply, State}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_request(Id, ?MCP_METHOD_TASKS_RESULT, Params, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_tasks_result">>, ServerId),
    ServerPid = self(),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_TASKS_RESULT
        }),

        case maps:get(?MCP_PARAM_TASK_ID, Params, undefined) of
            undefined ->
                erlmcp_tracing:record_error_details(SpanCtx, missing_task_id, undefined),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS,
                    ?MCP_MSG_TASK_NOT_FOUND);
            TaskId ->
                case wait_for_task_result(ServerPid, TaskId, 30000) of
                    {ok, Result} ->
                        erlmcp_tracing:set_status(SpanCtx, ok),
                        Response = #{
                            ?MCP_PARAM_RESULT => Result
                        },
                        send_response_via_registry(State, TransportId, Id, Response);
                    {error, ResultReason} ->
                        erlmcp_tracing:record_error_details(SpanCtx, task_result_failed, ResultReason),
                        send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_TASK_RESULT_NOT_READY,
                            ?MCP_MSG_TASK_RESULT_NOT_READY)
                end
        end,
        {noreply, State}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_request(Id, ?MCP_METHOD_TASKS_CANCEL, Params, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_tasks_cancel">>, ServerId),
    ServerPid = self(),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_TASKS_CANCEL
        }),

        case maps:get(?MCP_PARAM_TASK_ID, Params, undefined) of
            undefined ->
                erlmcp_tracing:record_error_details(SpanCtx, missing_task_id, undefined),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS,
                    ?MCP_MSG_TASK_NOT_FOUND);
            TaskId ->
                CancelReason = maps:get(?MCP_PARAM_REASON, Params, <<"Cancelled by client">>),
                case erlmcp_tasks:cancel(ServerPid, TaskId, CancelReason) of
                    {ok, cancelled} ->
                        erlmcp_tracing:set_status(SpanCtx, ok),
                        send_response_via_registry(State, TransportId, Id, #{});
                    {error, task_not_found} ->
                        erlmcp_tracing:record_error_details(SpanCtx, task_not_found, TaskId),
                        send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_TASK_NOT_FOUND,
                            ?MCP_MSG_TASK_NOT_FOUND);
                    {error, CancelError} ->
                        erlmcp_tracing:record_error_details(SpanCtx, task_cancel_failed, CancelError),
                        send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_TASK_STATE_INVALID,
                            io_lib:format("Failed to cancel task: ~p", [CancelError]))
                end
        end,
        {noreply, State}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_request(Id, ?MCP_METHOD_RESOURCES_TEMPLATES_LIST, Params, TransportId, State) ->
    Templates = list_all_templates(State),
    case handle_paginated_templates_list(Templates, Params) of
        {ok, Response} ->
            send_response_via_registry(State, TransportId, Id, Response),
            {noreply, State};
        {error, TemplateReason} ->
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, TemplateReason),
            {noreply, State}
    end;
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
            % Remove only this transport's subscription for the URI
            NewSubscriptions = remove_subscription(Uri, self(), State#state.subscriptions),
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

%% Sampling/createMessage endpoint (Task #136: Implement Sampling Capability)
handle_request(Id, ?MCP_METHOD_SAMPLING_CREATE_MESSAGE, Params, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_sampling_create_message">>, ServerId),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_SAMPLING_CREATE_MESSAGE
        }),

        %% Extract messages and model preferences
        Messages = maps:get(?MCP_PARAM_MESSAGES, Params, []),
        ModelPreferences = maps:get(?MCP_PARAM_MODEL_PREFERENCES, Params, #{}),

        %% Validate messages format
        case validate_sampling_messages(Messages) of
            ok ->
                %% Build sampling params from model preferences
                SamplingParams = #{
                    <<"model">> => maps:get(<<"model">>, Params, undefined),
                    <<"temperature">> => maps:get(<<"temperature">>, ModelPreferences, undefined),
                    <<"maxTokens">> => maps:get(<<"maxTokens">>, ModelPreferences, undefined),
                    <<"stopSequences">> => maps:get(<<"stopSequences">>, ModelPreferences, undefined)
                },

                %% Call sampling module (30 second timeout for LLM calls)
                case erlmcp_sampling:create_message(Messages, SamplingParams, 30000) of
                    {ok, Result} ->
                        erlmcp_tracing:set_status(SpanCtx, ok),
                        send_response_via_registry(State, TransportId, Id, Result);
                    {error, SamplingReason} ->
                        erlmcp_tracing:record_error_details(SpanCtx, sampling_failed, SamplingReason),
                        logger:error("Sampling create_message failed: ~p", [SamplingReason]),
                        send_error_via_registry(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR,
                            format_sampling_error(SamplingReason))
                end;
            {error, ValidationError} ->
                erlmcp_tracing:record_error_details(SpanCtx, messages_validation_failed, ValidationError),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS,
                    format_sampling_error(ValidationError))
        end,

        {noreply, State}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            logger:error("Sampling request crashed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

%% Logging/setLevel endpoint (Task #137: Implement Logging Capability)
handle_request(Id, ?MCP_METHOD_LOGGING_SET_LEVEL, Params, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_logging_set_level">>, ServerId),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_LOGGING_SET_LEVEL
        }),

        %% Extract and validate level parameter
        case maps:get(<<"level">>, Params, undefined) of
            undefined ->
                erlmcp_tracing:record_error_details(SpanCtx, missing_level_parameter, undefined),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS, <<"Missing 'level' parameter">>),
                {noreply, State};
            LevelBinary when is_binary(LevelBinary) ->
                case validate_log_level_binary(LevelBinary) of
                    {ok, Level} ->
                        ClientPid = self(),
                        case erlmcp_logging:set_level(ClientPid, Level) of
                            ok ->
                                erlmcp_tracing:set_status(SpanCtx, ok),
                                Response = #{<<"level">> => LevelBinary},
                                send_response_safe(State, TransportId, Id, Response),
                                {noreply, State};
                            {error, {invalid_level, _InvalidLevel}} ->
                                erlmcp_tracing:record_error_details(SpanCtx, invalid_log_level, LevelBinary),
                                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS,
                                    <<"Invalid log level: ", LevelBinary/binary>>),
                                {noreply, State}
                        end;
                    {error, invalid_level} ->
                        erlmcp_tracing:record_error_details(SpanCtx, invalid_log_level, LevelBinary),
                        send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS,
                            <<"Invalid log level: ", LevelBinary/binary>>),
                        {noreply, State}
                end
        end,
        {noreply, State}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            logger:error("Logging set_level request crashed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_request(Id, _Method, _Params, TransportId, State) ->
    send_error_via_registry(State, TransportId, Id, ?JSONRPC_METHOD_NOT_FOUND, ?JSONRPC_MSG_METHOD_NOT_FOUND),
    {noreply, State}.

-spec handle_notification(binary(), map(), state()) -> {noreply, state()}.
handle_notification(Method, Params, #state{notification_handlers = Handlers} = State) ->
    % Dispatch to registered handler if one exists
    case maps:get(Method, Handlers, undefined) of
        undefined ->
            % No handler registered, just log and continue
            logger:debug("No handler registered for notification method: ~p", [Method]),
            {noreply, State};
        {HandlerPid, _Ref} ->
            % Send notification to handler process
            try
                HandlerPid ! {mcp_notification, Method, Params},
                logger:debug("Dispatched notification ~p to handler ~p", [Method, HandlerPid])
            catch
                Class:Reason ->
                    logger:error("Failed to dispatch notification ~p to handler ~p: ~p:~p",
                               [Method, HandlerPid, Class, Reason])
            end,
            {noreply, State}
    end.

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

%% @doc Validate client info from initialize request.
%% Ensures required clientInfo field is present and valid.
-spec validate_client_info(map()) -> ok | {error, binary()}.
validate_client_info(Params) when is_map(Params) ->
    case maps:get(<<"clientInfo">>, Params, undefined) of
        undefined ->
            {error, <<"Missing required field: clientInfo">>};
        ClientInfo when is_map(ClientInfo) ->
            case validate_client_info_fields(ClientInfo) of
                ok -> ok;
                {error, _} = Error -> Error
            end;
        _ ->
            {error, <<"Invalid clientInfo format">>}
    end.

%% @doc Validate client info fields.
%% Checks that name and version are present and valid.
-spec validate_client_info_fields(map()) -> ok | {error, binary()}.
validate_client_info_fields(ClientInfo) ->
    Name = maps:get(<<"name">>, ClientInfo, undefined),
    Version = maps:get(<<"version">>, ClientInfo, undefined),

    case {Name, Version} of
        {undefined, _} ->
            {error, <<"Missing required field: clientInfo.name">>};
        {_, undefined} ->
            {error, <<"Missing required field: clientInfo.version">>};
        {N, V} when is_binary(N), is_binary(V), byte_size(N) > 0, byte_size(V) > 0 ->
            ok;
        {N, _} when is_binary(N); is_list(N) ->
            {error, <<"Invalid clientInfo.name: must be non-empty string">>};
        {_, V} when is_binary(V); is_list(V) ->
            {error, <<"Invalid clientInfo.version: must be non-empty string">>};
        _ ->
            {error, <<"Invalid clientInfo format">>}
    end.

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

                % Extract client_id from state for CPU quota tracking
                % Use server_id as fallback for client identification
                ClientId = case State#state.server_id of
                    {_, SessionId} when is_binary(SessionId) -> SessionId;
                    ServerId when is_binary(ServerId) -> ServerId;
                    _ -> <<"unknown_client">>
                end,

                try
                    % TASK #107: Execute with CPU protection (quota + timeout)
                    % This prevents CPU-intensive DoS attacks
                    TimeoutMs = case application:get_env(erlmcp, tool_timeout_ms) of
                        {ok, Timeout} -> Timeout;
                        undefined -> 5000  % Default 5 second timeout
                    end,

                    case erlmcp_cpu_guard:execute_with_protection(
                        ClientId, tool_call, Handler, [Arguments], TimeoutMs
                    ) of
                        {ok, Result, CpuTime} ->
                            % Tool executed successfully
                            erlmcp_tracing:set_attributes(HandlerSpanCtx, #{
                                <<"cpu_time_ms">> => CpuTime
                            }),

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

                            erlmcp_tracing:set_status(SpanCtx, ok);
                        {error, quota_exceeded, cpu_time} ->
                            % CPU time quota exceeded
                            erlmcp_tracing:record_error_details(HandlerSpanCtx, cpu_quota_exceeded, cpu_time),
                            logger:warning("CPU quota exceeded for tool ~p", [Name]),
                            send_error_safe(State, TransportId, Id, -32603,
                                <<"CPU quota exceeded. Please retry later.">>);
                        {error, quota_exceeded, operations} ->
                            % Operations count quota exceeded
                            erlmcp_tracing:record_error_details(HandlerSpanCtx, ops_quota_exceeded, operations),
                            logger:warning("Operations quota exceeded for tool ~p", [Name]),
                            send_error_safe(State, TransportId, Id, -32603,
                                <<"Operation rate limit exceeded. Please retry later.">>);
                        {error, timeout} ->
                            % Tool execution timeout
                            erlmcp_tracing:record_error_details(HandlerSpanCtx, timeout, TimeoutMs),
                            logger:error("Tool ~p timed out after ~pms", [Name, TimeoutMs]),
                            send_error_safe(State, TransportId, Id, -32603,
                                <<"Tool execution timeout. Operation took too long.">>);
                        {error, Reason} ->
                            % Other error
                            erlmcp_tracing:record_error_details(HandlerSpanCtx, execution_error, Reason),
                            logger:error("Tool handler error: ~p", [Reason]),
                            send_error_safe(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR)
                    end,

                    % Clean up progress token after completion
                    erlmcp_progress:cleanup_completed(ProgressToken)
                catch
                    ClassCatch:ReasonCatch:StackCatch ->
                        erlmcp_tracing:record_exception(HandlerSpanCtx, ClassCatch, ReasonCatch, StackCatch),
                        logger:error("Tool handler crashed: ~p:~p~n~p", [ClassCatch, ReasonCatch, StackCatch]),
                        % Cleanup even on error
                        erlmcp_progress:cleanup_completed(ProgressToken),
                        send_error_safe(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR)
                after
                    erlmcp_tracing:end_span(HandlerSpanCtx)
                end
        end
    catch
        ClassOuter:ReasonOuter:StackOuter ->
            erlmcp_tracing:record_exception(SpanCtx, ClassOuter, ReasonOuter, StackOuter),
            erlang:raise(ClassOuter, ReasonOuter, StackOuter)
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

%% @doc Handle async tool call execution (MCP 2025-11-25 Tasks API)
%% Creates a task and executes the tool asynchronously
-spec handle_tool_call_async(
    json_rpc_id(),
    binary(),
    map(),
    map(),
    atom(),
    state(),
    term()
) -> ok.
handle_tool_call_async(Id, Name, Arguments, TaskParams, TransportId, State, _ParentSpanCtx) ->
    %% First, verify the tool exists
    case maps:get(Name, State#state.tools, undefined) of
        undefined ->
            erlmcp_tracing:record_error_details(_ParentSpanCtx, tool_not_found, Name),
            send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_TOOL_NOT_FOUND, ?MCP_MSG_TOOL_NOT_FOUND);
        {_Tool, Handler, _Schema} ->
            %% Extract task options
            TTL = maps:get(<<"ttl">>, TaskParams, 300000),  % 5 minutes default
            TimeoutMs = maps:get(<<"timeout">>, TaskParams, 30000),  % 30 seconds default

            %% Build action map for task
            Action = #{
                <<"type">> => <<"tool_call">>,
                <<"toolName">> => Name,
                <<"arguments">> => Arguments
            },

            %% Build metadata for task
            Metadata = #{
                <<"timeout">> => TimeoutMs,
                <<"transportId">> => TransportId,
                <<"requestId">> => Id,
                <<"serverPid">> => self()
            },

            %% Build options for task
            Options = #{ttl_ms => TTL},

            ServerPid = self(),

            %% Create the task
            case erlmcp_tasks:create(ServerPid, Action, Metadata, Options) of
                {ok, TaskId} ->
                    %% Get task details for response
                    case erlmcp_tasks:get(ServerPid, TaskId) of
                        {ok, TaskMap} ->
                            %% Start async worker for tool execution
                            spawn(fun() ->
                                execute_tool_async_task(
                                    TaskId, ServerPid, Name, Handler, Arguments, TimeoutMs
                                )
                            end),

                            %% Return task info
                            Response = #{
                                ?MCP_PARAM_TASK => TaskMap
                            },
                            send_response_via_registry(State, TransportId, Id, Response);
                        {error, _} ->
                            %% Return minimal task info
                            Response = #{
                                ?MCP_PARAM_TASK => #{
                                    ?MCP_PARAM_TASK_ID => TaskId,
                                    ?MCP_PARAM_STATUS => <<"pending">>
                                }
                            },
                            send_response_via_registry(State, TransportId, Id, Response)
                    end;
                {error, CreateError} ->
                    send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_TASK_FAILED,
                        io_lib:format("Failed to create task: ~p", [CreateError]))
            end
    end.

%% @doc Execute a tool asynchronously and update task status
-spec execute_tool_async_task(
    binary(),
    pid() | undefined,
    binary(),
    tool_handler(),
    map(),
    pos_integer()
) -> ok.
execute_tool_async_task(TaskId, ClientPid, Name, Handler, Arguments, TimeoutMs) ->
    %% Start task execution
    case erlmcp_tasks:start_task_execution(TaskId, self()) of
        ok ->
            try
                %% Execute the tool
                Result = case TimeoutMs of
                    undefined ->
                        Handler(Arguments);
                    Timeout ->
                        %% With timeout
                        case catch timer:apply_after(Timeout, erlang, exit, [normal]) of
                            {'EXIT', {timeout, _}} ->
                                {error, timeout};
                            _ ->
                                Handler(Arguments)
                        end
                end,

                %% Normalize and complete task
                ContentList = normalize_tool_result(Result),
                erlmcp_tasks:complete(ClientPid, TaskId, #{
                    ?MCP_PARAM_CONTENT => ContentList
                })
            catch
                Class:Reason:Stack ->
                    logger:error("Async tool ~p crashed: ~p:~p~n~p", [Name, Class, Reason, Stack]),
                    erlmcp_tasks:fail_task(TaskId, #{
                        code => ?MCP_ERROR_TOOL_EXECUTION_FAILED,
                        message => list_to_binary(io_lib:format("~p:~p", [Class, Reason])),
                        data => #{
                            class => Class,
                            reason => Reason
                        }
                    })
            end;
        {error, _} ->
            %% Task already started or failed
            ok
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
    _SpanCtx
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
    Base = #{
        ?MCP_PARAM_URI => Resource#mcp_resource.uri,
        ?MCP_PARAM_NAME => Resource#mcp_resource.name,
        ?MCP_PARAM_MIME_TYPE => Resource#mcp_resource.mime_type
    },
    Base1 = maybe_add_field(Base, ?MCP_PARAM_DESCRIPTION, Resource#mcp_resource.description),
    Base2 = maybe_add_field(Base1, ?MCP_PARAM_ANNOTATIONS, Resource#mcp_resource.annotations),
    Base3 = maybe_add_field(Base2, <<"size">>, Resource#mcp_resource.size),
    %% Add metadata (optional field)
    maybe_add_field(Base3, ?MCP_PARAM_METADATA, Resource#mcp_resource.metadata).

-spec encode_resource_template(#mcp_resource_template{}) -> map().
encode_resource_template(#mcp_resource_template{} = Template) ->
    Base = #{
        ?MCP_PARAM_URI_TEMPLATE => Template#mcp_resource_template.uri_template,
        ?MCP_PARAM_NAME => Template#mcp_resource_template.name,
        ?MCP_PARAM_MIME_TYPE => Template#mcp_resource_template.mime_type
    },
    maybe_add_field(Base, ?MCP_PARAM_DESCRIPTION, Template#mcp_resource_template.description).

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
%% Content Type Helper Functions (Tool Results Support)
%%====================================================================

%% @doc Normalize tool result to content array format.
%% Supports multiple content types: text, image, resource, audio, resource_link
%% Validates content type against allowed values per MCP 2025-11-25 spec.
-spec normalize_tool_result(term()) -> [map()].
normalize_tool_result(BinaryResult) when is_binary(BinaryResult) ->
    [#{?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT, ?MCP_PARAM_TEXT => BinaryResult}];
normalize_tool_result(ContentList) when is_list(ContentList) ->
    %% Handle list of content objects
    lists:map(fun normalize_content_item/1, ContentList);
normalize_tool_result(#mcp_content{} = Content) ->
    %% Handle single mcp_content record
    [encode_mcp_content(Content)];
normalize_tool_result({#mcp_content{}} = ContentTuple) ->
    %% Handle single mcp_content wrapped in tuple
    [encode_mcp_content(element(1, ContentTuple))];
normalize_tool_result(ContentMap) when is_map(ContentMap) ->
    %% Handle single content object as map
    case maps:get(?MCP_PARAM_TYPE, ContentMap, undefined) of
        undefined ->
            %% No type field, assume text content
            [#{?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT,
               ?MCP_PARAM_TEXT => maps:get(?MCP_PARAM_TEXT, ContentMap, <<>>)}];
        Type ->
            case validate_content_type(Type) of
                ok ->
                    [ContentMap];
                {error, invalid_type} ->
                    logger:warning("Invalid content type in tool result: ~p, defaulting to text", [Type]),
                    [#{?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT,
                       ?MCP_PARAM_TEXT => format_invalid_content(ContentMap)}]
            end
    end;
normalize_tool_result(Other) ->
    logger:warning("Unexpected tool result format: ~p, converting to text", [Other]),
    [#{?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT,
       ?MCP_PARAM_TEXT => io_lib:format("~p", [Other])}].

%% @doc Normalize a single content item from various formats to map.
-spec normalize_content_item(term()) -> map().
normalize_content_item(#mcp_content{} = Content) ->
    encode_mcp_content(Content);
normalize_content_item(ContentMap) when is_map(ContentMap) ->
    Type = maps:get(?MCP_PARAM_TYPE, ContentMap, ?MCP_CONTENT_TYPE_TEXT),
    case validate_content_type(Type) of
        ok -> ContentMap;
        {error, invalid_type} ->
            logger:warning("Invalid content type: ~p, defaulting to text", [Type]),
            #{?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT,
              ?MCP_PARAM_TEXT => format_invalid_content(ContentMap)}
    end;
normalize_content_item(BinaryContent) when is_binary(BinaryContent) ->
    #{?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT, ?MCP_PARAM_TEXT => BinaryContent};
normalize_content_item(Other) ->
    logger:warning("Unexpected content item format: ~p, converting to text", [Other]),
    #{?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT,
      ?MCP_PARAM_TEXT => io_lib:format("~p", [Other])}.

%% @doc Encode mcp_content record to map format for JSON response.
-spec encode_mcp_content(#mcp_content{}) -> map().
encode_mcp_content(#mcp_content{type = Type} = Content) ->
    case validate_content_type(Type) of
        ok ->
            Base = #{?MCP_PARAM_TYPE => Type},
            %% Add type-specific fields
            Base1 = case Type of
                ?MCP_CONTENT_TYPE_TEXT ->
                    maybe_add_field(Base, ?MCP_PARAM_TEXT, Content#mcp_content.text);
                ?MCP_CONTENT_TYPE_IMAGE ->
                    ImageBase = maybe_add_field(Base, ?MCP_PARAM_DATA, Content#mcp_content.data),
                    maybe_add_field(ImageBase, ?MCP_PARAM_MIME_TYPE, Content#mcp_content.mime_type);
                ?MCP_CONTENT_TYPE_AUDIO ->
                    AudioBase = maybe_add_field(Base, ?MCP_PARAM_DATA, Content#mcp_content.data),
                    maybe_add_field(AudioBase, ?MCP_PARAM_MIME_TYPE, Content#mcp_content.mime_type);
                ?MCP_CONTENT_TYPE_RESOURCE ->
                    ResourceBase = maybe_add_field(Base, ?MCP_PARAM_URI, Content#mcp_content.text),
                    maybe_add_field(ResourceBase, ?MCP_PARAM_MIME_TYPE, Content#mcp_content.mime_type);
                ?MCP_CONTENT_TYPE_RESOURCE_LINK ->
                    maybe_add_resource_link(Base, Content#mcp_content.resource_link);
                _ ->
                    logger:warning("Unknown content type: ~p, using text field", [Type]),
                    maybe_add_field(Base, ?MCP_PARAM_TEXT, Content#mcp_content.text)
            end,
            %% Add annotations if present (Gap #22)
            maybe_add_annotations(Base1, Content#mcp_content.annotations);
        {error, invalid_type} ->
            logger:error("Invalid content type in mcp_content record: ~p", [Type]),
            #{?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT,
              ?MCP_PARAM_TEXT => <<"Error: Invalid content type">>}
    end.

%% @doc Validate content type against allowed values per MCP 2025-11-25 spec.
%% Allowed types: text, image, resource, audio, resource_link
-spec validate_content_type(binary()) -> ok | {error, invalid_type}.
validate_content_type(?MCP_CONTENT_TYPE_TEXT) -> ok;
validate_content_type(?MCP_CONTENT_TYPE_IMAGE) -> ok;
validate_content_type(?MCP_CONTENT_TYPE_RESOURCE) -> ok;
validate_content_type(?MCP_CONTENT_TYPE_AUDIO) -> ok;
validate_content_type(?MCP_CONTENT_TYPE_RESOURCE_LINK) -> ok;
validate_content_type(_) -> {error, invalid_type}.

%% @doc Format invalid content object as text string.
-spec format_invalid_content(map()) -> binary().
format_invalid_content(ContentMap) ->
    Text = maps:get(?MCP_PARAM_TEXT, ContentMap, <<>>),
    Data = maps:get(?MCP_PARAM_DATA, ContentMap, <<>>),
    case {Text, Data} of
        {<<>>, <<>>} -> <<"Invalid content object">>;
        {<<>>, _} -> Data;
        {_, <<>>} -> Text;
        {T, D} -> <<T/binary, " ", D/binary>>
    end.

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

-spec remove_subscription(binary(), pid(), map()) -> map().
remove_subscription(Uri, Subscriber, Subscriptions) ->
    case maps:get(Uri, Subscriptions, undefined) of
        undefined ->
            Subscriptions;
        Subscribers ->
            NewSubscribers = sets:del_element(Subscriber, Subscribers),
            case sets:size(NewSubscribers) of
                0 ->
                    maps:remove(Uri, Subscriptions);
                _ ->
                    maps:put(Uri, NewSubscribers, Subscriptions)
            end
    end.

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
%% Internal functions - Pagination Support
%%====================================================================

-spec handle_paginated_list_with_key([map()], map(), binary()) ->
    {ok, map()} | {error, binary()}.
handle_paginated_list_with_key(Items, Params, ListKey) ->
    %% Extract pagination parameters
    PageSize = maps:get(<<"pageSize">>, Params, undefined),
    Cursor = maps:get(<<"cursor">>, Params, null),
    TotalCount = length(Items),

    try
        %% Use pagination module for cursor-based pagination
        {PageItems, PageInfo} = erlmcp_pagination:paginate(
            Items, PageSize, Cursor, TotalCount
        ),

        %% Build response with pagination metadata
        Response = #{
            ListKey => PageItems,
            <<"nextCursor">> => maps:get(<<"cursor">>, PageInfo),
            <<"hasMore">> => maps:get(<<"hasMore">>, PageInfo),
            <<"total">> => maps:get(<<"total">>, PageInfo)
        },

        {ok, Response}
    catch
        _:_ ->
            %% Fallback to simple pagination on error
            {ok, #{ListKey => Items}}
    end.

%% @doc Handle paginated resource templates list with cursor-based pagination.
%% Supports filtering by URI pattern and returns _nextCursor for pagination.
%% Cursor format: base64:encode(<<"templates:", Offset/binary>>)
-spec handle_paginated_templates_list([map()], map()) ->
    {ok, map()} | {error, binary()}.
handle_paginated_templates_list(Templates, Params) ->
    try
        %% Extract pagination parameters
        Limit = case maps:get(<<"limit">>, Params, undefined) of
            undefined -> 100;  %% Default limit
            L when is_integer(L), L > 0, L =< 1000 -> L;
            _ -> throw({error, <<"Invalid limit parameter">>})
        end,

        Cursor = maps:get(<<"cursor">>, Params, undefined),
        UriPattern = maps:get(<<"uriPattern">>, Params, undefined),

        %% Apply URI pattern filter if provided
        FilteredTemplates = case UriPattern of
            undefined -> Templates;
            Pattern when is_binary(Pattern) ->
                filter_templates_by_uri_pattern(Templates, Pattern);
            _ ->
                throw({error, <<"Invalid uriPattern parameter">>})
        end,

        %% Decode cursor to get offset
        Offset = case Cursor of
            undefined -> 0;
            null -> 0;
            <<>> -> 0;
            CursorBin when is_binary(CursorBin) ->
                decode_template_cursor(CursorBin)
        end,

        %% Get total count before pagination
        TotalCount = length(FilteredTemplates),

        %% Apply offset and limit
        PaginatedTemplates = apply_offset_limit(FilteredTemplates, Offset, Limit),

        %% Build response
        ResponseBase = #{
            ?MCP_PARAM_RESOURCE_TEMPLATES => PaginatedTemplates
        },

        %% Add _nextCursor if more results exist
        Response = case Offset + length(PaginatedTemplates) < TotalCount of
            true ->
                NextOffset = Offset + length(PaginatedTemplates),
                NextCursor = encode_template_cursor(NextOffset),
                ResponseBase#{<<"_nextCursor">> => NextCursor};
            false ->
                ResponseBase
        end,

        {ok, Response}
    catch
        throw:{error, Reason} -> {error, Reason};
        _:_ -> {error, <<"Pagination processing failed">>}
    end.

%% @doc Filter templates by URI pattern (simple prefix match).
-spec filter_templates_by_uri_pattern([map()], binary()) -> [map()].
filter_templates_by_uri_pattern(Templates, Pattern) ->
    lists:filter(fun(Template) ->
        UriTemplate = maps:get(<<"uriTemplate">>, Template, <<>>),
        case UriTemplate of
            <<>> -> false;
            _ ->
                %% Simple prefix match for URI pattern
                PatternSize = byte_size(Pattern),
                case UriTemplate of
                    <<Pattern:PatternSize/binary, _/binary>> -> true;
                    _ -> false
                end
        end
    end, Templates).

%% @doc Apply offset and limit to a list.
-spec apply_offset_limit([map()], non_neg_integer(), pos_integer()) -> [map()].
apply_offset_limit(List, Offset, Limit) ->
    Length = length(List),
    Start = min(Offset, Length),
    End = min(Start + Limit, Length),
    lists:sublist(List, Start + 1, End - Start).

%% @doc Encode template cursor: base64:encode(<<"templates:", Offset/binary>>).
-spec encode_template_cursor(non_neg_integer()) -> binary().
encode_template_cursor(Offset) ->
    OffsetBin = integer_to_binary(Offset),
    CursorBin = <<"templates:", OffsetBin/binary>>,
    base64:encode(CursorBin).

%% @doc Decode template cursor: extract offset from base64:decode(<<"templates:", Offset/binary>>).
-spec decode_template_cursor(binary()) -> non_neg_integer().
decode_template_cursor(Cursor) ->
    try
        Decoded = base64:decode(Cursor),
        case Decoded of
            <<"templates:", OffsetBin/binary>> ->
                binary_to_integer(OffsetBin);
            _ ->
                0  %% Invalid cursor format, start from beginning
        end
    catch
        _:_ -> 0  %% Invalid base64, start from beginning
    end.


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

-spec notify_tools_changed(state()) -> ok.
notify_tools_changed(State) ->
    gen_server:cast(self(), notify_tools_changed),
    ok.

%% @doc Check if client supports tools/list_changed and send notification with rate limiting.
%% Only sends if:
%% 1. Client declared tools.listChanged capability
%% 2. At least 1 second has passed since last notification (rate limiting)
-spec maybe_send_tools_list_changed(state()) -> state().
maybe_send_tools_list_changed(#state{client_capabilities = undefined} = State) ->
    % No client capabilities yet, don't send
    State;
maybe_send_tools_list_changed(#state{client_capabilities = ClientCaps} = State) ->
    % Check if client supports tools.listChanged capability
    case client_supports_tools_list_changed(ClientCaps) of
        false ->
            % Client doesn't support this notification, skip
            State;
        true ->
            % Client supports it, check rate limit
            CurrentTime = erlang:system_time(millisecond),
            case State#state.last_tools_notification of
                undefined ->
                    % Never sent, send now
                    send_notification_safe(State, ?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED, #{}),
                    State#state{last_tools_notification = CurrentTime};
                LastTime when CurrentTime - LastTime >= 1000 ->
                    % At least 1 second passed, send now
                    send_notification_safe(State, ?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED, #{}),
                    State#state{last_tools_notification = CurrentTime};
                _LastTime ->
                    % Less than 1 second since last notification, skip (rate limited)
                    State
            end
    end.

%% @doc Check if client declared tools/list_changed capability.
%% Returns true if client supports it, false otherwise.
-spec client_supports_tools_list_changed(#mcp_client_capabilities{}) -> boolean().
client_supports_tools_list_changed(ClientCaps) ->
    % Check if client's tools capability has listChanged flag set to true
    ToolsCap = ClientCaps#mcp_client_capabilities.tools,
    case ToolsCap of
        undefined ->
            false;
        #mcp_tools_capability{listChanged = true} ->
            true;
        #mcp_tools_capability{} ->
            false
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

%%====================================================================
%% Internal functions - Memory Management (Gap #10)
%%====================================================================

%% @doc Start periodic garbage collection to prevent binary heap exhaustion.
%% Runs every 60 seconds to free accumulated binary data from JSON-RPC messages.
-spec start_periodic_gc() -> reference().
start_periodic_gc() ->
    erlang:send_after(60000, self(), force_gc).

%%====================================================================
%% Internal functions - Sampling Support (Task #136)
%%====================================================================

%% @doc Validate sampling messages format
-spec validate_sampling_messages(term()) -> ok | {error, binary()}.
validate_sampling_messages(Messages) when is_list(Messages) ->
    case length(Messages) of
        0 ->
            {error, <<"Messages list cannot be empty">>};
        _ ->
            case lists:all(fun validate_sampling_message/1, Messages) of
                true -> ok;
                false -> {error, <<"Invalid message format">>}
            end
    end;
validate_sampling_messages(_) ->
    {error, <<"Messages must be a list">>}.

%% @doc Validate a single sampling message
-spec validate_sampling_message(map()) -> boolean().
validate_sampling_message(Message) when is_map(Message) ->
    Role = maps:get(<<"role">>, Message, undefined),
    Content = maps:get(<<"content">>, Message, undefined),

    case {Role, Content} of
        {undefined, _} -> false;
        {_, undefined} -> false;
        {R, _} when is_binary(R) ->
            case Content of
                C when is_binary(C); is_map(C) -> true;
                _ -> false
            end;
        _ -> false
    end;
validate_sampling_message(_) ->
    false.

%% @doc Format sampling error for JSON-RPC response
-spec format_sampling_error(term()) -> binary().
format_sampling_error(Reason) when is_binary(Reason) ->
    Reason;
format_sampling_error(empty_messages) ->
    <<"Messages list cannot be empty">>;
format_sampling_error(invalid_message_format) ->
    <<"Invalid message format">>;
format_sampling_error(invalid_temperature) ->
    <<"Temperature must be between 0.0 and 2.0">>;
format_sampling_error(provider_error) ->
    <<"LLM provider error">>;
format_sampling_error(timeout) ->
    <<"LLM request timeout">>;
format_sampling_error(Reason) when is_atom(Reason) ->
    binary:list_to_bin(io_lib:format("~p", [Reason]));
format_sampling_error(_) ->
    <<"Unknown sampling error">>.

%%====================================================================
%% Internal functions - Logging Validation (Task #137)
%%====================================================================

%% @doc Validate log level binary parameter
%% Converts binary level string to atom and validates against allowed levels
-spec validate_log_level_binary(binary()) -> {ok, atom()} | {error, invalid_level}.
validate_log_level_binary(LevelBinary) when is_binary(LevelBinary) ->
    LevelString = binary_to_list(LevelBinary),
    try
        Level = list_to_existing_atom(LevelString),
        case lists:member(Level, ?MCP_VALID_LOG_LEVELS) of
            true -> {ok, Level};
            false -> {error, invalid_level}
        end
    catch
        error:badarg ->
            {error, invalid_level}
    end.

%%====================================================================
%% Internal functions - Tasks API Support (MCP 2025-11-25)
%%====================================================================

%% @doc Validate task creation parameters
%% Extracts action, metadata, and options from params
-spec validate_task_create_params(map()) ->
    {ok, map(), map(), map()} | {error, {integer(), binary()}}.
validate_task_create_params(Params) ->
    case maps:get(<<"action">>, Params, undefined) of
        undefined ->
            {error, {?JSONRPC_INVALID_PARAMS, <<"Missing required 'action' parameter">>}};
        Action when is_map(Action) ->
            Metadata = maps:get(<<"metadata">>, Params, #{}),
            Options = maps:get(<<"options">>, Params, #{}),
            {ok, Action, Metadata, Options};
        _ ->
            {error, {?JSONRPC_INVALID_PARAMS, <<"Action must be an object">>}}
    end.

%% @doc Wait for task result with timeout
%% Polls task status until complete, failed, or timeout
-spec wait_for_task_result(pid() | undefined, binary(), pos_integer()) ->
    {ok, term()} | {error, term()}.
wait_for_task_result(ClientPid, TaskId, Timeout) ->
    Start = erlang:system_time(millisecond),
    wait_for_task_result_loop(ClientPid, TaskId, Timeout, Start).

-spec wait_for_task_result_loop(pid() | undefined, binary(), pos_integer(), integer()) ->
    {ok, term()} | {error, term()}.
wait_for_task_result_loop(ClientPid, TaskId, Timeout, Start) ->
    case erlmcp_tasks:get_result(ClientPid, TaskId) of
        {ok, Result} ->
            {ok, Result};
        {error, ?MCP_ERROR_TASK_RESULT_NOT_READY} ->
            Now = erlang:system_time(millisecond),
            if
                Now - Start >= Timeout ->
                    {error, timeout};
                true ->
                    %% Wait before polling again (500ms)
                    timer:sleep(500),
                    wait_for_task_result_loop(ClientPid, TaskId, Timeout, Start)
            end;
        {error, Reason} ->
            {error, Reason}
    end.
