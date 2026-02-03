%%%-------------------------------------------------------------------
%%% @doc A2A (Agent-to-Agent) Server Module
%%%
%%% This module implements the main A2A server process that handles
%%% agent-to-agent protocol operations. It provides a task-centric
%%% model for agent communication, complementing MCP's tool-centric
%%% approach.
%%%
%%% Key Features:
%%% - JSON-RPC 2.0 based request handling
%%% - Task lifecycle management (create, get, list, cancel)
%%% - Multi-tenant isolation support
%%% - Skill handler registration and dispatch
%%% - Context management for conversation continuity
%%% - Integration with transport layer
%%% - Push notification support
%%% - Streaming support
%%%
%%% Protocol Specification: https://github.com/google/a2a-spec
%%% Version: 0.3 (Draft)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_a2a_server).

-behaviour(gen_server).

-include("erlmcp.hrl").
-include("erlmcp_a2a.hrl").

%% API exports
-export([
    start_link/1,
    start_link/2,
    stop/1,

    %% A2A RPC Methods
    send_message/2,
    get_task/2,
    list_tasks/2,
    cancel_task/2,

    %% Skill Handler Registration
    register_skill_handler/3,
    unregister_skill_handler/2,
    get_skill_handlers/1,

    %% Agent Card
    get_agent_card/1,
    set_agent_card/2,
    get_extended_agent_card/1,

    %% Context Management
    create_context/1,
    get_context/2,
    delete_context/2,

    %% Subscription Management
    subscribe_to_task/3,
    unsubscribe_from_task/3,

    %% Push Notification Config
    create_push_config/2,
    get_push_config/3,
    list_push_configs/2,
    delete_push_config/3,

    %% Connection Management
    get_connections/1,
    close_connection/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Types
-type server_ref() :: pid() | atom() | {atom(), node()} | {via, module(), term()}.
-type skill_handler() :: fun((#a2a_message{}, #a2a_task{} | undefined) ->
    {ok, #a2a_task{}} | {error, term()}).
-type connection_info() :: #{
    pid := pid(),
    transport := atom(),
    tenant := binary() | undefined,
    connected_at := integer()
}.

-export_type([server_ref/0, skill_handler/0, connection_info/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the A2A server with given options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    start_link(undefined, Options).

%% @doc Start the A2A server with name and options
-spec start_link(atom() | undefined, map()) -> {ok, pid()} | {error, term()}.
start_link(undefined, Options) ->
    gen_server:start_link(?MODULE, Options, []);
start_link(Name, Options) ->
    gen_server:start_link({local, Name}, ?MODULE, Options, []).

%% @doc Stop the A2A server
-spec stop(server_ref()) -> ok.
stop(ServerRef) ->
    gen_server:stop(ServerRef).

%% @doc Handle SendMessage RPC
%% Creates a new task or continues an existing one based on the message
-spec send_message(server_ref(), #a2a_send_message_request{}) ->
    {ok, #a2a_send_message_response{}} | {error, term()}.
send_message(ServerRef, Request) ->
    gen_server:call(ServerRef, {send_message, Request}, 30000).

%% @doc Handle GetTask RPC
%% Retrieves a task by ID with optional history
-spec get_task(server_ref(), #a2a_get_task_request{}) ->
    {ok, #a2a_task{}} | {error, term()}.
get_task(ServerRef, Request) ->
    gen_server:call(ServerRef, {get_task, Request}, 5000).

%% @doc Handle ListTasks RPC
%% Lists tasks with optional filtering and pagination
-spec list_tasks(server_ref(), #a2a_list_tasks_request{}) ->
    {ok, #a2a_list_tasks_response{}} | {error, term()}.
list_tasks(ServerRef, Request) ->
    gen_server:call(ServerRef, {list_tasks, Request}, 10000).

%% @doc Handle CancelTask RPC
%% Cancels a task if it's in a cancelable state
-spec cancel_task(server_ref(), #a2a_cancel_task_request{}) ->
    {ok, #a2a_task{}} | {error, term()}.
cancel_task(ServerRef, Request) ->
    gen_server:call(ServerRef, {cancel_task, Request}, 5000).

%% @doc Register a skill handler
%% The handler function is called when a message matches the skill
-spec register_skill_handler(server_ref(), binary(), skill_handler()) ->
    ok | {error, term()}.
register_skill_handler(ServerRef, SkillId, Handler) when is_binary(SkillId), is_function(Handler, 2) ->
    gen_server:call(ServerRef, {register_skill_handler, SkillId, Handler}, 5000).

%% @doc Unregister a skill handler
-spec unregister_skill_handler(server_ref(), binary()) -> ok | {error, not_found}.
unregister_skill_handler(ServerRef, SkillId) when is_binary(SkillId) ->
    gen_server:call(ServerRef, {unregister_skill_handler, SkillId}, 5000).

%% @doc Get all registered skill handlers
-spec get_skill_handlers(server_ref()) -> {ok, #{binary() => skill_handler()}}.
get_skill_handlers(ServerRef) ->
    gen_server:call(ServerRef, get_skill_handlers, 5000).

%% @doc Get the agent card
-spec get_agent_card(server_ref()) -> {ok, #a2a_agent_card{}} | {error, not_configured}.
get_agent_card(ServerRef) ->
    gen_server:call(ServerRef, get_agent_card, 5000).

%% @doc Set the agent card configuration
-spec set_agent_card(server_ref(), #a2a_agent_card{}) -> ok.
set_agent_card(ServerRef, AgentCard) ->
    gen_server:call(ServerRef, {set_agent_card, AgentCard}, 5000).

%% @doc Get the extended agent card (with additional details)
-spec get_extended_agent_card(server_ref()) ->
    {ok, #a2a_agent_card{}} | {error, not_supported | not_configured}.
get_extended_agent_card(ServerRef) ->
    gen_server:call(ServerRef, get_extended_agent_card, 5000).

%% @doc Create a new context
-spec create_context(server_ref()) -> {ok, binary()}.
create_context(ServerRef) ->
    gen_server:call(ServerRef, create_context, 5000).

%% @doc Get context by ID
-spec get_context(server_ref(), binary()) -> {ok, [binary()]} | {error, not_found}.
get_context(ServerRef, ContextId) when is_binary(ContextId) ->
    gen_server:call(ServerRef, {get_context, ContextId}, 5000).

%% @doc Delete a context and optionally its tasks
-spec delete_context(server_ref(), binary()) -> ok | {error, not_found}.
delete_context(ServerRef, ContextId) when is_binary(ContextId) ->
    gen_server:call(ServerRef, {delete_context, ContextId}, 5000).

%% @doc Subscribe to task updates
-spec subscribe_to_task(server_ref(), binary(), pid()) -> ok | {error, term()}.
subscribe_to_task(ServerRef, TaskId, SubscriberPid) when is_binary(TaskId), is_pid(SubscriberPid) ->
    gen_server:call(ServerRef, {subscribe_to_task, TaskId, SubscriberPid}, 5000).

%% @doc Unsubscribe from task updates
-spec unsubscribe_from_task(server_ref(), binary(), pid()) -> ok.
unsubscribe_from_task(ServerRef, TaskId, SubscriberPid) when is_binary(TaskId), is_pid(SubscriberPid) ->
    gen_server:call(ServerRef, {unsubscribe_from_task, TaskId, SubscriberPid}, 5000).

%% @doc Create push notification config for a task
-spec create_push_config(server_ref(), #a2a_create_push_config_request{}) ->
    {ok, #a2a_task_push_notification_config{}} | {error, term()}.
create_push_config(ServerRef, Request) ->
    gen_server:call(ServerRef, {create_push_config, Request}, 5000).

%% @doc Get push notification config
-spec get_push_config(server_ref(), binary(), binary()) ->
    {ok, #a2a_task_push_notification_config{}} | {error, not_found}.
get_push_config(ServerRef, TaskId, ConfigId) when is_binary(TaskId), is_binary(ConfigId) ->
    gen_server:call(ServerRef, {get_push_config, TaskId, ConfigId}, 5000).

%% @doc List push notification configs for a task
-spec list_push_configs(server_ref(), #a2a_list_push_configs_request{}) ->
    {ok, #a2a_list_push_configs_response{}}.
list_push_configs(ServerRef, Request) ->
    gen_server:call(ServerRef, {list_push_configs, Request}, 5000).

%% @doc Delete push notification config
-spec delete_push_config(server_ref(), binary(), binary()) -> ok | {error, not_found}.
delete_push_config(ServerRef, TaskId, ConfigId) when is_binary(TaskId), is_binary(ConfigId) ->
    gen_server:call(ServerRef, {delete_push_config, TaskId, ConfigId}, 5000).

%% @doc Get active connections
-spec get_connections(server_ref()) -> {ok, [connection_info()]}.
get_connections(ServerRef) ->
    gen_server:call(ServerRef, get_connections, 5000).

%% @doc Close a specific connection
-spec close_connection(server_ref(), pid()) -> ok | {error, not_found}.
close_connection(ServerRef, ConnectionPid) when is_pid(ConnectionPid) ->
    gen_server:call(ServerRef, {close_connection, ConnectionPid}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #a2a_server_state{}}.
init(Options) ->
    process_flag(trap_exit, true),

    %% Extract configuration from options
    AgentCard = maps:get(agent_card, Options, undefined),
    ExtendedAgentCard = maps:get(extended_agent_card, Options, undefined),
    Tenant = maps:get(tenant, Options, undefined),
    Capabilities = maps:get(capabilities, Options, #a2a_agent_capabilities{}),

    %% Initialize state using the record from erlmcp_a2a.hrl
    State = #a2a_server_state{
        agent_card = AgentCard,
        extended_agent_card = ExtendedAgentCard,
        tasks = #{},
        contexts = #{},
        push_configs = #{},
        subscriptions = #{},
        streams = #{},
        skill_handlers = #{},
        tenant = Tenant,
        capabilities = Capabilities
    },

    logger:info("A2A server started with tenant=~p, capabilities=~p",
                [Tenant, Capabilities]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #a2a_server_state{}) ->
    {reply, term(), #a2a_server_state{}}.

%% SendMessage RPC
handle_call({send_message, Request}, _From, State) ->
    Result = handle_send_message(Request, State),
    case Result of
        {ok, Response, NewState} ->
            {reply, {ok, Response}, NewState};
        {error, _} = Error ->
            {reply, Error, State}
    end;

%% GetTask RPC
handle_call({get_task, Request}, _From, State) ->
    Result = handle_get_task(Request, State),
    {reply, Result, State};

%% ListTasks RPC
handle_call({list_tasks, Request}, _From, State) ->
    Result = handle_list_tasks(Request, State),
    {reply, Result, State};

%% CancelTask RPC
handle_call({cancel_task, Request}, _From, State) ->
    Result = handle_cancel_task(Request, State),
    case Result of
        {ok, Task, NewState} ->
            {reply, {ok, Task}, NewState};
        {error, _} = Error ->
            {reply, Error, State}
    end;

%% Skill Handler Registration
handle_call({register_skill_handler, SkillId, Handler}, _From, State) ->
    %% Verify skill exists in agent card if configured
    case validate_skill_registration(SkillId, State) of
        ok ->
            NewHandlers = maps:put(SkillId, Handler, State#a2a_server_state.skill_handlers),
            {reply, ok, State#a2a_server_state{skill_handlers = NewHandlers}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({unregister_skill_handler, SkillId}, _From, State) ->
    case maps:is_key(SkillId, State#a2a_server_state.skill_handlers) of
        true ->
            NewHandlers = maps:remove(SkillId, State#a2a_server_state.skill_handlers),
            {reply, ok, State#a2a_server_state{skill_handlers = NewHandlers}};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_skill_handlers, _From, State) ->
    {reply, {ok, State#a2a_server_state.skill_handlers}, State};

%% Agent Card
handle_call(get_agent_card, _From, State) ->
    case State#a2a_server_state.agent_card of
        undefined ->
            {reply, {error, not_configured}, State};
        AgentCard ->
            {reply, {ok, AgentCard}, State}
    end;

handle_call({set_agent_card, AgentCard}, _From, State) ->
    {reply, ok, State#a2a_server_state{agent_card = AgentCard}};

handle_call(get_extended_agent_card, _From, State) ->
    case State#a2a_server_state.capabilities of
        #a2a_agent_capabilities{extended_agent_card = true} ->
            case State#a2a_server_state.extended_agent_card of
                undefined ->
                    %% Fall back to regular agent card
                    case State#a2a_server_state.agent_card of
                        undefined ->
                            {reply, {error, not_configured}, State};
                        Card ->
                            {reply, {ok, Card}, State}
                    end;
                ExtCard ->
                    {reply, {ok, ExtCard}, State}
            end;
        _ ->
            {reply, {error, not_supported}, State}
    end;

%% Context Management
handle_call(create_context, _From, State) ->
    ContextId = generate_uuid(),
    NewContexts = maps:put(ContextId, [], State#a2a_server_state.contexts),
    {reply, {ok, ContextId}, State#a2a_server_state{contexts = NewContexts}};

handle_call({get_context, ContextId}, _From, State) ->
    case maps:get(ContextId, State#a2a_server_state.contexts, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TaskIds ->
            {reply, {ok, TaskIds}, State}
    end;

handle_call({delete_context, ContextId}, _From, State) ->
    case maps:is_key(ContextId, State#a2a_server_state.contexts) of
        true ->
            NewContexts = maps:remove(ContextId, State#a2a_server_state.contexts),
            {reply, ok, State#a2a_server_state{contexts = NewContexts}};
        false ->
            {reply, {error, not_found}, State}
    end;

%% Subscription Management
handle_call({subscribe_to_task, TaskId, SubscriberPid}, _From, State) ->
    case maps:is_key(TaskId, State#a2a_server_state.tasks) of
        true ->
            %% Monitor subscriber
            _ = monitor(process, SubscriberPid),
            Subscribers = maps:get(TaskId, State#a2a_server_state.subscriptions, []),
            NewSubscribers = lists:usort([SubscriberPid | Subscribers]),
            NewSubscriptions = maps:put(TaskId, NewSubscribers, State#a2a_server_state.subscriptions),
            {reply, ok, State#a2a_server_state{subscriptions = NewSubscriptions}};
        false ->
            {reply, {error, task_not_found}, State}
    end;

handle_call({unsubscribe_from_task, TaskId, SubscriberPid}, _From, State) ->
    Subscribers = maps:get(TaskId, State#a2a_server_state.subscriptions, []),
    NewSubscribers = lists:delete(SubscriberPid, Subscribers),
    NewSubscriptions = case NewSubscribers of
        [] -> maps:remove(TaskId, State#a2a_server_state.subscriptions);
        _ -> maps:put(TaskId, NewSubscribers, State#a2a_server_state.subscriptions)
    end,
    {reply, ok, State#a2a_server_state{subscriptions = NewSubscriptions}};

%% Push Notification Config
handle_call({create_push_config, Request}, _From, State) ->
    Result = handle_create_push_config(Request, State),
    case Result of
        {ok, Config, NewState} ->
            {reply, {ok, Config}, NewState};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({get_push_config, TaskId, ConfigId}, _From, State) ->
    case maps:get(TaskId, State#a2a_server_state.push_configs, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ConfigMap ->
            case maps:get(ConfigId, ConfigMap, undefined) of
                undefined ->
                    {reply, {error, not_found}, State};
                Config ->
                    {reply, {ok, Config}, State}
            end
    end;

handle_call({list_push_configs, Request}, _From, State) ->
    Result = handle_list_push_configs(Request, State),
    {reply, Result, State};

handle_call({delete_push_config, TaskId, ConfigId}, _From, State) ->
    case maps:get(TaskId, State#a2a_server_state.push_configs, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ConfigMap ->
            case maps:is_key(ConfigId, ConfigMap) of
                true ->
                    NewConfigMap = maps:remove(ConfigId, ConfigMap),
                    NewPushConfigs = case maps:size(NewConfigMap) of
                        0 -> maps:remove(TaskId, State#a2a_server_state.push_configs);
                        _ -> maps:put(TaskId, NewConfigMap, State#a2a_server_state.push_configs)
                    end,
                    {reply, ok, State#a2a_server_state{push_configs = NewPushConfigs}};
                false ->
                    {reply, {error, not_found}, State}
            end
    end;

%% Connection Management
handle_call(get_connections, _From, State) ->
    %% Streams map contains connection info
    Connections = maps:fold(
        fun(StreamId, Pid, Acc) ->
            [#{
                id => StreamId,
                pid => Pid,
                transport => unknown,
                tenant => State#a2a_server_state.tenant,
                connected_at => erlang:system_time(millisecond)
            } | Acc]
        end,
        [],
        State#a2a_server_state.streams
    ),
    {reply, {ok, Connections}, State};

handle_call({close_connection, ConnectionPid}, _From, State) ->
    case find_stream_by_pid(ConnectionPid, State#a2a_server_state.streams) of
        {ok, StreamId} ->
            %% Notify connection to close
            ConnectionPid ! {close, normal},
            NewStreams = maps:remove(StreamId, State#a2a_server_state.streams),
            {reply, ok, State#a2a_server_state{streams = NewStreams}};
        not_found ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #a2a_server_state{}) -> {noreply, #a2a_server_state{}}.
handle_cast({notify_subscribers, TaskId, Event}, State) ->
    Subscribers = maps:get(TaskId, State#a2a_server_state.subscriptions, []),
    lists:foreach(
        fun(Pid) ->
            Pid ! {a2a_task_event, Event}
        end,
        Subscribers
    ),
    {noreply, State};

handle_cast({register_stream, StreamId, Pid}, State) ->
    _ = monitor(process, Pid),
    NewStreams = maps:put(StreamId, Pid, State#a2a_server_state.streams),
    {noreply, State#a2a_server_state{streams = NewStreams}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #a2a_server_state{}) -> {noreply, #a2a_server_state{}}.
handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    %% Clean up subscriptions for dead subscriber
    NewSubscriptions = maps:map(
        fun(_TaskId, Subscribers) ->
            lists:delete(Pid, Subscribers)
        end,
        State#a2a_server_state.subscriptions
    ),

    %% Clean up streams for dead connection
    NewStreams = maps:filter(
        fun(_StreamId, StreamPid) ->
            StreamPid =/= Pid
        end,
        State#a2a_server_state.streams
    ),

    {noreply, State#a2a_server_state{
        subscriptions = NewSubscriptions,
        streams = NewStreams
    }};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #a2a_server_state{}) -> ok.
terminate(Reason, State) ->
    logger:info("A2A server terminating: ~p", [Reason]),

    %% Notify all subscribers of shutdown
    maps:foreach(
        fun(TaskId, Subscribers) ->
            lists:foreach(
                fun(Pid) ->
                    Pid ! {a2a_server_shutdown, TaskId}
                end,
                Subscribers
            )
        end,
        State#a2a_server_state.subscriptions
    ),

    %% Close all streams
    maps:foreach(
        fun(_StreamId, Pid) ->
            Pid ! {close, shutdown}
        end,
        State#a2a_server_state.streams
    ),

    ok.

-spec code_change(term(), #a2a_server_state{}, term()) -> {ok, #a2a_server_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Request Handlers
%%====================================================================

%% @private Handle SendMessage request
-spec handle_send_message(#a2a_send_message_request{}, #a2a_server_state{}) ->
    {ok, #a2a_send_message_response{}, #a2a_server_state{}} | {error, term()}.
handle_send_message(#a2a_send_message_request{message = Message} = Request, State) ->
    %% Validate tenant if multi-tenant
    case validate_tenant(Request#a2a_send_message_request.tenant, State) of
        ok ->
            %% Validate message
            case erlmcp_a2a_protocol:validate_message(Message) of
                ok ->
                    %% Dispatch to skill handler
                    dispatch_message(Request, State);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @private Dispatch message to appropriate skill handler
-spec dispatch_message(#a2a_send_message_request{}, #a2a_server_state{}) ->
    {ok, #a2a_send_message_response{}, #a2a_server_state{}} | {error, term()}.
dispatch_message(#a2a_send_message_request{message = Message} = Request, State) ->
    %% Determine context and task
    ContextId = case Message#a2a_message.context_id of
        undefined -> generate_uuid();
        CId -> CId
    end,

    TaskId = case Message#a2a_message.task_id of
        undefined -> generate_uuid();
        TId -> TId
    end,

    %% Get or create task
    ExistingTask = maps:get(TaskId, State#a2a_server_state.tasks, undefined),

    %% Check if task is in terminal state
    case ExistingTask of
        #a2a_task{status = #a2a_task_status{state = TaskState}} ->
            case erlmcp_a2a_protocol:is_terminal_state(TaskState) of
                true ->
                    {error, {task_already_terminal, TaskId}};
                false ->
                    execute_skill_handler(Request, ContextId, TaskId, ExistingTask, State)
            end;
        undefined ->
            execute_skill_handler(Request, ContextId, TaskId, undefined, State)
    end.

%% @private Execute skill handler for message
-spec execute_skill_handler(#a2a_send_message_request{}, binary(), binary(),
                            #a2a_task{} | undefined, #a2a_server_state{}) ->
    {ok, #a2a_send_message_response{}, #a2a_server_state{}} | {error, term()}.
execute_skill_handler(#a2a_send_message_request{message = Message} = _Request,
                      ContextId, TaskId, ExistingTask, State) ->
    %% Find matching skill handler
    %% For now, use a default handler if no specific skill matches
    Handler = find_skill_handler(Message, State),

    %% Create initial task if needed
    Task = case ExistingTask of
        undefined ->
            #a2a_task{
                id = TaskId,
                context_id = ContextId,
                status = #a2a_task_status{
                    state = submitted,
                    timestamp = iso8601_timestamp()
                },
                history = [Message],
                artifacts = [],
                metadata = #{}
            };
        T ->
            %% Append message to history
            T#a2a_task{
                history = (T#a2a_task.history) ++ [Message]
            }
    end,

    %% Update task to working state
    WorkingTask = Task#a2a_task{
        status = #a2a_task_status{
            state = working,
            timestamp = iso8601_timestamp()
        }
    },

    %% Execute handler
    case Handler(Message, WorkingTask) of
        {ok, ResultTask} ->
            %% Store updated task
            NewTasks = maps:put(TaskId, ResultTask, State#a2a_server_state.tasks),

            %% Update context
            NewContexts = update_context(ContextId, TaskId, State#a2a_server_state.contexts),

            %% Notify subscribers
            notify_task_update(TaskId, ResultTask, State),

            Response = #a2a_send_message_response{task = ResultTask},
            NewState = State#a2a_server_state{
                tasks = NewTasks,
                contexts = NewContexts
            },
            {ok, Response, NewState};
        {error, Reason} ->
            %% Task failed
            FailedTask = WorkingTask#a2a_task{
                status = #a2a_task_status{
                    state = failed,
                    message = #a2a_message{
                        message_id = generate_uuid(),
                        role = agent,
                        parts = [#a2a_part{text = format_error(Reason)}]
                    },
                    timestamp = iso8601_timestamp()
                }
            },
            NewTasks = maps:put(TaskId, FailedTask, State#a2a_server_state.tasks),
            NewContexts = update_context(ContextId, TaskId, State#a2a_server_state.contexts),
            notify_task_update(TaskId, FailedTask, State),
            {error, Reason}
    end.

%% @private Handle GetTask request
-spec handle_get_task(#a2a_get_task_request{}, #a2a_server_state{}) ->
    {ok, #a2a_task{}} | {error, term()}.
handle_get_task(#a2a_get_task_request{id = TaskId} = Request, State) ->
    case validate_tenant(Request#a2a_get_task_request.tenant, State) of
        ok ->
            case maps:get(TaskId, State#a2a_server_state.tasks, undefined) of
                undefined ->
                    {error, task_not_found};
                Task ->
                    %% Apply history length filter if specified
                    FilteredTask = apply_history_filter(Task, Request#a2a_get_task_request.history_length),
                    {ok, FilteredTask}
            end;
        {error, _} = Error ->
            Error
    end.

%% @private Handle ListTasks request
-spec handle_list_tasks(#a2a_list_tasks_request{}, #a2a_server_state{}) ->
    {ok, #a2a_list_tasks_response{}}.
handle_list_tasks(Request, State) ->
    %% Get all tasks
    AllTasks = maps:values(State#a2a_server_state.tasks),

    %% Apply filters
    FilteredTasks = filter_tasks(AllTasks, Request),

    %% Apply pagination
    PageSize = min(
        case Request#a2a_list_tasks_request.page_size of
            undefined -> ?A2A_DEFAULT_PAGE_SIZE;
            PS -> PS
        end,
        ?A2A_MAX_PAGE_SIZE
    ),

    {PagedTasks, NextPageToken} = paginate_tasks(FilteredTasks, PageSize,
                                                  Request#a2a_list_tasks_request.page_token),

    %% Apply history length filter to each task
    HistoryLength = Request#a2a_list_tasks_request.history_length,
    FinalTasks = [apply_history_filter(T, HistoryLength) || T <- PagedTasks],

    Response = #a2a_list_tasks_response{
        tasks = FinalTasks,
        next_page_token = NextPageToken,
        page_size = length(FinalTasks),
        total_size = length(FilteredTasks)
    },
    {ok, Response}.

%% @private Handle CancelTask request
-spec handle_cancel_task(#a2a_cancel_task_request{}, #a2a_server_state{}) ->
    {ok, #a2a_task{}, #a2a_server_state{}} | {error, term()}.
handle_cancel_task(#a2a_cancel_task_request{id = TaskId} = Request, State) ->
    case validate_tenant(Request#a2a_cancel_task_request.tenant, State) of
        ok ->
            case maps:get(TaskId, State#a2a_server_state.tasks, undefined) of
                undefined ->
                    {error, task_not_found};
                #a2a_task{status = #a2a_task_status{state = TaskState}} = Task ->
                    case erlmcp_a2a_protocol:is_terminal_state(TaskState) of
                        true ->
                            {error, task_already_terminal};
                        false ->
                            %% Cancel the task
                            CanceledTask = Task#a2a_task{
                                status = #a2a_task_status{
                                    state = canceled,
                                    timestamp = iso8601_timestamp()
                                }
                            },
                            NewTasks = maps:put(TaskId, CanceledTask, State#a2a_server_state.tasks),
                            notify_task_update(TaskId, CanceledTask, State),
                            {ok, CanceledTask, State#a2a_server_state{tasks = NewTasks}}
                    end
            end;
        {error, _} = Error ->
            Error
    end.

%% @private Handle CreatePushConfig request
-spec handle_create_push_config(#a2a_create_push_config_request{}, #a2a_server_state{}) ->
    {ok, #a2a_task_push_notification_config{}, #a2a_server_state{}} | {error, term()}.
handle_create_push_config(Request, State) ->
    #a2a_create_push_config_request{
        tenant = Tenant,
        task_id = TaskId,
        config_id = ConfigId,
        config = Config
    } = Request,

    case validate_tenant(Tenant, State) of
        ok ->
            case maps:is_key(TaskId, State#a2a_server_state.tasks) of
                true ->
                    PushConfig = #a2a_task_push_notification_config{
                        tenant = Tenant,
                        id = ConfigId,
                        task_id = TaskId,
                        push_notification_config = Config
                    },
                    TaskConfigs = maps:get(TaskId, State#a2a_server_state.push_configs, #{}),
                    NewTaskConfigs = maps:put(ConfigId, PushConfig, TaskConfigs),
                    NewPushConfigs = maps:put(TaskId, NewTaskConfigs, State#a2a_server_state.push_configs),
                    {ok, PushConfig, State#a2a_server_state{push_configs = NewPushConfigs}};
                false ->
                    {error, task_not_found}
            end;
        {error, _} = Error ->
            Error
    end.

%% @private Handle ListPushConfigs request
-spec handle_list_push_configs(#a2a_list_push_configs_request{}, #a2a_server_state{}) ->
    {ok, #a2a_list_push_configs_response{}}.
handle_list_push_configs(#a2a_list_push_configs_request{task_id = TaskId}, State) ->
    Configs = case maps:get(TaskId, State#a2a_server_state.push_configs, undefined) of
        undefined -> [];
        ConfigMap -> maps:values(ConfigMap)
    end,
    {ok, #a2a_list_push_configs_response{
        configs = Configs,
        next_page_token = undefined
    }}.

%%====================================================================
%% Internal Functions - Helpers
%%====================================================================

%% @private Generate a UUID
-spec generate_uuid() -> binary().
generate_uuid() ->
    %% Simple UUID v4 generation
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format(
        "~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
        [A, B, C band 16#0fff, (D band 16#3fff) bor 16#8000, E]
    )).

%% @private Generate ISO 8601 timestamp
-spec iso8601_timestamp() -> binary().
iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format(
        "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Year, Month, Day, Hour, Min, Sec]
    )).

%% @private Validate tenant for multi-tenant isolation
-spec validate_tenant(binary() | undefined, #a2a_server_state{}) -> ok | {error, term()}.
validate_tenant(undefined, #a2a_server_state{tenant = undefined}) ->
    ok;
validate_tenant(RequestTenant, #a2a_server_state{tenant = ServerTenant})
  when RequestTenant =:= ServerTenant ->
    ok;
validate_tenant(undefined, #a2a_server_state{tenant = _}) ->
    %% Allow undefined tenant in request if server has tenant (backward compat)
    ok;
validate_tenant(_, _) ->
    {error, tenant_mismatch}.

%% @private Validate skill registration
-spec validate_skill_registration(binary(), #a2a_server_state{}) -> ok | {error, term()}.
validate_skill_registration(SkillId, #a2a_server_state{agent_card = undefined}) ->
    %% No agent card configured, allow any skill
    logger:debug("Registering skill ~p without agent card validation", [SkillId]),
    ok;
validate_skill_registration(SkillId, #a2a_server_state{agent_card = Card}) ->
    Skills = Card#a2a_agent_card.skills,
    case lists:any(fun(#a2a_agent_skill{id = Id}) -> Id =:= SkillId end, Skills) of
        true -> ok;
        false -> {error, skill_not_in_agent_card}
    end.

%% @private Find skill handler for message
-spec find_skill_handler(#a2a_message{}, #a2a_server_state{}) -> skill_handler().
find_skill_handler(_Message, #a2a_server_state{skill_handlers = Handlers}) when map_size(Handlers) =:= 0 ->
    %% Return default handler
    default_skill_handler();
find_skill_handler(_Message, #a2a_server_state{skill_handlers = Handlers}) ->
    %% For now, return the first registered handler
    %% In production, would implement proper skill matching
    case maps:to_list(Handlers) of
        [{_SkillId, Handler} | _] -> Handler;
        [] -> default_skill_handler()
    end.

%% @private Default skill handler
-spec default_skill_handler() -> skill_handler().
default_skill_handler() ->
    fun(_Message, Task) ->
        %% Simple echo handler that completes the task
        CompletedTask = Task#a2a_task{
            status = #a2a_task_status{
                state = completed,
                message = #a2a_message{
                    message_id = generate_uuid(),
                    role = agent,
                    parts = [#a2a_part{text = <<"Task completed successfully.">>}]
                },
                timestamp = iso8601_timestamp()
            }
        },
        {ok, CompletedTask}
    end.

%% @private Update context with task
-spec update_context(binary(), binary(), #{binary() => [binary()]}) -> #{binary() => [binary()]}.
update_context(ContextId, TaskId, Contexts) ->
    TaskIds = maps:get(ContextId, Contexts, []),
    case lists:member(TaskId, TaskIds) of
        true -> Contexts;
        false -> maps:put(ContextId, [TaskId | TaskIds], Contexts)
    end.

%% @private Notify subscribers of task update
-spec notify_task_update(binary(), #a2a_task{}, #a2a_server_state{}) -> ok.
notify_task_update(TaskId, Task, State) ->
    Event = #a2a_task_status_update_event{
        task_id = TaskId,
        context_id = Task#a2a_task.context_id,
        status = Task#a2a_task.status
    },
    gen_server:cast(self(), {notify_subscribers, TaskId, Event}),
    ok.

%% @private Filter tasks based on request parameters
-spec filter_tasks([#a2a_task{}], #a2a_list_tasks_request{}) -> [#a2a_task{}].
filter_tasks(Tasks, Request) ->
    lists:filter(
        fun(Task) ->
            filter_by_context(Task, Request#a2a_list_tasks_request.context_id) andalso
            filter_by_status(Task, Request#a2a_list_tasks_request.status) andalso
            filter_by_timestamp(Task, Request#a2a_list_tasks_request.status_timestamp_after)
        end,
        Tasks
    ).

-spec filter_by_context(#a2a_task{}, binary() | undefined) -> boolean().
filter_by_context(_, undefined) -> true;
filter_by_context(#a2a_task{context_id = ContextId}, FilterContextId) ->
    ContextId =:= FilterContextId.

-spec filter_by_status(#a2a_task{}, a2a_task_state() | undefined) -> boolean().
filter_by_status(_, undefined) -> true;
filter_by_status(#a2a_task{status = #a2a_task_status{state = State}}, FilterState) ->
    State =:= FilterState.

-spec filter_by_timestamp(#a2a_task{}, binary() | undefined) -> boolean().
filter_by_timestamp(_, undefined) -> true;
filter_by_timestamp(#a2a_task{status = #a2a_task_status{timestamp = Timestamp}}, FilterTimestamp)
  when Timestamp =/= undefined ->
    Timestamp > FilterTimestamp;
filter_by_timestamp(_, _) -> true.

%% @private Paginate tasks
-spec paginate_tasks([#a2a_task{}], integer(), binary() | undefined) ->
    {[#a2a_task{}], binary()}.
paginate_tasks(Tasks, PageSize, undefined) ->
    case length(Tasks) > PageSize of
        true ->
            {lists:sublist(Tasks, PageSize), encode_page_token(PageSize)};
        false ->
            {Tasks, <<>>}
    end;
paginate_tasks(Tasks, PageSize, PageToken) ->
    Offset = decode_page_token(PageToken),
    RemainingTasks = lists:nthtail(min(Offset, length(Tasks)), Tasks),
    case length(RemainingTasks) > PageSize of
        true ->
            {lists:sublist(RemainingTasks, PageSize), encode_page_token(Offset + PageSize)};
        false ->
            {RemainingTasks, <<>>}
    end.

-spec encode_page_token(integer()) -> binary().
encode_page_token(Offset) ->
    base64:encode(integer_to_binary(Offset)).

-spec decode_page_token(binary()) -> integer().
decode_page_token(Token) ->
    try
        binary_to_integer(base64:decode(Token))
    catch
        _:_ -> 0
    end.

%% @private Apply history length filter to task
-spec apply_history_filter(#a2a_task{}, integer() | undefined) -> #a2a_task{}.
apply_history_filter(Task, undefined) ->
    Task;
apply_history_filter(#a2a_task{history = undefined} = Task, _) ->
    Task;
apply_history_filter(#a2a_task{history = History} = Task, Length) when Length >= 0 ->
    TrimmedHistory = lists:sublist(lists:reverse(History), Length),
    Task#a2a_task{history = lists:reverse(TrimmedHistory)};
apply_history_filter(Task, _) ->
    Task.

%% @private Find stream by pid
-spec find_stream_by_pid(pid(), #{binary() => pid()}) -> {ok, binary()} | not_found.
find_stream_by_pid(Pid, Streams) ->
    case maps:fold(
        fun(StreamId, StreamPid, Acc) ->
            case StreamPid =:= Pid of
                true -> {ok, StreamId};
                false -> Acc
            end
        end,
        not_found,
        Streams
    ) of
        {ok, _} = Result -> Result;
        not_found -> not_found
    end.

%% @private Format error for message
-spec format_error(term()) -> binary().
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason);
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).
