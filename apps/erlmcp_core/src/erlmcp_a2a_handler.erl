%%%-------------------------------------------------------------------
%%% @doc A2A Message Handler Module
%%%
%%% This module handles incoming A2A (Agent-to-Agent) requests via JSON-RPC.
%%% It provides routing, validation, and response formatting for all A2A
%%% protocol methods as defined in the Google A2A specification.
%%%
%%% Responsibilities:
%%% - Request routing based on method name
%%% - Request validation against A2A schemas
%%% - Error handling with proper A2A error codes
%%% - Response formatting
%%% - Integration with task manager and agent card modules
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_a2a_handler).

-include("erlmcp.hrl").
-include("erlmcp_a2a.hrl").

%% API exports
-export([
    handle_request/2,
    handle_request/3
]).

%% Method handlers
-export([
    handle_send_message/2,
    handle_send_streaming_message/2,
    handle_get_task/2,
    handle_list_tasks/2,
    handle_cancel_task/2,
    handle_subscribe_to_task/2,
    handle_create_push_notification_config/2,
    handle_get_push_notification_config/2,
    handle_list_push_notification_configs/2,
    handle_delete_push_notification_config/2,
    handle_get_extended_agent_card/2
]).

%% Types
-type request_id() :: binary() | integer() | null.
-type handler_context() :: #{
    tenant => binary() | undefined,
    caller_pid => pid() | undefined,
    request_id => request_id(),
    timestamp => integer()
}.
-type handler_result() :: {ok, map()} | {error, {integer(), binary()}} | {error, {integer(), binary(), map()}}.

-export_type([handler_context/0, handler_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Handle an incoming A2A request.
%% Takes a JSON-RPC method name and params, returns the appropriate response.
-spec handle_request(binary(), map() | undefined) -> handler_result().
handle_request(Method, Params) ->
    handle_request(Method, Params, #{}).

%% @doc Handle an incoming A2A request with context.
%% Context can include tenant info, caller PID, request ID, etc.
-spec handle_request(binary(), map() | undefined, handler_context()) -> handler_result().
handle_request(Method, Params, Context) ->
    %% Normalize params to empty map if undefined
    NormalizedParams = case Params of
        undefined -> #{};
        _ when is_map(Params) -> Params;
        _ -> #{}
    end,

    %% Add timestamp to context if not present
    ContextWithTime = case maps:is_key(timestamp, Context) of
        true -> Context;
        false -> Context#{timestamp => erlang:system_time(millisecond)}
    end,

    %% Route to appropriate handler
    try
        route_request(Method, NormalizedParams, ContextWithTime)
    catch
        error:badarg ->
            {error, {?JSONRPC_INVALID_PARAMS, ?JSONRPC_MSG_INVALID_PARAMS}};
        error:{badmatch, _} ->
            {error, {?A2A_ERROR_MESSAGE_INVALID, ?A2A_MSG_MESSAGE_INVALID}};
        _Class:Reason:_Stacktrace ->
            logger:error("A2A handler error: ~p", [Reason]),
            {error, {?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR}}
    end.

%%====================================================================
%% Request Routing
%%====================================================================

%% @private Route request to appropriate handler based on method name
-spec route_request(binary(), map(), handler_context()) -> handler_result().
route_request(?A2A_METHOD_SEND_MESSAGE, Params, Context) ->
    handle_send_message(Params, Context);

route_request(?A2A_METHOD_SEND_STREAMING_MESSAGE, Params, Context) ->
    handle_send_streaming_message(Params, Context);

route_request(?A2A_METHOD_GET_TASK, Params, Context) ->
    handle_get_task(Params, Context);

route_request(?A2A_METHOD_LIST_TASKS, Params, Context) ->
    handle_list_tasks(Params, Context);

route_request(?A2A_METHOD_CANCEL_TASK, Params, Context) ->
    handle_cancel_task(Params, Context);

route_request(?A2A_METHOD_SUBSCRIBE_TO_TASK, Params, Context) ->
    handle_subscribe_to_task(Params, Context);

route_request(?A2A_METHOD_CREATE_PUSH_CONFIG, Params, Context) ->
    handle_create_push_notification_config(Params, Context);

route_request(?A2A_METHOD_GET_PUSH_CONFIG, Params, Context) ->
    handle_get_push_notification_config(Params, Context);

route_request(?A2A_METHOD_LIST_PUSH_CONFIGS, Params, Context) ->
    handle_list_push_notification_configs(Params, Context);

route_request(?A2A_METHOD_DELETE_PUSH_CONFIG, Params, Context) ->
    handle_delete_push_notification_config(Params, Context);

route_request(?A2A_METHOD_GET_EXTENDED_AGENT_CARD, Params, Context) ->
    handle_get_extended_agent_card(Params, Context);

route_request(Method, _Params, _Context) ->
    logger:warning("Unknown A2A method: ~p", [Method]),
    {error, {?JSONRPC_METHOD_NOT_FOUND, ?JSONRPC_MSG_METHOD_NOT_FOUND}}.

%%====================================================================
%% Method Handlers
%%====================================================================

%% @doc Handle a2a/sendMessage request
%% Sends a message to an agent and receives a response (blocking or non-blocking)
-spec handle_send_message(map(), handler_context()) -> handler_result().
handle_send_message(Params, Context) ->
    case validate_send_message_params(Params) of
        {ok, Request} ->
            execute_send_message(Request, Context);
        {error, Reason} ->
            {error, {?A2A_ERROR_MESSAGE_INVALID, format_validation_error(Reason)}}
    end.

%% @doc Handle a2a/sendStreamingMessage request
%% Sends a message and receives streaming response via SSE
-spec handle_send_streaming_message(map(), handler_context()) -> handler_result().
handle_send_streaming_message(Params, Context) ->
    %% Validate streaming is supported
    case check_streaming_capability() of
        false ->
            {error, {?A2A_ERROR_STREAMING_NOT_SUPPORTED,
                     <<"Streaming is not supported by this agent">>}};
        true ->
            case validate_send_message_params(Params) of
                {ok, Request} ->
                    execute_streaming_message(Request, Context);
                {error, Reason} ->
                    {error, {?A2A_ERROR_MESSAGE_INVALID, format_validation_error(Reason)}}
            end
    end.

%% @doc Handle a2a/getTask request
%% Retrieves a task by ID with optional history
-spec handle_get_task(map(), handler_context()) -> handler_result().
handle_get_task(Params, Context) ->
    case validate_get_task_params(Params) of
        {ok, Request} ->
            execute_get_task(Request, Context);
        {error, Reason} ->
            {error, {?JSONRPC_INVALID_PARAMS, format_validation_error(Reason)}}
    end.

%% @doc Handle a2a/listTasks request
%% Lists tasks with optional filtering and pagination
-spec handle_list_tasks(map(), handler_context()) -> handler_result().
handle_list_tasks(Params, Context) ->
    case validate_list_tasks_params(Params) of
        {ok, Request} ->
            execute_list_tasks(Request, Context);
        {error, Reason} ->
            {error, {?JSONRPC_INVALID_PARAMS, format_validation_error(Reason)}}
    end.

%% @doc Handle a2a/cancelTask request
%% Cancels a task if it's in a cancelable state
-spec handle_cancel_task(map(), handler_context()) -> handler_result().
handle_cancel_task(Params, Context) ->
    case validate_cancel_task_params(Params) of
        {ok, Request} ->
            execute_cancel_task(Request, Context);
        {error, Reason} ->
            {error, {?JSONRPC_INVALID_PARAMS, format_validation_error(Reason)}}
    end.

%% @doc Handle a2a/subscribeToTask request
%% Subscribes to task updates (for SSE streaming)
-spec handle_subscribe_to_task(map(), handler_context()) -> handler_result().
handle_subscribe_to_task(Params, Context) ->
    case check_streaming_capability() of
        false ->
            {error, {?A2A_ERROR_STREAMING_NOT_SUPPORTED,
                     <<"Streaming subscriptions are not supported by this agent">>}};
        true ->
            case validate_subscribe_params(Params) of
                {ok, Request} ->
                    execute_subscribe_to_task(Request, Context);
                {error, Reason} ->
                    {error, {?JSONRPC_INVALID_PARAMS, format_validation_error(Reason)}}
            end
    end.

%% @doc Handle a2a/createTaskPushNotificationConfig request
%% Creates a push notification configuration for a task
-spec handle_create_push_notification_config(map(), handler_context()) -> handler_result().
handle_create_push_notification_config(Params, Context) ->
    case check_push_notification_capability() of
        false ->
            {error, {?A2A_ERROR_PUSH_DISABLED,
                     <<"Push notifications are not supported by this agent">>}};
        true ->
            case validate_create_push_config_params(Params) of
                {ok, Request} ->
                    execute_create_push_config(Request, Context);
                {error, Reason} ->
                    {error, {?A2A_ERROR_PUSH_CONFIG_INVALID, format_validation_error(Reason)}}
            end
    end.

%% @doc Handle a2a/getTaskPushNotificationConfig request
%% Retrieves a push notification configuration
-spec handle_get_push_notification_config(map(), handler_context()) -> handler_result().
handle_get_push_notification_config(Params, Context) ->
    case validate_get_push_config_params(Params) of
        {ok, Request} ->
            execute_get_push_config(Request, Context);
        {error, Reason} ->
            {error, {?JSONRPC_INVALID_PARAMS, format_validation_error(Reason)}}
    end.

%% @doc Handle a2a/listTaskPushNotificationConfigs request
%% Lists push notification configurations for a task
-spec handle_list_push_notification_configs(map(), handler_context()) -> handler_result().
handle_list_push_notification_configs(Params, Context) ->
    case validate_list_push_configs_params(Params) of
        {ok, Request} ->
            execute_list_push_configs(Request, Context);
        {error, Reason} ->
            {error, {?JSONRPC_INVALID_PARAMS, format_validation_error(Reason)}}
    end.

%% @doc Handle a2a/deleteTaskPushNotificationConfig request
%% Deletes a push notification configuration
-spec handle_delete_push_notification_config(map(), handler_context()) -> handler_result().
handle_delete_push_notification_config(Params, Context) ->
    case validate_delete_push_config_params(Params) of
        {ok, Request} ->
            execute_delete_push_config(Request, Context);
        {error, Reason} ->
            {error, {?JSONRPC_INVALID_PARAMS, format_validation_error(Reason)}}
    end.

%% @doc Handle a2a/getExtendedAgentCard request
%% Retrieves the extended agent card with additional capabilities
-spec handle_get_extended_agent_card(map(), handler_context()) -> handler_result().
handle_get_extended_agent_card(Params, Context) ->
    case check_extended_card_capability() of
        false ->
            {error, {?A2A_ERROR_EXTENDED_CARD_NOT_AVAILABLE,
                     <<"Extended agent card is not available">>}};
        true ->
            execute_get_extended_agent_card(Params, Context)
    end.

%%====================================================================
%% Validation Functions
%%====================================================================

%% @private Validate send message request parameters
-spec validate_send_message_params(map()) -> {ok, #a2a_send_message_request{}} | {error, term()}.
validate_send_message_params(Params) ->
    case erlmcp_a2a_protocol:decode_send_message_request(Params) of
        {ok, Request} ->
            %% Additional validation
            case erlmcp_a2a_protocol:validate_message(Request#a2a_send_message_request.message) of
                ok -> {ok, Request};
                Error -> Error
            end;
        Error -> Error
    end.

%% @private Validate get task request parameters
-spec validate_get_task_params(map()) -> {ok, #a2a_get_task_request{}} | {error, term()}.
validate_get_task_params(Params) ->
    case maps:get(<<"id">>, Params, undefined) of
        undefined ->
            {error, missing_task_id};
        Id when is_binary(Id) ->
            erlmcp_a2a_protocol:decode_get_task_request(Params);
        _ ->
            {error, invalid_task_id}
    end.

%% @private Validate list tasks request parameters
-spec validate_list_tasks_params(map()) -> {ok, #a2a_list_tasks_request{}} | {error, term()}.
validate_list_tasks_params(Params) ->
    %% Validate pagination parameters
    PageSize = maps:get(<<"pageSize">>, Params, ?A2A_DEFAULT_PAGE_SIZE),
    case validate_page_size(PageSize) of
        ok ->
            erlmcp_a2a_protocol:decode_list_tasks_request(Params);
        Error ->
            Error
    end.

%% @private Validate cancel task request parameters
-spec validate_cancel_task_params(map()) -> {ok, #a2a_cancel_task_request{}} | {error, term()}.
validate_cancel_task_params(Params) ->
    case maps:get(<<"id">>, Params, undefined) of
        undefined ->
            {error, missing_task_id};
        Id when is_binary(Id) ->
            erlmcp_a2a_protocol:decode_cancel_task_request(Params);
        _ ->
            {error, invalid_task_id}
    end.

%% @private Validate subscribe to task parameters
-spec validate_subscribe_params(map()) -> {ok, #a2a_subscribe_to_task_request{}} | {error, term()}.
validate_subscribe_params(Params) ->
    case maps:get(<<"id">>, Params, undefined) of
        undefined ->
            {error, missing_task_id};
        Id when is_binary(Id) ->
            {ok, #a2a_subscribe_to_task_request{
                tenant = maps:get(<<"tenant">>, Params, undefined),
                id = Id
            }};
        _ ->
            {error, invalid_task_id}
    end.

%% @private Validate create push config parameters
-spec validate_create_push_config_params(map()) -> {ok, #a2a_create_push_config_request{}} | {error, term()}.
validate_create_push_config_params(Params) ->
    TaskId = maps:get(<<"taskId">>, Params, undefined),
    ConfigId = maps:get(<<"configId">>, Params, undefined),
    Config = maps:get(<<"config">>, Params, undefined),

    case {TaskId, ConfigId, Config} of
        {undefined, _, _} ->
            {error, missing_task_id};
        {_, undefined, _} ->
            {error, missing_config_id};
        {_, _, undefined} ->
            {error, missing_config};
        {T, C, Cfg} when is_binary(T), is_binary(C), is_map(Cfg) ->
            case erlmcp_a2a_protocol:decode_push_notification_config(Cfg) of
                {ok, PushConfig} ->
                    {ok, #a2a_create_push_config_request{
                        tenant = maps:get(<<"tenant">>, Params, undefined),
                        task_id = T,
                        config_id = C,
                        config = PushConfig
                    }};
                Error -> Error
            end;
        _ ->
            {error, invalid_params}
    end.

%% @private Validate get push config parameters
-spec validate_get_push_config_params(map()) -> {ok, #a2a_get_push_config_request{}} | {error, term()}.
validate_get_push_config_params(Params) ->
    TaskId = maps:get(<<"taskId">>, Params, undefined),
    Id = maps:get(<<"id">>, Params, undefined),

    case {TaskId, Id} of
        {undefined, _} ->
            {error, missing_task_id};
        {_, undefined} ->
            {error, missing_config_id};
        {T, I} when is_binary(T), is_binary(I) ->
            {ok, #a2a_get_push_config_request{
                tenant = maps:get(<<"tenant">>, Params, undefined),
                task_id = T,
                id = I
            }};
        _ ->
            {error, invalid_params}
    end.

%% @private Validate list push configs parameters
-spec validate_list_push_configs_params(map()) -> {ok, #a2a_list_push_configs_request{}} | {error, term()}.
validate_list_push_configs_params(Params) ->
    case maps:get(<<"taskId">>, Params, undefined) of
        undefined ->
            {error, missing_task_id};
        TaskId when is_binary(TaskId) ->
            {ok, #a2a_list_push_configs_request{
                tenant = maps:get(<<"tenant">>, Params, undefined),
                task_id = TaskId,
                page_size = maps:get(<<"pageSize">>, Params, undefined),
                page_token = maps:get(<<"pageToken">>, Params, undefined)
            }};
        _ ->
            {error, invalid_task_id}
    end.

%% @private Validate delete push config parameters
-spec validate_delete_push_config_params(map()) -> {ok, #a2a_delete_push_config_request{}} | {error, term()}.
validate_delete_push_config_params(Params) ->
    TaskId = maps:get(<<"taskId">>, Params, undefined),
    Id = maps:get(<<"id">>, Params, undefined),

    case {TaskId, Id} of
        {undefined, _} ->
            {error, missing_task_id};
        {_, undefined} ->
            {error, missing_config_id};
        {T, I} when is_binary(T), is_binary(I) ->
            {ok, #a2a_delete_push_config_request{
                tenant = maps:get(<<"tenant">>, Params, undefined),
                task_id = T,
                id = I
            }};
        _ ->
            {error, invalid_params}
    end.

%% @private Validate page size
-spec validate_page_size(term()) -> ok | {error, term()}.
validate_page_size(Size) when is_integer(Size), Size >= ?A2A_MIN_PAGE_SIZE, Size =< ?A2A_MAX_PAGE_SIZE ->
    ok;
validate_page_size(Size) when is_integer(Size), Size < ?A2A_MIN_PAGE_SIZE ->
    {error, {page_size_invalid, <<"Page size too small">>}};
validate_page_size(Size) when is_integer(Size), Size > ?A2A_MAX_PAGE_SIZE ->
    {error, {page_size_too_large, <<"Page size exceeds maximum">>}};
validate_page_size(_) ->
    {error, {page_size_invalid, <<"Invalid page size type">>}}.

%%====================================================================
%% Execution Functions
%%====================================================================

%% @private Execute send message request
-spec execute_send_message(#a2a_send_message_request{}, handler_context()) -> handler_result().
execute_send_message(Request, Context) ->
    %% Extract tenant from request or context
    Tenant = case Request#a2a_send_message_request.tenant of
        undefined -> maps:get(tenant, Context, undefined);
        T -> T
    end,

    %% Delegate to task manager
    case call_task_manager(send_message, Request, Tenant) of
        {ok, Task} when is_record(Task, a2a_task) ->
            {ok, erlmcp_a2a_protocol:encode_task(Task)};
        {ok, Message} when is_record(Message, a2a_message) ->
            {ok, #{<<"message">> => erlmcp_a2a_protocol:encode_message(Message)}};
        {error, task_not_found} ->
            {error, {?A2A_ERROR_TASK_NOT_FOUND, ?A2A_MSG_TASK_NOT_FOUND}};
        {error, {state_transition_invalid, From, To}} ->
            {error, {?A2A_ERROR_TASK_STATE_TRANSITION_INVALID,
                     iolist_to_binary([<<"Cannot transition from ">>,
                                       erlmcp_a2a_protocol:task_state_to_binary(From),
                                       <<" to ">>,
                                       erlmcp_a2a_protocol:task_state_to_binary(To)])}};
        {error, Reason} ->
            {error, {?JSONRPC_INTERNAL_ERROR, format_error_reason(Reason)}}
    end.

%% @private Execute streaming message request
-spec execute_streaming_message(#a2a_send_message_request{}, handler_context()) -> handler_result().
execute_streaming_message(Request, Context) ->
    Tenant = case Request#a2a_send_message_request.tenant of
        undefined -> maps:get(tenant, Context, undefined);
        T -> T
    end,

    %% Delegate to task manager for streaming
    case call_task_manager(send_streaming_message, Request, Tenant) of
        {ok, StreamId} when is_binary(StreamId) ->
            %% Return stream ID for SSE connection
            {ok, #{<<"streamId">> => StreamId}};
        {ok, Task} when is_record(Task, a2a_task) ->
            {ok, erlmcp_a2a_protocol:encode_task(Task)};
        {error, Reason} ->
            {error, {?A2A_ERROR_STREAM_ERROR, format_error_reason(Reason)}}
    end.

%% @private Execute get task request
-spec execute_get_task(#a2a_get_task_request{}, handler_context()) -> handler_result().
execute_get_task(Request, Context) ->
    Tenant = case Request#a2a_get_task_request.tenant of
        undefined -> maps:get(tenant, Context, undefined);
        T -> T
    end,

    case call_task_manager(get_task, Request#a2a_get_task_request.id, Tenant) of
        {ok, Task} ->
            %% Apply history length limit if specified
            LimitedTask = case Request#a2a_get_task_request.history_length of
                undefined -> Task;
                Limit -> limit_task_history(Task, Limit)
            end,
            {ok, erlmcp_a2a_protocol:encode_task(LimitedTask)};
        {error, not_found} ->
            {error, {?A2A_ERROR_TASK_NOT_FOUND, ?A2A_MSG_TASK_NOT_FOUND}};
        {error, Reason} ->
            {error, {?JSONRPC_INTERNAL_ERROR, format_error_reason(Reason)}}
    end.

%% @private Execute list tasks request
-spec execute_list_tasks(#a2a_list_tasks_request{}, handler_context()) -> handler_result().
execute_list_tasks(Request, Context) ->
    Tenant = case Request#a2a_list_tasks_request.tenant of
        undefined -> maps:get(tenant, Context, undefined);
        T -> T
    end,

    case call_task_manager(list_tasks, Request, Tenant) of
        {ok, Response} when is_record(Response, a2a_list_tasks_response) ->
            {ok, erlmcp_a2a_protocol:encode_list_tasks_response(Response)};
        {ok, Tasks} when is_list(Tasks) ->
            %% Convert list to response format
            Response = #a2a_list_tasks_response{
                tasks = Tasks,
                next_page_token = <<>>,
                page_size = length(Tasks),
                total_size = length(Tasks)
            },
            {ok, erlmcp_a2a_protocol:encode_list_tasks_response(Response)};
        {error, invalid_page_token} ->
            {error, {?A2A_ERROR_PAGE_TOKEN_INVALID, <<"Invalid page token">>}};
        {error, Reason} ->
            {error, {?JSONRPC_INTERNAL_ERROR, format_error_reason(Reason)}}
    end.

%% @private Execute cancel task request
-spec execute_cancel_task(#a2a_cancel_task_request{}, handler_context()) -> handler_result().
execute_cancel_task(Request, Context) ->
    Tenant = case Request#a2a_cancel_task_request.tenant of
        undefined -> maps:get(tenant, Context, undefined);
        T -> T
    end,

    case call_task_manager(cancel_task, Request#a2a_cancel_task_request.id, Tenant) of
        {ok, Task} ->
            {ok, erlmcp_a2a_protocol:encode_task(Task)};
        {error, not_found} ->
            {error, {?A2A_ERROR_TASK_NOT_FOUND, ?A2A_MSG_TASK_NOT_FOUND}};
        {error, already_terminal} ->
            {error, {?A2A_ERROR_TASK_ALREADY_TERMINAL, ?A2A_MSG_TASK_ALREADY_TERMINAL}};
        {error, not_cancelable} ->
            {error, {?A2A_ERROR_TASK_NOT_CANCELABLE, ?A2A_MSG_TASK_NOT_CANCELABLE}};
        {error, Reason} ->
            {error, {?JSONRPC_INTERNAL_ERROR, format_error_reason(Reason)}}
    end.

%% @private Execute subscribe to task request
-spec execute_subscribe_to_task(#a2a_subscribe_to_task_request{}, handler_context()) -> handler_result().
execute_subscribe_to_task(Request, Context) ->
    Tenant = case Request#a2a_subscribe_to_task_request.tenant of
        undefined -> maps:get(tenant, Context, undefined);
        T -> T
    end,
    CallerPid = maps:get(caller_pid, Context, self()),

    case call_task_manager(subscribe, {Request#a2a_subscribe_to_task_request.id, CallerPid}, Tenant) of
        {ok, SubscriptionId} ->
            {ok, #{<<"subscriptionId">> => SubscriptionId}};
        {error, not_found} ->
            {error, {?A2A_ERROR_TASK_NOT_FOUND, ?A2A_MSG_TASK_NOT_FOUND}};
        {error, Reason} ->
            {error, {?A2A_ERROR_SUBSCRIBE_FAILED, format_error_reason(Reason)}}
    end.

%% @private Execute create push config request
-spec execute_create_push_config(#a2a_create_push_config_request{}, handler_context()) -> handler_result().
execute_create_push_config(Request, Context) ->
    Tenant = case Request#a2a_create_push_config_request.tenant of
        undefined -> maps:get(tenant, Context, undefined);
        T -> T
    end,

    case call_task_manager(create_push_config, Request, Tenant) of
        {ok, Config} when is_record(Config, a2a_task_push_notification_config) ->
            {ok, #{
                <<"id">> => Config#a2a_task_push_notification_config.id,
                <<"taskId">> => Config#a2a_task_push_notification_config.task_id,
                <<"pushNotificationConfig">> =>
                    erlmcp_a2a_protocol:encode_push_notification_config(
                        Config#a2a_task_push_notification_config.push_notification_config)
            }};
        {error, task_not_found} ->
            {error, {?A2A_ERROR_TASK_NOT_FOUND, ?A2A_MSG_TASK_NOT_FOUND}};
        {error, invalid_url} ->
            {error, {?A2A_ERROR_PUSH_URL_INVALID, <<"Invalid push notification URL">>}};
        {error, Reason} ->
            {error, {?JSONRPC_INTERNAL_ERROR, format_error_reason(Reason)}}
    end.

%% @private Execute get push config request
-spec execute_get_push_config(#a2a_get_push_config_request{}, handler_context()) -> handler_result().
execute_get_push_config(Request, Context) ->
    Tenant = case Request#a2a_get_push_config_request.tenant of
        undefined -> maps:get(tenant, Context, undefined);
        T -> T
    end,

    case call_task_manager(get_push_config, {Request#a2a_get_push_config_request.task_id,
                                              Request#a2a_get_push_config_request.id}, Tenant) of
        {ok, Config} when is_record(Config, a2a_task_push_notification_config) ->
            {ok, #{
                <<"id">> => Config#a2a_task_push_notification_config.id,
                <<"taskId">> => Config#a2a_task_push_notification_config.task_id,
                <<"pushNotificationConfig">> =>
                    erlmcp_a2a_protocol:encode_push_notification_config(
                        Config#a2a_task_push_notification_config.push_notification_config)
            }};
        {error, not_found} ->
            {error, {?A2A_ERROR_PUSH_CONFIG_NOT_FOUND, <<"Push notification config not found">>}};
        {error, Reason} ->
            {error, {?JSONRPC_INTERNAL_ERROR, format_error_reason(Reason)}}
    end.

%% @private Execute list push configs request
-spec execute_list_push_configs(#a2a_list_push_configs_request{}, handler_context()) -> handler_result().
execute_list_push_configs(Request, Context) ->
    Tenant = case Request#a2a_list_push_configs_request.tenant of
        undefined -> maps:get(tenant, Context, undefined);
        T -> T
    end,

    case call_task_manager(list_push_configs, Request, Tenant) of
        {ok, Response} when is_record(Response, a2a_list_push_configs_response) ->
            Configs = [#{
                <<"id">> => C#a2a_task_push_notification_config.id,
                <<"taskId">> => C#a2a_task_push_notification_config.task_id,
                <<"pushNotificationConfig">> =>
                    erlmcp_a2a_protocol:encode_push_notification_config(
                        C#a2a_task_push_notification_config.push_notification_config)
            } || C <- Response#a2a_list_push_configs_response.configs],
            {ok, #{
                <<"configs">> => Configs,
                <<"nextPageToken">> => Response#a2a_list_push_configs_response.next_page_token
            }};
        {ok, Configs} when is_list(Configs) ->
            ConfigMaps = [#{
                <<"id">> => C#a2a_task_push_notification_config.id,
                <<"taskId">> => C#a2a_task_push_notification_config.task_id,
                <<"pushNotificationConfig">> =>
                    erlmcp_a2a_protocol:encode_push_notification_config(
                        C#a2a_task_push_notification_config.push_notification_config)
            } || C <- Configs],
            {ok, #{<<"configs">> => ConfigMaps}};
        {error, task_not_found} ->
            {error, {?A2A_ERROR_TASK_NOT_FOUND, ?A2A_MSG_TASK_NOT_FOUND}};
        {error, Reason} ->
            {error, {?JSONRPC_INTERNAL_ERROR, format_error_reason(Reason)}}
    end.

%% @private Execute delete push config request
-spec execute_delete_push_config(#a2a_delete_push_config_request{}, handler_context()) -> handler_result().
execute_delete_push_config(Request, Context) ->
    Tenant = case Request#a2a_delete_push_config_request.tenant of
        undefined -> maps:get(tenant, Context, undefined);
        T -> T
    end,

    case call_task_manager(delete_push_config, {Request#a2a_delete_push_config_request.task_id,
                                                  Request#a2a_delete_push_config_request.id}, Tenant) of
        ok ->
            {ok, #{<<"deleted">> => true}};
        {error, not_found} ->
            {error, {?A2A_ERROR_PUSH_CONFIG_NOT_FOUND, <<"Push notification config not found">>}};
        {error, Reason} ->
            {error, {?JSONRPC_INTERNAL_ERROR, format_error_reason(Reason)}}
    end.

%% @private Execute get extended agent card request
-spec execute_get_extended_agent_card(map(), handler_context()) -> handler_result().
execute_get_extended_agent_card(Params, Context) ->
    Tenant = case maps:get(<<"tenant">>, Params, undefined) of
        undefined -> maps:get(tenant, Context, undefined);
        T -> T
    end,

    case call_agent_card(get_extended, Tenant) of
        {ok, Card} when is_record(Card, a2a_agent_card) ->
            {ok, erlmcp_a2a_protocol:encode_agent_card(Card)};
        {error, not_available} ->
            {error, {?A2A_ERROR_EXTENDED_CARD_NOT_AVAILABLE,
                     <<"Extended agent card is not available">>}};
        {error, Reason} ->
            {error, {?JSONRPC_INTERNAL_ERROR, format_error_reason(Reason)}}
    end.

%%====================================================================
%% Capability Check Functions
%%====================================================================

%% @private Check if streaming is supported
-spec check_streaming_capability() -> boolean().
check_streaming_capability() ->
    case call_agent_card(get_capabilities, undefined) of
        {ok, #a2a_agent_capabilities{streaming = true}} -> true;
        _ -> false
    end.

%% @private Check if push notifications are supported
-spec check_push_notification_capability() -> boolean().
check_push_notification_capability() ->
    case call_agent_card(get_capabilities, undefined) of
        {ok, #a2a_agent_capabilities{push_notifications = true}} -> true;
        _ -> false
    end.

%% @private Check if extended agent card is available
-spec check_extended_card_capability() -> boolean().
check_extended_card_capability() ->
    case call_agent_card(get_capabilities, undefined) of
        {ok, #a2a_agent_capabilities{extended_agent_card = true}} -> true;
        _ -> false
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Call task manager with operation
-spec call_task_manager(atom(), term(), binary() | undefined) -> term().
call_task_manager(Op, Args, Tenant) ->
    %% Try to call erlmcp_a2a_task_manager if available
    case erlang:function_exported(erlmcp_a2a_task_manager, Op, 2) of
        true ->
            erlmcp_a2a_task_manager:Op(Args, Tenant);
        false ->
            %% Fallback for when task manager is not available
            logger:warning("A2A task manager not available for operation: ~p", [Op]),
            {error, not_implemented}
    end.

%% @private Call agent card module
-spec call_agent_card(atom(), binary() | undefined) -> term().
call_agent_card(Op, Tenant) ->
    case erlang:function_exported(erlmcp_a2a_agent_card, Op, 1) of
        true ->
            erlmcp_a2a_agent_card:Op(Tenant);
        false ->
            %% Return default capabilities when agent card module not available
            case Op of
                get_capabilities ->
                    {ok, #a2a_agent_capabilities{
                        streaming = false,
                        push_notifications = false,
                        extended_agent_card = false
                    }};
                _ ->
                    {error, not_implemented}
            end
    end.

%% @private Limit task history to specified length
-spec limit_task_history(#a2a_task{}, integer()) -> #a2a_task{}.
limit_task_history(Task, Limit) when Limit =< 0 ->
    Task#a2a_task{history = undefined};
limit_task_history(#a2a_task{history = undefined} = Task, _Limit) ->
    Task;
limit_task_history(#a2a_task{history = History} = Task, Limit) ->
    LimitedHistory = case length(History) > Limit of
        true -> lists:sublist(lists:reverse(History), Limit);
        false -> History
    end,
    Task#a2a_task{history = lists:reverse(LimitedHistory)}.

%% @private Format validation error to binary
-spec format_validation_error(term()) -> binary().
format_validation_error(missing_message_id) ->
    <<"Missing required field: messageId">>;
format_validation_error(invalid_role) ->
    <<"Invalid or missing role">>;
format_validation_error(empty_parts) ->
    <<"Message must have at least one part">>;
format_validation_error(invalid_parts) ->
    <<"One or more message parts are invalid">>;
format_validation_error(missing_task_id) ->
    <<"Missing required field: id">>;
format_validation_error(invalid_task_id) ->
    <<"Invalid task ID format">>;
format_validation_error(missing_config_id) ->
    <<"Missing required field: configId">>;
format_validation_error(missing_config) ->
    <<"Missing required field: config">>;
format_validation_error({page_size_invalid, Msg}) ->
    Msg;
format_validation_error({page_size_too_large, Msg}) ->
    Msg;
format_validation_error({invalid_request, Reason}) ->
    iolist_to_binary([<<"Invalid request: ">>, format_error_reason(Reason)]);
format_validation_error({invalid_part, Reason}) ->
    iolist_to_binary([<<"Invalid part: ">>, format_error_reason(Reason)]);
format_validation_error({invalid_message, Reason}) ->
    iolist_to_binary([<<"Invalid message: ">>, format_error_reason(Reason)]);
format_validation_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_validation_error(Reason) when is_binary(Reason) ->
    Reason;
format_validation_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% @private Format error reason to binary
-spec format_error_reason(term()) -> binary().
format_error_reason(Reason) when is_binary(Reason) ->
    Reason;
format_error_reason(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error_reason(Reason) when is_list(Reason) ->
    try
        list_to_binary(Reason)
    catch
        _:_ -> iolist_to_binary(io_lib:format("~p", [Reason]))
    end;
format_error_reason(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).
