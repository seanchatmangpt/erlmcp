%%%-------------------------------------------------------------------
%%% @doc A2A JSON-RPC Bridge Module
%%%
%%% This module bridges the A2A (Agent-to-Agent) protocol to the existing
%%% JSON-RPC infrastructure in erlmcp. It provides:
%%% - Method registration for A2A methods with JSON-RPC router
%%% - Request parsing and validation from JSON-RPC to A2A records
%%% - Response formatting from A2A to JSON-RPC format
%%% - Error mapping from A2A errors to JSON-RPC errors
%%% - Notification formatting for streaming events
%%% - Batch request handling
%%%
%%% Protocol Specification: https://github.com/google/a2a-spec
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_a2a_jsonrpc).

-include("erlmcp.hrl").
-include("erlmcp_a2a.hrl").

%% API exports
-export([
    %% Method registration
    register_methods/0,
    unregister_methods/0,
    get_registered_methods/0,

    %% Request handling
    handle_method/3,
    handle_batch/2,

    %% Request parsing
    parse_request/2,
    parse_send_message_request/1,
    parse_get_task_request/1,
    parse_list_tasks_request/1,
    parse_cancel_task_request/1,
    parse_subscribe_to_task_request/1,
    parse_create_push_config_request/1,
    parse_get_push_config_request/1,
    parse_list_push_configs_request/1,
    parse_delete_push_config_request/1,
    parse_get_extended_agent_card_request/1,

    %% Response formatting
    format_response/2,
    format_task_response/1,
    format_message_response/1,
    format_list_tasks_response/1,
    format_list_push_configs_response/1,
    format_agent_card_response/1,

    %% Error formatting
    format_error/2,
    map_a2a_error_to_jsonrpc/1,

    %% Notification formatting
    format_notification/2,
    format_task_status_update_notification/1,
    format_task_artifact_update_notification/1,

    %% Utility functions
    is_a2a_method/1,
    get_method_handler/1
]).

%% Types
-type method() :: binary().
-type params() :: map() | undefined.
-type context() :: map().
-type a2a_request() :: #a2a_send_message_request{} |
                       #a2a_get_task_request{} |
                       #a2a_list_tasks_request{} |
                       #a2a_cancel_task_request{} |
                       #a2a_subscribe_to_task_request{} |
                       #a2a_create_push_config_request{} |
                       #a2a_get_push_config_request{} |
                       #a2a_list_push_configs_request{} |
                       #a2a_delete_push_config_request{} |
                       #a2a_get_extended_agent_card_request{}.

-type a2a_response() :: #a2a_send_message_response{} |
                        #a2a_task{} |
                        #a2a_list_tasks_response{} |
                        #a2a_list_push_configs_response{} |
                        #a2a_agent_card{} |
                        #a2a_task_push_notification_config{} |
                        ok.

-export_type([a2a_request/0, a2a_response/0]).

%%====================================================================
%% Method Registration
%%====================================================================

%% @doc Register all A2A methods with the JSON-RPC router
%% Returns ok on success, {error, Reason} on failure
-spec register_methods() -> ok | {error, term()}.
register_methods() ->
    Methods = get_method_definitions(),
    register_methods_loop(Methods).

-spec register_methods_loop([{method(), atom()}]) -> ok | {error, term()}.
register_methods_loop([]) ->
    ok;
register_methods_loop([{Method, Handler} | Rest]) ->
    case register_single_method(Method, Handler) of
        ok ->
            register_methods_loop(Rest);
        {error, _} = Error ->
            Error
    end.

-spec register_single_method(method(), atom()) -> ok | {error, term()}.
register_single_method(Method, Handler) ->
    %% Store method registration in process dictionary or ETS
    %% Integration point with erlmcp JSON-RPC router
    put({a2a_method, Method}, Handler),
    ok.

%% @doc Unregister all A2A methods from the JSON-RPC router
-spec unregister_methods() -> ok.
unregister_methods() ->
    Methods = get_method_definitions(),
    lists:foreach(fun({Method, _}) ->
        erase({a2a_method, Method})
    end, Methods),
    ok.

%% @doc Get list of registered A2A methods
-spec get_registered_methods() -> [method()].
get_registered_methods() ->
    [Method || {Method, _} <- get_method_definitions()].

%% @doc Get method definitions with their handler atoms
-spec get_method_definitions() -> [{method(), atom()}].
get_method_definitions() ->
    [
        %% Core message operations
        {?A2A_METHOD_SEND_MESSAGE, handle_send_message},
        {?A2A_METHOD_SEND_STREAMING_MESSAGE, handle_send_streaming_message},

        %% Task operations
        {?A2A_METHOD_GET_TASK, handle_get_task},
        {?A2A_METHOD_LIST_TASKS, handle_list_tasks},
        {?A2A_METHOD_CANCEL_TASK, handle_cancel_task},
        {?A2A_METHOD_SUBSCRIBE_TO_TASK, handle_subscribe_to_task},

        %% Push notification operations
        {?A2A_METHOD_CREATE_PUSH_CONFIG, handle_create_push_config},
        {?A2A_METHOD_GET_PUSH_CONFIG, handle_get_push_config},
        {?A2A_METHOD_LIST_PUSH_CONFIGS, handle_list_push_configs},
        {?A2A_METHOD_DELETE_PUSH_CONFIG, handle_delete_push_config},

        %% Agent card operations
        {?A2A_METHOD_GET_EXTENDED_AGENT_CARD, handle_get_extended_agent_card}
    ].

%%====================================================================
%% Request Handling
%%====================================================================

%% @doc Handle a specific A2A method
%% Returns {ok, Response} | {error, ErrorMap}
-spec handle_method(method(), params(), context()) ->
    {ok, map()} | {error, map()}.
handle_method(Method, Params, Context) ->
    case is_a2a_method(Method) of
        true ->
            case parse_request(Method, Params) of
                {ok, Request} ->
                    case dispatch_request(Method, Request, Context) of
                        {ok, Response} ->
                            format_response(Method, Response);
                        {error, A2AError} ->
                            {error, format_error(Method, A2AError)}
                    end;
                {error, ParseError} ->
                    {error, format_parse_error(ParseError)}
            end;
        false ->
            {error, #{
                <<"code">> => ?JSONRPC_METHOD_NOT_FOUND,
                <<"message">> => ?JSONRPC_MSG_METHOD_NOT_FOUND,
                <<"data">> => #{<<"method">> => Method}
            }}
    end.

%% @doc Handle batch of A2A requests
-spec handle_batch([{json_rpc_id(), method(), params()}], context()) ->
    [{json_rpc_id(), {ok, map()} | {error, map()}}].
handle_batch(Requests, Context) ->
    lists:map(fun({Id, Method, Params}) ->
        Result = handle_method(Method, Params, Context),
        {Id, Result}
    end, Requests).

%% @doc Dispatch request to appropriate handler
-spec dispatch_request(method(), a2a_request(), context()) ->
    {ok, a2a_response()} | {error, term()}.
dispatch_request(Method, Request, Context) ->
    case get_method_handler(Method) of
        {ok, Handler} ->
            %% Handler is expected to be implemented in erlmcp_a2a_server
            case erlang:function_exported(erlmcp_a2a_server, Handler, 2) of
                true ->
                    erlmcp_a2a_server:Handler(Request, Context);
                false ->
                    {error, {?A2A_ERROR_CAPABILITY_NOT_SUPPORTED, Method}}
            end;
        {error, not_found} ->
            {error, {?JSONRPC_METHOD_NOT_FOUND, Method}}
    end.

%%====================================================================
%% Request Parsing
%%====================================================================

%% @doc Parse JSON-RPC params to A2A request record
-spec parse_request(method(), params()) -> {ok, a2a_request()} | {error, term()}.
parse_request(?A2A_METHOD_SEND_MESSAGE, Params) ->
    parse_send_message_request(Params);
parse_request(?A2A_METHOD_SEND_STREAMING_MESSAGE, Params) ->
    parse_send_message_request(Params);
parse_request(?A2A_METHOD_GET_TASK, Params) ->
    parse_get_task_request(Params);
parse_request(?A2A_METHOD_LIST_TASKS, Params) ->
    parse_list_tasks_request(Params);
parse_request(?A2A_METHOD_CANCEL_TASK, Params) ->
    parse_cancel_task_request(Params);
parse_request(?A2A_METHOD_SUBSCRIBE_TO_TASK, Params) ->
    parse_subscribe_to_task_request(Params);
parse_request(?A2A_METHOD_CREATE_PUSH_CONFIG, Params) ->
    parse_create_push_config_request(Params);
parse_request(?A2A_METHOD_GET_PUSH_CONFIG, Params) ->
    parse_get_push_config_request(Params);
parse_request(?A2A_METHOD_LIST_PUSH_CONFIGS, Params) ->
    parse_list_push_configs_request(Params);
parse_request(?A2A_METHOD_DELETE_PUSH_CONFIG, Params) ->
    parse_delete_push_config_request(Params);
parse_request(?A2A_METHOD_GET_EXTENDED_AGENT_CARD, Params) ->
    parse_get_extended_agent_card_request(Params);
parse_request(Method, _Params) ->
    {error, {unknown_method, Method}}.

%% @doc Parse sendMessage request
-spec parse_send_message_request(params()) ->
    {ok, #a2a_send_message_request{}} | {error, term()}.
parse_send_message_request(undefined) ->
    {error, {missing_params, <<"params required">>}};
parse_send_message_request(Params) when is_map(Params) ->
    case maps:get(<<"message">>, Params, undefined) of
        undefined ->
            {error, {missing_field, <<"message">>}};
        MessageMap when is_map(MessageMap) ->
            case parse_message(MessageMap) of
                {ok, Message} ->
                    Configuration = parse_send_message_configuration(
                        maps:get(<<"configuration">>, Params, undefined)
                    ),
                    {ok, #a2a_send_message_request{
                        tenant = maps:get(<<"tenant">>, Params, undefined),
                        message = Message,
                        configuration = Configuration,
                        metadata = maps:get(<<"metadata">>, Params, undefined)
                    }};
                {error, _} = Error ->
                    Error
            end;
        _ ->
            {error, {invalid_field, <<"message must be an object">>}}
    end.

%% @doc Parse getTask request
-spec parse_get_task_request(params()) ->
    {ok, #a2a_get_task_request{}} | {error, term()}.
parse_get_task_request(undefined) ->
    {error, {missing_params, <<"params required">>}};
parse_get_task_request(Params) when is_map(Params) ->
    case maps:get(<<"id">>, Params, undefined) of
        undefined ->
            {error, {missing_field, <<"id">>}};
        Id when is_binary(Id) ->
            {ok, #a2a_get_task_request{
                tenant = maps:get(<<"tenant">>, Params, undefined),
                id = Id,
                history_length = maps:get(<<"historyLength">>, Params, undefined)
            }};
        _ ->
            {error, {invalid_field, <<"id must be a string">>}}
    end.

%% @doc Parse listTasks request
-spec parse_list_tasks_request(params()) ->
    {ok, #a2a_list_tasks_request{}} | {error, term()}.
parse_list_tasks_request(undefined) ->
    {ok, #a2a_list_tasks_request{}};
parse_list_tasks_request(Params) when is_map(Params) ->
    Status = case maps:get(<<"status">>, Params, undefined) of
        undefined -> undefined;
        StatusBin when is_binary(StatusBin) -> parse_task_state(StatusBin);
        _ -> undefined
    end,
    {ok, #a2a_list_tasks_request{
        tenant = maps:get(<<"tenant">>, Params, undefined),
        context_id = maps:get(<<"contextId">>, Params, undefined),
        status = Status,
        page_size = maps:get(<<"pageSize">>, Params, undefined),
        page_token = maps:get(<<"pageToken">>, Params, undefined),
        history_length = maps:get(<<"historyLength">>, Params, undefined),
        status_timestamp_after = maps:get(<<"statusTimestampAfter">>, Params, undefined),
        include_artifacts = maps:get(<<"includeArtifacts">>, Params, undefined)
    }}.

%% @doc Parse cancelTask request
-spec parse_cancel_task_request(params()) ->
    {ok, #a2a_cancel_task_request{}} | {error, term()}.
parse_cancel_task_request(undefined) ->
    {error, {missing_params, <<"params required">>}};
parse_cancel_task_request(Params) when is_map(Params) ->
    case maps:get(<<"id">>, Params, undefined) of
        undefined ->
            {error, {missing_field, <<"id">>}};
        Id when is_binary(Id) ->
            {ok, #a2a_cancel_task_request{
                tenant = maps:get(<<"tenant">>, Params, undefined),
                id = Id
            }};
        _ ->
            {error, {invalid_field, <<"id must be a string">>}}
    end.

%% @doc Parse subscribeToTask request
-spec parse_subscribe_to_task_request(params()) ->
    {ok, #a2a_subscribe_to_task_request{}} | {error, term()}.
parse_subscribe_to_task_request(undefined) ->
    {error, {missing_params, <<"params required">>}};
parse_subscribe_to_task_request(Params) when is_map(Params) ->
    case maps:get(<<"id">>, Params, undefined) of
        undefined ->
            {error, {missing_field, <<"id">>}};
        Id when is_binary(Id) ->
            {ok, #a2a_subscribe_to_task_request{
                tenant = maps:get(<<"tenant">>, Params, undefined),
                id = Id
            }};
        _ ->
            {error, {invalid_field, <<"id must be a string">>}}
    end.

%% @doc Parse createTaskPushNotificationConfig request
-spec parse_create_push_config_request(params()) ->
    {ok, #a2a_create_push_config_request{}} | {error, term()}.
parse_create_push_config_request(undefined) ->
    {error, {missing_params, <<"params required">>}};
parse_create_push_config_request(Params) when is_map(Params) ->
    TaskId = maps:get(<<"taskId">>, Params, undefined),
    ConfigId = maps:get(<<"configId">>, Params, undefined),
    ConfigMap = maps:get(<<"config">>, Params, undefined),
    case {TaskId, ConfigId, ConfigMap} of
        {undefined, _, _} ->
            {error, {missing_field, <<"taskId">>}};
        {_, undefined, _} ->
            {error, {missing_field, <<"configId">>}};
        {_, _, undefined} ->
            {error, {missing_field, <<"config">>}};
        {T, C, Cfg} when is_binary(T), is_binary(C), is_map(Cfg) ->
            case parse_push_notification_config(Cfg) of
                {ok, Config} ->
                    {ok, #a2a_create_push_config_request{
                        tenant = maps:get(<<"tenant">>, Params, undefined),
                        task_id = T,
                        config_id = C,
                        config = Config
                    }};
                {error, _} = Error ->
                    Error
            end;
        _ ->
            {error, {invalid_params, <<"invalid parameter types">>}}
    end.

%% @doc Parse getTaskPushNotificationConfig request
-spec parse_get_push_config_request(params()) ->
    {ok, #a2a_get_push_config_request{}} | {error, term()}.
parse_get_push_config_request(undefined) ->
    {error, {missing_params, <<"params required">>}};
parse_get_push_config_request(Params) when is_map(Params) ->
    TaskId = maps:get(<<"taskId">>, Params, undefined),
    Id = maps:get(<<"id">>, Params, undefined),
    case {TaskId, Id} of
        {undefined, _} ->
            {error, {missing_field, <<"taskId">>}};
        {_, undefined} ->
            {error, {missing_field, <<"id">>}};
        {T, I} when is_binary(T), is_binary(I) ->
            {ok, #a2a_get_push_config_request{
                tenant = maps:get(<<"tenant">>, Params, undefined),
                task_id = T,
                id = I
            }};
        _ ->
            {error, {invalid_params, <<"invalid parameter types">>}}
    end.

%% @doc Parse listTaskPushNotificationConfigs request
-spec parse_list_push_configs_request(params()) ->
    {ok, #a2a_list_push_configs_request{}} | {error, term()}.
parse_list_push_configs_request(undefined) ->
    {error, {missing_params, <<"params required">>}};
parse_list_push_configs_request(Params) when is_map(Params) ->
    case maps:get(<<"taskId">>, Params, undefined) of
        undefined ->
            {error, {missing_field, <<"taskId">>}};
        TaskId when is_binary(TaskId) ->
            {ok, #a2a_list_push_configs_request{
                tenant = maps:get(<<"tenant">>, Params, undefined),
                task_id = TaskId,
                page_size = maps:get(<<"pageSize">>, Params, undefined),
                page_token = maps:get(<<"pageToken">>, Params, undefined)
            }};
        _ ->
            {error, {invalid_field, <<"taskId must be a string">>}}
    end.

%% @doc Parse deleteTaskPushNotificationConfig request
-spec parse_delete_push_config_request(params()) ->
    {ok, #a2a_delete_push_config_request{}} | {error, term()}.
parse_delete_push_config_request(undefined) ->
    {error, {missing_params, <<"params required">>}};
parse_delete_push_config_request(Params) when is_map(Params) ->
    TaskId = maps:get(<<"taskId">>, Params, undefined),
    Id = maps:get(<<"id">>, Params, undefined),
    case {TaskId, Id} of
        {undefined, _} ->
            {error, {missing_field, <<"taskId">>}};
        {_, undefined} ->
            {error, {missing_field, <<"id">>}};
        {T, I} when is_binary(T), is_binary(I) ->
            {ok, #a2a_delete_push_config_request{
                tenant = maps:get(<<"tenant">>, Params, undefined),
                task_id = T,
                id = I
            }};
        _ ->
            {error, {invalid_params, <<"invalid parameter types">>}}
    end.

%% @doc Parse getExtendedAgentCard request
-spec parse_get_extended_agent_card_request(params()) ->
    {ok, #a2a_get_extended_agent_card_request{}} | {error, term()}.
parse_get_extended_agent_card_request(undefined) ->
    {ok, #a2a_get_extended_agent_card_request{}};
parse_get_extended_agent_card_request(Params) when is_map(Params) ->
    {ok, #a2a_get_extended_agent_card_request{
        tenant = maps:get(<<"tenant">>, Params, undefined)
    }}.

%%====================================================================
%% Response Formatting
%%====================================================================

%% @doc Format A2A response as JSON-RPC response
-spec format_response(method(), a2a_response()) -> {ok, map()}.
format_response(?A2A_METHOD_SEND_MESSAGE, Response) ->
    format_send_message_response(Response);
format_response(?A2A_METHOD_SEND_STREAMING_MESSAGE, Response) ->
    format_send_message_response(Response);
format_response(?A2A_METHOD_GET_TASK, Response) ->
    format_task_response(Response);
format_response(?A2A_METHOD_LIST_TASKS, Response) ->
    format_list_tasks_response(Response);
format_response(?A2A_METHOD_CANCEL_TASK, Response) ->
    format_task_response(Response);
format_response(?A2A_METHOD_SUBSCRIBE_TO_TASK, _Response) ->
    {ok, #{<<"subscribed">> => true}};
format_response(?A2A_METHOD_CREATE_PUSH_CONFIG, Response) ->
    format_push_config_response(Response);
format_response(?A2A_METHOD_GET_PUSH_CONFIG, Response) ->
    format_push_config_response(Response);
format_response(?A2A_METHOD_LIST_PUSH_CONFIGS, Response) ->
    format_list_push_configs_response(Response);
format_response(?A2A_METHOD_DELETE_PUSH_CONFIG, _Response) ->
    {ok, #{<<"deleted">> => true}};
format_response(?A2A_METHOD_GET_EXTENDED_AGENT_CARD, Response) ->
    format_agent_card_response(Response);
format_response(_Method, ok) ->
    {ok, #{}};
format_response(_Method, Response) when is_map(Response) ->
    {ok, Response}.

%% @doc Format sendMessage response
-spec format_send_message_response(#a2a_send_message_response{}) -> {ok, map()}.
format_send_message_response(#a2a_send_message_response{task = Task})
    when Task =/= undefined ->
    format_task_response(Task);
format_send_message_response(#a2a_send_message_response{message = Message})
    when Message =/= undefined ->
    format_message_response(Message);
format_send_message_response(_) ->
    {ok, #{}}.

%% @doc Format task response
-spec format_task_response(#a2a_task{}) -> {ok, map()}.
format_task_response(#a2a_task{} = Task) ->
    {ok, task_to_map(Task)}.

%% @doc Format message response
-spec format_message_response(#a2a_message{}) -> {ok, map()}.
format_message_response(#a2a_message{} = Message) ->
    {ok, message_to_map(Message)}.

%% @doc Format listTasks response
-spec format_list_tasks_response(#a2a_list_tasks_response{}) -> {ok, map()}.
format_list_tasks_response(#a2a_list_tasks_response{
    tasks = Tasks,
    next_page_token = NextPageToken,
    page_size = PageSize,
    total_size = TotalSize
}) ->
    TaskMaps = [task_to_map(T) || T <- Tasks],
    Result = #{
        <<"tasks">> => TaskMaps,
        <<"pageSize">> => PageSize,
        <<"totalSize">> => TotalSize
    },
    Result1 = case NextPageToken of
        undefined -> Result;
        <<>> -> Result;
        Token -> Result#{<<"nextPageToken">> => Token}
    end,
    {ok, Result1}.

%% @doc Format push config response
-spec format_push_config_response(#a2a_task_push_notification_config{}) -> {ok, map()}.
format_push_config_response(#a2a_task_push_notification_config{} = Config) ->
    {ok, push_config_to_map(Config)}.

%% @doc Format listPushConfigs response
-spec format_list_push_configs_response(#a2a_list_push_configs_response{}) -> {ok, map()}.
format_list_push_configs_response(#a2a_list_push_configs_response{
    configs = Configs,
    next_page_token = NextPageToken
}) ->
    ConfigMaps = [push_config_to_map(C) || C <- Configs],
    Result = #{<<"configs">> => ConfigMaps},
    Result1 = case NextPageToken of
        undefined -> Result;
        Token -> Result#{<<"nextPageToken">> => Token}
    end,
    {ok, Result1}.

%% @doc Format agent card response
-spec format_agent_card_response(#a2a_agent_card{}) -> {ok, map()}.
format_agent_card_response(#a2a_agent_card{} = Card) ->
    {ok, agent_card_to_map(Card)}.

%%====================================================================
%% Error Formatting
%%====================================================================

%% @doc Format A2A error as JSON-RPC error
-spec format_error(method(), term()) -> map().
format_error(_Method, {Code, Message}) when is_integer(Code), is_binary(Message) ->
    #{
        <<"code">> => Code,
        <<"message">> => Message
    };
format_error(_Method, {Code, Message, Data})
    when is_integer(Code), is_binary(Message) ->
    #{
        <<"code">> => Code,
        <<"message">> => Message,
        <<"data">> => Data
    };
format_error(_Method, Code) when is_integer(Code) ->
    map_a2a_error_to_jsonrpc(Code);
format_error(_Method, Error) ->
    #{
        <<"code">> => ?JSONRPC_INTERNAL_ERROR,
        <<"message">> => ?JSONRPC_MSG_INTERNAL_ERROR,
        <<"data">> => #{<<"error">> => format_error_data(Error)}
    }.

%% @doc Format parse error
-spec format_parse_error(term()) -> map().
format_parse_error({missing_params, Details}) ->
    #{
        <<"code">> => ?JSONRPC_INVALID_PARAMS,
        <<"message">> => ?JSONRPC_MSG_INVALID_PARAMS,
        <<"data">> => #{<<"details">> => Details}
    };
format_parse_error({missing_field, Field}) ->
    #{
        <<"code">> => ?JSONRPC_INVALID_PARAMS,
        <<"message">> => <<"Missing required field">>,
        <<"data">> => #{<<"field">> => Field}
    };
format_parse_error({invalid_field, Details}) ->
    #{
        <<"code">> => ?JSONRPC_INVALID_PARAMS,
        <<"message">> => <<"Invalid field value">>,
        <<"data">> => #{<<"details">> => Details}
    };
format_parse_error({invalid_params, Details}) ->
    #{
        <<"code">> => ?JSONRPC_INVALID_PARAMS,
        <<"message">> => ?JSONRPC_MSG_INVALID_PARAMS,
        <<"data">> => #{<<"details">> => Details}
    };
format_parse_error({unknown_method, Method}) ->
    #{
        <<"code">> => ?JSONRPC_METHOD_NOT_FOUND,
        <<"message">> => ?JSONRPC_MSG_METHOD_NOT_FOUND,
        <<"data">> => #{<<"method">> => Method}
    };
format_parse_error(Error) ->
    #{
        <<"code">> => ?JSONRPC_INVALID_PARAMS,
        <<"message">> => ?JSONRPC_MSG_INVALID_PARAMS,
        <<"data">> => #{<<"error">> => format_error_data(Error)}
    }.

%% @doc Map A2A error code to JSON-RPC error object
-spec map_a2a_error_to_jsonrpc(integer()) -> map().
%% Core A2A errors
map_a2a_error_to_jsonrpc(?A2A_ERROR_TASK_NOT_FOUND) ->
    #{<<"code">> => ?A2A_ERROR_TASK_NOT_FOUND,
      <<"message">> => ?A2A_MSG_TASK_NOT_FOUND};
map_a2a_error_to_jsonrpc(?A2A_ERROR_CONTEXT_NOT_FOUND) ->
    #{<<"code">> => ?A2A_ERROR_CONTEXT_NOT_FOUND,
      <<"message">> => ?A2A_MSG_CONTEXT_NOT_FOUND};
map_a2a_error_to_jsonrpc(?A2A_ERROR_ARTIFACT_NOT_FOUND) ->
    #{<<"code">> => ?A2A_ERROR_ARTIFACT_NOT_FOUND,
      <<"message">> => ?A2A_MSG_ARTIFACT_NOT_FOUND};
map_a2a_error_to_jsonrpc(?A2A_ERROR_MESSAGE_INVALID) ->
    #{<<"code">> => ?A2A_ERROR_MESSAGE_INVALID,
      <<"message">> => ?A2A_MSG_MESSAGE_INVALID};
map_a2a_error_to_jsonrpc(?A2A_ERROR_AGENT_NOT_FOUND) ->
    #{<<"code">> => ?A2A_ERROR_AGENT_NOT_FOUND,
      <<"message">> => ?A2A_MSG_AGENT_NOT_FOUND};
map_a2a_error_to_jsonrpc(?A2A_ERROR_CAPABILITY_NOT_SUPPORTED) ->
    #{<<"code">> => ?A2A_ERROR_CAPABILITY_NOT_SUPPORTED,
      <<"message">> => ?A2A_MSG_CAPABILITY_NOT_SUPPORTED};
map_a2a_error_to_jsonrpc(?A2A_ERROR_PUSH_NOTIFICATION_FAILED) ->
    #{<<"code">> => ?A2A_ERROR_PUSH_NOTIFICATION_FAILED,
      <<"message">> => ?A2A_MSG_PUSH_NOTIFICATION_FAILED};
map_a2a_error_to_jsonrpc(?A2A_ERROR_SUBSCRIPTION_NOT_FOUND) ->
    #{<<"code">> => ?A2A_ERROR_SUBSCRIPTION_NOT_FOUND,
      <<"message">> => ?A2A_MSG_SUBSCRIPTION_NOT_FOUND};
map_a2a_error_to_jsonrpc(?A2A_ERROR_SKILL_NOT_FOUND) ->
    #{<<"code">> => ?A2A_ERROR_SKILL_NOT_FOUND,
      <<"message">> => ?A2A_MSG_SKILL_NOT_FOUND};
map_a2a_error_to_jsonrpc(?A2A_ERROR_EXTENSION_NOT_SUPPORTED) ->
    #{<<"code">> => ?A2A_ERROR_EXTENSION_NOT_SUPPORTED,
      <<"message">> => ?A2A_MSG_EXTENSION_NOT_SUPPORTED};
%% Task state errors
map_a2a_error_to_jsonrpc(?A2A_ERROR_TASK_ALREADY_TERMINAL) ->
    #{<<"code">> => ?A2A_ERROR_TASK_ALREADY_TERMINAL,
      <<"message">> => ?A2A_MSG_TASK_ALREADY_TERMINAL};
map_a2a_error_to_jsonrpc(?A2A_ERROR_TASK_NOT_CANCELABLE) ->
    #{<<"code">> => ?A2A_ERROR_TASK_NOT_CANCELABLE,
      <<"message">> => ?A2A_MSG_TASK_NOT_CANCELABLE};
map_a2a_error_to_jsonrpc(?A2A_ERROR_TASK_STATE_TRANSITION_INVALID) ->
    #{<<"code">> => ?A2A_ERROR_TASK_STATE_TRANSITION_INVALID,
      <<"message">> => ?A2A_MSG_TASK_STATE_TRANSITION_INVALID};
%% Default fallback
map_a2a_error_to_jsonrpc(Code) when is_integer(Code) ->
    #{<<"code">> => Code,
      <<"message">> => <<"A2A error">>}.

%%====================================================================
%% Notification Formatting
%%====================================================================

%% @doc Format streaming event as JSON-RPC notification
-spec format_notification(binary(), term()) -> map().
format_notification(?A2A_NOTIFICATION_TASK_STATUS_UPDATE, Event) ->
    format_task_status_update_notification(Event);
format_notification(?A2A_NOTIFICATION_TASK_ARTIFACT_UPDATE, Event) ->
    format_task_artifact_update_notification(Event);
format_notification(Method, Params) when is_map(Params) ->
    #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"method">> => Method,
        <<"params">> => Params
    }.

%% @doc Format task status update notification
-spec format_task_status_update_notification(#a2a_task_status_update_event{}) -> map().
format_task_status_update_notification(#a2a_task_status_update_event{
    task_id = TaskId,
    context_id = ContextId,
    status = Status,
    metadata = Metadata
}) ->
    Params = #{
        <<"taskId">> => TaskId,
        <<"contextId">> => ContextId,
        <<"status">> => task_status_to_map(Status)
    },
    Params1 = maybe_add_field(Params, <<"metadata">>, Metadata),
    #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"method">> => ?A2A_NOTIFICATION_TASK_STATUS_UPDATE,
        <<"params">> => Params1
    }.

%% @doc Format task artifact update notification
-spec format_task_artifact_update_notification(#a2a_task_artifact_update_event{}) -> map().
format_task_artifact_update_notification(#a2a_task_artifact_update_event{
    task_id = TaskId,
    context_id = ContextId,
    artifact = Artifact,
    append = Append,
    last_chunk = LastChunk,
    metadata = Metadata
}) ->
    Params = #{
        <<"taskId">> => TaskId,
        <<"contextId">> => ContextId,
        <<"artifact">> => artifact_to_map(Artifact),
        <<"append">> => Append,
        <<"lastChunk">> => LastChunk
    },
    Params1 = maybe_add_field(Params, <<"metadata">>, Metadata),
    #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"method">> => ?A2A_NOTIFICATION_TASK_ARTIFACT_UPDATE,
        <<"params">> => Params1
    }.

%%====================================================================
%% Utility Functions
%%====================================================================

%% @doc Check if method is an A2A method
-spec is_a2a_method(method()) -> boolean().
is_a2a_method(Method) when is_binary(Method) ->
    case binary:match(Method, <<"a2a/">>) of
        {0, _} -> true;
        _ -> false
    end.

%% @doc Get handler for a method
-spec get_method_handler(method()) -> {ok, atom()} | {error, not_found}.
get_method_handler(Method) ->
    case get({a2a_method, Method}) of
        undefined ->
            %% Fallback to definitions
            case lists:keyfind(Method, 1, get_method_definitions()) of
                {_, Handler} -> {ok, Handler};
                false -> {error, not_found}
            end;
        Handler ->
            {ok, Handler}
    end.

%%====================================================================
%% Internal Helper Functions - Parsing
%%====================================================================

%% @doc Parse message from map
-spec parse_message(map()) -> {ok, #a2a_message{}} | {error, term()}.
parse_message(Map) when is_map(Map) ->
    case maps:get(<<"messageId">>, Map, undefined) of
        undefined ->
            {error, {missing_field, <<"messageId">>}};
        MessageId when is_binary(MessageId) ->
            case maps:get(<<"role">>, Map, undefined) of
                undefined ->
                    {error, {missing_field, <<"role">>}};
                RoleBin when is_binary(RoleBin) ->
                    case maps:get(<<"parts">>, Map, undefined) of
                        undefined ->
                            {error, {missing_field, <<"parts">>}};
                        Parts when is_list(Parts), length(Parts) > 0 ->
                            case parse_parts(Parts) of
                                {ok, ParsedParts} ->
                                    {ok, #a2a_message{
                                        message_id = MessageId,
                                        context_id = maps:get(<<"contextId">>, Map, undefined),
                                        task_id = maps:get(<<"taskId">>, Map, undefined),
                                        role = parse_role(RoleBin),
                                        parts = ParsedParts,
                                        metadata = maps:get(<<"metadata">>, Map, undefined),
                                        extensions = maps:get(<<"extensions">>, Map, undefined),
                                        reference_task_ids = maps:get(<<"referenceTaskIds">>, Map, undefined)
                                    }};
                                {error, _} = Error ->
                                    Error
                            end;
                        _ ->
                            {error, {invalid_field, <<"parts must be a non-empty array">>}}
                    end;
                _ ->
                    {error, {invalid_field, <<"role must be a string">>}}
            end;
        _ ->
            {error, {invalid_field, <<"messageId must be a string">>}}
    end.

%% @doc Parse parts list
-spec parse_parts([map()]) -> {ok, [#a2a_part{}]} | {error, term()}.
parse_parts(Parts) ->
    parse_parts(Parts, []).

parse_parts([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_parts([Part | Rest], Acc) when is_map(Part) ->
    ParsedPart = #a2a_part{
        text = maps:get(<<"text">>, Part, undefined),
        raw = maps:get(<<"raw">>, Part, undefined),
        url = maps:get(<<"url">>, Part, undefined),
        data = maps:get(<<"data">>, Part, undefined),
        metadata = maps:get(<<"metadata">>, Part, undefined),
        filename = maps:get(<<"filename">>, Part, undefined),
        media_type = maps:get(<<"mediaType">>, Part, undefined)
    },
    parse_parts(Rest, [ParsedPart | Acc]);
parse_parts([_ | _], _) ->
    {error, {invalid_field, <<"each part must be an object">>}}.

%% @doc Parse role from binary
-spec parse_role(binary()) -> a2a_role().
parse_role(<<"user">>) -> user;
parse_role(<<"agent">>) -> agent;
parse_role(_) -> unspecified.

%% @doc Parse task state from binary
-spec parse_task_state(binary()) -> a2a_task_state().
parse_task_state(<<"submitted">>) -> submitted;
parse_task_state(<<"working">>) -> working;
parse_task_state(<<"completed">>) -> completed;
parse_task_state(<<"failed">>) -> failed;
parse_task_state(<<"canceled">>) -> canceled;
parse_task_state(<<"input_required">>) -> input_required;
parse_task_state(<<"rejected">>) -> rejected;
parse_task_state(<<"auth_required">>) -> auth_required;
parse_task_state(_) -> unspecified.

%% @doc Parse send message configuration
-spec parse_send_message_configuration(map() | undefined) ->
    #a2a_send_message_configuration{} | undefined.
parse_send_message_configuration(undefined) ->
    undefined;
parse_send_message_configuration(Map) when is_map(Map) ->
    PushConfig = case maps:get(<<"pushNotificationConfig">>, Map, undefined) of
        undefined -> undefined;
        ConfigMap when is_map(ConfigMap) ->
            case parse_push_notification_config(ConfigMap) of
                {ok, Config} -> Config;
                {error, _} -> undefined
            end;
        _ -> undefined
    end,
    #a2a_send_message_configuration{
        accepted_output_modes = maps:get(<<"acceptedOutputModes">>, Map, undefined),
        push_notification_config = PushConfig,
        history_length = maps:get(<<"historyLength">>, Map, undefined),
        blocking = maps:get(<<"blocking">>, Map, false)
    }.

%% @doc Parse push notification config
-spec parse_push_notification_config(map()) ->
    {ok, #a2a_push_notification_config{}} | {error, term()}.
parse_push_notification_config(Map) when is_map(Map) ->
    case maps:get(<<"url">>, Map, undefined) of
        undefined ->
            {error, {missing_field, <<"url">>}};
        Url when is_binary(Url) ->
            Auth = case maps:get(<<"authentication">>, Map, undefined) of
                undefined -> undefined;
                AuthMap when is_map(AuthMap) ->
                    #a2a_authentication_info{
                        scheme = maps:get(<<"scheme">>, AuthMap, undefined),
                        credentials = maps:get(<<"credentials">>, AuthMap, undefined)
                    };
                _ -> undefined
            end,
            {ok, #a2a_push_notification_config{
                id = maps:get(<<"id">>, Map, undefined),
                url = Url,
                token = maps:get(<<"token">>, Map, undefined),
                authentication = Auth
            }};
        _ ->
            {error, {invalid_field, <<"url must be a string">>}}
    end.

%%====================================================================
%% Internal Helper Functions - Serialization
%%====================================================================

%% @doc Convert task record to map
-spec task_to_map(#a2a_task{}) -> map().
task_to_map(#a2a_task{
    id = Id,
    context_id = ContextId,
    status = Status,
    artifacts = Artifacts,
    history = History,
    metadata = Metadata
}) ->
    Base = #{
        <<"id">> => Id,
        <<"contextId">> => ContextId,
        <<"status">> => task_status_to_map(Status)
    },
    Base1 = maybe_add_field(Base, <<"artifacts">>,
        case Artifacts of
            undefined -> undefined;
            Arts -> [artifact_to_map(A) || A <- Arts]
        end),
    Base2 = maybe_add_field(Base1, <<"history">>,
        case History of
            undefined -> undefined;
            Hist -> [message_to_map(M) || M <- Hist]
        end),
    maybe_add_field(Base2, <<"metadata">>, Metadata).

%% @doc Convert task status record to map
-spec task_status_to_map(#a2a_task_status{}) -> map().
task_status_to_map(#a2a_task_status{
    state = State,
    message = Message,
    timestamp = Timestamp
}) ->
    Base = #{<<"state">> => task_state_to_binary(State)},
    Base1 = maybe_add_field(Base, <<"message">>,
        case Message of
            undefined -> undefined;
            Msg -> message_to_map(Msg)
        end),
    maybe_add_field(Base1, <<"timestamp">>, Timestamp).

%% @doc Convert task state atom to binary
-spec task_state_to_binary(a2a_task_state()) -> binary().
task_state_to_binary(submitted) -> <<"submitted">>;
task_state_to_binary(working) -> <<"working">>;
task_state_to_binary(completed) -> <<"completed">>;
task_state_to_binary(failed) -> <<"failed">>;
task_state_to_binary(canceled) -> <<"canceled">>;
task_state_to_binary(input_required) -> <<"input_required">>;
task_state_to_binary(rejected) -> <<"rejected">>;
task_state_to_binary(auth_required) -> <<"auth_required">>;
task_state_to_binary(_) -> <<"unspecified">>.

%% @doc Convert message record to map
-spec message_to_map(#a2a_message{}) -> map().
message_to_map(#a2a_message{
    message_id = MessageId,
    context_id = ContextId,
    task_id = TaskId,
    role = Role,
    parts = Parts,
    metadata = Metadata,
    extensions = Extensions,
    reference_task_ids = RefTaskIds
}) ->
    Base = #{
        <<"messageId">> => MessageId,
        <<"role">> => role_to_binary(Role),
        <<"parts">> => [part_to_map(P) || P <- Parts]
    },
    Base1 = maybe_add_field(Base, <<"contextId">>, ContextId),
    Base2 = maybe_add_field(Base1, <<"taskId">>, TaskId),
    Base3 = maybe_add_field(Base2, <<"metadata">>, Metadata),
    Base4 = maybe_add_field(Base3, <<"extensions">>, Extensions),
    maybe_add_field(Base4, <<"referenceTaskIds">>, RefTaskIds).

%% @doc Convert role atom to binary
-spec role_to_binary(a2a_role()) -> binary().
role_to_binary(user) -> <<"user">>;
role_to_binary(agent) -> <<"agent">>;
role_to_binary(_) -> <<"unspecified">>.

%% @doc Convert part record to map
-spec part_to_map(#a2a_part{}) -> map().
part_to_map(#a2a_part{
    text = Text,
    raw = Raw,
    url = Url,
    data = Data,
    metadata = Metadata,
    filename = Filename,
    media_type = MediaType
}) ->
    Base = #{},
    Base1 = maybe_add_field(Base, <<"text">>, Text),
    Base2 = maybe_add_field(Base1, <<"raw">>, Raw),
    Base3 = maybe_add_field(Base2, <<"url">>, Url),
    Base4 = maybe_add_field(Base3, <<"data">>, Data),
    Base5 = maybe_add_field(Base4, <<"metadata">>, Metadata),
    Base6 = maybe_add_field(Base5, <<"filename">>, Filename),
    maybe_add_field(Base6, <<"mediaType">>, MediaType).

%% @doc Convert artifact record to map
-spec artifact_to_map(#a2a_artifact{}) -> map().
artifact_to_map(#a2a_artifact{
    artifact_id = ArtifactId,
    name = Name,
    description = Description,
    parts = Parts,
    metadata = Metadata,
    extensions = Extensions
}) ->
    Base = #{
        <<"artifactId">> => ArtifactId,
        <<"parts">> => [part_to_map(P) || P <- Parts]
    },
    Base1 = maybe_add_field(Base, <<"name">>, Name),
    Base2 = maybe_add_field(Base1, <<"description">>, Description),
    Base3 = maybe_add_field(Base2, <<"metadata">>, Metadata),
    maybe_add_field(Base3, <<"extensions">>, Extensions).

%% @doc Convert push config record to map
-spec push_config_to_map(#a2a_task_push_notification_config{}) -> map().
push_config_to_map(#a2a_task_push_notification_config{
    tenant = Tenant,
    id = Id,
    task_id = TaskId,
    push_notification_config = Config
}) ->
    Base = #{
        <<"id">> => Id,
        <<"taskId">> => TaskId,
        <<"pushNotificationConfig">> => push_notification_config_to_map(Config)
    },
    maybe_add_field(Base, <<"tenant">>, Tenant).

%% @doc Convert push notification config to map
-spec push_notification_config_to_map(#a2a_push_notification_config{}) -> map().
push_notification_config_to_map(#a2a_push_notification_config{
    id = Id,
    url = Url,
    token = Token,
    authentication = Auth
}) ->
    Base = #{<<"url">> => Url},
    Base1 = maybe_add_field(Base, <<"id">>, Id),
    Base2 = maybe_add_field(Base1, <<"token">>, Token),
    case Auth of
        undefined -> Base2;
        #a2a_authentication_info{scheme = Scheme, credentials = Creds} ->
            AuthMap = #{<<"scheme">> => Scheme},
            AuthMap1 = maybe_add_field(AuthMap, <<"credentials">>, Creds),
            Base2#{<<"authentication">> => AuthMap1}
    end.

%% @doc Convert agent card record to map
-spec agent_card_to_map(#a2a_agent_card{}) -> map().
agent_card_to_map(#a2a_agent_card{
    name = Name,
    description = Description,
    supported_interfaces = Interfaces,
    provider = Provider,
    version = Version,
    documentation_url = DocUrl,
    capabilities = Capabilities,
    security_schemes = SecuritySchemes,
    security_requirements = SecurityReqs,
    default_input_modes = InputModes,
    default_output_modes = OutputModes,
    skills = Skills,
    signatures = Signatures,
    icon_url = IconUrl
}) ->
    Base = #{
        <<"name">> => Name,
        <<"description">> => Description,
        <<"supportedInterfaces">> => [interface_to_map(I) || I <- Interfaces],
        <<"version">> => Version,
        <<"capabilities">> => capabilities_to_map(Capabilities),
        <<"defaultInputModes">> => InputModes,
        <<"defaultOutputModes">> => OutputModes,
        <<"skills">> => [skill_to_map(S) || S <- Skills]
    },
    Base1 = maybe_add_field(Base, <<"provider">>,
        case Provider of
            undefined -> undefined;
            #a2a_agent_provider{url = PUrl, organization = Org} ->
                #{<<"url">> => PUrl, <<"organization">> => Org}
        end),
    Base2 = maybe_add_field(Base1, <<"documentationUrl">>, DocUrl),
    Base3 = maybe_add_field(Base2, <<"securitySchemes">>, SecuritySchemes),
    Base4 = maybe_add_field(Base3, <<"securityRequirements">>, SecurityReqs),
    Base5 = maybe_add_field(Base4, <<"signatures">>,
        case Signatures of
            undefined -> undefined;
            Sigs -> [signature_to_map(S) || S <- Sigs]
        end),
    maybe_add_field(Base5, <<"iconUrl">>, IconUrl).

%% @doc Convert interface record to map
-spec interface_to_map(#a2a_agent_interface{}) -> map().
interface_to_map(#a2a_agent_interface{
    url = Url,
    protocol_binding = Binding,
    tenant = Tenant,
    protocol_version = Version
}) ->
    Base = #{
        <<"url">> => Url,
        <<"protocolBinding">> => Binding,
        <<"protocolVersion">> => Version
    },
    maybe_add_field(Base, <<"tenant">>, Tenant).

%% @doc Convert capabilities record to map
-spec capabilities_to_map(#a2a_agent_capabilities{}) -> map().
capabilities_to_map(#a2a_agent_capabilities{
    streaming = Streaming,
    push_notifications = PushNotifications,
    extensions = Extensions,
    extended_agent_card = ExtendedCard
}) ->
    Base = #{},
    Base1 = maybe_add_field(Base, <<"streaming">>, Streaming),
    Base2 = maybe_add_field(Base1, <<"pushNotifications">>, PushNotifications),
    Base3 = maybe_add_field(Base2, <<"extensions">>,
        case Extensions of
            undefined -> undefined;
            Exts -> [extension_to_map(E) || E <- Exts]
        end),
    maybe_add_field(Base3, <<"extendedAgentCard">>, ExtendedCard).

%% @doc Convert extension record to map
-spec extension_to_map(#a2a_agent_extension{}) -> map().
extension_to_map(#a2a_agent_extension{
    uri = Uri,
    description = Description,
    required = Required,
    params = Params
}) ->
    Base = #{<<"uri">> => Uri, <<"required">> => Required},
    Base1 = maybe_add_field(Base, <<"description">>, Description),
    maybe_add_field(Base1, <<"params">>, Params).

%% @doc Convert skill record to map
-spec skill_to_map(#a2a_agent_skill{}) -> map().
skill_to_map(#a2a_agent_skill{
    id = Id,
    name = Name,
    description = Description,
    tags = Tags,
    examples = Examples,
    input_modes = InputModes,
    output_modes = OutputModes,
    security_requirements = SecurityReqs
}) ->
    Base = #{
        <<"id">> => Id,
        <<"name">> => Name,
        <<"description">> => Description,
        <<"tags">> => Tags
    },
    Base1 = maybe_add_field(Base, <<"examples">>, Examples),
    Base2 = maybe_add_field(Base1, <<"inputModes">>, InputModes),
    Base3 = maybe_add_field(Base2, <<"outputModes">>, OutputModes),
    maybe_add_field(Base3, <<"securityRequirements">>, SecurityReqs).

%% @doc Convert signature record to map
-spec signature_to_map(#a2a_agent_card_signature{}) -> map().
signature_to_map(#a2a_agent_card_signature{
    protected = Protected,
    signature = Signature,
    header = Header
}) ->
    Base = #{
        <<"protected">> => Protected,
        <<"signature">> => Signature
    },
    maybe_add_field(Base, <<"header">>, Header).

%%====================================================================
%% Internal Utility Functions
%%====================================================================

%% @doc Add field to map only if value is not undefined
-spec maybe_add_field(map(), binary(), term()) -> map().
maybe_add_field(Map, _Key, undefined) ->
    Map;
maybe_add_field(Map, Key, Value) ->
    Map#{Key => Value}.

%% @doc Format error data for JSON
-spec format_error_data(term()) -> binary() | map().
format_error_data(Bin) when is_binary(Bin) ->
    Bin;
format_error_data(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
format_error_data(List) when is_list(List) ->
    try
        list_to_binary(List)
    catch
        _:_ -> iolist_to_binary(io_lib:format("~p", [List]))
    end;
format_error_data(Map) when is_map(Map) ->
    Map;
format_error_data(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).
