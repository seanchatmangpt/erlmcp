-module(erlmcp_prompt_list_change_notifier).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% API exports
-export([
    notify_prompt_added/4,
    notify_prompt_removed/2,
    notify_prompt_updated/4,
    broadcast_to_subscribers/3,
    send_notification_to_client/3
]).

%% Types
-type prompt_name() :: binary().
-type operation() :: added | removed | updated.

-export_type([server_id/0, prompt_name/0, operation/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Send prompt added notification with full metadata
-spec notify_prompt_added(server_id(), prompt_name(), #mcp_prompt{}, pid()) -> ok.
notify_prompt_added(ServerId, PromptName, Prompt, NotifierPid) ->
    SpanCtx = erlmcp_tracing:start_span(<<"prompt_list_change.notify_added">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"server_id">> => atom_to_binary(ServerId, utf8),
            <<"prompt.name">> => PromptName,
            <<"operation">> => <<"added">>
        }),

        PromptMetadata = encode_prompt_metadata(Prompt),
        Params = #{
            <<"operation">> => <<"added">>,
            <<"prompt">> => PromptMetadata
        },

        broadcast_via_notifier(ServerId, prompts, Params, NotifierPid),
        erlmcp_tracing:set_status(SpanCtx, ok)
    catch
        Class:Reason:Stack ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stack),
            logger:error("Failed to notify prompt added: ~p:~p~n~p", [Class, Reason, Stack])
    after
        erlmcp_tracing:end_span(SpanCtx)
    end,
    ok.

%% @doc Send prompt removed notification
-spec notify_prompt_removed(server_id(), prompt_name()) -> ok.
notify_prompt_removed(ServerId, PromptName) ->
    SpanCtx = erlmcp_tracing:start_span(<<"prompt_list_change.notify_removed">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"server_id">> => atom_to_binary(ServerId, utf8),
            <<"prompt.name">> => PromptName,
            <<"operation">> => <<"removed">>
        }),

        Params = #{
            <<"operation">> => <<"removed">>,
            <<"prompt">> => #{
                <<"name">> => PromptName
            }
        },

        % Get notifier from change_notifier if running
        case erlmcp_change_notifier:get_subscribers(prompts) of
            [] ->
                ok;
            Subscribers ->
                Method = ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED,
                notify_subscribers(Subscribers, Method, Params)
        end,

        erlmcp_tracing:set_status(SpanCtx, ok)
    catch
        Class:Reason:Stack ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stack),
            logger:error("Failed to notify prompt removed: ~p:~p~n~p", [Class, Reason, Stack])
    after
        erlmcp_tracing:end_span(SpanCtx)
    end,
    ok.

%% @doc Send prompt updated notification
-spec notify_prompt_updated(server_id(), prompt_name(), #mcp_prompt{}, pid()) -> ok.
notify_prompt_updated(ServerId, PromptName, UpdatedPrompt, NotifierPid) ->
    SpanCtx = erlmcp_tracing:start_span(<<"prompt_list_change.notify_updated">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"server_id">> => atom_to_binary(ServerId, utf8),
            <<"prompt.name">> => PromptName,
            <<"operation">> => <<"updated">>
        }),

        PromptMetadata = encode_prompt_metadata(UpdatedPrompt),
        Params = #{
            <<"operation">> => <<"updated">>,
            <<"prompt">> => PromptMetadata
        },

        broadcast_via_notifier(ServerId, prompts, Params, NotifierPid),
        erlmcp_tracing:set_status(SpanCtx, ok)
    catch
        Class:Reason:Stack ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stack),
            logger:error("Failed to notify prompt updated: ~p:~p~n~p", [Class, Reason, Stack])
    after
        erlmcp_tracing:end_span(SpanCtx)
    end,
    ok.

%% @doc Broadcast notification to all subscribers
-spec broadcast_to_subscribers(binary(), [pid()], map()) -> ok.
broadcast_to_subscribers(Method, Subscribers, Params) ->
    SpanCtx = erlmcp_tracing:start_span(<<"prompt_list_change.broadcast_to_subscribers">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"method">> => Method,
            <<"subscriber_count">> => length(Subscribers)
        }),

        notify_subscribers(Subscribers, Method, Params),
        erlmcp_tracing:set_status(SpanCtx, ok)
    catch
        Class:Reason:Stack ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stack),
            logger:error("Failed to broadcast notification: ~p:~p~n~p", [Class, Reason, Stack])
    after
        erlmcp_tracing:end_span(SpanCtx)
    end,
    ok.

%% @doc Send notification to individual client
-spec send_notification_to_client(pid(), binary(), map()) -> ok.
send_notification_to_client(ClientPid, Method, Params) ->
    try
        Notification = erlmcp_json_rpc:encode_notification(Method, Params),
        ClientPid ! {list_changed_notification, Method, Notification},
        ok
    catch
        Class:Reason ->
            logger:warning("Failed to send notification to client ~p: ~p:~p",
                          [ClientPid, Class, Reason]),
            ok
    end.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

-spec broadcast_via_notifier(server_id(), atom(), map(), pid() | undefined) -> ok.
broadcast_via_notifier(_ServerId, Feature, Params, undefined) ->
    % Notifier not available, try change_notifier
    case erlmcp_change_notifier:get_subscribers(Feature) of
        [] ->
            ok;
        Subscribers ->
            Method = ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED,
            notify_subscribers(Subscribers, Method, Params)
    end;
broadcast_via_notifier(_ServerId, Feature, Params, NotifierPid) when is_pid(NotifierPid) ->
    % Use provided notifier
    case erlmcp_change_notifier:get_subscribers(Feature) of
        [] ->
            ok;
        Subscribers ->
            Method = ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED,
            notify_subscribers(Subscribers, Method, Params)
    end.

-spec notify_subscribers([pid()], binary(), map()) -> ok.
notify_subscribers(Subscribers, Method, Params) ->
    lists:foreach(fun(Pid) ->
        send_notification_to_client(Pid, Method, Params)
    end, Subscribers),
    ok.

%% @doc Encode prompt metadata for notification
-spec encode_prompt_metadata(#mcp_prompt{}) -> map().
encode_prompt_metadata(Prompt) ->
    Base = #{
        <<"name">> => Prompt#mcp_prompt.name
    },

    % Add optional fields
    Base1 = case Prompt#mcp_prompt.description of
        undefined -> Base;
        Desc -> Base#{<<"description">> => Desc}
    end,

    case Prompt#mcp_prompt.arguments of
        undefined -> Base1;
        Args -> Base1#{<<"arguments">> => [encode_prompt_argument(Arg) || Arg <- Args]}
    end.

%% @doc Encode individual prompt argument
-spec encode_prompt_argument(#mcp_prompt_argument{}) -> map().
encode_prompt_argument(Arg) ->
    Base = #{
        <<"name">> => Arg#mcp_prompt_argument.name,
        <<"required">> => Arg#mcp_prompt_argument.required
    },

    case Arg#mcp_prompt_argument.description of
        undefined -> Base;
        Desc -> Base#{<<"description">> => Desc}
    end.
