-module(erlmcp_resource_list_changed).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% API
-export([
    notify_added/3,
    notify_removed/3,
    notify_updated/3,
    build_notification/3
]).

%% Types
-type operation() :: added | removed | updated.

-export_type([operation/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Notify that a resource was added to the server.
%% Sends a resources/list_changed notification with operation=added.
-spec notify_added(pid(), binary(), #mcp_resource{}) -> ok.
notify_added(ServerPid, Uri, Resource) when is_pid(ServerPid), is_binary(Uri) ->
    Notification = build_notification(added, Uri, Resource),
    send_notification_safe(ServerPid, Notification).

%% @doc Notify that a resource was removed from the server.
%% Sends a resources/list_changed notification with operation=removed.
-spec notify_removed(pid(), binary(), #mcp_resource{}) -> ok.
notify_removed(ServerPid, Uri, Resource) when is_pid(ServerPid), is_binary(Uri) ->
    Notification = build_notification(removed, Uri, Resource),
    send_notification_safe(ServerPid, Notification).

%% @doc Notify that a resource was updated in the server.
%% Sends a resources/list_changed notification with operation=updated.
-spec notify_updated(pid(), binary(), #mcp_resource{}) -> ok.
notify_updated(ServerPid, Uri, Resource) when is_pid(ServerPid), is_binary(Uri) ->
    Notification = build_notification(updated, Uri, Resource),
    send_notification_safe(ServerPid, Notification).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Build a resources/list_changed notification message.
%% Format follows JSON-RPC 2.0 notification structure with MCP-specific fields.
-spec build_notification(operation(), binary(), #mcp_resource{}) -> {Method :: binary(), Params :: map()}.
build_notification(Operation, Uri, Resource) when is_atom(Operation) ->
    Method = ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED,

    % Build notification parameters with operation and resource metadata
    Params = #{
        <<"operation">> => atom_to_binary(Operation, utf8),
        <<"uri">> => Uri,
        <<"resource">> => encode_resource_metadata(Resource)
    },

    {Method, Params}.

%% @doc Encode resource metadata for notification.
%% Includes name, description, MIME type, and other metadata.
-spec encode_resource_metadata(#mcp_resource{}) -> map().
encode_resource_metadata(#mcp_resource{} = Resource) ->
    Base = #{
        <<"uri">> => Resource#mcp_resource.uri,
        <<"name">> => Resource#mcp_resource.name
    },

    % Optionally add description if present
    Base1 = case Resource#mcp_resource.description of
        undefined -> Base;
        Desc -> Base#{<<"description">> => Desc}
    end,

    % Optionally add MIME type if present
    Base2 = case Resource#mcp_resource.mime_type of
        undefined -> Base1;
        MimeType -> Base1#{<<"mimeType">> => MimeType}
    end,

    % Optionally add metadata if present
    case Resource#mcp_resource.metadata of
        undefined -> Base2;
        Metadata when is_map(Metadata) -> Base2#{<<"metadata">> => Metadata};
        _ -> Base2
    end.

%% @doc Send notification to server safely (with error handling).
%% Sends a notification cast to the server process.
-spec send_notification_safe(pid(), {Method :: binary(), Params :: map()}) -> ok.
send_notification_safe(ServerPid, {Method, Params}) when is_pid(ServerPid) ->
    try
        SpanCtx = erlmcp_tracing:start_span(<<"resource_list_changed.send">>),
        try
            erlmcp_tracing:set_attributes(SpanCtx, #{
                <<"method">> => Method,
                <<"server_pid">> => erlmcp_tracing:pid_to_string(ServerPid)
            }),

            % Send notification to server to broadcast to all transports
            gen_server:cast(ServerPid, {resource_list_changed_notification, Method, Params}),

            erlmcp_tracing:set_status(SpanCtx, ok),
            ok
        after
            erlmcp_tracing:end_span(SpanCtx)
        end
    catch
        Class:Reason ->
            logger:warning("Failed to send resource list changed notification: ~p:~p",
                          [Class, Reason]),
            ok
    end.
