-module(erlmcp_gap25_resource_list_changed_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/erlmcp.hrl").

%% Tests for Gap #25: Resource List Changed Event Notifications
%% Verifies that resources/list_changed notifications are sent when resources are added/removed/updated.

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    %% Start application and required services
    application:start(erlmcp),
    {ok, ServerPid} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = true,
            listChanged = true
        }
    }),
    {ok, NotifierPid} = erlmcp_change_notifier:start_link(),
    {ServerPid, NotifierPid}.

cleanup({ServerPid, NotifierPid}) ->
    catch erlmcp_server:stop(ServerPid),
    catch erlmcp_change_notifier:stop(),
    application:stop(erlmcp).

%%====================================================================
%% Test Cases
%%====================================================================

resource_added_sends_notification_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun test_resource_added_sends_notification/1}.

test_resource_added_sends_notification({ServerPid, _NotifierPid}) ->
    % Subscribe to notifications
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, self()),

    % Add a resource
    Uri = <<"resource://test/file.txt">>,
    Handler = fun(_) -> <<"content">> end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    % Expect notification
    receive
        {list_changed_notification, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, Notification} ->
            % Decode and verify notification format
            case erlmcp_json_rpc:decode_message(Notification) of
                {ok, #json_rpc_notification{method = Method, params = Params}} ->
                    ?assertEqual(?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, Method),
                    ?assertMatch(#{<<"operation">> := <<"added">>}, Params),
                    ?assertMatch(#{<<"uri">> := Uri}, Params),
                    ?assertMatch(#{<<"resource">> := _}, Params);
                {error, _} ->
                    ?fail("Failed to decode notification")
            end;
        _Other ->
            ?fail("Expected list changed notification")
    after
        1000 ->
            ?fail("Timeout waiting for notification")
    end.

resource_removed_sends_notification_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun test_resource_removed_sends_notification/1}.

test_resource_removed_sends_notification({ServerPid, _NotifierPid}) ->
    % Subscribe to notifications
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, self()),

    % Add a resource first
    Uri = <<"resource://test/file.txt">>,
    Handler = fun(_) -> <<"content">> end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    % Clear the added notification
    receive
        {list_changed_notification, _, _} -> ok
    after
        500 -> ok
    end,

    % Remove the resource
    ok = erlmcp_server:remove_resource(ServerPid, Uri),

    % Expect removal notification
    receive
        {list_changed_notification, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, Notification} ->
            case erlmcp_json_rpc:decode_message(Notification) of
                {ok, #json_rpc_notification{method = Method, params = Params}} ->
                    ?assertEqual(?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, Method),
                    ?assertMatch(#{<<"operation">> := <<"removed">>}, Params),
                    ?assertMatch(#{<<"uri">> := Uri}, Params);
                {error, _} ->
                    ?fail("Failed to decode notification")
            end;
        _Other ->
            ?fail("Expected list changed notification")
    after
        1000 ->
            ?fail("Timeout waiting for notification")
    end.

resource_updated_sends_notification_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun test_resource_updated_sends_notification/1}.

test_resource_updated_sends_notification({ServerPid, _NotifierPid}) ->
    % Subscribe to notifications
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, self()),

    % Add a resource first
    Uri = <<"resource://test/file.txt">>,
    Handler = fun(_) -> <<"content">> end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    % Clear the added notification
    receive
        {list_changed_notification, _, _} -> ok
    after
        500 -> ok
    end,

    % Update the resource with new metadata
    UpdatedResource = #mcp_resource{
        uri = Uri,
        name = <<"Updated Name">>,
        description = <<"Updated description">>,
        mime_type = <<"text/markdown">>
    },
    ok = erlmcp_server:update_resource(ServerPid, Uri, UpdatedResource),

    % Expect update notification
    receive
        {list_changed_notification, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, Notification} ->
            case erlmcp_json_rpc:decode_message(Notification) of
                {ok, #json_rpc_notification{method = Method, params = Params}} ->
                    ?assertEqual(?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, Method),
                    ?assertMatch(#{<<"operation">> := <<"updated">>}, Params),
                    ?assertMatch(#{<<"uri">> := Uri}, Params),
                    ?assertMatch(#{<<"resource">> := _}, Params);
                {error, _} ->
                    ?fail("Failed to decode notification")
            end;
        _Other ->
            ?fail("Expected list changed notification")
    after
        1000 ->
            ?fail("Timeout waiting for notification")
    end.

notification_contains_operation_metadata_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun test_notification_contains_operation_metadata/1}.

test_notification_contains_operation_metadata({ServerPid, _NotifierPid}) ->
    % Subscribe to notifications
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, self()),

    % Add a resource with full metadata
    Uri = <<"resource://test/detailed.md">>,
    Handler = fun(_) -> <<"# Documentation\n\nContent here">> end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    % Receive and verify notification format
    receive
        {list_changed_notification, Method, Notification} ->
            ?assertEqual(?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, Method),
            case erlmcp_json_rpc:decode_message(Notification) of
                {ok, #json_rpc_notification{params = Params}} ->
                    % Verify required fields
                    ?assertMatch(#{<<"operation">> := <<"added">>}, Params),
                    ?assertMatch(#{<<"uri">> := Uri}, Params),
                    ?assertMatch(#{<<"resource">> := _}, Params),

                    % Verify resource metadata
                    Resource = maps:get(<<"resource">>, Params),
                    ?assertMatch(#{<<"uri">> := Uri}, Resource),
                    ?assertMatch(#{<<"name">> := Uri}, Resource),
                    ?assertMatch(#{<<"mimeType">> := _}, Resource);
                {error, _} ->
                    ?fail("Failed to decode notification")
            end;
        _Other ->
            ?fail("Expected list changed notification")
    after
        1000 ->
            ?fail("Timeout waiting for notification")
    end.

notification_format_is_jsonrpc_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun test_notification_format_is_jsonrpc/1}.

test_notification_format_is_jsonrpc({ServerPid, _NotifierPid}) ->
    % Subscribe to notifications
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, self()),

    % Add a resource
    Uri = <<"resource://test/file.txt">>,
    Handler = fun(_) -> <<"content">> end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    % Receive notification and verify JSON-RPC 2.0 format
    receive
        {list_changed_notification, Method, Notification} ->
            % Verify it's a valid JSON-RPC notification (no ID field)
            case erlmcp_json_rpc:decode_message(Notification) of
                {ok, #json_rpc_notification{method = DecodedMethod, params = _Params}} ->
                    ?assertEqual(?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, DecodedMethod),
                    % Verify notification has no ID (unlike requests)
                    NotificationBin = jsx:encode(jsx:decode(Notification)),
                    DecodedMap = jsx:decode(NotificationBin),
                    ?assertNot(maps:is_key(<<"id">>, DecodedMap)),
                    ?assertTrue(maps:is_key(<<"method">>, DecodedMap)),
                    ?assertTrue(maps:is_key(<<"params">>, DecodedMap));
                {error, _} ->
                    ?fail("Failed to decode notification")
            end;
        _Other ->
            ?fail("Expected list changed notification")
    after
        1000 ->
            ?fail("Timeout waiting for notification")
    end.

remove_nonexistent_resource_returns_error_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun test_remove_nonexistent_resource_returns_error/1}.

test_remove_nonexistent_resource_returns_error({ServerPid, _NotifierPid}) ->
    % Try to remove non-existent resource
    Uri = <<"resource://nonexistent/file.txt">>,
    Result = erlmcp_server:remove_resource(ServerPid, Uri),
    ?assertEqual({error, not_found}, Result).

update_nonexistent_resource_returns_error_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun test_update_nonexistent_resource_returns_error/1}.

test_update_nonexistent_resource_returns_error({ServerPid, _NotifierPid}) ->
    % Try to update non-existent resource
    Uri = <<"resource://nonexistent/file.txt">>,
    UpdatedResource = #mcp_resource{
        uri = Uri,
        name = <<"Updated">>,
        mime_type = <<"text/plain">>
    },
    Result = erlmcp_server:update_resource(ServerPid, Uri, UpdatedResource),
    ?assertEqual({error, not_found}, Result).

resource_with_description_in_notification_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun test_resource_with_description_in_notification/1}.

test_resource_with_description_in_notification({ServerPid, _NotifierPid}) ->
    % Subscribe to notifications
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, self()),

    % Add a resource with description
    Uri = <<"resource://test/described.txt">>,
    Handler = fun(_) -> <<"content">> end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    % Update with description
    DescribedResource = #mcp_resource{
        uri = Uri,
        name = Uri,
        description = <<"This is a test resource">>,
        mime_type = <<"text/plain">>
    },
    ok = erlmcp_server:update_resource(ServerPid, Uri, DescribedResource),

    % Skip the added notification
    receive {list_changed_notification, _, _} -> ok after 500 -> ok end,

    % Get update notification
    receive
        {list_changed_notification, _, Notification} ->
            case erlmcp_json_rpc:decode_message(Notification) of
                {ok, #json_rpc_notification{params = Params}} ->
                    Resource = maps:get(<<"resource">>, Params),
                    ?assertMatch(#{<<"description">> := <<"This is a test resource">>}, Resource);
                {error, _} ->
                    ?fail("Failed to decode notification")
            end;
        _Other ->
            ?fail("Expected list changed notification")
    after
        1000 ->
            ?fail("Timeout waiting for notification")
    end.

resource_with_metadata_in_notification_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun test_resource_with_metadata_in_notification/1}.

test_resource_with_metadata_in_notification({ServerPid, _NotifierPid}) ->
    % Subscribe to notifications
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, self()),

    % Add a resource with metadata
    Uri = <<"resource://test/withmetadata.txt">>,
    Handler = fun(_) -> <<"content">> end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    % Update with metadata
    Metadata = #{
        <<"author">> => <<"Test Author">>,
        <<"version">> => <<"1.0">>,
        <<"created">> => <<"2026-01-27T00:00:00Z">>
    },
    ResourceWithMetadata = #mcp_resource{
        uri = Uri,
        name = Uri,
        mime_type = <<"text/plain">>,
        metadata = Metadata
    },
    ok = erlmcp_server:update_resource(ServerPid, Uri, ResourceWithMetadata),

    % Skip the added notification
    receive {list_changed_notification, _, _} -> ok after 500 -> ok end,

    % Get update notification
    receive
        {list_changed_notification, _, Notification} ->
            case erlmcp_json_rpc:decode_message(Notification) of
                {ok, #json_rpc_notification{params = Params}} ->
                    Resource = maps:get(<<"resource">>, Params),
                    ?assertMatch(#{<<"metadata">> := _}, Resource),
                    NotifMetadata = maps:get(<<"metadata">>, Resource),
                    ?assertEqual(Metadata, NotifMetadata);
                {error, _} ->
                    ?fail("Failed to decode notification")
            end;
        _Other ->
            ?fail("Expected list changed notification")
    after
        1000 ->
            ?fail("Timeout waiting for notification")
    end.
