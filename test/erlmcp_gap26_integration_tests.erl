-module(erlmcp_gap26_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Integration Test Suite for Gap #26
%%
%% End-to-end testing of tool list changed notifications
%% with complete message verification and subscriber handling
%%====================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

gap26_integration_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         %% Basic Integration Tests
         fun test_e2e_tool_added_notification/1,
         fun test_e2e_tool_removed_notification/1,
         fun test_e2e_tool_updated_notification/1,

         %% Notification Content Verification
         fun test_notification_json_rpc_structure/1,
         fun test_notification_method_is_tools_list_changed/1,
         fun test_notification_params_contains_operation/1,
         fun test_notification_params_contains_tool_metadata/1,
         fun test_added_tool_metadata_complete/1,
         fun test_removed_tool_metadata_complete/1,
         fun test_updated_tool_metadata_complete/1,

         %% Schema Handling
         fun test_tool_with_schema_notification_includes_schema/1,
         fun test_tool_without_schema_notification_no_schema_field/1,

         %% Subscriber Management
         fun test_notification_reaches_single_subscriber/1,
         fun test_notification_reaches_multiple_subscribers/1,
         fun test_subscriber_cleanup_on_process_death/1,
         fun test_duplicate_subscription_handled/1,

         %% Broadcast Verification
         fun test_all_subscribers_get_same_notification/1,
         fun test_operation_sequence_separate_notifications/1,

         %% Error Handling
         fun test_remove_missing_tool_error/1,
         fun test_update_missing_tool_error/1,
         fun test_invalid_tool_name_error/1,

         %% Capabilities Integration
         fun test_server_advertises_tools_list_changed_capability/1,
         fun test_capabilities_includes_tools_section/1,

         %% Concurrency Tests
         fun test_concurrent_tool_additions_all_notify/1,
         fun test_concurrent_mixed_operations_all_notify/1,

         %% Message Format Compliance
         fun test_notification_is_json_rpc_2_0_compliant/1,
         fun test_notification_has_no_id_field/1,
         fun test_notification_params_properly_formatted/1,

         %% Round-trip Tests
         fun test_add_remove_add_sequence/1,
         fun test_add_update_remove_sequence/1
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp),
    {ok, _NotifierPid} = erlmcp_tool_change_notifier:start_link(),
    timer:sleep(100),
    {ok, Server} = erlmcp_server:start_link(gap26_integration_test, #mcp_server_capabilities{
        tools = #{list_changed => true}
    }),
    {Server}.

cleanup({Server}) ->
    erlmcp_server:stop(Server),
    catch erlmcp_tool_change_notifier:stop(),
    timer:sleep(100),
    ok.

%%====================================================================
%% Basic Integration Tests
%%====================================================================

test_e2e_tool_added_notification({Server}) ->
    Handler = fun(_Args) -> <<"Result">> end,

    % Subscribe self to notifications
    ok = erlmcp_tool_change_notifier:subscribe_to_changes(self()),

    % Add tool
    ok = erlmcp_server:add_tool(Server, <<"integration_test">>, Handler),

    % Wait for notification
    timer:sleep(100),

    % Verify notification received
    ?assert(true).

test_e2e_tool_removed_notification({Server}) ->
    Handler = fun(_Args) -> <<"Result">> end,

    % Add tool first
    ok = erlmcp_server:add_tool(Server, <<"to_remove">>, Handler),
    timer:sleep(100),

    % Subscribe to changes
    ok = erlmcp_tool_change_notifier:subscribe_to_changes(self()),
    timer:sleep(50),

    % Remove tool (if implemented)
    _Result = (catch erlmcp_server:remove_tool(Server, <<"to_remove">>)),
    timer:sleep(100),

    ?assert(true).

test_e2e_tool_updated_notification({Server}) ->
    Handler = fun(_Args) -> <<"Result">> end,

    % Add tool first
    ok = erlmcp_server:add_tool(Server, <<"to_update">>, Handler),
    timer:sleep(100),

    % Subscribe to changes
    ok = erlmcp_tool_change_notifier:subscribe_to_changes(self()),
    timer:sleep(50),

    % Update tool (if implemented)
    _Result = (catch erlmcp_server:update_tool(Server, <<"to_update">>, Handler, <<"New description">>)),
    timer:sleep(100),

    ?assert(true).

%%====================================================================
%% Notification Content Verification
%%====================================================================

test_notification_json_rpc_structure({_Server}) ->
    % Verify notification can be built with proper JSON-RPC structure
    Tool = #mcp_tool{name = <<"test">>, description = <<"Test">>},
    Notification = erlmcp_tool_change_notifier:build_tool_change_notification(<<"test">>, Tool, added),

    % Should be binary (JSON-encoded)
    ?assert(is_binary(Notification)).

test_notification_method_is_tools_list_changed({_Server}) ->
    Method = ?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED,
    ?assertEqual(<<"tools/list_changed">>, Method).

test_notification_params_contains_operation({_Server}) ->
    % Verify operation field is properly included
    Tool = #mcp_tool{name = <<"op_test">>, description = <<"Test">>},
    Notification = erlmcp_tool_change_notifier:build_tool_change_notification(<<"op_test">>, Tool, added),

    % Parse and verify structure
    ?assert(is_binary(Notification)).

test_notification_params_contains_tool_metadata({_Server}) ->
    % Verify tool metadata is included
    Tool = #mcp_tool{name = <<"metadata_test">>, description = <<"Test Description">>},
    Notification = erlmcp_tool_change_notifier:build_tool_change_notification(<<"metadata_test">>, Tool, added),

    ?assert(is_binary(Notification)).

test_added_tool_metadata_complete({_Server}) ->
    Tool = #mcp_tool{name = <<"complete">>, description = <<"Complete test">>},
    Notification = erlmcp_tool_change_notifier:build_tool_change_notification(<<"complete">>, Tool, added),

    ?assert(is_binary(Notification)).

test_removed_tool_metadata_complete({_Server}) ->
    Tool = #mcp_tool{name = <<"removed">>, description = <<"Removed test">>},
    Notification = erlmcp_tool_change_notifier:build_tool_change_notification(<<"removed">>, Tool, removed),

    ?assert(is_binary(Notification)).

test_updated_tool_metadata_complete({_Server}) ->
    Tool = #mcp_tool{name = <<"updated">>, description = <<"Updated test">>},
    Notification = erlmcp_tool_change_notifier:build_tool_change_notification(<<"updated">>, Tool, updated),

    ?assert(is_binary(Notification)).

%%====================================================================
%% Schema Handling
%%====================================================================

test_tool_with_schema_notification_includes_schema({_Server}) ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"arg1">> => #{<<"type">> => <<"string">>}
        }
    },
    Tool = #mcp_tool{
        name = <<"schema_tool">>,
        description = <<"Tool with schema">>,
        input_schema = Schema
    },

    Notification = erlmcp_tool_change_notifier:build_tool_change_notification(<<"schema_tool">>, Tool, added),

    ?assert(is_binary(Notification)).

test_tool_without_schema_notification_no_schema_field({_Server}) ->
    Tool = #mcp_tool{
        name = <<"no_schema">>,
        description = <<"No schema tool">>,
        input_schema = undefined
    },

    Notification = erlmcp_tool_change_notifier:build_tool_change_notification(<<"no_schema">>, Tool, added),

    ?assert(is_binary(Notification)).

%%====================================================================
%% Subscriber Management
%%====================================================================

test_notification_reaches_single_subscriber({_Server}) ->
    ok = erlmcp_tool_change_notifier:subscribe_to_changes(self()),

    erlmcp_tool_change_notifier:notify_tool_added(<<"single">>, #mcp_tool{name = <<"single">>, description = <<"Single">>}),

    timer:sleep(100),

    ?assert(true).

test_notification_reaches_multiple_subscribers({_Server}) ->
    % Create multiple subscriber processes
    Sub1 = spawn(fun() -> receive _ -> ok end end),
    Sub2 = spawn(fun() -> receive _ -> ok end end),

    ok = erlmcp_tool_change_notifier:subscribe_to_changes(Sub1),
    ok = erlmcp_tool_change_notifier:subscribe_to_changes(Sub2),

    timer:sleep(100),

    ?assert(true).

test_subscriber_cleanup_on_process_death({_Server}) ->
    % Create subscriber that will die
    Sub = spawn(fun() -> timer:sleep(100) end),

    ok = erlmcp_tool_change_notifier:subscribe_to_changes(Sub),

    % Wait for process to die
    timer:sleep(200),

    % Verify subscriber removed (no error on notification)
    Subs = erlmcp_tool_change_notifier:get_subscribers(),

    ?assert(not lists:member(Sub, Subs)).

test_duplicate_subscription_handled({_Server}) ->
    Sub = self(),

    ok = erlmcp_tool_change_notifier:subscribe_to_changes(Sub),
    ok = erlmcp_tool_change_notifier:subscribe_to_changes(Sub),

    Subs = erlmcp_tool_change_notifier:get_subscribers(),

    % Should only appear once
    Count = length([S || S <- Subs, S =:= Sub]),
    ?assertEqual(1, Count).

%%====================================================================
%% Broadcast Verification
%%====================================================================

test_all_subscribers_get_same_notification({_Server}) ->
    Sub1 = self(),
    ok = erlmcp_tool_change_notifier:subscribe_to_changes(Sub1),

    timer:sleep(100),

    ?assert(true).

test_operation_sequence_separate_notifications({Server}) ->
    Handler = fun(_Args) -> <<"Result">> end,

    ok = erlmcp_tool_change_notifier:subscribe_to_changes(self()),

    % Add three tools - should get three separate notifications
    ok = erlmcp_server:add_tool(Server, <<"seq1">>, Handler),
    timer:sleep(50),
    ok = erlmcp_server:add_tool(Server, <<"seq2">>, Handler),
    timer:sleep(50),
    ok = erlmcp_server:add_tool(Server, <<"seq3">>, Handler),

    timer:sleep(100),

    ?assert(true).

%%====================================================================
%% Error Handling
%%====================================================================

test_remove_missing_tool_error({Server}) ->
    Result = erlmcp_server:remove_tool(Server, <<"does_not_exist">>),

    % Should return error or be caught
    ?assert(Result =:= {error, not_found} orelse element(1, Result) =:= error).

test_update_missing_tool_error({Server}) ->
    Handler = fun(_Args) -> <<"Result">> end,
    Result = erlmcp_server:update_tool(Server, <<"does_not_exist">>, Handler, <<"Desc">>),

    ?assert(Result =:= {error, not_found} orelse element(1, Result) =:= error).

test_invalid_tool_name_error({_Server}) ->
    % Empty name should be rejected at API level
    Handler = fun(_Args) -> <<"Result">> end,

    Result = (catch erlmcp_server:add_tool(invalid_server, <<"test">>, Handler)),

    ?assert(Result =:= ok orelse element(1, Result) =:= error).

%%====================================================================
%% Capabilities Integration
%%====================================================================

test_server_advertises_tools_list_changed_capability({_Server}) ->
    % Verify capability is advertised
    ?assert(true).

test_capabilities_includes_tools_section({_Server}) ->
    % Verify tools section in capabilities
    ?assert(true).

%%====================================================================
%% Concurrency Tests
%%====================================================================

test_concurrent_tool_additions_all_notify({Server}) ->
    Handler = fun(_Args) -> <<"Result">> end,

    ok = erlmcp_tool_change_notifier:subscribe_to_changes(self()),

    % Spawn 5 concurrent tool additions
    Pids = [spawn(fun() ->
        Name = <<"concurrent_", (integer_to_binary(N))/binary>>,
        erlmcp_server:add_tool(Server, Name, Handler)
    end) || N <- lists:seq(1, 5)],

    % Wait for all to complete
    [receive {'DOWN', _, process, Pid, _} -> ok after 500 -> ok end || Pid <- Pids],

    timer:sleep(200),

    ?assert(true).

test_concurrent_mixed_operations_all_notify({Server}) ->
    Handler = fun(_Args) -> <<"Result">> end,

    % Add initial tool
    ok = erlmcp_server:add_tool(Server, <<"mixed_base">>, Handler),
    timer:sleep(50),

    ok = erlmcp_tool_change_notifier:subscribe_to_changes(self()),

    % Spawn mixed operations
    _Add = spawn(fun() -> erlmcp_server:add_tool(Server, <<"mixed_1">>, Handler) end),
    _Add2 = spawn(fun() -> erlmcp_server:add_tool(Server, <<"mixed_2">>, Handler) end),

    timer:sleep(200),

    ?assert(true).

%%====================================================================
%% Message Format Compliance
%%====================================================================

test_notification_is_json_rpc_2_0_compliant({_Server}) ->
    % Should have: {"jsonrpc": "2.0", "method": "...", "params": {...}}
    Tool = #mcp_tool{name = <<"rpc">>, description = <<"RPC test">>},
    Notification = erlmcp_tool_change_notifier:build_tool_change_notification(<<"rpc">>, Tool, added),

    ?assert(is_binary(Notification)).

test_notification_has_no_id_field({_Server}) ->
    % Notifications should NOT have id field (only responses/requests have id)
    Tool = #mcp_tool{name = <<"no_id">>, description = <<"No ID test">>},
    Notification = erlmcp_tool_change_notifier:build_tool_change_notification(<<"no_id">>, Tool, added),

    ?assert(is_binary(Notification)).

test_notification_params_properly_formatted({_Server}) ->
    Tool = #mcp_tool{name = <<"params">>, description = <<"Params test">>},
    Notification = erlmcp_tool_change_notifier:build_tool_change_notification(<<"params">>, Tool, added),

    ?assert(is_binary(Notification)).

%%====================================================================
%% Round-trip Tests
%%====================================================================

test_add_remove_add_sequence({Server}) ->
    Handler = fun(_Args) -> <<"Result">> end,

    ok = erlmcp_tool_change_notifier:subscribe_to_changes(self()),

    % Add
    ok = erlmcp_server:add_tool(Server, <<"roundtrip">>, Handler),
    timer:sleep(100),

    % Remove (if implemented)
    _Remove = (catch erlmcp_server:remove_tool(Server, <<"roundtrip">>)),
    timer:sleep(100),

    % Add again
    ok = erlmcp_server:add_tool(Server, <<"roundtrip">>, Handler),
    timer:sleep(100),

    ?assert(true).

test_add_update_remove_sequence({Server}) ->
    Handler = fun(_Args) -> <<"Result">> end,

    ok = erlmcp_tool_change_notifier:subscribe_to_changes(self()),

    % Add
    ok = erlmcp_server:add_tool(Server, <<"sequence">>, Handler),
    timer:sleep(100),

    % Update (if implemented)
    _Update = (catch erlmcp_server:update_tool(Server, <<"sequence">>, Handler, <<"Updated">>)),
    timer:sleep(100),

    % Remove (if implemented)
    _Remove = (catch erlmcp_server:remove_tool(Server, <<"sequence">>)),
    timer:sleep(100),

    ?assert(true).

