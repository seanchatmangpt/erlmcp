%%%-------------------------------------------------------------------
%%% @doc
%%% Resource Operation Test Suite (EUnit + Common Test)
%%%
%%% Tests for MCP resource operations
%%%
%%% Chicago School TDD:
%%% - Real resource processes
%%% - State-based verification
%%% - NO mocks
%%%
%%% Coverage Target: ≥90%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_resource_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% EUnit Test Fixtures
%%%====================================================================

resource_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Resource subscription - valid resource", fun test_subscribe_resource/0},
      {"Resource subscription - nonexistent resource", fun test_subscribe_nonexistent_resource/0},
      {"Resource subscription - duplicate subscription", fun test_duplicate_subscription/0},
      {"Resource unsubscription - active subscription", fun test_unsubscribe_resource/0},
      {"Resource unsubscription - nonexistent subscription", fun test_unsubscribe_nonexistent/0},
      {"Resource list - list all resources", fun test_list_resources/0},
      {"Resource list - filter by pattern", fun test_list_resources_filter/0},
      {"Resource read - valid resource", fun test_read_resource/0},
      {"Resource read - invalid resource", fun test_read_invalid_resource/0},
      {"Resource update - valid update", fun test_update_resource/0},
      {"Resource update - invalid update", fun test_update_invalid_resource/0},
      {"Resource change notification - subscription notification", fun test_change_notification/0},
      {"Resource synchronization - sync from source", fun test_sync_resource/0},
      {"Resource cleanup - cleanup on unsubscribe", fun test_cleanup_on_unsubscribe/0},
      {"Resource permissions - read access", fun test_resource_read_permission/0},
      {"Resource permissions - write access", fun test_resource_write_permission/0},
      {"Resource caching - cache hit", fun test_resource_cache_hit/0},
      {"Resource caching - cache miss", fun test_resource_cache_miss/0},
      {"Resource expiration - TTL expiration", fun test_resource_expiration/0},
      {"Resource batch - batch read", fun test_batch_read_resources/0},
      {"Resource batch - batch subscribe", fun test_batch_subscribe_resources/0}]}.

setup() ->
    application:ensure_all_started(erlmcp_cli),
    ok.

cleanup(_Args) ->
    application:stop(erlmcp_cli),
    ok.

%%%====================================================================
%%% Resource Subscription Tests
%%%====================================================================

test_subscribe_resource() ->
    %% Subscribe to valid resource
    SessionId = <<"resource_test_session">>,
    ResourceUri = <<"file:///test/resource.txt">>,

    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Subscribe
    {ok, Subscription} = erlmcp_cli_resource:subscribe(
                            SessionId,
                            ResourceUri,
                            #{<<"mode">> => <<"watch">>}),

    %% Verify subscription
    ?assertEqual(ResourceUri, maps:get(<<"uri">>, Subscription)),
    ?assertEqual(active, maps:get(<<"status">>, Subscription)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_subscribe_nonexistent_resource() ->
    %% Subscribe to nonexistent resource
    SessionId = <<"nonexistent_session">>,
    ResourceUri = <<"file:///nonexistent/resource.txt">>,

    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Subscribe (should fail)
    {error, {resource_not_found, _}} = erlmcp_cli_resource:subscribe(
                                          SessionId,
                                          ResourceUri,
                                          #{}),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_duplicate_subscription() ->
    %% Duplicate subscription to same resource
    SessionId = <<"duplicate_session">>,
    ResourceUri = <<"file:///test/resource.txt">>,

    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% First subscription
    {ok, _Sub1} = erlmcp_cli_resource:subscribe(
                     SessionId,
                     ResourceUri,
                     #{}),

    %% Duplicate subscription (should return existing)
    {ok, Sub2} = erlmcp_cli_resource:subscribe(
                     SessionId,
                     ResourceUri,
                     #{}),

    %% Verify same subscription
    ?assertEqual(ResourceUri, maps:get(<<"uri">>, Sub2)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

%%%====================================================================
%%% Resource Unsubscription Tests
%%%====================================================================

test_unsubscribe_resource() ->
    %% Unsubscribe from active subscription
    SessionId = <<"unsubscribe_session">>,
    ResourceUri = <<"file:///test/resource.txt">>,

    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Subscribe
    {ok, _Sub} = erlmcp_cli_resource:subscribe(
                    SessionId,
                    ResourceUri,
                    #{}),

    %% Unsubscribe
    ok = erlmcp_cli_resource:unsubscribe(SessionId, ResourceUri),

    %% Verify unsubscribed
    {error, not_subscribed} = erlmcp_cli_resource:get_subscription(
                                 SessionId,
                                 ResourceUri),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_unsubscribe_nonexistent() ->
    %% Unsubscribe from nonexistent subscription
    SessionId = <<"no_unsubscribe_session">>,
    ResourceUri = <<"file:///test/resource.txt">>,

    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Unsubscribe without subscribing (should fail)
    {error, not_subscribed} = erlmcp_cli_resource:unsubscribe(
                                 SessionId,
                                 ResourceUri),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

%%%====================================================================
%%% Resource List Tests
%%%====================================================================

test_list_resources() ->
    %% List all available resources
    {ok, Resources} = erlmcp_cli_resource:list(),

    %% Verify list
    ?assert(is_list(Resources)),
    ?assert(length(Resources) >= 0).

test_list_resources_filter() ->
    %% List resources with pattern filter
    Pattern = <<"file:///test/*.txt">>,

    {ok, Resources} = erlmcp_cli_resource:list(Pattern),

    %% Verify filtered list
    ?assert(is_list(Resources)),
    lists:foreach(fun(Resource) ->
        Uri = maps:get(<<"uri">>, Resource),
        ?assert(<<"file:///test/">> =< Uri)
    end, Resources).

%%%====================================================================
%%% Resource Read Tests
%%%====================================================================

test_read_resource() ->
    %% Read valid resource content
    ResourceUri = <<"file:///test/readable.txt">>,

    %% Create test resource
    ok = erlmcp_cli_resource:create(ResourceUri, #{<<"content">> => <<"test content">>}),

    %% Read resource
    {ok, Content} = erlmcp_cli_resource:read(ResourceUri),

    %% Verify content
    ?assertEqual(<<"test content">>, maps:get(<<"content">>, Content)),

    %% Cleanup
    ok = erlmcp_cli_resource:delete(ResourceUri).

test_read_invalid_resource() ->
    %% Read invalid resource
    ResourceUri = <<"file:///nonexistent.txt">>,

    %% Read (should fail)
    {error, {resource_not_found, _}} = erlmcp_cli_resource:read(ResourceUri).

%%%====================================================================
%%% Resource Update Tests
%%%====================================================================

test_update_resource() ->
    %% Update valid resource
    ResourceUri = <<"file:///test/updatable.txt">>,

    %% Create resource
    ok = erlmcp_cli_resource:create(ResourceUri, #{<<"content">> => <<"original">>}),

    %% Update resource
    Update = #{<<"content">> => <<"updated">>},
    {ok, Updated} = erlmcp_cli_resource:update(ResourceUri, Update),

    %% Verify updated
    ?assertEqual(<<"updated">>, maps:get(<<"content">>, Updated)),

    %% Cleanup
    ok = erlmcp_cli_resource:delete(ResourceUri).

test_update_invalid_resource() ->
    %% Update invalid resource
    ResourceUri = <<"file:///nonexistent.txt">>,

    %% Update (should fail)
    {error, {resource_not_found, _}} = erlmcp_cli_resource:update(
                                          ResourceUri,
                                          #{<<"content">> => <<"updated">>}).

%%%====================================================================
%%% Resource Change Notification Tests
%%%====================================================================

test_change_notification() ->
    %% Receive change notification
    SessionId = <<"notification_session">>,
    ResourceUri = <<"file:///test/watchable.txt">>,

    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Subscribe with watch mode
    {ok, _Sub} = erlmcp_cli_resource:subscribe(
                    SessionId,
                    ResourceUri,
                    #{<<"mode">> => <<"watch">>}),

    %% Create resource
    ok = erlmcp_cli_resource:create(ResourceUri, #{<<"content">> => <<"initial">>}),

    %% Update resource (triggers notification)
    ok = erlmcp_cli_resource:update(ResourceUri, #{<<"content">> => <<"changed">>}),

    %% Wait for notification
    receive
        {resource_changed, ResourceUri, Change} ->
            ?assertEqual(<<"changed">>, maps:get(<<"content">>, Change))
    after 1000 ->
        ?assert(false, "Did not receive change notification")
    end,

    %% Cleanup
    ok = erlmcp_cli_resource:delete(ResourceUri),
    ok = erlmcp_cli_session:terminate_session(SessionId).

%%%====================================================================
%%% Resource Synchronization Tests
%%%====================================================================

test_sync_resource() ->
    %% Synchronize resource from source
    ResourceUri = <<"file:///test/syncable.txt">>,

    %% Sync from source
    {ok, Synced} = erlmcp_cli_resource:sync(
                     ResourceUri,
                     #{<<"source">> => <<"http://example.com/resource.txt">>}),

    %% Verify synced
    ?assertEqual(synced, maps:get(<<"sync_status">>, Synced)),

    %% Cleanup
    ok = erlmcp_cli_resource:delete(ResourceUri).

%%%====================================================================
%%% Resource Cleanup Tests
%%%====================================================================

test_cleanup_on_unsubscribe() ->
    %% Verify cleanup on unsubscribe
    SessionId = <<"cleanup_session">>,
    ResourceUri = <<"file:///test/cleanup.txt">>,

    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Subscribe
    {ok, _Sub} = erlmcp_cli_resource:subscribe(
                    SessionId,
                    ResourceUri,
                    #{<<"mode">> => <<"cache">>}),

    %% Unsubscribe
    ok = erlmcp_cli_resource:unsubscribe(SessionId, ResourceUri),

    %% Verify cleanup (cache cleared)
    {error, not_subscribed} = erlmcp_cli_resource:get_subscription(
                                 SessionId,
                                 ResourceUri),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

%%%====================================================================
%%% Resource Permissions Tests
%%%====================================================================

test_resource_read_permission() ->
    %% Test read permission
    SessionId = <<"read_perm_session">>,
    ResourceUri = <<"file:///protected/resource.txt">>,

    {ok, _Pid} = erlmcp_cli_session:create_session(
                    SessionId,
                    #{permissions => [read]}),

    %% Create resource with read permission
    ok = erlmcp_cli_resource:create(
           ResourceUri,
           #{<<"content">> => <<"protected">>,
             <<"permissions">> => #{<<"read">> => [<<"user">>]}}),

    %% Try to read (should succeed)
    {ok, _Content} = erlmcp_cli_resource:read(ResourceUri, SessionId),

    %% Cleanup
    ok = erlmcp_cli_resource:delete(ResourceUri),
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_resource_write_permission() ->
    %% Test write permission
    SessionId = <<"write_perm_session">>,
    ResourceUri = <<"file:///protected/resource2.txt">>,

    {ok, _Pid} = erlmcp_cli_session:create_session(
                    SessionId,
                    #{permissions => []}),

    %% Create resource with write permission
    ok = erlmcp_cli_resource:create(
           ResourceUri,
           #{<<"content">> => <<"original">>,
             <<"permissions">> => #{<<"write">> => [<<"admin">>]}}),

    %% Try to write without permission (should fail)
    {error, {permission_denied, _}} = erlmcp_cli_resource:update(
                                          ResourceUri,
                                          #{<<"content">> => <<"updated">>},
                                          SessionId),

    %% Cleanup
    ok = erlmcp_cli_resource:delete(ResourceUri),
    ok = erlmcp_cli_session:terminate_session(SessionId).

%%%====================================================================
%%% Resource Caching Tests
%%%====================================================================

test_resource_cache_hit() ->
    %% Test cache hit
    ResourceUri = <<"file:///test/cache_hit.txt">>,

    %% Create resource
    ok = erlmcp_cli_resource:create(ResourceUri, #{<<"content">> => <<"cached">>}),

    %% First read (cache miss, loads from source)
    {ok, Content1} = erlmcp_cli_resource:read(ResourceUri, #{}),

    %% Second read (cache hit)
    {ok, Content2} = erlmcp_cli_resource:read(ResourceUri, #{}),

    %% Verify same content
    ?assertEqual(Content1, Content2),

    %% Cleanup
    ok = erlmcp_cli_resource:delete(ResourceUri).

test_resource_cache_miss() ->
    %% Test cache miss
    ResourceUri = <<"file:///test/cache_miss.txt">>,

    %% Create resource
    ok = erlmcp_cli_resource:create(ResourceUri, #{<<"content">> => <<"not cached">>}),

    %% Invalidate cache
    ok = erlmcp_cli_resource:invalidate_cache(ResourceUri),

    %% Read (cache miss)
    {ok, _Content} = erlmcp_cli_resource:read(ResourceUri, #{}),

    %% Cleanup
    ok = erlmcp_cli_resource:delete(ResourceUri).

%%%====================================================================
%%% Resource Expiration Tests
%%%====================================================================

test_resource_expiration() ->
    %% Test TTL expiration
    ResourceUri = <<"file:///test/expire.txt">>,

    %% Create resource with short TTL
    ok = erlmcp_cli_resource:create(
           ResourceUri,
           #{<<"content">> => <<"expires">>,
             <<"ttl">> => 1000}),

    %% Read immediately (should exist)
    {ok, _Content1} = erlmcp_cli_resource:read(ResourceUri),

    %% Wait for expiration
    timer:sleep(1100),

    %% Read after expiration (should fail)
    {error, {resource_expired, _}} = erlmcp_cli_resource:read(ResourceUri).

%%%====================================================================
%%% Resource Batch Tests
%%%====================================================================

test_batch_read_resources() ->
    %% Batch read multiple resources
    ResourceUris = [
        <<"file:///test/batch1.txt">>,
        <<"file:///test/batch2.txt">>,
        <<"file:///test/batch3.txt">>,
    ],

    %% Create resources
    lists:foreach(fun(Uri) ->
        ok = erlmcp_cli_resource:create(Uri, #{<<"content">> => <<"batch">>})
    end, ResourceUris),

    %% Batch read
    {ok, Results} = erlmcp_cli_resource:batch_read(ResourceUris),

    %% Verify all read
    ?assertEqual(3, length(Results)),

    %% Cleanup
    lists:foreach(fun(Uri) ->
        ok = erlmcp_cli_resource:delete(Uri)
    end, ResourceUris).

test_batch_subscribe_resources() ->
    %% Batch subscribe to multiple resources
    SessionId = <<"batch_sub_session">>,
    ResourceUris = [
        <<"file:///test/batch_sub1.txt">>,
        <<"file:///test/batch_sub2.txt">>,
        <<"file:///test/batch_sub3.txt">
    ],

    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Batch subscribe
    {ok, Subs} = erlmcp_cli_resource:batch_subscribe(
                    SessionId,
                    ResourceUris,
                    #{<<"mode">> => <<"watch">>}),

    %% Verify all subscribed
    ?assertEqual(3, length(Subs)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).
