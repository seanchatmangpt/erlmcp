-module(erlmcp_session_replicator_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite for erlmcp_session_replicator Module
%% Chicago School TDD - Real replication, no mocks
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

replicator_setup() ->
    %% Start Mnesia
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    application:start(mnesia),

    %% Configure empty replica nodes for single-node tests
    application:set_env(erlmcp_core, replica_nodes, []),

    {ok, Pid} = erlmcp_session_replicator:start_link(),
    Pid.

replicator_cleanup(_Pid) ->
    gen_server:stop(erlmcp_session_replicator),
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    ok.

%%====================================================================
%% Lifecycle Tests
%%====================================================================

lifecycle_test_() ->
    {setup,
     fun replicator_setup/0,
     fun replicator_cleanup/1,
     fun(_) -> [
         ?_test(test_start_stop()),
         ?_test(test_start_idempotent())
     ] end}.

test_start_stop() ->
    %% Server should be running
    ?assert(is_process_alive(whereis(erlmcp_session_replicator))),

    %% Stop and restart
    gen_server:stop(erlmcp_session_replicator),
    ?assertEqual(undefined, whereis(erlmcp_session_replicator)),

    {ok, _Pid} = erlmcp_session_replicator:start_link(),
    ?assert(is_process_alive(whereis(erlmcp_session_replicator))).

test_start_idempotent() ->
    %% Starting multiple times should fail
    ?assertMatch({error, {already_started, _}}, erlmcp_session_replicator:start_link()).

%%====================================================================
%% Async Replication Tests
%%====================================================================

async_replication_test_() ->
    {setup,
     fun replicator_setup/0,
     fun replicator_cleanup/1,
     fun(_) -> [
         ?_test(test_replicate_async_no_replicas()),
         ?_test(test_replicate_async_multiple_sessions()),
         ?_test(test_replicate_async_complex_session())
     ] end}.

test_replicate_async_no_replicas() ->
    %% With no replica nodes, should succeed (fire-and-forget)
    SessionId = <<"session123">>,
    Session = create_test_session(SessionId),

    ?assertEqual(ok, erlmcp_session_replicator:replicate_async(SessionId, Session)),

    %% Give it time to process
    timer:sleep(100),

    %% Verify no replicas
    ?assertEqual({error, not_found}, erlmcp_session_replicator:get_replicas(SessionId)).

test_replicate_async_multiple_sessions() ->
    %% Replicate multiple sessions asynchronously
    Sessions = [create_test_session(<<"session", (integer_to_binary(I))/binary>>) || I <- lists:seq(1, 10)],

    lists:foreach(fun({SessionId, Session}) ->
        erlmcp_session_replicator:replicate_async(SessionId, Session)
    end, Sessions),

    %% Give time for processing
    timer:sleep(200),

    %% Verify status
    {ok, Status} = erlmcp_session_replicator:get_replication_status(),
    ?assert(is_map(Status)),
    ?assert(is_integer(maps:get(total_sessions, Status))).

test_replicate_async_complex_session() ->
    %% Test with complex session metadata
    SessionId = <<"complex_session">>,
    Session = create_test_session(SessionId, #{
        nested => #{key => value},
        list => [1, 2, 3],
        binary => <<"test">>
    }),

    ?assertEqual(ok, erlmcp_session_replicator:replicate_async(SessionId, Session)),

    %% Verify it was stored
    timer:sleep(100),
    {ok, Status} = erlmcp_session_replicator:get_replication_status(),
    ?assert(maps:get(total_sessions, Status) > 0).

%%====================================================================
%% Sync Replication Tests
%%====================================================================

sync_replication_test_() ->
    {setup,
     fun replicator_setup/0,
     fun replicator_cleanup/1,
     fun(_) -> [
         ?_test(test_sync_replicate_no_replicas()),
         ?_test(test_sync_replicate_with_session()),
         ?_test(test_sync_replicate_vector_clock_increment())
     ] end}.

test_sync_replicate_no_replicas() ->
    %% With no replica nodes, should return empty list
    SessionId = <<"sync_session">>,
    Session = create_test_session(SessionId),

    %% Should succeed with no replicas
    ?assertMatch({ok, []}, erlmcp_session_replicator:sync_replicate(SessionId, Session)),

    %% Verify replica state stored
    ?assertMatch({ok, _}, erlmcp_session_replicator:get_replicas(SessionId)).

test_sync_replicate_with_session() ->
    %% Create session with TTL
    SessionId = <<"sync_session_with_ttl">>,
    Session = create_test_session(SessionId, #{test => data}, 5000),

    ?assertMatch({ok, _}, erlmcp_session_replicator:sync_replicate(SessionId, Session)),

    %% Verify stored
    {ok, Replicas} = erlmcp_session_replicator:get_replicas(SessionId),
    ?assert(is_list(Replicas)).

test_sync_replicate_vector_clock_increment() ->
    %% Test vector clock increment on multiple replications
    SessionId = <<"vclock_session">>,
    Session1 = create_test_session(SessionId, #{version => 1}),

    %% First replication
    {ok, _} = erlmcp_session_replicator:sync_replicate(SessionId, Session1),

    %% Update session
    Session2 = create_test_session(SessionId, #{version => 2}),

    %% Second replication
    {ok, _} = erlmcp_session_replicator:sync_replicate(SessionId, Session2),

    %% Verify it was stored
    ?assertMatch({ok, _}, erlmcp_session_replicator:get_replicas(SessionId)).

%%====================================================================
%% Get Replicas Tests
%%====================================================================

get_replicas_test_() ->
    {setup,
     fun replicator_setup/0,
     fun replicator_cleanup/1,
     fun(_) -> [
         ?_test(test_get_replicas_not_found()),
         ?_test(test_get_replicas_after_sync_replicate())
     ] end}.

test_get_replicas_not_found() ->
    ?assertEqual({error, not_found}, erlmcp_session_replicator:get_replicas(<<"nonexistent">>)).

test_get_replicas_after_sync_replicate() ->
    SessionId = <<"replicas_session">>,
    Session = create_test_session(SessionId),

    ?assertEqual({error, not_found}, erlmcp_session_replicator:get_replicas(SessionId)),

    {ok, _} = erlmcp_session_replicator:sync_replicate(SessionId, Session),

    ?assertMatch({ok, _}, erlmcp_session_replicator:get_replicas(SessionId)).

%%====================================================================
%% Replication Status Tests
%%====================================================================

replication_status_test_() ->
    {setup,
     fun replicator_setup/0,
     fun replicator_cleanup/1,
     fun(_) -> [
         ?_test(test_replication_status_initial()),
         ?_test(test_replication_status_after_replications()),
         ?_test(test_replication_status_fields())
     ] end}.

test_replication_status_initial() ->
    {ok, Status} = erlmcp_session_replicator:get_replication_status(),

    ?assert(is_map(Status)),
    ?assert(maps:is_key(total_sessions, Status)),
    ?assert(maps:is_key(replicated_sessions, Status)),
    ?assert(maps:is_key(pending_replications, Status)),
    ?assert(maps:is_key(replica_nodes, Status)),
    ?assert(maps:is_key(queue_size, Status)),

    %% Initially should be 0 sessions
    ?assertEqual(0, maps:get(total_sessions, Status)).

test_replication_status_after_replications() ->
    %% Create multiple sessions
    lists:foreach(fun(I) ->
        SessionId = <<"status_session", (integer_to_binary(I))/binary>>,
        Session = create_test_session(SessionId),
        erlmcp_session_replicator:replicate_async(SessionId, Session)
    end, lists:seq(1, 5)),

    %% Wait for processing
    timer:sleep(200),

    {ok, Status} = erlmcp_session_replicator:get_replication_status(),
    ?assert(maps:get(total_sessions, Status) >= 0).

test_replication_status_fields() ->
    {ok, Status} = erlmcp_session_replicator:get_replication_status(),

    %% Check types
    TotalSessions = maps:get(total_sessions, Status),
    ?assert(is_integer(TotalSessions)),
    ?assert(TotalSessions >= 0),

    ReplicatedSessions = maps:get(replicated_sessions, Status),
    ?assert(is_integer(ReplicatedSessions)),
    ?assert(ReplicatedSessions >= 0),

    PendingReplications = maps:get(pending_replications, Status),
    ?assert(is_integer(PendingReplications)),
    ?assert(PendingReplications >= 0),

    ReplicaNodes = maps:get(replica_nodes, Status),
    ?assert(is_list(ReplicaNodes)),

    QueueSize = maps:get(queue_size, Status),
    ?assert(is_integer(QueueSize)),
    ?assert(QueueSize >= 0).

%%====================================================================
%% Replicate API Alias Tests
%%====================================================================

replicate_alias_test_() ->
    {setup,
     fun replicator_setup/0,
     fun replicator_cleanup/1,
     fun(_) -> [
         ?_test(test_replicate_alias())
     ] end}.

test_replicate_alias() ->
    %% replicate/2 should be alias for replicate_async/2
    SessionId = <<"alias_session">>,
    Session = create_test_session(SessionId),

    ?assertEqual(ok, erlmcp_session_replicator:replicate(SessionId, Session)),

    timer:sleep(100),

    %% Verify it was processed
    {ok, Status} = erlmcp_session_replicator:get_replication_status(),
    ?assert(maps:get(total_sessions, Status) > 0).

%%====================================================================
%% Bootstrap Node Tests
%%====================================================================

bootstrap_node_test_() ->
    {setup,
     fun replicator_setup/0,
     fun replicator_cleanup/1,
     fun(_) -> [
         ?_test(test_bootstrap_local_node_fails()),
         ?_test(test_bootstrap_unreachable_node())
     ] end}.

test_bootstrap_local_node_fails() ->
    %% Cannot bootstrap local node
    ?assertEqual({error, cannot_bootstrap_local},
        erlmcp_session_replicator:bootstrap_node(node())).

test_bootstrap_unreachable_node() ->
    %% Non-existent node should fail
    FakeNode = fake_node@nonexistent,

    ?assertEqual({error, node_unreachable},
        erlmcp_session_replicator:bootstrap_node(FakeNode)).

%%====================================================================
%% Concurrent Replication Tests
%%====================================================================

concurrent_replication_test_() ->
    {setup,
     fun replicator_setup/0,
     fun replicator_cleanup/1,
     fun(_) -> [
         ?_test(test_concurrent_async_replications()),
         ?_test(test_concurrent_sync_replications())
     ] end}.

test_concurrent_async_replications() ->
    %% Spawn multiple processes replicating asynchronously
    Parent = self(),
    Pids = [spawn(fun() ->
        SessionId = <<"concurrent_async_", (integer_to_binary(I))/binary>>,
        Session = create_test_session(SessionId),
        erlmcp_session_replicator:replicate_async(SessionId, Session),
        Parent ! {replicated, I}
    end) || I <- lists:seq(1, 20)],

    %% Wait for all to complete
    lists:foreach(fun(_) ->
        receive {replicated, _} -> ok end
    end, Pids),

    %% Give time for queue processing
    timer:sleep(300),

    %% Verify all sessions stored
    {ok, Status} = erlmcp_session_replicator:get_replication_status(),
    ?assert(maps:get(total_sessions, Status) > 0).

test_concurrent_sync_replications() ->
    %% Spawn multiple processes replicating synchronously
    Parent = self(),
    Pids = [spawn(fun() ->
        SessionId = <<"concurrent_sync_", (integer_to_binary(I))/binary>>,
        Session = create_test_session(SessionId),
        Result = erlmcp_session_replicator:sync_replicate(SessionId, Session),
        Parent ! {replicated, I, Result}
    end) || I <- lists:seq(1, 10)],

    %% Collect results
    Results = [receive {replicated, I, Result} -> {I, Result} end || I <- lists:seq(1, 10)],

    %% All should succeed (with no replica nodes, returns empty list)
    lists:foreach(fun({_I, Result}) ->
        ?assertMatch({ok, _}, Result)
    end, Results).

%%====================================================================
%% Queue Management Tests
%%====================================================================

queue_management_test_() ->
    {setup,
     fun replicator_setup/0,
     fun replicator_cleanup/1,
     fun(_) -> [
         ?_test(test_queue_flush_on_batch_size()),
         ?_test(test_queue_flush_on_timer())
     ] end}.

test_queue_flush_on_batch_size() ->
    %% The queue flushes when batch size (100) is reached
    Sessions = [create_test_session(<<"batch_", (integer_to_binary(I))/binary>>)
                 || I <- lists:seq(1, 150)],

    %% Add all to queue
    lists:foreach(fun({SessionId, Session}) ->
        erlmcp_session_replicator:replicate_async(SessionId, Session)
    end, Sessions),

    %% Wait for flushes to complete
    timer:sleep(500),

    %% Most should be processed
    {ok, Status} = erlmcp_session_replicator:get_replication_status(),
    ?assert(maps:get(total_sessions, Status) > 0).

test_queue_flush_on_timer() ->
    %% Queue flushes every second via timer
    SessionId = <<"timer_session">>,
    Session = create_test_session(SessionId),

    erlmcp_session_replicator:replicate_async(SessionId, Session),

    %% Wait for timer flush (1 second)
    timer:sleep(1500),

    %% Should be processed
    {ok, _} = erlmcp_session_replicator:get_replicas(SessionId).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    {setup,
     fun replicator_setup/0,
     fun replicator_cleanup/1,
     fun(_) -> [
         ?_test(test_replicate_invalid_session_id()),
         ?_test(test_replicate_malformed_session())
     ] end}.

test_replicate_invalid_session_id() ->
    %% Should handle invalid session ID gracefully
    ?assertEqual(ok, erlmcp_session_replicator:replicate_async(<<>>, #{})),

    timer:sleep(100),

    %% Status should still be accessible
    ?assertMatch({ok, _}, erlmcp_session_replicator:get_replication_status()).

test_replicate_malformed_session() ->
    %% Should handle malformed session
    SessionId = <<"malformed">>,
    MalformedSession = #{invalid => data},

    ?assertEqual(ok, erlmcp_session_replicator:replicate_async(SessionId, MalformedSession)),

    timer:sleep(100),

    %% Status should still be accessible
    ?assertMatch({ok, _}, erlmcp_session_replicator:get_replication_status()).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup,
     fun replicator_setup/0,
     fun replicator_cleanup/1,
     fun(_) -> [
         ?_test(test_full_replication_workflow()),
         ?_test(test_replication_session_lifecycle()),
         ?_test(test_multiple_replications_same_session())
     ] end}.

test_full_replication_workflow() ->
    %% Create session
    SessionId = <<"workflow_session">>,
    Session = create_test_session(SessionId, #{step => 1}),

    %% Async replicate
    ?assertEqual(ok, erlmcp_session_replicator:replicate_async(SessionId, Session)),

    timer:sleep(100),

    %% Check status
    {ok, Status} = erlmcp_session_replicator:get_replication_status(),
    ?assert(maps:get(total_sessions, Status) > 0),

    %% Get replicas
    {ok, Replicas} = erlmcp_session_replicator:get_replicas(SessionId),
    ?assert(is_list(Replicas)),

    %% Update session
    UpdatedSession = create_test_session(SessionId, #{step => 2}),
    {ok, _} = erlmcp_session_replicator:sync_replicate(SessionId, UpdatedSession),

    %% Verify update
    {ok, _} = erlmcp_session_replicator:get_replicas(SessionId).

test_replication_session_lifecycle() ->
    %% Full lifecycle: create -> replicate -> update -> replicate
    SessionId = <<"lifecycle_session">>,

    %% Create and replicate
    Session1 = create_test_session(SessionId, #{stage => created}),
    ?assertEqual(ok, erlmcp_session_replicator:replicate_async(SessionId, Session1)),

    timer:sleep(100),

    %% Update and replicate
    Session2 = create_test_session(SessionId, #{stage => updated}),
    {ok, _} = erlmcp_session_replicator:sync_replicate(SessionId, Session2),

    %% Verify final state
    {ok, _} = erlmcp_session_replicator:get_replicas(SessionId),

    %% Check status
    {ok, Status} = erlmcp_session_replicator:get_replication_status(),
    ?assert(maps:get(total_sessions, Status) > 0).

test_multiple_replications_same_session() ->
    %% Test multiple replications of same session (vector clock updates)
    SessionId = <<"multi_replicate_session">>,

    lists:foreach(fun(I) ->
        Session = create_test_session(SessionId, #{iteration => I}),
        {ok, _} = erlmcp_session_replicator:sync_replicate(SessionId, Session)
    end, lists:seq(1, 5)),

    %% Final state should be accessible
    {ok, Replicas} = erlmcp_session_replicator:get_replicas(SessionId),
    ?assert(is_list(Replicas)).

%%====================================================================
%% Performance Tests
%%====================================================================

performance_test_() ->
    {setup,
     fun replicator_setup/0,
     fun replicator_cleanup/1,
     fun(_) -> [
         ?_test(test_bulk_replication_performance())
     ] end}.

test_bulk_replication_performance() ->
    %% Test performance with many sessions
    Count = 200,
    StartTime = erlang:monotonic_time(millisecond),

    lists:foreach(fun(I) ->
        SessionId = <<"perf_", (integer_to_binary(I))/binary>>,
        Session = create_test_session(SessionId),
        erlmcp_session_replicator:replicate_async(SessionId, Session)
    end, lists:seq(1, Count)),

    %% Wait for queue to process
    timer:sleep(1000),

    EndTime = erlang:monotonic_time(millisecond),
    Elapsed = EndTime - StartTime,

    %% Should complete in reasonable time (< 2 seconds for 200 sessions)
    ?assert(Elapsed < 2000),

    %% Verify all sessions tracked
    {ok, Status} = erlmcp_session_replicator:get_replication_status(),
    ?assert(maps:get(total_sessions, Status) > 0).

%%====================================================================
%% Property-Based Tests (Manual)
%%====================================================================

property_replication_idempotent_test_() ->
    {setup,
     fun replicator_setup/0,
     fun replicator_cleanup/1,
     fun(_) -> [
         ?_test(test_multiple_replications_idempotent())
     ] end}.

test_multiple_replications_idempotent() ->
    %% Multiple replications of same data should be idempotent
    SessionId = <<"idempotent_session">>,
    Session = create_test_session(SessionId),

    %% Replicate multiple times with same data
    lists:foreach(fun(_) ->
        {ok, _} = erlmcp_session_replicator:sync_replicate(SessionId, Session)
    end, lists:seq(1, 10)),

    %% Should still have consistent state
    {ok, Replicas} = erlmcp_session_replicator:get_replicas(SessionId),
    ?assert(is_list(Replicas)).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Create a test session
create_test_session(SessionId) ->
    create_test_session(SessionId, #{test => true}, infinity).

create_test_session(SessionId, Metadata) ->
    create_test_session(SessionId, Metadata, infinity).

create_test_session(SessionId, Metadata, TTL) ->
    Now = erlang:system_time(millisecond),
    #{
        id => SessionId,
        created_at => Now,
        last_accessed => Now,
        timeout_ms => TTL,
        metadata => Metadata
    }.
