-module(erlmcp_distributed_lock_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup (Chicago School TDD: Real gen_server, no mocks)
%%%===================================================================

setup() ->
    {ok, Pid} = erlmcp_distributed_lock:start_link(),
    Pid.

cleanup(_Pid) ->
    ok = gen_server:stop(erlmcp_distributed_lock).

%%%===================================================================
%%% Test Generators
%%%===================================================================

erlmcp_distributed_lock_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun acquire_simple_lock_test/0,
      fun acquire_lock_with_ttl_test/0,
      fun acquire_lock_already_held_test/0,
      fun release_lock_test/0,
      fun status_test/0,
      fun priority_queue_test/0,
      fun concurrent_acquisition_test/0,
      fun lock_expiration_test/0,
      fun auto_extend_test/0,
      fun multiple_locks_test/0
     ]}.

%%%===================================================================
%%% Individual Tests (Chicago School: State-based, real processes)
%%%===================================================================

acquire_simple_lock_test() ->
    {"acquire obtains lock successfully", fun() ->
        %% Exercise: Acquire lock
        {ok, LockRef} = erlmcp_distributed_lock:acquire(<<"lock1">>),

        %% Verify: Lock acquired (reference returned)
        ?assert(is_reference(LockRef)),

        %% Verify: Lock status shows held
        {ok, Status} = erlmcp_distributed_lock:status(<<"lock1">>),
        ?assertEqual(locked, maps:get(status, Status)),
        ?assert(maps:is_key(owner, Status))
    end}.

acquire_lock_with_ttl_test() ->
    {"acquire with TTL sets expiration", fun() ->
        %% Exercise: Acquire lock with 100ms TTL
        Options = #{ttl_ms => 100},
        {ok, LockRef} = erlmcp_distributed_lock:acquire(<<"ttl_lock">>, Options),

        %% Verify: Lock acquired
        ?assert(is_reference(LockRef)),

        %% Verify: Lock has expiration
        {ok, Status} = erlmcp_distributed_lock:status(<<"ttl_lock">>),
        ?assert(maps:is_key(expires_at, Status)),
        ?assert(maps:get(expires_at, Status) > os:system_time(millisecond))
    end}.

acquire_lock_already_held_test() ->
    {"acquire returns error when lock held", fun() ->
        %% Setup: Acquire lock
        {ok, _LockRef1} = erlmcp_distributed_lock:acquire(<<"held_lock">>),

        %% Exercise: Try to acquire same lock from different process
        Waiter = spawn(fun() ->
            Result = erlmcp_distributed_lock:acquire(
                <<"held_lock">>,
                #{wait_timeout_ms => 100}
            ),
            self() ! {result, Result}
        end),

        %% Verify: Acquisition blocked
        receive
            {result, {error, timeout}} -> ok
        after 200 ->
            ct:fail("Expected timeout error")
        end
    end}.

release_lock_test() ->
    {"release frees lock for acquisition", fun() ->
        %% Setup: Acquire lock
        {ok, LockRef} = erlmcp_distributed_lock:acquire(<<"release_lock">>),

        %% Exercise: Release lock
        ok = erlmcp_distributed_lock:release(LockRef),

        %% Verify: Lock available
        {ok, _LockRef2} = erlmcp_distributed_lock:acquire(<<"release_lock">>)
    end}.

status_test() ->
    {"status returns lock information", fun() ->
        %% Setup: Acquire lock
        {ok, _LockRef} = erlmcp_distributed_lock:acquire(<<"status_lock">>),

        %% Exercise: Get status
        AllStatus = erlmcp_distributed_lock:status(),
        {ok, LockStatus} = erlmcp_distributed_lock:status(<<"status_lock">>),

        %% Verify: Status contains all fields
        ?assert(is_map(AllStatus)),
        ?assert(maps:is_key(<<"status_lock">>, AllStatus)),
        ?assert(maps:is_key(status, LockStatus)),
        ?assert(maps:is_key(owner, LockStatus)),
        ?assert(maps:is_key(acquired_at, LockStatus)),
        ?assert(maps:is_key(expires_at, LockStatus))
    end}.

priority_queue_test() ->
    {"acquire respects priority queue", fun() ->
        %% Setup: Acquire lock
        {ok, _LockRef} = erlmcp_distributed_lock:acquire(<<"priority_lock">>),

        %% Exercise: Queue multiple waiters with different priorities
        Waiters = [spawn(fun() ->
            Priority = N,
            Result = erlmcp_distributed_lock:acquire(
                <<"priority_lock">>,
                #{wait_timeout_ms => 5000, priority => Priority}
            ),
            self() ! {result, Priority, Result}
        end) || N <- [1, 3, 2]],

        %% Exercise: Release lock
        timer:sleep(100),
        {ok, LockRef} = erlmcp_distributed_lock:acquire(<<"priority_lock">>),
        ok = erlmcp_distributed_lock:release(LockRef),

        %% Verify: Highest priority waiter acquires first
        receive {result, 3, {ok, _}} -> ok after 1000 -> ct:fail("Priority not respected") end
    end}.

concurrent_acquisition_test() ->
    {"acquire handles concurrent requests", fun() ->
        %% Exercise: Multiple processes compete for lock
        Competitors = [spawn(fun() ->
            case erlmcp_distributed_lock:acquire(
                <<"concurrent_lock">>,
                #{wait_timeout_ms => 1000}
            ) of
                {ok, LockRef} ->
                    timer:sleep(50),
                    erlmcp_distributed_lock:release(LockRef),
                    self() ! {acquired, self()};
                {error, _} ->
                    self() ! {failed, self()}
            end
        end) || _ <- lists:seq(1, 10)],

        %% Wait for all to complete
        Acquired = length([receive {acquired, P} -> P after 2000 -> nil end || _ <- Competitors]),

        %% Verify: At least one acquired, no crashes
        ?assert(Acquired >= 1)
    end}.

lock_expiration_test() ->
    {"Lock expires after TTL", fun() ->
        %% Exercise: Acquire lock with short TTL
        {ok, LockRef} = erlmcp_distributed_lock:acquire(
            <<"expire_lock">>,
            #{ttl_ms => 100}
        ),

        %% Verify: Lock held initially
        {ok, Status1} = erlmcp_distributed_lock:status(<<"expire_lock">>),
        ?assertEqual(locked, maps:get(status, Status1)),

        %% Wait for expiration
        timer:sleep(150),

        %% Verify: Lock expired (available for acquisition)
        {ok, _LockRef2} = erlmcp_distributed_lock:acquire(<<"expire_lock">>)
    end}.

auto_extend_test() ->
    {"auto_extend keeps lock alive", fun() ->
        %% Exercise: Acquire lock with auto-extend
        Parent = self(),
        Worker = spawn(fun() ->
            {ok, LockRef} = erlmcp_distributed_lock:acquire(
                <<"auto_extend_lock">>,
                #{ttl_ms => 100, auto_extend => true}
            ),
            Parent ! {acquired, LockRef},
            timer:sleep(300),
            Parent ! {held, erlmcp_distributed_lock:release(LockRef)}
        end),

        receive {acquired, _LockRef} -> ok end,

        %% Verify: Lock still held after initial TTL
        timer:sleep(150),
        {ok, Status} = erlmcp_distributed_lock:status(<<"auto_extend_lock">>),
        ?assertEqual(locked, maps:get(status, Status)),

        receive {held, ok} -> ok end
    end}.

multiple_locks_test() ->
    {"Multiple locks can be held simultaneously", fun() ->
        %% Exercise: Acquire multiple different locks
        {ok, L1} = erlmcp_distributed_lock:acquire(<<"lock1">>),
        {ok, L2} = erlmcp_distributed_lock:acquire(<<"lock2">>),
        {ok, L3} = erlmcp_distributed_lock:acquire(<<"lock3">>),

        %% Verify: All locks acquired
        ?assert(is_reference(L1)),
        ?assert(is_reference(L2)),
        ?assert(is_reference(L3)),

        %% Verify: All locks show as held
        AllStatus = erlmcp_distributed_lock:status(),
        ?assertEqual(3, maps:size(AllStatus))
    end}.
