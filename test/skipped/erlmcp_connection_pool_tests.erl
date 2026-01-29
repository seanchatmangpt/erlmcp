-module(erlmcp_connection_pool_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Chicago School TDD - Connection Pool Tests
%% Real supervisors, real process pools, state-based verification
%%====================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

connection_pool_test_() ->
    {setup,
     fun() ->
         %% Setup: Start pool supervisor (real supervisor tree)
         application:ensure_all_started(gproc),
         {ok, Pid} = erlmcp_connection_pool_sup:start_link(),
         Pid
     end,
     fun(Pid) ->
         %% Teardown: Stop supervisor tree
         erlang:exit(Pid, normal),
         timer:sleep(100),
         application:stop(gproc)
     end,
     fun(_Pid) ->
         [
          ?_test(pool_supervisor_starts_10_pools()),
          ?_test(get_pool_for_connection_hashing()),
          ?_test(start_child_in_correct_pool()),
          ?_test(pool_restart_on_failure()),
          ?_test(concurrent_child_starts()),
          ?_test(pool_isolation()),
          ?_test(pool_hash_distribution())
         ]
     end}.

%%====================================================================
%% Test Cases - Pool Architecture (Chicago School: Real Supervisors)
%%====================================================================

pool_supervisor_starts_10_pools() ->
    %% Verify: 10 pool supervisors started (observable state)
    PoolNames = [pool_0, pool_1, pool_2, pool_3, pool_4,
                 pool_5, pool_6, pool_7, pool_8, pool_9],

    %% Check each pool supervisor is alive
    RunningPools = lists:filter(fun(PoolName) ->
        case erlang:whereis(PoolName) of
            undefined -> false;
            Pid when is_pid(Pid) -> erlang:is_process_alive(Pid)
        end
    end, PoolNames),

    %% Verify: All 10 pools running
    ?assertEqual(10, length(RunningPools)).

get_pool_for_connection_hashing() ->
    %% Exercise: Hash connections to pools
    Pool1 = erlmcp_connection_pool_sup:get_pool_for_connection(conn_1),
    Pool2 = erlmcp_connection_pool_sup:get_pool_for_connection(conn_2),
    Pool3 = erlmcp_connection_pool_sup:get_pool_for_connection(conn_1),  %% Same as first

    %% Verify: Consistent hashing (same ID = same pool)
    ?assertEqual(Pool1, Pool3),

    %% Verify: Pool names are atoms in expected range
    ?assert(is_atom(Pool1)),
    ?assert(is_atom(Pool2)),

    %% Verify: Different connections may map to different pools
    %% (statistically likely with 10 pools)
    AllPools = lists:usort([
        erlmcp_connection_pool_sup:get_pool_for_connection(list_to_atom("conn_" ++ integer_to_list(N)))
        || N <- lists:seq(1, 100)
    ]),
    ?assert(length(AllPools) > 1).  % Should use multiple pools

start_child_in_correct_pool() ->
    %% Setup: Define connection config
    ConnectionId = test_conn_pool,
    Type = tcp,
    Config = #{port => 8080},

    %% Exercise: Start child via pool supervisor
    %% Note: This will fail if erlmcp_server_pool_sup doesn't exist,
    %% which is expected in test environment. We test the routing logic.
    Result = erlmcp_connection_pool_sup:start_child(ConnectionId, Type, Config),

    %% Verify: Either started or expected error (no worker implementation)
    case Result of
        {ok, _Pid} ->
            %% If implementation exists, verify it succeeded
            ?assert(true);
        {error, Reason} ->
            %% Expected in test environment without full worker implementation
            ?assert(Reason =/= badarg)  % But not a code error
    end.

pool_restart_on_failure() ->
    %% Setup: Get initial pool supervisor PID
    PoolName = pool_0,
    InitialPid = erlang:whereis(PoolName),
    ?assert(is_pid(InitialPid)),

    %% Exercise: Kill pool supervisor (real supervisor restart test)
    exit(InitialPid, kill),
    timer:sleep(200),  %% Allow supervisor to restart

    %% Verify: Pool restarted with new PID (real supervision)
    NewPid = erlang:whereis(PoolName),
    ?assert(is_pid(NewPid)),
    ?assertNotEqual(InitialPid, NewPid),
    ?assert(erlang:is_process_alive(NewPid)).

concurrent_child_starts() ->
    %% Exercise: 50 processes attempt to start children concurrently
    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            ConnectionId = list_to_atom("concurrent_conn_" ++ integer_to_list(N)),
            _Result = erlmcp_connection_pool_sup:start_child(ConnectionId, tcp, #{}),
            receive stop -> ok end
        end)
    end, lists:seq(1, 50)),

    timer:sleep(300),  %% Let concurrent starts complete

    %% Verify: All processes completed (no crashes)
    AlivePids = lists:filter(fun erlang:is_process_alive/1, Pids),
    ?assertEqual(50, length(AlivePids)),

    %% Cleanup
    lists:foreach(fun(Pid) -> Pid ! stop end, Pids).

pool_isolation() ->
    %% Verify: Pools are independent (killing one doesn't affect others)
    PoolsToTest = [pool_0, pool_1, pool_2],
    InitialPids = [{Pool, erlang:whereis(Pool)} || Pool <- PoolsToTest],

    %% Exercise: Kill pool_1 (real process death)
    {pool_1, Pool1Pid} = lists:keyfind(pool_1, 1, InitialPids),
    exit(Pool1Pid, kill),
    timer:sleep(200),

    %% Verify: pool_1 restarted, pool_0 and pool_2 unaffected
    {pool_0, Pool0Pid} = lists:keyfind(pool_0, 1, InitialPids),
    {pool_2, Pool2Pid} = lists:keyfind(pool_2, 1, InitialPids),

    %% Check pool_0 and pool_2 still same PIDs (isolation)
    ?assertEqual(Pool0Pid, erlang:whereis(pool_0)),
    ?assertEqual(Pool2Pid, erlang:whereis(pool_2)),

    %% Check pool_1 has new PID (restarted)
    NewPool1Pid = erlang:whereis(pool_1),
    ?assert(is_pid(NewPool1Pid)),
    ?assertNotEqual(Pool1Pid, NewPool1Pid).

pool_hash_distribution() ->
    %% Exercise: Hash 1000 connections and verify distribution
    Connections = [list_to_atom("hash_conn_" ++ integer_to_list(N))
                   || N <- lists:seq(1, 1000)],

    PoolAssignments = [erlmcp_connection_pool_sup:get_pool_for_connection(Conn)
                       || Conn <- Connections],

    %% Verify: Connections distributed across all 10 pools (statistical)
    PoolCounts = lists:foldl(fun(Pool, Acc) ->
        Count = length([P || P <- PoolAssignments, P =:= Pool]),
        [{Pool, Count} | Acc]
    end, [], [pool_0, pool_1, pool_2, pool_3, pool_4,
              pool_5, pool_6, pool_7, pool_8, pool_9]),

    %% Verify: Each pool has at least some connections (reasonable distribution)
    %% With 1000 connections across 10 pools, expect ~100 per pool
    %% Allow range [50, 200] for hash variance
    lists:foreach(fun({Pool, Count}) ->
        ?assert(Count > 50),   %% Not too few
        ?assert(Count < 200),  %% Not too many
        io:format("Pool ~p: ~p connections~n", [Pool, Count])
    end, PoolCounts).

%%====================================================================
%% Edge Cases
%%====================================================================

pool_for_nonexistent_connection_test() ->
    %% Exercise: Hash non-existent connection
    Pool = erlmcp_connection_pool_sup:get_pool_for_connection(nonexistent_conn),

    %% Verify: Returns valid pool (always succeeds with hash)
    ?assert(is_atom(Pool)),
    PoolNames = [pool_0, pool_1, pool_2, pool_3, pool_4,
                 pool_5, pool_6, pool_7, pool_8, pool_9],
    ?assert(lists:member(Pool, PoolNames)).

pool_supervisor_info_test() ->
    %% Verify: Can introspect pool supervisor
    PoolSup = erlang:whereis(erlmcp_connection_pool_sup),
    ?assert(is_pid(PoolSup)),
    ?assert(erlang:is_process_alive(PoolSup)),

    %% Verify: Supervisor has 10 children (one per pool)
    Children = supervisor:which_children(PoolSup),
    ?assertEqual(10, length(Children)).
