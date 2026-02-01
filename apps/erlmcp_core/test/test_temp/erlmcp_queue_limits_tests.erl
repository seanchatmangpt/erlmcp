-module(erlmcp_queue_limits_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%% ====================================================================
%%% Test Setup and Teardown
%%% ====================================================================

queue_limits_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(State) ->
        [test_start_stop(State),
         test_check_capacity(State),
         test_capacity_exceeded(State),
         test_enqueue_dequeue(State),
         test_role_limits(State),
         test_dynamic_limit_adjustment(State),
         test_queue_depth_tracking(State),
         test_statistics_collection(State),
         test_process_monitoring(State),
         test_backpressure_shedding(State)]
     end}.

setup() ->
    %% Start queue limits server
    {ok, Pid} = erlmcp_queue_limits:start_link(),
    #{queue_limits_pid => Pid}.

teardown(#{queue_limits_pid := Pid}) ->
    %% Stop queue limits server
    exit(Pid, normal),
    timer:sleep(100),
    ok.

%%% ====================================================================
%%% Test Cases
%%% ====================================================================

test_start_stop(_State) ->
    {"Start and stop queue limits server",
     fun() ->
        ?assertMatch({ok, _Pid}, erlmcp_queue_limits:start_link()),
        Limits = erlmcp_queue_limits:get_all_limits(),
        ?assertMatch(#{session := 10000}, Limits),
        ?assertMatch(#{sse_stream := 5000}, Limits),
        ?assertMatch(#{task_worker := 1000}, Limits),
        ?assertMatch(#{tool_executor := 500}, Limits)
     end}.

test_check_capacity(_State) ->
    {"Check capacity before queuing work",
     fun() ->
        %% Should be OK initially
        ?assertEqual(ok, erlmcp_queue_limits:check_capacity(session, self())),
        ?assertEqual(ok, erlmcp_queue_limits:check_capacity(task_worker, self()))
     end}.

test_capacity_exceeded(_State) ->
    {"Capacity exceeded returns error",
     fun() ->
        %% Create a test process
        TestPid = spawn(fun() -> timer:sleep(infinity) end),

        %% Set low limit for testing
        ok = erlmcp_queue_limits:set_limit(test_role, 5),

        %% Fill up to capacity
        lists:foreach(fun(_) ->
                         ok = erlmcp_queue_limits:check_capacity(test_role, TestPid),
                         erlmcp_queue_limits:record_enqueue(test_role, TestPid)
                      end,
                      lists:seq(1, 5)),

        %% Next check should fail
        Result = erlmcp_queue_limits:check_capacity(test_role, TestPid),
        ?assertMatch({error, {capacity_exceeded, _Stats}}, Result),

        %% Clean up
        exit(TestPid, kill)
     end}.

test_enqueue_dequeue(_State) ->
    {"Enqueue and dequeue tracking",
     fun() ->
        TestPid = spawn(fun() -> timer:sleep(infinity) end),

        %% Enqueue some messages
        erlmcp_queue_limits:record_enqueue(session, TestPid),
        erlmcp_queue_limits:record_enqueue(session, TestPid),
        erlmcp_queue_limits:record_enqueue(session, TestPid),

        %% Check depth
        timer:sleep(10),  % Allow async cast to process
        Depth = erlmcp_queue_limits:get_queue_depth(session, TestPid),
        ?assertEqual(3, Depth),

        %% Dequeue one
        erlmcp_queue_limits:record_dequeue(session, TestPid),
        timer:sleep(10),
        NewDepth = erlmcp_queue_limits:get_queue_depth(session, TestPid),
        ?assertEqual(2, NewDepth),

        %% Clean up
        exit(TestPid, kill)
     end}.

test_role_limits(_State) ->
    {"Different limits for different roles",
     fun() ->
        %% Verify default limits
        ?assertEqual(10000, erlmcp_queue_limits:get_limit(session)),
        ?assertEqual(5000, erlmcp_queue_limits:get_limit(sse_stream)),
        ?assertEqual(1000, erlmcp_queue_limits:get_limit(task_worker)),
        ?assertEqual(500, erlmcp_queue_limits:get_limit(tool_executor)),
        ?assertEqual(2000, erlmcp_queue_limits:get_limit(transport))
     end}.

test_dynamic_limit_adjustment(_State) ->
    {"Dynamically adjust role limits",
     fun() ->
        %% Set new limit
        ok = erlmcp_queue_limits:set_limit(session, 20000),
        ?assertEqual(20000, erlmcp_queue_limits:get_limit(session)),

        %% Restore original
        ok = erlmcp_queue_limits:set_limit(session, 10000),
        ?assertEqual(10000, erlmcp_queue_limits:get_limit(session))
     end}.

test_queue_depth_tracking(_State) ->
    {"Track queue depth per process",
     fun() ->
        Pid1 = spawn(fun() -> timer:sleep(infinity) end),
        Pid2 = spawn(fun() -> timer:sleep(infinity) end),

        %% Enqueue to different processes
        erlmcp_queue_limits:record_enqueue(session, Pid1),
        erlmcp_queue_limits:record_enqueue(session, Pid1),
        erlmcp_queue_limits:record_enqueue(session, Pid2),

        timer:sleep(10),

        %% Check individual depths
        ?assertEqual(2, erlmcp_queue_limits:get_queue_depth(session, Pid1)),
        ?assertEqual(1, erlmcp_queue_limits:get_queue_depth(session, Pid2)),

        %% Clean up
        exit(Pid1, kill),
        exit(Pid2, kill)
     end}.

test_statistics_collection( _State ) -> { "Collect and report statistics" , fun ( ) -> TestPid = spawn( fun ( ) -> timer : sleep( infinity ) end ) , erlmcp_queue_limits : record_enqueue( task_worker , TestPid ) , erlmcp_queue_limits : record_enqueue( task_worker , TestPid ) , erlmcp_queue_limits : record_dequeue( task_worker , TestPid ) , timer : sleep( 10 ) , Stats = erlmcp_queue_limits : get_role_stats( task_worker ) , ?assertMatch( #{ role := task_worker } , Stats ) , ?assertMatch( #{ total_enqueued := E } when E >= 2 , Stats ) , ?assertMatch( #{ total_dequeued := D } when D >= 1 , Stats ) , ok = erlmcp_queue_limits : reset_stats( task_worker ) , NewStats = erlmcp_queue_limits : get_role_stats( task_worker ) , ?assertMatch( #{ total_enqueued := 0 } , NewStats ) , exit( TestPid , kill ) end } .

         %% Generate some activity

         %% Get stats

         %% Reset stats

         %% Clean up

test_process_monitoring(_State) ->
    {"Monitor processes and clean up on termination",
     fun() ->
        TestPid = spawn(fun() -> timer:sleep(100) end),

        %% Enqueue to process
        erlmcp_queue_limits:record_enqueue(session, TestPid),
        timer:sleep(10),

        %% Verify tracked
        Depth1 = erlmcp_queue_limits:get_queue_depth(session, TestPid),
        ?assertEqual(1, Depth1),

        %% Wait for process to die
        timer:sleep(200),

        %% Depth should be cleaned up
        Depth2 = erlmcp_queue_limits:get_queue_depth(session, TestPid),
        ?assertEqual(0, Depth2)
     end}.

test_backpressure_shedding( _State ) -> { "Return 429 when capacity reached" , fun ( ) -> TestPid = spawn( fun ( ) -> timer : sleep( infinity ) end ) , ok = erlmcp_queue_limits : set_limit( shed_test , 2 ) , ok = erlmcp_queue_limits : check_capacity( shed_test , TestPid ) , erlmcp_queue_limits : record_enqueue( shed_test , TestPid ) , ok = erlmcp_queue_limits : check_capacity( shed_test , TestPid ) , erlmcp_queue_limits : record_enqueue( shed_test , TestPid ) , timer : sleep( 10 ) , Result = erlmcp_queue_limits : check_capacity( shed_test , TestPid ) , ?assertMatch( { error , { capacity_exceeded , #{ total_rejected := R } } } when R > 0 , Result ) , Stats = erlmcp_queue_limits : get_role_stats( shed_test ) , ?assertMatch( #{ capacity_exceeded_count := C } when C > 0 , Stats ) , exit( TestPid , kill ) end } .

         %% Set very low limit

         %% Fill to capacity

         %% Next should be rejected

         %% Verify rejection counted in stats

         %% Clean up
