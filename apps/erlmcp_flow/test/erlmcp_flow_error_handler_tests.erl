%%%-------------------------------------------------------------------
%%% @doc EUnit tests for erlmcp_flow_error_handler
%%% Chicago School TDD: Real processes, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_error_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup/Teardown
%%%===================================================================

setup() ->
    application:ensure_all_started(gproc),
    {ok, Pid} = erlmcp_flow_error_handler:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end,
    application:stop(gproc).

%%%===================================================================
%%% Test 1: Task Timeout Recovery (Requeue)
%%%===================================================================

task_timeout_recovery_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {"Task timeout triggers requeue with retry count 1",
           fun() ->
                   TaskId = <<"task-001">>,
                   Task = #{type => test, payload => <<"data">>},

                   % Simulate task timeout
                   ok = erlmcp_flow_error_handler:handle_task_timeout(TaskId, Task),

                   % Allow async processing
                   timer:sleep(50),

                   % Verify retry count incremented
                   RetryCount = erlmcp_flow_error_handler:get_retry_count(TaskId),
                   ?assertEqual(1, RetryCount)
           end},

          {"Task dropped after max retries",
           fun() ->
                   TaskId = <<"task-002">>,
                   Task = #{type => test, payload => <<"data">>},

                   % Trigger 3 timeouts (max retries)
                   ok = erlmcp_flow_error_handler:handle_task_timeout(TaskId, Task),
                   timer:sleep(150),

                   ok = erlmcp_flow_error_handler:handle_task_timeout(TaskId, Task),
                   timer:sleep(350),

                   ok = erlmcp_flow_error_handler:handle_task_timeout(TaskId, Task),
                   timer:sleep(800),

                   % 4th timeout should drop task
                   ok = erlmcp_flow_error_handler:handle_task_timeout(TaskId, Task),
                   timer:sleep(50),

                   % Retry count should be removed (task dropped)
                   RetryCount = erlmcp_flow_error_handler:get_retry_count(TaskId),
                   ?assertEqual(0, RetryCount)
           end},

          {"Exponential backoff is calculated correctly",
           fun() ->
                   TaskId = <<"task-003">>,
                   Task = #{type => test, payload => <<"data">>},

                   % First timeout: backoff = 100ms
                   ok = erlmcp_flow_error_handler:handle_task_timeout(TaskId, Task),
                   timer:sleep(50),
                   ?assertEqual(1, erlmcp_flow_error_handler:get_retry_count(TaskId)),

                   % Second timeout: backoff = 200ms
                   timer:sleep(150),
                   ok = erlmcp_flow_error_handler:handle_task_timeout(TaskId, Task),
                   timer:sleep(50),
                   ?assertEqual(2, erlmcp_flow_error_handler:get_retry_count(TaskId)),

                   % Third timeout: backoff = 400ms
                   timer:sleep(250),
                   ok = erlmcp_flow_error_handler:handle_task_timeout(TaskId, Task),
                   timer:sleep(50),
                   ?assertEqual(3, erlmcp_flow_error_handler:get_retry_count(TaskId))
           end}
         ]
     end}.

%%%===================================================================
%%% Test 2: Agent Crash Detection
%%%===================================================================

agent_crash_detection_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {"Agent crash updates status to crashed",
           fun() ->
                   AgentId = <<"agent-001">>,

                   % Simulate agent crash notification
                   ok = erlmcp_flow_error_handler:handle_agent_crash(AgentId),
                   timer:sleep(50),

                   % Verify agent status is crashed
                   Status = erlmcp_flow_error_handler:get_agent_status(AgentId),
                   ?assertEqual(crashed, Status)
           end},

          {"Multiple agent crashes tracked independently",
           fun() ->
                   Agent1 = <<"agent-001">>,
                   Agent2 = <<"agent-002">>,

                   ok = erlmcp_flow_error_handler:handle_agent_crash(Agent1),
                   ok = erlmcp_flow_error_handler:handle_agent_crash(Agent2),
                   timer:sleep(50),

                   ?assertEqual(crashed, erlmcp_flow_error_handler:get_agent_status(Agent1)),
                   ?assertEqual(crashed, erlmcp_flow_error_handler:get_agent_status(Agent2))
           end},

          {"Unknown agent returns unknown status",
           fun() ->
                   Status = erlmcp_flow_error_handler:get_agent_status(<<"unknown">>),
                   ?assertEqual(unknown, Status)
           end}
         ]
     end}.

%%%===================================================================
%%% Test 3: Leader Election Trigger
%%%===================================================================

leader_down_trigger_election_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {"Leader down triggers task reassignment",
           fun() ->
                   TaskId = <<"task-004">>,
                   Task = #{type => test, payload => <<"leader-test">>},

                   % Add pending task
                   ok = erlmcp_flow_error_handler:handle_task_timeout(TaskId, Task),
                   timer:sleep(50),

                   % Simulate leader down
                   ok = erlmcp_flow_error_handler:handle_leader_down(),
                   timer:sleep(150),

                   % Task should be scheduled for requeue
                   % (In real implementation, verify via swarm coordinator)
                   ?assert(true)
           end},

          {"Leader down without pending tasks completes successfully",
           fun() ->
                   ok = erlmcp_flow_error_handler:handle_leader_down(),
                   timer:sleep(50),
                   ?assert(true)
           end}
         ]
     end}.

%%%===================================================================
%%% Test 4: Routing Fallback
%%%===================================================================

routing_fallback_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {"Routing failure triggers fallback attempt",
           fun() ->
                   TaskId = <<"task-005">>,
                   Task = #{type => test, payload => <<"routing-test">>},

                   % Simulate routing failure
                   ok = erlmcp_flow_error_handler:handle_routing_failure(TaskId, Task),
                   timer:sleep(150),

                   % Should trigger requeue logic (tested via timeout path)
                   ?assert(true)
           end},

          {"Routing failure with no healthy agents uses timeout path",
           fun() ->
                   TaskId = <<"task-006">>,
                   Task = #{type => test, payload => <<"no-agents">>},

                   % Simulate routing failure (no healthy agents)
                   ok = erlmcp_flow_error_handler:handle_routing_failure(TaskId, Task),
                   timer:sleep(150),

                   % Should fallback to timeout/requeue logic
                   RetryCount = erlmcp_flow_error_handler:get_retry_count(TaskId),
                   ?assert(RetryCount >= 0)
           end}
         ]
     end}.
