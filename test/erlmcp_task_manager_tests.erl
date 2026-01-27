-module(erlmcp_task_manager_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Helper setup/teardown -------------------------------------------------
setup() ->
    Started = case whereis(erlmcp_task_manager) of
        undefined ->
            {ok, Pid} = erlmcp_task_manager:start_link(),
            {started, Pid};
        Pid ->
            {existing, Pid}
    end,
    maybe_clear_table(),
    ok = erlmcp_task_manager:register_server(test_server, self()),
    Started.

cleanup({started, _Pid}) ->
    erlmcp_task_manager:unregister_server(test_server),
    ok = erlmcp_task_manager:stop();
cleanup({existing, _Pid}) ->
    erlmcp_task_manager:unregister_server(test_server).

maybe_clear_table() ->
    case ets:info(erlmcp_tasks) of
        undefined -> ok;
        _ -> ets:delete_all_objects(erlmcp_tasks)
    end.

%% Tests -----------------------------------------------------------------

task_lifecycle_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun task_lifecycle/1}.

task_cancel_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun task_cancel/1}.

%% Implementation ---------------------------------------------------------

task_lifecycle(_SetupInfo) ->
    {ok, Task} = erlmcp_task_manager:create_tool_task(test_server, transport_a, <<"req-1">>, <<"echo">>, #{}),
    TaskId = maps:get(id, Task),
    %% initial queued notification
    maybe_consume_status(),
    receive
        {task_execute, ExecTask} ->
            ?assertEqual(TaskId, maps:get(id, ExecTask)),
            erlmcp_task_manager:complete_task(TaskId, #{?MCP_PARAM_CONTENT => []})
    after 1000 ->
        ?assert(false)
    end,
    maybe_expect_status(running),
    receive
        {task_status_update, Update2} ->
            ?assertEqual(completed, maps:get(status, Update2))
    after 1000 -> ?assert(false)
    end,
    {ok, FinalTask} = erlmcp_task_manager:get_task(TaskId),
    ?assertEqual(completed, maps:get(status, FinalTask)),
    ok.

task_cancel(_SetupInfo) ->
    {ok, Task} = erlmcp_task_manager:create_tool_task(test_server, transport_b, <<"req-2">>, <<"echo">>, #{}),
    TaskId = maps:get(id, Task),
    ok = erlmcp_task_manager:cancel_task(TaskId),
    maybe_expect_status(cancelled),
    {ok, CancelledTask} = erlmcp_task_manager:get_task(TaskId),
    ?assertEqual(cancelled, maps:get(status, CancelledTask)),
    ok.

maybe_consume_status() ->
    receive
        {task_status_update, _} -> ok
    after 10 -> ok
    end.

maybe_expect_status(Expected) ->
    receive
        {task_status_update, Update} ->
            case maps:get(status, Update) of
                Expected -> ok;
                _ -> maybe_expect_status(Expected)
            end
    after 1000 -> ok
    end.
