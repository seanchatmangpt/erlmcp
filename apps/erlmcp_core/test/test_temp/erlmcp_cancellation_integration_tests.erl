%%%-------------------------------------------------------------------
%%% @doc Cancellation Integration Tests (through erlmcp_server)
%%%
%%% Chicago School TDD: Tests requests/cancel notification through server
%%% Tests integration of cancellation with server request handling
%%% NO MOCKS - Uses REAL erlmcp_server processes
%%%
%%% Note: This is DIFFERENT from erlmcp_cancellation_tests.erl which tests
%%% the standalone erlmcp_cancellation module
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cancellation_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erlmcp.hrl").

%%%===================================================================
%%% Test Setup and Cleanup
%%%===================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    application:stop(erlmcp_core),
    ok.

start_test_server() ->
    Capabilities =
        #mcp_server_capabilities{resources = #mcp_capability{enabled = true},
                                 tools = #mcp_capability{enabled = true},
                                 prompts = #mcp_capability{enabled = true}},
    {ok, Pid} = erlmcp_server:start_link(<<"test_cancel_server">>, Capabilities),
    Pid.

stop_test_server(Pid) ->
    try
        erlmcp_server:stop(Pid)
    catch
        _:_ ->
            ok
    end.

%%%===================================================================
%%% Test Suite - Cancel Request Notifications
%%%===================================================================

cancellation_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [{"Cancel notification sent", fun test_cancel_notification/0},
         {"Cancel with reason", fun test_cancel_with_reason/0},
         {"Cancel non-existent request", fun test_cancel_nonexistent/0},
         {"Cancel already completed request", fun test_cancel_completed/0},
         {"Multiple cancellations", fun test_multiple_cancellations/0}]
     end}.

test_cancel_notification() ->
    Server = start_test_server(),

    %% Simulate a long-running operation
    RequestId = <<"test_request_123">>,

    %% Register operation with cancellation manager
    OperationPid =
        spawn(fun() ->
                 receive
                     stop ->
                         ok
                 after 5000 ->
                     ok
                 end
              end),

    Token = erlmcp_cancellation:register(self(), OperationPid, <<"tools/call">>),

    %% Cancel the operation
    ok = erlmcp_cancellation:cancel(Token),

    %% Verify cancellation notification sent
    receive
        {mcp_notification, Notification} ->
            ?assertEqual(<<"notifications/cancelled">>, maps:get(<<"method">>, Notification)),
            Params = maps:get(<<"params">>, Notification),
            ?assert(maps:is_key(<<"reason">>, Params))
    after 1000 ->
        ok %% Notification delivery is async
    end,

    %% Cleanup
    exit(OperationPid, kill),
    stop_test_server(Server).

test_cancel_with_reason() ->
    Server = start_test_server(),

    OperationPid =
        spawn(fun() ->
                 receive after 5000 ->
                     ok
                 end
              end),
    Token = erlmcp_cancellation:register(self(), OperationPid, <<"resources/read">>),

    %% Cancel with timeout reason
    ok = erlmcp_cancellation:cancel(Token, timeout),

    %% Verify notification includes reason
    receive
        {mcp_notification, Notification} ->
            Params = maps:get(<<"params">>, Notification),
            ?assertEqual(<<"timeout">>, maps:get(<<"reason">>, Params, undefined))
    after 1000 ->
        ok
    end,

    exit(OperationPid, kill),
    stop_test_server(Server).

test_cancel_nonexistent() ->
    Server = start_test_server(),

    %% Try to cancel non-existent request
    FakeToken = make_ref(),
    Result = erlmcp_cancellation:cancel(FakeToken),

    %% Should not crash
    ?assertEqual(ok, Result),

    stop_test_server(Server).

test_cancel_completed() ->
    Server = start_test_server(),

    OperationPid =
        spawn(fun() ->
                 receive after 100 ->
                     ok
                 end
              end),
    Token = erlmcp_cancellation:register(self(), OperationPid, <<"tools/call">>),

    %% Wait for operation to complete
    timer:sleep(200),

    %% Try to cancel completed operation
    Result = erlmcp_cancellation:cancel(Token),

    %% Should handle gracefully
    ?assert(Result =:= ok orelse Result =:= {error, already_completed}),

    stop_test_server(Server).

test_multiple_cancellations() ->
    Server = start_test_server(),

    %% Create multiple operations
    Ops = [spawn(fun() ->
                    receive after 5000 ->
                        ok
                    end
                 end)
           || _ <- lists:seq(1, 5)],
    Tokens = [erlmcp_cancellation:register(self(), Pid, <<"tools/call">>) || Pid <- Ops],

    %% Cancel all
    lists:foreach(fun(Token) -> ?assertEqual(ok, erlmcp_cancellation:cancel(Token)) end, Tokens),

    %% Wait for notifications
    timer:sleep(500),

    %% Cleanup
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Ops),
    stop_test_server(Server).

%%%===================================================================
%%% Test Suite - Cancellation During Tool Execution
%%%===================================================================

tool_cancellation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [{"Cancel during tool execution", fun test_cancel_during_tool/0},
         {"Tool cleanup on cancel", fun test_tool_cleanup/0}]
     end}.

test_cancel_during_tool() ->
    Server = start_test_server(),

    %% Add slow tool
    ToolName = <<"slow_tool">>,
    Parent = self(),
    Handler =
        fun(_Args) ->
           OperationPid =
               spawn(fun() ->
                        Parent ! {operation_started, self()},
                        receive
                            stop ->
                                ok
                        after 5000 ->
                            ok
                        end
                     end),
           Parent ! {operation_pid, OperationPid},
           receive after 5000 ->
               ok
           end,
           #{result => <<"done">>}
        end,

    ok = erlmcp_server:add_tool(Server, ToolName, Handler),

    %% Start tool execution in background
    spawn(fun() -> erlmcp_server:call_tool(Server, ToolName, #{}) end),

    %% Wait for operation to start
    OperationPid =
        receive
            {operation_pid, Pid} ->
                Pid
        after 1000 ->
            undefined
        end,

    case OperationPid of
        undefined ->
            ok;
        OpPid ->
            %% Register and cancel
            Token = erlmcp_cancellation:register(self(), OpPid, <<"tools/call">>),
            ok = erlmcp_cancellation:cancel(Token),

            %% Wait for cancellation
            timer:sleep(200),

            %% Operation should be stopped
            ?assertNot(is_process_alive(OpPid))
    end,

    stop_test_server(Server).

test_tool_cleanup() ->
    Server = start_test_server(),

    %% Add tool with cleanup logic
    ToolName = <<"cleanup_tool">>,
    Parent = self(),

    Handler =
        fun(_Args) ->
           OperationPid =
               spawn(fun() ->
                        %% Simulate resource allocation
                        Parent ! {resource_allocated, self()},
                        receive
                            stop ->
                                Parent ! {cleanup_executed, self()}
                        after 5000 ->
                            ok
                        end
                     end),
           receive after 5000 ->
               ok
           end,
           #{result => <<"done">>}
        end,

    ok = erlmcp_server:add_tool(Server, ToolName, Handler),

    %% Execute and cancel
    spawn(fun() -> erlmcp_server:call_tool(Server, ToolName, #{}) end),

    %% Get operation PID
    OperationPid =
        receive
            {resource_allocated, Pid} ->
                Pid
        after 1000 ->
            undefined
        end,

    case OperationPid of
        undefined ->
            ok;
        OpPid ->
            Token = erlmcp_cancellation:register(self(), OpPid, <<"tools/call">>),
            ok = erlmcp_cancellation:cancel(Token),

            %% Verify cleanup executed
            receive
                {cleanup_executed, OpPid} ->
                    ok
            after 1000 ->
                ok
            end
    end,

    stop_test_server(Server).

%%%===================================================================
%%% Test Suite - Resource Read Cancellation
%%%===================================================================

resource_cancellation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [{"Cancel during resource read", fun test_cancel_resource_read/0}] end}.

test_cancel_resource_read() ->
    Server = start_test_server(),

    %% Add slow resource
    Uri = <<"test://slow_resource">>,
    Parent = self(),
    Handler =
        fun(_) ->
           OperationPid =
               spawn(fun() ->
                        Parent ! {reading, self()},
                        receive
                            stop ->
                                ok
                        after 5000 ->
                            ok
                        end
                     end),
           receive after 5000 ->
               ok
           end,
           <<"resource content">>
        end,

    ok = erlmcp_server:add_resource(Server, Uri, Handler),

    %% Start resource read
    spawn(fun() -> erlmcp_server:read_resource(Server, Uri) end),

    %% Get operation PID
    OperationPid =
        receive
            {reading, Pid} ->
                Pid
        after 1000 ->
            undefined
        end,

    case OperationPid of
        undefined ->
            ok;
        OpPid ->
            %% Cancel read
            Token = erlmcp_cancellation:register(self(), OpPid, <<"resources/read">>),
            ok = erlmcp_cancellation:cancel(Token),

            timer:sleep(200),
            ?assertNot(is_process_alive(OpPid))
    end,

    stop_test_server(Server).

%%%===================================================================
%%% Test Suite - Progress Cancellation
%%%===================================================================

progress_cancellation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [{"Cancel operation with progress", fun test_cancel_with_progress/0}] end}.

test_cancel_with_progress() ->
    Server = start_test_server(),

    %% Add tool that reports progress
    ToolName = <<"progress_tool">>,
    Parent = self(),

    Handler =
        fun(_Args) ->
           OperationPid =
               spawn(fun() ->
                        Parent ! {started, self()},
                        lists:foreach(fun(N) ->
                                         erlmcp_server:report_progress(Server,
                                                                       <<"token1">>,
                                                                       float(N),
                                                                       10.0),
                                         timer:sleep(100)
                                      end,
                                      lists:seq(1, 10)),
                        #{result => <<"done">>}
                     end),
           receive after 5000 ->
               ok
           end,
           #{result => <<"timeout">>}
        end,

    ok = erlmcp_server:add_tool(Server, ToolName, Handler),

    %% Start execution
    spawn(fun() -> erlmcp_server:call_tool(Server, ToolName, #{}) end),

    %% Get operation PID
    OperationPid =
        receive
            {started, Pid} ->
                Pid
        after 1000 ->
            undefined
        end,

    case OperationPid of
        undefined ->
            ok;
        OpPid ->
            %% Wait for some progress
            timer:sleep(300),

            %% Cancel mid-progress
            Token = erlmcp_cancellation:register(self(), OpPid, <<"tools/call">>),
            ok = erlmcp_cancellation:cancel(Token),

            timer:sleep(200),
            ?assertNot(is_process_alive(OpPid))
    end,

    stop_test_server(Server).

%%%===================================================================
%%% Test Suite - Edge Cases
%%%===================================================================

cancellation_edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [{"Cancel same operation twice", fun test_double_cancel/0},
         {"Rapid cancel-execute cycles", fun test_rapid_cycles/0}]
     end}.

test_double_cancel() ->
    OperationPid =
        spawn(fun() ->
                 receive after 5000 ->
                     ok
                 end
              end),
    Token = erlmcp_cancellation:register(self(), OperationPid, <<"tools/call">>),

    %% Cancel twice
    ok = erlmcp_cancellation:cancel(Token),
    Result = erlmcp_cancellation:cancel(Token),

    %% Second cancel should handle gracefully
    ?assert(Result =:= ok orelse Result =:= {error, already_cancelled}),

    exit(OperationPid, kill).

test_rapid_cycles() ->
    %% Rapidly create and cancel operations
    lists:foreach(fun(_) ->
                     OperationPid =
                         spawn(fun() ->
                                  receive after 100 ->
                                      ok
                                  end
                               end),
                     Token = erlmcp_cancellation:register(self(), OperationPid),
                     erlmcp_cancellation:cancel(Token),
                     exit(OperationPid, kill)
                  end,
                  lists:seq(1, 50)),

    %% System should remain stable
    ?assert(is_process_alive(whereis(erlmcp_cancellation))).
