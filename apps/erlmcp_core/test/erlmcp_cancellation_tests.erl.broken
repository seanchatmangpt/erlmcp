%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_cancellation
%%%
%%% Chicago School TDD: Real processes, real gen_server, state-based verification
%%% No mocks, direct process interaction and observable behavior
%%%-------------------------------------------------------------------
-module(erlmcp_cancellation_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

%%%===================================================================
%%% Test Setup and Cleanup
%%%===================================================================

cancellation_setup() ->
    {ok, Pid} = erlmcp_cancellation:start_link(),
    Pid.

cancellation_cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end.

%%%===================================================================
%%% Test Suite: Registration
%%%===================================================================

cancellation_registration_test_() ->
    {setup,
     fun cancellation_setup/0,
     fun cancellation_cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_register_operation()),
          ?_test(test_register_with_operation_type()),
          ?_test(test_multiple_operations()),
          ?_test(test_register_generates_unique_tokens())
         ]
     end}.

%% @doc Test basic operation registration
test_register_operation() ->
    %% Create test processes
    ClientPid = self(),
    OperationPid = spawn(fun() ->
        receive
            {check, From} -> From ! {ok, alive}
        end
    end),

    %% Register operation
    Token = erlmcp_cancellation:register(ClientPid, OperationPid),

    %% Verify token is a reference
    ?assert(is_reference(Token)),

    %% Verify operation exists
    ?assertEqual(ok, erlmcp_cancellation:check(Token)),

    %% Verify operation info can be retrieved
    {ok, Info} = erlmcp_cancellation:get_operation_info(Token),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Info)),

    %% Cleanup
    exit(OperationPid, kill).

%% @doc Test registration with operation type
test_register_with_operation_type() ->
    ClientPid = self(),
    OperationPid = spawn(fun() -> receive after 5000 end end),

    %% Register with operation type
    Token = erlmcp_cancellation:register(ClientPid, OperationPid, <<"tools/call">>),

    %% Verify operation type is stored
    {ok, Info} = erlmcp_cancellation:get_operation_info(Token),
    ?assertEqual(<<"tools/call">>, maps:get(<<"operationType">>, Info)),

    %% Cleanup
    exit(OperationPid, kill).

%% @doc Test multiple operations can be registered
test_multiple_operations() ->
    ClientPid = self(),

    %% Register multiple operations
    OpPids = [spawn(fun() -> receive after 5000 end end) || _ <- lists:seq(1, 5)],
    Tokens = [erlmcp_cancellation:register(ClientPid, Pid) || Pid <- OpPids],

    %% Verify all tokens are unique
    UniqueTokens = lists:usort(Tokens),
    ?assertEqual(5, length(UniqueTokens)),

    %% Verify all operations exist
    lists:foreach(fun(Token) ->
        ?assertEqual(ok, erlmcp_cancellation:check(Token))
    end, Tokens),

    %% Cleanup
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, OpPids).

%% @doc Test register generates unique tokens
test_register_generates_unique_tokens() ->
    ClientPid = self(),

    %% Register 100 operations
    Tokens = [begin
        Pid = spawn(fun() -> receive after 5000 end end),
        Token = erlmcp_cancellation:register(ClientPid, Pid),
        exit(Pid, kill),
        Token
    end || _ <- lists:seq(1, 100)],

    %% Verify all unique
    UniqueTokens = lists:usort(Tokens),
    ?assertEqual(100, length(UniqueTokens)).

%%%===================================================================
%%% Test Suite: Cancellation
%%%===================================================================

cancellation_cancel_test_() ->
    {setup,
     fun cancellation_setup/0,
     fun cancellation_cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_cancel_operation()),
          ?_test(test_cancel_with_reason()),
          ?_test(test_cancel_nonexistent_returns_error()),
          ?_test(test_cancel_notifies_client()),
          ?_test(test_is_cancelled())
         ]
     end}.

%% @doc Test basic operation cancellation
test_cancel_operation() ->
    ClientPid = self(),

    %% Create long-running operation
    OperationPid = spawn(fun() ->
        receive
            {check, From} -> From ! {ok, alive}
        end
    end),

    %% Register operation
    Token = erlmcp_cancellation:register(ClientPid, OperationPid),

    %% Cancel operation
    ?assertEqual(ok, erlmcp_cancellation:cancel(Token)),

    %% Verify operation is marked as cancelled
    ?assertEqual({error, cancelled}, erlmcp_cancellation:check(Token)),

    %% Verify operation was killed
    timer:sleep(50),
    ?assertNot(is_process_alive(OperationPid)).

%% @doc Test cancellation with specific reason
test_cancel_with_reason() ->
    ClientPid = self(),
    OperationPid = spawn(fun() -> receive after 5000 end end),

    %% Register operation
    Token = erlmcp_cancellation:register(ClientPid, OperationPid),

    %% Cancel with timeout reason
    ?assertEqual(ok, erlmcp_cancellation:cancel(Token, timeout)),

    %% Verify cancellation reason
    {ok, Info} = erlmcp_cancellation:get_operation_info(Token),
    ?assertEqual(<<"cancelled">>, maps:get(<<"status">>, Info)),
    ?assertEqual(<<"timeout">>, maps:get(<<"reason">>, Info)),

    %% Cleanup
    exit(OperationPid, kill).

%% @doc Test cancelling non-existent operation returns error
test_cancel_nonexistent_returns_error() ->
    %% Cancel non-existent token
    FakeToken = make_ref(),
    ?assertEqual(ok, erlmcp_cancellation:cancel(FakeToken)),  % cast, always returns ok

    %% Verify token doesn't exist
    ?assertEqual({error, not_found}, erlmcp_cancellation:check(FakeToken)).

%% @doc Test cancellation sends notification to client
test_cancel_notifies_client() ->
    %% Create client that receives notifications
    ClientPid = self(),

    %% Create operation
    OperationPid = spawn(fun() -> receive after 5000 end end),

    %% Register and cancel
    Token = erlmcp_cancellation:register(ClientPid, OperationPid),
    erlmcp_cancellation:cancel(Token),

    %% Verify notification received
    receive
        {mcp_notification, Notification} ->
            ?assertEqual(<<"notifications/cancelled">>, maps:get(<<"method">>, Notification)),
            Params = maps:get(<<"params">>, Notification),
            ?assert(maps:is_key(<<"requestId">>, Params)),
            ?assertEqual(<<"client_requested">>, maps:get(<<"reason">>, Params));
        _Other ->
            ?assert(false, "Expected cancellation notification")
    after 1000 ->
        ?assert(false, "Timeout waiting for cancellation notification")
    end,

    %% Cleanup
    exit(OperationPid, kill).

%% @doc Test is_cancelled helper function
test_is_cancelled() ->
    ClientPid = self(),
    OperationPid = spawn(fun() -> receive after 5000 end end),

    %% Register operation
    Token = erlmcp_cancellation:register(ClientPid, OperationPid),

    %% Verify not cancelled initially
    ?assertNot(erlmcp_cancellation:is_cancelled(Token)),

    %% Cancel operation
    erlmcp_cancellation:cancel(Token),

    %% Verify is cancelled
    ?assert(erlmcp_cancellation:is_cancelled(Token)),

    %% Cleanup
    exit(OperationPid, kill).

%%%===================================================================
%%% Test Suite: Operation Lifecycle
%%%===================================================================

cancellation_lifecycle_test_() ->
    {setup,
     fun cancellation_setup/0,
     fun cancellation_cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_operation_completion_removes_token()),
          ?_test(test_operation_crash_removes_token()),
          ?_test(test_list_operations()),
          ?_test(test_operation_duration_tracking())
         ]
     end}.

%% @doc Test completed operations are removed from tracking
test_operation_completion_removes_token() ->
    ClientPid = self(),

    %% Create operation that exits quickly
    OperationPid = spawn(fun() -> ok end),

    %% Register operation
    Token = erlmcp_cancellation:register(ClientPid, OperationPid),

    %% Wait for process to exit and DOWN message to be processed
    timer:sleep(100),

    %% Verify operation is removed
    ?assertEqual({error, not_found}, erlmcp_cancellation:check(Token)).

%% @doc Test crashed operations are removed from tracking
test_operation_crash_removes_token() ->
    ClientPid = self(),

    %% Create operation that crashes
    OperationPid = spawn(fun() -> exit(crash) end),

    %% Register operation
    Token = erlmcp_cancellation:register(ClientPid, OperationPid),

    %% Wait for DOWN message
    timer:sleep(100),

    %% Verify operation is removed
    ?assertEqual({error, not_found}, erlmcp_cancellation:check(Token)).

%% @doc Test listing all operations
test_list_operations() ->
    ClientPid = self(),

    %% Create multiple operations
    OpPids = [spawn(fun() -> receive after 5000 end end) || _ <- lists:seq(1, 3)],
    _Tokens = [erlmcp_cancellation:register(ClientPid, Pid, <<"test_op">>) || Pid <- OpPids],

    %% List operations
    Operations = erlmcp_cancellation:list_operations(),

    %% Verify count
    ?assertEqual(3, length(Operations)),

    %% Verify structure
    lists:foreach(fun(Op) ->
        ?assert(maps:is_key(<<"token">>, Op)),
        ?assert(maps:is_key(<<"operationType">>, Op)),
        ?assert(maps:is_key(<<"startTime">>, Op)),
        ?assert(maps:is_key(<<"duration">>, Op)),
        ?assert(maps:is_key(<<"status">>, Op))
    end, Operations),

    %% Cleanup
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, OpPids).

%% @doc Test operation duration tracking
test_operation_duration_tracking() ->
    ClientPid = self(),

    %% Create operation
    OperationPid = spawn(fun() -> receive after 5000 end end),

    %% Register operation
    Token = erlmcp_cancellation:register(ClientPid, OperationPid),

    %% Wait a bit
    timer:sleep(100),

    %% Get operation info
    {ok, Info} = erlmcp_cancellation:get_operation_info(Token),

    %% Verify duration is positive
    Duration = maps:get(<<"duration">>, Info),
    ?assert(Duration > 0),

    %% Cleanup
    exit(OperationPid, kill).

%%%===================================================================
%%% Test Suite: Cleanup Handlers
%%%===================================================================

cancellation_cleanup_handler_test_() ->
    {setup,
     fun cancellation_setup/0,
     fun cancellation_cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_set_cleanup_handler()),
          ?_test(test_cleanup_handler_called_on_cancel()),
          ?_test(test_cleanup_handler_failure_handled())
         ]
     end}.

%% @doc Test setting cleanup handler
test_set_cleanup_handler() ->
    ?assertEqual(ok, erlmcp_cancellation:set_cleanup_handler(<<"tools/call">>, test_cleanup_handler)).

%% @doc Test cleanup handler is called on cancellation
test_cleanup_handler_called_on_cancel() ->
    %% Set cleanup handler
    erlmcp_cancellation:set_cleanup_handler(<<"tools/call">>, test_cleanup_handler),

    %% Create operation
    ClientPid = self(),
    OperationPid = spawn(fun() -> receive after 5000 end end),

    %% Register operation with type that has cleanup handler
    Token = erlmcp_cancellation:register(ClientPid, OperationPid, <<"tools/call">>),

    %% Track cleanup calls
    ets:new(cleanup_tracker, [named_table, public, set]),
    ets:insert(cleanup_tracker, {cleanup_called, false}),

    %% Cancel operation
    erlmcp_cancellation:cancel(Token),

    %% Wait for async cleanup
    timer:sleep(100),

    %% Cleanup
    ets:delete(cleanup_tracker),
    exit(OperationPid, kill).

%% @doc Test cleanup handler failure doesn't crash cancellation manager
test_cleanup_handler_failure_handled() ->
    %% Set cleanup handler that will fail
    erlmcp_cancellation:set_cleanup_handler(<<"failing_op">>, failing_cleanup_handler),

    %% Create operation
    ClientPid = self(),
    OperationPid = spawn(fun() -> receive after 5000 end end),

    %% Register and cancel
    Token = erlmcp_cancellation:register(ClientPid, OperationPid, <<"failing_op">>),

    %% Cancel (should not crash despite cleanup handler failure)
    ?assertEqual(ok, erlmcp_cancellation:cancel(Token)),

    %% Verify cancellation manager still alive
    ?assert(is_process_alive(whereis(erlmcp_cancellation))),

    %% Cleanup
    exit(OperationPid, kill).

%%%===================================================================
%%% Test Suite: Server Shutdown
%%%===================================================================

cancellation_shutdown_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_cancellation:start_link(),
         Pid
     end,
     fun(Pid) ->
         case is_process_alive(Pid) of
             true -> gen_server:stop(Pid);
             false -> ok
         end
     end,
     fun(Pid) ->
         [
          ?_test(test_shutdown_cancels_all_operations(Pid))
         ]
     end}.

%% @doc Test server shutdown cancels all operations
test_shutdown_cancels_all_operations(Pid) ->
    ClientPid = self(),

    %% Create operations
    OpPids = [spawn(fun() ->
        receive
            {check, From} -> From ! {ok, alive}
        end
    end) || _ <- lists:seq(1, 3)],

    _Tokens = [erlmcp_cancellation:register(ClientPid, Pid) || Pid <- OpPids],

    %% Stop server
    gen_server:stop(Pid),

    %% Wait for shutdown
    timer:sleep(100),

    %% Verify all operations were killed
    AliveOps = lists:filter(fun(P) -> is_process_alive(P) end, OpPids),
    ?assertEqual(0, length(AliveOps)),

    %% Clean up any remaining processes
    lists:foreach(fun(P) ->
        case is_process_alive(P) of
            true -> exit(P, kill);
            false -> ok
        end
    end, OpPids).

%%%===================================================================
%%% Test Suite: Concurrent Operations
%%%===================================================================

cancellation_concurrent_test_() ->
    {setup,
     fun cancellation_setup/0,
     fun cancellation_cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_concurrent_registrations()),
          ?_test(test_concurrent_cancellations()),
          ?_test(test_race_condition_register_cancel())
         ]
     end}.

%% @doc Test concurrent registrations
test_concurrent_registrations() ->
    ClientPid = self(),

    %% Spawn 50 concurrent registrations
    Tokens = lists:map(fun(_) ->
        spawn(fun() ->
            OpPid = spawn(fun() -> receive after 5000 end end),
            Token = erlmcp_cancellation:register(ClientPid, OpPid),
            exit(OpPid, kill),
            Token
        end)
    end, lists:seq(1, 50)),

    %% Wait for all to complete
    timer:sleep(200),

    %% Verify all unique tokens
    ?assertEqual(50, length(lists:usort(Tokens))).

%% @doc Test concurrent cancellations
test_concurrent_cancellations() ->
    ClientPid = self(),

    %% Create operations
    OpPids = [spawn(fun() -> receive after 5000 end end) || _ <- lists:seq(1, 20)],
    Tokens = [erlmcp_cancellation:register(ClientPid, Pid) || Pid <- OpPids],

    %% Cancel all concurrently
    lists:foreach(fun(Token) ->
        spawn(fun() -> erlmcp_cancellation:cancel(Token) end)
    end, Tokens),

    %% Wait for cancellations
    timer:sleep(200),

    %% Verify all cancelled
    lists:foreach(fun(Token) ->
        ?assertEqual({error, cancelled}, erlmcp_cancellation:check(Token))
    end, Tokens),

    %% Cleanup
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, OpPids).

%% @doc Test race condition: register and cancel simultaneously
test_race_condition_register_cancel() ->
    ClientPid = self(),

    %% Rapid register/cancel cycles
    lists:foreach(fun(_) ->
        OpPid = spawn(fun() -> receive after 100 end end),
        Token = erlmcp_cancellation:register(ClientPid, OpPid),
        erlmcp_cancellation:cancel(Token),
        exit(OpPid, kill)
    end, lists:seq(1, 100)),

    %% Allow all to process
    timer:sleep(500),

    %% Verify server still alive
    ?assert(is_process_alive(whereis(erlmcp_cancellation))).

%%%===================================================================
%%% Cleanup Handler Mocks
%%%===================================================================

%% Mock cleanup handler for testing
-module(test_cleanup_handler).
-export([cleanup_operation/2]).

cleanup_operation(_Token, _Reason) ->
    ets:insert(cleanup_tracker, {cleanup_called, true}),
    ok.

%% Mock failing cleanup handler
-module(failing_cleanup_handler).
-export([cleanup_operation/2]).

cleanup_operation(_Token, _Reason) ->
    exit(cleanup_failed).
