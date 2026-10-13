%%%-------------------------------------------------------------------
%%% @doc
%%% Test suite for erlmcp_shutdown graceful shutdown coordinator
%%%
%%% Tests all phases of graceful shutdown:
%%% - Initiation phase (stop accepting connections)
%%% - Drain phase (wait for existing requests)
%%% - Cleanup phase (close resources)
%%% - State persistence phase (save state)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_shutdown_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    %% Start required applications
    application:ensure_all_started(gproc),
    application:ensure_all_started(erlmcp_core),

    %% Start the shutdown coordinator
    {ok, Pid} = erlmcp_shutdown:start_link(),

    %% Register test cleanup handler
    Handler = #cleanup_handler{
        id = test_handler,
        module = ?MODULE,
        function = test_cleanup,
        args = [],
        timeout = 5000,
        priority = normal
    },
    ok = erlmcp_shutdown:register_cleanup_handler(test_handler, Handler),

    {Pid, Handler}.

cleanup({Pid, _Handler}) ->
    %% Unregister handler
    erlmcp_shutdown:unregister_cleanup_handler(test_handler),

    %% Stop shutdown coordinator
    gen_server:stop(Pid),

    %% Cleanup ETS tables
    catch ets:delete(erlmcp_shutdown_status),
    catch ets:delete(erlmcp_cleanup_handlers),

    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test shutdown coordinator starts successfully
%%--------------------------------------------------------------------
startup_test() ->
    {ok, Pid} = setup(),
    ?assert(is_process_alive(Pid)),
    cleanup({Pid, undefined}).

%%--------------------------------------------------------------------
%% @doc Test get_status returns error when not shutting down
%%--------------------------------------------------------------------
get_status_not_shutting_down_test() ->
    {ok, _Pid} = setup(),
    Result = erlmcp_shutdown:get_status(),
    ?assertEqual({error, not_shutting_down}, Result),
    cleanup({undefined, undefined}).

%%--------------------------------------------------------------------
%% @doc Test shutdown initiation
%%--------------------------------------------------------------------
initiate_shutdown_test() ->
    {Pid, _Handler} = setup(),

    %% Initiate shutdown
    ok = erlmcp_shutdown:shutdown(normal),

    %% Verify status
    {ok, Status} = erlmcp_shutdown:get_status(),
    ?assertEqual(true, Status#shutdown_status.initiating),
    ?assertEqual(normal, Status#shutdown_status.reason),

    cleanup({Pid, undefined}).

%%--------------------------------------------------------------------
%% @doc Test shutdown with custom drain timeout
%%--------------------------------------------------------------------
shutdown_custom_timeout_test() ->
    {Pid, _Handler} = setup(),

    %% Initiate shutdown with 10s timeout
    ok = erlmcp_shutdown:shutdown(normal, 10000),

    %% Verify status
    {ok, Status} = erlmcp_shutdown:get_status(),
    ?assertEqual(10000, Status#shutdown_status.drain_timeout),

    cleanup({Pid, undefined}).

%%--------------------------------------------------------------------
%% @doc Test duplicate shutdown request is rejected
%%--------------------------------------------------------------------
duplicate_shutdown_test() ->
    {Pid, _Handler} = setup(),

    %% First shutdown
    ok = erlmcp_shutdown:shutdown(normal),

    %% Second shutdown should fail
    Result = erlmcp_shutdown:shutdown(normal),
    ?assertEqual({error, already_shutting_down}, Result),

    cleanup({Pid, undefined}).

%%--------------------------------------------------------------------
%% @doc Test cleanup handler registration
%%--------------------------------------------------------------------
register_cleanup_handler_test() ->
    {Pid, _Handler} = setup(),

    Handler = #cleanup_handler{
        id = custom_handler,
        module = ?MODULE,
        function = test_cleanup,
        args = [],
        timeout = 5000,
        priority = high
    },

    ok = erlmcp_shutdown:register_cleanup_handler(custom_handler, Handler),

    %% Verify handler is registered
    ?assertMatch([{custom_handler, _}], ets:lookup(erlmcp_cleanup_handlers, custom_handler)),

    cleanup({Pid, Handler}).

%%--------------------------------------------------------------------
%% @doc Test cleanup handler unregistration
%%--------------------------------------------------------------------
unregister_cleanup_handler_test() ->
    {Pid, _Handler} = setup(),

    %% Register handler
    Handler = #cleanup_handler{
        id = temp_handler,
        module = ?MODULE,
        function = test_cleanup,
        args = [],
        timeout = 5000,
        priority = normal
    },
    ok = erlmcp_shutdown:register_cleanup_handler(temp_handler, Handler),

    %% Unregister handler
    ok = erlmcp_shutdown:unregister_cleanup_handler(temp_handler),

    %% Verify handler is removed
    ?assertEqual([], ets:lookup(erlmcp_cleanup_handlers, temp_handler)),

    cleanup({Pid, Handler}).

%%--------------------------------------------------------------------
%% @doc Test cleanup handler execution order by priority
%%--------------------------------------------------------------------
cleanup_handler_priority_test() ->
    %% Track execution order
    Pid = self(),
    HandlerFun = fun(Id, Priority) ->
        #cleanup_handler{
            id = Id,
            module = ?MODULE,
            function = record_priority,
            args = [Pid, Id, Priority],
            timeout = 5000,
            priority = Priority
        }
    end,

    {ShutdownPid, _Handler} = setup(),

    %% Register handlers in different order than priority
    ok = erlmcp_shutdown:register_cleanup_handler(handler1, HandlerFun(handler1, normal)),
    ok = erlmcp_shutdown:register_cleanup_handler(handler2, HandlerFun(handler2, urgent)),
    ok = erlmcp_shutdown:register_cleanup_handler(handler3, HandlerFun(handler3, low)),
    ok = erlmcp_shutdown:register_cleanup_handler(handler4, HandlerFun(handler4, high)),

    %% Initiate shutdown (will trigger cleanup handlers)
    erlang:send(ShutdownPid, drain_timeout),

    %% Wait for handlers to execute
    timer:sleep(1000),

    %% Verify execution order (urgent > high > normal > low)
    ?assertMatch([handler2, handler4, handler1, handler3], get_execution_order()),

    cleanup({ShutdownPid, undefined}).

%%--------------------------------------------------------------------
%% @doc Test await_shutdown returns immediately when not shutting down
%%--------------------------------------------------------------------
await_shutdown_not_shutting_down_test() ->
    {_Pid, _Handler} = setup(),

    %% Should return immediately
    ok = erlmcp_shutdown:await_shutdown(),

    cleanup({undefined, undefined}).

%%--------------------------------------------------------------------
%% @doc Test shutdown phases progression
%%--------------------------------------------------------------------
shutdown_phases_test() ->
    {Pid, _Handler} = setup(),

    %% Initiate shutdown
    ok = erlmcp_shutdown:shutdown(normal),

    %% Check phase progression
    {ok, Status} = erlmcp_shutdown:get_status(),

    %% Should be in initiation or drain phase
    ?assert(lists:member(Status#shutdown_status.phase, [initiation, drain])),

    %% Verify phases are recorded
    ?assert(is_list(Status#shutdown_status.phases)),

    cleanup({Pid, undefined}).

%%--------------------------------------------------------------------
%% @doc Test connection counts during shutdown
%%--------------------------------------------------------------------
connection_counts_test() ->
    {Pid, _Handler} = setup(),

    %% Initiate shutdown
    ok = erlmcp_shutdown:shutdown(normal),

    %% Get status
    {ok, Status} = erlmcp_shutdown:get_status(),

    %% Verify connection counts are non-negative
    ?assert(Status#shutdown_status.connections_active >= 0),
    ?assert(Status#shutdown_status.connections_total >= 0),
    ?assert(Status#shutdown_status.connections_active =< Status#shutdown_status.connections_total),

    cleanup({Pid, undefined}).

%%--------------------------------------------------------------------
%% @doc Test shutdown status timestamps
%%--------------------------------------------------------------------
shutdown_timestamps_test() ->
    {Pid, _Handler} = setup(),

    BeforeShutdown = erlang:system_time(millisecond),

    %% Initiate shutdown
    ok = erlmcp_shutdown:shutdown(normal),

    %% Get status
    {ok, Status} = erlmcp_shutdown:get_status(),

    %% Verify timestamps
    ?assert(Status#shutdown_status.start_time >= BeforeShutdown),
    ?assert(is_integer(Status#shutdown_status.estimated_completion)),

    cleanup({Pid, undefined}).

%%====================================================================
%% Helper Functions for Tests
%%--------------------------------------------------------------------

%% @doc Test cleanup function
-spec test_cleanup(list()) -> ok.
test_cleanup(_Args) ->
    logger:info("Test cleanup function called"),
    ok.

%% @doc Record handler execution order
-spec record_priority(pid(), term(), atom()) -> ok.
record_priority(TesterPid, Id, Priority) ->
    TesterPid ! {handler_executed, Id, Priority},
    ok.

%% @doc Get execution order from messages
get_execution_order() ->
    receive
        {handler_executed, Id, _Priority} ->
            [Id | get_execution_order()]
    after 100 ->
        []
    end.

%%====================================================================
%% Test Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generator: Test various shutdown reasons
%%--------------------------------------------------------------------
shutdown_reasons_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_Setup) ->
         [
          ?_assertEqual(ok, erlmcp_shutdown:shutdown(normal)),
          ?_assertEqual(ok, erlmcp_shutdown:shutdown(shutdown)),
          ?_assertEqual(ok, erlmcp_shutdown:shutdown({shutdown, "User initiated"}))
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Generator: Test invalid drain timeouts
%%--------------------------------------------------------------------
invalid_timeout_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_Setup) ->
         [
          ?_test(begin
                   %% Too short timeout
                   ?assertExit(_, erlmcp_shutdown:shutdown(normal, 1000))
                  end),
          ?_test(begin
                   %% Negative timeout
                   ?assertExit(_, erlmcp_shutdown:shutdown(normal, -5000))
                  end)
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test full shutdown sequence with mocks
%%--------------------------------------------------------------------
full_shutdown_sequence_test_() ->
    {setup,
     fun() ->
         %% Setup
         application:ensure_all_started(gproc),
         application:ensure_all_started(erlmcp_core),
         {ok, Pid} = erlmcp_shutdown:start_link(),
         Pid
     end,
     fun(Pid) ->
         %% Cleanup
         gen_server:stop(Pid),
         catch ets:delete(erlmcp_shutdown_status),
         catch ets:delete(erlmcp_cleanup_handlers)
     end,
     fun(_Pid) ->
         [
          ?_test(begin
                   %% Register cleanup handlers
                   Handler1 = #cleanup_handler{
                       id = handler1,
                       module = ?MODULE,
                       function = test_cleanup,
                       args = [],
                       timeout = 5000,
                       priority = urgent
                   },
                   ok = erlmcp_shutdown:register_cleanup_handler(handler1, Handler1),

                   %% Initiate shutdown
                   ok = erlmcp_shutdown:shutdown(normal),

                   %% Verify status
                   {ok, Status} = erlmcp_shutdown:get_status(),
                   ?assertEqual(true, Status#shutdown_status.initiating),

                   %% Simulate drain completion
                   timer:sleep(100),

                   %% Verify phases progressed
                   {ok, Status2} = erlmcp_shutdown:get_status(),
                   ?assert(length(Status2#shutdown_status.phases) > 0)
                  end)
         ]
     end}.
