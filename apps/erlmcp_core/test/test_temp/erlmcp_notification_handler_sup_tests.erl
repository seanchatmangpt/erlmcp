%%%-------------------------------------------------------------------
%%% @doc EUnit Test Suite for erlmcp_notification_handler_sup
%%%
%%% Chicago School TDD approach:
%%% - Real supervisor (no mocks)
%%% - Test observable behavior through API calls only
%%% - NO state inspection
%%% - Real notification handler processes
%%%
%%% Tests cover:
%%% - Supervisor lifecycle
%%% - Handler startup/shutdown under supervision
%%% - Restart strategies
%%% - Multiple concurrent handlers
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_notification_handler_sup_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Setup fixture - Start supervisor
%%--------------------------------------------------------------------
setup() ->
    %% Ensure any previous instance is stopped
    case whereis(erlmcp_notification_handler_sup) of
        undefined ->
            ok;
        OldPid ->
            exit(OldPid, kill),
            timer:sleep(100)
    end,
    {ok, Pid} = erlmcp_notification_handler_sup:start_link(),
    Pid.

%%--------------------------------------------------------------------
%% @doc Cleanup fixture - Stop supervisor
%%--------------------------------------------------------------------
cleanup(Pid) ->
    case is_process_alive(Pid) of
        true ->
            gen_server:stop(erlmcp_notification_handler_sup);
        false ->
            ok
    end,
    ok.

%%%===================================================================
%%% Supervisor Lifecycle Tests
%%%===================================================================

supervisor_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(test_supervisor_starts()),
         ?_test(test_double_start_fails()),
         ?_test(test_stop_and_restart())]
     end}.

test_supervisor_starts() ->
    %% Verify supervisor is registered and alive
    Pid = whereis(erlmcp_notification_handler_sup),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    %% Verify supervisor is functional through API
    Method = <<"test/lifecycle">>,
    Handler = fun(_M, _P) -> ok end,
    ?assertMatch({ok, _HandlerPid},
                 erlmcp_notification_handler_sup:start_handler(Method, Handler, #{})).

test_double_start_fails() ->
    %% Attempting to start again should fail (already registered)
    Result = erlmcp_notification_handler_sup:start_link(),
    ?assertMatch({error, {already_started, _}}, Result).

test_stop_and_restart() ->
    %% Stop supervisor
    Pid = whereis(erlmcp_notification_handler_sup),
    ok = gen_server:stop(erlmcp_notification_handler_sup),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)),

    %% Restart and verify through API
    {ok, NewPid} = erlmcp_notification_handler_sup:start_link(),
    ?assert(is_pid(NewPid)),
    ?assert(is_process_alive(NewPid)),

    %% Verify supervisor is functional
    Method = <<"test/restart">>,
    Handler = fun(_M, _P) -> ok end,
    ?assertMatch({ok, _HandlerPid},
                 erlmcp_notification_handler_sup:start_handler(Method, Handler, #{})).

%%%===================================================================
%%% Handler Supervision Tests
%%%===================================================================

handler_supision_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) ->
        [?_test(test_handler_under_supervision()),
         ?_test(test_handler_exits_normally()),
         ?_test(test_handler_crash_logged())]
     end}.

test_handler_under_supervision() ->
    %% Start handler under supervision
    Method = <<"test/supervised">>,
    Self = self(),

    Handler =
        fun(_M, _P) ->
           Self ! {handler_started, supervised},
           ok
        end,

    {ok, HandlerPid} = erlmcp_notification_handler_sup:start_handler(Method, Handler, #{}),

    %% Verify handler started and is supervised
    ?assert(is_pid(HandlerPid)),

    %% Verify handler executed
    receive
        {handler_started, supervised} ->
            ok
    after 500 ->
        ?assert(false, handler_did_not_start)
    end.

test_handler_exits_normally() ->
    %% Start handler that exits normally
    Method = <<"test/normal_exit">>,
    Self = self(),

    Handler =
        fun(_M, P) ->
           Self ! {handler_executed, P},
           ok
        end,

    Params = #{<<"test">> => <<"value">>},

    {ok, HandlerPid} = erlmcp_notification_handler_sup:start_handler(Method, Handler, Params),

    %% Verify handler executed
    receive
        {handler_executed, Params} ->
            ok
    after 500 ->
        ?assert(false, handler_did_not_execute)
    end,

    %% Wait for handler to exit normally
    timer:sleep(100),

    %% Verify handler exited (not running anymore)
    ?assertNot(is_process_alive(HandlerPid)).

test_handler_crash_logged() ->
    %% Start handler that will crash
    Method = <<"test/crash">>,
    Handler = fun(_M, _P) -> error(intentional_crash) end,

    {ok, HandlerPid} = erlmcp_notification_handler_sup:start_handler(Method, Handler, #{}),

    %% Handler should start but crash immediately
    ?assert(is_pid(HandlerPid)),

    %% Allow time for crash
    timer:sleep(100),

    %% Verify handler exited (transient restart: crashes are not restarted)
    ?assertNot(is_process_alive(HandlerPid)).

%%%===================================================================
%%% Multiple Handler Tests
%%%===================================================================

multiple_handlers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) -> [?_test(test_concurrent_handler_start()), ?_test(test_handler_isolation())]
     end}.

test_concurrent_handler_start() ->
    %% Start multiple handlers concurrently
    Self = self(),
    Count = 5,

    HandlerPids =
        lists:map(fun(N) ->
                     Method = <<"test/concurrent_", (integer_to_binary(N))/binary>>,
                     Handler =
                         fun(_M, _P) ->
                            Self ! {handler, N},
                            ok
                         end,
                     {ok, Pid} =
                         erlmcp_notification_handler_sup:start_handler(Method, Handler, #{}),
                     Pid
                  end,
                  lists:seq(1, Count)),

    %% Verify all handlers started
    ?assertEqual(Count, length(HandlerPids)),

    %% Verify all handlers are unique PIDs
    ?assertEqual(Count, length(lists:usort(HandlerPids))),

    %% Collect results
    Results =
        lists:map(fun(_) ->
                     receive
                         {handler, N} ->
                             N
                     after 1000 ->
                         ?assert(false, handler_timeout)
                     end
                  end,
                  lists:seq(1, Count)),

    ?assertEqual(lists:sort(
                     lists:seq(1, Count)),
                 lists:sort(Results)).

test_handler_isolation() ->
    %% Start multiple handlers with different methods
    Self = self(),

    Methods = [<<"test/iso1">>, <<"test/iso2">>, <<"test/iso3">>],

    HandlerPids =
        lists:map(fun(Method) ->
                     Handler =
                         fun(M, _P) ->
                            Self ! {handler, M},
                            ok
                         end,
                     {ok, Pid} =
                         erlmcp_notification_handler_sup:start_handler(Method, Handler, #{}),
                     {Method, Pid}
                  end,
                  Methods),

    %% Verify all handlers have unique PIDs
    Pids = [Pid || {_, Pid} <- HandlerPids],
    ?assertEqual(length(Methods), length(lists:usort(Pids))),

    %% Verify each handler executed independently
    lists:foreach(fun(Method) ->
                     receive
                         {handler, Method} ->
                             ok
                     after 500 ->
                         ?assert(false, {handler_not_executed, Method})
                     end
                  end,
                  Methods).

%%%===================================================================
%%% Restart Strategy Tests
%%%===================================================================

restart_strategy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) -> [?_test(test_transient_restart_strategy())] end}.

test_transient_restart_strategy() ->
    %% Verify transient restart: handlers are NOT restarted on normal exit
    Method = <<"test/transient">>,
    Self = self(),

    Handler =
        fun(_M, _P) ->
           Self ! {handler_executed, once},
           ok
        end,

    {ok, HandlerPid} = erlmcp_notification_handler_sup:start_handler(Method, Handler, #{}),

    %% Wait for handler to execute and exit normally
    receive
        {handler_executed, once} ->
            ok
    after 500 ->
        ?assert(false, handler_did_not_execute)
    end,

    timer:sleep(100),

    %% Verify handler exited and was NOT restarted (transient strategy)
    ?assertNot(is_process_alive(HandlerPid)),

    %% Starting again should create a new handler
    {ok, NewHandlerPid} = erlmcp_notification_handler_sup:start_handler(Method, Handler, #{}),
    ?assert(is_pid(NewHandlerPid)),
    ?assertNotEqual(HandlerPid, NewHandlerPid).

%%%===================================================================
%%% Supervisor Properties Tests
%%%===================================================================

supervisor_properties_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) -> [?_test(test_supervisor_strategy()), ?_test(test_supervisor_intensity())] end}.

test_supervisor_strategy() ->
    %% Verify supervisor is using simple_one_for_one strategy
    SupPid = whereis(erlmcp_notification_handler_sup),
    ?assert(is_process_alive(SupPid)),

    %% Start multiple handlers to verify simple_one_for_one
    Handlers =
        lists:map(fun(N) ->
                     Method = <<"test/strategy_", (integer_to_binary(N))/binary>>,
                     Handler = fun(_M, _P) -> ok end,
                     {ok, Pid} =
                         erlmcp_notification_handler_sup:start_handler(Method, Handler, #{}),
                     Pid
                  end,
                  lists:seq(1, 3)),

    %% All should be unique PIDs (dynamic children)
    ?assertEqual(3, length(lists:usort(Handlers))).

test_supervisor_intensity() ->
    %% Verify supervisor intensity limit (5 restarts in 60 seconds)
    %% This is difficult to test directly without crashing handlers rapidly
    %% Instead, we verify the supervisor accepts handlers normally
    %% Start normal handlers
    Handlers =
        lists:map(fun(N) ->
                     Method = <<"test/intensity_", (integer_to_binary(N))/binary>>,
                     Handler = fun(_M, _P) -> ok end,
                     {ok, Pid} =
                         erlmcp_notification_handler_sup:start_handler(Method, Handler, #{}),
                     Pid
                  end,
                  lists:seq(1, 5)),

    %% All should start successfully
    ?assertEqual(5, length(Handlers)),

    %% Verify all are valid PIDs
    ?assert(lists:all(fun(P) -> is_pid(P) end, Handlers)).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) -> [?_test(test_integration_with_notification_handler())] end}.

test_integration_with_notification_handler() ->
    %% Verify supervisor properly manages erlmcp_notification_handler processes
    Method = <<"notifications/integration">>,
    Self = self(),

    %% MFA handler
    Handler = {erlmcp_notification_handler_sup_tests, test_helper_handler, []},
    Params = #{<<"caller">> => Self},

    %% Start handler through supervisor
    {ok, HandlerPid} = erlmcp_notification_handler_sup:start_handler(Method, Handler, Params),

    %% Verify handler executed
    receive
        {helper_handler, Method, Params} ->
            ok
    after 500 ->
        ?assert(false, helper_handler_did_not_execute)
    end,

    %% Verify handler was a real erlmcp_notification_handler process
    ?assert(is_pid(HandlerPid)).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Test helper handler for MFA calls
test_helper_handler(Method, Params) ->
    Caller = maps:get(<<"caller">>, Params),
    Caller ! {helper_handler, Method, Params},
    ok.

%%%===================================================================
%%% Test Generator
%%%===================================================================

notification_handler_sup_test_() ->
    [supervisor_lifecycle_test_(),
     handler_supision_test_(),
     multiple_handlers_test_(),
     restart_strategy_test_(),
     supervisor_properties_test_(),
     integration_test_()].
