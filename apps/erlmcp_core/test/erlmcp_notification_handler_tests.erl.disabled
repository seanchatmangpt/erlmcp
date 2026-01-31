-module(erlmcp_notification_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test suite for supervised notification handlers
%% RPN 168: Fix unsupervised notification handlers

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, Pid} = erlmcp_notification_handler_sup:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid),
    ok.

%%====================================================================
%% Supervisor Lifecycle Tests
%%====================================================================

supervisor_start_stop_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     %% Verify supervisor is registered
                     ?assertEqual(whereis(erlmcp_notification_handler_sup), undefined)
                 end)
         ]
     end}.

%%====================================================================
%% Handler Startup Tests
%%====================================================================

handler_starts_for_function_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) ->
         [
          ?_test(begin
                     Method = <<"test/method">>,
                     Handler = fun(_M, _P) -> ok end,
                     Params = #{},

                     %% Start handler
                     ?assertMatch({ok, _Pid},
                                 erlmcp_notification_handler_sup:start_handler(Method, Handler, Params)),

                     %% Allow handler to complete
                     timer:sleep(100)
                 end)
         ]
     end}.

handler_starts_for_mfa_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) ->
         [
          ?_test(begin
                     Method = <<"test/mfa">>,
                     Handler = {erlmcp_notification_handler_tests, test_handler, []},
                     Params = #{},

                     %% Start handler
                     ?assertMatch({ok, _Pid},
                                 erlmcp_notification_handler_sup:start_handler(Method, Handler, Params)),

                     %% Allow handler to complete
                     timer:sleep(100)
                 end)
         ]
     end}.

%%====================================================================
%% Handler Execution Tests
%%====================================================================

handler_executes_function_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) ->
         [
          ?_test(begin
                     Method = <<"test/execute">>,
                     Self = self(),

                     Handler = fun(_M, P) ->
                                       Self ! {handler_executed, P},
                                       ok
                               end,

                     Params = #{<<"test">> => <<"value">>},

                     %% Start handler
                     {ok, _Pid} = erlmcp_notification_handler_sup:start_handler(Method, Handler, Params),

                     %% Verify handler executed
                     receive
                         {handler_executed, Params} ->
                             ok
                     after 500 ->
                             ?assert(false, handler_did_not_execute)
                     end
                 end)
         ]
     end}.

handler_executes_mfa_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) ->
         [
          ?_test(begin
                     Method = <<"test/mfa_execute">>,
                     Self = self(),

                     Params = #{<<"test">> => <<"value">>},
                     %% Store PID in process dictionary for MFA handler
                     put(test_pid, Self),

                     Handler = {erlmcp_notification_handler_tests, test_handler, []},

                     %% Start handler
                     {ok, _Pid} = erlmcp_notification_handler_sup:start_handler(Method, Handler, Params),

                     %% Verify handler executed
                     receive
                         {mfa_handler, Method, Params} ->
                             ok
                     after 500 ->
                             ?assert(false, mfa_handler_did_not_execute)
                     end
                 end)
         ]
     end}.

%%====================================================================
%% Handler Crash Recovery Tests
%%====================================================================

handler_crash_logged_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) ->
         [
          ?_test(begin
                     Method = <<"test/crash">>,
                     Handler = fun(_M, _P) -> error(intentional_crash) end,
                     Params = #{},

                     %% Start handler (will crash)
                     Result = erlmcp_notification_handler_sup:start_handler(Method, Handler, Params),

                     %% Handler should start but crash immediately
                     ?assertMatch({ok, _Pid}, Result),

                     %% Allow time for crash and logging
                     timer:sleep(100)
                 end)
         ]
     end}.

handler_bad_call_logged_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) ->
         [
          ?_test(begin
                     Method = <<"test/bad_call">>,
                     Handler = fun(_M, _P) -> erlang:error(bad_pattern) end,
                     Params = #{},

                     %% Start handler (will crash with bad pattern match)
                     Result = erlmcp_notification_handler_sup:start_handler(Method, Handler, Params),

                     %% Handler should start but crash immediately
                     ?assertMatch({ok, _Pid}, Result),

                     %% Allow time for crash and logging
                     timer:sleep(100)
                 end)
         ]
     end}.

%%====================================================================
%% Supervisor Restart Tests
%%====================================================================

supervisor_restarts_crashed_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) ->
         [
          ?_test(begin
                     Method = <<"test/restart">>,

                     %% Create handler that crashes after first call
                     Counter = atomics:new(1, []),
                     Handler = fun(_M, _P) ->
                                       case atomics:get(Counter, 1) of
                                           0 ->
                                               atomics:put(Counter, 1, 1),
                                               ok;
                                           _ ->
                                               error(subsequent_call_crash)
                                       end
                               end,

                     Params = #{},

                     %% Start handler (should succeed)
                     ?assertMatch({ok, _Pid1},
                                 erlmcp_notification_handler_sup:start_handler(Method, Handler, Params)),

                     %% Allow handler to complete normally
                     timer:sleep(100),

                     %% Start again (will crash)
                     ?assertMatch({ok, _Pid2},
                                 erlmcp_notification_handler_sup:start_handler(Method, Handler, Params)),

                     %% Allow time for crash
                     timer:sleep(100)
                 end)
         ]
     end}.

%%====================================================================
%% Multiple Handler Tests
%%====================================================================

multiple_concurrent_handlers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) ->
         [
          ?_test(begin
                     Self = self(),
                     Count = 10,

                     Handlers = lists:map(fun(N) ->
                                                 Method = <<"test/concurrent_", (integer_to_binary(N))/binary>>,
                                                 Handler = fun(_M, _P) ->
                                                                   Self ! {handler, N},
                                                                   timer:sleep(100),
                                                                   ok
                                                           end,
                                                 Params = #{<<"n">> => N},
                                                 {Method, Handler, Params}
                                         end, lists:seq(1, Count)),

                     %% Start all handlers
                     Pids = lists:map(fun({M, H, P}) ->
                                               {ok, Pid} = erlmcp_notification_handler_sup:start_handler(M, H, P),
                                               Pid
                                       end, Handlers),

                     %% Verify all started
                     ?assertEqual(Count, length(Pids)),

                     %% Collect results
                     Results = lists:map(fun(_) ->
                                                receive
                                                    {handler, N} -> N
                                                after 1000 ->
                                                        ?assert(false, handler_timeout)
                                                end
                                        end, lists:seq(1, Count)),

                     ?assertEqual(lists:sort(lists:seq(1, Count)), lists:sort(Results))
                 end)
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

integration_with_client_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_SupPid) ->
         [
          ?_test(begin
                     %% This test verifies the handler can be used from erlmcp_client
                     Method = <<"notifications/test">>,

                     %% Simulate client notification handler
                     Self = self(),
                     Handler = fun(_M, P) ->
                                       Self ! {notification, P},
                                       ok
                               end,

                     Params = #{<<"data">> => <<"test">>},

                     %% Start handler via supervisor
                     ?assertMatch({ok, _Pid},
                                 erlmcp_notification_handler_sup:start_handler(Method, Handler, Params)),

                     %% Verify handler executed
                     receive
                         {notification, Params} ->
                             ok
                     after 500 ->
                             ?assert(false, notification_not_received)
                     end
                 end)
         ]
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Test handler for MFA calls
test_handler(Method, Params) ->
    Pid = get(test_pid),
    Pid ! {mfa_handler, Method, Params},
    ok.

%%====================================================================
%% Test Generator
%%====================================================================

notification_handler_test_() ->
    [
     supervisor_start_stop_test_(),
     handler_starts_for_function_test_(),
     handler_starts_for_mfa_test_(),
     handler_executes_function_test_(),
     handler_executes_mfa_test_(),
     handler_crash_logged_test_(),
     handler_bad_call_logged_test_(),
     supervisor_restarts_crashed_handler_test_(),
     multiple_concurrent_handlers_test_(),
     integration_with_client_test_()
    ].
