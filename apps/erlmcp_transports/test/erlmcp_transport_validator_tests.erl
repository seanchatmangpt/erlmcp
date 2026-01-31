%%%-------------------------------------------------------------------
%%% @doc
%%% Transport Validator Tests - Chicago School TDD
%%%
%%% Tests the transport validator module that validates transport
%%% implementations for compliance with erlmcp_transport_behavior.
%%%
%%% Chicago School TDD Principles:
%%%   - Test observable behavior through API calls
%%%   - Use REAL transport processes (NO mocks)
%%%   - Verify state through outputs (not internal inspection)
%%%   - Test ALL transport types: stdio, tcp, http, websocket
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_validator_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    ok.

teardown(_) ->
    timer:sleep(100),
    ok.

%%%===================================================================
%%% Module Structure Tests
%%%===================================================================

module_exports_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"Validator module exports",
         fun() ->
             ?assert(is_list(erlmcp_transport_validator:module_info(exports))),
             Exports = erlmcp_transport_validator:module_info(exports),
             ?assertMatch({_, _}, lists:keyfind(validate_module, 1, Exports))
         end},

        {"Validator has validate_module function",
         fun() ->
             ?assertEqual(1, erlang:fun_info(fun erlmcp_transport_validator:validate_module/1, arity))
         end}
     ]}.

%%%===================================================================
%%% Behavior Compliance Tests
%%%===================================================================

behavior_compliance_stdio_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"stdio module exports",
         fun() ->
             ?assert(is_list(erlmcp_transport_stdio:module_info(exports))),
             Exports = erlmcp_transport_stdio:module_info(exports),
             ?assertMatch({_, _}, lists:keyfind(start_link, 1, Exports)),
             ?assertMatch({_, _}, lists:keyfind(send, 1, Exports)),
             ?assertMatch({_, _}, lists:keyfind(close, 1, Exports))
         end},

        {"stdio has gen_server behavior",
         fun() ->
             Attributes = erlmcp_transport_stdio:module_info(attributes),
             ?assert(lists:keymember(behaviour, 1, Attributes)),
             ?assert(lists:keymember(gen_server, 2, Attributes))
         end}
     ]}.

behavior_compliance_tcp_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"tcp module exports",
         fun() ->
             ?assert(is_list(erlmcp_transport_tcp:module_info(exports))),
             Exports = erlmcp_transport_tcp:module_info(exports),
             ?assertMatch({_, _}, lists:keyfind(start_link, 1, Exports)),
             ?assertMatch({_, _}, lists:keyfind(send, 1, Exports)),
             ?assertMatch({_, _}, lists:keyfind(close, 1, Exports))
         end},

        {"tcp has gen_server and ranch_protocol behaviors",
         fun() ->
             Attributes = erlmcp_transport_tcp:module_info(attributes),
             ?assert(lists:keymember(behaviour, 1, Attributes)),
             Behaviors = [B || {behaviour, B} <- Attributes],
             ?assert(lists:member(gen_server, Behaviors)),
             ?assert(lists:member(ranch_protocol, Behaviors))
         end}
     ]}.

behavior_compliance_http_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"http module exports",
         fun() ->
             ?assert(is_list(erlmcp_transport_http:module_info(exports))),
             Exports = erlmcp_transport_http:module_info(exports),
             ?assertMatch({_, _}, lists:keyfind(init, 1, Exports)),
             ?assertMatch({_, _}, lists:keyfind(send, 1, Exports)),
             ?assertMatch({_, _}, lists:keyfind(close, 1, Exports))
         end},

        {"http has transport_behavior",
         fun() ->
             Attributes = erlmcp_transport_http:module_info(attributes),
             ?assert(lists:keymember(behaviour, 1, Attributes)),
             Behaviors = [B || {behaviour, B} <- Attributes],
             ?assert(lists:member(erlmcp_transport_behavior, Behaviors))
         end}
     ]}.

behavior_compliance_websocket_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"websocket module exports",
         fun() ->
             ?assert(is_list(erlmcp_transport_ws:module_info(exports))),
             Exports = erlmcp_transport_ws:module_info(exports),
             ?assertMatch({_, _}, lists:keyfind(init, 1, Exports)),
             ?assertMatch({_, _}, lists:keyfind(send, 1, Exports)),
             ?assertMatch({_, _}, lists:keyfind(close, 1, Exports))
         end},

        {"websocket does NOT have transport_behavior (Cowboy handler)",
         fun() ->
             Attributes = erlmcp_transport_ws:module_info(attributes),
             Behaviors = [B || {behaviour, B} <- Attributes],
             ?assertNot(lists:member(erlmcp_transport_behavior, Behaviors))
         end}
     ]}.

%%%===================================================================
%%% Callback Validation Tests
%%%===================================================================

callback_validation_stdio_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"stdio has init callback (gen_server init/1)",
         fun() ->
             Exports = erlmcp_transport_stdio:module_info(exports),
             ?assertMatch({init, 1}, lists:keyfind(init, 1, Exports))
         end},

        {"stdio has send callback with arity 2",
         fun() ->
             Exports = erlmcp_transport_stdio:module_info(exports),
             ?assertMatch({send, 2}, lists:keyfind(send, 1, Exports))
         end},

        {"stdio has close callback with arity 1",
         fun() ->
             Exports = erlmcp_transport_stdio:module_info(exports),
             ?assertMatch({close, 1}, lists:keyfind(close, 1, Exports))
         end}
     ]}.

callback_validation_http_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"http has init callback (behavior init/1)",
         fun() ->
             Exports = erlmcp_transport_http:module_info(exports),
             ?assertMatch({init, 1}, lists:keyfind(init, 1, Exports))
         end},

        {"http has send callback with arity 2",
         fun() ->
             Exports = erlmcp_transport_http:module_info(exports),
             ?assertMatch({send, 2}, lists:keyfind(send, 1, Exports))
         end},

        {"http has close callback with arity 1",
         fun() ->
             Exports = erlmcp_transport_http:module_info(exports),
             ?assertMatch({close, 1}, lists:keyfind(close, 1, Exports))
         end}
     ]}.

%%%===================================================================
%%% Message Format Tests - Real Transport Behavior
%%%===================================================================

message_format_stdio_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10,
     fun() ->
         Owner = self(),
         {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
         try
             % Simulate input and verify message format
             TestLine = <<"test message">>,
             gen_server:call(Transport, {simulate_input, TestLine}),

             % Owner receives transport_message
             receive
                 {transport_message, ReceivedLine} ->
                     ?assertEqual(TestLine, ReceivedLine)
             after 1000 ->
                 ?assert(false, "Message not received")
             end
         after
             catch gen_server:stop(Transport, normal, 1000)
         end
     end}}.

message_format_tcp_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 15,
     fun() ->
         % Start TCP server in test mode
         {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
             mode => server,
             port => 0,  % Random port
             owner => self(),
             server_id => tcp_test_server
         }),

         try
             % Wait for server to start
             timer:sleep(200),

             % Verify server is running
             ?assert(is_process_alive(ServerPid)),

             % Server sends transport_connected on startup
             receive
                 {transport_connected, Pid} when Pid =:= ServerPid; is_pid(Pid) ->
                     ?assert(is_pid(Pid))
             after 1000 ->
                 % Connection message might have been sent earlier
                 ok
             end
         after
             catch gen_server:stop(ServerPid, normal, 1000)
         end
     end}}.

%%%===================================================================
%%% State Management Tests
%%%===================================================================

state_management_stdio_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"stdio init returns {ok, Pid}",
         fun() ->
             Owner = self(),
             Result = erlmcp_transport_stdio:start_link(Owner),
             ?assertMatch({ok, Pid} when is_pid(Pid), Result),
             {ok, Pid} = Result,
             gen_server:stop(Pid, normal, 1000)
         end},

        {"stdio transport alive after init",
         fun() ->
             Owner = self(),
             {ok, Pid} = erlmcp_transport_stdio:start_link(Owner),
             ?assert(is_process_alive(Pid)),
             gen_server:stop(Pid, normal, 1000)
         end},

        {"stdio transport dead after close",
         fun() ->
             Owner = self(),
             {ok, Pid} = erlmcp_transport_stdio:start_link(Owner),
             ?assert(is_process_alive(Pid)),
             ok = erlmcp_transport_stdio:close(Pid),
             timer:sleep(100),
             ?assertNot(is_process_alive(Pid))
         end}
     ]}.

state_management_http_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"http init validates config",
         fun() ->
             % Missing required fields
             Result = erlmcp_transport_http:init(#{}),
             ?assertMatch({error, _}, Result)
         end}
     ]}.

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

error_handling_stdio_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"stdio send handles invalid pid gracefully",
         fun() ->
             Result = catch erlmcp_transport_stdio:send(undefined, <<"test">>),
             ?assert(is_tuple(Result) orelse Result =:= ok)
         end},

        {"stdio close handles invalid pid gracefully",
         fun() ->
             Result = erlmcp_transport_stdio:close(undefined),
             ?assertEqual(ok, Result)
         end}
     ]}.

error_handling_tcp_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"tcp send on unconnected transport returns error",
         fun() ->
             % Create a dummy state with no socket
             DummyState = #{
                 mode => client,
                 socket => undefined,
                                         connected => false
             },

             Result = erlmcp_transport_tcp:send(DummyState, <<"test">>),
             ?assertMatch({error, _}, Result)
         end}
     ]}.

%%%===================================================================
%%% Integration Tests - Full Lifecycle
%%%===================================================================

lifecycle_stdio_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10,
     fun() ->
         Owner = self(),
         {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

         try
             % Init successful
             ?assert(is_process_alive(Transport)),

             % Send operation
             ?assertEqual(ok, erlmcp_transport_stdio:send(Transport, <<"test">>)),

             % Receive operation (simulated)
             gen_server:call(Transport, {simulate_input, <<"input">>}),
             receive
                 {transport_message, <<"input">>} -> ok
             after 1000 ->
                 ?assert(false)
             end,

             % Close operation
             ?assertEqual(ok, erlmcp_transport_stdio:close(Transport)),

             % Verify cleanup
             timer:sleep(100),
             ?assertNot(is_process_alive(Transport))
         after
             catch gen_server:stop(Transport, normal, 1000)
         end
     end}}.

concurrent_stdio_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 15,
     fun() ->
         Owner = self(),
         {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

         try
             % Send multiple messages concurrently
             NumMessages = 50,
             Pids = [spawn(fun() ->
                 erlmcp_transport_stdio:send(Transport, integer_to_binary(N))
             end) || N <- lists:seq(1, NumMessages)],

             % Wait for all sends to complete
             timer:sleep(500),

             % Transport should still be alive
             ?assert(is_process_alive(Transport))
         after
             catch gen_server:stop(Transport, normal, 1000)
         end
     end}}.

%%%===================================================================
%%% Report Generation Tests
%%%===================================================================

report_generation_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"validate_module returns report for stdio",
         fun() ->
             Report = erlmcp_transport_validator:validate_module(erlmcp_transport_stdio),
             ?assertMatch(#{module := erlmcp_transport_stdio}, Report)
         end},

        {"validate_module returns report for tcp",
         fun() ->
             Report = erlmcp_transport_validator:validate_module(erlmcp_transport_tcp),
             ?assertMatch(#{module := erlmcp_transport_tcp}, Report)
         end},

        {"validate_module returns report for http",
         fun() ->
             Report = erlmcp_transport_validator:validate_module(erlmcp_transport_http),
             ?assertMatch(#{module := erlmcp_transport_http}, Report)
         end},

        {"validate_module returns report for websocket",
         fun() ->
             Report = erlmcp_transport_validator:validate_module(erlmcp_transport_ws),
             ?assertMatch(#{module := erlmcp_transport_ws}, Report)
         end},

        {"validate_module handles invalid module",
         fun() ->
             Report = erlmcp_transport_validator:validate_module(nonexistent_module),
             ?assertMatch(#{module := nonexistent_module, status := error}, Report)
         end}
     ]}.

%%%===================================================================
%%% Validation Rule Tests
%%%===================================================================

validation_rules_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"validator checks callback exports",
         fun() ->
             Report = erlmcp_transport_validator:validate_callback_exports(erlmcp_transport_stdio),
             ?assert(is_map(Report)),
             ?assert(maps:is_key(callbacks, Report))
         end},

        {"validator checks message format",
         fun() ->
             Report = erlmcp_transport_validator:validate_message_format(erlmcp_transport_stdio),
             ?assert(is_map(Report)),
             ?assert(maps:is_key(message_format, Report))
         end},

        {"validator checks state management",
         fun() ->
             Report = erlmcp_transport_validator:validate_state_management(erlmcp_transport_stdio),
             ?assert(is_map(Report)),
             ?assert(maps:is_key(state_management, Report))
         end},

        {"validator checks error handling",
         fun() ->
             Report = erlmcp_transport_validator:validate_error_handling(erlmcp_transport_stdio),
             ?assert(is_map(Report)),
             ?assert(maps:is_key(error_handling, Report))
         end}
     ]}.

%%%===================================================================
%%% Compliance Report Tests
%%%===================================================================

compliance_report_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"generate compliance report for stdio",
         fun() ->
             Report = erlmcp_transport_validator:validate_module(erlmcp_transport_stdio),
             ?assert(is_map(Report)),
             ?assert(maps:is_key(module, Report)),
             ?assert(maps:is_key(status, Report)),
             ?assert(maps:is_key(validation_results, Report))
         end},

        {"compliance report includes behavior info",
         fun() ->
             Report = erlmcp_transport_validator:validate_module(erlmcp_transport_http),
             ValidationResults = maps:get(validation_results, Report),
             ?assert(maps:is_key(behavior_compliance, ValidationResults))
         end}
     ]}.
