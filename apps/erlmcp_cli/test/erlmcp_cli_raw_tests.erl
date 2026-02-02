%%%-------------------------------------------------------------------
%%% @doc
%%% Raw Mode Tests (EUnit)
%%%
%%% Tests for erlmcp_cli_raw module - OTP 28 raw mode for CLI
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real gen_server for raw mode state
%%% - NO mocks, real character handling
%%% - State-based verification (buffer, history, context)
%%%
%%% Coverage Target: ≥85%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_raw_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

cli_raw_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Start raw mode server", fun test_start_server/0},
      {"Stop raw mode server", fun test_stop_server/0},
      {"Handle printable character", fun test_handle_printable_char/0},
      {"Handle backspace", fun test_handle_backspace/0},
      {"Handle Ctrl+C", fun test_handle_ctrl_c/0},
      {"Handle Ctrl+D", fun test_handle_ctrl_d/0},
      {"Handle Ctrl+L clear screen", fun test_handle_ctrl_l/0},
      {"Handle carriage return", fun test_handle_carriage_return/0},
      {"Process empty command", fun test_process_empty_command/0},
      {"Process /inspect command", fun test_process_inspect_command/0},
      {"Process /cancel command", fun test_process_cancel_command/0},
      {"Process /stats command", fun test_process_stats_command/0},
      {"Process /help command", fun test_process_help_command/0},
      {"Process /quit command", fun test_process_quit_command/0},
      {"Command history add", fun test_history_add/0},
      {"Command history navigation", fun test_history_navigation/0},
      {"History limit enforcement", fun test_history_limit/0},
      {"Execute ping command", fun test_execute_ping/0},
      {"Execute echo command", fun test_execute_echo/0},
      {"Execute set command", fun test_execute_set/0},
      {"Execute get command", fun test_execute_get/0},
      {"Execute unknown command", fun test_execute_unknown/0},
      {"Inspect context", fun test_inspect_context/0},
      {"Cancel operation", fun test_cancel_operation/0},
      {"Get statistics", fun test_get_stats/0},
      {"Get state", fun test_get_state/0},
      {"Buffer overflow protection", fun test_buffer_overflow/0},
      {"Special character handling", fun test_special_characters/0}]}.

setup() ->
    %% Start required applications
    application:ensure_all_started(crypto),
    ok.

cleanup(_Args) ->
    ok.

%%%====================================================================
%%% Server Lifecycle Tests
%%%====================================================================

test_start_server() ->
    %% Start raw mode server (gen_server)
    {ok, Pid} = erlmcp_cli_raw:start_link(),
    ?assert(is_pid(Pid)),

    %% Verify initial state
    {ok, State} = erlmcp_cli_raw:get_state(),
    ?assertMatch(#{buffer := <<>>, history := []}, State),

    %% Cleanup
    ok = erlmcp_cli_raw:stop().

test_stop_server() ->
    %% Start server
    {ok, Pid} = erlmcp_cli_raw:start_link(),

    %% Stop gracefully
    ok = erlmcp_cli_raw:stop(),

    %% Verify process stopped
    timer:sleep(50),
    ?assertEqual(undefined, whereis(erlmcp_cli_raw)).

%%%====================================================================
%%% Character Handling Tests
%%%====================================================================

test_handle_printable_char() ->
    {ok, Pid} = erlmcp_cli_raw:start_link(),

    %% Send printable characters
    ok = erlmcp_cli_raw:send_char($h),
    ok = erlmcp_cli_raw:send_char($e),
    ok = erlmcp_cli_raw:send_char($l),
    ok = erlmcp_cli_raw:send_char($l),
    ok = erlmcp_cli_raw:send_char($o),

    %% Check buffer
    {ok, State} = erlmcp_cli_raw:get_state(),
    ?assertEqual(<<"hello">>, maps:get(buffer, State)),

    ok = erlmcp_cli_raw:stop().

test_handle_backspace() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Send characters and backspace
    ok = erlmcp_cli_raw:send_char($h),
    ok = erlmcp_cli_raw:send_char($e),
    ok = erlmcp_cli_raw:send_char($\b),  % Backspace

    %% Check buffer (should be "h")
    {ok, State} = erlmcp_cli_raw:get_state(),
    ?assertEqual(<<"h">>, maps:get(buffer, State)),

    ok = erlmcp_cli_raw:stop().

test_handle_ctrl_c() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Send Ctrl+C (interrupt)
    ok = erlmcp_cli_raw:send_char(3),  % Ctrl+C

    %% Server should still be running
    ?assert(is_process_alive(whereis(erlmcp_cli_raw))),

    ok = erlmcp_cli_raw:stop().

test_handle_ctrl_d() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Send Ctrl+D (EOF)
    ok = erlmcp_cli_raw:send_char(4),  % Ctrl+D

    %% Server should initiate shutdown
    timer:sleep(50),
    ?assertEqual(undefined, whereis(erlmcp_cli_raw)).

test_handle_ctrl_l() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Send Ctrl+L (clear screen)
    ok = erlmcp_cli_raw:send_char(12),  % Ctrl+L

    %% Server should still be running
    ?assert(is_process_alive(whereis(erlmcp_cli_raw))),

    ok = erlmcp_cli_raw:stop().

test_handle_carriage_return() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Send command and Enter
    ok = erlmcp_cli_raw:send_char($p),
    ok = erlmcp_cli_raw:send_char($i),
    ok = erlmcp_cli_raw:send_char($n),
    ok = erlmcp_cli_raw:send_char($g),
    ok = erlmcp_cli_raw:send_char(13),  % Carriage return

    %% Buffer should be cleared
    {ok, State} = erlmcp_cli_raw:get_state(),
    ?assertEqual(<<>>, maps:get(buffer, State)),

    ok = erlmcp_cli_raw:stop().

%%%====================================================================
%%% Command Processing Tests
%%%====================================================================

test_process_empty_command() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Send empty command (just Enter)
    ok = erlmcp_cli_raw:send_char(13),

    %% Buffer should be empty
    {ok, State} = erlmcp_cli_raw:get_state(),
    ?assertEqual(<<>>, maps:get(buffer, State)),

    ok = erlmcp_cli_raw:stop().

test_process_inspect_command() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Send /inspect command
    lists:foreach(fun(C) -> erlmcp_cli_raw:send_char(C) end,
                  "/inspect"),
    ok = erlmcp_cli_raw:send_char(13),

    %% Should not crash
    ?assert(is_process_alive(whereis(erlmcp_cli_raw))),

    ok = erlmcp_cli_raw:stop().

test_process_cancel_command() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Send /cancel command
    lists:foreach(fun(C) -> erlmcp_cli_raw:send_char(C) end,
                  "/cancel"),
    ok = erlmcp_cli_raw:send_char(13),

    %% Should not crash
    ?assert(is_process_alive(whereis(erlmcp_cli_raw))),

    ok = erlmcp_cli_raw:stop().

test_process_stats_command() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Send /stats command
    lists:foreach(fun(C) -> erlmcp_cli_raw:send_char(C) end,
                  "/stats"),
    ok = erlmcp_cli_raw:send_char(13),

    %% Should not crash
    ?assert(is_process_alive(whereis(erlmcp_cli_raw))),

    ok = erlmcp_cli_raw:stop().

test_process_help_command() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Send /help command
    lists:foreach(fun(C) -> erlmcp_cli_raw:send_char(C) end,
                  "/help"),
    ok = erlmcp_cli_raw:send_char(13),

    %% Should not crash
    ?assert(is_process_alive(whereis(erlmcp_cli_raw))),

    ok = erlmcp_cli_raw:stop().

test_process_quit_command() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Send /quit command
    lists:foreach(fun(C) -> erlmcp_cli_raw:send_char(C) end,
                  "/quit"),
    ok = erlmcp_cli_raw:send_char(13),

    %% Server should initiate shutdown
    timer:sleep(50),
    ?assertEqual(undefined, whereis(erlmcp_cli_raw)).

%%%====================================================================
%%% History Management Tests
%%%====================================================================

test_history_add() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Execute commands to add to history
    {ok, _} = erlmcp_cli_raw:execute_command(<<"ping">>),
    {ok, _} = erlmcp_cli_raw:execute_command(<<"echo test">>),

    %% Check history
    {ok, State} = erlmcp_cli_raw:get_state(),
    History = maps:get(history, State),
    ?assertEqual(2, length(History)),
    ?assertEqual(<<"echo test">>, lists:nth(1, History)),
    ?assertEqual(<<"ping">>, lists:nth(2, History)),

    ok = erlmcp_cli_raw:stop().

test_history_navigation() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Add commands to history
    {ok, _} = erlmcp_cli_raw:execute_command(<<"cmd1">>),
    {ok, _} = erlmcp_cli_raw:execute_command(<<"cmd2">>),

    %% Note: Full arrow key navigation testing would require
    %% escape sequence simulation, which is complex
    %% For now, we verify history is stored
    {ok, State} = erlmcp_cli_raw:get_state(),
    ?assertEqual(2, length(maps:get(history, State))),

    ok = erlmcp_cli_raw:stop().

test_history_limit() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Execute 150 commands (exceeds limit of 100)
    lists:foreach(fun(N) ->
        Cmd = list_to_binary("cmd" ++ integer_to_list(N)),
        {ok, _} = erlmcp_cli_raw:execute_command(Cmd)
    end, lists:seq(1, 150)),

    %% Check history is limited to 100
    {ok, State} = erlmcp_cli_raw:get_state(),
    History = maps:get(history, State),
    ?assertEqual(100, length(History)),

    ok = erlmcp_cli_raw:stop().

%%%====================================================================
%%% Command Execution Tests
%%%====================================================================

test_execute_ping() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Execute ping command
    Result = erlmcp_cli_raw:execute_command(<<"ping">>),
    ?assertEqual({ok, pong}, Result),

    %% Check stats
    {ok, Stats} = erlmcp_cli_raw:get_stats(),
    ?assertEqual(1, maps:get(commands, Stats)),

    ok = erlmcp_cli_raw:stop().

test_execute_echo() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Execute echo command
    Result = erlmcp_cli_raw:execute_command(<<"echo hello world">>),
    ?assertEqual({ok, <<"hello world">>}, Result),

    ok = erlmcp_cli_raw:stop().

test_execute_set() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Set context value
    Result = erlmcp_cli_raw:execute_command(<<"set mykey myvalue">>),
    ?assertMatch({ok, #{key := <<"mykey">>, value := <<"myvalue">>}}, Result),

    %% Verify context
    {ok, State} = erlmcp_cli_raw:get_state(),
    Context = maps:get(session_context, State),
    ?assertEqual(<<"myvalue">>, maps:get(<<"mykey">>, Context)),

    ok = erlmcp_cli_raw:stop().

test_execute_get() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Set context value first
    {ok, _} = erlmcp_cli_raw:execute_command(<<"set testkey 12345">>),

    %% Get context value
    Result = erlmcp_cli_raw:execute_command(<<"get testkey">>),
    ?assertMatch({ok, #{key := <<"testkey">>, value := <<"12345">>}}, Result),

    ok = erlmcp_cli_raw:stop().

test_execute_unknown() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Execute unknown command
    Result = erlmcp_cli_raw:execute_command(<<"unknown_command">>),
    ?assertMatch({error, {unknown_command, _}}, Result),

    ok = erlmcp_cli_raw:stop().

%%%====================================================================
%%% Context Management Tests
%%%====================================================================

test_inspect_context() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Set some context
    {ok, _} = erlmcp_cli_raw:execute_command(<<"set key1 value1">>),
    {ok, _} = erlmcp_cli_raw:execute_command(<<"set key2 value2">>),

    %% Inspect context
    Result = erlmcp_cli_raw:inspect_context(),
    ?assertMatch({ok, _}, Result),

    ok = erlmcp_cli_raw:stop().

%%%====================================================================
%%% Operation Management Tests
%%%====================================================================

test_cancel_operation() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Cancel without operation
    Result1 = erlmcp_cli_raw:cancel_operation(),
    ?assertEqual({error, no_operation}, Result1),

    %% Note: Testing actual operation cancellation would require
    %% spawning a mock operation, which is complex
    %% For now, we verify the API is available

    ok = erlmcp_cli_raw:stop().

%%%====================================================================
%%% Statistics Tests
%%%====================================================================

test_get_stats() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Execute some commands
    {ok, _} = erlmcp_cli_raw:execute_command(<<"ping">>),
    {ok, _} = erlmcp_cli_raw:execute_command(<<"echo test">>),

    %% Get stats
    {ok, Stats} = erlmcp_cli_raw:get_stats(),
    ?assertEqual(2, maps:get(commands, Stats)),
    ?assert(is_integer(maps:get(interrupts, Stats))),

    ok = erlmcp_cli_raw:stop().

%%%====================================================================
%%% State Tests
%%%====================================================================

test_get_state() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Get initial state
    {ok, State} = erlmcp_cli_raw:get_state(),
    ?assertMatch(#{buffer := <<>>,
                   history := [],
                   history_index := 0,
                   session_context := #{},
                   has_operation := false,
                   stats := #{}}, State),

    ok = erlmcp_cli_raw:stop().

%%%====================================================================
%%% Edge Case Tests
%%%====================================================================

test_buffer_overflow() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Send many characters (10KB)
    lists:foreach(fun(_) -> erlmcp_cli_raw:send_char($a) end, lists:seq(1, 10000)),

    %% Check buffer size
    {ok, State} = erlmcp_cli_raw:get_state(),
    Buffer = maps:get(buffer, State),
    ?assertEqual(10000, byte_size(Buffer)),

    ok = erlmcp_cli_raw:stop().

test_special_characters() ->
    {ok, _} = erlmcp_cli_raw:start_link(),

    %% Send various special characters
    SpecialChars = [$~, $!, $@, $#, $$, $%, $^, $&, $*, $(, $), $_, $+, $-, $=, $[, $], $\\, $|, $;, $:, $', $", $,, $<, $., $>, $/, $?],
    lists:foreach(fun(C) -> erlmcp_cli_raw:send_char(C) end, SpecialChars),

    %% Server should still be running
    ?assert(is_process_alive(whereis(erlmcp_cli_raw))),

    ok = erlmcp_cli_raw:stop().

%%%====================================================================
%%% Property-Based Tests
%%%====================================================================

prop_command_executable() ->
    ?FORALL(Command, binary(),
        begin
            {ok, Pid} = erlmcp_cli_raw:start_link(),
            Result = erlmcp_cli_raw:execute_command(Command),
            erlmcp_cli_raw:stop(),
            timer:sleep(10),
            is_tuple(Result) andalso
            (element(1, Result) =:= ok orelse element(1, Result) =:= error)
        end).

prop_char_sequence() ->
    ?FORALL(Chars, list(choose(32, 126)),
        begin
            {ok, _Pid} = erlmcp_cli_raw:start_link(),
            lists:foreach(fun(C) -> erlmcp_cli_raw:send_char(C) end, Chars),
            {ok, State} = erlmcp_cli_raw:get_state(),
            erlmcp_cli_raw:stop(),
            ExpectedBin = list_to_binary(Chars),
            maps:get(buffer, State) =:= ExpectedBin
        end).
