%%%-------------------------------------------------------------------
%%% @doc
%%% Interactive CLI Test Suite (EUnit)
%%%
%%% Tests for erlmcp_cli_interactive module - REPL-style command interface
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real gen_server for REPL state
%%% - NO mocks, real command execution
%%% - State-based verification (command history, session state)
%%%
%%% Coverage Target: ≥85%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_interactive_tests).
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

cli_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Start interactive session", fun test_start_session/0},
      {"Stop interactive session", fun test_stop_session/0},
      {"Parse simple command", fun test_parse_simple_command/0},
      {"Parse command with arguments", fun test_parse_command_with_args/0},
      {"Parse command with flags", fun test_parse_command_with_flags/0},
      {"Parse invalid command", fun test_parse_invalid_command/0},
      {"Execute help command", fun test_execute_help/0},
      {"Execute version command", fun test_execute_version/0},
      {"Execute exit command", fun test_execute_exit/0},
      {"Command history add", fun test_history_add/0},
      {"Command history retrieve", fun test_history_retrieve/0},
      {"Command history limit", fun test_history_limit/0},
      {"Command history navigation", fun test_history_navigation/0},
      {"Tab completion trigger", fun test_tab_completion_trigger/0},
      {"Ctrl+C signal handling", fun test_ctrl_c_handling/0},
      {"Ctrl+D EOF handling", fun test_ctrl_d_handling/0},
      {"Multi-line command", fun test_multiline_command/0},
      {"Command aliases", fun test_command_aliases/0},
      {"Environment variables", fun test_environment_variables/0},
      {"Session state persistence", fun test_session_state/0},
      {"Prompt customization", fun test_custom_prompt/0},
      {"Error recovery", fun test_error_recovery/0},
      {"Concurrent command execution", fun test_concurrent_commands/0},
      {"Command timeout", fun test_command_timeout/0},
      {"ANSI color support", fun test_ansi_colors/0},
      {"Input sanitization", fun test_input_sanitization/0},
      {"Command chaining with &&", fun test_command_chaining/0},
      {"Command piping", fun test_command_piping/0},
      {"Background execution", fun test_background_execution/0},
      {"Auto-suggestion display", fun test_auto_suggestions/0}
     ]}.

setup() ->
    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_Args) ->
    ok.

%%%====================================================================
%%% Session Management Tests
%%%====================================================================

test_start_session() ->
    %% Start a real interactive session (gen_server)
    {ok, Pid} = erlmcp_cli_interactive:start_link(),
    ?assert(is_pid(Pid)),

    %% Verify initial state
    {ok, State} = erlmcp_cli_interactive:get_state(Pid),
    ?assertMatch(#{history := [], prompt := _}, State),

    %% Cleanup
    ok = erlmcp_cli_interactive:stop(Pid).

test_stop_session() ->
    %% Start session
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Stop gracefully
    ok = erlmcp_cli_interactive:stop(Pid),

    %% Verify process stopped
    timer:sleep(50),
    ?assertEqual(undefined, whereis(erlmcp_cli_interactive)).

%%%====================================================================
%%% Command Parsing Tests
%%%====================================================================

test_parse_simple_command() ->
    %% Test: "help" → {ok, {help, []}}
    Input = "help",
    Result = erlmcp_cli_interactive:parse_command(Input),
    ?assertMatch({ok, {help, [], #{}}}, Result).

test_parse_command_with_args() ->
    %% Test: "validate spec" → {ok, {validate, [spec], #{}}}
    Input = "validate spec",
    Result = erlmcp_cli_interactive:parse_command(Input),
    ?assertMatch({ok, {validate, [<<"spec">>], #{}}}, Result).

test_parse_command_with_flags() ->
    %% Test: "validate spec --format json" → {ok, {validate, [spec], #{format => json}}}
    Input = "validate spec --format json",
    Result = erlmcp_cli_interactive:parse_command(Input),
    ?assertMatch({ok, {validate, [<<"spec">>], #{format := <<"json">>}}}, Result).

test_parse_invalid_command() ->
    %% Test: empty string → {error, empty_command}
    ?assertEqual({error, empty_command}, erlmcp_cli_interactive:parse_command("")),

    %% Test: whitespace only → {error, empty_command}
    ?assertEqual({error, empty_command}, erlmcp_cli_interactive:parse_command("   ")),

    %% Test: invalid characters
    Result = erlmcp_cli_interactive:parse_command("test\x00command"),
    ?assertMatch({error, {invalid_character, _}}, Result).

%%%====================================================================
%%% Command Execution Tests
%%%====================================================================

test_execute_help() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Execute help command
    {ok, Output} = erlmcp_cli_interactive:execute(Pid, "help"),

    %% Verify help text contains expected commands
    ?assert(is_binary(Output)),
    ?assert(byte_size(Output) > 0),
    ?assert(binary:match(Output, <<"validate">>) =/= nomatch),

    ok = erlmcp_cli_interactive:stop(Pid).

test_execute_version() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Execute version command
    {ok, Output} = erlmcp_cli_interactive:execute(Pid, "version"),

    %% Verify version format
    ?assert(is_binary(Output)),
    ?assert(binary:match(Output, <<"erlmcp">>) =/= nomatch),

    ok = erlmcp_cli_interactive:stop(Pid).

test_execute_exit() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Execute exit command
    Result = erlmcp_cli_interactive:execute(Pid, "exit"),

    %% Should return exit signal
    ?assertMatch({exit, _Reason}, Result).

%%%====================================================================
%%% Command History Tests
%%%====================================================================

test_history_add() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Add commands to history
    ok = erlmcp_cli_interactive:add_to_history(Pid, "help"),
    ok = erlmcp_cli_interactive:add_to_history(Pid, "version"),

    %% Retrieve history
    {ok, History} = erlmcp_cli_interactive:get_history(Pid),

    %% Verify commands added (LIFO order)
    ?assertEqual([<<"version">>, <<"help">>], History),

    ok = erlmcp_cli_interactive:stop(Pid).

test_history_retrieve() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Add commands
    ok = erlmcp_cli_interactive:add_to_history(Pid, "cmd1"),
    ok = erlmcp_cli_interactive:add_to_history(Pid, "cmd2"),
    ok = erlmcp_cli_interactive:add_to_history(Pid, "cmd3"),

    %% Get last command
    {ok, LastCmd} = erlmcp_cli_interactive:get_history_entry(Pid, 0),
    ?assertEqual(<<"cmd3">>, LastCmd),

    %% Get second-to-last
    {ok, SecondCmd} = erlmcp_cli_interactive:get_history_entry(Pid, 1),
    ?assertEqual(<<"cmd2">>, SecondCmd),

    ok = erlmcp_cli_interactive:stop(Pid).

test_history_limit() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(#{history_limit => 5}),

    %% Add 10 commands
    [ok = erlmcp_cli_interactive:add_to_history(Pid, "cmd" ++ integer_to_list(N))
     || N <- lists:seq(1, 10)],

    %% Get history
    {ok, History} = erlmcp_cli_interactive:get_history(Pid),

    %% Should only keep last 5
    ?assertEqual(5, length(History)),
    ?assertEqual([<<"cmd10">>, <<"cmd9">>, <<"cmd8">>, <<"cmd7">>, <<"cmd6">>], History),

    ok = erlmcp_cli_interactive:stop(Pid).

test_history_navigation() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Add commands
    ok = erlmcp_cli_interactive:add_to_history(Pid, "cmd1"),
    ok = erlmcp_cli_interactive:add_to_history(Pid, "cmd2"),

    %% Navigate up (previous command)
    {ok, Cmd1} = erlmcp_cli_interactive:history_prev(Pid),
    ?assertEqual(<<"cmd2">>, Cmd1),

    %% Navigate up again
    {ok, Cmd2} = erlmcp_cli_interactive:history_prev(Pid),
    ?assertEqual(<<"cmd1">>, Cmd2),

    %% Navigate down (next command)
    {ok, Cmd3} = erlmcp_cli_interactive:history_next(Pid),
    ?assertEqual(<<"cmd2">>, Cmd3),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Tab Completion Tests
%%%====================================================================

test_tab_completion_trigger() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Trigger tab completion for partial command
    {ok, Completions} = erlmcp_cli_interactive:complete(Pid, "val"),

    %% Should suggest "validate"
    ?assert(lists:member(<<"validate">>, Completions)),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Signal Handling Tests
%%%====================================================================

test_ctrl_c_handling() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Send Ctrl+C signal
    ok = erlmcp_cli_interactive:handle_signal(Pid, ctrl_c),

    %% Should interrupt current command, not exit
    {ok, State} = erlmcp_cli_interactive:get_state(Pid),
    ?assertMatch(#{interrupted := true}, State),

    ok = erlmcp_cli_interactive:stop(Pid).

test_ctrl_d_handling() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Send Ctrl+D (EOF)
    Result = erlmcp_cli_interactive:handle_signal(Pid, ctrl_d),

    %% Should trigger exit
    ?assertMatch({exit, eof}, Result).

%%%====================================================================
%%% Multi-line Command Tests
%%%====================================================================

test_multiline_command() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Start multi-line command with backslash
    ok = erlmcp_cli_interactive:add_line(Pid, "validate spec \\"),
    ok = erlmcp_cli_interactive:add_line(Pid, "--format json"),

    %% Get accumulated command
    {ok, FullCmd} = erlmcp_cli_interactive:get_accumulated_command(Pid),
    ?assertEqual(<<"validate spec --format json">>, FullCmd),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Alias Tests
%%%====================================================================

test_command_aliases() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Define alias: "v" → "validate"
    ok = erlmcp_cli_interactive:add_alias(Pid, "v", "validate"),

    %% Use alias
    {ok, Parsed} = erlmcp_cli_interactive:parse_command("v spec"),
    ?assertMatch({ok, {validate, [<<"spec">>], #{}}}, Parsed),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Environment Variable Tests
%%%====================================================================

test_environment_variables() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Set environment variable
    ok = erlmcp_cli_interactive:set_env(Pid, "FORMAT", "json"),

    %% Get environment variable
    {ok, Value} = erlmcp_cli_interactive:get_env(Pid, "FORMAT"),
    ?assertEqual(<<"json">>, Value),

    %% Use in command: "validate spec --format $FORMAT"
    {ok, Expanded} = erlmcp_cli_interactive:expand_vars(Pid, "validate spec --format $FORMAT"),
    ?assertEqual(<<"validate spec --format json">>, Expanded),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Session State Tests
%%%====================================================================

test_session_state() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Add history
    ok = erlmcp_cli_interactive:add_to_history(Pid, "cmd1"),

    %% Set env
    ok = erlmcp_cli_interactive:set_env(Pid, "VAR", "value"),

    %% Get full session state
    {ok, State} = erlmcp_cli_interactive:get_state(Pid),

    %% Verify state contains history and env
    ?assertMatch(#{history := [<<"cmd1">>], env := #{<<"VAR">> := <<"value">>}}, State),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Prompt Customization Tests
%%%====================================================================

test_custom_prompt() ->
    %% Start with custom prompt
    {ok, Pid} = erlmcp_cli_interactive:start_link(#{prompt => "erlmcp> "}),

    %% Get prompt
    {ok, Prompt} = erlmcp_cli_interactive:get_prompt(Pid),
    ?assertEqual(<<"erlmcp> ">>, Prompt),

    %% Change prompt
    ok = erlmcp_cli_interactive:set_prompt(Pid, "new> "),
    {ok, NewPrompt} = erlmcp_cli_interactive:get_prompt(Pid),
    ?assertEqual(<<"new> ">>, NewPrompt),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Error Recovery Tests
%%%====================================================================

test_error_recovery() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Execute invalid command
    {error, Reason} = erlmcp_cli_interactive:execute(Pid, "invalid_command"),
    ?assertMatch({unknown_command, _}, Reason),

    %% Session should still be alive and functional
    ?assert(is_process_alive(Pid)),

    %% Execute valid command
    {ok, _} = erlmcp_cli_interactive:execute(Pid, "help"),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Concurrency Tests
%%%====================================================================

test_concurrent_commands() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Spawn 10 concurrent command executions
    Pids = [spawn(fun() ->
        {ok, _} = erlmcp_cli_interactive:execute(Pid, "help")
    end) || _ <- lists:seq(1, 10)],

    %% Wait for all to complete
    [begin
        Ref = monitor(process, P),
        receive {'DOWN', Ref, process, P, _} -> ok after 5000 -> error(timeout) end
    end || P <- Pids],

    %% Session should be stable
    ?assert(is_process_alive(Pid)),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Timeout Tests
%%%====================================================================

test_command_timeout() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(#{command_timeout => 1000}),

    %% Execute long-running command (mock)
    Result = erlmcp_cli_interactive:execute(Pid, "sleep 5000"),

    %% Should timeout
    ?assertMatch({error, timeout}, Result),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% ANSI Color Tests
%%%====================================================================

test_ansi_colors() ->
    %% Enable ANSI colors
    {ok, Pid} = erlmcp_cli_interactive:start_link(#{ansi_colors => true}),

    %% Format error message with color
    Formatted = erlmcp_cli_interactive:format_error(Pid, "test error"),

    %% Should contain ANSI escape codes
    ?assert(is_binary(Formatted)),
    ?assert(binary:match(Formatted, <<"\e[">>) =/= nomatch),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Input Sanitization Tests
%%%====================================================================

test_input_sanitization() ->
    %% Test null bytes
    Result1 = erlmcp_cli_interactive:parse_command("test\x00command"),
    ?assertMatch({error, {invalid_character, _}}, Result1),

    %% Test excessive whitespace
    {ok, Parsed} = erlmcp_cli_interactive:parse_command("  help   "),
    ?assertMatch({ok, {help, [], #{}}}, Parsed),

    %% Test special characters
    {ok, Parsed2} = erlmcp_cli_interactive:parse_command("echo \"hello world\""),
    ?assertMatch({ok, {echo, [<<"hello world">>], #{}}}, Parsed2).

%%%====================================================================
%%% Command Chaining Tests
%%%====================================================================

test_command_chaining() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Execute chained commands: "help && version"
    {ok, Results} = erlmcp_cli_interactive:execute(Pid, "help && version"),

    %% Should execute both commands
    ?assertMatch([{ok, _}, {ok, _}], Results),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Command Piping Tests
%%%====================================================================

test_command_piping() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Execute piped commands: "validate spec | grep error"
    {ok, Output} = erlmcp_cli_interactive:execute(Pid, "validate spec | grep error"),

    %% Should pipe output through grep
    ?assert(is_binary(Output)),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Background Execution Tests
%%%====================================================================

test_background_execution() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Execute command in background: "sleep 1000 &"
    {ok, JobId} = erlmcp_cli_interactive:execute(Pid, "sleep 1000 &"),

    %% Should return job ID
    ?assert(is_integer(JobId)),

    %% Session should not block
    {ok, _} = erlmcp_cli_interactive:execute(Pid, "help"),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Auto-suggestion Tests
%%%====================================================================

test_auto_suggestions() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Get suggestions for partial input
    {ok, Suggestions} = erlmcp_cli_interactive:suggest(Pid, "val"),

    %% Should suggest commands starting with "val"
    ?assert(lists:member(<<"validate">>, Suggestions)),

    ok = erlmcp_cli_interactive:stop(Pid).

%%%====================================================================
%%% Edge Case Tests
%%%====================================================================

edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Empty history navigation", fun test_empty_history_navigation/0},
      {"History overflow", fun test_history_overflow/0},
      {"Invalid alias definition", fun test_invalid_alias/0},
      {"Circular alias reference", fun test_circular_alias/0},
      {"Unicode input", fun test_unicode_input/0},
      {"Very long command", fun test_long_command/0}
     ]}.

test_empty_history_navigation() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Navigate history when empty
    Result = erlmcp_cli_interactive:history_prev(Pid),
    ?assertMatch({error, empty_history}, Result),

    ok = erlmcp_cli_interactive:stop(Pid).

test_history_overflow() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(#{history_limit => 2}),

    %% Add 5 commands
    [ok = erlmcp_cli_interactive:add_to_history(Pid, "cmd" ++ integer_to_list(N))
     || N <- lists:seq(1, 5)],

    %% Should only keep last 2
    {ok, History} = erlmcp_cli_interactive:get_history(Pid),
    ?assertEqual(2, length(History)),

    ok = erlmcp_cli_interactive:stop(Pid).

test_invalid_alias() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Try to create alias with invalid name
    Result = erlmcp_cli_interactive:add_alias(Pid, "", "validate"),
    ?assertMatch({error, invalid_alias_name}, Result),

    ok = erlmcp_cli_interactive:stop(Pid).

test_circular_alias() ->
    {ok, Pid} = erlmcp_cli_interactive:start_link(),

    %% Create circular alias: a -> b, b -> a
    ok = erlmcp_cli_interactive:add_alias(Pid, "a", "b"),
    Result = erlmcp_cli_interactive:add_alias(Pid, "b", "a"),

    %% Should detect circular reference
    ?assertMatch({error, circular_alias}, Result),

    ok = erlmcp_cli_interactive:stop(Pid).

test_unicode_input() ->
    %% Test Unicode command
    {ok, Parsed} = erlmcp_cli_interactive:parse_command("echo \"hello 世界\""),
    ?assertMatch({ok, {echo, [<<"hello 世界">>], #{}}}, Parsed).

test_long_command() ->
    %% Test very long command (10KB)
    LongCmd = binary:copy(<<"a">>, 10000),
    Result = erlmcp_cli_interactive:parse_command(LongCmd),

    %% Should handle or reject gracefully
    ?assert(is_tuple(Result)).
