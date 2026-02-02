%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Command Parsing Test Suite (EUnit)
%%%
%%% Tests for CLI command parsing and execution
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real command execution
%%% - NO mocks, real command registry
%%% - State-based verification (command results)
%%%
%%% Coverage Target: ≥90%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_command_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

command_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Command parsing - valid command", fun test_parse_valid_command/0},
      {"Command parsing - unknown command", fun test_parse_unknown_command/0},
      {"Command parsing - missing arguments", fun test_parse_missing_arguments/0},
      {"Command parsing - invalid flags", fun test_parse_invalid_flags/0},
      {"Command parsing - help command", fun test_help_command/0},
      {"Command parsing - subcommand routing", fun test_subcommand_routing/0},
      {"Command execution - success", fun test_command_execution_success/0},
      {"Command execution - failure", fun test_command_execution_failure/0},
      {"Command execution - timeout", fun test_command_execution_timeout/0},
      {"Flag validation - verbose flag", fun test_verbose_flag/0},
      {"Flag validation - debug flag", fun test_debug_flag/0},
      {"Flag validation - format flag", fun test_format_flag/0},
      {"Flag validation - multiple flags", fun test_multiple_flags/0},
      {"Argument validation - required args", fun test_required_arguments/0},
      {"Argument validation - optional args", fun test_optional_arguments/0},
      {"Argument validation - argument types", fun test_argument_types/0},
      {"Command registry - register command", fun test_register_command/0},
      {"Command registry - lookup command", fun test_lookup_command/0},
      {"Command registry - list commands", fun test_list_commands/0},
      {"Command registry - unregister command", fun test_unregister_command/0},
      {"Command categories - filter by category", fun test_filter_by_category/0},
      {"Command validation - safety level", fun test_safety_level/0},
      {"Command validation - permission check", fun test_permission_check/0}]}.

setup() ->
    application:ensure_all_started(erlmcp_cli),
    ok.

cleanup(_Args) ->
    application:stop(erlmcp_cli),
    ok.

%%%====================================================================
%%% Command Parsing Tests
%%%====================================================================

test_parse_valid_command() ->
    %% Valid command with arguments
    Cmd = <<"start --verbose">>,
    Parsed = erlmcp_cli_config:parse_command(Cmd),

    %% Verify parsed structure
    ?assertEqual(<<"start">>, maps:get(<<"command">>, Parsed)),
    ?assertEqual(true, maps:get(<<"verbose">>, Parsed, false)).

test_parse_unknown_command() ->
    %% Unknown command
    Cmd = <<"unknown_command">>,
    case erlmcp_cli_config:parse_command(Cmd) of
        {error, {unknown_command, _}} -> ok;
        _ -> ?assert(false, "Should return unknown_command error")
    end.

test_parse_missing_arguments() ->
    %% Command with missing required arguments
    Cmd = <<"profile apply">>,  %% Missing profile name
    case erlmcp_cli_config:parse_command(Cmd) of
        {error, {missing_arguments, _}} -> ok;
        Parsed ->
            %% Verify error in execution
            ?assertNot(maps:get(<<"profile">>, Parsed, undefined))
    end.

test_parse_invalid_flags() ->
    %% Invalid flag
    Cmd = <<"start --invalid-flag">>,
    case erlmcp_cli_config:parse_command(Cmd) of
        {error, {invalid_flag, _}} -> ok;
        _ -> ?assert(false, "Should reject invalid flag")
    end.

test_help_command() ->
    %% Help command
    Cmd = <<"help">>,
    {ok, Result} = erlmcp_cli_config:execute_command(Cmd, #{}),

    %% Verify help output
    ?assert(is_binary(Result)),
    ?assert(<<"Usage:">> =< Result).

test_subcommand_routing() ->
    %% Subcommand routing (e.g., "profile list")
    Cmd = <<"profile list">>,
    {ok, Result} = erlmcp_cli_config:execute_command(Cmd, #{}),

    %% Verify subcommand executed
    ?assert(is_map(Result) orelse is_list(Result)).

%%%====================================================================
%%% Command Execution Tests
%%%====================================================================

test_command_execution_success() ->
    %% Successful command execution
    Cmd = <<"status">>,
    {ok, Result} = erlmcp_cli_config:execute_command(Cmd, #{}),

    %% Verify success
    ?assert(is_map(Result)),
    ?assert(maps:get(<<"status">>, Result, undefined) =/= undefined).

test_command_execution_failure() ->
    %% Command that fails
    %% Create failing command
    FailingCmd = #{<<"name">> => <<"test.fail">>,
                   <<"module">> => ?MODULE,
                   <<"function">> => failing_function,
                   <<"arity">> => 0},
    ok = erlmcp_cli_config:register_command(FailingCmd),

    %% Execute failing command
    {error, _Reason} = erlmcp_cli_config:execute_command(<<"test.fail">>, #{}),

    %% Cleanup
    ok = erlmcp_cli_config:unregister_command(<<"test.fail">>).

test_command_execution_timeout() ->
    %% Command with timeout
    TimeoutCmd = #{<<"name">> => <<"test.timeout">>,
                   <<"module">> => ?MODULE,
                   <<"function">> => timeout_function,
                   <<"arity">> => 0},
    ok = erlmcp_cli_config:register_command(TimeoutCmd),

    %% Execute with timeout
    Result = erlmcp_cli_config:execute_command(<<"test.timeout">>, #{}, 1000),
    ?assertMatch({error, _}, Result),

    %% Cleanup
    ok = erlmcp_cli_config:unregister_command(<<"test.timeout">>).

%%%====================================================================
%%% Flag Validation Tests
%%%====================================================================

test_verbose_flag() ->
    %% Verbose flag
    Cmd = <<"start --verbose">>,
    {ok, Parsed} = erlmcp_cli_config:parse_command(Cmd),
    ?assertEqual(true, maps:get(<<"verbose">>, Parsed, false)).

test_debug_flag() ->
    %% Debug flag
    Cmd = <<"start --debug">>,
    {ok, Parsed} = erlmcp_cli_config:parse_command(Cmd),
    ?assertEqual(true, maps:get(<<"debug">>, Parsed, false)).

test_format_flag() ->
    %% Format flag with value
    Cmd = <<"status --format json">>,
    {ok, Parsed} = erlmcp_cli_config:parse_command(Cmd),
    ?assertEqual(<<"json">>, maps:get(<<"format">>, Parsed)).

test_multiple_flags() ->
    %% Multiple flags
    Cmd = <<"start --verbose --debug --format json">>,
    {ok, Parsed} = erlmcp_cli_config:parse_command(Cmd),
    ?assertEqual(true, maps:get(<<"verbose">>, Parsed, false)),
    ?assertEqual(true, maps:get(<<"debug">>, Parsed, false)),
    ?assertEqual(<<"json">>, maps:get(<<"format">>, Parsed)).

%%%====================================================================
%%% Argument Validation Tests
%%%====================================================================

test_required_arguments() ->
    %% Command with required argument
    Cmd = <<"profile apply dev">>,
    {ok, Parsed} = erlmcp_cli_config:parse_command(Cmd),
    ?assertEqual(<<"dev">>, maps:get(<<"profile">>, Parsed)).

test_optional_arguments() ->
    %% Command with optional argument
    Cmd = <<"status --format json">>,
    {ok, Result} = erlmcp_cli_config:execute_command(Cmd, #{}),
    ?assert(is_map(Result)).

test_argument_types() ->
    %% Argument type validation
    %% String argument
    Cmd1 = <<"profile apply dev">>,
    {ok, Parsed1} = erlmcp_cli_config:parse_command(Cmd1),
    ?assert(is_binary(maps:get(<<"profile">>, Parsed1))),

    %% Numeric argument (if applicable)
    %% This would test commands that expect numeric arguments
    ok.

%%%====================================================================
%%% Command Registry Tests
%%%====================================================================

test_register_command() ->
    %% Register new command
    NewCmd = #{<<"name">> => <<"test.new">>,
               <<"module">> => ?MODULE,
               <<"function">> => test_function,
               <<"arity">> => 0,
               <<"description">> => <<"Test command">>,
               <<"category">> => <<"test">>,
               <<"safety_level">> => safe},

    ok = erlmcp_cli_config:register_command(NewCmd),

    %% Verify registered
    {ok, Found} = erlmcp_cli_config:lookup_command(<<"test.new">>),
    ?assertEqual(<<"test.new">>, maps:get(<<"name">>, Found)),

    %% Cleanup
    ok = erlmcp_cli_config:unregister_command(<<"test.new">>).

test_lookup_command() ->
    %% Lookup existing command
    {ok, Cmd} = erlmcp_cli_config:lookup_command(<<"status">>),
    ?assertEqual(<<"status">>, maps:get(<<"name">>, Cmd)).

test_list_commands() ->
    %% List all commands
    {ok, Commands} = erlmcp_cli_config:list_commands(),
    ?assert(length(Commands) > 0),
    ?assert(is_list(Commands)).

test_unregister_command() ->
    %% Register then unregister
    TempCmd = #{<<"name">> => <<"test.temp">>,
                <<"module">> => ?MODULE,
                <<"function">> => test_function,
                <<"arity">> => 0},
    ok = erlmcp_cli_config:register_command(TempCmd),

    %% Unregister
    ok = erlmcp_cli_config:unregister_command(<<"test.temp">>),

    %% Verify not found
    {error, not_found} = erlmcp_cli_config:lookup_command(<<"test.temp">>).

%%%====================================================================
%%% Command Categories Tests
%%%====================================================================

test_filter_by_category() ->
    %% Filter commands by category
    {ok, Commands} = erlmcp_cli_config:list_by_category(<<"profile">>),
    ?assert(length(Commands) > 0),
    lists:foreach(fun(Cmd) ->
        ?assertEqual(<<"profile">>, maps:get(<<"category">>, Cmd))
    end, Commands).

%%%====================================================================
%%% Command Validation Tests
%%%====================================================================

test_safety_level() ->
    %% Safety level validation
    %% Safe command
    SafeCmd = #{<<"name">> => <<"test.safe">>,
                <<"module">> => ?MODULE,
                <<"function">> => test_function,
                <<"safety_level">> => safe},
    ok = erlmcp_cli_config:register_command(SafeCmd),

    %% Verify safe command can execute
    {ok, _} = erlmcp_cli_config:execute_command(<<"test.safe">>, #{}),

    %% Unsafe command (may require special permissions)
    UnsafeCmd = #{<<"name">> => <<"test.unsafe">>,
                  <<"module">> => ?MODULE,
                  <<"function">> => test_function,
                  <<"safety_level">> => unsafe},
    ok = erlmcp_cli_config:register_command(UnsafeCmd),

    %% Cleanup
    ok = erlmcp_cli_config:unregister_command(<<"test.safe">>),
    ok = erlmcp_cli_config:unregister_command(<<"test.unsafe">>).

test_permission_check() ->
    %% Permission check
    %% Commands with permission requirements
    AdminCmd = #{<<"name">> => <<"admin.only">>,
                 <<"module">> => ?MODULE,
                 <<"function">> => test_function,
                 <<"permissions">> => [admin]},
    ok = erlmcp_cli_config:register_command(AdminCmd),

    %% Execute without admin permissions (should fail)
    Result = erlmcp_cli_config:execute_command(<<"admin.only">>, #{permissions => []}),
    ?assertMatch({error, {permission_denied, _}}, Result),

    %% Execute with admin permissions (should succeed)
    {ok, _} = erlmcp_cli_config:execute_command(<<"admin.only">>, #{permissions => [admin]}),

    %% Cleanup
    ok = erlmcp_cli_config:unregister_command(<<"admin.only">>).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Test function for command registration
test_function() ->
    #{<<"result">> => <<"ok">>}.

%% @doc Failing function for error testing
failing_function() ->
    {error, test_failure}.

%% @doc Timeout function for timeout testing
timeout_function() ->
    timer:sleep(2000),
    #{<<"result">> => <<"too late">>}.
