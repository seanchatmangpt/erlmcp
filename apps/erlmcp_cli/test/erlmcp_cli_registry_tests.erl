%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Registry Test Suite (EUnit)
%%%
%%% Tests for erlmcp_cli_registry module - Command registry and execution
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real gen_server for registry state
%%% - NO mocks, real command execution
%%% - State-based verification (command registration, metrics)
%%%
%%% Coverage Target: ≥90%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

registry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Command registration - register valid command", fun test_register_valid_command/0},
      {"Command registration - register duplicate command", fun test_register_duplicate_command/0},
      {"Command registration - register command with invalid name", fun test_register_invalid_command_name/0},
      {"Command registration - register command with invalid module", fun test_register_invalid_module/0},
      {"Command lookup - lookup existing command", fun test_lookup_existing_command/0},
      {"Command lookup - lookup nonexistent command", fun test_lookup_nonexistent_command/0},
      {"Command lookup - lookup by category", fun test_lookup_by_category/0},
      {"Command execution - execute valid command", fun test_execute_valid_command/0},
      {"Command execution - execute with arguments", fun test_execute_with_arguments/0},
      {"Command execution - execute invalid command", fun test_execute_invalid_command/0},
      {"Command execution - execute with invalid arguments", fun test_execute_invalid_arguments/0},
      {"Command execution - safety level validation", fun test_safety_level_validation/0},
      {"Command execution - concurrent execution", fun test_concurrent_execution/0},
      {"Metrics - initial metrics", fun test_initial_metrics/0},
      {"Metrics - metrics increment", fun test_metrics_increment/0},
      {"Metrics - get metrics", fun test_get_metrics/0},
      {"Metrics - reset metrics", fun test_reset_metrics/0},
      {"Category - list categories", fun test_list_categories/0},
      {"Category - filter by safety level", fun test_filter_by_safety_level/0},
      {"Registry state - get all commands", fun test_get_all_commands/0},
      {"Registry state - command count", fun test_command_count/0},
      {"Registry state - unregister command", fun test_unregister_command/0},
      {"Registry state - clear all commands", fun test_clear_all_commands/0}]}.

setup() ->
    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(erlmcp_cli),
    ok.

cleanup(_Args) ->
    %% Clean up registry state
    case whereis(erlmcp_cli_registry) of
        undefined -> ok;
        Pid -> erlmcp_cli_registry:reset()
    end,
    ok.

%%%====================================================================
%%% Command Registration Tests
%%%====================================================================

test_register_valid_command() ->
    %% Register valid command
    Command = #{
        name => <<"test.command1">>,
        module => erlmcp_cli_registry,
        function => test_function,
        arity => 1,
        description => <<"Test command">>,
        category => <<"test">>,
        safety_level => safe
    },

    ok = erlmcp_cli_registry:register_command(Command),

    %% Verify command registered
    {ok, Registered} = erlmcp_cli_registry:lookup_command(<<"test.command1">>),
    ?assertEqual(<<"test.command1">>, maps:get(name, Registered)),
    ?assertEqual(<<"Test command">>, maps:get(description, Registered)).

test_register_duplicate_command() ->
    %% Register command
    Command = #{
        name => <<"test.dup1">>,
        module => erlmcp_cli_registry,
        function => test_function,
        arity => 1,
        description => <<"Duplicate test">>,
        category => <<"test">>,
        safety_level => safe
    },

    ok = erlmcp_cli_registry:register_command(Command),

    %% Attempt to register duplicate
    {error, command_already_exists} = erlmcp_cli_registry:register_command(Command).

test_register_invalid_command_name() ->
    %% Invalid command name (not a binary)
    Command = #{
        name => invalid_name,
        module => erlmcp_cli_registry,
        function => test_function,
        arity => 1,
        description => <<"Test">>,
        category => <<"test">>,
        safety_level => safe
    },

    {error, invalid_command_name} = erlmcp_cli_registry:register_command(Command).

test_register_invalid_module() ->
    %% Invalid module (not an atom)
    Command = #{
        name => <<"test.invalid_module">>,
        module => <<"not_an_atom">>,
        function => test_function,
        arity => 1,
        description => <<"Test">>,
        category => <<"test">>,
        safety_level => safe
    },

    {error, invalid_module} = erlmcp_cli_registry:register_command(Command).

%%%====================================================================
%%% Command Lookup Tests
%%%====================================================================

test_lookup_existing_command() ->
    %% Register command
    Command = #{
        name => <<"test.lookup1">>,
        module => erlmcp_cli_registry,
        function => test_function,
        arity => 1,
        description => <<"Lookup test">>,
        category => <<"test">>,
        safety_level => safe
    },

    ok = erlmcp_cli_registry:register_command(Command),

    %% Lookup command
    {ok, Found} = erlmcp_cli_registry:lookup_command(<<"test.lookup1">>),
    ?assertEqual(<<"test.lookup1">>, maps:get(name, Found)),
    ?assertEqual(erlmcp_cli_registry, maps:get(module, Found)).

test_lookup_nonexistent_command() ->
    %% Lookup nonexistent command
    {error, command_not_found} = erlmcp_cli_registry:lookup_command(<<"nonexistent.command">>).

test_lookup_by_category() ->
    %% Register multiple commands in same category
    Commands = [
        #{name => <<"test.cat1">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Test 1">>, category => <<"category1">>, safety_level => safe},
        #{name => <<"test.cat2">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Test 2">>, category => <<"category1">>, safety_level => safe},
        #{name => <<"test.cat3">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Test 3">>, category => <<"category2">>, safety_level => safe}
    ],

    lists:foreach(fun(Cmd) -> erlmcp_cli_registry:register_command(Cmd) end, Commands),

    %% Lookup by category
    {ok, Category1Commands} = erlmcp_cli_registry:lookup_by_category(<<"category1">>),
    ?assertEqual(2, length(Category1Commands)),

    {ok, Category2Commands} = erlmcp_cli_registry:lookup_by_category(<<"category2">>),
    ?assertEqual(1, length(Category2Commands)).

%%%====================================================================
%%% Command Execution Tests
%%%====================================================================

test_execute_valid_command() ->
    %% Execute registered command (mcp.health is built-in)
    Result = erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),

    %% Verify result
    ?assert(is_map(Result)),
    ?assert(maps:get(<<"status">>, Result, undefined) =/= undefined).

test_execute_with_arguments() ->
    %% Execute command with arguments
    Args = [<<"arg1">>, <<"arg2">>],
    Result = erlmcp_cli_registry:execute_command(<<"mcp.health">>, Args),

    %% Verify result structure
    ?assert(is_map(Result)).

test_execute_invalid_command() ->
    %% Execute nonexistent command
    {error, command_not_found} = erlmcp_cli_registry:execute_command(<<"nonexistent.command">>, []).

test_execute_invalid_arguments() ->
    %% Execute with invalid arguments
    {error, invalid_arguments} = erlmcp_cli_registry:execute_command(<<"mcp.health">>, invalid).

test_safety_level_validation() ->
    %% Register unsafe command
    UnsafeCommand = #{
        name => <<"test.unsafe">>,
        module => erlmcp_cli_registry,
        function => test_function,
        arity => 1,
        description => <<"Unsafe command">>,
        category => <<"test">>,
        safety_level => unsafe
    },

    ok = erlmcp_cli_registry:register_command(UnsafeCommand),

    %% Verify safety level is stored
    {ok, Cmd} = erlmcp_cli_registry:lookup_command(<<"test.unsafe">>),
    ?assertEqual(unsafe, maps:get(safety_level, Cmd)).

test_concurrent_execution() ->
    %% Spawn multiple processes executing commands concurrently
    NumProcesses = 10,
    Pids = [spawn(fun() ->
        Result = erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
        self() ! {result, Result}
    end) || _ <- lists:seq(1, NumProcesses)],

    %% Wait for all executions
    Results = [receive
        {result, R} -> R
    after 5000 ->
        ct:fail("Concurrent execution timeout")
    end || _ <- Pids],

    %% Verify all executions succeeded
    ?assertEqual(NumProcesses, length(Results)),
    lists:foreach(fun(R) ->
        ?assert(is_map(R))
    end, Results).

%%%====================================================================
%%% Metrics Tests
%%%====================================================================

test_initial_metrics() ->
    %% Get initial metrics
    Metrics = erlmcp_cli_registry:get_metrics(),

    %% Verify initial state
    ?assert(is_map(Metrics)),
    ?assert(maps:get("lookups", Metrics, 0) >= 0),
    ?assert(maps:get("executions", Metrics, 0) >= 0),
    ?assert(maps:get("errors", Metrics, 0) >= 0).

test_metrics_increment() ->
    %% Reset metrics
    ok = erlmcp_cli_registry:reset_metrics(),

    %% Execute command to increment metrics
    erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),

    %% Verify metrics incremented
    Metrics = erlmcp_cli_registry:get_metrics(),
    ?assert(maps:get("executions", Metrics, 0) > 0).

test_get_metrics() ->
    %% Execute some commands
    erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
    erlmcp_cli_registry:lookup_command(<<"mcp.health">>),

    %% Get metrics
    Metrics = erlmcp_cli_registry:get_metrics(),

    %% Verify metrics structure
    ?assert(is_map(Metrics)),
    ?assert(is_integer(maps:get("lookups", Metrics, 0))),
    ?assert(is_integer(maps:get("executions", Metrics, 0))),
    ?assert(is_integer(maps:get("errors", Metrics, 0))).

test_reset_metrics() ->
    %% Execute commands to populate metrics
    erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
    erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),

    %% Reset metrics
    ok = erlmcp_cli_registry:reset_metrics(),

    %% Verify metrics reset
    Metrics = erlmcp_cli_registry:get_metrics(),
    ?assertEqual(0, maps:get("lookups", Metrics, 0)),
    ?assertEqual(0, maps:get("executions", Metrics, 0)),
    ?assertEqual(0, maps:get("errors", Metrics, 0)).

%%%====================================================================
%%% Category Tests
%%%====================================================================

test_list_categories() ->
    %% Register commands in different categories
    Commands = [
        #{name => <<"test.cat_list1">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Test">>, category => <<"cat1">>, safety_level => safe},
        #{name => <<"test.cat_list2">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Test">>, category => <<"cat2">>, safety_level => safe},
        #{name => <<"test.cat_list3">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Test">>, category => <<"cat3">>, safety_level => safe}
    ],

    lists:foreach(fun(Cmd) -> erlmcp_cli_registry:register_command(Cmd) end, Commands),

    %% List categories
    Categories = erlmcp_cli_registry:list_categories(),

    %% Verify categories
    ?assert(is_list(Categories)),
    ?assert(length(Categories) >= 3).

test_filter_by_safety_level() ->
    %% Register commands with different safety levels
    Commands = [
        #{name => <<"test.safe1">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Safe">>, category => <<"test">>, safety_level => safe},
        #{name => <<"test.unsafe1">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Unsafe">>, category => <<"test">>, safety_level => unsafe}
    ],

    lists:foreach(fun(Cmd) -> erlmcp_cli_registry:register_command(Cmd) end, Commands),

    %% Filter by safety level
    {ok, SafeCommands} = erlmcp_cli_registry:filter_by_safety_level(safe),
    ?assert(lists:any(fun(Cmd) -> maps:get(name, Cmd) =:= <<"test.safe1">> end, SafeCommands)).

%%%====================================================================
%%% Registry State Tests
%%%====================================================================

test_get_all_commands() ->
    %% Register multiple commands
    Commands = [
        #{name => <<"test.all1">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Test 1">>, category => <<"test">>, safety_level => safe},
        #{name => <<"test.all2">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Test 2">>, category => <<"test">>, safety_level => safe}
    ],

    lists:foreach(fun(Cmd) -> erlmcp_cli_registry:register_command(Cmd) end, Commands),

    %% Get all commands
    AllCommands = erlmcp_cli_registry:get_all_commands(),

    %% Verify all commands present
    ?assert(is_list(AllCommands)),
    ?assert(length(AllCommands) >= 2).

test_command_count() ->
    %% Register commands
    Commands = [
        #{name => <<"test.count1">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Test">>, category => <<"test">>, safety_level => safe},
        #{name => <<"test.count2">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Test">>, category => <<"test">>, safety_level => safe},
        #{name => <<"test.count3">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Test">>, category => <<"test">>, safety_level => safe}
    ],

    lists:foreach(fun(Cmd) -> erlmcp_cli_registry:register_command(Cmd) end, Commands),

    %% Get command count
    Count = erlmcp_cli_registry:command_count(),

    %% Verify count (at least our 3 commands + built-ins)
    ?assert(Count >= 3).

test_unregister_command() ->
    %% Register command
    Command = #{
        name => <<"test.unregister1">>,
        module => erlmcp_cli_registry,
        function => test_function,
        arity => 1,
        description => <<"Unregister test">>,
        category => <<"test">>,
        safety_level => safe
    },

    ok = erlmcp_cli_registry:register_command(Command),

    %% Verify registered
    {ok, _} = erlmcp_cli_registry:lookup_command(<<"test.unregister1">>),

    %% Unregister
    ok = erlmcp_cli_registry:unregister_command(<<"test.unregister1">>),

    %% Verify unregistered
    {error, command_not_found} = erlmcp_cli_registry:lookup_command(<<"test.unregister1">>).

test_clear_all_commands() ->
    %% Register test commands
    Commands = [
        #{name => <<"test.clear1">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Test">>, category => <<"test">>, safety_level => safe},
        #{name => <<"test.clear2">>, module => erlmcp_cli_registry, function => test_function, arity => 1, description => <<"Test">>, category => <<"test">>, safety_level => safe}
    ],

    lists:foreach(fun(Cmd) -> erlmcp_cli_registry:register_command(Cmd) end, Commands),

    %% Clear all commands
    ok = erlmcp_cli_registry:clear_all_commands(),

    %% Verify all cleared
    AllCommands = erlmcp_cli_registry:get_all_commands(),
    ?assertEqual(0, length([Cmd || Cmd <- AllCommands, maps:get(category, Cmd) =:= <<"test">>])).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% Test function stub (not actually called, just for registration)
test_function(_Args) ->
    #{<<"result">> => <<"ok">>}.
