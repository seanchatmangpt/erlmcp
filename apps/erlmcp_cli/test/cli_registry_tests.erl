%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for erlmcp_cli_registry module
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test basic command registration and lookup
basic_registry_test() ->
    %% Create a test registry process
    Pid = start_test_registry(),

    %% Test command lookup for non-existent command
    {error, not_found} = erlmcp_cli_registry:lookup_command(<<"test_nonexistent">>),

    %% Test successful command lookup
    {ok, CommandInfo} = erlmcp_cli_registry:lookup_command(<<"mcp.health">>),
    ?assert(is_record(CommandInfo, command_info)),
    ?assertEqual(<<"mcp.health">>, CommandInfo#command_info.name),

    %% Cleanup
    stop_test_registry(Pid).

%% @doc Test command registration
command_registration_test() ->
    Pid = start_test_registry(),

    %% Register a new command
    Command = #command_info{
        name = <<"test.command">>,
        module = test_module,
        function = test_function,
        arity = 2,
        description = <<"Test command">>,
        category = <<"test">>,
        safety_level = safe
    },

    ok = erlmcp_cli_registry:register_command(Command),

    %% Verify registration
    {ok, RegisteredCommand} = erlmcp_cli_registry:lookup_command(<<"test.command">>),
    ?assertEqual(Command, RegisteredCommand),

    %% Cleanup
    stop_test_registry(Pid).

%% @doc Test command execution
command_execution_test() ->
    Pid = start_test_registry(),

    %% Execute a command with valid arguments
    Args = [<<"arg1">>, <<"arg2">>],
    Result = erlmcp_cli_registry:execute_command(<<"mcp.health">>, Args),
    ?assert(is_map(Result)),

    %% Execute command with invalid number of arguments
    {error, invalid_arguments} = erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
    {error, invalid_arguments} = erlmcp_cli_registry:execute_command(<<"mcp.health">>, [<<"extra">>]),

    %% Cleanup
    stop_test_registry(Pid).

%% @doc Test command validation
command_validation_test() ->
    Pid = start_test_registry(),

    %% Test valid command
    ?assertEqual(true, erlmcp_cli_registry:is_valid_command(<<"mcp.health">>)),

    %% Test invalid command (non-existent)
    ?assertEqual(false, erlmcp_cli_registry:is_valid_command(<<"invalid.command">>)),

    %% Test invalid command name format
    ?assertEqual(false, erlmcp_cli_registry:is_valid_command(123)),
    ?assertEqual(false, erlmcp_cli_registry:is_valid_command(<<>>)),

    %% Cleanup
    stop_test_registry(Pid).

%% @doc Test command categories
command_categories_test() ->
    Pid = start_test_registry(),

    %% List commands by category
    Commands = erlmcp_cli_registry:list_commands_by_category(<<"mcp">>),
    ?assert(is_list(Commands)),
    ?assert(length(Commands) > 0),

    %% Test non-existent category
    [] = erlmcp_cli_registry:list_commands_by_category(<<"nonexistent">>),

    %% Cleanup
    stop_test_registry(Pid).

%% @doc Test hot-reload functionality
hot_reload_test() ->
    Pid = start_test_registry(),

    %% Get initial command count
    InitialCommands = erlmcp_cli_registry:list_commands(),
    InitialCount = length(InitialCommands),

    %% Reload registry (this should reload from persistent storage)
    ok = erlmcp_cli_registry:reload(),

    %% Verify command count remains the same
    ReloadedCommands = erlmcp_cli_registry:list_commands(),
    ?assertEqual(InitialCount, length(ReloadedCommands)),

    %% Cleanup
    stop_test_registry(Pid).

%% @doc Test metrics collection
registry_metrics_test() ->
    Pid = start_test_registry(),

    %% Execute some commands to generate metrics
    ok = erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
    ok = erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
    {error, invalid_arguments} = erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),

    %% Get metrics
    Metrics = erlmcp_cli_registry:get_metrics(),
    ?assert(is_map(Metrics)),
    ?assert(is_integer(maps:get("lookups", Metrics, 0))),
    ?assert(is_integer(maps:get("executions", Metrics, 0))),
    ?assert(is_integer(maps:get("errors", Metrics, 0))),

    %% Cleanup
    stop_test_registry(Pid).

%% @doc Test error handling for invalid operations
error_handling_test() ->
    Pid = start_test_registry(),

    %% Test lookup with invalid command name
    {error, not_found} = erlmcp_cli_registry:lookup_command(<<>>),
    {error, not_found} = erlmcp_cli_registry:lookup_command(<<42>>),

    %% Test execute with invalid command name
    {error, not_found} = erlmcp_cli_registry:execute_command(<<>>, []),

    %% Test execute with non-list arguments
    {error, invalid_arguments} = erlmcp_cli_registry:execute_command(<<"mcp.health">>, <<"not_a_list">>),

    %% Cleanup
    stop_test_registry(Pid).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Start a test registry process
start_test_registry() ->
    %% Start the registry process
    Pid = erlmcp_cli_registry:start_link(),

    %% Wait for process to initialize
    timer:sleep(100),

    %% Verify process is alive
    ?assert(is_process_alive(Pid)),

    Pid.

%% @doc Stop a test registry process
stop_test_registry(Pid) ->
    erlmcp_cli_registry:stop(),
    timer:sleep(50),  # Allow cleanup
    ok.

%% @doc Test setup - register test commands
setup_test_commands() ->
    %% Register test commands for testing
    TestCommand1 = #command_info{
        name = <<"test.command1">>,
        module = test_module1,
        function = test_function1,
        arity = 1,
        description = <<"Test command 1">>,
        category = <<"test">>,
        safety_level = safe
    },

    TestCommand2 = #command_info{
        name = <<"test.command2">>,
        module = test_module2,
        function = test_function2,
        arity = 2,
        description = <<"Test command 2">>,
        category = <<"test">>,
        safety_level = unsafe
    },

    erlmcp_cli_registry:register_command(TestCommand1),
    erlmcp_cli_registry:register_command(TestCommand2),
    ok.