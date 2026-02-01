%%%-------------------------------------------------------------------
%%% @doc erlmcp_cli_interactive_SUITE - Interactive CLI Integration Tests
%%%
%%% Full REPL workflow integration tests for interactive CLI
%%%
%%% Chicago School TDD:
%%% - Real gen_server processes
%%% - Real command execution
%%% - Full session lifecycle
%%% - NO mocks
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_interactive_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%%====================================================================
%%% CT Callbacks
%%%====================================================================

all() ->
    [
     full_session_workflow_test,
     multi_command_execution_test,
     history_persistence_test,
     tab_completion_integration_test,
     multi_line_command_test,
     concurrent_sessions_test,
     session_timeout_test,
     command_chaining_test,
     error_recovery_workflow_test,
     plugin_integration_test
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%====================================================================
%%% Test Cases
%%%====================================================================

full_session_workflow_test(_Config) ->
    %% Start interactive session
    {ok, Session} = erlmcp_cli_interactive:start_link(),

    %% Execute help
    {ok, HelpOutput} = erlmcp_cli_interactive:execute(Session, "help"),
    true = is_binary(HelpOutput),

    %% Execute version
    {ok, VersionOutput} = erlmcp_cli_interactive:execute(Session, "version"),
    true = is_binary(VersionOutput),

    %% Check history
    {ok, History} = erlmcp_cli_interactive:get_history(Session),
    2 = length(History),

    %% Exit session
    {exit, normal} = erlmcp_cli_interactive:execute(Session, "exit"),

    ok.

multi_command_execution_test(_Config) ->
    {ok, Session} = erlmcp_cli_interactive:start_link(),

    %% Execute 100 commands
    Commands = [io_lib:format("echo command_~p", [N]) || N <- lists:seq(1, 100)],
    Results = [erlmcp_cli_interactive:execute(Session, Cmd) || Cmd <- Commands],

    %% All should succeed
    100 = length([ok || {ok, _} <- Results]),

    %% Verify history
    {ok, History} = erlmcp_cli_interactive:get_history(Session),
    true = length(History) >= 100,

    ok = erlmcp_cli_interactive:stop(Session).

history_persistence_test(_Config) ->
    %% Create session with persistent history
    {ok, Session} = erlmcp_cli_interactive:start_link(#{
        history_file => "/tmp/cli_history_test.txt"
    }),

    %% Execute commands
    {ok, _} = erlmcp_cli_interactive:execute(Session, "help"),
    {ok, _} = erlmcp_cli_interactive:execute(Session, "version"),

    %% Stop session
    ok = erlmcp_cli_interactive:stop(Session),

    %% Start new session with same history file
    {ok, NewSession} = erlmcp_cli_interactive:start_link(#{
        history_file => "/tmp/cli_history_test.txt"
    }),

    %% Verify history loaded
    {ok, History} = erlmcp_cli_interactive:get_history(NewSession),
    true = lists:member(<<"help">>, History),
    true = lists:member(<<"version">>, History),

    ok = erlmcp_cli_interactive:stop(NewSession),
    file:delete("/tmp/cli_history_test.txt").

tab_completion_integration_test(_Config) ->
    {ok, Session} = erlmcp_cli_interactive:start_link(),

    %% Trigger tab completion for "val"
    {ok, Completions} = erlmcp_cli_interactive:complete(Session, "val"),

    %% Should suggest "validate"
    true = lists:member(<<"validate">>, Completions),

    %% Complete "validate s"
    {ok, SubCompletions} = erlmcp_cli_interactive:complete(Session, "validate s"),

    %% Should suggest "spec"
    true = lists:member(<<"spec">>, SubCompletions),

    ok = erlmcp_cli_interactive:stop(Session).

multi_line_command_test(_Config) ->
    {ok, Session} = erlmcp_cli_interactive:start_link(),

    %% Start multi-line command
    ok = erlmcp_cli_interactive:add_line(Session, "validate spec \\"),
    ok = erlmcp_cli_interactive:add_line(Session, "--format json \\"),
    ok = erlmcp_cli_interactive:add_line(Session, "--verbose"),

    %% Get accumulated command
    {ok, FullCmd} = erlmcp_cli_interactive:get_accumulated_command(Session),
    <<"validate spec --format json --verbose">> = FullCmd,

    %% Execute accumulated command
    {ok, _} = erlmcp_cli_interactive:execute_accumulated(Session),

    ok = erlmcp_cli_interactive:stop(Session).

concurrent_sessions_test(_Config) ->
    %% Start 10 concurrent sessions
    Sessions = [begin
        {ok, S} = erlmcp_cli_interactive:start_link(#{session_id => N}),
        S
    end || N <- lists:seq(1, 10)],

    %% Execute commands concurrently
    [spawn(fun() ->
        {ok, _} = erlmcp_cli_interactive:execute(S, "help")
    end) || S <- Sessions],

    timer:sleep(1000),

    %% All sessions should still be alive
    [true = is_process_alive(S) || S <- Sessions],

    %% Stop all sessions
    [ok = erlmcp_cli_interactive:stop(S) || S <- Sessions],

    ok.

session_timeout_test(_Config) ->
    %% Start session with 1 second idle timeout
    {ok, Session} = erlmcp_cli_interactive:start_link(#{
        idle_timeout => 1000
    }),

    %% Wait for timeout
    timer:sleep(1500),

    %% Session should have stopped
    false = is_process_alive(Session),

    ok.

command_chaining_test(_Config) ->
    {ok, Session} = erlmcp_cli_interactive:start_link(),

    %% Execute chained commands
    {ok, Results} = erlmcp_cli_interactive:execute(Session, "help && version"),

    %% Should return list of results
    true = is_list(Results),
    2 = length(Results),

    ok = erlmcp_cli_interactive:stop(Session).

error_recovery_workflow_test(_Config) ->
    {ok, Session} = erlmcp_cli_interactive:start_link(),

    %% Execute invalid command
    {error, _} = erlmcp_cli_interactive:execute(Session, "invalid_command"),

    %% Session should still be functional
    true = is_process_alive(Session),

    %% Execute valid command
    {ok, _} = erlmcp_cli_interactive:execute(Session, "help"),

    ok = erlmcp_cli_interactive:stop(Session).

plugin_integration_test(_Config) ->
    {ok, Session} = erlmcp_cli_interactive:start_link(),

    %% Load plugin
    PluginPath = "/tmp/test_cli_plugin.erl",
    ok = create_test_plugin(PluginPath),

    {ok, _} = erlmcp_cli_interactive:execute(Session, "plugin load " ++ PluginPath),

    %% Execute plugin command
    {ok, _} = erlmcp_cli_interactive:execute(Session, "test_plugin hello"),

    ok = erlmcp_cli_interactive:stop(Session),
    file:delete(PluginPath).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

create_test_plugin(Path) ->
    Code = <<"-module(test_plugin).
-export([init/1, execute/2]).

init(_Config) -> {ok, #{}}.
execute(hello, _Args) -> {ok, <<"Hello from plugin">>}.
">>,
    file:write_file(Path, Code).
