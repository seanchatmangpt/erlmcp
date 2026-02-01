%%%-------------------------------------------------------------------
%%% @doc erlmcp_cli_completion_SUITE - Shell Completion Integration Tests
%%%
%%% Full shell completion workflow integration tests
%%%
%%% Chicago School TDD - Real completion engine, real file system
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_completion_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [bash_completion_test,
     zsh_completion_test,
     command_completion_workflow_test,
     subcommand_completion_workflow_test,
     flag_completion_workflow_test,
     file_path_completion_test,
     dynamic_completion_test,
     context_aware_completion_test].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

bash_completion_test(_Config) ->
    {ok, Completer} = erlmcp_cli_completer:start_link(),

    %% Generate bash completion script
    {ok, BashScript} = erlmcp_cli_completer:generate_bash_completion(Completer),
    true = is_binary(BashScript),
    true = binary:match(BashScript, <<"complete">>) =/= nomatch,

    ok = erlmcp_cli_completer:stop(Completer).

zsh_completion_test(_Config) ->
    {ok, Completer} = erlmcp_cli_completer:start_link(),

    %% Generate zsh completion script
    {ok, ZshScript} = erlmcp_cli_completer:generate_zsh_completion(Completer),
    true = is_binary(ZshScript),
    true = binary:match(ZshScript, <<"compdef">>) =/= nomatch,

    ok = erlmcp_cli_completer:stop(Completer).

command_completion_workflow_test(_Config) ->
    {ok, Completer} = erlmcp_cli_completer:start_link(),
    ok =
        erlmcp_cli_completer:add_entries(Completer,
                                         [{command, <<"help">>},
                                          {command, <<"version">>},
                                          {command, <<"validate">>}]),

    %% Complete "v"
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"v">>),
    true = lists:member(<<"validate">>, Matches),
    true = lists:member(<<"version">>, Matches),

    ok = erlmcp_cli_completer:stop(Completer).

subcommand_completion_workflow_test(_Config) ->
    {ok, Completer} = erlmcp_cli_completer:start_link(),
    ok =
        erlmcp_cli_completer:register_command(Completer,
                                              "validate",
                                              [{subcommand, <<"spec">>},
                                               {subcommand, <<"protocol">>}]),

    %% Complete subcommands
    {ok, Subs} =
        erlmcp_cli_completer:complete_with_context(Completer,
                                                   <<"s">>,
                                                   #{context => [<<"validate">>]}),
    true = lists:member(<<"spec">>, Subs),

    ok = erlmcp_cli_completer:stop(Completer).

flag_completion_workflow_test(_Config) ->
    {ok, Completer} = erlmcp_cli_completer:start_link(),
    ok =
        erlmcp_cli_completer:add_entries(Completer, [{flag, <<"--format">>}, {flag, <<"--file">>}]),

    {ok, Flags} = erlmcp_cli_completer:complete(Completer, <<"--f">>),
    2 = length(Flags),

    ok = erlmcp_cli_completer:stop(Completer).

file_path_completion_test(_Config) ->
    {ok, Completer} = erlmcp_cli_completer:start_link(),
    ok = erlmcp_cli_completer:enable_file_completion(Completer),

    %% Complete /tmp/
    {ok, Files} = erlmcp_cli_completer:complete_path(Completer, <<"/tmp/">>),
    true = is_list(Files),

    ok = erlmcp_cli_completer:stop(Completer).

dynamic_completion_test(_Config) ->
    {ok, Completer} = erlmcp_cli_completer:start_link(),

    %% Add dynamic provider
    Provider =
        fun(Prefix) ->
           case Prefix of
               <<"dynamic_">> ->
                   [<<"dynamic_cmd1">>, <<"dynamic_cmd2">>];
               _ ->
                   []
           end
        end,
    ok = erlmcp_cli_completer:add_provider(Completer, dynamic, Provider),

    {ok, Results} = erlmcp_cli_completer:complete(Completer, <<"dynamic_">>),
    2 = length(Results),

    ok = erlmcp_cli_completer:stop(Completer).

context_aware_completion_test(_Config) ->
    {ok, Completer} = erlmcp_cli_completer:start_link(),

    %% Register command with context-aware completion
    ok =
        erlmcp_cli_completer:register_command(Completer,
                                              "git",
                                              [{subcommand, <<"commit">>},
                                               {subcommand, <<"push">>}]),

    %% Complete in git context
    {ok, GitSubs} =
        erlmcp_cli_completer:complete_with_context(Completer, <<"c">>, #{context => [<<"git">>]}),
    true = lists:member(<<"commit">>, GitSubs),

    ok = erlmcp_cli_completer:stop(Completer).
