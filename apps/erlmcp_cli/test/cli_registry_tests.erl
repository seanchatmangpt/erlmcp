%%%-------------------------------------------------------------------
%%% @doc EUnit tests for erlmcp_cli_registry (real exported API).
%%%
%%% Rewritten from non-Erlang pseudo-code (python comment syntax, undefined
%%% #command_info record, nonexistent functions is_valid_command/1,
%%% list_commands_by_category/1, reload/0, get_metrics/0, stop/0).
%%%
%%% Real API under test (apps/erlmcp_cli/src/erlmcp_cli_registry.erl):
%%%   start_link/0, register_command/2, unregister_command/1,
%%%   lookup_command/1, list_commands/0, execute_command/2,
%%%   get_command_schema/1, reload_commands/0
%%%
%%% Chicago school: the test module itself is the real command module
%%% (exports execute/1); no mocks.
%%% @end
%%%-------------------------------------------------------------------
-module(cli_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%% The registry executes commands via Module:execute(Args); this test
%% module is a real command module.
-export([execute/1]).

execute(_Args) ->
    {ok, #{<<"status">> => <<"ok">>}}.

%%====================================================================
%% Tests
%%====================================================================

lookup_unknown_command_test() ->
    {ok, _} = erlmcp_cli_registry:start_link(),
    ?assertMatch({error, command_not_found},
                 erlmcp_cli_registry:lookup_command(<<"test_nonexistent">>)),
    gen_server:stop(erlmcp_cli_registry).

register_lookup_execute_roundtrip_test() ->
    {ok, _} = erlmcp_cli_registry:start_link(),
    ?assertEqual(ok, erlmcp_cli_registry:register_command(<<"test.command">>,
                                                          cli_registry_tests)),
    {ok, Info} = erlmcp_cli_registry:lookup_command(<<"test.command">>),
    ?assertMatch({ok, #{<<"status">> := <<"ok">>}},
                 erlmcp_cli_registry:execute_command(<<"test.command">>, [<<"a">>])),
    %% The returned #command_info record carries the registered module
    %% (field 2 of #command_info is 'module').
    ?assertEqual(cli_registry_tests, element(2, Info)),
    gen_server:stop(erlmcp_cli_registry).

unregister_registered_test() ->
    {ok, _} = erlmcp_cli_registry:start_link(),
    ok = erlmcp_cli_registry:register_command(<<"test.command">>, cli_registry_tests),
    ?assertEqual(ok, erlmcp_cli_registry:unregister_command(<<"test.command">>)),
    ?assertMatch({error, command_not_found},
                 erlmcp_cli_registry:lookup_command(<<"test.command">>)),
    gen_server:stop(erlmcp_cli_registry).

unregister_missing_test() ->
    {ok, _} = erlmcp_cli_registry:start_link(),
    ?assertMatch({error, command_not_found},
                 erlmcp_cli_registry:unregister_command(<<"never/registered">>)),
    gen_server:stop(erlmcp_cli_registry).

list_commands_test() ->
    {ok, _} = erlmcp_cli_registry:start_link(),
    ok = erlmcp_cli_registry:register_command(<<"test.command">>, cli_registry_tests),
    ?assertMatch({ok, [_ | _]}, erlmcp_cli_registry:list_commands()),
    gen_server:stop(erlmcp_cli_registry).

get_command_schema_test() ->
    {ok, _} = erlmcp_cli_registry:start_link(),
    ok = erlmcp_cli_registry:register_command(<<"test.command">>, cli_registry_tests),
    ?assertMatch({ok, _}, erlmcp_cli_registry:get_command_schema(<<"test.command">>)),
    ?assertMatch({error, command_not_found},
                 erlmcp_cli_registry:get_command_schema(<<"no.such.command">>)),
    gen_server:stop(erlmcp_cli_registry).

reload_commands_test() ->
    {ok, _} = erlmcp_cli_registry:start_link(),
    ?assertEqual(ok, erlmcp_cli_registry:reload_commands()),
    gen_server:stop(erlmcp_cli_registry).

execute_unknown_command_test() ->
    {ok, _} = erlmcp_cli_registry:start_link(),
    ?assertMatch({error, command_not_found},
                 erlmcp_cli_registry:execute_command(<<"no.such.command">>, [])),
    gen_server:stop(erlmcp_cli_registry).
