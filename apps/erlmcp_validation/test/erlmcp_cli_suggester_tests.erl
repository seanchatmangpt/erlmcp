%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Suggester Test Suite (EUnit)
%%%
%%% Tests for erlmcp_cli_suggester module - Intelligent command suggestions
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real suggestion engine with ML/heuristics
%%% - NO mocks, real command history analysis
%%% - State-based verification (suggestion quality and ranking)
%%%
%%% Coverage Target: ≥85%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_suggester_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

suggester_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Initialize suggester", fun test_init_suggester/0},
      {"Suggest based on history", fun test_suggest_from_history/0},
      {"Suggest based on context", fun test_suggest_from_context/0},
      {"Suggest based on frequency", fun test_suggest_by_frequency/0},
      {"Suggest based on recency", fun test_suggest_by_recency/0},
      {"Suggest corrections for typos", fun test_suggest_corrections/0},
      {"Suggest flags for command", fun test_suggest_flags/0},
      {"Learn from user behavior", fun test_learn_behavior/0},
      {"Rank suggestions", fun test_rank_suggestions/0},
      {"Filter irrelevant suggestions", fun test_filter_suggestions/0}]}.

setup() ->
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_Args) ->
    ok.

%%%====================================================================
%%% Basic Tests
%%%====================================================================

test_init_suggester() ->
    {ok, Suggester} = erlmcp_cli_suggester:start_link(),
    ?assert(is_pid(Suggester)),
    ok = erlmcp_cli_suggester:stop(Suggester).

test_suggest_from_history() ->
    {ok, Suggester} = erlmcp_cli_suggester:start_link(),
    ok = erlmcp_cli_suggester:record_command(Suggester, "validate spec"),
    {ok, Suggestions} = erlmcp_cli_suggester:suggest(Suggester, "val"),
    ?assert(lists:member(<<"validate spec">>, Suggestions)),
    ok = erlmcp_cli_suggester:stop(Suggester).

test_suggest_from_context() ->
    {ok, Suggester} = erlmcp_cli_suggester:start_link(),
    ok = erlmcp_cli_suggester:set_context(Suggester, #{working_directory => <<"/tmp">>}),
    {ok, Suggestions} = erlmcp_cli_suggester:suggest(Suggester, ""),
    ?assert(is_list(Suggestions)),
    ok = erlmcp_cli_suggester:stop(Suggester).

test_suggest_by_frequency() ->
    {ok, Suggester} = erlmcp_cli_suggester:start_link(),
    [ok = erlmcp_cli_suggester:record_command(Suggester, "help") || _ <- lists:seq(1, 10)],
    {ok, Suggestions} = erlmcp_cli_suggester:suggest_by_frequency(Suggester),
    [First | _] = Suggestions,
    ?assertEqual(<<"help">>, First),
    ok = erlmcp_cli_suggester:stop(Suggester).

test_suggest_by_recency() ->
    {ok, Suggester} = erlmcp_cli_suggester:start_link(),
    ok = erlmcp_cli_suggester:record_command(Suggester, "help"),
    timer:sleep(10),
    ok = erlmcp_cli_suggester:record_command(Suggester, "version"),
    {ok, Suggestions} = erlmcp_cli_suggester:suggest_by_recency(Suggester),
    [First | _] = Suggestions,
    ?assertEqual(<<"version">>, First),
    ok = erlmcp_cli_suggester:stop(Suggester).

test_suggest_corrections() ->
    {ok, Suggester} = erlmcp_cli_suggester:start_link([{command, <<"validate">>}]),
    {ok, Corrections} = erlmcp_cli_suggester:suggest_corrections(Suggester, "valdate"),
    ?assert(lists:member(<<"validate">>, Corrections)),
    ok = erlmcp_cli_suggester:stop(Suggester).

test_suggest_flags() ->
    {ok, Suggester} = erlmcp_cli_suggester:start_link(),
    ok =
        erlmcp_cli_suggester:register_command(Suggester,
                                              "validate",
                                              [{flag, <<"--format">>, [<<"json">>]}]),
    {ok, Flags} = erlmcp_cli_suggester:suggest_flags(Suggester, "validate"),
    ?assert(lists:member(<<"--format">>, Flags)),
    ok = erlmcp_cli_suggester:stop(Suggester).

test_learn_behavior() ->
    {ok, Suggester} = erlmcp_cli_suggester:start_link(),
    ok = erlmcp_cli_suggester:record_command(Suggester, "validate spec"),
    ok = erlmcp_cli_suggester:record_command(Suggester, "validate spec"),
    {ok, Patterns} = erlmcp_cli_suggester:get_learned_patterns(Suggester),
    ?assertMatch(#{<<"validate spec">> := _}, Patterns),
    ok = erlmcp_cli_suggester:stop(Suggester).

test_rank_suggestions() ->
    {ok, Suggester} = erlmcp_cli_suggester:start_link(),
    [ok = erlmcp_cli_suggester:record_command(Suggester, "help") || _ <- lists:seq(1, 10)],
    [ok = erlmcp_cli_suggester:record_command(Suggester, "version") || _ <- lists:seq(1, 5)],
    {ok, Ranked} = erlmcp_cli_suggester:get_ranked_suggestions(Suggester),
    [First, Second | _] = Ranked,
    ?assertEqual(<<"help">>, First),
    ?assertEqual(<<"version">>, Second),
    ok = erlmcp_cli_suggester:stop(Suggester).

test_filter_suggestions() ->
    {ok, Suggester} = erlmcp_cli_suggester:start_link(),
    ok = erlmcp_cli_suggester:record_command(Suggester, "validate spec"),
    ok = erlmcp_cli_suggester:record_command(Suggester, "help"),
    {ok, Filtered} =
        erlmcp_cli_suggester:suggest_filtered(Suggester,
                                              fun(Cmd) ->
                                                 binary:match(Cmd, <<"validate">>) =/= nomatch
                                              end),
    ?assertEqual([<<"validate spec">>], Filtered),
    ok = erlmcp_cli_suggester:stop(Suggester).
