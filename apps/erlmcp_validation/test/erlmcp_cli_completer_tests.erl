%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Completer Test Suite (EUnit)
%%%
%%% Tests for erlmcp_cli_completer module - TAB completion engine
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real completion engine
%%% - NO mocks, real trie/tree data structures
%%% - State-based verification (completion results, match quality)
%%%
%%% Coverage Target: ≥85%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_completer_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

completer_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Initialize completer", fun test_init_completer/0},
      {"Add completion entry", fun test_add_entry/0},
      {"Add multiple entries", fun test_add_multiple/0},
      {"Complete simple prefix", fun test_complete_simple/0},
      {"Complete with multiple matches", fun test_complete_multiple_matches/0},
      {"Complete with no matches", fun test_complete_no_matches/0},
      {"Complete exact match", fun test_complete_exact_match/0},
      {"Complete partial word", fun test_complete_partial/0},
      {"Complete with description", fun test_complete_with_description/0},
      {"Complete with type hints", fun test_complete_with_types/0},
      {"Complete commands", fun test_complete_commands/0},
      {"Complete subcommands", fun test_complete_subcommands/0},
      {"Complete flags", fun test_complete_flags/0},
      {"Complete file paths", fun test_complete_file_paths/0},
      {"Complete environment variables", fun test_complete_env_vars/0},
      {"Rank completions by frequency", fun test_rank_by_frequency/0},
      {"Rank completions by recency", fun test_rank_by_recency/0},
      {"Fuzzy matching", fun test_fuzzy_matching/0},
      {"Case-insensitive completion", fun test_case_insensitive/0},
      {"Complete with context", fun test_complete_with_context/0},
      {"Complete nested paths", fun test_complete_nested_paths/0},
      {"Complete with wildcards", fun test_complete_wildcards/0},
      {"Cache completion results", fun test_cache_completions/0},
      {"Clear cache", fun test_clear_cache/0},
      {"Custom completion provider", fun test_custom_provider/0},
      {"Async completion", fun test_async_completion/0},
      {"Completion timeout", fun test_completion_timeout/0}]}.

setup() ->
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_Args) ->
    ok.

%%%====================================================================
%%% Initialization Tests
%%%====================================================================

test_init_completer() ->
    %% Initialize completer with default commands
    {ok, Completer} =
        erlmcp_cli_completer:init([{command, <<"help">>},
                                   {command, <<"version">>},
                                   {command, <<"validate">>}]),

    %% Verify completer state
    ?assert(is_pid(Completer)),

    %% Get registered commands
    {ok, Commands} = erlmcp_cli_completer:get_commands(Completer),
    ?assertEqual(3, length(Commands)),

    ok = erlmcp_cli_completer:stop(Completer).

test_add_entry() ->
    {ok, Completer} = erlmcp_cli_completer:init([]),

    %% Add single entry
    ok = erlmcp_cli_completer:add_entry(Completer, {command, <<"test">>}),

    %% Verify entry added
    {ok, Entries} = erlmcp_cli_completer:get_all_entries(Completer),
    ?assert(lists:member({command, <<"test">>}, Entries)),

    ok = erlmcp_cli_completer:stop(Completer).

test_add_multiple() ->
    {ok, Completer} = erlmcp_cli_completer:init([]),

    %% Add multiple entries
    Entries = [{command, <<"validate">>}, {command, <<"verify">>}, {command, <<"version">>}],
    ok = erlmcp_cli_completer:add_entries(Completer, Entries),

    %% Verify all added
    {ok, AllEntries} = erlmcp_cli_completer:get_all_entries(Completer),
    ?assertEqual(3, length(AllEntries)),

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% Basic Completion Tests
%%%====================================================================

test_complete_simple() ->
    {ok, Completer} = erlmcp_cli_completer:init([{command, <<"help">>}, {command, <<"version">>}]),

    %% Complete "h" → should match "help"
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"h">>),
    ?assertEqual([<<"help">>], Matches),

    ok = erlmcp_cli_completer:stop(Completer).

test_complete_multiple_matches() ->
    {ok, Completer} =
        erlmcp_cli_completer:init([{command, <<"validate">>},
                                   {command, <<"verify">>},
                                   {command, <<"version">>}]),

    %% Complete "v" → should match all three
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"v">>),
    ?assertEqual(3, length(Matches)),
    ?assert(lists:member(<<"validate">>, Matches)),
    ?assert(lists:member(<<"verify">>, Matches)),
    ?assert(lists:member(<<"version">>, Matches)),

    ok = erlmcp_cli_completer:stop(Completer).

test_complete_no_matches() ->
    {ok, Completer} = erlmcp_cli_completer:init([{command, <<"help">>}]),

    %% Complete "x" → no matches
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"x">>),
    ?assertEqual([], Matches),

    ok = erlmcp_cli_completer:stop(Completer).

test_complete_exact_match() ->
    {ok, Completer} = erlmcp_cli_completer:init([{command, <<"help">>}]),

    %% Complete "help" → exact match
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"help">>),
    ?assertEqual([<<"help">>], Matches),

    ok = erlmcp_cli_completer:stop(Completer).

test_complete_partial() ->
    {ok, Completer} = erlmcp_cli_completer:init([{command, <<"validate">>}]),

    %% Complete "val" → should match "validate"
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"val">>),
    ?assertEqual([<<"validate">>], Matches),

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% Enhanced Completion Tests
%%%====================================================================

test_complete_with_description() ->
    {ok, Completer} =
        erlmcp_cli_completer:init([{command, <<"help">>, <<"Show help information">>},
                                   {command, <<"version">>, <<"Show version">>}]),

    %% Complete with descriptions
    {ok, Results} = erlmcp_cli_completer:complete_with_info(Completer, <<"h">>),

    %% Should return [{<<"help">>, <<"Show help information">>}]
    ?assertMatch([{<<"help">>, <<"Show help information">>}], Results),

    ok = erlmcp_cli_completer:stop(Completer).

test_complete_with_types() ->
    {ok, Completer} =
        erlmcp_cli_completer:init([{command, <<"help">>, command},
                                   {flag, <<"--format">>, flag},
                                   {file, <<"config.json">>, file}]),

    %% Complete and get types
    {ok, Results} = erlmcp_cli_completer:complete_with_type(Completer, <<"h">>),

    %% Should include type info
    ?assertMatch([{<<"help">>, command}], Results),

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% Command Completion Tests
%%%====================================================================

test_complete_commands() ->
    {ok, Completer} =
        erlmcp_cli_completer:init_with_commands([<<"help">>,
                                                 <<"version">>,
                                                 <<"validate">>,
                                                 <<"verify">>]),

    %% Complete "ve" → ["verify", "version"]
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"ve">>),
    ?assertEqual(2, length(Matches)),
    ?assert(lists:member(<<"verify">>, Matches)),
    ?assert(lists:member(<<"version">>, Matches)),

    ok = erlmcp_cli_completer:stop(Completer).

test_complete_subcommands() ->
    {ok, Completer} =
        erlmcp_cli_completer:init([{command,
                                    <<"validate">>,
                                    [{subcommand, <<"spec">>},
                                     {subcommand, <<"protocol">>},
                                     {subcommand, <<"transport">>}]}]),

    %% Complete "validate s" → ["spec"]
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"validate s">>),
    ?assertEqual([<<"spec">>], Matches),

    ok = erlmcp_cli_completer:stop(Completer).

test_complete_flags() ->
    {ok, Completer} =
        erlmcp_cli_completer:init([{flag, <<"--format">>},
                                   {flag, <<"--file">>},
                                   {flag, <<"--force">>}]),

    %% Complete "--f" → ["--file", "--force", "--format"]
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"--f">>),
    ?assertEqual(3, length(Matches)),
    ?assert(lists:member(<<"--format">>, Matches)),

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% File Path Completion Tests
%%%====================================================================

test_complete_file_paths() ->
    {ok, Completer} = erlmcp_cli_completer:init([]),

    %% Enable file path completion
    ok = erlmcp_cli_completer:enable_file_completion(Completer),

    %% Complete file path (should list files in current directory)
    {ok, Matches} = erlmcp_cli_completer:complete_path(Completer, <<"/tmp/">>),

    %% Should return list of files
    ?assert(is_list(Matches)),

    ok = erlmcp_cli_completer:stop(Completer).

test_complete_nested_paths() ->
    {ok, Completer} = erlmcp_cli_completer:init([]),
    ok = erlmcp_cli_completer:enable_file_completion(Completer),

    %% Complete nested path
    {ok, Matches} = erlmcp_cli_completer:complete_path(Completer, <<"/home/">>),

    %% Should return directories under /home
    ?assert(is_list(Matches)),

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% Environment Variable Completion Tests
%%%====================================================================

test_complete_env_vars() ->
    {ok, Completer} = erlmcp_cli_completer:init([]),

    %% Add environment variables
    ok = erlmcp_cli_completer:add_env_var(Completer, <<"PATH">>),
    ok = erlmcp_cli_completer:add_env_var(Completer, <<"PORT">>),

    %% Complete "$P" → ["PATH", "PORT"]
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"$P">>),
    ?assertEqual(2, length(Matches)),

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% Ranking Tests
%%%====================================================================

test_rank_by_frequency() ->
    {ok, Completer} = erlmcp_cli_completer:init([{command, <<"help">>}, {command, <<"version">>}]),

    %% Use "help" multiple times
    ok = erlmcp_cli_completer:record_usage(Completer, <<"help">>),
    ok = erlmcp_cli_completer:record_usage(Completer, <<"help">>),
    ok = erlmcp_cli_completer:record_usage(Completer, <<"version">>),

    %% Complete "h" or "v" - "help" should rank higher due to frequency
    {ok, Ranked} = erlmcp_cli_completer:complete_ranked(Completer, <<"h">>),
    ?assertEqual([<<"help">>], Ranked),

    ok = erlmcp_cli_completer:stop(Completer).

test_rank_by_recency() ->
    {ok, Completer} = erlmcp_cli_completer:init([{command, <<"help">>}, {command, <<"version">>}]),

    %% Use "version" most recently
    ok = erlmcp_cli_completer:record_usage(Completer, <<"help">>),
    timer:sleep(10),
    ok = erlmcp_cli_completer:record_usage(Completer, <<"version">>),

    %% When completing "h" or "v", recency should influence ranking
    {ok, Ranked} = erlmcp_cli_completer:complete_ranked_by_recency(Completer, <<"v">>),
    ?assertEqual([<<"version">>], Ranked),

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% Fuzzy Matching Tests
%%%====================================================================

test_fuzzy_matching() ->
    {ok, Completer} = erlmcp_cli_completer:init([{command, <<"validate">>}]),

    %% Enable fuzzy matching
    ok = erlmcp_cli_completer:enable_fuzzy(Completer),

    %% Complete "vldt" → should fuzzy match "validate"
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"vldt">>),
    ?assert(lists:member(<<"validate">>, Matches)),

    ok = erlmcp_cli_completer:stop(Completer).

test_case_insensitive() ->
    {ok, Completer} = erlmcp_cli_completer:init([{command, <<"Validate">>}]),

    %% Enable case-insensitive matching
    ok = erlmcp_cli_completer:set_case_sensitive(Completer, false),

    %% Complete "val" → should match "Validate"
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"val">>),
    ?assertEqual([<<"Validate">>], Matches),

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% Context-aware Completion Tests
%%%====================================================================

test_complete_with_context() ->
    {ok, Completer} =
        erlmcp_cli_completer:init([{command,
                                    <<"validate">>,
                                    [{subcommand, <<"spec">>}, {subcommand, <<"protocol">>}]}]),

    %% Complete with context: after "validate", suggest subcommands
    {ok, Matches} =
        erlmcp_cli_completer:complete_with_context(Completer,
                                                   <<"s">>,
                                                   #{context => [<<"validate">>]}),

    %% Should suggest "spec"
    ?assertEqual([<<"spec">>], Matches),

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% Wildcard Tests
%%%====================================================================

test_complete_wildcards() ->
    {ok, Completer} =
        erlmcp_cli_completer:init([{command, <<"test1">>},
                                   {command, <<"test2">>},
                                   {command, <<"test3">>}]),

    %% Complete "test*" → all test commands
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"test*">>),
    ?assertEqual(3, length(Matches)),

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% Caching Tests
%%%====================================================================

test_cache_completions() ->
    {ok, Completer} = erlmcp_cli_completer:init([{command, <<"help">>}]),

    %% Enable caching
    ok = erlmcp_cli_completer:enable_cache(Completer),

    %% First completion (cache miss)
    {ok, Matches1} = erlmcp_cli_completer:complete(Completer, <<"h">>),
    ?assertEqual([<<"help">>], Matches1),

    %% Second completion (cache hit)
    {ok, Matches2} = erlmcp_cli_completer:complete(Completer, <<"h">>),
    ?assertEqual([<<"help">>], Matches2),

    %% Verify cache was used
    {ok, Stats} = erlmcp_cli_completer:get_cache_stats(Completer),
    ?assertMatch(#{hits := 1, misses := 1}, Stats),

    ok = erlmcp_cli_completer:stop(Completer).

test_clear_cache() ->
    {ok, Completer} = erlmcp_cli_completer:init([]),
    ok = erlmcp_cli_completer:enable_cache(Completer),

    %% Populate cache
    {ok, _} = erlmcp_cli_completer:complete(Completer, <<"h">>),

    %% Clear cache
    ok = erlmcp_cli_completer:clear_cache(Completer),

    %% Verify cache cleared
    {ok, Stats} = erlmcp_cli_completer:get_cache_stats(Completer),
    ?assertMatch(#{hits := 0, misses := 0}, Stats),

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% Custom Provider Tests
%%%====================================================================

test_custom_provider() ->
    %% Custom completion provider function
    CustomProvider =
        fun(Prefix) ->
           case Prefix of
               <<"custom_">> ->
                   [<<"custom_cmd1">>, <<"custom_cmd2">>];
               _ ->
                   []
           end
        end,

    {ok, Completer} = erlmcp_cli_completer:init([]),

    %% Register custom provider
    ok = erlmcp_cli_completer:add_provider(Completer, custom, CustomProvider),

    %% Complete using custom provider
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"custom_">>),
    ?assertEqual(2, length(Matches)),

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% Async Completion Tests
%%%====================================================================

test_async_completion() ->
    {ok, Completer} = erlmcp_cli_completer:init([{command, <<"help">>}]),

    %% Request async completion
    Self = self(),
    ok =
        erlmcp_cli_completer:complete_async(Completer,
                                            <<"h">>,
                                            fun(Matches) -> Self ! {completion_result, Matches}
                                            end),

    %% Wait for async result
    receive
        {completion_result, Matches} ->
            ?assertEqual([<<"help">>], Matches)
    after 5000 ->
        error(timeout)
    end,

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% Timeout Tests
%%%====================================================================

test_completion_timeout() ->
    %% Slow custom provider
    SlowProvider =
        fun(_Prefix) ->
           timer:sleep(10000),
           [<<"slow">>]
        end,

    {ok, Completer} = erlmcp_cli_completer:init([], #{timeout => 1000}),
    ok = erlmcp_cli_completer:add_provider(Completer, slow, SlowProvider),

    %% Should timeout
    Result = erlmcp_cli_completer:complete(Completer, <<"test">>),
    ?assertMatch({error, timeout}, Result),

    ok = erlmcp_cli_completer:stop(Completer).

%%%====================================================================
%%% Edge Cases
%%%====================================================================

edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Empty prefix completion", fun test_empty_prefix/0},
      {"Very long prefix", fun test_long_prefix/0},
      {"Special characters in prefix", fun test_special_chars/0},
      {"Unicode prefix", fun test_unicode_prefix/0},
      {"Completion with duplicates", fun test_duplicates/0}]}.

test_empty_prefix() ->
    {ok, Completer} = erlmcp_cli_completer:init([{command, <<"help">>}]),

    %% Empty prefix → should return all commands
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"">>),
    ?assert(length(Matches) > 0),

    ok = erlmcp_cli_completer:stop(Completer).

test_long_prefix() ->
    {ok, Completer} = erlmcp_cli_completer:init([]),

    %% Very long prefix (10KB)
    LongPrefix = binary:copy(<<"a">>, 10000),
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, LongPrefix),
    ?assertEqual([], Matches),

    ok = erlmcp_cli_completer:stop(Completer).

test_special_chars() ->
    {ok, Completer} = erlmcp_cli_completer:init([{command, <<"--help">>}]),

    %% Complete with special characters
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"--h">>),
    ?assertEqual([<<"--help">>], Matches),

    ok = erlmcp_cli_completer:stop(Completer).

test_unicode_prefix() ->
    {ok, Completer} = erlmcp_cli_completer:init([{command, <<"世界">>}]),

    %% Complete Unicode prefix
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"世">>),
    ?assertEqual([<<"世界">>], Matches),

    ok = erlmcp_cli_completer:stop(Completer).

test_duplicates() ->
    {ok, Completer} =
        erlmcp_cli_completer:init([{command, <<"help">>},
                                   {command, <<"help">>}]),  %% Duplicate

    %% Should deduplicate results
    {ok, Matches} = erlmcp_cli_completer:complete(Completer, <<"h">>),
    ?assertEqual([<<"help">>], Matches),

    ok = erlmcp_cli_completer:stop(Completer).
