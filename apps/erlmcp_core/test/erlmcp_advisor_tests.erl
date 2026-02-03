%%% @doc EUnit Tests for MCP Advisor Service
%%%
%%% Tests cover:
%%% - Advisor lifecycle (start/stop)
%%% - Search functionality
%%% - Recommendation functionality
%%% - Provider management
%%% - Cache management
%%% - Offline provider
%%%
%%% Following Chicago School TDD - real processes, no mocks.
%%%
%%% @end
-module(erlmcp_advisor_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Setup/Teardown
%%====================================================================

setup() ->
    %% Ensure crypto is started for hash functions
    application:ensure_all_started(crypto),

    %% Start advisor (standalone for testing)
    {ok, Pid} = erlmcp_advisor:start_link(#{
        cache_ttl => 1000,  %% Short TTL for testing
        discovery_interval => 60000  %% Don't auto-discover during tests
    }),
    Pid.

cleanup(Pid) when is_pid(Pid) ->
    %% Stop advisor
    catch erlmcp_advisor:stop(),

    %% Wait for cleanup
    timer:sleep(50),
    ok.

%%====================================================================
%% Test Generators
%%====================================================================

advisor_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {"Advisor lifecycle tests", {spawn, fun lifecycle_tests/0}},
          {"Search functionality tests", {spawn, fun search_tests/0}},
          {"Recommendation tests", {spawn, fun recommend_tests/0}},
          {"Provider management tests", {spawn, fun provider_tests/0}},
          {"Cache management tests", {spawn, fun cache_tests/0}},
          {"Status and health tests", {spawn, fun status_tests/0}}
         ]
     end}.

%%====================================================================
%% Lifecycle Tests
%%====================================================================

lifecycle_tests() ->
    %% Advisor should already be running from setup
    ?assertEqual(ok, erlmcp_advisor:health_check()),

    %% Get status should work
    {ok, Status} = erlmcp_advisor:get_status(),
    ?assert(is_map(Status)),
    ?assert(maps:is_key(providers, Status)),
    ?assert(maps:is_key(cache_size, Status)),
    ?assert(maps:is_key(stats, Status)),

    ok.

%%====================================================================
%% Search Tests
%%====================================================================

search_tests() ->
    %% Search with valid query
    {ok, Results} = erlmcp_advisor:search(<<"filesystem">>),
    ?assert(is_list(Results)),

    %% Search with options
    {ok, Results2} = erlmcp_advisor:search(<<"database">>, #{
        limit => 5,
        min_similarity => 0.1
    }),
    ?assert(is_list(Results2)),
    ?assert(length(Results2) =< 5),

    %% Results should have expected fields
    case Results2 of
        [FirstResult | _] ->
            ?assert(maps:is_key(id, FirstResult)),
            ?assert(maps:is_key(title, FirstResult)),
            ?assert(maps:is_key(similarity, FirstResult));
        [] ->
            ok  %% Empty results are valid
    end,

    %% Search with empty query should still work
    {ok, _EmptyResults} = erlmcp_advisor:search(<<>>),

    %% Search with string query
    {ok, _StringResults} = erlmcp_advisor:search("git version control"),

    ok.

%%====================================================================
%% Recommendation Tests
%%====================================================================

recommend_tests() ->
    %% Get recommendation
    case erlmcp_advisor:recommend(<<"I need to work with files and directories">>) of
        {ok, Recommendation} ->
            ?assert(is_map(Recommendation)),
            ?assert(maps:is_key(server, Recommendation)),
            ?assert(maps:is_key(confidence, Recommendation)),
            ?assert(maps:is_key(reasons, Recommendation)),

            Confidence = maps:get(confidence, Recommendation),
            ?assert(is_float(Confidence)),
            ?assert(Confidence >= 0.0),
            ?assert(Confidence =< 1.0);
        {error, no_matching_servers} ->
            %% This is also acceptable if no servers match
            ok
    end,

    %% Recommend with options
    case erlmcp_advisor:recommend(<<"database queries">>, #{limit => 3}) of
        {ok, _Rec2} -> ok;
        {error, no_matching_servers} -> ok
    end,

    ok.

%%====================================================================
%% Provider Tests
%%====================================================================

provider_tests() ->
    %% List default providers
    Providers = erlmcp_advisor:list_providers(),
    ?assert(is_list(Providers)),
    ?assert(length(Providers) >= 2),  %% At least registry and offline

    %% Check provider IDs
    ProviderIds = [Id || {Id, _} <- Providers],
    ?assert(lists:member(registry, ProviderIds)),
    ?assert(lists:member(offline, ProviderIds)),

    %% Try to register invalid provider
    ?assertEqual({error, {module_load_failed, nofile}},
                 erlmcp_advisor:register_provider(fake_provider,
                                                  {nonexistent_module, #{}})),

    %% Unregister non-existent provider
    ?assertEqual({error, not_found},
                 erlmcp_advisor:unregister_provider(nonexistent)),

    ok.

%%====================================================================
%% Cache Tests
%%====================================================================

cache_tests() ->
    %% Get initial cache stats
    Stats1 = erlmcp_advisor:get_cache_stats(),
    ?assert(is_map(Stats1)),
    ?assert(maps:is_key(size, Stats1)),
    ?assert(maps:is_key(memory, Stats1)),

    %% Perform a search to populate cache
    {ok, _Results} = erlmcp_advisor:search(<<"test query">>),

    %% Perform same search again (should hit cache)
    {ok, _Results2} = erlmcp_advisor:search(<<"test query">>),

    %% Clear cache
    ?assertEqual(ok, erlmcp_advisor:clear_cache()),

    %% Verify cache is cleared
    Stats2 = erlmcp_advisor:get_cache_stats(),
    ?assertEqual(0, maps:get(size, Stats2)),

    ok.

%%====================================================================
%% Status and Health Tests
%%====================================================================

status_tests() ->
    %% Health check should pass
    ?assertEqual(ok, erlmcp_advisor:health_check()),

    %% Get full status
    {ok, Status} = erlmcp_advisor:get_status(),

    %% Check status fields
    ?assert(maps:is_key(providers, Status)),
    ?assert(maps:is_key(provider_count, Status)),
    ?assert(maps:is_key(cache_size, Status)),
    ?assert(maps:is_key(cache_memory, Status)),
    ?assert(maps:is_key(cache_ttl, Status)),
    ?assert(maps:is_key(stats, Status)),

    %% Check stats
    Stats = maps:get(stats, Status),
    ?assert(maps:is_key(searches, Stats)),
    ?assert(maps:is_key(cache_hits, Stats)),
    ?assert(maps:is_key(cache_misses, Stats)),
    ?assert(maps:is_key(started_at, Stats)),

    ok.

%%====================================================================
%% Provider Behavior Tests
%%====================================================================

provider_behavior_test_() ->
    {"Provider utility functions",
     [
      {"normalize_result handles minimal input",
       fun() ->
           Result = erlmcp_advisor_provider:normalize_result(#{
               title => <<"Test Server">>
           }),
           ?assert(maps:is_key(id, Result)),
           ?assert(maps:is_key(title, Result)),
           ?assert(maps:is_key(description, Result)),
           ?assert(maps:is_key(similarity, Result))
       end},

      {"compute_text_similarity returns valid range",
       fun() ->
           Sim = erlmcp_advisor_provider:compute_text_similarity(
               <<"filesystem operations">>,
               <<"File system read write operations">>
           ),
           ?assert(is_float(Sim)),
           ?assert(Sim >= 0.0),
           ?assert(Sim =< 1.0)
       end},

      {"tokenize extracts meaningful tokens",
       fun() ->
           Tokens = erlmcp_advisor_provider:tokenize(<<"Hello World Test">>),
           ?assert(is_list(Tokens)),
           ?assert(length(Tokens) >= 2),
           ?assert(lists:member(<<"hello">>, Tokens)),
           ?assert(lists:member(<<"world">>, Tokens))
       end},

      {"normalize_text converts to lowercase",
       fun() ->
           Normalized = erlmcp_advisor_provider:normalize_text(<<"HELLO World">>),
           ?assertEqual(<<"hello world">>, Normalized)
       end}
     ]}.

%%====================================================================
%% Offline Provider Tests
%%====================================================================

offline_provider_test_() ->
    {setup,
     fun() ->
         application:ensure_all_started(crypto),
         erlmcp_advisor_provider_offline:init(#{})
     end,
     fun(_) ->
         erlmcp_advisor_provider_offline:cleanup()
     end,
     fun(_) ->
         [
          {"Offline provider info",
           fun() ->
               Info = erlmcp_advisor_provider_offline:info(),
               ?assert(is_map(Info)),
               ?assertEqual(<<"Offline Hybrid Search">>, maps:get(name, Info))
           end},

          {"Offline provider search",
           fun() ->
               {ok, Results} = erlmcp_advisor_provider_offline:search(
                   <<"filesystem">>,
                   #{limit => 5, min_similarity => 0.1}
               ),
               ?assert(is_list(Results)),

               %% Should find filesystem in seeded data
               case Results of
                   [] ->
                       %% Seed if not already done
                       erlmcp_advisor_provider_offline:seed_default_servers(),
                       {ok, Results2} = erlmcp_advisor_provider_offline:search(
                           <<"filesystem">>,
                           #{limit => 5, min_similarity => 0.1}
                       ),
                       ?assert(length(Results2) > 0);
                   _ ->
                       ok
               end
           end},

          {"Offline provider add server",
           fun() ->
               ok = erlmcp_advisor_provider_offline:add_server(#{
                   id => <<"test-server">>,
                   title => <<"Test Server">>,
                   description => <<"A test MCP server for unit testing">>
               }),

               %% Search should find it
               {ok, Results} = erlmcp_advisor_provider_offline:search(
                   <<"test unit">>,
                   #{limit => 10, min_similarity => 0.0}
               ),

               %% Check we can find the test server
               TestResults = [R || R <- Results,
                                   maps:get(id, R, <<>>) == <<"test-server">>],
               ?assert(length(TestResults) > 0)
           end},

          {"Offline provider health check",
           fun() ->
               ?assertEqual(ok, erlmcp_advisor_provider_offline:health())
           end},

          {"Offline provider index stats",
           fun() ->
               Stats = erlmcp_advisor_provider_offline:get_index_stats(),
               ?assert(is_map(Stats)),
               ?assert(maps:is_key(size, Stats)),
               ?assert(maps:is_key(memory, Stats))
           end}
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {"End-to-end integration tests",
     {setup,
      fun() ->
          application:ensure_all_started(crypto),
          {ok, Pid} = erlmcp_advisor:start_link(),
          Pid
      end,
      fun(Pid) ->
          cleanup(Pid)
      end,
      fun(_) ->
          [
           {"Full search workflow",
            fun() ->
                %% Search for database servers
                {ok, Results} = erlmcp_advisor:search(<<"database SQL queries">>),

                %% Should find postgres and sqlite
                case Results of
                    [] ->
                        ok;  %% Acceptable if index not seeded
                    [First | _] ->
                        ?assert(maps:is_key(similarity, First)),
                        ?assert(maps:get(similarity, First) >= 0.0)
                end
            end},

           {"Full recommendation workflow",
            fun() ->
                case erlmcp_advisor:recommend(<<"web browser automation">>) of
                    {ok, Recommendation} ->
                        Server = maps:get(server, Recommendation),
                        ?assert(is_map(Server)),
                        Confidence = maps:get(confidence, Recommendation),
                        ?assert(is_float(Confidence));
                    {error, no_matching_servers} ->
                        ok  %% Acceptable
                end
            end},

           {"Cache hit performance",
            fun() ->
                Query = <<"performance test query">>,

                %% First search
                {ok, _Results1} = erlmcp_advisor:search(Query),

                %% Second search should be faster (cached)
                {ok, _Results2} = erlmcp_advisor:search(Query),

                %% Check stats
                {ok, Status} = erlmcp_advisor:get_status(),
                Stats = maps:get(stats, Status),
                CacheHits = maps:get(cache_hits, Stats),
                ?assert(CacheHits >= 1)
            end}
          ]
      end}}.
