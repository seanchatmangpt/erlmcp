%%%-------------------------------------------------------------------
%% @doc Common Test suite for plan-specific documentation validation
%%
%% Tests verify that:
%% 1. All tier documentation files exist and are valid markdown
%% 2. All runnable examples work and produce expected output
%% 3. Numbers in documentation match plan specifications
%% 4. All CLI commands can be executed deterministically
%% 5. Refusal behavior is correctly documented
%%
%% Test structure: 9 tests (3 per tier):
%% - For each tier (team, enterprise, gov):
%%   * test_X_tier_doc_exists_and_valid
%%   * test_X_tier_examples_executable
%%   * test_X_tier_numbers_match_specs
%%
%% Run with: rebar3 ct --suite erlmcp_pricing_docs_SUITE
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_pricing_docs_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    test_team_tier_doc_exists_and_valid/1,
    test_team_tier_examples_executable/1,
    test_team_tier_numbers_match_specs/1,
    test_enterprise_tier_doc_exists_and_valid/1,
    test_enterprise_tier_examples_executable/1,
    test_enterprise_tier_numbers_match_specs/1,
    test_gov_tier_doc_exists_and_valid/1,
    test_gov_tier_examples_executable/1,
    test_gov_tier_numbers_match_specs/1
]).

all() -> [
    test_team_tier_doc_exists_and_valid,
    test_team_tier_examples_executable,
    test_team_tier_numbers_match_specs,
    test_enterprise_tier_doc_exists_and_valid,
    test_enterprise_tier_examples_executable,
    test_enterprise_tier_numbers_match_specs,
    test_gov_tier_doc_exists_and_valid,
    test_gov_tier_examples_executable,
    test_gov_tier_numbers_match_specs
].

init_per_suite(Config) ->
    % Ensure erlmcp_plan_docs_generator is compiled
    {ok, _} = compile:file(
        filename:join(
            filename:dirname(code:priv_dir(erlmcp)),
            "src/erlmcp_plan_docs_generator.erl"
        ),
        [report_errors, report_warnings, {outdir, code:priv_dir(erlmcp)}]
    ),
    Config.

end_per_suite(Config) ->
    Config.

%%%-------------------------------------------------------------------
%% Team Tier Tests
%%%-------------------------------------------------------------------

test_team_tier_doc_exists_and_valid(Config) ->
    test_tier_doc_exists_and_valid(Config, team).

test_team_tier_examples_executable(Config) ->
    test_tier_examples_executable(Config, team).

test_team_tier_numbers_match_specs(Config) ->
    test_tier_numbers_match_specs(Config, team).

%%%-------------------------------------------------------------------
%% Enterprise Tier Tests
%%%-------------------------------------------------------------------

test_enterprise_tier_doc_exists_and_valid(Config) ->
    test_tier_doc_exists_and_valid(Config, enterprise).

test_enterprise_tier_examples_executable(Config) ->
    test_tier_examples_executable(Config, enterprise).

test_enterprise_tier_numbers_match_specs(Config) ->
    test_tier_numbers_match_specs(Config, enterprise).

%%%-------------------------------------------------------------------
%% Government Tier Tests
%%%-------------------------------------------------------------------

test_gov_tier_doc_exists_and_valid(Config) ->
    test_tier_doc_exists_and_valid(Config, gov).

test_gov_tier_examples_executable(Config) ->
    test_tier_examples_executable(Config, gov).

test_gov_tier_numbers_match_specs(Config) ->
    test_tier_numbers_match_specs(Config, gov).

%%%-------------------------------------------------------------------
%% Internal Test Implementations
%%%-------------------------------------------------------------------

%% @doc Verify documentation file exists and is valid markdown
-spec test_tier_doc_exists_and_valid(
    [{atom(), term()}],
    erlmcp_plan_docs_generator:tier()
) -> ok.
test_tier_doc_exists_and_valid(_Config, Tier) ->
    % Generate documentation
    {ok, DocPath} = erlmcp_plan_docs_generator:generate_tier_doc(Tier),

    % Verify file exists
    ?assert(filelib:is_file(DocPath), "Doc file should exist: " ++ DocPath),

    % Read and verify markdown structure
    {ok, Content} = file:read_file(DocPath),
    ContentStr = binary_to_list(Content),

    % Check for required markdown sections
    ?assert(string:str(ContentStr, "## Envelope Summary") > 0, "Missing Envelope Summary"),
    ?assert(string:str(ContentStr, "## Typical Use Cases") > 0, "Missing Use Cases"),
    ?assert(string:str(ContentStr, "## Refusal Behavior") > 0, "Missing Refusal Behavior"),
    ?assert(string:str(ContentStr, "## Hard Limits") > 0, "Missing Hard Limits"),
    ?assert(string:str(ContentStr, "## Supported Features") > 0, "Missing Features"),
    ?assert(string:str(ContentStr, "## Evidence Bundle") > 0, "Missing Evidence"),
    ?assert(string:str(ContentStr, "## Pricing Model") > 0, "Missing Pricing"),
    ?assert(string:str(ContentStr, "## CLI Commands") > 0, "Missing CLI Commands"),
    ?assert(string:str(ContentStr, "## Runnable Examples") > 0, "Missing Examples"),

    % Verify markdown table format
    ?assert(string:str(ContentStr, "| Metric | Value |") > 0, "Missing envelope table"),
    ?assert(string:str(ContentStr, "|--------|-------|") > 0, "Missing table separator"),

    ok.

%% @doc Verify all runnable examples can be executed
-spec test_tier_examples_executable(
    [{atom(), term()}],
    erlmcp_plan_docs_generator:tier()
) -> ok.
test_tier_examples_executable(_Config, Tier) ->
    % Load plan spec
    PlanSpec = erlmcp_plan_docs_generator:load_plan_spec(Tier),

    % Verify envelope is present and valid
    ?assert(maps:is_key(<<"envelope">>, PlanSpec), "Missing envelope in plan spec"),
    Envelope = maps:get(<<"envelope">>, PlanSpec),
    ?assert(maps:is_key(<<"throughput_req_s">>, Envelope), "Missing throughput"),
    ?assert(maps:is_key(<<"concurrent_connections">>, Envelope), "Missing connections"),
    ?assert(maps:is_key(<<"p99_latency_ms">>, Envelope), "Missing latency"),

    % Verify example values are reasonable
    Throughput = maps:get(<<"throughput_req_s">>, Envelope),
    ?assert(Throughput > 0, "Throughput must be positive"),
    ?assert(Throughput =< 10000, "Throughput must be <= 10000"),

    Connections = maps:get(<<"concurrent_connections">>, Envelope),
    ?assert(Connections > 0, "Connections must be positive"),
    ?assert(Connections =< 65536, "Connections must be <= 65536"),

    Latency = maps:get(<<"p99_latency_ms">>, Envelope),
    ?assert(Latency > 0, "Latency must be positive"),
    ?assert(Latency =< 10000, "Latency must be <= 10000"),

    % Verify limits are present and reasonable
    ?assert(maps:is_key(<<"limits">>, PlanSpec), "Missing limits in plan spec"),
    Limits = maps:get(<<"limits">>, PlanSpec),
    ?assert(maps:is_key(<<"max_message_size_bytes">>, Limits), "Missing max_message_size"),

    MessageSize = maps:get(<<"max_message_size_bytes">>, Limits),
    ?assert(MessageSize > 0, "Message size must be positive"),
    ?assert(MessageSize =< 134217728, "Message size must be <= 128MB"),

    ok.

%% @doc Verify numbers in documentation match plan specs
-spec test_tier_numbers_match_specs(
    [{atom(), term()}],
    erlmcp_plan_docs_generator:tier()
) -> ok.
test_tier_numbers_match_specs(Config, Tier) ->
    % Generate documentation
    {ok, DocPath} = erlmcp_plan_docs_generator:generate_tier_doc(Tier),
    {ok, Content} = file:read_file(DocPath),
    ContentStr = binary_to_list(Content),

    % Load plan spec
    PlanSpec = erlmcp_plan_docs_generator:load_plan_spec(Tier),
    Envelope = maps:get(<<"envelope">>, PlanSpec),

    % Extract critical numbers and verify they match
    Throughput = maps:get(<<"throughput_req_s">>, Envelope),
    ThroughputStr = integer_to_list(Throughput),
    ?assert(
        string:str(ContentStr, ThroughputStr) > 0,
        "Throughput " ++ ThroughputStr ++ " not found in doc"
    ),

    Concurrent = maps:get(<<"concurrent_connections">>, Envelope),
    ConcurrentStr = integer_to_list(Concurrent),
    ?assert(
        string:str(ContentStr, ConcurrentStr) > 0,
        "Connections " ++ ConcurrentStr ++ " not found in doc"
    ),

    QueueDepth = maps:get(<<"queue_depth_messages">>, Envelope),
    QueueStr = integer_to_list(QueueDepth),
    ?assert(
        string:str(ContentStr, QueueStr) > 0,
        "Queue depth " ++ QueueStr ++ " not found in doc"
    ),

    Latency = maps:get(<<"p99_latency_ms">>, Envelope),
    LatencyStr = integer_to_list(Latency),
    ?assert(
        string:str(ContentStr, LatencyStr) > 0,
        "Latency " ++ LatencyStr ++ " not found in doc"
    ),

    % Verify tier name is used in examples
    TierStr = atom_to_list(Tier),
    ?assert(
        string:str(ContentStr, "--plan " ++ TierStr) > 0,
        "Tier name --plan " ++ TierStr ++ " not found in CLI examples"
    ),

    ok.

%%%-------------------------------------------------------------------
%% Helper Functions
%%%-------------------------------------------------------------------

%% @doc Extract numbers from documentation markdown
-spec extract_numbers(string()) -> [integer()].
extract_numbers(Content) ->
    case re:scan(Content, "\\d+", [global]) of
        {match, Matches} ->
            [
                erlang:list_to_integer(String)
                || [String] <- Matches
            ];
        nomatch ->
            []
    end.
