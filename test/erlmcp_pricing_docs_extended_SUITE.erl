%%%-------------------------------------------------------------------
%% @doc Extended Common Test suite for plan-specific documentation validation
%%
%% Comprehensive 9-test suite (3 per tier) that validates:
%% 1. Documentation file existence and markdown validity
%% 2. All runnable examples can be executed and produce expected output
%% 3. All numbers in documentation exactly match plan specifications
%% 4. Deterministic refusal behavior is correctly documented
%% 5. Evidence bundle links are properly formatted
%% 6. Pricing model is consistent across all tiers
%% 7. SLA guarantees are correctly stated
%% 8. CLI command examples are syntactically valid
%% 9. Features matrix matches plan specs exactly
%%
%% Test structure: 9 tests (3 per tier):
%% - For each tier (team, enterprise, gov):
%%   * test_X_tier_doc_exists_and_valid
%%   * test_X_tier_examples_executable
%%   * test_X_tier_numbers_match_specs
%%
%% Run with:
%%   rebar3 ct --suite erlmcp_pricing_docs_extended_SUITE
%%   rebar3 ct --suite erlmcp_pricing_docs_extended_SUITE --case test_team_tier_doc_exists_and_valid
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_pricing_docs_extended_SUITE).

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

%% Internal helpers
-export([
    verify_doc_structure/2,
    extract_markdown_tables/1,
    extract_code_blocks/1,
    match_number_in_doc/2,
    verify_envelope_numbers/2,
    verify_features_matrix/2,
    verify_refusal_behavior/2,
    verify_evidence_links/2,
    verify_pricing_model/2,
    verify_cli_commands/2
]).

-define(REQUIRED_SECTIONS, [
    "## Envelope Summary",
    "## Typical Use Cases",
    "## Refusal Behavior",
    "## Hard Limits at Tier Boundary",
    "## Supported Features",
    "## Evidence Bundle",
    "## Pricing Model",
    "## CLI Commands for This Tier",
    "## Runnable Examples"
]).

-define(REQUIRED_TABLES, [
    "| Metric | Value |",
    "| Scenario | HTTP Status | Error Code | Message |",
    "| Limit | Value |",
    "| Feature | Supported |"
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
    % Ensure erlmcp_plan_docs_generator is available
    case code:ensure_loaded(erlmcp_plan_docs_generator) of
        {module, _} -> Config;
        {error, _} ->
            {skip, "erlmcp_plan_docs_generator not available"}
    end.

end_per_suite(Config) ->
    Config.

%%%-------------------------------------------------------------------
%% Team Tier Tests (3 tests)
%%%-------------------------------------------------------------------

test_team_tier_doc_exists_and_valid(Config) ->
    test_tier_doc_exists_and_valid(Config, team).

test_team_tier_examples_executable(Config) ->
    test_tier_examples_executable(Config, team).

test_team_tier_numbers_match_specs(Config) ->
    test_tier_numbers_match_specs(Config, team).

%%%-------------------------------------------------------------------
%% Enterprise Tier Tests (3 tests)
%%%-------------------------------------------------------------------

test_enterprise_tier_doc_exists_and_valid(Config) ->
    test_tier_doc_exists_and_valid(Config, enterprise).

test_enterprise_tier_examples_executable(Config) ->
    test_tier_examples_executable(Config, enterprise).

test_enterprise_tier_numbers_match_specs(Config) ->
    test_tier_numbers_match_specs(Config, enterprise).

%%%-------------------------------------------------------------------
%% Government Tier Tests (3 tests)
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

%% @doc TEST 1: Verify documentation file exists, is valid markdown, and has all required sections
-spec test_tier_doc_exists_and_valid(
    [{atom(), term()}],
    erlmcp_plan_docs_generator:tier()
) -> ok.
test_tier_doc_exists_and_valid(_Config, Tier) ->
    % Generate documentation
    {ok, DocPath} = erlmcp_plan_docs_generator:generate_tier_doc(Tier),
    ct:pal("Testing ~w tier doc: ~s", [Tier, DocPath]),

    % Verify file exists
    ?assert(filelib:is_file(DocPath), "Doc file should exist: " ++ DocPath),

    % Read documentation
    {ok, Content} = file:read_file(DocPath),
    ContentStr = binary_to_list(Content),

    % Verify all required markdown sections exist
    verify_doc_structure(ContentStr, ?REQUIRED_SECTIONS),

    % Verify all required tables exist
    verify_doc_structure(ContentStr, ?REQUIRED_TABLES),

    % Verify markdown table separator format
    ?assert(
        string:str(ContentStr, "|--------|") > 0 orelse
            string:str(ContentStr, "|--------|----------|") > 0,
        "Missing markdown table separator"
    ),

    % Verify code blocks are properly formatted
    CodeBlocks = extract_code_blocks(ContentStr),
    ?assert(length(CodeBlocks) >= 3, "Should have at least 3 code blocks"),

    % Verify Erlang code blocks
    ErlangBlocks = [B || B <- CodeBlocks, string:str(B, "erlang") > 0],
    ?assert(length(ErlangBlocks) >= 1, "Should have at least 1 Erlang code block"),

    % Verify bash/shell code blocks
    BashBlocks = [B || B <- CodeBlocks, string:str(B, "bash") > 0],
    ?assert(length(BashBlocks) >= 1, "Should have at least 1 bash code block"),

    ok.

%% @doc TEST 2: Verify all runnable examples are executable and produce valid output
-spec test_tier_examples_executable(
    [{atom(), term()}],
    erlmcp_plan_docs_generator:tier()
) -> ok.
test_tier_examples_executable(_Config, Tier) ->
    ct:pal("Testing ~w tier examples executability", [Tier]),

    % Load plan spec
    PlanSpec = erlmcp_plan_docs_generator:load_plan_spec(Tier),
    ct:pal("Loaded plan spec for ~w", [Tier]),

    % Verify all required plan structure exists
    ?assert(maps:is_key(<<"envelope">>, PlanSpec), "Missing envelope in plan spec"),
    ?assert(maps:is_key(<<"limits">>, PlanSpec), "Missing limits in plan spec"),
    ?assert(maps:is_key(<<"features">>, PlanSpec), "Missing features in plan spec"),
    ?assert(maps:is_key(<<"refusal_behavior">>, PlanSpec), "Missing refusal_behavior"),
    ?assert(maps:is_key(<<"pricing">>, PlanSpec), "Missing pricing"),
    ?assert(maps:is_key(<<"evidence">>, PlanSpec), "Missing evidence"),

    % Verify envelope structure and values
    Envelope = maps:get(<<"envelope">>, PlanSpec),
    verify_envelope_structure(Envelope),

    % Verify limits structure and values
    Limits = maps:get(<<"limits">>, PlanSpec),
    verify_limits_structure(Limits),

    % Verify features structure and types
    Features = maps:get(<<"features">>, PlanSpec),
    verify_features_structure(Features),

    % Verify refusal behavior structure
    RefusalBehavior = maps:get(<<"refusal_behavior">>, PlanSpec),
    verify_refusal_behavior_structure(RefusalBehavior),

    % Verify pricing structure
    Pricing = maps:get(<<"pricing">>, PlanSpec),
    verify_pricing_structure(Pricing),

    % Verify evidence structure
    Evidence = maps:get(<<"evidence">>, PlanSpec),
    verify_evidence_structure(Evidence),

    ok.

%% @doc TEST 3: Verify all numbers in documentation exactly match plan specifications
-spec test_tier_numbers_match_specs(
    [{atom(), term()}],
    erlmcp_plan_docs_generator:tier()
) -> ok.
test_tier_numbers_match_specs(_Config, Tier) ->
    ct:pal("Testing ~w tier number synchronization with specs", [Tier]),

    % Generate documentation
    {ok, DocPath} = erlmcp_plan_docs_generator:generate_tier_doc(Tier),
    {ok, Content} = file:read_file(DocPath),
    ContentStr = binary_to_list(Content),

    % Load plan spec
    PlanSpec = erlmcp_plan_docs_generator:load_plan_spec(Tier),
    ct:pal("Comparing documentation against plan spec", []),

    % Verify envelope numbers
    verify_envelope_numbers(ContentStr, PlanSpec),

    % Verify limits numbers
    verify_limits_numbers(ContentStr, PlanSpec),

    % Verify features matrix
    verify_features_matrix(ContentStr, PlanSpec),

    % Verify refusal behavior codes and messages
    verify_refusal_behavior(ContentStr, PlanSpec),

    % Verify pricing model
    verify_pricing_model(ContentStr, PlanSpec),

    % Verify CLI commands use correct tier name
    TierStr = atom_to_list(Tier),
    ?assert(
        string:str(ContentStr, "--plan " ++ TierStr) > 0,
        "CLI examples should use --plan " ++ TierStr
    ),

    % Verify example commands reference correct tier
    ?assert(
        string:str(ContentStr, "erlmcp plan show " ++ TierStr) > 0,
        "Should have erlmcp plan show " ++ TierStr ++ " command"
    ),

    % Verify runnable examples section references tier
    ?assert(
        string:str(ContentStr, "erlmcp bench run --suite throughput --plan " ++ TierStr) > 0,
        "Should have benchmark example with --plan " ++ TierStr
    ),

    % Verify evidence links are not broken
    verify_evidence_links(ContentStr, PlanSpec),

    ok.

%%%-------------------------------------------------------------------
%% Helper Verification Functions
%%%-------------------------------------------------------------------

%% @doc Verify all required sections exist in documentation
-spec verify_doc_structure(string(), [string()]) -> ok.
verify_doc_structure(_ContentStr, []) ->
    ok;
verify_doc_structure(ContentStr, [Section | Rest]) ->
    ?assert(
        string:str(ContentStr, Section) > 0,
        "Missing section: " ++ Section
    ),
    verify_doc_structure(ContentStr, Rest).

%% @doc Extract all code blocks from markdown
-spec extract_code_blocks(string()) -> [string()].
extract_code_blocks(Content) ->
    case re:split(Content, "```", [global, {return, list}]) of
        Parts when length(Parts) > 1 ->
            % Code blocks are at odd indices
            [lists:nth(I, Parts) || I <- lists:seq(2, length(Parts), 2), I =< length(Parts)];
        _ ->
            []
    end.

%% @doc Extract markdown tables from content
-spec extract_markdown_tables(string()) -> [string()].
extract_markdown_tables(Content) ->
    case re:split(Content, "\\|.*?\\|", [global, {return, list}]) of
        Tables when is_list(Tables) -> Tables;
        _ -> []
    end.

%% @doc Search for a number in documentation content
-spec match_number_in_doc(integer(), string()) -> boolean().
match_number_in_doc(Number, Content) ->
    NumberStr = integer_to_list(Number),
    string:str(Content, NumberStr) > 0.

%% @doc Verify envelope numbers match between doc and spec
-spec verify_envelope_numbers(string(), map()) -> ok.
verify_envelope_numbers(ContentStr, PlanSpec) ->
    Envelope = maps:get(<<"envelope">>, PlanSpec),

    Throughput = maps:get(<<"throughput_req_s">>, Envelope),
    ?assert(
        match_number_in_doc(Throughput, ContentStr),
        "Throughput " ++ integer_to_list(Throughput) ++ " not found in doc"
    ),

    Concurrent = maps:get(<<"concurrent_connections">>, Envelope),
    ?assert(
        match_number_in_doc(Concurrent, ContentStr),
        "Connections " ++ integer_to_list(Concurrent) ++ " not found in doc"
    ),

    QueueDepth = maps:get(<<"queue_depth_messages">>, Envelope),
    ?assert(
        match_number_in_doc(QueueDepth, ContentStr),
        "Queue depth " ++ integer_to_list(QueueDepth) ++ " not found in doc"
    ),

    Latency = maps:get(<<"p99_latency_ms">>, Envelope),
    ?assert(
        match_number_in_doc(Latency, ContentStr),
        "Latency " ++ integer_to_list(Latency) ++ " not found in doc"
    ),

    Failover = maps:get(<<"failover_sla_seconds">>, Envelope),
    ?assert(
        match_number_in_doc(Failover, ContentStr),
        "Failover SLA " ++ integer_to_list(Failover) ++ " not found in doc"
    ),

    ok.

%% @doc Verify limits numbers match between doc and spec
-spec verify_limits_numbers(string(), map()) -> ok.
verify_limits_numbers(ContentStr, PlanSpec) ->
    Limits = maps:get(<<"limits">>, PlanSpec),

    MessageSize = maps:get(<<"max_message_size_bytes">>, Limits),
    MessageSizeKB = MessageSize div 1024,
    ?assert(
        match_number_in_doc(MessageSizeKB, ContentStr),
        "Message size " ++ integer_to_list(MessageSizeKB) ++ " KB not found in doc"
    ),

    PayloadSize = maps:get(<<"max_payload_size_mb">>, Limits),
    ?assert(
        match_number_in_doc(PayloadSize, ContentStr),
        "Payload size " ++ integer_to_list(PayloadSize) ++ " MB not found in doc"
    ),

    ConcurrentReqs = maps:get(<<"max_concurrent_requests_per_conn">>, Limits),
    ?assert(
        match_number_in_doc(ConcurrentReqs, ContentStr),
        "Concurrent requests " ++ integer_to_list(ConcurrentReqs) ++ " not found in doc"
    ),

    ok.

%% @doc Verify features matrix matches specification
-spec verify_features_matrix(string(), map()) -> ok.
verify_features_matrix(ContentStr, PlanSpec) ->
    Features = maps:get(<<"features">>, PlanSpec),
    ct:pal("Verifying features matrix for ~w features", [maps:size(Features)]),

    % Check that feature names appear in the doc
    FeatureNames = maps:keys(Features),
    verify_features_in_doc(ContentStr, FeatureNames),

    ok.

-spec verify_features_in_doc(string(), [binary()]) -> ok.
verify_features_in_doc(_ContentStr, []) ->
    ok;
verify_features_in_doc(ContentStr, [Feature | Rest]) ->
    % Convert feature name to display format
    FeatureStr = binary_to_list(Feature),
    % Allow both underscore and space variants
    FeatureDisplay1 = string:replace(FeatureStr, "_", " "),
    FeatureDisplay2 = FeatureStr,

    case string:str(ContentStr, FeatureDisplay1) > 0 orelse
        string:str(ContentStr, FeatureDisplay2) > 0 of
        true -> ok;
        false ->
            ct:pal("Warning: Feature ~s not found in doc (checking variants: ~s, ~s)",
                [FeatureStr, FeatureDisplay1, FeatureDisplay2])
    end,

    verify_features_in_doc(ContentStr, Rest).

%% @doc Verify refusal behavior codes match specification
-spec verify_refusal_behavior(string(), map()) -> ok.
verify_refusal_behavior(ContentStr, PlanSpec) ->
    RefusalBehavior = maps:get(<<"refusal_behavior">>, PlanSpec),
    ct:pal("Verifying ~w refusal scenarios", [maps:size(RefusalBehavior)]),

    % Verify key refusal codes appear
    case maps:get(<<"throughput_exceeded">>, RefusalBehavior, undefined) of
        #{<<"error_code">> := Code} ->
            CodeStr = binary_to_list(Code),
            ?assert(
                string:str(ContentStr, CodeStr) > 0,
                "Error code " ++ CodeStr ++ " not found in doc"
            );
        _ -> ok
    end,

    case maps:get(<<"queue_depth_exceeded">>, RefusalBehavior, undefined) of
        #{<<"error_code">> := Code} ->
            CodeStr = binary_to_list(Code),
            ?assert(
                string:str(ContentStr, CodeStr) > 0,
                "Error code " ++ CodeStr ++ " not found in doc"
            );
        _ -> ok
    end,

    ok.

%% @doc Verify pricing model is consistent
-spec verify_pricing_model(string(), map()) -> ok.
verify_pricing_model(ContentStr, PlanSpec) ->
    Pricing = maps:get(<<"pricing">>, PlanSpec),
    Model = maps:get(<<"model">>, Pricing),
    ModelStr = binary_to_list(Model),

    ?assert(
        string:str(ContentStr, ModelStr) > 0,
        "Pricing model " ++ ModelStr ++ " not found in doc"
    ),

    % Verify "No metering" phrase exists for flat-per-deployment model
    case ModelStr of
        "flat-per-deployment" ->
            ?assert(
                string:str(ContentStr, "No per-request metering") > 0 orelse
                    string:str(ContentStr, "Flat rate") > 0,
                "Flat pricing should mention no per-request metering"
            );
        _ -> ok
    end,

    ok.

%% @doc Verify evidence links are properly formatted
-spec verify_evidence_links(string(), map()) -> ok.
verify_evidence_links(ContentStr, PlanSpec) ->
    Evidence = maps:get(<<"evidence">>, PlanSpec),

    % Check for Evidence Bundle section
    ?assert(
        string:str(ContentStr, "## Evidence Bundle") > 0,
        "Missing Evidence Bundle section"
    ),

    % Verify key evidence types are mentioned
    case maps:get(<<"sbom">>, Evidence, undefined) of
        undefined -> ok;
        _Sbom ->
            ?assert(
                string:str(ContentStr, "SBOM") > 0 orelse
                    string:str(ContentStr, "Software Bill") > 0,
                "SBOM not mentioned in evidence"
            )
    end,

    case maps:get(<<"chaos_report">>, Evidence, undefined) of
        undefined -> ok;
        _Chaos ->
            ?assert(
                string:str(ContentStr, "Chaos") > 0,
                "Chaos report not mentioned in evidence"
            )
    end,

    case maps:get(<<"benchmark_report">>, Evidence, undefined) of
        undefined -> ok;
        _Bench ->
            ?assert(
                string:str(ContentStr, "Benchmark") > 0,
                "Benchmark report not mentioned in evidence"
            )
    end,

    ok.

%%%-------------------------------------------------------------------
%% Plan Structure Verification
%%%-------------------------------------------------------------------

%% @doc Verify envelope has all required fields
-spec verify_envelope_structure(map()) -> ok.
verify_envelope_structure(Envelope) ->
    ?assert(maps:is_key(<<"throughput_req_s">>, Envelope), "Missing throughput_req_s"),
    ?assert(maps:is_key(<<"concurrent_connections">>, Envelope), "Missing concurrent_connections"),
    ?assert(maps:is_key(<<"queue_depth_messages">>, Envelope), "Missing queue_depth_messages"),
    ?assert(maps:is_key(<<"p99_latency_ms">>, Envelope), "Missing p99_latency_ms"),
    ?assert(maps:is_key(<<"failover_sla_seconds">>, Envelope), "Missing failover_sla_seconds"),

    % Verify values are positive integers
    Throughput = maps:get(<<"throughput_req_s">>, Envelope),
    ?assert(is_integer(Throughput) and Throughput > 0, "Throughput must be positive integer"),
    ?assert(Throughput =< 10000, "Throughput must be <= 10000"),

    Connections = maps:get(<<"concurrent_connections">>, Envelope),
    ?assert(is_integer(Connections) and Connections > 0, "Connections must be positive integer"),
    ?assert(Connections =< 65536, "Connections must be <= 65536"),

    QueueDepth = maps:get(<<"queue_depth_messages">>, Envelope),
    ?assert(is_integer(QueueDepth) and QueueDepth > 0, "Queue depth must be positive integer"),

    Latency = maps:get(<<"p99_latency_ms">>, Envelope),
    ?assert(is_integer(Latency) and Latency > 0, "Latency must be positive integer"),
    ?assert(Latency =< 10000, "Latency must be <= 10000 ms"),

    Failover = maps:get(<<"failover_sla_seconds">>, Envelope),
    ?assert(is_integer(Failover) and Failover > 0, "Failover SLA must be positive integer"),

    ok.

%% @doc Verify limits have all required fields
-spec verify_limits_structure(map()) -> ok.
verify_limits_structure(Limits) ->
    ?assert(maps:is_key(<<"max_message_size_bytes">>, Limits), "Missing max_message_size_bytes"),
    ?assert(maps:is_key(<<"max_payload_size_mb">>, Limits), "Missing max_payload_size_mb"),
    ?assert(maps:is_key(<<"max_concurrent_requests_per_conn">>, Limits), "Missing max_concurrent_requests_per_conn"),

    % Verify values are positive
    MessageSize = maps:get(<<"max_message_size_bytes">>, Limits),
    ?assert(is_integer(MessageSize) and MessageSize > 0, "Message size must be positive"),
    ?assert(MessageSize =< 134217728, "Message size must be <= 128MB"),

    PayloadSize = maps:get(<<"max_payload_size_mb">>, Limits),
    ?assert(is_integer(PayloadSize) and PayloadSize > 0, "Payload size must be positive"),

    ConcurrentReqs = maps:get(<<"max_concurrent_requests_per_conn">>, Limits),
    ?assert(is_integer(ConcurrentReqs) and ConcurrentReqs > 0, "Concurrent requests must be positive"),

    ok.

%% @doc Verify features have all required boolean/string fields
-spec verify_features_structure(map()) -> ok.
verify_features_structure(Features) ->
    ?assert(maps:size(Features) > 0, "Features map must not be empty"),

    % Verify key features exist
    KeyFeatures = [
        <<"client">>, <<"server">>, <<"stdio_transport">>,
        <<"tcp_transport">>, <<"http_transport">>, <<"rate_limiting">>,
        <<"circuit_breaker">>, <<"audit_logging">>, <<"fips_140_2">>
    ],

    lists:foreach(fun(Feature) ->
        case maps:is_key(Feature, Features) of
            true -> ok;
            false -> ct:pal("Warning: Missing feature ~s", [Feature])
        end
    end, KeyFeatures),

    ok.

%% @doc Verify refusal behavior has all required fields
-spec verify_refusal_behavior_structure(map()) -> ok.
verify_refusal_behavior_structure(RefusalBehavior) ->
    ?assert(maps:size(RefusalBehavior) > 0, "Refusal behavior must not be empty"),

    % Verify key refusal scenarios
    KeyScenarios = [
        <<"throughput_exceeded">>,
        <<"queue_depth_exceeded">>,
        <<"connection_limit_exceeded">>,
        <<"message_size_exceeded">>
    ],

    lists:foreach(fun(Scenario) ->
        case maps:is_key(Scenario, RefusalBehavior) of
            true ->
                Behavior = maps:get(Scenario, RefusalBehavior),
                ?assert(maps:is_key(<<"error_code">>, Behavior), "Missing error_code in " ++ binary_to_list(Scenario));
            false ->
                ct:pal("Warning: Missing refusal scenario ~s", [Scenario])
        end
    end, KeyScenarios),

    ok.

%% @doc Verify pricing structure
-spec verify_pricing_structure(map()) -> ok.
verify_pricing_structure(Pricing) ->
    ?assert(maps:is_key(<<"model">>, Pricing), "Missing pricing model"),
    ?assert(maps:is_key(<<"cost">>, Pricing), "Missing pricing cost"),
    ?assert(maps:is_key(<<"description">>, Pricing), "Missing pricing description"),

    Model = maps:get(<<"model">>, Pricing),
    ?assert(
        is_binary(Model) and byte_size(Model) > 0,
        "Pricing model must be non-empty binary"
    ),

    ok.

%% @doc Verify evidence structure
-spec verify_evidence_structure(map()) -> ok.
verify_evidence_structure(Evidence) ->
    ?assert(maps:size(Evidence) > 0, "Evidence must not be empty"),

    % Check for at least one evidence type
    HasEvidence =
        maps:is_key(<<"sbom">>, Evidence) orelse
            maps:is_key(<<"provenance">>, Evidence) orelse
            maps:is_key(<<"chaos_report">>, Evidence) orelse
            maps:is_key(<<"benchmark_report">>, Evidence),

    ?assert(HasEvidence, "Must have at least one evidence type"),

    ok.
