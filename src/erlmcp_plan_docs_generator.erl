%%%-------------------------------------------------------------------
%% @doc erlmcp_plan_docs_generator - Auto-generates tier-specific documentation
%% from plan specifications (plans/*.plan.json).
%%
%% This module reads plan specifications and generates markdown documentation
%% that includes:
%% - Envelope summary (throughput, concurrent, queue depth, latency, SLA)
%% - Use cases and example scenarios
%% - Refusal behavior with deterministic codes
%% - Limits at boundary conditions
%% - Evidence bundle links
%% - Pricing model
%% - CLI command examples
%% - Runnable examples with doc-tests
%%
%% Documentation is generated into docs/plans/{tier}.md and verified by tests.
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plan_docs_generator).

-export([
    generate_all_docs/0,
    generate_tier_doc/1,
    load_plan_spec/1,
    format_envelope_table/1,
    format_limits_table/1,
    format_features_table/1,
    format_refusal_table/1
]).


-type tier() :: team | enterprise | gov.
-type plan_spec() :: map().

%%%-------------------------------------------------------------------
%% Public API
%%%-------------------------------------------------------------------

%% @doc Generate all tier documentation files from plan specs
-spec generate_all_docs() -> {ok, [file:name()]} | {error, term()}.
generate_all_docs() ->
    Tiers = [team, enterprise, gov],
    Results = [generate_tier_doc(Tier) || Tier <- Tiers],
    case lists:partition(fun({ok, _}) -> true; (_) -> false end, Results) of
        {Success, []} ->
            Files = [F || {ok, F} <- Success],
            {ok, Files};
        {_Success, Errors} ->
            {error, {generation_failed, Errors}}
    end.

%% @doc Generate documentation for a specific tier
-spec generate_tier_doc(tier()) -> {ok, file:name()} | {error, term()}.
generate_tier_doc(Tier) ->
    try
        PlanSpec = load_plan_spec(Tier),
        Markdown = generate_markdown(Tier, PlanSpec),
        OutputPath = doc_path(Tier),
        ok = file:write_file(OutputPath, Markdown),
        {ok, OutputPath}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%% @doc Load plan specification from JSON file
-spec load_plan_spec(tier()) -> plan_spec().
load_plan_spec(Tier) ->
    FileName = filename:join(
        filename:dirname(code:priv_dir(erlmcp)),
        "plans/" ++ atom_to_list(Tier) ++ ".plan.json"
    ),
    {ok, Binary} = file:read_file(FileName),
    jsx:decode(Binary, [return_maps]).

%%%-------------------------------------------------------------------
%% Documentation Generation
%%%-------------------------------------------------------------------

-spec generate_markdown(tier(), plan_spec()) -> iodata().
generate_markdown(Tier, Spec) ->
    [
        generate_header(Tier, Spec),
        "\n\n",
        generate_envelope_section(Tier, Spec),
        "\n\n",
        generate_use_cases_section(Tier),
        "\n\n",
        generate_refusal_section(Spec),
        "\n\n",
        generate_limits_section(Spec),
        "\n\n",
        generate_features_section(Spec),
        "\n\n",
        generate_evidence_section(Tier, Spec),
        "\n\n",
        generate_pricing_section(Spec),
        "\n\n",
        generate_cli_examples_section(Tier, Spec),
        "\n\n",
        generate_runnable_examples_section(Tier, Spec),
        "\n"
    ].

-spec generate_header(tier(), plan_spec()) -> iodata().
generate_header(Tier, Spec) ->
    Name = maps:get(<<"name">>, Spec),
    Description = maps:get(<<"description">>, Spec),
    [
        "# ", Name, "\n\n",
        Description, "\n"
    ].

-spec generate_envelope_section(tier(), plan_spec()) -> iodata().
generate_envelope_section(Tier, Spec) ->
    Envelope = maps:get(<<"envelope">>, Spec),
    [
        "## Envelope Summary\n\n",
        "Your tier provides the following guaranteed capacity:\n\n",
        format_envelope_table(Envelope),
        "\n\n### What This Means\n\n",
        case Tier of
            team ->
                "- Perfect for hobby projects, proof-of-concepts, and startup MVPs\n"
                "- Suitable for development and testing environments\n"
                "- Ideal for learning the MCP protocol\n";
            enterprise ->
                "- Production-ready for enterprise applications\n"
                "- Supports multiple concurrent user sessions\n"
                "- Designed for business-critical workloads\n";
            gov ->
                "- Government and regulated environments\n"
                "- FIPS-140-2 compliant encryption\n"
                "- Comprehensive audit logging for compliance\n"
        end,
        "\n"
    ].

-spec generate_use_cases_section(tier()) -> iodata().
generate_use_cases_section(Tier) ->
    Cases = case Tier of
        team ->
            [
                "- **Hobby Projects**: Erlang learning projects with MCP integration",
                "- **Proof-of-Concepts**: Quick prototypes for new ideas",
                "- **Development**: Local development environments",
                "- **Testing**: Integration testing with modest load",
                "- **Open Source**: Community projects and research"
            ];
        enterprise ->
            [
                "- **Web Applications**: High-traffic web services",
                "- **Microservices**: Distributed system coordination",
                "- **IoT Platforms**: Real-time IoT device communication",
                "- **Gaming**: Multiplayer game backends",
                "- **SaaS**: Multi-tenant applications"
            ];
        gov ->
            [
                "- **Federal Agencies**: FIPS-140-2 compliant systems",
                "- **Regulated Finance**: Compliance-sensitive financial systems",
                "- **Healthcare**: HIPAA-compliant medical data systems",
                "- **Defense Contractors**: Secure government communication",
                "- **Audit-Required Industries**: Systems requiring immutable logs"
            ]
    end,
    [
        "## Typical Use Cases\n\n",
        string:join(Cases, "\n"),
        "\n"
    ].

-spec generate_refusal_section(plan_spec()) -> iodata().
generate_refusal_section(Spec) ->
    Refusal = maps:get(<<"refusal_behavior">>, Spec),
    [
        "## Refusal Behavior\n\n",
        "When your deployment hits tier limits, you'll receive deterministic errors:\n\n",
        format_refusal_table(Refusal),
        "\n\n### How to Handle Refusals\n\n",
        "```erlang\n",
        "handle_refusal(Response) ->\n",
        "    case Response of\n",
        "        {429, Code} when Code =:= 'rate_limit_exceeded' ->\n",
        "            RetryAfter = get_retry_after(Response),\n",
        "            timer:sleep(RetryAfter * 1000),\n",
        "            retry_request();\n",
        "        {503, Code} when Code =:= 'service_unavailable' ->\n",
        "            % Wait and retry with exponential backoff\n",
        "            backoff_retry(3);\n",
        "        {413, <<"payload_too_large">>} ->\n",
        "            % Split large payload into chunks\n",
        "            split_and_retry();\n",
        "        Error ->\n",
        "            handle_error(Error)\n",
        "    end.\n",
        "```\n"
    ].

-spec generate_limits_section(plan_spec()) -> iodata().
generate_limits_section(Spec) ->
    Limits = maps:get(<<"limits">>, Spec),
    [
        "## Hard Limits at Tier Boundary\n\n",
        "These limits are enforced deterministically. Exceeding them triggers refusal:\n\n",
        format_limits_table(Limits),
        "\n\n### Boundary Behavior\n\n",
        "**Testing boundary conditions:**\n\n",
        "```bash\n",
        "# Test throughput limit at boundary (last request succeeds, next fails)\n",
        "rebar3 do compile, eunit\n",
        "erlmcp bench run --suite throughput --plan team --target 450 --run 10s\n",
        "\n",
        "# Test message size limit\n",
        "erlmcp bench run --suite message-size --plan team\n",
        "\n",
        "# Test concurrent connections\n",
        "erlmcp bench run --suite connections --plan team\n",
        "```\n"
    ].

-spec generate_features_section(plan_spec()) -> iodata().
generate_features_section(Spec) ->
    Features = maps:get(<<"features">>, Spec),
    [
        "## Supported Features\n\n",
        format_features_table(Features),
        "\n"
    ].

-spec generate_evidence_section(tier(), plan_spec()) -> iodata().
generate_evidence_section(Tier, Spec) ->
    Evidence = maps:get(<<"evidence">>, Spec),
    [
        "## Evidence Bundle\n\n",
        "Your tier includes comprehensive validation evidence:\n\n",
        evidence_links(Evidence, Tier),
        "\n"
    ].

-spec evidence_links(map(), tier()) -> iodata().
evidence_links(Evidence, Tier) ->
    Links = [
        case maps:get(<<"sbom">>, Evidence, undefined) of
            undefined -> [];
            Sbom -> ["- [SBOM (Software Bill of Materials)](../", binary_to_list(Sbom), ")\n"]
        end,
        case maps:get(<<"provenance">>, Evidence, undefined) of
            undefined -> [];
            Prov -> ["- [Build Provenance](../", binary_to_list(Prov), ")\n"]
        end,
        case maps:get(<<"chaos_report">>, Evidence, undefined) of
            undefined -> [];
            Chaos -> ["- [Chaos Engineering Report](", binary_to_list(Chaos), ")\n"]
        end,
        case maps:get(<<"benchmark_report">>, Evidence, undefined) of
            undefined -> [];
            Bench -> ["- [Benchmark Report](", binary_to_list(Bench), ")\n"]
        end,
        case maps:get(<<"compliance_report">>, Evidence, undefined) of
            undefined -> [];
            Compliance -> ["- [Compliance Report](", binary_to_list(Compliance), ")\n"]
        end
    ],
    lists:append(Links).

-spec generate_pricing_section(plan_spec()) -> iodata().
generate_pricing_section(Spec) ->
    Pricing = maps:get(<<"pricing">>, Spec),
    Model = maps:get(<<"model">>, Pricing),
    Cost = maps:get(<<"cost">>, Pricing),
    Description = maps:get(<<"description">>, Pricing),
    [
        "## Pricing Model\n\n",
        "**Model**: ", binary_to_list(Model), "\n\n",
        "**Cost**: ", binary_to_list(Cost), "\n\n",
        "**Description**: ", binary_to_list(Description), "\n\n",
        "### No Surprises\n\n",
        "- No per-request metering\n",
        "- No overage charges\n",
        "- No hidden fees\n",
        "- Flat rate per deployment\n",
        "- Transparent SLA guarantees\n"
    ].

-spec generate_cli_examples_section(tier(), plan_spec()) -> iodata().
generate_cli_examples_section(Tier, Spec) ->
    [
        "## CLI Commands for This Tier\n\n",
        "Check your current tier and explore tier-specific operations:\n\n",
        "### Check Current Plan\n\n",
        "```bash\n",
        "erlmcp plan status\n",
        "```\n\n",
        "**Output:**\n",
        "```\n",
        "Current Plan: ", atom_to_list(Tier), "\n",
        "Throughput:   ", format_throughput(Spec), " req/s\n",
        "Connections:  ", format_connections(Spec), " concurrent\n",
        "Latency SLA:  ", format_latency(Spec), " ms (p99)\n",
        "```\n\n",
        "### Display Tier Specification\n\n",
        "```bash\n",
        "erlmcp plan show ", atom_to_list(Tier), "\n",
        "```\n\n",
        "### Run Tier-Specific Benchmark\n\n",
        "```bash\n",
        "erlmcp bench run --suite throughput --plan ", atom_to_list(Tier), "\n",
        "```\n\n",
        "### Export Audit Trail\n\n",
        "```bash\n",
        "erlmcp receipt export ", atom_to_list(Tier), " json | jq .\n",
        "```\n\n",
        "### Upgrade to Another Tier\n\n",
        "```bash\n",
        "erlmcp upgrade plan ", atom_to_list(Tier), " enterprise\n",
        "```\n"
    ].

-spec generate_runnable_examples_section(tier(), plan_spec()) -> iodata().
generate_runnable_examples_section(Tier, Spec) ->
    Envelope = maps:get(<<"envelope">>, Spec),
    Throughput = maps:get(<<"throughput_req_s">>, Envelope),
    [
        "## Runnable Examples\n\n",
        "These examples can be executed immediately and demonstrate tier behavior:\n\n",
        "### Example 1: Check Plan Status\n\n",
        "```erlang\n",
        "erl> erlmcp_plan:current_plan().\n",
        "{ok, #{tier => ", atom_to_list(Tier), ", throughput => ",
            integer_to_list(Throughput), "}}\n",
        "```\n\n",
        "### Example 2: Display Full Tier Envelope\n\n",
        "```erlang\n",
        "erl> erlmcp_plan:show(", atom_to_list(Tier), ").\n",
        "#{envelope => #{throughput_req_s => ", integer_to_list(Throughput),
            ", ...}}\n",
        "```\n\n",
        "### Example 3: Run Tier Benchmark\n\n",
        "```bash\n",
        "$ rebar3 do compile, eunit\n",
        "$ erlmcp bench run --suite throughput --plan ", atom_to_list(Tier),
            " --target ", integer_to_list(Throughput), " --duration 30s\n",
        "```\n\n",
        "### Example 4: Test Refusal Behavior\n\n",
        "```erlang\n",
        "erl> erlmcp_plan:test_refusal(throughput_exceeded).\n",
        "{429, <<"rate_limit_exceeded">>, <<"Request rate exceeds tier limit">>}\n",
        "```\n\n",
        "### Example 5: Export Audit Trail\n\n",
        "```bash\n",
        "$ erlmcp receipt export ", atom_to_list(Tier), " json | jq '.events | length'\n",
        "```\n"
    ].

%%%-------------------------------------------------------------------
%% Formatting Utilities
%%%-------------------------------------------------------------------

%% @doc Format envelope as markdown table
-spec format_envelope_table(map()) -> iodata().
format_envelope_table(Envelope) ->
    Throughput = maps:get(<<"throughput_req_s">>, Envelope),
    Concurrent = maps:get(<<"concurrent_connections">>, Envelope),
    QueueDepth = maps:get(<<"queue_depth_messages">>, Envelope),
    Latency = maps:get(<<"p99_latency_ms">>, Envelope),
    Failover = maps:get(<<"failover_sla_seconds">>, Envelope),
    [
        "| Metric | Value |\n",
        "|--------|-------|\n",
        "| Throughput | ", integer_to_list(Throughput), " req/s |\n",
        "| Concurrent Connections | ", integer_to_list(Concurrent), " |\n",
        "| Queue Depth | ", integer_to_list(QueueDepth), " messages |\n",
        "| P99 Latency | ", integer_to_list(Latency), " ms |\n",
        "| Failover SLA | ", integer_to_list(Failover), " seconds |\n"
    ].

%% @doc Format limits as markdown table
-spec format_limits_table(map()) -> iodata().
format_limits_table(Limits) ->
    MessageSize = maps:get(<<"max_message_size_bytes">>, Limits),
    PayloadSize = maps:get(<<"max_payload_size_mb">>, Limits),
    ConcurrentReqs = maps:get(<<"max_concurrent_requests_per_conn">>, Limits),
    [
        "| Limit | Value |\n",
        "|-------|-------|\n",
        "| Max Message Size | ", integer_to_list(MessageSize div 1024), " KB |\n",
        "| Max Payload | ", integer_to_list(PayloadSize), " MB |\n",
        "| Concurrent Requests/Conn | ", integer_to_list(ConcurrentReqs), " |\n"
    ].

%% @doc Format features as markdown table
-spec format_features_table(map()) -> iodata().
format_features_table(Features) ->
    FeatureList = [
        <<"client">>, <<"server">>, <<"stdio_transport">>, <<"tcp_transport">>,
        <<"http_transport">>, <<"websocket_transport">>, <<"sse_transport">>,
        <<"rate_limiting">>, <<"connection_pooling">>, <<"circuit_breaker">>,
        <<"otel_observability">>, <<"audit_logging">>, <<"fips_140_2">>,
        <<"high_availability">>
    ],
    [
        "| Feature | Supported |\n",
        "|---------|----------|\n",
        [format_feature_row(Feature, Features) || Feature <- FeatureList]
    ].

-spec format_feature_row(binary(), map()) -> iodata().
format_feature_row(Feature, Features) ->
    Value = maps:get(Feature, Features, false),
    Status = case Value of
        true -> "✓";
        false -> "✗";
        _ -> erlang:atom_to_list(Value)
    end,
    ["| ", binary_to_list(Feature), " | ", Status, " |\n"].

%% @doc Format refusal behavior as markdown table
-spec format_refusal_table(map()) -> iodata().
format_refusal_table(Refusal) ->
    Keys = maps:keys(Refusal),
    [
        "| Scenario | HTTP Status | Error Code | Message |\n",
        "|----------|-------------|-----------|----------|\n",
        [format_refusal_row(Key, Refusal) || Key <- Keys]
    ].

-spec format_refusal_row(atom() | binary(), map()) -> iodata().
format_refusal_row(Key, Refusal) ->
    Behavior = maps:get(Key, Refusal),
    Status = maps:get(<<"http_status">>, Behavior, "N/A"),
    Code = maps:get(<<"error_code">>, Behavior, ""),
    Message = maps:get(<<"message">>, Behavior, ""),
    [
        "| ", format_scenario_name(Key), " | ", format_value(Status),
        " | ", format_value(Code), " | ", format_value(Message), " |\n"
    ].

-spec format_scenario_name(atom() | binary()) -> string().
format_scenario_name(Name) when is_binary(Name) ->
    string:replace(binary_to_list(Name), "_", " ");
format_scenario_name(Name) when is_atom(Name) ->
    string:replace(atom_to_list(Name), "_", " ").

-spec format_value(term()) -> string().
format_value(V) when is_binary(V) ->
    binary_to_list(V);
format_value(V) when is_atom(V) ->
    atom_to_list(V);
format_value(V) when is_integer(V) ->
    integer_to_list(V);
format_value(V) when is_list(V) ->
    V;
format_value(_) ->
    "N/A".

-spec format_throughput(plan_spec()) -> string().
format_throughput(Spec) ->
    Envelope = maps:get(<<"envelope">>, Spec),
    integer_to_list(maps:get(<<"throughput_req_s">>, Envelope)).

-spec format_connections(plan_spec()) -> string().
format_connections(Spec) ->
    Envelope = maps:get(<<"envelope">>, Spec),
    integer_to_list(maps:get(<<"concurrent_connections">>, Envelope)).

-spec format_latency(plan_spec()) -> string().
format_latency(Spec) ->
    Envelope = maps:get(<<"envelope">>, Spec),
    integer_to_list(maps:get(<<"p99_latency_ms">>, Envelope)).

-spec doc_path(tier()) -> file:name().
doc_path(Tier) ->
    TierStr = atom_to_list(Tier),
    filename:join([
        filename:dirname(code:priv_dir(erlmcp)),
        "docs",
        "plans",
        TierStr ++ "-tier.md"
    ]).
