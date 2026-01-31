-module(erlmcp_compliance_report).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    generate_report/2,
    calculate_compliance/1,
    format_text/1,
    format_markdown/1,
    format_json/1,
    format_html/1,
    create_traceability_matrix/1,
    identify_gaps/1,
    get_report_summary/1,
    %% Evidence collection API
    collect_evidence/2,
    store_evidence_bundle/2,
    generate_evidence_report/1,
    hash_evidence/1,
    verify_evidence_integrity/2,
    create_evidence_bundle/1,
    link_receipt_chain/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Type definitions
-type report_format() :: json | markdown | html.
-type compliance_data() :: #{
    spec_version => binary(),
    test_results => [map()],
    spec_requirements => [map()],
    timestamp => binary()
}.
-type compliance_report() :: #{
    overall => float(),
    by_section => map(),
    evidence => [map()],
    gaps => [map()],
    recommendations => [binary()],
    traceability => map()
}.
-type gap_analysis() :: #{
    requirement => map(),
    status => missing | incomplete | failed,
    severity => critical | high | medium | low,
    recommendation => binary()
}.

%% Evidence types
-type evidence_type() :: test_result | coverage_metrics | security_scan |
                         performance_benchmark | compliance_validation.
-type evidence() :: #{
    evidence_id => binary(),
    evidence_type => binary(),
    content => map(),
    hash => binary(),
    timestamp => binary()
}.
-type evidence_bundle() :: #{
    bundle_id => binary(),
    evidence_count => integer(),
    timestamp => binary(),
    evidence_items => [evidence()]
}.

-record(state, {
    reports :: #{binary() => compliance_report()},
    specs :: #{binary() => map()}
}).

%%%====================================================================
%%% API
%%%====================================================================

%% @doc Start the compliance report server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Generate a compliance report in the specified format
%% This function works without requiring the gen_server to be started
-spec generate_report(report_format(), compliance_data()) -> {ok, binary()} | {error, term()}.
generate_report(Format, Data) when is_map(Data) ->
    try
        %% Try gen_server first if it's running
        case whereis(?MODULE) of
            Pid when is_pid(Pid) ->
                gen_server:call(?MODULE, {generate_report, Format, Data});
            undefined ->
                %% If gen_server not running, do direct generation
                generate_report_direct(Format, Data)
        end
    catch
        exit:{noproc, _} ->
            %% Fallback to direct generation if gen_server call fails
            generate_report_direct(Format, Data);
        _:Error ->
            {error, {report_generation_failed, Error}}
    end.

%% @doc Calculate overall compliance percentage
-spec calculate_compliance(compliance_data()) -> {ok, float(), map()}.
calculate_compliance(Data) when is_map(Data) ->
    try
        TestResults = maps:get(test_results, Data, []),
        Requirements = maps:get(spec_requirements, Data, []),

        TotalReqs = length(Requirements),
        PassedTests = count_passed_tests(TestResults),

        %% Calculate compliance as percentage of requirements met
        Compliance = case TotalReqs of
            0 -> 0.0;
            _ -> (PassedTests / TotalReqs) * 100.0
        end,

        %% Calculate compliance by section
        BySection = calculate_section_compliance(TestResults, Requirements),

        {ok, Compliance, #{total_requirements => TotalReqs,
                          passed_tests => PassedTests,
                          by_section => BySection}}
    catch
        _:Error -> {error, {calculation_failed, Error}}
    end.

%% @doc Format report as Markdown
-spec format_markdown(compliance_report()) -> binary().
format_markdown(Report) when is_map(Report) ->
    Markdown = [
        "# MCP Specification Compliance Report\n\n",
        format_markdown_header(maps:get(timestamp, Report, <<"Unknown">>)),
        format_markdown_summary(Report),
        format_markdown_sections(Report),
        format_markdown_evidence(Report),
        format_markdown_gaps(Report),
        format_markdown_recommendations(Report),
        format_markdown_traceability(Report)
    ],
    iolist_to_binary(Markdown).

%% @doc Format report as JSON
-spec format_json(compliance_report()) -> binary().
format_json(Report) when is_map(Report) ->
    jsx:encode(Report).

%% @doc Format report as HTML
-spec format_html(compliance_report()) -> binary().
format_html(Report) when is_map(Report) ->
    HTML = [
        "<!DOCTYPE html>\n",
        "<html lang=\"en\">\n",
        "<head>\n",
        "    <meta charset=\"UTF-8\">\n",
        "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n",
        "    <title>MCP Compliance Report</title>\n",
        "    <style>\n",
        "        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }\n",
        "        .container { max-width: 1200px; margin: 0 auto; padding: 20px; }\n",
        "        .header { background: #2c3e50; color: white; padding: 20px; border-radius: 8px; }\n",
        "        .summary { background: #ecf0f1; padding: 20px; border-radius: 8px; margin: 20px 0; }\n",
        "        .section { margin: 30px 0; }\n",
        "        .compliance-high { color: #27ae60; font-weight: bold; }\n",
        "        .compliance-medium { color: #f39c12; font-weight: bold; }\n",
        "        .compliance-low { color: #e74c3c; font-weight: bold; }\n",
        "        .evidence { background: #f8f9fa; border-left: 4px solid #3498db; padding: 15px; margin: 10px 0; }\n",
        "        .gap { background: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0; }\n",
        "        table { width: 100%; border-collapse: collapse; margin: 20px 0; }\n",
        "        th, td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }\n",
        "        th { background: #34495e; color: white; }\n",
        "        .passed { color: #27ae60; }\n",
        "        .failed { color: #e74c3c; }\n",
        "    </style>\n",
        "</head>\n",
        "<body>\n",
        "    <div class=\"container\">\n",
        format_html_header(Report),
        format_html_summary(Report),
        format_html_sections(Report),
        format_html_evidence(Report),
        format_html_gaps(Report),
        format_html_recommendations(Report),
        format_html_traceability(Report),
        "    </div>\n",
        "</body>\n",
        "</html>"
    ],
    iolist_to_binary(HTML).

%% @doc Create traceability matrix mapping spec requirements to tests
-spec create_traceability_matrix(compliance_data()) -> map().
create_traceability_matrix(Data) when is_map(Data) ->
    Requirements = maps:get(spec_requirements, Data, []),
    TestResults = maps:get(test_results, Data, []),

    lists:foldl(fun(Req, Acc) ->
        ReqId = maps:get(id, Req),
        Section = maps:get(section, Req, <<"Unknown">>),
        ReqName = maps:get(name, Req),

        %% Find tests that validate this requirement
        RelatedTests = find_related_tests(ReqId, ReqName, TestResults),

        maps:put(ReqId, #{
            <<"requirement">> => ReqName,
            <<"section">> => Section,
            <<"tests">> => [maps:get(name, T) || T <- RelatedTests],
            <<"status">> => determine_requirement_status(RelatedTests),
            <<"last_tested">> => get_latest_test_date(RelatedTests)
        }, Acc)
    end, #{}, Requirements).

%% @doc Identify gaps in compliance
-spec identify_gaps(compliance_data()) -> [gap_analysis()].
identify_gaps(Data) when is_map(Data) ->
    Requirements = maps:get(spec_requirements, Data, []),
    TestResults = maps:get(test_results, Data, []),

    lists:foldl(fun(Req, Acc) ->
        ReqId = maps:get(id, Req),
        ReqName = maps:get(name, Req),
        Section = maps:get(section, Req, <<"Unknown">>),

        RelatedTests = find_related_tests(ReqId, ReqName, TestResults),

        case RelatedTests of
            [] ->
                %% No tests for this requirement
                [#{
                    <<"requirement">> => Req,
                    <<"status">> => missing,
                    <<"severity">> => determine_severity(Section),
                    <<"recommendation">> => <<"Create tests to validate this requirement">>
                } | Acc];
            Tests ->
                %% Check if all tests passed
                FailedTests = [T || T <- Tests,
                                  maps:get(status, T, <<"unknown">>) =/= <<"passed">>],

                case FailedTests of
                    [] -> Acc; %% All tests passed
                    _ ->
                        [#{
                            <<"requirement">> => Req,
                            <<"status">> => failed,
                            <<"severity">> => high,
                            <<"recommendation">> => <<"Fix failing tests for this requirement">>
                        } | Acc]
                end
        end
    end, [], Requirements).

%% @doc Get a summary of the compliance report
-spec get_report_summary(compliance_report()) -> map().
get_report_summary(Report) when is_map(Report) ->
    #{
        overall_compliance => maps:get(overall, Report),
        total_sections => maps:size(maps:get(by_section, Report, #{})),
        total_evidence => length(maps:get(evidence, Report, [])),
        total_gaps => length(maps:get(gaps, Report, [])),
        timestamp => maps:get(timestamp, Report)
    }.

%%%====================================================================
%%% Evidence Collection API
%%%====================================================================

%% @doc Collect evidence for a specific validation type
-spec collect_evidence(evidence_type(), map()) -> {ok, evidence()} | {error, term()}.
collect_evidence(EvidenceType, Data) when is_map(Data) ->
    try
        %% Validate evidence type
        ValidTypes = [test_result, coverage_metrics, security_scan,
                      performance_benchmark, compliance_validation],
        case lists:member(EvidenceType, ValidTypes) of
            false ->
                {error, {unknown_evidence_type, EvidenceType}};
            true ->
                %% Generate unique evidence ID
                EvidenceId = generate_evidence_id(EvidenceType),

                %% Extract content based on type
                Content = extract_evidence_content(EvidenceType, Data),

                %% Generate hash of content
                {ok, Hash} = hash_evidence(Content),

                %% Get timestamp
                Timestamp = maps:get(timestamp, Data, iso8601_timestamp()),

                %% Build evidence record
                Evidence = #{
                    evidence_id => EvidenceId,
                    evidence_type => atom_to_binary(EvidenceType, utf8),
                    content => Content,
                    hash => Hash,
                    timestamp => Timestamp
                },

                {ok, Evidence}
        end
    catch
        _:Error -> {error, {evidence_collection_failed, Error}}
    end.

%% @doc Store evidence bundle to filesystem with SHA-256 hashes
-spec store_evidence_bundle(file:filename(), [evidence()]) -> {ok, file:filename()} | {error, term()}.
store_evidence_bundle(BundlePath, EvidenceItems) when is_list(EvidenceItems) ->
    try
        %% Ensure bundle directory exists
        ok = filelib:ensure_dir(BundlePath),

        %% Create evidence subdirectory
        EvidenceDir = filename:join([BundlePath, "evidence"]),
        ok = filelib:ensure_dir(filename:join([EvidenceDir, ".gitkeep"])),

        %% Create metadata subdirectory
        MetadataDir = filename:join([BundlePath, "metadata"]),
        ok = filelib:ensure_dir(filename:join([MetadataDir, ".gitkeep"])),

        %% Store each evidence item as separate JSON file
        lists:foreach(fun(Evidence) ->
            EvidenceId = maps:get(evidence_id, Evidence),
            Filename = binary_to_list(EvidenceId) ++ ".json",
            FilePath = filename:join([EvidenceDir, Filename]),

            %% Encode evidence as JSON
            JSON = jsx:encode(Evidence, [space]),

            %% Write to file
            ok = file:write_file(FilePath, JSON)
        end, EvidenceItems),

        %% Create bundle manifest
        Manifest = #{
            bundle_id => list_to_binary(filename:basename(BundlePath)),
            evidence_count => length(EvidenceItems),
            timestamp => iso8601_timestamp(),
            evidence_types => lists:usort([maps:get(evidence_type, E) || E <- EvidenceItems])
        },

        ManifestPath = filename:join([BundlePath, "bundle_manifest.json"]),
        ManifestJSON = jsx:encode(Manifest, [space]),
        ok = file:write_file(ManifestPath, ManifestJSON),

        {ok, BundlePath}
    catch
        _:Error -> {error, {bundle_storage_failed, Error}}
    end.

%% @doc Generate evidence report with all evidence
-spec generate_evidence_report([evidence()]) -> map().
generate_evidence_report(EvidenceItems) when is_list(EvidenceItems) ->
    #{
        report_id => generate_evidence_id(report),
        timestamp => iso8601_timestamp(),
        evidence_count => length(EvidenceItems),
        evidence_items => EvidenceItems,
        summary => generate_evidence_summary(EvidenceItems)
    }.

%% @doc Generate SHA-256 hash of evidence content
-spec hash_evidence(map() | binary()) -> {ok, binary()} | {error, term()}.
hash_evidence(Content) when is_map(Content) orelse is_binary(Content) ->
    try
        %% Convert content to binary for hashing
        Binary = case Content of
            _ when is_map(Content) -> jsx:encode(Content);
            _ when is_binary(Content) -> Content
        end,

        %% Generate SHA-256 hash using crypto
        Hash = crypto:hash(sha256, Binary),

        %% Convert to hex string
        HashHex = binary:encode_hex(Hash),

        {ok, HashHex}
    catch
        _:Error -> {error, {hashing_failed, Error}}
    end.

%% @doc Verify evidence hasn't been tampered
-spec verify_evidence_integrity(map() | binary(), binary()) -> {ok, boolean()}.
verify_evidence_integrity(Content, ExpectedHash) ->
    case hash_evidence(Content) of
        {ok, ExpectedHash} -> {ok, true};
        {ok, _OtherHash} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Create evidence bundle directory structure
-spec create_evidence_bundle(file:filename()) -> {ok, file:filename()} | {error, term()}.
create_evidence_bundle(BasePath) ->
    try
        %% Create main bundle directory
        ok = filelib:ensure_dir(BasePath),

        %% Create evidence subdirectory
        EvidenceDir = filename:join([BasePath, "evidence"]),
        ok = filelib:ensure_dir(filename:join([EvidenceDir, ".gitkeep"])),

        %% Create metadata subdirectory
        MetadataDir = filename:join([BasePath, "metadata"]),
        ok = filelib:ensure_dir(filename:join([MetadataDir, ".gitkeep"])),

        %% Create empty manifest
        EmptyManifest = #{
            bundle_id => list_to_binary(filename:basename(BasePath)),
            evidence_count => 0,
            timestamp => iso8601_timestamp(),
            status => <<"initialized">>
        },

        ManifestPath = filename:join([BasePath, "bundle_manifest.json"]),
        ManifestJSON = jsx:encode(EmptyManifest, [space]),
        ok = file:write_file(ManifestPath, ManifestJSON),

        {ok, BasePath}
    catch
        _:Error -> {error, {bundle_creation_failed, Error}}
    end.

%% @doc Link evidence to receipt chain
-spec link_receipt_chain(file:filename(), map()) -> {ok, file:filename()} | {error, term()}.
link_receipt_chain(BundlePath, ReceiptChain) when is_map(ReceiptChain) ->
    try
        %% Ensure bundle directory exists
        ok = filelib:ensure_dir(BundlePath),

        %% Create receipt chain file
        ReceiptPath = filename:join([BundlePath, "receipt_chain.json"]),

        %% Add timestamp to receipt chain
        ReceiptWithTimestamp = maps:put(<<"linked_at">>, iso8601_timestamp(), ReceiptChain),

        %% Encode as JSON
        ReceiptJSON = jsx:encode(ReceiptWithTimestamp, [space]),

        %% Write to file
        ok = file:write_file(ReceiptPath, ReceiptJSON),

        {ok, ReceiptPath}
    catch
        _:Error -> {error, {receipt_linking_failed, Error}}
    end.

%% @private Generate report directly without gen_server
generate_report_direct(Format, Data) ->
    try
        %% Calculate compliance
        {ok, Compliance, Details} = calculate_compliance(Data),

        %% Create traceability matrix
        Traceability = create_traceability_matrix(Data),

        %% Identify gaps
        Gaps = identify_gaps(Data),

        %% Build report
        Report = #{
            spec_version => maps:get(spec_version, Data, <<"unknown">>),
            timestamp => maps:get(timestamp, Data, iso8601_timestamp()),
            status => determine_status(Compliance),
            overall => Compliance,
            by_section => maps:get(by_section, Details),
            evidence => extract_evidence(Data),
            gaps => Gaps,
            recommendations => generate_recommendations(Gaps, Compliance),
            traceability => Traceability,
            details => Details
        },

        %% Format report
        Formatted = case Format of
            text -> format_text(Report);
            markdown -> format_markdown(Report);
            json -> format_json(Report);
            html -> format_html(Report)
        end,

        {ok, Formatted}
    catch
        _:Error -> {error, {report_generation_failed, Error}}
    end.

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

init([]) ->
    {ok, #state{reports = #{}, specs = #{}}}.

handle_call({generate_report, Format, Data}, _From, State) ->
    try
        %% Calculate compliance
        {ok, Compliance, Details} = calculate_compliance(Data),

        %% Create traceability matrix
        Traceability = create_traceability_matrix(Data),

        %% Identify gaps
        Gaps = identify_gaps(Data),

        %% Build report
        Report = #{
            spec_version => maps:get(spec_version, Data, <<"unknown">>),
            timestamp => maps:get(timestamp, Data, iso8601_timestamp()),
            status => determine_status(Compliance),
            overall => Compliance,
            by_section => maps:get(by_section, Details),
            evidence => extract_evidence(Data),
            gaps => Gaps,
            recommendations => generate_recommendations(Gaps, Compliance),
            traceability => Traceability,
            details => Details
        },

        %% Format report
        Formatted = case Format of
            text -> format_text(Report);
            markdown -> format_markdown(Report);
            json -> format_json(Report);
            html -> format_html(Report)
        end,

        %% Cache report
        ReportId = maps:get(spec_version, Data, <<"latest">>),
        NewState = State#state{reports = maps:put(ReportId, Report, State#state.reports)},

        {reply, {ok, Formatted}, NewState}
    catch
        _:Error ->
            {reply, {error, {report_generation_failed, Error}}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal functions
%%%====================================================================

%% @private Count passed tests
count_passed_tests(TestResults) ->
    length([T || T <- TestResults,
                maps:get(status, T, <<"unknown">>) =:= <<"passed">>]).

%% @private Calculate compliance by section
calculate_section_compliance(TestResults, Requirements) ->
    GroupedReqs = lists:foldl(fun(Req, Acc) ->
        Section = maps:get(section, Req, <<"Unknown">>),
        maps:update_with(Section, fun(V) -> [Req | V] end, [Req], Acc)
    end, #{}, Requirements),

    maps:map(fun(Section, Reqs) ->
        ReqIds = [maps:get(id, R) || R <- Reqs],
        RelatedTests = [T || T <- TestResults,
                           lists:any(fun(Id) ->
                               maps:get(requirement_id, T, undefined) =:= Id
                           end, ReqIds)],
        Passed = length([T || T <- RelatedTests,
                            maps:get(status, T, <<"unknown">>) =:= <<"passed">>]),
        case length(Reqs) of
            0 -> 0.0;
            Total -> (Passed / Total) * 100.0
        end
    end, GroupedReqs).

%% @private Find tests related to a requirement
find_related_tests(ReqId, ReqName, TestResults) ->
    [T || T <- TestResults,
         (maps:get(requirement_id, T, undefined) =:= ReqId orelse
          maps:get(requirement_name, T, undefined) =:= ReqName orelse
          string:find(maps:get(name, T, <<>>), ReqName) =/= nomatch)].

%% @private Determine requirement status from tests
determine_requirement_status([]) ->
    <<"untested">>;
determine_requirement_status(Tests) ->
    case [T || T <- Tests, maps:get(status, T, <<"unknown">>) =:= <<"passed">>] of
        [] -> <<"failed">>;
        Passed when length(Tests) =:= length(Passed) -> <<"passed">>;
        _ -> <<"partial">>
    end.

%% @private Get latest test date
get_latest_test_date([]) ->
    <<"never">>;
get_latest_test_date(Tests) ->
    Dates = [maps:get(timestamp, T, <<"">>) || T <- Tests,
             maps:is_key(timestamp, T)],
    case Dates of
        [] -> <<"unknown">>;
        _ -> lists:max(Dates)
    end.

%% @private Determine severity based on section
determine_severity(<<"Lifecycle">>) -> <<"critical">>;
determine_severity(<<"Tools">>) -> <<"critical">>;
determine_severity(<<"Resources">>) -> <<"high">>;
determine_severity(<<"Prompts">>) -> <<"high">>;
determine_severity(<<"Transports">>) -> <<"critical">>;
determine_severity(_) -> <<"medium">>.

%% @private Determine status based on compliance percentage
determine_status(Compliance) when Compliance >= 80.0 -> <<"passed">>;
determine_status(Compliance) when Compliance >= 50.0 -> <<"warning">>;
determine_status(_Compliance) -> <<"failed">>.

%% @private Extract evidence from test results
extract_evidence(Data) ->
    TestResults = maps:get(test_results, Data, []),
    [begin
        #{
            <<"test">> => maps:get(name, T),
            <<"requirement">> => maps:get(requirement_name, T, <<"unknown">>),
            <<"status">> => maps:get(status, T, <<"unknown">>),
            <<"evidence">> => maps:get(evidence, T, <<"No evidence provided">>),
            <<"timestamp">> => maps:get(timestamp, T, <<"unknown">>)
        }
    end || T <- TestResults, maps:get(status, T, <<"unknown">>) =:= <<"passed">>].

%% @private Generate recommendations based on gaps
generate_recommendations(Gaps, Compliance) ->
    Recs1 = case Compliance of
        C when C < 50.0 ->
            [<<"Critical: Compliance below 50%. Immediate action required.">>];
        C when C < 80.0 ->
            [<<"Warning: Compliance below 80%. Review and fix gaps.">>];
        _ ->
            [<<"Good: Compliance above 80%. Continue monitoring.">>]
    end,

    Recs2 = lists:map(fun(Gap) ->
        Status = maps:get(<<"status">>, Gap, <<"unknown">>),
        case Status of
            <<"missing">> ->
                <<"Add test coverage for missing requirements">>;
            <<"failed">> ->
                <<"Fix failing tests">>;
            <<"incomplete">> ->
                <<"Complete partial implementations">>;
            _ ->
                <<"Review this requirement">>
        end
    end, Gaps),

    lists:usort(Recs1 ++ Recs2).

%% @private Format report as plain text
format_text(Report) when is_map(Report) ->
    Text = [
        "================================================================================\n",
        "MCP COMPLIANCE REPORT\n",
        "================================================================================\n\n",
        format_text_summary(Report),
        format_text_sections(Report),
        format_text_evidence(Report),
        format_text_gaps(Report),
        format_text_recommendations(Report),
        "\n================================================================================\n"
    ],
    iolist_to_binary(Text).

%% @private Format text summary
format_text_summary(Report) ->
    Overall = maps:get(overall, Report, 0.0),
    BySection = maps:get(by_section, Report, #{}),
    [
        "SUMMARY\n",
        "-------\n",
        io_lib:format("Overall Compliance: ~.2f%~n~n", [Overall]),
        "By Section:\n",
        lists:map(fun({Section, Compliance}) ->
            io_lib:format("  ~s: ~.2f%~n", [Section, Compliance])
        end, maps:to_list(BySection)),
        "\n"
    ].

%% @private Format text sections
format_text_sections(Report) ->
    BySection = maps:get(by_section, Report, #{}),
    [
        "COMPLIANCE BY SECTION\n",
        "---------------------\n",
        lists:map(fun({Section, Compliance}) ->
            io_lib:format("~s\n  Compliance: ~.2f%~n~n", [Section, Compliance])
        end, maps:to_list(BySection))
    ].

%% @private Format text evidence
format_text_evidence(Report) ->
    Evidence = maps:get(evidence, Report, []),
    [
        "EVIDENCE\n",
        "--------\n",
        lists:map(fun(E) ->
            Test = maps:get(<<"test">>, E, <<"unknown">>),
            Status = maps:get(<<"status">>, E, <<"unknown">>),
            Ev = maps:get(<<"evidence">>, E, <<"No evidence">>),
            [
                io_lib:format("~s~n", [Test]),
                io_lib:format("  Status: ~s~n", [Status]),
                io_lib:format("  Evidence: ~s~n~n", [Ev])
            ]
        end, Evidence)
    ].

%% @private Format text gaps
format_text_gaps(Report) ->
    Gaps = maps:get(gaps, Report, []),
    case Gaps of
        [] -> ["NO GAPS IDENTIFIED\n\n"];
        _ ->
            [
                "GAPS\n",
                "----\n",
                lists:map(fun(Gap) ->
                    Req = maps:get(<<"requirement">>, Gap, #{}),
                    Status = maps:get(<<"status">>, Gap, <<"unknown">>),
                    Rec = maps:get(<<"recommendation">>, Gap, <<"No recommendation">>),
                    ReqName = maps:get(name, Req, <<"unknown">>),
                    [
                        io_lib:format("~s [~s]~n", [ReqName, Status]),
                        io_lib:format("  Recommendation: ~s~n~n", [Rec])
                    ]
                end, Gaps)
            ]
    end.

%% @private Format text recommendations
format_text_recommendations(Report) ->
    Recs = maps:get(recommendations, Report, []),
    [
        "RECOMMENDATIONS\n",
        "---------------\n",
        lists:map(fun(Rec) ->
            io_lib:format("  - ~s~n", [Rec])
        end, Recs),
        "\n"
    ].

%% @private Format Markdown header
format_markdown_header(Timestamp) ->
    io_lib:format("**Generated**: ~s\n**Specification**: MCP 2025-11-25\n**Validation Tool**: erlmcp-validation v1.0.0\n\n",
                  [Timestamp]).

%% @private Format Markdown summary
format_markdown_summary(Report) ->
    Overall = maps:get(overall, Report, 0.0),
    BySection = maps:get(by_section, Report, #{}),

    SummaryTable = [
        "## Summary\n\n",
        "| Section | Requirements | Tested | Passed | Compliance |\n",
        "|---------|--------------|--------|--------|------------|\n"
    ],

    Rows = maps:fold(fun(Section, Compliance, Acc) ->
        [io_lib:format("| ~s | - | - | - | ~.2f% |\n", [Section, Compliance]) | Acc]
    end, [], BySection),

    OverallLine = [io_lib:format("\n**Overall Compliance**: ~.2f%\n\n", [Overall])],

    SummaryTable ++ lists:reverse(Rows) ++ OverallLine.

%% @private Format Markdown sections
format_markdown_sections(Report) ->
    ["## Compliance by Section\n\n",
     format_section_details(maps:get(by_section, Report, #{})),
     "\n"].

format_section_details(Sections) ->
    maps:fold(fun(_Section, Compliance, Acc) ->
        [io_lib:format("### ~s\n\nCompliance: ~.2f%\n\n", [_Section, Compliance]) | Acc]
    end, [], Sections).

%% @private Format Markdown evidence
format_markdown_evidence(Report) ->
    Evidence = maps:get(evidence, Report, []),
    ["## Detailed Evidence\n\n",
     lists:map(fun(E) ->
         Test = maps:get(<<"test">>, E),
         Status = maps:get(<<"status">>, E),
         Ev = maps:get(<<"evidence">>, E),
         io_lib:format("### ~s\n\nStatus: ~s\n\nEvidence:\n```\n~s\n```\n\n",
                      [Test, Status, Ev])
     end, Evidence),
     "\n"].

%% @private Format Markdown gaps
format_markdown_gaps(Report) ->
    Gaps = maps:get(gaps, Report, []),
    case Gaps of
        [] ->
            ["## Gap Analysis\n\nNo gaps identified. All requirements tested.\n\n"];
        _ ->
            ["## Gap Analysis\n\n",
             "| Requirement | Status | Severity | Recommendation |\n",
             "|-------------|--------|----------|----------------|\n",
             lists:map(fun(G) ->
                 Req = maps:get(<<"requirement">>, G, #{}),
                 ReqName = maps:get(name, Req, <<"unknown">>),
                 Status = maps:get(<<"status">>, G),
                 Severity = maps:get(<<"severity">>, G),
                 Rec = maps:get(<<"recommendation">>, G),
                 io_lib:format("| ~s | ~s | ~s | ~s |\n",
                              [ReqName, Status, Severity, Rec])
             end, Gaps),
             "\n"]
    end.

%% @private Format Markdown recommendations
format_markdown_recommendations(Report) ->
    Recs = maps:get(recommendations, Report, []),
    ["## Recommendations\n\n",
     lists:map(fun(R) ->
         io_lib:format("- ~s\n", [R])
     end, Recs),
     "\n"].

%% @private Format Markdown traceability
format_markdown_traceability(Report) ->
    Traceability = maps:get(traceability, Report, #{}),
    ["## Traceability Matrix\n\n",
     "| Spec Requirement | Test | Status | Last Tested |\n",
     "|------------------|------|--------|-------------|\n",
     maps:fold(fun(_ReqId, ReqData, Acc) ->
         ReqName = maps:get(<<"requirement">>, ReqData),
         Tests = maps:get(<<"tests">>, ReqData, []),
         Status = maps:get(<<"status">>, ReqData),
         LastTested = maps:get(<<"last_tested">>, ReqData),
         TestsStr = string:join([binary_to_list(T) || T <- Tests], ", "),
         [io_lib:format("| ~s | ~s | ~s | ~s |\n",
                        [ReqName, TestsStr, Status, LastTested]) | Acc]
     end, [], Traceability),
     "\n"].

%% @private Format HTML header
format_html_header(Report) ->
    Timestamp = maps:get(timestamp, Report, <<"Unknown">>),
    Overall = maps:get(overall, Report, 0.0),

    ComplianceClass = case Overall of
        Score when Score >= 80.0 -> "compliance-high";
        Score when Score >= 50.0 -> "compliance-medium";
        _ -> "compliance-low"
    end,

    [
        "        <div class=\"header\">\n",
        "            <h1>MCP Specification Compliance Report</h1>\n",
        io_lib:format("            <p>Generated: ~s</p>\n", [Timestamp]),
        "            <p>Specification: MCP 2025-11-25</p>\n",
        io_lib:format("            <p>Overall Compliance: <span class=\"~s\">~.2f%</span></p>\n",
                     [ComplianceClass, Overall]),
        "        </div>\n"
    ].

%% @private Format HTML summary
format_html_summary(Report) ->
    [
        "        <div class=\"summary\">\n",
        "            <h2>Summary</h2>\n",
        format_html_table(Report),
        "        </div>\n"
    ].

format_html_table(Report) ->
    BySection = maps:get(by_section, Report, #{}),
    [
        "            <table>\n",
        "                <thead>\n",
        "                    <tr>\n",
        "                        <th>Section</th>\n",
        "                        <th>Compliance</th>\n",
        "                        <th>Status</th>\n",
        "                    </tr>\n",
        "                </thead>\n",
        "                <tbody>\n",
        maps:fold(fun(Section, Compliance, Acc) ->
            Class = case Compliance of
                Score when Score >= 80.0 -> "compliance-high";
                Score when Score >= 50.0 -> "compliance-medium";
                _ -> "compliance-low"
            end,
            [io_lib:format("                    <tr>\n",
                         []),
             io_lib:format("                        <td>~s</td>\n", [Section]),
             io_lib:format("                        <td class=\"~s\">~.2f%</td>\n", [Class, Compliance]),
             io_lib:format("                        <td>~s</td>\n",
                          [case Compliance of
                               PassScore when PassScore >= 80.0 -> "&#10003; Passed";
                               PartialScore when PartialScore >= 50.0 -> "&#9888; Partial";
                               _ -> "&#10007; Failed"
                           end]),
             io_lib:format("                    </tr>\n", []),
             Acc]
        end, [], BySection),
        "                </tbody>\n",
        "            </table>\n"
    ].

%% @private Format HTML sections
format_html_sections(_Report) ->
    [].

%% @private Format HTML evidence
format_html_evidence(Report) ->
    Evidence = maps:get(evidence, Report, []),
    case Evidence of
        [] -> [];
        _ ->
            [
                "        <div class=\"section\">\n",
                "            <h2>Detailed Evidence</h2>\n",
                lists:map(fun(E) ->
                    Test = maps:get(<<"test">>, E),
                    Status = maps:get(<<"status">>, E),
                    Ev = maps:get(<<"evidence">>, E),
                    [
                        "            <div class=\"evidence\">\n",
                        io_lib:format("                <h3>~s</h3>\n", [Test]),
                        io_lib:format("                <p><strong>Status:</strong> ~s</p>\n", [Status]),
                        "                <pre><code>",
                        Ev,
                        "</code></pre>\n",
                        "            </div>\n"
                    ]
                end, Evidence),
                "        </div>\n"
            ]
    end.

%% @private Format HTML gaps
format_html_gaps(Report) ->
    Gaps = maps:get(gaps, Report, []),
    case Gaps of
        [] -> [];
        _ ->
            [
                "        <div class=\"section\">\n",
                "            <h2>Gap Analysis</h2>\n",
                lists:map(fun(G) ->
                    Req = maps:get(requirement, G, #{}),
                    ReqName = maps:get(name, Req, <<"unknown">>),
                    Status = maps:get(status, G),
                    Severity = maps:get(severity, G),
                    Rec = maps:get(recommendation, G),
                    [
                        "            <div class=\"gap\">\n",
                        io_lib:format("                <h3>~s</h3>\n", [ReqName]),
                        io_lib:format("                <p><strong>Status:</strong> ~s</p>\n", [Status]),
                        io_lib:format("                <p><strong>Severity:</strong> ~s</p>\n", [Severity]),
                        io_lib:format("                <p><strong>Recommendation:</strong> ~s</p>\n", [Rec]),
                        "            </div>\n"
                    ]
                end, Gaps),
                "        </div>\n"
            ]
    end.

%% @private Format HTML recommendations
format_html_recommendations(Report) ->
    Recs = maps:get(recommendations, Report, []),
    case Recs of
        [] -> [];
        _ ->
            [
                "        <div class=\"section\">\n",
                "            <h2>Recommendations</h2>\n",
                "            <ul>\n",
                lists:map(fun(R) ->
                    io_lib:format("                <li>~s</li>\n", [R])
                end, Recs),
                "            </ul>\n",
                "        </div>\n"
            ]
    end.

%% @private Format HTML traceability
format_html_traceability(_Report) ->
    [
        "        <div class=\"section\">\n",
        "            <h2>Traceability Matrix</h2>\n",
        "            <p>Full traceability matrix available in JSON format.</p>\n",
        "        </div>\n"
    ].

%% @private Generate ISO 8601 timestamp
iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                   [Year, Month, Day, Hour, Min, Sec])).

%%%====================================================================
%%% Evidence Collection Internal Functions
%%%====================================================================

%% @private Generate unique evidence ID
generate_evidence_id(Type) when is_atom(Type) ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(16#ffffffff),
    TypeBin = atom_to_binary(Type, utf8),
    <<TypeBin/binary, "_", (integer_to_binary(Timestamp))/binary, "_",
      (integer_to_binary(Random, 16))/binary>>.

%% @private Extract evidence content based on type
extract_evidence_content(test_result, Data) ->
    #{
        module => maps:get(module, Data, unknown),
        function => maps:get(function, Data, unknown),
        status => maps:get(status, Data, unknown),
        runtime_ms => maps:get(runtime_ms, Data, 0)
    };
extract_evidence_content(coverage_metrics, Data) ->
    #{
        module => maps:get(module, Data, unknown),
        coverage_percentage => maps:get(coverage_percentage, Data, 0.0),
        lines_covered => maps:get(lines_covered, Data, 0),
        lines_total => maps:get(lines_total, Data, 0)
    };
extract_evidence_content(security_scan, Data) ->
    #{
        scanner => maps:get(scanner, Data, unknown),
        issues_found => maps:get(issues_found, Data, 0),
        scan_report => maps:get(scan_report, Data, #{})
    };
extract_evidence_content(performance_benchmark, Data) ->
    #{
        benchmark_name => maps:get(benchmark_name, Data, unknown),
        throughput_ops_per_sec => maps:get(throughput_ops_per_sec, Data, 0),
        latency_p50_us => maps:get(latency_p50_us, Data, 0),
        latency_p95_us => maps:get(latency_p95_us, Data, 0),
        latency_p99_us => maps:get(latency_p99_us, Data, 0)
    };
extract_evidence_content(compliance_validation, Data) ->
    #{
        spec_version => maps:get(spec_version, Data, "unknown"),
        overall_compliance => maps:get(overall_compliance, Data, 0.0),
        requirements_checked => maps:get(requirements_checked, Data, 0),
        requirements_passed => maps:get(requirements_passed, Data, 0)
    }.

%% @private Generate evidence summary
generate_evidence_summary(EvidenceItems) ->
    TypeCounts = lists:foldl(fun(Evidence, Acc) ->
        Type = maps:get(evidence_type, Evidence),
        maps:update_with(Type, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, EvidenceItems),

    #{
        total_evidence => length(EvidenceItems),
        by_type => TypeCounts
    }.
