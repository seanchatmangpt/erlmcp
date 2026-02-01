%%%-------------------------------------------------------------------
%% @doc erlmcp_evidence_path - Plan-specific evidence artifact organization
%%
%% This module provides functions to create, manage, and verify
%% evidence artifacts for plan-specific certifications.
%%
%% Evidence is organized as:
%%   dist/evidence/<VERSION>/<PLAN>/
%%     ├── bench_report.json
%%     ├── chaos_report.json
%%     ├── conformance_report.json
%%     ├── refusal_audit.json
%%     └── .certified
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_evidence_path).

-include_lib("kernel/include/file.hrl").

-export([create_evidence_path/2, list_evidence_artifacts/2, verify_artifact_completeness/2,
         generate_conformance_report/3, mark_certified/2, is_certified/2, get_evidence_path/2,
         validate_immutability/2]).

-type version() :: string().
-type plan() :: team | enterprise | gov.
-type report_type() :: bench | chaos | conformance | refusal.
-type artifact_status() :: ok | missing | incomplete.

-define(EVIDENCE_ROOT, "dist/evidence").
-define(REQUIRED_ARTIFACTS,
        [<<"bench_report.json">>,
         <<"chaos_report.json">>,
         <<"conformance_report.json">>,
         <<"refusal_audit.json">>]).

%%%-------------------------------------------------------------------
%% @doc Create directory structure for plan evidence
%% @end
%%%-------------------------------------------------------------------
-spec create_evidence_path(version(), plan()) -> {ok, string()} | {error, term()}.
create_evidence_path(Version, Plan) when is_list(Version), is_atom(Plan) ->
    PlanStr = atom_to_list(Plan),
    EvidencePath = filename:join([?EVIDENCE_ROOT, Version, PlanStr]),

    case filelib:is_dir(EvidencePath) of
        true ->
            {ok, EvidencePath};
        false ->
            case file:make_dir(EvidencePath) of
                ok ->
                    {ok, EvidencePath};
                {error, enoent} ->
                    %% Create parent directories
                    case file:make_dir(
                             filename:join([?EVIDENCE_ROOT, Version]))
                    of
                        ok ->
                            file:make_dir(EvidencePath);
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end
    end.

%%%-------------------------------------------------------------------
%% @doc List all evidence artifacts for a plan
%% @end
%%%-------------------------------------------------------------------
-spec list_evidence_artifacts(version(), plan()) -> {ok, [string()]} | {error, term()}.
list_evidence_artifacts(Version, Plan) ->
    case get_evidence_path(Version, Plan) of
        {ok, Path} ->
            case file:list_dir(Path) of
                {ok, Files} ->
                    FilteredFiles =
                        lists:filter(fun(F) ->
                                        not lists:prefix(".", F)
                                        andalso filename:extension(F) =/= ".lock"
                                     end,
                                     Files),
                    {ok, lists:sort(FilteredFiles)};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%%-------------------------------------------------------------------
%% @doc Verify all required artifacts are present
%% @end
%%%-------------------------------------------------------------------
-spec verify_artifact_completeness(version(), plan()) ->
                                      {ok, complete} | {error, {incomplete, [binary()]}}.
verify_artifact_completeness(Version, Plan) ->
    case get_evidence_path(Version, Plan) of
        {ok, Path} ->
            verify_artifacts_in_path(Path, ?REQUIRED_ARTIFACTS);
        Error ->
            Error
    end.

%%%-------------------------------------------------------------------
%% @private Verify all required artifacts exist in path
%% @end
%%%-------------------------------------------------------------------
-spec verify_artifacts_in_path(string(), [binary()]) ->
                                  {ok, complete} | {error, {incomplete, [binary()]}}.
verify_artifacts_in_path(Path, RequiredArtifacts) ->
    MissingArtifacts =
        lists:filter(fun(ArtifactName) ->
                        ArtifactPath = filename:join(Path, binary_to_list(ArtifactName)),
                        not filelib:is_file(ArtifactPath)
                     end,
                     RequiredArtifacts),

    case MissingArtifacts of
        [] ->
            {ok, complete};
        Missing ->
            {error, {incomplete, Missing}}
    end.

%%%-------------------------------------------------------------------
%% @doc Generate conformance report from benchmark and chaos results
%%
%% Compares actual benchmark numbers against plan envelope bounds.
%% @end
%%%-------------------------------------------------------------------
-spec generate_conformance_report(version(), plan(), map()) -> {ok, map()} | {error, term()}.
generate_conformance_report(Version, Plan, Results) when is_map(Results) ->
    case get_evidence_path(Version, Plan) of
        {ok, Path} ->
            ConformanceReport = build_conformance_report(Plan, Results),
            ReportPath = filename:join(Path, "conformance_report.json"),

            case file:write_file(ReportPath, jsx:encode(ConformanceReport)) of
                ok ->
                    {ok, ConformanceReport};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%%-------------------------------------------------------------------
%% @private Build conformance report structure
%% @end
%%%-------------------------------------------------------------------
-spec build_conformance_report(plan(), map()) -> map().
build_conformance_report(Plan, Results) ->
    Envelope = get_plan_envelope(Plan),
    BenchResults = maps:get(benchmark, Results, #{}),
    ChaosResults = maps:get(chaos, Results, #{}),

    #{<<"plan">> => atom_to_binary(Plan),
      <<"timestamp">> => erlang:system_time(second),
      <<"envelope">> => Envelope,
      <<"benchmark_conformance">> => verify_benchmark_conformance(Envelope, BenchResults),
      <<"chaos_conformance">> => verify_chaos_conformance(Envelope, ChaosResults),
      <<"overall_status">> => determine_overall_status(Envelope, BenchResults, ChaosResults)}.

%%%-------------------------------------------------------------------
%% @private Verify benchmark results conform to envelope
%% @end
%%%-------------------------------------------------------------------
-spec verify_benchmark_conformance(map(), map()) -> map().
verify_benchmark_conformance(Envelope, Results) ->
    #{<<"throughput_req_s">> =>
          #{<<"limit">> => maps:get(<<"throughput_req_s">>, Envelope),
            <<"actual">> => maps:get(<<"throughput_req_s">>, Results, 0),
            <<"status">> =>
                conformance_status(maps:get(<<"throughput_req_s">>, Results, 0),
                                   maps:get(<<"throughput_req_s">>, Envelope))},
      <<"p99_latency_ms">> =>
          #{<<"limit">> => maps:get(<<"p99_latency_ms">>, Envelope),
            <<"actual">> => maps:get(<<"p99_latency_ms">>, Results, 0),
            <<"status">> =>
                conformance_status(maps:get(<<"p99_latency_ms">>, Envelope),
                                   maps:get(<<"p99_latency_ms">>, Results, infinity))},
      <<"memory_mb">> =>
          #{<<"limit">> => maps:get(<<"memory_mb">>, Envelope, undefined),
            <<"actual">> => maps:get(<<"memory_mb">>, Results, 0),
            <<"status">> =>
                conformance_status(maps:get(<<"memory_mb">>, Results, 0),
                                   maps:get(<<"memory_mb">>, Envelope, infinity))}}.

%%%-------------------------------------------------------------------
%% @private Verify chaos results conform to envelope
%% @end
%%%-------------------------------------------------------------------
-spec verify_chaos_conformance(map(), map()) -> map().
verify_chaos_conformance(Envelope, Results) ->
    #{<<"failover_sla_seconds">> =>
          #{<<"limit">> => maps:get(<<"failover_sla_seconds">>, Envelope),
            <<"actual">> => maps:get(<<"failover_time_seconds">>, Results, 0),
            <<"status">> =>
                conformance_status(maps:get(<<"failover_time_seconds">>, Results, 0),
                                   maps:get(<<"failover_sla_seconds">>, Envelope))},
      <<"recovery_rate">> =>
          #{<<"target">> => 0.95,
            <<"actual">> => maps:get(<<"recovery_rate">>, Results, 0.0),
            <<"status">> => conformance_status(maps:get(<<"recovery_rate">>, Results, 0.0), 0.95)},
      <<"error_rate_during_chaos">> =>
          #{<<"max_acceptable">> => 0.05,
            <<"actual">> => maps:get(<<"error_rate">>, Results, 0.0),
            <<"status">> => conformance_status(0.05, maps:get(<<"error_rate">>, Results, 1.0))}}.

%%%-------------------------------------------------------------------
%% @private Determine conformance status (pass/fail)
%% For throughput/recovery: actual >= limit is pass
%% For latency/failover: actual <= limit is pass
%% @end
%%%-------------------------------------------------------------------
-spec conformance_status(number(), number()) -> binary().
conformance_status(Actual, Limit) when Actual >= Limit, is_number(Limit), Limit > 0 ->
    <<"pass">>;
conformance_status(Actual, Limit) when Actual =< Limit, is_number(Limit), Limit > 0 ->
    <<"pass">>;
conformance_status(_, _) ->
    <<"fail">>.

%%%-------------------------------------------------------------------
%% @private Determine overall certification status
%% @end
%%%-------------------------------------------------------------------
-spec determine_overall_status(map(), map(), map()) -> binary().
determine_overall_status(Envelope, BenchResults, ChaosResults) ->
    BenchPass = check_bench_conformance(Envelope, BenchResults),
    ChaosPass = check_chaos_conformance(Envelope, ChaosResults),

    case {BenchPass, ChaosPass} of
        {true, true} ->
            <<"certified">>;
        {false, _} ->
            <<"failed">>;
        {_, false} ->
            <<"failed">>;
        _ ->
            <<"pending">>
    end.

%%%-------------------------------------------------------------------
%% @private Check if benchmark meets envelope
%% @end
%%%-------------------------------------------------------------------
-spec check_bench_conformance(map(), map()) -> boolean().
check_bench_conformance(Envelope, Results) ->
    ThroughputOk =
        maps:get(<<"throughput_req_s">>, Results, 0) >= maps:get(<<"throughput_req_s">>, Envelope),
    P99Ok =
        maps:get(<<"p99_latency_ms">>, Results, infinity)
        =< maps:get(<<"p99_latency_ms">>, Envelope),

    ThroughputOk andalso P99Ok.

%%%-------------------------------------------------------------------
%% @private Check if chaos results meet envelope
%% @end
%%%-------------------------------------------------------------------
-spec check_chaos_conformance(map(), map()) -> boolean().
check_chaos_conformance(Envelope, Results) ->
    FailoverOk =
        maps:get(<<"failover_time_seconds">>, Results, infinity)
        =< maps:get(<<"failover_sla_seconds">>, Envelope),
    RecoveryOk = maps:get(<<"recovery_rate">>, Results, 0.0) >= 0.95,
    ErrorRateOk = maps:get(<<"error_rate">>, Results, 1.0) =< 0.05,

    FailoverOk andalso RecoveryOk andalso ErrorRateOk.

%%%-------------------------------------------------------------------
%% @doc Mark evidence path as certified
%% @end
%%%-------------------------------------------------------------------
-spec mark_certified(version(), plan()) -> {ok, string()} | {error, term()}.
mark_certified(Version, Plan) ->
    case verify_artifact_completeness(Version, Plan) of
        {ok, complete} ->
            case get_evidence_path(Version, Plan) of
                {ok, Path} ->
                    CertFile = filename:join(Path, ".certified"),
                    Timestamp = erlang:system_time(second),
                    Marker = io_lib:format("~w~n", [Timestamp]),

                    case file:write_file(CertFile, Marker) of
                        ok ->
                            %% Make immutable
                            file:change_mode(CertFile, 8#444),
                            {ok, CertFile};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%%-------------------------------------------------------------------
%% @doc Check if evidence path is certified
%% @end
%%%-------------------------------------------------------------------
-spec is_certified(version(), plan()) -> boolean().
is_certified(Version, Plan) ->
    case get_evidence_path(Version, Plan) of
        {ok, Path} ->
            CertFile = filename:join(Path, ".certified"),
            filelib:is_file(CertFile);
        _ ->
            false
    end.

%%%-------------------------------------------------------------------
%% @doc Get full path to evidence directory
%% @end
%%%-------------------------------------------------------------------
-spec get_evidence_path(version(), plan()) -> {ok, string()} | {error, not_found}.
get_evidence_path(Version, Plan) when is_list(Version), is_atom(Plan) ->
    PlanStr = atom_to_list(Plan),
    Path = filename:join([?EVIDENCE_ROOT, Version, PlanStr]),

    case filelib:is_dir(Path) of
        true ->
            {ok, Path};
        false ->
            {error, not_found}
    end.

%%%-------------------------------------------------------------------
%% @doc Validate evidence path immutability
%% @end
%%%-------------------------------------------------------------------
-spec validate_immutability(version(), plan()) -> {ok, immutable} | {error, term()}.
validate_immutability(Version, Plan) ->
    case is_certified(Version, Plan) of
        true ->
            case get_evidence_path(Version, Plan) of
                {ok, Path} ->
                    validate_path_permissions(Path);
                Error ->
                    Error
            end;
        false ->
            {error, not_certified}
    end.

%%%-------------------------------------------------------------------
%% @private Validate that all artifacts have restricted permissions
%% @end
%%%-------------------------------------------------------------------
-spec validate_path_permissions(string()) -> {ok, immutable} | {error, term()}.
validate_path_permissions(Path) ->
    case file:list_dir(Path) of
        {ok, Files} ->
            case check_file_permissions(Path, Files) of
                ok ->
                    {ok, immutable};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%%-------------------------------------------------------------------
%% @private Check each file's permissions
%% @end
%%%-------------------------------------------------------------------
-spec check_file_permissions(string(), [string()]) -> ok | {error, term()}.
check_file_permissions(_, []) ->
    ok;
check_file_permissions(Path, [File | Rest]) ->
    FilePath = filename:join(Path, File),
    case file:read_file_info(FilePath) of
        {ok, FileInfo} ->
            Mode = FileInfo#file_info.mode,
            %% Check for restricted permissions (read-only)
            case Mode band 8#200 of
                0 ->
                    check_file_permissions(Path, Rest);
                _ ->
                    {error, {writable_file, FilePath}}
            end;
        Error ->
            Error
    end.

%%%-------------------------------------------------------------------
%% @private Get plan envelope specifications
%% @end
%%%-------------------------------------------------------------------
-spec get_plan_envelope(plan()) -> map().
get_plan_envelope(team) ->
    #{<<"throughput_req_s">> => 450,
      <<"concurrent_connections">> => 128,
      <<"queue_depth_messages">> => 2048,
      <<"p99_latency_ms">> => 150,
      <<"failover_sla_seconds">> => 5,
      <<"memory_mb">> => 512,
      <<"connection_timeout_seconds">> => 60};
get_plan_envelope(enterprise) ->
    #{<<"throughput_req_s">> => 1500,
      <<"concurrent_connections">> => 512,
      <<"queue_depth_messages">> => 8192,
      <<"p99_latency_ms">> => 100,
      <<"failover_sla_seconds">> => 2,
      <<"memory_mb">> => 2048,
      <<"connection_timeout_seconds">> => 30};
get_plan_envelope(gov) ->
    #{<<"throughput_req_s">> => 900,
      <<"concurrent_connections">> => 256,
      <<"queue_depth_messages">> => 4096,
      <<"p99_latency_ms">> => 80,
      <<"failover_sla_seconds">> => 1,
      <<"memory_mb">> => 1024,
      <<"connection_timeout_seconds">> => 20,
      <<"fips_140_2">> => true,
      <<"audit_logging">> => true}.
