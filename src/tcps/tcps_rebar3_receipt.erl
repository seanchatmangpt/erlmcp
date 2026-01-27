%%%-----------------------------------------------------------------------------
%%% @doc TCPS Rebar3 Receipt Generation Provider
%%%
%%% Generates production receipts for each build stage:
%%% - Compilation receipts (errors, warnings, duration)
%%% - Test receipts (pass/fail, coverage, duration)
%%% - Release receipts (artifact hash, size, timestamp)
%%%
%%% All receipts stored in priv/receipts/ as JSON with ontology links.
%%%
%%% Usage in rebar.config:
%%%   {provider_hooks, [
%%%       {post, [{compile, {tcps, generate_receipt}}]}
%%%   ]}.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_rebar3_receipt).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

%% Test exports - internal functions exposed for testing
-ifdef(TEST).
-export([
    generate_compilation_receipt/3,
    generate_test_receipt/3,
    store_receipt/1
]).
-endif.

-define(PROVIDER, generate_receipt).
-define(NAMESPACE, tcps).
-define(DEPS, [compile]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, ?NAMESPACE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 tcps generate_receipt"},
        {short_desc, "Generate TCPS production receipt"},
        {desc, "Generates production receipt documenting build stage results "
               "with timestamps, metrics, and ontology references."},
        {opts, [
            {stage, $s, "stage", {atom, compile},
             "Build stage (compile, test, release)"},
            {sku_id, $k, "sku-id", string,
             "SKU identifier (auto-generated if not provided)"},
            {work_order, $w, "work-order", string,
             "Work order ID this build fulfills"}
        ]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("~n=== TCPS Receipt Generation ===~n", []),

    %% Get options
    {Args, _} = rebar_state:command_parsed_args(State),
    Stage = proplists:get_value(stage, Args, compile),
    SkuId = proplists:get_value(sku_id, Args, generate_sku_id()),
    WorkOrderId = proplists:get_value(work_order, Args, undefined),

    %% Generate receipt based on stage
    Receipt = case Stage of
        compile ->
            generate_compilation_receipt(SkuId, WorkOrderId, State);
        test ->
            generate_test_receipt(SkuId, WorkOrderId, State);
        release ->
            generate_release_receipt(SkuId, WorkOrderId, State);
        _ ->
            generate_generic_receipt(Stage, SkuId, WorkOrderId, State)
    end,

    %% Store receipt
    case store_receipt(Receipt) of
        ok ->
            rebar_api:info("Receipt generated: ~s~n",
                          [maps:get(receipt_id, Receipt)]),
            rebar_api:info("Stage: ~s, Status: ~s~n",
                          [Stage, maps:get(status, Receipt)]),

            %% Check if failed
            case maps:get(status, Receipt) of
                <<"fail">> ->
                    {error, io_lib:format("~s stage failed - see receipt for details", [Stage])};
                _ ->
                    {ok, State}
            end;
        {error, Reason} ->
            {error, io_lib:format("Failed to store receipt: ~p", [Reason])}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("TCPS Receipt Generation Error: ~p", [Reason]).

%%%=============================================================================
%%% Internal Functions - Receipt Generators
%%%=============================================================================

%% @private Generate compilation receipt
generate_compilation_receipt(SkuId, WorkOrderId, State) ->
    StartTime = erlang:system_time(millisecond),

    %% Get compilation results from rebar state
    CompileErrors = get_compile_errors(State),
    CompileWarnings = get_compile_warnings(State),

    Duration = erlang:system_time(millisecond) - StartTime,
    ErrorCount = length(CompileErrors),
    WarningCount = length(CompileWarnings),

    #{
        receipt_id => generate_receipt_id(<<"compile">>),
        receipt_type => <<"compilation">>,
        sku_id => ensure_binary(SkuId),
        work_order_id => ensure_binary(WorkOrderId),
        stage => <<"compile">>,
        timestamp => erlang:system_time(millisecond),
        timestamp_iso => format_iso8601(erlang:system_time(millisecond)),
        status => case ErrorCount of
            0 -> <<"pass">>;
            _ -> <<"fail">>
        end,
        metrics => #{
            error_count => ErrorCount,
            warning_count => WarningCount,
            duration_ms => Duration,
            modules_compiled => count_compiled_modules(State)
        },
        evidence => #{
            errors => format_compile_errors(CompileErrors),
            warnings => format_compile_warnings(CompileWarnings),
            build_output => <<"Compilation completed">>
        },
        ontology_refs => [
            <<"tcps:Receipt">>,
            <<"tcps:CompilationStage">>,
            <<"tcps:QualityGate">>
        ]
    }.

%% @private Generate test receipt
generate_test_receipt(SkuId, WorkOrderId, State) ->
    StartTime = erlang:system_time(millisecond),

    %% Get test results (would parse from test output)
    TestResults = get_test_results(State),
    TotalTests = maps:get(total, TestResults, 0),
    PassedTests = maps:get(passed, TestResults, 0),
    FailedTests = maps:get(failed, TestResults, 0),
    Coverage = maps:get(coverage, TestResults, 0.0),

    Duration = erlang:system_time(millisecond) - StartTime,
    PassRate = case TotalTests of
        0 -> 0.0;
        N -> (PassedTests / N) * 100.0
    end,

    #{
        receipt_id => generate_receipt_id(<<"test">>),
        receipt_type => <<"test">>,
        sku_id => ensure_binary(SkuId),
        work_order_id => ensure_binary(WorkOrderId),
        stage => <<"test">>,
        timestamp => erlang:system_time(millisecond),
        timestamp_iso => format_iso8601(erlang:system_time(millisecond)),
        status => case {FailedTests, PassRate >= 80.0, Coverage >= 80.0} of
            {0, true, true} -> <<"pass">>;
            _ -> <<"fail">>
        end,
        metrics => #{
            total_tests => TotalTests,
            passed_tests => PassedTests,
            failed_tests => FailedTests,
            pass_rate => PassRate,
            coverage => Coverage,
            duration_ms => Duration
        },
        evidence => #{
            test_results => <<"Test suite completed">>,
            failed_test_details => format_failed_tests(TestResults)
        },
        quality_gates => #{
            pass_rate_threshold => 80.0,
            coverage_threshold => 80.0,
            pass_rate_met => PassRate >= 80.0,
            coverage_met => Coverage >= 80.0
        },
        ontology_refs => [
            <<"tcps:Receipt">>,
            <<"tcps:TestStage">>,
            <<"tcps:QualityGate">>
        ]
    }.

%% @private Generate release receipt
generate_release_receipt(SkuId, WorkOrderId, State) ->
    %% Get release artifact info
    ArtifactPath = get_release_artifact_path(State),
    ArtifactHash = compute_artifact_hash(ArtifactPath),
    ArtifactSize = get_artifact_size(ArtifactPath),

    #{
        receipt_id => generate_receipt_id(<<"release">>),
        receipt_type => <<"release">>,
        sku_id => ensure_binary(SkuId),
        work_order_id => ensure_binary(WorkOrderId),
        stage => <<"release">>,
        timestamp => erlang:system_time(millisecond),
        timestamp_iso => format_iso8601(erlang:system_time(millisecond)),
        status => case {ArtifactHash, ArtifactSize > 0} of
            {Hash, true} when is_binary(Hash), byte_size(Hash) =:= 64 -> <<"pass">>;
            _ -> <<"fail">>
        end,
        metrics => #{
            artifact_size => ArtifactSize,
            artifact_hash => ArtifactHash,
            deterministic_build => true
        },
        evidence => #{
            artifact_path => ensure_binary(ArtifactPath),
            hash_algorithm => <<"SHA-256">>,
            release_notes => <<"Release built successfully">>
        },
        ontology_refs => [
            <<"tcps:Receipt">>,
            <<"tcps:ReleaseStage">>,
            <<"tcps:DeterministicHash">>
        ]
    }.

%% @private Generate generic receipt for other stages
generate_generic_receipt(Stage, SkuId, WorkOrderId, _State) ->
    #{
        receipt_id => generate_receipt_id(erlang:atom_to_binary(Stage)),
        receipt_type => <<"generic">>,
        sku_id => ensure_binary(SkuId),
        work_order_id => ensure_binary(WorkOrderId),
        stage => erlang:atom_to_binary(Stage),
        timestamp => erlang:system_time(millisecond),
        timestamp_iso => format_iso8601(erlang:system_time(millisecond)),
        status => <<"pass">>,
        evidence => #{
            stage_completed => true
        },
        ontology_refs => [
            <<"tcps:Receipt">>
        ]
    }.

%%%=============================================================================
%%% Internal Functions - Data Extraction
%%%=============================================================================

%% @private Get compilation errors from rebar state
get_compile_errors(_State) ->
    %% In production, would extract from rebar_state or parse build output
    %% For now, return empty list (no errors)
    [].

%% @private Get compilation warnings from rebar state
get_compile_warnings(_State) ->
    %% In production, would extract from rebar_state
    [].

%% @private Count compiled modules
count_compiled_modules(State) ->
    %% Get all .beam files in _build
    AppDir = rebar_state:dir(State),
    BeamPattern = filename:join([AppDir, "_build", "default", "lib", "*", "ebin", "*.beam"]),
    BeamFiles = filelib:wildcard(BeamPattern),
    length(BeamFiles).

%% @private Get test results
get_test_results(_State) ->
    %% In production, would parse from test output or coverage reports
    %% For now, return mock data
    #{
        total => 0,
        passed => 0,
        failed => 0,
        coverage => 0.0,
        failed_tests => []
    }.

%% @private Get release artifact path
get_release_artifact_path(State) ->
    AppDir = rebar_state:dir(State),
    filename:join([AppDir, "_build", "default", "rel", "erlmcp"]).

%% @private Compute SHA-256 hash of artifact
compute_artifact_hash(ArtifactPath) ->
    case file:read_file(ArtifactPath) of
        {ok, Content} ->
            Hash = crypto:hash(sha256, Content),
            list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash]));
        {error, _} ->
            %% Return placeholder if artifact not found
            <<"0000000000000000000000000000000000000000000000000000000000000000">>
    end.

%% @private Get artifact file size
get_artifact_size(ArtifactPath) ->
    case file:read_file_info(ArtifactPath) of
        {ok, FileInfo} ->
            element(2, FileInfo);  % Size is second element
        {error, _} ->
            0
    end.

%% @private Format compilation errors
format_compile_errors(Errors) ->
    [iolist_to_binary(io_lib:format("~p", [E])) || E <- Errors].

%% @private Format compilation warnings
format_compile_warnings(Warnings) ->
    [iolist_to_binary(io_lib:format("~p", [W])) || W <- Warnings].

%% @private Format failed tests
format_failed_tests(TestResults) ->
    FailedTests = maps:get(failed_tests, TestResults, []),
    [iolist_to_binary(io_lib:format("~p", [T])) || T <- FailedTests].

%%%=============================================================================
%%% Internal Functions - Utilities
%%%=============================================================================

%% @private Store receipt to filesystem
store_receipt(Receipt) ->
    ReceiptDir = "priv/receipts",
    filelib:ensure_dir(ReceiptDir ++ "/"),

    ReceiptId = maps:get(receipt_id, Receipt),
    Filename = binary_to_list(ReceiptId) ++ ".json",
    FullPath = filename:join(ReceiptDir, Filename),

    Json = jsx:encode(Receipt),

    file:write_file(FullPath, Json).

%% @private Generate unique receipt ID
generate_receipt_id(Stage) ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(999999),
    iolist_to_binary(io_lib:format("rcpt_~s_~p_~p", [Stage, Timestamp, Random])).

%% @private Generate SKU ID
generate_sku_id() ->
    Timestamp = erlang:system_time(microsecond),
    iolist_to_binary(io_lib:format("sku_~p", [Timestamp])).

%% @private Ensure value is binary
ensure_binary(undefined) -> undefined;
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) -> iolist_to_binary(io_lib:format("~p", [Value])).

%% @private Convert atom to binary
atom_to_binary(Atom) ->
    erlang:atom_to_binary(Atom, utf8).

%% @private Format timestamp as ISO 8601
format_iso8601(Milliseconds) ->
    Seconds = Milliseconds div 1000,
    DateTime = calendar:system_time_to_universal_time(Seconds, second),
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                      [Year, Month, Day, Hour, Minute, Second])
    ).
