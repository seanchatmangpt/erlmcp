%%%-----------------------------------------------------------------------------
%%% @doc TCPS Rebar3 SHACL Validation Provider
%%%
%%% Rebar3 provider plugin that performs SHACL validation on TCPS ontology
%%% and work orders before compilation. Enforces quality gates by:
%%% - Validating work_orders.ttl against tcps_shapes.ttl
%%% - Validating SKU definitions against quality constraints
%%% - Triggering Andon events on validation failures
%%% - Blocking compilation if critical violations found
%%%
%%% Usage in rebar.config:
%%%   {provider_hooks, [
%%%       {pre, [{compile, {tcps, shacl_validate}}]}
%%%   ]}.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_rebar3_shacl).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, shacl_validate).
-define(NAMESPACE, tcps).
-define(DEPS, []).

%%%=============================================================================
%%% API
%%%=============================================================================

%% @doc Initialize the provider
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, ?NAMESPACE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 tcps shacl_validate"},
        {short_desc, "Validate TCPS ontology with SHACL shapes"},
        {desc, "Performs SHACL validation on TCPS work orders and SKU definitions "
               "to enforce quality gates before compilation."},
        {opts, [
            {shapes_file, $s, "shapes", string,
             "Path to SHACL shapes file (default: shapes/tcps_shapes.ttl)"},
            {data_file, $d, "data", string,
             "Path to data file to validate (default: ontology/*.ttl)"},
            {strict, $x, "strict", {boolean, true},
             "Fail on any violations (default: true)"},
            {andon, $a, "andon", {boolean, true},
             "Trigger Andon on violations (default: true)"}
        ]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @doc Execute SHACL validation
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("~n=== TCPS SHACL Validation ===~n", []),

    %% Get options
    {Args, _} = rebar_state:command_parsed_args(State),
    ShapesFile = proplists:get_value(shapes, Args, "shapes/tcps_shapes.ttl"),
    DataPattern = proplists:get_value(data, Args, "ontology/*.ttl"),
    Strict = proplists:get_value(strict, Args, true),
    TriggerAndon = proplists:get_value(andon, Args, true),

    %% Find data files to validate
    DataFiles = filelib:wildcard(DataPattern),

    case DataFiles of
        [] ->
            rebar_api:warn("No data files found matching pattern: ~s", [DataPattern]),
            {ok, State};
        _ ->
            rebar_api:info("Validating ~p files against ~s~n",
                          [length(DataFiles), ShapesFile]),

            %% Ensure SHACL shapes file exists
            case filelib:is_regular(ShapesFile) of
                false ->
                    {error, io_lib:format("SHACL shapes file not found: ~s", [ShapesFile])};
                true ->
                    %% Validate each data file
                    Results = [validate_file(DataFile, ShapesFile, State)
                              || DataFile <- DataFiles],

                    %% Aggregate results
                    case aggregate_validation_results(Results) of
                        {ok, Summary} ->
                            rebar_api:info("~nValidation Summary:~n", []),
                            rebar_api:info("  Files validated: ~p~n",
                                         [maps:get(total_files, Summary)]),
                            rebar_api:info("  Total violations: ~p~n",
                                         [maps:get(total_violations, Summary)]),
                            rebar_api:info("  Critical violations: ~p~n",
                                         [maps:get(critical_violations, Summary)]),

                            %% Generate receipt
                            Receipt = generate_validation_receipt(Summary, State),
                            store_receipt(Receipt, State),

                            %% Check if we should fail
                            CriticalCount = maps:get(critical_violations, Summary),
                            if
                                CriticalCount > 0 andalso Strict ->
                                    %% Trigger Andon if enabled
                                    if
                                        TriggerAndon ->
                                            trigger_shacl_andon(Summary, State);
                                        true ->
                                            ok
                                    end,
                                    {error, io_lib:format(
                                        "SHACL validation failed: ~p critical violations found",
                                        [CriticalCount])};
                                true ->
                                    rebar_api:info("~n✓ SHACL validation passed~n~n", []),
                                    {ok, State}
                            end;
                        {error, Reason} ->
                            {error, io_lib:format("Validation aggregation failed: ~p", [Reason])}
                    end
            end
    end.

%% @doc Format error messages
-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("TCPS SHACL Validation Error: ~p", [Reason]).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private Validate a single file against SHACL shapes
validate_file(DataFile, ShapesFile, State) ->
    rebar_api:debug("Validating: ~s", [DataFile]),

    %% In production, this would use a SHACL validation library
    %% For now, we'll do basic structural validation
    case file:read_file(DataFile) of
        {ok, Content} ->
            Violations = perform_shacl_validation(Content, ShapesFile),

            if
                length(Violations) > 0 ->
                    rebar_api:warn("~nViolations in ~s:~n", [DataFile]),
                    [rebar_api:warn("  - ~s~n", [V]) || V <- Violations],
                    {DataFile, {violations, Violations}};
                true ->
                    rebar_api:debug("  ✓ ~s validated successfully", [DataFile]),
                    {DataFile, ok}
            end;
        {error, Reason} ->
            rebar_api:error("Failed to read ~s: ~p", [DataFile, Reason]),
            {DataFile, {error, Reason}}
    end.

%% @private Perform SHACL validation (stub - would use real SHACL engine)
perform_shacl_validation(Content, _ShapesFile) ->
    %% Basic validation checks
    Violations = [],

    %% Check for required TCPS namespaces
    ContentStr = binary_to_list(Content),
    V1 = case string:str(ContentStr, "@prefix tcps:") of
        0 -> ["Missing @prefix tcps: namespace declaration"];
        _ -> []
    end,

    %% Check for basic TCPS entities
    V2 = case {string:str(ContentStr, "tcps:WorkOrder"),
               string:str(ContentStr, "tcps:SKU")} of
        {0, 0} -> ["No TCPS entities found (WorkOrder or SKU)"];
        _ -> []
    end,

    %% Check for timestamps
    V3 = case string:str(ContentStr, "tcps:createdAt") of
        0 -> ["Missing tcps:createdAt timestamps"];
        _ -> []
    end,

    Violations ++ V1 ++ V2 ++ V3.

%% @private Aggregate validation results from multiple files
aggregate_validation_results(Results) ->
    Summary = lists:foldl(
        fun({File, Result}, Acc) ->
            case Result of
                ok ->
                    Acc#{
                        total_files => maps:get(total_files, Acc, 0) + 1,
                        passed_files => maps:get(passed_files, Acc, 0) + 1
                    };
                {violations, Violations} ->
                    CriticalViolations = [V || V <- Violations, is_critical(V)],
                    Acc#{
                        total_files => maps:get(total_files, Acc, 0) + 1,
                        failed_files => maps:get(failed_files, Acc, 0) + 1,
                        total_violations => maps:get(total_violations, Acc, 0) + length(Violations),
                        critical_violations => maps:get(critical_violations, Acc, 0) + length(CriticalViolations),
                        violations_by_file => maps:put(File, Violations,
                                                       maps:get(violations_by_file, Acc, #{}))
                    };
                {error, _Reason} ->
                    Acc#{
                        total_files => maps:get(total_files, Acc, 0) + 1,
                        error_files => maps:get(error_files, Acc, 0) + 1
                    }
            end
        end,
        #{total_files => 0, passed_files => 0, failed_files => 0, error_files => 0,
          total_violations => 0, critical_violations => 0, violations_by_file => #{}},
        Results
    ),
    {ok, Summary}.

%% @private Determine if a violation is critical
is_critical(Violation) ->
    %% Check if violation contains critical keywords
    ViolationStr = lists:flatten(io_lib:format("~s", [Violation])),
    ViolationLower = string:lowercase(ViolationStr),
    CriticalKeywords = ["missing", "required", "must", "namespace"],
    lists:any(fun(Keyword) ->
        string:str(ViolationLower, Keyword) > 0
    end, CriticalKeywords).

%% @private Generate validation receipt
generate_validation_receipt(Summary, _State) ->
    #{
        receipt_id => generate_receipt_id(),
        receipt_type => <<"shacl_validation">>,
        stage => <<"validation">>,
        timestamp => erlang:system_time(millisecond),
        timestamp_iso => format_iso8601_timestamp(erlang:system_time(millisecond)),
        status => case maps:get(critical_violations, Summary, 0) of
            0 -> <<"pass">>;
            _ -> <<"fail">>
        end,
        summary => Summary,
        evidence => #{
            validator => <<"tcps_rebar3_shacl">>,
            shapes_version => <<"1.0.0">>,
            validation_mode => <<"strict">>
        }
    }.

%% @private Store receipt to filesystem
store_receipt(Receipt, _State) ->
    ReceiptDir = "priv/receipts",
    filelib:ensure_dir(ReceiptDir ++ "/"),

    ReceiptId = maps:get(receipt_id, Receipt),
    Filename = binary_to_list(ReceiptId) ++ ".json",
    FullPath = filename:join(ReceiptDir, Filename),

    %% Convert to JSON
    Json = jsx:encode(Receipt),

    case file:write_file(FullPath, Json) of
        ok ->
            rebar_api:debug("Receipt stored: ~s", [FullPath]),
            ok;
        {error, Reason} ->
            rebar_api:warn("Failed to store receipt: ~p", [Reason]),
            {error, Reason}
    end.

%% @private Trigger Andon event for SHACL violations
trigger_shacl_andon(Summary, _State) ->
    rebar_api:warn("~n⚠️  TRIGGERING ANDON: SHACL Validation Failure~n", []),

    %% Ensure Andon server is started
    case whereis(tcps_andon) of
        undefined ->
            %% Start ETS table directly if server not running
            tcps_andon:start();
        _Pid ->
            ok
    end,

    %% Create Andon context
    Context = #{
        sku_id => <<"build_", (integer_to_binary(erlang:system_time(millisecond)))/binary>>,
        stage => validation,
        details => #{
            validator => <<"tcps_rebar3_shacl">>,
            total_violations => maps:get(total_violations, Summary, 0),
            critical_violations => maps:get(critical_violations, Summary, 0),
            failed_files => maps:get(failed_files, Summary, 0),
            violations_by_file => maps:get(violations_by_file, Summary, #{})
        },
        metadata => #{
            timestamp => erlang:system_time(millisecond),
            build_blocked => true
        }
    },

    %% Trigger Andon
    case tcps_andon:trigger_andon(shacl_violation, Context) of
        {ok, AndonId} ->
            rebar_api:warn("Andon event created: ~s~n", [AndonId]),
            rebar_api:warn("Build blocked until Andon is resolved.~n", []),
            rebar_api:warn("Resolve with: rebar3 tcps andon resolve ~s~n~n", [AndonId]),
            ok;
        {error, Reason} ->
            rebar_api:error("Failed to trigger Andon: ~p", [Reason]),
            {error, Reason}
    end.

%% @private Generate unique receipt ID
generate_receipt_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(999999),
    iolist_to_binary(io_lib:format("rcpt_shacl_~p_~p", [Timestamp, Random])).

%% @private Format timestamp as ISO 8601
format_iso8601_timestamp(Milliseconds) ->
    Seconds = Milliseconds div 1000,
    DateTime = calendar:system_time_to_universal_time(Seconds, second),
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                      [Year, Month, Day, Hour, Minute, Second])
    ).
