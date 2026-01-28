%%%====================================================================
%%% @doc erlmcp_bench_helpers - Benchmark Helper Utilities
%%%
%%% Provides common utilities for benchmark modules:
%%% - JSON formatting for results
%%% - Metrology-compliant metric construction
%%% - Summary report generation
%%% - Result file validation and persistence
%%%
%%% Used by all benchmark modules to ensure consistent output format.
%%% @end
%%%====================================================================
-module(erlmcp_bench_helpers).

-export([
    format_result_json/2,
    format_metric/3,
    format_metric/4,
    save_result/2,
    validate_and_save/2,
    generate_workload_id/2,
    get_environment_info/0,
    format_timestamp/0,
    format_timestamp/1
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type metric() :: #{
    value := number(),
    unit := binary(),
    precision_us => non_neg_integer()
}.

-type benchmark_result() :: #{
    workload_id := binary(),
    transport := binary(),
    duration_seconds := number(),
    timestamp := integer(),
    environment := map(),
    _ => term()
}.

-export_type([metric/0, benchmark_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Format a complete benchmark result as JSON-compatible map
%%
%% Ensures all required metrology fields are present:
%% - workload_id (binary)
%% - transport (binary)
%% - duration_seconds (number)
%% - timestamp (integer)
%% - environment (map with os/otp/cores)
%% @end
%%--------------------------------------------------------------------
-spec format_result_json(binary(), map()) -> benchmark_result().
format_result_json(WorkloadId, Result) when is_binary(WorkloadId), is_map(Result) ->
    Now = erlang:system_time(second),

    BaseResult = #{
        <<"workload_id">> => WorkloadId,
        <<"transport">> => maps:get(<<"transport">>, Result, <<"unknown">>),
        <<"duration_seconds">> => maps:get(<<"duration_seconds">>, Result, 0),
        <<"timestamp">> => Now,
        <<"timestamp_iso">> => format_timestamp(Now),
        <<"environment">> => maps:get(<<"environment">>, Result, get_environment_info())
    },

    % Merge with provided result
    maps:merge(Result, BaseResult).

%%--------------------------------------------------------------------
%% @doc Format a metric with value and unit
%% @end
%%--------------------------------------------------------------------
-spec format_metric(number(), binary()) -> metric().
format_metric(Value, Unit) when is_number(Value), is_binary(Unit) ->
    #{
        <<"value">> => Value,
        <<"unit">> => Unit
    }.

%%--------------------------------------------------------------------
%% @doc Format a metric with value, unit, and scope
%% Example: format_metric(48.5, <<"MiB">>, <<"/conn">>) -> "MiB/conn"
%% @end
%%--------------------------------------------------------------------
-spec format_metric(number(), binary(), binary()) -> metric().
format_metric(Value, Unit, Scope) when is_number(Value), is_binary(Unit), is_binary(Scope) ->
    CompositeUnit = <<Unit/binary, Scope/binary>>,
    #{
        <<"value">> => Value,
        <<"unit">> => CompositeUnit
    }.

%%--------------------------------------------------------------------
%% @doc Format a time metric with microsecond precision
%% Includes raw microsecond value for metrology compliance
%% @end
%%--------------------------------------------------------------------
-spec format_metric(number(), binary(), binary(), microseconds) -> metric().
format_metric(Value, Unit, _Scope, microseconds) when is_number(Value), is_binary(Unit) ->
    PrecisionUs = case Unit of
        <<"ms">> -> round(Value * 1000);
        <<"s">> -> round(Value * 1000000);
        <<"µs">> -> round(Value);
        <<"us">> -> round(Value);
        _ -> round(Value)
    end,

    #{
        <<"value">> => Value,
        <<"unit">> => Unit,
        <<"precision_us">> => PrecisionUs
    }.

%%--------------------------------------------------------------------
%% @doc Save benchmark result to JSON file
%% @end
%%--------------------------------------------------------------------
-spec save_result(benchmark_result(), file:filename()) -> ok | {error, term()}.
save_result(Result, FilePath) when is_map(Result) ->
    try
        Json = jsx:encode(Result, [space, indent]),
        ok = file:write_file(FilePath, Json),
        ?LOG_INFO("Benchmark result saved to: ~s", [FilePath]),
        ok
    catch
        Class:Error:Stacktrace ->
            ?LOG_ERROR("Failed to save result: ~p:~p~n~p", [Class, Error, Stacktrace]),
            {error, {save_failed, Error}}
    end.

%%--------------------------------------------------------------------
%% @doc Validate metrology compliance and save result
%% @end
%%--------------------------------------------------------------------
-spec validate_and_save(benchmark_result(), file:filename()) -> ok | {error, term()}.
validate_and_save(Result, FilePath) when is_map(Result) ->
    case erlmcp_metrology_validator:validate_report(Result) of
        ok ->
            save_result(Result, FilePath);
        {error, Violations} ->
            ?LOG_ERROR("Metrology violations detected (~p):~n~p", [length(Violations), Violations]),

            % Save anyway with warning
            ResultWithWarning = Result#{
                <<"metrology_warnings">> => [
                    erlmcp_metrology_validator:format_violation(V) || V <- Violations
                ]
            },
            save_result(ResultWithWarning, FilePath),
            {error, {metrology_violations, Violations}}
    end.

%%--------------------------------------------------------------------
%% @doc Generate standardized workload ID
%% Format: "<category>_<name>"
%% Example: generate_workload_id(<<"tcp">>, <<"sustained_25k">>) -> <<"tcp_sustained_25k">>
%% @end
%%--------------------------------------------------------------------
-spec generate_workload_id(binary(), binary()) -> binary().
generate_workload_id(Category, Name) when is_binary(Category), is_binary(Name) ->
    <<Category/binary, "_", Name/binary>>.

%%--------------------------------------------------------------------
%% @doc Get environment information for benchmark context
%% @end
%%--------------------------------------------------------------------
-spec get_environment_info() -> map().
get_environment_info() ->
    #{
        <<"os">> => list_to_binary(erlang:system_info(system_architecture)),
        <<"otp_version">> => list_to_binary(erlang:system_info(otp_release)),
        <<"erts_version">> => list_to_binary(erlang:system_info(version)),
        <<"cores">> => erlang:system_info(logical_processors),
        <<"schedulers">> => erlang:system_info(schedulers_online),
        <<"hostname">> => list_to_binary(net_adm:localhost())
    }.

%%--------------------------------------------------------------------
%% @doc Format current timestamp as ISO8601
%% @end
%%--------------------------------------------------------------------
-spec format_timestamp() -> binary().
format_timestamp() ->
    format_timestamp(erlang:system_time(second)).

%%--------------------------------------------------------------------
%% @doc Format Unix timestamp as ISO8601
%% @end
%%--------------------------------------------------------------------
-spec format_timestamp(integer()) -> binary().
format_timestamp(UnixSeconds) when is_integer(UnixSeconds) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:system_time_to_universal_time(UnixSeconds, second),

    iolist_to_binary(io_lib:format(
        "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Year, Month, Day, Hour, Min, Sec]
    )).

%%====================================================================
%% Internal Functions
%%====================================================================

% None currently
