%%%-------------------------------------------------------------------
%%% @doc
%%% Metrology validator for benchmark output.
%%% Validates that benchmark metrics use canonical units and follow
%%% metrology standards to eliminate ambiguous measurements.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_metrology_validator).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, validate_benchmark_output/1, validate_metric_units/1,
         validate_required_fields/1, detect_ambiguous_units/1, validate_scope/1,
         generate_violations_report/1, get_validation_rules/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_VALIDATOR, #{}).

-record(state, {validation_rules :: map(), violation_count :: non_neg_integer()}).

-type benchmark_output() :: map().
-type validation_result() :: {ok, map()} | {error, [binary()]}.
-type violation() ::
    #{type := binary(),
      field := binary(),
      reason => binary()}.
-type violations_report() :: #{valid := boolean(), violations := [violation()]}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [?DEFAULT_VALIDATOR], []).

-spec start_link(map()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Options], []).

%% @doc Validate complete benchmark output
-spec validate_benchmark_output(benchmark_output()) -> validation_result().
validate_benchmark_output(Output) when is_map(Output) ->
    Validations =
        [fun validate_required_fields/1, fun validate_metric_units/1, fun validate_scope/1],

    Results = lists:map(fun(F) -> F(Output) end, Validations),
    Errors =
        lists:filtermap(fun ({error, _} = Error) ->
                                {true, Error};
                            ({ok, _}) ->
                                false
                        end,
                        Results),

    case Errors of
        [] ->
            {ok, #{status => valid, validations_passed => length(Results)}};
        _ ->
            ErrorMessages = lists:flatmap(fun({error, Msgs}) -> Msgs end, Errors),
            {error, ErrorMessages}
    end.

%% @doc Validate all metric units use canonical forms
-spec validate_metric_units(benchmark_output()) -> validation_result().
validate_metric_units(Output) when is_map(Output) ->
    AllViolations = detect_ambiguous_units(Output),
    case AllViolations of
        [] ->
            {ok, #{units => canonical}};
        _ ->
            ErrorMsgs = [format_violation(V) || V <- AllViolations],
            {error, ErrorMsgs}
    end.

%% @doc Validate required fields are present
-spec validate_required_fields(benchmark_output()) -> validation_result().
validate_required_fields(Output) when is_map(Output) ->
    RequiredFields =
        [{workload_id, "Workload identifier"},
         {transport, "Transport type"},
         {duration_s, "Duration in seconds"},
         {scope, "Measurement scope"}],

    Missing =
        lists:filtermap(fun({Field, Desc}) ->
                           case maps:get(Field, Output, undefined) of
                               undefined ->
                                   {true, {Field, Desc}};
                               _ ->
                                   false
                           end
                        end,
                        RequiredFields),

    case Missing of
        [] ->
            {ok, #{required_fields => present}};
        _ ->
            ErrorMsgs =
                [io_lib:format("Missing required field '~s': ~s", [Field, Desc])
                 || {Field, Desc} <- Missing],
            {error, [list_to_binary(Msg) || Msg <- ErrorMsgs]}
    end.

%% @doc Detect ambiguous or non-canonical units
-spec detect_ambiguous_units(benchmark_output()) -> [violation()].
detect_ambiguous_units(Output) when is_map(Output) ->
    AmbiguousPatterns =
        [{<<"req/s">>, <<"msg_per_s">>, <<"Throughput must use canonical msg_per_s unit">>},
         {<<"ops/s">>, <<"msg_per_s">>, <<"Throughput must use canonical msg_per_s unit">>},
         {<<"throughput">>,
          <<"throughput_msg_per_s">>,
          <<"Throughput metric must specify msg_per_s unit">>},
         {<<"latency">>,
          <<"latency_p50_us">>,
          <<"Latency metric must specify percentile and _us unit">>},
         {<<"latency_ms">>,
          <<"latency_p50_us">>,
          <<"Latency must use microseconds (_us), not milliseconds (_ms)">>},
         {<<"p50_ms">>, <<"p50_us">>, <<"Percentile must use microseconds (_us)">>},
         {<<"p95_ms">>, <<"p95_us">>, <<"Percentile must use microseconds (_us)">>},
         {<<"p99_ms">>, <<"p99_us">>, <<"Percentile must use microseconds (_us)">>},
         {<<"memory">>,
          <<"memory_heap_mib_per_conn">>,
          <<"Memory metric must specify scope (heap/rss) and unit (mib/gib)">>},
         {<<"mem">>,
          <<"memory_heap_mib_per_conn">>,
          <<"Memory metric must specify scope and unit">>}],

    lists:filtermap(fun(Key) ->
                       case maps:get(Key, Output, undefined) of
                           undefined ->
                               false;
                           _Value ->
                               {Ambiguous, Canonical, Reason} =
                                   lists:keyfind(Key, 1, AmbiguousPatterns),
                               Violation =
                                   #{type => <<"ambiguous_unit">>,
                                     field => Ambiguous,
                                     expected => Canonical,
                                     reason => Reason},
                               {true, Violation}
                       end
                    end,
                    [Key || {Key, _, _} <- AmbiguousPatterns]).

%% @doc Validate scope is properly specified
-spec validate_scope(benchmark_output()) -> validation_result().
validate_scope(Output) when is_map(Output) ->
    Scope = maps:get(scope, Output, undefined),

    ValidScopes =
        [<<"per_connection_heap">>,
         <<"per_connection_total">>,
         <<"per_node_heap">>,
         <<"per_node_total">>,
         <<"system_wide">>],

    case Scope of
        undefined ->
            {error, [<<"Scope field is required and must specify measurement context">>]};
        Scope when is_binary(Scope) ->
            case lists:member(Scope, ValidScopes) of
                true ->
                    {ok, #{scope => valid}};
                false ->
                    ErrorMsg =
                        io_lib:format("Invalid scope '~s'. Valid scopes: ~p", [Scope, ValidScopes]),
                    {error, [list_to_binary(ErrorMsg)]}
            end;
        _ ->
            {error, [<<"Scope must be a binary string specifying measurement context">>]}
    end.

%% @doc Generate comprehensive violations report
-spec generate_violations_report(benchmark_output()) -> violations_report().
generate_violations_report(Output) when is_map(Output) ->
    UnitViolations = detect_ambiguous_units(Output),

    RequiredViolations =
        case validate_required_fields(Output) of
            {error, Msgs} ->
                [#{type => <<"missing_required">>, reason => Msg} || Msg <- Msgs];
            _ ->
                []
        end,

    ScopeViolations =
        case validate_scope(Output) of
            {error, ScopeMsgs} ->
                [#{type => <<"invalid_scope">>, reason => Msg} || Msg <- ScopeMsgs];
            _ ->
                []
        end,

    AllViolations = UnitViolations ++ RequiredViolations ++ ScopeViolations,

    #{valid => AllViolations =:= [],
      violations => AllViolations,
      total_violations => length(AllViolations),
      timestamp => erlang:system_time(millisecond)}.

%% @doc Get validation rules documentation
-spec get_validation_rules() -> map().
get_validation_rules() ->
    gen_server:call(?SERVER, get_validation_rules).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([_Options]) ->
    Rules =
        #{throughput =>
              #{canonical => <<"msg_per_s">>,
                forbidden => [<<"req/s">>, <<"ops/s">>, <<"throughput">>],
                description => <<"Messages per second, not requests/operations">>},
          latency =>
              #{canonical => <<"_us">>,
                percentiles => [<<"p50_us">>, <<"p95_us">>, <<"p99_us">>],
                forbidden => [<<"_ms">>, <<"latency_ms">>, <<"p50_ms">>],
                description =>
                    <<"Latency percentiles in microseconds (us), not milliseconds (ms)">>},
          memory =>
              #{scopes =>
                    [<<"per_connection_heap">>,
                     <<"per_connection_total">>,
                     <<"per_node_heap">>,
                     <<"per_node_total">>,
                     <<"system_wide">>],
                units => [<<"mib">>, <<"gib">>],
                required_format => <<"memory_{type}_{unit}_{scope}">>,
                examples => [<<"memory_heap_mib_per_conn">>, <<"memory_rss_mib_per_node">>]},
          required_fields =>
              [{workload_id, <<"Workload identifier for traceability">>},
               {transport, <<"Transport type (stdio, tcp, http, websocket)">>},
               {duration_s, <<"Duration of benchmark in seconds">>},
               {scope, <<"Measurement scope (per_connection, per_node)">>},
               {precision, <<"Precision level or measurement methodology">>}]},
    {ok, #state{validation_rules = Rules, violation_count = 0}}.

handle_call(get_validation_rules, _From, State) ->
    {reply, State#state.validation_rules, State};
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

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Format violation for error reporting
-spec format_violation(violation()) -> binary().
format_violation(#{type := Type, field := Field} = Violation) ->
    Reason = maps:get(reason, Violation, <<"Unit validation failed">>),
    Expected = maps:get(expected, Violation, <<>>),
    case Expected of
        <<>> ->
            <<Type/binary, ": ", Field/binary, " - ", Reason/binary>>;
        _ ->
            <<Type/binary,
              ": ",
              Field/binary,
              " - ",
              Reason/binary,
              " (expected: ",
              Expected/binary,
              ")">>
    end.
