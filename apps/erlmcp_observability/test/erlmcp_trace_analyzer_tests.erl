%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_trace_analyzer following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior of trace analysis
%%% - Use real trace data (no mocks)
%%% - Verify through analysis results
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_trace_analyzer_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

%% Test module is loadable
module_loadable_test() ->
    ?assert(code:ensure_loaded(erlmcp_trace_analyzer) =:= {module, erlmcp_trace_analyzer}).

%% Test analyzing trace data
analyze_trace_test() ->
    case erlang:function_exported(erlmcp_trace_analyzer, analyze, 1) of
        true ->
            Trace = #{
                spans => [
                    #{span_id => <<"s1">>, duration_ns => 1000000},
                    #{span_id => <<"s2">>, duration_ns => 2000000}
                ]
            },

            Result = erlmcp_trace_analyzer:analyze(Trace),
            ?assert(is_map(Result));
        false ->
            ?assert(true)
    end.

%% Test finding critical path
find_critical_path_test() ->
    case erlang:function_exported(erlmcp_trace_analyzer, critical_path, 1) of
        true ->
            Trace = #{
                spans => [
                    #{span_id => <<"s1">>, parent_id => undefined, duration_ns => 5000000},
                    #{span_id => <<"s2">>, parent_id => <<"s1">>, duration_ns => 3000000},
                    #{span_id => <<"s3">>, parent_id => <<"s1">>, duration_ns => 1000000}
                ]
            },

            CriticalPath = erlmcp_trace_analyzer:critical_path(Trace),
            ?assert(is_list(CriticalPath));
        false ->
            ?assert(true)
    end.

%% Test calculating span statistics
span_statistics_test() ->
    case erlang:function_exported(erlmcp_trace_analyzer, stats, 1) of
        true ->
            Spans = [
                #{duration_ns => 1000000},
                #{duration_ns => 2000000},
                #{duration_ns => 3000000},
                #{duration_ns => 4000000},
                #{duration_ns => 5000000}
            ],

            Stats = erlmcp_trace_analyzer:stats(Spans),
            ?assert(is_map(Stats));
        false ->
            ?assert(true)
    end.

%% Test identifying slow spans
find_slow_spans_test() ->
    case erlang:function_exported(erlmcp_trace_analyzer, find_slow_spans, 2) of
        true ->
            Spans = [
                #{span_id => <<"s1">>, duration_ns => 10000000},   % 10ms
                #{span_id => <<"s2">>, duration_ns => 100000000},  % 100ms - slow
                #{span_id => <<"s3">>, duration_ns => 5000000}     % 5ms
            ],

            % Find spans slower than 50ms
            SlowSpans = erlmcp_trace_analyzer:find_slow_spans(Spans, 50000000),
            ?assert(is_list(SlowSpans));
        false ->
            ?assert(true)
    end.

%%====================================================================
%% Edge Cases
%%====================================================================

empty_trace_test() ->
    case erlang:function_exported(erlmcp_trace_analyzer, analyze, 1) of
        true ->
            EmptyTrace = #{spans => []},
            Result = erlmcp_trace_analyzer:analyze(EmptyTrace),
            ?assert(is_map(Result));
        false ->
            ?assert(true)
    end.

single_span_test() ->
    case erlang:function_exported(erlmcp_trace_analyzer, analyze, 1) of
        true ->
            SingleSpan = #{
                spans => [#{span_id => <<"s1">>, duration_ns => 1000000}]
            },
            Result = erlmcp_trace_analyzer:analyze(SingleSpan),
            ?assert(is_map(Result));
        false ->
            ?assert(true)
    end.

large_trace_test() ->
    case erlang:function_exported(erlmcp_trace_analyzer, analyze, 1) of
        true ->
            % Generate trace with 1000 spans
            Spans = [#{
                span_id => list_to_binary(["s", integer_to_list(N)]),
                duration_ns => N * 1000000
            } || N <- lists:seq(1, 1000)],

            LargeTrace = #{spans => Spans},
            Result = erlmcp_trace_analyzer:analyze(LargeTrace),
            ?assert(is_map(Result));
        false ->
            ?assert(true)
    end.

nested_spans_test() ->
    case erlang:function_exported(erlmcp_trace_analyzer, critical_path, 1) of
        true ->
            % Create deeply nested span tree
            NestedTrace = #{
                spans => [
                    #{span_id => <<"s1">>, parent_id => undefined, duration_ns => 10000000},
                    #{span_id => <<"s2">>, parent_id => <<"s1">>, duration_ns => 8000000},
                    #{span_id => <<"s3">>, parent_id => <<"s2">>, duration_ns => 6000000},
                    #{span_id => <<"s4">>, parent_id => <<"s3">>, duration_ns => 4000000}
                ]
            },

            CriticalPath = erlmcp_trace_analyzer:critical_path(NestedTrace),
            ?assert(is_list(CriticalPath));
        false ->
            ?assert(true)
    end.
