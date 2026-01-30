%%%-------------------------------------------------------------------
%%% @doc
%%% Record definitions for erlmcp_trace_analyzer
%%% @end
%%%-------------------------------------------------------------------

%% Trace analysis result record
-record(trace_analysis, {
    trace_id :: binary(),
    bottlenecks :: list(),
    performance_score :: number(),
    avg_duration :: number(),
    error_rate :: number(),
    recommendations :: list(),
    analyzed_at :: integer()
}).
