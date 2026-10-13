%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_trace_visualizer
%%%
%%% Covers:
%%%  - correlate_messages/1 — correlation by correlation_id
%%%  - calculate_duration_percentiles/1 — p50/p95/p99
%%%  - calculate_avg_call_duration (via analyze_performance/1)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_trace_visualizer_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

make_send(TS, CorrId, From, To) ->
    #{timestamp  => TS,
      event_type => send,
      data       => #{pid            => From,
                      to             => To,
                      message        => hello,
                      correlation_id => CorrId}}.

make_recv(TS, CorrId, From, To) ->
    #{timestamp  => TS,
      event_type => 'receive',
      data       => #{pid            => To,
                      from           => From,
                      correlation_id => CorrId}}.

make_call(TS, Pid, M, F, A) ->
    #{timestamp  => TS,
      event_type => call,
      data       => #{pid => Pid, module => M, function => F, arity => A}}.

make_return(TS, Pid, M, F, A) ->
    #{timestamp  => TS,
      event_type => return_from,
      data       => #{pid => Pid, module => M, function => F, arity => A}}.

%%====================================================================
%% correlate_messages/1 Tests
%%====================================================================

%% Two spans sharing a correlation_id are returned together
correlation_id_grouping_test() ->
    CorrId = <<"req-42">>,
    Pid1 = self(),
    Pid2 = list_to_pid("<0.1.0>"),
    Events = [
        make_send(1000, CorrId, Pid1, Pid2),
        make_recv(1050, CorrId, Pid1, Pid2),
        %% A second unrelated send/receive pair with a different id
        make_send(2000, <<"other">>, Pid2, Pid1),
        make_recv(2050, <<"other">>, Pid2, Pid1)
    ],
    Corr = erlmcp_trace_visualizer:correlate_messages(Events),
    %% Both correlation ids should appear as keys
    ?assert(maps:is_key(CorrId, Corr)),
    ?assert(maps:is_key(<<"other">>, Corr)).

correlation_id_matched_pair_test() ->
    CorrId = <<"tx-1">>,
    Pid1 = self(),
    Pid2 = list_to_pid("<0.1.0>"),
    Events = [
        make_send(100, CorrId, Pid1, Pid2),
        make_recv(150, CorrId, Pid1, Pid2)
    ],
    Corr = erlmcp_trace_visualizer:correlate_messages(Events),
    ?assert(map_size(Corr) >= 1),
    %% The matched correlation should carry from/to/send_time/receive_time
    Matched = maps:get(CorrId, Corr, undefined),
    ?assertNotEqual(undefined, Matched),
    ?assert(maps:is_key(from, Matched)),
    ?assert(maps:is_key(to, Matched)),
    ?assert(maps:is_key(send_time, Matched)),
    ?assert(maps:is_key('receive_time', Matched)).

correlation_empty_test() ->
    Corr = erlmcp_trace_visualizer:correlate_messages([]),
    ?assertEqual(#{}, Corr).

%%====================================================================
%% calculate_duration_percentiles/1 Tests
%%====================================================================

%% Synthetic dataset: 100 call events with known durations.
%% Durations: 1..100 microseconds.
%% p50 ≈ 50, p95 ≈ 95, p99 ≈ 99.
duration_percentiles_synthetic_test() ->
    Pid = self(),
    %% Build call + return pairs so extract_durations finds durations
    CallEvents = [#{timestamp  => I * 1000,
                    event_type => call,
                    data       => #{pid => Pid, module => m, function => f,
                                    arity => 0,
                                    return_timestamp => I * 1000 + I}}
                  || I <- lists:seq(1, 100)],

    Percentiles = erlmcp_trace_visualizer:calculate_duration_percentiles(CallEvents),

    P50 = maps:get(p50, Percentiles),
    P95 = maps:get(p95, Percentiles),
    P99 = maps:get(p99, Percentiles),

    %% All percentiles must be within the dataset range [1, 100]
    ?assert(P50 >= 1.0),
    ?assert(P50 =< 100.0),
    ?assert(P95 >= P50),
    ?assert(P99 >= P95).

duration_percentiles_empty_test() ->
    Percentiles = erlmcp_trace_visualizer:calculate_duration_percentiles([]),
    ?assertEqual(#{p50 => 0, p75 => 0, p90 => 0, p95 => 0, p99 => 0}, Percentiles).

duration_percentiles_ordering_test() ->
    %% Ensure p50 <= p95 <= p99 for a random-ish dataset
    Pid = self(),
    Events = [#{timestamp  => N * 100,
                event_type => call,
                data       => #{pid => Pid, module => m, function => f,
                                arity => 0,
                                return_timestamp => N * 100 + (N * N rem 200)}}
              || N <- lists:seq(1, 50)],
    Ps = erlmcp_trace_visualizer:calculate_duration_percentiles(Events),
    ?assert(maps:get(p50, Ps) =< maps:get(p95, Ps)),
    ?assert(maps:get(p95, Ps) =< maps:get(p99, Ps)).

%%====================================================================
%% analyze_performance / calculate_avg_call_duration Tests
%%====================================================================

avg_call_duration_with_returns_test() ->
    %% Build two call/return pairs with known durations: 100us and 300us
    Pid = self(),
    Events = [
        make_call(1000, Pid, test_mod, test_fun, 0),
        make_return(1100, Pid, test_mod, test_fun, 0),  %% duration = 100
        make_call(2000, Pid, test_mod, other_fun, 1),
        make_return(2300, Pid, test_mod, other_fun, 1)   %% duration = 300
    ],
    Perf = erlmcp_trace_visualizer:analyze_performance(Events),
    Avg = maps:get(avg_call_duration_us, Perf),
    %% Average of 100 and 300 = 200; allow small float rounding
    ?assert(Avg >= 199.0),
    ?assert(Avg =< 201.0).

avg_call_duration_no_returns_test() ->
    %% No return_from events → average should be 0.0
    Pid = self(),
    Events = [make_call(1000, Pid, m, f, 0)],
    Perf = erlmcp_trace_visualizer:analyze_performance(Events),
    ?assertEqual(0.0, maps:get(avg_call_duration_us, Perf)).
