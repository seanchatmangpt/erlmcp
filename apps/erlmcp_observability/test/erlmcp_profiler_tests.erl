%%%-------------------------------------------------------------------
%%% @doc
%%% Test suite for Timeline Profiler (OTP 26-27)
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls only
%%% - Use REAL gen_server processes (no mocks, no fakes)
%%% - NO internal state inspection (test API boundaries only)
%%% - NO record duplication (respect encapsulation)
%%% - Test what system DOES (outputs), not HOW it does it (internals)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_profiler_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

%% @doc Test basic timeline profiling
profile_timeline_test_() ->
    {setup,
     fun setup_profiler/0,
     fun cleanup_profiler/1,
     fun(_) ->
         [
          ?_test(profile_simple_function()),
          ?_test(profile_function_with_error()),
          ?_test(profile_function_with_options()),
          ?_test(profile_tool_call()),
          ?_test(profile_session_request())
         ]
     end}.

%% @doc Test timeline visualization
timeline_viz_test_() ->
    {setup,
     fun setup_profiler/0,
     fun cleanup_profiler/1,
     fun(_) ->
         [
          ?_test(generate_svg()),
          ?_test(generate_html()),
          ?_test(export_json()),
          ?_test(export_csv())
         ]
     end}.

%% @doc Test profile aggregation and comparison
profile_analysis_test_() ->
    {setup,
     fun setup_profiler/0,
     fun cleanup_profiler/1,
     fun(_) ->
         [
          ?_test(aggregate_profiles()),
          ?_test(compare_profiles()),
          ?_test(empty_profiles())
         ]
     end}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup_profiler() ->
    {ok, Pid} = erlmcp_profiler:start_link(),
    Pid.

cleanup_profiler(_Pid) ->
    erlmcp_profiler:stop().

%%====================================================================
%% Test Functions
%%====================================================================

%% @private Test profiling a simple function
profile_simple_function() ->
    Fun = fun() -> lists:sum(lists:seq(1, 100)) end,
    {ok, Result, Timeline} = erlmcp_profiler:profile_timeline(Fun, <<"simple_sum">>),

    ?assertEqual(5050, Result),
    ?assert(maps:is_key(profile_id, Timeline)),
    ?assert(maps:is_key(start_time, Timeline)),
    ?assert(maps:is_key(end_time, Timeline)),
    ?assert(maps:is_key(total_duration_us, Timeline)),
    ?assert(maps:is_key(events, Timeline)),
    ?assert(maps:is_key(statistics, Timeline)),

    %% Verify duration is positive
    DurationUs = maps:get(total_duration_us, Timeline),
    ?assert(DurationUs > 0).

%% @private Test profiling function with error
profile_function_with_error() ->
    Fun = fun() -> error(intentional_error) end,

    try
        erlmcp_profiler:profile_timeline(Fun, <<"error_test">>),
        ?assert(false)  %% Should not reach here
    catch
        error:intentional_error ->
            ok
    end.

%% @private Test profiling with custom options
profile_function_with_options() ->
    Fun = fun() -> timer:sleep(10) end,
    Opts = #{include_scheduler => true,
             include_gc => false,
             include_ports => false,
             max_events => 100},

    {ok, ok, Timeline} = erlmcp_profiler:profile_function(Fun, <<"sleep_test">>, Opts),

    %% Verify timeline was created
    ?assert(maps:is_key(events, Timeline)),
    Events = maps:get(events, Timeline, []),
    ?assert(is_list(Events)).

%% @private Test profiling tool call (mock)
profile_tool_call() ->
    %% Create a mock tool call
    Fun = fun() ->
                  %% Simulate tool execution
                  timer:sleep(5),
                  {ok, #{result => <<"mock_result">>}}
          end,

    {ok, Result, Timeline} = erlmcp_profiler:profile_function(
        Fun,
        <<"tool.mock.execute">>,
        #{}
    ),

    ?assertMatch({ok, #{result := <<"mock_result">>}}, Result),
    ?assert(maps:is_key(total_duration_us, Timeline)).

%% @private Test profiling session request
profile_session_request() ->
    SessionId = <<"test_session_123">>,
    Request = #{action => test},

    {ok, Result, Timeline} = erlmcp_profiler:profile_session_request(SessionId, Request),

    ?assertMatch({ok, #{status := processed}}, Result),
    ?assert(maps:is_key(label, Timeline)),
    ?assert(<< <<"session.", SessionId/binary>> >> =:= maps:get(label, Timeline)).

%% @private Test SVG generation
generate_svg() ->
    Timeline = create_mock_timeline(),
    {ok, SVG} = erlmcp_timeline_viz:generate_svg(Timeline),

    ?assert(is_binary(SVG)),
    ?assert(<<"<?xml version=\"1.0\"">> =< SVG),
    ?assert(<<"<svg">> =< SVG),
    ?assert(<<"</svg>">> =< SVG).

%% @private Test HTML generation
generate_html() ->
    Timeline = create_mock_timeline(),
    {ok, HTML} = erlmcp_timeline_viz:generate_html(Timeline),

    ?assert(is_binary(HTML)),
    ?assert(<<"<!DOCTYPE html>">> =< HTML),
    ?assert(<<"<html>">> =< HTML),
    ?assert(<<"</html>">> =< HTML),
    ?assert(<<"Timeline Profile">> =< HTML).

%% @private Test JSON export
export_json() ->
    Timeline = create_mock_timeline(),
    {ok, JSON} = erlmcp_timeline_viz:export_json(Timeline),

    ?assert(is_binary(JSON)),
    %% Verify it's valid JSON by decoding
    Decoded = jsx:decode(JSON, [return_maps]),
    ?assert(maps:is_key(<<"profile_id">>, Decoded)).

%% @private Test CSV export
export_csv() ->
    Timeline = create_mock_timeline(),
    {ok, CSV} = erlmcp_timeline_viz:export_csv(Timeline),

    ?assert(is_binary(CSV)),
    %% Verify CSV header
    Lines = binary:split(CSV, <<"\n">>, [global]),
    [Header | _] = Lines,
    ?assert(<<"timestamp,type,duration_us,relative_time_us">> =< Header).

%% @private Test profile aggregation
aggregate_profiles() ->
    Profile1 = create_mock_timeline(),
    Profile2 = create_mock_timeline(),

    {ok, Aggregated} = erlmcp_profiler:aggregate_profiles([Profile1, Profile2]),

    ?assert(maps:is_key(profile_id, Aggregated)),
    ?assert(maps:is_key(label, Aggregated)),
    ?assert(<<"aggregate_2_profiles">> =:= maps:get(label, Aggregated)),

    Stats = maps:get(statistics, Aggregated),
    ?assert(maps:is_key(profile_count, Stats)),
    ?assertEqual(2, maps:get(profile_count, Stats)).

%% @private Test profile comparison
compare_profiles() ->
    Profile1 = create_mock_timeline(#{total_duration_us => 1000}),
    Profile2 = create_mock_timeline(#{total_duration_us => 900}),  %% 10% faster

    Diff = erlmcp_profiler:compare_profiles(Profile1, Profile2),

    ?assert(maps:is_key(baseline, Diff)),
    ?assert(maps:is_key(comparison, Diff)),
    ?assert(maps:is_key(diff, Diff)),

    DiffMap = maps:get(diff, Diff),
    ?assert(maps:is_key(duration_us_diff, DiffMap)),
    ?assert(maps:is_key(duration_percent_change, DiffMap)),
    ?assert(maps:is_key(faster, DiffMap)),
    ?assertEqual(true, maps:get(faster, DiffMap)).

%% @private Test empty profiles
empty_profiles() ->
    ?assertEqual({error, no_profiles},
                 erlmcp_profiler:aggregate_profiles([])).

%%====================================================================
%% Advanced Profiling Tests
%%====================================================================

%% @doc Test transport profiling
profile_transport_test_() ->
    {setup,
     fun setup_profiler/0,
     fun cleanup_profiler/1,
     fun(_) ->
         [
          ?_test(profile_stdio_transport()),
          ?_test(profile_tcp_transport()),
          ?_test(profile_http_transport())
         ]
     end}.

%% @private Test stdio transport profiling
profile_stdio_transport() ->
    {ok, Result, Timeline} = erlmcp_profiler:profile_transport(<<"stdio">>, 1024),

    ?assertMatch({ok, _}, Result),
    ?assert(maps:is_key(total_duration_us, Timeline)),
    ?assert(maps:is_key(label, Timeline)),
    ?assert(<<"transport.stdio">> =:= maps:get(label, Timeline)).

%% @private Test tcp transport profiling
profile_tcp_transport() ->
    {ok, Result, Timeline} = erlmcp_profiler:profile_transport(<<"tcp">>, 2048),

    ?assertMatch({ok, _}, Result),
    ?assert(maps:is_key(events, Timeline)),
    Events = maps:get(events, Timeline, []),
    ?assert(is_list(Events)).

%% @private Test http transport profiling
profile_http_transport() ->
    {ok, Result, Timeline} = erlmcp_profiler:profile_transport(<<"http">>, 4096),

    ?assertMatch({ok, _}, Result),
    ?assert(maps:is_key(statistics, Timeline)).

%%====================================================================
%% Timeline Visualization Tests
%%====================================================================

%% @doc Test advanced visualization features
timeline_viz_advanced_test_() ->
    {setup,
     fun setup_profiler/0,
     fun cleanup_profiler/1,
     fun(_) ->
         [
          ?_test(generate_svg_with_custom_opts()),
          ?_test(generate_html_with_custom_opts()),
          ?_test(generate_flamegraph()),
          ?_test(generate_interaction_graph()),
          ?_test(export_json_with_complex_timeline()),
          ?_test(export_csv_with_multiple_events())
         ]
     end}.

%% @private Test SVG generation with custom options
generate_svg_with_custom_opts() ->
    Timeline = create_complex_timeline(),
    Opts = #{width => 1600, height => 800, show_grid => true, show_legend => false},

    {ok, SVG} = erlmcp_timeline_viz:generate_svg(Timeline, Opts),

    ?assert(is_binary(SVG)),
    ?assert(<<"<?xml version=\"1.0\"">> =< SVG),
    ?assert(<<"<svg">> =< SVG),
    ?assert(<<"width=\"1600\"">> =< SVG),
    ?assert(<<"height=\"800\"">> =< SVG).

%% @private Test HTML generation with custom options
generate_html_with_custom_opts() ->
    Timeline = create_complex_timeline(),
    Opts = #{width => 1400, height => 700, zoom_level => 1.5},

    {ok, HTML} = erlmcp_timeline_viz:generate_html(Timeline, Opts),

    ?assert(is_binary(HTML)),
    ?assert(<<"<!DOCTYPE html>">> =< HTML),
    ?assert(<<"Timeline Profile:">> =< HTML),
    ?assert(<<"zoomIn()">> =< HTML),
    ?assert(<<"zoomOut()">> =< HTML).

%% @private Test flamegraph generation
generate_flamegraph() ->
    Timeline = create_complex_timeline(),

    Result = erlmcp_timeline_viz:generate_flamegraph(Timeline),

    %% Flamegraph is a placeholder, so it should succeed
    ?assertMatch({ok, _}, Result).

%% @private Test interaction graph generation
generate_interaction_graph() ->
    Timeline = create_complex_timeline(),

    Result = erlmcp_timeline_viz:generate_interaction_graph(Timeline),

    %% Interaction graph is a placeholder, so it should succeed
    ?assertMatch({ok, _}, Result).

%% @private Test JSON export with complex timeline
export_json_with_complex_timeline() ->
    Timeline = create_complex_timeline(),
    {ok, JSON} = erlmcp_timeline_viz:export_json(Timeline),

    ?assert(is_binary(JSON)),
    %% Verify it's valid JSON by decoding
    Decoded = jsx:decode(JSON, [return_maps]),
    ?assert(maps:is_key(<<"profile_id">>, Decoded)),
    ?assert(maps:is_key(<<"events">>, Decoded)),
    ?assert(maps:is_key(<<"statistics">>, Decoded)).

%% @private Test CSV export with multiple events
export_csv_with_multiple_events() ->
    Timeline = create_complex_timeline(),
    {ok, CSV} = erlmcp_timeline_viz:export_csv(Timeline),

    ?assert(is_binary(CSV)),
    %% Verify CSV header
    Lines = binary:split(CSV, <<"\n">>, [global, trim_all]),
    [Header | Rows] = Lines,
    ?assert(<<"timestamp,type,duration_us,relative_time_us">> =< Header),

    %% Verify we have multiple data rows
    ?assert(length(Rows) >= 3).

%%====================================================================
%% Profile Aggregation Tests
%%====================================================================

%% @doc Test profile aggregation with various scenarios
profile_aggregation_advanced_test_() ->
    {setup,
     fun setup_profiler/0,
     fun cleanup_profiler/1,
     fun(_) ->
         [
          ?_test(aggregate_many_profiles()),
          ?_test(aggregate_with_different_durations()),
          ?_test(aggregate_with_different_event_counts()),
          ?_test(aggregate_maintains_timeline_order())
         ]
     end}.

%% @private Test aggregating many profiles
aggregate_many_profiles() ->
    Profiles = [create_mock_timeline(#{total_duration_us => 1000 + N * 100})
                || N <- lists:seq(1, 10)],

    {ok, Aggregated} = erlmcp_profiler:aggregate_profiles(Profiles),

    ?assert(maps:is_key(profile_id, Aggregated)),
    ?assert(maps:is_key(label, Aggregated)),
    ?assert(<<"aggregate_10_profiles">> =:= maps:get(label, Aggregated)),

    Stats = maps:get(statistics, Aggregated),
    ?assertEqual(10, maps:get(profile_count, Stats)),

    %% Verify all events are merged
    TotalEvents = lists:foldl(fun(P, Acc) ->
        length(maps:get(events, P, [])) + Acc
    end, 0, Profiles),
    AggregatedEvents = length(maps:get(events, Aggregated, [])),
    ?assertEqual(TotalEvents, AggregatedEvents).

%% @private Test aggregation with different durations
aggregate_with_different_durations() ->
    Profile1 = create_mock_timeline(#{total_duration_us => 1000}),
    Profile2 = create_mock_timeline(#{total_duration_us => 2000}),
    Profile3 = create_mock_timeline(#{total_duration_us => 3000}),

    {ok, Aggregated} = erlmcp_profiler:aggregate_profiles([Profile1, Profile2, Profile3]),

    %% Verify duration is sum of all
    TotalDuration = maps:get(total_duration_us, Aggregated),
    ?assertEqual(6000, TotalDuration).

%% @private Test aggregation with different event counts
aggregate_with_different_event_counts() ->
    Events1 = [create_event(scheduler, 100)],
    Events2 = [create_event(process, 200), create_event(gc, 300)],
    Events3 = [create_event(scheduler, 400), create_event(process, 500), create_event(port, 600)],

    Profile1 = create_mock_timeline(#{events => Events1}),
    Profile2 = create_mock_timeline(#{events => Events2}),
    Profile3 = create_mock_timeline(#{events => Events3}),

    {ok, Aggregated} = erlmcp_profiler:aggregate_profiles([Profile1, Profile2, Profile3]),

    %% Verify all events are present
    AggregatedEvents = maps:get(events, Aggregated, []),
    ?assertEqual(6, length(AggregatedEvents)).

%% @private Test aggregation maintains timeline order
aggregate_maintains_timeline_order() ->
    Now = erlang:system_time(microsecond),

    Events1 = [create_event_with_time(scheduler, Now + 100)],
    Events2 = [create_event_with_time(process, Now + 200)],
    Events3 = [create_event_with_time(gc, Now + 300)],

    Profile1 = create_mock_timeline(#{events => Events1, start_time => Now}),
    Profile2 = create_mock_timeline(#{events => Events2, start_time => Now}),
    Profile3 = create_mock_timeline(#{events => Events3, start_time => Now}),

    {ok, Aggregated} = erlmcp_profiler:aggregate_profiles([Profile3, Profile1, Profile2]),

    %% Verify events are sorted by timestamp
    AggregatedEvents = maps:get(events, Aggregated, []),
    [First, Second, Third] = AggregatedEvents,
    ?assertEqual(scheduler, maps:get(type, First)),
    ?assertEqual(process, maps:get(type, Second)),
    ?assertEqual(gc, maps:get(type, Third)).

%%====================================================================
%% Profile Comparison Tests
%%====================================================================

%% @doc Test profile comparison scenarios
profile_comparison_advanced_test_() ->
    {setup,
     fun setup_profiler/0,
     fun cleanup_profiler/1,
     fun(_) ->
         [
          ?_test(compare_faster_profile()),
          ?_test(compare_slower_profile()),
          ?_test(compare_with_same_duration()),
          ?_test(compare_with_event_count_diff()),
          ?_test(compare_detects_regression())
         ]
     end}.

%% @private Test comparison with faster profile
compare_faster_profile() ->
    Profile1 = create_mock_timeline(#{total_duration_us => 1000}),
    Profile2 = create_mock_timeline(#{total_duration_us => 800}),  %% 20% faster

    Diff = erlmcp_profiler:compare_profiles(Profile1, Profile2),

    ?assert(maps:is_key(baseline, Diff)),
    ?assert(maps:is_key(comparison, Diff)),
    ?assert(maps:is_key(diff, Diff)),

    DiffMap = maps:get(diff, Diff),
    ?assertEqual(true, maps:get(faster, DiffMap)),
    ?assertEqual(-200, maps:get(duration_us_diff, DiffMap)),

    %% Verify percentage calculation
    PercentChange = maps:get(duration_percent_change, DiffMap),
    ?assert(PercentChange < 0).  %% Negative means faster

%% @private Test comparison with slower profile
compare_slower_profile() ->
    Profile1 = create_mock_timeline(#{total_duration_us => 1000}),
    Profile2 = create_mock_timeline(#{total_duration_us => 1200}),  %% 20% slower

    Diff = erlmcp_profiler:compare_profiles(Profile1, Profile2),

    DiffMap = maps:get(diff, Diff),
    ?assertEqual(false, maps:get(faster, DiffMap)),
    ?assertEqual(200, maps:get(duration_us_diff, DiffMap)),

    %% Verify percentage calculation
    PercentChange = maps:get(duration_percent_change, DiffMap),
    ?assert(PercentChange > 0).  %% Positive means slower

%% @private Test comparison with same duration
compare_with_same_duration() ->
    Profile1 = create_mock_timeline(#{total_duration_us => 1000}),
    Profile2 = create_mock_timeline(#{total_duration_us => 1000}),

    Diff = erlmcp_profiler:compare_profiles(Profile1, Profile2),

    DiffMap = maps:get(diff, Diff),
    ?assertEqual(0, maps:get(duration_us_diff, DiffMap)),
    ?assertEqual(0.0, maps:get(duration_percent_change, DiffMap)).

%% @private Test comparison with event count difference
compare_with_event_count_diff() ->
    Events1 = [create_event(scheduler, 100)],
    Events2 = [create_event(scheduler, 100), create_event(process, 200)],

    Profile1 = create_mock_timeline(#{events => Events1}),
    Profile2 = create_mock_timeline(#{events => Events2}),

    Diff = erlmcp_profiler:compare_profiles(Profile1, Profile2),

    DiffMap = maps:get(diff, Diff),
    ?assertEqual(1, maps:get(event_count_diff, DiffMap)).

%% @private Test comparison detects regression
compare_detects_regression() ->
    %% Baseline: Fast execution
    Baseline = create_mock_timeline(#{total_duration_us => 1000}),
    %% Current: Slow execution (regression)
    Current = create_mock_timeline(#{total_duration_us => 1500}),  %% 50% slower

    Diff = erlmcp_profiler:compare_profiles(Baseline, Current),

    DiffMap = maps:get(diff, Diff),
    ?assertEqual(false, maps:get(faster, DiffMap)),
    ?assertEqual(500, maps:get(duration_us_diff, DiffMap)),

    %% Check if regression exceeds threshold (e.g., 10%)
    PercentChange = maps:get(duration_percent_change, DiffMap),
    ?assert(PercentChange > 10.0).

%%====================================================================
%% Error Handling Tests
%%====================================================================

%% @doc Test error handling in profiler operations
profiler_error_handling_test_() ->
    {setup,
     fun setup_profiler/0,
     fun cleanup_profiler/1,
     fun(_) ->
         [
          ?_test(profile_with_invalid_function()),
          ?_test(viz_with_invalid_timeline()),
          ?_test(aggregate_with_invalid_profile()),
          ?_test(compare_with_invalid_profiles())
         ]
     end}.

%% @private Test profiling with invalid function
profile_with_invalid_function() ->
    %% Not a function
    ?assertError(_, erlmcp_profiler:profile_timeline(not_a_function, <<"test">>)).

%% @private Test visualization with invalid timeline
viz_with_invalid_timeline() ->
    %% Missing required fields
    InvalidTimeline = #{},

    Result = erlmcp_timeline_viz:generate_svg(InvalidTimeline),
    ?assertMatch({error, _}, Result).

%% @private Test aggregation with invalid profile
aggregate_with_invalid_profile() ->
    InvalidProfile = #{invalid => data},

    Result = erlmcp_profiler:aggregate_profiles([InvalidProfile]),
    ?assertMatch({error, _}, Result).

%% @private Test comparison with invalid profiles
compare_with_invalid_profiles() ->
    Profile1 = #{invalid => data1},
    Profile2 = #{invalid => data2},

    %% Should not crash, but return comparison with zeros
    Diff = erlmcp_profiler:compare_profiles(Profile1, Profile2),
    ?assert(maps:is_key(diff, Diff)).

%%====================================================================
%% Performance Tests
%%====================================================================

%% @doc Test performance characteristics
profiler_performance_test_() ->
    {setup,
     fun setup_profiler/0,
     fun cleanup_profiler/1,
     fun(_) ->
         [
          ?_test(profile_overhead_minimal()),
          ?_test(aggregation_scales_linearly()),
          ?_test(visualization_fast_for_large_timelines())
         ]
     end}.

%% @private Test profiling overhead is minimal
profile_overhead_minimal() ->
    %% Measure without profiling
    {T1, _} = timer:tc(fun() ->
        lists:sum(lists:seq(1, 1000))
    end),

    %% Measure with profiling
    {T2, {ok, _Result, _Timeline}} = timer:tc(fun() ->
        erlmcp_profiler:profile_timeline(
            fun() -> lists:sum(lists:seq(1, 1000)) end,
            <<"overhead_test">>
        )
    end),

    %% Profiling overhead should be reasonable (< 10x)
    Overhead = T2 / T1,
    ?assert(Overhead < 10.0).

%% @private Test aggregation scales linearly
aggregation_scales_linearly() ->
    Profiles10 = [create_mock_timeline() || _ <- lists:seq(1, 10)],
    Profiles100 = [create_mock_timeline() || _ <- lists:seq(1, 100)],

    {T10, {ok, _}} = timer:tc(fun() ->
        erlmcp_profiler:aggregate_profiles(Profiles10)
    end),

    {T100, {ok, _}} = timer:tc(fun() ->
        erlmcp_profiler:aggregate_profiles(Profiles100)
    end),

    %% 10x profiles should take roughly 10x time (within factor of 2)
    Ratio = T100 / T10,
    ?assert(Ratio < 20).  %% Allow 2x slop factor

%% @private Test visualization is fast for large timelines
visualization_fast_for_large_timelines() ->
    %% Create timeline with many events
    Events = [create_event(scheduler, N * 100) || N <- lists:seq(1, 1000)],
    LargeTimeline = create_mock_timeline(#{events => Events}),

    {T, {ok, _SVG}} = timer:tc(fun() ->
        erlmcp_timeline_viz:generate_svg(LargeTimeline)
    end),

    %% Should complete in reasonable time (< 1 second)
    ?assert(T < 1000000).  %% 1 second in microseconds

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Create a mock timeline for testing
-spec create_mock_timeline() -> map().
create_mock_timeline() ->
    create_mock_timeline(#{}).

%% @private Create a mock timeline with custom fields
-spec create_mock_timeline(map()) -> map().
create_mock_timeline(Overrides) ->
    Now = erlang:system_time(microsecond),
    Events = [
        #{type => scheduler,
          timestamp => Now,
          duration_us => 100,
          details => #{mfa => <<"erlang:apply/3">>}},
        #{type => process,
          timestamp => Now + 100,
          duration_us => 50,
          details => #{mfa => <<"lists:seq/2">>}},
        #{type => gc,
          timestamp => Now + 200,
          duration_us => 25,
          details => #{reason => gc}}
    ],

    BaseTimeline = #{
        profile_id => <<"test_profile_123">>,
        label => <<"test_profile">>,
        start_time => Now,
        end_time => Now + 1000,
        total_duration_us => 1000,
        events => Events,
        statistics => #{
            total_duration_us => 1000,
            event_counts => #{scheduler => 1, process => 1, gc => 1},
            event_count => 3
        }
    },

    maps:merge(BaseTimeline, Overrides).

%% @private Create a complex timeline for testing
-spec create_complex_timeline() -> map().
create_complex_timeline() ->
    Now = erlang:system_time(microsecond),
    Events = [
        #{type => scheduler,
          timestamp => Now,
          duration_us => 100,
          details => #{mfa => <<"erlang:apply/3">>}},
        #{type => process,
          timestamp => Now + 100,
          duration_us => 150,
          details => #{mfa => <<"lists:seq/2">>}},
        #{type => gc,
          timestamp => Now + 250,
          duration_us => 50,
          details => #{reason => gc}},
        #{type => port,
          timestamp => Now + 300,
          duration_us => 75,
          details => #{action => port_operation}},
        #{type => process,
          timestamp => Now + 400,
          duration_us => 200,
          details => #{mfa => <<"jsx:encode/2">>}},
        #{type => scheduler,
          timestamp => Now + 600,
          duration_us => 80,
          details => #{mfa => <<"erlang:spawn/1">>}}
    ],

    #{
        profile_id => <<"complex_profile_456">>,
        label => <<"complex_test_profile">>,
        start_time => Now,
        end_time => Now + 1000,
        total_duration_us => 1000,
        events => Events,
        statistics => #{
            total_duration_us => 1000,
            event_counts => #{scheduler => 2, process => 2, gc => 1, port => 1},
            event_count => 6
        }
    }.

%% @private Create a single event
-spec create_event(atom(), integer()) -> map().
create_event(Type, Timestamp) ->
    create_event_with_time(Type, erlang:system_time(microsecond) + Timestamp).

%% @private Create an event with specific timestamp
-spec create_event_with_time(atom(), integer()) -> map().
create_event_with_time(Type, Timestamp) ->
    #{
        type => Type,
        timestamp => Timestamp,
        duration_us => 100,
        details => #{mfa => <<"test:function/0">>}
    }.
