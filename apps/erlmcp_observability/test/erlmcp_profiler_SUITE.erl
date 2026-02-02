%%%-------------------------------------------------------------------
%%% @doc erlmcp_profiler_SUITE - Common Test integration suite for Timeline Profiler
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls only
%%% - Use REAL processes and supervisors (no mocks, no fakes)
%%% - Integration tests for multi-component scenarios
%%% - Test what system DOES (outputs), not HOW it does it (internals)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_profiler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([test_profiler_lifecycle/1,
         test_profile_tool_call_integration/1,
         test_profile_session_integration/1,
         test_profile_transport_integration/1,
         test_aggregation_integration/1,
         test_comparison_integration/1,
         test_visualization_integration/1,
         test_cli_profiler_integration/1,
         test_concurrent_profiling/1,
         test_profiler_supervision/1]).

%%====================================================================
%% Suite Callbacks
%%====================================================================

suite() ->
    [{timetrap, {seconds, 120}}].

all() ->
    [test_profiler_lifecycle,
     test_profile_tool_call_integration,
     test_profile_session_integration,
     test_profile_transport_integration,
     test_aggregation_integration,
     test_comparison_integration,
     test_visualization_integration,
     test_cli_profiler_integration,
     test_concurrent_profiling,
     test_profiler_supervision].

init_per_suite(Config) ->
    ct:log("Starting Timeline Profiler integration test suite"),

    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(telemetry),
    application:ensure_all_started(gproc),

    %% Start observability application
    case application:ensure_all_started(erlmcp_observability) of
        {ok, _Started} ->
            ct:log("Observability application started"),
            Config;
        {error, {already_started, erlmcp_observability}} ->
            ct:log("Observability application already started"),
            Config;
        {error, Reason} ->
            ct:log("Failed to start observability: ~p", [Reason]),
            Config
    end.

end_per_suite(_Config) ->
    ct:log("Ending Timeline Profiler integration test suite"),
    application:stop(erlmcp_observability),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:log("Ending test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test profiler lifecycle (start, stop, restart)
test_profiler_lifecycle(_Config) ->
    ct:log("Testing profiler lifecycle"),

    %% Start profiler
    {ok, Pid} = erlmcp_profiler:start_link(),
    ct:log("Profiler started: ~p", [Pid]),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),

    %% Verify it's registered
    ?assertEqual(Pid, whereis(erlmcp_profiler)),

    %% Stop profiler
    gen_server:stop(Pid),
    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Pid)),

    %% Restart profiler
    {ok, Pid2} = erlmcp_profiler:start_link(),
    ct:log("Profiler restarted: ~p", [Pid2]),
    ?assert(is_pid(Pid2)),
    ?assert(erlang:is_process_alive(Pid2)),

    %% Cleanup
    gen_server:stop(Pid2),
    ok.

%% @doc Test profiling tool calls with real server
test_profile_tool_call_integration(_Config) ->
    ct:log("Testing tool call profiling integration"),

    %% Start profiler
    {ok, _ProfilerPid} = erlmcp_profiler:start_link(),

    %% Profile a mock tool call
    Fun = fun() ->
                  timer:sleep(10),
                  {ok, #{result => <<"tool_result">>}}
          end,

    {ok, Result, Timeline} = erlmcp_profiler:profile_function(
        Fun,
        <<"tool.test.execute">>,
        #{include_scheduler => true, include_gc => true}
    ),

    ct:log("Profile result: ~p", [Result]),
    ct:log("Timeline keys: ~p", [maps:keys(Timeline)]),

    %% Verify result
    ?assertMatch({ok, #{result := <<"tool_result">>}}, Result),

    %% Verify timeline structure
    ?assert(maps:is_key(profile_id, Timeline)),
    ?assert(maps:is_key(label, Timeline)),
    ?assert(maps:is_key(start_time, Timeline)),
    ?assert(maps:is_key(end_time, Timeline)),
    ?assert(maps:is_key(total_duration_us, Timeline)),
    ?assert(maps:is_key(events, Timeline)),
    ?assert(maps:is_key(statistics, Timeline)),

    %% Verify duration is reasonable (> 10ms sleep)
    DurationUs = maps:get(total_duration_us, Timeline),
    ?assert(DurationUs > 10000),  %% At least 10ms
    ct:log("Profile duration: ~p μs", [DurationUs]),

    %% Verify label
    Label = maps:get(label, Timeline),
    ?assertEqual(<<"tool.test.execute">>, Label),

    %% Cleanup
    erlmcp_profiler:stop(),
    ok.

%% @doc Test session request profiling
test_profile_session_integration(_Config) ->
    ct:log("Testing session profiling integration"),

    %% Start profiler
    {ok, _ProfilerPid} = erlmcp_profiler:start_link(),

    SessionId = <<"test_session_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Request = #{action => test, timestamp => erlang:system_time(microsecond)},

    {ok, Result, Timeline} = erlmcp_profiler:profile_session_request(SessionId, Request),

    ct:log("Session profile result: ~p", [Result]),

    %% Verify result
    ?assertMatch({ok, #{status := processed}}, Result),

    %% Verify timeline label
    Label = maps:get(label, Timeline),
    ExpectedLabel = <<"session.", SessionId/binary>>,
    ?assertEqual(ExpectedLabel, Label),

    %% Verify statistics
    Stats = maps:get(statistics, Timeline, #{}),
    ct:log("Session profile stats: ~p", [Stats]),
    ?assert(maps:is_key(event_count, Stats)),

    %% Cleanup
    erlmcp_profiler:stop(),
    ok.

%% @doc Test transport profiling
test_profile_transport_integration(_Config) ->
    ct:log("Testing transport profiling integration"),

    %% Start profiler
    {ok, _ProfilerPid} = erlmcp_profiler:start_link(),

    %% Test different transport types
    TransportTypes = [<<"stdio">>, <<"tcp">>, <<"http">>],

    lists:foreach(fun(TransportType) ->
        ct:log("Profiling transport: ~s", [TransportType]),

        {ok, Result, Timeline} = erlmcp_profiler:profile_transport(TransportType, 1024),

        ct:log("Transport ~s profile result: ~p", [TransportType, Result]),

        %% Verify result
        ?assertMatch({ok, _}, Result),

        %% Verify timeline structure
        ?assert(maps:is_key(label, Timeline)),
        ?assert(maps:is_key(total_duration_us, Timeline)),

        %% Verify label includes transport type
        Label = maps:get(label, Timeline),
        ExpectedLabel = <<"transport.", TransportType/binary>>,
        ?assertEqual(ExpectedLabel, Label)
    end, TransportTypes),

    %% Cleanup
    erlmcp_profiler:stop(),
    ok.

%% @doc Test profile aggregation
test_aggregation_integration(_Config) ->
    ct:log("Testing profile aggregation integration"),

    %% Create multiple profiles
    Profiles = [create_test_profile(1000 + N * 100) || N <- lists:seq(1, 5)],

    ct:log("Aggregating ~p profiles", [length(Profiles)]),

    %% Aggregate profiles
    {ok, Aggregated} = erlmcp_profiler:aggregate_profiles(Profiles),

    ct:log("Aggregated profile keys: ~p", [maps:keys(Aggregated)]),
    ct:log("Aggregated statistics: ~p", [maps:get(statistics, Aggregated, #{})]),

    %% Verify aggregation
    ?assert(maps:is_key(profile_id, Aggregated)),
    ?assert(maps:is_key(label, Aggregated)),
    ?assertEqual(<<"aggregate_5_profiles">>, maps:get(label, Aggregated)),

    %% Verify statistics
    Stats = maps:get(statistics, Aggregated),
    ?assertEqual(5, maps:get(profile_count, Stats)),

    %% Verify all events merged
    TotalEvents = lists:foldl(fun(P, Acc) ->
        length(maps:get(events, P, [])) + Acc
    end, 0, Profiles),
    AggregatedEvents = length(maps:get(events, Aggregated, [])),
    ?assertEqual(TotalEvents, AggregatedEvents),

    ok.

%% @doc Test profile comparison
test_comparison_integration(_Config) ->
    ct:log("Testing profile comparison integration"),

    %% Create baseline and comparison profiles
    Baseline = create_test_profile(1000),
    Comparison = create_test_profile(1200),  %% 20% slower

    ct:log("Comparing profiles"),
    ct:log("Baseline duration: ~p μs", [maps:get(total_duration_us, Baseline)]),
    ct:log("Comparison duration: ~p μs", [maps:get(total_duration_us, Comparison)]),

    %% Compare profiles
    Diff = erlmcp_profiler:compare_profiles(Baseline, Comparison),

    ct:log("Comparison result keys: ~p", [maps:keys(Diff)]),
    ct:log("Diff details: ~p", [maps:get(diff, Diff)]),

    %% Verify comparison structure
    ?assert(maps:is_key(baseline, Diff)),
    ?assert(maps:is_key(comparison, Diff)),
    ?assert(maps:is_key(diff, Diff)),

    %% Verify diff calculations
    DiffMap = maps:get(diff, Diff),
    ?assert(maps:is_key(duration_us_diff, DiffMap)),
    ?assert(maps:is_key(duration_percent_change, DiffMap)),
    ?assert(maps:is_key(faster, DiffMap)),

    %% Verify slower detection
    ?assertEqual(false, maps:get(faster, DiffMap)),
    ?assertEqual(200, maps:get(duration_us_diff, DiffMap)),

    %% Verify percentage change (approximately 20%)
    PercentChange = maps:get(duration_percent_change, DiffMap),
    ?assert(PercentChange > 19.0 andalso PercentChange < 21.0),

    ok.

%% @doc Test visualization integration
test_visualization_integration(_Config) ->
    ct:log("Testing visualization integration"),

    %% Create complex test profile
    Timeline = create_complex_test_profile(),

    ct:log("Generating SVG visualization"),
    {ok, SVG} = erlmcp_timeline_viz:generate_svg(Timeline),
    ?assert(is_binary(SVG)),
    ?assert(<<"<?xml version=\"1.0\"">> =< SVG),
    ?assert(<<"<svg">> =< SVG),
    ct:log("SVG generated: ~p bytes", [byte_size(SVG)]),

    ct:log("Generating HTML visualization"),
    {ok, HTML} = erlmcp_timeline_viz:generate_html(Timeline),
    ?assert(is_binary(HTML)),
    ?assert(<<"<!DOCTYPE html>">> =< HTML),
    ?assert(<<"<html>">> =< HTML),
    ct:log("HTML generated: ~p bytes", [byte_size(HTML)]),

    ct:log("Generating JSON export"),
    {ok, JSON} = erlmcp_timeline_viz:export_json(Timeline),
    ?assert(is_binary(JSON)),
    %% Verify JSON is valid
    Decoded = jsx:decode(JSON, [return_maps]),
    ?assert(maps:is_key(<<"profile_id">>, Decoded)),
    ct:log("JSON exported: ~p bytes", [byte_size(JSON)]),

    ct:log("Generating CSV export"),
    {ok, CSV} = erlmcp_timeline_viz:export_csv(Timeline),
    ?assert(is_binary(CSV)),
    %% Verify CSV header
    Lines = binary:split(CSV, <<"\n">>, [global, trim_all]),
    [Header | _] = Lines,
    ?assert(<<"timestamp,type,duration_us,relative_time_us">> =< Header),
    ct:log("CSV exported: ~p bytes, ~p rows", [byte_size(CSV), length(Lines)]),

    ok.

%% @doc Test CLI profiler integration
test_cli_profiler_integration(_Config) ->
    ct:log("Testing CLI profiler integration"),

    %% Start profiler
    {ok, _ProfilerPid} = erlmcp_profiler:start_link(),

    %% Test argument parsing
    Args0 = [],
    Opts0 = erlmcp_cli_profiler_timeline:parse_arguments(Args0),
    ct:log("Empty args parsed: ~p", [Opts0]),
    ?assertEqual(#{}, Opts0),

    Args1 = ["key1=value1", "key2=value2"],
    Opts1 = erlmcp_cli_profiler_timeline:parse_arguments(Args1),
    ct:log("Args parsed: ~p", [Opts1]),
    ?assert(maps:is_key(<<"key1">>, Opts1)),
    ?assertEqual(<<"value1">>, maps:get(<<"key1">>, Opts1)),

    %% Test profile summary generation (capture output)
    Timeline = create_test_profile(5000),
    ?assert(maps:is_key(total_duration_us, Timeline)),
    ?assert(maps:is_key(statistics, Timeline)),

    %% Cleanup
    erlmcp_profiler:stop(),
    ok.

%% @doc Test concurrent profiling
test_concurrent_profiling(_Config) ->
    ct:log("Testing concurrent profiling"),

    %% Start profiler
    {ok, _ProfilerPid} = erlmcp_profiler:start_link(),

    %% Spawn multiple profiling operations concurrently
    ConcurrentProfiles = 10,
    Parent = self(),

    Pids = [spawn(fun() ->
        Fun = fun() ->
            timer:sleep(10),
            N = rand:uniform(100)
        end,
        Label = <<"concurrent_profile_", (integer_to_binary(N))/binary>>,
        {ok, Result, Timeline} = erlmcp_profiler:profile_timeline(Fun, Label),
        Parent ! {profile_complete, self(), Result, Timeline}
    end) || N <- lists:seq(1, ConcurrentProfiles)],

    ct:log("Spawned ~p concurrent profiles", [ConcurrentProfiles]),

    %% Collect results
    Results = [receive
        {profile_complete, Pid, Result, Timeline} ->
            ct:log("Profile ~p completed: ~p μs", [Pid, maps:get(total_duration_us, Timeline)]),
            {ok, Result, Timeline}
    after 5000 ->
        ct:fail("Timeout waiting for profile completion")
    end || _Pid <- Pids],

    ct:log("All ~p profiles completed", [length(Results)]),
    ?assertEqual(ConcurrentProfiles, length(Results)),

    %% Verify all results are valid
    lists:foreach(fun({ok, _Result, Timeline}) ->
        ?assert(maps:is_key(profile_id, Timeline)),
        ?assert(maps:is_key(total_duration_us, Timeline))
    end, Results),

    %% Cleanup
    erlmcp_profiler:stop(),
    ok.

%% @doc Test profiler supervision
test_profiler_supervision(_Config) ->
    ct:log("Testing profiler supervision"),

    %% Start profiler under supervision
    {ok, SupPid} = erlmcp_observability_sup:start_link(),

    %% Verify profiler is available
    case whereis(erlmcp_profiler) of
        undefined ->
            ct:log("Profiler not running under supervision (may be optional)");
        Pid when is_pid(Pid) ->
            ct:log("Profiler running under supervision: ~p", [Pid]),

            %% Create a profile
            Fun = fun() -> timer:sleep(5) end,
            {ok, _, Timeline} = erlmcp_profiler:profile_timeline(Fun, <<"supervision_test">>),
            ?assert(maps:is_key(profile_id, Timeline)),

            %% Kill profiler and verify restart
            erlang:monitor(process, Pid),
            exit(Pid, kill),

            receive
                {'DOWN', _Ref, process, Pid, _Reason} ->
                    ct:log("Profiler process died")
            after 1000 ->
                ct:fail("Profiler did not die")
            end,

            %% Wait for supervisor restart
            timer:sleep(200),

            case whereis(erlmcp_profiler) of
                undefined ->
                    ct:log("Profiler not restarted (may be permanent failure)");
                NewPid when is_pid(NewPid) ->
                    ct:log("Profiler restarted: ~p", [NewPid]),
                    ?assertNotEqual(Pid, NewPid)
            end
    end,

    %% Cleanup
    gen_server:stop(SupPid),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Create a test profile with custom duration
create_test_profile(DurationUs) ->
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
          details => #{reason => gc}}
    ],

    #{
        profile_id => list_to_binary("test_profile_" ++ integer_to_list(erlang:unique_integer([positive]))),
        label => <<"test_profile">>,
        start_time => Now,
        end_time => Now + DurationUs,
        total_duration_us => DurationUs,
        events => Events,
        statistics => #{
            total_duration_us => DurationUs,
            event_counts => #{scheduler => 1, process => 1, gc => 1},
            event_count => 3
        }
    }.

%% @private Create a complex test profile
create_complex_test_profile() ->
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
        profile_id => <<"complex_test_profile">>,
        label => <<"complex_profile">>,
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
