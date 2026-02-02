%%%-------------------------------------------------------------------
%%% @doc
%%% Test suite for Timeline Profiler (OTP 26-27)
%%%
%%% Tests timeline profiling, visualization, and CLI commands.
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
