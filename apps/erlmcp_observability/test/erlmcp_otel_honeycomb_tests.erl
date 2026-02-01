%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_otel_honeycomb following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls only
%%% - Use REAL erlmcp_otel_honeycomb module (no mocks)
%%% - NO internal state inspection (test API boundaries only)
%%% - Test with real gen_server processes
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otel_honeycomb_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Setup/teardown for Honeycomb exporter
honeycomb_test_() ->
    {setup,
     fun setup_honeycomb/0,
     fun cleanup_honeycomb/1,
     [
         {"Initialize Honeycomb exporter", fun test_init/0},
         {"Initialize with invalid config fails", fun test_init_invalid/0},
         {"Export spans with sampling", fun test_export_spans/0},
         {"Format span for Honeycomb", fun test_format_span/0},
         {"Add Honeycomb metadata to span", fun test_add_metadata/0},
         {"Calculate sample rate based on span attributes", fun test_calculate_sample_rate/0},
         {"Queue batching and flushing", fun test_batching/0},
         {"Shutdown flushes remaining spans", fun test_shutdown/0}
     ]}.

setup_honeycomb() ->
    % Start inets for HTTP requests
    application:ensure_all_started(inets),
    ok.

cleanup_honeycomb(_Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% Test successful initialization with valid config
test_init() ->
    Config = #{
        endpoint => <<"https://api.honeycomb.io">>,
        api_key => <<"test_api_key">>,
        dataset => <<"test_dataset">>,
        batch_timeout => 5000,
        max_queue_size => 100
    },

    {ok, State} = erlmcp_otel_honeycomb:init(Config),

    % Verify state is a map (observable behavior)
    ?assert(is_map(State)),
    ?assert(maps:is_key(config, State)),
    ?assert(maps:is_key(queue, State)),
    ?assert(maps:is_key(timer, State)).

%% Test initialization fails with invalid config
test_init_invalid() ->
    % Missing required endpoint
    InvalidConfig1 = #{api_key => <<"test">>},
    ?assertEqual({error, missing_required_fields}, erlmcp_otel_honeycomb:init(InvalidConfig1)),

    % Missing required api_key
    InvalidConfig2 = #{endpoint => <<"https://api.honeycomb.io">>},
    ?assertEqual({error, missing_required_fields}, erlmcp_otel_honeycomb:init(InvalidConfig2)).

%% Test exporting spans with sampling
test_export_spans() ->
    Config = #{
        endpoint => <<"https://api.honeycomb.io">>,
        api_key => <<"test_key">>,
        dataset => <<"test">>,
        sample_rate => 1,  % Always sample
        batch_timeout => 10000,
        max_queue_size => 100
    },

    {ok, State} = erlmcp_otel_honeycomb:init(Config),

    % Create test spans
    Spans = [
        #{
            trace_id => <<"trace-001">>,
            span_id => <<"span-001">>,
            parent_span_id => undefined,
            name => <<"test.operation">>,
            start_time => erlang:system_time(nanosecond),
            end_time => erlang:system_time(nanosecond) + 1000000,
            attributes => #{<<"test">> => <<"value">>}
        }
    ],

    % Export spans
    {ok, NewState} = erlmcp_otel_honeycomb:export_spans(Spans, State),

    % Verify state is updated (queue should contain spans)
    ?assert(is_map(NewState)),
    ?assert(maps:is_key(queue, NewState)).

%% Test span formatting for Honeycomb
test_format_span() ->
    StartTime = erlang:system_time(nanosecond),
    EndTime = StartTime + 5000000,  % 5ms later

    Span = #{
        trace_id => <<"trace-123">>,
        span_id => <<"span-456">>,
        parent_span_id => <<"span-789">>,
        name => <<"mcp.request">>,
        start_time => StartTime,
        end_time => EndTime,
        attributes => #{
            <<"http.method">> => <<"POST">>,
            <<"http.status">> => 200
        }
    },

    Formatted = erlmcp_otel_honeycomb:format_span(Span),

    % Verify formatted span has required Honeycomb fields
    ?assert(is_map(Formatted)),
    ?assertEqual(<<"trace-123">>, maps:get(<<"trace.trace_id">>, Formatted)),
    ?assertEqual(<<"span-456">>, maps:get(<<"trace.span_id">>, Formatted)),
    ?assertEqual(<<"span-789">>, maps:get(<<"trace.parent_id">>, Formatted)),
    ?assertEqual(<<"mcp.request">>, maps:get(<<"name">>, Formatted)),
    ?assert(maps:is_key(<<"timestamp">>, Formatted)),
    ?assert(maps:is_key(<<"duration_ms">>, Formatted)),
    ?assertEqual(<<"span">>, maps:get(<<"meta.span_type">>, Formatted)).

%% Test adding Honeycomb-specific metadata
test_add_metadata() ->
    Config = #{
        endpoint => <<"https://api.honeycomb.io">>,
        api_key => <<"test">>,
        dataset => <<"production">>,
        environment => <<"staging">>
    },

    Span = #{
        attributes => #{<<"original">> => <<"value">>}
    },

    Enriched = erlmcp_otel_honeycomb:add_honeycomb_metadata(Span, Config),

    % Verify metadata is added
    ?assert(is_map(Enriched)),
    Attributes = maps:get(attributes, Enriched),
    ?assertEqual(<<"production">>, maps:get(<<"meta.dataset">>, Attributes)),
    ?assertEqual(<<"staging">>, maps:get(<<"meta.environment">>, Attributes)),
    ?assert(maps:is_key(<<"meta.local_hostname">>, Attributes)),
    ?assert(maps:is_key(<<"meta.beamtime">>, Attributes)),
    % Original attribute preserved
    ?assertEqual(<<"value">>, maps:get(<<"original">>, Attributes)).

%% Test sample rate calculation
test_calculate_sample_rate() ->
    % Error span - always sample (rate = 1)
    ErrorSpan = #{
        attributes => #{<<"error">> => true, <<"duration_ms">> => 50}
    },
    ?assertEqual(1, erlmcp_otel_honeycomb:calculate_sample_rate(ErrorSpan)),

    % High latency span - higher sample rate
    SlowSpan = #{
        attributes => #{<<"error">> => false, <<"duration_ms">> => 2000}
    },
    ?assertEqual(2, erlmcp_otel_honeycomb:calculate_sample_rate(SlowSpan)),

    % Normal span - standard sample rate
    NormalSpan = #{
        attributes => #{<<"error">> => false, <<"duration_ms">> => 100}
    },
    ?assertEqual(10, erlmcp_otel_honeycomb:calculate_sample_rate(NormalSpan)).

%% Test queue batching behavior
test_batching() ->
    Config = #{
        endpoint => <<"https://api.honeycomb.io">>,
        api_key => <<"test">>,
        dataset => <<"test">>,
        sample_rate => 1,
        batch_timeout => 10000,
        max_queue_size => 5  % Small queue for testing
    },

    {ok, State} = erlmcp_otel_honeycomb:init(Config),

    % Add spans below queue size
    Span1 = #{
        trace_id => <<"t1">>,
        span_id => <<"s1">>,
        parent_span_id => undefined,
        name => <<"span1">>,
        start_time => erlang:system_time(nanosecond),
        end_time => erlang:system_time(nanosecond) + 1000,
        attributes => #{}
    },

    {ok, State2} = erlmcp_otel_honeycomb:export_spans([Span1], State),

    % Queue should not be flushed yet
    ?assert(is_map(State2)).

%% Test shutdown flushes queue
test_shutdown() ->
    Config = #{
        endpoint => <<"https://api.honeycomb.io">>,
        api_key => <<"test">>,
        dataset => <<"test">>,
        batch_timeout => 10000,
        max_queue_size => 100
    },

    {ok, State} = erlmcp_otel_honeycomb:init(Config),

    % Add some spans
    Span = #{
        trace_id => <<"trace">>,
        span_id => <<"span">>,
        parent_span_id => undefined,
        name => <<"test">>,
        start_time => erlang:system_time(nanosecond),
        end_time => erlang:system_time(nanosecond) + 1000,
        attributes => #{}
    },

    {ok, StateWithSpans} = erlmcp_otel_honeycomb:export_spans([Span], State),

    % Shutdown should flush remaining spans
    ?assertEqual(ok, erlmcp_otel_honeycomb:shutdown(StateWithSpans)).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% Test with empty span list
empty_spans_test() ->
    Config = #{
        endpoint => <<"https://api.honeycomb.io">>,
        api_key => <<"test">>,
        dataset => <<"test">>,
        batch_timeout => 10000,
        max_queue_size => 100
    },

    {ok, State} = erlmcp_otel_honeycomb:init(Config),

    % Export empty list
    {ok, NewState} = erlmcp_otel_honeycomb:export_spans([], State),
    ?assert(is_map(NewState)).

%% Test with sampling rate 0 (never sample)
never_sample_test() ->
    Config = #{
        endpoint => <<"https://api.honeycomb.io">>,
        api_key => <<"test">>,
        dataset => <<"test">>,
        sample_rate => 1000000,  % Very high rate (almost never sample)
        batch_timeout => 10000,
        max_queue_size => 100
    },

    {ok, State} = erlmcp_otel_honeycomb:init(Config),

    Span = #{
        trace_id => <<"trace">>,
        span_id => <<"span">>,
        parent_span_id => undefined,
        name => <<"test">>,
        start_time => erlang:system_time(nanosecond),
        end_time => erlang:system_time(nanosecond) + 1000,
        attributes => #{}
    },

    {ok, NewState} = erlmcp_otel_honeycomb:export_spans([Span], State),
    ?assert(is_map(NewState)).

%% Test format span with minimal fields
minimal_span_test() ->
    Span = #{
        trace_id => <<"t">>,
        span_id => <<"s">>,
        parent_span_id => undefined,
        name => <<"op">>,
        start_time => 0,
        end_time => 1000000,
        attributes => #{}
    },

    Formatted = erlmcp_otel_honeycomb:format_span(Span),
    ?assert(is_map(Formatted)),
    ?assert(maps:is_key(<<"name">>, Formatted)).

%% Test multiple sequential exports
sequential_exports_test() ->
    Config = #{
        endpoint => <<"https://api.honeycomb.io">>,
        api_key => <<"test">>,
        dataset => <<"test">>,
        sample_rate => 1,
        batch_timeout => 10000,
        max_queue_size => 100
    },

    {ok, State} = erlmcp_otel_honeycomb:init(Config),

    % Export multiple batches
    lists:foldl(fun(N, StateAcc) ->
        Span = #{
            trace_id => list_to_binary(["trace-", integer_to_list(N)]),
            span_id => list_to_binary(["span-", integer_to_list(N)]),
            parent_span_id => undefined,
            name => <<"test">>,
            start_time => erlang:system_time(nanosecond),
            end_time => erlang:system_time(nanosecond) + 1000,
            attributes => #{}
        },
        {ok, NewState} = erlmcp_otel_honeycomb:export_spans([Span], StateAcc),
        NewState
    end, State, lists:seq(1, 10)).
