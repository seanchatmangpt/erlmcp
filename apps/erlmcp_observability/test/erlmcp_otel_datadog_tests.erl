%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_otel_datadog following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls only
%%% - Use REAL erlmcp_otel_datadog module (no mocks)
%%% - NO internal state inspection
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otel_datadog_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

datadog_test_() ->
    {setup,
     fun() -> application:ensure_all_started(inets) end,
     fun(_) -> ok end,
     [
         {"Initialize Datadog exporter", fun test_init/0},
         {"Initialize with invalid config", fun test_init_invalid/0},
         {"Export spans", fun test_export_spans/0},
         {"Format span", fun test_format_span/0},
         {"Add Datadog tags", fun test_add_tags/0},
         {"Encode batch", fun test_encode_batch/0},
         {"Shutdown flushes queue", fun test_shutdown/0}
     ]}.

%%====================================================================
%% Test Cases
%%====================================================================

test_init() ->
    Config = #{
        endpoint => <<"http://localhost:4318/v1/traces">>,
        env => <<"staging">>,
        service => <<"test_service">>,
        version => <<"1.0.0">>,
        batch_timeout => 5000,
        max_queue_size => 100
    },

    {ok, State} = erlmcp_otel_datadog:init(Config),

    ?assert(is_map(State)),
    ?assert(maps:is_key(config, State)),
    ?assert(maps:is_key(queue, State)).

test_init_invalid() ->
    InvalidConfig = #{
        env => <<"production">>
        % Missing required endpoint
    },

    ?assertEqual({error, missing_endpoint}, erlmcp_otel_datadog:init(InvalidConfig)).

test_export_spans() ->
    Config = #{
        endpoint => <<"http://localhost:4318/v1/traces">>,
        env => <<"test">>,
        service => <<"erlmcp">>,
        version => <<"2.0.0">>,
        batch_timeout => 10000,
        max_queue_size => 100
    },

    {ok, State} = erlmcp_otel_datadog:init(Config),

    Span = #{
        trace_id => <<"trace-dd-001">>,
        span_id => <<"span-dd-001">>,
        parent_span_id => undefined,
        name => <<"test.operation">>,
        start_time => erlang:system_time(nanosecond),
        end_time => erlang:system_time(nanosecond) + 1000000,
        attributes => #{<<"http.method">> => <<"GET">>}
    },

    {ok, NewState} = erlmcp_otel_datadog:export_spans([Span], State),
    ?assert(is_map(NewState)).

test_format_span() ->
    Span = #{
        trace_id => <<"trace">>,
        span_id => <<"span">>,
        parent_span_id => <<"parent">>,
        name => <<"operation">>,
        start_time => 0,
        end_time => 1000,
        attributes => #{
            <<"span.kind">> => <<"client">>,
            <<"service.name">> => <<"my-service">>,
            <<"custom.tag">> => <<"value">>
        }
    },

    Formatted = erlmcp_otel_datadog:format_span(Span),

    ?assert(is_map(Formatted)),
    Attributes = maps:get(attributes, Formatted),
    ?assert(maps:is_key(<<"span.kind">>, Attributes)).

test_add_tags() ->
    Config = #{
        endpoint => <<"http://localhost:4318/v1/traces">>,
        env => <<"production">>,
        service => <<"api-server">>,
        version => <<"3.2.1">>,
        tags => #{
            <<"team">> => <<"backend">>,
            <<"region">> => <<"us-east-1">>
        }
    },

    Span = #{
        attributes => #{<<"original">> => <<"value">>}
    },

    Tagged = erlmcp_otel_datadog:add_datadog_tags(Span, Config),

    Attributes = maps:get(attributes, Tagged),
    ?assertEqual(<<"production">>, maps:get(<<"env">>, Attributes)),
    ?assertEqual(<<"api-server">>, maps:get(<<"service">>, Attributes)),
    ?assertEqual(<<"3.2.1">>, maps:get(<<"version">>, Attributes)),
    ?assertEqual(<<"backend">>, maps:get(<<"team">>, Attributes)),
    ?assertEqual(<<"us-east-1">>, maps:get(<<"region">>, Attributes)),
    ?assertEqual(<<"value">>, maps:get(<<"original">>, Attributes)).

test_encode_batch() ->
    Spans = [
        #{
            trace_id => <<"t1">>,
            span_id => <<"s1">>,
            parent_span_id => undefined,
            name => <<"span1">>,
            start_time => 0,
            end_time => 1000,
            attributes => #{}
        }
    ],

    Encoded = erlmcp_otel_datadog:encode_batch(Spans),

    ?assert(is_binary(Encoded)),
    ?assert(byte_size(Encoded) > 0).

test_shutdown() ->
    Config = #{
        endpoint => <<"http://localhost:4318/v1/traces">>,
        env => <<"test">>,
        service => <<"test">>,
        version => <<"1.0.0">>,
        batch_timeout => 10000,
        max_queue_size => 100
    },

    {ok, State} = erlmcp_otel_datadog:init(Config),

    ?assertEqual(ok, erlmcp_otel_datadog:shutdown(State)).

%%====================================================================
%% Edge Cases
%%====================================================================

empty_spans_test() ->
    Config = #{
        endpoint => <<"http://localhost:4318/v1/traces">>,
        env => <<"test">>,
        service => <<"test">>,
        version => <<"1.0">>,
        batch_timeout => 10000,
        max_queue_size => 100
    },

    {ok, State} = erlmcp_otel_datadog:init(Config),
    {ok, NewState} = erlmcp_otel_datadog:export_spans([], State),
    ?assert(is_map(NewState)).

minimal_config_test() ->
    MinimalConfig = #{
        endpoint => <<"http://localhost:4318/v1/traces">>
    },

    {ok, State} = erlmcp_otel_datadog:init(MinimalConfig),
    ?assert(is_map(State)).

encode_empty_batch_test() ->
    Encoded = erlmcp_otel_datadog:encode_batch([]),
    ?assert(is_binary(Encoded)).
