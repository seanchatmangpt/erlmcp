%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_dashboard_filter_unit_tests - Unit Tests for Metrics Filtering
%%%
%%% Pure unit tests for filter_metrics_message and apply_metric_filter.
%%% These tests don't require WebSocket connections or Cowboy.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_dashboard_filter_unit_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Filter Functions Tests
%%====================================================================

%% @doc Test that empty filter returns original message
filter_empty_test() ->
    Message = jsx:encode(#{
        type => <<"metrics">>,
        data => #{cpu_percent => 50, memory_mb => 1024}
    }),
    Result = erlmcp_dashboard_server:filter_metrics_message(Message, #{}),
    ?assertEqual(Message, Result).

%% @doc Test filtering by CPU metric type
filter_cpu_only_test() ->
    Message = jsx:encode(#{
        type => <<"metrics">>,
        data => #{
            <<"cpu_percent">> => 75.5,
            <<"memory_mb">> => 2048,
            <<"throughput">> => 1000
        }
    }),
    Filter = #{<<"types">> => [<<"cpu_percent">>]},
    Result = erlmcp_dashboard_server:filter_metrics_message(Message, Filter),
    Decoded = jsx:decode(Result, [return_maps]),
    Data = maps:get(<<"data">>, Decoded),

    ?assert(maps:is_key(<<"cpu_percent">>, Data)),
    ?assertNot(maps:is_key(<<"memory_mb">>, Data)),
    ?assertNot(maps:is_key(<<"throughput">>, Data)).

%% @doc Test filtering by multiple metric types
filter_multiple_types_test() ->
    Message = jsx:encode(#{
        type => <<"metrics">>,
        data => #{
            <<"cpu_percent">> => 60.0,
            <<"memory_mb">> => 1536,
            <<"throughput">> => 2000,
            <<"connections">> => 50
        }
    }),
    Filter = #{<<"types">> => [<<"cpu_percent">>, <<"memory_mb">>]},
    Result = erlmcp_dashboard_server:filter_metrics_message(Message, Filter),
    Decoded = jsx:decode(Result, [return_maps]),
    Data = maps:get(<<"data">>, Decoded),

    ?assert(maps:is_key(<<"cpu_percent">>, Data)),
    ?assert(maps:is_key(<<"memory_mb">>, Data)),
    ?assertNot(maps:is_key(<<"throughput">>, Data)),
    ?assertNot(maps:is_key(<<"connections">>, Data)).

%% @doc Test filtering reduces message size
filter_reduces_size_test() ->
    Message = jsx:encode(#{
        type => <<"metrics">>,
        data => #{
            <<"cpu_percent">> => 55.0,
            <<"memory_mb">> => 1024,
            <<"throughput">> => 1500,
            <<"connections">> => 100,
            <<"errors">> => 5
        }
    }),
    OriginalSize = byte_size(Message),

    Filter = #{<<"types">> => [<<"cpu_percent">>]},
    Result = erlmcp_dashboard_server:filter_metrics_message(Message, Filter),
    FilteredSize = byte_size(Result),

    ?assert(FilteredSize < OriginalSize),
    % Save at least 10% bandwidth (more realistic for small JSON objects)
    SavingsPercent = (OriginalSize - FilteredSize) * 100 / OriginalSize,
    ?assert(SavingsPercent > 10.0).

%% @doc Test filtering preserves message structure
filter_preserves_structure_test() ->
    Message = jsx:encode(#{
        type => <<"metrics">>,
        data => #{<<"cpu_percent">> => 45.0}
    }),
    Filter = #{<<"types">> => [<<"cpu_percent">>]},
    Result = erlmcp_dashboard_server:filter_metrics_message(Message, Filter),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"metrics">>, maps:get(<<"type">>, Decoded)),
    ?assert(maps:is_key(<<"data">>, Decoded)).

%% @doc Test filtering non-existent metric type
filter_nonexistent_type_test() ->
    Message = jsx:encode(#{
        type => <<"metrics">>,
        data => #{
            <<"cpu_percent">> => 65.0,
            <<"memory_mb">> => 768
        }
    }),
    Filter = #{<<"types">> => [<<"nonexistent_metric">>]},
    Result = erlmcp_dashboard_server:filter_metrics_message(Message, Filter),
    Decoded = jsx:decode(Result, [return_maps]),
    Data = maps:get(<<"data">>, Decoded),

    % Should return empty data map for non-existent types
    ?assertEqual(#{}, Data).

%% @doc Test filtering with latency metrics
filter_latency_metrics_test() ->
    Message = jsx:encode(#{
        type => <<"metrics">>,
        data => #{
            <<"latency_p50_us">> => 15000,
            <<"latency_p95_us">> => 25000,
            <<"latency_p99_us">> => 50000,
            <<"cpu_percent">> => 50.0
        }
    }),
    Filter = #{<<"types">> => [<<"latency_p50_us">>, <<"latency_p95_us">>, <<"latency_p99_us">>]},
    Result = erlmcp_dashboard_server:filter_metrics_message(Message, Filter),
    Decoded = jsx:decode(Result, [return_maps]),
    Data = maps:get(<<"data">>, Decoded),

    ?assert(maps:is_key(<<"latency_p50_us">>, Data)),
    ?assert(maps:is_key(<<"latency_p95_us">>, Data)),
    ?assert(maps:is_key(<<"latency_p99_us">>, Data)),
    ?assertNot(maps:is_key(<<"cpu_percent">>, Data)).

%% @doc Test filtering with no types specified
filter_no_types_key_test() ->
    Message = jsx:encode(#{
        type => <<"metrics">>,
        data => #{<<"cpu_percent">> => 70.0, <<"memory_mb">> => 512}
    }),
    Filter = #{<<"other_key">> => <<"value">>},
    Result = erlmcp_dashboard_server:filter_metrics_message(Message, Filter),
    Decoded = jsx:decode(Result, [return_maps]),
    Data = maps:get(<<"data">>, Decoded),

    % No types filter should return all data
    ?assertEqual(#{<<"cpu_percent">> => 70.0, <<"memory_mb">> => 512}, Data).

%% @doc Test filtering preserves data types
filter_preserves_types_test() ->
    Message = jsx:encode(#{
        type => <<"metrics">>,
        data => #{
            <<"cpu_percent">> => 55.5,  % float
            <<"throughput">> => 1000,     % integer
            <<"timestamp">> => 1234567890 % integer
        }
    }),
    Filter = #{<<"types">> => [<<"cpu_percent">>, <<"throughput">>, <<"timestamp">>]},
    Result = erlmcp_dashboard_server:filter_metrics_message(Message, Filter),
    Decoded = jsx:decode(Result, [return_maps]),
    Data = maps:get(<<"data">>, Decoded),

    ?assert(is_float(maps:get(<<"cpu_percent">>, Data))),
    ?assert(is_integer(maps:get(<<"throughput">>, Data))),
    ?assert(is_integer(maps:get(<<"timestamp">>, Data))).

%%====================================================================
%% Helper Functions Tests
%%====================================================================

%% @doc Test apply_metric_filter with empty types list
apply_filter_empty_list_test() ->
    Metrics = #{<<"cpu_percent">> => 50.0, <<"memory_mb">> => 1024},
    Filter = #{<<"types">> => []},
    Result = erlmcp_dashboard_server:apply_metric_filter(Metrics, Filter),
    ?assertEqual(#{}, Result).

%% @doc Test apply_metric_filter with single type
apply_filter_single_type_test() ->
    Metrics = #{<<"cpu_percent">> => 60.0, <<"memory_mb">> => 768, <<"throughput">> => 500},
    Filter = #{<<"types">> => [<<"throughput">>]},
    Result = erlmcp_dashboard_server:apply_metric_filter(Metrics, Filter),
    ?assertEqual(#{<<"throughput">> => 500}, Result).

%% @doc Test apply_metric_filter preserves values
apply_filter_preserves_values_test() ->
    Metrics = #{
        <<"cpu_percent">> => 45.5,
        <<"memory_mb">> => 2048.0,
        <<"throughput">> => 2500,
        <<"connections">> => 75
    },
    Filter = #{<<"types">> => [<<"cpu_percent">>, <<"memory_mb">>]},
    Result = erlmcp_dashboard_server:apply_metric_filter(Metrics, Filter),

    ?assertEqual(45.5, maps:get(<<"cpu_percent">>, Result)),
    ?assertEqual(2048.0, maps:get(<<"memory_mb">>, Result)).
