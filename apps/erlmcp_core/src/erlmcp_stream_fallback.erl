%%%-------------------------------------------------------------------
%%% @doc
%%% Stream Processing Fallback Implementation
%%%
%%% This module provides stream processing fallback for OTP versions
%%% that don't support EEP 72 streams (OTP < 27).
%%%
%%% Features:
%%%   - Legacy stream processing using lists
%%%   - Performance optimization for small to medium datasets
%%%   - Memory-efficient processing for large datasets
%%%   - Graceful degradation when EEP 72 streams are available
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_stream_fallback).

%% API
-export([
    enable_fallback/0,
    disable_fallback/0,
    is_fallback_enabled/0,
    stream_from_list/1,
    stream_from_generator/1,
    stream_map/2,
    stream_filter/2,
    stream_fold/3,
    stream_take/2,
    stream_drop/2,
    stream_to_list/1,
    process_large_stream/2,
    get_stream_metrics/0
]).

%% Types
-type stream_element() :: any().
-type stream() :: fun(() -> stream_element() | '$end_of_stream').
-type mapper() :: fun((stream_element()) -> stream_element()).
-type predicate() :: fun((stream_element()) -> boolean()).
-type accumulator() :: any().
-type fold_fun() :: fun((accumulator(), stream_element()) -> accumulator()).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Enable stream fallback mode
-spec enable_fallback() -> ok.
enable_fallback() ->
    application:set_env(erlmcp, stream_fallback_enabled, true),
    application:set_env(erlmcp, stream_method, legacy),
    logger:info("Stream fallback mode enabled - using legacy stream processing"),
    start_stream_monitoring(),
    ok.

%% @doc Disable stream fallback mode
-spec disable_fallback() -> ok.
disable_fallback() ->
    case erlang:function_exported(gen_stream, new, 1) of
        true ->
            application:set_env(erlmcp, stream_fallback_enabled, false),
            application:set_env(erlmcp, stream_method, modern),
            logger:info("Stream fallback mode disabled - using EEP 72 streams");
        false ->
            logger:warning("Cannot disable fallback - EEP 72 streams not available"),
            enable_fallback()
    end,
    ok.

%% @doc Check if fallback mode is enabled
-spec is_fallback_enabled() -> boolean().
is_fallback_enabled() ->
    application:get_env(erlmcp, stream_fallback_enabled, true).

%% @doc Create stream from list
-spec stream_from_list([stream_element()]) -> stream().
stream_from_list(List) ->
    fun() ->
        case List of
            [Head | Tail] -> Head;
            [] -> '$end_of_stream'
        end
    end.

%% @doc Create stream from generator function
-spec stream_from_generator(fun(() -> stream_element() | '$end_of_stream')) -> stream().
stream_from_generator(Generator) ->
    Generator.

%% @todo Map over stream
-spec stream_map(stream(), mapper()) -> stream().
stream_map(Stream, Mapper) ->
    fun() ->
        case Stream() of
            '$end_of_stream' -> '$end_of_stream';
            Element -> Mapper(Element)
        end
    end.

%% @todo Filter stream
-spec stream_filter(stream(), predicate()) -> stream().
stream_filter(Stream, Predicate) ->
    fun() ->
        case Stream() of
            '$end_of_stream' -> '$end_of_stream';
            Element ->
                case Predicate(Element) of
                    true -> Element;
                    false -> stream_filter(Stream, Predicate)()
                end
        end
    end.

%% @todo Fold stream
-spec stream_fold(stream(), accumulator(), fold_fun()) -> accumulator().
stream_fold(Stream, Acc, FoldFun) ->
    case Stream() of
        '$end_of_stream' -> Acc;
        Element ->
            NewAcc = FoldFun(Acc, Element),
            stream_fold(Stream, NewAcc, FoldFun)
    end.

%% @todo Take N elements from stream
-spec stream_take(stream(), pos_integer()) -> [stream_element()].
stream_take(Stream, N) when N > 0 ->
    stream_take(Stream, N, []).

%% @private Take N elements helper
-spec stream_take(stream(), pos_integer(), [stream_element()]) -> [stream_element()].
stream_take(_Stream, 0, Acc) ->
    lists:reverse(Acc);
stream_take(Stream, N, Acc) ->
    case Stream() of
        '$end_of_stream' -> lists:reverse(Acc);
        Element ->
            stream_take(Stream, N - 1, [Element | Acc])
    end.

%% @todo Drop N elements from stream
-spec stream_drop(stream(), pos_integer()) -> stream().
stream_drop(Stream, N) when N > 0 ->
    fun() ->
        case drop_n(Stream, N) of
            '$end_of_stream' -> '$end_of_stream';
            Element -> Element
        end
    end.

%% @private Drop N elements helper
-spec drop_n(stream(), pos_integer()) -> stream_element() | '$end_of_stream'.
drop_n(Stream, 0) ->
    Stream();
drop_n(Stream, N) ->
    case Stream() of
        '$end_of_stream' -> '$end_of_stream';
        _ -> drop_n(Stream, N - 1)
    end.

%% @todo Convert stream to list
-spec stream_to_list(stream()) -> [stream_element()].
stream_to_list(Stream) ->
    stream_to_list(Stream, []).

%% @private Convert stream to list helper
-spec stream_to_list(stream(), [stream_element()]) -> [stream_element()].
stream_to_list(Stream, Acc) ->
    case Stream() of
        '$end_of_stream' -> lists:reverse(Acc);
        Element -> stream_to_list(Stream, [Element | Acc])
    end.

%% @todo Process large stream with memory optimization
-spec process_large_stream(stream(), fun()) -> any().
process_large_stream(Stream, Processor) ->
    % Process in chunks to avoid memory issues
    process_stream_chunks(Stream, Processor, 1000, []).

%% @private Process stream in chunks
-spec process_stream_chunks(stream(), fun(), pos_integer(), [stream_element()]) -> any().
process_stream_chunks(Stream, Processor, ChunkSize, Chunk) ->
    case Stream() of
        '$end_of_stream' ->
            % Process final chunk
            case Chunk of
                [] -> Processor([]);
                [Elements] -> Processor(Elements)
            end;
        Element ->
            case length(Chunk) >= ChunkSize of
                true ->
                    % Process current chunk and start new one
                    Processor(lists:reverse(Chunk)),
                    process_stream_chunks(Stream, Processor, ChunkSize, [Element]);
                false ->
                    % Add to current chunk
                    process_stream_chunks(Stream, Processor, ChunkSize, [Element | Chunk])
            end
    end.

%% @todo Get stream processing metrics
-spec get_stream_metrics() -> map().
get_stream_metrics() ->
    #{
        method => get_current_method(),
        fallback_enabled => is_fallback_enabled(),
        processing_time => get_processing_time(),
        memory_usage => get_memory_usage(),
        chunks_processed => get_chunks_processed()
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Get current method
-spec get_current_method() -> legacy | modern.
get_current_method() ->
    case is_fallback_enabled() of
        true -> legacy;
        false -> modern
    end.

%% @private Start stream monitoring
-spec start_stream_monitoring() -> ok.
start_stream_monitoring() ->
    case erlang:whereis(stream_monitor) of
        undefined ->
            spawn_link(fun stream_monitor_loop/0);
        _ ->
            ok
    end.

%% @private Stream monitoring loop
-spec stream_monitor_loop() -> no_return().
stream_monitor_loop() ->
    register(stream_monitor, self()),

    stream_monitor_loop(0).

%% @private Stream monitoring loop with iteration
-spec stream_monitor_loop(non_neg_integer()) -> no_return().
stream_monitor_loop(Iteration) ->
    % Check stream metrics every 60 seconds
    timer:sleep(60000),

    Metrics = get_stream_metrics(),

    case Metrics#{
        memory_usage := Memory,
        chunks_processed := Chunks
    } of
        #{memory_usage := M} when M > 1000000000 ->  % > 1GB
            logger:warning("High memory usage in stream processing: ~p MB",
                          [M div 1048576]);
        #{chunks_processed := C} when C > 10000 ->
            logger:info("Large number of chunks processed: ~p", [C]);
        _ ->
            ok
    end,

    % Continue monitoring
    stream_monitor_loop(Iteration + 1).

%% @private Get processing time
-spec get_processing_time() -> integer().
get_processing_time() ->
    % This would track actual processing time
    % For now, return a placeholder
    0.

%% @private Get memory usage
-spec get_memory_usage() -> integer().
get_memory_usage() ->
    erlang:memory(total).

%% @private Get chunks processed
-spec get_chunks_processed() -> non_neg_integer().
get_chunks_processed() ->
    % This would track the number of chunks processed
    % For now, return a placeholder
    0.

%% @private Test stream performance
-spec test_stream_performance() -> map().
test_stream_performance() ->
    TestData = lists:seq(1, 10000),

    % Test list-based stream
    Stream = stream_from_list(TestData),
    Start = erlang:monotonic_time(microsecond),
    Result1 = stream_to_list(stream_map(Stream, fun(X) -> X * 2 end)),
    End1 = erlang:monotonic_time(microsecond),

    % Test filtered stream
    Filtered = stream_filter(Stream, fun(X) -> X rem 2 =:= 0 end),
    Start2 = erlang:monotonic_time(microsecond),
    Result2 = stream_to_list(Filtered),
    End2 = erlang:monotonic_time(microsecond),

    #{
        list_processing_time => End1 - Start1,
        filter_processing_time => End2 - Start2,
        doubled_elements => length(Result1),
        filtered_elements => length(Result2)
    }.

%% @private Example stream processing
-spec example_stream_processing() -> ok.
example_stream_processing() ->
    % Create sample data
    Data = lists:seq(1, 1000),

    % Create stream
    Stream = stream_from_list(Data),

    % Process with map
    Doubled = stream_map(Stream, fun(X) -> X * 2 end),

    % Process with filter
    Even = stream_filter(Doubled, fun(X) -> X rem 4 =:= 0 end),

    % Convert to list
    Result = stream_to_list(Even),

    logger:info("Processed ~p even numbers doubled from 1000 elements", [length(Result)]),

    % Process large stream
    LargeData = lists:seq(1, 1000000),
    LargeStream = stream_from_list(LargeData),

    % Process in chunks
    Result2 = process_large_stream(LargeStream, fun(Chunk) ->
        logger:info("Processing chunk of ~p elements", [length(Chunk)])
    end),

    ok.

%% @private Benchmark stream operations
-spec benchmark_stream_operations() -> map().
benchmark_stream_operations() ->
    Sizes = [100, 1000, 10000, 100000],
    Results = lists:foldl(fun(Size, Acc) ->
        Data = lists:seq(1, Size),
        Stream = stream_from_list(Data),

        % Time map operation
        Start = erlang:monotonic_time(microsecond),
        _Mapped = stream_to_list(stream_map(Stream, fun(X) -> X * 2 end)),
        MapTime = erlang:monotonic_time(microsecond) - Start,

        % Time filter operation
        FilterStart = erlang:monotonic_time(microsecond),
        _Filtered = stream_to_list(stream_filter(Stream, fun(X) -> X rem 2 =:= 0 end)),
        FilterTime = erlang:monotonic_time(microsecond) - FilterStart,

        maps:put(Size, #{map_time => MapTime, filter_time => FilterTime}, Acc)
    end, [], Sizes),

    Results.

%% @private Validate stream results
-spec validate_stream_results([stream_element()]) -> boolean().
validate_stream_results(Results) ->
    % Basic validation: check that results are a list
    is_list(Results) andalso lists:all(fun is_element/1, Results).