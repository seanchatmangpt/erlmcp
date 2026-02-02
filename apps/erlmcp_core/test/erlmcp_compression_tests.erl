-module(erlmcp_compression_tests).
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Setup
%%%====================================================================

compress_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"compress small data", fun test_compress_small/0},
      {"compress large data", fun test_compress_large/0},
      {"compress with level", fun test_compress_with_level/0},
      {"decompress", fun test_decompress/0},
      {"compress with metadata", fun test_compress_with_metadata/0},
      {"compress threshold", fun test_compress_threshold/0},
      {"compress empty", fun test_compress_empty/0},
      {"compress decompress roundtrip", fun test_roundtrip/0},
      {"compress unicode", fun test_compress_unicode/0},
      {"compress threshold config", fun test_set_threshold/0},
      {"get stats", fun test_get_stats/0},
      {"reset stats", fun test_reset_stats/0},
      {"benchmark comparison", fun test_benchmark_comparison/0},
      {"compress error handling", fun test_compress_error/0},
      {"decompress error handling", fun test_decompress_error/0}
     ]}.

setup() ->
    {ok, Pid} = erlmcp_compression:start_link(),
    Pid.

cleanup(_Pid) ->
    erlmcp_compression:stop().

%%%====================================================================
%%% Test Cases
%%%====================================================================

%% @doc Test compressing small data
test_compress_small() ->
    Data = <<"hello world">>,
    {ok, Compressed} = erlmcp_compression:compress(Data),
    ?assert(is_binary(Compressed)),
    ?assert(byte_size(Compressed) > 0).

%% @doc Test compressing large data
test_compress_large() ->
    Data = crypto:strong_rand_bytes(1024 * 1024), % 1MB
    {ok, Compressed} = erlmcp_compression:compress(Data),
    ?assert(is_binary(Compressed)),
    ?assert(byte_size(Compressed) > 0),
    ?assert(byte_size(Compressed) < byte_size(Data)).

%% @doc Test compressing with different levels
test_compress_with_level() ->
    Data = crypto:strong_rand_bytes(1024 * 100), % 100KB
    {ok, Level1} = erlmcp_compression:compress(Data, 1),
    {ok, Level3} = erlmcp_compression:compress(Data, 3),
    {ok, Level10} = erlmcp_compression:compress(Data, 10),
    {ok, Level22} = erlmcp_compression:compress(Data, 22),
    %% Higher levels should compress better (smaller size)
    ?assert(byte_size(Level22) =< byte_size(Level10)),
    ?assert(byte_size(Level10) =< byte_size(Level3)),
    ?assert(byte_size(Level3) =< byte_size(Level1)).

%% @doc Test decompressing
test_decompress() ->
    Original = <<"The quick brown fox jumps over the lazy dog">>,
    {ok, Compressed} = erlmcp_compression:compress(Original),
    {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
    ?assertEqual(Original, Decompressed).

%% @doc Test compress with metadata
test_compress_with_metadata() ->
    Data = crypto:strong_rand_bytes(1024 * 100), % 100KB
    {ok, Compressed, Metadata} = erlmcp_compression:compress_with_metadata(Data),
    ?assert(is_binary(Compressed)),
    ?assertEqual(<<"zstd">>, maps:get(encoding, Metadata)),
    ?assertEqual(byte_size(Data), maps:get(original_size, Metadata)),
    ?assertEqual(byte_size(Compressed), maps:get(compressed_size, Metadata)),
    ?assert(maps:get(ratio, Metadata) > 0),
    ?assert(maps:get(ratio, Metadata) =< 1).

%% @doc Test compress threshold
test_compress_threshold() ->
    %% Set threshold to 10KB
    ok = erlmcp_compression:set_compression_threshold(10240),

    %% Small data (should not compress)
    SmallData = crypto:strong_rand_bytes(1024), % 1KB
    {ok, SmallResult} = erlmcp_compression:compress_threshold(SmallData),
    ?assertEqual(SmallData, SmallResult),

    %% Large data (should compress)
    LargeData = crypto:strong_rand_bytes(1024 * 100), % 100KB
    {ok, LargeCompressed, Metadata} = erlmcp_compression:compress_threshold(LargeData),
    ?assert(is_binary(LargeCompressed)),
    ?assertEqual(<<"zstd">>, maps:get(encoding, Metadata)),
    ?assert(maps:get(ratio, Metadata) < 1).

%% @doc Test compressing empty data
test_compress_empty() ->
    Data = <<>>,
    {ok, Compressed} = erlmcp_compression:compress(Data),
    ?assert(is_binary(Compressed)),
    {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
    ?assertEqual(Data, Decompressed).

%% @doc Test roundtrip compression/decompression
test_roundtrip() ->
    Original = crypto:strong_rand_bytes(1024 * 512), % 512KB
    {ok, Compressed} = erlmcp_compression:compress(Original),
    {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
    ?assertEqual(Original, Decompressed).

%% @doc Test compressing unicode data
test_compress_unicode() ->
    Data = unicode:characters_to_binary("Hello 世界 🌍 Привет Мир"),
    {ok, Compressed} = erlmcp_compression:compress(Data),
    {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
    ?assertEqual(Data, Decompressed).

%% @doc Test setting threshold
test_set_threshold() ->
    %% Test various thresholds
    lists:foreach(fun(Threshold) ->
        ok = erlmcp_compression:set_compression_threshold(Threshold),
        ?assertEqual(ok, erlmcp_compression:set_compression_threshold(Threshold))
    end, [0, 1024, 10240, 1048576]).

%% @doc Test getting stats
test_get_stats() ->
    %% Reset stats first
    ok = erlmcp_compression:reset_stats(),

    %% Do some compression
    Data = crypto:strong_rand_bytes(1024 * 100),
    {ok, _, _} = erlmcp_compression:compress_with_metadata(Data),

    %% Get stats
    {ok, Stats} = erlmcp_compression:get_stats(),
    ?assert(maps:get(total_compress_ops, Stats) > 0),
    ?assert(maps:get(total_original_bytes, Stats) > 0),
    ?assert(maps:get(total_compressed_bytes, Stats) > 0),
    ?assert(maps:get(avg_ratio, Stats) > 0),
    ?assert(maps:get(avg_ratio, Stats) =< 1).

%% @doc Test resetting stats
test_reset_stats() ->
    %% Do some compression
    Data = crypto:strong_rand_bytes(1024 * 100),
    {ok, _, _} = erlmcp_compression:compress_with_metadata(Data),

    %% Reset stats
    ok = erlmcp_compression:reset_stats(),

    %% Verify stats are reset
    {ok, Stats} = erlmcp_compression:get_stats(),
    ?assertEqual(0, maps:get(total_compress_ops, Stats)),
    ?assertEqual(0, maps:get(total_original_bytes, Stats)),
    ?assertEqual(0, maps:get(total_compressed_bytes, Stats)).

%% @doc Test benchmark comparison
test_benchmark_comparison() ->
    Results = erlmcp_compression:benchmark_comparison(1024 * 100), % 100KB
    ?assert(maps:get(data_size, Results) =:= 102400),
    ?assert(maps:is_key(zstd, Results)),
    ?assert(maps:is_key(zlib, Results)),
    ?assert(maps:is_key(comparison, Results)),

    Zstd = maps:get(zstd, Results),
    ?assert(maps:is_key(compressed_size, Zstd)),
    ?assert(maps:is_key(compression_ratio, Zstd)),
    ?assert(maps:is_key(compress_time_us, Zstd)),
    ?assert(maps:is_key(decompress_time_us, Zstd)),
    ?assert(maps:is_key(throughput_mb_s, Zstd)),

    Zlib = maps:get(zlib, Results),
    ?assert(maps:is_key(compressed_size, Zlib)),
    ?assert(maps:is_key(compression_ratio, Zlib)),

    %% zstd should be better than zlib
    ?assert(maps:get(compression_ratio, Zstd) =< maps:get(compression_ratio, Zlib)).

%% @doc Test error handling for invalid data
test_compress_error() ->
    %% Compress only accepts binary
    ?assertException(error, function_clause, erlmcp_compression:compress(123)),
    ?assertException(error, function_clause, erlmcp_compression:compress("not binary")),
    ?assertException(error, function_clause, erlmcp_compression:compress([])).

%% @doc Test error handling for decompressing invalid data
test_decompress_error() ->
    %% Invalid compressed data
    InvalidData = <<"invalid compressed data">>,
    Result = erlmcp_compression:decompress(InvalidData),
    ?assertMatch({error, _}, Result).
