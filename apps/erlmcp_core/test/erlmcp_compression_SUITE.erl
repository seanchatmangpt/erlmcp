%%%-------------------------------------------------------------------
%%% @doc
%%% ZSTD Compression Test Suite
%%%
%%% Tests OTP 28 zstd compression functionality:
%%% - Basic compression/decompression roundtrip
%%% - Compression levels (1-22)
%%% - Compression thresholds and conditional compression
%%% - Performance benchmarks vs zlib
%%% - Edge cases (empty, large, invalid data)
%%% - Statistics tracking
%%% - gen_server behavior (Chicago School TDD)
%%%
%%% Chicago School TDD:
%%% - Real zstd module (OTP 28)
%%% - Observable behavior: compressed data size, decompressed output
%%% - No mocks or fakes
%%% - State-based verification
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_compression_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suite callbacks
-export([all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    %% Basic compression tests
    test_compress_decompress_roundtrip/1,
    test_compress_with_default_level/1,
    test_compress_with_custom_level/1,
    test_compress_with_metadata/1,

    %% Compression level tests
    test_all_compression_levels/1,
    test_fast_compression_levels/1,
    test_default_compression_level/1,
    test_archival_compression_levels/1,

    %% Threshold tests
    test_compress_threshold_below/1,
    test_compress_threshold_above/1,
    test_compress_threshold_exact/1,
    test_set_compression_threshold/1,
    test_zero_threshold_always_compress/1,

    %% Decompression tests
    test_decompress_valid_data/1,
    test_decompress_invalid_data/1,
    test_decompress_empty_data/1,

    %% Edge cases
    test_empty_binary/1,
    test_small_binary/1,
    test_large_binary/1,
    test_very_large_binary/1,
    test_already_compressed_data/1,

    %% Statistics tests
    test_get_initial_stats/1,
    test_stats_tracking/1,
    test_reset_stats/1,
    test_stats_average_ratio/1,

    %% Performance tests
    test_benchmark_zstd_vs_zlib/1,
    test_compression_speed/1,
    test_decompression_speed/1,

    %% gen_server behavior tests
    test_gen_server_lifecycle/1,
    test_concurrent_compression_requests/1,
    test_concurrent_threshold_compression/1
]).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [
        {group, basic_compression},
        {group, compression_levels},
        {group, thresholds},
        {group, decompression},
        {group, edge_cases},
        {group, statistics},
        {group, performance},
        {group, gen_server_behavior}
    ].

groups() ->
    [
        {basic_compression, [sequence], [
            test_compress_decompress_roundtrip,
            test_compress_with_default_level,
            test_compress_with_custom_level,
            test_compress_with_metadata
        ]},
        {compression_levels, [sequence], [
            test_all_compression_levels,
            test_fast_compression_levels,
            test_default_compression_level,
            test_archival_compression_levels
        ]},
        {thresholds, [sequence], [
            test_compress_threshold_below,
            test_compress_threshold_above,
            test_compress_threshold_exact,
            test_set_compression_threshold,
            test_zero_threshold_always_compress
        ]},
        {decompression, [sequence], [
            test_decompress_valid_data,
            test_decompress_invalid_data,
            test_decompress_empty_data
        ]},
        {edge_cases, [sequence], [
            test_empty_binary,
            test_small_binary,
            test_large_binary,
            test_very_large_binary,
            test_already_compressed_data
        ]},
        {statistics, [sequence], [
            test_get_initial_stats,
            test_stats_tracking,
            test_reset_stats,
            test_stats_average_ratio
        ]},
        {performance, [sequence], [
            test_benchmark_zstd_vs_zlib,
            test_compression_speed,
            test_decompression_speed
        ]},
        {gen_server_behavior, [sequence], [
            test_gen_server_lifecycle,
            test_concurrent_compression_requests,
            test_concurrent_threshold_compression
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("Starting ZSTD Compression Test Suite"),
    ct:pal("OTP Release: ~s", [erlang:system_info(otp_release)]),

    % Start erlmcp_compression gen_server
    {ok, Pid} = erlmcp_compression:start_link(),
    ct:pal("Started erlmcp_compression: ~p", [Pid]),

    [{compression_pid, Pid} | Config].

end_per_suite(Config) ->
    ct:pal("ZSTD Compression Test Suite completed"),
    Pid = proplists:get_value(compression_pid, Config),
    erlmcp_compression:stop(),
    ok.

init_per_group(Group, Config) ->
    ct:pal("Starting group: ~p", [Group]),
    Config.

end_per_group(Group, _Config) ->
    ct:pal("Ended group: ~p", [Group]),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Ending test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Basic Compression Tests
%%====================================================================

%% @doc Test basic compress/decompress roundtrip
test_compress_decompress_roundtrip(_Config) ->
    ct:pal("Testing compress/decompress roundtrip"),

    % Generate random data
    Original = crypto:strong_rand_bytes(4096),

    % Compress
    {ok, Compressed} = erlmcp_compression:compress(Original),
    ct:pal("  Original size: ~p bytes", [byte_size(Original)]),
    ct:pal("  Compressed size: ~p bytes", [byte_size(Compressed)]),

    % Verify compression achieved
    CompressedSize = byte_size(Compressed),
    OriginalSize = byte_size(Original),
    ?assert(CompressedSize < OriginalSize,
            "Compressed data should be smaller than original"),

    % Decompress
    {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
    ct:pal("  Decompressed size: ~p bytes", [byte_size(Decompressed)]),

    % Verify roundtrip
    ?assertEqual(Original, Decompressed,
                 "Decompressed data should match original").

%% @doc Test compression with default level (3)
test_compress_with_default_level(_Config) ->
    ct:pal("Testing compression with default level"),

    Data = crypto:strong_rand_bytes(8192),

    % Compress with default level
    {ok, Compressed} = erlmcp_compression:compress(Data),

    % Verify successful compression
    ?assert(is_binary(Compressed)),
    ?assert(byte_size(Compressed) < byte_size(Data)),

    % Verify roundtrip
    {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
    ?assertEqual(Data, Decompressed).

%% @doc Test compression with custom level
test_compress_with_custom_level(_Config) ->
    ct:pal("Testing compression with custom levels"),

    Data = crypto:strong_rand_bytes(8192),

    % Test different levels
    Levels = [1, 3, 9, 15, 19],
    lists:foreach(fun(Level) ->
        {ok, Compressed} = erlmcp_compression:compress(Data, Level),
        ct:pal("  Level ~p: ~p -> ~p bytes",
                [Level, byte_size(Data), byte_size(Compressed)]),
        ?assert(is_binary(Compressed)),

        % Verify roundtrip
        {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
        ?assertEqual(Data, Decompressed)
    end, Levels).

%% @doc Test compression with metadata
test_compress_with_metadata(_Config) ->
    ct:pal("Testing compression with metadata"),

    Data = crypto:strong_rand_bytes(16384),

    % Compress with metadata
    {ok, Compressed, Metadata} = erlmcp_compression:compress_with_metadata(Data),

    % Verify metadata structure
    ?assertMatch(#{encoding := <<"zstd">>,
                   original_size := _,
                   compressed_size := _,
                   ratio := _}, Metadata),

    ?assertEqual(<<"zstd">>, maps:get(encoding, Metadata)),
    ?assertEqual(byte_size(Data), maps:get(original_size, Metadata)),
    ?assertEqual(byte_size(Compressed), maps:get(compressed_size, Metadata)),

    % Verify compression ratio
    Ratio = maps:get(ratio, Metadata),
    ct:pal("  Compression ratio: ~.4f", [Ratio]),
    ?assert(Ratio > 0.0 andalso Ratio < 1.0,
            "Compression ratio should be between 0 and 1"),

    % Verify roundtrip
    {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
    ?assertEqual(Data, Decompressed).

%%====================================================================
%% Compression Level Tests
%%====================================================================

%% @doc Test all compression levels (1-22)
test_all_compression_levels(_Config) ->
    ct:pal("Testing all compression levels (1-22)"),

    Data = crypto:strong_rand_bytes(4096),
    Levels = lists:seq(1, 22),

    Results = lists:map(fun(Level) ->
        {ok, Compressed} = erlmcp_compression:compress(Data, Level),
        Ratio = byte_size(Compressed) / byte_size(Data),
        ct:pal("  Level ~2p: ratio ~.4f", [Level, Ratio]),
        {Level, Ratio}
    end, Levels),

    % Higher levels should generally compress better (lower ratio)
    % Note: This is a general trend, not guaranteed for all data
    {_MinLevel, MinRatio} = hd(lists:keysort(2, Results)),
    {_MaxLevel, MaxRatio} = hd(lists:reverse(lists:keysort(2, Results))),

    ct:pal("  Best compression: ~.4f", [MinRatio]),
    ct:pal("  Worst compression: ~.4f", [MaxRatio]),

    % All levels should produce valid compressed data
    lists:foreach(fun({Level, _Ratio}) ->
        {ok, Compressed} = erlmcp_compression:compress(Data, Level),
        {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
        ?assertEqual(Data, Decompressed)
    end, Results).

%% @doc Test fast compression levels (1-3)
test_fast_compression_levels(_Config) ->
    ct:pal("Testing fast compression levels (1-3)"),

    Data = crypto:strong_rand_bytes(10240), % 10KB
    FastLevels = [1, 2, 3],

    % Measure compression speed for fast levels
    Times = lists:map(fun(Level) ->
        {Time, {ok, Compressed}} = timer:tc(fun() ->
            erlmcp_compression:compress(Data, Level)
        end),
        Ratio = byte_size(Compressed) / byte_size(Data),
        ct:pal("  Level ~p: ~p us, ratio ~.4f",
                [Level, Time, Ratio]),
        {Level, Time, Ratio}
    end, FastLevels),

    % Fast levels should complete quickly (< 10ms for 10KB)
    lists:foreach(fun({_Level, Time, _Ratio}) ->
        ?assert(Time < 10000, "Fast compression should be < 10ms")
    end, Times).

%% @doc Test default compression level (3)
test_default_compression_level(_Config) ->
    ct:pal("Testing default compression level (3)"),

    Data = crypto:strong_rand_bytes(8192),

    % Compress with default and explicit level 3
    {ok, CompressedDefault} = erlmcp_compression:compress(Data),
    {ok, CompressedLevel3} = erlmcp_compression:compress(Data, 3),

    % Both should produce identical results
    ?assertEqual(CompressedLevel3, CompressedDefault,
                 "Default level should be 3").

%% @doc Test archival compression levels (16-22)
test_archival_compression_levels(_Config) ->
    ct:pal("Testing archival compression levels (16-22)"),

    Data = crypto:strong_rand_bytes(4096),
    ArchivalLevels = lists:seq(16, 22),

    lists:foreach(fun(Level) ->
        {ok, Compressed} = erlmcp_compression:compress(Data, Level),
        Ratio = byte_size(Compressed) / byte_size(Data),
        ct:pal("  Archival level ~p: ratio ~.4f", [Level, Ratio]),

        % Verify valid compression
        ?assert(Ratio > 0.0 andalso Ratio =< 1.0),

        % Verify roundtrip
        {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
        ?assertEqual(Data, Decompressed)
    end, ArchivalLevels).

%%====================================================================
%% Threshold Tests
%%====================================================================

%% @doc Test compression below threshold (should not compress)
test_compress_threshold_below(_Config) ->
    ct:pal("Testing compression below threshold"),

    % Set threshold to 10KB
    ok = erlmcp_compression:set_compression_threshold(10240),

    % Generate 5KB data (below threshold)
    SmallData = crypto:strong_rand_bytes(5120),

    % Compress with threshold
    {ok, Result} = erlmcp_compression:compress_threshold(SmallData),

    % Should return uncompressed data
    ?assertEqual(SmallData, Result,
                 "Data below threshold should not be compressed").

%% @doc Test compression above threshold (should compress)
test_compress_threshold_above(_Config) ->
    ct:pal("Testing compression above threshold"),

    % Set threshold to 5KB
    ok = erlmcp_compression:set_compression_threshold(5120),

    % Generate 10KB data (above threshold)
    LargeData = crypto:strong_rand_bytes(10240),

    % Compress with threshold
    {ok, Compressed, Metadata} = erlmcp_compression:compress_threshold(LargeData),

    % Should return compressed data with metadata
    ?assertMatch(#{encoding := <<"zstd">>}, Metadata),
    ?assert(byte_size(Compressed) < byte_size(LargeData)),

    % Verify decompression
    {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
    ?assertEqual(LargeData, Decompressed).

%% @doc Test compression at exact threshold (should compress)
test_compress_threshold_exact(_Config) ->
    ct:pal("Testing compression at exact threshold"),

    % Set threshold to 8KB
    ok = erlmcp_compression:set_compression_threshold(8192),

    % Generate exactly 8KB data
    ExactData = crypto:strong_rand_bytes(8192),

    % Compress with threshold
    {ok, Result} = erlmcp_compression:compress_threshold(ExactData),

    % Should compress (>= threshold)
    case Result of
        {ok, Compressed, Metadata} ->
            ?assertMatch(#{encoding := <<"zstd">>}, Metadata);
        {ok, _Data} ->
            ct:fail("Data at threshold should be compressed")
    end.

%% @doc Test setting compression threshold
test_set_compression_threshold(_Config) ->
    ct:pal("Testing setting compression threshold"),

    % Test various thresholds
    Thresholds = [0, 1024, 10240, 1048576, 10485760], % 0, 1KB, 10KB, 1MB, 10MB

    lists:foreach(fun(Threshold) ->
        ok = erlmcp_compression:set_compression_threshold(Threshold),
        ct:pal("  Set threshold to ~p bytes", [Threshold])
    end, Thresholds).

%% @doc Test zero threshold (always compress)
test_zero_threshold_always_compress(_Config) ->
    ct:pal("Testing zero threshold (always compress)"),

    % Set threshold to 0 (always compress)
    ok = erlmcp_compression:set_compression_threshold(0),

    % Generate small data (1 byte)
    TinyData = <<42>>,

    % Should still compress
    {ok, Compressed, Metadata} = erlmcp_compression:compress_threshold(TinyData),

    % Verify compressed
    ?assertMatch(#{encoding := <<"zstd">>}, Metadata),

    % Verify decompression
    {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
    ?assertEqual(TinyData, Decompressed).

%%====================================================================
%% Decompression Tests
%%====================================================================

%% @doc Test decompression of valid data
test_decompress_valid_data(_Config) ->
    ct:pal("Testing decompression of valid data"),

    Original = crypto:strong_rand_bytes(4096),
    {ok, Compressed} = erlmcp_compression:compress(Original),
    {ok, Decompressed} = erlmcp_compression:decompress(Compressed),

    ?assertEqual(Original, Decompressed).

%% @doc Test decompression of invalid data
test_decompress_invalid_data(_Config) ->
    ct:pal("Testing decompression of invalid data"),

    InvalidData = <<1, 2, 3, 4, 5>>,

    % Should return error
    Result = erlmcp_compression:decompress(InvalidData),

    case Result of
        {error, _Reason} ->
            ok;  % Expected
        {ok, _} ->
            ct:fail("Decompressing invalid data should fail")
    end.

%% @doc Test decompression of empty data
test_decompress_empty_data(_Config) ->
    ct:pal("Testing decompression of empty data"),

    % Empty compressed data
    Result = erlmcp_compression:decompress(<<>>),

    % Should return error (empty data is invalid zstd)
    case Result of
        {error, _Reason} ->
            ok;  % Expected
        {ok, _} ->
            ct:fail("Decompressing empty data should fail")
    end.

%%====================================================================
%% Edge Cases
%%====================================================================

%% @doc Test empty binary
test_empty_binary(_Config) ->
    ct:pal("Testing empty binary"),

    EmptyData = <<>>,

    % Compress empty data
    {ok, Compressed} = erlmcp_compression:compress(EmptyData),

    % Verify compressed
    ?assert(is_binary(Compressed)),
    ?assert(byte_size(Compressed) > 0, "Compressed empty data should have size"),

    % Verify decompression
    {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
    ?assertEqual(EmptyData, Decompressed).

%% @doc Test small binary (100 bytes)
test_small_binary(_Config) ->
    ct:pal("Testing small binary (100 bytes)"),

    SmallData = crypto:strong_rand_bytes(100),

    {ok, Compressed} = erlmcp_compression:compress(SmallData),
    {ok, Decompressed} = erlmcp_compression:decompress(Compressed),

    ?assertEqual(SmallData, Decompressed).

%% @doc Test large binary (1MB)
test_large_binary(_Config) ->
    ct:pal("Testing large binary (1MB)"),

    LargeData = crypto:strong_rand_bytes(1048576), % 1MB

    {CompressTime, {ok, Compressed}} = timer:tc(fun() ->
        erlmcp_compression:compress(LargeData)
    end),

    Ratio = byte_size(Compressed) / byte_size(LargeData),
    ct:pal("  Compressed 1MB in ~p us (~.2f ms)", [CompressTime, CompressTime / 1000]),
    ct:pal("  Compression ratio: ~.4f", [Ratio]),

    % Should achieve reasonable compression
    ?assert(Ratio < 0.8, "Should achieve at least 20% compression"),

    % Verify decompression
    {DecompressTime, {ok, Decompressed}} = timer:tc(fun() ->
        erlmcp_compression:decompress(Compressed)
    end),

    ct:pal("  Decompressed in ~p us (~.2f ms)", [DecompressTime, DecompressTime / 1000]),

    ?assertEqual(LargeData, Decompressed).

%% @doc Test very large binary (10MB)
test_very_large_binary(_Config) ->
    ct:pal("Testing very large binary (10MB)"),

    VeryLargeData = crypto:strong_rand_bytes(10485760), % 10MB

    {CompressTime, {ok, Compressed}} = timer:tc(fun() ->
        erlmcp_compression:compress(VeryLargeData)
    end),

    Ratio = byte_size(Compressed) / byte_size(VeryLargeData),
    ct:pal("  Compressed 10MB in ~p us (~.2f ms, ~.2f s)",
            [CompressTime, CompressTime / 1000, CompressTime / 1000000]),
    ct:pal("  Compression ratio: ~.4f", [Ratio]),

    % Verify decompression
    {DecompressTime, {ok, Decompressed}} = timer:tc(fun() ->
        erlmcp_compression:decompress(Compressed)
    end),

    ct:pal("  Decompressed in ~p us (~.2f ms, ~.2f s)",
            [DecompressTime, DecompressTime / 1000, DecompressTime / 1000000]),

    ?assertEqual(VeryLargeData, Decompressed).

%% @doc Test already compressed data (should not compress well)
test_already_compressed_data(_Config) ->
    ct:pal("Testing already compressed data"),

    % Generate random data and compress it
    Original = crypto:strong_rand_bytes(8192),
    {ok, CompressedOnce} = erlmcp_compression:compress(Original),

    % Try to compress already compressed data
    {ok, CompressedTwice} = erlmcp_compression:compress(CompressedOnce),

    % Second compression should not help much (ratio close to 1.0)
    Ratio1 = byte_size(CompressedOnce) / byte_size(Original),
    Ratio2 = byte_size(CompressedTwice) / byte_size(CompressedOnce),

    ct:pal("  First compression ratio: ~.4f", [Ratio1]),
    ct:pal("  Second compression ratio: ~.4f", [Ratio2]),

    % Second compression should be less effective
    ?assert(Ratio2 > Ratio1, "Compressing already compressed data is less effective").

%%====================================================================
%% Statistics Tests
%%====================================================================

%% @doc Test initial statistics
test_get_initial_stats(_Config) ->
    ct:pal("Testing initial statistics"),

    % Reset stats
    ok = erlmcp_compression:reset_stats(),

    % Get initial stats
    {ok, Stats} = erlmcp_compression:get_stats(),

    % Verify initial state
    ?assertEqual(0, maps:get(total_compress_ops, Stats)),
    ?assertEqual(0, maps:get(total_decompress_ops, Stats)),
    ?assertEqual(0, maps:get(total_original_bytes, Stats)),
    ?assertEqual(0, maps:get(total_compressed_bytes, Stats)),
    ?assertEqual(0.0, maps:get(avg_ratio, Stats)).

%% @doc Test statistics tracking
test_stats_tracking(_Config) ->
    ct:pal("Testing statistics tracking"),

    % Reset stats
    ok = erlmcp_compression:reset_stats(),

    % Perform some compression operations
    Data1 = crypto:strong_rand_bytes(4096),
    Data2 = crypto:strong_rand_bytes(8192),
    Data3 = crypto:strong_rand_bytes(16384),

    % Compress with threshold (tracks stats)
    ok = erlmcp_compression:set_compression_threshold(0), % Always compress
    {ok, _, _} = erlmcp_compression:compress_threshold(Data1),
    {ok, _, _} = erlmcp_compression:compress_threshold(Data2),
    {ok, _, _} = erlmcp_compression:compress_threshold(Data3),

    % Get stats
    {ok, Stats} = erlmcp_compression:get_stats(),

    % Verify tracking
    ?assertEqual(3, maps:get(total_compress_ops, Stats)),
    ?assertEqual(28672, maps:get(total_original_bytes, Stats)), % 4KB + 8KB + 16KB
    ?assert(maps:get(total_compressed_bytes, Stats) > 0),
    ?assert(maps:get(total_compressed_bytes, Stats) < 28672).

%% @doc Test reset statistics
test_reset_stats(_Config) ->
    ct:pal("Testing reset statistics"),

    % Perform some operations
    Data = crypto:strong_rand_bytes(4096),
    ok = erlmcp_compression:set_compression_threshold(0),
    {ok, _, _} = erlmcp_compression:compress_threshold(Data),

    % Reset stats
    ok = erlmcp_compression:reset_stats(),

    % Verify stats cleared
    {ok, Stats} = erlmcp_compression:get_stats(),
    ?assertEqual(0, maps:get(total_compress_ops, Stats)),
    ?assertEqual(0, maps:get(total_original_bytes, Stats)),
    ?assertEqual(0, maps:get(total_compressed_bytes, Stats)).

%% @doc Test average ratio calculation
test_stats_average_ratio(_Config) ->
    ct:pal("Testing average ratio calculation"),

    % Reset stats
    ok = erlmcp_compression:reset_stats(),

    % Perform compressions
    Data1 = crypto:strong_rand_bytes(4096),
    Data2 = crypto:strong_rand_bytes(8192),

    ok = erlmcp_compression:set_compression_threshold(0),
    {ok, Compressed1, _} = erlmcp_compression:compress_threshold(Data1),
    {ok, Compressed2, _} = erlmcp_compression:compress_threshold(Data2),

    % Get stats
    {ok, Stats} = erlmcp_compression:get_stats(),

    % Calculate expected average ratio
    TotalOriginal = byte_size(Data1) + byte_size(Data2),
    TotalCompressed = byte_size(Compressed1) + byte_size(Compressed2),
    ExpectedAvgRatio = TotalCompressed / TotalOriginal,
    ActualAvgRatio = maps:get(avg_ratio, Stats),

    ct:pal("  Expected avg ratio: ~.4f", [ExpectedAvgRatio]),
    ct:pal("  Actual avg ratio: ~.4f", [ActualAvgRatio]),

    ?assertEqual(ExpectedAvgRatio, ActualAvgRatio).

%%====================================================================
%% Performance Tests
%%====================================================================

%% @doc Benchmark zstd vs zlib
test_benchmark_zstd_vs_zlib(_Config) ->
    ct:pal("Benchmarking zstd vs zlib"),

    Sizes = [1024, 10240, 102400, 1048576], % 1KB, 10KB, 100KB, 1MB

    lists:foreach(fun(Size) ->
        ct:pal("~nBenchmarking ~p bytes:", [Size]),

        Data = crypto:strong_rand_bytes(Size),

        % Benchmark zstd
        {ZstdCompressTime, {ok, ZstdCompressed}} = timer:tc(fun() ->
            erlmcp_compression:compress(Data)
        end),
        {ZstdDecompressTime, {ok, _ZstdDecompressed}} = timer:tc(fun() ->
            erlmcp_compression:decompress(ZstdCompressed)
        end),

        % Benchmark zlib
        {ZlibCompressTime, ZlibCompressed} = timer:tc(fun() ->
            zlib:compress(Data)
        end),
        {ZlibDecompressTime, _ZlibDecompressed} = timer:tc(fun() ->
            zlib:uncompress(ZlibCompressed)
        end),

        % Calculate metrics
        ZstdRatio = byte_size(ZstdCompressed) / Size,
        ZlibRatio = byte_size(ZlibCompressed) / Size,

        ct:pal("  zstd:"),
        ct:pal("    Compress: ~p us (~.2f ms)", [ZstdCompressTime, ZstdCompressTime / 1000]),
        ct:pal("    Decompress: ~p us (~.2f ms)", [ZstdDecompressTime, ZstdDecompressTime / 1000]),
        ct:pal("    Ratio: ~.4f", [ZstdRatio]),
        ct:pal("  zlib:"),
        ct:pal("    Compress: ~p us (~.2f ms)", [ZlibCompressTime, ZlibCompressTime / 1000]),
        ct:pal("    Decompress: ~p us (~.2f ms)", [ZlibDecompressTime, ZlibDecompressTime / 1000]),
        ct:pal("    Ratio: ~.4f", [ZlibRatio]),

        % zstd should generally be faster and compress better
        SpaceImprovement = (ZlibRatio - ZstdRatio) / ZlibRatio * 100,
        SpeedImprovement = ZlibCompressTime / ZstdCompressTime,

        ct:pal("  Comparison:"),
        ct:pal("    Space improvement: ~.2f%", [SpaceImprovement]),
        ct:pal("    Speed improvement: ~.2fx", [SpeedImprovement])

    end, Sizes).

%% @doc Test compression speed
test_compression_speed(_Config) ->
    ct:pal("Testing compression speed"),

    Data = crypto:strong_rand_bytes(1048576), % 1MB

    % Measure 10 iterations
    Iterations = 10,
    {TotalTime, _Results} = timer:tc(fun() ->
        [erlmcp_compression:compress(Data) || _ <- lists:seq(1, Iterations)]
    end),

    AvgTime = TotalTime / Iterations,
    Throughput = (byte_size(Data) / 1024 / 1024) / (AvgTime / 1000000),

    ct:pal("  Average compression time: ~p us (~.2f ms)", [AvgTime, AvgTime / 1000]),
    ct:pal("  Throughput: ~.2f MB/s", [Throughput]),

    % Should compress 1MB in reasonable time (< 100ms)
    ?assert(AvgTime < 100000, "1MB compression should be < 100ms").

%% @doc Test decompression speed
test_decompression_speed(_Config) ->
    ct:pal("Testing decompression speed"),

    Data = crypto:strong_rand_bytes(1048576), % 1MB
    {ok, Compressed} = erlmcp_compression:compress(Data),

    % Measure 10 iterations
    Iterations = 10,
    {TotalTime, _Results} = timer:tc(fun() ->
        [erlmcp_compression:decompress(Compressed) || _ <- lists:seq(1, Iterations)]
    end),

    AvgTime = TotalTime / Iterations,
    Throughput = (byte_size(Data) / 1024 / 1024) / (AvgTime / 1000000),

    ct:pal("  Average decompression time: ~p us (~.2f ms)", [AvgTime, AvgTime / 1000]),
    ct:pal("  Throughput: ~.2f MB/s", [Throughput]),

    % Decompression should be very fast (< 50ms for 1MB)
    ?assert(AvgTime < 50000, "1MB decompression should be < 50ms").

%%====================================================================
%% gen_server Behavior Tests
%%====================================================================

%% @doc Test gen_server lifecycle
test_gen_server_lifecycle(_Config) ->
    ct:pal("Testing gen_server lifecycle"),

    % Server should be running from init_per_suite
    Pid = whereis(erlmcp_compression),
    ?assert(is_pid(Pid)),

    % Test call
    {ok, Stats} = erlmcp_compression:get_stats(),
    ?assert(is_map(Stats)),

    % Stop and restart
    erlmcp_compression:stop(),
    timer:sleep(100),

    ?assertEqual(undefined, whereis(erlmcp_compression)),

    {ok, NewPid} = erlmcp_compression:start_link(),
    ?assert(is_pid(NewPid)),
    ?assertNotEqual(undefined, whereis(erlmcp_compression)).

%% @doc Test concurrent compression requests
test_concurrent_compression_requests(_Config) ->
    ct:pal("Testing concurrent compression requests"),

    % Spawn multiple processes compressing concurrently
    Parent = self(),
    Data = crypto:strong_rand_bytes(4096),

    Pids = [spawn(fun() ->
        Result = erlmcp_compression:compress(Data),
        Parent ! {result, self(), Result}
    end) || _ <- lists:seq(1, 20)],

    % Collect results
    Results = [receive
        {result, Pid, Result} ->
            {Pid, Result}
    end || Pid <- Pids],

    % Verify all succeeded
    lists:foreach(fun({_Pid, Result}) ->
        ?assertMatch({ok, _Compressed}, Result)
    end, Results),

    ct:pal("  All ~p concurrent requests succeeded", [length(Results)]).

%% @doc Test concurrent threshold compression
test_concurrent_threshold_compression(_Config) ->
    ct:pal("Testing concurrent threshold compression"),

    % Reset stats and set threshold
    ok = erlmcp_compression:reset_stats(),
    ok = erlmcp_compression:set_compression_threshold(2048), % 2KB threshold

    % Spawn multiple processes
    Parent = self(),
    Sizes = [1024, 2048, 4096, 8192, 16384], % Mix of below/at/above threshold

    Pids = lists:map(fun(Size) ->
        Data = crypto:strong_rand_bytes(Size),
        spawn(fun() ->
            Result = erlmcp_compression:compress_threshold(Data),
            Parent ! {result, Size, Result}
        end)
    end, Sizes),

    % Collect results
    Results = [receive
        {result, Size, Result} ->
            {Size, Result}
    end || _ <- Pids],

    % Verify results
    lists:foreach(fun({Size, Result}) ->
        case Size < 2048 of
            true ->
                % Below threshold - should return uncompressed
                ?assertMatch({ok, _Data}, Result);
            false ->
                % At or above threshold - should return compressed with metadata
                ?assertMatch({ok, _Compressed, #{encoding := <<"zstd">>}}, Result)
        end
    end, Results),

    % Check stats
    {ok, Stats} = erlmcp_compression:get_stats(),
    CompressedCount = maps:get(total_compress_ops, Stats),
    ct:pal("  Stats show ~p compression operations", [CompressedCount]),
    ?assert(CompressedCount >= 3). % At least 3 sizes >= 2048
