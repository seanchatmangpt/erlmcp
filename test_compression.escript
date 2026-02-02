#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/erlmcp_core/ebin

main(_) ->
    io:format("~n=== Testing erlmcp_compression (OTP 28 zstd) ===~n~n"),

    %% Test 1: Basic compression
    io:format("Test 1: Basic compression (100KB)~n"),
    Data = crypto:strong_rand_bytes(1024 * 100),

    {ok, Compressed} = erlmcp_compression:compress(Data),
    Ratio = byte_size(Compressed) / byte_size(Data),
    io:format("  Original: ~p bytes~n", [byte_size(Data)]),
    io:format("  Compressed: ~p bytes~n", [byte_size(Compressed)]),
    io:format("  Ratio: ~.4f~n", [Ratio]),

    %% Test 2: Roundtrip
    io:format("~nTest 2: Roundtrip compression/decompression~n"),
    {ok, Decompressed} = erlmcp_compression:decompress(Compressed),
    case Data =:= Decompressed of
        true -> io:format("  Roundtrip: SUCCESS~n");
        false -> io:format("  Roundtrip: FAILED~n"), halt(1)
    end,

    %% Test 3: Compression with metadata
    io:format("~nTest 3: Compression with metadata~n"),
    {ok, _, Metadata} = erlmcp_compression:compress_with_metadata(Data),
    io:format("  Encoding: ~p~n", [maps:get(encoding, Metadata)]),
    io:format("  Original size: ~p bytes~n", [maps:get(original_size, Metadata)]),
    io:format("  Compressed size: ~p bytes~n", [maps:get(compressed_size, Metadata)]),
    io:format("  Ratio: ~.4f~n", [maps:get(ratio, Metadata)]),

    %% Test 4: Different compression levels
    io:format("~nTest 4: Compression levels comparison~n"),
    {ok, L1} = erlmcp_compression:compress(Data, 1),
    {ok, L3} = erlmcp_compression:compress(Data, 3),
    {ok, L10} = erlmcp_compression:compress(Data, 10),
    {ok, L22} = erlmcp_compression:compress(Data, 22),
    io:format("  Level 1 (fastest): ~p bytes (ratio ~.4f)~n",
              [byte_size(L1), byte_size(L1) / byte_size(Data)]),
    io:format("  Level 3 (default): ~p bytes (ratio ~.4f)~n",
              [byte_size(L3), byte_size(L3) / byte_size(Data)]),
    io:format("  Level 10 (good): ~p bytes (ratio ~.4f)~n",
              [byte_size(L10), byte_size(L10) / byte_size(Data)]),
    io:format("  Level 22 (best): ~p bytes (ratio ~.4f)~n",
              [byte_size(L22), byte_size(L22) / byte_size(Data)]),

    %% Test 5: Benchmark comparison
    io:format("~nTest 5: Zstd vs Zlib benchmark~n"),
    Results = erlmcp_compression:benchmark_comparison(1024 * 100),
    Zstd = maps:get(zstd, Results),
    Zlib = maps:get(zlib, Results),
    Comp = maps:get(comparison, Results),
    io:format("  Zstd ratio: ~.4f~n", [maps:get(compression_ratio, Zstd)]),
    io:format("  Zlib ratio: ~.4f~n", [maps:get(compression_ratio, Zlib)]),
    io:format("  Space improvement: ~.1f%~n", [maps:get(space_improvement, Comp)]),
    io:format("  Speed improvement: ~.1fx~n", [maps:get(speed_improvement, Comp)]),

    %% Test 6: Threshold-based compression
    io:format("~nTest 6: Threshold-based compression~n"),
    {ok, _} = erlmcp_compression:start_link(),
    ok = erlmcp_compression:set_compression_threshold(102400), % 100KB

    SmallData = crypto:strong_rand_bytes(1024 * 50), % 50KB
    {ok, SmallResult} = erlmcp_compression:compress_threshold(SmallData),
    io:format("  Small data (50KB): compressed = ~p~n",
              [byte_size(SmallResult) < byte_size(SmallData)]),

    LargeData = crypto:strong_rand_bytes(1024 * 500), % 500KB
    {ok, LargeResult, LargeMeta} = erlmcp_compression:compress_threshold(LargeData),
    io:format("  Large data (500KB): compressed = ~p, ratio = ~.4f~n",
              [byte_size(LargeResult) < byte_size(LargeData),
               maps:get(ratio, LargeMeta)]),

    %% Test 7: Statistics
    io:format("~nTest 7: Compression statistics~n"),
    {ok, Stats} = erlmcp_compression:get_stats(),
    io:format("  Total compress ops: ~p~n", [maps:get(total_compress_ops, Stats)]),
    io:format("  Total original bytes: ~p~n", [maps:get(total_original_bytes, Stats)]),
    io:format("  Total compressed bytes: ~p~n", [maps:get(total_compressed_bytes, Stats)]),
    io:format("  Average ratio: ~.4f~n", [maps:get(avg_ratio, Stats)]),

    erlmcp_compression:stop(),

    io:format("~n=== All tests PASSED ===~n"),
    halt(0).
