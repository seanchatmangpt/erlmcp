-module(erlmcp_bench_compression).
-include("erlmcp.hrl").

%% Benchmark exports
-export([run_all/0, run_compression_suite/0, run_comparison/1]).
-export([bench_zstd/1, bench_zlib/1, bench_threshold/0]).
-export([run_real_workload/0]).

%%%====================================================================
%%% Benchmark Suite
%%%====================================================================

%% @doc Run all compression benchmarks
run_all() ->
    io:format("~n=== ErlMCP Compression Benchmark Suite (OTP 28 zstd) ===~n~n"),
    run_compression_suite(),
    run_comparison(100),
    run_threshold(),
    run_real_workload(),
    io:format("~n=== All benchmarks complete ===~n").

%% @doc Run comprehensive compression suite with various data sizes
run_compression_suite() ->
    io:format("~n--- Compression Performance Suite ---~n"),
    DataSizes = [
        {small, 1024},              % 1KB
        {medium, 1024 * 100},       % 100KB
        {large, 1024 * 1024},       % 1MB
        {xlarge, 1024 * 1024 * 10}  % 10MB
    ],

    lists:foreach(fun({Name, Size}) ->
        io:format("~nTesting ~p (~p bytes):~n", [Name, Size]),
        bench_zstd(Size),
        bench_zlib(Size),
        io:format("~n")
    end, DataSizes).

%% @doc Run zstd vs zlib comparison at specified size
run_comparison(Size) ->
    io:format("~n--- Zstd vs Zlib Comparison (~p bytes) ---~n", [Size]),
    Results = erlmcp_compression:benchmark_comparison(Size),

    Zstd = maps:get(zstd, Results),
    Zlib = maps:get(zlib, Results),
    Comp = maps:get(comparison, Results),

    io:format("Zstd:~n"),
    io:format("  Compressed size: ~p bytes~n", [maps:get(compressed_size, Zstd)]),
    io:format("  Ratio: ~.4f~n", [maps:get(compression_ratio, Zstd)]),
    io:format("  Compress time: ~p us~n", [maps:get(compress_time_us, Zstd)]),
    io:format("  Decompress time: ~p us~n", [maps:get(decompress_time_us, Zstd)]),
    io:format("  Throughput: ~.2f MB/s~n", [maps:get(throughput_mb_s, Zstd)]),

    io:format("Zlib:~n"),
    io:format("  Compressed size: ~p bytes~n", [maps:get(compressed_size, Zlib)]),
    io:format("  Ratio: ~.4f~n", [maps:get(compression_ratio, Zlib)]),
    io:format("  Compress time: ~p us~n", [maps:get(compress_time_us, Zlib)]),
    io:format("  Decompress time: ~p us~n", [maps:get(decompress_time_us, Zlib)]),
    io:format("  Throughput: ~.2f MB/s~n", [maps:get(throughput_mb_s, Zlib)]),

    io:format("Improvement:~n"),
    io:format("  Space: ~.2f%~n", [maps:get(space_improvement, Comp)]),
    io:format("  Speed: ~.2fx~n", [maps:get(speed_improvement, Comp)]).

%% @doc Benchmark threshold-based compression
run_threshold() ->
    io:format("~n--- Threshold Compression Benchmark ---~n"),
    {ok, Pid} = erlmcp_compression:start_link(),

    %% Set threshold to 100KB
    ok = erlmcp_compression:set_compression_threshold(102400),

    %% Test small data (should not compress)
    SmallData = crypto:strong_rand_bytes(1024 * 50), % 50KB
    {TimeSmall, {ok, SmallResult}} = timer:tc(fun() ->
        erlmcp_compression:compress_threshold(SmallData)
    end),
    io:format("Small data (50KB): ~p us, compressed: ~p~n",
              [TimeSmall, byte_size(SmallResult) < byte_size(SmallData)]),

    %% Test large data (should compress)
    LargeData = crypto:strong_rand_bytes(1024 * 500), % 500KB
    {TimeLarge, {ok, LargeCompressed, Metadata}} = timer:tc(fun() ->
        erlmcp_compression:compress_threshold(LargeData)
    end),
    io:format("Large data (500KB): ~p us, ratio: ~.4f~n",
              [TimeLarge, maps:get(ratio, Metadata)]),

    %% Get stats
    {ok, Stats} = erlmcp_compression:get_stats(),
    io:format("Stats: ~p~n", [Stats]),

    erlmcp_compression:stop().

%% @doc Benchmark zstd compression at different levels
bench_zstd(Size) ->
    Data = crypto:strong_rand_bytes(Size),

    %% Benchmark default level (3)
    {TimeDefault, {ok, CompressedDefault}} = timer:tc(fun() ->
        erlmcp_compression:compress(Data)
    end),
    RatioDefault = byte_size(CompressedDefault) / Size,
    ThroughputDefault = (Size / 1024 / 1024) / (TimeDefault / 1_000_000),

    io:format("  Zstd (level 3): ~p us, ratio: ~.4f, throughput: ~.2f MB/s~n",
              [TimeDefault, RatioDefault, ThroughputDefault]),

    %% Benchmark fast level (1)
    {TimeFast, {ok, CompressedFast}} = timer:tc(fun() ->
        erlmcp_compression:compress(Data, 1)
    end),
    RatioFast = byte_size(CompressedFast) / Size,
    ThroughputFast = (Size / 1024 / 1024) / (TimeFast / 1_000_000),

    io:format("  Zstd (level 1): ~p us, ratio: ~.4f, throughput: ~.2f MB/s~n",
              [TimeFast, RatioFast, ThroughputFast]),

    %% Benchmark best level (22)
    {TimeBest, {ok, CompressedBest}} = timer:tc(fun() ->
        erlmcp_compression:compress(Data, 22)
    end),
    RatioBest = byte_size(CompressedBest) / Size,
    ThroughputBest = (Size / 1024 / 1024) / (TimeBest / 1_000_000),

    io:format("  Zstd (level 22): ~p us, ratio: ~.4f, throughput: ~.2f MB/s~n",
              [TimeBest, RatioBest, ThroughputBest]).

%% @doc Benchmark zlib compression
bench_zlib(Size) ->
    Data = crypto:strong_rand_bytes(Size),

    {Time, Compressed} = timer:tc(fun() ->
        zlib:compress(Data)
    end),
    Ratio = byte_size(Compressed) / Size,
    Throughput = (Size / 1024 / 1024) / (Time / 1_000_000),

    io:format("  Zlib: ~p us, ratio: ~.4f, throughput: ~.2f MB/s~n",
              [Time, Ratio, Throughput]).

%% @doc Benchmark with realistic MCP workloads
run_real_workload() ->
    io:format("~n--- Real MCP Workload Simulation ---~n"),

    %% Simulate resource data (files, logs)
    ResourceData = generate_json_resource(1024 * 256), % 256KB JSON
    {Time1, {ok, Compressed1}} = timer:tc(fun() ->
        erlmcp_compression:compress(ResourceData)
    end),
    io:format("Resource data (256KB JSON): ~p us, ratio: ~.4f~n",
              [Time1, byte_size(Compressed1) / byte_size(ResourceData)]),

    %% Simulate tool output
    ToolOutput = generate_tool_output(1024 * 512), % 512KB text
    {Time2, {ok, Compressed2}} = timer:tc(fun() ->
        erlmcp_compression:compress(ToolOutput)
    end),
    io:format("Tool output (512KB text): ~p us, ratio: ~.4f~n",
              [Time2, byte_size(Compressed2) / byte_size(ToolOutput)]),

    %% Simulate conversation history
    History = generate_conversation_history(1024 * 1024), % 1MB history
    {Time3, {ok, Compressed3, Metadata}} = timer:tc(fun() ->
        erlmcp_compression:compress_with_metadata(History)
    end),
    io:format("Conversation history (1MB): ~p us, ratio: ~.4f~n",
              [Time3, maps:get(ratio, Metadata)]),

    %% Simulate log batch
    LogBatch = generate_log_batch(1024 * 2048), % 2MB logs
    {Time4, {ok, Compressed4}} = timer:tc(fun() ->
        erlmcp_compression:compress(LogBatch)
    end),
    io:format("Log batch (2MB): ~p us, ratio: ~.4f~n",
              [Time4, byte_size(Compressed4) / byte_size(LogBatch)]).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Generate JSON-like resource data
generate_json_resource(Size) ->
    BaseJson = <<"{\"data\": \"", (binary:copy(<<$x>>, Size))/binary, "\"}">>,
    BaseJson.

%% @doc Generate tool output (text with patterns)
generate_tool_output(Size) ->
    BaseText = <<"Tool output: ", (binary:copy(<<$a>>, Size div 2))/binary, "\n">>,
    BaseText.

%% @doc Generate conversation history (JSON array)
generate_conversation_history(Size) ->
    Message = <<"{\"role\": \"user\", \"content\": \"",
                (binary:copy(<<$b>>, Size div 10))/binary, "\"}">>,
    Messages = lists:duplicate(10, Message),
    Joined = lists:foldl(fun(M, Acc) ->
        <<Acc/binary, ",\n", M/binary>>
    end, hd(Messages), tl(Messages)),
    <<"[", Joined/binary, "]">>.

%% @doc Generate log batch (multiple log lines)
generate_log_batch(Size) ->
    LogLine = <<"[2026-02-01 12:00:00] INFO: Log message: ",
                (binary:copy(<<$c>>, 100))/binary, "\n">>,
    Lines = Size div byte_size(LogLine) + 1,
    binary:copy(LogLine, Lines).
