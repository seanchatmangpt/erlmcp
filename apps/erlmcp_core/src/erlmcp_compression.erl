-module(erlmcp_compression).
-behaviour(gen_server).

%% API exports
-export([start_link/0, stop/0]).
-export([compress/1, compress/2, decompress/1, compress_with_metadata/1]).
-export([compress_threshold/1, set_compression_threshold/1]).
-export([get_stats/0, reset_stats/0]).
-export([benchmark_comparison/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Include records
-include("erlmcp.hrl").

%%%====================================================================
%%% Types
%%%====================================================================

-type compression_level() :: 1..22.
-type compression_threshold() :: non_neg_integer(). % Bytes, 0 = always compress
-type compression_metadata() :: #{
    encoding := binary(),
    original_size := non_neg_integer(),
    compressed_size := non_neg_integer(),
    ratio := float()
}.
-type compression_stats() :: #{
    total_compress_ops := non_neg_integer(),
    total_decompress_ops := non_neg_integer(),
    total_original_bytes => non_neg_integer(),
    total_compressed_bytes => non_neg_integer(),
    avg_ratio => float()
}.

-record(state, {
    threshold = 1048576 :: compression_threshold(), % Default 1MB
    stats = #{
        total_compress_ops => 0,
        total_decompress_ops => 0,
        total_original_bytes => 0,
        total_compressed_bytes => 0
    } :: map()
}).

-type state() :: #state{}.

-export_type([compression_level/0, compression_threshold/0, compression_metadata/0, compression_stats/0]).

%%%====================================================================
%%% API Functions
%%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Compress binary data using zstd with default level (3).
%% Level 3 provides good balance between speed and compression ratio.
-spec compress(binary()) -> {ok, binary()} | {error, term()}.
compress(Data) when is_binary(Data) ->
    try
        Compressed = zstd:compress(Data),
        {ok, Compressed}
    catch
        Error:Reason:Stack ->
            logger:error("zstd:compress failed: ~p:~p~n~p", [Error, Reason, Stack]),
            {error, {Error, Reason}}
    end.

%% @doc Compress binary data using zstd with specified compression level.
%% Levels 1-3: Fast compression (real-time)
%% Levels 4-9: Good compression (default use)
%% Levels 10-15: Better compression (archival)
%% Levels 16-22: Best compression (offline)
-spec compress(binary(), compression_level()) -> {ok, binary()} | {error, term()}.
compress(Data, Level) when is_binary(Data), is_integer(Level), Level >= 1, Level =< 22 ->
    try
        Compressed = zstd:compress(Data, Level),
        {ok, Compressed}
    catch
        Error:Reason:Stack ->
            logger:error("zstd:compress/~p failed: ~p:~p~n~p", [Level, Error, Reason, Stack]),
            {error, {Error, Reason}}
    end.

%% @doc Decompress binary data using zstd.
-spec decompress(binary()) -> {ok, binary()} | {error, term()}.
decompress(Data) when is_binary(Data) ->
    try
        Decompressed = zstd:decompress(Data),
        {ok, Decompressed}
    catch
        Error:Reason:Stack ->
            logger:error("zstd:decompress failed: ~p:~p~n~p", [Error, Reason, Stack]),
            {error, {Error, Reason}}
    end.

%% @doc Compress data with metadata including size and ratio.
-spec compress_with_metadata(binary()) ->
    {ok, binary(), compression_metadata()} | {error, term()}.
compress_with_metadata(Data) when is_binary(Data) ->
    OriginalSize = byte_size(Data),
    case compress(Data) of
        {ok, Compressed} ->
            CompressedSize = byte_size(Compressed),
            Ratio = case OriginalSize > 0 of
                true -> CompressedSize / OriginalSize;
                false -> 1.0
            end,
            Metadata = #{
                encoding => <<"zstd">>,
                original_size => OriginalSize,
                compressed_size => CompressedSize,
                ratio => Ratio
            },
            {ok, Compressed, Metadata};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Compress data if it exceeds the configured threshold.
-spec compress_threshold(binary()) ->
    {ok, binary()} | {ok, binary(), compression_metadata()} | {error, term()}.
compress_threshold(Data) when is_binary(Data) ->
    gen_server:call(?MODULE, {compress_threshold, Data}).

%% @doc Set the compression threshold (in bytes).
%% Data smaller than this threshold will not be compressed.
-spec set_compression_threshold(compression_threshold()) -> ok.
set_compression_threshold(Threshold) when is_integer(Threshold), Threshold >= 0 ->
    gen_server:call(?MODULE, {set_threshold, Threshold}).

%% @doc Get compression statistics.
-spec get_stats() -> {ok, compression_stats()}.
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% @doc Reset compression statistics.
-spec reset_stats() -> ok.
reset_stats() ->
    gen_server:call(?MODULE, reset_stats).

%% @doc Benchmark zstd vs zlib for comparison.
%% Generates random data of specified size and compares compression.
-spec benchmark_comparison(pos_integer()) -> map().
benchmark_comparison(Size) when is_integer(Size), Size > 0 ->
    Data = crypto:strong_rand_bytes(Size),
    logger:info("Running compression benchmark for ~p bytes", [Size]),

    % Benchmark zstd
    {ZstdCompressTime, {ok, ZstdCompressed}} = timer:tc(fun() -> compress(Data) end),
    {ZstdDecompressTime, {ok, _ZstdDecpressed}} = timer:tc(fun() -> decompress(ZstdCompressed) end),

    % Benchmark zlib
    {ZlibCompressTime, ZlibCompressed} = timer:tc(fun() -> zlib:compress(Data) end),
    {ZlibDecompressTime, _ZlibDecompressed} = timer:tc(fun() -> zlib:uncompress(ZlibCompressed) end),

    % Calculate metrics
    ZstdRatio = byte_size(ZstdCompressed) / Size,
    ZlibRatio = byte_size(ZlibCompressed) / Size,

    #{
        data_size => Size,
        zstd => #{
            compressed_size => byte_size(ZstdCompressed),
            compression_ratio => ZstdRatio,
            compress_time_us => ZstdCompressTime,
            decompress_time_us => ZstdDecompressTime,
            throughput_mb_s =>
                (Size / 1024 / 1024) / (ZstdCompressTime / 1_000_000)
        },
        zlib => #{
            compressed_size => byte_size(ZlibCompressed),
            compression_ratio => ZlibRatio,
            compress_time_us => ZlibCompressTime,
            decompress_time_us => ZlibDecompressTime,
            throughput_mb_s =>
                (Size / 1024 / 1024) / (ZlibCompressTime / 1_000_000)
        },
        comparison => #{
            space_improvement => (ZlibRatio - ZstdRatio) / ZlibRatio * 100,
            speed_improvement => ZlibCompressTime / ZstdCompressTime
        }
    }.

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    logger:info("Initializing erlmcp_compression with OTP 28 zstd"),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({compress_threshold, Data}, _From, #state{threshold = Threshold} = State) ->
    DataSize = byte_size(Data),
    case DataSize >= Threshold of
        true ->
            case compress_with_metadata(Data) of
                {ok, Compressed, Metadata} ->
                    %% Update stats
                    NewStats = update_compress_stats(State#state.stats, DataSize, byte_size(Compressed)),
                    {reply, {ok, Compressed, Metadata}, State#state{stats = NewStats}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        false ->
            %% Data too small, return uncompressed
            {reply, {ok, Data}, State}
    end;
handle_call({set_threshold, Threshold}, _From, State) ->
    logger:info("Setting compression threshold to ~p bytes", [Threshold]),
    {reply, ok, State#state{threshold = Threshold}};
handle_call(get_stats, _From, #state{stats = Stats} = State) ->
    %% Calculate average ratio
    AvgRatio = case maps:get(total_compress_ops, Stats, 0) of
        0 -> 0.0;
        _Count -> maps:get(total_compressed_bytes, Stats, 0) / maps:get(total_original_bytes, Stats, 1)
    end,
    StatsWithAvg = Stats#{avg_ratio => AvgRatio},
    {reply, {ok, StatsWithAvg}, State};
handle_call(reset_stats, _From, State) ->
    logger:info("Resetting compression statistics"),
    NewStats = #{
        total_compress_ops => 0,
        total_decompress_ops => 0,
        total_original_bytes => 0,
        total_compressed_bytes => 0
    },
    {reply, ok, State#state{stats = NewStats}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Update compression statistics after a compress operation.
-spec update_compress_stats(map(), non_neg_integer(), non_neg_integer()) -> map().
update_compress_stats(Stats, OriginalSize, CompressedSize) ->
    Stats#{
        total_compress_ops => maps:get(total_compress_ops, Stats, 0) + 1,
        total_original_bytes => maps:get(total_original_bytes, Stats, 0) + OriginalSize,
        total_compressed_bytes => maps:get(total_compressed_bytes, Stats, 0) + CompressedSize
    }.
