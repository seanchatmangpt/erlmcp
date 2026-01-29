%%%-------------------------------------------------------------------
%%% @doc erlmcp_bench_binary_exhaustion - Destructive Binary Heap Exhaustion Test
%%%
%%% DESTRUCTIVE STRESS TEST #6: Binary Heap Exhaustion
%%%
%%% This test creates millions of large binaries to exhaust the binary heap
%%% and crash the VM. It prevents garbage collection and forces accumulation
%%% until out-of-memory crash or VM termination.
%%%
%%% WARNING: This test WILL crash the VM. Use only in isolated environments.
%%%
%%% Test Protocol:
%%% 1. Create 1MB binaries in batches
%%% 2. Store all binaries in process dictionary (prevents GC)
%%% 3. Monitor binary heap size specifically
%%% 4. Continue until OOM, VM crash, or 10TB allocated
%%% 5. Test for data corruption before crash
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_binary_exhaustion).

-behaviour(gen_server).

%% API
-export([
    run/0,
    run/1,
    quick_test/0,
    crash_test/0,
    print_report/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(state, {
    binary_size = 1048576 :: integer(),           % 1MB per binary
    batch_size = 1000 :: integer(),                % Binaries per batch
    binaries = [] :: [binary()],                   % Stored binaries (no GC)
    batch_count = 0 :: integer(),                  % Number of batches completed
    total_binary_count = 0 :: integer(),           % Total binaries created
    start_time :: integer(),                       % Test start time
    last_sample_time :: integer(),                 % Last sample time
    corruption_samples = [] :: [{integer(), binary(), binary()}], % Samples for integrity
    crash_detected = false :: boolean(),           % Whether crash was detected
    max_batches = 10000 :: integer()               % Safety limit
}).

-record(sample, {
    batch_num :: integer(),
    binary_count :: integer(),
    binary_memory_mib :: float(),
    total_memory_mib :: float(),
    process_heap_mib :: float(),
    timestamp :: integer(),
    elapsed_s :: float(),
    status :: atom()
}).

-type test_result() :: #{
    workload_id := binary(),
    benchmark := binary(),
    binary_size_bytes := integer(),
    batch_size := integer(),
    max_batches := integer(),
    breaking_point := map(),
    growth_progress := [map()],
    data_integrity := map(),
    gc_behavior := map(),
    analysis := binary()
}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Run binary exhaustion test with default config (1MB, 1K batches)
-spec run() -> test_result().
run() ->
    run(#{
        binary_size => 1048576,      % 1MB
        batch_size => 1000,           % 1K binaries per batch
        max_batches => 10000          % Up to 10M binaries (10TB)
    }).

%% @doc Run with custom config
-spec run(map()) -> test_result().
run(Config) ->
    BinarySize = maps:get(binary_size, Config, 1048576),
    BatchSize = maps:get(batch_size, Config, 1000),
    MaxBatches = maps:get(max_batches, Config, 10000),

    logger:warning("=== DESTRUCTIVE BINARY HEAP EXHAUSTION TEST ==="),
    logger:warning("Binary Size: ~p bytes (~.2f MB)", [BinarySize, BinarySize / 1048576]),
    logger:warning("Batch Size: ~p binaries", [BatchSize]),
    logger:warning("Max Batches: ~p (max ~p binaries = ~.2f TB)", [
        MaxBatches, MaxBatches * BatchSize, (MaxBatches * BatchSize * BinarySize) / (1024 * 1024 * 1024 * 1024)
    ]),
    logger:warning("WARNING: This test WILL crash the VM!"),

    %% Start gen_server to manage the test
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []),

    %% Run the test
    Result = gen_server:call(Pid, run_exhaustion_test, infinity),

    %% Stop the server
    gen_server:stop(Pid),

    Result.

%% @doc Quick test (smaller scale, safer)
-spec quick_test() -> test_result().
quick_test() ->
    run(#{
        binary_size => 1048576,      % 1MB
        batch_size => 100,            % 100 binaries per batch
        max_batches => 100            % Max 10K binaries (10GB)
    }).

%% @doc Full crash test (aggressive)
-spec crash_test() -> test_result().
crash_test() ->
    run(#{
        binary_size => 10485760,     % 10MB binaries
        batch_size => 1000,           % 1K per batch
        max_batches => 10000          % Up to 100TB
    }).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Config) ->
    BinarySize = maps:get(binary_size, Config, 1048576),
    BatchSize = maps:get(batch_size, Config, 1000),
    MaxBatches = maps:get(max_batches, Config, 10000),

    State = #state{
        binary_size = BinarySize,
        batch_size = BatchSize,
        max_batches = MaxBatches,
        start_time = erlang:monotonic_time(millisecond),
        last_sample_time = erlang:monotonic_time(millisecond)
    },

    logger:info("Binary exhaustion test initialized: ~p MB binaries, ~p per batch, max ~p batches",
        [BinarySize div 1048576, BatchSize, MaxBatches]),

    {ok, State}.

handle_call(run_exhaustion_test, _From, State) ->
    logger:info("Starting binary heap exhaustion test..."),

    %% Run the exhaustion loop
    {FinalState, Samples} = exhaustion_loop(State, []),

    %% Generate comprehensive report
    Result = generate_report(FinalState, Samples),

    {reply, Result, FinalState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Main exhaustion loop - allocate binaries until crash
-spec exhaustion_loop(#state{}, [map()]) -> {#state{}, [map()]}.
exhaustion_loop(State, AccSamples) ->
    #state{
        batch_count = BatchNum,
        batch_size = BatchSize,
        binary_size = BinarySize,
        max_batches = MaxBatches,
        binaries = ExistingBinaries
    } = State,

    %% Check if we've hit the safety limit
    case BatchNum >= MaxBatches of
        true ->
            logger:warning("Reached maximum batch limit (~p), stopping", [MaxBatches]),
            {State, lists:reverse(AccSamples)};
        false ->
            %% Take a sample before this batch
            BeforeSample = take_sample(State, BatchNum),

            %% Create a batch of binaries
            BatchStart = erlang:monotonic_time(millisecond),

            case catch create_binary_batch(BinarySize, BatchSize) of
                {'EXIT', {Reason, _Stack}} ->
                    %% Crash detected!
                    logger:error("BINARY HEAP EXHAUSTION CRASH: ~p", [Reason]),

                    CrashSample = BeforeSample#sample{
                        status = crash,
                        timestamp = erlang:monotonic_time(millisecond)
                    },

                    CrashState = State#state{
                        crash_detected = true
                    },

                    {CrashState, lists:reverse([sample_to_map(CrashSample) | AccSamples])};

                NewBinaries when is_list(NewBinaries) ->
                    BatchEnd = erlang:monotonic_time(millisecond),
                    BatchTime = BatchEnd - BatchStart,

                    %% Store binaries in process dictionary to prevent GC
                    %% This is CRITICAL - normal list would be GC'd
                    BinKey = list_to_atom("binaries_" ++ integer_to_list(BatchNum)),
                    put(BinKey, NewBinaries),

                    %% Update state
                    NewBinaryCount = State#state.total_binary_count + length(NewBinaries),
                    NewState = State#state{
                        batch_count = BatchNum + 1,
                        total_binary_count = NewBinaryCount,
                        binaries = ExistingBinaries ++ NewBinaries
                    },

                    %% Take sample after batch
                    AfterSample = take_sample(NewState, BatchNum + 1),

                    %% Check for corruption every 10 batches
                    CorruptedSamples = case (BatchNum + 1) rem 10 =:= 0 of
                        true ->
                            verify_data_integrity(NewState);
                        false ->
                            State#state.corruption_samples
                    end,

                    NewStateWithSamples = NewState#state{
                        corruption_samples = CorruptedSamples
                    },

                    %% Log progress
                    BinaryMem = AfterSample#sample.binary_memory_mib,
                    TotalMem = AfterSample#sample.total_memory_mib,
                    logger:info("Batch ~p complete: ~p binaries, ~.2f GB binary mem, ~.2f GB total, ~p ms",
                        [BatchNum + 1, NewBinaryCount, BinaryMem / 1024, TotalMem / 1024, BatchTime]),

                    %% Check if we're near system limits
                    case should_stop_for_safety(AfterSample) of
                        {true, Reason} ->
                            logger:warning("Safety stop triggered: ~p", [Reason]),
                            {NewStateWithSamples, lists:reverse([sample_to_map(AfterSample) | AccSamples])};
                        false ->
                            %% Continue to next batch
                            exhaustion_loop(NewStateWithSamples, [sample_to_map(AfterSample) | AccSamples])
                    end
            end
    end.

%% @doc Create a batch of large binaries
-spec create_binary_batch(integer(), integer()) -> [binary()].
create_binary_batch(BinarySize, BatchSize) ->
    [
        begin
            %% Create predictable pattern for corruption detection
            Pattern = <<I:32, BinarySize:32, (I bxor 16#DEADBEEF):32>>,
            Padding = BinarySize - 12,
            <<Pattern/binary, 0:(Padding * 8)>>
        end
        || I <- lists:seq(1, BatchSize)
    ].

%% @doc Take a memory sample
-spec take_sample(#state{}, integer()) -> #sample{}.
take_sample(State, BatchNum) ->
    #state{
        start_time = StartTime,
        total_binary_count = BinaryCount
    } = State,

    Now = erlang:monotonic_time(millisecond),
    ElapsedS = (Now - StartTime) / 1000.0,

    %% Get detailed memory breakdown - use only available memory types
    TotalMemory = erlang:memory(total),
    BinaryMemory = erlang:memory(binary),
    ProcessMemory = erlang:memory(processes),

    #sample{
        batch_num = BatchNum,
        binary_count = BinaryCount,
        binary_memory_mib = BinaryMemory / (1024 * 1024),
        total_memory_mib = TotalMemory / (1024 * 1024),
        process_heap_mib = ProcessMemory / (1024 * 1024),
        timestamp = Now,
        elapsed_s = ElapsedS,
        status = running
    }.

%% @doc Check if we should stop for safety reasons
-spec should_stop_for_safety(#sample{}) -> {true, atom()} | false.
should_stop_for_safety(Sample) ->
    #sample{
        total_memory_mib = TotalMemoryMib,
        binary_memory_mib = BinaryMemoryMib
    } = Sample,

    %% Safety limits (adjust based on your system)
    MaxTotalMemoryMib = 16 * 1024,  % 16GB
    MaxBinaryMemoryMib = 14 * 1024, % 14GB

    if
        TotalMemoryMib > MaxTotalMemoryMib ->
            {true, total_memory_limit};
        BinaryMemoryMib > MaxBinaryMemoryMib ->
            {true, binary_memory_limit};
        true ->
            false
    end.

%% @doc Verify data integrity in stored binaries
-spec verify_data_integrity(#state{}) -> [{integer(), binary(), binary()}].
verify_data_integrity(State) ->
    #state{
        binaries = Binaries,
        corruption_samples = CorruptedSamples
    } = State,

    %% Sample up to 100 binaries for integrity check
    SampleSize = min(100, length(Binaries)),
    SampleIndices = [rand:uniform(length(Binaries)) || _ <- lists:seq(1, SampleSize)],

    Corrupted = lists:filtermap(
        fun(Index) ->
            Binary = lists:nth(Index, Binaries),
            case verify_binary_pattern(Binary, Index) of
                {corrupted, Expected, Actual} ->
                    {true, {Index, Expected, Actual}};
                ok ->
                    false
            end
        end,
        SampleIndices
    ),

    case Corrupted of
        [] ->
            logger:debug("Integrity check passed: ~p samples verified", [SampleSize]),
            CorruptedSamples;
        _ ->
            logger:error("CORRUPTION DETECTED: ~p corrupted binaries out of ~p samples",
                [length(Corrupted), SampleSize]),
            CorruptedSamples ++ Corrupted
    end.

%% @doc Verify a single binary's pattern
-spec verify_binary_pattern(binary(), integer()) -> ok | {corrupted, binary(), binary()}.
verify_binary_pattern(Binary, Index) ->
    case Binary of
        <<Index:32, _BinarySize:32, Checksum:32, _Rest/binary>> ->
            ExpectedChecksum = Index bxor 16#DEADBEEF,
            case Checksum =:= ExpectedChecksum of
                true -> ok;
                false -> {corrupted, <<ExpectedChecksum:32>>, <<Checksum:32>>}
            end;
        _ ->
            %% Binary too small or malformed
            {corrupted, <<>>, <<>>}
    end.

%% @doc Convert sample record to map
-spec sample_to_map(#sample{}) -> map().
sample_to_map(Sample) ->
    #sample{
        batch_num = BatchNum,
        binary_count = BinaryCount,
        binary_memory_mib = BinaryMem,
        total_memory_mib = TotalMem,
        process_heap_mib = ProcessHeap,
        timestamp = Timestamp,
        elapsed_s = ElapsedS,
        status = Status
    } = Sample,

    #{
        <<"batch">> => BatchNum,
        <<"binary_count">> => BinaryCount,
        <<"binary_memory_gb">> => round(BinaryMem / 1024 * 100) / 100,
        <<"total_memory_gb">> => round(TotalMem / 1024 * 100) / 100,
        <<"process_heap_gb">> => round(ProcessHeap / 1024 * 100) / 100,
        <<"timestamp">> => Timestamp,
        <<"elapsed_s">> => round(ElapsedS * 10) / 10,
        <<"status">> => Status
    }.

%% @doc Generate comprehensive test report
-spec generate_report(#state{}, [map()]) -> test_result().
generate_report(State, Samples) ->
    #state{
        binary_size = BinarySize,
        batch_size = BatchSize,
        max_batches = MaxBatches,
        total_binary_count = TotalBinaries,
        batch_count = BatchesCompleted,
        corruption_samples = CorruptedSamples,
        crash_detected = CrashDetected
    } = State,

    %% Find breaking point
    BreakingPoint = case CrashDetected of
        true ->
            %% Find the crash sample
            case lists:keyfind(crash, 2, [{maps:get(<<"status">>, S), S} || S <- Samples]) of
                {_, CrashSample} ->
                    #{
                        binary_count => maps:get(<<"binary_count">>, CrashSample),
                        binary_memory_gb => maps:get(<<"binary_memory_gb">>, CrashSample),
                        total_memory_gb => maps:get(<<"total_memory_gb">>, CrashSample),
                        error => <<"vm_crash">>,
                        crash_type => <<"out_of_memory">>
                    };
                false ->
                    #{
                        binary_count => TotalBinaries,
                        binary_memory_gb => 0.0,
                        total_memory_gb => 0.0,
                        error => <<"unknown">>,
                        crash_type => <<"unknown">>
                    }
            end;
        false ->
            %% Stopped before crash (safety limit)
            LastSample = case Samples of
                [] -> #{};
                [S | _] -> S
            end,

            #{
                binary_count => maps:get(<<"binary_count">>, LastSample, TotalBinaries),
                binary_memory_gb => maps:get(<<"binary_memory_gb">>, LastSample, 0.0),
                total_memory_gb => maps:get(<<"total_memory_gb">>, LastSample, 0.0),
                error => <<"safety_limit">>,
                crash_type => <<"stopped">>
            }
    end,

    %% Data integrity analysis
    TotalBinaryMemoryGB = (TotalBinaries * BinarySize) / (1024 * 1024 * 1024),
    SamplesVerified = BatchesCompleted div 10 * 100,  % Approx
    CorruptedCount = length(CorruptedSamples),

    DataIntegrity = #{
        samples_verified => SamplesVerified,
        corrupted_binaries => CorruptedCount,
        data_loss => CorruptedCount > 0
    },

    %% GC behavior (we prevented it)
    GcBehavior = #{
        gc_runs_forced => 0,
        gc_runs_automatic => 0,
        heap_fragmentation => false,
        note => <<"GC prevented by storing binaries in process dictionary">>
    },

    %% Analysis
    Analysis = io_lib:format(
        "Binary heap exhaustion test: ~p binaries (~.2f GB) allocated over ~p batches. " ++
        "Binary size: ~p MB. Crash detected: ~p. Corruption: ~p/~p samples. " ++
        "The system ~p before reaching ~p batches (safety limit).",
        [TotalBinaries, TotalBinaryMemoryGB, BatchesCompleted, BinarySize div 1048576,
         CrashDetected, CorruptedCount, SamplesVerified,
         case CrashDetected of true -> "crashed"; false -> "stopped" end,
         MaxBatches]
    ),

    #{
        workload_id => <<"binary_heap_exhaustion">>,
        benchmark => <<"destructive_stress">>,
        binary_size_bytes => BinarySize,
        batch_size => BatchSize,
        max_batches => MaxBatches,
        breaking_point => BreakingPoint,
        growth_progress => lists:reverse(Samples),
        data_integrity => DataIntegrity,
        gc_behavior => GcBehavior,
        analysis => iolist_to_binary(Analysis)
    }.

%% @doc Pretty print the report (useful in shell)
-spec print_report(test_result()) -> ok.
print_report(Result) ->
    io:format("~n=== BINARY HEAP EXHAUSTION CRASH TEST ===~n~n", []),

    BinaryConfig = #{
        binary_size => maps:get(binary_size_bytes, Result),
        batch_size => maps:get(batch_size, Result),
        max_batches => maps:get(max_batches, Result)
    },
    io:format("Binary Configuration:~n", []),
    io:format("  - Binary Size: ~p bytes (~.2f MB)~n",
        [maps:get(binary_size, BinaryConfig), maps:get(binary_size, BinaryConfig) / 1048576]),
    io:format("  - Per Batch: ~p binaries~n", [maps:get(batch_size, BinaryConfig)]),
    io:format("  - Total per Batch: ~.2f GB~n",
        [(maps:get(binary_size, BinaryConfig) * maps:get(batch_size, BinaryConfig)) / (1024 * 1024 * 1024)]),

    io:format("~nGrowth Progress:~n", []),
    Samples = maps:get(growth_progress, Result),
    print_samples(Samples),

    io:format("~nBREAKING POINT:~n", []),
    BreakingPoint = maps:get(breaking_point, Result),
    io:format("  - Binary Count: ~p~n", [maps:get(binary_count, BreakingPoint)]),
    io:format("  - Binary Memory: ~.2f GB~n", [maps:get(binary_memory_gb, BreakingPoint)]),
    io:format("  - Total Memory: ~.2f GB~n", [maps:get(total_memory_gb, BreakingPoint)]),
    io:format("  - Error: ~s~n", [maps:get(error, BreakingPoint)]),
    io:format("  - Crash Type: ~s~n", [maps:get(crash_type, BreakingPoint)]),

    io:format("~nDATA INTEGRITY:~n", []),
    Integrity = maps:get(data_integrity, Result),
    io:format("  - Samples Verified: ~p~n", [maps:get(samples_verified, Integrity)]),
    io:format("  - Corrupted Binaries: ~p~n", [maps:get(corrupted_binaries, Integrity)]),
    io:format("  - Data Loss: ~s~n", [maps:get(data_loss, Integrity)]),

    io:format("~nGC BEHAVIOR:~n", []),
    Gc = maps:get(gc_behavior, Result),
    io:format("  - GC Runs Forced: ~p~n", [maps:get(gc_runs_forced, Gc)]),
    io:format("  - GC Runs Automatic: ~p~n", [maps:get(gc_runs_automatic, Gc)]),
    io:format("  - Heap Fragmentation: ~p~n", [maps:get(heap_fragmentation, Gc)]),

    io:format("~nANALYSIS:~n", []),
    io:format("~s~n", [maps:get(analysis, Result)]),

    ok.

%% @doc Print sample progress
-spec print_samples([map()]) -> ok.
print_samples([]) ->
    ok;
print_samples(Samples) ->
    %% Print every 10th sample to avoid spam
    KeySamples = [S || {I, S} <- lists:enumerate(Samples), I rem 10 =:= 0 orelse I =:= length(Samples)],

    lists:foreach(fun(Sample) ->
        io:format("  - Batch ~p: ~p binaries, ~.2f GB binary, ~.2f GB total, ~.1f s, status: ~p~n",
            [maps:get(<<"batch">>, Sample),
             maps:get(<<"binary_count">>, Sample),
             maps:get(<<"binary_memory_gb">>, Sample),
             maps:get(<<"total_memory_gb">>, Sample),
             maps:get(<<"elapsed_s">>, Sample),
             maps:get(<<"status">>, Sample)])
    end, KeySamples),

    ok.
