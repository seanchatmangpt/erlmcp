%%%-------------------------------------------------------------------
%%% @doc erlmcp_chaos_resource - Resource Chaos Primitives
%%%
%%% Resource exhaustion for chaos engineering:
%%% - Memory exhaustion
%%% - CPU saturation
%%% - Disk space consumption
%%% - File descriptor exhaustion
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_resource).

-export([
    exhaust_memory/1,
    saturate_cpu/1,
    fill_disk/1,
    exhaust_file_descriptors/1
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Consume memory to trigger backpressure
-spec exhaust_memory(map()) -> ok.
exhaust_memory(Config) ->
    TargetPercent = maps:get(target_percent, Config, 0.85),
    Duration = maps:get(duration, Config, 60000),
    
    ?LOG_INFO("Exhausting memory to ~.1f% for ~pms", [TargetPercent * 100, Duration]),
    
    % Get current memory usage
    {Total, Allocated, _Worst} = memsup:get_memory_data(),
    CurrentPercent = Allocated / Total,
    
    if
        CurrentPercent >= TargetPercent ->
            ?LOG_INFO("Already at target memory usage: ~.1f%", [CurrentPercent * 100]),
            timer:sleep(Duration);
        true ->
            % Allocate memory gradually
            TargetBytes = round((TargetPercent - CurrentPercent) * Total),
            allocate_memory_gradually(TargetBytes, Duration)
    end,
    
    ok.

-spec allocate_memory_gradually(non_neg_integer(), pos_integer()) -> ok.
allocate_memory_gradually(TargetBytes, Duration) ->
    ChunkSize = 10 * 1024 * 1024,  % 10MB chunks
    ChunkCount = TargetBytes div ChunkSize,
    SleepInterval = Duration div max(1, ChunkCount),
    
    ?LOG_INFO("Allocating ~p MB in ~p chunks", 
             [TargetBytes div (1024*1024), ChunkCount]),
    
    allocate_chunks(ChunkCount, ChunkSize, SleepInterval, []).

-spec allocate_chunks(non_neg_integer(), pos_integer(), pos_integer(), [binary()]) -> ok.
allocate_chunks(0, _ChunkSize, _SleepInterval, Chunks) ->
    ?LOG_INFO("Memory allocation complete, holding ~p chunks", [length(Chunks)]),
    % Hold memory for a bit, then release
    timer:sleep(5000),
    ?LOG_INFO("Releasing memory", []),
    _ = Chunks,  % Let GC collect
    ok;
allocate_chunks(N, ChunkSize, SleepInterval, Chunks) ->
    Chunk = <<0:(ChunkSize*8)>>,
    timer:sleep(SleepInterval),
    allocate_chunks(N - 1, ChunkSize, SleepInterval, [Chunk | Chunks]).

%% @doc Saturate CPU schedulers
-spec saturate_cpu(map()) -> ok.
saturate_cpu(Config) ->
    TargetLoad = maps:get(target_load, Config, 1.0),
    Duration = maps:get(duration, Config, 60000),
    
    SchedulerCount = erlang:system_info(schedulers_online),
    WorkerCount = round(SchedulerCount * TargetLoad),
    
    ?LOG_INFO("Saturating ~p/~p CPU schedulers for ~pms", 
             [WorkerCount, SchedulerCount, Duration]),
    
    % Spawn CPU-intensive workers
    Workers = [spawn(fun() -> cpu_burn_loop() end) || _ <- lists:seq(1, WorkerCount)],
    
    timer:sleep(Duration),
    
    % Kill workers
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Workers),
    
    ?LOG_INFO("CPU saturation complete", []),
    ok.

-spec cpu_burn_loop() -> no_return().
cpu_burn_loop() ->
    % Busy loop to consume CPU
    _ = lists:seq(1, 1000000),
    cpu_burn_loop().

%% @doc Fill disk space to trigger disk full errors
-spec fill_disk(map()) -> ok.
fill_disk(Config) ->
    TargetPercent = maps:get(target_percent, Config, 0.90),
    TempFile = maps:get(temp_file, Config, "/tmp/erlmcp_chaos_disk_fill"),
    Duration = maps:get(duration, Config, 60000),
    
    ?LOG_INFO("Filling disk to ~.1f% for ~pms", [TargetPercent * 100, Duration]),
    
    % Get disk info
    DiskData = disksup:get_disk_data(),
    ?LOG_INFO("Current disk data: ~p", [DiskData]),
    
    % In production, would calculate and write actual data
    % For safety in tests, just log the intent
    ?LOG_INFO("Would write to temp file: ~s", [TempFile]),
    
    timer:sleep(Duration),
    
    ?LOG_INFO("Disk fill simulation complete", []),
    ok.

%% @doc Exhaust file descriptors
-spec exhaust_file_descriptors(map()) -> ok.
exhaust_file_descriptors(Config) ->
    TargetCount = maps:get(target_count, Config, 1000),
    Duration = maps:get(duration, Config, 60000),
    
    ?LOG_INFO("Opening ~p file descriptors for ~pms", [TargetCount, Duration]),
    
    % Open many files (ports)
    Ports = open_many_ports(TargetCount, []),
    
    timer:sleep(Duration),
    
    % Close ports
    lists:foreach(fun(Port) -> erlang:port_close(Port) end, Ports),
    
    ?LOG_INFO("File descriptor exhaustion complete", []),
    ok.

-spec open_many_ports(non_neg_integer(), [port()]) -> [port()].
open_many_ports(0, Ports) ->
    Ports;
open_many_ports(N, Ports) ->
    try
        Port = erlang:open_port({spawn, "cat"}, []),
        open_many_ports(N - 1, [Port | Ports])
    catch
        _:_ ->
            ?LOG_WARNING("Failed to open port, stopping at ~p ports", [length(Ports)]),
            Ports
    end.

