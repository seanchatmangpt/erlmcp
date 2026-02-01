%%%-----------------------------------------------------------------------------
%%% @doc erlmcp_memory_analyzer - Advanced Memory Analysis
%%%
%%% Provides:
%%% - Process memory ranking
%%% - ETS table size analysis
%%% - Binary leak detection
%%% - Heap fragmentation analysis
%%% - Memory trend tracking
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_memory_analyzer).

%% API
-export([
    analyze/0,
    analyze/1,
    top_processes/1,
    ets_tables/0,
    ets_tables/1,
    detect_leaks/0,
    heap_analysis/0,
    heap_analysis/1,
    memory_trends/1
]).

-include_lib("kernel/include/logger.hrl").

-type analysis_opts() :: #{
    top => pos_integer(),
    sort_by => memory | reductions | message_queue,
    include_ets => boolean(),
    include_binaries => boolean()
}.

%%%=============================================================================
%%% API
%%%=============================================================================

%% @doc Full memory analysis
-spec analyze() -> {ok, map()}.
analyze() ->
    analyze(#{}).

%% @doc Memory analysis with options
-spec analyze(analysis_opts()) -> {ok, map()}.
analyze(Opts) ->
    Top = maps:get(top, Opts, 20),
    IncludeEts = maps:get(include_ets, Opts, true),
    IncludeBinaries = maps:get(include_binaries, Opts, true),
    
    SystemMem = erlang:memory(),
    Processes = top_processes(Top),
    
    Result = #{
        timestamp => erlang:system_time(millisecond),
        system_memory => format_memory_info(SystemMem),
        top_processes => Processes,
        total_processes => length(erlang:processes())
    },
    
    Result2 = case IncludeEts of
        true -> Result#{ets_tables => ets_tables(Top)};
        false -> Result
    end,
    
    Result3 = case IncludeBinaries of
        true -> Result2#{binary_leaks => detect_leaks()};
        false -> Result2
    end,
    
    {ok, Result3}.

%% @doc Get top N processes by memory usage
-spec top_processes(pos_integer()) -> [map()].
top_processes(N) ->
    Processes = [{Pid, process_info(Pid, [memory, message_queue_len, reductions, 
                                          registered_name, current_function])}
                 || Pid <- erlang:processes()],
    
    Sorted = lists:sort(
        fun({_, InfoA}, {_, InfoB}) ->
            MemA = proplists:get_value(memory, InfoA, 0),
            MemB = proplists:get_value(memory, InfoB, 0),
            MemA >= MemB
        end,
        Processes
    ),
    
    TopN = lists:sublist(Sorted, N),
    
    [format_process_info(Pid, Info) || {Pid, Info} <- TopN].

%% @doc Analyze all ETS tables
-spec ets_tables() -> [map()].
ets_tables() ->
    ets_tables(all).

%% @doc Get top N ETS tables by size
-spec ets_tables(pos_integer() | all) -> [map()].
ets_tables(N) ->
    Tables = ets:all(),
    
    InfoList = lists:filtermap(
        fun(Tab) ->
            try
                Info = ets:info(Tab),
                case Info of
                    undefined -> false;
                    _ ->
                        {true, #{
                            table => Tab,
                            name => proplists:get_value(name, Info),
                            size => proplists:get_value(size, Info, 0),
                            memory_words => proplists:get_value(memory, Info, 0),
                            memory_bytes => proplists:get_value(memory, Info, 0) * erlang:system_info(wordsize),
                            type => proplists:get_value(type, Info),
                            owner => proplists:get_value(owner, Info)
                        }}
                end
            catch
                _:_ -> false
            end
        end,
        Tables
    ),
    
    Sorted = lists:sort(
        fun(#{memory_bytes := A}, #{memory_bytes := B}) -> A >= B end,
        InfoList
    ),
    
    case N of
        all -> Sorted;
        _ -> lists:sublist(Sorted, N)
    end.

%% @doc Detect potential memory leaks
-spec detect_leaks() -> map().
detect_leaks() ->
    %% Check for processes with excessive binary memory
    BinaryLeaks = erlmcp_profiler:binary_leaks(),
    
    %% Check for growing message queues
    LongQueues = lists:filtermap(
        fun(Pid) ->
            case process_info(Pid, [message_queue_len, registered_name]) of
                undefined -> false;
                Info ->
                    QLen = proplists:get_value(message_queue_len, Info, 0),
                    case QLen > 1000 of
                        true ->
                            RegName = proplists:get_value(registered_name, Info, undefined),
                            {true, #{
                                pid => Pid,
                                queue_length => QLen,
                                registered_name => RegName,
                                severity => queue_severity(QLen)
                            }};
                        false -> false
                    end
            end
        end,
        erlang:processes()
    ),
    
    %% Check for large ETS tables
    LargeTables = lists:filter(
        fun(#{size := Size}) -> Size > 10000 end,
        ets_tables(all)
    ),
    
    #{
        binary_leaks => BinaryLeaks,
        long_message_queues => LongQueues,
        large_ets_tables => LargeTables,
        leak_score => calculate_leak_score(BinaryLeaks, LongQueues, LargeTables)
    }.

%% @doc Analyze heap fragmentation for all processes
-spec heap_analysis() -> {ok, map()}.
heap_analysis() ->
    heap_analysis(#{threshold => 30.0}).

%% @doc Analyze heap fragmentation with threshold
-spec heap_analysis(map()) -> {ok, map()}.
heap_analysis(Opts) ->
    Threshold = maps:get(threshold, Opts, 30.0),
    
    Fragmented = lists:filtermap(
        fun(Pid) ->
            case process_info(Pid, [heap_size, total_heap_size, registered_name]) of
                undefined -> false;
                Info ->
                    HeapSize = proplists:get_value(heap_size, Info, 0),
                    TotalHeap = proplists:get_value(total_heap_size, Info, 0),
                    RegName = proplists:get_value(registered_name, Info, undefined),
                    
                    Fragmentation = case TotalHeap of
                        0 -> 0.0;
                        _ -> ((TotalHeap - HeapSize) / TotalHeap) * 100
                    end,
                    
                    case Fragmentation >= Threshold of
                        true ->
                            {true, #{
                                pid => Pid,
                                registered_name => RegName,
                                fragmentation_pct => Fragmentation,
                                heap_size_words => HeapSize,
                                total_heap_size_words => TotalHeap,
                                wasted_words => TotalHeap - HeapSize
                            }};
                        false -> false
                    end
            end
        end,
        erlang:processes()
    ),
    
    Sorted = lists:sort(
        fun(#{fragmentation_pct := A}, #{fragmentation_pct := B}) -> A >= B end,
        Fragmented
    ),
    
    {ok, #{
        threshold_pct => Threshold,
        fragmented_processes => Sorted,
        count => length(Sorted)
    }}.

%% @doc Track memory trends over time
-spec memory_trends(pos_integer()) -> {ok, pid()}.
memory_trends(IntervalMs) ->
    Pid = proc_lib:spawn_link(fun() -> trend_tracker(IntervalMs, []) end),
    {ok, Pid}.

%%%=============================================================================
%%% INTERNAL FUNCTIONS
%%%=============================================================================

-spec format_memory_info(list()) -> map().
format_memory_info(MemInfo) ->
    #{
        total => proplists:get_value(total, MemInfo, 0),
        processes => proplists:get_value(processes, MemInfo, 0),
        processes_used => proplists:get_value(processes_used, MemInfo, 0),
        system => proplists:get_value(system, MemInfo, 0),
        atom => proplists:get_value(atom, MemInfo, 0),
        atom_used => proplists:get_value(atom_used, MemInfo, 0),
        binary => proplists:get_value(binary, MemInfo, 0),
        code => proplists:get_value(code, MemInfo, 0),
        ets => proplists:get_value(ets, MemInfo, 0)
    }.

-spec format_process_info(pid(), list()) -> map().
format_process_info(Pid, Info) ->
    Memory = proplists:get_value(memory, Info, 0),
    MsgQueue = proplists:get_value(message_queue_len, Info, 0),
    Reductions = proplists:get_value(reductions, Info, 0),
    RegName = proplists:get_value(registered_name, Info, undefined),
    CurrentFun = proplists:get_value(current_function, Info, undefined),
    
    #{
        pid => Pid,
        registered_name => RegName,
        memory_bytes => Memory,
        memory_mb => Memory / 1024 / 1024,
        message_queue_len => MsgQueue,
        reductions => Reductions,
        current_function => CurrentFun
    }.

-spec queue_severity(non_neg_integer()) -> atom().
queue_severity(QLen) when QLen > 100000 -> critical;
queue_severity(QLen) when QLen > 10000 -> high;
queue_severity(QLen) when QLen > 1000 -> medium;
queue_severity(_) -> low.

-spec calculate_leak_score(term(), list(), list()) -> float().
calculate_leak_score({ok, BinaryLeaks}, LongQueues, LargeTables) ->
    BinaryScore = length(BinaryLeaks) * 10,
    QueueScore = length(LongQueues) * 5,
    TableScore = length(LargeTables) * 2,
    
    min(100.0, float(BinaryScore + QueueScore + TableScore));
calculate_leak_score(_, LongQueues, LargeTables) ->
    QueueScore = length(LongQueues) * 5,
    TableScore = length(LargeTables) * 2,
    min(100.0, float(QueueScore + TableScore)).

-spec trend_tracker(pos_integer(), [map()]) -> ok.
trend_tracker(IntervalMs, History) ->
    Snapshot = #{
        timestamp => erlang:system_time(millisecond),
        memory => erlang:memory(),
        process_count => length(erlang:processes())
    },
    
    %% Keep last 100 snapshots
    NewHistory = lists:sublist([Snapshot | History], 100),
    
    timer:sleep(IntervalMs),
    trend_tracker(IntervalMs, NewHistory).
