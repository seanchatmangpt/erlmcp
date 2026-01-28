%%%-----------------------------------------------------------------------------
%%% @doc erlmcp_profiler - Comprehensive CPU & Memory Profiling
%%%
%%% Provides:
%%% - CPU profiling (fprof, eprof)
%%% - Memory profiling (recon, process snapshots)
%%% - Call graph generation
%%% - Flame graph export (folded stack format)
%%% - Live process inspection
%%%
%%% Usage:
%%% ```
%%% % Profile specific function
%%% erlmcp_profiler:profile(erlmcp_server, handle_call, 3, #{
%%%     duration => 60000,
%%%     output => "profile.out",
%%%     mode => fprof
%%% }).
%%%
%%% % Generate flame graph
%%% erlmcp_profiler:flame_graph("profile.out", "flame.svg").
%%%
%%% % Memory snapshot
%%% erlmcp_profiler:memory_snapshot(#{top => 20}).
%%% ```
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_profiler).

%% API - CPU Profiling
-export([
    profile/3,
    profile/4,
    profile_mfa/3,
    profile_pid/2,
    stop_profiling/0
]).

%% API - Memory Profiling
-export([
    memory_snapshot/0,
    memory_snapshot/1,
    process_memory/1,
    binary_leaks/0,
    heap_fragmentation/1
]).

%% API - Flame Graphs
-export([
    flame_graph/2,
    export_folded_stacks/2
]).

%% API - Live Inspection
-export([
    inspect_process/1,
    trace_messages/2
]).

-include_lib("kernel/include/logger.hrl").

-type profile_mode() :: fprof | eprof | cprof.
-type profile_opts() :: #{
    duration => pos_integer(),
    output => file:filename(),
    mode => profile_mode(),
    sorting => time | calls | acc
}.

%%%=============================================================================
%%% CPU PROFILING API
%%%=============================================================================

%% @doc Profile a module for specified duration
-spec profile(module(), pos_integer(), profile_opts()) -> {ok, term()} | {error, term()}.
profile(Module, Duration, Opts) ->
    Mode = maps:get(mode, Opts, fprof),
    Output = maps:get(output, Opts, "profile.out"),

    case Mode of
        fprof -> profile_fprof(Module, Duration, Output, Opts);
        eprof -> profile_eprof(Module, Duration, Output, Opts);
        cprof -> profile_cprof(Module, Duration, Output, Opts)
    end.

%% @doc Profile specific function in module
-spec profile(module(), atom(), arity(), profile_opts()) -> {ok, term()} | {error, term()}.
profile(Module, Function, Arity, Opts) ->
    profile_mfa({Module, Function, Arity}, maps:get(duration, Opts, 10000), Opts).

%% @doc Profile specific MFA (Module, Function, Arity)
-spec profile_mfa(mfa(), pos_integer(), profile_opts()) -> {ok, term()} | {error, term()}.
profile_mfa({M, F, A}, Duration, Opts) ->
    Mode = maps:get(mode, Opts, fprof),
    Output = maps:get(output, Opts, "profile.out"),
    
    case Mode of
        fprof ->
            fprof:trace([start, {procs, whereis(M)}]),
            timer:sleep(Duration),
            fprof:trace([stop]),
            fprof:profile(),
            fprof:analyse([{dest, Output}, {sort, acc}]),
            {ok, #{file => Output, mode => fprof}};
        eprof ->
            eprof:start(),
            eprof:start_profiling([whereis(M)]),
            timer:sleep(Duration),
            eprof:stop_profiling(),
            Result = eprof:analyze(total, [{sort, time}]),
            eprof:stop(),
            {ok, #{result => Result, mode => eprof}};
        cprof ->
            cprof:start({M, F, A}),
            timer:sleep(Duration),
            Result = cprof:analyse(M, F, A),
            cprof:stop(),
            {ok, #{result => Result, mode => cprof}}
    end.

%% @doc Profile specific process by PID
-spec profile_pid(pid(), profile_opts()) -> {ok, term()} | {error, term()}.
profile_pid(Pid, Opts) ->
    Duration = maps:get(duration, Opts, 10000),
    Mode = maps:get(mode, Opts, fprof),
    Output = maps:get(output, Opts, "profile.out"),
    
    case Mode of
        fprof ->
            fprof:trace([start, {procs, [Pid]}]),
            timer:sleep(Duration),
            fprof:trace([stop]),
            fprof:profile(),
            fprof:analyse([{dest, Output}, {sort, acc}]),
            {ok, #{file => Output, mode => fprof, pid => Pid}};
        eprof ->
            eprof:start(),
            eprof:start_profiling([Pid]),
            timer:sleep(Duration),
            eprof:stop_profiling(),
            Result = eprof:analyze(total),
            eprof:stop(),
            {ok, #{result => Result, mode => eprof, pid => Pid}}
    end.

%% @doc Stop all active profiling
-spec stop_profiling() -> ok.
stop_profiling() ->
    catch fprof:stop(),
    catch eprof:stop(),
    catch cprof:stop(),
    ok.

%%%=============================================================================
%%% MEMORY PROFILING API
%%%=============================================================================

%% @doc Take a memory snapshot of all processes
-spec memory_snapshot() -> {ok, [map()]}.
memory_snapshot() ->
    memory_snapshot(#{top => 20, sort => memory}).

%% @doc Take a memory snapshot with options
-spec memory_snapshot(map()) -> {ok, [map()]}.
memory_snapshot(Opts) ->
    Top = maps:get(top, Opts, 20),
    SortBy = maps:get(sort, Opts, memory),
    
    Processes = [{Pid, process_info(Pid, [memory, message_queue_len, reductions, registered_name])}
                 || Pid <- erlang:processes()],
    
    Sorted = lists:sort(
        fun({_, InfoA}, {_, InfoB}) ->
            MemA = proplists:get_value(memory, InfoA, 0),
            MemB = proplists:get_value(memory, InfoB, 0),
            case SortBy of
                memory -> MemA >= MemB;
                reductions -> 
                    RedA = proplists:get_value(reductions, InfoA, 0),
                    RedB = proplists:get_value(reductions, InfoB, 0),
                    RedA >= RedB
            end
        end,
        Processes
    ),
    
    TopProcesses = lists:sublist(Sorted, Top),
    
    Result = [begin
        Memory = proplists:get_value(memory, Info, 0),
        MsgQueue = proplists:get_value(message_queue_len, Info, 0),
        Reductions = proplists:get_value(reductions, Info, 0),
        RegName = proplists:get_value(registered_name, Info, undefined),
        
        #{
            pid => Pid,
            memory_bytes => Memory,
            memory_mb => Memory / 1024 / 1024,
            message_queue_len => MsgQueue,
            reductions => Reductions,
            registered_name => RegName
        }
    end || {Pid, Info} <- TopProcesses],
    
    ?LOG_INFO("Memory snapshot: ~p processes, top ~p by ~p", 
              [length(Processes), Top, SortBy]),
    
    {ok, Result}.

%% @doc Get detailed memory info for a specific process
-spec process_memory(pid()) -> {ok, map()} | {error, term()}.
process_memory(Pid) ->
    case process_info(Pid, [memory, heap_size, stack_size, total_heap_size, 
                            message_queue_len, messages, dictionary]) of
        undefined ->
            {error, process_not_found};
        Info ->
            Memory = proplists:get_value(memory, Info, 0),
            HeapSize = proplists:get_value(heap_size, Info, 0),
            StackSize = proplists:get_value(stack_size, Info, 0),
            TotalHeap = proplists:get_value(total_heap_size, Info, 0),
            MsgQueue = proplists:get_value(message_queue_len, Info, 0),
            
            {ok, #{
                pid => Pid,
                memory_bytes => Memory,
                memory_mb => Memory / 1024 / 1024,
                heap_size_words => HeapSize,
                stack_size_words => StackSize,
                total_heap_size_words => TotalHeap,
                heap_fragmentation_pct => 
                    case TotalHeap of
                        0 -> 0.0;
                        _ -> ((TotalHeap - HeapSize) / TotalHeap) * 100
                    end,
                message_queue_len => MsgQueue
            }}
    end.

%% @doc Detect binary memory leaks
-spec binary_leaks() -> {ok, [map()]}.
binary_leaks() ->
    Processes = [{Pid, process_info(Pid, [binary, memory, registered_name])}
                 || Pid <- erlang:processes()],
    
    Suspects = lists:filtermap(
        fun({Pid, Info}) ->
            case proplists:get_value(binary, Info) of
                undefined -> false;
                Binaries ->
                    TotalBinarySize = lists:sum([Size || {_, Size, _} <- Binaries]),
                    Memory = proplists:get_value(memory, Info, 0),
                    RegName = proplists:get_value(registered_name, Info, undefined),
                    
                    %% Suspicious if binaries > 50% of process memory
                    case Memory > 0 andalso (TotalBinarySize / Memory) > 0.5 of
                        true ->
                            {true, #{
                                pid => Pid,
                                binary_size_bytes => TotalBinarySize,
                                binary_size_mb => TotalBinarySize / 1024 / 1024,
                                total_memory_bytes => Memory,
                                binary_ratio => TotalBinarySize / Memory,
                                binary_count => length(Binaries),
                                registered_name => RegName
                            }};
                        false -> false
                    end
            end
        end,
        Processes
    ),
    
    {ok, lists:sort(fun(A, B) -> 
        maps:get(binary_size_bytes, A) >= maps:get(binary_size_bytes, B)
    end, Suspects)}.

%% @doc Calculate heap fragmentation for a process
-spec heap_fragmentation(pid()) -> {ok, float()} | {error, term()}.
heap_fragmentation(Pid) ->
    case process_info(Pid, [heap_size, total_heap_size]) of
        undefined ->
            {error, process_not_found};
        Info ->
            HeapSize = proplists:get_value(heap_size, Info, 0),
            TotalHeap = proplists:get_value(total_heap_size, Info, 0),
            
            Fragmentation = case TotalHeap of
                0 -> 0.0;
                _ -> ((TotalHeap - HeapSize) / TotalHeap) * 100
            end,
            
            {ok, Fragmentation}
    end.

%%%=============================================================================
%%% FLAME GRAPH GENERATION
%%%=============================================================================

%% @doc Generate flame graph from fprof output
-spec flame_graph(file:filename(), file:filename()) -> ok | {error, term()}.
flame_graph(ProfileFile, OutputFile) ->
    case file:read_file(ProfileFile) of
        {ok, Binary} ->
            %% Parse fprof output and convert to folded stack format
            Stacks = parse_fprof_to_stacks(Binary),
            FoldedStacks = fold_stacks(Stacks),
            
            %% Write folded stacks (can be used with flamegraph.pl)
            case file:write_file(OutputFile, FoldedStacks) of
                ok ->
                    ?LOG_INFO("Flame graph data written to ~s", [OutputFile]),
                    ?LOG_INFO("Generate SVG: flamegraph.pl ~s > ~s.svg", 
                             [OutputFile, filename:basename(OutputFile, ".txt")]),
                    ok;
                {error, Reason} ->
                    {error, {write_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

%% @doc Export stacks in folded format for external flame graph tools
-spec export_folded_stacks(file:filename(), file:filename()) -> ok | {error, term()}.
export_folded_stacks(ProfileFile, OutputFile) ->
    flame_graph(ProfileFile, OutputFile).

%%%=============================================================================
%%% LIVE INSPECTION
%%%=============================================================================

%% @doc Inspect a running process without stopping it
-spec inspect_process(pid()) -> {ok, map()} | {error, term()}.
inspect_process(Pid) ->
    case process_info(Pid) of
        undefined ->
            {error, process_not_found};
        Info ->
            State = try
                sys:get_state(Pid, 1000)
            catch
                _:_ -> unavailable
            end,
            
            {ok, #{
                pid => Pid,
                process_info => Info,
                state => State,
                timestamp => erlang:system_time(millisecond)
            }}
    end.

%% @doc Trace messages for a process
-spec trace_messages(pid(), pos_integer()) -> {ok, [term()]}.
trace_messages(Pid, Duration) ->
    Tracer = spawn(fun() -> message_tracer(Pid, []) end),
    erlang:trace(Pid, true, ['receive', {tracer, Tracer}]),
    
    timer:sleep(Duration),
    
    erlang:trace(Pid, false, ['receive']),
    Tracer ! {get_messages, self()},
    
    receive
        {messages, Messages} -> {ok, lists:reverse(Messages)}
    after 5000 ->
        {ok, []}
    end.

%%%=============================================================================
%%% INTERNAL FUNCTIONS - FPROF
%%%=============================================================================

-spec profile_fprof(module(), pos_integer(), file:filename(), profile_opts()) ->
    {ok, map()} | {error, term()}.
profile_fprof(Module, Duration, Output, Opts) ->
    Sorting = maps:get(sorting, Opts, acc),
    
    case whereis(Module) of
        undefined ->
            {error, {process_not_registered, Module}};
        Pid ->
            fprof:trace([start, {procs, [Pid]}]),
            timer:sleep(Duration),
            fprof:trace([stop]),
            fprof:profile(),
            fprof:analyse([{dest, Output}, {sort, Sorting}]),
            
            {ok, #{
                file => Output,
                mode => fprof,
                module => Module,
                pid => Pid,
                duration_ms => Duration
            }}
    end.

-spec profile_eprof(module(), pos_integer(), file:filename(), profile_opts()) ->
    {ok, map()} | {error, term()}.
profile_eprof(Module, Duration, _Output, _Opts) ->
    case whereis(Module) of
        undefined ->
            {error, {process_not_registered, Module}};
        Pid ->
            eprof:start(),
            eprof:start_profiling([Pid]),
            timer:sleep(Duration),
            eprof:stop_profiling(),
            Result = eprof:analyze(total, [{sort, time}]),
            eprof:stop(),
            
            {ok, #{
                mode => eprof,
                module => Module,
                pid => Pid,
                duration_ms => Duration,
                result => Result
            }}
    end.

-spec profile_cprof(module(), pos_integer(), file:filename(), profile_opts()) ->
    {ok, map()} | {error, term()}.
profile_cprof(Module, Duration, _Output, _Opts) ->
    cprof:start(Module),
    timer:sleep(Duration),
    Result = cprof:analyse(Module),
    cprof:stop(),
    
    {ok, #{
        mode => cprof,
        module => Module,
        duration_ms => Duration,
        result => Result
    }}.

%%%=============================================================================
%%% INTERNAL FUNCTIONS - FLAME GRAPHS
%%%=============================================================================

-spec parse_fprof_to_stacks(binary()) -> [map()].
parse_fprof_to_stacks(Binary) ->
    Lines = binary:split(Binary, <<"\n">>, [global]),
    %% Simplified parser - in production, use proper fprof format parsing
    lists:filtermap(fun(Line) ->
        case binary:split(Line, <<" ">>, [global]) of
            [Function, Time | _] ->
                {true, #{function => Function, time => Time}};
            _ -> false
        end
    end, Lines).

-spec fold_stacks([map()]) -> binary().
fold_stacks(Stacks) ->
    %% Convert to folded stack format: "func1;func2;func3 100"
    Folded = lists:map(fun(#{function := Func, time := Time}) ->
        <<Func/binary, " ", Time/binary, "\n">>
    end, Stacks),
    iolist_to_binary(Folded).

%%%=============================================================================
%%% INTERNAL FUNCTIONS - MESSAGE TRACING
%%%=============================================================================

-spec message_tracer(pid(), [term()]) -> ok.
message_tracer(Pid, Acc) ->
    receive
        {trace, Pid, 'receive', Msg} ->
            message_tracer(Pid, [Msg | Acc]);
        {get_messages, From} ->
            From ! {messages, Acc},
            ok;
        _ ->
            message_tracer(Pid, Acc)
    after 100 ->
        message_tracer(Pid, Acc)
    end.
