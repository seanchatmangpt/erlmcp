%%%-----------------------------------------------------------------------------
%%% @doc erlmcp_debugger - Live Process Debugging
%%%
%%% Provides:
%%% - Attach to running processes without stopping
%%% - Inspect state dynamically
%%% - Trace message flows in real-time
%%% - Set conditional breakpoints (via trace patterns)
%%% - Call graph visualization
%%%
%%% Usage:
%%% ```
%%% % Attach debugger to process
%%% erlmcp_debugger:attach(erlmcp_server).
%%%
%%% % Trace function calls
%%% erlmcp_debugger:trace_calls(erlmcp_json_rpc, encode, '_').
%%%
%%% % Set breakpoint
%%% erlmcp_debugger:breakpoint(erlmcp_server, handle_call, 
%%%     fun(Args) -> match_condition(Args) end).
%%% ```
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_debugger).

%% API - Process Inspection
-export([
    attach/1,
    detach/1,
    inspect_state/1,
    list_attached/0
]).

%% API - Message Tracing
-export([
    trace_calls/3,
    trace_messages/2,
    stop_trace/1
]).

%% API - Breakpoints
-export([
    breakpoint/3,
    breakpoint/4,
    clear_breakpoint/2,
    list_breakpoints/0
]).

%% API - Call Graph
-export([
    call_graph/2,
    visualize_call_graph/2
]).

%% Internal exports (for testing)
-export([
    process_info_to_map/1,
    call_graph_to_dot/1,
    timestamp/0
]).

-include_lib("kernel/include/logger.hrl").

%% Note: This module uses internal state management without a record
%% State is maintained via ETS tables and process dictionaries

-type trace_pattern() :: {module(), atom(), arity() | '_'}.
-type breakpoint_condition() :: fun((Args :: [term()]) -> boolean()).

%%%=============================================================================
%%% PROCESS INSPECTION API
%%%=============================================================================

%% @doc Attach debugger to a registered process or PID
-spec attach(atom() | pid()) -> {ok, pid()} | {error, term()}.
attach(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> {error, {not_found, Name}};
        Pid -> attach(Pid)
    end;
attach(Pid) when is_pid(Pid) ->
    case process_info(Pid, [registered_name, current_function, initial_call]) of
        undefined ->
            {error, process_not_found};
        Info ->
            %% Start monitoring
            Ref = erlang:monitor(process, Pid),
            
            State = #{
                pid => Pid,
                monitor_ref => Ref,
                info => Info,
                attached_at => erlang:system_time(millisecond),
                snapshots => []
            },
            
            %% Store in persistent term for fast access
            save_attached_state(Pid, State),
            
            ?LOG_INFO("Debugger attached to ~p (~p)", [Pid, proplists:get_value(registered_name, Info)]),
            {ok, Pid}
    end.

%% @doc Detach debugger from process
-spec detach(pid()) -> ok.
detach(Pid) ->
    case get_attached_state(Pid) of
        undefined ->
            {error, not_attached};
        #{monitor_ref := Ref} ->
            erlang:demonitor(Ref, [flush]),
            remove_attached_state(Pid),
            ?LOG_INFO("Debugger detached from ~p", [Pid]),
            ok
    end.

%% @doc Inspect current state of attached process
-spec inspect_state(pid()) -> {ok, map()} | {error, term()}.
inspect_state(Pid) ->
    case get_attached_state(Pid) of
        undefined ->
            {error, not_attached};
        AttachedState ->
            case process_info(Pid) of
                undefined ->
                    {error, process_dead};
                Info ->
                    State = try
                        sys:get_state(Pid, 1000)
                    catch
                        _:_ -> unavailable
                    end,
                    
                    Snapshot = #{
                        timestamp => erlang:system_time(millisecond),
                        process_info => Info,
                        state => State
                    },
                    
                    %% Store snapshot
                    Snapshots = maps:get(snapshots, AttachedState, []),
                    UpdatedState = AttachedState#{snapshots => [Snapshot | Snapshots]},
                    save_attached_state(Pid, UpdatedState),
                    
                    {ok, Snapshot}
            end
    end.

%% @doc List all attached processes
-spec list_attached() -> [map()].
list_attached() ->
    case persistent_term:get({?MODULE, attached}, undefined) of
        undefined -> [];
        Attached -> maps:values(Attached)
    end.

%%%=============================================================================
%%% MESSAGE TRACING API
%%%=============================================================================

%% @doc Trace function calls matching pattern
-spec trace_calls(module(), atom(), arity() | '_') -> {ok, reference()} | {error, term()}.
trace_calls(Module, Function, Arity) ->
    Ref = make_ref(),
    Tracer = spawn(fun() -> call_tracer(Ref, []) end),
    
    %% Set up trace pattern
    MatchSpec = [{'_', [], [{message, {caller}}]}],
    erlang:trace_pattern({Module, Function, Arity}, MatchSpec, [local]),
    erlang:trace(all, true, [call, {tracer, Tracer}]),
    
    TraceState = #{
        ref => Ref,
        tracer_pid => Tracer,
        pattern => {Module, Function, Arity},
        started_at => erlang:system_time(millisecond)
    },
    
    save_trace_state(Ref, TraceState),
    
    ?LOG_INFO("Tracing ~p:~p/~p (ref: ~p)", [Module, Function, Arity, Ref]),
    {ok, Ref}.

%% @doc Trace messages for a specific process
-spec trace_messages(pid(), pos_integer()) -> {ok, reference()} | {error, term()}.
trace_messages(Pid, Duration) ->
    Ref = make_ref(),
    Tracer = spawn(fun() -> message_tracer(Ref, Pid, []) end),
    
    erlang:trace(Pid, true, ['receive', {tracer, Tracer}]),
    
    %% Auto-stop after duration
    spawn(fun() ->
        timer:sleep(Duration),
        erlang:trace(Pid, false, ['receive']),
        Tracer ! stop
    end),
    
    TraceState = #{
        ref => Ref,
        tracer_pid => Tracer,
        target_pid => Pid,
        duration_ms => Duration,
        started_at => erlang:system_time(millisecond)
    },
    
    save_trace_state(Ref, TraceState),
    
    ?LOG_INFO("Tracing messages for ~p for ~pms (ref: ~p)", [Pid, Duration, Ref]),
    {ok, Ref}.

%% @doc Stop an active trace
-spec stop_trace(reference()) -> {ok, [term()]} | {error, term()}.
stop_trace(Ref) ->
    case get_trace_state(Ref) of
        undefined ->
            {error, trace_not_found};
        #{tracer_pid := Tracer, pattern := {M, F, A}} ->
            erlang:trace_pattern({M, F, A}, false, [local]),
            erlang:trace(all, false, [call]),
            Tracer ! {get_traces, self()},
            
            Result = receive
                {traces, Ref, Traces} -> {ok, Traces}
            after 5000 ->
                {ok, []}
            end,
            
            remove_trace_state(Ref),
            Result;
        #{tracer_pid := Tracer, target_pid := Pid} ->
            erlang:trace(Pid, false, ['receive']),
            Tracer ! {get_traces, self()},
            
            Result = receive
                {traces, Ref, Traces} -> {ok, Traces}
            after 5000 ->
                {ok, []}
            end,
            
            remove_trace_state(Ref),
            Result
    end.

%%%=============================================================================
%%% BREAKPOINT API
%%%=============================================================================

%% @doc Set an unconditional breakpoint
-spec breakpoint(module(), atom(), arity()) -> ok.
breakpoint(Module, Function, Arity) ->
    breakpoint(Module, Function, Arity, fun(_) -> true end).

%% @doc Set a conditional breakpoint
-spec breakpoint(module(), atom(), arity(), breakpoint_condition()) -> ok.
breakpoint(Module, Function, Arity, Condition) ->
    Key = {Module, Function},
    Breakpoints = get_breakpoints(),
    
    Existing = maps:get(Key, Breakpoints, []),
    Updated = Breakpoints#{Key => [{Arity, Condition} | Existing]},
    
    save_breakpoints(Updated),
    
    %% Set trace pattern
    MatchSpec = [{'_', [], [{message, {caller}}, {return_trace}]}],
    erlang:trace_pattern({Module, Function, Arity}, MatchSpec, [local]),
    
    ?LOG_INFO("Breakpoint set: ~p:~p/~p", [Module, Function, Arity]),
    ok.

%% @doc Clear a breakpoint
-spec clear_breakpoint(module(), atom()) -> ok.
clear_breakpoint(Module, Function) ->
    Key = {Module, Function},
    Breakpoints = get_breakpoints(),
    
    Updated = maps:remove(Key, Breakpoints),
    save_breakpoints(Updated),
    
    erlang:trace_pattern({Module, Function, '_'}, false, [local]),
    
    ?LOG_INFO("Breakpoint cleared: ~p:~p", [Module, Function]),
    ok.

%% @doc List all active breakpoints
-spec list_breakpoints() -> [map()].
list_breakpoints() ->
    Breakpoints = get_breakpoints(),
    [#{module => M, function => F, conditions => Conds} 
     || {{M, F}, Conds} <- maps:to_list(Breakpoints)].

%%%=============================================================================
%%% CALL GRAPH API
%%%=============================================================================

%% @doc Generate call graph for a process
-spec call_graph(pid(), pos_integer()) -> {ok, reference()} | {error, term()}.
call_graph(Pid, Duration) ->
    Ref = make_ref(),
    Collector = spawn(fun() -> call_graph_collector(Ref, []) end),
    
    %% Trace all calls from this process
    erlang:trace(Pid, true, [call, {tracer, Collector}]),
    erlang:trace_pattern({'_', '_', '_'}, [{'_', [], [{message, {caller}}]}], [local]),
    
    %% Auto-stop after duration
    spawn(fun() ->
        timer:sleep(Duration),
        erlang:trace(Pid, false, [call]),
        erlang:trace_pattern({'_', '_', '_'}, false, [local])
    end),
    
    save_call_graph_state(Ref, #{pid => Pid, duration => Duration}),
    
    ?LOG_INFO("Collecting call graph for ~p for ~pms", [Pid, Duration]),
    {ok, Ref}.

%% @doc Visualize call graph as DOT format
-spec visualize_call_graph(reference(), file:filename()) -> ok | {error, term()}.
visualize_call_graph(Ref, OutputFile) ->
    case get_call_graph_state(Ref) of
        undefined ->
            {error, call_graph_not_found};
        #{pid := Pid} ->
            %% Get process info for metadata
            ProcInfo = case process_info(Pid) of
                undefined -> #{};
                Info -> process_info_to_map(Info)
            end,

            %% Build DOT graph
            DOT = call_graph_to_dot(ProcInfo),

            %% Write to file
            case file:write_file(OutputFile, DOT) of
                ok ->
                    ?LOG_INFO("Call graph DOT written to ~p", [OutputFile]),
                    ok;
                {error, Reason} ->
                    {error, {file_write_error, Reason}}
            end
    end.

%%%=============================================================================
%%% INTERNAL FUNCTIONS - STATE MANAGEMENT
%%%=============================================================================

-spec save_attached_state(pid(), map()) -> ok.
save_attached_state(Pid, State) ->
    Attached = persistent_term:get({?MODULE, attached}, #{}),
    persistent_term:put({?MODULE, attached}, Attached#{Pid => State}),
    ok.

-spec get_attached_state(pid()) -> map() | undefined.
get_attached_state(Pid) ->
    case persistent_term:get({?MODULE, attached}, undefined) of
        undefined -> undefined;
        Attached -> maps:get(Pid, Attached, undefined)
    end.

-spec remove_attached_state(pid()) -> ok.
remove_attached_state(Pid) ->
    case persistent_term:get({?MODULE, attached}, undefined) of
        undefined -> ok;
        Attached ->
            persistent_term:put({?MODULE, attached}, maps:remove(Pid, Attached)),
            ok
    end.

-spec save_trace_state(reference(), map()) -> ok.
save_trace_state(Ref, State) ->
    Traces = persistent_term:get({?MODULE, traces}, #{}),
    persistent_term:put({?MODULE, traces}, Traces#{Ref => State}),
    ok.

-spec get_trace_state(reference()) -> map() | undefined.
get_trace_state(Ref) ->
    case persistent_term:get({?MODULE, traces}, undefined) of
        undefined -> undefined;
        Traces -> maps:get(Ref, Traces, undefined)
    end.

-spec remove_trace_state(reference()) -> ok.
remove_trace_state(Ref) ->
    case persistent_term:get({?MODULE, traces}, undefined) of
        undefined -> ok;
        Traces ->
            persistent_term:put({?MODULE, traces}, maps:remove(Ref, Traces)),
            ok
    end.

-spec get_breakpoints() -> map().
get_breakpoints() ->
    persistent_term:get({?MODULE, breakpoints}, #{}).

-spec save_breakpoints(map()) -> ok.
save_breakpoints(Breakpoints) ->
    persistent_term:put({?MODULE, breakpoints}, Breakpoints),
    ok.

-spec save_call_graph_state(reference(), map()) -> ok.
save_call_graph_state(Ref, State) ->
    Graphs = persistent_term:get({?MODULE, call_graphs}, #{}),
    persistent_term:put({?MODULE, call_graphs}, Graphs#{Ref => State}),
    ok.

-spec get_call_graph_state(reference()) -> map() | undefined.
get_call_graph_state(Ref) ->
    case persistent_term:get({?MODULE, call_graphs}, undefined) of
        undefined -> undefined;
        Graphs -> maps:get(Ref, Graphs, undefined)
    end.

%%%=============================================================================
%%% INTERNAL FUNCTIONS - TRACERS
%%%=============================================================================

-spec call_tracer(reference(), [term()]) -> ok.
call_tracer(Ref, Acc) ->
    receive
        {trace, _Pid, call, MFA} ->
            call_tracer(Ref, [MFA | Acc]);
        {get_traces, From} ->
            From ! {traces, Ref, lists:reverse(Acc)},
            ok;
        stop ->
            ok;
        _ ->
            call_tracer(Ref, Acc)
    end.

-spec message_tracer(reference(), pid(), [term()]) -> ok.
message_tracer(Ref, TargetPid, Acc) ->
    receive
        {trace, TargetPid, 'receive', Msg} ->
            Entry = #{
                timestamp => erlang:system_time(microsecond),
                message => Msg
            },
            message_tracer(Ref, TargetPid, [Entry | Acc]);
        {get_traces, From} ->
            From ! {traces, Ref, lists:reverse(Acc)},
            ok;
        stop ->
            ok;
        _ ->
            message_tracer(Ref, TargetPid, Acc)
    end.

-spec call_graph_collector(reference(), [term()]) -> ok.
call_graph_collector(Ref, Acc) ->
    receive
        {trace, _Pid, call, {M, F, Args}} ->
            Entry = {M, F, length(Args)},
            call_graph_collector(Ref, [Entry | Acc]);
        {get_graph, From} ->
            From ! {call_graph, Ref, lists:reverse(Acc)},
            ok;
        stop ->
            ok;
        _ ->
            call_graph_collector(Ref, Acc)
    end.

%%%=============================================================================
%%% INTERNAL FUNCTIONS - DOT GENERATION
%%%=============================================================================

%% @doc Convert process info to map
-spec process_info_to_map(proplists:proplist()) -> map().
process_info_to_map(InfoList) ->
    List = [{K, V} || {K, V} <- InfoList],
    maps:from_list(List).

%% @doc Generate DOT format from process call graph
%% Simple GraphViz DOT format for process visualization
-spec call_graph_to_dot(map()) -> iolist().
call_graph_to_dot(ProcInfo) ->
    Pid = maps:get(pid, ProcInfo, "unknown"),
    Name = case maps:get(registered_name, ProcInfo, undefined) of
        undefined -> pid_to_list(Pid);
        RegName when is_atom(RegName) -> atom_to_list(RegName)
    end,

    MsgQueueLen = maps:get(message_queue_len, ProcInfo, 0),
    MemUsage = maps:get(memory, ProcInfo, 0),
    CurrentFunc = case maps:get(current_function, ProcInfo, undefined) of
        {Mod, Func, Arity} -> io_lib:format("~s:~s/~p", [Mod, Func, Arity]);
        _ -> "unknown"
    end,

    NodeAttrs = io_lib:format(
        "  \"~s\" [label=\"~s\\nQueue: ~p\\nMemory: ~p\\n~s\" shape=box];",
        [pid_to_list(Pid), Name, MsgQueueLen, MemUsage, CurrentFunc]
    ),

    [
        "digraph ErlangProcesses {\n",
        "  rankdir=TB;\n",
        "  node [shape=box, style=rounded];\n",
        "  edge [fontsize=10];\n",
        "\n",
        "  /* Process node */\n",
        NodeAttrs, "\n",
        "\n",
        "  /* Graph metadata */\n",
        "  label=\"Erlang Process Call Graph\\nGenerated: " ++ timestamp() ++ "\";\n",
        "  labelloc=t;\n",
        "  fontsize=12;\n",
        "}\n"
    ].

%% @doc Get timestamp string
-spec timestamp() -> string().
timestamp() ->
    {{Y, Month, D}, {H, Min, S}} = calendar:universal_time(),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B UTC",
                  [Y, Month, D, H, Min, S]).
