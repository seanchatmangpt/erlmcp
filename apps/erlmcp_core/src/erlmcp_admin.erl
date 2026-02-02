%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_admin - Admin API for MCP Process Introspection
%%%
%%% Administrative commands for inspecting and managing MCP processes.
%%% Uses OTP 28 process iterators for efficient system introspection.
%%%
%%% Commands:
%%% - inspect_contexts()  - List all model context processes
%%% - inspect_tools()      - List all tool processes
%%% - inspect_transports() - List all transport processes
%%% - inspect_sessions()   - List all session backend processes
%%% - inspect_stats()      - Get aggregate statistics
%%% - inspect_process(Pid) - Get detailed info for specific process
%%% - inspect_by_type(Type) - List processes by type
%%% - export_snapshot()     - Export system state as JSON
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_admin).

%% API exports
-export([inspect_contexts/0, inspect_tools/0, inspect_transports/0,
         inspect_sessions/0, inspect_stats/0, inspect_process/1,
         inspect_by_type/1, export_snapshot/0, format_inspection/1]).

%% Types
-type formatted_inspection() :: iolist().

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Inspect all model context processes (erlmcp_server).
%% Returns list of process info maps.
-spec inspect_contexts() -> [erlmcp_inspector:process_info()].
inspect_contexts() ->
    erlmcp_inspector:inspect_by_type(model_context).

%% @doc Inspect all tool processes (erlmcp_client).
%% Returns list of process info maps.
-spec inspect_tools() -> [erlmcp_inspector:process_info()].
inspect_tools() ->
    erlmcp_inspector:inspect_by_type(tool_process).

%% @doc Inspect all transport handler processes.
%% Returns list of process info maps.
-spec inspect_transports() -> [erlmcp_inspector:process_info()].
inspect_transports() ->
    erlmcp_inspector:inspect_by_type(transport_handler).

%% @doc Inspect all session backend processes.
%% Returns list of process info maps.
-spec inspect_sessions() -> [erlmcp_inspector:process_info()].
inspect_sessions() ->
    erlmcp_inspector:inspect_by_type(session_backend).

%% @doc Get aggregate statistics across all MCP processes.
%% Returns stats map with counts, memory, and queue info.
-spec inspect_stats() -> erlmcp_inspector:aggregate_stats().
inspect_stats() ->
    erlmcp_inspector:get_aggregate_stats().

%% @doc Get detailed information for a specific process.
%% Returns enriched process info or error if process doesn't exist.
-spec inspect_process(pid()) -> {ok, erlmcp_inspector:process_info()} | {error, term()}.
inspect_process(Pid) when is_pid(Pid) ->
    erlmcp_inspector:get_process_info(Pid).

%% @doc Inspect processes by type with full details.
%% Type can be: model_context, tool_process, transport_handler,
%%              session_backend, registry, monitor, unknown
-spec inspect_by_type(atom()) -> [erlmcp_inspector:process_info()].
inspect_by_type(Type) when is_atom(Type) ->
    erlmcp_inspector:inspect_by_type(Type).

%% @doc Export system state snapshot as JSON.
%% Returns binary JSON string with full process dump.
-spec export_snapshot() -> binary().
export_snapshot() ->
    %% Get aggregate stats
    Stats = erlmcp_inspector:get_aggregate_stats(),

    %% Get all MCP processes with full details
    Processes = [maps:remove(pid, Info) || {_Pid, Info} <- erlmcp_inspector:list_mcp_processes()],

    %% Build snapshot map
    Snapshot = #{timestamp => erlang:system_time(millisecond),
                 stats => normalize_for_json(Stats),
                 processes => Processes},

    %% Encode as JSON
    jsx:encode(Snapshot, [space]).

%% @doc Format inspection result for human-readable display.
%% Returns formatted iolist suitable for console output.
-spec format_inspection(term()) -> formatted_inspection().
format_inspection({ok, Info}) when is_map(Info) ->
    format_process_info(Info);
format_inspection({error, Reason}) ->
    io_lib:format("Error: ~p~n", [Reason]);
format_inspection(List) when is_list(List) ->
    format_process_list(List);
format_inspection(Map) when is_map(Map) ->
    format_stats(Map).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Format single process info for display.
-spec format_process_info(map()) -> iolist().
format_process_info(Info) ->
    Pid = maps:get(pid, Info, unknown),
    Type = maps:get(type, Info, unknown),
    Name = maps:get(name, Info, undefined),
    Memory = maps:get(memory, Info, 0),
    QueueLen = maps:get(queue_len, Info, 0),

    ["Process: ", pid_to_list(Pid), "\n",
     "  Type: ", atom_to_list(Type), "\n",
     "  Name: ", format_name(Name), "\n",
     "  Memory: ", format_memory(Memory), "\n",
     "  Queue: ", integer_to_list(QueueLen), "\n"].

%% @doc Format process list for display.
-spec format_process_list([map()]) -> iolist().
format_process_list([]) ->
    "No processes found.\n";
format_process_list(Processes) ->
    Count = length(Processes),
    ["Total processes: ", integer_to_list(Count), "\n",
     lists:map(fun(Info) -> format_process_info(Info) end, Processes)].

%% @doc Format aggregate stats for display.
-spec format_stats(map()) -> iolist().
format_stats(Stats) ->
    Total = maps:get(total, Stats, 0),
    ByType = maps:get(by_type, Stats, #{}),
    TotalMemory = maps:get(total_memory, Stats, 0),
    MemoryByType = maps:get(memory_by_type, Stats, #{}),
    QueueStats = maps:get(queue_stats, Stats, #{}),

    ["=== MCP Process Statistics ===\n"
     "Total Processes: ", integer_to_list(Total), "\n"
     "Total Memory: ", format_memory(TotalMemory), "\n"
     "\n"
     "=== By Type ===\n", format_by_type(ByType),
     "\n"
     "=== Memory by Type ===\n", format_memory_by_type(MemoryByType),
     "\n"
     "=== Queue Statistics ===\n", format_queue_stats(QueueStats),
     "\n"].

%% @doc Format "by type" section.
-spec format_by_type(map()) -> iolist().
format_by_type(ByType) ->
    maps:fold(fun(Type, Count, Acc) ->
                     [atom_to_list(Type), ": ", integer_to_list(Count), "\n" | Acc]
              end,
              [],
              ByType).

%% @doc Format memory by type section.
-spec format_memory_by_type(map()) -> iolist().
format_memory_by_type(MemoryByType) ->
    maps:fold(fun(Type, Bytes, Acc) ->
                     [atom_to_list(Type), ": ", format_memory(Bytes), "\n" | Acc]
              end,
              [],
              MemoryByType).

%% @doc Format queue statistics section.
-spec format_queue_stats(map()) -> iolist().
format_queue_stats(QueueStats) ->
    maps:fold(fun(Type, Stats, Acc) ->
                     Min = maps:get(min, Stats, 0),
                     Max = maps:get(max, Stats, 0),
                     Avg = maps:get(avg, Stats, 0.0),

                     [atom_to_list(Type), ":\n"
                      "  Min: ", integer_to_list(Min), "\n"
                      "  Max: ", integer_to_list(Max), "\n"
                      "  Avg: ", format_float(Avg), "\n"
                      | Acc]
              end,
              [],
              QueueStats).

%% @doc Format process name (handle undefined).
-spec format_name(atom() | undefined) -> iolist().
format_name(undefined) -> "unnamed";
format_name(Name) when is_atom(Name) -> atom_to_list(Name).

%% @doc Format memory size in human-readable format.
-spec format_memory(non_neg_integer()) -> iolist().
format_memory(Bytes) when Bytes < 1024 ->
    [integer_to_list(Bytes), " B"];
format_memory(Bytes) when Bytes < 1024 * 1024 ->
    [format_float(Bytes / 1024), " KB"];
format_memory(Bytes) ->
    [format_float(Bytes / 1024 / 1024), " MB"].

%% @doc Format float with 2 decimal places.
-spec format_float(float()) -> iolist().
format_float(Float) ->
    io_lib:format("~.2f", [Float]).

%% @doc Normalize stats map for JSON serialization.
%% Converts atom keys to strings for better JSON compatibility.
-spec normalize_for_json(map()) -> map().
normalize_for_json(Stats) ->
    maps:fold(fun(Key, Value, Acc) when is_atom(Key) ->
                     KeyStr = atom_to_binary(Key),
                     maps:put(KeyStr, normalize_value(Value), Acc);
                (Key, Value, Acc) ->
                     maps:put(Key, normalize_value(Value), Acc)
              end,
              #{},
              Stats).

%% @doc Normalize value for JSON (recursively convert atoms to strings).
-spec normalize_value(term()) -> term().
normalize_value(Map) when is_map(Map) ->
    normalize_for_json(Map);
normalize_value(List) when is_list(List) ->
    [normalize_value(V) || V <- List];
normalize_value(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
normalize_value(Value) ->
    Value.
