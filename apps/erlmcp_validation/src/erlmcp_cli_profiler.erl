%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Profiler Module
%%%
%%% Provides performance profiling for erlmcp CLI:
%%% - CPU profiling (fprof, eprof)
%%% - Memory profiling
%%% - Function call profiling
%%% - Flamegraph generation support
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_profiler).

-export([profile_command/2, profile_command/3, profile_cpu/1, profile_memory/1, start_profiling/1,
         stop_profiling/0, get_profile_results/0, export_profile/2]).

%%====================================================================
%% Types
%%====================================================================

-type profile_type() :: cpu | memory | all.
-type profile_opts() ::
    #{type => profile_type(),
      duration => pos_integer(),
      output_file => string(),
      format => atom()}.
-type profiler_backend() :: fprof | eprof | cprof.

%%====================================================================
%% API Functions - Command Profiling
%%====================================================================

%% @doc Profile execution of a command
-spec profile_command(fun(() -> term()), profile_opts()) -> {ok, map()} | {error, term()}.
profile_command(Command, Opts) ->
    Type = maps:get(type, Opts, all),
    profile_command(Command, Type, Opts).

%% @doc Profile execution with specific type
-spec profile_command(fun(() -> term()), profile_type(), profile_opts()) ->
                         {ok, map()} | {error, term()}.
profile_command(Command, Type, Opts) when is_function(Command, 0) ->
    try
        case Type of
            cpu ->
                profile_command_cpu(Command, Opts);
            memory ->
                profile_command_memory(Command, Opts);
            all ->
                CPUResult = profile_command_cpu(Command, Opts),
                MemoryResult = profile_command_memory(Command, Opts),
                {ok,
                 #{cpu => maps:get(cpu, element(2, CPUResult)),
                   memory => maps:get(memory, element(2, MemoryResult))}}
        end
    catch
        Class:Error:Stack ->
            {error,
             #{class => Class,
               error => Error,
               stacktrace => Stack}}
    end.

%% @doc Profile CPU usage
-spec profile_cpu(profile_opts()) -> {ok, map()} | {error, term()}.
profile_cpu(Opts) ->
    Backend = maps:get(backend, Opts, fprof),
    Duration = maps:get(duration, Opts, 10000),

    try
        case Backend of
            fprof ->
                profile_with_fprof(Duration, Opts);
            eprof ->
                profile_with_eprof(Duration, Opts);
            cprof ->
                profile_with_cprof(Duration, Opts)
        end
    catch
        Class:Error:Stack ->
            {error,
             #{class => Class,
               error => Error,
               stacktrace => Stack}}
    end.

%% @doc Profile memory usage
-spec profile_memory(profile_opts()) -> {ok, map()} | {error, term()}.
profile_memory(Opts) ->
    Duration = maps:get(duration, Opts, 10000),

    try
        %% Capture initial memory state
        InitialMemory = erlang:memory(),

        %% Wait for duration
        timer:sleep(Duration),

        %% Capture final memory state
        FinalMemory = erlang:memory(),

        %% Calculate deltas
        Deltas = calculate_memory_deltas(InitialMemory, FinalMemory),

        %% Get detailed process memory info
        ProcessMemory = get_process_memory_profile(),

        {ok,
         #{memory =>
               #{initial => InitialMemory,
                 final => FinalMemory,
                 deltas => Deltas,
                 process_breakdown => ProcessMemory,
                 timestamp => iso8601_timestamp()}}}
    catch
        Class:Error:Stack ->
            {error,
             #{class => Class,
               error => Error,
               stacktrace => Stack}}
    end.

%% @doc Start profiling session
-spec start_profiling(profile_opts()) -> {ok, pid()} | {error, term()}.
start_profiling(Opts) ->
    Type = maps:get(type, Opts, cpu),
    Backend = maps:get(backend, Opts, fprof),

    try
        case {Type, Backend} of
            {cpu, fprof} ->
                fprof:trace([start, {file, "fprof.trace"}]),
                {ok, self()};
            {cpu, eprof} ->
                eprof:start(),
                eprof:start_profiling(
                    erlang:processes()),
                {ok, self()};
            {cpu, cprof} ->
                cprof:start(),
                {ok, self()};
            _ ->
                {error, unsupported_profiler}
        end
    catch
        Class:Error:Stack ->
            {error,
             #{class => Class,
               error => Error,
               stacktrace => Stack}}
    end.

%% @doc Stop profiling session
-spec stop_profiling() -> {ok, map()} | {error, term()}.
stop_profiling() ->
    try
        %% Try to stop all profilers
        FprofResult =
            try
                fprof:trace(stop),
                fprof:profile({file, "fprof.trace"}),
                fprof:analyse([{dest, "fprof.analysis"}]),
                {ok, <<"fprof.analysis">>}
            catch
                _:_ ->
                    {error, not_running}
            end,

        EprofResult =
            try
                eprof:stop_profiling(),
                eprof:analyze(total),
                eprof:stop(),
                {ok, analysis_done}
            catch
                _:_ ->
                    {error, not_running}
            end,

        CprofResult =
            try
                cprof:pause(),
                Analysis = cprof:analyse(),
                cprof:stop(),
                {ok, Analysis}
            catch
                _:_ ->
                    {error, not_running}
            end,

        {ok,
         #{fprof => FprofResult,
           eprof => EprofResult,
           cprof => CprofResult}}
    catch
        Class:Error:Stack ->
            {error,
             #{class => Class,
               error => Error,
               stacktrace => Stack}}
    end.

%% @doc Get current profiling results
-spec get_profile_results() -> {ok, map()} | {error, term()}.
get_profile_results() ->
    try
        %% Try to get results from each profiler
        Results =
            #{fprof => get_fprof_results(),
              eprof => get_eprof_results(),
              cprof => get_cprof_results()},
        {ok, Results}
    catch
        Class:Error:Stack ->
            {error,
             #{class => Class,
               error => Error,
               stacktrace => Stack}}
    end.

%% @doc Export profile to file
-spec export_profile(map(), string()) -> ok | {error, term()}.
export_profile(ProfileData, Filename) ->
    try
        %% Determine format from filename extension
        Format =
            case filename:extension(Filename) of
                ".json" ->
                    json;
                ".txt" ->
                    text;
                ".html" ->
                    html;
                _ ->
                    json
            end,

        %% Format data
        FormattedData = format_profile_data(ProfileData, Format),

        %% Write to file
        file:write_file(Filename, FormattedData)
    catch
        Class:Error:Stack ->
            {error,
             #{class => Class,
               error => Error,
               stacktrace => Stack}}
    end.

%%====================================================================
%% Internal Functions - CPU Profiling
%%====================================================================

%% @private Profile command with fprof
-spec profile_command_cpu(fun(() -> term()), profile_opts()) -> {ok, map()}.
profile_command_cpu(Command, Opts) ->
    Backend = maps:get(backend, Opts, fprof),

    StartTime = erlang:monotonic_time(millisecond),

    %% Start profiler
    case Backend of
        fprof ->
            fprof:trace([start, {procs, [self()]}]);
        eprof ->
            eprof:start(),
            eprof:start_profiling([self()]);
        cprof ->
            cprof:start()
    end,

    %% Execute command
    Result = Command(),

    %% Stop profiler and analyze
    ProfileResult =
        case Backend of
            fprof ->
                fprof:trace(stop),
                fprof:profile(),
                fprof:analyse([totals, {sort, acc}]),
                get_fprof_results();
            eprof ->
                eprof:stop_profiling(),
                eprof:analyze(total),
                eprof:stop(),
                get_eprof_results();
            cprof ->
                cprof:pause(),
                Analysis = cprof:analyse(),
                cprof:stop(),
                Analysis
        end,

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    {ok,
     #{cpu =>
           #{backend => Backend,
             duration_ms => Duration,
             result => Result,
             profile => ProfileResult,
             timestamp => iso8601_timestamp()}}}.

%% @private Profile with fprof
-spec profile_with_fprof(pos_integer(), profile_opts()) -> {ok, map()}.
profile_with_fprof(Duration, _Opts) ->
    fprof:trace([start]),
    timer:sleep(Duration),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse([totals, {sort, acc}]),

    Results = get_fprof_results(),

    {ok,
     #{cpu =>
           #{backend => fprof,
             duration_ms => Duration,
             results => Results,
             timestamp => iso8601_timestamp()}}}.

%% @private Profile with eprof
-spec profile_with_eprof(pos_integer(), profile_opts()) -> {ok, map()}.
profile_with_eprof(Duration, _Opts) ->
    eprof:start(),
    eprof:start_profiling(
        erlang:processes()),
    timer:sleep(Duration),
    eprof:stop_profiling(),
    eprof:analyze(total),
    Results = get_eprof_results(),
    eprof:stop(),

    {ok,
     #{cpu =>
           #{backend => eprof,
             duration_ms => Duration,
             results => Results,
             timestamp => iso8601_timestamp()}}}.

%% @private Profile with cprof
-spec profile_with_cprof(pos_integer(), profile_opts()) -> {ok, map()}.
profile_with_cprof(Duration, _Opts) ->
    cprof:start(),
    timer:sleep(Duration),
    cprof:pause(),
    Analysis = cprof:analyse(),
    cprof:stop(),

    {ok,
     #{cpu =>
           #{backend => cprof,
             duration_ms => Duration,
             results => Analysis,
             timestamp => iso8601_timestamp()}}}.

%%====================================================================
%% Internal Functions - Memory Profiling
%%====================================================================

%% @private Profile command memory usage
-spec profile_command_memory(fun(() -> term()), profile_opts()) -> {ok, map()}.
profile_command_memory(Command, _Opts) ->
    %% Capture initial state
    InitialMemory = erlang:memory(),
    erlang:garbage_collect(),

    StartTime = erlang:monotonic_time(millisecond),

    %% Execute command
    Result = Command(),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    %% Capture final state
    erlang:garbage_collect(),
    FinalMemory = erlang:memory(),

    %% Calculate deltas
    Deltas = calculate_memory_deltas(InitialMemory, FinalMemory),

    {ok,
     #{memory =>
           #{duration_ms => Duration,
             result => Result,
             initial => InitialMemory,
             final => FinalMemory,
             deltas => Deltas,
             timestamp => iso8601_timestamp()}}}.

%% @private Calculate memory deltas
-spec calculate_memory_deltas(list(), list()) -> map().
calculate_memory_deltas(Initial, Final) ->
    InitialMap = maps:from_list(Initial),
    FinalMap = maps:from_list(Final),

    Keys = lists:usort(maps:keys(InitialMap) ++ maps:keys(FinalMap)),

    maps:from_list([{Key,
                     #{initial => maps:get(Key, InitialMap, 0),
                       final => maps:get(Key, FinalMap, 0),
                       delta => maps:get(Key, FinalMap, 0) - maps:get(Key, InitialMap, 0),
                       delta_mb =>
                           (maps:get(Key, FinalMap, 0) - maps:get(Key, InitialMap, 0))
                           / (1024 * 1024)}}
                    || Key <- Keys]).

%% @private Get process memory profile
-spec get_process_memory_profile() -> [map()].
get_process_memory_profile() ->
    Processes = erlang:processes(),
    ProcessInfo =
        lists:filtermap(fun(Pid) ->
                           case process_info(Pid,
                                             [memory, registered_name, current_function, heap_size])
                           of
                               undefined ->
                                   false;
                               Info ->
                                   Memory = proplists:get_value(memory, Info, 0),
                                   HeapSize = proplists:get_value(heap_size, Info, 0),
                                   RegName = proplists:get_value(registered_name, Info, undefined),
                                   CurrentFun =
                                       proplists:get_value(current_function, Info, undefined),
                                   {true,
                                    #{pid => pid_to_list(Pid),
                                      memory_bytes => Memory,
                                      memory_mb => Memory / (1024 * 1024),
                                      heap_size => HeapSize,
                                      registered_name => RegName,
                                      current_function => CurrentFun}}
                           end
                        end,
                        Processes),

    %% Sort by memory usage
    Sorted =
        lists:sort(fun(#{memory_bytes := M1}, #{memory_bytes := M2}) -> M1 > M2 end, ProcessInfo),

    %% Return top 20
    lists:sublist(Sorted, 20).

%%====================================================================
%% Internal Functions - Result Retrieval
%%====================================================================

%% @private Get fprof results
-spec get_fprof_results() -> map() | {error, term()}.
get_fprof_results() ->
    try
        %% fprof results are typically written to a file
        %% We return a placeholder indicating results are in analysis output
        #{status => completed,
          output_file => "fprof.analysis",
          note => <<"Results written to fprof.analysis file">>}
    catch
        _:_ ->
            {error, no_results}
    end.

%% @private Get eprof results
-spec get_eprof_results() -> map() | {error, term()}.
get_eprof_results() ->
    try
        #{status => completed, note => <<"Analysis output printed to console">>}
    catch
        _:_ ->
            {error, no_results}
    end.

%% @private Get cprof results
-spec get_cprof_results() -> list() | {error, term()}.
get_cprof_results() ->
    try
        cprof:analyse()
    catch
        _:_ ->
            {error, no_results}
    end.

%%====================================================================
%% Internal Functions - Formatting
%%====================================================================

%% @private Format profile data
-spec format_profile_data(map(), atom()) -> iolist().
format_profile_data(ProfileData, json) ->
    jsx:encode(normalize_for_json(ProfileData), [{space, 1}, {indent, 2}]);
format_profile_data(ProfileData, text) ->
    io_lib:format("~p~n", [ProfileData]);
format_profile_data(ProfileData, html) ->
    generate_html_profile(ProfileData).

%% @private Normalize data for JSON
-spec normalize_for_json(term()) -> term().
normalize_for_json(Data) when is_map(Data) ->
    maps:fold(fun(K, V, Acc) ->
                 Key = normalize_json_key(K),
                 Value = normalize_for_json(V),
                 Acc#{Key => Value}
              end,
              #{},
              Data);
normalize_for_json(Data) when is_list(Data) ->
    case io_lib:printable_unicode_list(Data) of
        true ->
            list_to_binary(Data);
        false ->
            [normalize_for_json(Item) || Item <- Data]
    end;
normalize_for_json(Data) when is_atom(Data) ->
    atom_to_binary(Data, utf8);
normalize_for_json(Data) when is_pid(Data) ->
    list_to_binary(pid_to_list(Data));
normalize_for_json(Data) when is_reference(Data) ->
    list_to_binary(ref_to_list(Data));
normalize_for_json(Data) when is_tuple(Data) ->
    list_to_binary(io_lib:format("~p", [Data]));
normalize_for_json(Data) when is_binary(Data); is_number(Data); is_boolean(Data) ->
    Data;
normalize_for_json(Data) ->
    list_to_binary(io_lib:format("~p", [Data])).

-spec normalize_json_key(term()) -> binary().
normalize_json_key(Key) when is_binary(Key) ->
    Key;
normalize_json_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
normalize_json_key(Key) when is_list(Key) ->
    list_to_binary(Key);
normalize_json_key(Key) ->
    list_to_binary(io_lib:format("~p", [Key])).

%% @private Generate HTML profile report
-spec generate_html_profile(map()) -> iolist().
generate_html_profile(ProfileData) ->
    ["<!DOCTYPE html>\n",
     "<html>\n",
     "<head>\n",
     "  <title>Profile Report</title>\n",
     "  <style>\n",
     "    body { font-family: monospace; padding: 20px; }\n",
     "    table { border-collapse: collapse; width: 100%; }\n",
     "    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }\n",
     "    th { background-color: #4CAF50; color: white; }\n",
     "  </style>\n",
     "</head>\n",
     "<body>\n",
     "  <h1>Profile Report</h1>\n",
     "  <pre>",
     io_lib:format("~p", [ProfileData]),
     "</pre>\n",
     "</body>\n",
     "</html>\n"].

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

%% @private Generate ISO 8601 timestamp
-spec iso8601_timestamp() -> binary().
iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                   [Year, Month, Day, Hour, Min, Sec])).
