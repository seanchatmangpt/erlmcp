%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Tracer Module
%%%
%%% Provides detailed execution tracing for erlmcp CLI:
%%% - Selective function tracing
%%% - Message tracing
%%% - Process tracing
%%% - Trace output to JSON
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_tracer).

-export([trace_command/2, trace_command/3, start_trace/1, stop_trace/0, get_trace_results/0,
         export_trace/2, parse_trace_spec/1]).

%%====================================================================
%% Types
%%====================================================================

-type trace_spec() :: binary() | string().
-type trace_opts() ::
    #{spec => trace_spec(),
      duration => pos_integer(),
      output_file => string(),
      max_events => pos_integer()}.
-type trace_event() ::
    #{type => atom(),
      timestamp => integer(),
      pid => binary(),
      module => atom(),
      function => atom(),
      args => list(),
      result => term()}.

%%====================================================================
%% API Functions - Command Tracing
%%====================================================================

%% @doc Trace command execution
-spec trace_command(fun(() -> term()), trace_opts()) -> {ok, map()} | {error, term()}.
trace_command(Command, Opts) ->
    TraceSpec = maps:get(spec, Opts, all),
    trace_command(Command, TraceSpec, Opts).

%% @doc Trace command with specific trace specification
-spec trace_command(fun(() -> term()), trace_spec(), trace_opts()) -> {ok, map()} | {error, term()}.
trace_command(Command, TraceSpec, Opts) when is_function(Command, 0) ->
    try
        %% Parse trace specification
        TracePatterns = parse_trace_spec(TraceSpec),

        %% Start tracing
        {ok, TracePid} = start_trace_internal(TracePatterns, Opts),

        StartTime = erlang:monotonic_time(millisecond),

        %% Execute command
        Result = Command(),

        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        %% Stop tracing
        TraceResults = stop_trace_internal(TracePid),

        {ok,
         #{duration_ms => Duration,
           result => Result,
           trace => TraceResults,
           timestamp => iso8601_timestamp()}}
    catch
        Class:Error:Stack ->
            {error,
             #{class => Class,
               error => Error,
               stacktrace => Stack}}
    end.

%% @doc Start tracing session
-spec start_trace(trace_opts()) -> {ok, pid()} | {error, term()}.
start_trace(Opts) ->
    try
        TraceSpec = maps:get(spec, Opts, all),
        TracePatterns = parse_trace_spec(TraceSpec),
        start_trace_internal(TracePatterns, Opts)
    catch
        Class:Error:Stack ->
            {error,
             #{class => Class,
               error => Error,
               stacktrace => Stack}}
    end.

%% @doc Stop tracing session
-spec stop_trace() -> {ok, [trace_event()]} | {error, term()}.
stop_trace() ->
    try
        case erlang:whereis(erlmcp_trace_collector) of
            undefined ->
                {error, no_active_trace};
            Pid ->
                Events = stop_trace_internal(Pid),
                {ok, Events}
        end
    catch
        Class:Error:Stack ->
            {error,
             #{class => Class,
               error => Error,
               stacktrace => Stack}}
    end.

%% @doc Get current trace results
-spec get_trace_results() -> {ok, [trace_event()]} | {error, term()}.
get_trace_results() ->
    try
        case erlang:whereis(erlmcp_trace_collector) of
            undefined ->
                {error, no_active_trace};
            Pid ->
                Pid ! {get_events, self()},
                receive
                    {trace_events, Events} ->
                        {ok, Events}
                after 5000 ->
                    {error, timeout}
                end
        end
    catch
        Class:Error:Stack ->
            {error,
             #{class => Class,
               error => Error,
               stacktrace => Stack}}
    end.

%% @doc Export trace to file
-spec export_trace([trace_event()], string()) -> ok | {error, term()}.
export_trace(TraceEvents, Filename) ->
    try
        %% Determine format from extension
        Format =
            case filename:extension(Filename) of
                ".json" ->
                    json;
                ".txt" ->
                    text;
                _ ->
                    json
            end,

        %% Format data
        FormattedData = format_trace_data(TraceEvents, Format),

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
%% API Functions - Trace Specification Parsing
%%====================================================================

%% @doc Parse trace specification string
%% Examples:
%%   "http:request,validation:parse" -> trace http:request/N and validation:parse/N
%%   "erlmcp_*" -> trace all erlmcp_* modules
%%   "all" -> trace everything (use with caution)
-spec parse_trace_spec(trace_spec()) -> [trace_pattern()].
parse_trace_spec(all) ->
    [{all, all, all}];
parse_trace_spec(Spec) when is_binary(Spec) ->
    parse_trace_spec(binary_to_list(Spec));
parse_trace_spec(Spec) when is_list(Spec) ->
    %% Split by comma
    Patterns = string:split(Spec, ",", all),

    lists:map(fun(Pattern) -> parse_single_pattern(string:trim(Pattern)) end, Patterns).

-type trace_pattern() :: {module(), function(), arity() | all}.

%% @private Parse single trace pattern
-spec parse_single_pattern(string()) -> trace_pattern().
parse_single_pattern("all") ->
    {all, all, all};
parse_single_pattern(Pattern) ->
    case string:split(Pattern, ":") of
        [Module] ->
            %% Module only -> trace all functions
            {list_to_atom(Module), all, all};
        [Module, Function] ->
            %% Module:Function -> trace all arities
            case string:split(Function, "/") of
                [Fun] ->
                    {list_to_atom(Module), list_to_atom(Fun), all};
                [Fun, Arity] ->
                    {list_to_atom(Module), list_to_atom(Fun), list_to_integer(Arity)}
            end
    end.

%%====================================================================
%% Internal Functions - Tracing
%%====================================================================

%% @private Start trace collector and enable tracing
-spec start_trace_internal([trace_pattern()], trace_opts()) -> {ok, pid()}.
start_trace_internal(TracePatterns, Opts) ->
    MaxEvents = maps:get(max_events, Opts, 10000),

    %% Start trace collector process via supervisor
    {ok, CollectorPid} =
        case erlmcp_validation_sup:start_trace_collector(MaxEvents) of
            {ok, Pid} ->
                {ok, Pid};
            {error, {already_started, Pid}} ->
                {ok, Pid}
        end,

    %% Register collector
    erlang:register(erlmcp_trace_collector, CollectorPid),

    %% Set up tracer
    erlang:trace(all, true, [call, procs, {tracer, CollectorPid}]),

    %% Apply trace patterns
    lists:foreach(fun({Module, Function, Arity}) -> apply_trace_pattern(Module, Function, Arity)
                  end,
                  TracePatterns),

    {ok, CollectorPid}.

%% @private Stop tracing and collect results
-spec stop_trace_internal(pid()) -> [trace_event()].
stop_trace_internal(CollectorPid) ->
    %% Disable all tracing
    erlang:trace(all, false, [call, procs]),

    %% Clear all trace patterns
    erlang:trace_pattern({all, all, all}, false, []),

    %% Get collected events
    CollectorPid ! {get_events, self()},
    Events =
        receive
            {trace_events, E} ->
                E
        after 5000 ->
            []
        end,

    %% Stop collector
    CollectorPid ! stop,

    %% Unregister
    catch erlang:unregister(erlmcp_trace_collector),

    Events.

%% @private Apply trace pattern
-spec apply_trace_pattern(module(), function(), arity() | all) -> ok.
apply_trace_pattern(all, all, all) ->
    %% Trace all functions (use with caution!)
    erlang:trace_pattern({all, all, all}, true, [call_time]),
    ok;
apply_trace_pattern(Module, all, all) ->
    %% Trace all functions in module
    erlang:trace_pattern({Module, all, all}, true, [call_time]),
    ok;
apply_trace_pattern(Module, Function, all) ->
    %% Trace all arities of function
    erlang:trace_pattern({Module, Function, all}, true, [call_time]),
    ok;
apply_trace_pattern(Module, Function, Arity) ->
    %% Trace specific function/arity
    erlang:trace_pattern({Module, Function, Arity}, true, [call_time]),
    ok.

%% @private Trace collector loop
-spec trace_collector_loop([trace_event()], pos_integer()) -> ok.
trace_collector_loop(Events, MaxEvents) ->
    receive
        {trace, Pid, call, {M, F, Args}} ->
            Event =
                #{type => call,
                  timestamp => erlang:system_time(microsecond),
                  pid => list_to_binary(pid_to_list(Pid)),
                  module => M,
                  function => F,
                  args => Args},
            NewEvents = [Event | lists:sublist(Events, MaxEvents - 1)],
            trace_collector_loop(NewEvents, MaxEvents);
        {trace, Pid, return_from, {M, F, A}, Result} ->
            Event =
                #{type => return,
                  timestamp => erlang:system_time(microsecond),
                  pid => list_to_binary(pid_to_list(Pid)),
                  module => M,
                  function => F,
                  arity => A,
                  result => format_result(Result)},
            NewEvents = [Event | lists:sublist(Events, MaxEvents - 1)],
            trace_collector_loop(NewEvents, MaxEvents);
        {trace, Pid, spawn, Pid2, {M, F, Args}} ->
            Event =
                #{type => spawn,
                  timestamp => erlang:system_time(microsecond),
                  pid => list_to_binary(pid_to_list(Pid)),
                  spawned_pid => list_to_binary(pid_to_list(Pid2)),
                  module => M,
                  function => F,
                  args => Args},
            NewEvents = [Event | lists:sublist(Events, MaxEvents - 1)],
            trace_collector_loop(NewEvents, MaxEvents);
        {trace, Pid, exit, Reason} ->
            Event =
                #{type => exit,
                  timestamp => erlang:system_time(microsecond),
                  pid => list_to_binary(pid_to_list(Pid)),
                  reason => format_result(Reason)},
            NewEvents = [Event | lists:sublist(Events, MaxEvents - 1)],
            trace_collector_loop(NewEvents, MaxEvents);
        {get_events, From} ->
            From ! {trace_events, lists:reverse(Events)},
            trace_collector_loop(Events, MaxEvents);
        stop ->
            ok;
        _Other ->
            trace_collector_loop(Events, MaxEvents)
    end.

%%====================================================================
%% Internal Functions - Formatting
%%====================================================================

%% @private Format trace data
-spec format_trace_data([trace_event()], atom()) -> iolist().
format_trace_data(TraceEvents, json) ->
    jsx:encode(normalize_for_json(TraceEvents), [{space, 1}, {indent, 2}]);
format_trace_data(TraceEvents, text) ->
    [format_trace_event_text(Event) || Event <- TraceEvents].

%% @private Format single trace event as text
-spec format_trace_event_text(trace_event()) -> iolist().
format_trace_event_text(#{type := call,
                          timestamp := TS,
                          pid := Pid,
                          module := M,
                          function := F,
                          args := Args}) ->
    io_lib:format("[~p] ~s CALL ~s:~s(~p)~n", [TS, Pid, M, F, Args]);
format_trace_event_text(#{type := return,
                          timestamp := TS,
                          pid := Pid,
                          module := M,
                          function := F,
                          result := Result}) ->
    io_lib:format("[~p] ~s RETURN ~s:~s -> ~p~n", [TS, Pid, M, F, Result]);
format_trace_event_text(#{type := spawn,
                          timestamp := TS,
                          pid := Pid,
                          spawned_pid := Pid2}) ->
    io_lib:format("[~p] ~s SPAWN ~s~n", [TS, Pid, Pid2]);
format_trace_event_text(#{type := exit,
                          timestamp := TS,
                          pid := Pid,
                          reason := Reason}) ->
    io_lib:format("[~p] ~s EXIT ~p~n", [TS, Pid, Reason]);
format_trace_event_text(Event) ->
    io_lib:format("~p~n", [Event]).

%% @private Format result for storage
-spec format_result(term()) -> binary().
format_result(Result) when is_binary(Result) ->
    Result;
format_result(Result) when is_atom(Result) ->
    atom_to_binary(Result, utf8);
format_result(Result) when is_list(Result) ->
    case io_lib:printable_unicode_list(Result) of
        true ->
            list_to_binary(Result);
        false ->
            list_to_binary(io_lib:format("~p", [Result]))
    end;
format_result(Result) when is_pid(Result) ->
    list_to_binary(pid_to_list(Result));
format_result(Result) ->
    list_to_binary(io_lib:format("~p", [Result])).

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

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

%% @private Generate ISO 8601 timestamp
-spec iso8601_timestamp() -> binary().
iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                   [Year, Month, Day, Hour, Min, Sec])).
