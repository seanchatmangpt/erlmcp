%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Statistics Module
%%%
%%% Provides command execution analytics for erlmcp CLI:
%%% - Command execution history
%%% - Usage statistics
%%% - Performance trends
%%% - Most/least used commands
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_stats).

-behaviour(gen_server).

%% API
-export([start_link/0, record_command/3, get_stats/0, get_history/0, get_history/1,
         get_command_stats/1, clear_history/0, export_stats/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-record(command_execution,
        {command :: binary(),
         args :: list(),
         timestamp :: integer(),
         duration_ms :: non_neg_integer(),
         status :: atom(),
         error :: term() | undefined}).
-record(state,
        {history = [] :: [#command_execution{}],
         max_history = 1000 :: pos_integer(),
         stats = #{} :: map()}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the stats server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Record a command execution
-spec record_command(binary() | string(), list(), map()) -> ok.
record_command(Command, Args, Result) when is_list(Command) ->
    record_command(list_to_binary(Command), Args, Result);
record_command(Command, Args, Result) when is_binary(Command) ->
    gen_server:cast(?MODULE, {record_command, Command, Args, Result}).

%% @doc Get overall statistics
-spec get_stats() -> {ok, map()} | {error, term()}.
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% @doc Get command execution history
-spec get_history() -> {ok, [map()]} | {error, term()}.
get_history() ->
    gen_server:call(?MODULE, {get_history, all}).

%% @doc Get filtered history
-spec get_history(map()) -> {ok, [map()]} | {error, term()}.
get_history(Opts) ->
    gen_server:call(?MODULE, {get_history, Opts}).

%% @doc Get statistics for a specific command
-spec get_command_stats(binary() | string()) -> {ok, map()} | {error, term()}.
get_command_stats(Command) when is_list(Command) ->
    get_command_stats(list_to_binary(Command));
get_command_stats(Command) when is_binary(Command) ->
    gen_server:call(?MODULE, {get_command_stats, Command}).

%% @doc Clear execution history
-spec clear_history() -> ok.
clear_history() ->
    gen_server:call(?MODULE, clear_history).

%% @doc Export statistics to file
-spec export_stats(string()) -> ok | {error, term()}.
export_stats(Filename) ->
    case get_stats() of
        {ok, Stats} ->
            Format =
                case filename:extension(Filename) of
                    ".json" ->
                        json;
                    _ ->
                        json
                end,
            export_stats_internal(Stats, Filename, Format);
        Error ->
            Error
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call(get_stats, _From, State) ->
    Stats = calculate_stats(State),
    {reply, {ok, Stats}, State};
handle_call({get_history, all}, _From, State) ->
    History = format_history(State#state.history),
    {reply, {ok, History}, State};
handle_call({get_history, Opts}, _From, State) ->
    Filtered = filter_history(State#state.history, Opts),
    History = format_history(Filtered),
    {reply, {ok, History}, State};
handle_call({get_command_stats, Command}, _From, State) ->
    Stats = get_command_stats_internal(Command, State),
    {reply, {ok, Stats}, State};
handle_call(clear_history, _From, State) ->
    NewState = State#state{history = [], stats = #{}},
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({record_command, Command, Args, Result}, State) ->
    Execution =
        #command_execution{command = Command,
                           args = Args,
                           timestamp = erlang:system_time(millisecond),
                           duration_ms = maps:get(duration_ms, Result, 0),
                           status = maps:get(status, Result, unknown),
                           error = maps:get(error, Result, undefined)},

    NewHistory = [Execution | lists:sublist(State#state.history, State#state.max_history - 1)],
    NewStats = update_stats(Command, Execution, State#state.stats),

    NewState = State#state{history = NewHistory, stats = NewStats},
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Statistics Calculation
%%====================================================================

%% @private Calculate overall statistics
-spec calculate_stats(#state{}) -> map().
calculate_stats(State) ->
    History = State#state.history,
    Stats = State#state.stats,

    TotalCommands = length(History),

    %% Command frequency
    CommandFrequency = calculate_command_frequency(History),

    %% Status breakdown
    StatusBreakdown = calculate_status_breakdown(History),

    %% Performance statistics
    PerformanceStats = calculate_performance_stats(History),

    %% Time-based statistics
    TimeStats = calculate_time_stats(History),

    #{total_commands => TotalCommands,
      command_frequency => CommandFrequency,
      status_breakdown => StatusBreakdown,
      performance => PerformanceStats,
      time_stats => TimeStats,
      most_used_commands => get_most_used_commands(CommandFrequency, 10),
      slowest_commands => get_slowest_commands(History, 10),
      recent_errors => get_recent_errors(History, 10),
      per_command_stats => Stats}.

%% @private Calculate command frequency
-spec calculate_command_frequency([#command_execution{}]) -> map().
calculate_command_frequency(History) ->
    lists:foldl(fun(#command_execution{command = Cmd}, Acc) ->
                   maps:update_with(Cmd, fun(Count) -> Count + 1 end, 1, Acc)
                end,
                #{},
                History).

%% @private Calculate status breakdown
-spec calculate_status_breakdown([#command_execution{}]) -> map().
calculate_status_breakdown(History) ->
    lists:foldl(fun(#command_execution{status = Status}, Acc) ->
                   maps:update_with(Status, fun(Count) -> Count + 1 end, 1, Acc)
                end,
                #{},
                History).

%% @private Calculate performance statistics
-spec calculate_performance_stats([#command_execution{}]) -> map().
calculate_performance_stats([]) ->
    #{avg_duration_ms => 0.0,
      min_duration_ms => 0,
      max_duration_ms => 0,
      p50_duration_ms => 0.0,
      p95_duration_ms => 0.0,
      p99_duration_ms => 0.0};
calculate_performance_stats(History) ->
    Durations = [E#command_execution.duration_ms || E <- History],
    SortedDurations = lists:sort(Durations),

    #{avg_duration_ms => lists:sum(Durations) / length(Durations),
      min_duration_ms => lists:min(Durations),
      max_duration_ms => lists:max(Durations),
      p50_duration_ms => percentile(SortedDurations, 50),
      p95_duration_ms => percentile(SortedDurations, 95),
      p99_duration_ms => percentile(SortedDurations, 99)}.

%% @private Calculate time-based statistics
-spec calculate_time_stats([#command_execution{}]) -> map().
calculate_time_stats([]) ->
    #{first_command => undefined,
      last_command => undefined,
      commands_per_hour => 0.0};
calculate_time_stats(History) ->
    Timestamps = [E#command_execution.timestamp || E <- History],
    FirstTime = lists:min(Timestamps),
    LastTime = lists:max(Timestamps),

    DurationHours = (LastTime - FirstTime) / (1000 * 3600),
    CommandsPerHour =
        case DurationHours of
            0.0 ->
                0.0;
            _ ->
                length(History) / DurationHours
        end,

    #{first_command => format_timestamp(FirstTime),
      last_command => format_timestamp(LastTime),
      commands_per_hour => CommandsPerHour}.

%% @private Get most used commands
-spec get_most_used_commands(map(), pos_integer()) -> [map()].
get_most_used_commands(Frequency, N) ->
    Sorted = lists:sort(fun({_, C1}, {_, C2}) -> C1 > C2 end, maps:to_list(Frequency)),
    TopN = lists:sublist(Sorted, N),
    [#{command => Cmd, count => Count} || {Cmd, Count} <- TopN].

%% @private Get slowest commands
-spec get_slowest_commands([#command_execution{}], pos_integer()) -> [map()].
get_slowest_commands(History, N) ->
    Sorted =
        lists:sort(fun(E1, E2) ->
                      E1#command_execution.duration_ms > E2#command_execution.duration_ms
                   end,
                   History),
    TopN = lists:sublist(Sorted, N),
    [#{command => E#command_execution.command,
       duration_ms => E#command_execution.duration_ms,
       timestamp => format_timestamp(E#command_execution.timestamp)}
     || E <- TopN].

%% @private Get recent errors
-spec get_recent_errors([#command_execution{}], pos_integer()) -> [map()].
get_recent_errors(History, N) ->
    Errors =
        lists:filter(fun(#command_execution{status = Status, error = Error}) ->
                        Status =:= error orelse Error =/= undefined
                     end,
                     History),

    Recent = lists:sublist(Errors, N),
    [#{command => E#command_execution.command,
       error => E#command_execution.error,
       timestamp => format_timestamp(E#command_execution.timestamp)}
     || E <- Recent].

%% @private Get stats for specific command
-spec get_command_stats_internal(binary(), #state{}) -> map().
get_command_stats_internal(Command, State) ->
    CommandHistory =
        lists:filter(fun(#command_execution{command = Cmd}) -> Cmd =:= Command end,
                     State#state.history),

    case CommandHistory of
        [] ->
            #{command => Command,
              execution_count => 0,
              error => no_executions};
        _ ->
            Durations = [E#command_execution.duration_ms || E <- CommandHistory],
            SortedDurations = lists:sort(Durations),

            SuccessCount =
                length(lists:filter(fun(#command_execution{status = S}) ->
                                       S =:= ok orelse S =:= success
                                    end,
                                    CommandHistory)),

            #{command => Command,
              execution_count => length(CommandHistory),
              success_count => SuccessCount,
              error_count => length(CommandHistory) - SuccessCount,
              avg_duration_ms => lists:sum(Durations) / length(Durations),
              min_duration_ms => lists:min(Durations),
              max_duration_ms => lists:max(Durations),
              p95_duration_ms => percentile(SortedDurations, 95),
              last_execution => format_timestamp((hd(CommandHistory))#command_execution.timestamp)}
    end.

%%====================================================================
%% Internal Functions - History Management
%%====================================================================

%% @private Filter history by options
-spec filter_history([#command_execution{}], map()) -> [#command_execution{}].
filter_history(History, Opts) ->
    %% Filter by command
    ByCommand =
        case maps:get(command, Opts, undefined) of
            undefined ->
                History;
            Cmd ->
                CmdBin =
                    if is_list(Cmd) ->
                           list_to_binary(Cmd);
                       is_binary(Cmd) ->
                           Cmd;
                       true ->
                           atom_to_binary(Cmd, utf8)
                    end,
                lists:filter(fun(#command_execution{command = C}) -> C =:= CmdBin end, History)
        end,

    %% Filter by status
    ByStatus =
        case maps:get(status, Opts, undefined) of
            undefined ->
                ByCommand;
            Status ->
                lists:filter(fun(#command_execution{status = S}) -> S =:= Status end, ByCommand)
        end,

    %% Limit
    Limit = maps:get(limit, Opts, length(ByStatus)),
    lists:sublist(ByStatus, Limit).

%% @private Format history for output
-spec format_history([#command_execution{}]) -> [map()].
format_history(History) ->
    [#{command => E#command_execution.command,
       args => E#command_execution.args,
       timestamp => format_timestamp(E#command_execution.timestamp),
       duration_ms => E#command_execution.duration_ms,
       status => E#command_execution.status,
       error => E#command_execution.error}
     || E <- History].

%% @private Update per-command statistics
-spec update_stats(binary(), #command_execution{}, map()) -> map().
update_stats(Command, Execution, Stats) ->
    CurrentStats =
        maps:get(Command,
                 Stats,
                 #{count => 0,
                   total_duration => 0,
                   success_count => 0,
                   error_count => 0}),

    IsSuccess =
        Execution#command_execution.status =:= ok
        orelse Execution#command_execution.status =:= success,

    NewStats =
        CurrentStats#{count => maps:get(count, CurrentStats) + 1,
                      total_duration =>
                          maps:get(total_duration, CurrentStats)
                          + Execution#command_execution.duration_ms,
                      success_count =>
                          case IsSuccess of
                              true ->
                                  maps:get(success_count, CurrentStats) + 1;
                              false ->
                                  maps:get(success_count, CurrentStats)
                          end,
                      error_count =>
                          case IsSuccess of
                              false ->
                                  maps:get(error_count, CurrentStats) + 1;
                              true ->
                                  maps:get(error_count, CurrentStats)
                          end},

    maps:put(Command, NewStats, Stats).

%%====================================================================
%% Internal Functions - Export
%%====================================================================

%% @private Export statistics to file
-spec export_stats_internal(map(), string(), atom()) -> ok | {error, term()}.
export_stats_internal(Stats, Filename, json) ->
    try
        JsonData = erlmcp_json_native:encode(Stats, [{space, 1}, {indent, 2}]),
        file:write_file(Filename, JsonData)
    catch
        Class:Error:Stack ->
            {error,
             #{class => Class,
               error => Error,
               stacktrace => Stack}}
    end.

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

%% @private Calculate percentile
-spec percentile([number()], integer()) -> number().
percentile([], _) ->
    0;
percentile(SortedValues, Percentile) ->
    N = length(SortedValues),
    Index = ceil(N * Percentile / 100),
    ClampedIndex = max(1, min(Index, N)),
    lists:nth(ClampedIndex, SortedValues).

%% @private Format timestamp
-spec format_timestamp(integer()) -> binary().
format_timestamp(Milliseconds) ->
    Seconds = Milliseconds div 1000,
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + 62167219200),
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                   [Year, Month, Day, Hour, Min, Sec])).
