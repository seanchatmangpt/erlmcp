%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_log_rotation - OTP 28 Log Rotation and Compression
%%%
%%% This module implements daily log rotation with compression using
%%% OTP 28 features:
%%%
%%% - Daily log rotation at midnight
%%% - zstd compression for rotated logs (OTP 28 feature)
%%% - Configurable retention policy
%%% - Automatic cleanup of old logs
%%% - Async rotation to avoid blocking
%%%
%%% Configuration:
%%% - rotation_enabled: Enable/disable rotation (default: true)
%%% - rotation_schedule: Cron-style schedule (default: daily at 00:00)
%%% - compression_format: zstd | gzip | none (default: zstd)
%%% - retention_days: Days to keep logs (default: 30)
%%% - log_dir: Directory for log files (default: "log")
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_log_rotation).

-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         rotate_now/0,
         get_rotation_status/0,
         configure_rotation/1,
         cleanup_old_logs/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Type definitions
-type rotation_config() :: #{enabled => boolean(),
                            schedule => daily | hourly | weekly,
                            compression => zstd | gzip | none,
                            retention_days => pos_integer(),
                            log_dir => file:filename_all()}.
-type rotation_status() :: #{last_rotation => calendar:datetime() | undefined,
                             next_rotation => calendar:datetime() | undefined,
                             rotated_files => pos_integer(),
                             compression_enabled => boolean()}.

%% State record
-record(state,
        {config :: rotation_config(),
         last_rotation :: calendar:datetime() | undefined,
         next_rotation :: calendar:datetime() | undefined,
         rotated_count = 0 :: non_neg_integer(),
         timer_ref :: reference() | undefined}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the log rotation server with default configuration.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Start the log rotation server with custom configuration.
-spec start_link(rotation_config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Trigger an immediate log rotation.
-spec rotate_now() -> ok.
rotate_now() ->
    gen_server:cast(?MODULE, rotate_now).

%% @doc Get current rotation status.
-spec get_rotation_status() -> {ok, rotation_status()}.
get_rotation_status() ->
    gen_server:call(?MODULE, get_status).

%% @doc Update rotation configuration.
-spec configure_rotation(rotation_config()) -> ok.
configure_rotation(Config) when is_map(Config) ->
    gen_server:call(?MODULE, {configure, Config}).

%% @doc Clean up old log files based on retention policy.
-spec cleanup_old_logs() -> {ok, non_neg_integer()}.
cleanup_old_logs() ->
    gen_server:call(?MODULE, cleanup_old_logs).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

%% @private
-spec init([] | [rotation_config()]) -> {ok, #state{}}.
init([]) ->
    Config = default_config(),
    init_state(Config);
init([Config]) when is_map(Config) ->
    MergedConfig = maps:merge(default_config(), Config),
    init_state(MergedConfig).

%% @private
init_state(Config) ->
    logger:info("Starting log rotation server: ~p", [Config]),

    % Schedule first rotation
    NextRotation = calculate_next_rotation(maps:get(schedule, Config, daily)),
    TimerRef = schedule_rotation(NextRotation),

    State = #state{config = Config,
                   next_rotation = NextRotation,
                   timer_ref = TimerRef},

    {ok, State}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call(get_status, _From, #state{last_rotation = Last,
                                      next_rotation = Next,
                                      rotated_count = Count,
                                      config = Config} = State) ->
    Status = #{last_rotation => format_datetime(Last),
               next_rotation => format_datetime(Next),
               rotated_files => Count,
               compression_enabled => maps:get(compression, Config, none) =/= none},
    {reply, {ok, Status}, State};
handle_call({configure, NewConfig}, _From, State) ->
    MergedConfig = maps:merge(State#state.config, NewConfig),

    % Reschedule if rotation schedule changed
    NewState = case maps:get(schedule, NewConfig, undefined) of
        undefined ->
            State#state{config = MergedConfig};
        Schedule ->
            NextRotation = calculate_next_rotation(Schedule),
            % Cancel old timer
            case State#state.timer_ref of
                undefined -> ok;
                Ref -> erlang:cancel_timer(Ref, [{async, true}])
            end,
            TimerRef = schedule_rotation(NextRotation),
            State#state{config = MergedConfig,
                       next_rotation = NextRotation,
                       timer_ref = TimerRef}
    end,

    logger:info("Log rotation configured: ~p", [MergedConfig]),
    {reply, ok, NewState};
handle_call(cleanup_old_logs, _From, #state{config = Config} = State) ->
    RetentionDays = maps:get(retention_days, Config, 30),
    LogDir = maps:get(log_dir, Config, "log"),

    DeletedCount = cleanup_logs(LogDir, RetentionDays),

    logger:info("Cleaned up ~p old log files", [DeletedCount]),
    {reply, {ok, DeletedCount}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(rotate_now, State) ->
    NewState = perform_rotation(State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(rotation_due, #state{config = Config} = State) ->
    case maps:get(enabled, Config, true) of
        true ->
            NewState = perform_rotation(State),
            {noreply, NewState};
        false ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{timer_ref = TimerRef}) ->
    % Clean up timer
    case TimerRef of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref, [{async, true}])
    end,
    logger:info("Log rotation server terminating"),
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Get default rotation configuration.
-spec default_config() -> rotation_config().
default_config() ->
    #{enabled => true,
      schedule => daily,
      compression => zstd,
      retention_days => 30,
      log_dir => "log"}.

%% @doc Calculate the next rotation time based on schedule.
-spec calculate_next_rotation(daily | hourly | weekly) -> calendar:datetime().
calculate_next_rotation(daily) ->
    % Next midnight
    {Date, _Time} = calendar:local_time(),
    NextDay = calendar:date_to_gregorian_days(Date) + 1,
    NextDate = calendar:gregorian_days_to_date(NextDay),
    {NextDate, {0, 0, 0}};
calculate_next_rotation(hourly) ->
    % Next hour
    {Date, {Hour, _, _}} = calendar:local_time(),
    NextHour = Hour + 1,
    case NextHour >= 24 of
        true ->
            NextDay = calendar:date_to_gregorian_days(Date) + 1,
            NextDate = calendar:gregorian_days_to_date(NextDay),
            {NextDate, {0, 0, 0}};
        false ->
            {Date, {NextHour, 0, 0}}
    end;
calculate_next_rotation(weekly) ->
    % Next Monday
    {Date, _Time} = calendar:local_time(),
    DayOfWeek = calendar:day_of_the_week(Date),
    DaysToAdd = case DayOfWeek of
        1 -> 7;
        _ -> 8 - DayOfWeek
    end,
    NextDay = calendar:date_to_gregorian_days(Date) + DaysToAdd,
    NextDate = calendar:gregorian_days_to_date(NextDay),
    {NextDate, {0, 0, 0}}.

%% @doc Schedule the next rotation timer.
-spec schedule_rotation(calendar:datetime()) -> reference().
schedule_rotation(NextRotation) ->
    Now = calendar:local_time(),
    SecondsUntilRotation = calendar:datetime_to_gregorian_seconds(NextRotation) -
                          calendar:datetime_to_gregorian_seconds(Now),

    Milliseconds = max(0, SecondsUntilRotation * 1000),
    erlang:send_after(Milliseconds, self(), rotation_due).

%% @doc Perform log rotation.
-spec perform_rotation(#state{}) -> #state{}.
perform_rotation(#state{config = Config,
                       rotated_count = Count} = State) ->
    LogDir = maps:get(log_dir, Config, "log"),
    Compression = maps:get(compression, Config, none),

    logger:info("Performing log rotation in ~s", [LogDir]),

    % Find log files to rotate
    case file:list_dir(LogDir) of
        {ok, Files} ->
            LogFiles = lists:filter(fun(F) ->
                filename:extension(F) =:= ".log" orelse
                filename:extension(F) =:= ""
            end, Files),

            Rotated = lists:foldl(fun(File, Acc) ->
                case rotate_log_file(LogDir, File, Compression) of
                    {ok, _} -> Acc + 1;
                    {error, _} -> Acc
                end
            end, 0, LogFiles),

            % Update state
            Now = calendar:local_time(),
            NextRotation = calculate_next_rotation(maps:get(schedule, Config, daily)),
            TimerRef = schedule_rotation(NextRotation),

            logger:info("Rotated ~p log files, next rotation at ~p",
                       [Rotated, NextRotation]),

            State#state{last_rotation = Now,
                       next_rotation = NextRotation,
                       rotated_count = Count + Rotated,
                       timer_ref = TimerRef};
        {error, Reason} ->
            logger:error("Failed to list log directory ~s: ~p", [LogDir, Reason]),
            State
    end.

%% @doc Rotate a single log file with optional compression.
-spec rotate_log_file(file:filename_all(), file:name(), zstd | gzip | none) ->
    {ok, file:name()} | {error, term()}.
rotate_log_file(LogDir, File, Compression) ->
    FilePath = filename:join(LogDir, File),

    % Add timestamp to filename
    Timestamp = erlang:system_time(millisecond),
    BaseName = filename:rootname(File),
    Ext = filename:extension(File),

    RotatedName = case Compression of
        none -> iolist_to_binary([BaseName, ".", integer_to_binary(Timestamp), Ext]);
        zstd -> iolist_to_binary([BaseName, ".", integer_to_binary(Timestamp), Ext, ".zst"]);
        gzip -> iolist_to_binary([BaseName, ".", integer_to_binary(Timestamp), Ext, ".gz"])
    end,
    RotatedPath = filename:join(LogDir, RotatedName),

    % Move file
    case file:rename(FilePath, RotatedPath) of
        ok ->
            % Compress if requested
            case Compression of
                none ->
                    {ok, RotatedPath};
                zstd ->
                    compress_file_zstd(RotatedPath);
                gzip ->
                    compress_file_gzip(RotatedPath)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Compress file using zstd (OTP 28 feature).
-spec compress_file_zstd(file:filename_all()) -> {ok, file:filename_all()} | {error, term()}.
compress_file_zstd(FilePath) ->
    % Check if zstd is available
    case os:find_executable("zstd") of
        false ->
            logger:warning("zstd not found, skipping compression for ~s", [FilePath]),
            {ok, FilePath};
        ZstdPath ->
            CompressedPath = <<FilePath/binary, ".zst">>,
            Cmd = io_lib:format("~s -q ~s", [ZstdPath, FilePath]),
            case os:cmd(Cmd) of
                [] ->
                    % Remove original if compression succeeded
                    file:delete(FilePath),
                    {ok, CompressedPath};
                Error ->
                    logger:error("zstd compression failed: ~s", [Error]),
                    {error, Error}
            end
    end.

%% @doc Compress file using gzip (OTP 26+ built-in).
-spec compress_file_gzip(file:filename_all()) -> {ok, file:filename_all()} | {error, term()}.
compress_file_gzip(FilePath) ->
    CompressedPath = <<FilePath/binary, ".gz">>,

    case file:read_file(FilePath) of
        {ok, Data} ->
            try zlib:gzip(Data) of
                Compressed ->
                    case file:write_file(CompressedPath, Compressed) of
                        ok ->
                            file:delete(FilePath),
                            {ok, CompressedPath};
                        {error, Reason} ->
                            {error, Reason}
                    end
            catch
                _:Error ->
                    {error, Error}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Clean up old log files based on retention policy.
-spec cleanup_logs(file:filename_all(), pos_integer()) -> non_neg_integer().
cleanup_logs(LogDir, RetentionDays) ->
    CutoffSeconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()) -
                    (RetentionDays * 24 * 60 * 60),

    case file:list_dir(LogDir) of
        {ok, Files} ->
            lists:foldl(fun(File, DeletedCount) ->
                FilePath = filename:join(LogDir, File),
                case file:read_file_info(FilePath) of
                    {ok, #file_info{mtime = MTime}} ->
                        FileSeconds = calendar:datetime_to_gregorian_seconds(MTime),
                        case FileSeconds < CutoffSeconds of
                            true ->
                                case file:delete(FilePath) of
                                    ok ->
                                        logger:debug("Deleted old log: ~s", [File]),
                                        DeletedCount + 1;
                                    {error, Reason} ->
                                        logger:warning("Failed to delete ~s: ~p", [File, Reason]),
                                        DeletedCount
                                end;
                            false ->
                                DeletedCount
                        end;
                    {error, _Reason} ->
                        DeletedCount
                end
            end, 0, Files);
        {error, _Reason} ->
            0
    end.

%% @doc Format datetime for display.
-spec format_datetime(calendar:datetime() | undefined) -> binary() | undefined.
format_datetime(undefined) ->
    undefined;
format_datetime(DateTime) ->
    Date = calendar:datetime_to_gregorian_seconds(DateTime),
    Bin = integer_to_binary(Date),
    <<"datetime:", Bin/binary>>.
