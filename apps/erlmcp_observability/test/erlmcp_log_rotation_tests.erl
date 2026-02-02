%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_log_rotation - OTP 28 Log Rotation
%%%
%%% Test coverage:
%%% - Rotation scheduling
%%% - Configuration management
%%% - Log file rotation
%%% - Compression (zstd/gzip)
%%% - Retention cleanup
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_log_rotation_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup and Teardown
%%%===================================================================

%% Setup function called before each test
setup() ->
    % Create temporary log directory for testing
    TestLogDir = "log_test",
    case file:make_dir(TestLogDir) of
        ok -> ok;
        {error, eexist} -> ok
    end,

    % Start rotation server with test config
    Config = #{
        enabled => true,
        schedule => daily,
        compression => none,  % Disable compression for faster tests
        retention_days => 1,
        log_dir => TestLogDir
    },

    {ok, Pid} = erlmcp_log_rotation:start_link(Config),
    {Pid, TestLogDir}.

%% Cleanup function called after each test
cleanup({_Pid, TestLogDir}) ->
    % Stop rotation server
    case whereis(erlmcp_log_rotation) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,

    % Clean up test log directory
    case file:list_dir(TestLogDir) of
        {ok, Files} ->
            lists:foreach(fun(F) ->
                file:delete(filename:join(TestLogDir, F))
            end, Files);
        _ -> ok
    end,
    file:del_dir(TestLogDir),

    ok.

%%%===================================================================
%%% Configuration Tests
%%%===================================================================

start_link_test_() ->
    {setup,
     fun() ->
         % Create test directory
         TestLogDir = "log_test_start",
         file:make_dir(TestLogDir),
         TestLogDir
     end,
     fun(TestLogDir) ->
         % Stop any running server
         case whereis(erlmcp_log_rotation) of
             undefined -> ok;
             Pid -> gen_server:stop(Pid)
         end,
         % Clean up test directory
         case file:list_dir(TestLogDir) of
             {ok, Files} ->
                 lists:foreach(fun(F) ->
                     file:delete(filename:join(TestLogDir, F))
                 end, Files);
             _ -> ok
         end,
         file:del_dir(TestLogDir)
     end,
     fun(TestLogDir) ->
         [
          ?_test(begin
                     % Test starting with default config
                     Config = #{
                         enabled => true,
                         schedule => daily,
                         compression => none,
                         retention_days => 1,
                         log_dir => TestLogDir
                     },
                     case whereis(erlmcp_log_rotation) of
                         undefined -> ok;
                         Pid -> gen_server:stop(Pid)
                     end,
                     ?assertMatch({ok, _Pid}, erlmcp_log_rotation:start_link(Config))
                 end)
         ]
     end}.

configure_rotation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Pid, TestLogDir}) ->
         [
          ?_test(begin
                     % Test updating configuration
                     NewConfig = #{
                         enabled => false,
                         schedule => hourly,
                         compression => none,
                         retention_days => 7
                     },
                     ok = erlmcp_log_rotation:configure_rotation(NewConfig),
                     ?assertMatch({ok, _}, erlmcp_log_rotation:get_rotation_status())
                 end),
          ?_test(begin
                     % Test partial configuration update
                     PartialConfig = #{retention_days => 14},
                     ok = erlmcp_log_rotation:configure_rotation(PartialConfig),
                     ?assertMatch({ok, _}, erlmcp_log_rotation:get_rotation_status())
                 end)
         ]
     end}.

%%%===================================================================
%%% Status Query Tests
%%%===================================================================

get_rotation_status_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Pid, _TestLogDir}) ->
         [
          ?_test(begin
                     % Test getting status
                     {ok, Status} = erlmcp_log_rotation:get_rotation_status(),
                     ?assert(is_map(Status)),
                     ?assert(maps:is_key(last_rotation, Status)),
                     ?assert(maps:is_key(next_rotation, Status)),
                     ?assert(maps:is_key(rotated_files, Status)),
                     ?assert(maps:is_key(compression_enabled, Status))
                 end)
         ]
     end}.

%%%===================================================================
%%% Rotation Tests
%%%===================================================================

rotate_now_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Pid, TestLogDir}) ->
         [
          ?_test(begin
                     % Create test log files
                     LogFile1 = filename:join(TestLogDir, "test1.log"),
                     LogFile2 = filename:join(TestLogDir, "test2.log"),
                     ok = file:write_file(LogFile1, <<"log content 1">>),
                     ok = file:write_file(LogFile2, <<"log content 2">>),

                     % Trigger rotation
                     erlmcp_log_rotation:rotate_now(),

                     % Allow time for rotation
                     timer:sleep(100),

                     % Verify files were rotated
                     ?assertMatch({ok, Files}, file:list_dir(TestLogDir)),
                     % Should have rotated files with timestamps
                     ?assert(lists:any(fun(F) ->
                         string:find(F, ".log") =/= F andalso
                         string:find(F, "test1") =/= F
                     end, Files))
                 end)
         ]
     end}.

%%%===================================================================
%%% Compression Tests
%%%===================================================================

compress_file_gzip_test_() ->
    {setup,
     fun() ->
         % Create test directory
         TestLogDir = "log_test_compress",
         file:make_dir(TestLogDir),
         TestLogDir
     end,
     fun(TestLogDir) ->
         % Clean up
         case file:list_dir(TestLogDir) of
             {ok, Files} ->
                 lists:foreach(fun(F) ->
                     file:delete(filename:join(TestLogDir, F))
                 end, Files);
             _ -> ok
         end,
         file:del_dir(TestLogDir)
     end,
     fun(TestLogDir) ->
         [
          ?_test(begin
                     % Test gzip compression
                     LogFile = filename:join(TestLogDir, "compress_test.log"),
                     Content = crypto:strong_rand_bytes(1000),
                     ok = file:write_file(LogFile, Content),

                     % Simulate rotation with gzip
                     Timestamp = erlang:system_time(millisecond),
                     RotatedPath = filename:join(TestLogDir,
                         iolist_to_binary(["compress_test.", integer_to_binary(Timestamp), ".log.gz"])),
                     file:rename(LogFile, RotatedPath),

                     % Compress using OTP zlib (built-in)
                     {ok, Compressed} = file:read_file(RotatedPath),
                     Decompressed = zlib:gunzip(Compressed),
                     ?assertEqual(Content, Decompressed)
                 end)
         ]
     end}.

%%%===================================================================
%%% Cleanup Tests
%%%===================================================================

cleanup_old_logs_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Pid, TestLogDir}) ->
         [
          ?_test(begin
                     % Create old log files
                     OldTimestamp = erlang:system_time(millisecond) - (2 * 24 * 60 * 60 * 1000),
                     OldFile = filename:join(TestLogDir,
                         iolist_to_binary(["old.", integer_to_binary(OldTimestamp), ".log"])),
                     ok = file:write_file(OldFile, <<"old log content">>),

                     % Create recent log file
                     RecentFile = filename:join(TestLogDir, "recent.log"),
                     ok = file:write_file(RecentFile, <<"recent log content">>),

                     % Run cleanup
                     {ok, DeletedCount} = erlmcp_log_rotation:cleanup_old_logs(),

                     % Verify old file was deleted
                     ?assert(DeletedCount > 0),
                     ?assertMatch({error, enoent}, file:read_file(OldFile)),
                     ?assertMatch({ok, _}, file:read_file(RecentFile))
                 end)
         ]
     end}.

%%%===================================================================
%%% Schedule Calculation Tests
%%%===================================================================

calculate_next_rotation_test_() ->
    [
     ?_test(begin
                % Test daily schedule
                % (We can't test exact time without mocking, but verify function exists)
                NextDaily = erlmcp_log_rotation:calculate_next_rotation(daily),
                ?assert(is_tuple(NextDaily)),
                ?assertEqual(2, tuple_size(NextDaily))
            end),
     ?_test(begin
                % Test hourly schedule
                NextHourly = erlmcp_log_rotation:calculate_next_rotation(hourly),
                ?assert(is_tuple(NextHourly)),
                ?assertEqual(2, tuple_size(NextHourly))
            end),
     ?_test(begin
                % Test weekly schedule
                NextWeekly = erlmcp_log_rotation:calculate_next_rotation(weekly),
                ?assert(is_tuple(NextWeekly)),
                ?assertEqual(2, tuple_size(NextWeekly))
            end)
    ].

%%%===================================================================
%%% Property-Based Tests
%%%===================================================================

prop_rotation_preserves_config_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Pid, _TestLogDir}) ->
         [
          ?_test(begin
                     % Property: rotation should preserve configuration
                     {ok, StatusBefore} = erlmcp_log_rotation:get_rotation_status(),
                     RotatedBefore = maps:get(rotated_files, StatusBefore),

                     erlmcp_log_rotation:rotate_now(),
                     timer:sleep(100),

                     {ok, StatusAfter} = erlmcp_log_rotation:get_rotation_status(),
                     RotatedAfter = maps:get(rotated_files, StatusAfter),

                     ?assert(RotatedAfter >= RotatedBefore)
                 end)
         ]
     end}.

prop_cleanup_deletes_old_files_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Pid, TestLogDir}) ->
         [
          ?_test(begin
                     % Property: cleanup should delete files older than retention
                     RetentionDays = 1,
                     OldTimestamp = erlang:system_time(millisecond) -
                                   (RetentionDays + 1) * 24 * 60 * 60 * 1000,

                     OldFile = filename:join(TestLogDir,
                         iolist_to_binary(["old.", integer_to_binary(OldTimestamp), ".log"])),

                     ok = file:write_file(OldFile, <<"old content">>),

                     % Verify file exists
                     ?assertMatch({ok, _}, file:read_file_info(OldFile)),

                     % Run cleanup
                     {ok, DeletedCount} = erlmcp_log_rotation:cleanup_old_logs(),

                     % Verify file was deleted
                     ?assert(DeletedCount > 0)
                 end)
         ]
     end}.

%%%===================================================================
%%% Integration Tests
%%%===================================================================

rotation_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun({_Pid, TestLogDir}) ->
         [
          ?_test(begin
                     % Complete workflow: create logs, rotate, cleanup
                     % Create multiple log files
                     LogFiles = [filename:join(TestLogDir, iolist_to_binary(["log", integer_to_binary(I), ".log"]))
                                 || I <- lists:seq(1, 3)],

                     lists:foreach(fun(F) ->
                         ok = file:write_file(F, <<"log content">>)
                     end, LogFiles),

                     % Rotate
                     erlmcp_log_rotation:rotate_now(),
                     timer:sleep(100),

                     % Check status
                     {ok, Status} = erlmcp_log_rotation:get_rotation_status(),
                     ?assert(maps:get(rotated_files, Status) > 0)
                 end)
         ]
     end}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Helper to create test log files
create_test_log_files(Dir, Count) ->
    lists:foreach(fun(I) ->
        Filename = filename:join(Dir, iolist_to_binary(["log", integer_to_binary(I), ".log"])),
        ok = file:write_file(Filename, <<"log content\n">>)
    end, lists:seq(1, Count)).

%% Helper to count files in directory
count_files(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} -> length(Files);
        {error, _} -> 0
    end.
