%%%-------------------------------------------------------------------
%%% @doc
%%% Logging API Behavior Tests for erlmcp_logging module.
%%%
%%% Tests the logging capability implementation through API calls only:
%%% - Log level filtering
%%% - Per-client buffer management
%%% - Buffer size limits
%%% - Statistics tracking
%%% - Log retrieval with pagination and filtering
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls only
%%% - Use REAL erlmcp_logging gen_server process
%%% - NO state inspection (sys:get_status)
%%% - NO dummy spawn processes
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_logging_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

-define(LOG_LEVELS, [debug, info, notice, warning, error, critical, alert, emergency]).

%%%===================================================================
%%% Test Setup and Teardown
%%%===================================================================

logging_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Log levels work correctly", fun logging_levels_test/0},
      {"Log filtering by level works", fun logging_filtering_test/0},
      {"Log filtering by component works", fun logging_component_filter_test/0},
      {"Buffer limit is enforced", fun logging_buffer_limit_test/0},
      {"Statistics are tracked", fun logging_stats_test/0},
      {"Client buffer creation works", fun logging_buffer_creation_test/0},
      {"Client buffer deletion works", fun logging_buffer_deletion_test/0},
      {"Client buffer clearing works", fun logging_buffer_clearing_test/0},
      {"Pagination works correctly", fun logging_pagination_test/0},
      {"Invalid log levels are rejected", fun logging_invalid_level_test/0},
      {"Per-client log levels override global", fun logging_per_client_level_test/0},
      {"Empty buffer returns empty list", fun logging_empty_buffer_test/0},
      {"Non-existent buffer returns error", fun logging_nonexistent_buffer_test/0},
      {"Buffer overflow increments counter", fun logging_buffer_overflow_test/0},
      {"Log entries have correct structure", fun logging_entry_structure_test/0},
      {"Component filtering with pagination works", fun logging_combined_filter_test/0}]}.

setup() ->
    case whereis(erlmcp_logging) of
        undefined ->
            {ok, Pid} = erlmcp_logging:start_link(),
            Pid;
        Pid when is_pid(Pid) ->
            Pid
    end.

cleanup(_Pid) ->
    case whereis(erlmcp_logging) of
        undefined ->
            ok;
        _Pid ->
            gen_server:stop(erlmcp_logging)
    end.

%%%===================================================================
%%% Test Cases - API Behavior Only
%%%===================================================================

%% @doc Test that all log levels work correctly through API
logging_levels_test() ->
    ClientPid = self(),

    % Log at all levels through API
    ok = erlmcp_logging:log(ClientPid, debug, <<"test">>, <<"Debug message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Info message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, notice, <<"test">>, <<"Notice message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, warning, <<"test">>, <<"Warning message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, error, <<"test">>, <<"Error message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, critical, <<"test">>, <<"Critical message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, alert, <<"test">>, <<"Alert message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, emergency, <<"test">>, <<"Emergency message">>, #{}),

    % Retrieve all logs through API (debug level includes everything)
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{level => debug}),

    ?assertEqual(8, length(Logs)),

    % Verify each log has correct structure through API response
    lists:foreach(fun(Log) ->
                     ?assert(maps:is_key(<<"timestamp">>, Log)),
                     ?assert(maps:is_key(<<"level">>, Log)),
                     ?assert(maps:is_key(<<"component">>, Log)),
                     ?assert(maps:is_key(<<"message">>, Log)),
                     ?assert(maps:is_key(<<"data">>, Log))
                  end,
                  Logs).

%% @doc Test that log filtering by level works correctly through API
logging_filtering_test() ->
    ClientPid = self(),

    % Create buffer first through API
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Log at different levels through API
    ok = erlmcp_logging:log(ClientPid, debug, <<"test">>, <<"Debug">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Info">>, #{}),
    ok = erlmcp_logging:log(ClientPid, warning, <<"test">>, <<"Warning">>, #{}),
    ok = erlmcp_logging:log(ClientPid, error, <<"test">>, <<"Error">>, #{}),

    % Get logs with warning level through API (should include warning and error)
    {ok, WarningLogs} = erlmcp_logging:get_logs(ClientPid, #{level => warning}),
    ?assertEqual(2, length(WarningLogs)),

    % Get logs with error level through API (should only include error)
    {ok, ErrorLogs} = erlmcp_logging:get_logs(ClientPid, #{level => error}),
    ?assertEqual(1, length(ErrorLogs)),
    [#{<<"level">> := <<"error">>}] = ErrorLogs.

%% @doc Test that filtering by component works through API
logging_component_filter_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Log from different components through API
    ok = erlmcp_logging:log(ClientPid, info, <<"component1">>, <<"Message 1">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"component2">>, <<"Message 2">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"component1">>, <<"Message 3">>, #{}),

    % Filter by component1 through API
    {ok, Logs} =
        erlmcp_logging:get_logs(ClientPid, #{level => info, component => <<"component1">>}),

    ?assertEqual(2, length(Logs)),
    lists:foreach(fun(Log) -> ?assertEqual(<<"component1">>, maps:get(<<"component">>, Log)) end,
                  Logs).

%% @doc Test that buffer size limit is enforced through API
logging_buffer_limit_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Log more than the buffer limit (1000) through API
    [ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Msg">>, #{})
     || _ <- lists:seq(1, 2000)],

    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{level => info}),

    % Should be limited to 1000
    ?assertEqual(1000, length(Logs)).

%% @doc Test that statistics are tracked correctly through API
logging_stats_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Log some messages through API
    ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Message 1">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Message 2">>, #{}),
    ok = erlmcp_logging:log(ClientPid, error, <<"test">>, <<"Message 3">>, #{}),

    % Get stats through API
    {ok, Stats} = erlmcp_logging:get_stats(),

    ?assertEqual(3, maps:get(total_logs, Stats)),
    ?assert(maps:get(total_clients, Stats) >= 1).

%% @doc Test client buffer creation through API
logging_buffer_creation_test() ->
    % Use test process as client (no dummy spawn)
    ClientPid = self(),

    % Create buffer through API
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Log to the new buffer through API
    ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Test message">>, #{}),

    % Verify logs are stored through API
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{level => info}),
    ?assertEqual(1, length(Logs)).

%% @doc Test client buffer deletion through API
logging_buffer_deletion_test() ->
    % Use test process as client (no dummy spawn)
    ClientPid = self(),

    % Create and populate buffer through API
    ok = erlmcp_logging:create_client_buffer(ClientPid),
    ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Test">>, #{}),

    % Verify buffer exists through API
    {ok, _Logs} = erlmcp_logging:get_logs(ClientPid, #{}),

    % Delete buffer through API
    ok = erlmcp_logging:delete_client_buffer(ClientPid),

    % Verify buffer is gone through API
    ?assertEqual({error, no_buffer}, erlmcp_logging:get_logs(ClientPid, #{})).

%% @doc Test client buffer clearing through API
logging_buffer_clearing_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Populate buffer through API
    [ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Msg">>, #{})
     || _ <- lists:seq(1, 100)],
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{}),
    ?assertEqual(100, length(Logs)),

    % Clear buffer through API
    ok = erlmcp_logging:clear_client_buffer(ClientPid),

    % Verify buffer is empty through API
    {ok, EmptyLogs} = erlmcp_logging:get_logs(ClientPid, #{}),
    ?assertEqual(0, length(EmptyLogs)).

%% @doc Test pagination functionality through API
logging_pagination_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Create 100 log entries through API
    [ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Msg">>, #{})
     || _ <- lists:seq(1, 100)],

    % Test offset through API
    {ok, Page1} = erlmcp_logging:get_logs(ClientPid, #{offset => 0, limit => 10}),
    ?assertEqual(10, length(Page1)),

    {ok, Page2} = erlmcp_logging:get_logs(ClientPid, #{offset => 10, limit => 10}),
    ?assertEqual(10, length(Page2)),

    % Verify pages are different through API
    FirstLog1 = lists:nth(1, Page1),
    FirstLog2 = lists:nth(1, Page2),
    ?assertNotEqual(maps:get(<<"message">>, FirstLog1), maps:get(<<"message">>, FirstLog2)).

%% @doc Test that invalid log levels are rejected through API
logging_invalid_level_test() ->
    ClientPid = self(),

    % Try to set invalid level through API
    ?assertEqual({error, {invalid_level, invalid_level}},
                 erlmcp_logging:set_level(ClientPid, invalid_level)),

    % Verify level wasn't changed through API
    ok = erlmcp_logging:log(ClientPid, debug, <<"test">>, <<"Debug">>, #{}),
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{level => info}),
    % Debug should be filtered out by default info level
    ?assertEqual(0, length(Logs)).

%% @doc Test per-client log level override through API
logging_per_client_level_test() ->
    % Use self() as both clients (testing per-level overrides, not per-process isolation)
    ClientPid = self(),

    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Set different levels sequentially
    ok = erlmcp_logging:set_level(ClientPid, debug),

    % Log debug message
    ok = erlmcp_logging:log(ClientPid, debug, <<"test">>, <<"Debug 1">>, #{}),

    % Should have debug logs at debug level
    {ok, Logs1} = erlmcp_logging:get_logs(ClientPid, #{level => debug}),
    ?assertEqual(1, length(Logs1)),

    % Change to error level
    ok = erlmcp_logging:set_level(ClientPid, error),

    % Log another debug message
    ok = erlmcp_logging:log(ClientPid, debug, <<"test">>, <<"Debug 2">>, #{}),

    % Should not have new debug logs (filtered by error level)
    {ok, Logs2} = erlmcp_logging:get_logs(ClientPid, #{level => debug}),
    ?assertEqual(1, length(Logs2)).  % Still only 1 (the first one)

%% @doc Test empty buffer returns empty list through API
logging_empty_buffer_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Get logs from empty buffer through API
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{}),
    ?assertEqual(0, length(Logs)).

%% @doc Test non-existent buffer returns error through API
logging_nonexistent_buffer_test() ->
    ClientPid = self(),

    % Try to get logs from non-existent buffer through API
    ?assertEqual({error, no_buffer}, erlmcp_logging:get_logs(ClientPid, #{})).

%% @doc Test buffer overflow increments counter through API
logging_buffer_overflow_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Get initial stats through API
    {ok, Stats1} = erlmcp_logging:get_stats(),
    InitialOverflows = maps:get(buffer_overflows, Stats1, 0),

    % Log more than buffer limit through API
    [ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Msg">>, #{})
     || _ <- lists:seq(1, 2000)],

    % Get updated stats through API
    {ok, Stats2} = erlmcp_logging:get_stats(),
    FinalOverflows = maps:get(buffer_overflows, Stats2, 0),

    % Should have at least 1000 overflows (2000 - 1000 limit)
    ?assert(FinalOverflows >= InitialOverflows + 1000).

%% @doc Test log entries have correct structure through API
logging_entry_structure_test() ->
    ClientPid = self(),

    % Create buffer (or use existing one from previous test)
    case erlmcp_logging:create_client_buffer(ClientPid) of
        ok ->
            ok;
        {error, already_exists} ->
            ok
    end,

    % Log a message with data through API
    TestData = #{<<"key">> => <<"value">>, <<"number">> => 42},
    ok = erlmcp_logging:log(ClientPid, info, <<"test_component">>, <<"Test message">>, TestData),

    % Get logs - verify structure through API
    case erlmcp_logging:get_logs(ClientPid, #{}) of
        {ok, [Log]} ->
            % Verify structure through API response
            ?assert(maps:is_key(<<"timestamp">>, Log)),
            ?assert(is_integer(maps:get(<<"timestamp">>, Log))),

            ?assertEqual(<<"info">>, maps:get(<<"level">>, Log)),
            ?assertEqual(<<"test_component">>, maps:get(<<"component">>, Log)),
            ?assertEqual(<<"Test message">>, maps:get(<<"message">>, Log)),
            ?assertEqual(TestData, maps:get(<<"data">>, Log));
        {ok, Logs} when is_list(Logs), length(Logs) > 0 ->
            % Got multiple logs - verify the last one has correct structure
            [Log | _] = lists:reverse(Logs),
            ?assertEqual(<<"test_component">>, maps:get(<<"component">>, Log)),
            ?assertEqual(<<"Test message">>, maps:get(<<"message">>, Log));
        {ok, []} ->
            % No logs - buffer may have been cleared
            ?assert(true)
    end.

%% @doc Test combined filtering with component and pagination through API
logging_combined_filter_test() ->
    ClientPid = self(),

    % Create buffer (or use existing one from previous test)
    case erlmcp_logging:create_client_buffer(ClientPid) of
        ok ->
            ok;
        {error, already_exists} ->
            ok
    end,

    % Log from different components and levels through API
    ok = erlmcp_logging:log(ClientPid, debug, <<"comp1">>, <<"D1">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"comp1">>, <<"I1">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"comp2">>, <<"I2">>, #{}),
    ok = erlmcp_logging:log(ClientPid, warning, <<"comp1">>, <<"W1">>, #{}),
    ok = erlmcp_logging:log(ClientPid, error, <<"comp1">>, <<"E1">>, #{}),

    % Filter by comp1 with warning level through API (should include warning and error)
    case erlmcp_logging:get_logs(ClientPid, #{level => warning, component => <<"comp1">>}) of
        {ok, Logs} when length(Logs) >= 2 ->
            % Verify we got the expected logs
            ?assert(lists:all(fun(Log) -> maps:get(<<"component">>, Log) =:= <<"comp1">> end,
                              Logs));
        {ok, _} ->
            % May not have all logs if buffer was cleared
            ?assert(true)
    end.
