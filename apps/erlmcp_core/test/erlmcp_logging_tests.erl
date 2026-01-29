%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive test suite for erlmcp_logging module.
%%%
%%% Tests the logging capability implementation including:
%%% - Log level filtering
%%% - Per-client buffer management
%%% - Buffer size limits
%%% - Statistics tracking
%%% - Log retrieval with pagination and filtering
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
     [
         {"Log levels work correctly", fun logging_levels_test/0},
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
         {"Component filtering with pagination works", fun logging_combined_filter_test/0}
     ]
    }.

setup() ->
    {ok, Pid} = erlmcp_logging:start_link(),
    Pid.

cleanup(_Pid) ->
    gen_server:stop(erlmcp_logging).

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc Test that all log levels work correctly
logging_levels_test() ->
    ClientPid = self(),

    % Log at all levels
    ok = erlmcp_logging:log(ClientPid, debug, <<"test">>, <<"Debug message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Info message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, notice, <<"test">>, <<"Notice message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, warning, <<"test">>, <<"Warning message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, error, <<"test">>, <<"Error message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, critical, <<"test">>, <<"Critical message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, alert, <<"test">>, <<"Alert message">>, #{}),
    ok = erlmcp_logging:log(ClientPid, emergency, <<"test">>, <<"Emergency message">>, #{}),

    % Retrieve all logs (debug level includes everything)
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{level => debug}),

    ?assertEqual(8, length(Logs)),

    % Verify each log has correct structure
    lists:foreach(fun(Log) ->
        ?assert(maps:is_key(<<"timestamp">>, Log)),
        ?assert(maps:is_key(<<"level">>, Log)),
        ?assert(maps:is_key(<<"component">>, Log)),
        ?assert(maps:is_key(<<"message">>, Log)),
        ?assert(maps:is_key(<<"data">>, Log))
    end, Logs).

%% @doc Test that log filtering by level works correctly
logging_filtering_test() ->
    ClientPid = self(),

    % Create buffer first
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Log at different levels
    ok = erlmcp_logging:log(ClientPid, debug, <<"test">>, <<"Debug">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Info">>, #{}),
    ok = erlmcp_logging:log(ClientPid, warning, <<"test">>, <<"Warning">>, #{}),
    ok = erlmcp_logging:log(ClientPid, error, <<"test">>, <<"Error">>, #{}),

    % Get logs with warning level (should include warning and error)
    {ok, WarningLogs} = erlmcp_logging:get_logs(ClientPid, #{level => warning}),
    ?assertEqual(2, length(WarningLogs)),

    % Get logs with error level (should only include error)
    {ok, ErrorLogs} = erlmcp_logging:get_logs(ClientPid, #{level => error}),
    ?assertEqual(1, length(ErrorLogs)),
    [#{<<"level">> := <<"error">>}] = ErrorLogs.

%% @doc Test that filtering by component works
logging_component_filter_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Log from different components
    ok = erlmcp_logging:log(ClientPid, info, <<"component1">>, <<"Message 1">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"component2">>, <<"Message 2">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"component1">>, <<"Message 3">>, #{}),

    % Filter by component1
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{
        level => info,
        component => <<"component1">>
    }),

    ?assertEqual(2, length(Logs)),
    lists:foreach(fun(Log) ->
        ?assertEqual(<<"component1">>, maps:get(<<"component">>, Log))
    end, Logs).

%% @doc Test that buffer size limit is enforced
logging_buffer_limit_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Log more than the buffer limit (1000)
    [ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Msg">>, #{}) || _ <- lists:seq(1, 2000)],

    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{level => info}),

    % Should be limited to 1000
    ?assertEqual(1000, length(Logs)).

%% @doc Test that statistics are tracked correctly
logging_stats_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Log some messages
    ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Message 1">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Message 2">>, #{}),
    ok = erlmcp_logging:log(ClientPid, error, <<"test">>, <<"Message 3">>, #{}),

    {ok, Stats} = erlmcp_logging:get_stats(),

    ?assertEqual(3, maps:get(total_logs, Stats)),
    ?assert(maps:get(total_clients, Stats) >= 1).

%% @doc Test client buffer creation
logging_buffer_creation_test() ->
    ClientPid = spawn(fun() -> receive after infinity -> ok end end),

    % Create buffer
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Log to the new buffer
    ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Test message">>, #{}),

    % Verify logs are stored
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{level => info}),
    ?assertEqual(1, length(Logs)),

    % Clean up
    exit(ClientPid, kill).

%% @doc Test client buffer deletion
logging_buffer_deletion_test() ->
    ClientPid = spawn(fun() -> receive after infinity -> ok end end),

    % Create and populate buffer
    ok = erlmcp_logging:create_client_buffer(ClientPid),
    ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Test">>, #{}),

    % Verify buffer exists
    {ok, _Logs} = erlmcp_logging:get_logs(ClientPid, #{}),

    % Delete buffer
    ok = erlmcp_logging:delete_client_buffer(ClientPid),

    % Verify buffer is gone
    ?assertEqual({error, no_buffer}, erlmcp_logging:get_logs(ClientPid, #{})),

    % Clean up
    exit(ClientPid, kill).

%% @doc Test client buffer clearing
logging_buffer_clearing_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Populate buffer
    [ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Msg">>, #{}) || _ <- lists:seq(1, 100)],
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{}),
    ?assertEqual(100, length(Logs)),

    % Clear buffer
    ok = erlmcp_logging:clear_client_buffer(ClientPid),

    % Verify buffer is empty
    {ok, EmptyLogs} = erlmcp_logging:get_logs(ClientPid, #{}),
    ?assertEqual(0, length(EmptyLogs)).

%% @doc Test pagination functionality
logging_pagination_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Create 100 log entries
    [ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Msg">>, #{}) || _ <- lists:seq(1, 100)],

    % Test offset
    {ok, Page1} = erlmcp_logging:get_logs(ClientPid, #{offset => 0, limit => 10}),
    ?assertEqual(10, length(Page1)),

    {ok, Page2} = erlmcp_logging:get_logs(ClientPid, #{offset => 10, limit => 10}),
    ?assertEqual(10, length(Page2)),

    % Verify pages are different
    FirstLog1 = lists:nth(1, Page1),
    FirstLog2 = lists:nth(1, Page2),
    ?assertNotEqual(maps:get(<<"message">>, FirstLog1), maps:get(<<"message">>, FirstLog2)).

%% @doc Test that invalid log levels are rejected
logging_invalid_level_test() ->
    ClientPid = self(),

    % Try to set invalid level
    ?assertEqual({error, {invalid_level, invalid_level}}, erlmcp_logging:set_level(ClientPid, invalid_level)),

    % Verify level wasn't changed
    ok = erlmcp_logging:log(ClientPid, debug, <<"test">>, <<"Debug">>, #{}),
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{level => info}),
    % Debug should be filtered out by default info level
    ?assertEqual(0, length(Logs)).

%% @doc Test per-client log level override
logging_per_client_level_test() ->
    ClientPid1 = self(),
    ClientPid2 = spawn(fun() -> receive after infinity -> ok end end),

    ok = erlmcp_logging:create_client_buffer(ClientPid1),
    ok = erlmcp_logging:create_client_buffer(ClientPid2),

    % Set different levels for different clients
    ok = erlmcp_logging:set_level(ClientPid1, debug),
    ok = erlmcp_logging:set_level(ClientPid2, error),

    % Log debug messages to both
    ok = erlmcp_logging:log(ClientPid1, debug, <<"test">>, <<"Debug 1">>, #{}),
    ok = erlmcp_logging:log(ClientPid2, debug, <<"test">>, <<"Debug 2">>, #{}),

    % Client 1 should have debug logs
    {ok, Logs1} = erlmcp_logging:get_logs(ClientPid1, #{level => debug}),
    ?assertEqual(1, length(Logs1)),

    % Client 2 should not have debug logs (filtered by error level)
    {ok, Logs2} = erlmcp_logging:get_logs(ClientPid2, #{level => debug}),
    ?assertEqual(0, length(Logs2)),

    % Clean up
    exit(ClientPid2, kill).

%% @doc Test empty buffer returns empty list
logging_empty_buffer_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Get logs from empty buffer
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{}),
    ?assertEqual(0, length(Logs)).

%% @doc Test non-existent buffer returns error
logging_nonexistent_buffer_test() ->
    ClientPid = self(),

    % Try to get logs from non-existent buffer
    ?assertEqual({error, no_buffer}, erlmcp_logging:get_logs(ClientPid, #{})).

%% @doc Test buffer overflow increments counter
logging_buffer_overflow_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Get initial stats
    {ok, Stats1} = erlmcp_logging:get_stats(),
    InitialOverflows = maps:get(buffer_overflows, Stats1, 0),

    % Log more than buffer limit
    [ok = erlmcp_logging:log(ClientPid, info, <<"test">>, <<"Msg">>, #{}) || _ <- lists:seq(1, 2000)],

    % Get updated stats
    {ok, Stats2} = erlmcp_logging:get_stats(),
    FinalOverflows = maps:get(buffer_overflows, Stats2, 0),

    % Should have at least 1000 overflows (2000 - 1000 limit)
    ?assert(FinalOverflows >= InitialOverflows + 1000).

%% @doc Test log entries have correct structure
logging_entry_structure_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Log a message with data
    TestData = #{<<"key">> => <<"value">>, <<"number">> => 42},
    ok = erlmcp_logging:log(ClientPid, info, <<"test_component">>, <<"Test message">>, TestData),

    {ok, [Log]} = erlmcp_logging:get_logs(ClientPid, #{}),

    % Verify structure
    ?assert(maps:is_key(<<"timestamp">>, Log)),
    ?assert(is_integer(maps:get(<<"timestamp">>, Log))),

    ?assertEqual(<<"info">>, maps:get(<<"level">>, Log)),
    ?assertEqual(<<"test_component">>, maps:get(<<"component">>, Log)),
    ?assertEqual(<<"Test message">>, maps:get(<<"message">>, Log)),
    ?assertEqual(TestData, maps:get(<<"data">>, Log)).

%% @doc Test combined filtering with component and pagination
logging_combined_filter_test() ->
    ClientPid = self(),
    ok = erlmcp_logging:create_client_buffer(ClientPid),

    % Log from different components and levels
    ok = erlmcp_logging:log(ClientPid, debug, <<"comp1">>, <<"D1">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"comp1">>, <<"I1">>, #{}),
    ok = erlmcp_logging:log(ClientPid, info, <<"comp2">>, <<"I2">>, #{}),
    ok = erlmcp_logging:log(ClientPid, warning, <<"comp1">>, <<"W1">>, #{}),
    ok = erlmcp_logging:log(ClientPid, error, <<"comp1">>, <<"E1">>, #{}),

    % Filter by comp1 with warning level (should include warning and error)
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{
        level => warning,
        component => <<"comp1">>
    }),

    ?assertEqual(2, length(Logs)),
    lists:foreach(fun(Log) ->
        ?assertEqual(<<"comp1">>, maps:get(<<"component">>, Log)),
        Level = maps:get(<<"level">>, Log),
        ?assert(Level =:= <<"warning">> orelse Level =:= <<"error">>)
    end, Logs).
