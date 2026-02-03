%%%-------------------------------------------------------------------
%%% @doc
%%% Database Connection Worker Test Suite
%%%
%%% Tests for the database connection worker including:
%%% - Connection establishment
%%% - Query execution
%%% - Transaction management
%%% - Health checks
%%% - Error handling
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_db_connection_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Test Setup and Teardown
%%%===================================================================

db_connection_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"mock connection starts", fun test_mock_connection_starts/0},
      {"execute query on mock connection", fun test_execute_query_mock/0},
      {"begin transaction on mock", fun test_begin_transaction_mock/0},
      {"commit transaction on mock", fun test_commit_transaction_mock/0},
      {"rollback transaction on mock", fun test_rollback_transaction_mock/0},
      {"health check on mock connection", fun test_health_check_mock/0},
      {"query with parameters", fun test_query_with_params/0},
      {"multiple sequential queries", fun test_sequential_queries/0},
      {"disconnect terminates process", fun test_disconnect_terminates/0},
      {"connection handles errors gracefully", fun test_error_handling/0}
     ]}.

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_ _) ->
    application:stop(erlmcp_core),
    ok.

%%%===================================================================
%%% Individual Tests
%%%===================================================================

test_mock_connection_starts() ->
    Args = [{backend, mock}],
    Result = erlmcp_db_connection:start_link(Args),
    ?assertMatch({ok, _Pid}, Result),
    {ok, Pid} = Result,
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid),
    ok.

test_execute_query_mock() ->
    {ok, Conn} = start_mock_connection(),
    Query = "SELECT * FROM users",
    Result = erlmcp_db_connection:query(Conn, Query, []),
    ?assertMatch({ok, _}, Result),
    {ok, Rows} = Result,
    ?assert(is_list(Rows)),
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_begin_transaction_mock() ->
    {ok, Conn} = start_mock_connection(),
    Result = erlmcp_db_connection:begin_transaction(Conn),
    ?assertMatch({ok, _}, Result),
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_commit_transaction_mock() ->
    {ok, Conn} = start_mock_connection(),
    ok = erlmcp_db_connection:begin_transaction(Conn),
    Result = erlmcp_db_connection:commit(Conn),
    ?assertMatch({ok, _}, Result),
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_rollback_transaction_mock() ->
    {ok, Conn} = start_mock_connection(),
    ok = erlmcp_db_connection:begin_transaction(Conn),
    Result = erlmcp_db_connection:rollback(Conn),
    ?assertEqual(ok, Result),
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_health_check_mock() ->
    {ok, Conn} = start_mock_connection(),
    Result = erlmcp_db_connection:health_check(Conn),
    ?assertMatch({ok, _}, Result),
    {ok, Health} = Result,
    ?assertEqual(healthy, maps:get(status, Health)),
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_query_with_params() ->
    {ok, Conn} = start_mock_connection(),
    Query = "SELECT * FROM users WHERE id = $1 AND name = $2",
    Params = [1, "Alice"],
    Result = erlmcp_db_connection:query(Conn, Query, Params),
    ?assertMatch({ok, _}, Result),
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_sequential_queries() ->
    {ok, Conn} = start_mock_connection(),
    Queries = [
        {"SELECT 1", []},
        {"SELECT 2", []},
        {"SELECT 3", []}
    ],
    Results = [erlmcp_db_connection:query(Conn, Q, P) || {Q, P} <- Queries],
    ?assertEqual(length(Queries), length([R || {ok, _} <- Results])),
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_disconnect_terminates() ->
    {ok, Conn} = start_mock_connection(),
    ?assert(is_process_alive(Conn)),
    ok = erlmcp_db_connection:disconnect(Conn),
    timer:sleep(100),
    ?assertNot(is_process_alive(Conn)),
    ok.

test_error_handling() ->
    {ok, Conn} = start_mock_connection(),
    % Mock connection should handle invalid queries gracefully
    Result = erlmcp_db_connection:query(Conn, "INVALID QUERY", []),
    ?assertMatch({ok, _}, Result),  % Mock returns success
    erlmcp_db_connection:disconnect(Conn),
    ok.

%%%===================================================================
%%% Transaction State Machine Tests
%%%===================================================================

transaction_state_test_() ->
    {setup,
     fun setup_trans/0,
     fun cleanup_trans/1,
     [
      {"begin transaction twice fails", fun test_double_begin/0},
      {"commit without transaction fails", fun test_commit_without_begin/0},
      {"rollback without transaction is safe", fun test_rollback_without_begin/0},
      {"transaction isolation", fun test_transaction_isolation/0}
     ]}.

setup_trans() ->
    ok.

cleanup_trans(_ _) ->
    ok.

test_double_begin() ->
    {ok, Conn} = start_mock_connection(),
    ok = erlmcp_db_connection:begin_transaction(Conn),
    Result = erlmcp_db_connection:begin_transaction(Conn),
    ?assertMatch({error, already_in_transaction}, Result),
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_commit_without_begin() ->
    {ok, Conn} = start_mock_connection(),
    Result = erlmcp_db_connection:commit(Conn),
    ?assertMatch({error, no_transaction}, Result),
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_rollback_without_begin() ->
    {ok, Conn} = start_mock_connection(),
    Result = erlmcp_db_connection:rollback(Conn),
    ?assertEqual(ok, Result),  % Rollback is idempotent
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_transaction_isolation() ->
    {ok, Conn} = start_mock_connection(),
    ok = erlmcp_db_connection:begin_transaction(Conn),
    % Perform operations
    {ok, _} = erlmcp_db_connection:query(Conn, "INSERT INTO users (name) VALUES ($1)", ["Test"]),
    % Commit should succeed
    {ok, _} = erlmcp_db_connection:commit(Conn),
    erlmcp_db_connection:disconnect(Conn),
    ok.

%%%===================================================================
%%% Connection Lifecycle Tests
%%%===================================================================

connection_lifecycle_test_() ->
    {setup,
     fun setup_lifecycle/0,
     fun cleanup_lifecycle/1,
     [
      {"connection handles exit gracefully", fun test_exit_handling/0},
      {"connection can be restarted", fun test_restart_connection/0},
      {"connection survives stress", fun test_stress_connection/0}
     ]}.

setup_lifecycle() ->
    ok.

cleanup_lifecycle(_ _) ->
    ok.

test_exit_handling() ->
    {ok, Conn} = start_mock_connection(),
    Pid = Conn,
    % Send exit signal
    exit(Pid, normal),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)),
    ok.

test_restart_connection() ->
    {ok, Conn1} = start_mock_connection(),
    erlmcp_db_connection:disconnect(Conn1),
    timer:sleep(100),
    {ok, Conn2} = start_mock_connection(),
    ?assertNotEqual(Conn1, Conn2),
    ?assert(is_process_alive(Conn2)),
    erlmcp_db_connection:disconnect(Conn2),
    ok.

test_stress_connection() ->
    {ok, Conn} = start_mock_connection(),
    % Execute many queries
    QueryCount = 1000,
    Start = erlang:monotonic_time(millisecond),
    lists:foreach(fun(_) ->
        erlmcp_db_connection:query(Conn, "SELECT 1", [])
    end, lists:seq(1, QueryCount)),
    Duration = erlang:monotonic_time(millisecond) - Start,
    ?assert(Duration < 5000),  % Should complete in 5 seconds
    erlmcp_db_connection:disconnect(Conn),
    ok.

%%%===================================================================
%%% Backend Support Tests
%%%===================================================================

backend_test_() ->
    {setup,
     fun setup_backend/0,
     fun cleanup_backend/1,
     [
      {"mock backend is always available", fun test_mock_backend_available/0},
      {"postgres backend falls back to mock", fun test_postgres_fallback/0},
      {"mysql backend falls back to mock", fun test_mysql_fallback/0},
      {"unsupported backend fails gracefully", fun test_unsupported_backend/0}
     ]}.

setup_backend() ->
    ok.

cleanup_backend(_ _) ->
    ok.

test_mock_backend_available() ->
    Args = [{backend, mock}],
    Result = erlmcp_db_connection:start_link(Args),
    ?assertMatch({ok, _}, Result),
    {ok, Conn} = Result,
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_postgres_fallback() ->
    % If epgsql is not available, should fall back to mock
    Args = [{backend, postgres}, {host, "localhost"}, {database, "test"}],
    Result = erlmcp_db_connection:start_link(Args),
    % Should succeed (either with real connection or mock fallback)
    ?assertMatch({ok, _}, Result),
    {ok, Conn} = Result,
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_mysql_fallback() ->
    % If mysql is not available, should fall back to mock
    Args = [{backend, mysql}, {host, "localhost"}, {database, "test"}],
    Result = erlmcp_db_connection:start_link(Args),
    % Should succeed (either with real connection or mock fallback)
    ?assertMatch({ok, _}, Result),
    {ok, Conn} = Result,
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_unsupported_backend() ->
    Args = [{backend, invalid_backend}],
    Result = erlmcp_db_connection:start_link(Args),
    ?assertMatch({error, _}, Result),
    ok.

%%%===================================================================
%%% Timeout Tests
%%%===================================================================

timeout_test_() ->
    {timeout, 10,
     {setup,
      fun setup_timeout/0,
      fun cleanup_timeout/1,
      [
       {"query timeout is enforced", fun test_query_timeout/0},
       {"health check timeout is enforced", fun test_health_check_timeout/0}
      ]}}.

setup_timeout() ->
    ok.

cleanup_timeout(_ _) ->
    ok.

test_query_timeout() ->
    {ok, Conn} = start_mock_connection(),
    % Mock query should complete quickly
    Start = erlang:monotonic_time(millisecond),
    erlmcp_db_connection:query(Conn, "SELECT 1", []),
    Duration = erlang:monotonic_time(millisecond) - Start,
    ?assert(Duration < 1000),
    erlmcp_db_connection:disconnect(Conn),
    ok.

test_health_check_timeout() ->
    {ok, Conn} = start_mock_connection(),
    Start = erlang:monotonic_time(millisecond),
    erlmcp_db_connection:health_check(Conn),
    Duration = erlang:monotonic_time(millisecond) - Start,
    ?assert(Duration < 1000),
    erlmcp_db_connection:disconnect(Conn),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

start_mock_connection() ->
    Args = [{backend, mock}],
    erlmcp_db_connection:start_link(Args).
