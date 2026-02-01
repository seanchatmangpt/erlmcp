%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_audit_log following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through gen_server API
%%% - Use REAL gen_server processes (no mocks)
%%% - Test hash chain integrity through verification APIs
%%% - Test with real file I/O
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_audit_log_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

audit_log_test_() ->
    {setup,
     fun setup_audit_log/0,
     fun cleanup_audit_log/1,
     [
         {"Start audit log server", fun test_start_link/0},
         {"Log authentication success", fun test_log_auth_success/0},
         {"Log authentication failure", fun test_log_auth_failure/0},
         {"Log operation", fun test_log_operation/0},
         {"Log permission check", fun test_log_permission_check/0},
         {"Log sensitive operation", fun test_log_sensitive_operation/0},
         {"Verify hash chain integrity", fun test_verify_chain/0},
         {"Verify hash chain range", fun test_verify_chain_range/0},
         {"Export logs to JSON", fun test_export_json/0},
         {"Export logs to CSV", fun test_export_csv/0},
         {"Get user logs", fun test_get_user_logs/0},
         {"Search logs with query", fun test_search_logs/0}
     ]}.

setup_audit_log() ->
    % Create unique temp directory for test logs
    TempDir = "/tmp/erlmcp_audit_test_" ++ integer_to_list(erlang:system_time()),
    LogPath = TempDir ++ "/audit.log",

    Config = #{
        log_path => LogPath,
        buffer_size => 10,
        flush_interval_ms => 1000
    },

    #{config => Config, log_path => LogPath, temp_dir => TempDir}.

cleanup_audit_log(#{temp_dir := TempDir}) ->
    % Stop server if running
    case whereis(erlmcp_audit_log) of
        undefined -> ok;
        _Pid -> erlmcp_audit_log:stop()
    end,

    % Clean up test files
    os:cmd("rm -rf " ++ TempDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% Test starting audit log server
test_start_link() ->
    Config = #{
        log_path => "/tmp/erlmcp_audit_test_start/audit.log",
        buffer_size => 10,
        flush_interval_ms => 1000
    },

    {ok, Pid} = erlmcp_audit_log:start_link(Config),

    % Verify server is running
    ?assert(is_process_alive(Pid)),
    ?assertEqual(Pid, whereis(erlmcp_audit_log)),

    % Clean up
    erlmcp_audit_log:stop().

%% Test logging authentication success
test_log_auth_success() ->
    Config = #{log_path => "/tmp/erlmcp_audit_test_auth_success/audit.log"},
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Log successful authentication
    ok = erlmcp_audit_log:log_auth_success(
        <<"user123">>,
        #{method => <<"password">>, ip => <<"127.0.0.1">>}
    ),

    % Wait for flush
    timer:sleep(100),

    % Verify chain is intact
    ?assertEqual(ok, erlmcp_audit_log:verify_chain()),

    erlmcp_audit_log:stop().

%% Test logging authentication failure
test_log_auth_failure() ->
    Config = #{log_path => "/tmp/erlmcp_audit_test_auth_failure/audit.log"},
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Log failed authentication
    ok = erlmcp_audit_log:log_auth_failure(
        <<"attacker">>,
        #{reason => <<"invalid_password">>, ip => <<"192.168.1.100">>}
    ),

    timer:sleep(100),

    ?assertEqual(ok, erlmcp_audit_log:verify_chain()),

    erlmcp_audit_log:stop().

%% Test logging general operation
test_log_operation() ->
    Config = #{log_path => "/tmp/erlmcp_audit_test_operation/audit.log"},
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Log operation
    ok = erlmcp_audit_log:log_operation(
        <<"user456">>,
        <<"resource:document:123">>,
        <<"read">>,
        #{size => 4096, duration_ms => 15}
    ),

    timer:sleep(100),

    ?assertEqual(ok, erlmcp_audit_log:verify_chain()),

    erlmcp_audit_log:stop().

%% Test logging permission check
test_log_permission_check() ->
    Config = #{log_path => "/tmp/erlmcp_audit_test_permission/audit.log"},
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Log successful permission check
    ok = erlmcp_audit_log:log_permission_check(
        <<"user789">>,
        <<"resource:api:endpoint">>,
        <<"execute">>,
        ok
    ),

    % Log denied permission check
    ok = erlmcp_audit_log:log_permission_check(
        <<"user789">>,
        <<"resource:admin">>,
        <<"access">>,
        {error, forbidden}
    ),

    timer:sleep(100),

    ?assertEqual(ok, erlmcp_audit_log:verify_chain()),

    erlmcp_audit_log:stop().

%% Test logging sensitive operation
test_log_sensitive_operation() ->
    Config = #{log_path => "/tmp/erlmcp_audit_test_sensitive/audit.log"},
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Log sensitive operation (PII access)
    ok = erlmcp_audit_log:log_sensitive_operation(
        <<"admin_user">>,
        <<"access_pii">>,
        #{record_id => <<"patient_123">>, fields => [<<"ssn">>, <<"address">>]}
    ),

    timer:sleep(100),

    ?assertEqual(ok, erlmcp_audit_log:verify_chain()),

    erlmcp_audit_log:stop().

%% Test hash chain verification
test_verify_chain() ->
    Config = #{log_path => "/tmp/erlmcp_audit_test_verify/audit.log"},
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Log multiple entries
    lists:foreach(fun(N) ->
        erlmcp_audit_log:log_operation(
            <<"user">>,
            list_to_binary(["resource:", integer_to_list(N)]),
            <<"read">>,
            #{index => N}
        )
    end, lists:seq(1, 20)),

    % Force flush
    timer:sleep(200),

    % Verify entire chain
    ?assertEqual(ok, erlmcp_audit_log:verify_chain()),

    erlmcp_audit_log:stop().

%% Test hash chain range verification
test_verify_chain_range() ->
    Config = #{log_path => "/tmp/erlmcp_audit_test_verify_range/audit.log"},
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Log entries
    lists:foreach(fun(N) ->
        erlmcp_audit_log:log_operation(
            <<"user">>,
            <<"resource">>,
            <<"action">>,
            #{seq => N}
        )
    end, lists:seq(1, 10)),

    timer:sleep(200),

    % Verify range [0, 5]
    ?assertEqual(ok, erlmcp_audit_log:verify_chain(0, 5)),

    % Verify range [3, 7]
    ?assertEqual(ok, erlmcp_audit_log:verify_chain(3, 7)),

    erlmcp_audit_log:stop().

%% Test exporting logs to JSON
test_export_json() ->
    Config = #{log_path => "/tmp/erlmcp_audit_test_export_json/audit.log"},
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Log some entries
    erlmcp_audit_log:log_auth_success(<<"user1">>, #{}),
    erlmcp_audit_log:log_operation(<<"user1">>, <<"resource">>, <<"read">>, #{}),

    timer:sleep(200),

    % Export to JSON
    ExportPath = "/tmp/erlmcp_audit_test_export_json/export.json",
    ?assertEqual(ok, erlmcp_audit_log:export_logs(json, ExportPath)),

    % Verify file exists
    ?assert(filelib:is_file(ExportPath)),

    erlmcp_audit_log:stop().

%% Test exporting logs to CSV
test_export_csv() ->
    Config = #{log_path => "/tmp/erlmcp_audit_test_export_csv/audit.log"},
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Log entries
    erlmcp_audit_log:log_operation(<<"user2">>, <<"doc">>, <<"write">>, #{}),

    timer:sleep(200),

    % Export to CSV
    ExportPath = "/tmp/erlmcp_audit_test_export_csv/export.csv",
    ?assertEqual(ok, erlmcp_audit_log:export_logs(csv, ExportPath)),

    ?assert(filelib:is_file(ExportPath)),

    erlmcp_audit_log:stop().

%% Test getting user logs
test_get_user_logs() ->
    Config = #{log_path => "/tmp/erlmcp_audit_test_user_logs/audit.log"},
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    StartTime = erlang:system_time(microsecond),

    % Log operations for different users
    erlmcp_audit_log:log_operation(<<"alice">>, <<"res1">>, <<"read">>, #{}),
    erlmcp_audit_log:log_operation(<<"bob">>, <<"res2">>, <<"write">>, #{}),
    erlmcp_audit_log:log_operation(<<"alice">>, <<"res3">>, <<"delete">>, #{}),

    timer:sleep(200),

    EndTime = erlang:system_time(microsecond),

    % Get logs for alice
    {ok, AliceLogs} = erlmcp_audit_log:get_user_logs(<<"alice">>, {StartTime, EndTime}),

    % Should have 2 entries for alice
    ?assertEqual(2, length(AliceLogs)),

    erlmcp_audit_log:stop().

%% Test searching logs
test_search_logs() ->
    Config = #{log_path => "/tmp/erlmcp_audit_test_search/audit.log"},
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Log various operations
    erlmcp_audit_log:log_operation(<<"user1">>, <<"secret">>, <<"read">>, #{}),
    erlmcp_audit_log:log_operation(<<"user2">>, <<"public">>, <<"read">>, #{}),
    erlmcp_audit_log:log_operation(<<"user1">>, <<"secret">>, <<"write">>, #{}),

    timer:sleep(200),

    % Search for user1 operations on secret resource
    {ok, Results} = erlmcp_audit_log:search_logs(#{
        <<"user_id">> => <<"user1">>,
        <<"resource">> => <<"secret">>
    }),

    % Should find 2 matches
    ?assertEqual(2, length(Results)),

    erlmcp_audit_log:stop().

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% Test empty audit log
empty_log_verification_test() ->
    Config = #{log_path => "/tmp/erlmcp_audit_test_empty/audit.log"},
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Verify empty log (should be ok)
    ?assertEqual(ok, erlmcp_audit_log:verify_chain()),

    erlmcp_audit_log:stop().

%% Test high volume logging
high_volume_test() ->
    Config = #{
        log_path => "/tmp/erlmcp_audit_test_volume/audit.log",
        buffer_size => 100
    },
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Log 1000 entries
    lists:foreach(fun(N) ->
        erlmcp_audit_log:log_operation(
            <<"user">>,
            <<"resource">>,
            <<"action">>,
            #{index => N}
        )
    end, lists:seq(1, 1000)),

    timer:sleep(500),

    % Verify chain integrity
    ?assertEqual(ok, erlmcp_audit_log:verify_chain()),

    erlmcp_audit_log:stop().

%% Test concurrent logging
concurrent_logging_test() ->
    Config = #{log_path => "/tmp/erlmcp_audit_test_concurrent/audit.log"},
    {ok, _Pid} = erlmcp_audit_log:start_link(Config),

    % Spawn multiple processes logging concurrently
    Pids = [spawn(fun() ->
        lists:foreach(fun(N) ->
            erlmcp_audit_log:log_operation(
                <<"user">>,
                <<"resource">>,
                <<"action">>,
                #{worker => self(), n => N}
            )
        end, lists:seq(1, 10))
    end) || _ <- lists:seq(1, 10)],

    % Wait for all workers
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        receive
            {'DOWN', Ref, process, Pid, _} -> ok
        after 5000 -> timeout
        end
    end, Pids),

    timer:sleep(500),

    % Verify chain is still intact
    ?assertEqual(ok, erlmcp_audit_log:verify_chain()),

    erlmcp_audit_log:stop().
