%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Diagnostics Test Suite (EUnit)
%%%
%%% Tests for erlmcp_cli_diagnostics module - System diagnostics and health checks
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real diagnostic checks
%%% - NO mocks, real system inspection
%%% - State-based verification (diagnostic results, health status)
%%%
%%% Coverage Target: ≥85%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_diagnostics_tests).
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

diagnostics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Initialize diagnostics", fun test_init_diagnostics/0},
      {"Check Erlang version", fun test_check_erlang_version/0},
      {"Check OTP version", fun test_check_otp_version/0},
      {"Check system memory", fun test_check_memory/0},
      {"Check CPU usage", fun test_check_cpu/0},
      {"Check disk space", fun test_check_disk/0},
      {"Check process count", fun test_check_processes/0},
      {"Check port count", fun test_check_ports/0},
      {"Check ETS tables", fun test_check_ets/0},
      {"Check dependencies", fun test_check_dependencies/0},
      {"Check application status", fun test_check_applications/0},
      {"Check supervisor trees", fun test_check_supervisors/0},
      {"Check network connectivity", fun test_check_network/0},
      {"Health check summary", fun test_health_summary/0},
      {"Performance metrics", fun test_performance_metrics/0}
     ]}.

setup() ->
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_Args) ->
    ok.

%%%====================================================================
%%% Initialization Tests
%%%====================================================================

test_init_diagnostics() ->
    {ok, Diag} = erlmcp_cli_diagnostics:start_link(),
    ?assert(is_pid(Diag)),
    ok = erlmcp_cli_diagnostics:stop(Diag).

%%%====================================================================
%%% Version Check Tests
%%%====================================================================

test_check_erlang_version() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_erlang_version(),
    ?assertMatch(#{status := pass, version := _}, Result),
    {ok, Version} = maps:find(version, Result),
    ?assert(is_binary(Version)).

test_check_otp_version() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_otp_version(),
    ?assertMatch(#{status := _, version := _}, Result),
    {ok, Version} = maps:find(version, Result),
    ?assert(is_integer(Version) orelse is_binary(Version)).

%%%====================================================================
%%% System Resource Tests
%%%====================================================================

test_check_memory() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_memory(),
    ?assertMatch(#{total := _, used := _, free := _, status := _}, Result),
    {ok, Total} = maps:find(total, Result),
    {ok, Used} = maps:find(used, Result),
    ?assert(is_integer(Total)),
    ?assert(is_integer(Used)),
    ?assert(Used =< Total).

test_check_cpu() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_cpu(),
    ?assertMatch(#{usage_percent := _, cores := _, status := _}, Result),
    {ok, Usage} = maps:find(usage_percent, Result),
    ?assert(is_number(Usage)),
    ?assert(Usage >= 0),
    ?assert(Usage =< 100).

test_check_disk() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_disk(),
    ?assertMatch(#{total := _, used := _, free := _, status := _}, Result),
    {ok, Total} = maps:find(total, Result),
    {ok, Free} = maps:find(free, Result),
    ?assert(is_integer(Total)),
    ?assert(is_integer(Free)),
    ?assert(Free =< Total).

%%%====================================================================
%%% Process and Port Tests
%%%====================================================================

test_check_processes() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_processes(),
    ?assertMatch(#{count := _, limit := _, status := _}, Result),
    {ok, Count} = maps:find(count, Result),
    {ok, Limit} = maps:find(limit, Result),
    ?assert(is_integer(Count)),
    ?assert(is_integer(Limit)),
    ?assert(Count =< Limit).

test_check_ports() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_ports(),
    ?assertMatch(#{count := _, limit := _, status := _}, Result),
    {ok, Count} = maps:find(count, Result),
    ?assert(is_integer(Count)).

%%%====================================================================
%%% ETS Tables Tests
%%%====================================================================

test_check_ets() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_ets(),
    ?assertMatch(#{count := _, memory := _, status := _}, Result),
    {ok, Count} = maps:find(count, Result),
    {ok, Memory} = maps:find(memory, Result),
    ?assert(is_integer(Count)),
    ?assert(is_integer(Memory)).

%%%====================================================================
%%% Dependency Tests
%%%====================================================================

test_check_dependencies() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_dependencies(),
    ?assertMatch(#{status := _, missing := _, loaded := _}, Result),
    {ok, Missing} = maps:find(missing, Result),
    {ok, Loaded} = maps:find(loaded, Result),
    ?assert(is_list(Missing)),
    ?assert(is_list(Loaded)).

%%%====================================================================
%%% Application Status Tests
%%%====================================================================

test_check_applications() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_applications(),
    ?assertMatch(#{running := _, stopped := _, status := _}, Result),
    {ok, Running} = maps:find(running, Result),
    ?assert(is_list(Running)),
    ?assert(lists:member(erlmcp, Running) orelse lists:member(erlmcp_core, Running)).

%%%====================================================================
%%% Supervisor Tree Tests
%%%====================================================================

test_check_supervisors() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_supervisors(),
    ?assertMatch(#{trees := _, healthy := _, status := _}, Result),
    {ok, Healthy} = maps:find(healthy, Result),
    ?assert(is_integer(Healthy)).

%%%====================================================================
%%% Network Tests
%%%====================================================================

test_check_network() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_network(),
    ?assertMatch(#{connectivity := _, latency := _, status := _}, Result).

%%%====================================================================
%%% Health Summary Tests
%%%====================================================================

test_health_summary() ->
    {ok, Summary} = erlmcp_cli_diagnostics:health_summary(),
    ?assertMatch(#{
        overall_status := _,
        checks := _,
        passed := _,
        failed := _,
        warnings := _
    }, Summary),
    {ok, Passed} = maps:find(passed, Summary),
    {ok, Failed} = maps:find(failed, Summary),
    ?assert(is_integer(Passed)),
    ?assert(is_integer(Failed)).

%%%====================================================================
%%% Performance Metrics Tests
%%%====================================================================

test_performance_metrics() ->
    {ok, Metrics} = erlmcp_cli_diagnostics:performance_metrics(),
    ?assertMatch(#{
        throughput := _,
        latency := _,
        queue_depth := _
    }, Metrics).

%%%====================================================================
%%% Edge Cases
%%%====================================================================

edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Check with low memory", fun test_low_memory_warning/0},
      {"Check with high CPU", fun test_high_cpu_warning/0},
      {"Check missing application", fun test_missing_app/0}
     ]}.

test_low_memory_warning() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_memory(#{threshold => 99}),
    ?assertMatch(#{status := _}, Result).

test_high_cpu_warning() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_cpu(#{threshold => 1}),
    ?assertMatch(#{status := _}, Result).

test_missing_app() ->
    {ok, Result} = erlmcp_cli_diagnostics:check_application(fake_app),
    ?assertMatch(#{status := fail, reason := not_running}, Result).
