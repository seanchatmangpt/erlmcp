%%%-------------------------------------------------------------------
%%% @doc erlmcp_metrics_dashboard_tests - Unit tests for metrics dashboard
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_metrics_dashboard_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test suite
%%====================================================================

start_stop_test() ->
    % Start dashboard
    {ok, Pid} = erlmcp_metrics_dashboard:start_link([{snapshots_dir, "test/fixtures/metrics"}]),
    ?assert(is_process_alive(Pid)),
    
    % Stop dashboard
    erlmcp_metrics_dashboard:stop(),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

get_current_metrics_test() ->
    {ok, _Pid} = erlmcp_metrics_dashboard:start_link([{snapshots_dir, "test/fixtures/metrics"}]),
    
    % Get current metrics
    Result = erlmcp_metrics_dashboard:get_current_metrics(),
    ?assertMatch({ok, _}, Result),
    
    {ok, Metrics} = Result,
    ?assert(is_map(Metrics)),
    
    erlmcp_metrics_dashboard:stop().

get_trend_test() ->
    {ok, _Pid} = erlmcp_metrics_dashboard:start_link([{snapshots_dir, "test/fixtures/metrics"}]),
    
    % Get 7-day trend
    Result = erlmcp_metrics_dashboard:get_trend(7),
    ?assertMatch({ok, _}, Result),
    
    {ok, Trend} = Result,
    ?assert(is_map(Trend) orelse is_list(Trend)),
    
    erlmcp_metrics_dashboard:stop().

get_history_test() ->
    {ok, _Pid} = erlmcp_metrics_dashboard:start_link([{snapshots_dir, "test/fixtures/metrics"}]),
    
    % Get 30-day history
    Result = erlmcp_metrics_dashboard:get_history(30),
    ?assertMatch({ok, _}, Result),
    
    {ok, History} = Result,
    ?assert(is_list(History)),
    
    erlmcp_metrics_dashboard:stop().

alert_on_regression_test() ->
    {ok, _Pid} = erlmcp_metrics_dashboard:start_link([{snapshots_dir, "test/fixtures/metrics"}]),
    
    % Check for regressions
    Result = erlmcp_metrics_dashboard:alert_on_regression(),
    ?assert(Result =:= ok orelse element(1, Result) =:= alert),
    
    erlmcp_metrics_dashboard:stop().

update_snapshot_test() ->
    {ok, _Pid} = erlmcp_metrics_dashboard:start_link([{snapshots_dir, "test/fixtures/metrics"}]),
    
    % Update with new snapshot
    TestSnapshot = "test/fixtures/metrics/test-snapshot.json",
    ok = erlmcp_metrics_dashboard:update_snapshot(TestSnapshot),
    
    erlmcp_metrics_dashboard:stop().

%%====================================================================
%% Test fixtures
%%====================================================================

setup_test_snapshots() ->
    Dir = "test/fixtures/metrics",
    filelib:ensure_dir(filename:join(Dir, "dummy")),
    
    % Create test snapshot
    Snapshot = #{
        <<"timestamp">> => <<"2026-01-28T12:00:00Z">>,
        <<"date">> => <<"2026-01-28">>,
        <<"git">> => #{
            <<"hash">> => <<"abc123">>,
            <<"branch">> => <<"main">>,
            <<"tag">> => <<"v0.6.0">>
        },
        <<"tests">> => #{
            <<"success">> => true,
            <<"total">> => 100,
            <<"passed">> => 100,
            <<"failed">> => 0,
            <<"pass_rate">> => 100.0
        },
        <<"coverage">> => #{
            <<"percentage">> => 85
        },
        <<"quality_score">> => #{
            <<"overall">> => 87.5
        }
    },
    
    File = filename:join(Dir, "test-snapshot.json"),
    ok = file:write_file(File, jsx:encode(Snapshot)),
    File.

cleanup_test_snapshots() ->
    os:cmd("rm -rf test/fixtures/metrics").
