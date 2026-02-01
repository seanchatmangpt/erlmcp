%%%-------------------------------------------------------------------
%%% @doc erlmcp_cli_performance_SUITE - Performance Regression Tests
%%%
%%% Performance benchmarks and regression detection for CLI features
%%%
%%% Chicago School TDD - Real performance measurements, no mocks
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_performance_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     command_execution_throughput_test,
     completion_latency_test,
     plugin_loading_performance_test,
     history_search_performance_test,
     concurrent_command_performance_test,
     memory_usage_test,
     startup_time_test,
     regression_detection_test
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

command_execution_throughput_test(_Config) ->
    {ok, Session} = erlmcp_cli_interactive:start_link(),
    
    %% Execute 1000 commands and measure throughput
    StartTime = erlang:monotonic_time(millisecond),
    [erlmcp_cli_interactive:execute(Session, "help") || _ <- lists:seq(1, 1000)],
    EndTime = erlang:monotonic_time(millisecond),
    
    Duration = EndTime - StartTime,
    Throughput = 1000 / (Duration / 1000),
    
    %% Assert throughput > 100 commands/sec
    true = Throughput > 100,
    
    ct:pal("Command throughput: ~p commands/sec", [Throughput]),
    
    ok = erlmcp_cli_interactive:stop(Session).

completion_latency_test(_Config) ->
    {ok, Completer} = erlmcp_cli_completer:start_link(),
    
    %% Add 1000 entries
    Entries = [{command, list_to_binary("cmd_" ++ integer_to_list(N))} 
               || N <- lists:seq(1, 1000)],
    ok = erlmcp_cli_completer:add_entries(Completer, Entries),
    
    %% Measure completion latency
    StartTime = erlang:monotonic_time(microsecond),
    {ok, _} = erlmcp_cli_completer:complete(Completer, <<"cmd_5">>),
    EndTime = erlang:monotonic_time(microsecond),
    
    Latency = EndTime - StartTime,
    
    %% Assert latency < 10ms (10000 microseconds)
    true = Latency < 10000,
    
    ct:pal("Completion latency: ~p microseconds", [Latency]),
    
    ok = erlmcp_cli_completer:stop(Completer).

plugin_loading_performance_test(_Config) ->
    {ok, Manager} = erlmcp_plugin_manager:start_link(),
    
    %% Create 50 test plugins
    PluginDir = "/tmp/perf_test_plugins/",
    filelib:ensure_dir(PluginDir),
    Plugins = [begin
        Path = PluginDir ++ io_lib:format("plugin_~p.erl", [N]),
        create_test_plugin(Path, io_lib:format("plugin_~p", [N])),
        Path
    end || N <- lists:seq(1, 50)],
    
    %% Measure loading time
    StartTime = erlang:monotonic_time(millisecond),
    [erlmcp_plugin_manager:load_plugin(Manager, P) || P <- Plugins],
    EndTime = erlang:monotonic_time(millisecond),
    
    LoadTime = EndTime - StartTime,
    AvgLoadTime = LoadTime / 50,
    
    %% Assert average load time < 100ms
    true = AvgLoadTime < 100,
    
    ct:pal("Plugin load time: ~p ms (avg ~p ms/plugin)", [LoadTime, AvgLoadTime]),
    
    %% Cleanup
    [file:delete(P) || P <- Plugins],
    file:del_dir(PluginDir),
    ok = erlmcp_plugin_manager:stop(Manager).

history_search_performance_test(_Config) ->
    {ok, Session} = erlmcp_cli_interactive:start_link(#{history_limit => 10000}),
    
    %% Add 1000 commands to history
    [erlmcp_cli_interactive:add_to_history(Session, "command_" ++ integer_to_list(N))
     || N <- lists:seq(1, 1000)],
    
    %% Measure search time
    StartTime = erlang:monotonic_time(microsecond),
    {ok, _} = erlmcp_cli_interactive:search_history(Session, "command_500"),
    EndTime = erlang:monotonic_time(microsecond),
    
    SearchTime = EndTime - StartTime,
    
    %% Assert search time < 5ms
    true = SearchTime < 5000,
    
    ct:pal("History search time: ~p microseconds", [SearchTime]),
    
    ok = erlmcp_cli_interactive:stop(Session).

concurrent_command_performance_test(_Config) ->
    {ok, Session} = erlmcp_cli_interactive:start_link(),
    
    %% Execute 100 concurrent commands
    StartTime = erlang:monotonic_time(millisecond),
    Pids = [spawn(fun() ->
        {ok, _} = erlmcp_cli_interactive:execute(Session, "help")
    end) || _ <- lists:seq(1, 100)],
    
    %% Wait for all to complete
    [begin
        Ref = monitor(process, P),
        receive {'DOWN', Ref, process, P, _} -> ok after 5000 -> timeout end
    end || P <- Pids],
    
    EndTime = erlang:monotonic_time(millisecond),
    ConcurrentDuration = EndTime - StartTime,
    
    %% Assert concurrent execution < 2 seconds
    true = ConcurrentDuration < 2000,
    
    ct:pal("Concurrent command execution: ~p ms", [ConcurrentDuration]),
    
    ok = erlmcp_cli_interactive:stop(Session).

memory_usage_test(_Config) ->
    %% Get initial memory
    InitialMem = erlang:memory(total),
    
    %% Start session and execute commands
    {ok, Session} = erlmcp_cli_interactive:start_link(),
    [erlmcp_cli_interactive:execute(Session, "help") || _ <- lists:seq(1, 1000)],
    
    %% Get memory after
    FinalMem = erlang:memory(total),
    MemIncrease = FinalMem - InitialMem,
    
    %% Assert memory increase < 10MB
    true = MemIncrease < 10485760,
    
    ct:pal("Memory increase: ~p bytes (~p MB)", [MemIncrease, MemIncrease / 1048576]),
    
    ok = erlmcp_cli_interactive:stop(Session).

startup_time_test(_Config) ->
    %% Measure session startup time
    StartTime = erlang:monotonic_time(millisecond),
    {ok, Session} = erlmcp_cli_interactive:start_link(),
    EndTime = erlang:monotonic_time(millisecond),
    
    StartupTime = EndTime - StartTime,
    
    %% Assert startup < 100ms
    true = StartupTime < 100,
    
    ct:pal("Session startup time: ~p ms", [StartupTime]),
    
    ok = erlmcp_cli_interactive:stop(Session).

regression_detection_test(_Config) ->
    %% Run baseline performance tests
    Baseline = #{
        command_throughput => 100,
        completion_latency => 10000,
        plugin_load_time => 100,
        startup_time => 100
    },
    
    %% Run current performance tests
    {ok, Session} = erlmcp_cli_interactive:start_link(),
    
    StartTime1 = erlang:monotonic_time(millisecond),
    [erlmcp_cli_interactive:execute(Session, "help") || _ <- lists:seq(1, 1000)],
    EndTime1 = erlang:monotonic_time(millisecond),
    CurrentThroughput = 1000 / ((EndTime1 - StartTime1) / 1000),
    
    ok = erlmcp_cli_interactive:stop(Session),
    
    %% Check for regression (>10% slowdown)
    BaselineThroughput = maps:get(command_throughput, Baseline),
    RegressionThreshold = BaselineThroughput * 0.9,
    
    true = CurrentThroughput >= RegressionThreshold,
    
    ct:pal("Regression check: Current=~p, Baseline=~p, Threshold=~p", 
           [CurrentThroughput, BaselineThroughput, RegressionThreshold]),
    
    ok.

%%%====================================================================
%%% Helper Functions
%%%====================================================================

create_test_plugin(Path, ModuleName) ->
    Code = io_lib:format("-module(~s).
-export([init/1, execute/2]).

init(_Config) -> {ok, #{}}.
execute(_Command, _Args) -> {ok, #{status => success}}.
", [ModuleName]),
    file:write_file(Path, list_to_binary(Code)).
