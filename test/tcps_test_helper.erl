%%%-------------------------------------------------------------------
%%% @doc TCPS Test Helper Module
%%%
%%% Common setup/teardown functions for CT test suites.
%%% Handles application startup dependencies and provides
%%% reusable test fixtures.
%%%
%%% Chicago School TDD: Real application startup, real processes.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_test_helper).

-export([
    start_tcps_apps/0,
    stop_tcps_apps/0,
    ensure_test_dir/1,
    cleanup_test_dir/1,
    wait_for_application/2,
    start_minimal_system/0,
    stop_minimal_system/0
]).

%%%===================================================================
%%% Application Startup Helpers
%%%===================================================================

%% @doc Start TCPS applications with dependencies
%% Returns {ok, StartedApps} or {error, Reason}
-spec start_tcps_apps() -> {ok, [atom()]} | {error, term()}.
start_tcps_apps() ->
    %% Use application:ensure_all_started for automatic dependency resolution
    case application:ensure_all_started(tcps_erlmcp) of
        {ok, Started} ->
            {ok, Started};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Start minimal system (core only, no TCPS)
-spec start_minimal_system() -> {ok, [atom()]} | {error, term()}.
start_minimal_system() ->
    case application:ensure_all_started(erlmcp_core) of
        {ok, Started} ->
            {ok, Started};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Stop TCPS applications
-spec stop_tcps_apps() -> ok.
stop_tcps_apps() ->
    AppsToStop = [
        tcps_erlmcp,
        erlmcp_observability,
        erlmcp_core
    ],

    lists:foreach(fun(App) ->
        application:stop(App)
    end, AppsToStop),

    ok.

%% @doc Stop minimal system
-spec stop_minimal_system() -> ok.
stop_minimal_system() ->
    application:stop(erlmcp_core),
    ok.

%%%===================================================================
%%% Directory Helpers
%%%===================================================================

%% @doc Ensure test directory exists
-spec ensure_test_dir(string()) -> ok | {error, term()}.
ensure_test_dir(Dir) ->
    case filelib:ensure_dir(filename:join(Dir, "dummy")) of
        ok ->
            case file:make_dir(Dir) of
                ok -> ok;
                {error, eexist} -> ok;
                Error -> Error
            end;
        {error, eexist} -> ok;
        Error -> Error
    end.

%% @doc Cleanup test directory
-spec cleanup_test_dir(string()) -> ok.
cleanup_test_dir(Dir) ->
    os:cmd("rm -rf " ++ Dir),
    ok.

%%%===================================================================
%%% Wait Helpers
%%%===================================================================

%% @doc Wait for application to start (with timeout)
-spec wait_for_application(atom(), pos_integer()) -> ok | {error, timeout}.
wait_for_application(App, Timeout) ->
    wait_for_application_loop(App, Timeout, 100).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Start applications in order, tracking started apps
start_apps_in_order([], Started) ->
    {ok, lists:reverse(Started)};
start_apps_in_order([App | Rest], Started) ->
    case application:ensure_started(App) of
        ok ->
            start_apps_in_order(Rest, [App | Started]);
        {error, {already_started, App}} ->
            %% Already started, continue
            start_apps_in_order(Rest, Started);
        {error, Reason} ->
            %% Stop already started apps
            lists:foreach(fun(A) -> application:stop(A) end, Started),
            {error, {failed_to_start, App, Reason}}
    end.

%% Wait for application loop
wait_for_application_loop(_App, Timeout, _Interval) when Timeout =< 0 ->
    {error, timeout};
wait_for_application_loop(App, Timeout, Interval) ->
    case lists:keyfind(App, 1, application:which_applications()) of
        {App, _Desc, _Vsn} ->
            ok;
        false ->
            timer:sleep(Interval),
            wait_for_application_loop(App, Timeout - Interval, Interval)
    end.
