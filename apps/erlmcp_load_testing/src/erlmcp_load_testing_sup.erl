%%%-------------------------------------------------------------------
%%% @doc
%%% Load Testing Supervisor for erlmcp v3
%%%
%%% This supervisor manages the load testing infrastructure with proper
 OTP supervision for high concurrency and reliability.
%%%
%%% Supervision Strategy:
%%% - one_for_one: Individual process restarts
%%% - Temporary workers: Non-critical components
%%% - Permanent workers: Critical components
%%% - Intensity: 5 restarts in 60 seconds
%%% - Period: 60 seconds hibernation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_load_testing_sup).

-behaviour(supervisor).

-export([start_link/0, start_test/2, stop_test/1,
         get_test_status/1, get_test_pid/1, prepare_shutdown/0]).

-export([init/1]).

-include("erlmcp_load_testing.hrl").

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start a load test instance
-spec start_test(load_test_id(), load_test_config()) ->
                     {ok, pid()} | {error, term()}.
start_test(TestId, Config) ->
    case validate_test_config(Config) of
        ok ->
            ChildSpec = #{
                id => TestId,
                start => {erlmcp_load_testing_test_manager, start_link, [TestId, Config]},
                restart => transient,
                shutdown => 5000,
                type => worker,
                modules => [erlmcp_load_testing_test_manager]
            },
            supervisor:start_child(?MODULE, ChildSpec);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Stop a load test instance
-spec stop_test(load_test_id()) -> ok | {error, term()}.
stop_test(TestId) ->
    case supervisor:terminate_child(?MODULE, TestId) of
        ok ->
            supervisor:delete_child(?MODULE, TestId);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get test status
-spec get_test_status(load_test_id()) ->
                         {ok, load_test_status()} | {error, term()}.
get_test_status(TestId) ->
    case whereis(TestId) of
        undefined ->
            {error, not_found};
        TestPid ->
            case erlmcp_load_testing_test_manager:get_status(TestPid) of
                {ok, Status} ->
                    {ok, Status};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc Get test manager PID
-spec get_test_pid(load_test_id()) ->
                      {ok, pid()} | {error, not_found}.
get_test_pid(TestId) ->
    case whereis(TestId) of
        undefined ->
            {error, not_found};
        TestPid ->
            {ok, TestPid}
    end.

%% @doc Prepare for graceful shutdown
-spec prepare_shutdown() -> ok.
prepare_shutdown() ->
    %% Stop all running tests gracefully
    Children = supervisor:which_children(?MODULE),
    lists:foreach(fun({TestId, _Pid, worker, _Modules}) ->
                      stop_test(TestId)
                  end, Children).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% OTP 28: Auto-hibernation for idle supervisors
    %% Memory savings: ~90% when system stable
    %% Wake time: <1ms on child operation
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60,
        auto_hibernation => ?MODULE
    },

    %% Permanent: Core load testing infrastructure
    %% Failure: Test isolation prevents cascading failures
    %% Impact: New tests cannot start during recovery
    ChildSpecs = [
        %% Load Generator Pool Supervisor
        #{
            id => erlmcp_load_testing_generator_sup,
            start => {erlmcp_load_testing_generator_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_load_testing_generator_sup]
        },
        %% Metrics Collection Supervisor
        #{
            id => erlmcp_load_testing_metrics_sup,
            start => {erlmcp_load_testing_metrics_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_load_testing_metrics_sup]
        },
        %% Performance Analysis Supervisor
        #{
            id => erlmcp_load_testing_analysis_sup,
            start => {erlmcp_load_testing_analysis_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_load_testing_analysis_sup]
        },
        %% Stress Testing Supervisor
        #{
            id => erlmcp_load_testing_stress_sup,
            start => {erlmcp_load_testing_stress_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_load_testing_stress_sup]
        },
        %% Database Load Testing Supervisor
        #{
            id => erlmcp_load_testing_db_sup,
            start => {erlmcp_load_testing_db_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_load_testing_db_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Validate test configuration before starting
-spec validate_test_config(load_test_config()) -> ok | {error, term()}.
validate_test_config(Config) ->
    case maps:get(protocol, Config) of
        Protocol when Protocol == orelse Protocol == websocket orelse
                      Protocol == sse orelse Protocol == tcp ->
            case validate_user_count(maps:get(user_count, Config, ?DEFAULT_USER_COUNT)) of
                ok -> validate_duration(maps:get(duration, Config, ?DEFAULT_DURATION));
                Error -> Error
            end;
        _ ->
            {error, invalid_protocol}
    end.

%% Validate user count (must be between 1 and 1,000,000)
-spec validate_user_count(pos_integer()) -> ok | {error, term()}.
validate_user_count(Count) when Count > 0, Count =< 1000000 ->
    ok;
validate_user_count(_) ->
    {error, {invalid_user_count, out_of_range}}.

%% Validate duration (must be between 1 and 24 hours)
-spec validate_duration(pos_integer()) -> ok | {error, term()}.
validate_duration(Duration) when Duration > 0, Duration =< 86400 ->
    ok;
validate_duration(_) ->
    {error, {invalid_duration, out_of_range}}.