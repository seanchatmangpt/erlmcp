-module(erlmcp_user_simulation).

-author("erlmcp AGI Swarm").
-vsn("3.0.0").

-behaviour(gen_server).

%% API exports
-export([
    start/1,
    stop/1,
    ramp_up/2,
    ramp_down/2,
    get_stats/1,
    inject_failures/2
]).

%% gen_server exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% TYPE DEFINITIONS
%%====================================================================

-type user_profile() :: #{
    id := binary(),
    behavior_type := read_heavy | write_heavy | mixed | bursty,
    request_pattern := #{
        interval := pos_integer(),
        burst_size := pos_integer(),
        think_time := pos_integer()
    },
    think_time := pos_integer(),
    session_duration := pos_integer(),
    start_time := integer(),
    end_time := integer(),
    current_state := active | thinking | busy | completed,
    metrics := map()
}.

-type session_config() :: #{
    user_profiles := [user_profile()],
    session_patterns := [map()],
    behavior_models := [map()],
    max_concurrent_users := pos_integer(),
    ramp_up_duration := pos_integer(),
    ramp_down_duration := pos_integer(),
    sampling_interval := pos_integer()
}.

-type user_session() :: #{
    pid := pid(),
    profile := user_profile(),
    start_time := integer(),
    request_count := non_neg_integer(),
    success_count := non_neg_integer(),
    error_count := non_neg_integer(),
    total_latency := non_neg_integer(),
    last_activity := integer()
}.

%%====================================================================
%% GEN_SERVER STATE
%%====================================================================

-record(state, {
    config :: session_config(),
    active_users :: map(),  #{pid() => user_session()}
    ramp_up_start :: integer(),
    ramp_down_start :: integer(),
    total_sessions_started :: non_neg_integer(),
    total_sessions_completed :: non_neg_integer(),
    metrics :: map(),
    monitoring_timer :: reference() | undefined,
    shutdown_timer :: reference() | undefined
}).

%%====================================================================
%% API FUNCTIONS
%%====================================================================

-spec start(session_config()) -> {ok, pid()} | {error, term()}.
start(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop, 30000).

-spec ramp_up(pid(), pos_integer()) -> ok.
ramp_up(Pid, TargetUsers) ->
    gen_server:call(Pid, {ramp_up, TargetUsers}, 10000).

-spec ramp_down(pid(), pos_integer()) -> ok.
ramp_down(Pid, TargetUsers) ->
    gen_server:call(Pid, {ramp_down, TargetUsers}, 10000).

-spec get_stats(pid()) -> map().
get_stats(Pid) ->
    gen_server:call(Pid, get_stats, 5000).

-spec inject_failures(pid(), list()) -> ok.
inject_failures(Pid, FailureTypes) ->
    gen_server:cast(Pid, {inject_failures, FailureTypes}).

%%====================================================================
%% GEN_SERVER CALLBACKS
%%====================================================================

init(Config) ->
    ?LOG_INFO("Starting user simulation with config: ~p", [Config]),

    State = #state{
        config = Config,
        active_users = #{},
        ramp_up_start = 0,
        ramp_down_start = 0,
        total_sessions_started = 0,
        total_sessions_completed = 0,
        metrics = initialize_metrics(),
        monitoring_timer = undefined,
        shutdown_timer = undefined
    },

    %% Start monitoring
    MonitoringTimer = erlang:send_after(?config(sampling_interval, Config), self(), collect_metrics),

    {ok, State#state{monitoring_timer = MonitoringTimer}}.

handle_call({ramp_up, TargetUsers}, _From, State) ->
    CurrentUsers = maps:size(State#state.active_users),
    UsersToStart = min(TargetUsers - CurrentUsers, ?config(max_concurrent_users, State#state.config) - CurrentUsers),

    case UsersToStart > 0 of
        true ->
            ?LOG_INFO("Ramping up ~p users (target: ~p, current: ~p)",
                     [UsersToStart, TargetUsers, CurrentUsers]),

            NewState = start_users(UsersToStart, State),
            {reply, ok, NewState};
        false ->
            ?LOG_INFO("No need to ramp up (current: ~p, target: ~p)",
                     [CurrentUsers, TargetUsers]),
            {reply, ok, State}
    end;

handle_call({ramp_down, TargetUsers}, _From, State) ->
    CurrentUsers = maps:size(State#state.active_users),
    UsersToStop = CurrentUsers - max(TargetUsers, 0),

    case UsersToStop > 0 of
        true ->
            ?LOG_INFO("Ramping down ~p users (target: ~p, current: ~p)",
                     [UsersToStop, TargetUsers, CurrentUsers]),

            NewState = stop_users(UsersToStop, State),
            {reply, ok, NewState};
        false ->
            ?LOG_INFO("No need to ramp down (current: ~p, target: ~p)",
                     [CurrentUsers, TargetUsers]),
            {reply, ok, State}
    end;

handle_call(get_stats, _From, State) ->
    Stats = generate_stats(State),
    {reply, Stats, State};

handle_call(stop, _From, State) ->
    %% Initiate graceful shutdown
    ShutdownTimer = erlang:send_after(5000, self(), graceful_shutdown),
    {reply, ok, State#state{shutdown_timer = ShutdownTimer}}.

handle_cast({inject_failures, FailureTypes}, State) ->
    ?LOG_INFO("Injecting failures: ~p", [FailureTypes]),

    %% Inject failures in active users
    lists:foreach(fun(UserPid) ->
        gen_server:cast(UserPid, inject_failure)
    end, maps:keys(State#state.active_users)),

    {noreply, State};

handle_info(collect_metrics, State) ->
    %% Collect metrics from all active users
    Metrics = collect_user_metrics(State),

    %% Update state metrics
    NewState = State#state{
        metrics = merge_metrics(State#state.metrics, Metrics)
    },

    %% Schedule next metrics collection
    MonitoringTimer = erlang:send_after(?config(sampling_interval, State#state.config),
                                      self(), collect_metrics),

    {noreply, NewState#state{monitoring_timer = MonitoringTimer}};

handle_info({user_completed, UserPid, SessionStats}, State) ->
    %% Remove completed user
    UserSession = maps:get(UserPid, State#state.active_users, undefined),

    case UserSession =/= undefined of
        true ->
            NewActiveUsers = maps:remove(UserPid, State#state.active_users),
            CompletedUsers = State#state.total_sessions_completed + 1,

            %% Update metrics
            UpdatedMetrics = update_completion_metrics(State#state.metrics, SessionStats),

            NewState = State#state{
                active_users = NewActiveUsers,
                total_sessions_completed = CompletedUsers,
                metrics = UpdatedMetrics
            },

            ?LOG_DEBUG("User session completed: ~p, total completed: ~p",
                       [UserPid, CompletedUsers]),

            {noreply, NewState};
        false ->
            ?LOG_WARNING("Received completion from unknown user: ~p", [UserPid]),
            {noreply, State}
    end;

handle_info(graceful_shutdown, State) ->
    %% Stop all active users
    StopMessage = {stop, shutdown},
    lists:foreach(fun(UserPid) ->
        UserPid ! StopMessage
    end, maps:keys(State#state.active_users)),

    {noreply, State}.

terminate(_Reason, State) ->
    ?LOG_INFO("Terminating user simulation, active users: ~p",
             [maps:size(State#state.active_users)]),

    %% Stop monitoring timer
    case State#state.monitoring_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,

    %% Shutdown all active sessions
    lists:foreach(fun(UserPid) ->
        UserPid ! {stop, shutdown}
    end, maps:keys(State#state.active_users)),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% INTERNAL FUNCTIONS
%%====================================================================

initialize_metrics() ->
    #{
        timestamp => erlang:system_time(millisecond),
        total_sessions => 0,
        active_sessions => 0,
        completed_sessions => 0,
        requests_per_second => 0.0,
        success_rate => 0.0,
        average_latency => 0.0,
        p95_latency => 0.0,
        p99_latency => 0.0,
        error_rate => 0.0,
        response_time_distribution => #{},
        throughput_history => [],
        latency_history => [],
        error_history => []
    }.

start_users(Count, State) ->
    Profiles = ?config(user_profiles, State#state.config),

    case select_user_profiles(Count, Profiles, State#state.active_users) of
        {ok, SelectedProfiles} ->
            {NewUsers, NewState} = lists:foldl(fun(Profile, {AccUsers, AccState}) ->
                case create_user_session(Profile, AccState) of
                    {ok, UserPid, UserSession} ->
                        NewAccUsers = maps:put(UserPid, UserSession, AccUsers),
                        {NewAccUsers, AccState#state{
                            total_sessions_started = AccState#state.total_sessions_started + 1
                        }};
                    {error, Reason} ->
                        ?LOG_ERROR("Failed to create user session: ~p", [Reason]),
                        {AccUsers, AccState}
                end
            end, {#{}, State}, SelectedProfiles),

            NewState = AccState#state{
                active_users = NewUsers,
                metrics = update_metrics(State#state.metrics, NewUsers)
            },

            %% Ramp up delay for next batch
            RampDelay = calculate_ramp_delay(Count, State#state.config),
            erlang:send_after(RampDelay, self(), {start_users, Count}),

            NewState;
        {error, Reason} ->
            ?LOG_ERROR("Failed to select user profiles: ~p", [Reason]),
            State
    end.

select_user_profiles(Count, Profiles, ActiveUsers) ->
    AvailableProfiles = lists:filter(fun(Profile) ->
        not is_user_limit_reached(Profiles, ActiveUsers)
    end, Profiles),

    case AvailableProfiles of
        [] ->
            {error, no_available_profiles};
        _ ->
            %% Select profiles based on distribution
            Selected = select_profiles_by_distribution(Count, AvailableProfiles),
            {ok, Selected}
    end.

is_user_limit_reached(Profiles, ActiveUsers) ->
    %% Implement user limits based on profile type
    ActiveByType = count_active_by_type(ActiveUsers),
    ProfileTypeCount = count_profiles_by_type(Profiles),

    %% Simple check - in real implementation, would be more sophisticated
    maps:size(ActiveUsers) + maps:size(ProfileTypeCount) > 10000.

count_active_by_type(ActiveUsers) ->
    lists:foldl(fun(UserSession, Acc) ->
        Type = UserSession#profile.behavior_type,
        maps:get(Type, Acc, 0) + 1
    end, #{}, ActiveUsers).

count_profiles_by_type(Profiles) ->
    lists:foldl(fun(Profile, Acc) ->
        Type = Profile#profile.behavior_type,
        maps:get(Type, Acc, 0) + 1
    end, #{}, Profiles).

select_profiles_by_distribution(Count, Profiles) ->
    %% Select profiles based on desired distribution
    %% For simplicity, just select randomly
    [lists:nth(rand:uniform(length(Profiles)), Profiles) || _ <- lists:seq(1, Count)].

create_user_session(Profile, State) ->
    UserPid = spawn_link(fun() -> user_session_loop(Profile, State) end),

    UserSession = #{
        pid => UserPid,
        profile => Profile,
        start_time => erlang:system_time(millisecond),
        request_count => 0,
        success_count => 0,
        error_count => 0,
        total_latency => 0,
        last_activity => erlang:system_time(millisecond)
    },

    {ok, UserPid, UserSession}.

user_session_loop(Profile, State) ->
    user_session_loop(Profile, State, 0, 0).

user_session_loop(Profile, State, ThinkStartTime, RequestCount) ->
    receive
        {stop, Reason} ->
            %% Send completion notification
            gen_server:cast(?MODULE, {user_completed, self(), generate_session_stats(Profile, RequestCount)}),
            Reason;

        inject_failure ->
            %% Handle failure injection
            handle_injected_failure(),
            user_session_loop(Profile, State, erlang:system_time(millisecond), RequestCount)
    after max(0, ThinkStartTime - erlang:system_time(millisecond)) ->
        %% Time to make requests
        RequestStats = make_requests(Profile, State),

        %% Calculate think time for next request
        ThinkTime = get_think_time(Profile),
        NextThinkStartTime = erlang:system_time(millisecond) + ThinkTime,

        user_session_loop(Profile, State, NextThinkStartTime, RequestCount + 1)
    end.

make_requests(Profile, State) ->
    #{request_pattern := Pattern} = Profile,
    BurstSize = Pattern#burst_size,

    %% Make burst of requests
    Requests = [make_request(Profile) || _ <- lists:seq(1, BurstSize)],

    %% Process requests
    lists:foldl(fun(Request, Acc) ->
        case Request of
            {success, Latency} ->
                Acc#{
                    success_count := Acc#success_count + 1,
                    total_latency := Acc#total_latency + Latency,
                    last_activity := erlang:system_time(millisecond)
                };
            {error, Error} ->
                Acc#{
                    error_count := Acc#error_count + 1,
                    last_activity := erlang:system_time(millisecond)
                }
        end
    end, #{success_count => 0, total_latency => 0, error_count => 0}, Requests).

make_request(Profile) ->
    StartTime = erlang:system_time(millisecond),

    try
        %% Make actual request to erlmcp
        Result = erlmcp_client:call_tool(<<"test_load_generator">>, #{
            user_id => Profile#profile.id,
            behavior_type => Profile#profile.behavior_type,
            request_time => StartTime
        }),

        case Result of
            {ok, Response} ->
                Latency = erlang:system_time(millisecond) - StartTime,
                {success, Latency};
            {error, Error} ->
                {error, Error}
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Request failed: ~p:~p", [Error, Reason]),
            {error, Reason}
    end.

get_think_time(Profile) ->
    Profile#profile.think_time + rand:uniform(Profile#profile.think_time div 2).

handle_injected_failure() ->
    %% Simulate failure
    ?LOG_INFO("Handling injected failure"),
    timer:sleep(1000).

generate_session_stats(Profile, RequestCount) ->
    #{
        user_id => Profile#profile.id,
        behavior_type => Profile#profile.behavior_type,
        total_requests => RequestCount,
        completion_time => erlang:system_time(millisecond),
        session_duration => erlang:system_time(millisecond) - Profile#profile.start_time
    }.

stop_users(Count, State) ->
    %% Select users to stop
    UserPids = maps:keys(State#state.active_users),
    UsersToStop = lists:sublist(UserPids, Count),

    %% Stop users
    lists:foreach(fun(UserPid) ->
        UserPid ! {stop, shutdown}
    end, UsersToStop),

    %% Update active users
    NewActiveUsers = lists:foldl(fun(Pid, Acc) ->
        case maps:is_key(Pid, Acc) of
            true -> maps:remove(Pid, Acc);
            false -> Acc
        end
    end, State#state.active_users, UsersToStop),

    State#state{
        active_users = NewActiveUsers
    }.

calculate_ramp_delay(Count, Config) ->
    RampDuration = ?config(ramp_up_duration, Config),
    Interval = RampDuration div min(Count, 100),  % Max 100 users per interval
    max(Interval, 100).  % Minimum 100ms delay

collect_user_metrics(State) ->
    lists:foldl(fun(UserPid, Acc) ->
        case gen_server:call(UserPid, get_metrics, 1000) of
            {ok, Metrics} -> merge_metrics(Acc, Metrics);
            {error, _} -> Acc
        end
    end, #{}, maps:keys(State#state.active_users)).

merge_metrics(Base, New) ->
    %% Merge metrics from multiple sources
    lists:fold(fun({Key, Value}, Acc) ->
        case maps:get(Key, Acc, undefined) of
            undefined -> Acc#{Key => Value};
            Existing -> Acc#{Key => lists:flatten([Existing, Value])}
        end
    end, Base, maps:to_list(New)).

update_metrics(StateMetrics, NewUsers) ->
    ActiveCount = maps:size(NewUsers),
    CompletedCount = StateMetrics#metrics.completed_sessions,
    TotalSessions = StateMetrics#metrics.total_sessions,

    StateMetrics#metrics{
        active_sessions = ActiveCount,
        total_sessions = TotalSessions + ActiveCount,
        completed_sessions = CompletedCount
    }.

update_completion_metrics(StateMetrics, SessionStats) ->
    %% Update metrics based on completed session
    StateMetrics#metrics{
        completed_sessions = StateMetrics#metrics.completed_sessions + 1,
        throughput_history = [SessionStats | StateMetrics#metrics.throughput_history],
        latency_history = [SessionStats | StateMetrics#metrics.latency_history],
        error_history = [SessionStats | StateMetrics#metrics.error_history]
    }.

generate_stats(State) ->
    #{
        timestamp => erlang:system_time(millisecond),
        total_sessions_started => State#state.total_sessions_started,
        total_sessions_completed => State#state.total_sessions_completed,
        active_sessions => maps:size(State#state.active_users),
        success_rate => calculate_success_rate(State),
        average_latency => calculate_average_latency(State),
        p95_latency => calculate_p95_latency(State),
        p99_latency => calculate_p99_latency(State),
        error_rate => calculate_error_rate(State),
        requests_per_second => calculate_rps(State),
        response_time_distribution => calculate_response_time_distribution(State),
        breakdown_by_behavior => get_behavior_breakdown(State)
    }.

calculate_success_rate(State) ->
    TotalRequests = State#state.metrics#metrics.requests_per_second *
                   (erlang:system_time(millisecond) - State#state.metrics#metrics.timestamp) / 1000,
    SuccessRequests = State#state.metrics#metrics.success_rate * TotalRequests,
    if TotalRequests > 0 -> SuccessRequests / TotalRequests; true -> 0.0 end.

calculate_average_latency(State) ->
    lists:sum(State#state.metrics#metrics.latency_history) /
    max(1, length(State#state.metrics#metrics.latency_history)).

calculate_p95_latency(State) ->
    Latencies = State#state.metrics#metrics.latency_history,
    case length(Latencies) of
        0 -> 0;
        _ -> lists:nth(floor(0.95 * length(Latencies)), lists:sort(Latencies))
    end.

calculate_p99_latency(State) ->
    Latencies = State#state.metrics#metrics.latency_history,
    case length(Latencies) of
        0 -> 0;
        _ -> lists:nth(floor(0.99 * length(Latencies)), lists:sort(Latencies))
    end.

calculate_error_rate(State) ->
    TotalRequests = State#state.metrics#metrics.requests_per_second *
                   (erlang:system_time(millisecond) - State#state.metrics#metrics.timestamp) / 1000,
    ErrorRequests = State#state.metrics#metrics.error_rate * TotalRequests,
    if TotalRequests > 0 -> ErrorRequests / TotalRequests; true -> 0.0 end.

calculate_rps(State) ->
    State#state.metrics#metrics.requests_per_second.

calculate_response_time_distribution(State) ->
    Latencies = State#state.metrics#metrics.latency_history,

    %% Create buckets
    Buckets = [
        {0-100, []}, {100-200, []}, {200-500, []},
        {500-1000, []}, {1000-2000, []}, {2000-5000, []}, {5000, []}
    ],

    lists:foldl(fun(Latency, Acc) ->
        case Latency of
            L when L < 100 -> update_bucket(0-100, L, Acc);
            L when L < 200 -> update_bucket(100-200, L, Acc);
            L when L < 500 -> update_bucket(200-500, L, Acc);
            L when L < 1000 -> update_bucket(500-1000, L, Acc);
            L when L < 2000 -> update_bucket(1000-2000, L, Acc);
            L when L < 5000 -> update_bucket(2000-5000, L, Acc);
            _ -> update_bucket(5000, Latency, Acc)
        end
    end, Buckets, Latencies).

update_bucket(Bucket, Value, Buckets) ->
    lists:foldl(fun({Range, Values}, Acc) ->
        case Range of
            Bucket -> [{Range, [Value | Values]} | Acc];
            _ -> [Range, Values | Acc]
        end
    end, [], Buckets).

get_behavior_breakdown(State) ->
    lists:foldl(fun(UserSession, Acc) ->
        Type = UserSession#profile.behavior_type,
        maps:get(Type, Acc, 0) + 1
    end, #{}, State#state.active_users).