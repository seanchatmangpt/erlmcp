%%%-------------------------------------------------------------------
%%% @doc
%%% Transport Registry with Health Monitoring
%%%
%%% Central registry for managing all active transports with:
%%% - Health status tracking (up, degraded, down)
%%% - Automatic failover to healthy transports
%%% - Connection statistics and metrics
%%% - Transport lifecycle management
%%%
%%% This complements erlmcp_registry by providing transport-specific
%%% management capabilities.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_registry).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0,
    register_transport/3,
    unregister_transport/1,
    get_transport/1,
    get_all_transports/0,
    get_healthy_transports/0,
    get_transport_status/1,
    update_health_status/2,
    record_success/1,
    record_failure/2,
    select_transport/1,
    get_statistics/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Health status types
-type health_status() :: up | degraded | down | unknown.
-type transport_type() :: stdio | tcp | http | websocket | sse | custom.

-export_type([health_status/0, transport_type/0]).

%% Transport info record
-record(transport_info, {
    transport_id :: atom(),
    pid :: pid(),
    type :: transport_type(),
    config :: map(),
    health_status = unknown :: health_status(),
    registered_at :: non_neg_integer(),
    last_check :: non_neg_integer(),
    statistics = #{
        messages_sent => 0,
        messages_received => 0,
        bytes_sent => 0,
        bytes_received => 0,
        successes => 0,
        failures => 0,
        last_success => 0,
        last_failure => 0
    } :: map(),
    monitor_ref :: reference() | undefined
}).

-type transport_info() :: #transport_info{}.

%% State record
-record(state, {
    transports = #{} :: #{atom() => transport_info()},
    health_check_interval = 30000 :: non_neg_integer(),
    health_check_timer :: reference() | undefined,
    failure_threshold = 5 :: non_neg_integer(),
    degraded_threshold = 3 :: non_neg_integer()
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the transport registry
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register a transport
-spec register_transport(atom(), pid(), map()) -> ok | {error, term()}.
register_transport(TransportId, Pid, Config) when is_atom(TransportId), is_pid(Pid), is_map(Config) ->
    gen_server:call(?MODULE, {register_transport, TransportId, Pid, Config}).

%% @doc Unregister a transport
-spec unregister_transport(atom()) -> ok.
unregister_transport(TransportId) when is_atom(TransportId) ->
    gen_server:call(?MODULE, {unregister_transport, TransportId}).

%% @doc Get transport information
-spec get_transport(atom()) -> {ok, map()} | {error, not_found}.
get_transport(TransportId) when is_atom(TransportId) ->
    gen_server:call(?MODULE, {get_transport, TransportId}).

%% @doc Get all registered transports
-spec get_all_transports() -> [map()].
get_all_transports() ->
    gen_server:call(?MODULE, get_all_transports).

%% @doc Get only healthy transports (status = up)
-spec get_healthy_transports() -> [map()].
get_healthy_transports() ->
    gen_server:call(?MODULE, get_healthy_transports).

%% @doc Get health status of a transport
-spec get_transport_status(atom()) -> {ok, health_status()} | {error, not_found}.
get_transport_status(TransportId) when is_atom(TransportId) ->
    gen_server:call(?MODULE, {get_transport_status, TransportId}).

%% @doc Update health status manually
-spec update_health_status(atom(), health_status()) -> ok | {error, not_found}.
update_health_status(TransportId, Status) when is_atom(TransportId), is_atom(Status) ->
    gen_server:call(?MODULE, {update_health_status, TransportId, Status}).

%% @doc Record a successful operation
-spec record_success(atom()) -> ok.
record_success(TransportId) when is_atom(TransportId) ->
    gen_server:cast(?MODULE, {record_success, TransportId}).

%% @doc Record a failed operation
-spec record_failure(atom(), term()) -> ok.
record_failure(TransportId, Reason) when is_atom(TransportId) ->
    gen_server:cast(?MODULE, {record_failure, TransportId, Reason}).

%% @doc Select best transport based on type and health
-spec select_transport(transport_type()) -> {ok, atom()} | {error, no_available_transport}.
select_transport(Type) when is_atom(Type) ->
    gen_server:call(?MODULE, {select_transport, Type}).

%% @doc Get statistics for a transport
-spec get_statistics(atom()) -> {ok, map()} | {error, not_found}.
get_statistics(TransportId) when is_atom(TransportId) ->
    gen_server:call(?MODULE, {get_statistics, TransportId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @doc Initialize the registry
init([]) ->
    process_flag(trap_exit, true),

    ?LOG_INFO(#{what => transport_registry_init}),

    % Schedule first health check
    Timer = erlang:send_after(30000, self(), health_check),

    {ok, #state{health_check_timer = Timer}}.

%% @doc Handle synchronous calls
handle_call({register_transport, TransportId, Pid, Config}, _From, State) ->
    case maps:is_key(TransportId, State#state.transports) of
        true ->
            {reply, {error, already_registered}, State};
        false ->
            % Monitor the transport process
            MonitorRef = monitor(process, Pid),

            TransportInfo = #transport_info{
                transport_id = TransportId,
                pid = Pid,
                type = maps:get(type, Config, custom),
                config = Config,
                health_status = up,  % Assume healthy on registration
                registered_at = erlang:system_time(millisecond),
                last_check = erlang:system_time(millisecond),
                monitor_ref = MonitorRef
            },

            NewTransports = maps:put(TransportId, TransportInfo, State#state.transports),

            ?LOG_INFO(#{
                what => transport_registered,
                transport_id => TransportId,
                pid => Pid,
                type => TransportInfo#transport_info.type
            }),

            {reply, ok, State#state{transports = NewTransports}}
    end;

handle_call({unregister_transport, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            {reply, ok, State};
        TransportInfo ->
            % Demonitor the process
            case TransportInfo#transport_info.monitor_ref of
                undefined -> ok;
                MonitorRef -> demonitor(MonitorRef, [flush])
            end,

            NewTransports = maps:remove(TransportId, State#state.transports),

            ?LOG_INFO(#{
                what => transport_unregistered,
                transport_id => TransportId
            }),

            {reply, ok, State#state{transports = NewTransports}}
    end;

handle_call({get_transport, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TransportInfo ->
            Info = transport_info_to_map(TransportInfo),
            {reply, {ok, Info}, State}
    end;

handle_call(get_all_transports, _From, State) ->
    Transports = [transport_info_to_map(Info) || Info <- maps:values(State#state.transports)],
    {reply, Transports, State};

handle_call(get_healthy_transports, _From, State) ->
    Healthy = [transport_info_to_map(Info)
               || Info <- maps:values(State#state.transports),
                  Info#transport_info.health_status =:= up],
    {reply, Healthy, State};

handle_call({get_transport_status, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TransportInfo ->
            {reply, {ok, TransportInfo#transport_info.health_status}, State}
    end;

handle_call({update_health_status, TransportId, NewStatus}, _From, State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TransportInfo ->
            OldStatus = TransportInfo#transport_info.health_status,
            UpdatedInfo = TransportInfo#transport_info{
                health_status = NewStatus,
                last_check = erlang:system_time(millisecond)
            },
            NewTransports = maps:put(TransportId, UpdatedInfo, State#state.transports),

            case OldStatus =/= NewStatus of
                true ->
                    ?LOG_WARNING(#{
                        what => transport_health_status_changed,
                        transport_id => TransportId,
                        old_status => OldStatus,
                        new_status => NewStatus
                    });
                false ->
                    ok
            end,

            {reply, ok, State#state{transports = NewTransports}}
    end;

handle_call({select_transport, Type}, _From, State) ->
    % Select best healthy transport of given type
    Candidates = [Info
                  || Info <- maps:values(State#state.transports),
                     Info#transport_info.type =:= Type,
                     Info#transport_info.health_status =:= up],

    case Candidates of
        [] ->
            {reply, {error, no_available_transport}, State};
        _ ->
            % Select transport with best success rate
            Best = select_best_transport(Candidates),
            {reply, {ok, Best#transport_info.transport_id}, State}
    end;

handle_call({get_statistics, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TransportInfo ->
            {reply, {ok, TransportInfo#transport_info.statistics}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous casts
handle_cast({record_success, TransportId}, State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            {noreply, State};
        TransportInfo ->
            Stats = TransportInfo#transport_info.statistics,
            NewStats = Stats#{
                successes => maps:get(successes, Stats, 0) + 1,
                last_success => erlang:system_time(millisecond)
            },

            % Re-evaluate health status based on success
            NewStatus = evaluate_health_status(NewStats, State),

            UpdatedInfo = TransportInfo#transport_info{
                statistics = NewStats,
                health_status = NewStatus,
                last_check = erlang:system_time(millisecond)
            },

            NewTransports = maps:put(TransportId, UpdatedInfo, State#state.transports),
            {noreply, State#state{transports = NewTransports}}
    end;

handle_cast({record_failure, TransportId, Reason}, State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            {noreply, State};
        TransportInfo ->
            Stats = TransportInfo#transport_info.statistics,
            NewStats = Stats#{
                failures => maps:get(failures, Stats, 0) + 1,
                last_failure => erlang:system_time(millisecond)
            },

            ?LOG_WARNING(#{
                what => transport_failure_recorded,
                transport_id => TransportId,
                reason => Reason,
                total_failures => maps:get(failures, NewStats)
            }),

            % Re-evaluate health status based on failures
            NewStatus = evaluate_health_status(NewStats, State),

            UpdatedInfo = TransportInfo#transport_info{
                statistics = NewStats,
                health_status = NewStatus,
                last_check = erlang:system_time(millisecond)
            },

            NewTransports = maps:put(TransportId, UpdatedInfo, State#state.transports),
            {noreply, State#state{transports = NewTransports}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info(health_check, State) ->
    NewState = perform_health_checks(State),

    % Schedule next health check
    Timer = erlang:send_after(State#state.health_check_interval, self(), health_check),

    {noreply, NewState#state{health_check_timer = Timer}};

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    % Find transport by monitor ref
    case find_transport_by_monitor(MonitorRef, State#state.transports) of
        {ok, TransportId, TransportInfo} ->
            ?LOG_WARNING(#{
                what => transport_process_died,
                transport_id => TransportId,
                pid => Pid,
                reason => Reason
            }),

            % Mark as down and update
            UpdatedInfo = TransportInfo#transport_info{
                health_status = down,
                last_check = erlang:system_time(millisecond),
                monitor_ref = undefined
            },

            NewTransports = maps:put(TransportId, UpdatedInfo, State#state.transports),
            {noreply, State#state{transports = NewTransports}};
        not_found ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Cleanup on termination
terminate(_Reason, State) ->
    % Cancel health check timer
    case State#state.health_check_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,

    % Demonitor all transports
    maps:foreach(fun(_Id, Info) ->
        case Info#transport_info.monitor_ref of
            undefined -> ok;
            MonitorRef -> demonitor(MonitorRef, [flush])
        end
    end, State#state.transports),

    ok.

%% @doc Handle code upgrades
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Health Checks
%%====================================================================

%% @doc Perform health checks on all registered transports
-spec perform_health_checks(state()) -> state().
perform_health_checks(State) ->
    Now = erlang:system_time(millisecond),

    UpdatedTransports = maps:map(fun(_Id, Info) ->
        NewStatus = check_transport_health(Info, Now, State),

        case NewStatus =/= Info#transport_info.health_status of
            true ->
                ?LOG_INFO(#{
                    what => transport_health_changed,
                    transport_id => Info#transport_info.transport_id,
                    old_status => Info#transport_info.health_status,
                    new_status => NewStatus
                });
            false ->
                ok
        end,

        Info#transport_info{
            health_status = NewStatus,
            last_check = Now
        }
    end, State#state.transports),

    State#state{transports = UpdatedTransports}.

%% @doc Check health of a single transport
-spec check_transport_health(transport_info(), non_neg_integer(), state()) -> health_status().
check_transport_health(Info, _Now, State) ->
    % Check if process is alive
    case is_process_alive(Info#transport_info.pid) of
        false ->
            down;
        true ->
            % Evaluate based on statistics
            evaluate_health_status(Info#transport_info.statistics, State)
    end.

%% @doc Evaluate health status based on statistics
-spec evaluate_health_status(map(), state()) -> health_status().
evaluate_health_status(Stats, State) ->
    Successes = maps:get(successes, Stats, 0),
    Failures = maps:get(failures, Stats, 0),

    % Calculate recent failure rate (last 100 operations)
    Total = Successes + Failures,

    case Total of
        0 ->
            up;  % No data, assume healthy
        _ when Failures >= State#state.failure_threshold ->
            down;
        _ when Failures >= State#state.degraded_threshold ->
            degraded;
        _ ->
            up
    end.

%%====================================================================
%% Internal Functions - Transport Selection
%%====================================================================

%% @doc Select best transport from candidates
-spec select_best_transport([transport_info()]) -> transport_info().
select_best_transport([Single]) ->
    Single;
select_best_transport(Candidates) ->
    % Sort by success rate (highest first)
    Sorted = lists:sort(fun(A, B) ->
        RateA = success_rate(A#transport_info.statistics),
        RateB = success_rate(B#transport_info.statistics),
        RateA >= RateB
    end, Candidates),

    hd(Sorted).

%% @doc Calculate success rate
-spec success_rate(map()) -> float().
success_rate(Stats) ->
    Successes = maps:get(successes, Stats, 0),
    Failures = maps:get(failures, Stats, 0),
    Total = Successes + Failures,

    case Total of
        0 -> 1.0;  % No data, assume perfect
        _ -> Successes / Total
    end.

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

%% @doc Convert transport_info record to map
-spec transport_info_to_map(transport_info()) -> map().
transport_info_to_map(Info) ->
    #{
        transport_id => Info#transport_info.transport_id,
        pid => Info#transport_info.pid,
        type => Info#transport_info.type,
        config => Info#transport_info.config,
        health_status => Info#transport_info.health_status,
        registered_at => Info#transport_info.registered_at,
        last_check => Info#transport_info.last_check,
        statistics => Info#transport_info.statistics
    }.

%% @doc Find transport by monitor reference
-spec find_transport_by_monitor(reference(), #{atom() => transport_info()}) ->
    {ok, atom(), transport_info()} | not_found.
find_transport_by_monitor(MonitorRef, Transports) ->
    case maps:to_list(Transports) of
        [] ->
            not_found;
        List ->
            case lists:search(fun({_Id, Info}) ->
                Info#transport_info.monitor_ref =:= MonitorRef
            end, List) of
                {value, {Id, Info}} ->
                    {ok, Id, Info};
                false ->
                    not_found
            end
    end.
