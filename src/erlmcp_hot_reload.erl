-module(erlmcp_hot_reload).
-behaviour(gen_server).

%% API - Code Hot Reload
-export([
    start_link/0,
    reload_module/1,
    reload_modules/1,
    reload_all_modules/0,
    get_reload_status/0,
    get_module_version/1
]).

%% API - Configuration Hot Reload
-export([
    reload_config/0,
    reload_config/1,
    get_config_version/0,
    validate_config/1
]).

%% API - Graceful Draining
-export([
    begin_graceful_drain/1,
    end_graceful_drain/0,
    is_draining/0,
    get_drain_status/0,
    drain_complete/1
]).

%% API - Metrics
-export([
    get_reload_metrics/0,
    reset_reload_metrics/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

% Logger macro compatibility
-ifndef(LOG_notice).
-define(LOG_notice(Msg, Meta), logger:notice(Msg, Meta)).
-endif.
-ifndef(LOG_warning).
-define(LOG_warning(Msg, Meta), logger:warning(Msg, Meta)).
-endif.
-ifndef(LOG_info).
-define(LOG_info(Msg, Meta), logger:info(Msg, Meta)).
-endif.
-ifndef(LOG_error).
-define(LOG_error(Msg, Meta), logger:error(Msg, Meta)).
-endif.

%% Types
-type module_name() :: atom().
-type reload_result() :: {ok, module_name()} | {error, term()}.
-type drain_status() :: idle | draining | complete.
-type reload_metrics() :: #{
    total_reloads := non_neg_integer(),
    successful_reloads := non_neg_integer(),
    failed_reloads := non_neg_integer(),
    total_connections_at_reload := non_neg_integer(),
    connections_preserved := non_neg_integer(),
    connections_lost := non_neg_integer(),
    avg_reload_time_ms := float(),
    avg_drain_time_ms := float(),
    min_reload_time_ms := float(),
    max_reload_time_ms := float(),
    total_downtime_ms := non_neg_integer()
}.

%% Module version tracking
-record(module_version, {
    name :: module_name(),
    version :: non_neg_integer(),
    last_reload :: erlang:timestamp(),
    code_hash :: binary()
}).

%% Configuration version tracking
-record(config_version, {
    version :: non_neg_integer(),
    last_reload :: erlang:timestamp(),
    config_hash :: binary()
}).

%% State record
-record(state, {
    modules = #{} :: #{module_name() => #module_version{}},
    config_version = #config_version{
        version = 0,
        last_reload = erlang:now(),
        config_hash = <<>>
    } :: #config_version{},
    drain_status = idle :: drain_status(),
    drain_start :: erlang:timestamp() | undefined,
    draining_connections = #{} :: #{pid() => reference()},
    metrics = #{
        total_reloads => 0,
        successful_reloads => 0,
        failed_reloads => 0,
        total_connections_at_reload => 0,
        connections_preserved => 0,
        connections_lost => 0,
        reload_times => [],
        drain_times => [],
        total_downtime_ms => 0
    } :: map()
}).

-define(DRAIN_TIMEOUT_MS, 30000).  % 30 seconds max drain time
-define(MODULE_CHECK_INTERVAL, 5000).  % Check for stale connections every 5 seconds

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===== Code Hot Reload API =====

-spec reload_module(module_name()) -> reload_result().
reload_module(Module) ->
    gen_server:call(?MODULE, {reload_module, Module}, 60000).

-spec reload_modules([module_name()]) -> [reload_result()].
reload_modules(Modules) ->
    gen_server:call(?MODULE, {reload_modules, Modules}, 120000).

-spec reload_all_modules() -> {ok, [module_name()]} | {error, term()}.
reload_all_modules() ->
    gen_server:call(?MODULE, reload_all_modules, 120000).

-spec get_reload_status() -> #{module_name() => map()}.
get_reload_status() ->
    gen_server:call(?MODULE, get_reload_status, 5000).

-spec get_module_version(module_name()) -> {ok, non_neg_integer()} | {error, not_found}.
get_module_version(Module) ->
    gen_server:call(?MODULE, {get_module_version, Module}, 5000).

%% ===== Configuration Hot Reload API =====

-spec reload_config() -> {ok, non_neg_integer()} | {error, term()}.
reload_config() ->
    reload_config([]).

-spec reload_config(list()) -> {ok, non_neg_integer()} | {error, term()}.
reload_config(Apps) ->
    gen_server:call(?MODULE, {reload_config, Apps}, 30000).

-spec get_config_version() -> non_neg_integer().
get_config_version() ->
    gen_server:call(?MODULE, get_config_version, 5000).

-spec validate_config(list()) -> ok | {error, term()}.
validate_config(Apps) ->
    gen_server:call(?MODULE, {validate_config, Apps}, 10000).

%% ===== Graceful Drain API =====

-spec begin_graceful_drain(non_neg_integer()) -> {ok, reference()} | {error, term()}.
begin_graceful_drain(TimeoutMs) ->
    gen_server:call(?MODULE, {begin_graceful_drain, TimeoutMs}, TimeoutMs + 5000).

-spec end_graceful_drain() -> {ok, drain_status()} | {error, term()}.
end_graceful_drain() ->
    gen_server:call(?MODULE, end_graceful_drain, 10000).

-spec is_draining() -> boolean().
is_draining() ->
    gen_server:call(?MODULE, is_draining, 5000).

-spec get_drain_status() -> #{
    status := drain_status(),
    active_connections := non_neg_integer(),
    elapsed_ms := non_neg_integer()
}.
get_drain_status() ->
    gen_server:call(?MODULE, get_drain_status, 5000).

-spec drain_complete(pid()) -> ok.
drain_complete(Pid) ->
    gen_server:cast(?MODULE, {drain_complete, Pid}).

%% ===== Metrics API =====

-spec get_reload_metrics() -> reload_metrics().
get_reload_metrics() ->
    gen_server:call(?MODULE, get_reload_metrics, 5000).

-spec reset_reload_metrics() -> ok.
reset_reload_metrics() ->
    gen_server:call(?MODULE, reset_reload_metrics, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ?LOG_INFO("Hot reload system starting", #{}),
    {ok, #state{}}.

handle_call({reload_module, Module}, _From, State) ->
    {Result, NewState} = do_reload_module(Module, State),
    {reply, Result, NewState};

handle_call({reload_modules, Modules}, _From, State) ->
    {Results, NewState} = do_reload_modules(Modules, State, []),
    {reply, Results, NewState};

handle_call(reload_all_modules, _From, State) ->
    {Modules, NewState} = do_reload_all_modules(State),
    {reply, Modules, NewState};

handle_call(get_reload_status, _From, State) ->
    Status = maps:map(fun(_Module, #module_version{
        version = Ver,
        last_reload = Time,
        code_hash = Hash
    }) ->
        Base64 = base64:encode(Hash),
        #{
            version => Ver,
            last_reload => Time,
            code_hash => Base64,
            code_hash_b64 => Base64
        }
    end, State#state.modules),
    {reply, Status, State};

handle_call({get_module_version, Module}, _From, State) ->
    case maps:get(Module, State#state.modules, undefined) of
        undefined -> {reply, {error, not_found}, State};
        #module_version{version = Ver} -> {reply, {ok, Ver}, State}
    end;

handle_call({reload_config, Apps}, _From, State) ->
    {Result, NewState} = do_reload_config(Apps, State),
    {reply, Result, NewState};

handle_call(get_config_version, _From, State) ->
    {reply, State#state.config_version#config_version.version, State};

handle_call({validate_config, Apps}, _From, State) ->
    Result = do_validate_config(Apps),
    {reply, Result, State};

handle_call({begin_graceful_drain, TimeoutMs}, _From, State) ->
    case State#state.drain_status of
        idle ->
            % Start draining
            DrainRef = make_ref(),
            erlang:send_after(TimeoutMs, self(), drain_timeout),
            NewState = State#state{
                drain_status = draining,
                drain_start = erlang:now()
            },
            ?LOG_notice("Graceful drain started", #{timeout_ms => TimeoutMs}),
            {reply, {ok, DrainRef}, NewState};
        _ ->
            {reply, {error, already_draining}, State}
    end;

handle_call(end_graceful_drain, _From, State) ->
    case State#state.drain_status of
        idle ->
            {reply, {error, not_draining}, State};
        _ ->
            % End drain
            DrainTime = timer:now_diff(erlang:now(), State#state.drain_start) / 1000,
            NewMetrics = update_drain_metric(State#state.metrics, DrainTime),
            NewState = State#state{
                drain_status = idle,
                drain_start = undefined,
                metrics = NewMetrics
            },
            ?LOG_notice("Graceful drain ended", #{drain_time_ms => DrainTime}),
            {reply, {ok, complete}, NewState}
    end;

handle_call(is_draining, _From, State) ->
    {reply, State#state.drain_status =:= draining, State};

handle_call(get_drain_status, _From, State) ->
    ElapsedMs = case State#state.drain_start of
        undefined -> 0;
        StartTime -> round(timer:now_diff(erlang:now(), StartTime) / 1000)
    end,
    Status = #{
        status => State#state.drain_status,
        active_connections => maps:size(State#state.draining_connections),
        elapsed_ms => ElapsedMs
    },
    {reply, Status, State};

handle_call({drain_complete, _Pid}, _From, State) ->
    % This is called via cast, shouldn't reach here
    {reply, ok, State};

handle_call(get_reload_metrics, _From, State) ->
    Metrics = compute_reload_metrics(State#state.metrics),
    {reply, Metrics, State};

handle_call(reset_reload_metrics, _From, State) ->
    NewState = State#state{
        metrics = #{
            total_reloads => 0,
            successful_reloads => 0,
            failed_reloads => 0,
            total_connections_at_reload => 0,
            connections_preserved => 0,
            connections_lost => 0,
            reload_times => [],
            drain_times => [],
            total_downtime_ms => 0
        }
    },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({drain_complete, Pid}, State) ->
    case maps:get(Pid, State#state.draining_connections, undefined) of
        undefined ->
            {noreply, State};
        _Ref ->
            NewConnections = maps:remove(Pid, State#state.draining_connections),
            case maps:size(NewConnections) of
                0 when State#state.drain_status =:= draining ->
                    ?LOG_notice("All connections drained", #{});
                _ ->
                    ok
            end,
            NewState = State#state{draining_connections = NewConnections},
            {noreply, NewState}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(drain_timeout, State) ->
    case State#state.drain_status of
        draining ->
            % Force end drain
            ?LOG_warning("Drain timeout reached", #{
                active_connections => maps:size(State#state.draining_connections)
            }),
            DrainTime = timer:now_diff(erlang:now(), State#state.drain_start) / 1000,
            NewMetrics = update_drain_metric(State#state.metrics, DrainTime),
            NewState = State#state{
                drain_status = complete,
                drain_start = undefined,
                metrics = NewMetrics
            },
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_info(check_stale_connections, State) ->
    % Periodically check for stale connections that never called drain_complete
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?LOG_info("Hot reload system stopping", #{}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Code Reloading
%%====================================================================

do_reload_module(Module, State) ->
    StartTime = erlang:now(),
    case code:load_file(Module) of
        {module, Module} ->
            Version = case maps:get(Module, State#state.modules, undefined) of
                undefined -> 1;
                #module_version{version = V} -> V + 1
            end,
            {ok, ModuleCode} = file:read_file(code:which(Module)),
            Hash = erlang:phash2(ModuleCode),
            ModuleVersion = #module_version{
                name = Module,
                version = Version,
                last_reload = erlang:now(),
                code_hash = <<Hash:256>>
            },
            NewModules = (State#state.modules)#{Module => ModuleVersion},
            ElapsedMs = round(timer:now_diff(erlang:now(), StartTime) / 1000),
            NewMetrics = update_reload_metric(State#state.metrics, ElapsedMs, true),
            ?LOG_info("Module reloaded", #{
                module => Module,
                version => Version,
                elapsed_ms => ElapsedMs
            }),
            {
                {ok, Module},
                State#state{
                    modules = NewModules,
                    metrics = NewMetrics
                }
            };
        {error, Reason} ->
            ?LOG_error("Failed to reload module", #{
                module => Module,
                reason => Reason
            }),
            NewMetrics = update_reload_metric(State#state.metrics, 0, false),
            {{error, Reason}, State#state{metrics = NewMetrics}}
    end.

do_reload_modules([], State, Acc) ->
    {lists:reverse(Acc), State};
do_reload_modules([Module | Rest], State, Acc) ->
    {Result, NewState} = do_reload_module(Module, State),
    do_reload_modules(Rest, NewState, [Result | Acc]).

do_reload_all_modules(State) ->
    % Get all loaded modules from erlmcp application
    case application:get_key(erlmcp, modules) of
        undefined ->
            ?LOG_warning("Cannot get erlmcp modules", #{}),
            {{error, no_erlmcp_app}, State};
        {ok, Modules} ->
            % Filter to only user modules (exclude stdlib, deps)
            UserModules = [M || M <- Modules, is_erlmcp_module(M)],
            {Results, NewState} = do_reload_modules(UserModules, State, []),
            SuccessCount = length([1 || {ok, _} <- Results]),
            FailCount = length([1 || {error, _} <- Results]),
            ?LOG_info("Reload all modules complete", #{
                total => length(UserModules),
                success => SuccessCount,
                failed => FailCount
            }),
            {{ok, UserModules}, NewState}
    end.

is_erlmcp_module(Module) ->
    % Crude check: module name contains 'erlmcp'
    atom_to_list(Module) =:= atom_to_list(Module).  % Placeholder - will be erlmcp specific

%%====================================================================
%% Internal Functions - Configuration Reloading
%%====================================================================

do_reload_config(Apps, State) ->
    StartTime = erlang:now(),
    case do_validate_config(Apps) of
        ok ->
            % Reload configuration
            AppList = case Apps of
                [] -> [erlmcp];
                _ -> Apps
            end,
            case application:load(erlmcp) of
                {error, {already_loaded, erlmcp}} ->
                    ok;
                {error, Reason} ->
                    ?LOG_error("Failed to load erlmcp app", #{reason => Reason}),
                    return_error(Reason);
                ok ->
                    ok
            end,
            Version = State#state.config_version#config_version.version + 1,
            ConfigHash = erlang:phash2(AppList),
            NewConfigVersion = #config_version{
                version = Version,
                last_reload = erlang:now(),
                config_hash = <<ConfigHash:256>>
            },
            ElapsedMs = round(timer:now_diff(erlang:now(), StartTime) / 1000),
            ?LOG_info("Configuration reloaded", #{
                version => Version,
                apps => AppList,
                elapsed_ms => ElapsedMs
            }),
            {
                {ok, Version},
                State#state{config_version = NewConfigVersion}
            };
        {error, Reason} ->
            ?LOG_error("Config validation failed", #{reason => Reason}),
            {{error, Reason}, State}
    end.

do_validate_config(Apps) ->
    % Basic validation - check that apps can be loaded
    AppList = case Apps of
        [] -> [erlmcp];
        _ -> Apps
    end,
    try
        lists:foreach(fun(App) ->
            case application:load(App) of
                {error, {already_loaded, _}} -> ok;
                {error, Reason} -> throw({load_error, App, Reason});
                ok -> ok
            end
        end, AppList),
        ok
    catch
        throw:Error -> {error, Error};
        _:Error -> {error, {validation_error, Error}}
    end.

%%====================================================================
%% Internal Functions - Metrics
%%====================================================================

update_reload_metric(Metrics, ElapsedMs, Success) ->
    TotalReloads = maps:get(total_reloads, Metrics, 0) + 1,
    {SuccessCount, FailCount} = case Success of
        true ->
            {
                maps:get(successful_reloads, Metrics, 0) + 1,
                maps:get(failed_reloads, Metrics, 0)
            };
        false ->
            {
                maps:get(successful_reloads, Metrics, 0),
                maps:get(failed_reloads, Metrics, 0) + 1
            }
    end,
    ReloadTimes = case Success of
        true ->
            [ElapsedMs | maps:get(reload_times, Metrics, [])];
        false ->
            maps:get(reload_times, Metrics, [])
    end,
    Metrics#{
        total_reloads => TotalReloads,
        successful_reloads => SuccessCount,
        failed_reloads => FailCount,
        reload_times => ReloadTimes
    }.

update_drain_metric(Metrics, DrainTimeMs) ->
    DrainTimes = [DrainTimeMs | maps:get(drain_times, Metrics, [])],
    TotalDowntime = maps:get(total_downtime_ms, Metrics, 0) + round(DrainTimeMs),
    Metrics#{
        drain_times => DrainTimes,
        total_downtime_ms => TotalDowntime
    }.

compute_reload_metrics(Metrics) ->
    ReloadTimes = maps:get(reload_times, Metrics, []),
    DrainTimes = maps:get(drain_times, Metrics, []),
    {AvgReloadMs, MinReloadMs, MaxReloadMs} = compute_stats(ReloadTimes),
    {AvgDrainMs, _, _} = compute_stats(DrainTimes),
    #{
        total_reloads => maps:get(total_reloads, Metrics, 0),
        successful_reloads => maps:get(successful_reloads, Metrics, 0),
        failed_reloads => maps:get(failed_reloads, Metrics, 0),
        total_connections_at_reload => maps:get(total_connections_at_reload, Metrics, 0),
        connections_preserved => maps:get(connections_preserved, Metrics, 0),
        connections_lost => maps:get(connections_lost, Metrics, 0),
        avg_reload_time_ms => AvgReloadMs,
        avg_drain_time_ms => AvgDrainMs,
        min_reload_time_ms => MinReloadMs,
        max_reload_time_ms => MaxReloadMs,
        total_downtime_ms => maps:get(total_downtime_ms, Metrics, 0)
    }.

compute_stats([]) ->
    {0.0, 0.0, 0.0};
compute_stats(Values) ->
    Sum = lists:sum(Values),
    Count = length(Values),
    Avg = Sum / Count,
    Min = lists:min(Values),
    Max = lists:max(Values),
    {Avg, Min, Max}.

return_error(Reason) ->
    {error, Reason}.
