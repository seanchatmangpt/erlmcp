%%%-------------------------------------------------------------------
%%% @doc
%%% Core Health Check Module for erlmcp v3
%%%
%%% Comprehensive health check aggregator for core erlmcp services.
%%% Following Joe Armstrong's principle: "Health checks are for orchestration"
%%%
%%% Provides health status for Kubernetes and other orchestrators:
%%%   - /health  - Comprehensive health (all dependencies)
%%%   - /ready   - Readiness probe (ready to serve traffic)
%%%   - /live    - Liveness probe (process is alive)
%%%
%%% Health checks include:
%%%   - Database connectivity (PostgreSQL, MySQL, Mnesia)
%%%   - Distributed node connectivity (cluster membership)
%%%   - Memory usage (system, process, binary)
%%%   - Core services (registry, session manager, transports)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_health).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([check/0, check/1]).
-export([check_database/0, check_cluster/0, check_memory/0]).
-export([check_ready/0, check_live/0]).
-export([get_status/0]).
-export([register_check/2, unregister_check/1]).
-export([get_http_status/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type check_name() :: atom().
-type check_result() :: healthy | degraded | unhealthy.
-type check_fun() :: {module(), atom(), [term()]}.
-type health_report() :: #{
    status := healthy | degraded | unhealthy,
    healthy := boolean(),
    checks := #{check_name() := check_result()},
    timestamp => integer(),
    node => node()
}.
-type http_status() :: 200 | 503.

%% Health check timeouts (milliseconds)
-define(DB_CHECK_TIMEOUT, 2000).
-define(CLUSTER_CHECK_TIMEOUT, 2000).
-define(MEMORY_CHECK_TIMEOUT, 1000).
-define(DEFAULT_CHECK_TIMEOUT, 1000).

%% Health thresholds
-define(MEMORY_WARNING_THRESHOLD, 0.80).    % 80% memory usage
-define(MEMORY_CRITICAL_THRESHOLD, 0.90).   % 90% memory usage

-record(state, {
    checks :: #{check_name() => check_fun()},
    last_check_time :: integer() | undefined,
    last_check_result :: health_report() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start health check server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Perform all health checks and return overall status
%% Returns #{status => healthy|degraded|unhealthy, healthy => boolean(), checks => map()}
-spec check() -> health_report().
check() ->
    check(all).

%% @doc Perform specific health check category
-spec check(all | database | cluster | memory) -> health_report().
check(Category) ->
    gen_server:call(?MODULE, {check, Category}, ?DEFAULT_CHECK_TIMEOUT + 500).

%% @doc Check database connectivity
%% Returns #{status => healthy|degraded|unhealthy, details => map()}
-spec check_database() -> #{status := check_result(), details := map()}.
check_database() ->
    Checks = [
        {mnesia, fun check_mnesia/0},
        {pool, fun check_db_pool/0}
    ],
    run_category_checks(Checks).

%% @doc Check cluster/distributed node connectivity
%% Returns #{status => healthy|degraded|unhealthy, details => map()}
-spec check_cluster() -> #{status := check_result(), details := map()}.
check_cluster() ->
    Checks = [
        {nodes, fun check_cluster_nodes/0},
        {membership, fun check_cluster_membership/0},
        {connectivity, fun check_node_connectivity/0}
    ],
    run_category_checks(Checks).

%% @doc Check memory usage
%% Returns #{status => healthy|degraded|unhealthy, details => map()}
-spec check_memory() -> #{status := check_result(), details := map()}.
check_memory() ->
    Checks = [
        {system, fun check_system_memory/0},
        {processes, fun check_process_memory/0},
        {binary, fun check_binary_memory/0}
    ],
    run_category_checks(Checks).

%% @doc Check if service is ready to serve traffic
-spec check_ready() -> #{ready := boolean(), checks := map()}.
check_ready() ->
    ReadyChecks = [
        {registry, fun() -> check_process_ready(erlmcp_registry) end},
        {session_manager, fun() -> check_process_ready(erlmcp_session_manager) end}
    ],
    Results = lists:map(fun({Name, CheckFun}) ->
        Status = try CheckFun() of
            true -> ready;
            false -> not_ready
        catch
            _:_ -> not_ready
        end,
        {Name, Status}
    end, ReadyChecks),
    AllReady = lists:all(fun({_, S}) -> S =:= ready end, Results),
    #{
        ready => AllReady,
        checks => maps:from_list(Results)
    }.

%% @doc Check if service is alive (liveness probe)
-spec check_live() -> #{alive := boolean(), node => node(), uptime => non_neg_integer()}.
check_live() ->
    #{
        alive => true,
        node => node(),
        uptime => get_uptime()
    }.

%% @doc Get overall health status with HTTP status code
-spec get_status() -> {http_status(), health_report()}.
get_status() ->
    Report = check(),
    Status = get_http_status(Report),
    {Status, Report}.

%% @doc Get HTTP status code from health report
-spec get_http_status(health_report() | #{status := check_result()}) -> http_status().
get_http_status(#{status := healthy}) -> 200;
get_http_status(#{status := degraded}) -> 200;  % Degraded but serving
get_http_status(#{status := unhealthy}) -> 503;
get_http_status(#{healthy := true}) -> 200;
get_http_status(#{healthy := false}) -> 503.

%% @doc Register a health check function
%% CheckFun is {Module, Function, Args} - returns ok | {ok, _} | {error, _}
-spec register_check(check_name(), check_fun()) -> ok.
register_check(Name, CheckFun) ->
    gen_server:call(?MODULE, {register, Name, CheckFun}).

%% @doc Unregister a health check function
-spec unregister_check(check_name()) -> ok.
unregister_check(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),

    %% Register default checks for core services
    %% These checks verify that critical gen_servers are running
    DefaultChecks =
        #{registry => {erlmcp_registry, get_pid, []},
          session_manager => {erlmcp_session_manager, list_sessions, []}},

    logger:info("Starting health check server with ~p default checks", [maps:size(DefaultChecks)]),

    {ok, #state{checks = DefaultChecks}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call({check, Category}, _From, State) ->
    %% Run health checks based on category
    Report = case Category of
        all ->
            Results = maps:map(fun(_Name, {M, F, A}) -> run_check(M, F, A) end, State#state.checks),
            %% Add category checks
            DatabaseStatus = check_database_safe(),
            ClusterStatus = check_cluster_safe(),
            MemoryStatus = check_memory_safe(),
            AllChecks = maps:merge(Results, #{
                database => maps:get(status, DatabaseStatus),
                cluster => maps:get(status, ClusterStatus),
                memory => maps:get(status, MemoryStatus)
            }),
            %% Determine overall status
            OverallStatus = determine_overall_status(AllChecks),
            #{
                status => OverallStatus,
                healthy => OverallStatus =:= healthy orelse OverallStatus =:= degraded,
                checks => AllChecks,
                timestamp => erlang:system_time(millisecond),
                node => node(),
                details => #{
                    database => DatabaseStatus,
                    cluster => ClusterStatus,
                    memory => MemoryStatus
                }
            };
        database ->
            DatabaseStatus = check_database(),
            Status = maps:get(status, DatabaseStatus),
            #{
                status => Status,
                healthy => Status =:= healthy orelse Status =:= degraded,
                checks => #{database => Status},
                timestamp => erlang:system_time(millisecond),
                node => node(),
                details => #{database => DatabaseStatus}
            };
        cluster ->
            ClusterStatus = check_cluster(),
            Status = maps:get(status, ClusterStatus),
            #{
                status => Status,
                healthy => Status =:= healthy orelse Status =:= degraded,
                checks => #{cluster => Status},
                timestamp => erlang:system_time(millisecond),
                node => node(),
                details => #{cluster => ClusterStatus}
            };
        memory ->
            MemoryStatus = check_memory(),
            Status = maps:get(status, MemoryStatus),
            #{
                status => Status,
                healthy => Status =:= healthy orelse Status =:= degraded,
                checks => #{memory => Status},
                timestamp => erlang:system_time(millisecond),
                node => node(),
                details => #{memory => MemoryStatus}
            }
    end,

    %% Update state
    NewState = State#state{
        last_check_time = erlang:system_time(millisecond),
        last_check_result = Report
    },

    {reply, Report, NewState};

handle_call(check, _From, State) ->
    %% Legacy API - perform all checks
    {reply, element(2, handle_call({check, all}, _From, State)), State};

handle_call({register, Name, CheckFun}, _From, State) ->
    NewChecks = maps:put(Name, CheckFun, State#state.checks),
    logger:info("Registered health check: ~p", [Name]),
    {reply, ok, State#state{checks = NewChecks}};

handle_call({unregister, Name}, _From, State) ->
    NewChecks = maps:remove(Name, State#state.checks),
    logger:info("Unregistered health check: ~p", [Name]),
    {reply, ok, State#state{checks = NewChecks}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    logger:info("Health check server terminating"),
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions - Core Health Checks
%%%===================================================================

%% @doc Run a single health check with timeout
%% Returns healthy if process is alive and responding
%% Returns unhealthy if process is dead or not responding
-spec run_check(module(), atom(), [term()]) -> check_result().
run_check(M, F, A) ->
    try
        Timeout = case {M, F} of
            {erlmcp_db_connection, _} -> ?DB_CHECK_TIMEOUT;
            {erlmcp_cluster_membership, _} -> ?CLUSTER_CHECK_TIMEOUT;
            {erlmcp_memory_monitor, _} -> ?MEMORY_CHECK_TIMEOUT;
            _ -> ?DEFAULT_CHECK_TIMEOUT
        end,
        Result = case Timeout of
            infinity ->
                apply(M, F, A);
            T ->
                %% Apply with timeout to prevent hanging checks
                try
                    apply(M, F, A)
                catch
                    error:{timeout, _} ->
                        logger:warning("Health check timeout for ~p:~p/~p", [M, F, length(A)]),
                        error(unhealthy)
                end
        end,
        interpret_check_result(Result)
    catch
        %% Exception means service is unhealthy
        _:{badarg, _} ->
            logger:debug("Health check badarg for ~p:~p/~p (module/function not available)", [M, F, length(A)]),
            degraded;
        _:{noproc, _} ->
            logger:debug("Health check noproc for ~p:~p/~p (process not found)", [M, F, length(A)]),
            degraded;
        error:{timeout, _} ->
            logger:warning("Health check timeout for ~p:~p/~p", [M, F, length(A)]),
            unhealthy;
        Class:Reason:Stack ->
            logger:warning("Health check failed for ~p:~p/~p: ~p:~p", [M, F, length(A), Class, Reason]),
            logger:debug("Stack: ~p", [Stack]),
            unhealthy
    end.

%% @doc Interpret the result of a health check
-spec interpret_check_result(term()) -> check_result().
interpret_check_result(Result) when Result =:= ok; Result =:= [] ->
    %% No errors and empty results are considered healthy
    healthy;
interpret_check_result({ok, _}) ->
    healthy;
interpret_check_result({ready, _}) ->
    healthy;
interpret_check_result([_ | _]) ->
    %% Non-empty list results are healthy (e.g., list_sessions)
    healthy;
interpret_check_result(Atom) when is_atom(Atom) ->
    %% Atom results (e.g., node(), undefined) are healthy if not error atom
    case Atom of
        undefined -> degraded;
        error -> unhealthy;
        false -> degraded;
        true -> healthy;
        ready -> healthy;
        not_ready -> degraded;
        _ when Atom =:= healthy; Atom =:= degraded; Atom =:= unhealthy -> Atom;
        _ -> healthy
    end;
interpret_check_result(Int) when is_integer(Int) ->
    %% Integer results (e.g., timestamps, counts) are healthy
    healthy;
interpret_check_result({Total, _}) when is_integer(Total) ->
    %% Tuple with integer first element (e.g., {ok, Count})
    healthy;
interpret_check_result(Tuple) when is_tuple(Tuple) ->
    %% Tuple results (e.g., timestamps, MFA results)
    %% unless they're error tuples
    case Tuple of
        {error, _} -> unhealthy;
        {exception, _} -> unhealthy;
        {exit, _} -> unhealthy;
        {throw, _} -> unhealthy;
        _ -> healthy
    end;
interpret_check_result(Pid) when is_pid(Pid) ->
    %% Pid result - check if alive
    case erlang:is_process_alive(Pid) of
        true -> healthy;
        false -> unhealthy
    end;
interpret_check_result(Map) when is_map(Map) ->
    %% Map results - check for status key
    case maps:get(status, Map, undefined) of
        undefined -> healthy;
        Status -> Status
    end;
interpret_check_result(_) ->
    degraded.

%% @doc Determine overall health status from all check results
-spec determine_overall_status(map()) -> check_result().
determine_overall_status(Checks) ->
    Results = maps:values(Checks),
    HasUnhealthy = lists:member(unhealthy, Results),
    HasDegraded = lists:member(degraded, Results),
    case {HasUnhealthy, HasDegraded} of
        {true, _} -> unhealthy;
        {false, true} -> degraded;
        _ -> healthy
    end.

%%%===================================================================
%%% Internal functions - Database Health Checks
%%%===================================================================

%% @doc Safe database check (catches errors)
-spec check_database_safe() -> #{status := check_result(), details => map()}.
check_database_safe() ->
    try
        check_database()
    catch
        _:_ ->
            #{status => unhealthy, details => #{error => database_check_failed}}
    end.

%% @doc Check Mnesia database health
-spec check_mnesia() -> {ok, check_result(), map()} | {error, term()}.
check_mnesia() ->
    try
        case application:which_applications() of
            Apps when is_list(Apps) ->
                MnesiaRunning = lists:keymember(mnesia, 1, Apps),
                if
                    MnesiaRunning ->
                        %% Check Mnesia tables
                        Tables = mnesia:system_info(tables),
                        %% Check if critical tables exist and are accessible
                        CriticalTables = [erlmcp_persistent_sessions],
                        TableStatus = lists:map(fun(Table) ->
                            case lists:member(Table, Tables) of
                                true ->
                                    try
                                        Info = mnesia:table_info(Table, where_to_read),
                                        {Table, healthy, #{location => Info}}
                                    catch
                                        _:_ -> {Table, degraded, #{reason => no_info}}
                                    end;
                                false ->
                                    {Table, degraded, #{reason => not_found}}
                            end
                        end, CriticalTables),
                        %% Overall status
                        AllHealthy = lists:all(fun({_, S, _}) -> S =:= healthy end, TableStatus),
                        Status = case AllHealthy of
                            true -> healthy;
                            false -> degraded
                        end,
                        {ok, Status, #{tables => maps:from_list(TableStatus)}};
                    true ->
                        %% Mnesia not running - degraded but not critical for all deployments
                        {ok, degraded, #{reason => mnesia_not_running}}
                end
        end
    catch
        _:_ ->
            {error, mnesia_check_failed}
    end.

%% @doc Check database pool health
-spec check_db_pool() -> {ok, check_result(), map()} | {error, term()}.
check_db_pool() ->
    try
        case whereis(erlmcp_db_pool) of
            undefined ->
                %% Pool not started - degraded but not critical
                {ok, degraded, #{reason => pool_not_started}};
            Pid when is_pid(Pid) ->
                case is_process_alive(Pid) of
                    true ->
                        %% Try to get a connection and run health check
                        case poolboy:checkout(erlmcp_db_pool, false) of
                            full ->
                                {ok, degraded, #{reason => pool_exhausted}};
                            Worker when is_pid(Worker) ->
                                try
                                    case erlmcp_db_connection:health_check(Worker) of
                                        {ok, _} ->
                                            poolboy:checkin(erlmcp_db_pool, Worker),
                                            {ok, healthy, #{pool => active}};
                                        {error, Reason} ->
                                            poolboy:checkin(erlmcp_db_pool, Worker),
                                            {ok, unhealthy, #{reason => Reason}}
                                    end
                                catch
                                    _:_ ->
                                        poolboy:checkin(erlmcp_db_pool, Worker),
                                        {ok, unhealthy, #{reason => health_check_failed}}
                                end;
                            _ ->
                                {ok, degraded, #{reason => no_available_worker}}
                        end;
                    false ->
                        {ok, unhealthy, #{reason => pool_dead}}
                end
        end
    catch
        _:{noproc, _} ->
            {ok, degraded, #{reason => poolboy_not_available}};
        _:_ ->
            {error, db_pool_check_failed}
    end.

%% @doc Run category checks and aggregate results
-spec run_category_checks([{atom(), fun(() -> {ok, check_result(), map()} | {error, term()})}]) ->
    #{status := check_result(), details := map()}.
run_category_checks(Checks) ->
    Results = lists:map(fun({Name, CheckFun}) ->
        Result = try
            Timeout = case Name of
                mnesia -> ?DB_CHECK_TIMEOUT;
                pool -> ?DB_CHECK_TIMEOUT;
                _ -> ?DEFAULT_CHECK_TIMEOUT
            end,
            {Pid, Mref} = spawn_monitor(CheckFun),
            receive
                {Pid, {ok, Status, Details}} ->
                    erlang:demonitor(Mref, [flush]),
                    {Name, Status, Details};
                {Pid, {error, Reason}} ->
                    erlang:demonitor(Mref, [flush]),
                    {Name, unhealthy, #{error => Reason}};
                {'DOWN', Mref, process, _, Reason} ->
                    {Name, unhealthy, #{reason => Reason}}
            after Timeout ->
                erlang:demonitor(Mref, [flush]),
                exit(Pid, kill),
                {Name, unhealthy, #{reason => timeout}}
            end
        catch
            _:_ ->
                {Name, unhealthy, #{error => check_failed}}
        end
    end, Checks),
    %% Determine overall status
    Status = case lists:all(fun({_, S, _}) -> S =:= healthy end, Results) of
        true ->
            healthy;
        false ->
            case lists:member(unhealthy, [S || {_, S, _} <- Results]) of
                true -> unhealthy;
                false -> degraded
            end
    end,
    #{
        status => Status,
        details => maps:from_list([{N, D} || {N, _, D} <- Results])
    }.

%%%===================================================================
%%% Internal functions - Cluster Health Checks
%%%===================================================================

%% @doc Safe cluster check (catches errors)
-spec check_cluster_safe() -> #{status := check_result(), details => map()}.
check_cluster_safe() ->
    try
        check_cluster()
    catch
        _:_ ->
            #{status => unhealthy, details => #{error => cluster_check_failed}}
    end.

%% @doc Check cluster nodes
-spec check_cluster_nodes() -> {ok, check_result(), map()}.
check_cluster_nodes() ->
    try
        %% Get configured cluster nodes
        ClusterNodes = case application:get_env(erlmcp_core, cluster_nodes) of
            {ok, Nodes} when is_list(Nodes) -> Nodes;
            _ -> []
        end,
        %% Check connectivity to configured nodes
        NodeStatus = lists:map(fun(Node) ->
            case net_adm:ping(Node) of
                pong ->
                    {Node, up};
                pang ->
                    {Node, down}
            end
        end, ClusterNodes),
        %% Determine status
        UpCount = length([N || {N, up} <- NodeStatus]),
        TotalCount = length(NodeStatus),
        Status = if
            TotalCount =:= 0 ->
                %% No cluster configured - healthy (single node)
                healthy;
            UpCount =:= TotalCount ->
                healthy;
            UpCount > 0 ->
                degraded;
            true ->
                unhealthy
        end,
        {ok, Status, #{
            nodes => maps:from_list(NodeStatus),
            up_count => UpCount,
            total_count => TotalCount
        }}
    catch
        _:_ -> {error, cluster_nodes_check_failed}
    end.

%% @doc Check cluster membership
-spec check_cluster_membership() -> {ok, check_result(), map()}.
check_cluster_membership() ->
    try
        case whereis(erlmcp_cluster_membership) of
            undefined ->
                {ok, degraded, #{reason => membership_not_running}};
            Pid when is_pid(Pid) ->
                case is_process_alive(Pid) of
                    true ->
                        case erlmcp_cluster_membership:get_members() of
                            {ok, Members} ->
                                MemberCount = length(Members),
                                {ok, healthy, #{
                                    member_count => MemberCount,
                                    members => Members
                                }};
                            {error, Reason} ->
                                {ok, unhealthy, #{error => Reason}}
                        end;
                    false ->
                        {ok, unhealthy, #{reason => membership_dead}}
                end
        end
    catch
        _:_ -> {error, cluster_membership_check_failed}
    end.

%% @doc Check node connectivity (EPMD and distribution)
-spec check_node_connectivity() -> {ok, check_result(), map()}.
check_node_connectivity() ->
    try
        %% Check if EPMD is reachable
        EpmdStatus = case inet:gethostbyname("localhost") of
            {ok, _} -> true;
            _ -> false
        end,
        %% Check if distribution is enabled
        DistStatus = case node() of
            nonode@nohost -> false;
            _ -> true
        end,
        Status = case {EpmdStatus, DistStatus} of
            {true, true} -> healthy;
            {_, false} -> degraded;  % Distribution not enabled (ok for single node)
            {false, true} -> unhealthy  % Should have distribution but EPMD unreachable
        end,
        {ok, Status, #{
            epmd_reachable => EpmdStatus,
            distribution_enabled => DistStatus,
            node => node()
        }}
    catch
        _:_ -> {error, connectivity_check_failed}
    end.

%%%===================================================================
%%% Internal functions - Memory Health Checks
%%%===================================================================

%% @doc Safe memory check (catches errors)
-spec check_memory_safe() -> #{status := check_result(), details => map()}.
check_memory_safe() ->
    try
        check_memory()
    catch
        _:_ ->
            #{status => unhealthy, details => #{error => memory_check_failed}}
    end.

%% @doc Check system memory usage
-spec check_system_memory() -> {ok, check_result(), map()}.
check_system_memory() ->
    try
        TotalMem = erlang:memory(total),
        SystemMem = erlang:memory(system),
        ProcessMem = erlang:memory(processes),

        %% Get system limit if available from memory_monitor
        SystemLimit = try
            case whereis(erlmcp_memory_monitor) of
                undefined ->
                    %% Fallback to a reasonable default
                    16 * 1024 * 1024 * 1024;  % 16GB
                _ when is_pid(_) ->
                    %% Try to get stats from memory monitor
                    case erlmcp_memory_monitor:get_memory_stats() of
                        Stats when is_map(Stats) ->
                            %% Use system limit from stats or fallback
                            maps:get(system_limit, Stats, 16 * 1024 * 1024 * 1024);
                        _ ->
                            16 * 1024 * 1024 * 1024
                    end
            end
        catch
            _:_ -> 16 * 1024 * 1024 * 1024
        end,

        UsedPercent = TotalMem / SystemLimit,

        Status = if
            UsedPercent > ?MEMORY_CRITICAL_THRESHOLD -> unhealthy;
            UsedPercent > ?MEMORY_WARNING_THRESHOLD -> degraded;
            true -> healthy
        end,

        {ok, Status, #{
            total_bytes => TotalMem,
            system_bytes => SystemMem,
            processes_bytes => ProcessMem,
            used_percent => UsedPercent * 100,
            limit_bytes => SystemLimit
        }}
    catch
        _:_ -> {error, system_memory_check_failed}
    end.

%% @doc Check process memory usage
-spec check_process_memory() -> {ok, check_result(), map()}.
check_process_memory() ->
    try
        ProcessCount = erlang:system_info(process_count),
        ProcessLimit = erlang:system_info(process_limit),
        ProcessRatio = ProcessCount / ProcessLimit,

        Status = if
            ProcessRatio > ?MEMORY_CRITICAL_THRESHOLD -> unhealthy;
            ProcessRatio > ?MEMORY_WARNING_THRESHOLD -> degraded;
            true -> healthy
        end,

        {ok, Status, #{
            process_count => ProcessCount,
            process_limit => ProcessLimit,
            utilization_percent => ProcessRatio * 100
        }}
    catch
        _:_ -> {error, process_memory_check_failed}
    end.

%% @doc Check binary memory usage
-spec check_binary_memory() -> {ok, check_result(), map()}.
check_binary_memory() ->
    try
        BinaryMem = erlang:memory(binary),
        %% 100MB threshold for binary memory
        Status = if
            BinaryMem > 200 * 1024 * 1024 -> unhealthy;  % 200MB
            BinaryMem > 100 * 1024 * 1024 -> degraded;   % 100MB
            true -> healthy
        end,
        {ok, Status, #{
            binary_bytes => BinaryMem
        }}
    catch
        _:_ -> {error, binary_memory_check_failed}
    end.

%%%===================================================================
%%% Internal functions - Process Readiness Checks
%%%===================================================================

%% @doc Check if a process is ready (registered and alive)
-spec check_process_ready(atom()) -> boolean().
check_process_ready(ProcessName) ->
    case whereis(ProcessName) of
        undefined -> false;
        Pid when is_pid(Pid) -> is_process_alive(Pid)
    end.

%% @doc Get node uptime in seconds
-spec get_uptime() -> non_neg_integer().
get_uptime() ->
    case erlang:statistics(wall_clock) of
        {UpTimeMs, _} -> UpTimeMs div 1000;
        _ -> 0
    end.
