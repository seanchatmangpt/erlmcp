%%%-------------------------------------------------------------------
%%% @doc erlmcp_cluster_health - Cluster Health Monitoring
%%%
%%% Monitors cluster node connectivity, resource usage, and
%%% generates comprehensive health reports.
%%%
%%% Features:
%%% - Node connectivity monitoring
%%% - Resource usage tracking (memory, CPU, processes)
%%% - Health score calculation
%%% - Alert generation
%%% - Historical health data
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cluster_health).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         get_cluster_health/0,
         get_node_health/1,
         get_health_summary/0,
         get_health_history/0,
         set_alert_threshold/2,
         subscribe/0, unsubscribe/0,
         force_health_check/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_CHECK_INTERVAL, 10000).
-define(DEFAULT_HISTORY_SIZE, 100).
-define(DEFAULT_CPU_THRESHOLD, 80).  % %
-define(DEFAULT_MEMORY_THRESHOLD, 80).  % %
-define(DEFAULT_PROCESS_THRESHOLD, 100000).  % processes

%%====================================================================
%% Types
%%====================================================================

-type health_status() :: healthy | degraded | unhealthy | unknown.
-type alert_type() :: cpu_high | memory_high | process_high | node_down | partition.

-record(node_health, {
    node :: node(),
    status :: health_status(),
    cpu_utilization = 0 :: number(),
    memory_utilization = 0 :: number(),
    process_count = 0 :: non_neg_integer(),
    last_check :: integer(),
    consecutive_degraded = 0 :: non_neg_integer(),
    consecutive_unhealthy = 0 :: non_neg_integer()}).

-record(cluster_health, {
    timestamp :: integer(),
    nodes = #{} :: #{node() => #node_health{}},
    overall_status :: health_status(),
    total_nodes = 0 :: non_neg_integer(),
    healthy_nodes = 0 :: non_neg_integer(),
    degraded_nodes = 0 :: non_neg_integer(),
    unhealthy_nodes = 0 :: non_neg_integer()}).

-record(alert_threshold, {
    cpu = ?DEFAULT_CPU_THRESHOLD :: number(),
    memory = ?DEFAULT_MEMORY_THRESHOLD :: number(),
    processes = ?DEFAULT_PROCESS_THRESHOLD :: pos_integer()}).

-record(state, {
    health_history = [] :: [#cluster_health{}],
    history_size :: pos_integer(),
    check_interval :: pos_integer(),
    check_ref :: reference() | undefined,
    subscribers = [] :: [pid()],
    thresholds :: #alert_threshold{},
    current_health :: #cluster_health{}}).

-type state() :: #state{}.

-export_type([health_status/0, alert_type/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start health monitor with defaults
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start health monitor with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Get current cluster health
-spec get_cluster_health() -> #cluster_health{}.
get_cluster_health() ->
    gen_server:call(?SERVER, get_cluster_health).

%% @doc Get health of a specific node
-spec get_node_health(node()) -> {ok, #node_health{}} | {error, not_found}.
get_node_health(Node) ->
    gen_server:call(?SERVER, {get_node_health, Node}).

%% @doc Get health summary (simplified)
-spec get_health_summary() -> map().
get_health_summary() ->
    gen_server:call(?SERVER, get_health_summary).

%% @doc Get health history
-spec get_health_history() -> [#cluster_health{}].
get_health_history() ->
    gen_server:call(?SERVER, get_health_history).

%% @doc Set alert threshold
-spec set_alert_threshold(cpu | memory | processes, number()) -> ok.
set_alert_threshold(Type, Value) when is_number(Value), Value > 0 ->
    gen_server:call(?SERVER, {set_threshold, Type, Value}).

%% @doc Subscribe to health alerts
-spec subscribe() -> ok.
subscribe() ->
    gen_server:call(?SERVER, subscribe).

%% @doc Unsubscribe from health alerts
-spec unsubscribe() -> ok.
unsubscribe() ->
    gen_server:call(?SERVER, unsubscribe).

%% @doc Force immediate health check
-spec force_health_check() -> #cluster_health{}.
force_health_check() ->
    gen_server:call(?SERVER, force_health_check, 30000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map() | []) -> {ok, state()}.
init(Options) ->
    process_flag(trap_exit, true),

    CheckInterval = maps:get(check_interval, Options, ?DEFAULT_CHECK_INTERVAL),
    HistorySize = maps:get(history_size, Options, ?DEFAULT_HISTORY_SIZE),

    CpuThreshold = maps:get(cpu_threshold, Options, ?DEFAULT_CPU_THRESHOLD),
    MemThreshold = maps:get(memory_threshold, Options, ?DEFAULT_MEMORY_THRESHOLD),
    ProcThreshold = maps:get(process_threshold, Options, ?DEFAULT_PROCESS_THRESHOLD),

    Thresholds = #alert_threshold{
        cpu = CpuThreshold,
        memory = MemThreshold,
        processes = ProcThreshold
    },

    % Perform initial health check
    InitialHealth = check_cluster_health(Thresholds),

    % Start periodic checks
    CheckRef = erlang:send_after(CheckInterval, self(), periodic_check),

    ?LOG_INFO("Starting cluster health monitor: interval=~pms", [CheckInterval]),

    {ok, #state{
        health_history = [InitialHealth],
        history_size = HistorySize,
        check_interval = CheckInterval,
        check_ref = CheckRef,
        thresholds = Thresholds,
        current_health = InitialHealth
    }}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call(get_cluster_health, _From, State) ->
    {reply, State#state.current_health, State, hibernate};

handle_call({get_node_health, Node}, _From, State) ->
    Reply = case maps:get(Node, State#state.current_health#cluster_health.nodes, undefined) of
        undefined -> {error, not_found};
        NodeHealth -> {ok, NodeHealth}
    end,
    {reply, Reply, State, hibernate};

handle_call(get_health_summary, _From, State) ->
    Health = State#state.current_health,
    Summary = #{
        timestamp => Health#cluster_health.timestamp,
        overall_status => Health#cluster_health.overall_status,
        total_nodes => Health#cluster_health.total_nodes,
        healthy_nodes => Health#cluster_health.healthy_nodes,
        degraded_nodes => Health#cluster_health.degraded_nodes,
        unhealthy_nodes => Health#cluster_health.unhealthy_nodes,
        health_percentage => calculate_health_percentage(Health)
    },
    {reply, Summary, State, hibernate};

handle_call(get_health_history, _From, State) ->
    {reply, State#state.health_history, State, hibernate};

handle_call({set_threshold, Type, Value}, _From, State) ->
    NewThresholds = case Type of
        cpu -> State#state.thresholds#alert_threshold{cpu = Value};
        memory -> State#state.thresholds#alert_threshold{memory = Value};
        processes -> State#state.thresholds#alert_threshold{processes = trunc(Value)}
    end,
    {reply, ok, State#state{thresholds = NewThresholds}, hibernate};

handle_call(subscribe, {Pid, _Tag}, State) ->
    NewSubscribers = lists:usort([Pid | State#state.subscribers]),
    {reply, ok, State#state{subscribers = NewSubscribers}, hibernate};

handle_call(unsubscribe, {Pid, _Tag}, State) ->
    NewSubscribers = lists:delete(Pid, State#state.subscribers),
    {reply, ok, State#state{subscribers = NewSubscribers}, hibernate};

handle_call(force_health_check, _From, State) ->
    NewHealth = check_cluster_health(State#state.thresholds),
    NewState = State#state{current_health = NewHealth},
    {reply, NewHealth, NewState, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_info(periodic_check, State) ->
    NewHealth = check_cluster_health(State#state.thresholds),
    NewHistory = update_history(NewHealth, State),

    % Check for alerts
    check_alerts(State#state.current_health, NewHealth, State),

    % Schedule next check
    CheckRef = erlang:send_after(State#state.check_interval, self(), periodic_check),

    {noreply, State#state{
        current_health = NewHealth,
        health_history = NewHistory,
        check_ref = CheckRef
    }, hibernate};

handle_info({'EXIT', Pid, _Reason}, State) ->
    NewSubscribers = lists:delete(Pid, State#state.subscribers),
    {noreply, State#state{subscribers = NewSubscribers}, hibernate};

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    case State#state.check_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ?LOG_INFO("Cluster health monitor terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Check health of all cluster nodes
-spec check_cluster_health(#alert_threshold{}) -> #cluster_health{}.
check_cluster_health(Thresholds) ->
    Timestamp = erlang:system_time(millisecond),
    AllNodes = [node() | nodes()],

    NodeHealthMaps = lists:map(fun(Node) ->
        check_node_health(Node, Thresholds)
    end, AllNodes),

    NodeHealthMap = lists:foldl(fun(NH, Acc) ->
        maps:put(NH#node_health.node, NH, Acc)
    end, #{}, NodeHealthMaps),

    % Count by status
    Healthy = length([N || N <- NodeHealthMaps, N#node_health.status =:= healthy]),
    Degraded = length([N || N <- NodeHealthMaps, N#node_health.status =:= degraded]),
    Unhealthy = length([N || N <- NodeHealthMaps, N#node_health.status =:= unhealthy]),

    % Determine overall status
    OverallStatus = case {Healthy, Degraded, Unhealthy} of
        {_, _, U} when U > 0 -> unhealthy;
        {_, D, _} when D > 0 -> degraded;
        {H, _, _} when H > 0 -> healthy;
        _ -> unknown
    end,

    #cluster_health{
        timestamp = Timestamp,
        nodes = NodeHealthMap,
        overall_status = OverallStatus,
        total_nodes = length(AllNodes),
        healthy_nodes = Healthy,
        degraded_nodes = Degraded,
        unhealthy_nodes = Unhealthy
    }.

%% @doc Check health of a single node
-spec check_node_health(node(), #alert_threshold{}) -> #node_health{}.
check_node_health(Node, Thresholds) ->
    Status = case Node =:= node() of
        true ->
            % Local node - get actual metrics
            Cpu = get_cpu_utilization(),
            Memory = get_memory_utilization(),
            Processes = erlang:system_info(process_count),
            determine_node_status(Cpu, Memory, Processes, Thresholds);
        false ->
            % Remote node - use RPC or check connectivity
            case rpc:call(Node, erlang, system_info, [process_count], 1000) of
                {badrpc, _} ->
                    #node_health{
                        node = Node,
                        status = unhealthy,
                        last_check = erlang:system_time(millisecond)
                    };
                Processes ->
                    Cpu = get_remote_cpu_utilization(Node),
                    Memory = get_remote_memory_utilization(Node),
                    determine_node_status(Cpu, Memory, Processes, Thresholds)
            end
    end,

    Status.

%% @doc Determine node status based on metrics
-spec determine_node_status(number(), number(), pos_integer(), #alert_threshold{}) ->
    #node_health{}.
determine_node_status(Cpu, Memory, Processes, Thresholds) ->
    Status = case {Cpu >= Thresholds#alert_threshold.cpu,
                   Memory >= Thresholds#alert_threshold.memory,
                   Processes >= Thresholds#alert_threshold.processes} of
        {true, _, _} -> unhealthy;
        {_, true, _} -> unhealthy;
        {_, _, true} -> unhealthy;
        {false, false, false} ->
            % Check for degraded (above 50% of threshold)
            case {Cpu >= Thresholds#alert_threshold.cpu * 0.5,
                  Memory >= Thresholds#alert_threshold.memory * 0.5,
                  Processes >= Thresholds#alert_threshold.processes * 0.5} of
                {true, _, _} -> degraded;
                {_, true, _} -> degraded;
                {_, _, true} -> degraded;
                _ -> healthy
            end
    end,

    #node_health{
        node = node(),
        status = Status,
        cpu_utilization = Cpu,
        memory_utilization = Memory,
        process_count = Processes,
        last_check = erlang:system_time(millisecond)
    }.

%% @doc Get CPU utilization for local node
-spec get_cpu_utilization() -> number().
get_cpu_utilization() ->
    case cpu_sup:util([]) of
        {ok, [{_, Cpu, _, _}]} -> Cpu;
        _ -> 0
    end.

%% @doc Get memory utilization for local node
-spec get_memory_utilization() -> number().
get_memory_utilization() ->
    Total = erlang:memory(total),
    SystemTotal = erlang:system_info(total_memory),
    case SystemTotal of
        0 -> 0;
        _ -> (Total / SystemTotal) * 100
    end.

%% @doc Get CPU utilization for remote node
-spec get_remote_cpu_utilization(node()) -> number().
get_remote_cpu_utilization(Node) ->
    case rpc:call(Node, cpu_sup, util, [[]], 1000) of
        {ok, [{_, Cpu, _, _}]} -> Cpu;
        _ -> 0
    end.

%% @doc Get memory utilization for remote node
-spec get_remote_memory_utilization(node()) -> number().
get_remote_memory_utilization(Node) ->
    case rpc:call(Node, erlang, memory, [total], 1000) of
        Total when is_integer(Total) ->
            case rpc:call(Node, erlang, system_info, [total_memory], 1000) of
                SystemTotal when is_integer(SystemTotal), SystemTotal > 0 ->
                    (Total / SystemTotal) * 100;
                _ ->
                    0
            end;
        _ ->
            0
    end.

%% @doc Calculate health percentage
-spec calculate_health_percentage(#cluster_health{}) -> number().
calculate_health_percentage(Health) ->
    Total = Health#cluster_health.total_nodes,
    case Total of
        0 -> 100.0;
        _ ->
            Healthy = Health#cluster_health.healthy_nodes,
            Degraded = Health#cluster_health.degraded_nodes,
            ((Healthy * 2 + Degraded) / (Total * 2)) * 100
    end.

%% @doc Update health history
-spec update_history(#cluster_health{}, state()) -> [#cluster_health{}].
update_history(NewHealth, State) ->
    History = [NewHealth | State#state.health_history],
    case length(History) > State#state.history_size of
        true -> lists:sublist(History, State#state.history_size);
        false -> History
    end.

%% @doc Check for alerts based on health changes
-spec check_alerts(#cluster_health{}, #cluster_health{}, state()) -> ok.
check_alerts(OldHealth, NewHealth, State) ->
    % Check for status changes
    OldNodes = OldHealth#cluster_health.nodes,
    NewNodes = NewHealth#cluster_health.nodes,

    maps:foreach(fun(Node, NewNodeHealth) ->
        case maps:get(Node, OldNodes, undefined) of
            undefined ->
                % New node
                ok;
            OldNodeHealth when OldNodeHealth#node_health.status =/= NewNodeHealth#node_health.status ->
                % Status changed
                case NewNodeHealth#node_health.status of
                    degraded ->
                        send_alert(State#state.subscribers, {node_degraded, Node, NewNodeHealth});
                    unhealthy ->
                        send_alert(State#state.subscribers, {node_unhealthy, Node, NewNodeHealth});
                    healthy when OldNodeHealth#node_health.status =:= unhealthy;
                               OldNodeHealth#node_health.status =:= degraded ->
                        send_alert(State#state.subscribers, {node_recovered, Node, NewNodeHealth});
                    _ ->
                        ok
                end;
            _ ->
                ok
        end
    end, NewNodes),

    % Check for new unhealthy nodes
    NewUnhealthy = NewHealth#cluster_health.unhealthy_nodes - OldHealth#cluster_health.unhealthy_nodes,
    case NewUnhealthy > 0 of
        true ->
            send_alert(State#state.subscribers, {cluster_health_degraded, NewHealth});
        false ->
            ok
    end.

%% @doc Send alert to subscribers
-spec send_alert([pid()], term()) -> ok.
send_alert(Subscribers, Alert) ->
    lists:foreach(fun(Pid) ->
        catch Pid ! {health_alert, Alert}
    end, Subscribers),
    ok.
