%% @doc Auto-Healing System for erlmcp
%% Implements automated recovery for node and service failures
-module(erlmcp_autohealer).
-behaviour(gen_server).

%% API
-export([start_link/0, handle_node_down/1, handle_high_load/1,
         start_healing_process/2, get_healing_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

-record(healing_info, {
    node :: node(),
    issue_type :: 'down' | 'high_cpu' | 'high_memory' | 'unresponsive',
    start_time :: integer(),
    attempts :: integer(),
    status :: 'in_progress' | 'completed' | 'failed' | 'skipped',
    actions :: list(),
    next_action :: binary() | undefined
}).

-record(state, {
    healing_table :: ets:tid(),
    max_attempts :: integer(),
    heal_delay :: integer(),
    retry_delay :: integer(),
    status :: 'active' | 'paused'
}).

-define(TAB, erlmcp_autohealer).
-define(MAX_ATTEMPTS, 3).
-define(HEAL_DELAY, 5000). % 5 seconds
-define(RETRY_DELAY, 10000). % 10 seconds
-define(CPU_THRESHOLD, 90). % 90%
-define(MEMORY_THRESHOLD, 85). % 85%

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_node_down(Node) ->
    gen_server:call(?MODULE, {handle_node_down, Node}).

handle_high_load(Node) ->
    gen_server:call(?MODULE, {handle_high_load, Node}).

start_healing_process(Node, IssueType) ->
    gen_server:call(?MODULE, {start_healing_process, Node, IssueType}).

get_healing_status() ->
    gen_server:call(?MODULE, get_healing_status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Create ETS table for healing information
    HealingTable = ets:new(?TAB, [
        set,
        public,
        {keypos, #healing_info.node},
        named_table
    ]),

    State = #state{
        healing_table = HealingTable,
        max_attempts = ?MAX_ATTEMPTS,
        heal_delay = ?HEAL_DELAY,
        retry_delay = ?RETRY_DELAY,
        status = active
    },

    %% Start monitoring loop
    spawn_monitoring_loop(State),

    {ok, State}.

handle_call({handle_node_down, Node}, _From, State) ->
    %% Record node down event
    HealingInfo = #healing_info{
        node = Node,
        issue_type = down,
        start_time = erlang:system_time(millisecond),
        attempts = 0,
        status = in_progress,
        actions = [],
        next_action => restart_node
    },

    true = ets:insert(State#state.healing_table, HealingInfo),

    %% Start healing process
    Reply = start_healing(Node, down, State),

    {reply, Reply, State};

handle_call({handle_high_load, Node}, _From, State) ->
    %% Record high load event
    HealingInfo = #healing_info{
        node = Node,
        issue_type = high_cpu,
        start_time = erlang:system_time(millisecond),
        attempts = 0,
        status = in_progress,
        actions = [],
        next_action => scale_node
    },

    true = ets:insert(State#state.healing_table, HealingInfo),

    %% Start healing process
    Reply = start_healing(Node, high_cpu, State),

    {reply, Reply, State};

handle_call({start_healing_process, Node, IssueType}, _From, State) ->
    %% Check if healing already in progress
    case ets:lookup(State#state.healing_table, Node) of
        [] ->
            %% Start new healing process
            HealingInfo = #healing_info{
                node = Node,
                issue_type = IssueType,
                start_time = erlang:system_time(millisecond),
                attempts = 0,
                status = in_progress,
                actions = [],
                next_action => determine_first_action(IssueType)
            },

            true = ets:insert(State#state.healing_table, HealingInfo),

            %% Start healing
            start_healing(Node, IssueType, State),

            {reply, started, State};
        [_] ->
            {reply, already_in_progress, State}
    end;

handle_call(get_healing_status, _From, State) ->
    %% Get overall healing status
    HealingProcesses = ets:tab2list(State#state.healing_table),
    Active = [H || H <- HealingProcesses, H#healing_info.status =:= in_progress],
    Completed = [H || H <- HealingProcesses, H#healing_info.status =:= completed],
    Failed = [H || H <- HealingProcesses, H#healing_info.status =:= failed],

    Status = #{
        active => length(Active),
        completed => length(Completed),
        failed => length(Failed),
        total => length(HealingProcesses),
        status => State#state.status
    },

    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(healing_monitor, State) ->
    %% Check all healing processes
    check_healing_processes(State),
    spawn_monitoring_loop(State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Cleanup ETS table
    ets:delete(?TAB),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

spawn_monitoring_loop(State) ->
    spawn(fun() ->
        timer:sleep(?RETRY_DELAY),
        gen_server:cast(?MODULE, healing_monitor)
    end).

start_healing(Node, IssueType, State) ->
    %% Start healing process asynchronously
    spawn(fun() ->
        perform_healing(Node, IssueType, State)
    end),
    ok.

perform_healing(Node, IssueType, State) ->
    case get_healing_info(Node, State) of
        {ok, HealingInfo} ->
            Attempts = HealingInfo#healing_info.attempts + 1,

            case Attempts > State#state.max_attempts of
                true ->
                    %% Max attempts reached, mark as failed
                    update_healing_info(Node, HealingInfo#healing_info{
                        attempts = Attempts,
                        status = failed,
                        actions = HealingInfo#healing_info.actions ++ [max_attempts_reached]
                    }),
                    logger:error("Max healing attempts reached for node ~p", [Node]);
                false ->
                    %% Perform next healing action
                    case perform_healing_action(Node, IssueType, HealingInfo, State) of
                        success ->
                            %% Healing successful
                            update_healing_info(Node, HealingInfo#healing_info{
                                attempts = Attempts,
                                status = completed,
                                actions = HealingInfo#healing_info.actions ++ [healing_successful]
                            }),
                            logger:info("Healing successful for node ~p", [Node]);
                        {retry, Action} ->
                            %% Retry after delay
                            update_healing_info(Node, HealingInfo#healing_info{
                                attempts = Attempts,
                                next_action => Action,
                                actions = HealingInfo#healing_info.actions ++ [Action]
                            }),
                            timer:sleep(State#state.heal_delay),
                            perform_healing(Node, IssueType, State);
                        failed ->
                            update_healing_info(Node, HealingInfo#healing_info{
                                attempts = Attempts,
                                status = failed,
                                actions = HealingInfo#healing_info.actions ++ [healing_failed]
                            }),
                            logger:error("Healing failed for node ~p", [Node])
                    end
            end
    end.

perform_healing_action(Node, IssueType, HealingInfo, State) ->
    Action = HealingInfo#healing_info.next_action,

    case Action of
        restart_node ->
            %% Attempt to restart the node
            case restart_node(Node) of
                ok ->
                    success;
                {error, timeout} ->
                    {retry, restart_node};
                {error, _} ->
                    failed
            end;
        scale_node ->
            %% Attempt to scale the node
            case scale_node(Node) of
                ok ->
                    success;
                {error, scaling_in_progress} ->
                    {retry, scale_node};
                {error, _} ->
                    failed
            end;
        failover ->
            %% Attempt failover to another node
            case failover_node(Node) of
                ok ->
                    success;
                {error, no_fallback} ->
                    failed
            end;
        _ ->
            failed
    end.

restart_node(Node) ->
    %% Implementation for restarting a node
    %% This would typically involve orchestrating node restart through deployment systems
    case is_node_available(Node) of
        true ->
            %% Node is already available, no restart needed
            ok;
        false ->
            %% Attempt restart
            case rpc:call(Node, erlmcp_node, restart, [], 30000) of
                ok -> ok;
                {badrpc, nodedown} ->
                    ok; % Node is down, restart will handle it
                {badrpc, Reason} ->
                    {error, Reason}
            end
    end.

scale_node(Node) ->
    %% Implementation for scaling a node
    %% This would typically involve cloud auto-scaling APIs
    case is_high_load(Node) of
        false ->
            ok; % Load is normal now
        true ->
            %% Check if scaling is already in progress
            case rpc:call(Node, erlmcp_node, is_scaling, [], 5000) of
                true ->
                    {error, scaling_in_progress};
                false ->
                    %% Trigger scaling
                    case rpc:call(Node, erlmcp_node, scale_up, [], 30000) of
                        ok -> ok;
                        {error, Reason} -> {error, Reason}
                    end
            end
    end.

failover_node(Node) ->
    %% Implementation for failover to another node
    case get_fallback_node(Node) of
        undefined ->
            {error, no_fallback};
        FallbackNode ->
            case rpc:call(FallbackNode, erlmcp_node, take_over, [Node], 30000) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end
    end.

is_node_available(Node) ->
    %% Check if node is available
    case net_adm:ping(Node) of
        pong ->
            case rpc:call(Node, erlmcp_health_check, status, [], 2000) of
                ok -> true;
                _ -> false
            end;
        _ ->
            false
    end.

is_high_load(Node) ->
    %% Check if node has high load
    case rpc:call(Node, erlmcp_metrics, get_metrics, [], 5000) of
        {ok, Metrics} ->
            Metrics#metrics.cpu > ?CPU_THRESHOLD orelse
            Metrics#metrics.memory > ?MEMORY_THRESHOLD;
        _ ->
            false
    end.

get_fallback_node(Node) ->
    %% Get fallback node for the specified node
    %% This would typically be determined from configuration
    case Node of
        'us-east-1-node1@10.0.1.10' -> 'us-east-1-node2@10.0.1.11';
        'us-east-1-node2@10.0.1.11' -> 'us-east-1-node3@10.0.1.12';
        'us-east-1-node3@10.0.1.12' -> 'us-east-1-node1@10.0.1.10';
        'eu-west-1-node1@10.0.2.10' -> 'eu-west-1-node2@10.0.2.11';
        'eu-west-1-node2@10.0.2.11' -> 'eu-west-1-node3@10.0.2.12';
        'eu-west-1-node3@10.0.2.12' -> 'eu-west-1-node1@10.0.2.10';
        _ -> undefined
    end.

get_healing_info(Node, State) ->
    case ets:lookup(State#state.healing_table, Node) of
        [HealingInfo] -> {ok, HealingInfo};
        [] -> {error, not_found}
    end.

update_healing_info(Node, HealingInfo) ->
    true = ets:insert(?TAB, HealingInfo).

determine_first_action(IssueType) ->
    case IssueType of
        down -> restart_node;
        high_cpu -> scale_node;
        high_memory -> scale_node;
        unresponsive -> failover
    end.

check_healing_processes(State) ->
    %% Check all healing processes and retry if needed
    HealingProcesses = ets:tab2list(State#state.hearding_table),

    lists:foreach(fun(HealingInfo) ->
        case HealingInfo#healing_info.status of
            in_progress ->
                %% Check if next action needs to be performed
                case HealingInfo#healing_info.attempts < State#state.max_attempts of
                    true ->
                        perform_healing(HealingInfo#healing_info.node,
                                      HealingInfo#healing_info.issue_type,
                                      State);
                    false ->
                        %% Mark as failed
                        update_healing_info(HealingInfo#healing_info.node,
                                         HealingInfo#healing_info{status = failed})
                end;
            _ ->
                ok
        end
    end, HealingProcesses).