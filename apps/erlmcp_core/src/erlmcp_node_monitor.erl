-module(erlmcp_node_monitor).

-behaviour(gen_server).

%% API exports
-export([start_link/0, get_node_status/0, get_node_health/1, force_node_check/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State record
-record(node_monitor_state,
        {monitored_nodes = #{} :: #{node() => #{status => up | down, last_seen => integer()}},
         check_interval = 5000 :: pos_integer(),
         check_ref :: reference() | undefined}).

-type state() :: #node_monitor_state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_node_status() -> #{node() => #{status => up | down, last_seen => integer()}}.
get_node_status() ->
    gen_server:call(?MODULE, get_node_status).

-spec get_node_health(node()) ->
                         {ok, #{status => up | down, last_seen => integer()}} | {error, not_found}.
get_node_health(Node) ->
    gen_server:call(?MODULE, {get_node_health, Node}).

-spec force_node_check() -> ok.
force_node_check() ->
    gen_server:cast(?MODULE, force_check).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    ClusterNodes = application:get_env(erlmcp_core, cluster_nodes, []),
    CheckInterval = application:get_env(erlmcp_core, node_check_interval, 5000),

    %% Subscribe to node events
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),

    %% Initialize monitored nodes
    Now = erlang:system_time(second),
    MonitoredNodes =
        maps:from_list([{Node, #{status => down, last_seen => Now}} || Node <- ClusterNodes]),

    %% Check current connectivity
    ConnectedNodes = nodes(),
    UpdatedNodes =
        lists:foldl(fun(Node, Acc) ->
                       case lists:member(Node, ConnectedNodes) of
                           true ->
                               maps:put(Node, #{status => up, last_seen => Now}, Acc);
                           false ->
                               Acc
                       end
                    end,
                    MonitoredNodes,
                    ClusterNodes),

    State = #node_monitor_state{monitored_nodes = UpdatedNodes, check_interval = CheckInterval},

    %% Schedule periodic check
    CheckRef = erlang:send_after(CheckInterval, self(), periodic_check),

    logger:info("Node monitor started (monitoring ~p nodes)", [length(ClusterNodes)]),
    {ok, State#node_monitor_state{check_ref = CheckRef}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call(get_node_status, _From, State) ->
    {reply, State#node_monitor_state.monitored_nodes, State};
handle_call({get_node_health, Node}, _From, State) ->
    case maps:get(Node, State#node_monitor_state.monitored_nodes, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Health ->
            {reply, {ok, Health}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(force_check, State) ->
    NewState = perform_node_check(State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(periodic_check, State) ->
    %% Perform periodic node connectivity check
    NewState = perform_node_check(State),

    %% Schedule next check
    CheckRef = erlang:send_after(State#node_monitor_state.check_interval, self(), periodic_check),
    {noreply, NewState#node_monitor_state{check_ref = CheckRef}};
handle_info({nodeup, Node, _InfoList}, State) ->
    logger:info("Node up detected: ~p", [Node]),
    Now = erlang:system_time(second),
    UpdatedNodes =
        maps:put(Node, #{status => up, last_seen => Now}, State#node_monitor_state.monitored_nodes),
    {noreply, State#node_monitor_state{monitored_nodes = UpdatedNodes}};
handle_info({nodedown, Node, InfoList}, State) ->
    Reason = proplists:get_value(nodedown_reason, InfoList, unknown),
    logger:warning("Node down detected: ~p (reason: ~p)", [Node, Reason]),
    Now = erlang:system_time(second),
    UpdatedNodes =
        maps:update_with(Node,
                         fun(Health) -> Health#{status => down, last_seen => Now} end,
                         #{status => down, last_seen => Now},
                         State#node_monitor_state.monitored_nodes),
    {noreply, State#node_monitor_state{monitored_nodes = UpdatedNodes}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% Cancel periodic check
    case State#node_monitor_state.check_ref of
        undefined ->
            ok;
        Ref ->
            erlang:cancel_timer(Ref)
    end,

    %% Unsubscribe from node events
    net_kernel:monitor_nodes(false),

    logger:info("Node monitor terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec perform_node_check(state()) -> state().
perform_node_check(State) ->
    Now = erlang:system_time(second),
    ConnectedNodes = nodes(),

    UpdatedNodes =
        maps:map(fun(Node, Health) ->
                    Status =
                        case lists:member(Node, ConnectedNodes) of
                            true ->
                                up;
                            false ->
                                down
                        end,
                    Health#{status => Status, last_seen => Now}
                 end,
                 State#node_monitor_state.monitored_nodes),

    State#node_monitor_state{monitored_nodes = UpdatedNodes}.
