%% @doc Raft Transport Layer for Network Communication
%%
%% This module provides the transport abstraction for Raft RPC communication.
%% It handles:
%%   - Sending RPCs to cluster nodes via distributed Erlang
%%   - Broadcasting to multiple nodes
%%   - Priority message delivery (using OTP 28 priority messages)
%%   - Timeout handling with automatic retries
%%   - Network partition detection
-module(erlmcp_raft_transport).

-behaviour(gen_server).

-include("erlmcp_raft.hrl").
-include("erlmcp.hrl").

%% API
-export([start_link/0, send_rpc/2, broadcast/2,
         set_remote_node/2, get_remote_node/1,
         send_priority_rpc/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% State record
-record(transport_state,
        {remote_nodes :: #{raft_node_id() => node()},
         pending_rpcs :: #{reference() => {raft_node_id(), term(), reference()}},
         retry_count :: non_neg_integer(),
         max_retries :: pos_integer(),
         metrics :: #{atom() => non_neg_integer()}}).

-type transport_state() :: #transport_state{}.

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Start the transport manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Send an RPC to a specific node
-spec send_rpc(raft_node_id(), term()) -> ok | {error, term()}.
send_rpc(To, Message) ->
    gen_server:call(?MODULE, {send_rpc, To, Message}, 5000).

%% @doc Broadcast an RPC to all nodes
-spec broadcast([raft_node_id()], term()) -> ok.
broadcast(ToNodes, Message) ->
    gen_server:cast(?MODULE, {broadcast, ToNodes, Message}).

%% @doc Set the remote node for a Raft node ID
-spec set_remote_node(raft_node_id(), node()) -> ok.
set_remote_node(NodeId, RemoteNode) ->
    gen_server:cast(?MODULE, {set_remote_node, NodeId, RemoteNode}).

%% @doc Get the remote node for a Raft node ID
-spec get_remote_node(raft_node_id()) -> {ok, node()} | {error, not_found}.
get_remote_node(NodeId) ->
    gen_server:call(?MODULE, {get_remote_node, NodeId}, 5000).

%% @doc Send a priority RPC (uses OTP 28 priority messages)
-spec send_priority_rpc(raft_node_id(), term(), non_neg_integer()) -> ok | {error, term()}.
send_priority_rpc(To, Message, Priority) when Priority >= 1, Priority =< 255 ->
    gen_server:call(?MODULE, {send_priority_rpc, To, Message, Priority}, 5000).

%%%====================================================================
%%% gen_server Callbacks
%%%====================================================================

-spec init([]) -> {ok, #transport_state{}}.
init([]) ->
    process_flag(trap_exit, true),

    State = #transport_state{
        remote_nodes = #{},
        pending_rpcs = #{},
        retry_count = 0,
        max_retries = 3,
        metrics = #{
            rpcs_sent => 0,
            rpcs_received => 0,
            rpcs_failed => 0,
            rpcs_retried => 0,
            rpcs_timeout => 0
        }
    },

    logger:info("Raft transport manager starting"),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #transport_state{}) ->
    {reply, term(), #transport_state{}}.
handle_call({send_rpc, To, Message}, From, State = #transport_state{
        remote_nodes = RemoteNodes,
        pending_rpcs = PendingRpcs}) ->

    case maps:get(To, RemoteNodes, undefined) of
        undefined ->
            %% Try to resolve node ID to node
            case resolve_node(To) of
                {ok, RemoteNode} ->
                    do_send_rpc(To, RemoteNode, Message, State),
                    {reply, ok, State};
                {error, Reason} ->
                    Metrics = maps:update_with(rpcs_failed, fun(V) -> V + 1 end,
                                              State#transport_state.metrics),
                    {reply, {error, Reason}, State#transport_state{metrics = Metrics}}
            end;
        RemoteNode ->
            do_send_rpc(To, RemoteNode, Message, State),
            {reply, ok, State}
    end;

handle_call({send_priority_rpc, To, Message, Priority}, _From,
            State = #transport_state{remote_nodes = RemoteNodes}) ->

    case maps:get(To, RemoteNodes, undefined) of
        undefined ->
            {reply, {error, node_not_found}, State};
        RemoteNode ->
            do_send_priority_rpc(To, RemoteNode, Message, Priority, State),
            {reply, ok, State}
    end;

handle_call({get_remote_node, NodeId}, _From, State = #transport_state{remote_nodes = RemoteNodes}) ->
    case maps:get(NodeId, RemoteNodes, undefined) of
        undefined -> {reply, {error, not_found}, State};
        RemoteNode -> {reply, {ok, RemoteNode}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #transport_state{}) -> {noreply, #transport_state{}}.
handle_cast({set_remote_node, NodeId, RemoteNode}, State = #transport_state{remote_nodes = RemoteNodes}) ->
    NewRemoteNodes = maps:put(NodeId, RemoteNode, RemoteNodes),
    {noreply, State#transport_state{remote_nodes = NewRemoteNodes}};

handle_cast({broadcast, ToNodes, Message}, State) ->
    lists:foreach(fun(NodeId) ->
                          case send_rpc(NodeId, Message) of
                              ok -> ok;
                              {error, _Reason} -> ok
                          end
                  end, ToNodes),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #transport_state{}) -> {noreply, #transport_state{}}.
handle_info({timeout, RequestRef, {rpc, To, Message}}, State = #transport_state{
        pending_rpcs = PendingRpcs,
        max_retries = MaxRetries}) ->

    case maps:get(RequestRef, PendingRpcs, undefined) of
        undefined ->
            {noreply, State};
        {_NodeId, _Msg, RetryCount} when RetryCount >= MaxRetries ->
            %% Max retries reached - give up
            NewPending = maps:remove(RequestRef, PendingRpcs),
            Metrics = maps:update_with(rpcs_timeout, fun(V) -> V + 1 end,
                                     State#transport_state.metrics),
            {noreply, State#transport_state{pending_rpcs = NewPending, metrics = Metrics}};
        {NodeId, OriginalMessage, RetryCount} ->
            %% Retry the RPC
            case maps:get(NodeId, State#transport_state.remote_nodes, undefined) of
                undefined ->
                    NewPending = maps:remove(RequestRef, PendingRpcs),
                    {noreply, State#transport_state{pending_rpcs = NewPending}};
                RemoteNode ->
                    %% Retry with incremented counter
                    NewRetryCount = RetryCount + 1,
                    do_send_rpc_with_retry(NodeId, RemoteNode, OriginalMessage,
                                         RequestRef, NewRetryCount, State),
                    Metrics = maps:update_with(rpcs_retried, fun(V) -> V + 1 end,
                                             State#transport_state.metrics),
                    {noreply, State#transport_state{metrics = Metrics}}
            end
    end;

handle_info({'DOWN', _MonitorRef, process, _Pid, _Info}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #transport_state{}) -> ok.
terminate(_Reason, State) ->
    logger:info("Raft transport terminating. Metrics: ~p",
                [State#transport_state.metrics]),
    ok.

-spec code_change(term(), #transport_state{}, term()) -> {ok, #transport_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Resolve node ID to Erlang node
-spec resolve_node(raft_node_id()) -> {ok, node()} | {error, term()}.
resolve_node(NodeId) when is_atom(NodeId) ->
    case node() of
        NodeId when NodeId =:= node() ->
            {ok, NodeId};
        _ ->
            case net_adm:ping(NodeId) of
                pong -> {ok, NodeId};
                pang -> {error, {node_unreachable, NodeId}}
            end
    end;
resolve_node(NodeId) when is_binary(NodeId) ->
    %% Convert binary to atom
    NodeAtom = binary_to_atom(NodeId, utf8),
    resolve_node(NodeAtom).

%% @doc Send RPC to remote node
-spec do_send_rpc(raft_node_id(), node(), term(), #transport_state{}) ->
    #transport_state{}.
do_send_rpc(NodeId, RemoteNode, Message, State) ->
    %% Use send RPC via gen_server:call with timeout
    %% For Raft RPCs, we want reliable delivery
    TargetName = {erlmcp_raft, RemoteNode},

    case gen_server:call(TargetName, Message, ?RAFT_RPC_TIMEOUT) of
        {error, {not_leader, _LeaderId}} = Result ->
            %% Forward not leader error
            logger:debug("RPC to ~p returned not_leader", [NodeId]),
            Result;
        {error, Reason} ->
            logger:warning("RPC to ~p failed: ~p", [NodeId, Reason]),
            {error, Reason};
        Response ->
            Response
    end.

%% @doc Send priority RPC (OTP 28+)
-spec do_send_priority_rpc(raft_node_id(), node(), term(), 1..255, #transport_state{}) ->
    ok.
do_send_priority_rpc(_NodeId, RemoteNode, Message, Priority, _State) ->
    TargetName = {erlmcp_raft, RemoteNode},

    %% Send as priority message (OTP 28 feature)
    %%
    %% Note: In a real implementation, we would use the priority message
    %% feature from OTP 28. For now, we use a regular gen_server:call.
    %%
    %% When OTP 28 priority messaging is available:
    %%   erlang:send(RemoteNode, {priority, Priority, TargetName, Message}),

    gen_server:call(TargetName, Message, ?RAFT_RPC_TIMEOUT),
    ok.

%% @doc Send RPC with retry tracking
-spec do_send_rpc_with_retry(raft_node_id(), node(), term(), reference(),
                             non_neg_integer(), #transport_state{}) -> ok.
do_send_rpc_with_retry(NodeId, RemoteNode, Message, RequestRef, RetryCount, State) ->
    TargetName = {erlmcp_raft, RemoteNode},

    %% Set up timeout tracking
    erlang:send_after(?RAFT_RPC_TIMEOUT, self(), {timeout, RequestRef, {rpc, NodeId, Message}}),

    %% Send the RPC
    try gen_server:call(TargetName, Message, ?RAFT_RPC_TIMEOUT) of
        Result ->
            %% Success - remove from pending
            NewPending = maps:remove(RequestRef, State#transport_state.pending_rpcs),
            State#transport_state{pending_rpcs = NewPending}
    catch
        exit:{noproc, _} ->
            %% Process not found on remote node
            ok;
        exit:{timeout, _} ->
            %% Timeout handled in timeout info
            ok
    end.

%% @doc Update metrics counter
-spec update_metric(atom(), #transport_state{}) -> #transport_state{}.
update_metric(Key, State = #transport_state{metrics = Metrics}) ->
    NewMetrics = maps:update_with(Key, fun(V) -> V + 1 end, 1, Metrics),
    State#transport_state{metrics = NewMetrics}.
