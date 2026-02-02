%%%-------------------------------------------------------------------
%%% @doc OTP Distribution Manager
%%%-------------------------------------------------------------------
-module(erlmcp_distribution_manager).

-behaviour(gen_server).
-include("erlmcp.hrl").

%% API exports
-export([start_link/0, connect_node/1, disconnect_node/1, get_connected_nodes/0,
         get_node_info/1, is_node_available/1, set_distribution_mode/1,
         get_distribution_protocol/0, optimize_for_version/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type distribution_mode() :: standard | optimal | legacy.
-type distribution_protocol() :: {otp26, legacy} | {otp27, enhanced} | {otp28, optimized}.
-type node_info() :: #{
    node => node(),
    version => {non_neg_integer(), non_neg_integer(), non_neg_integer()},
    capabilities => [atom()],
    status => connected | disconnected | connecting | error
}.

%% State record
-record(state, {
    mode = standard :: distribution_mode(),
    protocol = {otp27, enhanced} :: distribution_protocol(),
    connected_nodes = #{} :: #{node() => node_info()},
    otp_version :: {non_neg_integer(), non_neg_integer(), non_neg_integer()},
    features :: #{atom() => boolean()},
    pending_connections = #{} :: #{node() => reference()}
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec connect_node(node()) -> ok | {error, term()}.
connect_node(Node) when is_atom(Node) ->
    gen_server:call(?MODULE, {connect_node, Node}, 10000).

-spec disconnect_node(node()) -> ok.
disconnect_node(Node) when is_atom(Node) ->
    gen_server:call(?MODULE, {disconnect_node, Node}, 5000).

-spec get_connected_nodes() -> [node_info()].
get_connected_nodes() ->
    gen_server:call(?MODULE, get_connected_nodes, 2000).

-spec get_node_info(node()) -> {ok, node_info()} | {error, not_found}.
get_node_info(Node) when is_atom(Node) ->
    gen_server:call(?MODULE, {get_node_info, Node}, 2000).

-spec is_node_available(node()) -> boolean().
is_node_available(Node) when is_atom(Node) ->
    case get_node_info(Node) of
        {ok, Info} -> maps:get(status, Info, disconnected) =:= connected;
        {error, not_found} -> false
    end.

-spec set_distribution_mode(distribution_mode()) -> ok.
set_distribution_mode(Mode) when Mode =:= standard; Mode =:= optimal; Mode =:= legacy ->
    gen_server:cast(?MODULE, {set_distribution_mode, Mode}).

-spec get_distribution_protocol() -> distribution_protocol().
get_distribution_protocol() ->
    gen_server:call(?MODULE, get_distribution_protocol, 2000).

-spec optimize_for_version() -> ok.
optimize_for_version() ->
    gen_server:cast(?MODULE, optimize_for_version).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    OTPVersion = {27, 0, 0},
    State = #state{
        otp_version = OTPVersion,
        features = #{}
    },
    {ok, State}.

-spec handle_call(term(), {pid(), reference()}, state()) ->
    {reply, term(), state()}.
handle_call({connect_node, Node}, _From, State) ->
    {reply, ok, State};
handle_call({disconnect_node, Node}, _From, State) ->
    {reply, ok, State};
handle_call(get_connected_nodes, _From, State) ->
    ConnectedNodes = maps:values(State#state.connected_nodes),
    {reply, ConnectedNodes, State};
handle_call({get_node_info, Node}, _From, State) ->
    ConnectedNodes = State#state.connected_nodes,
    case maps:get(Node, ConnectedNodes, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Info -> {reply, {ok, Info}, State}
    end;
handle_call(get_distribution_protocol, _From, State) ->
    {reply, State#state.protocol, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({set_distribution_mode, Mode}, State) ->
    {noreply, State#state{mode = Mode}};
handle_cast(optimize_for_version, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
