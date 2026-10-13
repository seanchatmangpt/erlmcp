%%%-------------------------------------------------------------------
%%% @doc erlmcp_cluster_discovery - Node Discovery Mechanism
%%%
%%% Implements auto-discovery via EPMD and static node list configuration.
%%% Follows Joe Armstrong's principle: systems should discover each other.
%%%
%%% Features:
%%% - EPMD-based auto-discovery for nodes with same name prefix
%%% - Static node list from configuration
%%% - DNS-based discovery for cloud environments
%%% - Periodic re-discovery for dynamic environments
%%% - Connection retry with exponential backoff
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cluster_discovery).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         discover_nodes/0, discover_nodes/1,
         add_static_node/1, remove_static_node/1,
         set_discovery_mode/1, get_discovered_nodes/0,
         force_discovery/0, get_discovery_status/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Macros
-define(SERVER, ?MODULE).
-define(DEFAULT_DISCOVERY_INTERVAL, 30000).
-define(EPMD_REQUEST, #erlang_epmd{request = names}).
-define(MAX_RETRY_ATTEMPTS, 5).
-define(RETRY_BASE_DELAY, 1000).

%%====================================================================
%% Types
%%====================================================================

-type discovery_mode() :: epmd | static | dns | hybrid.
-type node_status() :: connected | disconnected | pending.

-record(node_info, {
    node :: node(),
    status :: node_status(),
    last_seen :: integer() | undefined,
    retry_count = 0 :: non_neg_integer(),
    last_error :: term() | undefined}).

-record(state, {
    mode :: discovery_mode(),
    static_nodes = [] :: [node()],
    discovered_nodes = [] :: [node()],
    node_info = #{} :: #{node() => #node_info{}},
    discovery_interval :: pos_integer(),
    discovery_ref :: reference() | undefined,
    name_prefix :: string() | undefined,
    dns_domain :: string() | undefined,
    retry_timer :: reference() | undefined}).

-type state() :: #state{}.

-export_type([discovery_mode/0, node_status/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start discovery with default configuration
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start discovery with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Discover nodes using configured method
-spec discover_nodes() -> {ok, [node()]} | {error, term()}.
discover_nodes() ->
    gen_server:call(?SERVER, discover_nodes, 30000).

%% @doc Discover nodes with specific mode override
-spec discover_nodes(discovery_mode()) -> {ok, [node()]} | {error, term()}.
discover_nodes(Mode) ->
    gen_server:call(?SERVER, {discover_nodes, Mode}, 30000).

%% @doc Add a static node to discovery list
-spec add_static_node(node()) -> ok | {error, term()}.
add_static_node(Node) when is_atom(Node) ->
    gen_server:call(?SERVER, {add_static_node, Node}).

%% @doc Remove a static node from discovery list
-spec remove_static_node(node()) -> ok.
remove_static_node(Node) when is_atom(Node) ->
    gen_server:call(?SERVER, {remove_static_node, Node}).

%% @doc Set discovery mode
-spec set_discovery_mode(discovery_mode()) -> ok.
set_discovery_mode(Mode) when Mode =:= epmd; Mode =:= static;
                               Mode =:= dns; Mode =:= hybrid ->
    gen_server:call(?SERVER, {set_discovery_mode, Mode}).

%% @doc Get all discovered nodes
-spec get_discovered_nodes() -> [node()].
get_discovered_nodes() ->
    gen_server:call(?SERVER, get_discovered_nodes).

%% @doc Force immediate rediscovery
-spec force_discovery() -> {ok, [node()]} | {error, term()}.
force_discovery() ->
    gen_server:call(?SERVER, force_discovery, 30000).

%% @doc Get discovery status
-spec get_discovery_status() -> map().
get_discovery_status() ->
    gen_server:call(?SERVER, get_discovery_status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map() | []) -> {ok, state()}.
init(Options) ->
    process_flag(trap_exit, true),

    % Extract options
    Mode = maps:get(discovery_mode, Options, epmd),
    StaticNodes = maps:get(static_nodes, Options, []),
    Interval = maps:get(discovery_interval, Options, ?DEFAULT_DISCOVERY_INTERVAL),
    NamePrefix = maps:get(name_prefix, Options, undefined),
    DNSDomain = maps:get(dns_domain, Options, undefined),

    % Get configured nodes from app env
    AppConfigured = application:get_env(erlmcp_core, cluster_nodes, []),

    % Merge static nodes
    AllStaticNodes = lists:usort(StaticNodes ++ AppConfigured),

    State = #state{
        mode = Mode,
        static_nodes = AllStaticNodes,
        discovery_interval = Interval,
        name_prefix = NamePrefix,
        dns_domain = DNSDomain
    },

    ?LOG_INFO("Starting cluster discovery: mode=~p, static_nodes=~p",
              [Mode, AllStaticNodes]),

    % Start initial discovery
    NewState = perform_discovery(State),

    % Schedule periodic discovery
    DiscoveryRef = erlang:send_after(Interval, self(), periodic_discovery),

    {ok, NewState#state{discovery_ref = DiscoveryRef}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call(discover_nodes, _From, State) ->
    NewState = perform_discovery(State),
    {reply, {ok, NewState#state.discovered_nodes}, NewState, hibernate};

handle_call({discover_nodes, Mode}, _From, State) ->
    NewState = perform_discovery(State#state{mode = Mode}),
    {reply, {ok, NewState#state.discovered_nodes}, NewState, hibernate};

handle_call({add_static_node, Node}, _From, State) ->
    StaticNodes = lists:usort([Node | State#state.static_nodes]),
    NewState = State#state{static_nodes = StaticNodes},
    ?LOG_INFO("Added static node: ~p", [Node]),
    {reply, ok, NewState, hibernate};

handle_call({remove_static_node, Node}, _From, State) ->
    StaticNodes = lists:delete(Node, State#state.static_nodes),
    NewState = State#state{static_nodes = StaticNodes},
    ?LOG_INFO("Removed static node: ~p", [Node]),
    {reply, ok, NewState, hibernate};

handle_call({set_discovery_mode, Mode}, _From, State) ->
    NewState = State#state{mode = Mode},
    ?LOG_INFO("Discovery mode changed to: ~p", [Mode]),
    {reply, ok, NewState, hibernate};

handle_call(get_discovered_nodes, _From, State) ->
    {reply, State#state.discovered_nodes, State, hibernate};

handle_call(force_discovery, _From, State) ->
    NewState = perform_discovery(State),
    {reply, {ok, NewState#state.discovered_nodes}, NewState, hibernate};

handle_call(get_discovery_status, _From, State) ->
    Status = #{
        mode => State#state.mode,
        static_nodes => State#state.static_nodes,
        discovered_nodes => State#state.discovered_nodes,
        node_info => maps:fold(fun(Node, Info, Acc) ->
            Acc#{Node => #{
                status => Info#node_info.status,
                last_seen => Info#node_info.last_seen,
                retry_count => Info#node_info.retry_count,
                last_error => Info#node_info.last_error
            }}
        end, #{}, State#state.node_info),
        discovery_interval => State#state.discovery_interval
    },
    {reply, Status, State, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_info(periodic_discovery, State) ->
    NewState = perform_discovery(State),
    DiscoveryRef = erlang:send_after(State#state.discovery_interval, self(), periodic_discovery),
    {noreply, NewState#state{discovery_ref = DiscoveryRef}, hibernate};

handle_info({retry_connection, Node}, State) ->
    NewState = retry_node_connection(Node, State),
    {noreply, NewState, hibernate};

handle_info({nodeup, Node}, State) ->
    ?LOG_INFO("Node up event: ~p", [Node]),
    NewNodeInfo = maps:put(Node, #node_info{
        node = Node,
        status = connected,
        last_seen = erlang:system_time(millisecond),
        retry_count = 0,
        last_error = undefined
    }, State#state.node_info),
    Discovered = lists:usort([Node | State#state.discovered_nodes]),
    {noreply, State#state{node_info = NewNodeInfo, discovered_nodes = Discovered}, hibernate};

handle_info({nodedown, Node}, State) ->
    ?LOG_WARNING("Node down event: ~p", [Node]),
    NewNodeInfo = maps:update_with(Node, fun(Info) ->
        Info#node_info{status = disconnected, last_error = nodedown}
    end, State#state.node_info),
    {noreply, State#state{node_info = NewNodeInfo}, hibernate};

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    case State#state.discovery_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    case State#state.retry_timer of
        undefined -> ok;
        Ref2 -> erlang:cancel_timer(Ref2)
    end,
    ?LOG_INFO("Cluster discovery terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Perform node discovery based on configured mode
-spec perform_discovery(state()) -> state().
perform_discovery(State = #state{mode = Mode}) ->
    Discovered = case Mode of
        epmd ->
            discover_via_epmd(State);
        static ->
            discover_static(State);
        dns ->
            discover_via_dns(State);
        hybrid ->
            Static = discover_static(State),
            Epmd = discover_via_epmd(State#state{discovered_nodes = []}),
            % Merge results
            AllNodes = lists:usort(Static#state.discovered_nodes ++ Epmd#state.discovered_nodes),
            State#state{discovered_nodes = AllNodes}
    end,

    % Update node info for discovered nodes
    UpdatedInfo = lists:foldl(fun(Node, Acc) ->
        case maps:get(Node, Acc, undefined) of
            undefined ->
                Acc#{Node => #node_info{
                    node = Node,
                    status = connected,
                    last_seen = erlang:system_time(millisecond),
                    retry_count = 0,
                    last_error = undefined
                }};
            Info ->
                Acc#{Node => Info#node_info{
                    status = connected,
                    last_seen = erlang:system_time(millisecond),
                    retry_count = 0,
                    last_error = undefined
                }}
        end
    end, State#state.node_info, Discovered#state.discovered_nodes),

    Discovered#state{node_info = UpdatedInfo}.

%% @doc Discover nodes via EPMD
-spec discover_via_epmd(state()) -> state().
discover_via_epmd(State) ->
    % Get node name prefix from our node name
    MyNode = node(),
    Prefix = case State#state.name_prefix of
        undefined ->
            % Extract prefix from current node name
            [Name, _Host] = string:split(atom_to_list(MyNode), "@"),
            % Extract prefix before underscore if exists
            case string:split(Name, "_") of
                [Base, _Rest] -> Base;
                _ -> Name
            end;
        ConfiguredPrefix ->
            ConfiguredPrefix
    end,

    % Get EPMD port (unused but kept for future extension)
    _ = case init:get_argument(epmd_port) of
        {ok, [[PortStr]]} -> list_to_integer(PortStr);
        _ -> 4369
    end,

    % Get EPMD host
    EpmdHost = case inet_db:lookup_option(nameserver) of
        {ok, _} -> "localhost";
        _ -> "127.0.0.1"
    end,

    % Query EPMD for all registered nodes
    Nodes = case erlang:epmd_query(EpmdHost) of
        {ok, Names} when is_list(Names) ->
            [Node || {Name, _Port} <- Names,
                     is_node_with_prefix(Name, Prefix),
                     Node <- [list_to_atom(Name ++ "@" ++ EpmdHost)],
                     Node =/= MyNode];
        {error, Reason} ->
            ?LOG_WARNING("EPMD query failed: ~p", [Reason]),
            []
    end,

    % Connect to discovered nodes
    Connected = lists:filtermap(fun(Node) ->
        connect_to_node(Node)
    end, Nodes),

    State#state{discovered_nodes = Connected}.

%% @doc Discover static nodes from configuration
-spec discover_static(state()) -> state().
discover_static(State) ->
    Connected = lists:filtermap(fun(Node) ->
        connect_to_node(Node)
    end, State#state.static_nodes),

    State#state{discovered_nodes = Connected}.

%% @doc Discover nodes via DNS (SRV records or A records)
-spec discover_via_dns(state()) -> state().
discover_via_dns(State) ->
    Domain = State#state.dns_domain,
    case Domain of
        undefined ->
            ?LOG_WARNING("DNS discovery requested but no domain configured"),
            State#state{discovered_nodes = []};
        _ ->
            % Query SRV records for _erlmcp._tcp.<domain>
            case inet_res:lookup("_erlmcp._tcp." ++ Domain, srv) of
                [] ->
                    % Fallback to A records
                    case inet_res:lookup(Domain, a) of
                        [] ->
                            State#state{discovered_nodes = []};
                        IPs ->
                            Nodes = [list_to_atom("erlmcp@" ++ inet:ntoa(IP)) || IP <- IPs],
                            Connected = lists:filtermap(fun(Node) ->
                                connect_to_node(Node)
                            end, Nodes),
                            State#state{discovered_nodes = Connected}
                    end;
                SRVRecords ->
                    Nodes = [list_to_atom("erlmcp@" ++ Target) || {_, _, _, Target} <- SRVRecords],
                    Connected = lists:filtermap(fun(Node) ->
                        connect_to_node(Node)
                    end, Nodes),
                    State#state{discovered_nodes = Connected}
            end
    end.

%% @doc Check if node name matches prefix
-spec is_node_with_prefix(string(), string()) -> boolean().
is_node_with_prefix(_Name, []) ->
    false;
is_node_with_prefix(Name, Prefix) when is_list(Prefix) ->
    string:prefix(Name, Prefix) =/= nomatch.

%% @doc Connect to a node with retry
-spec connect_to_node(node()) -> {true, node()} | false.
connect_to_node(Node) ->
    case net_kernel:connect_node(Node) of
        true ->
            ?LOG_DEBUG("Connected to node: ~p", [Node]),
            {true, Node};
        false ->
            ?LOG_WARNING("Failed to connect to node: ~p", [Node]),
            false;
        ignored ->
            ?LOG_DEBUG("Connection ignored (already connected): ~p", [Node]),
            {true, Node}
    end.

%% @doc Retry connection to a node with exponential backoff
-spec retry_node_connection(node(), state()) -> state().
retry_node_connection(Node, State) ->
    Info = maps:get(Node, State#state.node_info, #node_info{node = Node, retry_count = 0}),
    RetryCount = Info#node_info.retry_count + 1,

    case RetryCount >= ?MAX_RETRY_ATTEMPTS of
        true ->
            ?LOG_ERROR("Max retry attempts reached for node: ~p", [Node]),
            NewInfo = Info#node_info{status = disconnected, last_error = max_retries_exceeded},
            State#state{node_info = maps:put(Node, NewInfo, State#state.node_info)};
        false ->
            Delay = ?RETRY_BASE_DELAY * (1 bsl RetryCount),  % Exponential backoff

            case connect_to_node(Node) of
                {true, _} ->
                    NewInfo = Info#node_info{
                        status = connected,
                        last_seen = erlang:system_time(millisecond),
                        retry_count = 0,
                        last_error = undefined
                    },
                    Discovered = lists:usort([Node | State#state.discovered_nodes]),
                    State#state{node_info = maps:put(Node, NewInfo, State#state.node_info),
                                discovered_nodes = Discovered};
                false ->
                    NewInfo = Info#node_info{
                        retry_count = RetryCount,
                        last_error = connection_failed
                    },
                    % Schedule next retry
                    RetryRef = erlang:send_after(Delay, self(), {retry_connection, Node}),
                    State#state{node_info = maps:put(Node, NewInfo, State#state.node_info),
                                retry_timer = RetryRef}
            end
    end.
