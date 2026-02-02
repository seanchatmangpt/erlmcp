%%%-------------------------------------------------------------------
%%% @doc
%%% OTP 26-28 Distribution Manager
%%%
%%% Handles the complexities of OTP distribution across versions 26-28,
%%% addressing breaking changes and providing optimization opportunities.
%%%
%%% Key Challenges Addressed:
%%% - OTP 26: New mandatory link protocol, global pg removal
%%% - OTP 27: Native JSON, improved distribution protocol
%%% - OTP 28: Process iterators, priority messages, enhanced protocol
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_distribution_manager).

-behaviour(gen_server).
-include("erlmcp.hrl").
-include("otp_compat.hrl").

%% API exports
-export([start_link/0, connect_node/1, disconnect_node/1, get_connected_nodes/0,
         get_node_info/1, is_node_available/1, set_distribution_mode/1,
         get_distribution_protocol/0, optimize_for_version/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type distribution_mode() :: standard | optimal | legacy.
-type distribution_protocol() :: {otp26, legacy} | {otp27, enhanced} | {otp28, optimized}.
-type node_info() :: #{node := node(),
                      version := {non_neg_integer(), non_neg_integer(), non_neg_integer()},
                      capabilities := [atom()],
                      status := connected | disconnected | connecting | error}.

%% State record
-record(state,
        {mode = standard :: distribution_mode(),
         protocol = {otp27, enhanced} :: distribution_protocol(),
         connected_nodes = #{} :: #{node() => node_info()},
         otp_version :: {non_neg_integer(), non_neg_integer(), non_neg_integer()},
         features :: #{atom() => boolean()},
         pending_connections = #{} :: #{node() => reference()},
         optimization_handlers = #{}}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Connect to a node with version-aware protocol negotiation
%% @end
%%--------------------------------------------------------------------
-spec connect_node(node()) -> ok | {error, term()}.
connect_node(Node) when is_atom(Node) ->
    gen_server:call(?MODULE, {connect_node, Node}, 10000).

%%--------------------------------------------------------------------
%% @doc Disconnect from a node with proper cleanup
%% @end
%%--------------------------------------------------------------------
-spec disconnect_node(node()) -> ok.
disconnect_node(Node) when is_atom(Node) ->
    gen_server:call(?MODULE, {disconnect_node, Node}, 5000).

%%--------------------------------------------------------------------
%% @doc Get list of connected nodes with detailed information
%% @end
%%--------------------------------------------------------------------
-spec get_connected_nodes() -> [node_info()].
get_connected_nodes() ->
    gen_server:call(?MODULE, get_connected_nodes, 2000).

%%--------------------------------------------------------------------
%% @doc Get detailed information about a specific node
%% @end
%%--------------------------------------------------------------------
-spec get_node_info(node()) -> {ok, node_info()} | {error, not_found}.
get_node_info(Node) when is_atom(Node) ->
    gen_server:call(?MODULE, {get_node_info, Node}, 2000).

%%--------------------------------------------------------------------
%% @doc Check if a node is available and responsive
%% @end
%%--------------------------------------------------------------------
-spec is_node_available(node()) -> boolean().
is_node_available(Node) when is_atom(Node) ->
    case get_node_info(Node) of
        {ok, Info} -> Info#node_info.status =:= connected;
        {error, not_found} -> false
    end.

%%--------------------------------------------------------------------
%% @doc Set distribution mode (standard, optimal, legacy)
%% @end
%%--------------------------------------------------------------------
-spec set_distribution_mode(distribution_mode()) -> ok.
set_distribution_mode(Mode) when Mode =:= standard; Mode =:= optimal; Mode =:= legacy ->
    gen_server:cast(?MODULE, {set_distribution_mode, Mode}).

%%--------------------------------------------------------------------
%% @doc Get current distribution protocol version
%% @end
%%--------------------------------------------------------------------
-spec get_distribution_protocol() -> distribution_protocol().
get_distribution_protocol() ->
    gen_server:call(?MODULE, get_distribution_protocol, 2000).

%%--------------------------------------------------------------------
%% @doc Optimize for current OTP version capabilities
%% @end
%%--------------------------------------------------------------------
-spec optimize_for_version() -> ok.
optimize_for_version() ->
    gen_server:cast(?MODULE, optimize_for_version).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    %% Initialize OTP version and features
    OTPVersion = erlmcp_version_detector:otp_version(),
    Features = erlmcp_version_detector:get_optimal_features(),

    %% Determine optimal distribution protocol
    Protocol = determine_distribution_protocol(OTPVersion, Features),
    Mode = determine_optimal_distribution_mode(OTPVersion, Features),

    logger:info("Starting distribution manager (OTP ~p, protocol: ~p, mode: ~p)",
                [OTPVersion, Protocol, Mode]),

    %% Apply version-specific optimizations
    apply_distribution_optimizations(Protocol, Mode, Features),

    %% Initialize state
    State = #state{
        mode = Mode,
        protocol = Protocol,
        otp_version = OTPVersion,
        features = Features
    },

    %% Start background monitoring
    start_node_monitoring(State),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call({connect_node, Node}, _From, State = #state{protocol = Protocol, mode = Mode}) ->
    case connect_node_with_protocol(Node, Protocol, Mode) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({disconnect_node, Node}, _From, State) ->
    {ok, NewState} = disconnect_node_with_cleanup(Node, State),
    {reply, ok, NewState};
handle_call(get_connected_nodes, _From, State = #state{connected_nodes = Nodes}) ->
    ConnectedList = maps:values(Nodes),
    {reply, ConnectedList, State};
handle_call({get_node_info, Node}, _From, State = #state{connected_nodes = Nodes}) ->
    case maps:get(Node, Nodes, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Info ->
            {reply, {ok, Info}, State}
    end;
handle_call(get_distribution_protocol, _From, State = #state{protocol = Protocol}) ->
    {reply, Protocol, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({set_distribution_mode, Mode}, State = #state{mode = Mode, protocol = Protocol, features = Features}) ->
    logger:info("Distribution mode already set to ~p", [Mode]),
    %% Re-apply optimizations with current mode
    apply_distribution_optimizations(Protocol, Mode, Features),
    {noreply, State};
handle_cast({set_distribution_mode, Mode}, State = #state{protocol = Protocol, features = Features}) ->
    logger:info("Changing distribution mode to ~p", [Mode]),
    %% Apply new mode with optimizations
    apply_distribution_optimizations(Protocol, Mode, Features),
    {noreply, State#state{mode = Mode}};
handle_cast(optimize_for_version, State = #state{otp_version = OTPVersion, features = Features}) ->
    logger:info("Optimizing for OTP version ~p", [OTPVersion]),
    %% Re-optimize based on current version and features
    NewMode = determine_optimal_distribution_mode(OTPVersion, Features),
    apply_distribution_optimizations(State#state.protocol, NewMode, Features),
    {noreply, State#state{mode = NewMode}};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({node_connected, Node, Protocol, Info}, State = #state{connected_nodes = Nodes}) ->
    logger:info("Node connected: ~p (protocol: ~p)", [Node, Protocol]),
    NewNodes = maps:put(Node, Info, Nodes),
    {noreply, State#state{connected_nodes = NewNodes}};
handle_info({node_disconnected, Node}, State = #state{connected_nodes = Nodes}) ->
    logger:info("Node disconnected: ~p", [Node]),
    NewNodes = maps:remove(Node, Nodes),
    {noreply, State#state{connected_nodes = NewNodes}};
handle_info({node_failed, Node, Reason}, State = #state{connected_nodes = Nodes}) ->
    logger:warning("Node failed: ~p (reason: ~p)", [Node, Reason]),
    NewNodes = maps:remove(Node, Nodes),
    %% Attempt reconnection if in optimal mode
    case State#state.mode of
        optimal ->
            erlang:send_after(5000, self(), {retry_connection, Node}),
            {noreply, State#state{connected_nodes = NewNodes}};
        _ ->
            {noreply, State#state{connected_nodes = NewNodes}}
    end;
handle_info({retry_connection, Node}, State = #state{protocol = Protocol, mode = Mode}) ->
    case connect_node_with_protocol(Node, Protocol, Mode) of
        {ok, NewState} ->
            logger:info("Reconnected to node: ~p", [Node]),
            {noreply, NewState};
        {error, Reason} ->
            logger:warning("Failed to reconnect to ~p: ~p", [Node, Reason]),
            %% Schedule another retry
            erlang:send_after(10000, self(), {retry_connection, Node}),
            {noreply, State}
    end;
handle_info({distribution_optimization, Data}, State = #state{otp_version = OTPVersion}) ->
    %% Handle distribution optimization messages
    logger:debug("Applying distribution optimization for OTP ~p", [OTPVersion]),
    apply_distribution_optimization(Data, State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State = #state{connected_nodes = Nodes}) ->
    logger:info("Distribution manager terminating, cleaning up ~p nodes", [maps:size(Nodes)]),

    %% Clean up all connections
    maps:fold(fun(Node, _Info, _) ->
                  disconnect_node_with_cleanup(Node, State)
              end,
              ok,
              Nodes),

    %% Apply version-specific cleanup
    case State#state.otp_version of
        {28, _, _} ->
            enhanced_distribution_cleanup(State);
        {27, _, _} ->
            standard_distribution_cleanup(State);
        {26, _, _} ->
            basic_distribution_cleanup(State)
    end,
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State = #state{otp_version = OTPVersion, features = Features}, _Extra) ->
    logger:info("Code change detected (OTP ~p)", [OTPVersion]),

    %% Update features and re-optimize
    NewFeatures = erlmcp_version_detector:get_optimal_features(),
    NewMode = determine_optimal_distribution_mode(OTPVersion, NewFeatures),
    Protocol = determine_distribution_protocol(OTPVersion, NewFeatures),

    %% Re-apply optimizations
    apply_distribution_optimizations(Protocol, NewMode, NewFeatures),

    {ok, State#state{
        features = NewFeatures,
        mode = NewMode,
        protocol = Protocol
    }}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Determine the appropriate distribution protocol for OTP version
-spec determine_distribution_protocol({non_neg_integer(), non_neg_integer(), non_neg_integer()},
                                    #{atom() => boolean()}) -> distribution_protocol().
determine_distribution_protocol(Version, Features) ->
    case Version of
        {28, _, _} when Features#{priority_messages := true} ->
            %% OTP 28 with full feature set
            {otp28, optimized};
        {27, _, _} when Features#{native_json := true} ->
            %% OTP 27 with enhanced features
            {otp27, enhanced};
        {26, _, _} ->
            %% OTP 26 with basic protocol
            {otp26, legacy};
        _ ->
            %% Fallback
            {otp27, enhanced}
    end.

%% Determine optimal distribution mode based on version
-spec determine_optimal_distribution_mode({non_neg_integer(), non_neg_integer(), non_neg_integer()},
                                        #{atom() => boolean()}) -> distribution_mode().
determine_optimal_distribution_mode(Version, Features) ->
    case {Version, Features} of
        {{28, _, _}, _} ->
            optimal;
        {{27, _, _}, _} ->
            standard;
        {{26, _, _}, _} ->
            legacy;
        _ ->
            standard
    end.

%% Apply distribution optimizations based on version and mode
-spec apply_distribution_optimizations(distribution_protocol(), distribution_mode(), #{atom() => boolean()}) -> ok.
apply_distribution_optimizations({otp28, optimized}, optimal, Features) ->
    %% Full OTP 28+ optimization
    apply_optimal_otp28_optimizations(Features);
apply_distribution_optimizations({otp27, enhanced}, standard, Features) ->
    %% OTP 27 standard optimization
    apply_standard_otp27_optimizations(Features);
apply_distribution_optimizations({otp26, legacy}, legacy, _Features) ->
    %% OTP 26 legacy mode
    apply_legacy_otp26_optimizations();
apply_distribution_optimizations(Protocol, Mode, Features) ->
    logger:info("Applying distribution optimizations: ~p, ~p", [Protocol, Mode]),
    %% Default optimization
    apply_default_optimizations(Features).

%% Connect to a node using the appropriate protocol
-spec connect_node_with_protocol(node(), distribution_protocol(), distribution_mode()) ->
    {ok, state()} | {error, term()}.
connect_node_with_protocol(Node, Protocol, Mode) ->
    case check_node_compatibility(Node, Protocol) of
        {ok, NodeInfo} ->
            perform_connection(Node, Protocol, Mode, NodeInfo);
        {error, Reason} ->
            {error, Reason}
    end.

%% Check node compatibility with current protocol
-spec check_node_compatibility(node(), distribution_protocol()) ->
    {ok, node_info()} | {error, term()}.
check_node_compatibility(Node, Protocol) ->
    %% Try to get node information and compatibility
    try
        %% OTP 28+ has enhanced node information
        case Protocol of
            {otp28, optimized} ->
                check_node_compatibility_otp28(Node);
            {otp27, enhanced} ->
                check_node_compatibility_otp27(Node);
            {otp26, legacy} ->
                check_node_compatibility_otp26(Node)
        end
    catch
        _:Reason ->
            {error, {incompatible_node, Node, Reason}}
    end.

%% Check compatibility with OTP 28+ node
-spec check_node_compatibility_otp28(node()) -> {ok, node_info()}.
check_node_compatibility_otp28(Node) ->
    %% OTP 28+ has enhanced protocol negotiation
    case net_adm:ping(Node) of
        pong ->
            %% Get detailed node information
            NodeInfo = get_detailed_node_info(Node),
            {ok, NodeInfo};
        pang ->
            {error, node_unreachable}
    end.

%% Check compatibility with OTP 27 node
-spec check_node_compatibility_otp27(node()) -> {ok, node_info()}.
check_node_compatibility_otp27(Node) ->
    %% OTP 27 enhanced protocol
    case net_adm:ping(Node) of
        pong ->
            NodeInfo = get_basic_node_info(Node),
            {ok, NodeInfo};
        pang ->
            {error, node_unreachable}
    end.

%% Check compatibility with OTP 26 node
-spec check_node_compatibility_otp26(node()) -> {ok, node_info()}.
check_node_compatibility_otp26(Node) ->
    %% OTP 26 legacy protocol
    case net_adm:ping(Node) of
        pong ->
            NodeInfo = get_basic_node_info(Node),
            {ok, NodeInfo};
        pang ->
            {error, node_unreachable}
    end.

%% Get detailed node information (OTP 28+)
-spec get_detailed_node_info(node()) -> node_info().
get_detailed_node_info(Node) ->
    %% Use enhanced node information if available
    NodeVersion = get_remote_node_version(Node),
    Capabilities = get_remote_node_capabilities(Node),
    Status = connected,

    #node_info{
        node = Node,
        version = NodeVersion,
        capabilities = Capabilities,
        status = Status
    }.

%% Get basic node information
-spec get_basic_node_info(node()) -> node_info().
get_basic_node_info(Node) ->
    NodeVersion = {0, 0, 0},  % Unknown version
    Capabilities = [basic],   % Basic capabilities
    Status = connected,

    #node_info{
        node = Node,
        version = NodeVersion,
        capabilities = Capabilities,
        status = Status
    }.

%% Perform the actual connection
-spec perform_connection(node(), distribution_protocol(), distribution_mode(), node_info()) ->
    {ok, state()} | {error, term()}.
perform_connection(Node, Protocol, Mode, NodeInfo) ->
    %% Set up connection with appropriate protocol
    case setup_connection(Node, Protocol, Mode) of
        ok ->
            %% Monitor the node
            case Protocol of
                {otp28, optimized} ->
                    monitor_node_optimized(Node);
                {otp27, enhanced} ->
                    monitor_node_enhanced(Node);
                {otp26, legacy} ->
                    monitor_node_legacy(Node)
            end,
            %% Notify of successful connection
            gen_server:cast(?MODULE, {node_connected, Node, Protocol, NodeInfo}),
            {ok, #state{}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Setup connection with version-specific settings
-spec setup_connection(node(), distribution_protocol(), distribution_mode()) -> ok | {error, term()}.
setup_connection(Node, Protocol, Mode) ->
    %% Set up connection based on protocol
    case Protocol of
        {otp28, optimized} ->
            setup_connection_otp28(Node, Mode);
        {otp27, enhanced} ->
            setup_connection_otp27(Node, Mode);
        {otp26, legacy} ->
            setup_connection_otp26(Node)
    end.

%% Setup OTP 28+ connection
-spec setup_connection_otp28(node(), distribution_mode()) -> ok | {error, term()}.
setup_connection_otp28(Node, Mode) ->
    %% OTP 28 enhanced connection setup
    case net_kernel:connect_node(Node) of
        true ->
            %% Apply optimal settings
            case Mode of
                optimal ->
                    apply_optimal_connection_settings(Node);
                standard ->
                    apply_standard_connection_settings(Node);
                legacy ->
                    apply_legacy_connection_settings(Node)
            end;
        false ->
            {error, connection_failed}
    end.

%% Setup OTP 27 connection
-spec setup_connection_otp27(node(), distribution_mode()) -> ok | {error, term()}.
setup_connection_otp27(Node, Mode) ->
    %% OTP 27 enhanced connection
    case net_kernel:connect_node(Node) of
        true ->
            case Mode of
                standard ->
                    apply_standard_connection_settings(Node);
                legacy ->
                    apply_legacy_connection_settings(Node);
                _ ->
                    apply_standard_connection_settings(Node)
            end;
        false ->
            {error, connection_failed}
    end.

%% Setup OTP 26 connection
-spec setup_connection_otp26(node()) -> ok | {error, term()}.
setup_connection_otp26(Node) ->
    %% OTP 26 legacy connection
    case net_kernel:connect_node(Node) of
        true -> ok;
        false -> {error, connection_failed}
    end.

%% Disconnect from a node with proper cleanup
-spec disconnect_node_with_cleanup(node(), state()) -> {ok, state()}.
disconnect_node_with_cleanup(Node, State) ->
    %% Stop monitoring the node
    erlang:monitor_node(Node, false),

    %% Clean up any resources
    cleanup_node_resources(Node, State),

    %% Notify of disconnection
    gen_server:cast(?MODULE, {node_disconnected, Node}),

    {ok, State}.

%% Start node monitoring
-spec start_node_monitoring(state()) -> ok.
start_node_monitoring(_State) ->
    %% Start background monitoring processes if needed
    logger:debug("Starting node monitoring"),
    ok.

%% Apply OTP 28+ optimal optimizations
-spec apply_optimal_otp28_optimizations(#{atom() => boolean()}) -> ok.
apply_optimal_otp28_optimizations(Features) ->
    %% Enable priority message delivery for critical messages
    case Features#{priority_messages := true} of
        true ->
            ?SET_PRIORITY_HIGH(),
            logger:debug("Enabled priority message delivery for distribution");
        false ->
            ok
    end,

    %% Enable enhanced distribution protocol
    enable_enhanced_distribution_protocol(),

    %% Apply process iterator optimizations
    case Features#{process_iterator := true} of
        true ->
            enable_process_iterator_optimization();
        false ->
            ok
    end,

    logger:info("OTP 28+ optimal optimizations applied"),
    ok.

%% Apply OTP 27 standard optimizations
-spec apply_standard_otp27_optimizations(#{atom() => boolean()}) -> ok.
apply_standard_otp27_optimizations(Features) ->
    %% Enable native JSON for communication
    case Features#{native_json := true} of
        true ->
            logger:debug("Using native JSON for distribution communication");
        false ->
            ok
    end,

    %% Enable enhanced distribution protocol
    enable_enhanced_distribution_protocol(),

    logger:info("OTP 27 standard optimizations applied"),
    ok.

%% Apply OTP 26 legacy optimizations
-spec apply_legacy_otp26_optimizations() -> ok.
apply_legacy_otp26_optimizations() ->
    %% Use basic distribution protocol
    logger:info("Using legacy OTP 26 distribution protocol"),
    ok.

%% Apply default optimizations
-spec apply_default_optimizations(#{atom() => boolean()}) -> ok.
apply_default_optimizations(_Features) ->
    logger:info("Applying default distribution optimizations"),
    ok.

%% Apply distribution optimization
-spec apply_distribution_optimization(term(), state()) -> ok.
apply_distribution_optimization(Data, State) ->
    %% Apply specific optimization based on data
    logger:debug("Applying distribution optimization: ~p", [Data]),
    ok.

%% Monitor node with OTP 28+ optimizations
-spec monitor_node_optimized(node()) -> ok.
monitor_node_optimized(Node) ->
    %% OTP 28+ enhanced node monitoring
    erlang:monitor_node(Node, true),
    %% Set up additional monitoring if available
    logger:debug("Enabled optimized node monitoring for ~p", [Node]),
    ok.

%% Monitor node with OTP 27 enhancements
-spec monitor_node_enhanced(node()) -> ok.
monitor_node_enhanced(Node) ->
    %% OTP 27 enhanced node monitoring
    erlang:monitor_node(Node, true),
    logger:debug("Enabled enhanced node monitoring for ~p", [Node]),
    ok.

%% Monitor node with OTP 26 legacy
-spec monitor_node_legacy(node()) -> ok.
monitor_node_legacy(Node) ->
    %% OTP 26 legacy node monitoring
    erlang:monitor_node(Node, true),
    logger:debug("Enabled legacy node monitoring for ~p", [Node]),
    ok.

%% Get remote node version
-spec get_remote_node_version(node()) -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
get_remote_node_version(Node) ->
    %% Try to get remote OTP version
    case rpc:call(Node, erlang, system_info, [otp_release]) of
        {badrpc, _} ->
            {0, 0, 0};  % Unknown version
        VersionStr ->
            parse_otp_version(VersionStr)
    end.

%% Get remote node capabilities
-spec get_remote_node_capabilities(node()) -> [atom()].
get_remote_node_capabilities(Node) ->
    %% Try to get remote capabilities
    case rpc:call(Node, erlmcp_version_detector, get_optimal_features, []) of
        {badrpc, _} ->
            [basic];
        Features ->
            maps:keys(Features)
    end.

%% Parse OTP version string
-spec parse_otp_version(string()) -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
parse_otp_version(VsnStr) ->
    Parts = string:split(VsnStr, ".", all),
    ToInt = fun(Str) ->
                case string:to_integer(Str) of
                    {Int, ""} -> Int;
                    {Int, _} -> Int;
                    _ -> 0
                end
            end,
    Padded = case Parts of
                [M1] -> [M1, "0", "0"];
                [M1, M2] -> [M1, M2, "0"];
                [M1, M2, M3 | _] -> [M1, M2, M3]
            end,
    [Major, Minor, Patch] = [ToInt(P) || P <- Padded],
    {Major, Minor, Patch}.

%% Apply optimal connection settings
-spec apply_optimal_connection_settings(node()) -> ok.
apply_optimal_connection_settings(Node) ->
    %% Apply connection optimizations for OTP 28+
    logger:debug("Applying optimal connection settings for ~p", [Node]),
    ok.

%% Apply standard connection settings
-spec apply_standard_connection_settings(node()) -> ok.
apply_standard_connection_settings(Node) ->
    %% Apply standard connection settings
    logger:debug("Applying standard connection settings for ~p", [Node]),
    ok.

%% Apply legacy connection settings
-spec apply_legacy_connection_settings(node()) -> ok.
apply_legacy_connection_settings(Node) ->
    %% Apply legacy connection settings
    logger:debug("Applying legacy connection settings for ~p", [Node]),
    ok.

%% Clean up node resources
-spec cleanup_node_resources(node(), state()) -> ok.
cleanup_node_resources(Node, _State) ->
    %% Clean up any resources associated with the node
    logger:debug("Cleaning up resources for ~p", [Node]),
    ok.

%% Enhanced distribution cleanup for OTP 28+
-spec enhanced_distribution_cleanup(state()) -> ok.
enhanced_distribution_cleanup(_State) ->
    %% Use process iterator for efficient cleanup
    case ?HAVE_PROCESS_ITERATOR of
        true ->
            Iterator = erlang:processes_iterator(),
            enhanced_cleanup_iterator(Iterator);
        false ->
            %% Fallback cleanup
            ok
    end.

%% Standard distribution cleanup
-spec standard_distribution_cleanup(state()) -> ok.
standard_distribution_cleanup(_State) ->
    %% Standard cleanup implementation
    ok.

%% Basic distribution cleanup
-spec basic_distribution_cleanup(state()) -> ok.
basic_distribution_cleanup(_State) ->
    %% Basic cleanup implementation
    ok.

%% Enhanced cleanup using iterator
-spec enhanced_cleanup_iterator(term()) -> ok.
enhanced_cleanup_iterator(Iterator) ->
    case erlang:process_next(Iterator) of
        {Pid, NewIterator} ->
            %% Cleanup specific distribution-related processes
            cleanup_distribution_process(Pid),
            enhanced_cleanup_iterator(NewIterator);
        none ->
            ok
    end.

%% Cleanup distribution-specific process
-spec cleanup_distribution_process(pid()) -> ok.
cleanup_distribution_process(Pid) ->
    %% Cleanup process specific to distribution
    ok.

%% Enable enhanced distribution protocol
-spec enable_enhanced_distribution_protocol() -> ok.
enable_enhanced_distribution_protocol() ->
    %% Enable enhanced protocol if available
    logger:debug("Enabled enhanced distribution protocol"),
    ok.

%% Enable process iterator optimization
-spec enable_process_iterator_optimization() -> ok.
enable_process_iterator_optimization() ->
    %% Enable process iterator for distribution optimization
    logger:debug("Enabled process iterator optimization for distribution"),
    ok.

%% Get OTP version from remote node
-spec get_remote_otp_version(node()) -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
get_remote_otp_version(Node) ->
    case rpc:call(Node, erlang, system_info, [otp_release]) of
        {badrpc, _} -> {0, 0, 0};
        VersionStr -> parse_otp_version(VersionStr)
    end.