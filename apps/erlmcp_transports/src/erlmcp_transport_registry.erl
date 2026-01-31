%%%-------------------------------------------------------------------
%%% @doc
%%% Transport Registry - Automatic Transport Discovery and Management
%%%
%%% This module provides a centralized registry for all transport types,
%%% supporting automatic discovery, registration, health monitoring,
%%% and capability querying.
%%%
%%% == Features ==
%%%
%%% 1. <b>Transport Registration</b>: All transports register on startup
%%% 2. <b>Automatic Discovery</b>: Find transports by type, capability, or ID
%%% 3. <b>Health Monitoring</b>: Integrated with erlmcp_transport_health
%%% 4. <b>Capability Queries</b>: Find transports supporting specific features
%%% 5. <b>Transport Lifecycle</b>: Track transport creation, updates, removal
%%%
%%% == Transport Types ==
%%%
%%% - <b>stdio</b>: Standard input/output transport
%%% - <b>tcp</b>: TCP transport (client/server modes)
%%% - <b>http</b>: HTTP transport (REST/JSON-RPC)
%%% - <b>websocket</b>: WebSocket transport (real-time duplex)
%%% - <b>sse</b>: Server-Sent Events transport (unidirectional)
%%%
%%% == Capabilities ==
%%%
%%% Transports may declare capabilities:
%%% - <b>resources</b>: Supports resource operations
%%% - <b>tools</b>: Supports tool invocation
%%% - <b>prompts</b>: Supports prompt templates
%%% - <b>logging</b>: Supports logging operations
%%% - <b>notifications</b>: Supports push notifications
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_registry).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    register_transport/3,
    register_transport/4,
    unregister_transport/1,
    unregister_transport/2,
    get_transport/1,
    get_transport/2,
    list_transports/0,
    list_transports/1,
    list_transports_by_type/1,
    list_transports_by_capability/1,
    find_transport_by_capability/1,
    update_transport_config/2,
    get_transport_capabilities/1,
    get_transport_stats/1,
    get_all_stats/0,
    health_check/1,
    health_check_all/0,
    discover_transports/0,
    discover_transports/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type transport_id() :: atom() | binary().
-type transport_type() :: stdio | tcp | http | websocket | sse | custom.
-type transport_status() :: initializing | running | stopping | stopped | error.
-type transport_capability() :: resources | tools | prompts | logging | notifications.
-type transport_scope() :: local | global.

-record(transport_info, {
    id :: transport_id(),
    type :: transport_type(),
    pid :: pid() | undefined,
    status :: transport_status(),
    capabilities :: [transport_capability()],
    config :: map(),
    stats :: #{
        created_at => integer(),
        last_activity => integer(),
        messages_sent => non_neg_integer(),
        messages_received => non_neg_integer(),
        bytes_sent => non_neg_integer(),
        bytes_received => non_neg_integer(),
        errors => non_neg_integer()
    },
    metadata :: map()
}).

-type transport_info() :: #transport_info{}.

-record(state, {
    transports = #{} :: #{transport_id() => transport_info()},
    by_type = #{} :: #{transport_type() => [transport_id()]},
    by_capability = #{} :: #{transport_capability() => [transport_id()]},
    scope = local :: transport_scope()
}).

-type state() :: #state{}.

-define(SERVER, ?MODULE).
-define(DEFAULT_HEALTH_CHECK_INTERVAL, 30000).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the transport registry with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the transport registry with custom options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Register a transport (local scope)
-spec register_transport(transport_id(), pid() | undefined, map()) ->
    ok | {error, term()}.
register_transport(TransportId, Pid, Config) ->
    register_transport(local, TransportId, Pid, Config).

%% @doc Register a transport with explicit scope
-spec register_transport(transport_scope(), transport_id(), pid() | undefined, map()) ->
    ok | {error, term()}.
register_transport(Scope, TransportId, Pid, Config) when is_map(Config) ->
    gen_server:call(?SERVER, {register_transport, Scope, TransportId, Pid, Config}).

%% @doc Unregister a transport (local scope)
-spec unregister_transport(transport_id()) -> ok.
unregister_transport(TransportId) ->
    unregister_transport(local, TransportId).

%% @doc Unregister a transport with explicit scope
-spec unregister_transport(transport_scope(), transport_id()) -> ok.
unregister_transport(Scope, TransportId) ->
    gen_server:call(?SERVER, {unregister_transport, Scope, TransportId}).

%% @doc Get transport information (local scope)
-spec get_transport(transport_id()) ->
    {ok, map()} | {error, not_found}.
get_transport(TransportId) ->
    get_transport(local, TransportId).

%% @doc Get transport information with explicit scope
-spec get_transport(transport_scope(), transport_id()) ->
    {ok, map()} | {error, not_found}.
get_transport(Scope, TransportId) ->
    gen_server:call(?SERVER, {get_transport, Scope, TransportId}).

%% @doc List all transports (local scope)
-spec list_transports() -> [{transport_id(), map()}].
list_transports() ->
    list_transports(local).

%% @doc List transports with explicit scope
-spec list_transports(transport_scope()) -> [{transport_id(), map()}].
list_transports(Scope) ->
    gen_server:call(?SERVER, {list_transports, Scope}).

%% @doc List transports by type
-spec list_transports_by_type(transport_type()) -> [{transport_id(), map()}].
list_transports_by_type(Type) ->
    gen_server:call(?SERVER, {list_transports_by_type, Type}).

%% @doc List transports by capability
-spec list_transports_by_capability(transport_capability()) -> [{transport_id(), map()}].
list_transports_by_capability(Capability) ->
    gen_server:call(?SERVER, {list_transports_by_capability, Capability}).

%% @doc Find a transport that supports a specific capability
%% Returns the first healthy transport matching the capability
-spec find_transport_by_capability(transport_capability()) ->
    {ok, transport_id(), map()} | {error, no_suitable_transport}.
find_transport_by_capability(Capability) ->
    gen_server:call(?SERVER, {find_transport_by_capability, Capability}).

%% @doc Update transport configuration
-spec update_transport_config(transport_id(), map()) ->
    ok | {error, not_found}.
update_transport_config(TransportId, ConfigUpdate) ->
    gen_server:call(?SERVER, {update_transport_config, TransportId, ConfigUpdate}).

%% @doc Get transport capabilities
-spec get_transport_capabilities(transport_id()) ->
    {ok, [transport_capability()]} | {error, not_found}.
get_transport_capabilities(TransportId) ->
    gen_server:call(?SERVER, {get_transport_capabilities, TransportId}).

%% @doc Get transport statistics
-spec get_transport_stats(transport_id()) ->
    {ok, map()} | {error, not_found}.
get_transport_stats(TransportId) ->
    gen_server:call(?SERVER, {get_transport_stats, TransportId}).

%% @doc Get statistics for all transports
-spec get_all_stats() -> #{transport_id() => map()}.
get_all_stats() ->
    gen_server:call(?SERVER, get_all_stats).

%% @doc Perform health check on a specific transport
-spec health_check(transport_id()) ->
    {ok, healthy | degraded | unhealthy} | {error, not_found}.
health_check(TransportId) ->
    gen_server:call(?SERVER, {health_check, TransportId}).

%% @doc Perform health check on all transports
-spec health_check_all() -> #{transport_id() => healthy | degraded | unhealthy}.
health_check_all() ->
    gen_server:call(?SERVER, health_check_all).

%% @doc Discover available transports from code modules
%% Automatically scans for transport modules and registers them
-spec discover_transports() -> {ok, [transport_id()]} | {error, term()}.
discover_transports() ->
    discover_transports(#{timeout => 5000}).

%% @doc Discover available transports with options
-spec discover_transports(map()) -> {ok, [transport_id()]} | {error, term()}.
discover_transports(Opts) when is_map(Opts) ->
    gen_server:call(?SERVER, {discover_transports, Opts}, maps:get(timeout, Opts, 5000)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init(Opts) ->
    Scope = maps:get(scope, Opts, local),
    ?LOG_INFO("Starting transport registry with scope: ~p", [Scope]),

    % Initialize with known transport modules
    State = #state{
        scope = Scope,
        transports = #{},
        by_type = #{},
        by_capability = #{}
    },

    % Optionally auto-discover transports on startup
    case maps:get(auto_discover, Opts, false) of
        true ->
            {ok, _} = discover_transports_internal(State),
            {ok, State};
        false ->
            {ok, State}
    end.

%% @private
handle_call({register_transport, Scope, _TransportId, _Pid, _Config}, _From,
            #state{scope = RegistryScope} = State) when Scope =/= RegistryScope ->
    {reply, {error, scope_mismatch}, State};

handle_call({register_transport, _Scope, TransportId, Pid, Config}, _From, State) ->
    case maps:is_key(TransportId, State#state.transports) of
        true ->
            {reply, {error, already_registered}, State};
        false ->
            % Extract transport information from config
            Type = maps:get(type, Config, custom),
            Capabilities = maps:get(capabilities, Config, []),
            Metadata = maps:get(metadata, Config, #{}),

            % Monitor the transport process if Pid is provided
            case Pid of
                undefined -> ok;
                _ -> erlang:monitor(process, Pid)
            end,

            % Create transport info record
            TransportInfo = #transport_info{
                id = TransportId,
                type = Type,
                pid = Pid,
                status = initializing,
                capabilities = normalize_capabilities(Capabilities),
                config = sanitize_config(Config),
                stats = initial_stats(),
                metadata = Metadata
            },

            % Update indices
            NewState = add_to_indices(TransportId, Type, Capabilities, TransportInfo, State),

            ?LOG_INFO("Registered transport: ~p (type: ~p, capabilities: ~p)",
                     [TransportId, Type, Capabilities]),
            {reply, ok, NewState#state{transports = maps:put(TransportId, TransportInfo, NewState#state.transports)}}
    end;

handle_call({unregister_transport, Scope, _TransportId}, _From,
            #state{scope = RegistryScope} = State) when Scope =/= RegistryScope ->
    {reply, {error, scope_mismatch}, State};

handle_call({unregister_transport, _Scope, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            {reply, ok, State};
        TransportInfo ->
            % Remove from indices
            NewState = remove_from_indices(TransportId, TransportInfo, State),

            % Demonitor if process is alive
            case TransportInfo#transport_info.pid of
                undefined -> ok;
                Pid when is_pid(Pid) ->
                    case is_process_alive(Pid) of
                        true -> erlang:demonitor(Pid, [flush]);
                        false -> ok
                    end
            end,

            ?LOG_INFO("Unregistered transport: ~p", [TransportId]),
            {reply, ok, NewState#state{transports = maps:remove(TransportId, NewState#state.transports)}}
    end;

handle_call({get_transport, Scope, _TransportId}, _From,
            #state{scope = RegistryScope} = State) when Scope =/= RegistryScope ->
    {reply, {error, scope_mismatch}, State};

handle_call({get_transport, _Scope, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TransportInfo ->
            ResponseMap = transport_info_to_map(TransportInfo),
            {reply, {ok, ResponseMap}, State}
    end;

handle_call({list_transports, Scope}, _From,
            #state{scope = RegistryScope} = State) when Scope =/= RegistryScope ->
    {reply, [], State};

handle_call({list_transports, _Scope}, _From, State) ->
    TransportList = maps:fold(fun(TransportId, TransportInfo, Acc) ->
        [{TransportId, transport_info_to_map(TransportInfo)} | Acc]
    end, [], State#state.transports),
    {reply, lists:reverse(TransportList), State};

handle_call({list_transports_by_type, Type}, _From, State) ->
    TransportIds = maps:get(Type, State#state.by_type, []),
    TransportList = lists:filtermap(fun(TransportId) ->
        case maps:get(TransportId, State#state.transports, undefined) of
            undefined -> false;
            TransportInfo -> {true, {TransportId, transport_info_to_map(TransportInfo)}}
        end
    end, TransportIds),
    {reply, TransportList, State};

handle_call({list_transports_by_capability, Capability}, _From, State) ->
    TransportIds = maps:get(Capability, State#state.by_capability, []),
    TransportList = lists:filtermap(fun(TransportId) ->
        case maps:get(TransportId, State#state.transports, undefined) of
            undefined -> false;
            TransportInfo -> {true, {TransportId, transport_info_to_map(TransportInfo)}}
        end
    end, TransportIds),
    {reply, TransportList, State};

handle_call({find_transport_by_capability, Capability}, _From, State) ->
    case maps:get(Capability, State#state.by_capability, []) of
        [] ->
            {reply, {error, no_suitable_transport}, State};
        TransportIds ->
            % Find first healthy transport
            case find_healthy_transport(TransportIds, State) of
                {ok, TransportId} ->
                    TransportInfo = maps:get(TransportId, State#state.transports),
                    {reply, {ok, TransportId, transport_info_to_map(TransportInfo)}, State};
                {error, _} = Error ->
                    {reply, Error, State}
            end
    end;

handle_call({update_transport_config, TransportId, ConfigUpdate}, _From, State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TransportInfo ->
            % Update config, merging with existing
            CurrentConfig = TransportInfo#transport_info.config,
            NewConfig = maps:merge(CurrentConfig, sanitize_config(ConfigUpdate)),

            % Update capabilities if provided
            NewCapabilities = case maps:get(capabilities, ConfigUpdate, undefined) of
                undefined -> TransportInfo#transport_info.capabilities;
                Caps -> normalize_capabilities(Caps)
            end,

            UpdatedInfo = TransportInfo#transport_info{
                config = NewConfig,
                capabilities = NewCapabilities
            },

            ?LOG_DEBUG("Updated config for transport: ~p", [TransportId]),
            {reply, ok, State#state{transports = maps:put(TransportId, UpdatedInfo, State#state.transports)}}
    end;

handle_call({get_transport_capabilities, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TransportInfo ->
            {reply, {ok, TransportInfo#transport_info.capabilities}, State}
    end;

handle_call({get_transport_stats, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TransportInfo ->
            Stats = TransportInfo#transport_info.stats,
            {reply, {ok, Stats}, State}
    end;

handle_call(get_all_stats, _From, State) ->
    AllStats = maps:map(fun(_TransportId, TransportInfo) ->
        TransportInfo#transport_info.stats
    end, State#state.transports),
    {reply, AllStats, State};

handle_call({health_check, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TransportInfo ->
            HealthStatus = check_transport_health(TransportInfo),
            {reply, {ok, HealthStatus}, State}
    end;

handle_call(health_check_all, _From, State) ->
    HealthResults = maps:map(fun(_TransportId, TransportInfo) ->
        check_transport_health(TransportInfo)
    end, State#state.transports),
    {reply, HealthResults, State};

handle_call({discover_transports, Opts}, _From, State) ->
    case discover_transports_internal(State) of
        {ok, DiscoveredIds, NewState} ->
            Timeout = maps:get(timeout, Opts, 5000),
            % Wait a bit for transports to initialize
            timer:sleep(min(Timeout, 1000)),
            {reply, {ok, DiscoveredIds}, NewState};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, State) ->
    % Find and mark transport as stopped/errored
    NewTransports = maps:map(fun(TransportId, TransportInfo) ->
        case TransportInfo#transport_info.pid of
            Pid ->
                ?LOG_WARNING("Transport process ~p died: ~p", [TransportId, Reason]),
                TransportInfo#transport_info{
                    status = error,
                    pid = undefined
                };
            _ ->
                TransportInfo
        end
    end, State#state.transports),
    {noreply, State#state{transports = NewTransports}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Add transport to type and capability indices
-spec add_to_indices(transport_id(), transport_type(), [transport_capability()],
                     transport_info(), state()) -> state().
add_to_indices(TransportId, Type, Capabilities, _TransportInfo, State) ->
    % Update type index
    ByType = State#state.by_type,
    TypeList = maps:get(Type, ByType, []),
    NewByType = maps:put(Type, [TransportId | TypeList], ByType),

    % Update capability indices
    ByCapability = State#state.by_capability,
    NewByCapability = lists:foldl(fun(Capability, Acc) ->
        CapList = maps:get(Capability, Acc, []),
        maps:put(Capability, [TransportId | CapList], Acc)
    end, ByCapability, Capabilities),

    State#state{
        by_type = NewByType,
        by_capability = NewByCapability
    }.

%% @private
%% Remove transport from type and capability indices
-spec remove_from_indices(transport_id(), transport_info(), state()) -> state().
remove_from_indices(TransportId, TransportInfo, State) ->
    Type = TransportInfo#transport_info.type,
    Capabilities = TransportInfo#transport_info.capabilities,

    % Update type index
    ByType = State#state.by_type,
    NewByType = case maps:get(Type, ByType, []) of
        [] -> ByType;
        TypeList ->
            FilteredList = lists:filter(fun(Id) -> Id =/= TransportId end, TypeList),
            case FilteredList of
                [] -> maps:remove(Type, ByType);
                _ -> maps:put(Type, FilteredList, ByType)
            end
    end,

    % Update capability indices
    ByCapability = State#state.by_capability,
    NewByCapability = lists:foldl(fun(Capability, Acc) ->
        case maps:get(Capability, Acc, []) of
            [] -> Acc;
            CapList ->
                CapFilteredList = lists:filter(fun(Id) -> Id =/= TransportId end, CapList),
                case CapFilteredList of
                    [] -> maps:remove(Capability, Acc);
                    _ -> maps:put(Capability, CapFilteredList, Acc)
                end
        end
    end, ByCapability, Capabilities),

    State#state{
        by_type = NewByType,
        by_capability = NewByCapability
    }.

%% @private
%% Find a healthy transport from a list of transport IDs
-spec find_healthy_transport([transport_id()], state()) ->
    {ok, transport_id()} | {error, no_healthy_transport}.
find_healthy_transport([], _State) ->
    {error, no_healthy_transport};
find_healthy_transport([TransportId | Rest], State) ->
    case maps:get(TransportId, State#state.transports, undefined) of
        undefined ->
            find_healthy_transport(Rest, State);
        TransportInfo ->
            case check_transport_health(TransportInfo) of
                healthy ->
                    {ok, TransportId};
                _ ->
                    find_healthy_transport(Rest, State)
            end
    end.

%% @private
%% Check health of a transport based on its state
-spec check_transport_health(transport_info()) -> healthy | degraded | unhealthy.
check_transport_health(TransportInfo) ->
    Status = TransportInfo#transport_info.status,
    Pid = TransportInfo#transport_info.pid,

    case Status of
        error -> unhealthy;
        stopped -> unhealthy;
        stopping -> unhealthy;
        initializing -> degraded;
        running ->
            % Check if process is alive
            case Pid of
                undefined -> unhealthy;
                P when is_pid(P) ->
                    case is_process_alive(P) of
                        true -> healthy;
                        false -> unhealthy
                    end;
                _ -> degraded
            end
    end.

%% @private
%% Convert transport info record to map for external consumption
-spec transport_info_to_map(transport_info()) -> map().
transport_info_to_map(TransportInfo) ->
    #{
        id => TransportInfo#transport_info.id,
        type => TransportInfo#transport_info.type,
        pid => TransportInfo#transport_info.pid,
        status => TransportInfo#transport_info.status,
        capabilities => TransportInfo#transport_info.capabilities,
        config => TransportInfo#transport_info.config,
        stats => TransportInfo#transport_info.stats,
        metadata => TransportInfo#transport_info.metadata
    }.

%% @private
%% Initialize transport stats
-spec initial_stats() -> map().
initial_stats() ->
    Now = erlang:system_time(millisecond),
    #{
        created_at => Now,
        last_activity => Now,
        messages_sent => 0,
        messages_received => 0,
        bytes_sent => 0,
        bytes_received => 0,
        errors => 0
    }.

%% @private
%% Normalize and validate capabilities list
-spec normalize_capabilities([term()]) -> [transport_capability()].
normalize_capabilities(Capabilities) when is_list(Capabilities) ->
    lists:filtermap(fun
        (resources) -> {true, resources};
        (tools) -> {true, tools};
        (prompts) -> {true, prompts};
        (logging) -> {true, logging};
        (notifications) -> {true, notifications};
        (_) -> false
    end, Capabilities);
normalize_capabilities(_) ->
    [].

%% @private
%% Sanitize config to remove sensitive information
-spec sanitize_config(map()) -> map().
sanitize_config(Config) when is_map(Config) ->
    % Remove sensitive keys
    SensitiveKeys = [password, secret, token, api_key, private_key],
    maps:map(fun(Key, Value) ->
        case lists:member(Key, SensitiveKeys) of
            true -> <<"***REDACTED***">>;
            false when is_map(Value) -> sanitize_config(Value);
            false when is_list(Value) ->
                % Recursively sanitize list elements
                [case V of
                    M when is_map(M) -> sanitize_config(M);
                    _ -> V
                 end || V <- Value];
            false -> Value
        end
    end, Config);
sanitize_config(Config) ->
    Config.

%% @private
%% Discover transports from code modules
-spec discover_transports_internal(state()) ->
    {ok, [transport_id()], state()} | {error, term()}.
discover_transports_internal(State) ->
    try
        % Get all loaded modules that match transport pattern
        AllModules = [M || M <- erlang:loaded_modules(),
                          is_transport_module(M)],

        % Register discovered transports
        {DiscoveredIds, NewState} = lists:foldl(fun(Module, {Ids, AccState}) ->
            TransportType = extract_transport_type(Module),
            TransportId = Module,

            % Create transport config from module info
            Config = #{
                type => TransportType,
                module => Module,
                discovered => true,
                auto_registered => true
            },

            % Register if not already present
            case maps:is_key(TransportId, AccState#state.transports) of
                true ->
                    {Ids, AccState};
                false ->
                    TransportInfo = #transport_info{
                        id = TransportId,
                        type = TransportType,
                        pid = undefined,
                        status = stopped,
                        capabilities = infer_capabilities(Module),
                        config = Config,
                        stats = initial_stats(),
                        metadata = #{discovered_at => erlang:system_time(millisecond)}
                    },

                    % Add to indices
                    IndexedState = add_to_indices(
                        TransportId, TransportType,
                        infer_capabilities(Module),
                        TransportInfo, AccState
                    ),

                    ?LOG_INFO("Discovered transport module: ~p (type: ~p)",
                             [TransportId, TransportType]),
                    {[TransportId | Ids], IndexedState#state{
                        transports = maps:put(TransportId, TransportInfo, IndexedState#state.transports)
                    }}
            end
        end, {[], State}, AllModules),

        {ok, lists:reverse(DiscoveredIds), NewState}
    catch
        Type:Error:Stacktrace ->
            ?LOG_ERROR("Transport discovery failed: ~p:~p~n~p", [Type, Error, Stacktrace]),
            {error, {discovery_failed, {Type, Error}}}
    end.

%% @private
%% Check if a module is a transport module
-spec is_transport_module(module()) -> boolean().
is_transport_module(Module) ->
    ModuleStr = atom_to_list(Module),
    % Check if module name matches transport pattern
    case string:find(ModuleStr, "erlmcp_transport_") of
        nomatch -> false;
        _ ->
            % Check if it's not the behavior, registry, or other utility modules
            ExcludePatterns = ["behavior", "registry", "health", "pool", "pipeline",
                               "discovery", "sup", "app", "validation", "adapter"],
            not lists:any(fun(Pattern) ->
                string:find(ModuleStr, Pattern) =/= nomatch
            end, ExcludePatterns)
    end.

%% @private
%% Extract transport type from module name
-spec extract_transport_type(module()) -> transport_type().
extract_transport_type(Module) ->
    ModuleStr = atom_to_list(Module),
    case string:split(ModuleStr, "erlmcp_transport_") of
        [_, "tcp"] -> tcp;
        [_, "stdio"] -> stdio;
        [_, "http"] -> http;
        [_, "ws"] -> websocket;
        [_, "sse"] -> sse;
        _ -> custom
    end.

%% @private
%% Infer capabilities from module exports
-spec infer_capabilities(module()) -> [transport_capability()].
infer_capabilities(Module) ->
    % Check module exports to infer capabilities
    Exports = Module:module_info(exports),

    % Default capabilities based on exports
    HasResources = lists:keymember(subscribe_resource, 2, Exports),
    HasTools = lists:keymember(call_tool, 2, Exports),
    HasPrompts = lists:keymember(get_prompt, 2, Exports),
    HasLogging = lists:keymember(log_message, 2, Exports),
    HasNotifications = lists:keymember(send_notification, 2, Exports),

    Caps0 = [tools || HasTools],
    Caps1 = [resources || HasResources] ++ Caps0,
    Caps2 = [prompts || HasPrompts] ++ Caps1,
    Caps3 = [logging || HasLogging] ++ Caps2,
    [notifications || HasNotifications] ++ Caps3.
