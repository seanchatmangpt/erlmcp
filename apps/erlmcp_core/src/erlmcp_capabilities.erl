-module(erlmcp_capabilities).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    extract_client_capabilities/1,
    extract_server_capabilities/1,
    capability_to_map/1,
    map_to_capability/1,
    validate_protocol_version/1,
    validate_capability_structures/2,
    negotiate_capabilities/2,
    merge_capability/3,
    get_server_capabilities/1,
    get_client_capabilities/0,
    has_capability/2,
    has_capability_feature/3,
    format_capability_error/1,
    is_feature_enabled/3,
    disable_feature/3,
    enable_feature/3,
    get_enabled_features/2,
    apply_graceful_degradation/2,
    validate_client_capability_record/1,
    validate_server_capability_record/1,
    validate_model_preferences/1,
    negotiate_capability_enhanced/3,
    supports_flag/2,
    validate_flag/3,
    apply_graceful_degradation_enhanced/2,
    extract_flag_from_map/3,
    build_capability_map/2,
    negotiate_subscribe_flag/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% State record
-record(state, {
    server_capabilities :: #mcp_server_capabilities{} | undefined,
    client_capabilities :: #mcp_client_capabilities{} | undefined,
    protocol_version :: binary() | undefined
}).

-type state() :: #state{}.

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Start the capabilities manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Extract client capabilities from initialize request parameters
%% Enhanced to properly handle feature flags like listChanged
-spec extract_client_capabilities(map()) -> #mcp_client_capabilities{}.
extract_client_capabilities(Params) when is_map(Params) ->
    CapsMap = maps:get(<<"capabilities">>, Params, #{}),
    #mcp_client_capabilities{
        roots = extract_roots_client_capability(CapsMap),
        sampling = extract_sampling_client_capability(CapsMap),
        experimental = maps:get(<<"experimental">>, CapsMap, undefined)
    }.

%% @doc Extract roots capability from client with feature flags
-spec extract_roots_client_capability(map()) -> #mcp_capability{}.
extract_roots_client_capability(CapsMap) ->
    case maps:get(<<"roots">>, CapsMap, undefined) of
        undefined ->
            #mcp_capability{enabled = false};
        CapMap when is_map(CapMap) ->
            %% Client roots capability may have listChanged feature
            %% Check if the capability map has any keys
            case maps:size(CapMap) of
                0 -> #mcp_capability{enabled = false};
                _ -> #mcp_capability{enabled = true}
            end;
        _ ->
            #mcp_capability{enabled = false}
    end.

%% @doc Extract sampling capability from client with model preferences
-spec extract_sampling_client_capability(map()) -> #mcp_capability{}.
extract_sampling_client_capability(CapsMap) ->
    case maps:get(<<"sampling">>, CapsMap, undefined) of
        undefined ->
            #mcp_capability{enabled = false};
        CapMap when is_map(CapMap) ->
            %% Sampling capability is present
            #mcp_capability{enabled = true};
        _ ->
            #mcp_capability{enabled = false}
    end.

%% @doc Extract server capabilities from initialize response
-spec extract_server_capabilities(map()) -> #mcp_server_capabilities{}.
extract_server_capabilities(Response) when is_map(Response) ->
    CapsMap = maps:get(<<"capabilities">>, Response, #{}),
    #mcp_server_capabilities{
        resources = extract_resources_capability(CapsMap),
        tools = extract_tools_capability(CapsMap),
        prompts = extract_prompts_capability(CapsMap),
        logging = extract_logging_capability(CapsMap),
        sampling = extract_sampling_capability(CapsMap),
        roots = extract_roots_capability(CapsMap),
        experimental = maps:get(<<"experimental">>, CapsMap, undefined)
    }.

%% @doc Convert capability record to map for JSON encoding
-spec capability_to_map(#mcp_server_capabilities{} | #mcp_client_capabilities{}) -> map().
capability_to_map(#mcp_server_capabilities{} = Caps) ->
    Base = #{},
    Base1 = maybe_add_capability_map(Base, <<"resources">>, Caps#mcp_server_capabilities.resources),
    Base2 = maybe_add_capability_map(Base1, <<"tools">>, Caps#mcp_server_capabilities.tools),
    Base3 = maybe_add_capability_map(Base2, <<"prompts">>, Caps#mcp_server_capabilities.prompts),
    Base4 = maybe_add_capability_map(Base3, <<"logging">>, Caps#mcp_server_capabilities.logging),
    Base5 = maybe_add_capability_map(Base4, <<"sampling">>, Caps#mcp_server_capabilities.sampling),
    Base6 = maybe_add_capability_map(Base5, <<"roots">>, Caps#mcp_server_capabilities.roots),
    maybe_add_experimental(Base6, Caps#mcp_server_capabilities.experimental);

capability_to_map(#mcp_client_capabilities{} = Caps) ->
    Base = #{},
    Base1 = maybe_add_capability_map(Base, <<"roots">>, Caps#mcp_client_capabilities.roots),
    Base2 = maybe_add_capability_map(Base1, <<"sampling">>, Caps#mcp_client_capabilities.sampling),
    maybe_add_experimental(Base2, Caps#mcp_client_capabilities.experimental).

%% @doc Convert map to capability record
-spec map_to_capability(map()) -> #mcp_server_capabilities{} | #mcp_client_capabilities{}.
map_to_capability(Map) when is_map(Map) ->
    %% Detect if this looks like server or client capabilities
    case maps:is_key(<<"resources">>, Map) orelse
         maps:is_key(<<"tools">>, Map) orelse
         maps:is_key(<<"prompts">>, Map) of
        true ->
            %% Server capabilities
            #mcp_server_capabilities{
                resources = extract_resources_capability(Map),
                tools = extract_tools_capability(Map),
                prompts = extract_prompts_capability(Map),
                logging = extract_logging_capability(Map),
                sampling = extract_sampling_capability(Map),
                roots = extract_roots_capability(Map),
                experimental = maps:get(<<"experimental">>, Map, undefined)
            };
        false ->
            %% Client capabilities
            #mcp_client_capabilities{
                roots = extract_capability(Map, <<"roots">>),
                sampling = extract_capability(Map, <<"sampling">>),
                experimental = maps:get(<<"experimental">>, Map, undefined)
            }
    end.

%% @doc Validate protocol version during initialization
-spec validate_protocol_version(binary()) -> ok | {error, binary()}.
validate_protocol_version(Version) when is_binary(Version) ->
    SupportedVersions = [<<"2024-11-05">>, <<"2025-11-25">>],
    case lists:member(Version, SupportedVersions) of
        true -> ok;
        false ->
            {error, <<"Unsupported protocol version: ", Version/binary,
                     ". Supported: ", (join_versions(SupportedVersions))/binary>>}
    end;
validate_protocol_version(_) ->
    {error, <<"Invalid protocol version format">>}.

%% @doc Negotiate capabilities between client and server
%% Returns the intersection of supported capabilities with graceful degradation
-spec negotiate_capabilities(#mcp_client_capabilities{}, #mcp_server_capabilities{}) ->
    #mcp_server_capabilities{}.
negotiate_capabilities(ClientCaps, ServerCaps) ->
    %% Validate capability structures before negotiation
    ok = validate_capability_structures(ClientCaps, ServerCaps),

    %% Negotiation strategy: server capabilities define what's available
    %% Client capabilities provide hints about what the client will use
    %% Graceful degradation: if client doesn't support a feature, disable it
    NegotiatedCaps = ServerCaps#mcp_server_capabilities{
        resources = negotiate_capability(resources, ClientCaps, ServerCaps),
        tools = negotiate_capability(tools, ClientCaps, ServerCaps),
        prompts = negotiate_capability(prompts, ClientCaps, ServerCaps),
        logging = negotiate_capability(logging, ClientCaps, ServerCaps),
        sampling = negotiate_capability(sampling, ClientCaps, ServerCaps),
        roots = negotiate_capability(roots, ClientCaps, ServerCaps),
        experimental = negotiate_experimental(ClientCaps, ServerCaps)
    },
    %% Apply additional graceful degradation based on client capabilities
    apply_graceful_degradation(ClientCaps, NegotiatedCaps).

%% @doc Negotiate experimental capabilities
%% Only include experimental features that both client and server support
-spec negotiate_experimental(#mcp_client_capabilities{}, #mcp_server_capabilities{}) ->
    map() | undefined.
negotiate_experimental(ClientCaps, ServerCaps) ->
    ServerExp = ServerCaps#mcp_server_capabilities.experimental,
    ClientExp = ClientCaps#mcp_client_capabilities.experimental,
    case {ServerExp, ClientExp} of
        {undefined, _} ->
            undefined;
        {_, undefined} ->
            undefined;
        {ServerMap, ClientMap} when is_map(ServerMap), is_map(ClientMap) ->
            %% Intersection of experimental features
            maps:filter(fun(Key, _Value) ->
                maps:is_key(Key, ClientMap)
            end, ServerMap);
        _ ->
            undefined
    end.

%% @doc Apply graceful degradation based on client capabilities
%% Disables features that the client doesn't support
-spec apply_graceful_degradation(#mcp_client_capabilities{}, #mcp_server_capabilities{}) ->
    #mcp_server_capabilities{}.
apply_graceful_degradation(ClientCaps, ServerCaps) ->
    %% Check if client supports roots capability
    case ClientCaps#mcp_client_capabilities.roots of
        #mcp_capability{enabled = false} ->
            %% Client doesn't support roots, disable listChanged on resources
            ResCaps = ServerCaps#mcp_server_capabilities.resources,
            ServerCaps#mcp_server_capabilities{
                resources = ResCaps#mcp_resources_capability{listChanged = false}
            };
        #mcp_capability{enabled = true} ->
            %% Client supports roots, keep capabilities as negotiated
            ServerCaps
    end.

%% @doc Merge a specific capability with client preferences
-spec merge_capability(atom(), term(), term()) -> term().
merge_capability(resources, _ClientVal, ServerVal) when is_record(ServerVal, mcp_resources_capability) ->
    ServerVal;
merge_capability(tools, _ClientVal, ServerVal) when is_record(ServerVal, mcp_tools_capability) ->
    ServerVal;
merge_capability(prompts, _ClientVal, ServerVal) when is_record(ServerVal, mcp_prompts_capability) ->
    ServerVal;
merge_capability(logging, _ClientVal, ServerVal) when is_record(ServerVal, mcp_logging_capability) ->
    ServerVal;
merge_capability(sampling, ClientVal, ServerVal) when is_record(ServerVal, mcp_sampling_capability) ->
    %% For sampling, merge model preferences from client
    case ClientVal of
        #mcp_sampling_capability{modelPreferences = ClientPrefs} when ClientPrefs =/= undefined ->
            ServerVal#mcp_sampling_capability{modelPreferences = ClientPrefs};
        _ ->
            ServerVal
    end;
merge_capability(roots, _ClientVal, ServerVal) when is_record(ServerVal, mcp_roots_capability) ->
    ServerVal;
merge_capability(_Key, _ClientVal, ServerVal) ->
    ServerVal.

%% @doc Get server capabilities for a given server process
-spec get_server_capabilities(pid()) -> {ok, #mcp_server_capabilities{}} | {error, term()}.
get_server_capabilities(ServerPid) when is_pid(ServerPid) ->
    case erlang:is_process_alive(ServerPid) of
        true ->
            try gen_server:call(ServerPid, get_capabilities, 1000) of
                {ok, Caps} -> {ok, Caps};
                {error, _} = Error -> Error
            catch
                _:_ -> {error, server_unreachable}
            end;
        false ->
            {error, server_not_running}
    end.

%% @doc Get default client capabilities
-spec get_client_capabilities() -> #mcp_client_capabilities{}.
get_client_capabilities() ->
    #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},  % Default: no sampling
        experimental = undefined
    }.

%% @doc Check if a capability is supported
-spec has_capability(#mcp_server_capabilities{}, atom()) -> boolean().
has_capability(Caps, resources) when is_record(Caps, mcp_server_capabilities) ->
    %% Check if the capability record is not the default empty one
    case Caps#mcp_server_capabilities.resources of
        #mcp_resources_capability{} -> true;
        _ -> false
    end;
has_capability(Caps, tools) when is_record(Caps, mcp_server_capabilities) ->
    %% Check if the capability record is not the default empty one
    case Caps#mcp_server_capabilities.tools of
        #mcp_tools_capability{} -> true;
        _ -> false
    end;
has_capability(Caps, prompts) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.prompts of
        #mcp_prompts_capability{} -> true;
        _ -> false
    end;
has_capability(Caps, logging) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.logging of
        #mcp_logging_capability{} -> true;
        _ -> false
    end;
has_capability(Caps, sampling) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.sampling of
        #mcp_sampling_capability{} -> true;
        _ -> false
    end;
has_capability(Caps, roots) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.roots of
        #mcp_roots_capability{} -> true;
        _ -> false
    end;
has_capability(_, _) ->
    false.

%% @doc Check if a capability has a specific feature enabled
-spec has_capability_feature(#mcp_server_capabilities{}, atom(), atom()) -> boolean().
has_capability_feature(Caps, resources, subscribe) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.resources of
        #mcp_resources_capability{subscribe = true} -> true;
        _ -> false
    end;
has_capability_feature(Caps, resources, listChanged) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.resources of
        #mcp_resources_capability{listChanged = true} -> true;
        _ -> false
    end;
has_capability_feature(Caps, tools, listChanged) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.tools of
        #mcp_tools_capability{listChanged = true} -> true;
        _ -> false
    end;
has_capability_feature(Caps, prompts, listChanged) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.prompts of
        #mcp_prompts_capability{listChanged = true} -> true;
        _ -> false
    end;
has_capability_feature(_, _, _) ->
    false.

%% @doc Check if a feature flag is enabled for a capability
%% Used for graceful degradation when client doesn't support a feature
-spec is_feature_enabled(#mcp_server_capabilities{}, atom(), atom()) -> boolean().
is_feature_enabled(Caps, resources, subscribe) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.resources of
        #mcp_resources_capability{subscribe = true} -> true;
        _ -> false
    end;
is_feature_enabled(Caps, resources, listChanged) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.resources of
        #mcp_resources_capability{listChanged = true} -> true;
        _ -> false
    end;
is_feature_enabled(Caps, tools, listChanged) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.tools of
        #mcp_tools_capability{listChanged = true} -> true;
        _ -> false
    end;
is_feature_enabled(Caps, prompts, listChanged) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.prompts of
        #mcp_prompts_capability{listChanged = true} -> true;
        _ -> false
    end;
is_feature_enabled(Caps, roots, listChanged) when is_record(Caps, mcp_server_capabilities) ->
    %% Roots is a client capability, but server can check if client supports it
    case Caps#mcp_server_capabilities.roots of
        #mcp_roots_capability{} -> true;
        _ -> false
    end;
is_feature_enabled(_, _, _) ->
    false.

%% @doc Disable a specific feature flag in a capability
%% Used for graceful degradation
-spec disable_feature(#mcp_server_capabilities{}, atom(), atom()) ->
    #mcp_server_capabilities{}.
disable_feature(Caps, resources, subscribe) when is_record(Caps, mcp_server_capabilities) ->
    ResCaps = Caps#mcp_server_capabilities.resources,
    Caps#mcp_server_capabilities{
        resources = ResCaps#mcp_resources_capability{subscribe = false}
    };
disable_feature(Caps, resources, listChanged) when is_record(Caps, mcp_server_capabilities) ->
    ResCaps = Caps#mcp_server_capabilities.resources,
    Caps#mcp_server_capabilities{
        resources = ResCaps#mcp_resources_capability{listChanged = false}
    };
disable_feature(Caps, tools, listChanged) when is_record(Caps, mcp_server_capabilities) ->
    ToolsCaps = Caps#mcp_server_capabilities.tools,
    Caps#mcp_server_capabilities{
        tools = ToolsCaps#mcp_tools_capability{listChanged = false}
    };
disable_feature(Caps, prompts, listChanged) when is_record(Caps, mcp_server_capabilities) ->
    PromptsCaps = Caps#mcp_server_capabilities.prompts,
    Caps#mcp_server_capabilities{
        prompts = PromptsCaps#mcp_prompts_capability{listChanged = false}
    };
disable_feature(Caps, _, _) ->
    Caps.

%% @doc Enable a specific feature flag in a capability
%% Used when client explicitly requests a feature
-spec enable_feature(#mcp_server_capabilities{}, atom(), atom()) ->
    #mcp_server_capabilities{}.
enable_feature(Caps, resources, subscribe) when is_record(Caps, mcp_server_capabilities) ->
    ResCaps = Caps#mcp_server_capabilities.resources,
    Caps#mcp_server_capabilities{
        resources = ResCaps#mcp_resources_capability{subscribe = true}
    };
enable_feature(Caps, resources, listChanged) when is_record(Caps, mcp_server_capabilities) ->
    ResCaps = Caps#mcp_server_capabilities.resources,
    Caps#mcp_server_capabilities{
        resources = ResCaps#mcp_resources_capability{listChanged = true}
    };
enable_feature(Caps, tools, listChanged) when is_record(Caps, mcp_server_capabilities) ->
    ToolsCaps = Caps#mcp_server_capabilities.tools,
    Caps#mcp_server_capabilities{
        tools = ToolsCaps#mcp_tools_capability{listChanged = true}
    };
enable_feature(Caps, prompts, listChanged) when is_record(Caps, mcp_server_capabilities) ->
    PromptsCaps = Caps#mcp_server_capabilities.prompts,
    Caps#mcp_server_capabilities{
        prompts = PromptsCaps#mcp_prompts_capability{listChanged = true}
    };
enable_feature(Caps, _, _) ->
    Caps.

%% @doc Get all enabled features for a capability
%% Returns a list of feature atoms that are enabled
-spec get_enabled_features(#mcp_server_capabilities{}, atom()) -> [atom()].
get_enabled_features(Caps, resources) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.resources of
        #mcp_resources_capability{subscribe = S, listChanged = L} ->
            Features = [],
            Features1 = case S of true -> [subscribe | Features]; false -> Features end,
            Features2 = case L of true -> [listChanged | Features1]; false -> Features1 end,
            lists:reverse(Features2);
        _ -> []
    end;
get_enabled_features(Caps, tools) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.tools of
        #mcp_tools_capability{listChanged = true} -> [listChanged];
        _ -> []
    end;
get_enabled_features(Caps, prompts) when is_record(Caps, mcp_server_capabilities) ->
    case Caps#mcp_server_capabilities.prompts of
        #mcp_prompts_capability{listChanged = true} -> [listChanged];
        _ -> []
    end;
get_enabled_features(_, _) ->
    [].

%% @doc Format capability error for JSON-RPC response
-spec format_capability_error(term()) -> binary().
format_capability_error({unsupported_capability, Cap}) when is_atom(Cap) ->
    <<"Capability not supported: ", (atom_to_binary(Cap))/binary>>;
format_capability_error({unsupported_feature, Cap, Feature}) ->
    <<"Feature '", (atom_to_binary(Feature))/binary, "' not supported for capability '",
      (atom_to_binary(Cap))/binary, "'">>;
format_capability_error(unsupported_capability) ->
    <<"Requested capability is not supported by this server">>;
format_capability_error({protocol_version_mismatch, Version}) when is_binary(Version) ->
    <<"Unsupported protocol version: ", Version/binary>>;
format_capability_error({feature_flag_disabled, Capability, Feature}) ->
    <<"Feature '", (atom_to_binary(Feature))/binary, "' is disabled for capability '",
      (atom_to_binary(Capability))/binary, "'">>;
format_capability_error(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, #state{
        server_capabilities = undefined,
        client_capabilities = undefined,
        protocol_version = undefined
    }}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call(get_capabilities, _From, State) ->
    {reply, {ok, State#state.server_capabilities}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
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

%%%====================================================================
%%% Internal functions
%%%====================================================================

%% @doc Extract a basic capability from a map
-spec extract_capability(map(), binary()) -> #mcp_capability{}.
extract_capability(Map, Key) ->
    case maps:get(Key, Map, undefined) of
        undefined -> #mcp_capability{enabled = false};
        _CapMap when is_map(_CapMap) -> #mcp_capability{enabled = true};
        _ -> #mcp_capability{enabled = false}
    end.

%% @doc Extract resources capability with features
-spec extract_resources_capability(map()) -> #mcp_resources_capability{}.
extract_resources_capability(Map) ->
    case maps:get(<<"resources">>, Map, undefined) of
        undefined ->
            #mcp_resources_capability{subscribe = false, listChanged = false};
        CapMap when is_map(CapMap) ->
            #mcp_resources_capability{
                subscribe = maps:get(<<"subscribe">>, CapMap, false),
                listChanged = maps:get(<<"listChanged">>, CapMap, false)
            }
    end.

%% @doc Extract tools capability with features
-spec extract_tools_capability(map()) -> #mcp_tools_capability{}.
extract_tools_capability(Map) ->
    case maps:get(<<"tools">>, Map, undefined) of
        undefined ->
            #mcp_tools_capability{listChanged = false};
        CapMap when is_map(CapMap) ->
            #mcp_tools_capability{
                listChanged = maps:get(<<"listChanged">>, CapMap, false)
            }
    end.

%% @doc Extract prompts capability with features
-spec extract_prompts_capability(map()) -> #mcp_prompts_capability{}.
extract_prompts_capability(Map) ->
    case maps:get(<<"prompts">>, Map, undefined) of
        undefined ->
            #mcp_prompts_capability{listChanged = false};
        CapMap when is_map(CapMap) ->
            #mcp_prompts_capability{
                listChanged = maps:get(<<"listChanged">>, CapMap, false)
            }
    end.

%% @doc Extract logging capability
-spec extract_logging_capability(map()) -> #mcp_logging_capability{}.
extract_logging_capability(Map) ->
    case maps:get(<<"logging">>, Map, undefined) of
        undefined -> #mcp_logging_capability{};
        _CapMap when is_map(_CapMap) -> #mcp_logging_capability{}
    end.

%% @doc Extract sampling capability with model preferences
-spec extract_sampling_capability(map()) -> #mcp_sampling_capability{}.
extract_sampling_capability(Map) ->
    case maps:get(<<"sampling">>, Map, undefined) of
        undefined ->
            #mcp_sampling_capability{modelPreferences = undefined};
        CapMap when is_map(CapMap) ->
            #mcp_sampling_capability{
                modelPreferences = maps:get(<<"modelPreferences">>, CapMap, undefined)
            }
    end.

%% @doc Extract roots capability
-spec extract_roots_capability(map()) -> #mcp_roots_capability{}.
extract_roots_capability(Map) ->
    case maps:get(<<"roots">>, Map, undefined) of
        undefined -> #mcp_roots_capability{};
        _CapMap when is_map(_CapMap) -> #mcp_roots_capability{}
    end.

%% @doc Negotiate a specific capability with graceful degradation
%% If client doesn't support a feature, disable it in the negotiated capabilities
-spec negotiate_capability(atom(), #mcp_client_capabilities{}, #mcp_server_capabilities{}) ->
    term().
negotiate_capability(resources, ClientCaps, ServerCaps) ->
    ResCap = ServerCaps#mcp_server_capabilities.resources,
    %% Check if client supports roots capability (affects listChanged)
    case ClientCaps#mcp_client_capabilities.roots of
        #mcp_capability{enabled = true} ->
            %% Client supports roots, keep all features enabled
            %% Subscribe flag is negotiated separately based on client subscription support
            NegotiatedSub = negotiate_subscribe_flag(ClientCaps, ResCap),
            ResCap#mcp_resources_capability{subscribe = NegotiatedSub};
        #mcp_capability{enabled = false} ->
            %% Client doesn't support roots, disable listChanged feature
            %% Keep subscribe if server has it (for backward compatibility)
            NegotiatedSub = negotiate_subscribe_flag(ClientCaps, ResCap),
            ResCap#mcp_resources_capability{
                subscribe = NegotiatedSub,
                listChanged = false
            }
    end;
negotiate_capability(tools, _ClientCaps, ServerCaps) ->
    %% Tools capability is always available on server side
    %% listChanged feature depends on server capabilities only
    ServerCaps#mcp_server_capabilities.tools;
negotiate_capability(prompts, _ClientCaps, ServerCaps) ->
    %% Prompts capability is always available on server side
    %% listChanged feature depends on server capabilities only
    ServerCaps#mcp_server_capabilities.prompts;
negotiate_capability(logging, _ClientCaps, ServerCaps) ->
    %% Logging capability is server-side only
    ServerCaps#mcp_server_capabilities.logging;
negotiate_capability(sampling, ClientCaps, ServerCaps) ->
    %% For sampling, merge client model preferences
    case ClientCaps#mcp_client_capabilities.sampling of
        #mcp_capability{enabled = true} ->
            %% Client supports sampling, merge preferences
            merge_capability(sampling, ClientCaps#mcp_client_capabilities.sampling,
                            ServerCaps#mcp_server_capabilities.sampling);
        #mcp_capability{enabled = false} ->
            %% Client doesn't support sampling, return default
            ServerCaps#mcp_server_capabilities.sampling
    end;
negotiate_capability(roots, ClientCaps, ServerCaps) ->
    %% Roots capability - check if client supports it
    case ClientCaps#mcp_client_capabilities.roots of
        #mcp_capability{enabled = true} ->
            ServerCaps#mcp_server_capabilities.roots;
        #mcp_capability{enabled = false} ->
            %% Return empty capability to disable roots
            #mcp_roots_capability{}
    end.

%% @doc Negotiate subscribe flag for resources capability
%% The subscribe flag indicates whether the client can handle resource subscription notifications
-spec negotiate_subscribe_flag(#mcp_client_capabilities{}, #mcp_resources_capability{}) -> boolean().
negotiate_subscribe_flag(_ClientCaps, ResCap) ->
    %% For now, subscribe is a server-side feature
    %% In the future, we may check client capabilities to determine if they support subscriptions
    %% The subscribe flag indicates the server supports sending resource/updated notifications
    ResCap#mcp_resources_capability.subscribe.

%% @doc Add capability to map if it's not the default empty record
-spec maybe_add_capability_map(map(), binary(), term()) -> map().
maybe_add_capability_map(Map, Key, #mcp_capability{enabled = false}) ->
    Map#{Key => #{}};
maybe_add_capability_map(Map, _Key, #mcp_resources_capability{subscribe = false, listChanged = false}) ->
    Map#{<<"resources">> => #{}};
maybe_add_capability_map(Map, _Key, #mcp_tools_capability{listChanged = false}) ->
    Map#{<<"tools">> => #{}};
maybe_add_capability_map(Map, _Key, #mcp_prompts_capability{listChanged = false}) ->
    Map#{<<"prompts">> => #{}};
maybe_add_capability_map(Map, Key, #mcp_capability{enabled = true}) ->
    Map#{Key => #{}};
maybe_add_capability_map(Map, Key, #mcp_resources_capability{subscribe = S, listChanged = L}) ->
    Features = #{},
    Features1 = case S of true -> Features#{<<"subscribe">> => true}; false -> Features end,
    Features2 = case L of true -> Features1#{<<"listChanged">> => true}; false -> Features1 end,
    case map_size(Features2) of
        0 -> Map;
        _ -> Map#{Key => Features2}
    end;
maybe_add_capability_map(Map, Key, #mcp_tools_capability{listChanged = L}) ->
    case L of
        true -> Map#{Key => #{<<"listChanged">> => true}};
        false -> Map
    end;
maybe_add_capability_map(Map, Key, #mcp_prompts_capability{listChanged = L}) ->
    case L of
        true -> Map#{Key => #{<<"listChanged">> => true}};
        false -> Map
    end;
maybe_add_capability_map(Map, Key, #mcp_logging_capability{}) ->
    Map#{Key => #{}};
maybe_add_capability_map(Map, Key, #mcp_sampling_capability{modelPreferences = Prefs}) ->
    case Prefs of
        undefined -> Map#{Key => #{}};
        _ -> Map#{Key => #{<<"modelPreferences">> => Prefs}}
    end;
maybe_add_capability_map(Map, Key, #mcp_roots_capability{}) ->
    Map#{Key => #{}};
maybe_add_capability_map(Map, Key, _Other) ->
    Map#{Key => #{}}.

%% @doc Add experimental capabilities if present
-spec maybe_add_experimental(map(), map() | undefined) -> map().
maybe_add_experimental(Map, undefined) ->
    Map;
maybe_add_experimental(Map, Experimental) when is_map(Experimental), map_size(Experimental) > 0 ->
    Map#{<<"experimental">> => Experimental};
maybe_add_experimental(Map, _) ->
    Map.

%% @doc Join version list for error messages
-spec join_versions([binary()]) -> binary().
join_versions(Versions) ->
    lists:foldl(fun(V, Acc) ->
        case Acc of
            <<>> -> V;
            _ -> <<Acc/binary, ", ", V/binary>>
        end
    end, <<>>, Versions).

%% @doc Validate capability structures before negotiation
%% Ensures all records are well-formed and feature flags are valid
-spec validate_capability_structures(#mcp_client_capabilities{}, #mcp_server_capabilities{}) -> ok.
validate_capability_structures(ClientCaps, ServerCaps) ->
    %% Validate client capabilities
    validate_client_capability_record(ClientCaps),
    %% Validate server capabilities
    validate_server_capability_record(ServerCaps),
    ok.

%% @doc Validate client capability record structure
-spec validate_client_capability_record(#mcp_client_capabilities{}) -> ok.
validate_client_capability_record(#mcp_client_capabilities{roots = Roots, sampling = Sampling}) ->
    %% Validate roots capability
    case Roots of
        #mcp_capability{enabled = IsEnabled} when is_boolean(IsEnabled) -> ok;
        _ -> error({invalid_capability_structure, roots})
    end,
    %% Validate sampling capability
    case Sampling of
        #mcp_capability{enabled = IsEnabled2} when is_boolean(IsEnabled2) -> ok;
        _ -> error({invalid_capability_structure, sampling})
    end,
    ok.

%% @doc Validate server capability record structure
-spec validate_server_capability_record(#mcp_server_capabilities{}) -> ok.
validate_server_capability_record(#mcp_server_capabilities{
    resources = Resources,
    tools = Tools,
    prompts = Prompts,
    logging = Logging,
    sampling = Sampling,
    roots = Roots
}) ->
    %% Validate resources capability with feature flags
    case Resources of
        #mcp_resources_capability{subscribe = S, listChanged = L}
          when is_boolean(S), is_boolean(L) -> ok;
        _ -> error({invalid_capability_structure, resources})
    end,
    %% Validate tools capability
    case Tools of
        #mcp_tools_capability{listChanged = L1} when is_boolean(L1) -> ok;
        _ -> error({invalid_capability_structure, tools})
    end,
    %% Validate prompts capability
    case Prompts of
        #mcp_prompts_capability{listChanged = L2} when is_boolean(L2) -> ok;
        _ -> error({invalid_capability_structure, prompts})
    end,
    %% Validate logging capability
    case Logging of
        #mcp_logging_capability{} -> ok;
        _ -> error({invalid_capability_structure, logging})
    end,
    %% Validate sampling capability
    case Sampling of
        #mcp_sampling_capability{modelPreferences = Prefs} ->
            case Prefs of
                undefined -> ok;
                Map when is_map(Map) -> validate_model_preferences(Map);
                _ -> error({invalid_capability_structure, sampling_model_preferences})
            end;
        _ -> error({invalid_capability_structure, sampling})
    end,
    %% Validate roots capability
    case Roots of
        #mcp_roots_capability{} -> ok;
        _ -> error({invalid_capability_structure, roots})
    end,
    ok.

%% @doc Validate model preferences structure
-spec validate_model_preferences(map()) -> ok | no_return().
validate_model_preferences(Prefs) ->
    %% Validate optional fields
    case maps:get(<<"costPriority">>, Prefs, undefined) of
        undefined -> ok;
        P when is_float(P); is_integer(P) -> ok;
        _ -> error({invalid_model_preferences, costPriority})
    end,
    case maps:get(<<"speedPriority">>, Prefs, undefined) of
        undefined -> ok;
        P2 when is_float(P2); is_integer(P2) -> ok;
        _ -> error({invalid_model_preferences, speedPriority})
    end,
    case maps:get(<<"intelligencePriority">>, Prefs, undefined) of
        undefined -> ok;
        P3 when is_float(P3); is_integer(P3) -> ok;
        _ -> error({invalid_model_preferences, intelligencePriority})
    end,
    case maps:get(<<"temperature">>, Prefs, undefined) of
        undefined -> ok;
        T when is_float(T), T >= 0.0, T =< 2.0 -> ok;
        _ -> error({invalid_model_preferences, temperature})
    end,
    case maps:get(<<"maxTokens">>, Prefs, undefined) of
        undefined -> ok;
        MT when is_integer(MT), MT > 0 -> ok;
        _ -> error({invalid_model_preferences, maxTokens})
    end,
    case maps:get(<<"stopSequences">>, Prefs, undefined) of
        undefined -> ok;
        SS when is_list(SS) -> ok;
        _ -> error({invalid_model_preferences, stopSequences})
    end,
    ok.

%% @doc Enhanced negotiate_capability with proper flag handling for all capabilities
-spec negotiate_capability_enhanced(atom(), #mcp_client_capabilities{}, #mcp_server_capabilities{}) ->
    term().
negotiate_capability_enhanced(resources, ClientCaps, ServerCaps) ->
    %% Resources capability: negotiate subscribe and listChanged flags
    ServerRes = ServerCaps#mcp_server_capabilities.resources,
    case ServerRes of
        #mcp_resources_capability{subscribe = SrvSub, listChanged = SrvLC} ->
            %% Check if client supports roots (affects listChanged)
            ClientRoots = ClientCaps#mcp_client_capabilities.roots,
            NegotiatedLC = case ClientRoots of
                #mcp_capability{enabled = true} -> SrvLC;
                #mcp_capability{enabled = false} -> false
            end,
            %% Subscribe flag: server-side feature, keep as-is
            #mcp_resources_capability{
                subscribe = SrvSub,
                listChanged = NegotiatedLC
            };
        _ ->
            ServerRes
    end;
negotiate_capability_enhanced(tools, _ClientCaps, ServerCaps) ->
    %% Tools capability: listChanged is server-side only
    ServerCaps#mcp_server_capabilities.tools;
negotiate_capability_enhanced(prompts, _ClientCaps, ServerCaps) ->
    %% Prompts capability: listChanged is server-side only
    ServerCaps#mcp_server_capabilities.prompts;
negotiate_capability_enhanced(logging, _ClientCaps, ServerCaps) ->
    %% Logging capability: no feature flags
    ServerCaps#mcp_server_capabilities.logging;
negotiate_capability_enhanced(sampling, ClientCaps, ServerCaps) ->
    %% Sampling capability: merge client model preferences
    ServerSamp = ServerCaps#mcp_server_capabilities.sampling,
    ClientSamp = ClientCaps#mcp_client_capabilities.sampling,
    case ClientSamp of
        #mcp_capability{enabled = true} ->
            %% Client supports sampling, check for model preferences
            %% Note: Client capability uses #mcp_capability{}, server uses #mcp_sampling_capability{}
            %% The client may send model preferences in a separate field during initialization
            ServerSamp;
        #mcp_capability{enabled = false} ->
            ServerSamp
    end;
negotiate_capability_enhanced(roots, ClientCaps, ServerCaps) ->
    %% Roots capability: check if client supports it
    ClientRoots = ClientCaps#mcp_client_capabilities.roots,
    case ClientRoots of
        #mcp_capability{enabled = true} ->
            ServerCaps#mcp_server_capabilities.roots;
        #mcp_capability{enabled = false} ->
            #mcp_roots_capability{}
    end.

%% @doc Check if a specific flag is supported for a capability
-spec supports_flag(atom(), atom()) -> boolean().
supports_flag(resources, subscribe) -> true;
supports_flag(resources, listChanged) -> true;
supports_flag(tools, listChanged) -> true;
supports_flag(prompts, listChanged) -> true;
supports_flag(logging, _) -> false;
supports_flag(sampling, _) -> false;
supports_flag(roots, _) -> false;
supports_flag(_, _) -> false.

%% @doc Validate flag value for a capability
-spec validate_flag(atom(), atom(), boolean()) -> ok | {error, term()}.
validate_flag(Capability, Flag, Value) when is_boolean(Value) ->
    case supports_flag(Capability, Flag) of
        true -> ok;
        false -> {error, {unsupported_flag, Capability, Flag}}
    end;
validate_flag(_Capability, _Flag, _Value) ->
    {error, invalid_flag_value}.

%% @doc Apply graceful degradation based on client capabilities
%% Enhanced to handle all flag variations and experimental features
-spec apply_graceful_degradation_enhanced(#mcp_client_capabilities{}, #mcp_server_capabilities{}) ->
    #mcp_server_capabilities{}.
apply_graceful_degradation_enhanced(ClientCaps, ServerCaps) ->
    %% Check if client supports roots capability (affects resources listChanged)
    ResCaps = ServerCaps#mcp_server_capabilities.resources,
    NegotiatedRes = case ClientCaps#mcp_client_capabilities.roots of
        #mcp_capability{enabled = false} ->
            %% Client doesn't support roots, disable listChanged on resources
            ResCaps#mcp_resources_capability{listChanged = false};
        #mcp_capability{enabled = true} ->
            %% Client supports roots, keep negotiated settings
            ResCaps
    end,

    %% Check if client supports sampling (affects sampling capability)
    SampCaps = ServerCaps#mcp_server_capabilities.sampling,
    NegotiatedSamp = case ClientCaps#mcp_client_capabilities.sampling of
        #mcp_capability{enabled = false} ->
            %% Client doesn't support sampling, clear model preferences
            SampCaps#mcp_sampling_capability{modelPreferences = undefined};
        #mcp_capability{enabled = true} ->
            %% Client supports sampling, keep negotiated settings
            SampCaps
    end,

    %% Update server capabilities with negotiated values
    ServerCaps#mcp_server_capabilities{
        resources = NegotiatedRes,
        sampling = NegotiatedSamp
    }.

%% @doc Extract flag value from capability map for negotiation
-spec extract_flag_from_map(map(), binary(), atom()) -> boolean().
extract_flag_from_map(CapMap, FlagKey, Default) ->
    case maps:get(FlagKey, CapMap, Default) of
        Value when is_boolean(Value) -> Value;
        _ -> Default
    end.

%% @doc Build capability map with negotiated flags
-spec build_capability_map(atom(), map()) -> map().
build_capability_map(resources, Flags) ->
    Base = #{},
    Base1 = case maps:get(subscribe, Flags, false) of
        true -> Base#{<<"subscribe">> => true};
        false -> Base
    end,
    case maps:get(listChanged, Flags, false) of
        true -> Base1#{<<"listChanged">> => true};
        false -> Base1
    end;
build_capability_map(tools, Flags) ->
    case maps:get(listChanged, Flags, false) of
        true -> #{<<"listChanged">> => true};
        false -> #{}
    end;
build_capability_map(prompts, Flags) ->
    case maps:get(listChanged, Flags, false) of
        true -> #{<<"listChanged">> => true};
        false -> #{}
    end;
build_capability_map(logging, _Flags) ->
    #{};
build_capability_map(sampling, Flags) ->
    case maps:get(modelPreferences, Flags, undefined) of
        undefined -> #{};
        Prefs -> #{<<"modelPreferences">> => Prefs}
    end;
build_capability_map(roots, _Flags) ->
    #{}.
