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
    negotiate_capabilities/2,
    merge_capability/3,
    get_server_capabilities/1,
    get_client_capabilities/0,
    has_capability/2,
    has_capability_feature/3,
    format_capability_error/1
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
-spec extract_client_capabilities(map()) -> #mcp_client_capabilities{}.
extract_client_capabilities(Params) when is_map(Params) ->
    CapsMap = maps:get(<<"capabilities">>, Params, #{}),
    #mcp_client_capabilities{
        roots = extract_capability(CapsMap, <<"roots">>),
        sampling = extract_capability(CapsMap, <<"sampling">>),
        experimental = maps:get(<<"experimental">>, CapsMap, undefined)
    }.

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
%% Returns the intersection of supported capabilities
-spec negotiate_capabilities(#mcp_client_capabilities{}, #mcp_server_capabilities{}) ->
    #mcp_server_capabilities{}.
negotiate_capabilities(ClientCaps, ServerCaps) ->
    %% Negotiation strategy: server capabilities define what's available
    %% Client capabilities provide hints about what the client will use
    %% Return server capabilities with optional adjustments based on client
    ServerCaps#mcp_server_capabilities{
        resources = negotiate_capability(resources, ClientCaps, ServerCaps),
        tools = negotiate_capability(tools, ClientCaps, ServerCaps),
        prompts = negotiate_capability(prompts, ClientCaps, ServerCaps),
        logging = negotiate_capability(logging, ClientCaps, ServerCaps),
        sampling = negotiate_capability(sampling, ClientCaps, ServerCaps),
        roots = negotiate_capability(roots, ClientCaps, ServerCaps)
    }.

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

%% @doc Negotiate a specific capability
-spec negotiate_capability(atom(), #mcp_client_capabilities{}, #mcp_server_capabilities{}) ->
    term().
negotiate_capability(resources, _ClientCaps, ServerCaps) ->
    ServerCaps#mcp_server_capabilities.resources;
negotiate_capability(tools, _ClientCaps, ServerCaps) ->
    ServerCaps#mcp_server_capabilities.tools;
negotiate_capability(prompts, _ClientCaps, ServerCaps) ->
    ServerCaps#mcp_server_capabilities.prompts;
negotiate_capability(logging, _ClientCaps, ServerCaps) ->
    ServerCaps#mcp_server_capabilities.logging;
negotiate_capability(sampling, ClientCaps, ServerCaps) ->
    %% For sampling, merge client model preferences
    merge_capability(sampling, ClientCaps#mcp_client_capabilities.sampling,
                     ServerCaps#mcp_server_capabilities.sampling);
negotiate_capability(roots, _ClientCaps, ServerCaps) ->
    ServerCaps#mcp_server_capabilities.roots.

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
