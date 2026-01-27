%%%-------------------------------------------------------------------
%% @doc Capability Negotiation for MCP 2025-11-25
%% Handles capability advertisement, extraction, validation, and enforcement.
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_capabilities).

-include("erlmcp.hrl").

%% Public API
-export([
    build_server_capabilities/0,
    build_server_capabilities/1,
    extract_client_capabilities/1,
    validate_protocol_version/1,
    validate_capability/2,
    validate_feature/3,
    capability_to_map/1,
    client_capabilities_to_map/1,
    server_capabilities_from_map/1,
    client_capabilities_from_map/1,
    supported_versions/0
]).

%%====================================================================
%% Public API - Capability Building
%%====================================================================

build_server_capabilities() ->
    #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = true,
            listChanged = true
        },
        tools = #mcp_tools_capability{
            listChanged = true
        },
        prompts = #mcp_prompts_capability{
            listChanged = true
        },
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = undefined
    }.

build_server_capabilities(Config) when is_map(Config) ->
    Default = build_server_capabilities(),
    case maps:get(resources, Config, undefined) of
        undefined -> Default;
        ResourcesCfg ->
            Resources = case ResourcesCfg of
                #{subscribe := Sub, listChanged := LC} ->
                    #mcp_resources_capability{subscribe = Sub, listChanged = LC};
                #{subscribe := Sub} ->
                    #mcp_resources_capability{subscribe = Sub};
                #{listChanged := LC} ->
                    #mcp_resources_capability{listChanged = LC};
                _ ->
                    Default#mcp_server_capabilities.resources
            end,
            Default#mcp_server_capabilities{resources = Resources}
    end.

%%====================================================================
%% Public API - Capability Extraction
%%====================================================================

extract_client_capabilities(undefined) ->
    #mcp_client_capabilities{};
extract_client_capabilities(Params) when is_map(Params) ->
    case maps:get(?MCP_FIELD_CAPABILITIES, Params, #{}) of
        CapMap when is_map(CapMap) ->
            client_capabilities_from_map(CapMap);
        _ ->
            #mcp_client_capabilities{}
    end.

%%====================================================================
%% Public API - Capability Validation
%%====================================================================

validate_protocol_version(Version) when is_binary(Version) ->
    SupportedVersions = [<<"2025-11-25">>, <<"2024-11-05">>],
    case lists:member(Version, SupportedVersions) of
        true -> ok;
        false -> {error, <<"Unsupported protocol version">>}
    end.

validate_capability(#mcp_server_capabilities{resources = Resources}, resources) ->
    case Resources of
        #mcp_resources_capability{} -> ok;
        undefined -> {error, capability_not_supported}
    end;
validate_capability(#mcp_server_capabilities{tools = Tools}, tools) ->
    case Tools of
        #mcp_tools_capability{} -> ok;
        undefined -> {error, capability_not_supported}
    end;
validate_capability(#mcp_server_capabilities{prompts = Prompts}, prompts) ->
    case Prompts of
        #mcp_prompts_capability{} -> ok;
        undefined -> {error, capability_not_supported}
    end;
validate_capability(#mcp_server_capabilities{logging = Logging}, logging) ->
    case Logging of
        #mcp_logging_capability{} -> ok;
        undefined -> {error, capability_not_supported}
    end;
validate_capability(#mcp_server_capabilities{sampling = Sampling}, sampling) ->
    case Sampling of
        #mcp_sampling_capability{} -> ok;
        undefined -> {error, capability_not_supported}
    end;
validate_capability(#mcp_server_capabilities{roots = RootsC}, roots) ->
    case RootsC of
        #mcp_roots_capability{} -> ok;
        undefined -> {error, capability_not_supported}
    end;
validate_capability(_, _) ->
    {error, unknown_capability}.

validate_feature(Caps, resources, subscribe) ->
    case validate_capability(Caps, resources) of
        ok ->
            Resources = Caps#mcp_server_capabilities.resources,
            case Resources#mcp_resources_capability.subscribe of
                true -> ok;
                false -> {error, feature_not_supported}
            end;
        Error -> Error
    end;
validate_feature(Caps, resources, listChanged) ->
    case validate_capability(Caps, resources) of
        ok ->
            Resources = Caps#mcp_server_capabilities.resources,
            case Resources#mcp_resources_capability.listChanged of
                true -> ok;
                false -> {error, feature_not_supported}
            end;
        Error -> Error
    end;
validate_feature(Caps, tools, listChanged) ->
    case validate_capability(Caps, tools) of
        ok ->
            Tools = Caps#mcp_server_capabilities.tools,
            case Tools#mcp_tools_capability.listChanged of
                true -> ok;
                false -> {error, feature_not_supported}
            end;
        Error -> Error
    end;
validate_feature(Caps, prompts, listChanged) ->
    case validate_capability(Caps, prompts) of
        ok ->
            Prompts = Caps#mcp_server_capabilities.prompts,
            case Prompts#mcp_prompts_capability.listChanged of
                true -> ok;
                false -> {error, feature_not_supported}
            end;
        Error -> Error
    end;
validate_feature(_, _, _) ->
    {error, unknown_feature}.

%%====================================================================
%% Public API - Serialization
%%====================================================================

capability_to_map(#mcp_server_capabilities{} = Caps) ->
    server_capabilities_to_map(Caps);
capability_to_map(#mcp_resources_capability{} = Res) ->
    #{
        ?MCP_FEATURE_SUBSCRIBE => Res#mcp_resources_capability.subscribe,
        ?MCP_FEATURE_LIST_CHANGED => Res#mcp_resources_capability.listChanged
    };
capability_to_map(#mcp_tools_capability{} = ToolsCap) ->
    #{
        ?MCP_FEATURE_LIST_CHANGED => ToolsCap#mcp_tools_capability.listChanged
    };
capability_to_map(#mcp_prompts_capability{} = PromptsCap) ->
    #{
        ?MCP_FEATURE_LIST_CHANGED => PromptsCap#mcp_prompts_capability.listChanged
    };
capability_to_map(#mcp_logging_capability{}) ->
    #{};
capability_to_map(#mcp_sampling_capability{} = SamplingCap) ->
    case SamplingCap#mcp_sampling_capability.modelPreferences of
        undefined -> #{};
        Prefs -> #{<<"modelPreferences">> => Prefs}
    end;
capability_to_map(#mcp_roots_capability{}) ->
    #{};
capability_to_map(_) ->
    #{}.

server_capabilities_to_map(#mcp_server_capabilities{} = Caps) ->
    M = #{},
    M1 = case Caps#mcp_server_capabilities.resources of
        #mcp_resources_capability{} = Res ->
            M#{?MCP_CAPABILITY_RESOURCES => capability_to_map(Res)};
        _ -> M
    end,
    M2 = case Caps#mcp_server_capabilities.tools of
        #mcp_tools_capability{} = ToolsCap ->
            M1#{?MCP_CAPABILITY_TOOLS => capability_to_map(ToolsCap)};
        _ -> M1
    end,
    M3 = case Caps#mcp_server_capabilities.prompts of
        #mcp_prompts_capability{} = PromptsCap ->
            M2#{?MCP_CAPABILITY_PROMPTS => capability_to_map(PromptsCap)};
        _ -> M2
    end,
    M4 = case Caps#mcp_server_capabilities.logging of
        #mcp_logging_capability{} = LoggingCap ->
            M3#{?MCP_CAPABILITY_LOGGING => capability_to_map(LoggingCap)};
        _ -> M3
    end,
    M5 = case Caps#mcp_server_capabilities.sampling of
        #mcp_sampling_capability{} = SamplingCap ->
            M4#{<<"sampling">> => capability_to_map(SamplingCap)};
        _ -> M4
    end,
    M6 = case Caps#mcp_server_capabilities.roots of
        #mcp_roots_capability{} = RootsCap ->
            M5#{?MCP_CAPABILITY_ROOTS => capability_to_map(RootsCap)};
        _ -> M5
    end,
    case Caps#mcp_server_capabilities.experimental of
        undefined -> M6;
        Exp -> M6#{<<"experimental">> => Exp}
    end.

client_capabilities_to_map(#mcp_client_capabilities{} = Caps) ->
    M = #{},
    M1 = case Caps#mcp_client_capabilities.roots of
        #mcp_capability{enabled = true} ->
            M#{?MCP_CAPABILITY_ROOTS => #{}};
        _ -> M
    end,
    M2 = case Caps#mcp_client_capabilities.sampling of
        #mcp_capability{enabled = true} ->
            M1#{<<"sampling">> => #{}};
        _ -> M1
    end,
    case Caps#mcp_client_capabilities.experimental of
        undefined -> M2;
        Exp -> M2#{<<"experimental">> => Exp}
    end.

%%====================================================================
%% Public API - Deserialization
%%====================================================================

server_capabilities_from_map(Map) when is_map(Map) ->
    #mcp_server_capabilities{
        resources = extract_resources_capability(Map),
        tools = extract_tools_capability(Map),
        prompts = extract_prompts_capability(Map),
        logging = extract_logging_capability(Map),
        sampling = extract_sampling_capability(Map),
        roots = extract_roots_capability(Map),
        experimental = maps:get(<<"experimental">>, Map, undefined)
    }.

client_capabilities_from_map(Map) when is_map(Map) ->
    #mcp_client_capabilities{
        roots = extract_client_root_capability(Map),
        sampling = extract_client_sampling_capability(Map),
        experimental = maps:get(<<"experimental">>, Map, undefined)
    }.

%%====================================================================
%% Internal Functions - Capability Extraction
%%====================================================================

extract_resources_capability(Map) ->
    case maps:get(?MCP_CAPABILITY_RESOURCES, Map, #{}) of
        ResourcesMap when is_map(ResourcesMap) ->
            #mcp_resources_capability{
                subscribe = maps:get(?MCP_FEATURE_SUBSCRIBE, ResourcesMap, false),
                listChanged = maps:get(?MCP_FEATURE_LIST_CHANGED, ResourcesMap, false)
            };
        _ ->
            #mcp_resources_capability{}
    end.

extract_tools_capability(Map) ->
    case maps:get(?MCP_CAPABILITY_TOOLS, Map, #{}) of
        ToolsMap when is_map(ToolsMap) ->
            #mcp_tools_capability{
                listChanged = maps:get(?MCP_FEATURE_LIST_CHANGED, ToolsMap, false)
            };
        _ ->
            #mcp_tools_capability{}
    end.

extract_prompts_capability(Map) ->
    case maps:get(?MCP_CAPABILITY_PROMPTS, Map, #{}) of
        PromptsMap when is_map(PromptsMap) ->
            #mcp_prompts_capability{
                listChanged = maps:get(?MCP_FEATURE_LIST_CHANGED, PromptsMap, false)
            };
        _ ->
            #mcp_prompts_capability{}
    end.

extract_logging_capability(Map) ->
    case maps:get(?MCP_CAPABILITY_LOGGING, Map, undefined) of
        LoggingMap when is_map(LoggingMap) ->
            #mcp_logging_capability{};
        _ ->
            #mcp_logging_capability{}
    end.

extract_sampling_capability(Map) ->
    case maps:get(<<"sampling">>, Map, #{}) of
        SamplingMap when is_map(SamplingMap) ->
            #mcp_sampling_capability{
                modelPreferences = maps:get(<<"modelPreferences">>, SamplingMap, undefined)
            };
        _ ->
            #mcp_sampling_capability{}
    end.

extract_roots_capability(Map) ->
    case maps:get(?MCP_CAPABILITY_ROOTS, Map, undefined) of
        RootsMap when is_map(RootsMap) ->
            #mcp_roots_capability{};
        _ ->
            #mcp_roots_capability{}
    end.

extract_client_root_capability(Map) ->
    case maps:get(?MCP_CAPABILITY_ROOTS, Map, undefined) of
        RootsMap when is_map(RootsMap) ->
            #mcp_capability{enabled = true};
        _ ->
            #mcp_capability{enabled = false}
    end.

extract_client_sampling_capability(Map) ->
    case maps:get(<<"sampling">>, Map, undefined) of
        SamplingMap when is_map(SamplingMap) ->
            #mcp_capability{enabled = true};
        _ ->
            #mcp_capability{enabled = false}
    end.

%%====================================================================
%% Public API - Supported Versions
%%====================================================================

supported_versions() ->
    [<<"2025-11-25">>, <<"2024-11-05">>].
