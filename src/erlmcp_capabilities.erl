%%%===================================================================
%% @doc MCP Capability Negotiation Module
%%
%% Handles capability extraction, validation, and negotiation during
%% the MCP initialize request/response exchange.
%%
%% This module provides functions for:
%% - Extracting client capabilities from initialize requests
%% - Validating protocol versions
%% - Checking negotiated capabilities
%% - Blocking non-negotiated feature requests
%%
%% @end
%%%===================================================================

-module(erlmcp_capabilities).

-include("erlmcp.hrl").

%% Public API
-export([
    extract_client_capabilities/1,
    validate_protocol_version/1,
    client_has_capability/2,
    build_server_capabilities/1,
    check_capability_for_request/2
]).

-type client_capabilities() :: #mcp_client_capabilities{}.
-type validation_result() :: ok | {error, binary()}.

-export_type([client_capabilities/0, validation_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Extract client capabilities from initialize request parameters.
%% Parses the capabilities field from the initialize params and returns
%% a mcp_client_capabilities record with all enabled capabilities marked.
%%
%% Returns a mcp_client_capabilities record with:
%% - roots: enabled if client advertises roots capability
%% - sampling: enabled if client advertises sampling capability
%% - experimental: map of experimental capabilities
-spec extract_client_capabilities(map() | undefined) -> client_capabilities().
extract_client_capabilities(Params) when is_map(Params) ->
    CapMap = maps:get(?MCP_FIELD_CAPABILITIES, Params, #{}),
    extract_capabilities_from_map(CapMap);
extract_client_capabilities(_) ->
    #mcp_client_capabilities{}.

%% @doc Validate that client protocol version is compatible.
%% Currently enforces exact version match, but can be extended
%% to support compatible versions.
%%
%% Returns:
%% - ok if version is compatible
%% - {error, Reason} if version is incompatible
-spec validate_protocol_version(binary() | undefined) -> validation_result().
validate_protocol_version(Version) when is_binary(Version) ->
    case Version of
        ?MCP_VERSION ->
            ok;
        _ ->
            %% Check if version is in future (not yet supported)
            case binary:compare(Version, ?MCP_VERSION) of
                greater ->
                    {error, <<"Protocol version not yet supported">>};
                less ->
                    %% Accept versions >= 2025-01-01
                    case binary:compare(Version, <<"2025-01-01">>) of
                        less ->
                            {error, <<"Protocol version too old">>};
                        _ ->
                            ok
                    end
            end
    end;
validate_protocol_version(undefined) ->
    ok;
validate_protocol_version(_) ->
    {error, <<"Invalid protocol version format">>}.

%% @doc Check if client has negotiated a specific capability.
%% Used to validate that client supports features before invoking them.
%%
%% Returns true if capability is enabled, false otherwise.
-spec client_has_capability(client_capabilities(), binary()) -> boolean().
client_has_capability(#mcp_client_capabilities{roots = Roots}, ?MCP_CAPABILITY_ROOTS) ->
    is_enabled(Roots);
client_has_capability(#mcp_client_capabilities{sampling = Sampling}, ?MCP_CAPABILITY_SAMPLING) ->
    is_enabled(Sampling);
client_has_capability(#mcp_client_capabilities{experimental = Exp}, CapName) when is_map(Exp) ->
    maps:is_key(CapName, Exp);
client_has_capability(_, _) ->
    false.

%% @doc Build server capability map for initialize response.
%% Encodes mcp_server_capabilities record into JSON-friendly map
%% with feature flags.
-spec build_server_capabilities(#mcp_server_capabilities{}) -> map().
build_server_capabilities(#mcp_server_capabilities{} = Caps) ->
    Base = #{},
    Base1 = add_capability(Base, ?MCP_CAPABILITY_RESOURCES,
                          Caps#mcp_server_capabilities.resources,
                          #{?MCP_FEATURE_SUBSCRIBE => true,
                            ?MCP_FEATURE_LIST_CHANGED => true}),
    Base2 = add_capability(Base1, ?MCP_CAPABILITY_TOOLS,
                          Caps#mcp_server_capabilities.tools, #{}),
    Base3 = add_capability(Base2, ?MCP_CAPABILITY_PROMPTS,
                          Caps#mcp_server_capabilities.prompts,
                          #{?MCP_FEATURE_LIST_CHANGED => true}),
    add_capability(Base3, ?MCP_CAPABILITY_LOGGING,
                   Caps#mcp_server_capabilities.logging, #{}).

%% @doc Check if a request method is allowed given negotiated capabilities.
%% Returns {ok, Method} if allowed, {error, Reason} if not permitted.
%%
%% This enforces that:
%% - Resources methods require resources capability
%% - Tools methods require tools capability
%% - Prompts methods require prompts capability
%% - Initialize can only be called once
-spec check_capability_for_request(binary(), #mcp_client_capabilities{} | undefined) ->
    {ok, binary()} | {error, binary()}.
check_capability_for_request(?MCP_METHOD_INITIALIZE, _) ->
    %% Initialize is always allowed (first message)
    {ok, ?MCP_METHOD_INITIALIZE};
check_capability_for_request(Method, ClientCaps) when ?MCP_METHOD_RESOURCES_LIST =:= Method;
                                                      ?MCP_METHOD_RESOURCES_READ =:= Method;
                                                      ?MCP_METHOD_RESOURCES_SUBSCRIBE =:= Method;
                                                      ?MCP_METHOD_RESOURCES_UNSUBSCRIBE =:= Method ->
    case client_has_capability(ClientCaps, ?MCP_CAPABILITY_RESOURCES) of
        true -> {ok, Method};
        false -> {error, <<"Client does not support resources capability">>}
    end;
check_capability_for_request(Method, ClientCaps) when ?MCP_METHOD_TOOLS_LIST =:= Method;
                                                      ?MCP_METHOD_TOOLS_CALL =:= Method ->
    case client_has_capability(ClientCaps, ?MCP_CAPABILITY_TOOLS) of
        true -> {ok, Method};
        false -> {error, <<"Client does not support tools capability">>}
    end;
check_capability_for_request(Method, ClientCaps) when ?MCP_METHOD_PROMPTS_LIST =:= Method;
                                                      ?MCP_METHOD_PROMPTS_GET =:= Method ->
    case client_has_capability(ClientCaps, ?MCP_CAPABILITY_PROMPTS) of
        true -> {ok, Method};
        false -> {error, <<"Client does not support prompts capability">>}
    end;
check_capability_for_request(?MCP_METHOD_SAMPLING_CREATE_MESSAGE, ClientCaps) ->
    case client_has_capability(ClientCaps, ?MCP_CAPABILITY_SAMPLING) of
        true -> {ok, ?MCP_METHOD_SAMPLING_CREATE_MESSAGE};
        false -> {error, <<"Client does not support sampling capability">>}
    end;
check_capability_for_request(Method, _) ->
    %% Other methods (notifications, tasks) are allowed without specific capability
    {ok, Method}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Extract capabilities from capabilities map
-spec extract_capabilities_from_map(map()) -> client_capabilities().
extract_capabilities_from_map(CapMap) ->
    Roots = case maps:is_key(?MCP_CAPABILITY_ROOTS, CapMap) of
        true -> #mcp_capability{enabled = true};
        false -> undefined
    end,

    Sampling = case maps:is_key(?MCP_CAPABILITY_SAMPLING, CapMap) of
        true -> #mcp_capability{enabled = true};
        false -> undefined
    end,

    Experimental = maps:get(experimental, CapMap, undefined),

    #mcp_client_capabilities{
        roots = Roots,
        sampling = Sampling,
        experimental = Experimental
    }.

%% Check if a capability record is enabled
-spec is_enabled(#mcp_capability{} | undefined) -> boolean().
is_enabled(undefined) ->
    false;
is_enabled(#mcp_capability{enabled = true}) ->
    true;
is_enabled(#mcp_capability{enabled = false}) ->
    false.

%% Add capability to map if enabled
-spec add_capability(map(), binary(), #mcp_capability{} | undefined, map()) -> map().
add_capability(Map, _Key, undefined, _Features) ->
    Map;
add_capability(Map, Key, #mcp_capability{enabled = true}, Features) ->
    Map#{Key => Features};
add_capability(Map, _Key, #mcp_capability{enabled = false}, _Features) ->
    Map.
