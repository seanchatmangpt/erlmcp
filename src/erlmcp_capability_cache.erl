%% @doc Fast capability lookup cache for hot path optimization.
%% Provides O(1) capability checking and supports caching of capability
%% checks across the request lifecycle. Reduces allocations and improves
%% performance in hot paths that check capabilities frequently.
-module(erlmcp_capability_cache).

-include("erlmcp.hrl").

%% API exports
-export([
    new/1,
    check_capability/2,
    check_capabilities/3,
    has_resource_support/1,
    has_tool_support/1,
    has_prompt_support/1,
    has_sampling_support/1,
    get_capabilities/1,
    update_client_capabilities/2
]).

%% Types
-type cache() :: #{
    capabilities => #mcp_server_capabilities{},
    client_capabilities => #mcp_client_capabilities{} | undefined,
    resource_support => boolean(),
    tool_support => boolean(),
    prompt_support => boolean(),
    sampling_support => boolean()
}.

-export_type([cache/0]).

%%====================================================================
%% API Functions - Cache Management
%%====================================================================

%% @doc Create new capability cache from server capabilities.
-spec new(#mcp_server_capabilities{}) -> cache().
new(ServerCapabilities) when is_record(ServerCapabilities, mcp_server_capabilities) ->
    #{
        capabilities => ServerCapabilities,
        client_capabilities => undefined,
        resource_support => has_resources(ServerCapabilities),
        tool_support => has_tools(ServerCapabilities),
        prompt_support => has_prompts(ServerCapabilities),
        sampling_support => has_sampling(ServerCapabilities)
    }.

%% @doc Update cache with client capabilities (call during initialization).
-spec update_client_capabilities(cache(), #mcp_client_capabilities{}) -> cache().
update_client_capabilities(Cache, ClientCapabilities)
  when is_map(Cache), is_record(ClientCapabilities, mcp_client_capabilities) ->
    Cache#{client_capabilities => ClientCapabilities}.

%%====================================================================
%% Fast Capability Checks (Hot Path)
%%====================================================================

%% @doc Check if capability is supported (O(1) lookup).
-spec check_capability(cache(), atom()) -> boolean().
check_capability(Cache, resource) when is_map(Cache) ->
    maps:get(resource_support, Cache, false);
check_capability(Cache, tool) when is_map(Cache) ->
    maps:get(tool_support, Cache, false);
check_capability(Cache, prompt) when is_map(Cache) ->
    maps:get(prompt_support, Cache, false);
check_capability(Cache, sampling) when is_map(Cache) ->
    maps:get(sampling_support, Cache, false);
check_capability(_Cache, _Unknown) ->
    false.

%% @doc Check multiple capabilities (fast short-circuit evaluation).
-spec check_capabilities(cache(), [atom()], any | all) -> boolean().
check_capabilities(_Cache, [], _) -> true;
check_capabilities(Cache, [Cap | Rest], any) ->
    case check_capability(Cache, Cap) of
        true -> true;
        false -> check_capabilities(Cache, Rest, any)
    end;
check_capabilities(Cache, [Cap | Rest], all) ->
    case check_capability(Cache, Cap) of
        true -> check_capabilities(Cache, Rest, all);
        false -> false
    end.

%%====================================================================
%% Specific Capability Checks (O(1) Inline)
%%====================================================================

%% @doc Check resource support (fast inline check).
-spec has_resource_support(cache()) -> boolean().
has_resource_support(Cache) when is_map(Cache) ->
    maps:get(resource_support, Cache, false).

%% @doc Check tool support (fast inline check).
-spec has_tool_support(cache()) -> boolean().
has_tool_support(Cache) when is_map(Cache) ->
    maps:get(tool_support, Cache, false).

%% @doc Check prompt support (fast inline check).
-spec has_prompt_support(cache()) -> boolean().
has_prompt_support(Cache) when is_map(Cache) ->
    maps:get(prompt_support, Cache, false).

%% @doc Check sampling support (fast inline check).
-spec has_sampling_support(cache()) -> boolean().
has_sampling_support(Cache) when is_map(Cache) ->
    maps:get(sampling_support, Cache, false).

%% @doc Get underlying capabilities structure.
-spec get_capabilities(cache()) -> #mcp_server_capabilities{}.
get_capabilities(Cache) when is_map(Cache) ->
    maps:get(capabilities, Cache).

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% Fast checks for capability support
-spec has_resources(#mcp_server_capabilities{}) -> boolean().
has_resources(Capabilities) when is_record(Capabilities, mcp_server_capabilities) ->
    case Capabilities#mcp_server_capabilities.resources of
        undefined -> false;
        _Resources -> true
    end.

-spec has_tools(#mcp_server_capabilities{}) -> boolean().
has_tools(Capabilities) when is_record(Capabilities, mcp_server_capabilities) ->
    case Capabilities#mcp_server_capabilities.tools of
        undefined -> false;
        _Tools -> true
    end.

-spec has_prompts(#mcp_server_capabilities{}) -> boolean().
has_prompts(Capabilities) when is_record(Capabilities, mcp_server_capabilities) ->
    case Capabilities#mcp_server_capabilities.prompts of
        undefined -> false;
        _Prompts -> true
    end.

-spec has_sampling(#mcp_server_capabilities{}) -> boolean().
has_sampling(Capabilities) when is_record(Capabilities, mcp_server_capabilities) ->
    case Capabilities#mcp_server_capabilities.sampling of
        undefined -> false;
        _Sampling -> true
    end.
