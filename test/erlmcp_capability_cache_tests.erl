-module(erlmcp_capability_cache_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Chicago School TDD - Capability Cache Tests
%% State-based verification, real capability records
%%====================================================================

%%====================================================================
%% Test Cases - Cache Creation
%%====================================================================

new_cache_test() ->
    %% Setup: Create server capabilities
    Capabilities = #mcp_server_capabilities{
        resources => #{},
        tools => #{},
        prompts => undefined,
        sampling => undefined
    },

    %% Exercise: Create capability cache
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Verify: Cache structure (state-based)
    ?assert(is_map(Cache)),
    ?assert(maps:is_key(capabilities, Cache)),
    ?assert(maps:is_key(client_capabilities, Cache)),
    ?assert(maps:is_key(resource_support, Cache)),
    ?assert(maps:is_key(tool_support, Cache)),
    ?assert(maps:is_key(prompt_support, Cache)),
    ?assert(maps:is_key(sampling_support, Cache)).

new_cache_with_all_capabilities_test() ->
    %% Setup: Capabilities with all features enabled
    Capabilities = #mcp_server_capabilities{
        resources => #{list => true},
        tools => #{list => true},
        prompts => #{list => true},
        sampling => #{enabled => true}
    },

    %% Exercise: Create cache
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Verify: All capabilities detected (observable state)
    ?assertEqual(true, maps:get(resource_support, Cache)),
    ?assertEqual(true, maps:get(tool_support, Cache)),
    ?assertEqual(true, maps:get(prompt_support, Cache)),
    ?assertEqual(true, maps:get(sampling_support, Cache)).

new_cache_with_no_capabilities_test() ->
    %% Setup: Empty capabilities
    Capabilities = #mcp_server_capabilities{
        resources => undefined,
        tools => undefined,
        prompts => undefined,
        sampling => undefined
    },

    %% Exercise: Create cache
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Verify: No capabilities detected
    ?assertEqual(false, maps:get(resource_support, Cache)),
    ?assertEqual(false, maps:get(tool_support, Cache)),
    ?assertEqual(false, maps:get(prompt_support, Cache)),
    ?assertEqual(false, maps:get(sampling_support, Cache)).

%%====================================================================
%% Test Cases - Capability Checks (O(1) Lookup)
%%====================================================================

check_capability_resource_test() ->
    %% Setup: Cache with resource support
    Capabilities = #mcp_server_capabilities{
        resources => #{list => true},
        tools => undefined,
        prompts => undefined,
        sampling => undefined
    },
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Exercise: Check resource capability (O(1) map lookup)
    Result = erlmcp_capability_cache:check_capability(Cache, resource),

    %% Verify: Resource supported
    ?assertEqual(true, Result),

    %% Verify: Other capabilities not supported
    ?assertEqual(false, erlmcp_capability_cache:check_capability(Cache, tool)),
    ?assertEqual(false, erlmcp_capability_cache:check_capability(Cache, prompt)),
    ?assertEqual(false, erlmcp_capability_cache:check_capability(Cache, sampling)).

check_capability_tool_test() ->
    %% Setup: Cache with tool support
    Capabilities = #mcp_server_capabilities{
        resources => undefined,
        tools => #{list => true},
        prompts => undefined,
        sampling => undefined
    },
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Exercise: Check tool capability
    ?assertEqual(true, erlmcp_capability_cache:check_capability(Cache, tool)),
    ?assertEqual(false, erlmcp_capability_cache:check_capability(Cache, resource)).

check_capability_unknown_test() ->
    %% Setup: Any cache
    Capabilities = #mcp_server_capabilities{},
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Exercise: Check unknown capability
    Result = erlmcp_capability_cache:check_capability(Cache, unknown_capability),

    %% Verify: Returns false for unknown
    ?assertEqual(false, Result).

%%====================================================================
%% Test Cases - Multi-Capability Checks
%%====================================================================

check_capabilities_any_test() ->
    %% Setup: Cache with resource and tool support
    Capabilities = #mcp_server_capabilities{
        resources => #{list => true},
        tools => #{list => true},
        prompts => undefined,
        sampling => undefined
    },
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Exercise: Check ANY of [resource, prompt]
    ResultAny = erlmcp_capability_cache:check_capabilities(Cache, [resource, prompt], any),

    %% Verify: Returns true (resource is supported, short-circuit)
    ?assertEqual(true, ResultAny),

    %% Exercise: Check ANY of [prompt, sampling]
    ResultNone = erlmcp_capability_cache:check_capabilities(Cache, [prompt, sampling], any),

    %% Verify: Returns false (neither supported)
    ?assertEqual(false, ResultNone).

check_capabilities_all_test() ->
    %% Setup: Cache with resource and tool support
    Capabilities = #mcp_server_capabilities{
        resources => #{list => true},
        tools => #{list => true},
        prompts => undefined,
        sampling => undefined
    },
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Exercise: Check ALL of [resource, tool]
    ResultAll = erlmcp_capability_cache:check_capabilities(Cache, [resource, tool], all),

    %% Verify: Returns true (both supported)
    ?assertEqual(true, ResultAll),

    %% Exercise: Check ALL of [resource, prompt]
    ResultPartial = erlmcp_capability_cache:check_capabilities(Cache, [resource, prompt], all),

    %% Verify: Returns false (prompt not supported)
    ?assertEqual(false, ResultPartial).

check_capabilities_empty_list_test() ->
    %% Setup: Any cache
    Capabilities = #mcp_server_capabilities{},
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Exercise: Check empty list
    ResultAny = erlmcp_capability_cache:check_capabilities(Cache, [], any),
    ResultAll = erlmcp_capability_cache:check_capabilities(Cache, [], all),

    %% Verify: Empty list returns true (vacuous truth)
    ?assertEqual(true, ResultAny),
    ?assertEqual(true, ResultAll).

%%====================================================================
%% Test Cases - Specific Capability Checks (Inline Fast Path)
%%====================================================================

has_resource_support_test() ->
    %% Setup: Cache with resources
    Capabilities = #mcp_server_capabilities{
        resources => #{list => true}
    },
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Exercise: Fast inline check
    ?assertEqual(true, erlmcp_capability_cache:has_resource_support(Cache)),

    %% Setup: Cache without resources
    CapabilitiesNo = #mcp_server_capabilities{resources => undefined},
    CacheNo = erlmcp_capability_cache:new(CapabilitiesNo),

    %% Exercise: Fast inline check
    ?assertEqual(false, erlmcp_capability_cache:has_resource_support(CacheNo)).

has_tool_support_test() ->
    %% Setup: Cache with tools
    Capabilities = #mcp_server_capabilities{tools => #{list => true}},
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Verify: Tool support detected
    ?assertEqual(true, erlmcp_capability_cache:has_tool_support(Cache)).

has_prompt_support_test() ->
    %% Setup: Cache with prompts
    Capabilities = #mcp_server_capabilities{prompts => #{list => true}},
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Verify: Prompt support detected
    ?assertEqual(true, erlmcp_capability_cache:has_prompt_support(Cache)).

has_sampling_support_test() ->
    %% Setup: Cache with sampling
    Capabilities = #mcp_server_capabilities{sampling => #{enabled => true}},
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Verify: Sampling support detected
    ?assertEqual(true, erlmcp_capability_cache:has_sampling_support(Cache)).

%%====================================================================
%% Test Cases - Client Capability Update
%%====================================================================

update_client_capabilities_test() ->
    %% Setup: Create cache
    ServerCaps = #mcp_server_capabilities{},
    Cache = erlmcp_capability_cache:new(ServerCaps),

    %% Verify: Initially no client capabilities
    ?assertEqual(undefined, maps:get(client_capabilities, Cache)),

    %% Exercise: Update with client capabilities
    ClientCaps = #mcp_client_capabilities{
        roots => #{list => true},
        sampling => #{enabled => true}
    },
    UpdatedCache = erlmcp_capability_cache:update_client_capabilities(Cache, ClientCaps),

    %% Verify: Client capabilities stored (state-based)
    ?assertEqual(ClientCaps, maps:get(client_capabilities, UpdatedCache)).

get_capabilities_test() ->
    %% Setup: Create cache with capabilities
    Capabilities = #mcp_server_capabilities{
        resources => #{list => true},
        tools => #{list => true}
    },
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Exercise: Get underlying capabilities record
    Retrieved = erlmcp_capability_cache:get_capabilities(Cache),

    %% Verify: Original capabilities returned
    ?assertEqual(Capabilities, Retrieved).

%%====================================================================
%% Edge Cases and Performance
%%====================================================================

cache_with_empty_maps_test() ->
    %% Setup: Capabilities with empty maps (not undefined)
    Capabilities = #mcp_server_capabilities{
        resources => #{},
        tools => #{},
        prompts => #{},
        sampling => #{}
    },
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Verify: Empty maps are considered "supported" (non-undefined)
    ?assertEqual(true, maps:get(resource_support, Cache)),
    ?assertEqual(true, maps:get(tool_support, Cache)),
    ?assertEqual(true, maps:get(prompt_support, Cache)),
    ?assertEqual(true, maps:get(sampling_support, Cache)).

multiple_cache_instances_test() ->
    %% Exercise: Create multiple independent caches (stateless operation)
    Cache1 = erlmcp_capability_cache:new(#mcp_server_capabilities{
        resources => #{list => true}
    }),
    Cache2 = erlmcp_capability_cache:new(#mcp_server_capabilities{
        tools => #{list => true}
    }),
    Cache3 = erlmcp_capability_cache:new(#mcp_server_capabilities{
        prompts => #{list => true}
    }),

    %% Verify: Each cache independent (no shared state)
    ?assertEqual(true, erlmcp_capability_cache:has_resource_support(Cache1)),
    ?assertEqual(false, erlmcp_capability_cache:has_resource_support(Cache2)),
    ?assertEqual(false, erlmcp_capability_cache:has_resource_support(Cache3)),

    ?assertEqual(false, erlmcp_capability_cache:has_tool_support(Cache1)),
    ?assertEqual(true, erlmcp_capability_cache:has_tool_support(Cache2)),
    ?assertEqual(false, erlmcp_capability_cache:has_tool_support(Cache3)).

check_capability_performance_test() ->
    %% Setup: Create cache
    Capabilities = #mcp_server_capabilities{
        resources => #{list => true},
        tools => #{list => true}
    },
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Exercise: Perform 10,000 capability checks (O(1) operations)
    StartTime = erlang:system_time(microsecond),
    lists:foreach(fun(_) ->
        erlmcp_capability_cache:check_capability(Cache, resource),
        erlmcp_capability_cache:check_capability(Cache, tool),
        erlmcp_capability_cache:check_capability(Cache, prompt),
        erlmcp_capability_cache:check_capability(Cache, sampling)
    end, lists:seq(1, 10000)),
    EndTime = erlang:system_time(microsecond),

    %% Verify: Fast execution (< 100ms for 40,000 checks)
    ElapsedUs = EndTime - StartTime,
    ElapsedMs = ElapsedUs / 1000,
    io:format("40,000 capability checks in ~.2f ms~n", [ElapsedMs]),
    ?assert(ElapsedMs < 100).

check_capabilities_short_circuit_test() ->
    %% Setup: Cache with resource support
    Capabilities = #mcp_server_capabilities{resources => #{list => true}},
    Cache = erlmcp_capability_cache:new(Capabilities),

    %% Exercise: Check with 'any' mode (should short-circuit after first match)
    %% Place supported capability first
    Result1 = erlmcp_capability_cache:check_capabilities(
        Cache, [resource, tool, prompt, sampling], any),

    %% Verify: Returns true immediately (short-circuit)
    ?assertEqual(true, Result1),

    %% Exercise: Check with 'all' mode (should short-circuit on first failure)
    Result2 = erlmcp_capability_cache:check_capabilities(
        Cache, [tool, resource], all),  % tool is first and false

    %% Verify: Returns false immediately (short-circuit)
    ?assertEqual(false, Result2).
