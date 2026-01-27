%%%-------------------------------------------------------------------
%% Unit tests for capability negotiation module
%% Tests for erlmcp_capabilities.erl
%%%-------------------------------------------------------------------

-module(erlmcp_capabilities_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test: Capability Building
%%====================================================================

build_default_server_capabilities_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    ?assert(is_record(Caps, mcp_server_capabilities)),
    ?assert(Caps#mcp_server_capabilities.resources#mcp_resources_capability.subscribe =:= true),
    ?assert(Caps#mcp_server_capabilities.resources#mcp_resources_capability.listChanged =:= true),
    ?assert(Caps#mcp_server_capabilities.tools#mcp_tools_capability.listChanged =:= true),
    ?assert(Caps#mcp_server_capabilities.prompts#mcp_prompts_capability.listChanged =:= true).

build_custom_capabilities_test() ->
    Config = #{
        resources => #{
            subscribe => false,
            listChanged => true
        }
    },
    Caps = erlmcp_capabilities:build_server_capabilities(Config),
    ?assert(Caps#mcp_server_capabilities.resources#mcp_resources_capability.subscribe =:= false),
    ?assert(Caps#mcp_server_capabilities.resources#mcp_resources_capability.listChanged =:= true).

%%====================================================================
%% Test: Client Capability Extraction
%%====================================================================

extract_client_capabilities_undefined_test() ->
    Caps = erlmcp_capabilities:extract_client_capabilities(undefined),
    ?assert(is_record(Caps, mcp_client_capabilities)),
    ?assert(Caps#mcp_client_capabilities.roots#mcp_capability.enabled =:= false).

extract_client_capabilities_with_roots_test() ->
    Params = #{
        <<"capabilities">> => #{
            <<"roots">> => #{}
        }
    },
    Caps = erlmcp_capabilities:extract_client_capabilities(Params),
    ?assert(Caps#mcp_client_capabilities.roots#mcp_capability.enabled =:= true).

extract_client_capabilities_with_sampling_test() ->
    Params = #{
        <<"capabilities">> => #{
            <<"sampling">> => #{}
        }
    },
    Caps = erlmcp_capabilities:extract_client_capabilities(Params),
    ?assert(Caps#mcp_client_capabilities.sampling#mcp_capability.enabled =:= true).

extract_client_capabilities_with_experimental_test() ->
    Params = #{
        <<"capabilities">> => #{
            <<"experimental">> => #{<<"myfeature">> => true}
        }
    },
    Caps = erlmcp_capabilities:extract_client_capabilities(Params),
    ?assert(Caps#mcp_client_capabilities.experimental =:= #{<<"myfeature">> => true}).

%%====================================================================
%% Test: Protocol Version Validation
%%====================================================================

validate_protocol_version_2025_test() ->
    Result = erlmcp_capabilities:validate_protocol_version(<<"2025-11-25">>),
    ?assertEqual(ok, Result).

validate_protocol_version_2024_test() ->
    Result = erlmcp_capabilities:validate_protocol_version(<<"2024-11-05">>),
    ?assertEqual(ok, Result).

validate_protocol_version_invalid_test() ->
    Result = erlmcp_capabilities:validate_protocol_version(<<"1.0.0">>),
    ?assertEqual({error, <<"Unsupported protocol version">>}, Result).

%%====================================================================
%% Test: Capability Validation
%%====================================================================

validate_capability_resources_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    Result = erlmcp_capabilities:validate_capability(Caps, resources),
    ?assertEqual(ok, Result).

validate_capability_tools_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    Result = erlmcp_capabilities:validate_capability(Caps, tools),
    ?assertEqual(ok, Result).

validate_capability_prompts_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    Result = erlmcp_capabilities:validate_capability(Caps, prompts),
    ?assertEqual(ok, Result).

validate_capability_logging_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    Result = erlmcp_capabilities:validate_capability(Caps, logging),
    ?assertEqual(ok, Result).

validate_capability_sampling_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    Result = erlmcp_capabilities:validate_capability(Caps, sampling),
    ?assertEqual(ok, Result).

validate_capability_roots_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    Result = erlmcp_capabilities:validate_capability(Caps, roots),
    ?assertEqual(ok, Result).

validate_capability_unknown_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    Result = erlmcp_capabilities:validate_capability(Caps, unknown_cap),
    ?assertEqual({error, unknown_capability}, Result).

%%====================================================================
%% Test: Feature Validation
%%====================================================================

validate_feature_resources_subscribe_enabled_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    Result = erlmcp_capabilities:validate_feature(Caps, resources, subscribe),
    ?assertEqual(ok, Result).

validate_feature_resources_subscribe_disabled_test() ->
    Config = #{
        resources => #{
            subscribe => false,
            listChanged => true
        }
    },
    Caps = erlmcp_capabilities:build_server_capabilities(Config),
    Result = erlmcp_capabilities:validate_feature(Caps, resources, subscribe),
    ?assertEqual({error, feature_not_supported}, Result).

validate_feature_resources_listChanged_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    Result = erlmcp_capabilities:validate_feature(Caps, resources, listChanged),
    ?assertEqual(ok, Result).

validate_feature_tools_listChanged_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    Result = erlmcp_capabilities:validate_feature(Caps, tools, listChanged),
    ?assertEqual(ok, Result).

validate_feature_prompts_listChanged_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    Result = erlmcp_capabilities:validate_feature(Caps, prompts, listChanged),
    ?assertEqual(ok, Result).

validate_feature_unknown_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    Result = erlmcp_capabilities:validate_feature(Caps, unknown, unknown),
    ?assertEqual({error, unknown_feature}, Result).

%%====================================================================
%% Test: Serialization to Map
%%====================================================================

capability_to_map_resources_test() ->
    Resources = #mcp_resources_capability{subscribe = true, listChanged = false},
    Map = erlmcp_capabilities:capability_to_map(Resources),
    ?assertEqual(true, maps:get(<<"subscribe">>, Map)),
    ?assertEqual(false, maps:get(<<"listChanged">>, Map)).

capability_to_map_tools_test() ->
    Tools = #mcp_tools_capability{listChanged = true},
    Map = erlmcp_capabilities:capability_to_map(Tools),
    ?assertEqual(true, maps:get(<<"listChanged">>, Map)).

capability_to_map_prompts_test() ->
    Prompts = #mcp_prompts_capability{listChanged = false},
    Map = erlmcp_capabilities:capability_to_map(Prompts),
    ?assertEqual(false, maps:get(<<"listChanged">>, Map)).

capability_to_map_logging_test() ->
    Logging = #mcp_logging_capability{},
    Map = erlmcp_capabilities:capability_to_map(Logging),
    ?assertEqual(#{}, Map).

capability_to_map_roots_test() ->
    Roots = #mcp_roots_capability{},
    Map = erlmcp_capabilities:capability_to_map(Roots),
    ?assertEqual(#{}, Map).

server_capabilities_to_map_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    Map = erlmcp_capabilities:capability_to_map(Caps),
    ?assert(maps:is_key(<<"resources">>, Map)),
    ?assert(maps:is_key(<<"tools">>, Map)),
    ?assert(maps:is_key(<<"prompts">>, Map)),
    ?assert(maps:is_key(<<"logging">>, Map)).

server_capabilities_to_map_with_experimental_test() ->
    DefaultCaps = erlmcp_capabilities:build_server_capabilities(),
    Caps = DefaultCaps#mcp_server_capabilities{experimental = #{<<"my_feature">> => true}},
    Map = erlmcp_capabilities:capability_to_map(Caps),
    ?assertEqual(#{<<"my_feature">> => true}, maps:get(<<"experimental">>, Map)).

client_capabilities_to_map_test() ->
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        experimental = undefined
    },
    Map = erlmcp_capabilities:client_capabilities_to_map(ClientCaps),
    ?assert(maps:is_key(<<"roots">>, Map)),
    ?assertNot(maps:is_key(<<"sampling">>, Map)).

%%====================================================================
%% Test: Deserialization from Map
%%====================================================================

server_capabilities_from_map_test() ->
    Map = #{
        <<"resources">> => #{
            <<"subscribe">> => true,
            <<"listChanged">> => false
        },
        <<"tools">> => #{
            <<"listChanged">> => true
        }
    },
    Caps = erlmcp_capabilities:server_capabilities_from_map(Map),
    ?assertEqual(true, Caps#mcp_server_capabilities.resources#mcp_resources_capability.subscribe),
    ?assertEqual(false, Caps#mcp_server_capabilities.resources#mcp_resources_capability.listChanged),
    ?assertEqual(true, Caps#mcp_server_capabilities.tools#mcp_tools_capability.listChanged).

server_capabilities_from_map_defaults_test() ->
    Map = #{},
    Caps = erlmcp_capabilities:server_capabilities_from_map(Map),
    ?assertEqual(false, Caps#mcp_server_capabilities.resources#mcp_resources_capability.subscribe),
    ?assertEqual(false, Caps#mcp_server_capabilities.resources#mcp_resources_capability.listChanged).

server_capabilities_from_map_with_experimental_test() ->
    Map = #{
        <<"experimental">> => #{<<"feature">> => <<"value">>}
    },
    Caps = erlmcp_capabilities:server_capabilities_from_map(Map),
    ?assertEqual(#{<<"feature">> => <<"value">>}, Caps#mcp_server_capabilities.experimental).

client_capabilities_from_map_test() ->
    Map = #{
        <<"roots">> => #{},
        <<"sampling">> => #{}
    },
    Caps = erlmcp_capabilities:client_capabilities_from_map(Map),
    ?assertEqual(true, Caps#mcp_client_capabilities.roots#mcp_capability.enabled),
    ?assertEqual(true, Caps#mcp_client_capabilities.sampling#mcp_capability.enabled).

client_capabilities_from_map_empty_test() ->
    Map = #{},
    Caps = erlmcp_capabilities:client_capabilities_from_map(Map),
    ?assertEqual(false, Caps#mcp_client_capabilities.roots#mcp_capability.enabled),
    ?assertEqual(false, Caps#mcp_client_capabilities.sampling#mcp_capability.enabled).

%%====================================================================
%% Test: Round Trip Serialization
%%====================================================================

roundtrip_server_capabilities_test() ->
    Original = erlmcp_capabilities:build_server_capabilities(),
    Map = erlmcp_capabilities:capability_to_map(Original),
    Restored = erlmcp_capabilities:server_capabilities_from_map(Map),
    
    ?assertEqual(
        Original#mcp_server_capabilities.resources#mcp_resources_capability.subscribe,
        Restored#mcp_server_capabilities.resources#mcp_resources_capability.subscribe
    ),
    ?assertEqual(
        Original#mcp_server_capabilities.resources#mcp_resources_capability.listChanged,
        Restored#mcp_server_capabilities.resources#mcp_resources_capability.listChanged
    ),
    ?assertEqual(
        Original#mcp_server_capabilities.tools#mcp_tools_capability.listChanged,
        Restored#mcp_server_capabilities.tools#mcp_tools_capability.listChanged
    ).

%%====================================================================
%% Test: Supported Versions
%%====================================================================

supported_versions_test() ->
    Versions = erlmcp_capabilities:supported_versions(),
    ?assert(lists:member(<<"2025-11-25">>, Versions)),
    ?assert(lists:member(<<"2024-11-05">>, Versions)).

%%====================================================================
%% Test Integration: Initialize Response with Capabilities
%%====================================================================

initialize_response_has_capabilities_test() ->
    Caps = erlmcp_capabilities:build_server_capabilities(),
    CapMap = erlmcp_capabilities:capability_to_map(Caps),
    
    Response = #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => CapMap,
        <<"serverInfo">> => #{
            <<"name">> => <<"erlmcp">>,
            <<"version">> => <<"0.6.0">>
        }
    },
    
    ?assert(maps:is_key(<<"capabilities">>, Response)),
    ?assert(maps:is_key(<<"resources">>, maps:get(<<"capabilities">>, Response))).

%%====================================================================
%% Test: Edge Cases
%%====================================================================

validate_capability_with_empty_record_test() ->
    %% Empty record still has default-initialized capabilities
    Caps = #mcp_server_capabilities{},
    Result = erlmcp_capabilities:validate_capability(Caps, resources),
    %% Should succeed because resources has default value of #mcp_resources_capability{}
    ?assertEqual(ok, Result).

capability_map_roundtrip_preserves_all_features_test() ->
    Resources = #mcp_resources_capability{subscribe = true, listChanged = true},
    Tools = #mcp_tools_capability{listChanged = true},
    Prompts = #mcp_prompts_capability{listChanged = false},
    Sampling = #mcp_sampling_capability{modelPreferences = #{<<"cost">> => 0.5}},
    
    OriginalCaps = #mcp_server_capabilities{
        resources = Resources,
        tools = Tools,
        prompts = Prompts,
        sampling = Sampling
    },
    
    Map = erlmcp_capabilities:capability_to_map(OriginalCaps),
    RestoredCaps = erlmcp_capabilities:server_capabilities_from_map(Map),
    
    ?assertEqual(true, RestoredCaps#mcp_server_capabilities.resources#mcp_resources_capability.subscribe),
    ?assertEqual(true, RestoredCaps#mcp_server_capabilities.resources#mcp_resources_capability.listChanged),
    ?assertEqual(true, RestoredCaps#mcp_server_capabilities.tools#mcp_tools_capability.listChanged),
    ?assertEqual(false, RestoredCaps#mcp_server_capabilities.prompts#mcp_prompts_capability.listChanged).

%%====================================================================
%% Integration Test Suite
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"build_default_server_capabilities", fun build_default_server_capabilities_test/0},
        {"extract_client_capabilities_undefined", fun extract_client_capabilities_undefined_test/0},
        {"validate_protocol_version_2025", fun validate_protocol_version_2025_test/0},
        {"validate_capability_resources", fun validate_capability_resources_test/0},
        {"validate_feature_resources_subscribe_enabled", fun validate_feature_resources_subscribe_enabled_test/0},
        {"server_capabilities_to_map", fun server_capabilities_to_map_test/0},
        {"roundtrip_server_capabilities", fun roundtrip_server_capabilities_test/0},
        {"supported_versions", fun supported_versions_test/0}
    ]}.
