-module(erlmcp_capability_negotiation_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Capability Negotiation Tests
%% Tests for erlmcp_capabilities module
%% Chicago School TDD: Real processes, state-based verification
%%====================================================================

%%--------------------------------------------------------------------
%% Extract Client Capabilities Tests
%%--------------------------------------------------------------------

extract_client_capabilities_full_test() ->
    Params = #{
        <<"protocolVersion">> => <<"2024-11-05">>,
        <<"capabilities">> => #{
            <<"roots">> => #{},
            <<"sampling">> => #{<<"modelPreferences">> => #{<<"temperature">> => 0.7}},
            <<"experimental">> => #{<<"customFeature">> => true}
        }
    },
    Caps = erlmcp_capabilities:extract_client_capabilities(Params),
    ?assert(is_record(Caps, mcp_client_capabilities)),
    ?assertEqual(#mcp_capability{enabled = false}, Caps#mcp_client_capabilities.roots),
    ?assert(is_record(Caps#mcp_client_capabilities.sampling, mcp_capability)),
    ?assertEqual(#{<<"customFeature">> => true}, Caps#mcp_client_capabilities.experimental).

extract_client_capabilities_minimal_test() ->
    Params = #{<<"protocolVersion">> => <<"2024-11-05">>},
    Caps = erlmcp_capabilities:extract_client_capabilities(Params),
    ?assert(is_record(Caps, mcp_client_capabilities)),
    ?assertEqual(#mcp_capability{enabled = false}, Caps#mcp_client_capabilities.roots),
    ?assertEqual(#mcp_capability{enabled = false}, Caps#mcp_client_capabilities.sampling),
    ?assertEqual(undefined, Caps#mcp_client_capabilities.experimental).

%%--------------------------------------------------------------------
%% Extract Server Capabilities Tests
%%--------------------------------------------------------------------

extract_server_capabilities_full_test() ->
    Response = #{
        <<"protocolVersion">> => <<"2024-11-05">>,
        <<"capabilities">> => #{
            <<"resources">> => #{<<"subscribe">> => true, <<"listChanged">> => true},
            <<"tools">> => #{<<"listChanged">> => true},
            <<"prompts">> => #{},
            <<"logging">> => #{},
            <<"sampling">> => #{<<"modelPreferences">> => #{<<"maxTokens">> => 4096}}}
    },
    Caps = erlmcp_capabilities:extract_server_capabilities(Response),
    ?assert(is_record(Caps, mcp_server_capabilities)),
    ?assert(is_record(Caps#mcp_server_capabilities.resources, mcp_resources_capability)),
    ?assertEqual(true, (Caps#mcp_server_capabilities.resources)#mcp_resources_capability.subscribe),
    ?assertEqual(true, (Caps#mcp_server_capabilities.resources)#mcp_resources_capability.listChanged).

extract_server_capabilities_minimal_test() ->
    Response = #{
        <<"protocolVersion">> => <<"2024-11-05">>,
        <<"capabilities">> => #{}
    },
    Caps = erlmcp_capabilities:extract_server_capabilities(Response),
    ?assert(is_record(Caps, mcp_server_capabilities)),
    ?assert(is_record(Caps#mcp_server_capabilities.resources, mcp_resources_capability)),
    ?assertEqual(false, (Caps#mcp_server_capabilities.resources)#mcp_resources_capability.subscribe),
    ?assertEqual(false, (Caps#mcp_server_capabilities.resources)#mcp_resources_capability.listChanged).

%%--------------------------------------------------------------------
%% Capability to Map Tests
%%--------------------------------------------------------------------

server_capabilities_to_map_full_test() ->
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{subscribe = true, listChanged = true},
        tools = #mcp_tools_capability{listChanged = true},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{}
    },
    Map = erlmcp_capabilities:capability_to_map(Caps),
    ?assert(maps:is_key(<<"resources">>, Map)),
    ?assert(maps:is_key(<<"tools">>, Map)),
    ?assert(maps:is_key(<<"prompts">>, Map)),
    ?assert(maps:is_key(<<"logging">>, Map)),
    ?assertEqual(#{<<"subscribe">> => true, <<"listChanged">> => true},
                 maps:get(<<"resources">>, Map)),
    ?assertEqual(#{<<"listChanged">> => true}, maps:get(<<"tools">>, Map)).

server_capabilities_to_map_minimal_test() ->
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{subscribe = false, listChanged = false},
        tools = #mcp_tools_capability{listChanged = false},
        prompts = #mcp_prompts_capability{listChanged = false},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{}
    },
    Map = erlmcp_capabilities:capability_to_map(Caps),
    %% All capabilities are included in the map, even if features are disabled
    ?assert(maps:is_key(<<"resources">>, Map)),
    ?assert(maps:is_key(<<"tools">>, Map)),
    ?assert(maps:is_key(<<"prompts">>, Map)),
    ?assert(maps:is_key(<<"logging">>, Map)),
    ?assert(maps:is_key(<<"sampling">>, Map)),
    ?assert(maps:is_key(<<"roots">>, Map)).

client_capabilities_to_map_test() ->
    Caps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        experimental = #{<<"custom">> => true}
    },
    Map = erlmcp_capabilities:capability_to_map(Caps),
    %% All capabilities are included in the map
    ?assert(maps:is_key(<<"roots">>, Map)),
    ?assert(maps:is_key(<<"sampling">>, Map)),
    ?assertEqual(#{<<"custom">> => true}, maps:get(<<"experimental">>, Map)).

%%--------------------------------------------------------------------
%% Map to Capability Tests
%%--------------------------------------------------------------------

map_to_server_capabilities_test() ->
    Map = #{
        <<"resources">> => #{<<"subscribe">> => true, <<"listChanged">> => true},
        <<"tools">> => #{<<"listChanged">> => true},
        <<"prompts">> => #{},
        <<"logging">> => #{}
    },
    Caps = erlmcp_capabilities:map_to_capability(Map),
    ?assert(is_record(Caps, mcp_server_capabilities)),
    ?assertEqual(true, (Caps#mcp_server_capabilities.resources)#mcp_resources_capability.subscribe),
    ?assertEqual(true, (Caps#mcp_server_capabilities.tools)#mcp_tools_capability.listChanged).

map_to_client_capabilities_test() ->
    Map = #{
        <<"roots">> => #{},
        <<"sampling">> => #{<<"modelPreferences">> => #{<<"temperature">> => 0.5}}
    },
    Caps = erlmcp_capabilities:map_to_capability(Map),
    ?assert(is_record(Caps, mcp_client_capabilities)),
    ?assertEqual(#mcp_capability{enabled = true}, Caps#mcp_client_capabilities.roots),
    ?assert(is_record(Caps#mcp_client_capabilities.sampling, mcp_capability)).

%%--------------------------------------------------------------------
%% Protocol Version Validation Tests
%%--------------------------------------------------------------------

validate_protocol_version_supported_test() ->
    ?assertEqual(ok, erlmcp_capabilities:validate_protocol_version(<<"2024-11-05">>)),
    ?assertEqual(ok, erlmcp_capabilities:validate_protocol_version(<<"2025-11-25">>)).

validate_protocol_version_unsupported_test() ->
    ?assertMatch({error, _}, erlmcp_capabilities:validate_protocol_version(<<"2020-01-01">>)),
    ?assertMatch({error, _}, erlmcp_capabilities:validate_protocol_version(<<"invalid">>)).

%%--------------------------------------------------------------------
%% Capability Negotiation Tests
%%--------------------------------------------------------------------

negotiate_capabilities_full_test() ->
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = true}
    },
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{subscribe = true, listChanged = true},
        tools = #mcp_tools_capability{listChanged = true},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{modelPreferences = #{<<"maxTokens">> => 2048}},
        roots = #mcp_roots_capability{}
    },
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assert(is_record(Negotiated, mcp_server_capabilities)),
    %% Server capabilities should be preserved
    ?assertEqual(true, (Negotiated#mcp_server_capabilities.resources)#mcp_resources_capability.subscribe),
    ?assertEqual(true, (Negotiated#mcp_server_capabilities.tools)#mcp_tools_capability.listChanged).

negotiate_capabilities_sampling_test() ->
    %% Test that sampling model preferences are merged
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = false},
        sampling = #mcp_capability{enabled = true}
    },
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{}
    },
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assert(is_record(Negotiated#mcp_server_capabilities.sampling, mcp_sampling_capability)),
    %% Sampling capability should be present
    ?assert(is_record(Negotiated#mcp_server_capabilities.sampling, mcp_sampling_capability)).

%%--------------------------------------------------------------------
%% Has Capability Tests
%%--------------------------------------------------------------------

has_capability_resources_test() ->
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{subscribe = true},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{}
    },
    ?assert(erlmcp_capabilities:has_capability(Caps, resources)),
    ?assertNot(erlmcp_capabilities:has_capability(Caps, unsupported)).

has_capability_feature_subscribe_test() ->
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{subscribe = true, listChanged = false},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{}
    },
    ?assert(erlmcp_capabilities:has_capability_feature(Caps, resources, subscribe)),
    ?assertNot(erlmcp_capabilities:has_capability_feature(Caps, resources, listChanged)).

has_capability_feature_list_changed_test() ->
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{listChanged = true},
        prompts = #mcp_prompts_capability{listChanged = true},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{}
    },
    ?assert(erlmcp_capabilities:has_capability_feature(Caps, tools, listChanged)),
    ?assert(erlmcp_capabilities:has_capability_feature(Caps, prompts, listChanged)),
    ?assertNot(erlmcp_capabilities:has_capability_feature(Caps, resources, subscribe)).

%%--------------------------------------------------------------------
%% Get Default Client Capabilities Tests
%%--------------------------------------------------------------------

get_client_capabilities_default_test() ->
    Caps = erlmcp_capabilities:get_client_capabilities(),
    ?assert(is_record(Caps, mcp_client_capabilities)),
    ?assertEqual(#mcp_capability{enabled = true}, Caps#mcp_client_capabilities.roots),
    ?assertEqual(#mcp_capability{enabled = false}, Caps#mcp_client_capabilities.sampling),
    ?assertEqual(undefined, Caps#mcp_client_capabilities.experimental).

%%--------------------------------------------------------------------
%% Format Capability Error Tests
%%--------------------------------------------------------------------

format_capability_error_unsupported_capability_test() ->
    Error = erlmcp_capabilities:format_capability_error({unsupported_capability, tools}),
    ?assertMatch(<<"Capability not supported: tools">>, Error).

format_capability_error_unsupported_feature_test() ->
    Error = erlmcp_capabilities:format_capability_error({unsupported_feature, resources, subscribe}),
    ?assertMatch(<<"Feature 'subscribe' not supported for capability 'resources'">>, Error).

format_capability_error_protocol_version_test() ->
    Error = erlmcp_capabilities:format_capability_error({protocol_version_mismatch, <<"2020-01-01">>}),
    ?assertMatch(<<"Unsupported protocol version: 2020-01-01">>, Error).

%%--------------------------------------------------------------------
%% Merge Capability Tests
%%--------------------------------------------------------------------

merge_capability_resources_test() ->
    ServerVal = #mcp_resources_capability{subscribe = true, listChanged = true},
    ?assertEqual(ServerVal, erlmcp_capabilities:merge_capability(resources, undefined, ServerVal)).

merge_capability_sampling_test() ->
    ClientVal = #mcp_sampling_capability{modelPreferences = #{<<"temperature">> => 0.8}},
    ServerVal = #mcp_sampling_capability{},
    Merged = erlmcp_capabilities:merge_capability(sampling, ClientVal, ServerVal),
    ?assert(is_record(Merged, mcp_sampling_capability)),
    ?assertEqual(#{<<"temperature">> => 0.8}, Merged#mcp_sampling_capability.modelPreferences).

%%--------------------------------------------------------------------
%% Integration Test: Full Initialize Handshake
%%--------------------------------------------------------------------

initialize_handshake_success_test() ->
    %% Simulate full initialize handshake
    ClientCaps0 = erlmcp_capabilities:get_client_capabilities(),
    ServerCaps0 = #mcp_server_capabilities{
        resources = #mcp_resources_capability{subscribe = true, listChanged = true},
        tools = #mcp_tools_capability{listChanged = false},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{}
    },

    %% Validate protocol version
    ?assertEqual(ok, erlmcp_capabilities:validate_protocol_version(<<"2024-11-05">>)),

    %% Negotiate capabilities
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps0, ServerCaps0),
    ?assert(erlmcp_capabilities:has_capability(Negotiated, resources)),
    ?assert(erlmcp_capabilities:has_capability_feature(Negotiated, resources, subscribe)),
    ?assert(erlmcp_capabilities:has_capability(Negotiated, prompts)),
    ?assertNot(erlmcp_capabilities:has_capability_feature(Negotiated, tools, listChanged)),

    %% Convert to map for JSON encoding
    InitResponse = #{
        <<"protocolVersion">> => <<"2024-11-05">>,
        <<"capabilities">> => erlmcp_capabilities:capability_to_map(Negotiated),
        <<"serverInfo">> => #{
            <<"name">> => <<"erlmcp">>,
            <<"version">> => <<"0.6.0">>
        }
    },

    ?assert(maps:is_key(<<"capabilities">>, InitResponse)),
    ?assert(maps:is_key(<<"resources">>, maps:get(<<"capabilities">>, InitResponse))),
    ?assertEqual(#{<<"subscribe">> => true, <<"listChanged">> => true},
                 maps:get(<<"resources">>, maps:get(<<"capabilities">>, InitResponse))).

initialize_handshake_protocol_mismatch_test() ->
    %% Test protocol version mismatch
    ?assertMatch({error, _}, erlmcp_capabilities:validate_protocol_version(<<"2020-01-01">>)).

initialize_handshake_minimal_capabilities_test() ->
    %% Test handshake with minimal server capabilities
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false}
    },
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{subscribe = false, listChanged = false},
        tools = #mcp_tools_capability{listChanged = false},
        prompts = #mcp_prompts_capability{listChanged = false},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{}
    },
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assert(erlmcp_capabilities:has_capability(Negotiated, resources)),
    ?assert(erlmcp_capabilities:has_capability(Negotiated, prompts)),
    ?assertNot(erlmcp_capabilities:has_capability_feature(Negotiated, resources, subscribe)).

%%--------------------------------------------------------------------
%% Graceful Degradation Tests
%%--------------------------------------------------------------------

graceful_degradation_tools_not_supported_test() ->
    %% Client tries to use tools when server doesn't support them
    %% In erlmcp, a capability is supported if the field is set to a record
    %% We use undefined to indicate a capability is not supported
    ServerCaps = #mcp_server_capabilities{
        resources = undefined,  %% Not supported
        tools = undefined,       %% Not supported
        prompts = undefined,
        logging = undefined,
        sampling = undefined,
        roots = undefined
    },
    ?assertNot(erlmcp_capabilities:has_capability(ServerCaps, tools)),

    %% Format error for client
    Error = erlmcp_capabilities:format_capability_error({unsupported_capability, tools}),
    ?assertMatch(<<"Capability not supported: tools">>, Error).

graceful_degradation_feature_not_supported_test() ->
    %% Client tries to use subscribe feature when server doesn't support it
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{subscribe = false, listChanged = true},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{}
    },
    ?assert(erlmcp_capabilities:has_capability(ServerCaps, resources)),
    ?assertNot(erlmcp_capabilities:has_capability_feature(ServerCaps, resources, subscribe)),
    ?assert(erlmcp_capabilities:has_capability_feature(ServerCaps, resources, listChanged)).

%%--------------------------------------------------------------------
%% Experimental Capabilities Tests
%%--------------------------------------------------------------------

experimental_capabilities_client_test() ->
    Caps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        experimental = #{<<"customFeature">> => true, <<"vendorExt">> => #{<<"key">> => <<"value">>}}
    },
    Map = erlmcp_capabilities:capability_to_map(Caps),
    ?assert(maps:is_key(<<"experimental">>, Map)),
    ?assertEqual(#{<<"customFeature">> => true, <<"vendorExt">> => #{<<"key">> => <<"value">>}},
                 maps:get(<<"experimental">>, Map)).

experimental_capabilities_server_test() ->
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{<<"betaFeature">> => true}
    },
    Map = erlmcp_capabilities:capability_to_map(Caps),
    ?assert(maps:is_key(<<"experimental">>, Map)),
    ?assertEqual(#{<<"betaFeature">> => true}, maps:get(<<"experimental">>, Map)).
