-module(erlmcp_capabilities_tests).
-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Chicago School TDD Tests for erlmcp_capabilities
%%%
%%% Philosophy:
%%% - Use REAL gen_server processes (no mocks)
%%% - Verify OBSERVABLE STATE (API results, not internals)
%%% - Test BEHAVIOR (what system does, not how)
%%% - Test ERROR PATHS and EDGE CASES
%%%-------------------------------------------------------------------

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

capabilities_test_() ->
    {setup,
     fun setup_capabilities/0,
     fun cleanup_capabilities/1,
     fun tests/1}.

setup_capabilities() ->
    % Start real capabilities gen_server
    {ok, Pid} = erlmcp_capabilities:start_link(),
    Pid.

cleanup_capabilities(_Pid) ->
    % Stop real capabilities gen_server
    gen_server:stop(erlmcp_capabilities),
    undefined = whereis(erlmcp_capabilities).

tests(_Pid) ->
    [
     {"Extract client capabilities from params", fun extract_client_capabilities_basic/0},
     {"Extract server capabilities from params", fun extract_server_capabilities_basic/0},
     {"Capability roundtrip (map <-> record)", fun capability_roundtrip/0},
     {"Validate protocol version (valid)", fun validate_protocol_version_valid/0},
     {"Validate protocol version (invalid)", fun validate_protocol_version_invalid/0},
     {"Negotiate capabilities (client + server)", fun negotiate_capabilities_basic/0},
     {"Merge capability with defaults", fun merge_capability_defaults/0},
     {"Check has_capability (roots)", fun has_capability_roots/0},
     {"Check has_capability_feature (listChanged)", fun has_capability_feature_list_changed/0},
     {"Format capability error", fun format_capability_error/0},
     {"Validate client capability record (valid)", fun validate_client_capability_record_valid/0},
     {"Validate server capability record (valid)", fun validate_server_capability_record_valid/0},
     {"Validate model preferences (valid)", fun validate_model_preferences_valid/0},
     {"Supports elicitation (experimental)", fun supports_elicitation/0},
     {"Supports tasks (experimental)", fun supports_tasks/0},
     {"Supports streaming (experimental)", fun supports_streaming/0},
     {"Supports progress (experimental)", fun supports_progress/0},
     {"Get required capabilities", fun get_required_capabilities/0},
     {"Get optional capabilities", fun get_optional_capabilities/0},
     {"Is capability required (roots)", fun is_capability_required_roots/0},
     {"Is capability optional (sampling)", fun is_capability_optional_sampling/0},
     {"Get capability dependencies", fun get_capability_dependencies/0},
     {"Validate capability dependencies (valid)", fun validate_capability_dependencies_valid/0},
     {"Get negotiated capabilities", fun get_negotiated_capabilities/0},
     {"Set negotiated capabilities", fun set_negotiated_capabilities/0},
     {"Reset negotiated capabilities", fun reset_negotiated_capabilities/0},
     {"Get capability flags", fun get_capability_flags/0},
     {"Set capability flag", fun set_capability_flag/0},
     {"Get capability description", fun get_capability_description/0},
     {"Get feature description", fun get_feature_description/0},
     {"Build client init params", fun build_client_init_params/0},
     {"Build server init response", fun build_server_init_response/0},
     {"Client supports tools list changed", fun client_supports_tools_list_changed/0}
    ].

%%%===================================================================
%%% Unit Tests (Chicago School: Real State Verification)
%%%===================================================================

%% @doc Extract client capabilities from initialize params
extract_client_capabilities_basic() ->
    % Setup: Real initialize params
    Params = #{
        <<"protocolVersion">> => <<"2024-11-05">>,
        <<"capabilities">> => #{
            <<"roots">> => #{
                <<"listChanged">> => true
            },
            <<"sampling">> => #{
                <<"?>> => 1.0
            }
        },
        <<"clientInfo">> => #{
            <<"name">> => <<"test-client">>,
            <<"version">> => <<"1.0.0">>
        }
    },

    % Exercise: Extract via API
    ClientCaps = erlmcp_capabilities:extract_client_capabilities(Params),

    % Verify: Observable state (record fields)
    ?assertMatch(#mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = true}
    }, ClientCaps),

    % Verify roots has listChanged flag
    #mcp_client_capabilities{roots = RootsCap} = ClientCaps,
    ?assert(maps:get(<<"listChanged">>, RootsCap#mcp_capability.flags, false)).

%% @doc Extract server capabilities from initialize result
extract_server_capabilities_basic() ->
    % Setup: Real initialize result
    Result = #{
        <<"protocolVersion">> => <<"2024-11-05">>,
        <<"capabilities">> => #{
            <<"resources">> => #{
                <<"subscribe">> => true,
                <<"listChanged">> => true
            },
            <<"tools">> => #{
                <<"listChanged">> => true
            }
        },
        <<"serverInfo">> => #{
            <<"name">> => <<"test-server">>,
            <<"version">> => <<"2.1.0">>
        }
    },

    % Exercise: Extract via API
    ServerCaps = erlmcp_capabilities:extract_server_capabilities(Result),

    % Verify: Observable state
    ?assertMatch(#mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true}
    }, ServerCaps),

    % Verify subscribe and listChanged flags
    #mcp_server_capabilities{resources = ResCap} = ServerCaps,
    ?assert(maps:get(<<"subscribe">>, ResCap#mcp_capability.flags, false)),
    ?assert(maps:get(<<"listChanged">>, ResCap#mcp_capability.flags, false)).

%% @doc Capability roundtrip: map -> record -> map
capability_roundtrip() ->
    % Setup: Real capability map
    CapMap = #{
        <<"enabled">> => true,
        <<"listChanged">> => true,
        <<"subscribe">> => false
    },

    % Exercise: Convert to record and back
    CapRecord = erlmcp_capabilities:map_to_capability(CapMap),
    CapMap2 = erlmcp_capabilities:capability_to_map(CapRecord),

    % Verify: Roundtrip preserves data
    ?assertEqual(maps:get(<<"enabled">>, CapMap), maps:get(<<"enabled">>, CapMap2)),
    ?assertEqual(maps:get(<<"listChanged">>, CapMap), maps:get(<<"listChanged">>, CapMap2)).

%% @doc Validate protocol version (valid)
validate_protocol_version_valid() ->
    % Setup: Valid protocol version
    Version = <<"2024-11-05">>,

    % Exercise: Validate via API
    Result = erlmcp_capabilities:validate_protocol_version(Version),

    % Verify: Valid result
    ?assertEqual({ok, Version}, Result).

%% @doc Validate protocol version (invalid)
validate_protocol_version_invalid() ->
    % Setup: Invalid protocol versions
    InvalidVersions = [
        <<"2023-01-01">>,  % Too old
        <<"invalid">>,      % Not a date
        <<>>                % Empty
    ],

    % Exercise & Verify: All rejected
    lists:foreach(fun(Version) ->
        Result = erlmcp_capabilities:validate_protocol_version(Version),
        ?assertMatch({error, _}, Result)
    end, InvalidVersions).

%% @doc Negotiate capabilities between client and server
negotiate_capabilities_basic() ->
    % Setup: Real client and server capabilities
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false}
    },
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true}
    },

    % Exercise: Negotiate via API
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),

    % Verify: Negotiated result
    ?assertMatch(#{client := _, server := _}, Negotiated),
    ?assert(maps:get(roots, maps:get(client, Negotiated))),
    ?assert(maps:get(resources, maps:get(server, Negotiated))).

%% @doc Merge capability with defaults
merge_capability_defaults() ->
    % Setup: Partial capability
    PartialCap = #mcp_capability{
        enabled = true,
        flags = #{}
    },

    % Exercise: Merge with defaults
    MergedCap = erlmcp_capabilities:merge_capability(roots, PartialCap, #{}),

    % Verify: Defaults applied
    ?assertEqual(true, MergedCap#mcp_capability.enabled).

%% @doc Check has_capability
has_capability_roots() ->
    % Setup: Real client capabilities
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false}
    },

    % Exercise: Check via API
    HasRoots = erlmcp_capabilities:has_capability(roots, ClientCaps),
    HasSampling = erlmcp_capabilities:has_capability(sampling, ClientCaps),

    % Verify: Observable results
    ?assertEqual(true, HasRoots),
    ?assertEqual(false, HasSampling).

%% @doc Check has_capability_feature (listChanged)
has_capability_feature_list_changed() ->
    % Setup: Real capability with listChanged flag
    CapWithFlag = #mcp_capability{
        enabled = true,
        flags = #{<<"listChanged">> => true}
    },

    % Exercise: Check feature flag via API
    HasListChanged = erlmcp_capabilities:has_capability_feature(
        tools, listChanged, CapWithFlag
    ),

    % Verify: Observable result
    ?assertEqual(true, HasListChanged).

%% @doc Format capability error
format_capability_error() ->
    % Setup: Various error types
    Errors = [
        {missing_capability, roots},
        {invalid_capability, sampling},
        {version_mismatch, <<"2024-11-05">>}
    ],

    % Exercise & Verify: All formatted
    lists:foreach(fun({ErrorType, Details}) ->
        Formatted = erlmcp_capabilities:format_capability_error({ErrorType, Details}),
        ?assert(is_list(Formatted)),
        ?assert(length(Formatted) > 0)
    end, Errors).

%% @doc Validate client capability record (valid)
validate_client_capability_record_valid() ->
    % Setup: Valid client capabilities
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_capability{enabled = true}
    },

    % Exercise: Validate via API
    Result = erlmcp_capabilities:validate_client_capability_record(ClientCaps),

    % Verify: Valid result
    ?assertEqual(ok, Result).

%% @doc Validate server capability record (valid)
validate_server_capability_record_valid() ->
    % Setup: Valid server capabilities
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = false}
    },

    % Exercise: Validate via API
    Result = erlmcp_capabilities:validate_server_capability_record(ServerCaps),

    % Verify: Valid result
    ?assertEqual(ok, Result).

%% @doc Validate model preferences (valid)
validate_model_preferences_valid() ->
    % Setup: Valid model preferences
    ModelPrefs = #{
        <<"maxTokens">> => 4096,
        <<"temperature">> => 0.7,
        <<"topP">> => 0.9
    },

    % Exercise: Validate via API
    Result = erlmcp_capabilities:validate_model_preferences(ModelPrefs),

    % Verify: Valid result
    ?assertEqual(ok, Result).

%% @doc Supports elicitation (experimental)
supports_elicitation() ->
    % Setup: Client capabilities with experimental elicitation
    ClientCaps = #mcp_client_capabilities{
        experimental = #{
            <<"elicitation">> => true
        }
    },

    % Exercise: Check via API
    Supports = erlmcp_capabilities:supports_elicitation(ClientCaps),

    % Verify: Observable result
    ?assertEqual(true, Supports).

%% @doc Supports tasks (experimental)
supports_tasks() ->
    % Setup: Client capabilities with experimental tasks
    ClientCaps = #mcp_client_capabilities{
        experimental = #{
            <<"tasks">> => true
        }
    },

    % Exercise: Check via API
    Supports = erlmcp_capabilities:supports_tasks(ClientCaps),

    % Verify: Observable result
    ?assertEqual(true, Supports).

%% @doc Supports streaming (experimental)
supports_streaming() ->
    % Setup: Client capabilities with streaming
    ClientCaps = #mcp_client_capabilities{
        experimental = #{
            <<"streaming">> => true
        }
    },

    % Exercise: Check via API
    Supports = erlmcp_capabilities:supports_streaming(ClientCaps),

    % Verify: Observable result
    ?assertEqual(true, Supports).

%% @doc Supports progress (experimental)
supports_progress() ->
    % Setup: Client capabilities with progress
    ClientCaps = #mcp_client_capabilities{
        experimental = #{
            <<"progress">> => true
        }
    },

    % Exercise: Check via API
    Supports = erlmcp_capabilities:supports_progress(ClientCaps),

    % Verify: Observable result
    ?assertEqual(true, Supports).

%% @doc Get required capabilities
get_required_capabilities() ->
    % Exercise: Get required capabilities
    Required = erlmcp_capabilities:get_required_capabilities(),

    % Verify: At least roots is required
    ?assert(lists:member(roots, Required)).

%% @doc Get optional capabilities
get_optional_capabilities() ->
    % Exercise: Get optional capabilities
    Optional = erlmcp_capabilities:get_optional_capabilities(),

    % Verify: Sampling is optional
    ?assert(lists:member(sampling, Optional)).

%% @doc Is capability required (roots)
is_capability_required_roots() ->
    % Exercise: Check if roots is required
    IsRequired = erlmcp_capabilities:is_capability_required(roots),

    % Verify: Roots is required
    ?assertEqual(true, IsRequired).

%% @doc Is capability optional (sampling)
is_capability_optional_sampling() ->
    % Exercise: Check if sampling is optional
    IsOptional = erlmcp_capabilities:is_capability_optional(sampling),

    % Verify: Sampling is optional
    ?assertEqual(true, IsOptional).

%% @doc Get capability dependencies
get_capability_dependencies() ->
    % Exercise: Get dependencies for resources
    Deps = erlmcp_capabilities:get_capability_dependencies(resources),

    % Verify: Dependencies list
    ?assert(is_list(Deps)).

%% @doc Validate capability dependencies (valid)
validate_capability_dependencies_valid() ->
    % Setup: Capabilities with dependencies satisfied
    Caps = #{roots => true, resources => true},

    % Exercise: Validate
    Result = erlmcp_capabilities:validate_capability_dependencies(Caps),

    % Verify: Valid result
    ?assertEqual(ok, Result).

%% @doc Get negotiated capabilities
get_negotiated_capabilities() ->
    % Setup: Set negotiated capabilities first
    Negotiated = #{roots => true, sampling => false},
    erlmcp_capabilities:set_negotiated_capabilities(Negotiated),

    % Exercise: Get negotiated capabilities
    Result = erlmcp_capabilities:get_negotiated_capabilities(),

    % Verify: Retrieved matches set
    ?assertEqual(Negotiated, Result),

    % Cleanup: Reset
    erlmcp_capabilities:reset_negotiated_capabilities().

%% @doc Set negotiated capabilities
set_negotiated_capabilities() ->
    % Setup: Negotiated capabilities
    Negotiated = #{tools => true, prompts => false},

    % Exercise: Set via API
    Result = erlmcp_capabilities:set_negotiated_capabilities(Negotiated),

    % Verify: Set succeeded
    ?assertEqual(ok, Result),

    % Cleanup: Reset
    erlmcp_capabilities:reset_negotiated_capabilities().

%% @doc Reset negotiated capabilities
reset_negotiated_capabilities() ->
    % Setup: Set some capabilities first
    Negotiated = #{roots => true},
    erlmcp_capabilities:set_negotiated_capabilities(Negotiated),

    % Exercise: Reset via API
    erlmcp_capabilities:reset_negotiated_capabilities(),

    % Verify: Cleared
    Result = erlmcp_capabilities:get_negotiated_capabilities(),
    ?assertEqual(#{}, Result).

%% @doc Get capability flags
get_capability_flags() ->
    % Setup: Set capability flag first
    erlmcp_capabilities:set_capability_flag(roots, listChanged, true, client),

    % Exercise: Get flags
    Flags = erlmcp_capabilities:get_capability_flags(roots, client),

    % Verify: Flag present
    ?assert(maps:get(<<"listChanged">>, Flags, false)).

%% @doc Set capability flag
set_capability_flag() ->
    % Exercise: Set flag via API
    Result = erlmcp_capabilities:set_capability_flag(
        tools, subscribe, true, server
    ),

    % Verify: Set succeeded
    ?assertEqual(ok, Result).

%% @doc Get capability description
get_capability_description() ->
    % Exercise: Get description for roots
    Desc = erlmcp_capabilities:get_capability_description(roots),

    % Verify: Description is a binary
    ?assert(is_binary(Desc)),
    ?assert(byte_size(Desc) > 0).

%% @doc Get feature description
get_feature_description() ->
    % Exercise: Get description for listChanged feature
    Desc = erlmcp_capabilities:get_feature_description(roots, listChanged),

    % Verify: Description is a binary
    ?assert(is_binary(Desc)),
    ?assert(byte_size(Desc) > 0).

%% @doc Build client init params
build_client_init_params() ->
    % Setup: Client capabilities
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true}
    },

    % Exercise: Build params
    Params = erlmcp_capabilities:build_client_init_params(ClientCaps),

    % Verify: Params structure
    ?assertMatch(#{<<"protocolVersion">> := _, <<"capabilities">> := _}, Params).

%% @doc Build server init response
build_server_init_response() ->
    % Setup: Server capabilities
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true}
    },

    % Exercise: Build response
    Response = erlmcp_capabilities:build_server_init_response(
        <<"2024-11-05">>, ServerCaps
    ),

    % Verify: Response structure
    ?assertMatch(#{<<"protocolVersion">> := _, <<"capabilities">> := _}, Response).

%% @doc Client supports tools list changed
client_supports_tools_list_changed() ->
    % Setup: Client capabilities with listChanged
    ClientCaps = #mcp_client_capabilities{
        tools = #mcp_capability{
            enabled = true,
            flags = #{<<"listChanged">> => true}
        }
    },

    % Exercise: Check via API
    Supports = erlmcp_capabilities:client_supports_tools_list_changed(ClientCaps),

    % Verify: Observable result
    ?assertEqual(true, Supports).

%%%===================================================================
%%% Property-Based Tests (Proper)
%%%===================================================================

%% @doc Capability map roundtrip preserves data
prop_capability_roundtrip() ->
    ?FORALL(CapMap, capability_map(),
        begin
            CapRecord = erlmcp_capabilities:map_to_capability(CapMap),
            CapMap2 = erlmcp_capabilities:capability_to_map(CapRecord),

            % Enabled flag must be preserved
            maps:get(<<"enabled">>, CapMap) =:= maps:get(<<"enabled">>, CapMap2)
        end
    ).

%%%===================================================================
%%% Generators
%%%===================================================================

capability_map() ->
    ?LET(Enabled, bool(),
        #{
            <<"enabled">> => Enabled,
            <<"listChanged">> => bool(),
            <<"subscribe">> => bool()
        }).
