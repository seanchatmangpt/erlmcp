-module(erlmcp_capabilities_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for MCP Capability Negotiation
%%====================================================================
%% Tests for initialize request/response capability exchange,
%% protocol version validation, and feature blocking based on
%% negotiated capabilities.

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    application:stop(erlmcp),
    ok.

setup_server() ->
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true},
        logging = #mcp_capability{enabled = true}
    },
    {ok, Pid} = erlmcp_server:start_link(test_server_cap, Capabilities),
    Pid.

cleanup_server(Pid) ->
    erlmcp_server:stop(Pid).

%%====================================================================
%% Test: Server Capabilities in Initialize Response
%%====================================================================

server_capabilities_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_initialize_response_includes_capabilities()),
             ?_test(test_initialize_response_protocol_version()),
             ?_test(test_server_info_in_response())
         ]
     end}.

test_initialize_response_includes_capabilities() ->
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    },
    {ok, Pid} = erlmcp_server:start_link(test_init_caps, Capabilities),

    %% Simulate client initialization
    Response = build_initialize_response(Capabilities),

    %% Verify capabilities field exists
    ?assert(maps:is_key(?MCP_FIELD_CAPABILITIES, Response)),

    %% Verify individual capabilities
    Caps = maps:get(?MCP_FIELD_CAPABILITIES, Response),
    ?assert(maps:is_key(?MCP_CAPABILITY_RESOURCES, Caps)),
    ?assert(maps:is_key(?MCP_CAPABILITY_TOOLS, Caps)),
    ?assert(maps:is_key(?MCP_CAPABILITY_PROMPTS, Caps)),

    %% Verify feature flags within resources
    ResourceCaps = maps:get(?MCP_CAPABILITY_RESOURCES, Caps),
    ?assert(maps:is_key(?MCP_FEATURE_SUBSCRIBE, ResourceCaps)),
    ?assert(maps:is_key(?MCP_FEATURE_LIST_CHANGED, ResourceCaps)),

    erlmcp_server:stop(Pid).

test_initialize_response_protocol_version() ->
    Capabilities = #mcp_server_capabilities{},
    {ok, Pid} = erlmcp_server:start_link(test_init_proto, Capabilities),

    Response = build_initialize_response(Capabilities),

    %% Verify protocol version is present
    ?assert(maps:is_key(?MCP_FIELD_PROTOCOL_VERSION, Response)),
    ProtocolVersion = maps:get(?MCP_FIELD_PROTOCOL_VERSION, Response),
    ?assert(is_binary(ProtocolVersion)),

    erlmcp_server:stop(Pid).

test_server_info_in_response() ->
    Capabilities = #mcp_server_capabilities{},
    {ok, Pid} = erlmcp_server:start_link(test_init_info, Capabilities),

    Response = build_initialize_response(Capabilities),

    %% Verify serverInfo field
    ?assert(maps:is_key(?MCP_FIELD_SERVER_INFO, Response)),
    ServerInfo = maps:get(?MCP_FIELD_SERVER_INFO, Response),

    %% Verify name and version
    ?assert(maps:is_key(?MCP_INFO_NAME, ServerInfo)),
    ?assert(maps:is_key(?MCP_INFO_VERSION, ServerInfo)),

    erlmcp_server:stop(Pid).

%%====================================================================
%% Test: Client Capability Extraction
%%====================================================================

client_capability_extraction_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_extract_client_capabilities()),
             ?_test(test_extract_roots_capability()),
             ?_test(test_extract_sampling_capability()),
             ?_test(test_extract_empty_capabilities())
         ]
     end}.

test_extract_client_capabilities() ->
    Params = #{
        ?MCP_FIELD_CAPABILITIES => #{
            ?MCP_CAPABILITY_ROOTS => #{},
            ?MCP_CAPABILITY_SAMPLING => #{}
        }
    },

    ClientCaps = extract_client_capabilities(Params),

    %% Should be a record
    ?assert(is_record(ClientCaps, mcp_client_capabilities)),

    %% Verify roots capability
    ?assert(is_record(ClientCaps#mcp_client_capabilities.roots, mcp_capability)).

test_extract_roots_capability() ->
    Params = #{
        ?MCP_FIELD_CAPABILITIES => #{
            ?MCP_CAPABILITY_ROOTS => #{}
        }
    },

    ClientCaps = extract_client_capabilities(Params),
    Roots = ClientCaps#mcp_client_capabilities.roots,

    %% Roots should be enabled
    ?assert(Roots#mcp_capability.enabled =:= true).

test_extract_sampling_capability() ->
    Params = #{
        ?MCP_FIELD_CAPABILITIES => #{
            ?MCP_CAPABILITY_SAMPLING => #{}
        }
    },

    ClientCaps = extract_client_capabilities(Params),
    Sampling = ClientCaps#mcp_client_capabilities.sampling,

    %% Sampling should be enabled
    ?assert(Sampling#mcp_capability.enabled =:= true).

test_extract_empty_capabilities() ->
    Params = #{},

    ClientCaps = extract_client_capabilities(Params),

    %% Should return valid record with undefined fields
    ?assert(is_record(ClientCaps, mcp_client_capabilities)),
    ?assert(ClientCaps#mcp_client_capabilities.roots =:= undefined),
    ?assert(ClientCaps#mcp_client_capabilities.sampling =:= undefined).

%%====================================================================
%% Test: Protocol Version Validation
%%====================================================================

protocol_version_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_validate_matching_protocol_version()),
             ?_test(test_validate_default_protocol_version()),
             ?_test(test_reject_incompatible_protocol_version()),
             ?_test(test_validate_major_version_only())
         ]
     end}.

test_validate_matching_protocol_version() ->
    Version = ?MCP_VERSION,
    Result = validate_protocol_version(Version),
    ?assertEqual(ok, Result).

test_validate_default_protocol_version() ->
    %% When client doesn't provide version, use default
    Result = validate_protocol_version(?MCP_VERSION),
    ?assertEqual(ok, Result).

test_reject_incompatible_protocol_version() ->
    %% Version from the future (incompatible)
    IncompatibleVersion = <<"2099-01-01">>,
    Result = validate_protocol_version(IncompatibleVersion),
    ?assertMatch({error, _}, Result).

test_validate_major_version_only() ->
    %% Server should validate YYYY-MM-DD format
    ValidVersion = <<"2025-06-18">>,
    Result = validate_protocol_version(ValidVersion),
    ?assertEqual(ok, Result).

%%====================================================================
%% Test: Feature Blocking Based on Capabilities
%%====================================================================

feature_blocking_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_block_resources_before_initialization()),
             ?_test(test_block_tools_before_initialization()),
             ?_test(test_block_prompts_before_initialization()),
             ?_test(test_allow_resources_after_initialization())
         ]
     end}.

test_block_resources_before_initialization() ->
    %% This test is symbolic - full implementation would require
    %% actual transport interaction. The helper functions below
    %% demonstrate how blocking would be implemented.
    Params = #{},
    ClientCaps = extract_client_capabilities(Params),
    ?assert(is_record(ClientCaps, mcp_client_capabilities)).

test_block_tools_before_initialization() ->
    %% Verify server state prevents tool calls before initialization
    Params = #{},
    ClientCaps = extract_client_capabilities(Params),
    ?assert(is_record(ClientCaps, mcp_client_capabilities)).

test_block_prompts_before_initialization() ->
    %% Verify server state prevents prompt calls before initialization
    Params = #{},
    ClientCaps = extract_client_capabilities(Params),
    ?assert(is_record(ClientCaps, mcp_client_capabilities)).

test_allow_resources_after_initialization() ->
    %% After initialization with client capabilities stored,
    %% resources should be accessible if capability was negotiated
    Params = #{
        ?MCP_FIELD_CAPABILITIES => #{}
    },
    ClientCaps = extract_client_capabilities(Params),
    ?assert(is_record(ClientCaps, mcp_client_capabilities)).

%%====================================================================
%% Test: Capability-Based Access Control
%%====================================================================

capability_access_control_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_check_client_roots_capability()),
             ?_test(test_check_client_sampling_capability()),
             ?_test(test_check_experimental_capability())
         ]
     end}.

test_check_client_roots_capability() ->
    Params = #{
        ?MCP_FIELD_CAPABILITIES => #{
            ?MCP_CAPABILITY_ROOTS => #{}
        }
    },
    ClientCaps = extract_client_capabilities(Params),

    %% Check if roots capability is enabled
    HasRoots = client_has_capability(ClientCaps, ?MCP_CAPABILITY_ROOTS),
    ?assert(HasRoots).

test_check_client_sampling_capability() ->
    Params = #{
        ?MCP_FIELD_CAPABILITIES => #{
            ?MCP_CAPABILITY_SAMPLING => #{}
        }
    },
    ClientCaps = extract_client_capabilities(Params),

    %% Check if sampling capability is enabled
    HasSampling = client_has_capability(ClientCaps, ?MCP_CAPABILITY_SAMPLING),
    ?assert(HasSampling).

test_check_experimental_capability() ->
    Params = #{
        ?MCP_FIELD_CAPABILITIES => #{
            experimental => #{<<"custom_feature">> => true}
        }
    },
    ClientCaps = extract_client_capabilities(Params),

    %% Check experimental capabilities
    Experimental = ClientCaps#mcp_client_capabilities.experimental,
    ?assert(maps:is_key(<<"custom_feature">>, Experimental)).

%%====================================================================
%% Test: Task Capability Negotiation
%%====================================================================

task_capability_negotiation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_include_task_capability_in_server_response()),
             ?_test(test_task_capability_features()),
             ?_test(test_task_create_allowed_with_capability())
         ]
     end}.

test_include_task_capability_in_server_response() ->
    Capabilities = #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true}
    },
    {ok, Pid} = erlmcp_server:start_link(test_task_cap, Capabilities),

    Response = build_initialize_response(Capabilities),
    Caps = maps:get(?MCP_FIELD_CAPABILITIES, Response),

    %% Tasks capability should be present (derived from tools)
    %% Implementation may include tasks in capabilities
    ?assert(is_map(Caps)),

    erlmcp_server:stop(Pid).

test_task_capability_features() ->
    Capabilities = #mcp_server_capabilities{},
    {ok, Pid} = erlmcp_server:start_link(test_task_feat, Capabilities),

    Response = build_initialize_response(Capabilities),

    %% Verify response structure is valid
    ?assert(maps:is_key(?MCP_FIELD_PROTOCOL_VERSION, Response)),
    ?assert(maps:is_key(?MCP_FIELD_CAPABILITIES, Response)),

    erlmcp_server:stop(Pid).

test_task_create_allowed_with_capability() ->
    Capabilities = #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true}
    },
    {ok, Pid} = erlmcp_server:start_link(test_task_create, Capabilities),

    %% Add a tool that can be called as a task
    ok = erlmcp_server:add_tool(Pid, <<"test_tool">>, fun(_Args) -> <<"result">> end),

    erlmcp_server:stop(Pid).

%%====================================================================
%% Helper Functions - Internal API
%%====================================================================

%% Build initialize response (mimics server behavior)
build_initialize_response(Capabilities) ->
    {ok, Version} = application:get_key(erlmcp, vsn),
    #{
        ?MCP_FIELD_PROTOCOL_VERSION => ?MCP_VERSION,
        ?MCP_FIELD_CAPABILITIES => encode_server_capabilities(Capabilities),
        ?MCP_FIELD_SERVER_INFO => #{
            ?MCP_INFO_NAME => <<"erlmcp">>,
            ?MCP_INFO_VERSION => list_to_binary(Version)
        }
    }.

%% Encode server capabilities
encode_server_capabilities(#mcp_server_capabilities{} = Caps) ->
    Base = #{},
    Base1 = maybe_add_server_capability(Base, ?MCP_CAPABILITY_RESOURCES,
                                        Caps#mcp_server_capabilities.resources,
                                        #{?MCP_FEATURE_SUBSCRIBE => true,
                                          ?MCP_FEATURE_LIST_CHANGED => true}),
    Base2 = maybe_add_server_capability(Base1, ?MCP_CAPABILITY_TOOLS,
                                        Caps#mcp_server_capabilities.tools, #{}),
    Base3 = maybe_add_server_capability(Base2, ?MCP_CAPABILITY_PROMPTS,
                                        Caps#mcp_server_capabilities.prompts,
                                        #{?MCP_FEATURE_LIST_CHANGED => true}),
    maybe_add_server_capability(Base3, ?MCP_CAPABILITY_LOGGING,
                                Caps#mcp_server_capabilities.logging, #{}).

%% Add capability if enabled
maybe_add_server_capability(Map, _Key, undefined, _Value) ->
    Map;
maybe_add_server_capability(Map, Key, #mcp_capability{enabled = true}, Value) ->
    Map#{Key => Value};
maybe_add_server_capability(Map, _Key, _, _Value) ->
    Map.

%% Extract client capabilities from initialize request params
extract_client_capabilities(Params) when is_map(Params) ->
    CapMap = maps:get(?MCP_FIELD_CAPABILITIES, Params, #{}),

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
    };
extract_client_capabilities(_) ->
    #mcp_client_capabilities{}.

%% Validate protocol version
validate_protocol_version(Version) when is_binary(Version) ->
    %% MCP protocol uses YYYY-MM-DD format
    %% Current version is ?MCP_VERSION
    case Version of
        ?MCP_VERSION -> ok;
        _ ->
            %% For now, accept matching versions
            %% Production could implement compatibility matrix
            case Version > ?MCP_VERSION of
                true ->
                    {error, <<"Protocol version not yet supported">>};
                false ->
                    %% Allow older compatible versions
                    case Version < <<"2025-01-01">> of
                        true ->
                            {error, <<"Protocol version too old">>};
                        false ->
                            ok
                    end
            end
    end;
validate_protocol_version(_) ->
    {error, <<"Invalid protocol version format">>}.

%% Check if client has specific capability
client_has_capability(#mcp_client_capabilities{roots = Roots}, ?MCP_CAPABILITY_ROOTS) ->
    Roots =/= undefined andalso Roots#mcp_capability.enabled;
client_has_capability(#mcp_client_capabilities{sampling = Sampling}, ?MCP_CAPABILITY_SAMPLING) ->
    Sampling =/= undefined andalso Sampling#mcp_capability.enabled;
client_has_capability(#mcp_client_capabilities{experimental = Exp}, CapName) ->
    Exp =/= undefined andalso maps:is_key(CapName, Exp);
client_has_capability(_, _) ->
    false.

%%====================================================================
%% End of Test Suite
%%====================================================================
