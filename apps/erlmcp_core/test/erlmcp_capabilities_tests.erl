-module(erlmcp_capabilities_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Test Suite Configuration
%%%====================================================================

%%%====================================================================
%%% Experimental Feature Negotiation Tests
%%%====================================================================

%% @doc Test negotiation of experimental elicitation feature
elicitation_negotiation_test() ->
    %% Both client and server support elicitation
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = #{<<"elicitation">> => true}
    },
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{<<"elicitation">> => true}
    },
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental = #{<<"elicitation">> := true}}, Negotiated).

%% @doc Test elicitation not negotiated when client doesn't support it
elicitation_client_unsupported_test() ->
    %% Server supports elicitation, client doesn't
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = undefined
    },
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{<<"elicitation">> => true}
    },
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental = undefined}, Negotiated).

%% @doc Test elicitation not negotiated when server doesn't support it
elicitation_server_unsupported_test() ->
    %% Client supports elicitation, server doesn't
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = #{<<"elicitation">> => true}
    },
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = undefined
    },
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental = undefined}, Negotiated).

%% @doc Test negotiation of experimental tasks feature
tasks_negotiation_test() ->
    %% Both client and server support tasks
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = #{<<"tasks">> => true}
    },
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{<<"tasks">> => true}
    },
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental = #{<<"tasks">> := true}}, Negotiated).

%% @doc Test negotiation of experimental streaming feature
streaming_negotiation_test() ->
    %% Both client and server support streaming
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = #{<<"streaming">> => true}
    },
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{<<"streaming">> => true}
    },
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental = #{<<"streaming">> := true}}, Negotiated).

%% @doc Test negotiation of experimental progress feature
progress_negotiation_test() ->
    %% Both client and server support progress
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = #{<<"progress">> => true}
    },
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{<<"progress">> => true}
    },
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental = #{<<"progress">> := true}}, Negotiated).

%% @doc Test negotiation of multiple experimental features
multiple_experimental_features_test() ->
    %% Both client and server support multiple experimental features
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = #{
            <<"elicitation">> => true,
            <<"tasks">> => true,
            <<"streaming">> => true
        }
    },
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{
            <<"elicitation">> => true,
            <<"tasks">> => true,
            <<"streaming">> => true,
            <<"progress">> => true
        }
    },
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{
        experimental = #{
            <<"elicitation">> := true,
            <<"tasks">> := true,
            <<"streaming">> := true
        }
    }, Negotiated),
    %% Progress should not be in negotiated since client doesn't support it
    NegotiatedExp = Negotiated#mcp_server_capabilities.experimental,
    ?assertNot(maps:is_key(<<"progress">>, NegotiatedExp)).

%% @doc Test partial overlap of experimental features
experimental_partial_overlap_test() ->
    %% Client and server have different experimental features
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = #{
            <<"elicitation">> => true,
            <<"streaming">> => true
        }
    },
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{
            <<"tasks">> => true,
            <<"streaming">> => true
        }
    },
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{
        experimental = #{<<"streaming">> := true}
    }, Negotiated),
    NegotiatedExp = Negotiated#mcp_server_capabilities.experimental,
    %% Only streaming should be negotiated (intersection)
    ?assertNot(maps:is_key(<<"elicitation">>, NegotiatedExp)),
    ?assertNot(maps:is_key(<<"tasks">>, NegotiatedExp)).

%%%====================================================================
%%% Experimental Feature Query Tests
%%%====================================================================

%% @doc Test supports_elicitation for client capabilities
supports_elicitation_client_test() ->
    CapsWithElicitation = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = #{<<"elicitation">> => true}
    },
    ?assert(erlmcp_capabilities:supports_elicitation(CapsWithElicitation)),

    CapsWithoutElicitation = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = undefined
    },
    ?assertNot(erlmcp_capabilities:supports_elicitation(CapsWithoutElicitation)).

%% @doc Test supports_elicitation for server capabilities
supports_elicitation_server_test() ->
    CapsWithElicitation = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{<<"elicitation">> => true}
    },
    ?assert(erlmcp_capabilities:supports_elicitation(CapsWithElicitation)),

    CapsWithoutElicitation = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = undefined
    },
    ?assertNot(erlmcp_capabilities:supports_elicitation(CapsWithoutElicitation)).

%% @doc Test supports_tasks for client capabilities
supports_tasks_client_test() ->
    CapsWithTasks = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = #{<<"tasks">> => true}
    },
    ?assert(erlmcp_capabilities:supports_tasks(CapsWithTasks)),

    CapsWithoutTasks = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = undefined
    },
    ?assertNot(erlmcp_capabilities:supports_tasks(CapsWithoutTasks)).

%% @doc Test supports_tasks for server capabilities
supports_tasks_server_test() ->
    CapsWithTasks = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{<<"tasks">> => true}
    },
    ?assert(erlmcp_capabilities:supports_tasks(CapsWithTasks)),

    CapsWithoutTasks = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = undefined
    },
    ?assertNot(erlmcp_capabilities:supports_tasks(CapsWithoutTasks)).

%% @doc Test supports_streaming for client capabilities
supports_streaming_client_test() ->
    CapsWithStreaming = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = #{<<"streaming">> => true}
    },
    ?assert(erlmcp_capabilities:supports_streaming(CapsWithStreaming)),

    CapsWithoutStreaming = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = undefined
    },
    ?assertNot(erlmcp_capabilities:supports_streaming(CapsWithoutStreaming)).

%% @doc Test supports_streaming for server capabilities
supports_streaming_server_test() ->
    CapsWithStreaming = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{<<"streaming">> => true}
    },
    ?assert(erlmcp_capabilities:supports_streaming(CapsWithStreaming)),

    CapsWithoutStreaming = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = undefined
    },
    ?assertNot(erlmcp_capabilities:supports_streaming(CapsWithoutStreaming)).

%% @doc Test supports_progress for client capabilities
supports_progress_client_test() ->
    CapsWithProgress = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = #{<<"progress">> => true}
    },
    ?assert(erlmcp_capabilities:supports_progress(CapsWithProgress)),

    CapsWithoutProgress = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = undefined
    },
    ?assertNot(erlmcp_capabilities:supports_progress(CapsWithoutProgress)).

%% @doc Test supports_progress for server capabilities
supports_progress_server_test() ->
    CapsWithProgress = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{<<"progress">> => true}
    },
    ?assert(erlmcp_capabilities:supports_progress(CapsWithProgress)),

    CapsWithoutProgress = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = undefined
    },
    ?assertNot(erlmcp_capabilities:supports_progress(CapsWithoutProgress)).

%% @doc Test get_experimental_features for client capabilities
get_experimental_features_client_test() ->
    Caps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = #{
            <<"elicitation">> => true,
            <<"tasks">> => true,
            <<"streaming">> => false
        }
    },
    Features = erlmcp_capabilities:get_experimental_features(Caps),
    ?assert(lists:member(<<"elicitation">>, Features)),
    ?assert(lists:member(<<"tasks">>, Features)),
    ?assertNot(lists:member(<<"streaming">>, Features)),
    ?assertEqual(2, length(Features)).

%% @doc Test get_experimental_features for server capabilities
get_experimental_features_server_test() ->
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{
            <<"elicitation">> => true,
            <<"tasks">> => true,
            <<"streaming">> => true,
            <<"progress">> => false
        }
    },
    Features = erlmcp_capabilities:get_experimental_features(Caps),
    ?assert(lists:member(<<"elicitation">>, Features)),
    ?assert(lists:member(<<"tasks">>, Features)),
    ?assert(lists:member(<<"streaming">>, Features)),
    ?assertNot(lists:member(<<"progress">>, Features)),
    ?assertEqual(3, length(Features)).

%% @doc Test get_experimental_features with no experimental features
get_experimental_features_none_test() ->
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental = undefined
    },
    ?assertEqual([], erlmcp_capabilities:get_experimental_features(ClientCaps)),

    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = undefined
    },
    ?assertEqual([], erlmcp_capabilities:get_experimental_features(ServerCaps)).

%%%====================================================================
%%% Integration Tests
%%%====================================================================

%% @doc Test full capability negotiation with experimental features
full_negotiation_with_experimental_test() ->
    %% Client with some experimental features
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = true},
        tools = #mcp_tools_capability{listChanged = true},
        experimental = #{
            <<"elicitation">> => true,
            <<"tasks">> => true
        }
    },
    %% Server with more experimental features
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{subscribe = true, listChanged = true},
        tools = #mcp_tools_capability{listChanged = true},
        prompts = #mcp_prompts_capability{listChanged = false},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{modelPreferences = #{<<"temperature">> => 0.7}},
        roots = #mcp_roots_capability{},
        experimental = #{
            <<"elicitation">> => true,
            <<"tasks">> => true,
            <<"streaming">> => true
        }
    },
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),

    %% Check that only mutually supported experimental features are negotiated
    ?assert(erlmcp_capabilities:supports_elicitation(Negotiated)),
    ?assert(erlmcp_capabilities:supports_tasks(Negotiated)),
    ?assertNot(erlmcp_capabilities:supports_streaming(Negotiated)),

    %% Verify negotiated features
    NegotiatedExp = Negotiated#mcp_server_capabilities.experimental,
    ?assert(maps:is_key(<<"elicitation">>, NegotiatedExp)),
    ?assert(maps:is_key(<<"tasks">>, NegotiatedExp)),
    ?assertNot(maps:is_key(<<"streaming">>, NegotiatedExp)).

%% @doc Test capability_to_map with experimental features
capability_to_map_with_experimental_test() ->
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental = #{
            <<"elicitation">> => true,
            <<"streaming">> => true
        }
    },
    Map = erlmcp_capabilities:capability_to_map(Caps),
    ?assert(maps:is_key(<<"experimental">>, Map)),
    Experimental = maps:get(<<"experimental">>, Map),
    ?assert(maps:get(<<"elicitation">>, Experimental, false)),
    ?assert(maps:get(<<"streaming">>, Experimental, false)).

%% @doc Test extract_client_capabilities with experimental features
extract_client_capabilities_with_experimental_test() ->
    Params = #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => #{
            <<"roots">> => #{},
            <<"sampling">> => #{},
            <<"experimental">> => #{
                <<"elicitation">> => true,
                <<"tasks">> => true
            }
        },
        <<"clientInfo">> => #{
            <<"name">> => <<"test_client">>,
            <<"version">> => <<"1.0.0">>
        }
    },
    Caps = erlmcp_capabilities:extract_client_capabilities(Params),
    ?assert(erlmcp_capabilities:supports_elicitation(Caps)),
    ?assert(erlmcp_capabilities:supports_tasks(Caps)),
    ?assertNot(erlmcp_capabilities:supports_streaming(Caps)).

%% @doc Test extract_server_capabilities with experimental features
extract_server_capabilities_with_experimental_test() ->
    Response = #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => #{
            <<"resources">> => #{},
            <<"tools">> => #{},
            <<"experimental">> => #{
                <<"streaming">> => true,
                <<"progress">> => true
            }
        },
        <<"serverInfo">> => #{
            <<"name">> => <<"test_server">>,
            <<"version">> => <<"1.0.0">>
        }
    },
    Caps = erlmcp_capabilities:extract_server_capabilities(Response),
    ?assertNot(erlmcp_capabilities:supports_elicitation(Caps)),
    ?assertNot(erlmcp_capabilities:supports_tasks(Caps)),
    ?assert(erlmcp_capabilities:supports_streaming(Caps)),
    ?assert(erlmcp_capabilities:supports_progress(Caps)).
