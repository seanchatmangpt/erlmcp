%%%-------------------------------------------------------------------
%%% @doc
%%% Capability Query and Extraction Tests for erlmcp_capabilities module.
%%%
%%% Tests the capability query functions:
%%% - supports_* functions (elicitation, tasks, streaming, progress)
%%% - get_experimental_features
%%% - capability_to_map
%%% - extract_client_capabilities
%%% - extract_server_capabilities
%%%
%%% Chicago School TDD Principles:
%%% - Tests observable behavior through API calls only
%%% - No state inspection
%%% - Tests pure functional query logic
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_capabilities_query_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

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
%%% Capability Conversion Tests
%%%====================================================================

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
