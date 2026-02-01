%%%-------------------------------------------------------------------
%%% @doc
%%% Capability Negotiation Tests for erlmcp_capabilities module.
%%%
%%% Tests the capability negotiation logic between client and server:
%%% - Experimental feature negotiation (elicitation, tasks, streaming, progress)
%%% - Partial overlap scenarios
%%% - Full negotiation with multiple features
%%%
%%% Chicago School TDD Principles:
%%% - Tests observable behavior through API calls only
%%% - No state inspection
%%% - Tests pure functional negotiation logic
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_capabilities_negotiation_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%%====================================================================
%%% Experimental Feature Negotiation Tests
%%%====================================================================

%% @doc Test negotiation of experimental elicitation feature
elicitation_negotiation_test() ->
    ClientCaps =
        #mcp_client_capabilities{roots = #mcp_capability{enabled = true},
                                 sampling = #mcp_capability{enabled = false},
                                 tools = #mcp_tools_capability{listChanged = false},
                                 experimental = #{<<"elicitation">> => true}},
    ServerCaps =
        #mcp_server_capabilities{resources = #mcp_resources_capability{},
                                 tools = #mcp_tools_capability{},
                                 prompts = #mcp_prompts_capability{},
                                 logging = #mcp_logging_capability{},
                                 sampling = #mcp_sampling_capability{},
                                 roots = #mcp_roots_capability{},
                                 experimental = #{<<"elicitation">> => true}},
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental = #{<<"elicitation">> := true}}, Negotiated).

%% @doc Test elicitation not negotiated when client doesn't support it
elicitation_client_unsupported_test() ->
    ClientCaps =
        #mcp_client_capabilities{roots = #mcp_capability{enabled = true},
                                 sampling = #mcp_capability{enabled = false},
                                 tools = #mcp_tools_capability{listChanged = false},
                                 experimental = undefined},
    ServerCaps =
        #mcp_server_capabilities{resources = #mcp_resources_capability{},
                                 tools = #mcp_tools_capability{},
                                 prompts = #mcp_prompts_capability{},
                                 logging = #mcp_logging_capability{},
                                 sampling = #mcp_sampling_capability{},
                                 roots = #mcp_roots_capability{},
                                 experimental = #{<<"elicitation">> => true}},
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental = undefined}, Negotiated).

%% @doc Test elicitation not negotiated when server doesn't support it
elicitation_server_unsupported_test() ->
    ClientCaps =
        #mcp_client_capabilities{roots = #mcp_capability{enabled = true},
                                 sampling = #mcp_capability{enabled = false},
                                 tools = #mcp_tools_capability{listChanged = false},
                                 experimental = #{<<"elicitation">> => true}},
    ServerCaps =
        #mcp_server_capabilities{resources = #mcp_resources_capability{},
                                 tools = #mcp_tools_capability{},
                                 prompts = #mcp_prompts_capability{},
                                 logging = #mcp_logging_capability{},
                                 sampling = #mcp_sampling_capability{},
                                 roots = #mcp_roots_capability{},
                                 experimental = undefined},
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental = undefined}, Negotiated).

%% @doc Test negotiation of experimental tasks feature
tasks_negotiation_test() ->
    ClientCaps =
        #mcp_client_capabilities{roots = #mcp_capability{enabled = true},
                                 sampling = #mcp_capability{enabled = false},
                                 tools = #mcp_tools_capability{listChanged = false},
                                 experimental = #{<<"tasks">> => true}},
    ServerCaps =
        #mcp_server_capabilities{resources = #mcp_resources_capability{},
                                 tools = #mcp_tools_capability{},
                                 prompts = #mcp_prompts_capability{},
                                 logging = #mcp_logging_capability{},
                                 sampling = #mcp_sampling_capability{},
                                 roots = #mcp_roots_capability{},
                                 experimental = #{<<"tasks">> => true}},
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental = #{<<"tasks">> := true}}, Negotiated).

%% @doc Test negotiation of experimental streaming feature
streaming_negotiation_test() ->
    ClientCaps =
        #mcp_client_capabilities{roots = #mcp_capability{enabled = true},
                                 sampling = #mcp_capability{enabled = false},
                                 tools = #mcp_tools_capability{listChanged = false},
                                 experimental = #{<<"streaming">> => true}},
    ServerCaps =
        #mcp_server_capabilities{resources = #mcp_resources_capability{},
                                 tools = #mcp_tools_capability{},
                                 prompts = #mcp_prompts_capability{},
                                 logging = #mcp_logging_capability{},
                                 sampling = #mcp_sampling_capability{},
                                 roots = #mcp_roots_capability{},
                                 experimental = #{<<"streaming">> => true}},
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental = #{<<"streaming">> := true}}, Negotiated).

%% @doc Test negotiation of experimental progress feature
progress_negotiation_test() ->
    ClientCaps =
        #mcp_client_capabilities{roots = #mcp_capability{enabled = true},
                                 sampling = #mcp_capability{enabled = false},
                                 tools = #mcp_tools_capability{listChanged = false},
                                 experimental = #{<<"progress">> => true}},
    ServerCaps =
        #mcp_server_capabilities{resources = #mcp_resources_capability{},
                                 tools = #mcp_tools_capability{},
                                 prompts = #mcp_prompts_capability{},
                                 logging = #mcp_logging_capability{},
                                 sampling = #mcp_sampling_capability{},
                                 roots = #mcp_roots_capability{},
                                 experimental = #{<<"progress">> => true}},
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental = #{<<"progress">> := true}}, Negotiated).

%% @doc Test negotiation of multiple experimental features
multiple_experimental_features_test() ->
    ClientCaps =
        #mcp_client_capabilities{roots = #mcp_capability{enabled = true},
                                 sampling = #mcp_capability{enabled = false},
                                 tools = #mcp_tools_capability{listChanged = false},
                                 experimental =
                                     #{<<"elicitation">> => true,
                                       <<"tasks">> => true,
                                       <<"streaming">> => true}},
    ServerCaps =
        #mcp_server_capabilities{resources = #mcp_resources_capability{},
                                 tools = #mcp_tools_capability{},
                                 prompts = #mcp_prompts_capability{},
                                 logging = #mcp_logging_capability{},
                                 sampling = #mcp_sampling_capability{},
                                 roots = #mcp_roots_capability{},
                                 experimental =
                                     #{<<"elicitation">> => true,
                                       <<"tasks">> => true,
                                       <<"streaming">> => true,
                                       <<"progress">> => true}},
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental =
                                              #{<<"elicitation">> := true,
                                                <<"tasks">> := true,
                                                <<"streaming">> := true}},
                 Negotiated),
    NegotiatedExp = Negotiated#mcp_server_capabilities.experimental,
    ?assertNot(maps:is_key(<<"progress">>, NegotiatedExp)).

%% @doc Test partial overlap of experimental features
experimental_partial_overlap_test() ->
    ClientCaps =
        #mcp_client_capabilities{roots = #mcp_capability{enabled = true},
                                 sampling = #mcp_capability{enabled = false},
                                 tools = #mcp_tools_capability{listChanged = false},
                                 experimental =
                                     #{<<"elicitation">> => true, <<"streaming">> => true}},
    ServerCaps =
        #mcp_server_capabilities{resources = #mcp_resources_capability{},
                                 tools = #mcp_tools_capability{},
                                 prompts = #mcp_prompts_capability{},
                                 logging = #mcp_logging_capability{},
                                 sampling = #mcp_sampling_capability{},
                                 roots = #mcp_roots_capability{},
                                 experimental = #{<<"tasks">> => true, <<"streaming">> => true}},
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
    ?assertMatch(#mcp_server_capabilities{experimental = #{<<"streaming">> := true}}, Negotiated),
    NegotiatedExp = Negotiated#mcp_server_capabilities.experimental,
    ?assertNot(maps:is_key(<<"elicitation">>, NegotiatedExp)),
    ?assertNot(maps:is_key(<<"tasks">>, NegotiatedExp)).

%% @doc Test full capability negotiation with experimental features
full_negotiation_with_experimental_test() ->
    ClientCaps =
        #mcp_client_capabilities{roots = #mcp_capability{enabled = true},
                                 sampling = #mcp_capability{enabled = true},
                                 tools = #mcp_tools_capability{listChanged = true},
                                 experimental = #{<<"elicitation">> => true, <<"tasks">> => true}},
    ServerCaps =
        #mcp_server_capabilities{resources =
                                     #mcp_resources_capability{subscribe = true,
                                                               listChanged = true},
                                 tools = #mcp_tools_capability{listChanged = true},
                                 prompts = #mcp_prompts_capability{listChanged = false},
                                 logging = #mcp_logging_capability{},
                                 sampling =
                                     #mcp_sampling_capability{modelPreferences =
                                                                  #{<<"temperature">> => 0.7}},
                                 roots = #mcp_roots_capability{},
                                 experimental =
                                     #{<<"elicitation">> => true,
                                       <<"tasks">> => true,
                                       <<"streaming">> => true}},
    Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),

    ?assert(erlmcp_capabilities:supports_elicitation(Negotiated)),
    ?assert(erlmcp_capabilities:supports_tasks(Negotiated)),
    ?assertNot(erlmcp_capabilities:supports_streaming(Negotiated)),

    NegotiatedExp = Negotiated#mcp_server_capabilities.experimental,
    ?assert(maps:is_key(<<"elicitation">>, NegotiatedExp)),
    ?assert(maps:is_key(<<"tasks">>, NegotiatedExp)),
    ?assertNot(maps:is_key(<<"streaming">>, NegotiatedExp)).
