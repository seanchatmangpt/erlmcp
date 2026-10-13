-module(erlmcp_capabilities_i18n_tests).
-author('erlmcp').

%% Internationalization tests for erlmcp_capabilities with OTP 28 UTF-8 support
%% Tests international capability names, experimental features, and metadata

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Test Data: International Capability Names
%%%====================================================================

%% Japanese capability names
-define(JAPANESE_CAP, <<"日本語能力">>).
-define(JAPANESE_PROMPT, <<"日本語_プロンプト">>).

%% Arabic capability names
-define(ARABIC_CAP, <<"القدرة">>).
-define(ARABIC_FEATURE, <<"ميزة_تجريبية">>).

%% Korean capability names
-define(KOREAN_CAP, <<"기능">>).
-define(KOREAN_EXPERIMENTAL, <<"실험적_기능">>).

%% Emoji capability names
-define(EMOJI_CAP, <<"🎨_creative_🚀">>).
-define(EMOJI_FEATURE, <<"✨_magic_feature">>).

%% Mixed language experimental features
-define(MIXED_EXPERIMENTAL, <<"experimental_実験_تجربة_실험">>).

%% Invalid names
-define(INVALID_CAP, <<"cap\0name">>).
-define(TOO_LONG_CAP, (binary:copy(<<"a">>, 256))).

%%%====================================================================
%%% Capability Validation Tests
%%%====================================================================

validate_japanese_capability_test() ->
    ?assertEqual(ok, erlmcp_capabilities:validate_capability_name(?JAPANESE_CAP)),
    ?assertEqual(ok, erlmcp_capabilities:validate_capability_name(?JAPANESE_PROMPT)).

validate_arabic_capability_test() ->
    ?assertEqual(ok, erlmcp_capabilities:validate_capability_name(?ARABIC_CAP)),
    ?assertEqual(ok, erlmcp_capabilities:validate_capability_name(?ARABIC_FEATURE)).

validate_korean_capability_test() ->
    ?assertEqual(ok, erlmcp_capabilities:validate_capability_name(?KOREAN_CAP)),
    ?assertEqual(ok, erlmcp_capabilities:validate_capability_name(?KOREAN_EXPERIMENTAL)).

validate_emoji_capability_test() ->
    ?assertEqual(ok, erlmcp_capabilities:validate_capability_name(?EMOJI_CAP)),
    ?assertEqual(ok, erlmcp_capabilities:validate_capability_name(?EMOJI_FEATURE)).

validate_mixed_capability_test() ->
    ?assertEqual(ok, erlmcp_capabilities:validate_capability_name(?MIXED_EXPERIMENTAL)).

validate_invalid_capability_test() ->
    ?assertEqual({error, invalid_binary}, erlmcp_capabilities:validate_capability_name(<<>>)),
    ?assertEqual({error, too_long}, erlmcp_capabilities:validate_capability_name(?TOO_LONG_CAP)).

%%%====================================================================
;;; Capability Normalization Tests
%%%====================================================================

normalize_japanese_capability_test() ->
    Normalized = erlmcp_capabilities:normalize_capability_name(?JAPANESE_CAP),
    ?assertEqual(?JAPANESE_CAP, Normalized),
    ?assert(is_binary(Normalized)).

normalize_arabic_capability_test() ->
    Normalized = erlmcp_capabilities:normalize_capability_name(?ARABIC_CAP),
    ?assertEqual(?ARABIC_CAP, Normalized),
    ?assert(is_binary(Normalized)).

normalize_emoji_capability_test() ->
    Normalized = erlmcp_capabilities:normalize_capability_name(?EMOJI_CAP),
    ?assertEqual(?EMOJI_CAP, Normalized),
    ?assert(is_binary(Normalized)).

normalize_mixed_capability_test() ->
    Normalized = erlmcp_capabilities:normalize_capability_name(?MIXED_EXPERIMENTAL),
    ?assertEqual(?MIXED_EXPERIMENTAL, Normalized),
    ?assert(is_binary(Normalized)).

%%%====================================================================
%%% Client Capabilities with International Features
%%%====================================================================

extract_client_international_capabilities_test() ->
    Params => #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => #{
            <<"roots">> => #{},
            <<"sampling">> => #{},
            <<"tools">> => #{},
            <<"experimental">> => #{
                ?JAPANESE_CAP => true,
                ?ARABIC_CAP => true,
                ?KOREAN_CAP => true
            }
        },
        <<"clientInfo">> => #{
            <<"name">> => <<"test_client">>,
            <<"version">> => <<"1.0.0">>
        }
    },

    ClientCaps = erlmcp_capabilities:extract_client_capabilities(Params),

    %% Verify experimental features are extracted
    ?assertNotEqual(undefined, ClientCaps#mcp_client_capabilities.experimental),
    ExpMap = ClientCaps#mcp_client_capabilities.experimental,
    ?assertEqual(true, maps:get(?JAPANESE_CAP, ExpMap, false)),
    ?assertEqual(true, maps:get(?ARABIC_CAP, ExpMap, false)),
    ?assertEqual(true, maps:get(?KOREAN_CAP, ExpMap, false)).

%%%====================================================================
%%% Server Capabilities with International Features
%%%====================================================================

extract_server_international_capabilities_test() ->
    Response => #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => #{
            <<"resources">> => #{},
            <<"tools">> => #{},
            <<"prompts">> => #{},
            <<"experimental">> => #{
                ?EMOJI_CAP => true,
                ?MIXED_EXPERIMENTAL => true
            }
        },
        <<"serverInfo">> => #{
            <<"name">> => <<"test_server">>,
            <<"version">> => <<"1.0.0">>
        }
    },

    ServerCaps = erlmcp_capabilities:extract_server_capabilities(Response),

    %% Verify experimental features
    ?assertNotEqual(undefined, ServerCaps#mcp_server_capabilities.experimental),
    ExpMap = ServerCaps#mcp_server_capabilities.experimental,
    ?assertEqual(true, maps:get(?EMOJI_CAP, ExpMap, false)),
    ?assertEqual(true, maps:get(?MIXED_EXPERIMENTAL, ExpMap, false)).

%%%====================================================================
;;; Experimental Feature Queries
%%%====================================================================

supports_japanese_experimental_test() ->
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = false},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental => #{
            ?JAPANESE_CAP => true,
            <<"other_feature">> => false
        }
    },

    ?assertEqual(true, erlmcp_capabilities:supports_elicitation(ClientCaps)),
    ?assert(lists:member(?JAPANESE_CAP, erlmcp_capabilities:get_experimental_features(ClientCaps))).

supports_arabic_experimental_test() ->
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental => #{
            ?ARABIC_CAP => true,
            ?KOREAN_CAP => true
        }
    },

    Features = erlmcp_capabilities:get_experimental_features(ServerCaps),
    ?assert(lists:member(?ARABIC_CAP, Features)),
    ?assert(lists:member(?KOREAN_CAP, Features)),
    ?assertEqual(2, length(Features)).

supports_emoji_experimental_test() ->
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = false},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental => #{
            ?EMOJI_CAP => true,
            ?EMOJI_FEATURE => true
        }
    },

    Features = erlmcp_capabilities:get_experimental_features(ClientCaps),
    ?assert(lists:member(?EMOJI_CAP, Features)),
    ?assert(lists:member(?EMOJI_FEATURE, Features)),
    ?assertEqual(2, length(Features)).

%%%====================================================================
%%% Capability Negotiation with International Features
%%%====================================================================

negotiate_international_capabilities_test() ->
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental => #{
            ?JAPANESE_CAP => true,
            ?ARABIC_CAP => true,
            ?KOREAN_CAP => true
        }
    },

    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental => #{
            ?JAPANESE_CAP => true,
            ?ARABIC_CAP => true,
            ?KOREAN_CAP => false,
            ?EMOJI_CAP => true
        }
    },

    NegotiatedCaps = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),

    %% Verify intersection of experimental features
    NegotiatedExp = NegotiatedCaps#mcp_server_capabilities.experimental,
    ?assertNotEqual(undefined, NegotiatedExp),
    ?assertEqual(true, maps:get(?JAPANESE_CAP, NegotiatedExp, false)),
    ?assertEqual(true, maps:get(?ARABIC_CAP, NegotiatedExp, false)),
    ?assertEqual(false, maps:get(?KOREAN_CAP, NegotiatedExp, false)),
    ?assertEqual(false, maps:get(?EMOJI_CAP, NegotiatedExp, false)).

%%%====================================================================
%%% Capability Map Conversion with International Names
%%%====================================================================

capability_to_map_international_test() ->
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental => #{
            ?JAPANESE_CAP => true,
            ?ARABIC_CAP => true,
            ?EMOJI_CAP => true
        }
    },

    CapMap = erlmcp_capabilities:capability_to_map(ServerCaps),

    %% Verify experimental features are in the map
    ?assert(maps:is_key(<<"experimental">>, CapMap)),
    ExpMap = maps:get(<<"experimental">>, CapMap),
    ?assertEqual(true, maps:get(?JAPANESE_CAP, ExpMap)),
    ?assertEqual(true, maps:get(?ARABIC_CAP, ExpMap)),
    ?assertEqual(true, maps:get(?EMOJI_CAP, ExpMap)).

map_to_capability_international_test() ->
    CapMap => #{
        <<"resources">> => #{},
        <<"tools">> => #{},
        <<"prompts">> => #{},
        <<"experimental">> => #{
            ?JAPANESE_CAP => true,
            ?ARABIC_CAP => true,
            ?KOREAN_CAP => true
        }
    },

    ServerCaps = erlmcp_capabilities:map_to_capability(CapMap),

    %% Verify experimental features are preserved
    ?assertNotEqual(undefined, ServerCaps#mcp_server_capabilities.experimental),
    ExpMap = ServerCaps#mcp_server_capabilities.experimental,
    ?assertEqual(true, maps:get(?JAPANESE_CAP, ExpMap)),
    ?assertEqual(true, maps:get(?ARABIC_CAP, ExpMap)),
    ?assertEqual(true, maps:get(?KOREAN_CAP, ExpMap)).

%%%====================================================================
%%% Character Length Tests
%%%====================================================================

char_length_capability_test() ->
    %% Japanese: 5 characters (日本語能力)
    ?assertEqual(5, string:length(?JAPANESE_CAP)),
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?JAPANESE_CAP)),

    %% Arabic: 4 characters (القدرة)
    ?assertEqual(4, string:length(?ARABIC_CAP)),
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?ARABIC_CAP)),

    %% Emoji: 13 characters (🎨_creative_🚀)
    ?assertEqual(13, string:length(?EMOJI_CAP)),
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?EMOJI_CAP)).

%%%====================================================================
%%% Performance Tests
%%%====================================================================

international_capability_validation_performance_test() ->
    Caps = lists:duplicate(1000, ?MIXED_EXPERIMENTAL),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(Cap) ->
            erlmcp_capabilities:validate_capability_name(Cap)
        end, Caps)
    end),

    %% Should complete 1000 validations in under 100ms
    ?assert(Time < 100000).

capability_negotiation_performance_test() ->
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental => #{
            ?JAPANESE_CAP => true,
            ?ARABIC_CAP => true,
            ?KOREAN_CAP => true
        }
    },

    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental => #{
            ?JAPANESE_CAP => true,
            ?ARABIC_CAP => true,
            ?KOREAN_CAP => false
        }
    },

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps)
        end, lists:seq(1, 100))
    end),

    %% Should complete 100 negotiations in under 100ms
    ?assert(Time < 100000).

%%%====================================================================
%%% Integration Tests
%%%====================================================================

full_capability_negotiation_cycle_test() ->
    %% Build client init params with international experimental features
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false},
        tools = #mcp_tools_capability{listChanged = false},
        experimental => #{
            ?JAPANESE_CAP => true,
            ?ARABIC_CAP => true
        }
    },

    %% Build server init response with experimental features
    ServerCaps = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{},
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{},
        experimental => #{
            ?JAPANESE_CAP => true,
            ?EMOJI_CAP => true
        }
    },

    %% Negotiate capabilities
    NegotiatedCaps = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),

    %% Verify only common features are negotiated
    NegotiatedExp = NegotiatedCaps#mcp_server_capabilities.experimental,
    ?assertEqual(true, maps:get(?JAPANESE_CAP, NegotiatedExp, false)),
    ?assertEqual(false, maps:get(?ARABIC_CAP, NegotiatedExp, false)),
    ?assertEqual(false, maps:get(?EMOJI_CAP, NegotiatedExp, false)).
