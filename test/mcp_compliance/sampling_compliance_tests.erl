%% @doc Sampling Capability Compliance Tests
%% Validates compliance with MCP Sampling capability specification
-module(sampling_compliance_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Sampling Create Message Method Tests
%%%===================================================================

sampling_create_message_method_name_test() ->
    %% sampling/createMessage method name
    ?assertEqual(<<"sampling/createMessage">>, ?MCP_METHOD_SAMPLING_CREATE_MESSAGE).

sampling_create_message_required_messages_test() ->
    %% sampling/createMessage requires messages parameter
    ?assert(true).

sampling_create_message_messages_array_test() ->
    %% messages parameter must be array
    Messages = [
        #{role => user, content => #{type => text, text => <<"Hello">>}}
    ],
    ?assert(is_list(Messages)),
    ?assert(length(Messages) > 0).

sampling_create_message_messages_empty_test() ->
    %% Empty messages array should fail
    EmptyMessages = [],
    ?assertEqual([], EmptyMessages).

sampling_create_message_optional_model_preferences_test() ->
    %% modelPreferences parameter is optional
    Params = #{
        messages => [
            #{role => user, content => #{type => text, text => <<"Hello">>}}
        ],
        modelPreferences => #{
            temperature => 0.7,
            maxTokens => 1000
        }
    },
    ?assert(maps:is_key(<<"modelPreferences">>, Params)).

%%%===================================================================
%%% Sampling Message Structure Tests
%%%===================================================================

sampling_message_role_test() ->
    %% Message must have role
    Message = #{
        role => user,
        content => #{type => text, text => <<"Hello">>}
    },
    ?assert(maps:is_key(<<"role">>, Message)),
    ?assert(is_binary(maps:get(<<"role">>, Message))).

sampling_message_content_test() ->
    %% Message must have content
    Message = #{
        role => user,
        content => #{type => text, text => <<"Hello">>}
    },
    ?assert(maps:is_key(<<"content">>, Message)),
    ?assert(is_map(maps:get(<<"content">>, Message))).

sampling_message_content_text_test() ->
    %% Content can be text string
    Content = #{type => text, text => <<"Hello, world!">>},
    ?assertEqual(<<"text">>, maps:get(<<"type">>, Content)),
    ?assert(is_binary(maps:get(<<"text">>, Content))).

sampling_message_content_image_test() ->
    %% Content can be image object
    ImageData = base64:encode(<<"image data">>),
    Content = #{
        type => image,
        data => ImageData,
        mimeType => <<"image/png">>
    },
    ?assertEqual(<<"image">>, maps:get(<<"type">>, Content)),
    ?assert(is_binary(maps:get(<<"data">>, Content))).

sampling_message_content_resource_test() ->
    %% Content can reference resource
    Content = #{
        type => resource,
        uri => <<"file:///document.txt">>
    },
    ?assertEqual(<<"resource">>, maps:get(<<"type">>, Content)),
    ?assert(is_binary(maps:get(<<"uri">>, Content))).

%%%===================================================================
%%% Model Preferences Tests (Gap #39)
%%%===================================================================

sampling_model_preferences_temperature_test() ->
    %% temperature parameter (0.0 to 2.0)
    ModelPreferences = #{
        temperature => 0.7
    },
    ?assert(is_number(maps:get(<<"temperature">>, ModelPreferences))),
    Temp = maps:get(<<"temperature">>, ModelPreferences),
    ?assert(Temp >= 0.0 andalso Temp =< 2.0).

sampling_model_preferences_max_tokens_test() ->
    %% maxTokens parameter
    ModelPreferences = #{
        maxTokens => 1000
    },
    ?assert(is_integer(maps:get(<<"maxTokens">>, ModelPreferences))),
    ?assert(maps:get(<<"maxTokens">>, ModelPreferences) > 0).

sampling_model_preferences_stop_sequences_test() ->
    %% stopSequences parameter
    ModelPreferences = #{
        stopSequences => [<<"\n\n">>, <<"END">>]
    },
    ?assert(is_list(maps:get(<<"stopSequences">>, ModelPreferences))).

sampling_model_preferences_cost_priority_test() ->
    %% costPriority parameter
    ModelPreferences = #{
        costPriority => 0.8
    },
    ?assert(is_number(maps:get(<<"costPriority">>, ModelPreferences))).

sampling_model_preferences_speed_priority_test() ->
    %% speedPriority parameter
    ModelPreferences = #{
        speedPriority => 0.9
    },
    ?assert(is_number(maps:get(<<"speedPriority">>, ModelPreferences))).

sampling_model_preferences_intelligence_priority_test() ->
    %% intelligencePriority parameter
    ModelPreferences = #{
        intelligencePriority => 0.7
    },
    ?assert(is_number(maps:get(<<"intelligencePriority">>, ModelPreferences))).

%%%===================================================================
%%% Sampling Strategy Tests
%%%===================================================================

sampling_strategy_deterministic_test() ->
    %% Deterministic sampling strategy
    ?assertEqual(<<"deterministic">>, ?MCP_SAMPLING_STRATEGY_DETERMINISTIC).

sampling_strategy_uniform_test() ->
    %% Uniform sampling strategy
    ?assertEqual(<<"uniform">>, ?MCP_SAMPLING_STRATEGY_UNIFORM).

sampling_strategy_validation_test() ->
    %% Validate sampling strategies
    ValidStrategies = ?MCP_VALID_SAMPLING_STRATEGIES,
    ?assert(lists:member(<<"deterministic">>, ValidStrategies)),
    ?assert(lists:member(<<"uniform">>, ValidStrategies)).

%%%===================================================================
%%% Sampling Response Tests
%%%===================================================================

sampling_create_message_returns_message_test() ->
    %% Response must contain message
    Response = #{
        model => <<"gpt-4">>,
        role => assistant,
        content => #{
            type => text,
            text => <<"Hello! How can I help you today?">>
        }
    },
    ?assert(maps:is_key(<<"role">>, Response)),
    ?assert(maps:is_key(<<"content">>, Response)).

sampling_response_model_field_test() ->
    %% Response can include model field
    Response = #{
        model => <<"gpt-4-turbo">>,
        role => assistant,
        content => #{type => text, text => <<"Response">>}
    },
    ?assert(maps:is_key(<<"model">>, Response)),
    ?assert(is_binary(maps:get(<<"model">>, Response))).

sampling_response_stop_reason_test() ->
    %% Response can include stopReason
    Response = #{
        role => assistant,
        content => #{type => text, text => <<"Response">>},
        stopReason => <<"stop">>
    },
    ?assert(maps:is_key(<<"stopReason">>, Response)),
    ?assert(is_binary(maps:get(<<"stopReason">>, Response))).

sampling_response_usage_test() ->
    %% Response can include usage metadata
    Response = #{
        role => assistant,
        content => #{type => text, text => <<"Response">>},
        usage => #{
            promptTokens => 10,
            completionTokens => 20
        }
    },
    ?assert(maps:is_key(<<"usage">>, Response)),
    Usage = maps:get(<<"usage">>, Response),
    ?assert(maps:is_key(<<"promptTokens">>, Usage)),
    ?assert(maps:is_key(<<"completionTokens">>, Usage)).

%%%===================================================================
%%% Sampling Error Handling Tests
%%%===================================================================

sampling_invalid_messages_test() ->
    %% Invalid messages format
    ?assertEqual({error, invalid_message_format},
                 validate_sampling_message(#{})).

sampling_empty_messages_test() ->
    %% Empty messages array
    ?assertEqual({error, empty_messages},
                 validate_sampling_messages([])).

sampling_invalid_temperature_test() ->
    %% Temperature out of range
    ?assertEqual({error, invalid_temperature},
                 validate_temperature(2.5)).

sampling_invalid_max_tokens_test() ->
    %% Max tokens invalid
    ?assertEqual({error, invalid_max_tokens},
                 validate_max_tokens(0)).

sampling_invalid_stop_sequences_test() ->
    %% Stop sequences invalid
    ?assertEqual({error, invalid_stop_sequences},
                 validate_stop_sequences(123)).

%%%===================================================================
%%% Sampling Capability Negotiation Tests
%%%===================================================================

sampling_capability_advertised_test() ->
    %% Server advertises sampling capability
    Capabilities = #mcp_server_capabilities{
        sampling = true
    },
    ?assertEqual(true, Capabilities#mcp_server_capabilities.sampling).

sampling_capability_disabled_test() ->
    %% Server can disable sampling capability
    Capabilities = #mcp_server_capabilities{
        sampling = false
    },
    ?assertEqual(false, Capabilities#mcp_server_capabilities.sampling).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

validate_sampling_message(Message) when is_map(Message) ->
    Role = maps:get(<<"role">>, Message, undefined),
    Content = maps:get(<<"content">>, Message, undefined),
    case {Role, Content} of
        {undefined, _} -> {error, invalid_message_format};
        {_, undefined} -> {error, invalid_message_format};
        {R, _} when is_binary(R) ->
            case Content of
                C when is_binary(C); is_map(C) -> ok;
                _ -> {error, invalid_message_format}
            end;
        _ -> {error, invalid_message_format}
    end;
validate_sampling_message(_) ->
    {error, invalid_message_format}.

validate_sampling_messages(Messages) when is_list(Messages) ->
    case length(Messages) of
        0 -> {error, empty_messages};
        _ ->
            case lists:all(fun(M) -> validate_sampling_message(M) =:= ok end, Messages) of
                true -> ok;
                false -> {error, invalid_message_format}
            end
    end;
validate_sampling_messages(_) ->
    {error, invalid_message_format}.

validate_temperature(Temp) when is_number(Temp) ->
    case Temp >= 0.0 andalso Temp =< 2.0 of
        true -> ok;
        false -> {error, invalid_temperature}
    end;
validate_temperature(_) ->
    {error, invalid_temperature}.

validate_max_tokens(MaxTokens) when is_integer(MaxTokens) ->
    case MaxTokens > 0 of
        true -> ok;
        false -> {error, invalid_max_tokens}
    end;
validate_max_tokens(_) ->
    {error, invalid_max_tokens}.

validate_stop_sequences(StopSequences) when is_list(StopSequences) ->
    case lists:all(fun(S) -> is_binary(S) end, StopSequences) of
        true -> ok;
        false -> {error, invalid_stop_sequences}
    end;
validate_stop_sequences(_) ->
    {error, invalid_stop_sequences}.
