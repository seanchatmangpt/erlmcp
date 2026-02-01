%%%-------------------------------------------------------------------
%%% @doc erlmcp_mock_llm - Mock LLM provider for testing and development
%%% Provides a simple mock implementation that generates responses
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_mock_llm).

%% API
-export([create_message/2, create_message_with_delay/2, set_response_mode/1, create_echo_response/2,
         create_template_response/2]).

-include("erlmcp.hrl").

%% Response modes
-type response_mode() :: echo | template | error | timeout.
-type message_map() :: #{binary() => term()}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create a mock message response
-spec create_message([message_map()], map()) -> {ok, message_map()} | {error, term()}.
create_message(Messages, Params) when is_list(Messages), is_map(Params) ->
    %% Get response mode from process dictionary or default to echo
    ResponseMode =
        case get(response_mode) of
            undefined ->
                echo;
            Val ->
                Val
        end,

    case ResponseMode of
        echo ->
            create_echo_response(Messages, Params);
        template ->
            create_template_response(Messages, Params);
        error ->
            {error, mock_provider_error};
        timeout ->
            %% Simulate timeout by sleeping
            timer:sleep(35000),
            {error, timeout}
    end.

%% @doc Create a message with simulated delay
-spec create_message_with_delay([message_map()], map()) -> {ok, message_map()} | {error, term()}.
create_message_with_delay(Messages, Params) ->
    DelayMs = maps:get(delay_ms, Params, 100),
    timer:sleep(DelayMs),
    create_message(Messages, maps:remove(delay_ms, Params)).

%% @doc Set the response mode for this process
-spec set_response_mode(response_mode()) -> ok.
set_response_mode(Mode) when Mode =:= echo; Mode =:= template; Mode =:= error; Mode =:= timeout ->
    put(response_mode, Mode),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Create an echo response that repeats the input
-spec create_echo_response([message_map()], map()) -> {ok, message_map()}.
create_echo_response(Messages, _Params) ->
    %% Extract the last user message
    LastUserMessage =
        case lists:reverse(Messages) of
            [#{<<"role">> := <<"user">>, <<"content">> := Content} | _] ->
                Content;
            [#{<<"content">> := Content} | _] ->
                Content;
            _ ->
                <<"(no message)">>
        end,

    ResponseContent =
        case is_binary(LastUserMessage) of
            true ->
                <<"Echo: ", LastUserMessage/binary>>;
            false when is_map(LastUserMessage) ->
                <<"Echo: [complex content]">>;
            _ ->
                <<"Echo: (unknown content)">>
        end,

    {ok,
     #{<<"role">> => <<"assistant">>,
       <<"content">> => ResponseContent,
       <<"model">> => <<"mock-model-echo">>,
       <<"stopReason">> => <<"end_of_turn">>,
       <<"usage">> =>
           #{<<"promptTokens">> => estimate_tokens(Messages),
             <<"completionTokens">> =>
                 estimate_tokens([#{<<"role">> => <<"assistant">>,
                                    <<"content">> => ResponseContent}]),
             <<"totalTokens">> => 0}}}.

%% @doc Create a template response with structured output
-spec create_template_response([message_map()], map()) -> {ok, message_map()}.
create_template_response(Messages, Params) ->
    Model = maps:get(<<"model">>, Params, <<"mock-model-template">>),
    Temperature = maps:get(<<"temperature">>, Params, 0.7),

    %% Generate a contextual response
    ResponseText =
        case length(Messages) of
            1 ->
                <<"Hello! This is a mock response. I received your message.">>;
            2 ->
                <<"I understand. This is a mock response continuing our conversation.">>;
            N when N > 2 ->
                <<"This is a mock response acknowledging the conversation history of ",
                  (integer_to_binary(N))/binary,
                  " messages.">>;
            _ ->
                <<"This is a mock response.">>
        end,

    {ok,
     #{<<"role">> => <<"assistant">>,
       <<"content">> => ResponseText,
       <<"model">> => Model,
       <<"stopReason">> => <<"end_of_turn">>,
       <<"usage">> =>
           #{<<"promptTokens">> => estimate_tokens(Messages), <<"completionTokens">> => 25},
       <<"_metadata">> => #{<<"temperature">> => Temperature, <<"provider">> => <<"mock">>}}}.

%% @doc Estimate token count (rough approximation: 1 token ≈ 4 characters)
-spec estimate_tokens([message_map()] | binary()) -> integer().
estimate_tokens(Messages) when is_list(Messages) ->
    lists:foldl(fun(#{<<"content">> := Content}, Acc) -> Acc + estimate_tokens(Content) end,
                0,
                Messages);
estimate_tokens(Text) when is_binary(Text) ->
    ByteSize = byte_size(Text),
    ByteSize div 4 + 1;
estimate_tokens(_) ->
    1.
