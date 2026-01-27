-module(erlmcp_sampling).

-include("erlmcp.hrl").

%% API
-export([
    extract_model_preferences/1,
    validate_model_preferences/1,
    apply_preferences_to_handler/2,
    get_default_preferences/0
]).

-include("erlmcp.hrl").

-type validation_error() :: {error, {integer(), binary(), map()}}.
-type preferences() :: #mcp_model_preferences{}.

%%====================================================================
%% API Functions
%%====================================================================

%% Extract model preferences from sampling/createMessage request parameters
%% If modelPreferences field is present, parse and return it
%% If absent, return default preferences
-spec extract_model_preferences(map()) -> {ok, preferences()} | validation_error().
extract_model_preferences(Params) when is_map(Params) ->
    case maps:get(?MCP_PARAM_MODEL_PREFERENCES, Params, undefined) of
        undefined ->
            {ok, get_default_preferences()};
        PrefsMap when is_map(PrefsMap) ->
            parse_model_preferences(PrefsMap);
        _Invalid ->
            {error, {?JSONRPC_INVALID_PARAMS, ?MCP_MSG_INVALID_MODEL_PREFERENCES, #{
                <<"reason">> => <<"modelPreferences must be an object">>
            }}}
    end;
extract_model_preferences(_) ->
    {error, {?JSONRPC_INVALID_PARAMS, ?MCP_MSG_INVALID_MODEL_PREFERENCES, #{
        <<"reason">> => <<"params must be an object">>
    }}}.

%% Validate model preferences record
%% Checks temperature range, maxTokens value, and stopSequences format
-spec validate_model_preferences(preferences()) -> ok | validation_error().
validate_model_preferences(#mcp_model_preferences{
    temperature = Temp,
    max_tokens = MaxTokens,
    stop_sequences = StopSeqs
} = _Prefs) ->
    case validate_temperature(Temp) of
        ok ->
            case validate_max_tokens(MaxTokens) of
                ok ->
                    validate_stop_sequences(StopSeqs);
                Error -> Error
            end;
        Error -> Error
    end.

%% Apply preferences to a sampling handler by passing them as context
%% This allows the handler to use the preferences when generating messages
apply_preferences_to_handler(Prefs, Handler) when is_function(Handler) ->
    try
        Handler(Prefs)
    catch
        Class:Reason:Stack ->
            logger:error("Sampling handler crashed: ~p:~p~n~p", [Class, Reason, Stack]),
            {error, {?JSONRPC_INTERNAL_ERROR, <<"Sampling handler error">>, #{
                <<"exception">> => atom_to_binary(Class, utf8),
                <<"reason">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            }}}
    end.

%% Get default model preferences when none are provided
%% Sensible defaults for general-purpose sampling
-spec get_default_preferences() -> preferences().
get_default_preferences() ->
    #mcp_model_preferences{
        cost_priority = undefined,
        speed_priority = undefined,
        intelligence_priority = undefined,
        temperature = 1.0,      % Moderate randomness
        max_tokens = 4096,      % Reasonable token limit
        stop_sequences = undefined
    }.

%%====================================================================
%% Internal Functions - Parsing
%%====================================================================

-spec parse_model_preferences(map()) -> {ok, preferences()} | validation_error().
parse_model_preferences(PrefsMap) ->
    try
        CostPriority = maps:get(?MCP_PARAM_COST_PRIORITY, PrefsMap, undefined),
        SpeedPriority = maps:get(?MCP_PARAM_SPEED_PRIORITY, PrefsMap, undefined),
        IntelligencePriority = maps:get(?MCP_PARAM_INTELLIGENCE_PRIORITY, PrefsMap, undefined),
        Temperature = maps:get(?MCP_PARAM_TEMPERATURE, PrefsMap, undefined),
        MaxTokens = maps:get(?MCP_PARAM_MAX_TOKENS, PrefsMap, undefined),
        StopSequences = maps:get(?MCP_PARAM_STOP_SEQUENCES, PrefsMap, undefined),

        % Create preferences record with provided values
        Prefs = #mcp_model_preferences{
            cost_priority = normalize_priority(CostPriority),
            speed_priority = normalize_priority(SpeedPriority),
            intelligence_priority = normalize_priority(IntelligencePriority),
            temperature = Temperature,
            max_tokens = MaxTokens,
            stop_sequences = StopSequences
        },

        % Validate the constructed preferences
        case validate_model_preferences(Prefs) of
            ok -> {ok, Prefs};
            Error -> Error
        end
    catch
        Class:Reason ->
            logger:warning("Error parsing model preferences: ~p:~p", [Class, Reason]),
            {error, {?JSONRPC_INVALID_PARAMS, ?MCP_MSG_INVALID_MODEL_PREFERENCES, #{
                <<"reason">> => <<"Failed to parse modelPreferences">>
            }}}
    end.

%%====================================================================
%% Internal Functions - Validation
%%====================================================================

%% Validate temperature is in valid range (0.0 to 2.0) if provided
-spec validate_temperature(float() | undefined) -> ok | validation_error().
validate_temperature(undefined) ->
    ok;
validate_temperature(Temp) when is_number(Temp) ->
    if
        Temp >= 0.0 andalso Temp =< 2.0 ->
            ok;
        true ->
            {error, {?JSONRPC_INVALID_PARAMS, ?MCP_MSG_INVALID_TEMPERATURE, #{
                <<"provided">> => Temp,
                <<"valid_range">> => #{<<"min">> => 0.0, <<"max">> => 2.0}
            }}}
    end;
validate_temperature(_InvalidType) ->
    {error, {?JSONRPC_INVALID_PARAMS, ?MCP_MSG_INVALID_TEMPERATURE, #{
        <<"reason">> => <<"temperature must be a number">>
    }}}.

%% Validate maxTokens is a positive integer if provided
-spec validate_max_tokens(integer() | undefined) -> ok | validation_error().
validate_max_tokens(undefined) ->
    ok;
validate_max_tokens(MaxTokens) when is_integer(MaxTokens) ->
    if
        MaxTokens > 0 ->
            ok;
        true ->
            {error, {?JSONRPC_INVALID_PARAMS, ?MCP_MSG_INVALID_MAX_TOKENS, #{
                <<"provided">> => MaxTokens,
                <<"reason">> => <<"must be positive">>
            }}}
    end;
validate_max_tokens(_InvalidType) ->
    {error, {?JSONRPC_INVALID_PARAMS, ?MCP_MSG_INVALID_MAX_TOKENS, #{
        <<"reason">> => <<"maxTokens must be an integer">>
    }}}.

%% Validate stopSequences is an array of strings if provided
-spec validate_stop_sequences([binary()] | undefined) -> ok | validation_error().
validate_stop_sequences(undefined) ->
    ok;
validate_stop_sequences(StopSeqs) when is_list(StopSeqs) ->
    case lists:all(fun is_binary/1, StopSeqs) of
        true -> ok;
        false ->
            {error, {?JSONRPC_INVALID_PARAMS, ?MCP_MSG_INVALID_STOP_SEQUENCES, #{
                <<"reason">> => <<"all elements must be strings">>
            }}}
    end;
validate_stop_sequences(_InvalidType) ->
    {error, {?JSONRPC_INVALID_PARAMS, ?MCP_MSG_INVALID_STOP_SEQUENCES, #{
        <<"reason">> => <<"stopSequences must be an array">>
    }}}.

%%====================================================================
%% Internal Functions - Normalization
%%====================================================================

%% Normalize priority values (ensure they're in 0.0-1.0 range if provided)
-spec normalize_priority(number() | undefined) -> float() | undefined.
normalize_priority(undefined) ->
    undefined;
normalize_priority(Priority) when is_number(Priority) ->
    % Clamp to 0.0-1.0 range
    case Priority of
        P when P < 0.0 -> 0.0;
        P when P > 1.0 -> 1.0;
        P -> float(P)
    end;
normalize_priority(_) ->
    undefined.
