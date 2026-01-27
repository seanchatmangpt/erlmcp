-module(erlmcp_sampling_strategy).

-include("erlmcp.hrl").

%% API
-export([
    validate_strategy/1,
    is_valid_strategy/1,
    get_valid_strategies/0
]).

-type validation_result() :: ok | {error, {integer(), binary(), map()}}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate a sampling strategy from a request parameter.
%% Returns ok if valid, or an error tuple with JSON-RPC error code -32602.
%% @end
-spec validate_strategy(binary() | undefined) -> validation_result().
validate_strategy(undefined) ->
    %% Strategy is optional - if not provided, use default
    ok;
validate_strategy(Strategy) when is_binary(Strategy) ->
    case is_valid_strategy(Strategy) of
        true ->
            ok;
        false ->
            {error, {
                ?JSONRPC_INVALID_PARAMS,
                ?MCP_MSG_INVALID_SAMPLING_STRATEGY,
                #{
                    <<"provided">> => Strategy,
                    <<"valid_strategies">> => get_valid_strategies(),
                    <<"reason">> => <<"strategy not supported">>
                }
            }}
    end;
validate_strategy(_InvalidType) ->
    {error, {
        ?JSONRPC_INVALID_PARAMS,
        ?MCP_MSG_INVALID_SAMPLING_STRATEGY,
        #{
            <<"reason">> => <<"strategy must be a string">>
        }
    }}.

%% @doc Check if a strategy is valid.
%% Returns true if strategy is in the valid strategies list.
%% @end
-spec is_valid_strategy(binary() | atom() | term()) -> boolean().
is_valid_strategy(Strategy) when is_binary(Strategy) ->
    lists:member(Strategy, ?MCP_VALID_SAMPLING_STRATEGIES);
is_valid_strategy(_) ->
    false.

%% @doc Get the list of valid sampling strategies.
%% @end
-spec get_valid_strategies() -> [binary()].
get_valid_strategies() ->
    ?MCP_VALID_SAMPLING_STRATEGIES.
