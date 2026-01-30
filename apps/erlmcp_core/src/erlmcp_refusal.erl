%%%-------------------------------------------------------------------
%%% @doc erlmcp_refusal - Refusal Message Lookup Module
%%%
%%% Provides functions to retrieve refusal messages and metadata
%%% from the standardized refusal code taxonomy.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_refusal).

%% API
-export([
    get_message/1,
    get_metadata/1,
    format_refusal/1,
    format_refusal/2
]).

-include_lib("kernel/include/logger.hrl").
-include("erlmcp_refusal.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Get refusal message by code
-spec get_message(refusal_code()) -> {ok, binary()} | {error, unknown_code}.
get_message(Code) ->
    case get_metadata(Code) of
        {ok, _Code, _Status, Message, _Hint, _Severity} ->
            {ok, Message};
        Error ->
            Error
    end.

%% @doc Get full refusal metadata by code
-spec get_metadata(refusal_code()) ->
    {ok, refusal_code(), pos_integer(), binary(), binary(), warn | error | critical} |
    {error, unknown_code}.
get_metadata(Code) ->
    case lists:keyfind(Code, 1, ?REFUSAL_METADATA) of
        {Code, Status, Message, Hint, Severity} ->
            {ok, Code, Status, Message, Hint, Severity};
        false ->
            {error, unknown_code}
    end.

%% @doc Format refusal as a binary string
-spec format_refusal(refusal_code()) -> binary().
format_refusal(Code) ->
    case get_metadata(Code) of
        {ok, _Code, _Status, Message, Hint, _Severity} ->
            iolist_to_binary([Message, <<". ">>, Hint]);
        {error, unknown_code} ->
            iolist_to_binary(io_lib:format("Unknown error code: ~w", [Code]))
    end.

%% @doc Format refusal with custom details
-spec format_refusal(refusal_code(), map()) -> binary().
format_refusal(Code, Details) when map_size(Details) =:= 0 ->
    format_refusal(Code);
format_refusal(Code, Details) ->
    Base = format_refusal(Code),
    DetailStr = iolist_to_binary(
        [<<" | Details: ">>,
         jsx:encode(Details)]
    ),
    <<Base/binary, DetailStr/binary>>.
