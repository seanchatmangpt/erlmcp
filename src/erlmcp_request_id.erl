%% @doc Request ID management with overflow protection
%% Implements safe request ID generation and increment with detection of ID exhaustion
%% P0 Security: Prevents integer overflow and request ID collisions
-module(erlmcp_request_id).

-export([
    safe_increment/1,
    safe_increment/2,
    validate_id/1,
    is_valid_id/1
]).

-include("erlmcp.hrl").

%% Maximum safe request ID (leave room for overflow detection)
%% Using 2^60 to ensure compatibility across all Erlang systems
-define(MAX_SAFE_REQUEST_ID, 1152921504606846975).  % 2^60 - 1

%% Minimum request ID
-define(MIN_REQUEST_ID, 1).

%% @doc Safely increment request ID with overflow protection
%% Returns {ok, NextId} or {error, overflow}
-spec safe_increment(pos_integer()) -> {ok, pos_integer()} | {error, overflow}.
safe_increment(CurrentId) ->
    safe_increment(CurrentId, 1).

%% @doc Safely increment request ID by specified amount
%% Returns {ok, NextId} or {error, overflow}
-spec safe_increment(pos_integer(), pos_integer()) -> {ok, pos_integer()} | {error, overflow}.
safe_increment(CurrentId, Increment) when is_integer(CurrentId), Increment > 0 ->
    NextId = CurrentId + Increment,
    case NextId > ?MAX_SAFE_REQUEST_ID of
        true ->
            {error, overflow};
        false ->
            {ok, NextId}
    end;
safe_increment(CurrentId, Increment) ->
    {error, {invalid_args, CurrentId, Increment}}.

%% @doc Validate request ID is in valid range and well-formed
-spec validate_id(term()) -> ok | {error, invalid_id}.
validate_id(Id) when is_integer(Id), Id >= ?MIN_REQUEST_ID, Id =< ?MAX_SAFE_REQUEST_ID ->
    ok;
validate_id(Id) when is_integer(Id), Id < ?MIN_REQUEST_ID ->
    {error, {id_too_small, Id, ?MIN_REQUEST_ID}};
validate_id(Id) when is_integer(Id), Id > ?MAX_SAFE_REQUEST_ID ->
    {error, {id_too_large, Id, ?MAX_SAFE_REQUEST_ID}};
validate_id(Id) ->
    {error, {invalid_id_type, Id}}.

%% @doc Check if ID is valid without throwing errors
-spec is_valid_id(term()) -> boolean().
is_valid_id(Id) when is_integer(Id), Id >= ?MIN_REQUEST_ID, Id =< ?MAX_SAFE_REQUEST_ID ->
    true;
is_valid_id(_) ->
    false.
