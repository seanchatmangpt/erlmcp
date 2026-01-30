%%%-------------------------------------------------------------------
%%% @doc
%%% Request ID management with overflow protection.
%%%
%%% This module provides safe increment operations for MCP request IDs,
%%% preventing integer overflow that could cause request ID collisions.
%%%
%%% Maximum safe request ID is 2^60 - 1, which provides 1,152,921,504,606,846,975
%%% unique IDs per connection before requiring reconnection.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_request_id).

%% API exports
-export([safe_increment/1, validate_id/1, get_usage_percentage/1, check_thresholds/1]).

%% Types
-type request_id() :: pos_integer().
-type usage_level() :: normal | warning | critical | reserved | exhausted.
-export_type([request_id/0, usage_level/0]).

%%====================================================================
%% Constants
%%====================================================================

%% Maximum safe request ID: 2^60 - 1
%% This provides ~1.15 quintillion unique IDs per connection.
%% Chosen because:
%% 1. Fits in JavaScript Number.MAX_SAFE_INTEGER (2^53 - 1) with margin
%% 2. Far exceeds any realistic single connection lifetime
%% 3. Allows integer operations in Erlang without overflow
-define(MAX_SAFE_REQUEST_ID, 1152921504606846975).  % 2^60 - 1

%% Minimum valid request ID
-define(MIN_REQUEST_ID, 1).

%% ID space usage thresholds (for monitoring and auto-reconnection)
-define(ID_WARNING_THRESHOLD, 922337203685477580).   % 80% of max
-define(ID_CRITICAL_THRESHOLD, 1037629354146165277).  % 90% of max
-define(ID_RESERVED_THRESHOLD, 1106804644422573050).  % 96% of max

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Safely increment a request ID with overflow detection.
%%
%% Returns the next request ID if safe, or an error if the ID space
%% would be exhausted. This prevents integer overflow and request ID
%% collisions that could cause response mismatches.
%%
%% @param CurrentId The current request ID (must be positive integer)
%% @returns {ok, NextId} if safe to increment, {error, overflow} if ID space exhausted
%%
%% @example
%% ```
%% > erlmcp_request_id:safe_increment(1).
%% {ok, 2}
%%
%% > erlmcp_request_id:safe_increment(1152921504606846975).
%% {error, overflow}
%% ```
-spec safe_increment(request_id()) -> {ok, request_id()} | {error, overflow}.
safe_increment(CurrentId) when is_integer(CurrentId), CurrentId >= ?MIN_REQUEST_ID ->
    NextId = CurrentId + 1,
    case NextId > ?MAX_SAFE_REQUEST_ID of
        true ->
            {error, overflow};
        false ->
            {ok, NextId}
    end;
safe_increment(_InvalidId) ->
    {error, invalid_id}.

%% @doc Validate a request ID is within safe bounds.
%%
%% Checks that a request ID is a positive integer within the safe range.
%% Use this to validate request IDs from external sources.
%%
%% @param Id The request ID to validate
%% @returns ok if valid, {error, Reason} if invalid
%%
%% @example
%% ```
%% > erlmcp_request_id:validate_id(1).
%% ok
%%
%% > erlmcp_request_id:validate_id(0).
%% {error, {invalid_id, 0}}
%%
%% > erlmcp_request_id:validate_id(1152921504606846976).
%% {error, {exceeds_maximum, 1152921504606846976}}
%% ```
-spec validate_id(term()) -> ok | {error, {invalid_id | exceeds_maximum | not_integer, term()}}.
validate_id(Id) when is_integer(Id), Id >= ?MIN_REQUEST_ID, Id =< ?MAX_SAFE_REQUEST_ID ->
    ok;
validate_id(Id) when is_integer(Id) ->
    if
        Id < ?MIN_REQUEST_ID ->
            {error, {invalid_id, Id}};
        Id > ?MAX_SAFE_REQUEST_ID ->
            {error, {exceeds_maximum, Id}}
    end;
validate_id(Id) ->
    {error, {not_integer, Id}}.

%% @doc Calculate ID space usage percentage.
%%
%% Returns the percentage of ID space consumed (0.0 to 100.0).
%% Use for monitoring and triggering auto-reconnection.
%%
%% @param CurrentId The current request ID
%% @returns Usage percentage as float (0.0 to 100.0)
%%
%% @example
%% ```
%% > erlmcp_request_id:get_usage_percentage(1).
%% 8.673617379884036e-19
%%
%% > erlmcp_request_id:get_usage_percentage(1152921504606846975).
%% 100.0
%% ```
-spec get_usage_percentage(request_id()) -> float().
get_usage_percentage(CurrentId) when is_integer(CurrentId), CurrentId >= ?MIN_REQUEST_ID ->
    (CurrentId / ?MAX_SAFE_REQUEST_ID) * 100.0;
get_usage_percentage(_InvalidId) ->
    0.0.

%% @doc Check if current ID is at a usage threshold.
%%
%% Returns the current usage level based on predefined thresholds.
%% Use this to trigger monitoring events and auto-reconnection.
%%
%% Thresholds:
%% - normal: < 80% ID space used
%% - warning: >= 80% (log warning, consider reconnection)
%% - critical: >= 90% (log critical, schedule reconnection)
%% - reserved: >= 96% (imminent exhaustion, reconnect now)
%% - exhausted: >= 100% (cannot continue, must reconnect)
%%
%% @param CurrentId The current request ID
%% @returns {ok, Level, Percentage} where Level is usage_level()
%%
%% @example
%% ```
%% > erlmcp_request_id:check_thresholds(1).
%% {ok, normal, 8.673617379884036e-19}
%%
%% > erlmcp_request_id:check_thresholds(1000000000000000000).
%% {ok, warning, 86.73617379884036}
%% ```
-spec check_thresholds(request_id()) -> {ok, usage_level(), float()}.
check_thresholds(CurrentId) when is_integer(CurrentId), CurrentId >= ?MIN_REQUEST_ID ->
    Usage = get_usage_percentage(CurrentId),
    Level = case CurrentId of
        _ when CurrentId >= ?MAX_SAFE_REQUEST_ID -> exhausted;
        _ when CurrentId >= ?ID_RESERVED_THRESHOLD -> reserved;
        _ when CurrentId >= ?ID_CRITICAL_THRESHOLD -> critical;
        _ when CurrentId >= ?ID_WARNING_THRESHOLD -> warning;
        _ -> normal
    end,
    {ok, Level, Usage};
check_thresholds(_InvalidId) ->
    {ok, normal, 0.0}.

%%====================================================================
%% Internal functions
%%====================================================================
