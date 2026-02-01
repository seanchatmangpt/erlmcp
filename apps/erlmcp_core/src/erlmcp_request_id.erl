-module(erlmcp_request_id).

%%% Request ID overflow protection for MCP clients
%%% Prevents request ID space exhaustion (P0 SECURITY)
%%% Per RPN 720: Request ID Overflow Fix

-export([safe_increment/1, check_thresholds/1]).

-include("erlmcp.hrl").

%% Threshold percentages for monitoring
-define(WARNING_THRESHOLD, 80).   %% 80%
-define(CRITICAL_THRESHOLD, 90).  %% 90%
-define(RESERVED_THRESHOLD, 96).  %% 96%

-type request_id() :: pos_integer().
-type threshold_level() :: normal | warning | critical | reserved.
-type usage_percentage() :: float().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Safely increment request ID with overflow detection
%% Returns {ok, NextId} or {error, overflow}
-spec safe_increment(request_id()) -> {ok, request_id()} | {error, overflow}.
safe_increment(RequestId) when RequestId >= ?MAX_SAFE_REQUEST_ID ->
    {error, overflow};
safe_increment(RequestId) when RequestId >= 0 ->
    {ok, RequestId + 1}.

%% @doc Check if request ID has reached warning thresholds
%% Returns {ok, Level, UsagePercentage}
-spec check_thresholds(request_id()) -> {ok, threshold_level(), usage_percentage()}.
check_thresholds(RequestId) ->
    Usage = RequestId / ?MAX_SAFE_REQUEST_ID * 100.0,
    Level = determine_threshold_level(Usage),
    {ok, Level, Usage}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Determine threshold level based on usage percentage
-spec determine_threshold_level(usage_percentage()) -> threshold_level().
determine_threshold_level(Usage) when Usage >= ?RESERVED_THRESHOLD ->
    reserved;
determine_threshold_level(Usage) when Usage >= ?CRITICAL_THRESHOLD ->
    critical;
determine_threshold_level(Usage) when Usage >= ?WARNING_THRESHOLD ->
    warning;
determine_threshold_level(_Usage) ->
    normal.
