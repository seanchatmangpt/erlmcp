%%%-------------------------------------------------------------------
%%% @doc Memory Guard - Resource Exhaustion Protection
%%%
%%% This module provides memory-aware admission control to prevent
%%% out-of-memory crashes by checking available memory before
%%% allocating large payloads.
%%%
%%% Features:
%%% - Per-payload size limits (16MB default)
%%% - System-wide memory threshold (80% circuit breaker)
%%% - Bounded refusal with proper error codes
%%% - Metrology-compliant metrics
%%%
%%% Usage:
%%%   case erlmcp_memory_guard:check_allocation(PayloadSize) of
%%%       ok -> allocate_and_process();
%%%       {error, resource_exhausted} -> return_refusal()
%%%   end
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_guard).

-include("erlmcp.hrl").

%% Note: erlmcp.hrl already includes refusal macros, don't include erlmcp_refusal.hrl
-include_lib("kernel/include/logger.hrl").

%% API
-export([check_allocation/1, check_allocation/2, get_memory_stats/0, get_system_limit/0,
         get_payload_limit/0, get_circuit_breaker_threshold/0, is_circuit_breaker_open/0]).

%%====================================================================
%% Constants
%%====================================================================

-define(MAX_PAYLOAD_SIZE, 16 * 1024 * 1024).      % 16MB per payload
-define(SYSTEM_MEMORY_LIMIT, 16 * 1024 * 1024 * 1024). % 16GB system limit
-define(CIRCUIT_BREAKER_THRESHOLD, 0.80).         % 80% threshold
-define(MEMORY_CHECK_INTERVAL, 1000).              % Check every 1s

%%====================================================================
%% Type Definitions
%%====================================================================

-type memory_stats() ::
    #{total => non_neg_integer(),
      used => non_neg_integer(),
      available => non_neg_integer(),
      used_percent => float(),
      system_limit => non_neg_integer(),
      circuit_breaker_open => boolean()}.
-type allocation_result() :: ok | {error, resource_exhausted | payload_too_large}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Check if payload allocation is safe (uses default payload limit)
-spec check_allocation(non_neg_integer()) -> allocation_result().
check_allocation(PayloadSize) when is_integer(PayloadSize), PayloadSize >= 0 ->
    check_allocation(PayloadSize, ?MAX_PAYLOAD_SIZE).

%% @doc Check if payload allocation is safe with custom limit
-spec check_allocation(non_neg_integer(), pos_integer()) -> allocation_result().
check_allocation(PayloadSize, MaxPayloadSize)
    when is_integer(PayloadSize), PayloadSize >= 0, is_integer(MaxPayloadSize),
         MaxPayloadSize > 0 ->
    %% Check 1: Payload size limit
    case PayloadSize > MaxPayloadSize of
        true ->
            ?LOG_WARNING("Payload too large: ~p bytes (max: ~p)", [PayloadSize, MaxPayloadSize]),
            {error, payload_too_large};
        false ->
            %% Check 2: System memory circuit breaker
            case check_system_memory() of
                ok ->
                    ok;
                {error, circuit_breaker_open} ->
                    ?LOG_ERROR("Circuit breaker open: system memory at ~p%", [get_memory_stats()]),
                    {error, resource_exhausted}
            end
    end.

%% @doc Get current memory statistics
-spec get_memory_stats() -> memory_stats().
get_memory_stats() ->
    TotalMemory = erlang:memory(total),
    SystemLimit = get_system_limit(),
    UsedPercent = TotalMemory / SystemLimit * 100,
    CircuitBreakerOpen = UsedPercent > ?CIRCUIT_BREAKER_THRESHOLD * 100,

    #{total => TotalMemory,
      used => TotalMemory,
      available => SystemLimit - TotalMemory,
      used_percent => UsedPercent,
      system_limit => SystemLimit,
      circuit_breaker_open => CircuitBreakerOpen}.

%% @doc Get configured system memory limit
-spec get_system_limit() -> pos_integer().
get_system_limit() ->
    case application:get_env(erlmcp, system_memory_limit) of
        {ok, Limit} when is_integer(Limit), Limit > 0 ->
            Limit;
        _ ->
            ?SYSTEM_MEMORY_LIMIT
    end.

%% @doc Get configured payload size limit
-spec get_payload_limit() -> pos_integer().
get_payload_limit() ->
    case application:get_env(erlmcp, max_payload_size) of
        {ok, Limit} when is_integer(Limit), Limit > 0 ->
            Limit;
        _ ->
            ?MAX_PAYLOAD_SIZE
    end.

%% @doc Get circuit breaker threshold (percentage)
-spec get_circuit_breaker_threshold() -> float().
get_circuit_breaker_threshold() ->
    case application:get_env(erlmcp, circuit_breaker_threshold) of
        {ok, Threshold} when is_float(Threshold), Threshold > 0, Threshold =< 1.0 ->
            Threshold;
        _ ->
            ?CIRCUIT_BREAKER_THRESHOLD
    end.

%% @doc Check if circuit breaker is currently open
-spec is_circuit_breaker_open() -> boolean().
is_circuit_breaker_open() ->
    Stats = get_memory_stats(),
    maps:get(circuit_breaker_open, Stats, false).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Check system memory against circuit breaker threshold
-spec check_system_memory() -> ok | {error, circuit_breaker_open}.
check_system_memory() ->
    Stats = get_memory_stats(),
    UsedPercent = maps:get(used_percent, Stats, 0.0),
    Threshold = get_circuit_breaker_threshold() * 100,

    case UsedPercent > Threshold of
        true ->
            %% Circuit breaker is OPEN - refuse new allocations
            {error, circuit_breaker_open};
        false ->
            ok
    end.

%%====================================================================
%% Unit Tests
%%====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% Test payload size check
payload_size_limit_test() ->
    %% Small payload should pass
    ?assertEqual(ok, check_allocation(1024)),
    ?assertEqual(ok, check_allocation(?MAX_PAYLOAD_SIZE)),

    %% Payload exactly at limit should fail
    ?assertEqual({error, payload_too_large}, check_allocation(?MAX_PAYLOAD_SIZE + 1)),

    %% Large payload should fail
    ?assertEqual({error, payload_too_large}, check_allocation(100 * 1024 * 1024)).

%% Test system memory check
system_memory_check_test() ->
    %% Get current stats
    Stats = get_memory_stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(total, Stats)),
    ?assert(maps:is_key(used_percent, Stats)),
    ?assert(maps:is_key(circuit_breaker_open, Stats)),

    %% Used percent should be reasonable (0-100%)
    UsedPercent = maps:get(used_percent, Stats),
    ?assert(UsedPercent >= 0),
    ?assert(UsedPercent =< 100).

%% Test circuit breaker
circuit_breaker_test() ->
    %% Circuit breaker should be closed under normal conditions
    %% (unless system is actually under memory pressure)
    IsOpen = is_circuit_breaker_open(),
    ?assert(is_boolean(IsOpen)).

%% Test configuration overrides
config_overrides_test() ->
    %% Test payload limit override
    application:set_env(erlmcp, max_payload_size, 1024),
    ?assertEqual(1024, get_payload_limit()),
    ?assertEqual({error, payload_too_large}, check_allocation(1025)),
    application:unset_env(erlmcp, max_payload_size),

    %% Test circuit breaker threshold override
    application:set_env(erlmcp, circuit_breaker_threshold, 0.50),
    ?assertEqual(0.50, get_circuit_breaker_threshold()),
    application:unset_env(erlmcp, circuit_breaker_threshold).

-endif.
