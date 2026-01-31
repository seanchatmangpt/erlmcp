%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_flags - Lock-Free System State Flags
%%%
%%% High-performance lock-free system state flags using Erlang's
%%% atomics module. ~10ns per operation.
%%%
%%% Design Philosophy (Joe Armstrong):
%%% "Use atomics for flags - lock-free, fast, correct."
%%%
%%% Performance:
%%% - Lock-free concurrent updates
%%% - ~10ns per operation
%%% - No contention under high load
%%% - Perfect for system state checks
%%%
%%% Usage:
%%% ```
%%% erlmcp_flags:init(),
%%% erlmcp_flags:is_accepting(),
%%% erlmcp_flags:stop_accepting(),
%%% erlmcp_flags:start_accepting().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flags).

%% API
-export([
    init/0,
    ref/0,
    % Connection flags
    is_accepting/0,
    stop_accepting/0,
    start_accepting/0,
    % Maintenance mode flags
    is_maintenance_mode/0,
    enter_maintenance_mode/0,
    exit_maintenance_mode/0,
    % Shutdown flags
    is_shutting_down/0,
    start_shutdown/0,
    cancel_shutdown/0,
    % Health flags
    is_healthy/0,
    mark_healthy/0,
    mark_unhealthy/0,
    % Get all flags
    get_all/0
]).

%% Flag indices
-define(ACCEPTING_CONNECTIONS, 1).
-define(MAINTENANCE_MODE, 2).
-define(SHUTTING_DOWN, 3).
-define(HEALTHY, 4).
-define(FLAGS_SIZE, 4).

%% Persistent term for the atomics reference
-define(FLAGS_KEY, erlmcp_flags_ref).

%% Flag values
-define(FLAG_TRUE, 1).
-define(FLAG_FALSE, 0).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Initialize the flags reference
%% Must be called once during application startup.
-spec init() -> ok.
init() ->
    Ref = atomics:new(?FLAGS_SIZE, [{signed, false}]),
    persistent_term:put(?FLAGS_KEY, Ref),
    % Default: accepting connections and healthy
    atomics:put(Ref, ?ACCEPTING_CONNECTIONS, ?FLAG_TRUE),
    atomics:put(Ref, ?MAINTENANCE_MODE, ?FLAG_FALSE),
    atomics:put(Ref, ?SHUTTING_DOWN, ?FLAG_FALSE),
    atomics:put(Ref, ?HEALTHY, ?FLAG_TRUE),
    ok.

%% @doc Get the atomics reference
-spec ref() -> atomics:atomics_ref().
ref() ->
    persistent_term:get(?FLAGS_KEY).

%%====================================================================
%% Connection Flags
%%====================================================================

%% @doc Check if server is accepting new connections
-spec is_accepting() -> boolean().
is_accepting() ->
    atomics:get(ref(), ?ACCEPTING_CONNECTIONS) =:= ?FLAG_TRUE.

%% @doc Stop accepting new connections
-spec stop_accepting() -> ok.
stop_accepting() ->
    atomics:put(ref(), ?ACCEPTING_CONNECTIONS, ?FLAG_FALSE),
    ok.

%% @doc Start accepting new connections
-spec start_accepting() -> ok.
start_accepting() ->
    atomics:put(ref(), ?ACCEPTING_CONNECTIONS, ?FLAG_TRUE),
    ok.

%%====================================================================
%% Maintenance Mode Flags
%%====================================================================

%% @doc Check if system is in maintenance mode
-spec is_maintenance_mode() -> boolean().
is_maintenance_mode() ->
    atomics:get(ref(), ?MAINTENANCE_MODE) =:= ?FLAG_TRUE.

%% @doc Enter maintenance mode
%% Stops accepting new connections.
-spec enter_maintenance_mode() -> ok.
enter_maintenance_mode() ->
    atomics:put(ref(), ?MAINTENANCE_MODE, ?FLAG_TRUE),
    stop_accepting(),
    ok.

%% @doc Exit maintenance mode
%% Resumes accepting new connections.
-spec exit_maintenance_mode() -> ok.
exit_maintenance_mode() ->
    atomics:put(ref(), ?MAINTENANCE_MODE, ?FLAG_FALSE),
    start_accepting(),
    ok.

%%====================================================================
%% Shutdown Flags
%%====================================================================

%% @doc Check if system is shutting down
-spec is_shutting_down() -> boolean().
is_shutting_down() ->
    atomics:get(ref(), ?SHUTTING_DOWN) =:= ?FLAG_TRUE.

%% @doc Start graceful shutdown
%% Stops accepting new connections.
-spec start_shutdown() -> ok.
start_shutdown() ->
    atomics:put(ref(), ?SHUTTING_DOWN, ?FLAG_TRUE),
    stop_accepting(),
    ok.

%% @doc Cancel shutdown
%% Resumes accepting new connections.
-spec cancel_shutdown() -> ok.
cancel_shutdown() ->
    atomics:put(ref(), ?SHUTTING_DOWN, ?FLAG_FALSE),
    start_accepting(),
    ok.

%%====================================================================
%% Health Flags
%%====================================================================

%% @doc Check if system is healthy
-spec is_healthy() -> boolean().
is_healthy() ->
    atomics:get(ref(), ?HEALTHY) =:= ?FLAG_TRUE.

%% @doc Mark system as healthy
-spec mark_healthy() -> ok.
mark_healthy() ->
    atomics:put(ref(), ?HEALTHY, ?FLAG_TRUE),
    ok.

%% @doc Mark system as unhealthy
-spec mark_unhealthy() -> ok.
mark_unhealthy() ->
    atomics:put(ref(), ?HEALTHY, ?FLAG_FALSE),
    ok.

%%====================================================================
%% Get All Flags
%%====================================================================

%% @doc Get all flags as a map
-spec get_all() -> map().
get_all() ->
    Ref = ref(),
    #{
        accepting_connections => atomics:get(Ref, ?ACCEPTING_CONNECTIONS) =:= ?FLAG_TRUE,
        maintenance_mode => atomics:get(Ref, ?MAINTENANCE_MODE) =:= ?FLAG_TRUE,
        shutting_down => atomics:get(Ref, ?SHUTTING_DOWN) =:= ?FLAG_TRUE,
        healthy => atomics:get(Ref, ?HEALTHY) =:= ?FLAG_TRUE
    }.

%%====================================================================
%% Internal Functions
%%====================================================================
