%%%-------------------------------------------------------------------
%%% @doc
%%% Registry Behavior - Abstract interface for registry backends
%%%
%%% Defines the callbacks required for registry backend implementations.
%%% Supports both local (gproc) and distributed (global+pg) backends.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_registry_behavior).

-include("erlmcp.hrl").

%% Types
-type entity_type() :: server | transport.
-type entity_id() :: server_id() | transport_id().
-type entity_config() :: map().
-type transport_id() :: atom() | binary().

-export_type([entity_type/0, entity_id/0, entity_config/0, transport_id/0]).

%%====================================================================
%% Behavior Callbacks
%%====================================================================

%% @doc Register an entity (server or transport) with the registry
-callback register(Type :: entity_type(),
                   Id :: entity_id(),
                   Pid :: pid(),
                   Config :: entity_config()) ->
                      ok | {error, Reason :: term()}.
%% @doc Unregister an entity from the registry
-callback unregister(Type :: entity_type(), Id :: entity_id()) -> ok.
%% @doc Find an entity in the registry
-callback whereis(Type :: entity_type(), Id :: entity_id()) ->
                     {ok, {Pid :: pid(), Config :: entity_config()}} |
                     {ok, {Node :: node(), Pid :: pid(), Config :: entity_config()}} |
                     {error, not_found}.
%% @doc List all entities of a given type
-callback list(Type :: entity_type()) ->
                  [{Id :: entity_id(), {Pid :: pid(), Config :: entity_config()}}] |
                  [{Id :: entity_id(), {Node :: node(), Pid :: pid(), Config :: entity_config()}}].
%% @doc Update entity configuration
-callback update(Type :: entity_type(), Id :: entity_id(), Config :: entity_config()) ->
                    ok | {error, Reason :: term()}.
%% @doc Join a process group (for distributed backends)
-callback join_group(Group :: atom(), Pid :: pid()) -> ok | {error, Reason :: term()}.
%% @doc Leave a process group (for distributed backends)
-callback leave_group(Group :: atom(), Pid :: pid()) -> ok.
%% @doc Get members of a process group
-callback get_group_members(Group :: atom()) -> [pid()].
%% @doc Check if backend is distributed
-callback is_distributed() -> boolean().
