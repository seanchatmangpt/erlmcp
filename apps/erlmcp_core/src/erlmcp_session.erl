-module(erlmcp_session).

-include("erlmcp.hrl").

%% API exports
-export([
    new/0,
    new/1,
    get_session_id/1,
    set_metadata/3,
    get_metadata/2,
    get_created_at/1,
    list_sessions/0,
    create/1,
    create/2,
    retrieve/1,
    update/2,
    delete/1,
    set_ttl/2,
    cleanup_expired/0,
    get_backend/0
]).

%% Types
-type session_id() :: binary().
-type session() :: #{
    id := session_id(),
    created_at := integer(),
    last_accessed := integer(),
    timeout_ms := pos_integer() | infinity,
    metadata := map()
}.
-type backend() :: ets | dets | leveldb | mnesia.

-export_type([session_id/0, session/0, backend/0]).

%%====================================================================
%% API Functions - Legacy (In-Memory) Interface
%%====================================================================

-spec new() -> session().
new() ->
    new(#{}).

-spec new(map()) -> session().
new(Metadata) ->
    %% SECURITY FIX (P1): Armstrong principle - "make unsafe defaults unrepresentable"
    %% Changed default timeout from 'infinity' to 1 hour (3600000ms)
    %% Previous behavior: sessions never expired (resource exhaustion attack vector)
    %% New behavior: sessions have finite lifetime by default
    #{
        id => generate_session_id(),
        created_at => erlang:system_time(millisecond),
        last_accessed => erlang:system_time(millisecond),
        timeout_ms => 3600000,  % 1 hour default (was: infinity)
        metadata => Metadata
    }.

-spec get_session_id(session()) -> session_id().
get_session_id(#{id := Id}) ->
    Id.

-spec set_metadata(session(), atom() | binary(), term()) -> session().
set_metadata(Session = #{metadata := Meta}, Key, Value) ->
    NewMeta = maps:put(Key, Value, Meta),
    Session#{metadata => NewMeta}.

-spec get_metadata(session(), atom() | binary()) -> term() | undefined.
get_metadata(#{metadata := Meta}, Key) ->
    maps:get(Key, Meta, undefined).

-spec get_created_at(session()) -> integer().
get_created_at(#{created_at := CreatedAt}) ->
    CreatedAt.

-spec list_sessions() -> [session()].
list_sessions() ->
    %% Delegate to session manager for persistent storage
    case whereis(erlmcp_session_manager) of
        undefined -> [];
        _Pid ->
            case erlmcp_session_manager:list_sessions() of
                Sessions when is_list(Sessions) -> Sessions;
                _ -> []
            end
    end.

%%====================================================================
%% API Functions - Persistent Storage Interface
%%====================================================================

%% @doc Create a new session with default TTL (infinity)
-spec create(map()) -> {ok, session_id()} | {error, term()}.
create(Metadata) ->
    create(Metadata, infinity).

%% @doc Create a new session with specified TTL
-spec create(map(), pos_integer() | infinity) -> {ok, session_id()} | {error, term()}.
create(Metadata, TTL) ->
    case whereis(erlmcp_session_manager) of
        undefined -> {error, session_manager_not_running};
        _Pid -> erlmcp_session_manager:create_session(Metadata, TTL)
    end.

%% @doc Retrieve a session by ID
-spec retrieve(session_id()) -> {ok, session()} | {error, not_found | term()}.
retrieve(SessionId) ->
    case whereis(erlmcp_session_manager) of
        undefined -> {error, session_manager_not_running};
        _Pid -> erlmcp_session_manager:get_session(SessionId)
    end.

%% @doc Update a session with a transformation function
-spec update(session_id(), fun((session()) -> session())) -> ok | {error, not_found | term()}.
update(SessionId, UpdateFun) ->
    case whereis(erlmcp_session_manager) of
        undefined -> {error, session_manager_not_running};
        _Pid -> erlmcp_session_manager:update_session(SessionId, UpdateFun)
    end.

%% @doc Delete a session
-spec delete(session_id()) -> ok | {error, term()}.
delete(SessionId) ->
    case whereis(erlmcp_session_manager) of
        undefined -> {error, session_manager_not_running};
        _Pid -> erlmcp_session_manager:delete_session(SessionId)
    end.

%% @doc Set TTL for a session
-spec set_ttl(session_id(), pos_integer() | infinity) -> ok | {error, not_found | term()}.
set_ttl(SessionId, TTL) ->
    case whereis(erlmcp_session_manager) of
        undefined -> {error, session_manager_not_running};
        _Pid -> erlmcp_session_manager:set_timeout(SessionId, TTL)
    end.

%% @doc Cleanup expired sessions
-spec cleanup_expired() -> {ok, non_neg_integer()} | {error, term()}.
cleanup_expired() ->
    case whereis(erlmcp_session_manager) of
        undefined -> {error, session_manager_not_running};
        _Pid -> erlmcp_session_manager:cleanup_expired()
    end.

%% @doc Get the configured backend type
-spec get_backend() -> {ok, backend()} | {error, term()}.
get_backend() ->
    case application:get_env(erlmcp_core, session_backend) of
        {ok, Backend} when Backend =:= ets; Backend =:= dets;
                           Backend =:= leveldb; Backend =:= mnesia ->
            {ok, Backend};
        undefined ->
            {ok, ets};  % Default backend
        {ok, Invalid} ->
            {error, {invalid_backend, Invalid}}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_session_id() -> session_id().
generate_session_id() ->
    %% Generate a unique session ID using crypto random bytes
    Rand = crypto:strong_rand_bytes(16),
    binary:encode_hex(Rand).
