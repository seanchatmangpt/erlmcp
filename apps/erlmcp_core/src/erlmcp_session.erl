-module(erlmcp_session).

-include("erlmcp.hrl").

%% API exports
-export([
    new/0,
    new/1,
    get_session_id/1,
    set_metadata/3,
    get_metadata/2,
    list_sessions/0
]).

%% Types
-type session_id() :: binary().
-type session() :: #{
    id := session_id(),
    created_at := integer(),
    metadata := map()
}.

-export_type([session_id/0, session/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec new() -> session().
new() ->
    new(#{}).

-spec new(map()) -> session().
new(Metadata) ->
    #{
        id => generate_session_id(),
        created_at => erlang:system_time(millisecond),
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

-spec list_sessions() -> [session()].
list_sessions() ->
    %% TODO: Implement persistent session storage
    [].

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_session_id() -> session_id().
generate_session_id() ->
    %% Generate a unique session ID using crypto random bytes
    Rand = crypto:strong_rand_bytes(16),
    binary:encode_hex(Rand).
