-module(erlmcp_session_ets).
-behavior(erlmcp_session_backend).

%% Backend callbacks
-export([
    init/1,
    store/3,
    fetch/2,
    delete/2,
    list/1,
    cleanup_expired/1
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
-type state() :: #{
    table := ets:tid(),
    table_name := atom()
}.

%%====================================================================
%% Backend Callbacks
%%====================================================================

-spec init(map()) -> {ok, state()} | {error, term()}.
init(Opts) ->
    TableName = maps:get(table_name, Opts, erlmcp_sessions_ets),

    case ets:whereis(TableName) of
        undefined ->
            Table = ets:new(TableName, [
                ordered_set,
                public,
                named_table,
                {read_concurrency, true}
            ]),
            {ok, #{table => Table, table_name => TableName}};
        _Table ->
            %% Table already exists
            {ok, #{table => TableName, table_name => TableName}}
    end.

-spec store(session_id(), session(), state()) ->
    {ok, state()} | {error, term()}.
store(SessionId, Session, State) ->
    try
        TableName = maps:get(table_name, State),
        true = ets:insert(TableName, {SessionId, Session}),
        {ok, State}
    catch
        _:Reason -> {error, Reason}
    end.

-spec fetch(session_id(), state()) ->
    {ok, session(), state()} | {error, not_found | term(), state()}.
fetch(SessionId, State) ->
    TableName = maps:get(table_name, State),
    case ets:lookup(TableName, SessionId) of
        [{SessionId, Session}] ->
            %% Update last accessed time
            UpdatedSession = Session#{last_accessed => erlang:system_time(millisecond)},
            true = ets:insert(TableName, {SessionId, UpdatedSession}),
            {ok, UpdatedSession, State};
        [] ->
            {error, not_found, State}
    end.

-spec delete(session_id(), state()) ->
    {ok, state()} | {error, term(), state()}.
delete(SessionId, State) ->
    ets:delete(maps:get(table_name, State), SessionId),
    {ok, State}.

-spec list(state()) ->
    {ok, [session_id()], state()}.
list(State) ->
    SessionIds = ets:foldl(
        fun({SessionId, _Session}, Acc) -> [SessionId | Acc] end,
        [],
        maps:get(table_name, State)
    ),
    {ok, lists:usort(SessionIds), State}.

-spec cleanup_expired(state()) ->
    {ok, non_neg_integer(), state()}.
cleanup_expired(State) ->
    Now = erlang:system_time(millisecond),

    ExpiredSessions = ets:foldl(
        fun({_SessionId, Session}, Acc) ->
            case is_expired(Session, Now) of
                true -> [maps:get(id, Session) | Acc];
                false -> Acc
            end
        end,
        [],
        maps:get(table_name, State)
    ),

    lists:foreach(
        fun(SessionId) -> ets:delete(maps:get(table_name, State), SessionId) end,
        ExpiredSessions
    ),

    {ok, length(ExpiredSessions), State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec is_expired(session(), integer()) -> boolean().
is_expired(#{timeout_ms := infinity}, _Now) ->
    false;
is_expired(#{last_accessed := LastAccessed, timeout_ms := TimeoutMs}, Now) ->
    (Now - LastAccessed) > TimeoutMs.
