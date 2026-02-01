-module(erlmcp_session_dets).
-behavior(erlmcp_session_backend).

%% Backend callbacks
-export([
    init/1,
    store/3,
    fetch/2,
    delete/2,
    list/1,
    cleanup_expired/1,
    shutdown/1
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
    table_name := atom(),
    file_path := file:filename_all(),
    auto_save := pos_integer()
}.

%%====================================================================
%% Backend Callbacks
%%====================================================================

-spec init(map()) -> {ok, state()} | {error, term()}.
init(Opts) ->
    TableName = maps:get(table_name, Opts, erlmcp_sessions_dets),
    FilePath = maps:get(file_path, Opts, "erlmcp_sessions.dets"),
    AutoSave = maps:get(auto_save, Opts, 60000),

    case dets:open_file(TableName, [
        {file, FilePath},
        {type, set},
        {access, read_write},
        {auto_save, AutoSave}
    ]) of
        {ok, TableName} ->
            {ok, #{
                table_name => TableName,
                file_path => FilePath,
                auto_save => AutoSave
            }};
        {error, Reason} ->
            {error, Reason}
    end.

-spec store(session_id(), session(), state()) ->
    {ok, state()} | {error, term()}.
store(SessionId, Session, State) ->
    case dets:insert(maps:get(table_name, State), {SessionId, Session}) of
        ok -> {ok, State};
        {error, Reason} -> {error, Reason}
    end.

-spec fetch(session_id(), state()) ->
    {ok, session(), state()} | {error, not_found | term(), state()}.
fetch(SessionId, State) ->
    case dets:lookup(maps:get(table_name, State), SessionId) of
        [{SessionId, Session}] ->
            %% Update last accessed time
            UpdatedSession = Session#{last_accessed => erlang:system_time(millisecond)},
            case dets:insert(maps:get(table_name, State), {SessionId, UpdatedSession}) of
                ok -> {ok, UpdatedSession, State};
                {error, Reason} -> {error, Reason, State}
            end;
        [] ->
            {error, not_found, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

-spec delete(session_id(), state()) ->
    {ok, state()} | {error, term(), state()}.
delete(SessionId, State) ->
    case dets:delete(maps:get(table_name, State), SessionId) of
        ok -> {ok, State};
        {error, Reason} -> {error, Reason, State}
    end.

-spec list(state()) ->
    {ok, [session_id()], state()}.
list(State) ->
    case dets:foldl(
        fun({SessionId, _Session}, Acc) -> [SessionId | Acc] end,
        [],
        maps:get(table_name, State)
    ) of
        SessionIds when is_list(SessionIds) ->
            {ok, lists:usort(SessionIds), State};
        {error, Reason} ->
            {error, Reason, State}
    end.

-spec cleanup_expired(state()) ->
    {ok, non_neg_integer(), state()}.
cleanup_expired(State) ->
    Now = erlang:system_time(millisecond),

    case dets:foldl(
        fun({_SessionId, Session}, Acc) ->
            case is_expired(Session, Now) of
                true -> [maps:get(id, Session) | Acc];
                false -> Acc
            end
        end,
        [],
        maps:get(table_name, State)
    ) of
        ExpiredSessions when is_list(ExpiredSessions) ->
            lists:foreach(
                fun(SessionId) ->
                    dets:delete(maps:get(table_name, State), SessionId)
                end,
                ExpiredSessions
            ),
            {ok, length(ExpiredSessions), State};
        {error, Reason} ->
            {ok, 0, State}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec is_expired(session(), integer()) -> boolean().
is_expired(#{timeout_ms := infinity}, _Now) ->
    false;
is_expired(#{last_accessed := LastAccessed, timeout_ms := TimeoutMs}, Now) ->
    (Now - LastAccessed) > TimeoutMs.

-spec shutdown(state()) -> ok.
shutdown(State) ->
    TableName = maps:get(table_name, State),
    case dets:close(TableName) of
        ok ->
            ok;
        {error, Reason} ->
            logger:error("Failed to close DETS table ~p: ~p", [TableName, Reason]),
            ok
    end.
