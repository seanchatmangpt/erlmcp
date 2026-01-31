-module(erlmcp_session_mnesia).
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
    table_name := atom(),
    nodes := [node()],
    disc_copies := boolean()
}.

%% Mnesia record definition
-record(erlmcp_session, {
    session_id :: session_id(),
    session_data :: session(),
    last_accessed :: integer()
}).

%%====================================================================
%% Backend Callbacks
%%====================================================================

-spec init(map()) -> {ok, state()} | {error, term()}.
init(Opts) ->
    TableName = maps:get(table_name, Opts, erlmcp_session),
    Nodes = maps:get(nodes, Opts, [node()]),
    DiscCopies = maps:get(disc_copies, Opts, true),

    %% Create table if it doesn't exist
    case mnesia:create_table(TableName, [
        {attributes, record_info(fields, erlmcp_session)},
        {disc_copies, case DiscCopies of true -> Nodes; false -> [] end},
        {ram_copies, case DiscCopies of true -> []; false -> Nodes end},
        {type, set}
    ]) of
        {atomic, ok} ->
            logger:info("Created Mnesia table: ~p", [TableName]),
            {ok, #{
                table_name => TableName,
                nodes => Nodes,
                disc_copies => DiscCopies
            }};
        {atomic, {already_exists, TableName}} ->
            logger:info("Mnesia table already exists: ~p", [TableName]),
            {ok, #{
                table_name => TableName,
                nodes => Nodes,
                disc_copies => DiscCopies
            }};
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec store(session_id(), session(), state()) ->
    {ok, state()} | {error, term()}.
store(SessionId, Session, State) ->
    Record = #erlmcp_session{
        session_id = SessionId,
        session_data = Session,
        last_accessed = erlang:system_time(millisecond)
    },

    Transaction = fun() -> mnesia:write(State.table_name, Record, write) end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} -> {ok, State};
        {aborted, Reason} -> {error, Reason}
    end.

-spec fetch(session_id(), state()) ->
    {ok, session(), state()} | {error, not_found | term(), state()}.
fetch(SessionId, State) ->
    Transaction = fun() ->
        case mnesia:read(State.table_name, SessionId) of
            [#erlmcp_session{session_data = Session}] ->
                %% Update last accessed time
                Now = erlang:system_time(millisecond),
                UpdatedSession = Session#{last_accessed => Now},
                Record = #erlmcp_session{
                    session_id = SessionId,
                    session_data = UpdatedSession,
                    last_accessed = Now
                },
                mnesia:write(State.table_name, Record, write),
                {ok, UpdatedSession};
            [] ->
                {error, not_found}
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, {ok, Session}} -> {ok, Session, State};
        {atomic, {error, not_found}} -> {error, not_found, State};
        {aborted, Reason} -> {error, Reason, State}
    end.

-spec delete(session_id(), state()) ->
    {ok, state()} | {error, term(), state()}.
delete(SessionId, State) ->
    Transaction = fun() -> mnesia:delete(State.table_name, SessionId, write) end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} -> {ok, State};
        {aborted, Reason} -> {error, Reason, State}
    end.

-spec list(state()) ->
    {ok, [session_id()], state()}.
list(State) ->
    Transaction = fun() ->
        mnesia:all_keys(State.table_name)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, SessionIds} -> {ok, SessionIds, State};
        {aborted, Reason} -> {error, Reason, State}
    end.

-spec cleanup_expired(state()) ->
    {ok, non_neg_integer(), state()}.
cleanup_expired(State) ->
    Now = erlang:system_time(millisecond),

    Transaction = fun() ->
        %% Get all session IDs
        SessionIds = mnesia:all_keys(State.table_name),

        %% Find expired sessions
        ExpiredSessions = lists:filter(fun(SessionId) ->
            case mnesia:read(State.table_name, SessionId) of
                [#erlmcp_session{session_data = Session}] ->
                    is_expired(Session, Now);
                [] ->
                    false
            end
        end, SessionIds),

        %% Delete expired sessions
        lists:foreach(fun(SessionId) ->
            mnesia:delete(State.table_name, SessionId, write)
        end, ExpiredSessions),

        length(ExpiredSessions)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Count} -> {ok, Count, State};
        {aborted, Reason} -> {ok, 0, State}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec is_expired(session(), integer()) -> boolean().
is_expired(#{timeout_ms := infinity}, _Now) ->
    false;
is_expired(#{last_accessed := LastAccessed, timeout_ms := TimeoutMs}, Now) ->
    (Now - LastAccessed) > TimeoutMs.
