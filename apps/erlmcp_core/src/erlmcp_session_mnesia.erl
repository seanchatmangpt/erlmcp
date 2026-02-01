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

    %% disc_copies requires a named node (not nonode@nohost)
    %% Auto-detect and fallback to ram_copies if needed
    UseDiscCopies = DiscCopies andalso (node() =/= 'nonode@nohost'),

    %% Check if table exists with wrong storage type and recreate if needed
    case lists:member(TableName, mnesia:system_info(tables)) of
        true ->
            %% Table exists, check if storage type matches
            %% storage_type returns an atom (disc_copies, ram_copies, etc.)
            CurrentStorage = mnesia:table_info(TableName, storage_type),
            NeedsRecreate = case CurrentStorage of
                disc_copies when not UseDiscCopies -> true;
                ram_copies when UseDiscCopies -> true;
                disc_only_copies when not UseDiscCopies -> true;
                _ -> false
            end,

            case NeedsRecreate of
                true ->
                    %% Delete and recreate with correct storage type
                    {atomic, ok} = mnesia:delete_table(TableName),
                    create_mnesia_table(TableName, Nodes, UseDiscCopies);
                false ->
                    logger:info("Mnesia table already exists: ~p", [TableName]),
                    {ok, #{
                        table_name => TableName,
                        nodes => Nodes,
                        disc_copies => UseDiscCopies
                    }}
            end;
        false ->
            %% Table doesn't exist, create it
            create_mnesia_table(TableName, Nodes, UseDiscCopies)
    end.

%% @private Create Mnesia table with specified storage type
-spec create_mnesia_table(atom(), [node()], boolean()) -> {ok, state()} | {error, term()}.
create_mnesia_table(TableName, Nodes, UseDiscCopies) ->
    case mnesia:create_table(TableName, [
        {attributes, record_info(fields, erlmcp_session)},
        {disc_copies, case UseDiscCopies of true -> Nodes; false -> [] end},
        {ram_copies, case UseDiscCopies of true -> []; false -> Nodes end},
        {type, set}
    ]) of
        {atomic, ok} ->
            logger:info("Created Mnesia table: ~p (storage: ~p)", [TableName,
                case UseDiscCopies of true -> disc_copies; false -> ram_copies end]),
            {ok, #{
                table_name => TableName,
                nodes => Nodes,
                disc_copies => UseDiscCopies
            }};
        {atomic, {already_exists, TableName}} ->
            logger:info("Mnesia table already exists: ~p", [TableName]),
            {ok, #{
                table_name => TableName,
                nodes => Nodes,
                disc_copies => UseDiscCopies
            }};
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec store(session_id(), session(), state()) ->
    {ok, state()} | {error, term()}.
store(SessionId, Session, State) ->
    TableName = maps:get(table_name, State),
    %% Create record with table name as first element for custom table names
    Record = {TableName,
        SessionId,
        Session,
        erlang:system_time(millisecond)
    },

    Transaction = fun() -> mnesia:write(TableName, Record, write) end,

    %% Simple write operation - 5000ms timeout
    case transaction_with_timeout(Transaction, 5000) of
        {atomic, ok} -> {ok, State};
        {aborted, Reason} -> {error, Reason}
    end.

-spec fetch(session_id(), state()) ->
    {ok, session(), state()} | {error, not_found | term(), state()}.
fetch(SessionId, State) ->
    TableName = maps:get(table_name, State),
    Transaction = fun() ->
        case mnesia:read(TableName, SessionId) of
            [{_TableName, SessionId, Session, _LastAccessed}] ->
                %% Update last accessed time
                Now = erlang:system_time(millisecond),
                UpdatedSession = Session#{last_accessed => Now},
                Record = {TableName, SessionId, UpdatedSession, Now},
                mnesia:write(TableName, Record, write),
                {ok, UpdatedSession};
            [] ->
                {error, not_found}
        end
    end,

    %% Read + write operation - 5000ms timeout
    case transaction_with_timeout(Transaction, 5000) of
        {atomic, {ok, Session}} -> {ok, Session, State};
        {atomic, {error, not_found}} -> {error, not_found, State};
        {aborted, Reason} -> {error, Reason, State}
    end.

-spec delete(session_id(), state()) ->
    {ok, state()} | {error, term(), state()}.
delete(SessionId, State) ->
    Transaction = fun() -> mnesia:delete(maps:get(table_name, State), SessionId, write) end,

    %% Simple delete operation - 5000ms timeout
    case transaction_with_timeout(Transaction, 5000) of
        {atomic, ok} -> {ok, State};
        {aborted, Reason} -> {error, Reason, State}
    end.

-spec list(state()) ->
    {ok, [session_id()], state()}.
list(State) ->
    Transaction = fun() ->
        mnesia:all_keys(maps:get(table_name, State))
    end,

    %% List all keys operation - 5000ms timeout
    case transaction_with_timeout(Transaction, 5000) of
        {atomic, SessionIds} -> {ok, SessionIds, State};
        {aborted, Reason} -> {error, Reason, State}
    end.

-spec cleanup_expired(state()) ->
    {ok, non_neg_integer(), state()}.
cleanup_expired(State) ->
    Now = erlang:system_time(millisecond),
    TableName = maps:get(table_name, State),

    Transaction = fun() ->
        %% Get all session IDs
        SessionIds = mnesia:all_keys(TableName),

        %% Find expired sessions
        ExpiredSessions = lists:filter(fun(SessionId) ->
            case mnesia:read(TableName, SessionId) of
                [{_TableName, SessionId, Session, _LastAccessed}] ->
                    is_expired(Session, Now);
                [] ->
                    false
            end
        end, SessionIds),

        %% Delete expired sessions
        lists:foreach(fun(SessionId) ->
            mnesia:delete(TableName, SessionId, write)
        end, ExpiredSessions),

        length(ExpiredSessions)
    end,

    %% Cleanup operation (may involve multiple records) - 10000ms timeout
    case transaction_with_timeout(Transaction, 10000) of
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

%% @private Execute Mnesia transaction with timeout to prevent indefinite hangs.
%% Mnesia transactions don't have built-in timeout support, so we wrap them
%% in a process with a timeout to prevent lock contention or network partition hangs.
%% Timeout: 5000ms for simple read/write, 10000ms for complex operations.
-spec transaction_with_timeout(fun(() -> term()), timeout()) ->
    {atomic, term()} | {aborted, term()}.
transaction_with_timeout(Fun, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn_link(fun() ->
        Result = mnesia:transaction(Fun),
        Parent ! {Ref, Result}
    end),

    receive
        {Ref, Result} -> Result
    after Timeout ->
        exit(Pid, kill),
        {aborted, timeout}
    end.
