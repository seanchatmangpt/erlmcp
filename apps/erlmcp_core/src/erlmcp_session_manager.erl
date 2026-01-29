-module(erlmcp_session_manager).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    create_session/1,
    create_session/2,
    get_session/1,
    update_session/2,
    delete_session/1,
    list_sessions/0,
    list_sessions/1,
    cleanup_expired/0,
    set_timeout/2,
    touch_session/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Types
-type session_id() :: binary().
-type session_data() :: #{
    id := session_id(),
    created_at := integer(),
    last_accessed := integer(),
    timeout_ms := pos_integer() | infinity,
    metadata := map(),
    replication_ref => reference()
}.

-export_type([session_id/0, session_data/0]).

%% State record
-record(state, {
    table :: ets:tid(),
    cleanup_timer :: reference() | undefined,
    cleanup_interval_ms = 60000 :: pos_integer(),  % 1 minute
    default_timeout_ms = 3600000 :: pos_integer()  % 1 hour
}).

-define(TABLE_NAME, erlmcp_sessions).
-define(DEFAULT_CLEANUP_INTERVAL, 60000).  % 1 minute
-define(DEFAULT_SESSION_TIMEOUT, 3600000). % 1 hour

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec create_session(map()) -> {ok, session_id()} | {error, term()}.
create_session(Metadata) ->
    create_session(Metadata, ?DEFAULT_SESSION_TIMEOUT).

-spec create_session(map(), pos_integer() | infinity) -> {ok, session_id()} | {error, term()}.
create_session(Metadata, TimeoutMs) ->
    gen_server:call(?MODULE, {create_session, Metadata, TimeoutMs}).

-spec get_session(session_id()) -> {ok, session_data()} | {error, not_found}.
get_session(SessionId) ->
    gen_server:call(?MODULE, {get_session, SessionId}).

-spec update_session(session_id(), fun((session_data()) -> session_data())) -> ok | {error, not_found}.
update_session(SessionId, UpdateFun) ->
    gen_server:call(?MODULE, {update_session, SessionId, UpdateFun}).

-spec delete_session(session_id()) -> ok.
delete_session(SessionId) ->
    gen_server:call(?MODULE, {delete_session, SessionId}).

-spec list_sessions() -> [session_data()].
list_sessions() ->
    gen_server:call(?MODULE, list_sessions).

-spec list_sessions(fun((session_data()) -> boolean())) -> [session_data()].
list_sessions(FilterFun) ->
    gen_server:call(?MODULE, {list_sessions, FilterFun}).

-spec cleanup_expired() -> {ok, non_neg_integer()}.
cleanup_expired() ->
    gen_server:call(?MODULE, cleanup_expired).

-spec set_timeout(session_id(), pos_integer() | infinity) -> ok | {error, not_found}.
set_timeout(SessionId, TimeoutMs) ->
    gen_server:call(?MODULE, {set_timeout, SessionId, TimeoutMs}).

-spec touch_session(session_id()) -> ok | {error, not_found}.
touch_session(SessionId) ->
    gen_server:call(?MODULE, {touch_session, SessionId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),

    %% Create ETS table for session storage
    %% - ordered_set for efficient range queries
    %% - public for direct reads (optional optimization)
    %% - {read_concurrency, true} for better read performance
    Table = ets:new(?TABLE_NAME, [
        ordered_set,
        public,
        named_table,
        {read_concurrency, true},
        {keypos, 2}  % Use session id as key (position 2 in tuple {session_data, id, ...})
    ]),

    %% Start cleanup timer
    CleanupTimer = schedule_cleanup(?DEFAULT_CLEANUP_INTERVAL),

    State = #state{
        table = Table,
        cleanup_timer = CleanupTimer,
        cleanup_interval_ms = ?DEFAULT_CLEANUP_INTERVAL,
        default_timeout_ms = ?DEFAULT_SESSION_TIMEOUT
    },

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.

handle_call({create_session, Metadata, TimeoutMs}, _From, State) ->
    SessionId = generate_session_id(),
    Now = erlang:system_time(millisecond),

    SessionData = #{
        id => SessionId,
        created_at => Now,
        last_accessed => Now,
        timeout_ms => TimeoutMs,
        metadata => Metadata
    },

    true = ets:insert(State#state.table, {SessionData, SessionId}),

    %% Hook for replication (if replicator available)
    notify_replicator({session_created, SessionId, SessionData}),

    {reply, {ok, SessionId}, State};

handle_call({get_session, SessionId}, _From, State) ->
    case ets:lookup(State#state.table, SessionId) of
        [{SessionData, SessionId}] ->
            %% Update last accessed time
            Now = erlang:system_time(millisecond),
            UpdatedData = SessionData#{last_accessed => Now},
            ets:insert(State#state.table, {UpdatedData, SessionId}),
            {reply, {ok, UpdatedData}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({update_session, SessionId, UpdateFun}, _From, State) ->
    case ets:lookup(State#state.table, SessionId) of
        [{SessionData, SessionId}] ->
            try UpdateFun(SessionData) of
                UpdatedData ->
                    Now = erlang:system_time(millisecond),
                    UpdatedDataWithAccess = UpdatedData#{last_accessed => Now},
                    ets:insert(State#state.table, {UpdatedDataWithAccess, SessionId}),

                    %% Hook for replication
                    notify_replicator({session_updated, SessionId, UpdatedDataWithAccess}),

                    {reply, ok, State}
            catch
                _:Reason ->
                    {reply, {error, {update_failed, Reason}}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_session, SessionId}, _From, State) ->
    ets:delete(State#state.table, SessionId),

    %% Hook for replication
    notify_replicator({session_deleted, SessionId}),

    {reply, ok, State};

handle_call(list_sessions, _From, State) ->
    Sessions = ets:foldl(
        fun({SessionData, _SessionId}, Acc) ->
            [SessionData | Acc]
        end,
        [],
        State#state.table
    ),
    {reply, Sessions, State};

handle_call({list_sessions, FilterFun}, _From, State) ->
    Sessions = ets:foldl(
        fun({SessionData, _SessionId}, Acc) ->
            case FilterFun(SessionData) of
                true -> [SessionData | Acc];
                false -> Acc
            end
        end,
        [],
        State#state.table
    ),
    {reply, Sessions, State};

handle_call(cleanup_expired, _From, State) ->
    Count = do_cleanup_expired(State),
    {reply, {ok, Count}, State};

handle_call({set_timeout, SessionId, TimeoutMs}, _From, State) ->
    case ets:lookup(State#state.table, SessionId) of
        [{SessionData, SessionId}] ->
            UpdatedData = SessionData#{timeout_ms => TimeoutMs},
            ets:insert(State#state.table, {UpdatedData, SessionId}),

            %% Hook for replication
            notify_replicator({session_updated, SessionId, UpdatedData}),

            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({touch_session, SessionId}, _From, State) ->
    case ets:lookup(State#state.table, SessionId) of
        [{SessionData, SessionId}] ->
            Now = erlang:system_time(millisecond),
            UpdatedData = SessionData#{last_accessed => Now},
            ets:insert(State#state.table, {UpdatedData, SessionId}),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(cleanup_expired, State) ->
    Count = do_cleanup_expired(State),
    %% Log cleanup if any sessions were removed
    case Count > 0 of
        true -> logger:debug("Session cleanup removed ~p expired sessions", [Count]);
        false -> ok
    end,
    NewTimer = schedule_cleanup(State#state.cleanup_interval_ms),
    {noreply, State#state{cleanup_timer = NewTimer}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    %% Cancel cleanup timer
    _ = case State#state.cleanup_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,

    %% Delete ETS table
    ets:delete(State#state.table),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_session_id() -> session_id().
generate_session_id() ->
    %% Generate a unique session ID using crypto random bytes
    Rand = crypto:strong_rand_bytes(16),
    binary:encode_hex(Rand).

-spec schedule_cleanup(pos_integer()) -> reference().
schedule_cleanup(IntervalMs) ->
    erlang:send_after(IntervalMs, self(), cleanup_expired).

-spec do_cleanup_expired(#state{}) -> non_neg_integer().
do_cleanup_expired(State) ->
    Now = erlang:system_time(millisecond),

    %% Find expired sessions
    ExpiredSessions = ets:foldl(
        fun({SessionData, SessionId}, Acc) ->
            case is_expired(SessionData, Now) of
                true -> [SessionId | Acc];
                false -> Acc
            end
        end,
        [],
        State#state.table
    ),

    %% Delete expired sessions
    lists:foreach(
        fun(SessionId) ->
            ets:delete(State#state.table, SessionId),
            notify_replicator({session_expired, SessionId})
        end,
        ExpiredSessions
    ),

    length(ExpiredSessions).

-spec is_expired(session_data(), integer()) -> boolean().
is_expired(#{timeout_ms := infinity}, _Now) ->
    false;
is_expired(#{last_accessed := LastAccessed, timeout_ms := TimeoutMs}, Now) ->
    (Now - LastAccessed) > TimeoutMs.

-spec notify_replicator(term()) -> ok.
notify_replicator(Event) ->
    %% Check if replicator is available
    case whereis(erlmcp_session_replicator) of
        undefined ->
            %% Replicator not running, no-op
            ok;
        Pid when is_pid(Pid) ->
            %% Send event to replicator for failover module
            Pid ! {session_event, Event},
            ok
    end.
