-module(erlmcp_session_manager).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    create_session/0,
    create_session/1,
    validate_session/1,
    touch_session/1,
    delete_session/1,
    get_session_info/1,
    cleanup_expired_sessions/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(SESSION_TABLE, erlmcp_sessions).

-record(state, {
    session_timeout :: pos_integer(),    % timeout in seconds
    cleanup_interval :: pos_integer()    % cleanup interval in milliseconds
}).

-type session_id() :: binary().
-type validation_result() :: {ok, session_info()} | {error, term()}.
-type session_info() :: #{created_at := integer(), expires_at := integer()}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the session manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Create a new session and return its ID
%% @returns {ok, SessionId} where SessionId is a binary UUID
-spec create_session() -> {ok, session_id()} | {error, term()}.
create_session() ->
    create_session(undefined).

%% @doc Create a new session with optional client ID
%% @param ClientId - Optional client identifier
%% @returns {ok, SessionId} where SessionId is a binary UUID
-spec create_session(binary() | undefined) -> {ok, session_id()} | {error, term()}.
create_session(ClientId) ->
    gen_server:call(?SERVER, {create_session, ClientId}).

%% @doc Validate a session ID
%% @param SessionId - The session ID to validate
%% @returns {ok, valid} if valid, {error, expired | not_found | invalid} otherwise
-spec validate_session(session_id() | string() | binary()) -> {ok, valid} | {error, expired | not_found | invalid}.
validate_session(SessionId) ->
    %% Ensure table exists (in case called before gen_server is fully started)
    ensure_table_exists(),
    NormalizedId = normalize_session_id(SessionId),
    case NormalizedId of
        {error, _} -> {error, invalid};
        _ ->
            case ets:lookup(?SESSION_TABLE, NormalizedId) of
                [{_Id, ExpiresAt}] ->
                    CurrentTime = erlang:system_time(second),
                    case CurrentTime < ExpiresAt of
                        true ->
                            %% Update last activity on validation
                            ets:insert(?SESSION_TABLE, {NormalizedId, ExpiresAt, erlang:system_time(second)}),
                            {ok, valid};
                        false ->
                            %% Session expired, delete it
                            ets:delete(?SESSION_TABLE, NormalizedId),
                            logger:info("Session expired: ~s", [NormalizedId]),
                            {error, expired}
                    end;
                [] ->
                    {error, not_found}
            end
    end.

%% @doc Refresh session timeout (touch)
%% @param SessionId - The session ID to refresh
%% @returns ok if successful, {error, not_found | invalid} otherwise
-spec touch_session(session_id() | string() | binary()) -> ok | {error, not_found | invalid}.
touch_session(SessionId) ->
    gen_server:call(?SERVER, {touch_session, SessionId}).

%% @doc Delete a session
%% @param SessionId - The session ID to delete
%% @returns ok
-spec delete_session(session_id() | string() | binary()) -> ok.
delete_session(SessionId) ->
    NormalizedId = normalize_session_id(SessionId),
    gen_server:cast(?SERVER, {delete_session, NormalizedId}).

%% @doc Get session information
%% @param SessionId - The session ID
%% @returns {ok, SessionInfo} or {error, not_found}
-spec get_session_info(session_id()) -> {ok, session_info()} | {error, not_found}.
get_session_info(SessionId) ->
    %% Ensure table exists (in case called before gen_server is fully started)
    ensure_table_exists(),
    NormalizedId = normalize_session_id(SessionId),
    case NormalizedId of
        {error, _} -> {error, not_found};
        _ ->
            case ets:lookup(?SESSION_TABLE, NormalizedId) of
                [{_Id, ExpiresAt, LastAccessed}] ->
                    {ok, #{
                        id => NormalizedId,
                        created_at => get_created_at(NormalizedId),
                        last_accessed => LastAccessed,
                        expires_at => ExpiresAt
                    }};
                [{_Id, ExpiresAt}] ->
                    %% Old format without last_accessed
                    {ok, #{
                        id => NormalizedId,
                        created_at => get_created_at(NormalizedId),
                        last_accessed => erlang:system_time(second),
                        expires_at => ExpiresAt
                    }};
                [] ->
                    {error, not_found}
            end
    end.

%% @doc Cleanup expired sessions manually
%% @returns {ok, Count} where Count is number of deleted sessions
-spec cleanup_expired_sessions() -> {ok, non_neg_integer()} | {error, term()}.
cleanup_expired_sessions() ->
    gen_server:call(?SERVER, cleanup_expired).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Create ETS table for sessions with {SessionId, ExpiresAt, LastAccessed} tuples
    ets:new(?SESSION_TABLE, [
        named_table,
        public,          % Allow direct read access
        {keypos, 1},
        {read_concurrency, true}
    ]),

    %% Get configuration
    {SessionTimeout, CleanupInterval} = get_config(),

    logger:info("Session manager started (timeout: ~ps, cleanup: ~pms)",
                [SessionTimeout, CleanupInterval]),

    %% Schedule periodic cleanup
    schedule_cleanup(CleanupInterval),

    {ok, #state{
        session_timeout = SessionTimeout,
        cleanup_interval = CleanupInterval
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.

handle_call({create_session, ClientId}, _From, #state{session_timeout = Timeout} = State) ->
    %% Generate session ID as random UUID
    SessionId = generate_session_id(),

    %% Calculate expiry and creation times
    CurrentTime = erlang:system_time(second),
    ExpiresAt = CurrentTime + Timeout,

    %% Store in ETS with metadata: {SessionId, ExpiresAt, LastAccessed}
    ets:insert(?SESSION_TABLE, {SessionId, ExpiresAt, CurrentTime}),

    logger:info("Session created: ~s (client: ~p, expires at ~p)", [SessionId, ClientId, ExpiresAt]),

    {reply, {ok, SessionId}, State};

handle_call({touch_session, SessionId}, _From, #state{session_timeout = Timeout} = State) ->
    NormalizedId = normalize_session_id(SessionId),
    case NormalizedId of
        {error, _} ->
            {reply, {error, invalid}, State};
        _ ->
            case ets:lookup(?SESSION_TABLE, NormalizedId) of
                [{_Id, _ExpiresAt, _LastAccessed}] ->
                    %% Refresh expiry time
                    CurrentTime = erlang:system_time(second),
                    NewExpiresAt = CurrentTime + Timeout,
                    ets:insert(?SESSION_TABLE, {NormalizedId, NewExpiresAt, CurrentTime}),
                    logger:debug("Session refreshed: ~s (new expiry: ~p)", [NormalizedId, NewExpiresAt]),
                    {reply, ok, State};
                [{_Id, _ExpiresAt}] ->
                    %% Old format, update to new format
                    CurrentTime = erlang:system_time(second),
                    NewExpiresAt = CurrentTime + Timeout,
                    ets:insert(?SESSION_TABLE, {NormalizedId, NewExpiresAt, CurrentTime}),
                    {reply, ok, State};
                [] ->
                    {reply, {error, not_found}, State}
            end
    end;

handle_call(cleanup_expired, _From, State) ->
    %% Remove expired sessions
    CurrentTime = erlang:system_time(second),
    MatchSpec = [{{?SESSION_TABLE, '_', '$1', '_'}, [{'<', '$1', CurrentTime}], [true]}],

    %% Manual matching since we need to handle both old and new formats
    AllSessions = ets:match_object(?SESSION_TABLE, {'_', '_', '_'}),
    OldFormatSessions = ets:match_object(?SESSION_TABLE, {'_', '_'}),

    DeletedOld = lists:foldl(fun
        ({_Id, ExpiresAt}, Acc) when ExpiresAt < CurrentTime ->
            Acc + 1;
        (_, Acc) ->
            Acc
    end, 0, OldFormatSessions),

    DeletedNew = lists:foldl(fun
        ({_Id, ExpiresAt, _LastAccessed}, Acc) when ExpiresAt < CurrentTime ->
            Acc + 1;
        (_, Acc) ->
            Acc
    end, 0, AllSessions),

    %% Delete using match_spec approach
    try
        NewFormatCount = ets:select_delete(?SESSION_TABLE,
            [{{'_', '$1', '_'}, [{'<', '$1', CurrentTime}], [true]}]),
        OldFormatCount = ets:select_delete(?SESSION_TABLE,
            [{{'_', '$1'}, [{'<', '$1', CurrentTime}], [true]}]),

        Total = NewFormatCount + OldFormatCount,
        Total > 0 andalso logger:info("Cleaned up ~p expired sessions", [Total]),
        {reply, {ok, Total}, State}
    catch
        _:_ ->
            {reply, {ok, 0}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.

handle_cast({delete_session, SessionId}, State) ->
    ets:delete(?SESSION_TABLE, SessionId),
    logger:info("Session deleted: ~s", [SessionId]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(cleanup, #state{cleanup_interval = CleanupInterval} = State) ->
    %% Remove expired sessions - both old and new formats
    CurrentTime = erlang:system_time(second),

    %% Delete new format: {SessionId, ExpiresAt, LastAccessed}
    try
        NewFormatCount = ets:select_delete(?SESSION_TABLE,
            [{{'_', '$1', '_'}, [{'<', '$1', CurrentTime}], [true]}]),

        %% Delete old format: {SessionId, ExpiresAt}
        OldFormatCount = ets:select_delete(?SESSION_TABLE,
            [{{'_', '$1'}, [{'<', '$1', CurrentTime}], [true]}]),

        Total = NewFormatCount + OldFormatCount,
        Total > 0 andalso logger:debug("Cleaned up ~p expired sessions", [Total])
    catch
        _:_ ->
            logger:warning("Error during session cleanup")
    end,

    %% Schedule next cleanup
    schedule_cleanup(CleanupInterval),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    logger:info("Session manager terminated"),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Get configuration from sys.config
-spec get_config() -> {pos_integer(), pos_integer()}.
get_config() ->
    Config = application:get_env(erlmcp, session_manager, []),
    SessionTimeout = proplists:get_value(timeout, Config, 1800),  % 30 minutes
    CleanupInterval = proplists:get_value(cleanup_interval, Config, 300000),  % 5 minutes
    {SessionTimeout, CleanupInterval}.

%% @private
%% Generate a random session ID as UUID v4
-spec generate_session_id() -> binary().
generate_session_id() ->
    %% Generate 16 random bytes
    RandomBytes = crypto:strong_rand_bytes(16),

    %% UUID v4 format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
    %% where x is random and y is 8, 9, A, or B
    case RandomBytes of
        <<
            A:32, B:16, C:16, D:16, E:48
        >> ->
            %% Set version to 4 and variant bits
            VersionedC = (C band 16#0fff) bor 16#4000,
            VariantD = (D band 16#3fff) bor 16#8000,

            %% Format as UUID string with hex
            UUID = io_lib:format(
                "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                [A, B, VersionedC, VariantD, E]
            ),
            list_to_binary(UUID)
    end.

%% @private
%% Normalize session ID to binary
-spec normalize_session_id(session_id() | string() | binary() | atom()) -> binary() | {error, invalid}.
normalize_session_id(SessionId) when is_binary(SessionId) ->
    SessionId;
normalize_session_id(SessionId) when is_list(SessionId) ->
    list_to_binary(SessionId);
normalize_session_id(SessionId) when is_atom(SessionId) ->
    {error, invalid};
normalize_session_id(_) ->
    {error, invalid}.

%% @private
%% Schedule periodic cleanup
-spec schedule_cleanup(pos_integer()) -> reference().
schedule_cleanup(Interval) ->
    erlang:send_after(Interval, self(), cleanup).

%% @private
%% Get session creation time (metadata stored on first creation)
%% For now, we only track expiration time in ETS
-spec get_created_at(binary()) -> integer().
get_created_at(_SessionId) ->
    %% This would require additional metadata storage
    %% For now, return current time (limitation of simple ETS storage)
    erlang:system_time(second).

%% @private
%% Ensure the ETS table exists (idempotent)
-spec ensure_table_exists() -> ok.
ensure_table_exists() ->
    case ets:info(?SESSION_TABLE) of
        undefined ->
            ets:new(?SESSION_TABLE, [
                named_table,
                public,          % Allow direct read access
                {keypos, 1},
                {read_concurrency, true}
            ]);
        _ ->
            ok
    end.
