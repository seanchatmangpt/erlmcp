-module(erlmcp_dev_portal_sessions).

-behaviour(gen_server).

%% API exports
-export([start_link/0, create/1, get/1, delete/1, cleanup_expired/0, get_active_count/0]).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(Session) ->
    %% Create new session
    gen_server:call(?MODULE, {create, Session}).

get(SessionId) ->
    %% Get session by ID
    gen_server:call(?MODULE, {get, SessionId}).

delete(SessionId) ->
    %% Delete session
    gen_server:call(?MODULE, {delete, SessionId}).

cleanup_expired() ->
    %% Clean up expired sessions
    gen_server:call(?MODULE, cleanup_expired).

get_active_count() ->
    %% Get count of active sessions
    gen_server:call(?MODULE, get_active_count).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize session management
    case mnesia:create_table(sessions, [
        {attributes, record_info(fields, session)},
        {disc_copies, [node()]},
        {type, set},
        {index, #session.user_id}
    ]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, sessions}} ->
            ok
    end,

    %% Set up cleanup timer
    {ok, _TRef} = timer:send_interval(300000, cleanup_expired), % Every 5 minutes

    {ok, #{}}.

handle_call({create, Session}, _From, State) ->
    %% Create new session
    SessionId = maps:get(id, Session),
    case mnesia:dirty_read(sessions, SessionId) of
        [] ->
            case save_session(Session) of
                {atomic, ok} ->
                    {reply, {ok, Session}, State};
                {atomic, {error, Reason}} ->
                    {reply, {error, Reason}, State}
            end;
        [_] ->
            {reply, {error, session_exists}, State}
    end;

handle_call({get, SessionId}, _From, State) ->
    %% Get session by ID
    case mnesia:dirty_read(sessions, SessionId) of
        [Session] ->
            case is_session_valid(Session) of
                true ->
                    {reply, {ok, Session}, State};
                false ->
                    %% Session expired, clean it up
                    delete_session(SessionId),
                    {reply, {error, expired}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(cleanup_expired, _From, State) ->
    %% Clean up expired sessions
    cleanup_expired_sessions(),
    {reply, ok, State};

handle_call(get_active_count, _From, State) ->
    %% Get count of active sessions
    ActiveCount = mnesia:table_info(sessions, size),
    {reply, {ok, ActiveCount}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_expired, State) ->
    %% Clean up expired sessions
    cleanup_expired_sessions(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

is_session_valid(Session) ->
    %% Check if session is valid
    ExpiryTime = maps:get(expires, Session),
    CurrentTime = ?TIMESTAMP(),
    ExpiryTime > CurrentTime.

save_session(Session) ->
    %% Save session to mnesia
    F = fun() ->
        mnesia:write(Session)
    end,
    mnesia:transaction(F).

delete_session(SessionId) ->
    %% Delete session from mnesia
    F = fun() ->
        mnesia:delete({sessions, SessionId})
    end,
    mnesia:transaction(F).

cleanup_expired_sessions() ->
    %% Clean up all expired sessions
    CurrentTime = ?TIMESTAMP(),
    F = fun() ->
        Sessions = mnesia:match_object(#session{_ = '_'}),
        lists:foreach(fun(Session) ->
            case maps:get(expires, Session) < CurrentTime of
                true ->
                    mnesia:delete({sessions, maps:get(id, Session)});
                false ->
                    ok
            end
        end, Sessions)
    end,
    mnesia:transaction(F).