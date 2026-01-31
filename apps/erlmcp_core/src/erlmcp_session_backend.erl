-module(erlmcp_session_backend).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    store/2,
    fetch/1,
    delete/1,
    list/0,
    cleanup_expired/0
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

%% Behavior callbacks that implementations must export
-callback init(map()) -> {ok, State :: term()} | {error, term()}.
-callback store(session_id(), session(), State :: term()) ->
    {ok, NewState :: term()} | {error, term()}.
-callback fetch(session_id(), State :: term()) ->
    {ok, session(), State :: term()} | {error, not_found | term(), State :: term()}.
-callback delete(session_id(), State :: term()) ->
    {ok, NewState :: term()} | {error, term(), State :: term()}.
-callback list(State :: term()) ->
    {ok, [session_id()], NewState :: term()}.
-callback cleanup_expired(State :: term()) ->
    {ok, Count :: non_neg_integer(), NewState :: term()}.

%% Types
-type session_id() :: binary().
-type session() :: #{
    id := session_id(),
    created_at := integer(),
    last_accessed := integer(),
    timeout_ms := pos_integer() | infinity,
    metadata := map()
}.
-type backend_opts() :: #{
    backend := module(),
    cleanup_interval := pos_integer()
}.

-export_type([session_id/0, session/0, backend_opts/0]).

%% State
-record(state, {
    backend :: module(),
    backend_state :: term(),
    cleanup_timer :: reference() | undefined,
    cleanup_interval :: pos_integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(backend_opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec store(session_id(), session()) -> ok | {error, term()}.
store(SessionId, Session) ->
    gen_server:call(?MODULE, {store, SessionId, Session}).

-spec fetch(session_id()) -> {ok, session()} | {error, not_found | term()}.
fetch(SessionId) ->
    gen_server:call(?MODULE, {fetch, SessionId}).

-spec delete(session_id()) -> ok | {error, term()}.
delete(SessionId) ->
    gen_server:call(?MODULE, {delete, SessionId}).

-spec list() -> {ok, [session_id()]}.
list() ->
    gen_server:call(?MODULE, list).

-spec cleanup_expired() -> {ok, non_neg_integer()}.
cleanup_expired() ->
    gen_server:call(?MODULE, cleanup_expired).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(backend_opts()) -> {ok, #state{}}.
init(Opts) ->
    Backend = maps:get(backend, Opts, erlmcp_session_ets),
    CleanupInterval = maps:get(cleanup_interval, Opts, 60000),

    case Backend:init(maps:without([backend, cleanup_interval], Opts)) of
        {ok, BackendState} ->
            CleanupTimer = schedule_cleanup(CleanupInterval),
            {ok, #state{
                backend = Backend,
                backend_state = BackendState,
                cleanup_timer = CleanupTimer,
                cleanup_interval = CleanupInterval
            }};
        {error, Reason} ->
            {error, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call({store, SessionId, Session}, _From, State) ->
    case (State#state.backend):store(SessionId, Session, State#state.backend_state) of
        {ok, NewBackendState} ->
            {reply, ok, State#state{backend_state = NewBackendState}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({fetch, SessionId}, _From, State) ->
    case (State#state.backend):fetch(SessionId, State#state.backend_state) of
        {ok, Session, NewBackendState} ->
            {reply, {ok, Session}, State#state{backend_state = NewBackendState}};
        {error, not_found, NewBackendState} ->
            {reply, {error, not_found}, State#state{backend_state = NewBackendState}};
        {error, Reason, NewBackendState} ->
            {reply, {error, Reason}, State#state{backend_state = NewBackendState}}
    end;

handle_call({delete, SessionId}, _From, State) ->
    case (State#state.backend):delete(SessionId, State#state.backend_state) of
        {ok, NewBackendState} ->
            {reply, ok, State#state{backend_state = NewBackendState}};
        {error, Reason, NewBackendState} ->
            {reply, {error, Reason}, State#state{backend_state = NewBackendState}}
    end;

handle_call(list, _From, State) ->
    case (State#state.backend):list(State#state.backend_state) of
        {ok, SessionIds, NewBackendState} ->
            {reply, {ok, SessionIds}, State#state{backend_state = NewBackendState}}
    end;

handle_call(cleanup_expired, _From, State) ->
    case (State#state.backend):cleanup_expired(State#state.backend_state) of
        {ok, Count, NewBackendState} ->
            {reply, {ok, Count}, State#state{backend_state = NewBackendState}}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(cleanup_expired, State) ->
    case (State#state.backend):cleanup_expired(State#state.backend_state) of
        {ok, Count, NewBackendState} ->
            case Count > 0 of
                true -> logger:debug("Session cleanup removed ~p expired sessions", [Count]);
                false -> ok
            end,
            NewTimer = schedule_cleanup(State#state.cleanup_interval),
            {noreply, State#state{
                backend_state = NewBackendState,
                cleanup_timer = NewTimer
            }}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.cleanup_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec schedule_cleanup(pos_integer()) -> reference().
schedule_cleanup(IntervalMs) ->
    erlang:send_after(IntervalMs, self(), cleanup_expired).
