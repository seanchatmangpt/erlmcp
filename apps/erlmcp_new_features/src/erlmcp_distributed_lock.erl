-module(erlmcp_distributed_lock).
-behaviour(gen_server).

%% API exports
-export([start_link/0, start_link/1,
         acquire/1, acquire/2,
         release/1,
         status/0,
         status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TTL_MS, 30000).
-define(DEFAULT_WAIT_TIMEOUT_MS, 30000).
-define(DEFAULT_RETRY_INTERVAL_MS, 500).

-record(lock, {
    name :: binary(),
    owner :: {node(), pid()},
    acquired_at :: integer(),
    expires_at :: integer()
}).

-record(lock_request, {
    request_id :: reference(),
    requester :: pid(),
    requested_at :: integer(),
    priority = 0 :: integer()
}).

-record(state, {
    locks = #{} :: #{binary() => #lock{}},
    pending = #{} :: #{binary() => [#lock_request{}]},
    next_request_id = 1 :: integer()
}).

-type state() :: #state{}.
-type lock_name() :: binary().
-type lock_options() :: #{
    ttl_ms => pos_integer(),
    wait_timeout_ms => pos_integer(),
    retry_interval_ms => pos_integer(),
    auto_extend => boolean()
}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

-spec acquire(lock_name()) -> {ok, reference()} | {error, term()}.
acquire(LockName) when is_binary(LockName) ->
    acquire(LockName, #{}).

-spec acquire(lock_name(), lock_options()) -> {ok, reference()} | {error, term()}.
acquire(LockName, Options) when is_binary(LockName), is_map(Options) ->
    WaitTimeout = maps:get(wait_timeout_ms, Options, ?DEFAULT_WAIT_TIMEOUT_MS),
    gen_server:call(?SERVER, {acquire, LockName, Options}, WaitTimeout + 1000).

-spec release(reference()) -> ok | {error, term()}.
release(LockRef) when is_reference(LockRef) ->
    gen_server:call(?SERVER, {release, LockRef}).

-spec status() -> #{binary() => map()}.
status() ->
    gen_server:call(?SERVER, status_all).

-spec status(lock_name()) -> {ok, map()} | {error, not_found}.
status(LockName) when is_binary(LockName) ->
    gen_server:call(?SERVER, {status, LockName}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([] | proplists:proplist()) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    logger:info("Starting distributed lock manager"),
    {ok, #state{}};
init(_Opts) ->
    process_flag(trap_exit, true),
    logger:info("Starting distributed lock manager with options"),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call({acquire, LockName, Options}, {Requester, _Tag}, State) ->
    TTL = maps:get(ttl_ms, Options, ?DEFAULT_TTL_MS),
    _RetryInterval = maps:get(retry_interval_ms, Options, ?DEFAULT_RETRY_INTERVAL_MS),
    WaitTimeout = maps:get(wait_timeout_ms, Options, ?DEFAULT_WAIT_TIMEOUT_MS),

    Now = erlang:system_time(millisecond),

    case maps:get(LockName, State#state.locks, undefined) of
        undefined ->
            %% Lock is available - acquire it immediately
            NewLock = #lock{
                name = LockName,
                owner = {node(), Requester},
                acquired_at = Now,
                expires_at = Now + TTL
            },
            NewLocks = maps:put(LockName, NewLock, State#state.locks),

            %% Set up expiration timer
            _ExpirationRef = erlang:send_after(TTL, self(), {lock_expire, LockName}),

            %% Monitor requester for cleanup
            _MonitorRef = monitor(process, Requester),

            logger:info("Lock ~p acquired by ~p", [LockName, Requester]),

            {reply, {ok, make_ref()}, State#state{locks = NewLocks}, hibernate};

        #lock{expires_at = ExpiresAt} when ExpiresAt =< Now ->
            %% Lock has expired - remove and acquire
            NewLocks = maps:remove(LockName, State#state.locks),

            Lock = #lock{
                name = LockName,
                owner = {node(), Requester},
                acquired_at = Now,
                expires_at = Now + TTL
            },
            NewLocks2 = maps:put(LockName, Lock, NewLocks),

            logger:info("Lock ~p (expired) acquired by ~p", [LockName, Requester]),

            {reply, {ok, make_ref()}, State#state{locks = NewLocks2}, hibernate};

        #lock{} ->
            %% Lock is held - queue request or return busy
            case WaitTimeout of
                0 ->
                    {reply, {error, locked}, State, hibernate};
                _ ->
                    %% Queue the request
                    RequestId = make_ref(),
                    LockRequest = #lock_request{
                        request_id = RequestId,
                        requester = Requester,
                        requested_at = Now,
                        priority = maps:get(priority, Options, 0)
                    },

                    PendingList = maps:get(LockName, State#state.pending, []),
                    %% Insert sorted by priority (higher first)
                    NewPendingList = insert_by_priority(LockRequest, PendingList),

                    NewPending = maps:put(LockName, NewPendingList, State#state.pending),

                    %% Set up wait timeout
                    erlang:send_after(WaitTimeout, self(), {wait_timeout, LockName, RequestId}),

                    logger:info("Lock ~p busy - request ~p queued for ~p",
                               [LockName, RequestId, Requester]),

                    %% Send pending reply - will be resolved when lock is available
                    {noreply, State#state{pending = NewPending}}
            end
    end;

handle_call({release, LockRef}, {Requester, _Tag}, State) ->
    %% Find lock owned by requester and release it
    LockName = find_lock_by_owner(LockRef, Requester, State#state.locks),

    case LockName of
        undefined ->
            {reply, {error, not_found}, State};
        Name ->
            {ok, NewState} = do_release_lock(Name, Requester, State),
            {reply, ok, NewState, hibernate}
    end;

handle_call(status_all, _From, State) ->
    Status = maps:map(fun(_Name, Lock) ->
        #{
            owner => element(1, Lock#lock.owner),
            pid => element(2, Lock#lock.owner),
            acquired_at => Lock#lock.acquired_at,
            expires_at => Lock#lock.expires_at,
            ttl_ms => Lock#lock.expires_at - erlang:system_time(millisecond)
        }
    end, State#state.locks),

    {reply, Status, State, hibernate};

handle_call({status, LockName}, _From, State) ->
    case maps:get(LockName, State#state.locks, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Lock ->
            Status = #{
                name => Lock#lock.name,
                owner => Lock#lock.owner,
                acquired_at => Lock#lock.acquired_at,
                expires_at => Lock#lock.expires_at,
                ttl_ms => Lock#lock.expires_at - erlang:system_time(millisecond)
            },
            {reply, {ok, Status}, State, hibernate}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({lock_expire, LockName}, State) ->
    Now = erlang:system_time(millisecond),

    case maps:get(LockName, State#state.locks, undefined) of
        #lock{expires_at = ExpiresAt} when ExpiresAt =< Now ->
            %% Lock has expired
            logger:info("Lock ~p expired at ~p", [LockName, ExpiresAt]),

            NewLocks = maps:remove(LockName, State#state.locks),
            NewState = State#state{locks = NewLocks},

            %% Process pending requests
            {noreply, process_pending_requests(LockName, NewState)};
        _ ->
            %% Lock was renewed or released - ignore
            {noreply, State}
    end;

handle_info({wait_timeout, LockName, RequestId}, State) ->
    %% Remove timed-out request from pending queue
    case maps:get(LockName, State#state.pending, undefined) of
        undefined ->
            {noreply, State};
        PendingList ->
            NewPendingList = lists:filter(
                fun(#lock_request{request_id = RID}) -> RID =/= RequestId end,
                PendingList
            ),

            NewPending = case NewPendingList of
                [] -> maps:remove(LockName, State#state.pending);
                _ -> maps:put(LockName, NewPendingList, State#state.pending)
            end,

            logger:info("Lock ~p request ~p timed out", [LockName, RequestId]),

            {noreply, State#state{pending = NewPending}}
    end;

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    %% Owner process died - release its locks
    LockNames = find_locks_by_owner_pid(Pid, State#state.locks),

    NewState = lists:foldl(
        fun(LockName, AccState) ->
            logger:warning("Lock ~p owner ~p died - releasing", [LockName, Pid]),
            {ok, RelState} = do_release_lock(LockName, Pid, AccState),
            RelState
        end,
        State,
        LockNames
    ),

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    logger:info("Distributed lock manager terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec do_release_lock(binary(), pid(), state()) -> {ok, state()}.
do_release_lock(LockName, Requester, State) ->
    case maps:get(LockName, State#state.locks, undefined) of
        undefined ->
            {ok, State};
        #lock{owner = {_Node, Pid}} when Pid =:= Requester ->
            logger:info("Lock ~p released by ~p", [LockName, Requester]),

            NewLocks = maps:remove(LockName, State#state.locks),
            NewState = State#state{locks = NewLocks},

            %% Process pending requests
            {ok, process_pending_requests(LockName, NewState)};
        _ ->
            %% Owner mismatch
            {ok, State}
    end.

-spec process_pending_requests(binary(), state()) -> state().
process_pending_requests(LockName, State) ->
    case maps:get(LockName, State#state.pending, undefined) of
        undefined ->
            State;
        [] ->
            State;
        [NextRequest | Rest] ->
            %% Grant lock to next in queue
            #lock_request{requester = Requester} = NextRequest,

            TTL = ?DEFAULT_TTL_MS,
            Now = erlang:system_time(millisecond),

            Lock = #lock{
                name = LockName,
                owner = {node(), Requester},
                acquired_at = Now,
                expires_at = Now + TTL
            },

            NewLocks = maps:put(LockName, Lock, State#state.locks),

            %% Update pending queue
            NewPending = case Rest of
                [] -> maps:remove(LockName, State#state.pending);
                _ -> maps:put(LockName, Rest, State#state.pending)
            end,

            logger:info("Lock ~p granted to queued request ~p", [LockName, Requester]),

            %% Notify requester
            Requester ! {lock_acquired, LockName, make_ref()},

            State#state{locks = NewLocks, pending = NewPending}
    end.

-spec find_lock_by_owner(reference(), pid(), #{binary() => #lock{}}) -> binary() | undefined.
find_lock_by_owner(_LockRef, _Requester, Locks) ->
    %% Simplified lookup - in production would track lock refs
    maps:fold(
        fun(Name, #lock{owner = {_Node, Pid}}, Acc) when Pid =:= _Requester ->
                case Acc of
                    undefined -> Name;
                    _ -> Acc
                end;
           (_, _, Acc) ->
                Acc
        end,
        undefined,
        Locks
    ).

-spec find_locks_by_owner_pid(pid(), #{binary() => #lock{}}) -> [binary()].
find_locks_by_owner_pid(Pid, Locks) ->
    maps:fold(
        fun(Name, #lock{owner = {_Node, Owner}}, Acc) when Owner =:= Pid ->
                [Name | Acc];
           (_, _, Acc) ->
                Acc
        end,
        [],
        Locks
    ).

-spec insert_by_priority(#lock_request{}, [#lock_request{}]) -> [#lock_request{}].
insert_by_priority(Request, []) ->
    [Request];
insert_by_priority(Request = #lock_request{priority = P}, [H = #lock_request{priority = HP} | T]) when P > HP ->
    [Request, H | T];
insert_by_priority(Request, [H | T]) ->
    [H | insert_by_priority(Request, T)].
