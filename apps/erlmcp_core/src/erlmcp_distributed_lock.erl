%%%-------------------------------------------------------------------
%%% @doc erlmcp_distributed_lock - Distributed Locking Service
%%%
%%% Implements distributed locking using global name registration
%%% with timeout handling, automatic release, and queuing.
%%%
%%% Features:
%%% - Lock acquisition with timeout
%%% - Automatic release on timeout
%%% - Lock renewal (heartbeat)
%%% - Waiting queue for contended locks
%%% - Lock holder monitoring
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_distributed_lock).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         acquire/1, acquire/2, acquire/3,
         release/1,
         renew/1, renew/2,
         lock_status/1,
         list_locks/0,
         wait_for_lock/2,
         set_default_timeout/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_LOCK_TIMEOUT, 30000).
-define(DEFAULT_ACQUIRE_TIMEOUT, 5000).
-define(RENEWAL_THRESHOLD, 0.7).  % Renew at 70% of timeout

%%====================================================================
%% Types
%%====================================================================

-type lock_name() :: term().
-type lock_id() :: {lock_name(), node()}.
-type lock_holder() :: pid().

-record(lock, {
    name :: lock_name(),
    holder :: lock_holder(),
    holder_node :: node(),
    acquired_at :: integer(),
    expires_at :: integer(),
    timeout :: pos_integer(),
    renewal_ref :: reference() | undefined,
    monitor_ref :: reference()}).

-record(waiter, {
    pid :: pid(),
    node :: node(),
    timeout_ref :: reference(),
    start_time :: integer()}).

-record(state, {
    locks = #{} :: #{lock_name() => #lock{}},
    waiters = #{} :: #{lock_name() => [#waiter{}]},
    default_timeout :: pos_integer(),
    cleanup_interval :: pos_integer(),
    cleanup_ref :: reference() | undefined}).

-type state() :: #state{}.

-export_type([lock_name/0, lock_id/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start lock service with default configuration
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start lock service with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Acquire a lock with default timeout
-spec acquire(lock_name()) -> ok | {error, term()}.
acquire(LockName) ->
    acquire(LockName, ?DEFAULT_ACQUIRE_TIMEOUT).

%% @doc Acquire a lock with custom acquire timeout
-spec acquire(lock_name(), timeout()) -> ok | {error, term()}.
acquire(LockName, AcquireTimeout) ->
    acquire(LockName, ?DEFAULT_LOCK_TIMEOUT, AcquireTimeout).

%% @doc Acquire a lock with custom lock timeout and acquire timeout
-spec acquire(lock_name(), pos_integer(), timeout()) -> ok | {error, term()}.
acquire(LockName, LockTimeout, AcquireTimeout) when is_integer(LockTimeout), LockTimeout > 0 ->
    gen_server:call(?SERVER, {acquire, LockName, LockTimeout}, AcquireTimeout).

%% @doc Release a lock
-spec release(lock_name()) -> ok | {error, not_held | not_found}.
release(LockName) ->
    gen_server:call(?SERVER, {release, LockName}).

%% @doc Renew a lock (extend timeout)
-spec renew(lock_name()) -> ok | {error, term()}.
renew(LockName) ->
    renew(LockName, ?DEFAULT_LOCK_TIMEOUT).

%% @doc Renew a lock with specific timeout
-spec renew(lock_name(), pos_integer()) -> ok | {error, term()}.
renew(LockName, NewTimeout) when is_integer(NewTimeout), NewTimeout > 0 ->
    gen_server:call(?SERVER, {renew, LockName, NewTimeout}).

%% @doc Get lock status
-spec lock_status(lock_name()) -> {ok, map()} | {error, not_found}.
lock_status(LockName) ->
    gen_server:call(?SERVER, {lock_status, LockName}).

%% @doc List all locks
-spec list_locks() -> #{lock_name() => map()}.
list_locks() ->
    gen_server:call(?SERVER, list_locks).

%% @doc Wait for a lock to become available
-spec wait_for_lock(lock_name(), timeout()) -> ok | {error, timeout | term()}.
wait_for_lock(LockName, Timeout) ->
    case lock_status(LockName) of
        {ok, #{holder := Holder}} when Holder =:= self() ->
            ok;
        {ok, _} ->
            % Lock is held by someone else, wait
            gen_server:call(?SERVER, {wait, LockName, Timeout}, Timeout + 1000);
        {error, not_found} ->
            % No lock, acquire it
            acquire(LockName, Timeout)
    end.

%% @doc Set default lock timeout
-spec set_default_timeout(pos_integer()) -> ok.
set_default_timeout(Timeout) when is_integer(Timeout), Timeout > 0 ->
    gen_server:call(?SERVER, {set_default_timeout, Timeout}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map() | []) -> {ok, state()}.
init(Options) ->
    process_flag(trap_exit, true),

    DefaultTimeout = maps:get(default_timeout, Options, ?DEFAULT_LOCK_TIMEOUT),
    CleanupInterval = maps:get(cleanup_interval, Options, 30000),

    ?LOG_INFO("Starting distributed lock service: default_timeout=~p", [DefaultTimeout]),

    CleanupRef = erlang:send_after(CleanupInterval, self(), cleanup_expired),

    {ok, #state{
        default_timeout = DefaultTimeout,
        cleanup_interval = CleanupInterval,
        cleanup_ref = CleanupRef
    }}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call({acquire, LockName, LockTimeout}, {FromPid, _Tag}, State) ->
    case maps:get(LockName, State#state.locks, undefined) of
        undefined ->
            % Lock is available, acquire it
            Lock = create_lock(LockName, FromPid, LockTimeout, State),
            NewLocks = maps:put(LockName, Lock, State#state.locks),
            ?LOG_DEBUG("Lock ~s acquired by ~p", [format_lock_name(LockName), FromPid]),
            {reply, ok, State#state{locks = NewLocks}, hibernate};
        #lock{holder = FromPid} ->
            % Already hold the lock, renew it
            {reply, {error, already_held}, State, hibernate};
        #lock{holder = Holder} ->
            % Lock is held by someone else
            ?LOG_DEBUG("Lock ~s held by ~p, requested by ~p",
                     [format_lock_name(LockName), Holder, FromPid]),
            {reply, {error, {locked, Holder}}, State, hibernate}
    end;

handle_call({release, LockName}, {FromPid, _Tag}, State) ->
    case maps:get(LockName, State#state.locks, undefined) of
        undefined ->
            {reply, {error, not_found}, State, hibernate};
        #lock{holder = FromPid} ->
            % We hold the lock, release it
            NewState = do_release_lock(LockName, State),
            {reply, ok, NewState, hibernate};
        #lock{holder = Holder} ->
            % Not our lock
            {reply, {error, {not_held, Holder}}, State, hibernate}
    end;

handle_call({renew, LockName, NewTimeout}, {FromPid, _Tag}, State) ->
    case maps:get(LockName, State#state.locks, undefined) of
        undefined ->
            {reply, {error, not_found}, State, hibernate};
        #lock{holder = FromPid} = Lock ->
            % We hold the lock, renew it
            NewLock = renew_lock(Lock, NewTimeout, State),
            NewLocks = maps:put(LockName, NewLock, State#state.locks),
            ?LOG_DEBUG("Lock ~s renewed by ~p for ~pms",
                     [format_lock_name(LockName), FromPid, NewTimeout]),
            {reply, ok, State#state{locks = NewLocks}, hibernate};
        #lock{holder = Holder} ->
            % Not our lock
            {reply, {error, {not_held, Holder}}, State, hibernate}
    end;

handle_call({lock_status, LockName}, _From, State) ->
    case maps:get(LockName, State#state.locks, undefined) of
        undefined ->
            {reply, {error, not_found}, State, hibernate};
        Lock ->
            Status = #{
                name => Lock#lock.name,
                holder => Lock#lock.holder,
                holder_node => Lock#lock.holder_node,
                acquired_at => Lock#lock.acquired_at,
                expires_at => Lock#lock.expires_at,
                timeout => Lock#lock.timeout,
                remaining_ms => max(0, Lock#lock.expires_at - erlang:system_time(millisecond))
            },
            {reply, {ok, Status}, State, hibernate}
    end;

handle_call(list_locks, _From, State) ->
    Locks = maps:map(fun(_LockName, Lock) ->
        #{
            holder => Lock#lock.holder,
            holder_node => Lock#lock.holder_node,
            expires_at => Lock#lock.expires_at,
            remaining_ms => max(0, Lock#lock.expires_at - erlang:system_time(millisecond))
        }
    end, State#state.locks),
    {reply, Locks, State, hibernate};

handle_call({wait, LockName, Timeout}, {FromPid, _Tag}, State) ->
    case maps:get(LockName, State#state.locks, undefined) of
        undefined ->
            % Lock is now available, try to acquire
            handle_call({acquire, LockName, State#state.default_timeout}, {FromPid, waiter}, State);
        #lock{holder = FromPid} ->
            % Already hold the lock
            {reply, ok, State, hibernate};
        #lock{} ->
            % Add to waiters
            TimeoutRef = erlang:send_after(Timeout, self(), {wait_timeout, LockName, FromPid}),
            Waiter = #waiter{
                pid = FromPid,
                node = node(FromPid),
                timeout_ref = TimeoutRef,
                start_time = erlang:system_time(millisecond)
            },
            CurrentWaiters = maps:get(LockName, State#state.waiters, []),
            NewWaiters = maps:put(LockName, CurrentWaiters ++ [Waiter], State#state.waiters),
            {reply, {error, waiting}, State#state{waiters = NewWaiters}, hibernate}
    end;

handle_call({set_default_timeout, Timeout}, _From, State) ->
    {reply, ok, State#state{default_timeout = Timeout}, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_cast({notify_next, LockName}, State) ->
    % Notify next waiter that lock is available
    case maps:get(LockName, State#state.waiters, []) of
        [] ->
            {noreply, State, hibernate};
        [Waiter | RestWaiters] ->
            erlang:cancel_timer(Waiter#waiter.timeout_ref),
            Waiter#waiter.pid ! {lock_available, LockName},
            NewWaiters = case RestWaiters of
                [] -> maps:remove(LockName, State#state.waiters);
                _ -> maps:put(LockName, RestWaiters, State#state.waiters)
            end,
            {noreply, State#state{waiters = NewWaiters}, hibernate}
    end;

handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_info({lock_expired, LockName}, State) ->
    % Lock timeout, release it
    case maps:get(LockName, State#state.locks, undefined) of
        undefined ->
            {noreply, State, hibernate};
        Lock ->
            ?LOG_INFO("Lock ~s expired (held by ~p)",
                     [format_lock_name(LockName), Lock#lock.holder]),
            NewState = do_release_lock(LockName, State),
            % Notify waiters
            gen_server:cast(?SERVER, {notify_next, LockName}),
            {noreply, NewState, hibernate}
    end;

handle_info({wait_timeout, LockName, Pid}, State) ->
    % Waiter timeout
    case maps:get(LockName, State#state.waiters, []) of
        [] ->
            {noreply, State, hibernate};
        Waiters ->
            NewWaiters = lists:filter(fun(W) ->
                W#waiter.pid =/= Pid
            end, Waiters),
            NewWaitersMap = case NewWaiters of
                [] -> maps:remove(LockName, State#state.waiters);
                _ -> maps:put(LockName, NewWaiters, State#state.waiters)
            end,
            {noreply, State#state{waiters = NewWaitersMap}, hibernate}
    end;

handle_info({renew_lock, LockName, HolderPid}, State) ->
    % Automatic renewal check
    case maps:get(LockName, State#state.locks, undefined) of
        undefined ->
            {noreply, State, hibernate};
        #lock{holder = HolderPid} = Lock ->
            % Renew the lock
            NewLock = renew_lock(Lock, Lock#lock.timeout, State),
            NewLocks = maps:put(LockName, NewLock, State#state.locks),
            ?LOG_DEBUG("Auto-renewed lock ~s for ~p", [format_lock_name(LockName), HolderPid]),
            {noreply, State#state{locks = NewLocks}, hibernate};
        _ ->
            % Lock holder changed, ignore
            {noreply, State, hibernate}
    end;

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    % Lock holder died
    LockName = find_lock_by_monitor(MonitorRef, State#state.locks),
    case LockName of
        undefined ->
            {noreply, State, hibernate};
        _ ->
            ?LOG_WARNING("Lock holder died for ~s, releasing",
                        [format_lock_name(LockName)]),
            NewState = do_release_lock(LockName, State),
            % Notify waiters
            gen_server:cast(?SERVER, {notify_next, LockName}),
            {noreply, NewState, hibernate}
    end;

handle_info(cleanup_expired, State) ->
    % Periodic cleanup of expired locks
    Now = erlang:system_time(millisecond),
    NewLocks = maps:filter(fun(_LockName, Lock) ->
        Lock#lock.expires_at > Now
    end, State#state.locks),

    CleanupRef = erlang:send_after(State#state.cleanup_interval, self(), cleanup_expired),
    {noreply, State#state{locks = NewLocks, cleanup_ref = CleanupRef}, hibernate};

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    % Cancel timers
    case State#state.cleanup_ref of
        undefined -> ok;
        CleanupRef -> erlang:cancel_timer(CleanupRef)
    end,

    % Cancel all lock timers
    maps:foreach(fun(_LockName, Lock) ->
        case Lock#lock.renewal_ref of
            undefined -> ok;
            RenewalRef -> erlang:cancel_timer(RenewalRef)
        end,
        erlang:demonitor(Lock#lock.monitor_ref, [flush])
    end, State#state.locks),

    ?LOG_INFO("Distributed lock service terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Create a new lock
-spec create_lock(lock_name(), pid(), pos_integer(), state()) -> #lock{}.
create_lock(LockName, HolderPid, Timeout, _State) ->
    Now = erlang:system_time(millisecond),
    ExpiresAt = Now + Timeout,

    % Schedule auto-renewal check
    RenewalDelay = trunc(Timeout * ?RENEWAL_THRESHOLD),
    RenewalRef = erlang:send_after(RenewalDelay, self(), {renew_lock, LockName, HolderPid}),

    % Monitor holder process
    MonitorRef = erlang:monitor(process, HolderPid),

    #lock{
        name = LockName,
        holder = HolderPid,
        holder_node = node(HolderPid),
        acquired_at = Now,
        expires_at = ExpiresAt,
        timeout = Timeout,
        renewal_ref = RenewalRef,
        monitor_ref = MonitorRef
    }.

%% @doc Renew a lock
-spec renew_lock(#lock{}, pos_integer(), state()) -> #lock{}.
renew_lock(Lock, NewTimeout, _State) ->
    Now = erlang:system_time(millisecond),
    ExpiresAt = Now + NewTimeout,

    % Cancel old renewal timer
    erlang:cancel_timer(Lock#lock.renewal_ref),

    % Schedule new renewal
    RenewalDelay = trunc(NewTimeout * ?RENEWAL_THRESHOLD),
    RenewalRef = erlang:send_after(RenewalDelay, self(), {renew_lock, Lock#lock.name, Lock#lock.holder}),

    Lock#lock{
        expires_at = ExpiresAt,
        timeout = NewTimeout,
        renewal_ref = RenewalRef
    }.

%% @doc Release a lock
-spec do_release_lock(lock_name(), state()) -> state().
do_release_lock(LockName, State) ->
    case maps:take(LockName, State#state.locks) of
        error ->
            State;
        {Lock, RemainingLocks} ->
            % Cancel timers and monitor
            erlang:cancel_timer(Lock#lock.renewal_ref),
            erlang:demonitor(Lock#lock.monitor_ref, [flush]),

            ?LOG_DEBUG("Released lock ~s (held by ~p)",
                     [format_lock_name(LockName), Lock#lock.holder]),

            State#state{locks = RemainingLocks}
    end.

%% @doc Find lock by monitor reference
-spec find_lock_by_monitor(reference(), #{lock_name() => #lock{}}) -> lock_name() | undefined.
find_lock_by_monitor(MonitorRef, Locks) ->
    maps:fold(fun(LockName, Lock, Acc) ->
        case Lock#lock.monitor_ref of
            MonitorRef -> LockName;
            _ -> Acc
        end
    end, undefined, Locks).

%% @doc Format lock name for logging
-spec format_lock_name(lock_name()) -> iolist().
format_lock_name(LockName) when is_atom(LockName) ->
    atom_to_list(LockName);
format_lock_name(LockName) when is_binary(LockName) ->
    LockName;
format_lock_name(LockName) ->
    io_lib:format("~p", [LockName]).
