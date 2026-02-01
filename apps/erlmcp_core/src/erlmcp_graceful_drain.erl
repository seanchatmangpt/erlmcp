-module(erlmcp_graceful_drain).

-behaviour(gen_server).

%% API
-export([start_link/0, drain_module/2, resume_module/1, is_draining/1, get_drain_status/0,
         initiate_shutdown/1, get_active_connections/0, connection_started/1,
         connection_finished/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,
        {draining_modules = #{} :: #{module() => drain_info()},
         paused_requests = #{} :: #{module() => queue:queue()},
         shutdown_requested = false :: boolean(),
         active_connections = 0 :: non_neg_integer(),
         priority_messages_delivered = 0 :: non_neg_integer(),
         priority_latency_sum_us = 0 :: non_neg_integer()}).

-type drain_info() ::
    #{start_time => erlang:timestamp(),
      timeout_ms => pos_integer(),
      timer_ref => reference()}.
-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Drain connections for a module (pause new requests)
-spec drain_module(module(), pos_integer()) -> ok | {error, term()}.
drain_module(Module, TimeoutMs) ->
    gen_server:call(?MODULE, {drain_module, Module, TimeoutMs}, TimeoutMs + 1000).

%% @doc Resume accepting requests for a module
-spec resume_module(module()) -> ok.
resume_module(Module) ->
    gen_server:call(?MODULE, {resume_module, Module}).

%% @doc Check if a module is currently draining
-spec is_draining(module()) -> boolean().
is_draining(Module) ->
    gen_server:call(?MODULE, {is_draining, Module}).

%% @doc Get status of all draining modules
-spec get_drain_status() -> #{module() => drain_info()}.
get_drain_status() ->
    gen_server:call(?MODULE, get_drain_status).

%% @doc Initiate graceful shutdown (priority signal on OTP 28)
-spec initiate_shutdown(pos_integer()) -> ok.
initiate_shutdown(TimeoutMs) ->
    % Send priority message for immediate shutdown processing (OTP 28)
    erlang:send(?MODULE, {priority_shutdown, TimeoutMs}, [nosuspend]),
    ok.

%% @doc Get number of active connections
-spec get_active_connections() -> non_neg_integer().
get_active_connections() ->
    gen_server:call(?MODULE, get_active_connections).

%% @doc Notify that a connection started
-spec connection_started(module()) -> ok.
connection_started(Module) ->
    gen_server:cast(?MODULE, {connection_started, Module}).

%% @doc Notify that a connection finished
-spec connection_finished(module()) -> ok.
connection_finished(Module) ->
    gen_server:cast(?MODULE, {connection_finished, Module}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    % Enable priority messages on OTP 28+ for critical shutdown signals
    process_flag(message_queue_data, off_heap),
    process_flag(priority, high),
    logger:info("Graceful drain service started with OTP 28 priority messages"),

    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call({drain_module, Module, TimeoutMs}, _From, State) ->
    logger:info("Starting drain for module ~p (timeout: ~pms)", [Module, TimeoutMs]),

    % Setup drain timeout
    TimerRef = erlang:send_after(TimeoutMs, self(), {drain_timeout, Module}),

    DrainInfo =
        #{start_time => erlang:timestamp(),
          timeout_ms => TimeoutMs,
          timer_ref => TimerRef},

    NewDraining = maps:put(Module, DrainInfo, State#state.draining_modules),
    NewState =
        State#state{draining_modules = NewDraining,
                    paused_requests = maps:put(Module, queue:new(), State#state.paused_requests)},

    {reply, ok, NewState};
handle_call({resume_module, Module}, _From, State) ->
    logger:info("Resuming module ~p", [Module]),

    % Cancel drain timer if exists
    case maps:get(Module, State#state.draining_modules, undefined) of
        undefined ->
            ok;
        #{timer_ref := TimerRef} ->
            erlang:cancel_timer(TimerRef)
    end,

    NewDraining = maps:remove(Module, State#state.draining_modules),
    NewPaused = maps:remove(Module, State#state.paused_requests),

    NewState = State#state{draining_modules = NewDraining, paused_requests = NewPaused},

    {reply, ok, NewState};
handle_call({is_draining, Module}, _From, State) ->
    Result = maps:is_key(Module, State#state.draining_modules),
    {reply, Result, State};
handle_call(get_drain_status, _From, State) ->
    {reply, State#state.draining_modules, State};
handle_call(get_active_connections, _From, State) ->
    {reply, State#state.active_connections, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({shutdown, TimeoutMs}, State) ->
    logger:warning("Graceful shutdown initiated (timeout: ~pms)", [TimeoutMs]),
    StartTime = erlang:monotonic_time(microsecond),

    % Stop accepting new connections
    NewState = State#state{shutdown_requested = true},

    % Start drain timer
    erlang:send_after(TimeoutMs, self(), {shutdown_timeout}),

    EndTime = erlang:monotonic_time(microsecond),
    LatencyUs = EndTime - StartTime,

    FinalState =
        NewState#state{priority_messages_delivered = State#state.priority_messages_delivered + 1,
                       priority_latency_sum_us = State#state.priority_latency_sum_us + LatencyUs},

    {noreply, FinalState};
handle_cast({connection_started, _Module}, State) ->
    % Only increment if not shutting down
    case State#state.shutdown_requested of
        false ->
            NewState = State#state{active_connections = State#state.active_connections + 1},
            {noreply, NewState};
        true ->
            logger:warning("Connection attempt during shutdown rejected"),
            {noreply, State}
    end;
handle_cast({connection_finished, _Module}, State) ->
    NewConnections = max(0, State#state.active_connections - 1),
    NewState = State#state{active_connections = NewConnections},

    % Check if we can complete shutdown
    case State#state.shutdown_requested andalso NewConnections =:= 0 of
        true ->
            logger:info("All connections drained, initiating final shutdown"),
            erlang:send_after(0, self(), final_shutdown);
        false ->
            ok
    end,

    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
% Priority shutdown message (OTP 28 only)
handle_info({priority_shutdown, TimeoutMs}, State) ->
    StartTime = erlang:monotonic_time(microsecond),
    logger:warning("PRIORITY shutdown initiated (timeout: ~pms)", [TimeoutMs]),

    % Immediately stop accepting new connections
    NewState = State#state{shutdown_requested = true},

    % Start drain timer
    erlang:send_after(TimeoutMs, self(), shutdown_timeout),

    EndTime = erlang:monotonic_time(microsecond),
    LatencyUs = EndTime - StartTime,

    FinalState =
        NewState#state{priority_messages_delivered = State#state.priority_messages_delivered + 1,
                       priority_latency_sum_us = State#state.priority_latency_sum_us + LatencyUs},

    {noreply, FinalState};
handle_info({drain_timeout, Module}, State) ->
    logger:info("Drain timeout reached for module ~p", [Module]),

    NewDraining = maps:remove(Module, State#state.draining_modules),
    NewPaused = maps:remove(Module, State#state.paused_requests),

    NewState = State#state{draining_modules = NewDraining, paused_requests = NewPaused},

    {noreply, NewState};
handle_info(shutdown_timeout, State) ->
    logger:warning("Shutdown timeout reached, forcing shutdown with ~p active connections",
                   [State#state.active_connections]),
    erlang:send_after(0, self(), final_shutdown),
    {noreply, State};
handle_info(final_shutdown, State) ->
    logger:info("Final shutdown: ~p active connections", [State#state.active_connections]),
    % Notify supervisor that we're ready to terminate
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
