-module(erlmcp_graceful_drain).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    drain_module/2,
    resume_module/1,
    is_draining/1,
    get_drain_status/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    draining_modules = #{} :: #{module() => drain_info()},
    paused_requests = #{} :: #{module() => queue:queue()}
}).

-type drain_info() :: #{
    start_time => erlang:timestamp(),
    timeout_ms => pos_integer(),
    timer_ref => reference()
}.

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

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    logger:info("Graceful drain service started"),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.

handle_call({drain_module, Module, TimeoutMs}, _From, State) ->
    logger:info("Starting drain for module ~p (timeout: ~pms)", [Module, TimeoutMs]),

    % Setup drain timeout
    TimerRef = erlang:send_after(TimeoutMs, self(), {drain_timeout, Module}),

    DrainInfo = #{
        start_time => erlang:timestamp(),
        timeout_ms => TimeoutMs,
        timer_ref => TimerRef
    },

    NewDraining = maps:put(Module, DrainInfo, State#state.draining_modules),
    NewState = State#state{
        draining_modules = NewDraining,
        paused_requests = maps:put(Module, queue:new(), State#state.paused_requests)
    },

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

    NewState = State#state{
        draining_modules = NewDraining,
        paused_requests = NewPaused
    },

    {reply, ok, NewState};

handle_call({is_draining, Module}, _From, State) ->
    Result = maps:is_key(Module, State#state.draining_modules),
    {reply, Result, State};

handle_call(get_drain_status, _From, State) ->
    {reply, State#state.draining_modules, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({drain_timeout, Module}, State) ->
    logger:info("Drain timeout reached for module ~p", [Module]),

    NewDraining = maps:remove(Module, State#state.draining_modules),
    NewPaused = maps:remove(Module, State#state.paused_requests),

    NewState = State#state{
        draining_modules = NewDraining,
        paused_requests = NewPaused
    },

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
