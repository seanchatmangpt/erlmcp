-module(erlmcp_circuit_breaker_poc).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/1,
    call/2,
    get_state/1,
    reset/1,
    get_stats/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    circuit_state = closed :: closed | open | half_open,
    failure_count = 0 :: non_neg_integer(),
    success_count = 0 :: non_neg_integer(),
    failure_threshold = 5 :: pos_integer(),
    success_threshold = 2 :: pos_integer(),
    timeout = 5000 :: pos_integer(),
    reset_timer :: reference() | undefined
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

-spec call(pid(), fun(() -> term())) -> {ok, term()} | {error, circuit_open}.
call(Pid, Fun) ->
    gen_server:call(Pid, {call, Fun}).

-spec get_state(pid()) -> {ok, closed | open | half_open}.
get_state(Pid) ->
    gen_server:call(Pid, get_state).

-spec reset(pid()) -> ok.
reset(Pid) ->
    gen_server:call(Pid, reset).

-spec get_stats(pid()) -> {ok, #{atom() => term()}}.
get_stats(Pid) ->
    gen_server:call(Pid, get_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    FailureThreshold = proplists:get_value(failure_threshold, Opts, 5),
    SuccessThreshold = proplists:get_value(success_threshold, Opts, 2),
    Timeout = proplists:get_value(timeout, Opts, 5000),

    {ok, #state{
        failure_threshold = FailureThreshold,
        success_threshold = SuccessThreshold,
        timeout = Timeout
    }}.

handle_call({call, Fun}, _From, State = #state{circuit_state = open}) ->
    {reply, {error, circuit_open}, State};

handle_call({call, Fun}, _From, State = #state{circuit_state = CircuitState}) ->
    try
        Result = Fun(),
        NewState = handle_success(State),
        {reply, {ok, Result}, NewState}
    catch
        _:Error ->
            NewState = handle_failure(State),
            case NewState#state.circuit_state of
                open -> {reply, {error, circuit_open}, NewState};
                _ -> {reply, {error, Error}, NewState}
            end
    end;

handle_call(get_state, _From, State = #state{circuit_state = CircuitState}) ->
    {reply, {ok, CircuitState}, State};

handle_call(reset, _From, State) ->
    cancel_reset_timer(State#state.reset_timer),
    {reply, ok, #state{
        failure_threshold = State#state.failure_threshold,
        success_threshold = State#state.success_threshold,
        timeout = State#state.timeout
    }};

handle_call(get_stats, _From, State) ->
    Stats = #{
        circuit_state => State#state.circuit_state,
        failure_count => State#state.failure_count,
        success_count => State#state.success_count,
        failure_threshold => State#state.failure_threshold,
        success_threshold => State#state.success_threshold
    },
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reset_timeout, State) ->
    %% Transition from open to half_open
    {noreply, State#state{circuit_state = half_open, reset_timer = undefined}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    cancel_reset_timer(State#state.reset_timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

handle_success(State = #state{circuit_state = half_open, success_count = SuccessCount, success_threshold = Threshold}) ->
    NewSuccessCount = SuccessCount + 1,
    case NewSuccessCount >= Threshold of
        true ->
            %% Transition to closed
            State#state{
                circuit_state = closed,
                success_count = 0,
                failure_count = 0
            };
        false ->
            State#state{success_count = NewSuccessCount}
    end;

handle_success(State = #state{circuit_state = closed}) ->
    %% Reset failure count on success
    State#state{failure_count = 0};

handle_success(State) ->
    State.

handle_failure(State = #state{circuit_state = half_open}) ->
    %% Immediate trip to open on failure in half_open
    Timer = start_reset_timer(State#state.timeout),
    State#state{
        circuit_state = open,
        failure_count = 0,
        success_count = 0,
        reset_timer = Timer
    };

handle_failure(State = #state{circuit_state = closed, failure_count = FailureCount, failure_threshold = Threshold}) ->
    NewFailureCount = FailureCount + 1,
    case NewFailureCount >= Threshold of
        true ->
            %% Trip to open
            Timer = start_reset_timer(State#state.timeout),
            State#state{
                circuit_state = open,
                failure_count = 0,
                reset_timer = Timer
            };
        false ->
            State#state{failure_count = NewFailureCount}
    end;

handle_failure(State) ->
    State.

start_reset_timer(Timeout) ->
    erlang:send_after(Timeout, self(), reset_timeout).

cancel_reset_timer(undefined) ->
    ok;
cancel_reset_timer(Timer) ->
    erlang:cancel_timer(Timer).
