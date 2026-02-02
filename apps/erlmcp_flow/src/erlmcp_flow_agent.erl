%%%-------------------------------------------------------------------
%%% @doc Agent Framework (gen_server) - Week 1 Days 1-2
%%% State machine: idle → assigned → executing → done
%%% Features: task queue (max 100), error recovery (3 retries),
%%%           heartbeat (10s interval to swarm)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_agent).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]).
-export([assign_task/2, get_status/1, get_result/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type agent_id() :: binary() | atom().
-type agent_status() :: idle | assigned | executing | done.
-type task() :: #{id := binary(), action := term(), timeout => timeout()}.
-type task_result() :: {ok, term()} | {error, term()}.

-export_type([agent_id/0, agent_status/0, task/0, task_result/0]).

%% State record
-record(state, {
    id :: agent_id(),
    status = idle :: agent_status(),
    task :: task() | undefined,
    result :: task_result() | undefined,
    stats = #{tasks_completed => 0, tasks_failed => 0} :: map(),
    health = #{last_heartbeat => erlang:monotonic_time(millisecond)} :: map(),
    task_queue = [] :: [task()],
    retry_count = 0 :: non_neg_integer(),
    max_retries = 3 :: pos_integer(),
    swarm_pid :: pid() | undefined,
    heartbeat_interval = 10000 :: pos_integer(), % 10s
    heartbeat_timer :: reference() | undefined
}).

-type state() :: #state{}.

%% Constants
-define(MAX_QUEUE_SIZE, 100).
-define(DEFAULT_TASK_TIMEOUT, 5000). % 5s
-define(BACKOFF_BASE, 100). % 100ms base
-define(BACKOFF_MAX, 500). % 500ms max

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(agent_id()) -> {ok, pid()} | {error, term()}.
start_link(AgentId) ->
    start_link(AgentId, #{}).

-spec start_link(agent_id(), map()) -> {ok, pid()} | {error, term()}.
start_link(AgentId, Opts) ->
    gen_server:start_link(?MODULE, [AgentId, Opts], []).

-spec assign_task(pid(), task()) -> ok | {error, term()}.
assign_task(Agent, Task) ->
    gen_server:cast(Agent, {assign_task, Task}).

-spec get_status(pid()) -> {ok, agent_status()} | {error, term()}.
get_status(Agent) ->
    gen_server:call(Agent, get_status, 2000).

-spec get_result(pid()) -> {ok, task_result()} | {error, no_result}.
get_result(Agent) ->
    gen_server:call(Agent, get_result, 2000).

-spec stop(pid()) -> ok.
stop(Agent) ->
    gen_server:stop(Agent).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([agent_id() | map()]) -> {ok, state()}.
init([AgentId, Opts]) ->
    process_flag(trap_exit, true),

    % Non-blocking init - schedule heartbeat asynchronously
    SwarmPid = maps:get(swarm_pid, Opts, undefined),
    Interval = maps:get(heartbeat_interval, Opts, 10000),
    MaxRetries = maps:get(max_retries, Opts, 3),

    State = #state{
        id = AgentId,
        status = idle,
        swarm_pid = SwarmPid,
        heartbeat_interval = Interval,
        max_retries = MaxRetries
    },

    % Schedule first heartbeat
    Timer = schedule_heartbeat(Interval),

    {ok, State#state{heartbeat_timer = Timer}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.

handle_call(get_status, _From, State) ->
    {reply, {ok, State#state.status}, State};

handle_call(get_result, _From, #state{result = undefined} = State) ->
    {reply, {error, no_result}, State};

handle_call(get_result, _From, #state{result = Result} = State) ->
    {reply, {ok, Result}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast({assign_task, Task}, #state{status = idle} = State) ->
    % Transition: idle → assigned
    NewState = State#state{
        status = assigned,
        task = Task,
        retry_count = 0,
        result = undefined
    },
    % Start execution
    {noreply, execute_task(NewState)};

handle_cast({assign_task, Task}, #state{task_queue = Queue} = State) ->
    % Agent busy - queue the task
    case length(Queue) < ?MAX_QUEUE_SIZE of
        true ->
            NewQueue = Queue ++ [Task],
            {noreply, State#state{task_queue = NewQueue}};
        false ->
            % Queue full - drop task
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info(heartbeat, #state{swarm_pid = undefined} = State) ->
    % No swarm - reschedule
    Timer = schedule_heartbeat(State#state.heartbeat_interval),
    NewHealth = maps:put(last_heartbeat, erlang:monotonic_time(millisecond), State#state.health),
    {noreply, State#state{heartbeat_timer = Timer, health = NewHealth}};

handle_info(heartbeat, #state{swarm_pid = SwarmPid} = State) ->
    % Send heartbeat to swarm
    SwarmPid ! {agent_heartbeat, self(), State#state.id},
    Timer = schedule_heartbeat(State#state.heartbeat_interval),
    NewHealth = maps:put(last_heartbeat, erlang:monotonic_time(millisecond), State#state.health),
    {noreply, State#state{heartbeat_timer = Timer, health = NewHealth}};

handle_info({task_result, Result}, #state{status = executing} = State) ->
    % Task completed successfully
    % Transition: executing → done
    NewStats = update_stats(State#state.stats, success),
    NewState = State#state{
        status = done,
        result = {ok, Result},
        stats = NewStats
    },
    % Process next task in queue
    {noreply, process_next_task(NewState)};

handle_info({task_error, _Reason},
            #state{status = executing, retry_count = Count, max_retries = Max} = State)
    when Count < Max ->
    % Task failed - retry with exponential backoff
    Backoff = calculate_backoff(Count),
    timer:sleep(Backoff),

    NewState = State#state{retry_count = Count + 1},
    {noreply, execute_task(NewState)};

handle_info({task_error, Reason}, #state{status = executing} = State) ->
    % Max retries exceeded
    NewStats = update_stats(State#state.stats, failure),
    NewState = State#state{
        status = done,
        result = {error, {max_retries, Reason}},
        stats = NewStats,
        retry_count = 0
    },
    {noreply, process_next_task(NewState)};

handle_info(task_timeout, #state{status = executing} = State) ->
    % Task timeout
    {noreply, handle_info({task_error, timeout}, State)};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) when is_record(State, state) ->
    % Cancel heartbeat timer
    case State#state.heartbeat_timer of
        undefined -> ok;
        Timer -> catch erlang:cancel_timer(Timer)
    end,
    ok;
terminate(_Reason, _State) ->
    % Handle any unexpected state format
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec execute_task(state()) -> state().
execute_task(#state{task = Task} = State) ->
    % Transition: assigned → executing
    Self = self(),
    Timeout = maps:get(timeout, Task, ?DEFAULT_TASK_TIMEOUT),
    Action = maps:get(action, Task),

    % Spawn task execution
    spawn_link(fun() ->
        try
            Result = execute_action(Action),
            Self ! {task_result, Result}
        catch
            _:Error ->
                Self ! {task_error, Error}
        end
    end),

    % Set timeout
    erlang:send_after(Timeout, Self, task_timeout),

    State#state{status = executing}.

-spec execute_action(term()) -> term().
execute_action({mfa, M, F, A}) ->
    erlang:apply(M, F, A);
execute_action(Fun) when is_function(Fun, 0) ->
    Fun();
execute_action(Data) ->
    % Default: echo back
    Data.

-spec process_next_task(state()) -> state().
process_next_task(#state{task_queue = []} = State) ->
    % No more tasks - return to idle
    State#state{status = idle, task = undefined, retry_count = 0};

process_next_task(#state{task_queue = [Next | Rest]} = State) ->
    % Process next task in queue
    NewState = State#state{
        status = assigned,
        task = Next,
        task_queue = Rest,
        retry_count = 0,
        result = undefined
    },
    execute_task(NewState).

-spec calculate_backoff(non_neg_integer()) -> pos_integer().
calculate_backoff(RetryCount) ->
    % Exponential backoff: 100ms → 200ms → 400ms (capped at 500ms)
    Backoff = ?BACKOFF_BASE * (1 bsl RetryCount),
    min(Backoff, ?BACKOFF_MAX).

-spec update_stats(map(), success | failure) -> map().
update_stats(Stats, success) ->
    Stats#{tasks_completed => maps:get(tasks_completed, Stats, 0) + 1};
update_stats(Stats, failure) ->
    Stats#{tasks_failed => maps:get(tasks_failed, Stats, 0) + 1}.

-spec schedule_heartbeat(pos_integer()) -> reference().
schedule_heartbeat(Interval) ->
    erlang:send_after(Interval, self(), heartbeat).
