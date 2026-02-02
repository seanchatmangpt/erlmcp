%%%-------------------------------------------------------------------
%%% @doc Task runner using proc_lib for supervised execution
%%% Provides OTP-compatible task execution for long-running MCP operations.
%%%
%%% == Design ==
%%% Uses proc_lib for proper OTP process initialization and system message
%%% handling. Tasks can be supervised, cancelled, and report progress.
%%%
%%% == Features ==
%%% - proc_lib spawning for supervisor compatibility
%%% - Progress reporting via erlmcp_progress
%%% - Graceful cancellation support
%%% - Timeout handling
%%% - sys module debugging support
%%% - Clean shutdown with result reporting
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_task_runner).

%% API
-export([start_task/2, start_task/3, start_link/1, start_link/2, cancel_task/1, cancel_task/2,
         get_status/1]).
%% Internal exports for proc_lib
-export([init/2]).
%% System message handling
-export([system_continue/3, system_terminate/4, system_code_change/4, system_get_state/1]).

-include("erlmcp.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type task_id() :: binary().
-type task_fn() :: fun(() -> term()).
-type task_spec() ::
    #{task_fn := task_fn(),
      timeout => pos_integer(),
      progress_token => reference(),
      task_id => task_id(),
      parent => pid(),
      metadata => map()}.
-type task_state() ::
    #{task_id := task_id(),
      task_fn := task_fn(),
      timeout := pos_integer(),
      progress_token => reference(),
      parent := pid(),
      start_time := integer(),
      timer_ref => reference(),
      metadata := map(),
      status := pending | running | completed | failed | cancelled}.

-export_type([task_id/0, task_fn/0, task_spec/0, task_state/0]).

%%%===================================================================
%%% Defaults
%%%===================================================================

-define(DEFAULT_TIMEOUT_MS, 300000).  % 5 minutes

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start a task with a simple function and options map.
%% Returns {ok, TaskId, Pid} on success.
-spec start_task(task_fn(), map()) -> {ok, task_id(), pid()} | {error, term()}.
start_task(Fun, Opts) when is_function(Fun, 0), is_map(Opts) ->
    TaskId = generate_task_id(),
    Parent = self(),

    TaskSpec =
        #{task_fn => Fun,
          timeout => maps:get(timeout, Opts, ?DEFAULT_TIMEOUT_MS),
          progress_token => maps:get(progress_token, Opts, undefined),
          task_id => TaskId,
          parent => Parent,
          metadata => maps:get(metadata, Opts, #{})},

    Pid = proc_lib:spawn_link(?MODULE, init, [Parent, TaskSpec]),

    receive
        {task_started, Pid, TaskId} ->
            {ok, TaskId, Pid};
        {'EXIT', Pid, Reason} ->
            {error, {task_start_failed, Reason}}
    after 5000 ->
        exit(Pid, kill),
        {error, timeout}
    end.

%% @doc Start a task with function, task_id, and options.
-spec start_task(task_fn(), task_id(), map()) -> {ok, pid()} | {error, term()}.
start_task(Fun, TaskId, Opts) when is_function(Fun, 0), is_binary(TaskId), is_map(Opts) ->
    Parent = self(),

    TaskSpec =
        #{task_fn => Fun,
          timeout => maps:get(timeout, Opts, ?DEFAULT_TIMEOUT_MS),
          progress_token => maps:get(progress_token, Opts, undefined),
          task_id => TaskId,
          parent => Parent,
          metadata => maps:get(metadata, Opts, #{})},

    Pid = proc_lib:spawn_link(?MODULE, init, [Parent, TaskSpec]),

    receive
        {task_started, Pid, TaskId} ->
            {ok, Pid};
        {'EXIT', Pid, Reason} ->
            {error, {task_start_failed, Reason}}
    after 5000 ->
        exit(Pid, kill),
        {error, timeout}
    end.

%% @doc Start a supervised task (supervisor-compatible).
%% TaskSpec must include 'fun', optionally 'timeout', 'progress_token', 'task_id'.
-spec start_link(task_spec()) -> {ok, pid()} | {error, term()}.
start_link(TaskSpec) when is_map(TaskSpec) ->
    case validate_task_spec(TaskSpec) of
        ok ->
            Parent = maps:get(parent, TaskSpec, self()),
            proc_lib:start_link(?MODULE, init, [Parent, TaskSpec]);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Start a supervised task with function and options.
-spec start_link(task_fn(), map()) -> {ok, pid()} | {error, term()}.
start_link(Fun, Opts) when is_function(Fun, 0), is_map(Opts) ->
    TaskId = maps:get(task_id, Opts, generate_task_id()),
    Parent = maps:get(parent, Opts, self()),

    TaskSpec =
        #{task_fn => Fun,
          timeout => maps:get(timeout, Opts, ?DEFAULT_TIMEOUT_MS),
          progress_token => maps:get(progress_token, Opts, undefined),
          task_id => TaskId,
          parent => Parent,
          metadata => maps:get(metadata, Opts, #{})},

    start_link(TaskSpec).

%% @doc Cancel a running task.
-spec cancel_task(pid()) -> ok.
cancel_task(Pid) when is_pid(Pid) ->
    cancel_task(Pid, <<"Task cancelled by request">>).

%% @doc Cancel a running task with reason.
-spec cancel_task(pid(), binary()) -> ok.
cancel_task(Pid, Reason) when is_pid(Pid), is_binary(Reason) ->
    Pid ! {cancel_task, Reason},
    ok.

%% @doc Get current task status (for debugging).
%% OTP 28: Modernized from catch to try/catch for better error handling
-spec get_status(pid()) -> {ok, map()} | {error, term()}.
get_status(Pid) when is_pid(Pid) ->
    try sys:get_state(Pid) of
        State when is_map(State) ->
            {ok,
             #{task_id => maps:get(task_id, State),
               status => maps:get(status, State),
               elapsed_ms => erlang:system_time(millisecond) - maps:get(start_time, State)}};
        State ->
            {ok, State}
    catch
        _:Reason ->
            {error, Reason}
    end.

%%%===================================================================
%%% Internal Functions - Process Initialization
%%%===================================================================

%% @doc Process initialization via proc_lib.
%% This is called by proc_lib:start_link/3.
-spec init(pid(), task_spec()) -> no_return().
init(Parent, TaskSpec) ->
    % Register with proc_lib
    process_flag(trap_exit, true),

    TaskId = maps:get(task_id, TaskSpec, generate_task_id()),
    Timeout = maps:get(timeout, TaskSpec, ?DEFAULT_TIMEOUT_MS),
    ProgressToken = maps:get(progress_token, TaskSpec, undefined),
    Metadata = maps:get(metadata, TaskSpec, #{}),
    TaskFun = maps:get(task_fn, TaskSpec),

    % Initialize state
    State =
        #{task_id => TaskId,
          task_fn => TaskFun,
          timeout => Timeout,
          progress_token => ProgressToken,
          parent => Parent,
          start_time => erlang:system_time(millisecond),
          timer_ref => undefined,
          metadata => Metadata,
          status => pending},

    % Notify parent we're ready (for proc_lib:start_link)
    proc_lib:init_ack(Parent, {ok, self()}),

    % Also notify via message (for spawn_link)
    Parent ! {task_started, self(), TaskId},

    % Notify erlmcp_tasks that execution started (if task_id matches pattern)
    notify_task_started(TaskId, self()),

    % Set up timeout timer
    TimerRef = erlang:send_after(Timeout, self(), task_timeout),
    State1 = State#{timer_ref => TimerRef, status => running},

    % Initialize progress tracking if token provided
    case ProgressToken of
        undefined ->
            ok;
        _ ->
            erlmcp_progress:create(Parent, <<"Task started: ", TaskId/binary>>)
    end,

    % Execute task
    execute_task(State1).

%% @doc Execute the task function and handle result.
-spec execute_task(task_state()) -> no_return().
execute_task(#{task_fn := Fun, progress_token := ProgressToken} = State) ->
    Result =
        try Fun() of
            R ->
                {ok, R}
        catch
            Reason ->
                {error, {throw, Reason}};
            error:Reason:Stacktrace ->
                {error, {error, Reason, Stacktrace}};
            exit:Reason ->
                {error, {exit, Reason}}
        end,

    % Cancel timeout timer
    cancel_timer(State),

    % Update progress to complete
    case ProgressToken of
        undefined ->
            ok;
        _ ->
            erlmcp_progress:complete(ProgressToken)
    end,

    % Notify result
    case Result of
        {ok, Value} ->
            notify_task_complete(State, Value),
            terminate_task(State#{status => completed}, {shutdown, {ok, Value}});
        {error, Error} ->
            notify_task_failed(State, Error),
            terminate_task(State#{status => failed}, {shutdown, {error, Error}})
    end.

%% @doc Main process loop for handling system messages and cancellation.
-spec task_loop(task_state()) -> no_return().
task_loop(State) ->
    #{parent := Parent, task_id := TaskId} = State,
    receive
        {cancel_task, Reason} ->
            logger:info("Task ~s cancelled: ~s", [TaskId, Reason]),
            cancel_timer(State),
            notify_task_cancelled(State, Reason),
            terminate_task(State#{status => cancelled}, {shutdown, cancelled});
        task_timeout ->
            logger:warning("Task ~s timed out after ~p ms", [TaskId, maps:get(timeout, State)]),
            notify_task_timeout(State),
            terminate_task(State#{status => failed}, {shutdown, timeout});
        {system, From, Request} ->
            % Handle system messages for sys module
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
        {'EXIT', Parent, Reason} ->
            % Parent died, clean up
            logger:info("Task ~s parent died: ~p", [TaskId, Reason]),
            terminate_task(State, {shutdown, parent_died});
        _Other ->
            % Ignore unknown messages
            task_loop(State)
    after 1000 ->
        % Periodic liveness check
        task_loop(State)
    end.

%%%===================================================================
%%% System Message Callbacks (for sys module support)
%%%===================================================================

%% @doc Continue execution after system message handling.
system_continue(_Parent, _Debug, State) ->
    task_loop(State).

%% @doc Terminate on system request.
system_terminate(Reason, _Parent, _Debug, State) ->
    terminate_task(State, Reason).

%% @doc Handle code change.
system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

%% @doc Get current state for debugging.
system_get_state(State) ->
    {ok, State}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @private
%% Generate unique task ID
-spec generate_task_id() -> task_id().
generate_task_id() ->
    Unique = erlang:unique_integer([positive, monotonic]),
    Time = erlang:system_time(microsecond),
    Random = crypto:strong_rand_bytes(8),
    base64:encode(<<Unique:64, Time:64, Random/binary>>).

%% @private
%% Validate task spec
-spec validate_task_spec(task_spec()) -> ok | {error, term()}.
validate_task_spec(Spec) when is_map(Spec) ->
    case maps:get(task_fn, Spec, undefined) of
        undefined ->
            {error, missing_task_fn};
        Fun when is_function(Fun, 0) ->
            ok;
        _ ->
            {error, invalid_task_fn}
    end;
validate_task_spec(_) ->
    {error, invalid_task_spec}.

%% @private
%% Cancel timeout timer
-spec cancel_timer(task_state()) -> ok.
cancel_timer(#{timer_ref := undefined}) ->
    ok;
cancel_timer(#{timer_ref := TimerRef}) ->
    erlang:cancel_timer(TimerRef),
    receive
        task_timeout ->
            ok
    after 0 ->
        ok
    end.

%% @private
%% Notify erlmcp_tasks that execution started
-spec notify_task_started(task_id(), pid()) -> ok.
notify_task_started(TaskId, WorkerPid) ->
    case whereis(erlmcp_tasks) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            try
                erlmcp_tasks:start_task_execution(TaskId, WorkerPid)
            catch
                _:_ ->
                    ok
            end
    end.

%% @private
%% Notify task completion
-spec notify_task_complete(task_state(), term()) -> ok.
notify_task_complete(#{task_id := TaskId, parent := Parent}, Result) ->
    Parent ! {task_complete, TaskId, Result},
    case whereis(erlmcp_tasks) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            try
                erlmcp_tasks:complete_task(TaskId, Result)
            catch
                _:_ ->
                    ok
            end
    end.

%% @private
%% Notify task failure
-spec notify_task_failed(task_state(), term()) -> ok.
notify_task_failed(#{task_id := TaskId,
                     parent := Parent,
                     progress_token := ProgressToken},
                   Error) ->
    Parent ! {task_failed, TaskId, Error},

    % Complete progress token
    case ProgressToken of
        undefined ->
            ok;
        _ ->
            erlmcp_progress:complete(ProgressToken)
    end,

    case whereis(erlmcp_tasks) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            try
                erlmcp_tasks:fail_task(TaskId, Error)
            catch
                _:_ ->
                    ok
            end
    end.

%% @private
%% Notify task cancellation
-spec notify_task_cancelled(task_state(), binary()) -> ok.
notify_task_cancelled(#{task_id := TaskId,
                        parent := Parent,
                        progress_token := ProgressToken},
                      Reason) ->
    Parent ! {task_cancelled, TaskId, Reason},

    % Complete progress token
    case ProgressToken of
        undefined ->
            ok;
        _ ->
            erlmcp_progress:complete(ProgressToken)
    end,

    case whereis(erlmcp_tasks) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            try
                erlmcp_tasks:cancel_task(undefined, TaskId, Reason)
            catch
                _:_ ->
                    ok
            end
    end.

%% @private
%% Notify task timeout
-spec notify_task_timeout(task_state()) -> ok.
notify_task_timeout(#{task_id := TaskId, timeout := Timeout} = State) ->
    Error =
        #{code => ?MCP_ERROR_TIMEOUT,
          message => <<"Task timed out">>,
          data => #{timeout => Timeout}},
    notify_task_failed(State, Error).

%% @private
%% Terminate task process
-spec terminate_task(task_state(), term()) -> no_return().
terminate_task(#{task_id := TaskId}, Reason) ->
    logger:debug("Task ~s terminating: ~p", [TaskId, Reason]),
    exit(Reason).
