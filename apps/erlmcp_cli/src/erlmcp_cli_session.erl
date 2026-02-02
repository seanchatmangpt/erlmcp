%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_session - CLI Session Manager
%%%
%%% Manages individual CLI session state, command processing,
%%% and OTEL tracing integration.
%%%
%%% == Key Features ==
%%%
%%% 1. **Session Isolation**: Complete process-per-session isolation
%%% 2. **OTEL Integration**: Distributed tracing for all session operations
%%% 3. **Command Processing**: Safe command parsing and execution
%%% 4. **State Management**: Session state persistence and recovery
%%% 5. **Metrics Collection**: Session performance metrics
%%%
%%% == Session Lifecycle ==
%%%
%%% create → activate → execute → pause → terminate
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_session).

-behaviour(gen_server).

%% API
-export([start_link/2, get_info/1, execute_command/3, pause_session/1,
         resume_session/1, terminate_session/1, get_metrics/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(cli_session, {
    id :: binary(),
    pid :: pid(),
    status :: active | paused | terminated,
    created :: integer(),
    last_activity :: integer(),
    command_count :: integer(),
    trace_context :: map(),
    metrics :: map(),
    config :: map(),
    parser :: pid() | undefined,
    executor :: pid() | undefined,
    transport :: pid() | undefined
}).

-record(session_state, {
    session :: #cli_session{},
    command_queue :: queue:queue(),
    active_commands :: map(),
    shutdown_timer :: reference() | undefined
}).

-define(SERVER, ?MODULE).
-define(DEFAULT_CONFIG, #{
    command_timeout => 10000,
    max_commands => 100,
    session_timeout => 300000,
    enable_metrics => true,
    enable_tracing => true
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a CLI session
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(SessionId, Config) ->
    gen_server:start_link({local, SessionId}, ?MODULE, [SessionId, Config], []).

%% @doc Get session information
-spec get_info(pid()) -> {ok, #cli_session{}} | {error, term()}.
get_info(SessionPid) ->
    gen_server:call(SessionPid, get_info).

%% @doc Execute a command in the session
-spec execute_command(pid(), binary(), list()) -> {ok, term()} | {error, term()}.
execute_command(SessionPid, Command, Args) ->
    gen_server:call(SessionPid, {execute_command, Command, Args}, 15000).

%% @doc Pause the session
-spec pause_session(pid()) -> ok | {error, term()}.
pause_session(SessionPid) ->
    gen_server:cast(SessionPid, pause_session).

%% @doc Resume the session
-spec resume_session(pid()) -> ok | {error, term()}.
resume_session(SessionPid) ->
    gen_server:cast(SessionPid, resume_session).

%% @doc Terminate the session
-spec terminate_session(pid()) -> ok.
terminate_session(SessionPid) ->
    gen_server:cast(SessionPid, terminate_session).

%% @doc Get session metrics
-spec get_metrics(pid()) -> {ok, map()} | {error, term()}.
get_metrics(SessionPid) ->
    gen_server:call(SessionPid, get_metrics).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the session
-spec init([binary(), map()]) -> {ok, #session_state{}} | {stop, term()}.
init([SessionId, Config]) ->
    %% Create OTEL span for session initialization
    SpanCtx = erlmcp_otel:with_span("cli.session.create", #{
        <<"session.id">> => SessionId,
        <<"session.created">> => erlang:system_time(millisecond)
    }, fun() ->
        ok
    end),

    %% Initialize session
    Session = #cli_session{
        id = SessionId,
        pid = self(),
        status = active,
        created = erlang:system_time(millisecond),
        last_activity = erlang:system_time(millisecond),
        command_count = 0,
        trace_context = SpanCtx,
        metrics = #{},
        config = maps:merge(?DEFAULT_CONFIG, Config),
        parser = undefined,
        executor = undefined,
        transport = undefined
    },

    State = #session_state{
        session = Session,
        command_queue = queue:new(),
        active_commands = #{},
        shutdown_timer = undefined
    },

    %% Start session components
    case start_session_components(Session) of
        {ok, NewSession} ->
            erlmcp_metrics:record("cli.sessions.created", 1),
            {ok, State#session_state{session = NewSession}};
        {error, Reason} ->
            {stop, Reason}
    end.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #session_state{}) ->
    {reply, term(), #session_state{}} | {stop, term(), #session_state{}}.
handle_call(get_info, _From, State) ->
    Reply = {ok, State#session_state.session},
    {reply, Reply, State};

handle_call({execute_command, Command, Args}, From, State) ->
    %% Check session status
    case State#session_state.session#cli_session.status of
        active ->
            %% Create span for command execution
            SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.command">>,
                                                make_request_id(),
                                                #{
                                                    <<"command">> => Command,
                                                    <<"args">> => Args
                                                },
                                                State#session_state.session#cli_session.trace_context),

            %% Validate command
            case validate_command(Command, Args) of
                {ok, Validated} ->
                    %% Add to command queue
                    CommandRef = make_command_ref(),
                    CommandData = #{
                        ref => CommandRef,
                        from => From,
                        command => Command,
                        args => Args,
                        validated => Validated,
                        start_time => erlang:system_time(millisecond),
                        span_ctx => SpanCtx
                    },

                    NewQueue = queue:in(CommandRef, State#session_state.command_queue),
                    NewState = State#session_state{
                        command_queue = NewQueue,
                        session = update_activity(State#session_state.session)
                    },

                    %% Process command immediately if queue is empty
                    case queue:len(NewQueue) of
                        1 ->
                            process_next_command(NewState);
                        _ ->
                            %% Queue command
                            {noreply, NewState}
                    end;
                {error, Reason} ->
                    %% Record error metrics
                    erlmcp_otel:record_error(SpanCtx, {validation_failed, Reason}),
                    erlmcp_metrics:record("cli.commands.failed", 1),
                    Reply = {error, {validation_failed, Reason}},
                    {reply, Reply, State}
            end;
        _ ->
            Reply = {error, {session_not_active, State#session_state.session#cli_session.status}},
            {reply, Reply, State}
    end;

handle_call(get_metrics, _From, State) ->
    Metrics = State#session_state.session#cli_session.metrics,
    Reply = {ok, Metrics},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #session_state{}) -> {noreply, #session_state{}} | {stop, term(), #session_state{}}.
handle_cast(pause_session, State) ->
    %% Pause the session
    UpdatedSession = State#session_state.session#cli_session{
        status = paused
    },

    %% Stop shutdown timer
    NewState = case State#session_state.shutdown_timer of
        undefined -> State;
        Timer -> erlang:cancel_timer(Timer), State
    end,

    %% Pause all active commands
    lists:foreach(fun(CommandRef) ->
        pause_command(CommandRef, NewState)
    end, maps:keys(State#session_state.active_commands)),

    %% Create span for pause
    erlmcp_otel:add_event(State#session_state.session#cli_session.trace_context,
                          <<"session.paused">>, #{
                              <<"session.id">> => UpdatedSession#cli_session.id
                          }),

    erlmcp_metrics:record("cli.sessions.paused", 1),
    {noreply, NewState#session_state{session = UpdatedSession}};

handle_cast(resume_session, State) ->
    %% Resume the session
    UpdatedSession = State#session_state.session#cli_session{
        status = active
    },

    %% Resume all paused commands
    lists:foreach(fun(CommandRef) ->
        resume_command(CommandRef, NewState)
    end, maps:keys(State#session_state.active_commands)),

    %% Process next command if queue is not empty
    NewState = case queue:out(State#session_state.command_queue) of
        {{value, CommandRef}, RestQueue} ->
            process_command(CommandRef, State#session_state{command_queue = RestQueue});
        {empty, RestQueue} ->
            State#session_state{command_queue = RestQueue}
    end,

    %% Create span for resume
    erlmcp_otel:add_event(State#session_state.session#cli_session.trace_context,
                          <<"session.resumed">>, #{
                              <<"session.id">> => UpdatedSession#cli_session.id
                          }),

    erlmcp_metrics:record("cli.sessions.resumed", 1),
    {noreply, NewState#session_state{session = UpdatedSession}};

handle_cast(terminate_session, State) ->
    %% Graceful termination
    Self = self(),
    spawn_link(fun() ->
        %% Send termination message after short delay
        timer:sleep(100),
        Self ! terminate_now
    end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #session_state{}) -> {noreply, #session_state{}} | {stop, term(), #session_state{}}.
handle_info({command_complete, CommandRef, Result}, State) ->
    %% Remove from active commands
    ActiveCommands = maps:remove(CommandRef, State#session_state.active_commands),

    %% Record metrics
    CommandTime = erlang:system_time(millisecond) -
                   get_command_start_time(CommandRef, State),

    erlmcp_metrics:record("cli.commands.completed", 1),
    erlmcp_metrics:record("cli.command.latency", CommandTime),

    %% Send response to caller
    case maps:get(CommandRef, State#session_state.active_commands, undefined) of
        #{from := From} ->
            gen_server:reply(From, Result);
        _ ->
            ok
    end,

    %% Update session metrics
    UpdatedSession = State#session_state.session#cli_session{
        command_count = State#session_state.session#cli_session.command_count + 1,
        metrics = update_metrics(State#session_state.session#cli_session.metrics, Result, CommandTime)
    },

    %% Process next command
    NewState = State#session_state{
        active_commands = ActiveCommands,
        session = UpdatedSession
    },

    process_next_command(NewState);

handle_info({command_failed, CommandRef, Error}, State) ->
    %% Remove from active commands
    ActiveCommands = maps:remove(CommandRef, State#session_state.active_commands),

    %% Record error metrics
    erlmcp_metrics:record("cli.commands.failed", 1),
    erlmcp_metrics:record("cli.errors.total", 1),

    %% Send error to caller
    case maps:get(CommandRef, State#session_state.active_commands, undefined) of
        #{from := From} ->
            gen_server:reply(From, {error, Error});
        _ ->
            ok
    end,

    %% Update session metrics
    UpdatedSession = State#session_state.session#cli_session{
        metrics = update_metrics(State#session_state.session#cli_session.metrics, {error, Error}, 0)
    },

    %% Process next command
    NewState = State#session_state{
        active_commands = ActiveCommands,
        session = UpdatedSession
    },

    process_next_command(NewState);

handle_info(terminate_now, State) ->
    %% Force termination
    {stop, normal, State};

handle_info(timeout, State) ->
    %% Session timeout - terminate
    {stop, timeout, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the session
-spec terminate(term(), #session_state{}) -> ok.
terminate(Reason, State) ->
    %% Record termination metrics
    erlmcp_metrics:record("cli.sessions.terminated", 1),

    %% Create termination span
    erlmcp_otel:with_span("cli.session.terminate", #{
        <<"session.id">> => State#session_state.session#cli_session.id,
        <<"session.status">> => State#session_state.session#cli_session.status,
        <<"termination.reason">> => atom_to_binary(Reason)
    }, fun() ->
        %% End session span
        erlmcp_otel:end_span(State#session_state.session#cli_session.trace_context),
        ok
    end).

%% @doc Code change
-spec code_change(term(), #session_state{}, term()) -> {ok, #session_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Start session components
-spec start_session_components(#cli_session{}) -> {ok, #cli_session{}} | {error, term()}.
start_session_components(Session) ->
    try
        %% Start command parser
        Parser = spawn_link(fun() -> parser_loop() end),

        %% Start command executor
        Executor = spawn_link(fun() -> executor_loop() end),

        %% Start transport
        Transport = spawn_link(fun() -> transport_loop() end),

        %% Update session with component PIDs
        UpdatedSession = Session#cli_session{
            parser = Parser,
            executor = Executor,
            transport = Transport
        },

        %% Start session timeout timer
        Timeout = Session#cli_session.config#{session_timeout},
        ShutdownTimer = erlang:start_timer(Timeout, self(), timeout),

        %% Record component start
        erlmcp_otel:add_event(Session#cli_session.trace_context,
                              <<"session.components.started">>, #{
                                  <<"parser.pid">> => pid_to_list(Parser),
                                  <<"executor.pid">> => pid_to_list(Executor),
                                  <<"transport.pid">> => pid_to_list(Transport)
                              }),

        {ok, UpdatedSession#cli_session{shutdown_timer = ShutdownTimer}}
    catch
        Error:Reason:Stacktrace ->
            erlmcp_otel:record_error(Session#cli_session.trace_context,
                                     {component_start_failed, Error, Reason, Stacktrace}),
            {error, Reason}
    end.

%% @doc Process the next command in the queue
-spec process_next_command(#session_state{}) -> {noreply, #session_state{}}.
process_next_command(State) ->
    case queue:out(State#session_state.command_queue) of
        {{value, CommandRef}, RestQueue} ->
            process_command(CommandRef, State#session_state{command_queue = RestQueue});
        {empty, RestQueue} ->
            State#session_state{command_queue = RestQueue}
    end.

%% @doc Process a command
-spec process_command(binary(), #session_state{}) -> {noreply, #session_state{}}.
process_command(CommandRef, State) ->
    case maps:get(CommandRef, State#session_state.command_queue_map, undefined) of
        #{command := Command, args := Args, span_ctx := SpanCtx} = CommandData ->
            %% Execute command in executor
            Executor = State#session_state.session#cli_session.executor,

            %% Add to active commands
            ActiveCommands = maps:put(CommandRef, CommandData, State#session_state.active_commands),

            %% Send command to executor
            Executor ! {execute, CommandRef, Command, Args, SpanCtx},

            %% Record command start time
            StartTimes = maps:put(CommandRef, erlang:system_time(millisecond),
                                 State#session_state.command_start_times),

            {noreply, State#session_state{
                active_commands = ActiveCommands,
                command_start_times = StartTimes
            }};
        undefined ->
            %% Command not found - should not happen
            {noreply, State}
    end.

%% @doc Validate command safety
-spec validate_command(binary(), list()) -> {ok, map()} | {error, term()}.
validate_command(Command, Args) ->
    %% Check for dangerous commands
    DangerousCommands = [<<"eval">>, <<"exec">>, <<"system">>, <<"shell">>],
    case lists:member(Command, DangerousCommands) of
        true ->
            {error, dangerous_command};
        false ->
            %% Validate command exists
            case erlmcp_cli_registry:lookup_command(Command) of
                {ok, _} ->
                    %% Validate arguments
                    case validate_arguments(Command, Args) of
                        {ok, Validated} ->
                            {ok, Validated};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, not_found} ->
                    {error, {command_not_found, Command}}
            end
    end.

%% @doc Validate command arguments
-spec validate_command(binary(), list()) -> {ok, map()} | {error, term()}.
validate_command(Command, Args) ->
    %% Validate based on command schema
    case erlmcp_cli_registry:get_command_schema(Command) of
        {ok, Schema} ->
            validate_against_schema(Args, Schema);
        {error, not_found} ->
            {error, {command_not_found, Command}}
    end.

%% @doc Update activity timestamp
-spec update_activity(#cli_session{}) -> #cli_session{}.
update_activity(Session) ->
    Session#cli_session{
        last_activity = erlang:system_time(millisecond)
    }.

%% @doc Update session metrics
-spec update_metrics(map(), term(), integer()) -> map().
update_metrics(Metrics, Result, CommandTime) ->
    Updated = Metrics,

    %% Update command count
    Updated1 = maps:update_with("commands.total", fun(V) -> V + 1 end, 1, Updated),

    %% Update success/failure count
    case Result of
        {ok, _} ->
            Updated2 = maps:update_with("commands.success", fun(V) -> V + 1 end, 1, Updated1);
        {error, _} ->
            Updated2 = maps:update_with("commands.failed", fun(V) -> V + 1 end, 1, Updated1)
    end,

    %% Update latency metrics
    Updated3 = maps:update_with("latency.values",
                               fun(V) -> [CommandTime | V] end,
                               [CommandTime], Updated2),

    %% Calculate percentiles
    Values = lists:sublist(lists:reverse(lists:sort(Updated3#{latency.values})), 1000),
    case length(Values) > 0 of
        true ->
            Percentiles = calculate_percentiles(Values),
            maps:merge(Updated3, Percentiles);
        false ->
            Updated3
    end.

%% @doc Calculate percentiles
-spec calculate_percentiles(list()) -> map().
calculate_percentiles(Values) ->
    Length = length(Values),
    Percentiles = #{
        "latency.p50" => lists:nth(max(1, trunc(Length * 0.5)), Values),
        "latency.p90" => lists:nth(max(1, trunc(Length * 0.9)), Values),
        "latency.p95" => lists:nth(max(1, trunc(Length * 0.95)), Values),
        "latency.p99" => lists:nth(max(1, trunc(Length * 0.99)), Values)
    },

    Percentiles.

%% @doc Make command reference
-spec make_command_ref() -> binary().
make_command_ref() ->
    Id = crypto:strong_rand_bytes(8),
    base64:encode(Id).

%% @doc Make request ID
-spec make_request_id() -> binary().
make_request_id() ->
    Id = crypto:strong_rand_bytes(16),
    base64:encode(Id).

%% @ Parser loop
-spec parser_loop() -> no_return().
parser_loop() ->
    receive
        {parse, CommandRef, Command, Args, ReplyTo} ->
            %% Parse command
            Parsed = parse_command(Command, Args),
            ReplyTo ! {parse_result, CommandRef, Parsed},
            parser_loop();
        stop ->
            ok
    end.

%% @doc Command executor loop
-spec executor_loop() -> no_return().
executor_loop() ->
    receive
        {execute, CommandRef, Command, Args, SpanCtx} ->
            %% Execute command
            Result = execute_command_impl(Command, Args),

            %% Record result
            erlmcp_otel:with_span("cli.command.execute", #{
                <<"command">> => Command
            }, fun() ->
                case Result of
                    {ok, _} ->
                        ok;
                    {error, _} ->
                        erlmcp_otel:record_error(SpanCtx, {command_failed, Result})
                end
            end),

            %% Send result
            erlmcp_cli_session ! {command_complete, CommandRef, Result},
            executor_loop();
        stop ->
            ok
    end.

%% @doc Transport loop
-spec transport_loop() -> no_return().
transport_loop() ->
    receive
        {send, Data, ReplyTo} ->
            %% Send data
            ReplyTo ! {send_result, ok},
            transport_loop();
        stop ->
            ok
    end.

%% @doc Parse command
-spec parse_command(binary(), list()) -> {ok, map()} | {error, term()}.
parse_command(Command, Args) ->
    %% Simple command parsing
    {ok, #{
        command => Command,
        args => Args,
        parsed => true
    }}.

%% @doc Execute command implementation
-spec execute_command_impl(binary(), list()) -> {ok, term()} | {error, term()}.
execute_command_impl(Command, Args) ->
    %% Execute command via registry
    erlmcp_cli_registry:execute_command(Command, Args).