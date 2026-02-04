-module(erlmcp_health_check).
-behaviour(gen_server).

%% API
-export([start_link/0, health_check/0, detailed_health_check/0]).
-export([register_check/2, unregister_check/1]).
-export([start_periodic_check/1, stop_periodic_check/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_CHECK_INTERVAL, 30000).  % 30 seconds
-define(CHECK_TIMEOUT, 5000).  % 5 seconds

%% Records
-record(check_result, {
    name :: atom(),
    status :: ok | warning | error,
    message :: binary(),
    timestamp :: erlang:timestamp(),
    duration :: non_neg_integer(),
    details :: map()
}).

-record(check_definition, {
    name :: atom(),
    module :: atom(),
    function :: atom(),
    args :: list(),
    timeout :: pos_integer(),
    critical :: boolean()
}).

-record(state, {
    checks :: #{atom() => #check_definition{}},
    results :: #{atom() => #check_result{}},
    periodic_timers :: #{atom() => reference()},
    overall_status :: ok | warning | error
}).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec health_check() -> map().
health_check() ->
    gen_server:call(?SERVER, health_check, ?CHECK_TIMEOUT).

-spec detailed_health_check() -> map().
detailed_health_check() ->
    gen_server:call(?SERVER, detailed_health_check, ?CHECK_TIMEOUT).

-spec register_check(atom(), module()) -> ok | {error, term()}.
register_check(Name, Module) when is_atom(Name), is_atom(Module) ->
    gen_server:call(?SERVER, {register_check, Name, Module}).

-spec unregister_check(atom()) -> ok | {error, term()}.
unregister_check(Name) when is_atom(Name) ->
    gen_server:call(?SERVER, {unregister_check, Name}).

-spec start_periodic_check(atom()) -> ok | {error, term()}.
start_periodic_check(Name) when is_atom(Name) ->
    gen_server:call(?SERVER, {start_periodic_check, Name}).

-spec stop_periodic_check(atom()) -> ok | {error, term()}.
stop_periodic_check(Name) when is_atom(Name) ->
    gen_server:call(?SERVER, {stop_periodic_check, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % Register default health checks
    DefaultChecks = [
        {memory, erlmcp_health_check_memory},
        {processes, erlmcp_health_check_processes},
        {connections, erlmcp_health_check_connections},
        {registry, erlmcp_health_check_registry},
        {supervision, erlmcp_health_check_supervision}
    ],

    Checks = lists:foldl(
        fun({Name, Module}, Acc) ->
            case validate_check_module(Module) of
                true ->
                    CheckDef = #check_definition{
                        name = Name,
                        module = Module,
                        function = check,
                        args = [],
                        timeout = ?CHECK_TIMEOUT,
                        critical = true
                    },
                    maps:put(Name, CheckDef, Acc);
                false ->
                    Acc
            end
        end,
        #{},
        DefaultChecks
    ),

    State = #state{
        checks = Checks,
        results = #{},
        periodic_timers = #{},
        overall_status = ok
    },

    {ok, State}.

handle_call(health_check, _From, State) ->
    % Run all checks and return summary
    Results = run_all_checks(State),
    Summary = generate_health_summary(Results),
    {reply, Summary, State};

handle_call(detailed_health_check, _From, State) ->
    % Run all checks and return detailed results
    Results = run_all_checks(State),
    DetailedReport = generate_detailed_report(Results),
    {reply, DetailedReport, State};

handle_call({register_check, Name, Module}, _From, State) ->
    case validate_check_module(Module) of
        true ->
            CheckDef = #check_definition{
                name = Name,
                module = Module,
                function = check,
                args = [],
                timeout = ?CHECK_TIMEOUT,
                critical = true
            },
            NewChecks = maps:put(Name, CheckDef, State#state.checks),
            {reply, ok, State#state{checks = NewChecks}};
        false ->
            {reply, {error, invalid_check_module}, State}
    end;

handle_call({unregister_check, Name}, _From, State) ->
    NewChecks = maps:remove(Name, State#state.checks),
    NewResults = maps:remove(Name, State#state.results),
    NewTimers = maps:remove(Name, State#state.periodic_timers),
    {reply, ok, State#state{
        checks = NewChecks,
        results = NewResults,
        periodic_timers = NewTimers
    }};

handle_call({start_periodic_check, Name}, _From, State) ->
    case maps:is_key(Name, State#state.checks) of
        true ->
            Timer = erlang:send_after(?DEFAULT_CHECK_INTERVAL, self(), {periodic_check, Name}),
            NewTimers = maps:put(Name, Timer, State#state.periodic_timers),
            {reply, ok, State#state{periodic_timers = NewTimers}};
        false ->
            {reply, {error, check_not_found}, State}
    end;

handle_call({stop_periodic_check, Name}, _From, State) ->
    case maps:find(Name, State#state.periodic_timers) of
        {ok, Timer} ->
            erlang:cancel_timer(Timer),
            NewTimers = maps:remove(Name, State#state.periodic_timers),
            {reply, ok, State#state{periodic_timers = NewTimers}};
        error ->
            {reply, ok, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({periodic_check, Name}, State) ->
    case maps:find(Name, State#state.checks) of
        {ok, CheckDef} ->
            % Run the check
            {Status, Message, Duration, Details} = run_single_check(CheckDef),
            Result = #check_result{
                name = Name,
                status = Status,
                message = Message,
                timestamp = erlang:timestamp(),
                duration = Duration,
                details = Details
            },

            % Update results
            NewResults = maps:put(Name, Result, State#state.results),

            % Update overall status
            NewOverallStatus = calculate_overall_status(NewResults),

            % Restart timer
            NewTimer = erlang:send_after(?DEFAULT_CHECK_INTERVAL, self(), {periodic_check, Name}),

            NewState = State#state{
                results = NewResults,
                overall_status = NewOverallStatus,
                periodic_timers = maps:put(Name, NewTimer, State#state.periodic_timers)
            },

            % Log the result
            log_check_result(Result),

            {noreply, NewState};
        error ->
            % Check not found, remove timer
            NewTimers = maps:remove(Name, State#state.periodic_timers),
            {noreply, State#state{periodic_timers = NewTimers}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Clean up all timers
    lists:foreach(fun(Timer) ->
        erlang:cancel_timer(Timer)
    end, maps:values(State#state.periodic_timers)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

validate_check_module(Module) ->
    % Validate that the module implements the check/0 function
    try
        Exported = Module:module_info(exports),
        lists:member({check, 0}, Exported)
    catch
        _ -> false
    end.

run_all_checks(State) ->
    lists:map(
        fun({Name, CheckDef}) ->
            {Status, Message, Duration, Details} = run_single_check(CheckDef),
            Result = #check_result{
                name = Name,
                status = Status,
                message = Message,
                timestamp = erlang:timestamp(),
                duration = Duration,
                details = Details
            },
            {Name, Result}
        end,
        maps:to_list(State#state.checks)
    ).

run_single_check(#check_definition{module = Module, function = Function, args = Args, timeout = _Timeout}) ->
    StartTime = erlang:monotonic_time(millisecond),

    try
        Result = apply(Module, Function, Args),
        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        case Result of
            {ok, Message, Details} ->
                {ok, Message, Duration, Details};
            {warning, Message, Details} ->
                {warning, Message, Duration, Details};
            {error, Message, Details} ->
                {error, Message, Duration, Details};
            ok ->
                {ok, <<"Check passed">>, Duration, #{}};
            {warning, Message} ->
                {warning, Message, Duration, #{}};
            {error, Message} ->
                {error, Message, Duration, #{}};
            _ ->
                {ok, <<"Check completed">>, Duration, #{result => Result}}
        end
    catch
        Error:Reason ->
            CatchEndTime = erlang:monotonic_time(millisecond),
            CatchDuration = CatchEndTime - StartTime,
            {error, binary_to_list(io_lib:format("Check failed: ~p:~p", [Error, Reason])), CatchDuration, #{}}
    end.

generate_health_summary(Results) ->
    StatusCounts = lists:foldl(
        fun({_Name, #check_result{status = Status}}, Acc) ->
            maps:update_with(Status, fun(N) -> N + 1 end, 1, Acc)
        end,
        #{ok => 0, warning => 0, error => 0},
        Results
    ),

    TotalChecks = maps:values(StatusCounts),
    Total = lists:sum(TotalChecks),

    #{
        status => calculate_overall_status_from_results(Results),
        total_checks => Total,
        passed => maps:get(ok, StatusCounts, 0),
        warnings => maps:get(warning, StatusCounts, 0),
        errors => maps:get(error, StatusCounts, 0),
        timestamp => erlang:timestamp(),
        uptime => uptime()
    }.

generate_detailed_report(Results) ->
    HealthSummary = generate_health_summary(Results),

    CheckResults = lists:map(
        fun({Name, Result}) ->
            #{
                name => Name,
                status => Result#check_result.status,
                message => Result#check_result.message,
                timestamp => timestamp_to_iso8601(Result#check_result.timestamp),
                duration_ms => Result#check_result.duration,
                details => Result#check_result.details
            }
        end,
        Results
    ),

    HealthSummary#{checks => CheckResults}.

calculate_overall_status(State) ->
    Results = State#state.results,
    calculate_overall_status_from_results(Results).

calculate_overall_status_from_results(Results) ->
    ErrorCount = length([R || {_Name, R} <- Results, R#check_result.status =:= error]),
    WarningCount = length([R || {_Name, R} <- Results, R#check_result.status =:= warning]),

    if
        ErrorCount > 0 -> error;
        WarningCount > 0 -> warning;
        true -> ok
    end.

log_check_result(Result) ->
    Message = io_lib:format("Health check '~s' ~s: ~s (~pms)",
                           [Result#check_result.name,
                            Result#check_result.status,
                            Result#check_result.message,
                            Result#check_result.duration]),

    case Result#check_result.status of
        ok -> erlmcp_observability:log(info, iolist_to_binary(Message), #{});
        warning -> erlmcp_observability:log(warn, iolist_to_binary(Message), #{});
        error -> erlmcp_observability:log(error, iolist_to_binary(Message), #{})
    end.

timestamp_to_iso8601({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    MicroSecPart = integer_to_binary(MicroSecs div 1000),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~sZ",
                 [Year, Month, Day, Hour, Min, Sec, MicroSecPart]).

uptime() ->
    % Return uptime in milliseconds since system start
    erlang:monotonic_time(millisecond).