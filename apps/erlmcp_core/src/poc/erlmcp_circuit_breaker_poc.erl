%%%-------------------------------------------------------------------
%%% @doc
%%% Circuit Breaker POC with Telemetry Integration
%%%
%%% Demonstrates:
%%% 1. gen_statem implementation with closed/open/half_open states
%%% 2. Telemetry events on all state transitions
%%% 3. Per-tool circuit breakers for failure isolation
%%% 4. Gradual recovery through half-open state
%%% 5. Dashboard-friendly state reporting
%%% 6. Metrics: trip count, recovery time, failure rate
%%%
%%% Usage:
%%%   erlmcp_circuit_breaker_poc:run_demo().
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_circuit_breaker_poc).

-behaviour(gen_statem).

%% API
-export([start_link/1, start_link/2, call_tool/2, get_state/1, get_stats/1, reset/1, stop/1]).
%% Demo API
-export([run_demo/0, run_demo/1]).
%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% State callbacks
-export([closed/3, open/3, half_open/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type tool_name() :: binary().
-type call_result() :: {ok, term()} | {error, term()}.
-type breaker_config() ::
    #{failure_threshold => pos_integer(),
      success_threshold => pos_integer(),
      timeout_ms => pos_integer(),
      half_open_max_calls => pos_integer()}.

                                                  % Failures to trip (default: 3)

                                             % Successes in half_open to close (default: 2)
             % Time before half_open attempt (default: 5000)

                                             % Max calls in half_open (default: 3)

-record(data,
        {tool_name :: tool_name(),
         config :: breaker_config(),
         %% Counters
         consecutive_failures = 0 :: non_neg_integer(),
         consecutive_successes = 0 :: non_neg_integer(),
         total_calls = 0 :: non_neg_integer(),
         total_successes = 0 :: non_neg_integer(),
         total_failures = 0 :: non_neg_integer(),
         total_rejected = 0 :: non_neg_integer(),
         trip_count = 0 :: non_neg_integer(),
         %% Timing
         last_failure_time :: undefined | integer(),
         last_state_change :: integer(),
         open_at :: undefined | integer(),
         recovery_start :: undefined | integer(),
         %% Half-open state tracking
         half_open_calls = 0 :: non_neg_integer(),
         %% Metrics
         failure_rate = 0.0 :: float(),
         avg_recovery_time_ms = 0.0 :: float(),
         total_recovery_time_ms = 0 :: non_neg_integer()}).

-define(DEFAULT_CONFIG,
        #{failure_threshold => 3,
          success_threshold => 2,
          timeout_ms => 5000,
          half_open_max_calls => 3}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a circuit breaker for a tool
-spec start_link(tool_name()) -> {ok, pid()} | {error, term()}.
start_link(ToolName) ->
    start_link(ToolName, #{}).

-spec start_link(tool_name(), breaker_config()) -> {ok, pid()} | {error, term()}.
start_link(ToolName, Config) ->
    gen_statem:start_link(?MODULE, {ToolName, Config}, []).

%% @doc Call a tool through the circuit breaker
-spec call_tool(pid(), fun(() -> call_result())) -> call_result().
call_tool(Pid, Fun) ->
    gen_statem:call(Pid, {call_tool, Fun}, 10000).

%% @doc Get current state
-spec get_state(pid()) -> {closed | open | half_open, map()}.
get_state(Pid) ->
    gen_statem:call(Pid, get_state).

%% @doc Get statistics
-spec get_stats(pid()) -> map().
get_stats(Pid) ->
    gen_statem:call(Pid, get_stats).

%% @doc Reset circuit breaker to closed state
-spec reset(pid()) -> ok.
reset(Pid) ->
    gen_statem:call(Pid, reset).

%% @doc Stop circuit breaker
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

%%====================================================================
%% Demo Functions
%%====================================================================

%% @doc Run the circuit breaker demo with default scenario
-spec run_demo() -> ok.
run_demo() ->
    run_demo(#{}).

%% @doc Run circuit breaker demo with custom options
-spec run_demo(map()) -> ok.
run_demo(Opts) ->
    io:format("~n=== Circuit Breaker POC Demo ===~n~n"),

    %% Initialize telemetry (mock if not available)
    init_telemetry(),

    ToolName = maps:get(tool_name, Opts, <<"test_tool">>),
    Config =
        maps:get(config,
                 Opts,
                 #{failure_threshold => 3,
                   success_threshold => 2,
                   timeout_ms => 2000,
                   half_open_max_calls => 3}),

    io:format("Starting circuit breaker for tool: ~s~n", [ToolName]),
    io:format("Configuration: ~p~n~n", [Config]),

    {ok, Breaker} = start_link(ToolName, Config),

    %% Phase 1: Normal operation (closed state)
    io:format("~n--- Phase 1: Normal Operation (CLOSED) ---~n"),
    demo_successful_calls(Breaker, 5),
    print_state(Breaker),

    %% Phase 2: Failures causing trip (closed -> open)
    io:format("~n--- Phase 2: Failures Causing Trip (CLOSED -> OPEN) ---~n"),
    demo_failing_calls(Breaker, 4),
    print_state(Breaker),

    %% Phase 3: Rejected calls while open
    io:format("~n--- Phase 3: Rejected Calls (OPEN) ---~n"),
    demo_rejected_calls(Breaker, 3),
    print_state(Breaker),

    %% Phase 4: Wait for timeout and half-open
    io:format("~n--- Phase 4: Waiting for Recovery Window ---~n"),
    TimeoutMs = maps:get(timeout_ms, Config),
    io:format("Waiting ~p ms for half-open timeout...~n", [TimeoutMs]),
    timer:sleep(TimeoutMs + 100),
    print_state(Breaker),

    %% Phase 5: Gradual recovery (half-open -> closed)
    io:format("~n--- Phase 5: Gradual Recovery (HALF_OPEN -> CLOSED) ---~n"),
    demo_recovery_calls(Breaker, 3),
    print_state(Breaker),

    %% Phase 6: Back to normal
    io:format("~n--- Phase 6: Back to Normal (CLOSED) ---~n"),
    demo_successful_calls(Breaker, 3),
    print_state(Breaker),

    %% Final statistics
    io:format("~n--- Final Statistics ---~n"),
    print_stats(Breaker),

    %% Cleanup
    stop(Breaker),

    io:format("~n=== Demo Complete ===~n~n"),
    ok.

%%====================================================================
%% gen_statem callbacks
%%====================================================================

init({ToolName, Config}) ->
    process_flag(trap_exit, true),

    MergedConfig = maps:merge(?DEFAULT_CONFIG, Config),

    Data =
        #data{tool_name = ToolName,
              config = MergedConfig,
              last_state_change = erlang:system_time(millisecond)},

    %% Emit telemetry event
    emit_telemetry(breaker_initialized,
                   ToolName,
                   #{config => MergedConfig, initial_state => closed}),

    ?LOG_INFO("Circuit breaker initialized for tool: ~p", [ToolName]),

    {ok, closed, Data}.

callback_mode() ->
    state_functions.

%%====================================================================
%% State Callbacks
%%====================================================================

%% CLOSED state - normal operation
closed({call, From}, {call_tool, Fun}, Data) ->
    StartTime = erlang:system_time(microsecond),

    Result = execute_call(Fun),

    EndTime = erlang:system_time(microsecond),
    Latency = EndTime - StartTime,

    NewData = Data#data{total_calls = Data#data.total_calls + 1},

    case Result of
        {ok, _Value} ->
            %% Success
            UpdatedData =
                NewData#data{consecutive_failures = 0,
                             consecutive_successes = Data#data.consecutive_successes + 1,
                             total_successes = Data#data.total_successes + 1},

            emit_telemetry(call_succeeded,
                           Data#data.tool_name,
                           #{state => closed,
                             latency_us => Latency,
                             consecutive_successes => UpdatedData#data.consecutive_successes}),

            {keep_state, UpdatedData, [{reply, From, Result}]};
        {error, Reason} ->
            %% Failure
            NewFailures = Data#data.consecutive_failures + 1,
            UpdatedData =
                NewData#data{consecutive_failures = NewFailures,
                             consecutive_successes = 0,
                             total_failures = Data#data.total_failures + 1,
                             last_failure_time = erlang:system_time(millisecond)},

            emit_telemetry(call_failed,
                           Data#data.tool_name,
                           #{state => closed,
                             error => Reason,
                             consecutive_failures => NewFailures}),

            %% Check if we should trip
            Threshold = maps:get(failure_threshold, Data#data.config),
            case NewFailures >= Threshold of
                true ->
                    %% Trip to OPEN
                    trip_to_open(From, Result, UpdatedData);
                false ->
                    {keep_state, UpdatedData, [{reply, From, Result}]}
            end
    end;
closed({call, From}, get_state, Data) ->
    StateData = build_state_data(closed, Data),
    {keep_state_and_data, [{reply, From, {closed, StateData}}]};
closed({call, From}, get_stats, Data) ->
    Stats = build_stats(closed, Data),
    {keep_state_and_data, [{reply, From, Stats}]};
closed({call, From}, reset, Data) ->
    {keep_state, reset_counters(Data), [{reply, From, ok}]};
closed(EventType, Event, Data) ->
    handle_common(EventType, Event, Data).

%% OPEN state - rejecting calls
open({call, From}, {call_tool, _Fun}, Data) ->
    %% Reject call
    UpdatedData =
        Data#data{total_calls = Data#data.total_calls + 1,
                  total_rejected = Data#data.total_rejected + 1},

    emit_telemetry(call_rejected,
                   Data#data.tool_name,
                   #{state => open, total_rejected => UpdatedData#data.total_rejected}),

    Result = {error, circuit_breaker_open},
    {keep_state, UpdatedData, [{reply, From, Result}]};
open(state_timeout, transition_to_half_open, Data) ->
    transition_to_half_open(Data);
open({call, From}, get_state, Data) ->
    StateData = build_state_data(open, Data),
    {keep_state_and_data, [{reply, From, {open, StateData}}]};
open({call, From}, get_stats, Data) ->
    Stats = build_stats(open, Data),
    {keep_state_and_data, [{reply, From, Stats}]};
open({call, From}, reset, Data) ->
    ResetData = reset_counters(Data),
    emit_telemetry(state_transition,
                   Data#data.tool_name,
                   #{from => open,
                     to => closed,
                     reason => manual_reset}),
    {next_state, closed, ResetData, [{reply, From, ok}]};
open(EventType, Event, Data) ->
    handle_common(EventType, Event, Data).

%% HALF_OPEN state - testing recovery
half_open({call, From}, {call_tool, Fun}, Data) ->
    %% Limit concurrent calls in half-open
    MaxCalls = maps:get(half_open_max_calls, Data#data.config),

    case Data#data.half_open_calls >= MaxCalls of
        true ->
            %% Reject - too many concurrent half-open calls
            UpdatedData =
                Data#data{total_calls = Data#data.total_calls + 1,
                          total_rejected = Data#data.total_rejected + 1},
            Result = {error, circuit_breaker_half_open_limit},
            {keep_state, UpdatedData, [{reply, From, Result}]};
        false ->
            %% Allow the call
            StartTime = erlang:system_time(microsecond),

            TempData =
                Data#data{total_calls = Data#data.total_calls + 1,
                          half_open_calls = Data#data.half_open_calls + 1},

            Result = execute_call(Fun),

            EndTime = erlang:system_time(microsecond),
            Latency = EndTime - StartTime,

            NewData =
                TempData#data{half_open_calls = Data#data.half_open_calls},  % Reset to original

            case Result of
                {ok, _Value} ->
                    %% Success in half-open
                    NewSuccesses = Data#data.consecutive_successes + 1,
                    UpdatedData =
                        NewData#data{consecutive_successes = NewSuccesses,
                                     consecutive_failures = 0,
                                     total_successes = Data#data.total_successes + 1},

                    emit_telemetry(call_succeeded,
                                   Data#data.tool_name,
                                   #{state => half_open,
                                     latency_us => Latency,
                                     consecutive_successes => NewSuccesses}),

                    %% Check if we can close
                    Threshold = maps:get(success_threshold, Data#data.config),
                    case NewSuccesses >= Threshold of
                        true ->
                            %% Close the circuit
                            close_circuit(From, Result, UpdatedData);
                        false ->
                            {keep_state, UpdatedData, [{reply, From, Result}]}
                    end;
                {error, Reason} ->
                    %% Failure in half-open - reopen circuit
                    UpdatedData =
                        NewData#data{consecutive_failures = Data#data.consecutive_failures + 1,
                                     consecutive_successes = 0,
                                     total_failures = Data#data.total_failures + 1,
                                     last_failure_time = erlang:system_time(millisecond)},

                    emit_telemetry(call_failed,
                                   Data#data.tool_name,
                                   #{state => half_open, error => Reason}),

                    %% Re-open the circuit
                    reopen_circuit(From, Result, UpdatedData)
            end
    end;
half_open({call, From}, get_state, Data) ->
    StateData = build_state_data(half_open, Data),
    {keep_state_and_data, [{reply, From, {half_open, StateData}}]};
half_open({call, From}, get_stats, Data) ->
    Stats = build_stats(half_open, Data),
    {keep_state_and_data, [{reply, From, Stats}]};
half_open({call, From}, reset, Data) ->
    ResetData = reset_counters(Data),
    emit_telemetry(state_transition,
                   Data#data.tool_name,
                   #{from => half_open,
                     to => closed,
                     reason => manual_reset}),
    {next_state, closed, ResetData, [{reply, From, ok}]};
half_open(EventType, Event, Data) ->
    handle_common(EventType, Event, Data).

%% Common event handler
handle_common(_EventType, _Event, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, Data) ->
    emit_telemetry(breaker_terminated,
                   Data#data.tool_name,
                   #{final_state => _State, total_calls => Data#data.total_calls}),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Execute a function call
-spec execute_call(fun(() -> call_result())) -> call_result().
execute_call(Fun) ->
    try
        Fun()
    catch
        Class:Reason:_Stacktrace ->
            {error, {exception, Class, Reason}}
    end.

%% @private Transition from CLOSED to OPEN
-spec trip_to_open(gen_statem:from(), call_result(), #data{}) ->
                      {next_state,
                       open,
                       #data{},
                       [{reply, gen_statem:from(), call_result()} |
                        {state_timeout, pos_integer(), atom()}]}.
trip_to_open(From, Result, Data) ->
    Now = erlang:system_time(millisecond),
    Timeout = maps:get(timeout_ms, Data#data.config),

    NewData =
        Data#data{trip_count = Data#data.trip_count + 1,
                  last_state_change = Now,
                  open_at = Now,
                  consecutive_successes = 0,
                  failure_rate = calculate_failure_rate(Data)},

    emit_telemetry(state_transition,
                   Data#data.tool_name,
                   #{from => closed,
                     to => open,
                     reason => failure_threshold_exceeded,
                     consecutive_failures => Data#data.consecutive_failures,
                     trip_count => NewData#data.trip_count,
                     timeout_ms => Timeout}),

    ?LOG_WARNING("Circuit breaker tripped to OPEN for tool: ~p (failures: ~p)",
                 [Data#data.tool_name, Data#data.consecutive_failures]),

    {next_state,
     open,
     NewData,
     [{reply, From, Result}, {state_timeout, Timeout, transition_to_half_open}]}.

%% @private Transition from OPEN to HALF_OPEN
-spec transition_to_half_open(#data{}) -> {next_state, half_open, #data{}, []}.
transition_to_half_open(Data) ->
    Now = erlang:system_time(millisecond),

    NewData =
        Data#data{last_state_change = Now,
                  recovery_start = Now,
                  consecutive_failures = 0,
                  consecutive_successes = 0,
                  half_open_calls = 0},

    emit_telemetry(state_transition,
                   Data#data.tool_name,
                   #{from => open,
                     to => half_open,
                     reason => timeout_expired,
                     time_open_ms => Now - Data#data.open_at}),

    ?LOG_INFO("Circuit breaker transitioning to HALF_OPEN for tool: ~p", [Data#data.tool_name]),

    {next_state, half_open, NewData, []}.

%% @private Close the circuit (HALF_OPEN -> CLOSED)
-spec close_circuit(gen_statem:from(), call_result(), #data{}) ->
                       {next_state, closed, #data{}, [{reply, gen_statem:from(), call_result()}]}.
close_circuit(From, Result, Data) ->
    Now = erlang:system_time(millisecond),
    RecoveryTime = Now - Data#data.recovery_start,

    NewTotalRecoveryTime = Data#data.total_recovery_time_ms + RecoveryTime,
    AvgRecoveryTime = NewTotalRecoveryTime / Data#data.trip_count,

    NewData =
        Data#data{last_state_change = Now,
                  consecutive_failures = 0,
                  recovery_start = undefined,
                  total_recovery_time_ms = NewTotalRecoveryTime,
                  avg_recovery_time_ms = AvgRecoveryTime,
                  failure_rate = calculate_failure_rate(Data)},

    emit_telemetry(state_transition,
                   Data#data.tool_name,
                   #{from => half_open,
                     to => closed,
                     reason => success_threshold_met,
                     consecutive_successes => Data#data.consecutive_successes,
                     recovery_time_ms => RecoveryTime,
                     avg_recovery_time_ms => AvgRecoveryTime}),

    ?LOG_INFO("Circuit breaker closed for tool: ~p (recovery time: ~p ms)",
              [Data#data.tool_name, RecoveryTime]),

    {next_state, closed, NewData, [{reply, From, Result}]}.

%% @private Reopen the circuit (HALF_OPEN -> OPEN)
-spec reopen_circuit(gen_statem:from(), call_result(), #data{}) ->
                        {next_state,
                         open,
                         #data{},
                         [{reply, gen_statem:from(), call_result()} |
                          {state_timeout, pos_integer(), atom()}]}.
reopen_circuit(From, Result, Data) ->
    Now = erlang:system_time(millisecond),
    Timeout = maps:get(timeout_ms, Data#data.config),

    NewData =
        Data#data{last_state_change = Now,
                  open_at = Now,
                  recovery_start = undefined,
                  consecutive_successes = 0,
                  trip_count = Data#data.trip_count + 1,
                  failure_rate = calculate_failure_rate(Data)},

    emit_telemetry(state_transition,
                   Data#data.tool_name,
                   #{from => half_open,
                     to => open,
                     reason => failure_in_half_open,
                     trip_count => NewData#data.trip_count,
                     timeout_ms => Timeout}),

    ?LOG_WARNING("Circuit breaker re-opened for tool: ~p (failed during recovery)",
                 [Data#data.tool_name]),

    {next_state,
     open,
     NewData,
     [{reply, From, Result}, {state_timeout, Timeout, transition_to_half_open}]}.

%% @private Calculate failure rate
-spec calculate_failure_rate(#data{}) -> float().
calculate_failure_rate(#data{total_calls = 0}) ->
    0.0;
calculate_failure_rate(#data{total_failures = Failures, total_calls = Calls}) ->
    Failures / Calls.

%% @private Reset counters
-spec reset_counters(#data{}) -> #data{}.
reset_counters(Data) ->
    Data#data{consecutive_failures = 0,
              consecutive_successes = 0,
              last_state_change = erlang:system_time(millisecond)}.

%% @private Build state data for get_state
-spec build_state_data(atom(), #data{}) -> map().
build_state_data(State, Data) ->
    #{state => State,
      tool_name => Data#data.tool_name,
      consecutive_failures => Data#data.consecutive_failures,
      consecutive_successes => Data#data.consecutive_successes,
      total_calls => Data#data.total_calls,
      total_rejected => Data#data.total_rejected,
      trip_count => Data#data.trip_count,
      failure_rate => calculate_failure_rate(Data)}.

%% @private Build statistics
-spec build_stats(atom(), #data{}) -> map().
build_stats(State, Data) ->
    #{state => State,
      tool_name => Data#data.tool_name,
      config => Data#data.config,
      %% Counters
      consecutive_failures => Data#data.consecutive_failures,
      consecutive_successes => Data#data.consecutive_successes,
      total_calls => Data#data.total_calls,
      total_successes => Data#data.total_successes,
      total_failures => Data#data.total_failures,
      total_rejected => Data#data.total_rejected,
      trip_count => Data#data.trip_count,
      %% Metrics
      failure_rate => calculate_failure_rate(Data),
      success_rate =>
          case Data#data.total_calls of
              0 ->
                  0.0;
              N ->
                  Data#data.total_successes / N
          end,
      avg_recovery_time_ms => Data#data.avg_recovery_time_ms,
      %% Timestamps
      last_failure_time => Data#data.last_failure_time,
      last_state_change => Data#data.last_state_change,
      open_at => Data#data.open_at}.

%% @private Emit telemetry event
-spec emit_telemetry(atom(), tool_name(), map()) -> ok.
emit_telemetry(Event, ToolName, Metadata) ->
    %% Try to use erlmcp_otel if available, otherwise log
    case erlang:function_exported(erlmcp_otel, add_event, 3) of
        true ->
            try
                EventName = iolist_to_binary([<<"circuit_breaker.">>, atom_to_binary(Event, utf8)]),
                Attributes =
                    maps:merge(#{<<"tool_name">> => ToolName,
                                 <<"timestamp">> => erlang:system_time(millisecond)},
                               convert_metadata(Metadata)),

                %% Try to add to current span if it exists
                case erlang:get(erlmcp_otel_current_context) of
                    undefined ->
                        ok;
                    SpanCtx ->
                        erlmcp_otel:add_event(SpanCtx, EventName, Attributes)
                end
            catch
                _:_ ->
                    ok  % Silently ignore telemetry errors
            end;
        false ->
            ok
    end,

    %% Also log significant events
    case Event of
        state_transition ->
            ?LOG_INFO("Circuit breaker ~s: ~p -> ~p (~p)",
                      [ToolName,
                       maps:get(from, Metadata, unknown),
                       maps:get(to, Metadata, unknown),
                       maps:get(reason, Metadata, unknown)]);
        _ ->
            ok
    end.

%% @private Convert metadata to binary keys
-spec convert_metadata(map()) -> map().
convert_metadata(Metadata) ->
    maps:fold(fun(K, V, Acc) ->
                 BinKey =
                     case is_atom(K) of
                         true ->
                             atom_to_binary(K, utf8);
                         false ->
                             K
                     end,
                 BinValue =
                     case V of
                         Val when is_atom(Val) ->
                             atom_to_binary(Val, utf8);
                         Val when is_integer(Val); is_float(Val); is_binary(Val) ->
                             Val;
                         Val ->
                             iolist_to_binary(io_lib:format("~p", [Val]))
                     end,
                 Acc#{BinKey => BinValue}
              end,
              #{},
              Metadata).

%%====================================================================
%% Demo Helper Functions
%%====================================================================

%% @private Initialize telemetry
-spec init_telemetry() -> ok.
init_telemetry() ->
    io:format("Initializing telemetry...~n"),
    %% In a real scenario, this would initialize erlmcp_otel
    ok.

%% @private Run successful calls
-spec demo_successful_calls(pid(), pos_integer()) -> ok.
demo_successful_calls(Breaker, Count) ->
    lists:foreach(fun(N) ->
                     Result = call_tool(Breaker, fun() -> {ok, N * 10} end),
                     io:format("  Call ~p: ~p~n", [N, Result]),
                     timer:sleep(100)
                  end,
                  lists:seq(1, Count)),
    ok.

%% @private Run failing calls
-spec demo_failing_calls(pid(), pos_integer()) -> ok.
demo_failing_calls(Breaker, Count) ->
    lists:foreach(fun(N) ->
                     Result = call_tool(Breaker, fun() -> {error, simulated_failure} end),
                     io:format("  Call ~p: ~p~n", [N, Result]),
                     timer:sleep(100)
                  end,
                  lists:seq(1, Count)),
    ok.

%% @private Demonstrate rejected calls
-spec demo_rejected_calls(pid(), pos_integer()) -> ok.
demo_rejected_calls(Breaker, Count) ->
    lists:foreach(fun(N) ->
                     Result = call_tool(Breaker, fun() -> {ok, N * 10} end),
                     io:format("  Call ~p: ~p~n", [N, Result]),
                     timer:sleep(100)
                  end,
                  lists:seq(1, Count)),
    ok.

%% @private Demonstrate recovery calls
-spec demo_recovery_calls(pid(), pos_integer()) -> ok.
demo_recovery_calls(Breaker, Count) ->
    lists:foreach(fun(N) ->
                     Result = call_tool(Breaker, fun() -> {ok, N * 100} end),
                     io:format("  Call ~p: ~p~n", [N, Result]),
                     timer:sleep(200)
                  end,
                  lists:seq(1, Count)),
    ok.

%% @private Print current state
-spec print_state(pid()) -> ok.
print_state(Breaker) ->
    {State, Data} = get_state(Breaker),
    io:format("~nCurrent State: ~p~n", [State]),
    io:format("  Consecutive Failures: ~p~n", [maps:get(consecutive_failures, Data)]),
    io:format("  Consecutive Successes: ~p~n", [maps:get(consecutive_successes, Data)]),
    io:format("  Total Calls: ~p~n", [maps:get(total_calls, Data)]),
    io:format("  Total Rejected: ~p~n", [maps:get(total_rejected, Data)]),
    io:format("  Trip Count: ~p~n", [maps:get(trip_count, Data)]),
    io:format("  Failure Rate: ~.2f%~n", [maps:get(failure_rate, Data) * 100]),
    ok.

%% @private Print statistics
-spec print_stats(pid()) -> ok.
print_stats(Breaker) ->
    Stats = get_stats(Breaker),
    io:format("~nFinal Statistics:~n"),
    io:format("  State: ~p~n", [maps:get(state, Stats)]),
    io:format("  Total Calls: ~p~n", [maps:get(total_calls, Stats)]),
    io:format("  Total Successes: ~p~n", [maps:get(total_successes, Stats)]),
    io:format("  Total Failures: ~p~n", [maps:get(total_failures, Stats)]),
    io:format("  Total Rejected: ~p~n", [maps:get(total_rejected, Stats)]),
    io:format("  Trip Count: ~p~n", [maps:get(trip_count, Stats)]),
    io:format("  Success Rate: ~.2f%~n", [maps:get(success_rate, Stats) * 100]),
    io:format("  Failure Rate: ~.2f%~n", [maps:get(failure_rate, Stats) * 100]),
    io:format("  Avg Recovery Time: ~.2f ms~n", [maps:get(avg_recovery_time_ms, Stats)]),
    ok.
