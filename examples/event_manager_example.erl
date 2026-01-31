%%%-------------------------------------------------------------------
%%% @doc
%%% Example usage of erlmcp_event_manager
%%%
%%% This example demonstrates:
%%% - Starting the event manager
%%% - Adding multiple handlers
%%% - Emitting different event types
%%% - Querying handler statistics
%%% - Creating custom handlers
%%%
%%% Run with:
%%%   erl -pa _build/default/lib/*/ebin
%%%   1> c(examples/event_manager_example).
%%%   2> event_manager_example:run().
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(event_manager_example).

-export([
    run/0,
    run_basic/0,
    run_custom_handler/0,
    run_metrics_demo/0,
    custom_handler_example/0
]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Run all examples
run() ->
    io:format("~n=== erlmcp Event Manager Examples ===~n~n"),

    run_basic(),
    run_metrics_demo(),
    run_custom_handler(),

    io:format("~n=== Examples Complete ===~n").

%% @doc Basic event manager usage
run_basic() ->
    io:format("~n--- Basic Usage Example ---~n"),

    %% Start event manager (usually done by supervisor)
    {ok, _Pid} = erlmcp_event_manager:start_link(),
    io:format("✓ Event manager started~n"),

    %% Add handlers
    ok = erlmcp_event_manager:add_handler(erlmcp_event_logger, #{}),
    io:format("✓ Logger handler added~n"),

    ok = erlmcp_event_manager:add_handler(erlmcp_event_metrics, []),
    io:format("✓ Metrics handler added~n"),

    ok = erlmcp_event_manager:add_handler(erlmcp_event_audit, #{
        enabled => true,
        log_all_events => false
    }),
    io:format("✓ Audit handler added~n"),

    %% Check registered handlers
    Handlers = erlmcp_event_manager:which_handlers(),
    io:format("Registered handlers: ~p~n", [Handlers]),

    %% Emit various events
    io:format("~nEmitting events...~n"),

    ok = erlmcp_event_manager:notify({tool_executed, <<"echo">>, 1000000, ok}),
    io:format("  • Tool execution event~n"),

    ok = erlmcp_event_manager:notify({resource_updated, <<"file://test.txt">>, #{size => 1024}}),
    io:format("  • Resource update event~n"),

    ok = erlmcp_event_manager:notify({connection_state, connected, #{transport => stdio}}),
    io:format("  • Connection state event~n"),

    ok = erlmcp_event_manager:notify({error, validation, invalid_params}),
    io:format("  • Error event~n"),

    %% Get statistics from handlers
    io:format("~nHandler Statistics:~n"),

    LoggerStats = gen_event:call(erlmcp_event_manager, erlmcp_event_logger, get_stats),
    io:format("  Logger: ~p~n", [LoggerStats]),

    MetricsStats = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics),
    io:format("  Metrics: ~p~n", [MetricsStats]),

    AuditStats = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),
    io:format("  Audit: ~p~n", [AuditStats]),

    %% Cleanup
    ok = erlmcp_event_manager:stop(),
    io:format("~n✓ Event manager stopped~n"),

    ok.

%% @doc Metrics demonstration
run_metrics_demo() ->
    io:format("~n--- Metrics Demo ---~n"),

    {ok, _Pid} = erlmcp_event_manager:start_link(),
    ok = erlmcp_event_manager:add_handler(erlmcp_event_metrics, []),

    %% Execute same tool multiple times
    ToolName = <<"test_tool">>,
    lists:foreach(fun(N) ->
        Duration = N * 100000,
        ok = erlmcp_event_manager:notify({tool_executed, ToolName, Duration, ok})
    end, lists:seq(1, 10)),

    io:format("Executed ~s 10 times~n", [ToolName]),

    %% Get metrics
    Metrics = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics),
    ToolExecs = maps:get(tool_executions, Metrics),

    io:format("Tool execution count: ~p~n", [maps:get(ToolName, ToolExecs)]),

    %% Reset and verify
    ok = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, reset_metrics),
    io:format("Metrics reset~n"),

    NewMetrics = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics),
    NewToolExecs = maps:get(tool_executions, NewMetrics),

    io:format("After reset: ~p~n", [NewToolExecs]),

    ok = erlmcp_event_manager:stop(),
    ok.

%% @doc Custom handler example
run_custom_handler() ->
    io:format("~n--- Custom Handler Example ---~n"),

    {ok, _Pid} = erlmcp_event_manager:start_link(),

    %% Add custom handler
    ok = erlmcp_event_manager:add_handler(example_custom_handler, #{threshold_ms => 500}),
    io:format("✓ Custom handler added~n"),

    %% Emit events
    ok = erlmcp_event_manager:notify({tool_executed, <<"fast_tool">>, 100000, ok}),
    ok = erlmcp_event_manager:notify({tool_executed, <<"slow_tool">>, 1000000, ok}),

    io:format("Events processed by custom handler~n"),

    ok = erlmcp_event_manager:stop(),
    ok.

%% @doc Show how to create a custom handler
custom_handler_example() ->
    io:format("~n--- Custom Handler Code Example ---~n"),
    Code = "
-module(example_custom_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    threshold_ms :: non_neg_integer(),
    slow_tools = [] :: [binary()]
}).

init(Args) ->
    ThresholdMs = maps:get(threshold_ms, Args, 1000),
    {ok, #state{threshold_ms = ThresholdMs}}.

handle_event({tool_executed, ToolName, Duration, _Result}, State) ->
    DurationMs = Duration div 1000,
    case DurationMs > State#state.threshold_ms of
        true ->
            io:format(\"[ALERT] Slow tool detected: ~s (~pms)~n\",
                      [ToolName, DurationMs]),
            NewSlowTools = [ToolName | State#state.slow_tools],
            {ok, State#state{slow_tools = NewSlowTools}};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_call(get_slow_tools, State) ->
    {ok, State#state.slow_tools, State};
handle_call(_Request, State) ->
    {ok, {error, unknown_request}, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
",
    io:format("~s~n", [Code]),
    ok.

%%====================================================================
%% Custom Handler Implementation (for demo)
%%====================================================================

-module(example_custom_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    threshold_ms :: non_neg_integer(),
    slow_tools = [] :: [binary()]
}).

init(Args) ->
    ThresholdMs = maps:get(threshold_ms, Args, 1000),
    {ok, #state{threshold_ms = ThresholdMs}}.

handle_event({tool_executed, ToolName, Duration, _Result}, State) ->
    DurationMs = Duration div 1000,
    case DurationMs > State#state.threshold_ms of
        true ->
            io:format("[ALERT] Slow tool detected: ~s (~pms)~n",
                      [ToolName, DurationMs]),
            NewSlowTools = [ToolName | State#state.slow_tools],
            {ok, State#state{slow_tools = NewSlowTools}};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_call(get_slow_tools, State) ->
    {ok, State#state.slow_tools, State};
handle_call(_Request, State) ->
    {ok, {error, unknown_request}, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
