# MCP Observability Implementation Strategy

## Overview

This document provides the detailed implementation strategy for instrumenting erlmcp's MCP implementation with comprehensive observability. It builds on the design specified in `MCP_OBSERVABILITY_DESIGN.md`.

---

## 1. Code Instrumentation Points

### 1.1 erlmcp_server.erl - Core MCP Operations

**Location:** `apps/erlmcp_core/src/erlmcp_server.erl`

#### Initialize Operation

```erlang
%% Current: Line ~150
handle_call({initialize, InitParams}, From, State) ->
    %% ADD: Start OTEL span
    Span = erlmcp_otel:start_span(<<"mcp.initialize">>, #{
        <<"protocol_version">> => maps:get(protocolVersion, InitParams, undefined),
        <<"client_info">> => maps:get(clientInfo, InitParams, #{})
    }),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    try
        %% Existing initialization logic...
        Result = perform_initialization(InitParams, State),
        
        %% ADD: Record metrics
        Latency = (erlang:monotonic_time(microsecond) - StartTime) / 1000,
        erlmcp_mcp_metrics:record_initialize(State#state.server_id, Latency, success),
        
        %% ADD: End span
        erlmcp_otel:add_attributes(Span, #{
            <<"success">> => true,
            <<"latency_ms">> => Latency
        }),
        erlmcp_otel:end_span(Span),
        
        {reply, Result, State#state{initialized = true}}
    catch
        Class:Reason:Stack ->
            Latency = (erlang:monotonic_time(microsecond) - StartTime) / 1000,
            erlmcp_mcp_metrics:record_initialize(State#state.server_id, Latency, error),
            erlmcp_otel:record_error(Span, {Class, Reason, Stack}),
            erlmcp_otel:end_span(Span),
            {reply, {error, Reason}, State}
    end.
```

#### Resource List Operation

```erlang
%% Current: Line ~300
handle_call({list_resources, Params}, From, State) ->
    Span = erlmcp_otel:start_span(<<"mcp.resources.list">>, #{
        <<"server_id">> => State#state.server_id,
        <<"cursor">> => maps:get(cursor, Params, undefined)
    }),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    try
        Resources = maps:keys(State#state.resources),
        ResourceTemplates = maps:keys(State#state.resource_templates),
        AllResources = Resources ++ ResourceTemplates,
        
        %% Pagination logic...
        Result = paginate_resources(AllResources, Params),
        
        Latency = (erlang:monotonic_time(microsecond) - StartTime) / 1000,
        
        %% Record metrics
        erlmcp_mcp_metrics:record_resource_list(
            State#state.server_id,
            Latency,
            #{count => length(AllResources)}
        ),
        
        erlmcp_otel:add_attributes(Span, #{
            <<"resource_count">> => length(AllResources),
            <<"latency_ms">> => Latency
        }),
        erlmcp_otel:end_span(Span),
        
        {reply, {ok, Result}, State}
    catch
        Class:Reason:Stack ->
            erlmcp_otel:record_error(Span, {Class, Reason, Stack}),
            erlmcp_otel:end_span(Span),
            {reply, {error, Reason}, State}
    end.
```

#### Resource Read Operation

```erlang
handle_call({read_resource, Uri}, From, State) ->
    Span = erlmcp_otel:start_span(<<"mcp.resources.read">>, #{
        <<"server_id">> => State#state.server_id,
        <<"uri">> => Uri
    }),
    
    StartTime = erlang:monotonic_time(microsecond),
    CacheHit = false,  %% TODO: Implement caching
    
    try
        %% Look up resource handler
        case maps:get(Uri, State#state.resources, undefined) of
            {Resource, Handler} ->
                %% Execute handler
                Content = Handler(Uri),
                Latency = (erlang:monotonic_time(microsecond) - StartTime) / 1000,
                
                %% Record metrics
                erlmcp_mcp_metrics:record_resource_read(
                    State#state.server_id,
                    Uri,
                    Latency,
                    #{cache_hit => CacheHit}
                ),
                
                erlmcp_otel:add_attributes(Span, #{
                    <<"cache_hit">> => CacheHit,
                    <<"latency_ms">> => Latency,
                    <<"content_size">> => byte_size(Content)
                }),
                erlmcp_otel:end_span(Span),
                
                {reply, {ok, Content}, State};
            undefined ->
                %% Try template matching
                handle_template_resource(Uri, Span, State)
        end
    catch
        Class:Reason:Stack ->
            erlmcp_otel:record_error(Span, {Class, Reason, Stack}),
            erlmcp_otel:end_span(Span),
            {reply, {error, Reason}, State}
    end.
```

#### Tool Call Operation (Critical Path)

```erlang
handle_call({call_tool, Name, Args}, From, State) ->
    %% Parent span for entire tool call
    ParentSpan = erlmcp_otel:start_span(<<"mcp.tools.call">>, #{
        <<"server_id">> => State#state.server_id,
        <<"tool.name">> => Name,
        <<"args.count">> => maps:size(Args)
    }),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    try
        %% Phase 1: Schema Validation (BOTTLENECK - 5-20ms)
        ValidationStart = erlang:monotonic_time(microsecond),
        ValidationSpan = erlmcp_otel:start_span(
            <<"mcp.tools.validate_schema">>,
            #{<<"tool.name">> => Name, <<"parent">> => ParentSpan}
        ),
        
        case validate_tool_schema(Name, Args, State) of
            ok ->
                ValidationTime = (erlang:monotonic_time(microsecond) - ValidationStart) / 1000,
                erlmcp_otel:end_span(ValidationSpan),
                
                %% Phase 2: Handler Execution
                ExecutionStart = erlang:monotonic_time(microsecond),
                ExecutionSpan = erlmcp_otel:start_span(
                    <<"mcp.tools.execute_handler">>,
                    #{<<"tool.name">> => Name, <<"parent">> => ParentSpan}
                ),
                
                {Tool, Handler, _Schema} = maps:get(Name, State#state.tools),
                Result = Handler(Args),
                
                ExecutionTime = (erlang:monotonic_time(microsecond) - ExecutionStart) / 1000,
                TotalTime = (erlang:monotonic_time(microsecond) - StartTime) / 1000,
                
                erlmcp_otel:end_span(ExecutionSpan),
                
                %% Record detailed metrics
                erlmcp_mcp_metrics:record_tool_call(
                    State#state.server_id,
                    Name,
                    ValidationTime,
                    ExecutionTime,
                    #{success => true}
                ),
                
                erlmcp_otel:add_attributes(ParentSpan, #{
                    <<"validation_time_ms">> => ValidationTime,
                    <<"execution_time_ms">> => ExecutionTime,
                    <<"total_time_ms">> => TotalTime,
                    <<"success">> => true
                }),
                erlmcp_otel:end_span(ParentSpan),
                
                {reply, {ok, Result}, State};
                
            {error, ValidationError} ->
                ValidationTime = (erlang:monotonic_time(microsecond) - ValidationStart) / 1000,
                erlmcp_otel:record_error(ValidationSpan, ValidationError),
                erlmcp_otel:end_span(ValidationSpan),
                erlmcp_otel:record_error(ParentSpan, ValidationError),
                erlmcp_otel:end_span(ParentSpan),
                
                erlmcp_mcp_metrics:record_tool_call(
                    State#state.server_id,
                    Name,
                    ValidationTime,
                    0,
                    #{success => false, error_type => validation_error}
                ),
                
                {reply, {error, ValidationError}, State}
        end
    catch
        Class:Reason:Stack ->
            TotalTime = (erlang:monotonic_time(microsecond) - StartTime) / 1000,
            erlmcp_otel:record_error(ParentSpan, {Class, Reason, Stack}),
            erlmcp_otel:end_span(ParentSpan),
            
            erlmcp_mcp_metrics:record_tool_call(
                State#state.server_id,
                Name,
                0,
                TotalTime,
                #{success => false, error_type => handler_crash}
            ),
            
            {reply, {error, Reason}, State}
    end.
```

#### Resource Subscription Fan-out

```erlang
handle_cast({notify_resource_updated, Uri, Content}, State) ->
    Span = erlmcp_otel:start_span(<<"mcp.resources.notify">>, #{
        <<"server_id">> => State#state.server_id,
        <<"uri">> => Uri
    }),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    %% Get all subscribers
    Subscribers = case maps:get(Uri, State#state.subscriptions, undefined) of
        undefined -> [];
        SubSet -> sets:to_list(SubSet)
    end,
    
    FanOutCount = length(Subscribers),
    
    %% Send notifications
    lists:foreach(fun(SubscriberPid) ->
        NotifyStart = erlang:monotonic_time(microsecond),
        SubscriberPid ! {resource_updated, Uri, Content},
        NotifyTime = (erlang:monotonic_time(microsecond) - NotifyStart) / 1000,
        
        %% Record per-subscriber latency
        erlmcp_mcp_metrics:record_notification_latency(
            State#state.server_id,
            Uri,
            NotifyTime
        )
    end, Subscribers),
    
    TotalTime = (erlang:monotonic_time(microsecond) - StartTime) / 1000,
    
    %% Record fan-out metrics
    erlmcp_mcp_metrics:record_subscription_fanout(
        State#state.server_id,
        Uri,
        FanOutCount,
        TotalTime
    ),
    
    erlmcp_otel:add_attributes(Span, #{
        <<"fan_out_count">> => FanOutCount,
        <<"total_time_ms">> => TotalTime,
        <<"avg_latency_ms">> => TotalTime / max(FanOutCount, 1)
    }),
    erlmcp_otel:end_span(Span),
    
    {noreply, State}.
```

---

## 2. New Modules to Create

### 2.1 erlmcp_mcp_metrics.erl

**Location:** `apps/erlmcp_observability/src/erlmcp_mcp_metrics.erl`

**Purpose:** MCP-specific metrics collection and aggregation.

```erlang
-module(erlmcp_mcp_metrics).
-behaviour(gen_server).

%% API
-export([start_link/0,
         record_initialize/3,
         record_resource_list/3,
         record_resource_read/4,
         record_tool_call/5,
         record_subscription_fanout/4,
         record_notification_latency/3,
         get_mcp_summary/0,
         get_compliance_report/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    %% Histograms (storing last 1000 samples)
    initialize_latency = [] :: [float()],
    resource_list_latency = [] :: [float()],
    resource_read_latency = #{} :: #{binary() => [float()]},
    tool_call_latency = #{} :: #{binary() => [float()]},
    tool_validation_latency = #{} :: #{binary() => [float()]},
    tool_execution_latency = #{} :: #{binary() => [float()]},
    subscription_fanout = #{} :: #{binary() => [integer()]},
    notification_latency = #{} :: #{binary() => [float()]},
    
    %% Counters
    total_operations = 0 :: integer(),
    total_errors = 0 :: integer(),
    tool_calls_by_name = #{} :: #{binary() => integer()},
    resource_reads_by_uri = #{} :: #{binary() => integer()},
    
    %% Compliance tracking
    latency_violations = [] :: [{Operation :: binary(), Latency :: float(), Timestamp :: integer()}],
    error_rate_violations = [] :: [{Operation :: binary(), Rate :: float(), Timestamp :: integer()}]
}).

%% API Implementation

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

record_initialize(ServerId, Latency, Status) ->
    gen_server:cast(?MODULE, {record_initialize, ServerId, Latency, Status}),
    
    %% Also export to OTEL
    otel_histogram:record(
        'erlmcp.mcp.initialize.latency_ms',
        Latency,
        #{server_id => ServerId, status => Status}
    ).

record_resource_list(ServerId, Latency, Metadata) ->
    gen_server:cast(?MODULE, {record_resource_list, ServerId, Latency, Metadata}),
    
    otel_histogram:record(
        'erlmcp.mcp.resources.list.latency_ms',
        Latency,
        #{server_id => ServerId}
    ),
    otel_counter:add('erlmcp.mcp.resources.list.total', 1, #{server_id => ServerId}).

record_tool_call(ServerId, ToolName, ValidationTime, ExecutionTime, Metadata) ->
    gen_server:cast(?MODULE, {record_tool_call, ServerId, ToolName, ValidationTime, ExecutionTime, Metadata}),
    
    TotalTime = ValidationTime + ExecutionTime,
    Labels = #{server_id => ServerId, tool_name => ToolName},
    
    otel_histogram:record('erlmcp.mcp.tools.call.latency_ms', TotalTime, Labels),
    otel_histogram:record('erlmcp.mcp.tools.call.validation_time_ms', ValidationTime, Labels),
    otel_histogram:record('erlmcp.mcp.tools.call.execution_time_ms', ExecutionTime, Labels),
    otel_counter:add('erlmcp.mcp.tools.call.total', 1, Labels).

get_mcp_summary() ->
    gen_server:call(?MODULE, get_mcp_summary).

get_compliance_report() ->
    gen_server:call(?MODULE, get_compliance_report).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call(get_mcp_summary, _From, State) ->
    Summary = #{
        total_operations => State#state.total_operations,
        total_errors => State#state.total_errors,
        error_rate => case State#state.total_operations of
            0 -> 0.0;
            N -> State#state.total_errors / N
        end,
        
        %% Latency percentiles
        initialize_p50 => percentile(State#state.initialize_latency, 50),
        initialize_p95 => percentile(State#state.initialize_latency, 95),
        initialize_p99 => percentile(State#state.initialize_latency, 99),
        
        resource_list_p50 => percentile(State#state.resource_list_latency, 50),
        resource_list_p95 => percentile(State#state.resource_list_latency, 95),
        
        %% Tool call stats
        tool_calls_total => maps:fold(fun(_, V, Acc) -> Acc + V end, 0, State#state.tool_calls_by_name),
        tools_by_name => State#state.tool_calls_by_name
    },
    {reply, Summary, State};

handle_call(get_compliance_report, _From, State) ->
    Report = #{
        protocol_version => <<"2025-11-25">>,
        timestamp => erlang:system_time(millisecond),
        
        compliance => #{
            initialize => check_compliance(initialize, State#state.initialize_latency, 100),
            resources_list => check_compliance(resources_list, State#state.resource_list_latency, 50),
            tools_call => check_compliance_multi(State#state.tool_call_latency, 500)
        },
        
        violations => State#state.latency_violations,
        error_rate => State#state.total_errors / max(State#state.total_operations, 1)
    },
    {reply, Report, State}.

handle_cast({record_initialize, _ServerId, Latency, Status}, State) ->
    NewState = State#state{
        initialize_latency = add_sample(State#state.initialize_latency, Latency),
        total_operations = State#state.total_operations + 1,
        total_errors = case Status of
            error -> State#state.total_errors + 1;
            _ -> State#state.total_errors
        end
    },
    {noreply, NewState};

handle_cast({record_tool_call, _ServerId, ToolName, ValidationTime, ExecutionTime, Metadata}, State) ->
    TotalTime = ValidationTime + ExecutionTime,
    Success = maps:get(success, Metadata, true),
    
    %% Update histograms
    ToolLatencies = maps:get(ToolName, State#state.tool_call_latency, []),
    ValidationLatencies = maps:get(ToolName, State#state.tool_validation_latency, []),
    ExecutionLatencies = maps:get(ToolName, State#state.tool_execution_latency, []),
    
    NewState = State#state{
        tool_call_latency = maps:put(ToolName, add_sample(ToolLatencies, TotalTime), State#state.tool_call_latency),
        tool_validation_latency = maps:put(ToolName, add_sample(ValidationLatencies, ValidationTime), State#state.tool_validation_latency),
        tool_execution_latency = maps:put(ToolName, add_sample(ExecutionLatencies, ExecutionTime), State#state.tool_execution_latency),
        
        %% Update counters
        tool_calls_by_name = maps:update_with(ToolName, fun(V) -> V + 1 end, 1, State#state.tool_calls_by_name),
        total_operations = State#state.total_operations + 1,
        total_errors = case Success of
            false -> State#state.total_errors + 1;
            _ -> State#state.total_errors
        end,
        
        %% Track violations
        latency_violations = case TotalTime > 500 of  % 500ms threshold for tools
            true -> [{tool_call, TotalTime, erlang:system_time(millisecond)} | State#state.latency_violations];
            false -> State#state.latency_violations
        end
    },
    {noreply, NewState}.

%% Helper functions

add_sample(Samples, NewSample) ->
    %% Keep last 1000 samples
    lists:sublist([NewSample | Samples], 1000).

percentile([], _P) -> 0.0;
percentile(Samples, P) ->
    Sorted = lists:sort(Samples),
    Index = round(length(Sorted) * P / 100),
    lists:nth(max(1, Index), Sorted).

check_compliance(Operation, Latencies, ThresholdMs) ->
    case percentile(Latencies, 95) of
        P95 when P95 < ThresholdMs -> #{compliant => true, p95 => P95, threshold => ThresholdMs};
        P95 -> #{compliant => false, p95 => P95, threshold => ThresholdMs}
    end.
```

### 2.2 erlmcp_mcp_chaos.erl

**Location:** `apps/erlmcp_observability/src/erlmcp_mcp_chaos.erl`

**Purpose:** MCP-specific chaos engineering scenarios.

```erlang
-module(erlmcp_mcp_chaos).

-export([run_all_scenarios/0,
         chaos_resource_handler_timeout/1,
         chaos_tool_handler_crash/1,
         chaos_subscription_flood/1,
         chaos_json_corruption/1,
         chaos_schema_validation_failure/1]).

run_all_scenarios() ->
    Scenarios = [
        {resource_handler_timeout, fun chaos_resource_handler_timeout/1},
        {tool_handler_crash, fun chaos_tool_handler_crash/1},
        {subscription_flood, fun chaos_subscription_flood/1},
        {json_corruption, fun chaos_json_corruption/1},
        {schema_validation_failure, fun chaos_schema_validation_failure/1}
    ],
    
    Results = lists:map(fun({Name, Fun}) ->
        io:format("Running chaos scenario: ~p~n", [Name]),
        Result = Fun(#{}),
        {Name, Result}
    end, Scenarios),
    
    %% Generate report
    generate_chaos_report(Results).

chaos_resource_handler_timeout(Config) ->
    erlmcp_chaos:run(#{
        experiment => resource_handler_timeout,
        target => resource_handlers,
        duration => 60000,  % 1 minute
        config => #{
            timeout_probability => 0.2,
            timeout_duration => 10000
        },
        sla_threshold => #{
            p95_latency_ms => 100,
            error_rate => 0.1
        },
        expected_behavior => #{
            description => "Resource handlers should timeout gracefully",
            metrics_to_check => [
                {p95_latency_ms, lt, 100},
                {error_rate, lt, 0.1}
            ]
        }
    }).

chaos_tool_handler_crash(Config) ->
    erlmcp_chaos:run(#{
        experiment => kill_random,
        target => tool_handlers,
        rate => 0.1,
        duration => 120000,
        auto_rollback => true,
        expected_behavior => #{
            description => "Supervisor should restart crashed tool handlers",
            recovery_time_ms => 1000,
            data_loss => false
        }
    }).

chaos_subscription_flood(Config) ->
    %% Test subscription fan-out under load
    erlmcp_chaos:run(#{
        experiment => subscription_flood,
        config => #{
            subscriber_count => 1000,
            update_rate_hz => 10,
            duration => 60000
        },
        expected_behavior => #{
            p95_notification_latency_ms => 50,
            dropped_notifications => 0,
            memory_growth_rate_mb_per_min => 10
        }
    }).
```

### 2.3 erlmcp_mcp_benchmarks.erl

**Location:** `apps/erlmcp_observability/src/erlmcp_mcp_benchmarks.erl`

**Purpose:** Automated performance benchmarks for MCP operations.

```erlang
-module(erlmcp_mcp_benchmarks).

-export([run_all/0,
         bench_initialize/0,
         bench_resource_list/0,
         bench_resource_read/0,
         bench_tool_call_simple/0,
         bench_tool_call_with_schema/0,
         bench_subscription_fanout/0,
         compare_with_baseline/1]).

run_all() ->
    Benchmarks = [
        {initialize, fun bench_initialize/0},
        {resource_list, fun bench_resource_list/0},
        {resource_read, fun bench_resource_read/0},
        {tool_call_simple, fun bench_tool_call_simple/0},
        {tool_call_with_schema, fun bench_tool_call_with_schema/0},
        {subscription_fanout, fun bench_subscription_fanout/0}
    ],
    
    Results = lists:map(fun({Name, Fun}) ->
        io:format("Running benchmark: ~p~n", [Name]),
        {Name, Fun()}
    end, Benchmarks),
    
    %% Save results
    save_benchmark_results(Results),
    
    %% Compare with baseline
    compare_with_baseline(Results).

bench_resource_list() ->
    %% Setup: Create server with 1000 resources
    {ok, Server} = erlmcp_server:start_link(bench_server, #{}),
    lists:foreach(fun(I) ->
        Uri = iolist_to_binary(io_lib:format("resource://bench/~p", [I])),
        erlmcp_server:add_resource(Server, Uri, fun(_) -> <<"content">> end)
    end, lists:seq(1, 1000)),
    
    %% Warmup
    [erlmcp_server:list_resources(Server) || _ <- lists:seq(1, 100)],
    
    %% Benchmark
    StartTime = erlang:monotonic_time(microsecond),
    [erlmcp_server:list_resources(Server) || _ <- lists:seq(1, 10000)],
    EndTime = erlang:monotonic_time(microsecond),
    
    TotalTime = (EndTime - StartTime) / 1000,  % ms
    AvgLatency = TotalTime / 10000,
    Throughput = 10000 / (TotalTime / 1000),  % ops/s
    
    %% Cleanup
    erlmcp_server:stop(Server),
    
    #{
        avg_latency_ms => AvgLatency,
        throughput_ops_per_sec => Throughput,
        total_operations => 10000
    }.
```

---

## 3. Dashboard Integration

### 3.1 MCP Compliance View

**Location:** `apps/erlmcp_observability/priv/static/mcp_compliance.html`

```html
<!DOCTYPE html>
<html>
<head>
    <title>MCP Compliance Dashboard</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
</head>
<body>
    <h1>MCP Compliance Dashboard</h1>
    
    <div id="protocol-compliance">
        <h2>Protocol Compliance</h2>
        <div id="protocol-status"></div>
    </div>
    
    <div id="performance-compliance">
        <h2>Performance Compliance</h2>
        <div id="latency-chart"></div>
        <div id="throughput-chart"></div>
    </div>
    
    <div id="violations">
        <h2>Real-time Violations</h2>
        <table id="violations-table">
            <thead>
                <tr><th>Timestamp</th><th>Operation</th><th>Metric</th><th>Value</th><th>Threshold</th></tr>
            </thead>
            <tbody id="violations-body"></tbody>
        </table>
    </div>
    
    <script>
        // WebSocket connection to erlmcp_dashboard_server
        const ws = new WebSocket('ws://localhost:9090/ws');
        
        ws.onmessage = (event) => {
            const data = JSON.parse(event.data);
            if (data.type === 'mcp_compliance') {
                updateComplianceDashboard(data.metrics);
            }
        };
        
        function updateComplianceDashboard(metrics) {
            // Update protocol status
            document.getElementById('protocol-status').innerHTML = `
                <p>MCP Spec Version: ${metrics.protocol_version}</p>
                <p>Compliance Status: ${metrics.compliance_status ? 'PASS' : 'FAIL'}</p>
            `;
            
            // Update latency chart
            const latencyData = [{
                x: ['initialize', 'resources/list', 'tools/call'],
                y: [metrics.initialize_p95, metrics.resource_list_p95, metrics.tool_call_p95],
                type: 'bar',
                name: 'P95 Latency (ms)'
            }];
            Plotly.newPlot('latency-chart', latencyData);
        }
    </script>
</body>
</html>
```

---

## 4. Testing Strategy

### 4.1 EUnit Tests for Metrics Module

**Location:** `apps/erlmcp_observability/test/erlmcp_mcp_metrics_tests.erl`

```erlang
-module(erlmcp_mcp_metrics_tests).
-include_lib("eunit/include/eunit.hrl").

mcp_metrics_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_record_initialize/1,
      fun test_record_tool_call/1,
      fun test_compliance_report/1,
      fun test_latency_percentiles/1
     ]}.

setup() ->
    {ok, Pid} = erlmcp_mcp_metrics:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

test_record_initialize(_Pid) ->
    erlmcp_mcp_metrics:record_initialize(test_server, 5.0, success),
    Summary = erlmcp_mcp_metrics:get_mcp_summary(),
    ?assertEqual(1, maps:get(total_operations, Summary)),
    ?assert(maps:get(initialize_p50, Summary) > 0).

test_compliance_report(_Pid) ->
    %% Record operations
    [erlmcp_mcp_metrics:record_initialize(test_server, 4.0, success) || _ <- lists:seq(1, 100)],
    
    Report = erlmcp_mcp_metrics:get_compliance_report(),
    Compliance = maps:get(compliance, Report),
    InitCompliance = maps:get(initialize, Compliance),
    
    ?assert(maps:get(compliant, InitCompliance)),
    ?assert(maps:get(p95, InitCompliance) < 100).
```

---

## 5. CI/CD Integration

### 5.1 GitHub Actions Workflow

**Location:** `.github/workflows/performance-benchmarks.yml`

```yaml
name: MCP Performance Benchmarks

on:
  pull_request:
    branches: [main]
  schedule:
    - cron: '0 2 * * *'  # Daily at 2 AM UTC

jobs:
  benchmark:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Setup OTP 28
      uses: erlef/setup-beam@v1
      with:
        otp-version: '28'
        rebar3-version: '3.22'
    
    - name: Compile
      run: rebar3 compile
    
    - name: Run MCP Benchmarks
      run: |
        rebar3 shell --eval "erlmcp_mcp_benchmarks:run_all()."
    
    - name: Compare with Baseline
      run: |
        python3 scripts/compare_benchmarks.py \
          --current results/benchmarks.json \
          --baseline baselines/v2.1.0.json \
          --threshold 0.10
    
    - name: Upload Results
      uses: actions/upload-artifact@v3
      with:
        name: benchmark-results
        path: results/benchmarks.json
```

---

## 6. Next Steps

1. **Create skeleton modules** (erlmcp_mcp_metrics.erl, erlmcp_mcp_chaos.erl, erlmcp_mcp_benchmarks.erl)
2. **Instrument erlmcp_server.erl** with OTEL spans and metrics
3. **Add health checks** to erlmcp_health_monitor.erl
4. **Create dashboard view** (mcp_compliance.html)
5. **Write tests** (EUnit + CT)
6. **CI integration** (GitHub Actions)

---

**Total Implementation Effort:** 6-8 weeks  
**Priority:** High (Performance & Compliance)  
**Dependencies:** OpenTelemetry Erlang SDK 1.7.0+

---

**END OF IMPLEMENTATION STRATEGY**
