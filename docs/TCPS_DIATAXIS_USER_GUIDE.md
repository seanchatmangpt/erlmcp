# TCPS MCP Diataxis Simulator - User Guide

## Table of Contents

1. [Quick Start](#quick-start)
2. [MCP Server Usage](#mcp-server-usage)
3. [Simulator Usage](#simulator-usage)
4. [Diataxis Documentation](#diataxis-documentation)
5. [Common Workflows](#common-workflows)
6. [Troubleshooting](#troubleshooting)

## Quick Start

### Prerequisites

- Erlang/OTP 24+
- Rebar3 build tool
- OpenTelemetry (optional, for telemetry)

### Installation

```bash
# Clone the repository
cd erlmcp

# Compile
rebar3 compile

# Run tests to verify installation
rebar3 eunit
rebar3 ct
```

### Starting the MCP Server

```erlang
% Start with default configuration (port 3000, STDIO transport)
{ok, Pid} = tcps_mcp_server:start_link().

% Start with custom configuration
{ok, Pid} = tcps_mcp_server:start_link([
    {port, 8080},
    {transport, http}
]).

% Get server information
Info = tcps_mcp_server:get_server_info().
% => #{version => <<"0.1.0">>, protocol_version => <<"2024-11-05">>, ...}
```

### Running Your First Simulation

```erlang
% Start the simulator
{ok, SimPid} = tcps_simulator:start_link().

% Run a basic production scenario
{ok, ScenarioId} = tcps_simulator:start_scenario(basic_production, #{
    work_orders => 10,
    quality_gate_pass_rate => 0.95,
    iteration_delay_ms => 100
}).

% Wait for completion
timer:sleep(2000).

% Get results
{ok, Metrics} = tcps_simulator:get_metrics(ScenarioId).
% => #{work_orders_created => 10, work_orders_completed => 9, ...}
```

## MCP Server Usage

### Tool Discovery

List all available tools:

```erlang
Tools = tcps_mcp_server:list_tools().
% Returns list of 8 tools with names, descriptions, and schemas
```

### Calling Tools

#### Work Order Creation

```erlang
{ok, WorkOrder} = tcps_mcp_server:call_tool(<<"tcps_work_order_create">>, #{
    <<"type">> => <<"feature">>,
    <<"priority">> => <<"high">>,
    <<"description">> => <<"Implement new API endpoint">>
}).

% WorkOrder structure:
% #{
%     id => <<"1234567890_123456">>,
%     type => feature,
%     priority => high,
%     description => <<"Implement new API endpoint">>,
%     status => created,
%     created_at => 1706234567
% }
```

#### Work Order Verification

```erlang
WorkOrderId = maps:get(id, WorkOrder),

{ok, Receipt} = tcps_mcp_server:call_tool(<<"tcps_work_order_verify">>, #{
    <<"work_order_id">> => WorkOrderId
}).

% Receipt structure:
% #{
%     work_order_id => <<"1234567890_123456">>,
%     verification_hash => <<"abc123...">>,
%     verified_at => 1706234580,
%     status => verified
% }
```

#### Quality Gates Check

```erlang
{ok, Result} = tcps_mcp_server:call_tool(<<"tcps_quality_gates_check">>, #{
    <<"project_path">> => <<"/path/to/project">>
}).

% Result structure:
% #{
%     status => pass,  % or fail
%     metrics => #{
%         coverage => 87.5,
%         complexity => 8,
%         duplication => 2.3
%     },
%     violations => [],  % List of violations if status is fail
%     checked_at => 1706234590,
%     project_path => <<"/path/to/project">>
% }
```

#### Andon Alert Triggering

```erlang
{ok, Alert} = tcps_mcp_server:call_tool(<<"tcps_andon_trigger">>, #{
    <<"severity">> => <<"critical">>,
    <<"message">> => <<"Quality threshold breached in module X">>
}).

% Alert structure:
% #{
%     id => <<"alert_1234567890_123456">>,
%     severity => critical,  % or warning
%     message => <<"Quality threshold breached in module X">>,
%     triggered_at => 1706234600,
%     status => active
% }
```

#### Root Cause Analysis

```erlang
{ok, Analysis} = tcps_mcp_server:call_tool(<<"tcps_root_cause_analyze">>, #{
    <<"incident_id">> => <<"inc_987">>,
    <<"description">> => <<"System crashed during deployment">>
}).

% Analysis structure:
% #{
%     incident_id => <<"inc_987">>,
%     root_cause => <<"Insufficient error handling in input validation">>,
%     why_chain => [
%         <<"System crashed">>,
%         <<"Uncaught exception in validation">>,
%         <<"Missing null check">>,
%         <<"No validation spec for optional fields">>,
%         <<"Incomplete requirements gathering">>
%     ],
%     corrective_actions => [
%         <<"Add null checks to all optional field validations">>,
%         <<"Update validation specs">>,
%         <<"Add integration tests for edge cases">>
%     ],
%     analyzed_at => 1706234610,
%     description => <<"System crashed during deployment">>
% }
```

### Diataxis Documentation Tools

#### Get Tutorial

```erlang
{ok, Tutorial} = tcps_mcp_server:call_tool(<<"tcps_diataxis_get_tutorial">>, #{
    <<"tutorial_id">> => <<"getting_started_tcps">>
}).

% Tutorial structure includes:
% - title, level, description
% - prerequisites, learning_outcomes
% - steps (with instructions, code examples, hints)
% - total_duration_minutes, tags
```

#### Get How-To Guide

```erlang
{ok, Guide} = tcps_mcp_server:call_tool(<<"tcps_diataxis_get_howto">>, #{
    <<"guide_id">> => <<"configure_quality_gates">>
}).

% Guide structure includes:
% - title, category, difficulty, summary
% - prerequisites, steps
% - success_criteria, common_pitfalls
% - see_also (related guides)
```

#### Search Documentation

```erlang
% Search all documentation types
{ok, AllResults} = tcps_mcp_server:call_tool(<<"tcps_diataxis_search">>, #{
    <<"query">> => <<"quality gates">>,
    <<"type">> => <<"all">>
}).

% Result structure:
% #{
%     tutorials => [Tutorial1, Tutorial2, ...],
%     howtos => [Guide1, Guide2, ...],
%     explanations => [Expl1, Expl2, ...],
%     references => [Ref1, Ref2, ...]
% }

% Search specific type only
{ok, HowtoResults} = tcps_mcp_server:call_tool(<<"tcps_diataxis_search">>, #{
    <<"query">> => <<"quality">>,
    <<"type">> => <<"howto">>
}).
% Returns list of matching how-to guides
```

### Custom Tool Registration

```erlang
% Define custom tool
CustomTool = #{
    name => <<"my_custom_tool">>,
    description => <<"Does something custom">>,
    input_schema => #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"param1">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"param1">>]
    },
    handler => fun(Args) ->
        Param1 = maps:get(<<"param1">>, Args),
        {ok, #{result => Param1}}
    end
},

% Register tool
ok = tcps_mcp_server:register_tool(<<"my_custom_tool">>, CustomTool),

% Call custom tool
{ok, Result} = tcps_mcp_server:call_tool(<<"my_custom_tool">>, #{
    <<"param1">> => <<"value">>
}).
```

## Simulator Usage

### Starting the Simulator

```erlang
% Start simulator gen_server
{ok, Pid} = tcps_simulator:start_link().
```

### Running Scenarios

#### Basic Production

```erlang
{ok, ScenarioId} = tcps_simulator:start_scenario(basic_production, #{
    work_orders => 50,
    quality_gate_pass_rate => 0.95,
    iteration_delay_ms => 50
}).
```

**Purpose**: Learn basic TCPS workflow with high success rate.

**Expected Results**:
- ~95% work orders pass quality gates
- Very few andon alerts
- Good throughput

#### Defect Scenario

```erlang
{ok, ScenarioId} = tcps_simulator:start_scenario(defect_scenario, #{
    work_orders => 30,
    quality_gate_pass_rate => 0.70,
    andon_trigger_rate => 0.15,
    iteration_delay_ms => 100
}).
```

**Purpose**: Practice handling defects and andon alerts.

**Expected Results**:
- ~30% work orders fail quality gates
- ~15% trigger andon alerts
- Opportunity to practice root cause analysis

#### High Load

```erlang
{ok, ScenarioId} = tcps_simulator:start_scenario(high_load, #{
    work_orders => 200,
    quality_gate_pass_rate => 0.90,
    iteration_delay_ms => 25
}).
```

**Purpose**: Test system scalability and performance.

**Expected Results**:
- 200 work orders created
- Throughput > 50 WO/sec
- System remains stable

#### Bottleneck Analysis

```erlang
{ok, ScenarioId} = tcps_simulator:start_scenario(bottleneck_analysis, #{
    work_orders => 50,
    quality_gate_pass_rate => 0.85,
    slow_operation_rate => 0.20,
    iteration_delay_ms => 150
}).
```

**Purpose**: Identify and analyze performance bottlenecks.

**Expected Results**:
- ~20% operations are slow
- Lower throughput than expected
- Telemetry shows where delays occur

#### Optimization Test

```erlang
{ok, ScenarioId} = tcps_simulator:start_scenario(optimization_test, #{
    work_orders => 40,
    quality_gate_pass_rate => 0.92,
    wip_limit => 5,
    iteration_delay_ms => 100
}).
```

**Purpose**: Optimize workflow with WIP (Work In Progress) limits.

**Expected Results**:
- WIP limit enforced
- Quality maintained or improved
- Smoother workflow

#### Stress Test

```erlang
{ok, ScenarioId} = tcps_simulator:start_scenario(stress_test, #{
    work_orders => 1000,
    quality_gate_pass_rate => 0.88,
    iteration_delay_ms => 5
}).
```

**Purpose**: Find system limits under extreme load.

**Expected Results**:
- 1000 work orders processed
- Very high throughput (100+ WO/sec)
- System stress test passed

### Managing Scenarios

#### Pause Scenario

```erlang
ok = tcps_simulator:pause_scenario(ScenarioId).

{ok, Status} = tcps_simulator:get_scenario_status(ScenarioId).
% => #{status => paused, ...}
```

#### Resume Scenario

```erlang
ok = tcps_simulator:resume_scenario(ScenarioId).

{ok, Status} = tcps_simulator:get_scenario_status(ScenarioId).
% => #{status => running, ...}
```

#### Stop Scenario

```erlang
ok = tcps_simulator:stop_scenario(ScenarioId).

{ok, Status} = tcps_simulator:get_scenario_status(ScenarioId).
% => #{status => stopped, completed_at => ..., ...}
```

#### List All Scenarios

```erlang
Scenarios = tcps_simulator:list_scenarios().
% Returns list of all running/paused/stopped scenarios
```

### Metrics and Monitoring

#### Get Scenario Metrics

```erlang
{ok, Metrics} = tcps_simulator:get_metrics(ScenarioId).

% Metrics structure:
% #{
%     work_orders_created => 50,
%     work_orders_completed => 47,
%     quality_gates_passed => 47,
%     quality_gates_failed => 3,
%     andon_alerts => 1,
%     total_duration_ms => 5234,
%     throughput_per_second => 9.0
% }
```

#### Calculate Pass Rate

```erlang
PassRate = maps:get(work_orders_completed, Metrics) /
           maps:get(work_orders_created, Metrics).
% => 0.94 (94% pass rate)
```

#### Analyze Throughput

```erlang
Throughput = maps:get(throughput_per_second, Metrics).
Duration = maps:get(total_duration_ms, Metrics) / 1000.0.

io:format("Processed ~p work orders in ~p seconds (~p WO/sec)~n", [
    maps:get(work_orders_completed, Metrics),
    Duration,
    Throughput
]).
```

## Diataxis Documentation

### Direct API Usage

#### Tutorial Management

```erlang
% List all tutorials
Tutorials = tcps_diataxis_tutorial:list_tutorials().

% Get by difficulty level
BeginnerTutorials = tcps_diataxis_tutorial:list_by_level(beginner).

% Get specific tutorial
{ok, Tutorial} = tcps_diataxis_tutorial:get_tutorial(<<"getting_started_tcps">>).

% Start tutorial for user
{ok, Progress} = tcps_diataxis_tutorial:start_tutorial(<<"getting_started_tcps">>, <<"user123">>).

% Complete a step
ok = tcps_diataxis_tutorial:complete_step(<<"getting_started_tcps">>, <<"user123">>, 1).

% Get next step
{ok, NextStep} = tcps_diataxis_tutorial:get_next_step(<<"getting_started_tcps">>, 1).

% Check progress
{ok, Progress} = tcps_diataxis_tutorial:get_progress(<<"getting_started_tcps">>, <<"user123">>).
% => #{completed_steps => [1], current_step => 2, ...}
```

#### How-To Guides

```erlang
% List all guides
Guides = tcps_diataxis_howto:list_guides().

% List by category
ConfigGuides = tcps_diataxis_howto:list_by_category(configuration).
TroubleshootGuides = tcps_diataxis_howto:list_by_category(troubleshooting).

% Search guides
Results = tcps_diataxis_howto:search_guides(<<"quality gates">>).

% Get specific guide
{ok, Guide} = tcps_diataxis_howto:get_guide(<<"configure_quality_gates">>).

% Get related guides
Related = tcps_diataxis_howto:get_related_guides(<<"configure_quality_gates">>).

% Get difficulty
{ok, Difficulty} = tcps_diataxis_howto:get_guide_difficulty(<<"root_cause_analysis">>).
% => hard
```

#### Explanations

```erlang
% List all explanations
Explanations = tcps_diataxis_explain:list_explanations().

% List by category
CoreConcepts = tcps_diataxis_explain:list_by_category(core_concepts).
DesignDecisions = tcps_diataxis_explain:list_by_category(design_decisions).
Comparisons = tcps_diataxis_explain:list_by_category(comparisons).

% Search explanations
Results = tcps_diataxis_explain:search_explanations(<<"jidoka">>).

% Get specific explanation
{ok, Explanation} = tcps_diataxis_explain:get_explanation(<<"why_tcps">>).

% Get learning path
Path = tcps_diataxis_explain:get_explanation_path(<<"why_tcps">>).
% Returns explanations sorted by difficulty
```

#### References

```erlang
% List all references
References = tcps_diataxis_reference:list_references().

% List by type
ApiRefs = tcps_diataxis_reference:list_by_type(api).
ProtocolRefs = tcps_diataxis_reference:list_by_type(protocol).

% Search references
Results = tcps_diataxis_reference:search_references(<<"work order">>).

% Get specific reference
{ok, Ref} = tcps_diataxis_reference:get_reference(<<"tcps_work_order">>).

% Get module reference (with dynamic generation)
{ok, ModRef} = tcps_diataxis_reference:get_module_reference(tcps_work_order).

% Get function reference
{ok, FuncRef} = tcps_diataxis_reference:get_function_reference(
    tcps_work_order,
    {create, 1}
).
```

## Common Workflows

### Workflow 1: AI Agent Learning TCPS

```erlang
% 1. Agent searches for introductory content
{ok, SearchResults} = tcps_mcp_server:call_tool(<<"tcps_diataxis_search">>, #{
    <<"query">> => <<"getting started">>,
    <<"type">> => <<"tutorial">>
}).

% 2. Agent retrieves beginner tutorial
{ok, Tutorial} = tcps_mcp_server:call_tool(<<"tcps_diataxis_get_tutorial">>, #{
    <<"tutorial_id">> => <<"getting_started_tcps">>
}).

% 3. Agent creates first work order
{ok, WO} = tcps_mcp_server:call_tool(<<"tcps_work_order_create">>, #{
    <<"type">> => <<"feature">>,
    <<"priority">> => <<"medium">>
}).

% 4. Agent verifies work order
{ok, Receipt} = tcps_mcp_server:call_tool(<<"tcps_work_order_verify">>, #{
    <<"work_order_id">> => maps:get(id, WO)
}).

% 5. Agent checks quality gates
{ok, QGResult} = tcps_mcp_server:call_tool(<<"tcps_quality_gates_check">>, #{}).
```

### Workflow 2: Practicing Defect Handling

```erlang
% 1. Start defect scenario
{ok, ScenarioId} = tcps_simulator:start_scenario(defect_scenario, #{
    work_orders => 20,
    iteration_delay_ms => 100
}).

% 2. Monitor until defect occurs
timer:sleep(500),
{ok, Metrics} = tcps_simulator:get_metrics(ScenarioId),
DefectCount = maps:get(quality_gates_failed, Metrics),

% 3. Trigger andon alert
{ok, Alert} = tcps_mcp_server:call_tool(<<"tcps_andon_trigger">>, #{
    <<"severity">> => <<"warning">>,
    <<"message">> => io_lib:format("~p defects detected", [DefectCount])
}).

% 4. Perform root cause analysis
{ok, Analysis} = tcps_mcp_server:call_tool(<<"tcps_root_cause_analyze">>, #{
    <<"incident_id">> => maps:get(id, Alert),
    <<"description">> => <<"Multiple quality gate failures">>
}).

% 5. Review corrective actions
CorrectiveActions = maps:get(corrective_actions, Analysis).
```

### Workflow 3: Performance Optimization

```erlang
% 1. Run baseline scenario
{ok, BaselineId} = tcps_simulator:start_scenario(basic_production, #{
    work_orders => 50
}),
timer:sleep(1000),
{ok, BaselineMetrics} = tcps_simulator:get_metrics(BaselineId),
BaselineThroughput = maps:get(throughput_per_second, BaselineMetrics),

% 2. Run optimized scenario
{ok, OptimizedId} = tcps_simulator:start_scenario(optimization_test, #{
    work_orders => 50,
    wip_limit => 5
}),
timer:sleep(1000),
{ok, OptimizedMetrics} = tcps_simulator:get_metrics(OptimizedId),
OptimizedThroughput = maps:get(throughput_per_second, OptimizedMetrics),

% 3. Compare results
Improvement = (OptimizedThroughput - BaselineThroughput) / BaselineThroughput * 100,
io:format("Throughput improved by ~.2f%~n", [Improvement]).
```

## Troubleshooting

### MCP Server Won't Start

**Problem**: Server fails to start or port is in use.

**Solution**:
```erlang
% Check if port is available
% Try different port
{ok, Pid} = tcps_mcp_server:start_link([{port, 3001}]).

% Or use STDIO transport instead
{ok, Pid} = tcps_mcp_server:start_link([{transport, stdio}]).
```

### Tool Call Returns Error

**Problem**: MCP tool call returns `{error, tool_not_found}`.

**Solution**:
```erlang
% List available tools
Tools = tcps_mcp_server:list_tools(),
ToolNames = [maps:get(name, T) || T <- Tools],
io:format("Available tools: ~p~n", [ToolNames]).

% Verify tool name spelling
{ok, Result} = tcps_mcp_server:call_tool(<<"tcps_work_order_create">>, #{...}).
```

### Scenario Not Completing

**Problem**: Scenario runs indefinitely or doesn't complete.

**Solution**:
```erlang
% Check scenario status
{ok, Status} = tcps_simulator:get_scenario_status(ScenarioId),
ScenarioStatus = maps:get(status, Status),

% If stuck, stop and restart
ok = tcps_simulator:stop_scenario(ScenarioId),

% Start new scenario with different parameters
{ok, NewScenarioId} = tcps_simulator:start_scenario(basic_production, #{
    work_orders => 10,  % Reduce work orders
    iteration_delay_ms => 50  % Reduce delay
}).
```

### Tutorial Progress Not Saved

**Problem**: User progress resets after completion.

**Solution**:
```erlang
% Progress is stored in persistent_term by default
% Verify progress was saved
{ok, Progress} = tcps_diataxis_tutorial:get_progress(TutorialId, UserId),
CompletedSteps = maps:get(completed_steps, Progress),
io:format("Completed steps: ~p~n", [CompletedSteps]).

% If lost, restart tutorial
ok = tcps_diataxis_tutorial:reset_progress(TutorialId, UserId),
{ok, NewProgress} = tcps_diataxis_tutorial:start_tutorial(TutorialId, UserId).
```

### Search Returns No Results

**Problem**: Documentation search returns empty list.

**Solution**:
```erlang
% Try broader search term
{ok, Results1} = tcps_mcp_server:call_tool(<<"tcps_diataxis_search">>, #{
    <<"query">> => <<"quality">>,  % Broader term
    <<"type">> => <<"all">>
}).

% Try different documentation type
{ok, HowtoResults} = tcps_mcp_server:call_tool(<<"tcps_diataxis_search">>, #{
    <<"query">> => <<"quality gates">>,
    <<"type">> => <<"howto">>  % More specific type
}).

% List all available documentation
Tutorials = tcps_diataxis_tutorial:list_tutorials(),
Guides = tcps_diataxis_howto:list_guides(),
Explanations = tcps_diataxis_explain:list_explanations(),
References = tcps_diataxis_reference:list_references().
```

## Next Steps

- **[API Reference](TCPS_SIMULATOR_API.md)** - Complete API documentation
- **[MCP Tools](TCPS_MCP_TOOLS.md)** - Detailed tool specifications
- **[Deployment](TCPS_DEPLOYMENT.md)** - Production deployment guide
- **[Pedagogy](TCPS_DIATAXIS_PEDAGOGY.md)** - Educational design principles

## Support

For issues, questions, or contributions:
- Review test suites in `test/` directory
- Check integration tests in `test/integration/`
- Run comprehensive test suite: `rebar3 eunit && rebar3 ct`
