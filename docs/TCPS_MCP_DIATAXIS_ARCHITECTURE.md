# TCPS MCP Diataxis Simulator - Architecture Design

## Executive Summary

The TCPS MCP Diataxis Simulator is an interactive educational platform that demonstrates Toyota Code Production System (TCPS) concepts through hands-on simulation. It integrates the Model Context Protocol (MCP) for AI tool integration and uses the Diataxis documentation framework to provide structured learning across four quadrants: tutorials, how-to guides, explanation, and reference.

**Design Philosophy**: Build on existing erlmcp infrastructure to create a production-grade educational simulator that teaches TCPS principles through real-world simulation scenarios with MCP-enabled AI assistance.

## 1. System Overview

### 1.1 High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    TCPS MCP Diataxis Simulator                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌────────────┐ │
│  │   Diataxis   │  │     TCPS     │  │     MCP      │  │   Web UI   │ │
│  │  Framework   │  │  Simulation  │  │  Integration │  │ Dashboard  │ │
│  │   Module     │  │    Engine    │  │    Layer     │  │  (Cowboy)  │ │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘  └─────┬──────┘ │
│         │                 │                 │                │        │
│         └─────────────────┴─────────────────┴────────────────┘        │
│                              │                                         │
│                    ┌─────────┴─────────┐                              │
│                    │  Erlang/OTP Core  │                              │
│                    │   (gen_server,    │                              │
│                    │   supervisor)     │                              │
│                    └─────────┬─────────┘                              │
│                              │                                         │
│         ┌────────────────────┼────────────────────┐                   │
│         │                    │                    │                   │
│  ┌──────▼──────┐  ┌──────────▼────────┐  ┌───────▼──────┐            │
│  │ Persistence │  │   Telemetry/OTEL  │  │   Receipt    │            │
│  │   (RDF)     │  │     Tracing       │  │  Verification│            │
│  └─────────────┘  └───────────────────┘  └──────────────┘            │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### 1.2 Core Components

1. **Diataxis Framework Module**: Structured documentation and learning paths
2. **TCPS Simulation Engine**: Interactive work order processing and quality gates
3. **MCP Integration Layer**: AI tool integration for guided learning
4. **Web Dashboard**: Real-time visualization and interaction
5. **Telemetry System**: OpenTelemetry integration for observability
6. **Persistence Layer**: RDF-based state management with receipts

## 2. Component Architecture

### 2.1 Diataxis Framework Module

```
src/tcps_mcp_diataxis/diataxis/
├── diataxis_tutorial.erl       % Tutorial scenarios and walkthroughs
├── diataxis_howto.erl          % Task-oriented how-to guides
├── diataxis_explanation.erl    % Conceptual explanations
├── diataxis_reference.erl      % Reference documentation
├── diataxis_coordinator.erl    % Orchestrates learning paths
└── diataxis_progress.erl       % Tracks user progress
```

#### 2.1.1 Module Responsibilities

**diataxis_tutorial.erl**
- Provides step-by-step guided tutorials for TCPS concepts
- Interactive scenarios: "Create your first work order", "Set up Kanban board", "Trigger an Andon event"
- Progress tracking and validation
- MCP tool recommendations at each step

**diataxis_howto.erl**
- Task-oriented guides for specific goals
- Problem-solution patterns
- "How to reduce WIP limits", "How to respond to quality gate failures"
- Links to relevant TCPS modules

**diataxis_explanation.erl**
- Conceptual explanations of TCPS principles
- "Understanding Pull Systems", "Quality Gates vs. Continuous Delivery"
- Background theory with practical examples
- Diagrams and visualizations

**diataxis_reference.erl**
- API documentation for simulator functions
- Configuration reference
- Metric definitions
- Error codes and troubleshooting

**diataxis_coordinator.erl**
- Routes users to appropriate documentation type
- Manages learning path transitions
- Suggests next steps based on progress
- MCP-enabled AI guidance integration

**diataxis_progress.erl**
- Tracks completion of tutorials and exercises
- Stores user achievements
- Generates learning reports
- Persistence to RDF ontology

#### 2.1.2 Data Structures

```erlang
-record(tutorial_scenario, {
    id :: binary(),
    title :: binary(),
    description :: binary(),
    difficulty :: beginner | intermediate | advanced,
    steps :: [tutorial_step()],
    prerequisites :: [binary()],
    estimated_time :: pos_integer(), % minutes
    tags :: [binary()]
}).

-record(tutorial_step, {
    order :: pos_integer(),
    title :: binary(),
    description :: binary(),
    action :: simulator_action(),
    validation :: validation_fn(),
    hints :: [binary()],
    mcp_tools :: [binary()] % Suggested MCP tools for this step
}).

-record(learning_path, {
    user_id :: binary(),
    current_tutorial :: binary() | undefined,
    completed_tutorials :: [binary()],
    completed_howtos :: [binary()],
    progress :: #{binary() => float()}, % tutorial_id => completion %
    achievements :: [binary()],
    started_at :: erlang:timestamp(),
    last_active :: erlang:timestamp()
}).
```

### 2.2 TCPS Simulation Engine

```
src/tcps_mcp_diataxis/simulation/
├── sim_coordinator.erl         % Main simulation orchestrator
├── sim_scenario.erl           % Scenario configuration and management
├── sim_work_order.erl         % Work order simulation wrapper
├── sim_kanban.erl             % Kanban board simulation
├── sim_quality_gates.erl      % Quality gate simulation
├── sim_andon.erl              % Andon event simulation
├── sim_metrics.erl            % Simulation metrics collection
├── sim_state.erl              % Simulation state management
└── sim_validation.erl         % Validation of user actions
```

#### 2.2.1 Simulation Coordinator

**sim_coordinator.erl** - Central orchestration
```erlang
-module(sim_coordinator).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    create_simulation/1,
    reset_simulation/1,
    execute_action/3,
    get_state/1,
    list_available_actions/1,
    advance_simulation/1,
    inject_event/2
]).

-record(simulation_state, {
    sim_id :: binary(),
    scenario_id :: binary(),
    mode :: tutorial | sandbox | challenge,
    current_step :: pos_integer(),
    work_orders :: [sim_work_order()],
    kanban_state :: sim_kanban_state(),
    quality_gates :: [gate_status()],
    andon_events :: [andon_event()],
    metrics :: simulation_metrics(),
    started_at :: erlang:timestamp(),
    completed :: boolean(),
    score :: float() | undefined
}).
```

#### 2.2.2 Scenario System

**sim_scenario.erl** - Pre-configured learning scenarios
```erlang
-record(scenario, {
    id :: binary(),
    title :: binary(),
    description :: binary(),
    difficulty :: beginner | intermediate | advanced,
    learning_objectives :: [binary()],
    initial_state :: simulation_state(),
    events :: [timed_event()], % Scheduled events during simulation
    success_criteria :: [criterion()],
    failure_conditions :: [condition()],
    time_limit :: pos_integer() | infinity % seconds
}).

%% Example scenarios:
%% - "kanban_basics": Manage WIP limits with increasing demand
%% - "andon_response": Respond to quality gate failures
%% - "heijunka_leveling": Balance work across buckets
%% - "continuous_flow": Optimize lead time and throughput
%% - "quality_gates": Navigate SHACL, testing, security gates
```

#### 2.2.3 Simulation Wrappers

Thin wrappers around real TCPS modules for educational context:

**sim_work_order.erl**
- Wraps tcps_work_order with educational annotations
- Explains consequences of each action
- Tracks metrics for learning feedback
- Validates user decisions

**sim_kanban.erl**
- Wraps tcps_kanban with WIP limit explanations
- Shows visualization of board state
- Demonstrates Heijunka leveling
- Tracks pull signal responses

**sim_quality_gates.erl**
- Wraps tcps_quality_gates with detailed feedback
- Explains gate failures with remediation steps
- Shows quality metrics trends
- Demonstrates Jidoka (stop-the-line) principle

**sim_andon.erl**
- Wraps tcps Andon system (from tcps/tcps_andon.erl)
- Shows proper escalation procedures
- Demonstrates root cause analysis
- Tracks response times and effectiveness

### 2.3 MCP Integration Layer

```
src/tcps_mcp_diataxis/mcp/
├── mcp_server.erl             % MCP server for simulator
├── mcp_tools.erl              % Tool definitions
├── mcp_resources.erl          % Resource providers
├── mcp_prompts.erl            % Prompt templates
└── mcp_subscriptions.erl      % Real-time updates
```

#### 2.3.1 MCP Server

**mcp_server.erl** - Main MCP server implementation
```erlang
-module(mcp_server).

%% Starts MCP server exposing simulator capabilities to AI assistants
-spec start_link() -> {ok, pid()}.
start_link() ->
    Capabilities = #{
        resources => true,
        tools => true,
        prompts => true,
        subscriptions => true
    },
    {ok, Server} = erlmcp_server:start_link(tcps_simulator, Capabilities),
    register_all_capabilities(Server),
    {ok, Server}.

register_all_capabilities(Server) ->
    %% Register resources
    mcp_resources:register_all(Server),

    %% Register tools
    mcp_tools:register_all(Server),

    %% Register prompts
    mcp_prompts:register_all(Server),

    ok.
```

#### 2.3.2 MCP Tools

**mcp_tools.erl** - Exposes simulator actions as MCP tools
```erlang
-module(mcp_tools).

%% MCP Tools for AI-assisted learning
-export([
    register_all/1,
    tool_create_work_order/1,
    tool_check_wip_limits/1,
    tool_trigger_andon/1,
    tool_run_quality_gate/1,
    tool_get_metrics/1,
    tool_explain_concept/1,
    tool_suggest_action/1,
    tool_validate_solution/1
]).

%% Tool Definitions:
%%
%% 1. create_work_order: Create a new work order in simulation
%%    Parameters: {bucket, priority, description}
%%    Returns: work order ID and explanation
%%
%% 2. check_wip_limits: Check WIP status for bucket
%%    Parameters: {bucket}
%%    Returns: current WIP, limit, utilization, recommendations
%%
%% 3. trigger_andon: Trigger Andon event for quality issue
%%    Parameters: {severity, category, description}
%%    Returns: andon ID, escalation level, required actions
%%
%% 4. run_quality_gate: Execute specific quality gate
%%    Parameters: {gate_name, sku_id}
%%    Returns: pass/fail, violations, remediation steps
%%
%% 5. get_metrics: Retrieve simulation metrics
%%    Parameters: {metric_type, time_range}
%%    Returns: metric values, trends, insights
%%
%% 6. explain_concept: Get detailed explanation of TCPS concept
%%    Parameters: {concept_name}
%%    Returns: explanation with examples and references
%%
%% 7. suggest_action: AI-powered action suggestion
%%    Parameters: {current_state}
%%    Returns: ranked action suggestions with reasoning
%%
%% 8. validate_solution: Validate user's proposed solution
%%    Parameters: {scenario_id, proposed_actions}
%%    Returns: validation result with feedback
```

#### 2.3.3 MCP Resources

**mcp_resources.erl** - Exposes simulator state as MCP resources
```erlang
-module(mcp_resources).

%% Resource URIs:
%%
%% tcps://simulation/{sim_id}/state
%% tcps://simulation/{sim_id}/metrics
%% tcps://simulation/{sim_id}/work-orders
%% tcps://simulation/{sim_id}/kanban
%% tcps://simulation/{sim_id}/quality-gates
%% tcps://simulation/{sim_id}/andon-events
%% tcps://tutorial/{tutorial_id}
%% tcps://howto/{howto_id}
%% tcps://explanation/{concept_id}
%% tcps://reference/{topic_id}

-export([
    register_all/1,
    resource_simulation_state/1,
    resource_metrics/1,
    resource_work_orders/1,
    resource_tutorial/1,
    resource_explanation/1
]).
```

#### 2.3.4 MCP Prompts

**mcp_prompts.erl** - Prompt templates for AI guidance
```erlang
-module(mcp_prompts).

%% Prompt Templates:
%%
%% 1. "tutorial_guide": Step-by-step tutorial assistance
%% 2. "concept_explanation": Deep dive into TCPS concepts
%% 3. "problem_diagnosis": Help diagnose simulation issues
%% 4. "action_recommendation": Suggest next best action
%% 5. "scenario_briefing": Explain scenario objectives
%% 6. "metrics_analysis": Analyze simulation metrics
%% 7. "best_practices": Provide TCPS best practices

-export([
    register_all/1,
    prompt_tutorial_guide/1,
    prompt_concept_explanation/1,
    prompt_problem_diagnosis/1,
    prompt_action_recommendation/1
]).
```

### 2.4 Web Dashboard (UI Layer)

```
src/tcps_mcp_diataxis/web/
├── web_supervisor.erl         % Cowboy supervisor
├── web_handlers.erl           % HTTP request handlers
├── web_api.erl                % REST API endpoints
├── web_websocket.erl          % WebSocket for real-time updates
├── web_sse.erl                % Server-Sent Events for metrics
└── static/                    % Static web assets
    ├── index.html
    ├── simulator.html
    ├── tutorial.html
    ├── css/
    │   └── simulator.css
    └── js/
        ├── simulator.js
        ├── kanban-board.js
        ├── metrics-charts.js
        └── tutorial-ui.js
```

#### 2.4.1 Dashboard Pages

**Main Dashboard (index.html)**
- Overview of available tutorials and scenarios
- Learning progress tracker
- Recent simulation results
- Quick start buttons

**Simulator Interface (simulator.html)**
- Real-time Kanban board visualization
- Work order queue display
- Quality gate status indicators
- Andon event timeline
- Metrics charts (lead time, throughput, defect rate)
- Action panel for user interactions

**Tutorial Interface (tutorial.html)**
- Step-by-step tutorial display
- Progress indicator
- Action validation feedback
- Hint system
- MCP-powered AI assistant chat
- Navigation controls

#### 2.4.2 REST API Endpoints

```
GET    /api/simulations              % List all simulations
POST   /api/simulations              % Create new simulation
GET    /api/simulations/:id          % Get simulation state
DELETE /api/simulations/:id          % Delete simulation
POST   /api/simulations/:id/actions  % Execute action
POST   /api/simulations/:id/reset    % Reset simulation

GET    /api/tutorials                % List tutorials
GET    /api/tutorials/:id            % Get tutorial content
POST   /api/tutorials/:id/start      % Start tutorial
GET    /api/tutorials/:id/progress   % Get progress

GET    /api/howtos                   % List how-to guides
GET    /api/explanations             % List explanations
GET    /api/reference                % Get reference docs

GET    /api/metrics/:sim_id          % Get simulation metrics
GET    /api/work-orders/:sim_id      % Get work orders
GET    /api/kanban/:sim_id           % Get Kanban state
GET    /api/quality-gates/:sim_id    % Get gate status
GET    /api/andon/:sim_id            % Get Andon events

WebSocket /ws/simulation/:id         % Real-time simulation updates
SSE    /sse/metrics/:id              % Server-sent metric events
```

#### 2.4.3 Real-Time Visualization Components

**Kanban Board Visualization**
```javascript
// kanban-board.js
class KanbanBoard {
    constructor(containerId, websocket) {
        this.ws = websocket;
        this.buckets = ['reliability', 'security', 'cost', 'compliance'];
        this.initBoard();
    }

    renderBucket(bucket, workOrders, wipLimit) {
        // Visual representation of WIP limits
        // Drag-and-drop for work order movement
        // Color coding for priority
        // Animations for state changes
    }

    updateMetrics(metrics) {
        // Update utilization bars
        // Show throughput rates
        // Display lead time trends
    }
}
```

**Quality Gates Dashboard**
```javascript
// metrics-charts.js
class QualityGatesDashboard {
    renderGateStatus(gates) {
        // Traffic light indicators (red/yellow/green)
        // Gate execution timeline
        // Failure rate trends
        // Remediation suggestions
    }

    renderMetricsTrends(metrics) {
        // Test coverage over time
        // Defect rate trends
        // First pass yield
        // Lead time distribution
    }
}
```

### 2.5 Telemetry System

```
src/tcps_mcp_diataxis/telemetry/
├── telemetry_config.erl       % OpenTelemetry configuration
├── telemetry_spans.erl        % Span creation and management
├── telemetry_metrics.erl      % Custom metrics
└── telemetry_exporter.erl     % Export to OTEL collector
```

#### 2.5.1 Instrumentation Points

**Simulation Events**
- Simulation lifecycle (create, start, complete, reset)
- User action execution (with timing and validation results)
- Scenario progression (steps completed, time taken)

**TCPS Operations**
- Work order operations (create, start, complete)
- Kanban operations (WIP checks, pull signals)
- Quality gate executions (gate name, pass/fail, duration)
- Andon events (trigger, escalation, resolution)

**Learning Analytics**
- Tutorial progress (steps completed, time per step)
- Concept understanding (hints used, retries)
- Success rates per scenario
- User engagement metrics

#### 2.5.2 Trace Context Propagation

```erlang
-module(telemetry_spans).

-export([
    start_simulation_span/1,
    start_action_span/3,
    start_tutorial_span/1,
    add_event/2,
    set_attributes/2,
    end_span/1
]).

%% Example trace hierarchy:
%%
%% Simulation
%%   ├── Tutorial Execution
%%   │   ├── Tutorial Step 1
%%   │   │   ├── Action: Create Work Order
%%   │   │   │   ├── TCPS: Work Order Creation
%%   │   │   │   └── TCPS: Kanban WIP Check
%%   │   │   └── Validation
%%   │   └── Tutorial Step 2
%%   │       └── Action: Check Metrics
%%   └── Scenario Execution
%%       ├── Event Injection
%%       └── Metrics Collection
```

#### 2.5.3 Custom Metrics

```erlang
%% Learning metrics
tutorial_completion_rate
tutorial_step_duration
hints_per_tutorial
retry_count
time_to_mastery

%% Simulation metrics
simulation_duration
actions_per_simulation
success_rate_by_scenario
average_score

%% TCPS metrics (from simulation)
work_orders_created
wip_utilization
quality_gate_pass_rate
andon_response_time
lead_time_variance
```

### 2.6 Persistence Layer

```
src/tcps_mcp_diataxis/persistence/
├── persistence_coordinator.erl % Main persistence interface
├── persistence_rdf.erl        % RDF serialization
├── persistence_receipt.erl    % Receipt generation
└── schemas/
    ├── simulation.ttl         % Simulation ontology
    ├── tutorial.ttl           % Tutorial ontology
    └── learning_path.ttl      % Learning path ontology
```

#### 2.6.1 RDF Ontology Structure

**Simulation Ontology (simulation.ttl)**
```turtle
@prefix tcps-sim: <http://tcps.example.org/simulator#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

tcps-sim:Simulation a rdfs:Class ;
    rdfs:label "TCPS Simulation" ;
    rdfs:comment "A single simulation instance" .

tcps-sim:hasScenario a rdf:Property ;
    rdfs:domain tcps-sim:Simulation ;
    rdfs:range tcps-sim:Scenario .

tcps-sim:hasState a rdf:Property ;
    rdfs:domain tcps-sim:Simulation ;
    rdfs:range tcps-sim:SimulationState .

tcps-sim:WorkOrder a rdfs:Class ;
    rdfs:subClassOf tcps:WorkOrder .

tcps-sim:KanbanBoard a rdfs:Class ;
    rdfs:label "Kanban Board State" .
```

**Tutorial Ontology (tutorial.ttl)**
```turtle
@prefix tcps-tutorial: <http://tcps.example.org/tutorial#> .

tcps-tutorial:Tutorial a rdfs:Class ;
    rdfs:label "TCPS Tutorial" .

tcps-tutorial:hasStep a rdf:Property ;
    rdfs:domain tcps-tutorial:Tutorial ;
    rdfs:range tcps-tutorial:TutorialStep .

tcps-tutorial:prerequisite a rdf:Property ;
    rdfs:domain tcps-tutorial:Tutorial ;
    rdfs:range tcps-tutorial:Tutorial .

tcps-tutorial:teaches a rdf:Property ;
    rdfs:domain tcps-tutorial:Tutorial ;
    rdfs:range tcps:Concept .
```

**Learning Path Ontology (learning_path.ttl)**
```turtle
@prefix tcps-learn: <http://tcps.example.org/learning#> .

tcps-learn:LearningPath a rdfs:Class ;
    rdfs:label "User Learning Path" .

tcps-learn:completed a rdf:Property ;
    rdfs:domain tcps-learn:LearningPath ;
    rdfs:range tcps-tutorial:Tutorial .

tcps-learn:hasProgress a rdf:Property ;
    rdfs:domain tcps-learn:LearningPath ;
    rdfs:range xsd:float .
```

#### 2.6.2 Receipt System

**persistence_receipt.erl** - Generates cryptographic receipts
```erlang
-module(persistence_receipt).

%% Receipt types for simulator
-type receipt_type() ::
    simulation_created |
    tutorial_completed |
    scenario_passed |
    achievement_earned |
    action_executed |
    state_persisted.

%% Generate receipt with timestamp, hash, and signature
-spec generate_receipt(receipt_type(), map()) -> receipt().
generate_receipt(Type, Data) ->
    Timestamp = erlang:system_time(microsecond),
    Receipt = #{
        type => Type,
        timestamp => Timestamp,
        data => Data,
        previous_hash => get_previous_hash(),
        version => <<"1.0.0">>
    },
    Hash = compute_hash(Receipt),
    Receipt#{
        hash => Hash,
        signature => sign_receipt(Hash)
    }.
```

## 3. Data Flow Diagrams

### 3.1 Tutorial Execution Flow

```
┌────────┐          ┌──────────────┐          ┌──────────────┐
│  User  │          │   Web UI     │          │   Diataxis   │
│        │          │  (Browser)   │          │ Coordinator  │
└───┬────┘          └──────┬───────┘          └──────┬───────┘
    │                      │                         │
    │  Start Tutorial      │                         │
    ├─────────────────────>│                         │
    │                      │  Load Tutorial          │
    │                      ├────────────────────────>│
    │                      │                         │
    │                      │  Tutorial Content       │
    │                      │<────────────────────────┤
    │  Display Tutorial    │                         │
    │<─────────────────────┤                         │
    │                      │                         │
    │  Execute Step Action │                         │
    ├─────────────────────>│                         │
    │                      │                         │
    │                      │         ┌───────────────┴──────────┐
    │                      │         │  Simulation Coordinator  │
    │                      │         └───────────────┬──────────┘
    │                      │  Invoke Simulator       │
    │                      ├────────────────────────>│
    │                      │                         │
    │                      │         ┌───────────────▼─────────┐
    │                      │         │   TCPS Work Order      │
    │                      │         │   (Real Module)        │
    │                      │         └───────────────┬─────────┘
    │                      │  Result + Explanation   │
    │                      │<────────────────────────┤
    │                      │                         │
    │                      │  Update Progress        │
    │                      ├────────────────────────>│
    │  Display Result      │                         │
    │<─────────────────────┤                         │
    │                      │                         │
    │                      │  OTEL Trace Context     │
    │                      │  Propagates Through     │
    │                      │  Entire Flow            │
```

### 3.2 MCP Tool Invocation Flow

```
┌───────────────┐      ┌──────────────┐      ┌──────────────┐
│  AI Assistant │      │  MCP Server  │      │  Simulator   │
│  (Claude)     │      │              │      │  Backend     │
└───────┬───────┘      └──────┬───────┘      └──────┬───────┘
        │                     │                     │
        │  List Tools         │                     │
        ├────────────────────>│                     │
        │                     │                     │
        │  Tool Catalog       │                     │
        │<────────────────────┤                     │
        │                     │                     │
        │  Call Tool:         │                     │
        │  check_wip_limits   │                     │
        ├────────────────────>│                     │
        │                     │  Get Kanban State   │
        │                     ├────────────────────>│
        │                     │                     │
        │                     │  WIP Status + Rec.  │
        │                     │<────────────────────┤
        │                     │                     │
        │  Tool Result        │                     │
        │  (with explanation) │                     │
        │<────────────────────┤                     │
        │                     │                     │
        │  Suggest Action:    │                     │
        │  "Create work order │                     │
        │   in reliability    │                     │
        │   bucket"           │                     │
```

### 3.3 Real-Time Metrics Flow (WebSocket + SSE)

```
┌─────────────┐     ┌──────────────┐     ┌──────────────┐
│   Browser   │     │  WebSocket   │     │  Simulation  │
│     UI      │     │   Handler    │     │  Coordinator │
└──────┬──────┘     └──────┬───────┘     └──────┬───────┘
       │                   │                    │
       │  Connect WS       │                    │
       ├──────────────────>│                    │
       │                   │  Subscribe         │
       │                   ├───────────────────>│
       │                   │                    │
       │                   │  User Action       │
       │  Execute Action   ├<───────────────────┤
       ├──────────────────>│                    │
       │                   ├───────────────────>│
       │                   │                    │
       │                   │  State Change      │
       │                   │<───────────────────┤
       │  State Update     │                    │
       │<──────────────────┤                    │
       │  (JSON)           │                    │
       │                   │                    │
       │                   │  ┌─────────────────┴──────┐
       │                   │  │  Metrics Aggregator   │
       │                   │  └─────────────────┬──────┘
       │                   │  Periodic Metrics  │
       │                   │<───────────────────┤
       │  Metrics Update   │                    │
       │<──────────────────┤                    │
       │  (via SSE)        │                    │
```

## 4. File Structure

### 4.1 Complete Source Tree

```
/Users/sac/erlmcp/src/tcps_mcp_diataxis/
├── README.md
├── rebar.config                      % Dependencies and config
│
├── diataxis/                         % Diataxis Framework
│   ├── diataxis_coordinator.erl
│   ├── diataxis_tutorial.erl
│   ├── diataxis_howto.erl
│   ├── diataxis_explanation.erl
│   ├── diataxis_reference.erl
│   └── diataxis_progress.erl
│
├── simulation/                       % Simulation Engine
│   ├── sim_coordinator.erl
│   ├── sim_scenario.erl
│   ├── sim_work_order.erl
│   ├── sim_kanban.erl
│   ├── sim_quality_gates.erl
│   ├── sim_andon.erl
│   ├── sim_metrics.erl
│   ├── sim_state.erl
│   └── sim_validation.erl
│
├── mcp/                              % MCP Integration
│   ├── mcp_server.erl
│   ├── mcp_tools.erl
│   ├── mcp_resources.erl
│   ├── mcp_prompts.erl
│   └── mcp_subscriptions.erl
│
├── web/                              % Web Dashboard
│   ├── web_supervisor.erl
│   ├── web_handlers.erl
│   ├── web_api.erl
│   ├── web_websocket.erl
│   └── web_sse.erl
│
├── telemetry/                        % Telemetry/OTEL
│   ├── telemetry_config.erl
│   ├── telemetry_spans.erl
│   ├── telemetry_metrics.erl
│   └── telemetry_exporter.erl
│
├── persistence/                      % Persistence Layer
│   ├── persistence_coordinator.erl
│   ├── persistence_rdf.erl
│   ├── persistence_receipt.erl
│   └── schemas/
│       ├── simulation.ttl
│       ├── tutorial.ttl
│       └── learning_path.ttl
│
├── priv/                             % Static Assets
│   ├── static/
│   │   ├── index.html
│   │   ├── simulator.html
│   │   ├── tutorial.html
│   │   ├── css/
│   │   │   ├── simulator.css
│   │   │   ├── kanban.css
│   │   │   └── tutorial.css
│   │   └── js/
│   │       ├── simulator.js
│   │       ├── kanban-board.js
│   │       ├── metrics-charts.js
│   │       ├── tutorial-ui.js
│   │       └── websocket-client.js
│   │
│   ├── tutorials/                    % Tutorial Content
│   │   ├── 01_kanban_basics.md
│   │   ├── 02_work_orders.md
│   │   ├── 03_quality_gates.md
│   │   ├── 04_andon_system.md
│   │   └── 05_continuous_flow.md
│   │
│   ├── howto/                        % How-To Guides
│   │   ├── reduce_wip.md
│   │   ├── handle_quality_failure.md
│   │   ├── optimize_throughput.md
│   │   └── configure_limits.md
│   │
│   ├── explanation/                  % Conceptual Docs
│   │   ├── pull_systems.md
│   │   ├── jidoka.md
│   │   ├── heijunka.md
│   │   ├── kaizen.md
│   │   └── quality_gates.md
│   │
│   └── reference/                    % Reference Docs
│       ├── api.md
│       ├── configuration.md
│       ├── metrics.md
│       └── troubleshooting.md
│
├── test/                             % Tests
│   ├── diataxis_tests.erl
│   ├── simulation_tests.erl
│   ├── mcp_integration_tests.erl
│   └── web_api_tests.erl
│
└── include/
    ├── simulator.hrl                 % Shared type definitions
    └── diataxis.hrl                  % Diataxis types
```

## 5. API Specifications

### 5.1 Simulation API

#### Create Simulation
```http
POST /api/simulations
Content-Type: application/json

{
  "scenario_id": "kanban_basics",
  "mode": "tutorial",
  "user_id": "user-123"
}

Response 201:
{
  "sim_id": "sim-abc-123",
  "scenario": {
    "id": "kanban_basics",
    "title": "Kanban Board Basics",
    "difficulty": "beginner",
    "estimated_time": 15
  },
  "state": "ready",
  "created_at": "2026-01-26T22:00:00Z"
}
```

#### Execute Action
```http
POST /api/simulations/{sim_id}/actions
Content-Type: application/json

{
  "action": "create_work_order",
  "parameters": {
    "bucket": "reliability",
    "priority": 7,
    "description": "Fix memory leak"
  }
}

Response 200:
{
  "result": "success",
  "work_order_id": "wo-456",
  "state_changes": {
    "wip_change": {
      "reliability": {"before": 2, "after": 3}
    }
  },
  "explanation": "Work order created successfully. Reliability bucket now at 60% capacity.",
  "metrics": {
    "execution_time_ms": 12,
    "validation_passed": true
  },
  "next_suggested_actions": [
    "check_wip_status",
    "view_kanban_board"
  ]
}
```

### 5.2 Tutorial API

#### Get Tutorial
```http
GET /api/tutorials/kanban_basics

Response 200:
{
  "id": "kanban_basics",
  "title": "Kanban Board Basics",
  "description": "Learn how to manage work-in-progress limits",
  "difficulty": "beginner",
  "steps": [
    {
      "order": 1,
      "title": "Understanding WIP Limits",
      "description": "Learn what WIP limits are and why they matter",
      "action": null,
      "validation": null
    },
    {
      "order": 2,
      "title": "Create Your First Work Order",
      "description": "Create a work order in the reliability bucket",
      "action": {
        "type": "create_work_order",
        "parameters": {
          "bucket": "reliability",
          "priority": 5
        }
      },
      "validation": {
        "type": "work_order_exists",
        "criteria": {"bucket": "reliability"}
      },
      "hints": [
        "Choose priority between 1-10",
        "Reliability bucket handles production bugs"
      ]
    }
  ],
  "estimated_time": 15,
  "prerequisites": []
}
```

### 5.3 MCP Tool API

#### Tool Schema: check_wip_limits
```json
{
  "name": "check_wip_limits",
  "description": "Check current WIP limits and utilization for a bucket",
  "inputSchema": {
    "type": "object",
    "properties": {
      "bucket": {
        "type": "string",
        "enum": ["reliability", "security", "cost", "compliance"],
        "description": "The Kanban bucket to check"
      },
      "sim_id": {
        "type": "string",
        "description": "Simulation ID (optional, uses current if not provided)"
      }
    },
    "required": ["bucket"]
  }
}
```

#### Tool Response: check_wip_limits
```json
{
  "content": [
    {
      "type": "text",
      "text": "WIP Status for 'reliability' bucket:\n\nCurrent WIP: 3/5 (60% utilization)\nAvailable Capacity: 2\n\nWork Orders:\n1. wo-123: Fix memory leak (priority 7)\n2. wo-124: Resolve crash on startup (priority 9)\n3. wo-125: Improve error handling (priority 5)\n\nRecommendations:\n- You have 40% capacity remaining\n- Consider starting work on highest priority item (wo-124)\n- Monitor for new high-priority pull signals"
    },
    {
      "type": "resource",
      "resource": {
        "uri": "tcps://simulation/sim-abc-123/kanban",
        "mimeType": "application/json",
        "text": "{\"bucket\": \"reliability\", \"wip\": 3, \"limit\": 5, ...}"
      }
    }
  ]
}
```

### 5.4 WebSocket Protocol

#### Connection
```javascript
ws://localhost:8080/ws/simulation/sim-abc-123
```

#### Message Format (Client -> Server)
```json
{
  "type": "action",
  "action": "create_work_order",
  "parameters": {
    "bucket": "security",
    "priority": 9,
    "description": "Patch CVE-2024-1234"
  }
}
```

#### Message Format (Server -> Client)
```json
{
  "type": "state_update",
  "timestamp": "2026-01-26T22:05:00Z",
  "changes": {
    "work_orders": {
      "added": ["wo-789"]
    },
    "kanban": {
      "security": {
        "wip": 4,
        "limit": 5
      }
    }
  },
  "event": {
    "type": "work_order_created",
    "details": {
      "id": "wo-789",
      "bucket": "security",
      "priority": 9
    }
  }
}
```

## 6. Integration Points

### 6.1 Integration with Existing TCPS Modules

The simulator acts as a **wrapper and orchestrator** around existing TCPS modules:

```erlang
%% sim_work_order.erl wraps tcps_work_order.erl
-module(sim_work_order).

create_work_order(SimId, Params) ->
    %% 1. Validate in simulation context
    case sim_validation:validate_action(SimId, create_work_order, Params) of
        {ok, ValidatedParams} ->
            %% 2. Call real TCPS module
            Result = tcps_work_order:create_work_order(ValidatedParams),

            %% 3. Generate educational feedback
            Explanation = generate_explanation(create_work_order, Result),

            %% 4. Update simulation state
            sim_state:record_action(SimId, create_work_order, Result),

            %% 5. Emit telemetry
            telemetry_spans:add_event(SimId, work_order_created, Result),

            %% 6. Return enhanced result
            {ok, Result#{explanation => Explanation}};
        {error, Reason} ->
            {error, Reason}
    end.
```

### 6.2 Integration with MCP Infrastructure

Uses existing erlmcp libraries:

```erlang
%% mcp_server.erl uses erlmcp_server
-module(mcp_server).

start_link() ->
    %% Use existing MCP server implementation
    Capabilities = #mcp_server_capabilities{
        resources = true,
        tools = true,
        prompts = true,
        subscriptions = true
    },
    {ok, Server} = erlmcp_server:start_link(tcps_simulator, Capabilities),

    %% Register simulator-specific resources
    erlmcp_server:add_resource(Server, <<"tcps://simulation/*/state">>,
        fun mcp_resources:resource_simulation_state/1),

    %% Register simulator-specific tools
    ToolSchema = #{<<"type">> => <<"object">>, ...},
    erlmcp_server:add_tool_with_schema(Server, <<"check_wip_limits">>,
        fun mcp_tools:tool_check_wip_limits/1, ToolSchema),

    {ok, Server}.
```

### 6.3 Integration with Web Dashboard

Uses existing dashboard infrastructure (tcps_dashboard.erl):

```erlang
%% web_supervisor.erl extends tcps_dashboard
-module(web_supervisor).

init([Config]) ->
    %% Start base dashboard
    BaseDashboard = {tcps_dashboard, {tcps_dashboard, start_link, [Config]},
                     permanent, 5000, worker, [tcps_dashboard]},

    %% Add simulator-specific handlers
    SimHandlers = {web_handlers, {web_handlers, start_link, []},
                   permanent, 5000, worker, [web_handlers]},

    %% Add WebSocket handler
    WSHandler = {web_websocket, {web_websocket, start_link, []},
                 permanent, 5000, worker, [web_websocket]},

    {ok, {{one_for_one, 5, 10}, [BaseDashboard, SimHandlers, WSHandler]}}.
```

### 6.4 Integration with Telemetry

Uses OpenTelemetry with custom semantic conventions:

```erlang
%% telemetry_spans.erl
-module(telemetry_spans).

start_action_span(SimId, Action, Params) ->
    %% Create span with simulation context
    Ctx = otel_ctx:get_current(),
    SpanCtx = otel_tracer:start_span(
        <<"simulator.action">>,
        #{
            attributes => #{
                <<"simulator.sim_id">> => SimId,
                <<"simulator.action">> => Action,
                <<"tcps.bucket">> => maps:get(bucket, Params, undefined),
                <<"tcps.priority">> => maps:get(priority, Params, undefined)
            },
            kind => internal
        }
    ),
    otel_ctx:attach(SpanCtx).
```

## 7. Deployment Architecture

### 7.1 Container Architecture

```yaml
# docker-compose.yml
version: '3.8'

services:
  simulator:
    build: .
    ports:
      - "8080:8080"      # Web dashboard
      - "9090:9090"      # MCP server
    environment:
      - OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318
      - RDF_STORAGE_PATH=/data/rdf
    volumes:
      - simulator-data:/data
    depends_on:
      - otel-collector

  otel-collector:
    image: otel/opentelemetry-collector:latest
    ports:
      - "4318:4318"      # OTLP HTTP
      - "13133:13133"    # Health check
    volumes:
      - ./otel-config.yaml:/etc/otel/config.yaml
    command: ["--config=/etc/otel/config.yaml"]

  jaeger:
    image: jaegertracing/all-in-one:latest
    ports:
      - "16686:16686"    # Jaeger UI
      - "4317:4317"      # OTLP gRPC

volumes:
  simulator-data:
```

### 7.2 System Requirements

**Minimum Requirements**
- Erlang/OTP 25+
- 2 CPU cores
- 4GB RAM
- 10GB disk space

**Recommended Requirements**
- Erlang/OTP 26+
- 4 CPU cores
- 8GB RAM
- 20GB disk space (for RDF storage and telemetry data)

### 7.3 Configuration

```erlang
%% config/sys.config
[
    {tcps_mcp_diataxis, [
        {web_port, 8080},
        {mcp_port, 9090},
        {max_simulations, 100},
        {simulation_timeout, 3600000}, % 1 hour in ms
        {enable_telemetry, true},
        {otel_endpoint, "http://localhost:4318"},
        {rdf_storage_path, "priv/data/rdf"},
        {tutorial_path, "priv/tutorials"},
        {max_concurrent_users, 50}
    ]},
    {kernel, [
        {logger_level, info},
        {logger, [
            {handler, default, logger_std_h, #{
                level => info,
                formatter => {logger_formatter, #{
                    single_line => false,
                    template => [time," [",level,"] ",msg,"\n"]
                }}
            }}
        ]}
    ]}
].
```

## 8. Security Considerations

### 8.1 Authentication and Authorization

```erlang
%% For production deployment (not MVP)
-module(web_auth).

%% Simple token-based authentication
-export([authenticate/1, authorize/2]).

authenticate(Token) ->
    %% Verify JWT or session token
    %% For educational use, can be simplified
    case validate_token(Token) of
        {ok, UserId} -> {ok, UserId};
        {error, _} -> {error, unauthorized}
    end.

authorize(UserId, Action) ->
    %% Check user permissions
    %% For MVP: all authenticated users can do everything
    {ok, allowed}.
```

### 8.2 Input Validation

All user inputs are validated at multiple layers:

1. **Web Layer**: HTTP parameter validation
2. **API Layer**: JSON schema validation
3. **Simulation Layer**: Business logic validation
4. **TCPS Layer**: Domain-specific validation

### 8.3 Data Isolation

- Each simulation runs in isolated state
- User progress tracked separately
- No cross-simulation data leakage
- RDF receipts ensure audit trail

## 9. Performance Considerations

### 9.1 Scalability Targets

**Single Instance**
- 50 concurrent simulations
- 100 concurrent WebSocket connections
- 1000 requests/second (API)
- <100ms p95 latency for actions

**Horizontal Scaling**
- Support for distributed Erlang cluster
- Simulation state can be partitioned by user_id
- Shared RDF storage (via distributed ETS or external DB)

### 9.2 Optimization Strategies

**Caching**
```erlang
%% Cache tutorial content in ETS
-module(tutorial_cache).

get_tutorial(TutorialId) ->
    case ets:lookup(tutorial_cache, TutorialId) of
        [{TutorialId, Content}] -> {ok, Content};
        [] ->
            Content = load_tutorial_from_disk(TutorialId),
            ets:insert(tutorial_cache, {TutorialId, Content}),
            {ok, Content}
    end.
```

**WebSocket Pooling**
- Use poolboy for WebSocket connection pooling
- Limit connections per user
- Automatic cleanup of stale connections

**Metrics Aggregation**
- Pre-aggregate common metrics
- Cache metrics for 5-second windows
- Use ETS for fast in-memory aggregation

## 10. Testing Strategy

### 10.1 Test Categories

**Unit Tests**
- Test each module in isolation
- Mock dependencies (TCPS modules, MCP server)
- Cover all public API functions

**Integration Tests**
- Test module interactions
- Test MCP tool invocations end-to-end
- Test WebSocket communication
- Test RDF persistence

**Property-Based Tests**
- Simulation state invariants
- Kanban WIP limit enforcement
- Quality gate ordering

**UI Tests**
- Selenium/Playwright for browser automation
- Test tutorial flows
- Test real-time updates

### 10.2 Test Structure

```
test/
├── unit/
│   ├── diataxis_tests.erl
│   ├── sim_coordinator_tests.erl
│   ├── mcp_tools_tests.erl
│   └── web_api_tests.erl
│
├── integration/
│   ├── tutorial_flow_tests.erl
│   ├── mcp_integration_tests.erl
│   ├── websocket_tests.erl
│   └── persistence_tests.erl
│
├── property/
│   ├── simulation_properties.erl
│   └── kanban_properties.erl
│
└── ui/
    ├── tutorial_ui_tests.js
    └── dashboard_tests.js
```

## 11. Documentation Strategy (Diataxis Applied)

### 11.1 Tutorial Documentation

**Target Audience**: New learners who want hands-on experience

**Content Structure**:
1. **Introduction**: What you'll learn, prerequisites, time estimate
2. **Setup**: Create simulation environment
3. **Step-by-step instructions**: With screenshots and code examples
4. **Validation**: How to verify you completed it correctly
5. **Next steps**: What to learn next

**Example Tutorials**:
- Tutorial 1: Your First Kanban Board (15 min)
- Tutorial 2: Managing Work Orders (20 min)
- Tutorial 3: Quality Gates in Action (25 min)
- Tutorial 4: Responding to Andon Events (20 min)
- Tutorial 5: Continuous Flow Optimization (30 min)

### 11.2 How-To Documentation

**Target Audience**: Users with specific goals

**Content Structure**:
1. **Problem statement**: What you want to achieve
2. **Solution steps**: Direct, numbered instructions
3. **Code/configuration examples**
4. **Troubleshooting**: Common issues

**Example How-Tos**:
- How to Reduce WIP Limits Safely
- How to Handle Quality Gate Failures
- How to Optimize Throughput
- How to Configure Bucket Priorities
- How to Analyze Simulation Metrics

### 11.3 Explanation Documentation

**Target Audience**: Users wanting conceptual understanding

**Content Structure**:
1. **Concept overview**: High-level introduction
2. **Why it matters**: Business/technical value
3. **How it works**: Detailed explanation with diagrams
4. **Relationship to other concepts**
5. **Trade-offs and considerations**

**Example Explanations**:
- Understanding Pull Systems vs. Push Systems
- Jidoka: Built-in Quality
- Heijunka: Production Leveling
- Kaizen: Continuous Improvement
- Quality Gates in TCPS

### 11.4 Reference Documentation

**Target Audience**: Users looking for specific information

**Content Structure**:
1. **API reference**: Complete function signatures
2. **Configuration options**: All available settings
3. **Metrics catalog**: Every metric defined
4. **Error codes**: All possible errors with resolution
5. **Glossary**: Term definitions

**Example References**:
- API Reference (complete REST API)
- MCP Tools Reference
- Configuration Reference
- Metrics Reference
- Glossary of TCPS Terms

## 12. Roadmap and Phases

### Phase 1: Foundation (Weeks 1-2)
- [x] Architecture design (this document)
- [ ] Basic simulation coordinator
- [ ] Simple scenario system
- [ ] Core MCP server integration
- [ ] Basic web UI (static HTML)

### Phase 2: Core Simulation (Weeks 3-4)
- [ ] Work order simulation wrapper
- [ ] Kanban board simulation
- [ ] Quality gates simulation
- [ ] Basic metrics collection
- [ ] RDF persistence

### Phase 3: Diataxis Content (Weeks 5-6)
- [ ] Tutorial system implementation
- [ ] 5 initial tutorials
- [ ] How-to guides (5)
- [ ] Explanation articles (5)
- [ ] Reference documentation

### Phase 4: Interactive UI (Weeks 7-8)
- [ ] WebSocket real-time updates
- [ ] Kanban board visualization
- [ ] Metrics charts
- [ ] Tutorial UI with progress tracking
- [ ] MCP-powered AI assistant integration

### Phase 5: Telemetry (Week 9)
- [ ] OpenTelemetry integration
- [ ] Custom spans and metrics
- [ ] Trace visualization
- [ ] Learning analytics

### Phase 6: Polish (Week 10)
- [ ] Comprehensive testing
- [ ] Performance optimization
- [ ] Documentation completion
- [ ] Deployment packaging

## 13. Success Metrics

### 13.1 Learning Outcomes

- **Tutorial Completion Rate**: >70% of users complete at least one tutorial
- **Concept Mastery**: >60% pass challenge scenarios on first attempt
- **Time to Proficiency**: Users understand core TCPS concepts within 2 hours
- **Engagement**: Average session duration >20 minutes

### 13.2 Technical Performance

- **API Latency**: p95 <100ms for all actions
- **WebSocket Updates**: <50ms from state change to browser update
- **Simulation Throughput**: >50 concurrent simulations per instance
- **Uptime**: >99.5% availability

### 13.3 Code Quality

- **Test Coverage**: >80% line coverage
- **Type Safety**: All exported functions have specs
- **Documentation**: 100% of public APIs documented
- **Build Time**: <30s for full build

## 14. Future Enhancements

### Post-MVP Features

1. **Multiplayer Simulations**: Multiple users collaborate on same simulation
2. **Competitive Mode**: Users compete on same scenario for best score
3. **Custom Scenario Builder**: Users create and share their own scenarios
4. **Advanced Analytics**: Machine learning-powered insights
5. **Mobile App**: iOS/Android apps for on-the-go learning
6. **Certification System**: Formal TCPS certification based on achievements
7. **Integration with Real Projects**: Connect to actual GitHub repos
8. **AI Tutor Mode**: Personalized learning paths based on performance
9. **Community Features**: Forums, leaderboards, scenario sharing
10. **Enterprise Features**: Team management, progress tracking, custom branding

## 15. Conclusion

This architecture provides a comprehensive blueprint for building a production-grade TCPS MCP Diataxis simulator. Key design decisions:

1. **Leverage Existing Infrastructure**: Build on top of erlmcp and existing TCPS modules rather than reinventing
2. **OTP-First Design**: Use gen_server, supervisors, and OTP patterns throughout
3. **Diataxis Structure**: Provide four distinct types of documentation for different learning modes
4. **MCP Integration**: Expose simulator capabilities as MCP tools for AI-assisted learning
5. **Real-Time Feedback**: WebSocket and SSE for immediate user feedback
6. **Production-Ready**: Telemetry, persistence, receipts, and quality gates from day one
7. **Extensible**: Clear module boundaries for future enhancements

The simulator serves as both an educational tool for learning TCPS concepts and a showcase of how MCP enables AI-assisted learning experiences. By combining hands-on simulation with structured documentation and AI guidance, users can quickly gain practical understanding of Toyota Production System principles applied to code development.

Next steps: Begin Phase 1 implementation with basic simulation coordinator and MCP server setup.
