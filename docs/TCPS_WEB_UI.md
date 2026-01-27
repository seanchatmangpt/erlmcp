# TCPS MCP Diataxis Simulator - Web UI Documentation

**Version:** 1.0.0
**Technology Stack:** Erlang (Cowboy), HTML5, CSS3, Vanilla JavaScript
**Architecture:** Production-ready, no external dependencies

---

## 🎯 Overview

The TCPS MCP Diataxis Simulator is an interactive web-based interface for learning, exploring, and testing the Toyota Code Production System (TCPS) with MCP (Model Context Protocol) integration. The simulator implements the Diataxis documentation framework to provide comprehensive learning paths.

### Key Features

1. **Diataxis Framework Navigation** - Interactive 4-quadrant layout (Tutorial, How-To, Explanation, Reference)
2. **TCPS Workflow Simulator** - Visual Kanban board with drag-and-drop work items
3. **Real-time Metrics Dashboard** - Live charts and visualization of TCPS metrics
4. **Quality Gates Monitoring** - Automatic validation and quality enforcement
5. **Andon Alert System** - Visual and interactive defect management
6. **MCP Tool Playground** - Test and execute MCP tools interactively
7. **WebSocket Real-time Updates** - Live synchronization across all clients

---

## 📁 Architecture

### Directory Structure

```
erlmcp/
├── src/tcps_mcp_diataxis/
│   ├── tcps_web_server.erl        # Main HTTP server (Cowboy)
│   ├── tcps_websocket_handler.erl # WebSocket bidirectional communication
│   └── tcps_api_handler.erl       # REST API endpoints
│
├── priv/static/
│   ├── index.html                 # Main UI page
│   ├── css/
│   │   └── simulator.css          # Dark theme styling
│   └── js/
│       ├── simulator.js           # Core simulator logic
│       ├── diataxis.js            # Diataxis navigation
│       └── visualizations.js      # Charts and visualizations
│
└── docs/
    └── TCPS_WEB_UI.md             # This documentation
```

### Technology Stack

| Layer | Technology | Purpose |
|-------|------------|---------|
| **Backend** | Erlang/OTP 25+ | Robust, concurrent server |
| **HTTP Server** | Cowboy 2.10.0 | High-performance HTTP/WebSocket |
| **Frontend** | Vanilla JavaScript | Zero dependencies, pure ES6+ |
| **Styling** | CSS3 Variables | Dark theme, responsive design |
| **Protocol** | WebSocket + REST | Real-time bidirectional communication |
| **Data Format** | JSON | API requests/responses |

---

## 🚀 Getting Started

### 1. Start the Web Server

#### Option A: From Erlang Shell

```erlang
%% Start the application
application:ensure_all_started(erlmcp).

%% Start the web server on port 8088
Config = #{port => 8088, host => "0.0.0.0"}.
{ok, Pid} = tcps_web_server:start_link(Config).
```

#### Option B: From Command Line

```bash
# Compile the application
rebar3 compile

# Start with web server
erl -pa _build/default/lib/*/ebin -s erlmcp -eval "tcps_web_server:start_link(#{port => 8088})."
```

### 2. Access the Simulator

Open your browser and navigate to:

```
http://localhost:8088
```

### 3. Verify Connection

- Check the header for **green status dot** indicating WebSocket connected
- You should see "Connected" in the top-right corner
- Console should show: `WebSocket connected`

---

## 📚 Diataxis Framework

The simulator implements the **Diataxis documentation framework** with four quadrants:

### 1. Tutorial Quadrant (Learning + Doing)

**Purpose:** Step-by-step guided learning experience

**Features:**
- Interactive tutorials with validation
- Progress tracking (steps completed)
- Auto-validation of actions
- Real-time feedback on completion

**Example Tutorial:**
1. Create a Work Item
2. Move Item Through Kanban
3. Observe Quality Gates
4. Handle Andon Alert

**Usage:**
```javascript
// Click Tutorial quadrant
// Follow step-by-step instructions
// Actions are automatically validated
// Progress bar shows completion
```

### 2. How-To Guide Quadrant (Problem + Doing)

**Purpose:** Task-oriented guides for specific problems

**Topics:**
- Setting WIP Limits
- Defining Quality Gates
- Configuring Andon Alert Rules
- MCP Tool Integration

**Example:**
```
How-To: Configure TCPS for Your Project
1. Set WIP limits for each Kanban bucket
2. Define quality gate thresholds
3. Configure Andon trigger conditions
4. Connect MCP tools to workflow
```

### 3. Explanation Quadrant (Learning + Thinking)

**Purpose:** Understanding Toyota principles in code

**Topics:**
- Just-In-Time Development
- Jidoka (Autonomation)
- Andon Cord Philosophy
- Kaizen (Continuous Improvement)
- Visual Management (Kanban)

### 4. Reference Quadrant (Problem + Thinking)

**Purpose:** Technical reference and API documentation

**Contents:**
- Kanban bucket states and WIP limits
- Quality gate metrics and thresholds
- Andon severity levels
- MCP tool schema
- REST API endpoints

---

## 🎮 TCPS Simulator Features

### Kanban Board

**Visual Workflow Management:**

```
┌──────────┬──────────┬──────────┬──────────┬──────────┐
│ Backlog  │  Ready   │In Progress│  Review  │  Done    │
│ (5/10)   │  (3/5)   │  (2/3)   │  (1/2)   │  (15/∞)  │
├──────────┼──────────┼──────────┼──────────┼──────────┤
│ [Item 1] │ [Item 3] │ [Item 4] │          │ [Item 5] │
│ [Item 2] │          │ [Item 6] │          │ [Item 7] │
│          │          │          │          │ [Item 8] │
└──────────┴──────────┴──────────┴──────────┴──────────┘
```

**Features:**
- **Drag-and-Drop:** Move items between buckets
- **WIP Limits:** Visual indicators when limits exceeded
- **Real-time Sync:** Changes broadcast to all clients
- **Add Items:** Click "+ Add Item" in any bucket

**API:**
```javascript
// Create work item via WebSocket
window.TCPSSimulator.sendWebSocketMessage('create_work_item', {
    title: 'New Feature',
    description: 'Implement authentication',
    priority: 'high',
    bucket: 'backlog'
});

// Move work item
window.TCPSSimulator.sendWebSocketMessage('move_work_item', {
    item_id: 'work-1234',
    to_bucket: 'in_progress'
});
```

### Quality Gates

**Automated Quality Enforcement:**

| Gate | Target | Current | Status |
|------|--------|---------|--------|
| Test Pass Rate | ≥80% | 92% | ✅ PASS |
| Code Coverage | ≥80% | 87% | ✅ PASS |
| Defect Rate | ≤1% | 1.5% | ⚠️ WARNING |
| First Pass Yield | ≥95% | 96% | ✅ PASS |

**Status Indicators:**
- 🟢 **PASS** - Meets or exceeds target
- 🟡 **WARNING** - Close to threshold
- 🔴 **FAIL** - Below acceptable level

**Visual Feedback:**
- Color-coded left border
- Real-time metric updates
- Trend indicators (↑ ↓ →)

### Andon Alerts

**Visual Defect Management:**

```
⚠️ Andon Alerts
┌────────────────────────────────────────┐
│ 🔴 CRITICAL                            │
│ Test failure in authentication module  │
│ ID: andon-001 | Elapsed: 1h 20m        │
├────────────────────────────────────────┤
│ 🟡 WARNING                             │
│ WIP limit exceeded in review bucket    │
│ ID: andon-002 | Elapsed: 30m           │
└────────────────────────────────────────┘
```

**Features:**
- **Severity Levels:** Critical, Warning, Info
- **Auto-trigger:** Quality gate failures
- **Manual Trigger:** Test button for simulation
- **Resolution Tracking:** Time to resolution

**Trigger Andon:**
```javascript
window.TCPSSimulator.sendWebSocketMessage('trigger_andon', {
    severity: 'critical',
    title: 'Build failure',
    description: 'Production build failed on CI/CD',
    affected_items: ['work-1234']
});
```

### Metrics Dashboard

**Real-time Visualization:**

**Metric Cards:**
- **Throughput:** Items completed per day
- **Lead Time:** Average hours from start to done
- **Quality Score:** Overall pass rate percentage
- **WIP Utilization:** Current WIP vs. capacity

**Charts:**
- **Sparklines:** Mini trend charts in each card
- **Flow Visualization:** Animated particle flow through stages
- **Auto-refresh:** Updates every 10 seconds

**API:**
```javascript
// Update dashboard metrics
window.VisualizationsModule.updateSparkline('throughput-chart', [3.0, 3.2, 3.5, 3.8, 4.0]);
window.VisualizationsModule.updateMetricValue('throughput-value', 4.0);
```

### MCP Tool Playground

**Interactive Tool Testing:**

**Features:**
1. **Tool List:** Browse available MCP tools
2. **Schema Display:** View input/output schemas
3. **JSON Editor:** Edit parameters with syntax
4. **Execute:** Run tools and see results
5. **Output Display:** Formatted JSON response

**Available Tools:**
- `tcps.work_order.create` - Create work orders
- `tcps.quality_gate.check` - Run quality validation
- `tcps.andon.trigger` - Trigger alerts
- `tcps.metrics.get` - Get current metrics

**Example:**
```javascript
// Execute MCP tool
const params = {
    title: 'New Feature',
    priority: 'high'
};

const result = await apiPost('/mcp/tools/tcps.work_order.create/execute', params);
console.log(result);
```

---

## 🔌 API Reference

### REST API Endpoints

#### Diataxis Content

```
GET /api/diataxis/quadrants
Returns: { quadrants: [...] }

GET /api/diataxis/content/:quadrant
Params: quadrant = "tutorial" | "howto" | "explanation" | "reference"
Returns: { quadrant: string, content: {...} }
```

#### Simulator Status

```
GET /api/simulator/status
Returns: {
    status: "running",
    uptime_seconds: 3600,
    mode: "interactive",
    active_workflows: 5,
    ws_connections: 2
}
```

#### Kanban Board

```
GET /api/simulator/kanban
Returns: {
    buckets: [
        {
            name: "backlog",
            limit: 10,
            items: [...]
        },
        ...
    ],
    wip_status: {
        total_wip: 6,
        total_capacity: 20,
        utilization: 0.30
    }
}
```

#### Metrics

```
GET /api/simulator/metrics
Returns: {
    throughput: { items_per_day: 3.2, trend: "increasing", ... },
    lead_time: { average_hours: 42, trend: "decreasing", ... },
    quality: { pass_rate: 0.92, coverage: 0.87, ... },
    wip: { current: 6, limit: 15, percentage: 0.40 }
}
```

#### Quality Gates

```
GET /api/simulator/quality-gates
Returns: {
    quality_gates: [
        {
            name: "Test Pass Rate",
            current: 0.92,
            target: 0.80,
            status: "pass",
            trend: "stable"
        },
        ...
    ]
}
```

#### Andon Alerts

```
GET /api/simulator/andons
Returns: {
    andons: [
        {
            id: "andon-001",
            severity: "critical",
            title: "Test failure",
            description: "...",
            triggered_at: 1706275200000,
            status: "active"
        },
        ...
    ]
}
```

#### MCP Tools

```
GET /api/mcp/tools
Returns: {
    tools: [
        {
            name: "tcps.work_order.create",
            description: "Create a new TCPS work order",
            inputSchema: {...}
        },
        ...
    ]
}

POST /api/mcp/tools/:tool/execute
Body: { ...params }
Returns: { result: {...} }
```

#### Health Check

```
GET /api/health
Returns: {
    status: "ok",
    timestamp: 1706275200000,
    server: "TCPS Diataxis Simulator",
    version: "1.0.0"
}
```

### WebSocket Protocol

**Connection URL:**
```
ws://localhost:8088/ws
```

**Message Format:**
```json
{
    "type": "command_type",
    "data": { ...payload },
    "timestamp": 1706275200000
}
```

**Supported Commands:**

#### Client → Server

```javascript
// Ping/Pong
{ type: "ping" }

// Subscribe to channels
{ type: "subscribe", channels: ["metrics", "andons", "workflow"] }

// Get metrics
{ type: "get_metrics" }

// Get simulator status
{ type: "get_simulator_status" }

// Create work item
{
    type: "create_work_item",
    data: {
        title: "New Feature",
        description: "...",
        priority: "high",
        bucket: "backlog"
    }
}

// Move work item
{
    type: "move_work_item",
    data: {
        item_id: "work-1234",
        to_bucket: "in_progress"
    }
}

// Trigger Andon
{
    type: "trigger_andon",
    data: {
        severity: "critical",
        title: "Build failure",
        description: "..."
    }
}

// Execute MCP tool
{
    type: "execute_mcp_tool",
    data: {
        tool: "tcps.work_order.create",
        params: {...}
    }
}

// Tutorial commands
{ type: "start_tutorial", data: { tutorial_id: "basic" } }
{ type: "complete_tutorial_step", data: { step_id: "step1" } }
```

#### Server → Client

```javascript
// Connection confirmed
{
    type: "connected",
    client_id: "client-1234",
    server_version: "1.0.0",
    timestamp: 1706275200000,
    simulator_status: {...}
}

// Pong response
{ type: "pong", timestamp: 1706275200000 }

// Metrics update
{
    type: "metrics",
    data: {...metrics},
    timestamp: 1706275200000
}

// Work item created
{
    type: "work_item_created",
    result: {
        success: true,
        work_item_id: "work-1234",
        work_item: {...}
    },
    timestamp: 1706275200000
}

// Andon triggered
{
    type: "andon_triggered",
    result: {
        success: true,
        andon_id: "andon-001",
        event: {...}
    },
    timestamp: 1706275200000
}

// Error
{
    type: "error",
    error: "Error message",
    timestamp: 1706275200000
}
```

---

## 🎨 UI Components

### Color Scheme (Dark Theme)

```css
/* Background Colors */
--bg-primary: #0d1117;    /* Main background */
--bg-secondary: #161b22;  /* Cards, panels */
--bg-tertiary: #21262d;   /* Nested elements */
--bg-hover: #30363d;      /* Hover states */

/* Text Colors */
--text-primary: #c9d1d9;   /* Main text */
--text-secondary: #8b949e; /* Secondary text */
--text-tertiary: #6e7681;  /* Tertiary text */

/* Brand Colors */
--brand-primary: #58a6ff;  /* Primary brand */
--brand-secondary: #1f6feb;/* Secondary brand */

/* Status Colors */
--status-success: #3fb950; /* Success/pass */
--status-warning: #d29922; /* Warning */
--status-error: #f85149;   /* Error/fail */
--status-info: #58a6ff;    /* Info */
```

### Responsive Breakpoints

```css
/* Desktop */
@media (min-width: 1024px) {
    /* Full 4-quadrant grid */
    /* Sidebar layouts */
}

/* Tablet */
@media (max-width: 1024px) {
    /* Single column Diataxis */
    /* Stacked Kanban columns */
}

/* Mobile */
@media (max-width: 768px) {
    /* Mobile-optimized layouts */
    /* Bottom navigation */
}
```

---

## 🧪 Testing & Validation

### Manual Testing Checklist

**Connection:**
- [ ] WebSocket connects on page load
- [ ] Status indicator turns green
- [ ] Reconnects after disconnect

**Diataxis Navigation:**
- [ ] All 4 quadrants clickable
- [ ] Content panel opens on right
- [ ] Content loads correctly
- [ ] Close button works

**Kanban Board:**
- [ ] Items display correctly
- [ ] Drag-and-drop functional
- [ ] WIP limits update
- [ ] Add item modal opens

**Quality Gates:**
- [ ] Gates display with status
- [ ] Color coding correct
- [ ] Metrics update real-time

**Andon Alerts:**
- [ ] Alerts display correctly
- [ ] Trigger button works
- [ ] Severity color-coded

**Metrics Dashboard:**
- [ ] Cards show values
- [ ] Sparklines render
- [ ] Flow visualization animates

**MCP Playground:**
- [ ] Tools list loads
- [ ] Tool selection works
- [ ] JSON editor functional
- [ ] Execute button works
- [ ] Output displays

### Automated Testing

```bash
# Run backend tests
rebar3 eunit

# Check for dialyzer errors
rebar3 dialyzer

# Format check
rebar3 format --verify
```

---

## 🐛 Troubleshooting

### WebSocket Connection Issues

**Problem:** "Disconnected" status, red dot

**Solutions:**
1. Check server is running: `http://localhost:8088/api/health`
2. Verify port 8088 is not blocked by firewall
3. Check browser console for errors
4. Try different browser

### Kanban Items Not Moving

**Problem:** Drag-and-drop not working

**Solutions:**
1. Ensure WebSocket connected (green dot)
2. Check browser supports drag API
3. Verify draggable attribute on items
4. Check console for JavaScript errors

### Charts Not Rendering

**Problem:** Blank canvases, no visualizations

**Solutions:**
1. Check canvas element exists in DOM
2. Verify canvas size is non-zero
3. Check device pixel ratio compatibility
4. Look for JavaScript errors in console

### API Calls Failing

**Problem:** 404 or 500 errors on API calls

**Solutions:**
1. Verify server running: `http://localhost:8088/api/health`
2. Check endpoint URL formatting
3. Verify request method (GET/POST)
4. Check server logs for errors

---

## 🚀 Production Deployment

### Build for Production

```bash
# Compile with optimizations
rebar3 as prod compile

# Create release
rebar3 as prod release

# Package release
rebar3 as prod tar
```

### Configuration

```erlang
%% config/sys.config
[
    {erlmcp, [
        {web_server, [
            {port, 8088},
            {host, "0.0.0.0"},
            {max_connections, 1000},
            {idle_timeout, 60000}
        ]}
    ]}
].
```

### Nginx Reverse Proxy

```nginx
server {
    listen 80;
    server_name tcps-simulator.example.com;

    location / {
        proxy_pass http://localhost:8088;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

### Performance Tuning

```erlang
%% vm.args
+K true                  %% Enable kernel poll
+A 32                    %% Async threads
+SDio 32                 %% Dirty IO schedulers
+stbt db                 %% Scheduler bind type
+swt very_low            %% Scheduler wakeup threshold
```

---

## 📈 Performance Metrics

**Target Performance:**
- **Page Load:** < 2 seconds
- **WebSocket Connection:** < 500ms
- **API Response:** < 100ms (p95)
- **Chart Rendering:** 60 FPS
- **Memory Usage:** < 50MB per client
- **Concurrent Users:** 1000+

**Monitoring:**
```erlang
%% Get server info
tcps_web_server:get_server_info().

%% Check WebSocket connections
ets:info(tcps_ws_connections, size).

%% Monitor memory
erlang:memory().
```

---

## 🤝 Contributing

### Code Style

- **Erlang:** Follow OTP design principles
- **JavaScript:** ES6+, no semicolons
- **CSS:** BEM-like naming convention
- **Comments:** JSDoc for functions

### Pull Request Process

1. Create feature branch
2. Write tests
3. Update documentation
4. Run `rebar3 format`
5. Submit PR with description

---

## 📝 License

MIT License - See LICENSE file for details

---

## 🔗 Resources

- [Diataxis Framework](https://diataxis.fr/)
- [Cowboy Documentation](https://ninenines.eu/docs/en/cowboy/2.10/guide/)
- [MCP Protocol](https://modelcontextprotocol.io/)
- [TCPS Documentation](./TCPS-IMPLEMENTATION-COMPLETE.md)

---

## 📧 Support

For issues, questions, or contributions:
- GitHub Issues: https://github.com/yourorg/erlmcp/issues
- Documentation: https://docs.example.com/tcps

---

**Built with ❤️ using Erlang, Cowboy, and Vanilla JavaScript**
