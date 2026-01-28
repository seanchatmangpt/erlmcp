# erlmcp_observability - Real-Time Metrics Dashboard

**Real-time metrics dashboard with WebSocket streaming, Chart.js visualizations, and CSV/JSON export.**

## Features

### Core Capabilities
- **Real-time WebSocket streaming** - Metrics updates every 1 second
- **Rich visualizations** - Chart.js line, bar, and multi-axis charts
- **Time-series aggregation** - 60s, 5min, 1hr buckets with 24-hour retention
- **Percentile calculations** - P50, P95, P99, P999 for latency distributions
- **Alert system** - Threshold-based alerts for latency, errors, memory
- **Export functionality** - CSV and JSON export of historical metrics
- **Dark mode** - Toggle between light and dark themes
- **REST API** - HTTP endpoints for metrics queries

## Quick Start

```erlang
% Start observability (includes dashboard)
application:ensure_all_started(erlmcp_observability).

% Dashboard available at: http://localhost:9090
```

## REST API Endpoints

- `GET /api/metrics` - Current metrics
- `GET /api/metrics/historical?start=X&end=Y` - Historical data
- `GET /api/metrics/export?format=csv|json` - Export metrics

## WebSocket Protocol

```javascript
const ws = new WebSocket('ws://localhost:9090/ws');

ws.onmessage = (event) => {
    const data = JSON.parse(event.data);
    // data.type: 'connected' | 'metrics' | 'pong'
};
```

See full documentation in apps/erlmcp_observability/DASHBOARD.md
