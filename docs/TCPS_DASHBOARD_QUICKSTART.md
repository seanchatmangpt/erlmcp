# TCPS Dashboard - Quick Start Guide

## 5-Minute Setup

### 1. Install Dependencies

```bash
cd /path/to/erlmcp
rebar3 deps
```

### 2. Start the Dashboard

```erlang
%% Start Erlang shell
rebar3 shell

%% In the Erlang shell:
1> application:ensure_all_started(cowboy).
2> {ok, _} = tcps_dashboard:start_dashboard(8080).
```

### 3. Access the Dashboard

Open your browser to: **http://localhost:8080/dashboard**

You should see the TCPS Dashboard with:
- Overview panel with metrics
- Quality gates status
- Kanban board
- Andon alerts
- Kaizen improvement tracking
- Production flow visualization

## Testing the Dashboard

### View Metrics via API

```bash
# Get overall summary
curl http://localhost:8080/api/metrics/summary

# Get Kanban board status
curl http://localhost:8080/api/metrics/kanban

# Get quality gates
curl http://localhost:8080/api/metrics/quality

# Check system health
curl http://localhost:8080/api/health
```

### Create a Work Order

```bash
curl -X POST http://localhost:8080/api/work-orders \
  -H "Content-Type: application/json" \
  -d '{
    "sku_id": "SKU-2024-001",
    "priority": "high"
  }'
```

### Subscribe to Real-Time Updates

Open browser console (F12) and paste:

```javascript
const events = new EventSource('/api/stream');

events.addEventListener('connected', (e) => {
    console.log('Connected to TCPS Dashboard:', e.data);
});

events.addEventListener('metrics-update', (e) => {
    const data = JSON.parse(e.data);
    console.log('Metrics updated:', data);
});

events.addEventListener('andon-triggered', (e) => {
    const andon = JSON.parse(e.data);
    console.log('Andon alert:', andon);
});
```

## Common Tasks

### Toggle Dark Mode

Click the moon/sun icon in the top-right corner.

### Export Data

1. Click "Export Data" button
2. Choose format: JSON, CSV, or Weekly Report
3. File downloads automatically

### Resolve an Andon

1. Find the Andon in the "Andon Alerts" panel
2. Click "Resolve" button
3. Enter resolution notes
4. Click OK

The Andon will be removed from the active list.

## Customization

### Change Port

```erlang
tcps_dashboard:start_dashboard(9090).
```

### Configure Refresh Interval

Edit `config/dashboard.config`:

```erlang
{tcps_dashboard, [
    {refresh_interval, 3000}  %% 3 seconds instead of 5
]}.
```

Restart the dashboard for changes to take effect.

### Set WIP Limits

Edit `config/dashboard.config`:

```erlang
{wip_limits, #{
    ready => 10,        %% Increase from 5
    in_progress => 5    %% Increase from 3
}}.
```

## Integration with TCPS Kanban

The dashboard integrates with `tcps_kanban` module:

```erlang
%% Get current WIP from Kanban
{ok, WorkItems} = tcps_kanban:get_work_items_by_bucket(in_progress).

%% Add work item
WorkItem = #{
    sku_id => <<"SKU-2024-001">>,
    priority => high,
    created_at => erlang:timestamp()
},
ok = tcps_kanban:add_work_item(in_progress, WorkItem).

%% Dashboard will auto-update via SSE
```

## Troubleshooting

### Dashboard Won't Start

**Error**: `{error, eaddrinuse}`

**Solution**: Port 8080 is already in use. Try a different port:

```erlang
tcps_dashboard:start_dashboard(8888).
```

### No Data Showing

**Problem**: All metrics show 0 or "No data"

**Solution**: Dashboard is showing mock data by default. Integrate with actual TCPS modules:

```erlang
%% Start TCPS Kanban
tcps_kanban:start_link().

%% Add some work items
tcps_kanban:add_work_item(backlog, #{sku_id => <<"SKU-001">>}).
```

### SSE Connection Fails

**Problem**: "Disconnected" status in header

**Solution**: Check browser console for errors. Try:

1. Disable browser extensions
2. Check firewall settings
3. Verify port is accessible: `curl http://localhost:8080/api/stream`

## Next Steps

- Read [full documentation](TCPS_DASHBOARD.md)
- Explore [API reference](TCPS_DASHBOARD.md#rest-api-endpoints)
- Check [configuration options](TCPS_DASHBOARD.md#configuration-reference)
- Review [test suite](../tests/tcps/tcps_dashboard_tests.erl)

## Production Deployment

For production use:

1. Enable authentication:
   ```erlang
   {enable_auth, true},
   {auth_username, <<"admin">>},
   {auth_password, <<"secure_password_here">>}
   ```

2. Use HTTPS (configure Cowboy with TLS):
   ```erlang
   {ssl, [
       {certfile, "/path/to/cert.pem"},
       {keyfile, "/path/to/key.pem"}
   ]}
   ```

3. Set production logging:
   ```erlang
   {logging, #{
       level => warning,
       log_http_requests => false
   }}
   ```

4. Configure CORS for specific domains:
   ```erlang
   {cors_allowed_origins, [
       <<"https://tcps.yourcompany.com">>
   ]}
   ```

---

**Need Help?**
- GitHub Issues: https://github.com/your-org/erlmcp/issues
- Documentation: [TCPS_DASHBOARD.md](TCPS_DASHBOARD.md)
