# erlmcp Integration Test Suite

End-to-end testing for the erlmcp Model Context Protocol SDK.

## Overview

This integration test suite validates the complete functionality of erlmcp:

- **Resource Access**: `resources/list`, `resources/read`, `resources/templates/list`
- **Tool Invocation**: `tools/list`, `tools/call` with argument validation
- **Prompt Templates**: `prompts/list`, `prompts/get`, template rendering
- **Subscriptions**: `resources/subscribe`, `resources/unsubscribe`, change notifications

## Architecture

```
┌─────────────────────┐
│  Test Runner        │
│  (erlmcp-integration)│
├─────────────────────┤
│                     │
│  ┌───────────────┐  │     ┌─────────────┐
│  │ Common Test   │◄─┼────►│ PostgreSQL  │
│  │ Suites        │  │     │ (sessions)  │
│  └───────────────┘  │     └─────────────┘
│                     │
│  ┌───────────────┐  │     ┌─────────────┐
│  │ Mock MCP      │◄─┼────►│ Redis       │
│  │ Server        │  │     │ (cache/pub) │
│  └───────────────┘  │     └─────────────┘
└─────────────────────┘
```

## Prerequisites

- Docker Engine 20.10+
- Docker Compose v2
- OTP 28+ (for local development)

## Quick Start

### Run via Docker (Recommended)

```bash
# Run full integration tests with PostgreSQL and Redis
./scripts/integration/run-integration-tests.sh

# Run quick tests (no external services)
./scripts/integration/run-integration-tests.sh --quick

# Run with coverage report
./scripts/integration/run-integration-tests.sh --coverage

# Keep containers running for inspection
./scripts/integration/run-integration-tests.sh --keep
```

### Run via Docker Compose Directly

```bash
# Start services
docker compose -f test/integration/docker-compose.test.yml up -d

# Run tests
docker compose -f test/integration/docker-compose.test.yml run --rm erlmcp-integration

# Stop services
docker compose -f test/integration/docker-compose.test.yml down -v
```

## Test Cases

### Resource Access Tests

| Test | Description | Validates |
|------|-------------|-----------|
| `t_resource_list_success` | List all resources | Response structure, URI validation |
| `t_resource_read_success` | Read a specific resource | Content retrieval |
| `t_resource_read_not_found` | Request non-existent resource | Error handling |
| `t_resource_templates_list` | List resource templates | Template support |

### Tool Invocation Tests

| Test | Description | Validates |
|------|-------------|-----------|
| `t_tools_list_success` | List available tools | Tool discovery |
| `t_tool_call_echo_success` | Invoke echo tool | Basic tool execution |
| `t_tool_call_calculate_success` | Invoke calculate tool | Complex tool execution |
| `t_tool_call_invalid_arguments` | Missing required args | Argument validation |
| `t_tool_call_tool_not_found` | Non-existent tool | Error handling |

### Prompt Template Tests

| Test | Description | Validates |
|------|-------------|-----------|
| `t_prompts_list_success` | List all prompts | Prompt discovery |
| `t_prompt_get_success` | Get specific prompt | Prompt retrieval |
| `t_prompt_render_simple` | Render simple template | Template rendering |
| `t_prompt_render_with_arguments` | Multiple arguments | Variable substitution |
| `t_prompt_render_missing_required` | Missing variable | Partial rendering |
| `t_prompt_not_found` | Non-existent prompt | Error handling |

### Subscription Tests

| Test | Description | Validates |
|------|-------------|-----------|
| `t_resource_subscribe_success` | Subscribe to resource | Subscription creation |
| `t_resource_unsubscribe_success` | Unsubscribe from resource | Subscription removal |
| `t_resource_notification_on_change` | Receive change notification | Pub/Sub notification |
| `t_resource_multiple_subscribers` | Multiple subscribers | Broadcast support |
| `t_subscription_rate_limiting` | Rate limit notifications | Throttling |

### Error Handling Tests

| Test | Description | Validates |
|------|-------------|-----------|
| `t_invalid_json_request` | Invalid JSON | Parse error response |
| `t_missing_method_field` | No method in request | Invalid request response |
| `t_unsupported_method` | Unknown method | Method not found response |

### Concurrency Tests

| Test | Description | Validates |
|------|-------------|-----------|
| `t_concurrent_tool_invocations` | 10 parallel tool calls | Concurrent execution |
| `t_concurrent_resource_access` | 20 parallel reads | Concurrent reads |

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `TEST_DB_PORT` | 5433 | PostgreSQL port |
| `TEST_REDIS_PORT` | 6380 | Redis port |
| `TEST_DB_NAME` | erlmcp_test | Database name |
| `TEST_DB_USER` | erlmcp_test | Database user |
| `TEST_DB_PASSWORD` | erlmcp_test_password | Database password |

### Services

#### PostgreSQL
- Image: `postgres:16-alpine`
- Port: `5433` (host), `5432` (container)
- Database: `erlmcp_test`
- Schema: See `test/integration/init-db.sql`

#### Redis
- Image: `redis:7-alpine`
- Port: `6380` (host), `6379` (container)
- Persistence: AOF enabled

## Coverage

Generate coverage reports:

```bash
./scripts/integration/run-integration-tests.sh --coverage
```

Coverage reports are generated in:
- `_build/test/cover/index.html` - HTML report
- `log/ct/` - Test logs

## Troubleshooting

### Services Not Starting

```bash
# Check service status
docker compose -f test/integration/docker-compose.test.yml ps

# View service logs
docker compose -f test/integration/docker-compose.test.yml logs postgres
docker compose -f test/integration/docker-compose.test.yml logs redis
```

### Port Conflicts

```bash
# Change ports via environment variables
TEST_DB_PORT=5434 TEST_REDIS_PORT=6381 \
  ./scripts/integration/run-integration-tests.sh
```

### Container Cleanup

```bash
# Remove all containers and volumes
docker compose -f test/integration/docker-compose.test.yml down -v

# Force remove all integration test containers
docker rm -f $(docker ps -aq --filter "name=erlmcp")
```

## Adding New Tests

1. Create a new test function in `erlmcp_integration_SUITE.erl`:

```erlang
t_my_new_test(_Config) ->
    %% Arrange
    Request = build_request(<<"my/method">>, #{}),

    %% Act
    Response = handle_mcp_request(Request),

    %% Assert
    ?assertMatch(#{<<"result">> := _}, Response),
    ok.
```

2. Add the test name to the `all()` function:

```erlang
all() ->
    [
        %% ... existing tests ...
        t_my_new_test
    ].
```

3. Run the tests:

```bash
./scripts/integration/run-integration-tests.sh
```

## Continuous Integration

The integration tests run in CI via:

```yaml
# .github/workflows/ci.yml
- name: Run integration tests
  run: ./scripts/integration/run-integration-tests.sh --coverage
```

## License

Apache-2.0
