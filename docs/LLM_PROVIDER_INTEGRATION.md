# LLM Provider Integration Guide

## Overview

erlmcp now supports **real LLM provider integration** with production-ready HTTP clients for OpenAI, Anthropic Claude, and local LLMs (Ollama, LM Studio).

## Joe Armstrong Principles Applied

- **"The program is the specification"** - Real HTTP calls to LLM APIs (no mocks in production)
- **Async streaming** - Using `gun` HTTP client with async await patterns
- **Let-it-crash** - Process monitors and supervisor recovery
- **No hidden state** - All configuration explicit via gen_server state

## Supported Providers

### 1. OpenAI Provider

**Module**: `erlmcp_llm_provider_openai`

**Models**:
- `gpt-4` - Most capable
- `gpt-4-turbo` - Faster, cheaper GPT-4
- `gpt-3.5-turbo` - Fast, cost-effective

**Configuration**:
```erlang
{erlmcp, [
  {llm_provider, openai},
  {llm_provider_config, #{
    api_key => <<"sk-...">>,  % Or set OPENAI_API_KEY env var
    model => <<"gpt-3.5-turbo">>,
    base_url => <<"https://api.openai.com">>,  % For Azure OpenAI
    timeout => 60000
  }}
]}.
```

**Usage**:
```erlang
{ok, Pid} = erlmcp_llm_provider_openai:start_link(#{
  api_key => <<"sk-...">>,
  model => <<"gpt-4">>
}).

Messages = [
  #{<<"role">> => <<"user">>, <<"content">> => <<"What is Erlang?">>}
].

{ok, Response} = erlmcp_llm_provider_openai:create_message(Messages, #{
  <<"temperature">> => 0.7,
  <<"maxTokens">> => 1000
}).
```

### 2. Anthropic Claude Provider

**Module**: `erlmcp_llm_provider_anthropic`

**Models**:
- `claude-3-opus-20240229` - Most capable
- `claude-3-sonnet-20240229` - Balanced performance
- `claude-3-haiku-20240307` - Fastest, most cost-effective

**Configuration**:
```erlang
{erlmcp, [
  {llm_provider, anthropic},
  {llm_provider_config, #{
    api_key => <<"sk-ant-...">>,  % Or set ANTHROPIC_API_KEY env var
    model => <<"claude-3-sonnet-20240229">>,
    version => <<"2023-06-01">>,
    timeout => 60000
  }}
]}.
```

**Usage**:
```erlang
{ok, Pid} = erlmcp_llm_provider_anthropic:start_link(#{
  api_key => <<"sk-ant-...">>,
  model => <<"claude-3-opus-20240229">>
}).

Messages = [
  #{<<"role">> => <<"system">>, <<"content">> => <<"You are a helpful assistant.">>},
  #{<<"role">> => <<"user">>, <<"content">> => <<"Explain OTP.">>}
].

{ok, Response} = erlmcp_llm_provider_anthropic:create_message(Messages, #{
  <<"maxTokens">> => 2000
}).
```

### 3. Local LLM Provider

**Module**: `erlmcp_llm_provider_local`

**Backends**:
- `ollama` - Ollama (localhost:11434)
- `lm_studio` - LM Studio (localhost:1234)
- `openai_compatible` - Any OpenAI-compatible API

**Configuration**:
```erlang
{erlmcp, [
  {llm_provider, local},
  {llm_provider_config, #{
    backend => ollama,
    base_url => <<"http://localhost:11434">>,
    model => <<"llama2">>,
    timeout => 120000
  }}
]}.
```

**Usage**:
```erlang
{ok, Pid} = erlmcp_llm_provider_local:start_link(#{
  backend => ollama,
  base_url => <<"http://localhost:11434">>,
  model => <<"mistral">>
}).

Messages = [
  #{<<"role">> => <<"user">>, <<"content">> => <<"Hello!">>}
].

{ok, Response} = erlmcp_llm_provider_local:create_message(Messages, #{
  <<"temperature">> => 0.8
}).
```

## Integration with erlmcp_sampling

The `erlmcp_sampling` module automatically loads the configured provider:

```erlang
%% Start sampling server (auto-starts configured provider)
{ok, Pid} = erlmcp_sampling:start_link(),

%% Create message (uses configured provider)
Messages = [
  #{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}
],

{ok, Response} = erlmcp_sampling:create_message(Messages, #{
  <<"model">> => <<"gpt-4">>,
  <<"temperature">> => 0.5
}).
```

**Provider Switching**:
```erlang
%% Switch to OpenAI
ok = erlmcp_sampling:set_model_provider(self(), erlmcp_llm_provider_openai),

%% Switch to Anthropic
ok = erlmcp_sampling:set_model_provider(self(), erlmcp_llm_provider_anthropic),

%% Switch to local
ok = erlmcp_sampling:set_model_provider(self(), erlmcp_llm_provider_local),

%% Switch to mock (for testing)
ok = erlmcp_sampling:set_model_provider(self(), erlmcp_mock_llm).
```

## HTTP Client Implementation

All providers use the `gun` HTTP client with async patterns:

```erlang
%% Open connection
{ok, ConnPid} = gun:open(Host, Port, #{
  transport => tls,  % or tcp
  protocols => [http]
}),

%% Wait for connection up
{up, Protocol} = gun:await_up(ConnPid, Timeout),

%% Make request
StreamRef = gun:post(ConnPid, Path, Headers, Body),

%% Await response
{response, nofin, 200, RespHeaders} = gun:await(ConnPid, StreamRef, Timeout),

%% Get response body
{ok, Body} = gun:await_body(ConnPid, StreamRef, Timeout),

%% Close connection
gun:close(ConnPid).
```

**Monitoring**: All connections use `monitor/2` for cleanup on crashes.

## Error Handling

Providers return structured errors:

```erlang
{error, missing_api_key}        % API key not configured
{error, {http_error, Status, Headers}}  % HTTP error response
{error, {openai_error, Type, Message}}   % OpenAI-specific error
{error, {anthropic_error, Type, Message}} % Anthropic-specific error
{error, json_decode_failed}      % Invalid JSON response
{error, invalid_response_format} % Unexpected response structure
```

## Testing

### Unit Tests (Chicago School TDD)

Tests use **real processes**, not mocks:

```bash
# Run provider tests
rebar3 eunit --module=erlmcp_llm_provider_tests

# Run sampling integration tests
rebar3 eunit --module=erlmcp_sampling_provider_tests
```

**Test Coverage**:
- Provider lifecycle (start/stop)
- Configuration management
- API key handling (environment + explicit)
- Message format validation
- Error handling (missing API keys, network failures)
- Provider switching

### Mock Server Testing (Cowboy)

For HTTP-level testing, use Cowboy mock servers:

```erlang
%% Start mock server
Dispatch = cowboy_router:compile([
  {'_', [
    {"/v1/chat/completions", openai_mock_handler, []}
  ]}
]),
{ok, _} = cowboy:start_clear(mock_openai, [{port, 18080}], #{
  env => #{dispatch => Dispatch}
}),
```

See `erlmcp_llm_openai_tests.erl` for complete example.

## Production Deployment

### Environment Variables

```bash
# OpenAI
export OPENAI_API_KEY="sk-..."

# Anthropic
export ANTHROPIC_API_KEY="sk-ant-..."
```

### sys.config

```erlang
{erlmcp, [
  %% Provider selection
  {llm_provider, openai},  % openai | anthropic | local | mock

  %% Provider configuration
  {llm_provider_config, #{
    api_key => <<"sk-...">>,
    model => <<"gpt-4">>,
    base_url => <<"https://api.openai.com">>,
    timeout => 60000
  }},

  %% Default sampling parameters
  {default_model, <<"gpt-3.5-turbo">>},
  {default_temperature, 0.7},
  {default_max_tokens, 1000}
]}.
```

### Supervision Tree

Providers are registered processes and can be added to supervision tree:

```erlang
%% In erlmcp_core_sup
{
  erlmcp_llm_provider_openai,
  {erlmcp_llm_provider_openai, start_link, [[]]},
  permanent, 5000, worker, [erlmcp_llm_provider_openai]
}
```

## Performance

**Baselines** (via `erlmcp_bench_network_real`):
- OpenAI API: ~100ms latency (US-East)
- Anthropic API: ~150ms latency (US-East)
- Local Ollama: ~50ms latency (localhost)

**Recommendations**:
- Use connection pooling for high-throughput scenarios
- Set appropriate timeouts (60s for cloud, 120s for local)
- Monitor API rate limits and implement backoff

## Troubleshooting

### Missing API Key
```
{error, missing_api_key}
```
**Solution**: Set environment variable or pass in config.

### Connection Timeout
```
{error, {http_error, timeout}}
```
**Solution**: Check network connectivity, increase timeout.

### Invalid Response
```
{error, json_decode_failed}
```
**Solution**: API may be down or returning errors, check logs.

### Module Not Loaded
```
{error, invalid_provider}
```
**Solution**: Ensure provider module is in code path.

## Migration from Mock

**Before** (fake responses):
```erlang
{erlmcp, [
  {model_provider, erlmcp_mock_llm}
]}.
```

**After** (real OpenAI):
```erlang
{erlmcp, [
  {llm_provider, openai},
  {llm_provider_config, #{
    api_key => <<"sk-...">>
  }}
]}.
```

## References

- [OpenAI API Docs](https://platform.openai.com/docs/api-reference)
- [Anthropic API Docs](https://docs.anthropic.com/claude/reference)
- [Ollama Docs](https://github.com/ollama/ollama)
- [gun HTTP Client](https://ninenines.eu/docs/en/gun/2.0/guide/introduction.html)
