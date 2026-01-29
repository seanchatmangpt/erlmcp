# Sampling Capability Implementation

## Overview

The sampling capability (`sampling/createMessage`) enables erlmcp servers to generate messages using Large Language Models (LLMs). This implementation follows the Model Context Protocol (MCP) specification for the sampling capability.

**Task #136**: Implement Sampling Capability

## Architecture

### Components

1. **erlmcp_sampling** - Main gen_server managing LLM interactions
2. **erlmcp_mock_llm** - Mock LLM provider for testing and development
3. **Server Integration** - `sampling/createMessage` endpoint in erlmcp_server

### Design Decisions

- **Provider Pattern**: Pluggable LLM provider interface for easy testing
- **gen_server**: Proper OTP supervision and state management
- **Validation**: Input validation for messages and parameters
- **Tracing**: OpenTelemetry integration for observability
- **Mock Provider**: Built-in mock for testing without real LLM calls

## Module Details

### erlmcp_sampling

The main sampling server implementation.

#### State
```erlang
-record(state, {
    model_provider :: module(),           % LLM provider callback module
    default_params :: map(),              % Default model parameters
    request_count = 0 :: non_neg_integer(), % Request counter
    last_request :: integer()            % Timestamp of last request
}).
```

#### API

```erlang
%% Start the sampling server
erlmcp_sampling:start_link() -> {ok, pid()} | {error, term()}.

%% Create a message using the configured LLM provider
erlmcp_sampling:create_message(Messages, Params) -> {ok, Response} | {error, Reason}.

%% Create a message with custom timeout
erlmcp_sampling:create_message(Messages, Params, Timeout) -> {ok, Response} | {error, Reason}.

%% Set the model provider module
erlmcp_sampling:set_model_provider(ServerPid, ProviderModule) -> ok.

%% Get the current model provider
erlmcp_sampling:get_model_provider() -> module().
```

#### Configuration

Add to `sys.config` or `rebar.config`:

```erlang
{erlmcp, [
    {model_provider, erlmcp_mock_llm},
    {default_model, <<"gpt-3.5-turbo">>},
    {default_temperature, 0.7},
    {default_max_tokens, 1000}
]}.
```

### erlmcp_mock_llm

Mock LLM provider for testing.

#### Response Modes

```erlang
%% Echo mode - repeats input
erlmcp_mock_llm:set_response_mode(echo).

%% Template mode - structured responses
erlmcp_mock_llm:set_response_mode(template).

%% Error mode - returns errors
erlmcp_mock_llm:set_response_mode(error).

%% Timeout mode - simulates timeout
erlmcp_mock_llm:set_response_mode(timeout).
```

## Usage Examples

### Basic Usage

```erlang
%% Start the sampling server
{ok, _SamplingPid} = erlmcp_sampling:start_link(),

%% Create a message
Messages = [
    #{<<"role">> => <<"user">>, <<"content">> => <<"Hello, MCP!">>}
],
Params = #{
    <<"temperature">> => 0.7,
    <<"maxTokens">> => 100
},

{ok, Response} = erlmcp_sampling:create_message(Messages, Params).
%% Response: #{<<"role">> => <<"assistant">>, <<"content">> => <<"Echo: Hello, MCP!">>, ...}
```

### With System Prompt

```erlang
Messages = [
    #{<<"role">> => <<"system">>, <<"content">> => <<"You are a helpful assistant.">>},
    #{<<"role">> => <<"user">>, <<"content">> => <<"What is 2+2?">>}
],
{ok, Response} = erlmcp_sampling:create_message(Messages, #{}).
```

### With Model Preferences

```erlang
Messages = [
    #{<<"role">> => <<"user">>, <<"content">> => <<"Explain quantum computing">>}
],
Params = #{
    <<"model">> => <<"gpt-4">>,
    <<"modelPreferences">> => #{
        <<"costPriority">> => 0.5,
        <<"speedPriority">> => 0.7,
        <<"intelligencePriority">> => 0.9
    },
    <<"temperature">> => 0.3,
    <<"maxTokens">> => 500,
    <<"stopSequences">> => [<<"END">>, <<"STOP">>]
},
{ok, Response} = erlmcp_sampling:create_message(Messages, Params).
```

### Conversation History

```erlang
Messages = [
    #{<<"role">> => <<"user">>, <<"content">> => <<"First question">>},
    #{<<"role">> => <<"assistant">>, <<"content">> => <<"First answer">>},
    #{<<"role">> => <<"user">>, <<"content">> => <<"Follow-up question">>}
],
{ok, Response} = erlmcp_sampling:create_message(Messages, #{<<"temperature">> => 0.5}).
```

## Server Integration

The sampling capability is integrated into the MCP server via the `sampling/createMessage` endpoint.

### JSON-RPC Request

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "sampling/createMessage",
  "params": {
    "messages": [
      {"role": "user", "content": "Hello!"}
    ],
    "modelPreferences": {
      "temperature": 0.7,
      "maxTokens": 100
    }
  }
}
```

### JSON-RPC Response

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "role": "assistant",
    "content": "Echo: Hello!",
    "model": "mock-model-echo",
    "stopReason": "end_of_turn",
    "usage": {
      "promptTokens": 2,
      "completionTokens": 3
    }
  }
}
```

## Implementing a Real LLM Provider

To integrate a real LLM provider (e.g., OpenAI, Anthropic), create a module that exports `create_message/2`:

```erlang
-module(my_llm_provider).

-export([create_message/2]).

create_message(Messages, Params) ->
    %% Extract parameters
    Model = maps:get(<<"model">>, Params, <<"gpt-3.5-turbo">>),
    Temperature = maps:get(<<"temperature">>, Params, 0.7),
    MaxTokens = maps:get(<<"maxTokens">>, Params, 1000),

    %% Call LLM API
    case call_llm_api(Messages, Model, Temperature, MaxTokens) of
        {ok, LLMResponse} ->
            %% Format response according to MCP spec
            {ok, #{
                <<"role">> => <<"assistant">>,
                <<"content">> => LLMResponse#{
                    <<"text">> => extract_text(LLMResponse)
                },
                <<"model">> => Model,
                <<"stopReason">> => extract_stop_reason(LLMResponse),
                <<"usage">> => #{
                    <<"promptTokens">> => extract_prompt_tokens(LLMResponse),
                    <<"completionTokens">> => extract_completion_tokens(LLMResponse)
                }
            }};
        {error, Reason} ->
            {error, Reason}
    end.

call_llm_api(Messages, Model, Temperature, MaxTokens) ->
    %% Implement HTTP call to LLM API
    %% Use gun or hackney for HTTP client
    ok.
```

Then configure it:

```erlang
%% In sys.config or at runtime
erlmcp_sampling:set_model_provider(my_llm_provider).
```

## Testing

### Unit Tests

Run the sampling test suite:

```bash
rebar3 eunit --module=erlmcp_sampling_tests
```

### Integration Tests

Test the full server integration:

```erlang
%% Start server with sampling capability
{ok, Server} = erlmcp_server:start_link(
    test_server,
    #mcp_server_capabilities{
        sampling = #mcp_sampling_capability{}
    }
).

%% Send sampling/createMessage request via transport
Request = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,
    <<"method">> => <<"sampling/createMessage">>,
    <<"params">> => #{
        <<"messages">> => [
            #{<<"role">> => <<"user">>, <<"content">> => <<"Test">>}
        ]
    }
}.
```

### Mock Provider Testing

```erlang
%% Test different response modes
erlmcp_mock_llm:set_response_mode(echo),
{ok, EchoResponse} = erlmcp_sampling:create_message(Messages, #{}).

erlmcp_mock_llm:set_response_mode(template),
{ok, TemplateResponse} = erlmcp_sampling:create_message(Messages, #{}).

erlmcp_mock_llm:set_response_mode(error),
{error, mock_provider_error} = erlmcp_sampling:create_message(Messages, #{}).
```

## Validation

The sampling capability validates:

1. **Message Format**
   - Must be a non-empty list
   - Each message must have `role` and `content` fields
   - Role must be a binary string
   - Content must be binary or map (for multimodal)

2. **Model Parameters**
   - Temperature must be between 0.0 and 2.0
   - MaxTokens must be positive integer
   - Stop sequences must be a list of binaries

3. **Model Preferences**
   - Priorities must be numeric (0.0 to 1.0)
   - Follows MCP spec structure

## Error Handling

Common error responses:

```erlang
%% Empty messages
{error, <<"Messages list cannot be empty">>}

%% Invalid format
{error, <<"Invalid message format">>}

%% Invalid temperature
{error, <<"Temperature must be between 0.0 and 2.0">>}

%% Provider error
{error, <<"LLM provider error">>}

%% Timeout
{error, <<"LLM request timeout">>}
```

## Performance Considerations

1. **Timeout**: Default 30 second timeout for LLM calls
2. **Concurrency**: gen_server serializes requests (use poolboy for parallel LLM calls)
3. **Memory**: Large message history can consume memory (consider truncation)
4. **Rate Limiting**: Implement rate limiting for production LLM APIs

## Future Enhancements

1. **Streaming Support**: Implement streaming responses for real-time generation
2. **Caching**: Cache responses for identical message sequences
3. **Batch Processing**: Support multiple message generation in one call
4. **Provider Pool**: Support multiple LLM providers with load balancing
5. **Cost Tracking**: Track token usage and API costs
6. **Fallback**: Implement fallback providers on errors

## Compliance

This implementation follows:

- **MCP Specification**: Model Context Protocol 2025-11-25
- **OTP Patterns**: gen_server behavior, proper supervision
- **Testing**: Chicago School TDD, real process testing
- **Documentation**: Comprehensive API documentation
- **Observability**: OpenTelemetry tracing integration

## References

- [MCP Protocol Specification](https://modelcontextprotocol.io)
- [erlmcp OTP Patterns](docs/otp-patterns.md)
- [erlmcp API Reference](docs/api-reference.md)
- [OpenTelemetry Erlang](https://opentelemetry.io/docs/instrumentation/erlang/)

## Changelog

### v0.6.0 (Task #136)
- Initial implementation of sampling capability
- Mock LLM provider for testing
- Server integration with `sampling/createMessage`
- Comprehensive test suite
- OpenTelemetry tracing support
- Documentation and examples
