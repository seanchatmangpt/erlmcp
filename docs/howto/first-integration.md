# First Integration: Building Your First MCP Connection

This guide walks you through creating your first MCP integration with erlmcp. We'll build a simple calculator service and connect it to an AI system.

## What You'll Build

A complete integration that:
1. **Registers a custom calculator tool** with erlmcp
2. **Handles MCP requests** from an AI client
3. **Returns calculated results** in MCP format
4. **Demonstrates error handling** for invalid inputs

## Step 1: Set Up Your Project

### Create Project Structure
```bash
mkdir -p first-integration/{src,config,test}
cd first-integration
```

### Initialize rebar3 Project
```bash
rebar3 new app appid=my_calculator
```

### Update rebar.config
```erlang
% rebar.config
{deps, [
    {erlmcp, {git, "https://github.com/your-org/erlmcp", {tag, "0.6.0"}}},
    {jsx, "3.1.2"},
    {gproc, "0.9.0"}
]}.

{erl_opts, [
    debug_info,
    {platform_define, "(linux|darwin)", 'HAVE_SINCE'}
]}.
```

## Step 2: Implement the Calculator Tool

### Create the Tool Module
```erlang
% src/my_calculator.erl
-module(my_calculator).
-behaviour(erlmcp_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Tool implementation
-export([calculate/1]).

init(_Args) ->
    % Register with the erlmcp registry
    erlmcp_server:register_tool(<<"calculator">>, ?MODULE),
    {ok, #{}}.

handle_call({call_tool, <<"calculate">>, Args}, _From, State) ->
    Result = calculate(Args),
    {reply, {ok, Result}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Tool function
calculate(#{expression := Expression}) ->
    % Safe evaluation of mathematical expressions
    try
        % This is a simplified example - use proper math library in production
        case safe_eval(Expression) of
            {ok, Result} ->
                #{content => [#{type => text, text => float_to_binary(Result, [{decimals, 2})]}]};
            {error, Reason} ->
                throw({error, Reason})
        end
    catch
        Error:Reason ->
            #{content => [#{type => text, text => io_lib:format("Error: ~p", [Reason])}]}
    end.

% Simplified expression evaluator
safe_eval(Expression) when is_binary(Expression) ->
    safe_eval(binary_to_list(Expression));
safe_eval(Expression) ->
    % Parse and evaluate the expression
    % NOTE: This is simplified - use proper parsing in production
    case catch erl_eval:expr_list(parse_expression(Expression), []) of
        {value, Result, _} -> {ok, Result};
        Error -> {error, Error}
    end.

parse_expression("2 + 2") -> [{integer, 1, 2}, {op, '+', '+', 1}, {integer, 1, 2}];
parse_expression("3 * 4") -> [{integer, 1, 3}, {op, '*', '*', 1}, {integer, 1, 4}];
parse_expression(_) -> {error, unsupported_expression}.
```

## Step 3: Create the Server Configuration

### Create sys.config
```erlang
% config/sys.config
[
    {my_calculator, [
        % Application-specific config
        {port, 8080},
        {host, "localhost"},
        {log_level, info}
    ]},

    {erlmcp, [
        % erlmcp configuration
        {transport, tcp},
        {port, 8080},
        {server, [
            {tools, [
                % Register our calculator tool
                {my_calculator, calculator}
            ]}
        ]}
    ]}
].
```

## Step 4: Create the Client Example

### Create MCP Client
```erlang
% src/calculator_client.erl
-module(calculator_client).
-export([start/0, calculate/1, stop/1]).

start() ->
    % Start erlmcp client
    Client = erlmcp_client:start(#{
        transport => tcp,
        host => "localhost",
        port => 8080,
        timeout => 5000
    }),
    Client.

calculate(Client) ->
    % Create MCP request
    Request = #{
        jsonrpc => "2.0",
        id => <<"calc-001">>,
        method => "tools/call",
        params => #{
            name => "calculator",
            arguments => #{expression => "2 + 2"}
        }
    },

    % Send request
    case erlmcp_client:call(Client, Request) of
        {ok, Response} ->
            io:format("Response: ~p~n", [Response]),
            Response;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            error
    end.

stop(Client) ->
    erlmcp_client:stop(Client).
```

## Step 5: Test the Integration

### Start the Server
```bash
# Build the project
rebar3 compile

# Start the application
rebar3 shell
```

In the Erlang shell:
```erlang
% Start the application
application:ensure_all_started(my_calculator).

% Check if tools are registered
erlmcp_server:tools().

% Should return [<<"calculator">>]
```

### Test with the Client
```erlang
% Start the client
Client = calculator_client:start().

% Calculate 2 + 2
Result = calculator_client:calculate(Client).

% Should return MCP response with content "4.00"

% Calculate 3 * 4
Result2 = calculator_client:calculate(Client),
% Should return "12.00"

% Stop the client
calculator_client:stop(Client).
```

### Test Error Handling
```erlang
% Test invalid expression
Request = #{
    jsonrpc => "2.0",
    id => <<"calc-error">>,
    method => "tools/call",
    params => #{
        name => "calculator",
        arguments => #{expression => "invalid"}
    }
},

% Should return error response
erlmcp_client:call(Client, Request).
```

## Step 6: Add Integration Tests

### Create Test Suite
```erlang
% test/calculator_client_tests.erl
-module(calculator_client_tests).
-include_lib("eunit/include/eunit.hrl").

integration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            fun test_calculator/0,
            fun test_error_handling/0
        ]
    }.

setup() ->
    % Start server
    application:ensure_all_started(my_calculator),
    % Start client
    calculator_client:start().

cleanup(Client) ->
    calculator_client:stop(Client).

test_calculator() ->
    Client = calculator_client:start(),
    Result = calculator_client:calculate(Client),
    ?assertMatch(#{<<"content">> := _}, Result).

test_error_handling() ->
    Client = calculator_client:start(),
    InvalidRequest = #{
        jsonrpc => "2.0",
        id => <<"test-error">>,
        method => "tools/call",
        params => #{
            name => "calculator",
            arguments => #{expression => "invalid"}
        }
    },
    ?assertMatch({error, _}, erlmcp_client:call(Client, InvalidRequest)).
```

### Run Tests
```bash
# Run EUnit tests
rebar3 eunit

# Run CT tests if you have them
rebar3 ct
```

## Step 7: Enhance with More Features

### Add More Operations
```erlang
% Update calculate function to support more operations
calculate(#{expression := Expression, operation := Op}) ->
    % Handle different operations
    case Op of
        add -> handle_addition(Expression);
        subtract -> handle_subtraction(Expression);
        multiply -> handle_multiplication(Expression);
        divide -> handle_division(Expression);
        _ -> #{content => [#{type => text, text => "Unsupported operation"}]}
    end.
```

### Add Input Validation
```erlang
validate_args(#{expression := _} = Args) ->
    % Validate required fields
    case maps:is_key(<<"expression">>, Args) of
        true -> {ok, Args};
        false -> {error, missing_expression}
    end;
validate_args(_) ->
    {error, invalid_arguments}.
```

### Add Logging
```erlang
log_request(RequestId, ToolName, Args) ->
    logger:info(#{event => request_received,
                   id => RequestId,
                   tool => ToolName,
                   arguments => Args}).
```

## Step 8: Production Considerations

### Add Supervision
```erlang
% src/my_calculator_sup.erl
-module(my_calculator_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Define child processes
    Children = [
        % Worker for calculator service
        #{id => calculator,
          start => {my_calculator, start_link, []},
          restart => temporary,
          shutdown => 5000,
          type => worker,
          modules => [my_calculator]}
    ],

    {ok, {{one_for_all, 5, 10}, Children}}.
```

### Update Application
```erlang
% src/my_calculator_app.erl
-module(my_calculator_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    my_calculator_sup:start_link().

stop(_State) ->
    ok.
```

## Step 9: Deploy the Integration

### Create Release
```erlang
% rel/config.config
{sys, [
    {lib_dirs, ["../deps"]},
    {rel, "my_calculator", "1.0.0", [
        kernel,
        stdlib,
        sasl,
        gproc,
        jsx,
        erlmcp,
        my_calculator
    ]},
    {releases, {
        my_calculator,
        "1.0.0",
        [
            {kernel, "5.3"},
            {stdlib, "3.7"},
            {sasl, "4.0"},
            {gproc, "0.9.0"},
            {jsx, "3.1.2"},
            {erlmcp, "0.6.0"},
            {my_calculator, "1.0.0"}
        ]
    }}
]}.
```

### Build and Package
```bash
# Build release
rebar3 release

# Package for deployment
tar -czf my_calculator-1.0.0.tar.gz _build/default/rel/my_calculator
```

## Troubleshooting Common Issues

### 1. Tool Not Registered
```erlang
% Check if tool is registered
erlmcp_server:tools().

% Should return [<<"calculator">>]
```

### 2. Connection Refused
```bash
# Check if server is running
netstat -tlnp | grep :8080

# Check firewall
sudo ufw status
```

### 3. Timeout Errors
```erlang
% Increase timeout
Client = erlmcp_client:start(#{timeout => 10000}).
```

## Next Steps

### Advanced Topics
- [Custom Tool Development](custom-tools.md) - Build more complex tools
- [Performance Tuning](performance-optimization.md) - Optimize your setup
- [Security Implementation](security-implementation.md) - Add security layers
- [GCP Integration](gcp-integration.md) - Connect to cloud services

### Real-World Examples
- [Enterprise Integration](../examples/enterprise.md) - Large-scale deployments
- [Performance Case Study](../performance/case-studies.md) - Real optimizations
- [Migration Guide](../migration/migration.md) - Upgrade paths

### Community Resources
- [Join Discussion](../appendices/community.md) - Get help and contribute
- [Report Issues](../appendices/contributing.md) - Contribute to erlmcp
- [Examples Gallery](../appendices/examples.md) - More examples

---

**Success!** You've built your first MCP integration. Next, explore [Custom Tools](custom-tools.md) to build more complex integrations.