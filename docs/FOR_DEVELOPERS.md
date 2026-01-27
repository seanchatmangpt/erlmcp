# For Developers: Complete Development Guide

**Estimated read time: 20 minutes**

This guide is tailored for developers who write code, run tests, and debug issues in erlmcp.

## Setup (5 minutes)

### Prerequisites Check
```bash
# Verify you have required tools
erl -version                    # Should show 25+
rebar3 version                  # Should show 3.22+
git --version                   # Any recent version
```

### One-Time Setup
```bash
# Clone and enter
git clone https://github.com/banyan-platform/erlmcp.git
cd erlmcp

# Enable environment loading
direnv allow

# Initialize workspace
make setup
```

### Verify Setup
```bash
make workspace-build
make workspace-test
# Both should complete successfully
```

## Daily Development Workflow

### Start Your Day
```bash
cd /path/to/erlmcp
direnv allow          # Auto-loads environment

# Quick build and test
make build
make test
```

### Make Changes
Edit source files in:
- `src/*.erl` - erlmcp source code
- `taiea/apps/*/src/*.erl` - TAIEA applications
- `test/*.erl` - Test files
- `include/*.hrl` - Header files

### Fast Feedback Loop
```bash
# After editing, run specific test
make test-unit                  # ~5s feedback

# Or test specific module
rebar3 eunit --module=erlmcp_transport_tcp_tests

# Or run specific test file
rebar3 ct --suite=erlmcp_SUITE
```

### Before Committing
```bash
# Full validation
make check

# If issues found, fix and re-run
make check

# Commit when green
git add .
git commit -m "feat: describe what changed"
```

## Code Organization

### Project Structure
```
erlmcp/
├── src/
│   ├── erlmcp.app.src              # Application definition
│   ├── erlmcp_server.erl           # MCP server implementation
│   ├── erlmcp_client.erl           # MCP client implementation
│   ├── erlmcp_json_rpc.erl         # JSON-RPC protocol
│   ├── erlmcp_types.erl            # Type definitions
│   ├── erlmcp_transport_*.erl      # Transport implementations
│   └── erlmcp_sup.erl              # Root supervisor
│
├── test/
│   ├── erlmcp_SUITE.erl            # Main Common Test suite
│   ├── *_tests.erl                 # EUnit tests
│   ├── properties_*.erl            # PropEr property tests
│   └── ct_helper.erl               # Test utilities
│
├── include/
│   └── erlmcp.hrl                  # Type definitions & records
│
├── examples/
│   ├── weather_server.erl          # Weather MCP server example
│   ├── calculator_client.erl       # Calculator client example
│   └── README.md                   # Example documentation
│
└── config/
    ├── sys.config                  # System configuration
    └── test.config                 # Test configuration
```

### Module Naming Conventions
- Core modules: `erlmcp_*.erl`
- Transport modules: `erlmcp_transport_*.erl`
- Supervisor modules: `*_sup.erl`
- Test modules: `*_tests.erl` (EUnit) or `*_SUITE.erl` (CT)
- Utilities: `erlmcp_*.erl`

## Writing Code

### Code Style
erlmcp follows standard Erlang conventions:

```erlang
% Module declaration first
-module(erlmcp_example).

% Exports (public API)
-export([public_function/1, public_function/2]).

% Includes
-include("erlmcp.hrl").

% Records
-record(state, {
    id :: binary(),
    count :: integer(),
    status :: atom()
}).

% Type definitions
-type request() :: {method, atom(), params, map()}.

% Function with documentation
%% @doc Does something useful.
%% @param Input The input value
%% @return The result
-spec public_function(term()) -> {ok, term()} | {error, atom()}.
public_function(Input) ->
    case validate_input(Input) of
        {ok, Valid} -> process(Valid);
        Error -> Error
    end.

% Private helper
validate_input(Input) ->
    % ... validation logic
    ok.
```

### Gen_server Pattern
For stateful modules, use gen_server:

```erlang
-module(erlmcp_example_server).
-behavior(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init(Options) ->
    {ok, #{options => Options}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
```

### Error Handling
Always return Result type:

```erlang
-spec do_something(term()) -> {ok, term()} | {error, atom()}.
do_something(Input) ->
    case validate(Input) of
        ok ->
            try process(Input) of
                Result -> {ok, Result}
            catch
                Type:Reason ->
                    {error, {Type, Reason}}
            end;
        Error ->
            Error
    end.
```

Never use unwrap/expect:
```erlang
% BAD - will crash on error
Value = maps:get(key, Map),

% GOOD - handles error
Value = case maps:find(key, Map) of
    {ok, V} -> V;
    error -> default_value
end.
```

## Testing

### Test-Driven Development (TDD)

1. **Red**: Write failing test
2. **Green**: Write minimal implementation
3. **Refactor**: Clean up code

Example workflow:

```erlang
% test/my_feature_tests.erl
-module(my_feature_tests).
-include_lib("eunit/include/eunit.hrl").

% Test case
my_function_returns_ok_test() ->
    Result = my_feature:my_function(input),
    ?assertEqual({ok, expected}, Result).

% To run:
% rebar3 eunit --module=my_feature_tests
```

### Types of Tests

#### Unit Tests (EUnit)
Fast, test individual functions:

```erlang
-module(encoder_tests).
-include_lib("eunit/include/eunit.hrl").

encode_json_test() ->
    Input = #{name => <<"Alice">>, age => 30},
    Result = encoder:to_json(Input),
    ?assertMatch(<<"\"name\":\"Alice\"">>, Result).

% Run: rebar3 eunit
```

#### Integration Tests (Common Test)
Test module interactions:

```erlang
-module(erlmcp_SUITE).
-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    ok.

client_server_handshake(_Config) ->
    {ok, Server} = erlmcp_server:start_link({stdio, []}, #{}),
    {ok, Client} = erlmcp_client:start_link({stdio, []}, #{}),
    % Test interaction...
    ok.

% Run: rebar3 ct --suite=erlmcp_SUITE
```

#### Property-Based Tests (PropEr)
Test invariants with random inputs:

```erlang
-module(properties_encoding).
-include_lib("proper/include/proper.hrl").

prop_encode_decode_round_trip() ->
    ?FORALL(Data, data_generator(),
        begin
            Encoded = encoder:encode(Data),
            {ok, Decoded} = decoder:decode(Encoded),
            Data =:= Decoded
        end).

data_generator() ->
    oneof([
        integer(),
        binary(),
        list(integer())
    ]).

% Run: rebar3 proper
```

### Running Tests

```bash
# All tests
make test

# Specific test suites
make test-unit                      # EUnit only
make test-integration               # Common Test only
make test-property                  # PropEr only

# Specific test
rebar3 eunit --module=module_name
rebar3 ct --suite=suite_name

# Verbose output
rebar3 ct -v
rebar3 eunit -v

# With coverage
make test && make coverage-report
```

### Coverage Analysis

```bash
# Generate report
make coverage-report

# View HTML
open _build/test/cover/index.html
```

Target: 80%+ coverage for production code

## Debugging

### Interactive Shell

```bash
# Start shell with code loaded
make console

# In shell, test your code
(erlmcp@hostname)1> erlmcp_server:start_link({stdio, []}, #{}).
{ok, <0.123.0>}

% Use debugger commands
(erlmcp@hostname)2> erlmcp_client:initialize(Client, #{}).
```

### Observer GUI
```bash
make observer
```

Explore:
- Running processes (pids, reductions, memory)
- Module details and exports
- System statistics
- Trace specific function calls

### Print Debugging
Use lager logging:

```erlang
-include_lib("lager/include/lager.hrl").

my_function(Input) ->
    lager:debug("Processing input: ~p", [Input]),
    Result = process(Input),
    lager:info("Result: ~p", [Result]),
    Result.
```

Configure in `config/sys.config`:
```erlang
{lager, [
    {handlers, [
        {lager_console_backend, [
            {level, debug}
        ]}
    ]}
]}.
```

### Trace Specific Functions

```erlang
% Trace all calls to a function
recon:trace(erlmcp_server, 'handle_call', 3, [{scope, g}])

% View trace output
recon:tracer()

% Stop tracing
recon:stop_tracing()
```

## Common Development Tasks

### Add a New Transport

1. Create module: `src/erlmcp_transport_newtype.erl`
2. Implement callbacks:
   ```erlang
   -module(erlmcp_transport_newtype).

   % Send data to remote
   -callback send(term(), binary()) -> ok | {error, term()}.

   % Receive data from remote
   -callback receive() -> {ok, binary()} | {error, term()}.

   % Close connection
   -callback close() -> ok.
   ```

3. Add tests: `test/erlmcp_transport_newtype_tests.erl`
4. Update docs: `docs/api-reference.md`

### Add a New Tool to MCP Server

```erlang
% In your server handler:
Schema = #{
    <<"type">> => <<"object">>,
    <<"properties">> => #{
        <<"name">> => #{<<"type">> => <<"string">>},
        <<"count">> => #{<<"type">> => <<"integer">>}
    },
    <<"required">> => [<<"name">>]
},

erlmcp_server:add_tool_with_schema(Server, <<"my_tool">>,
    fun(Args) ->
        Name = maps:get(<<"name">>, Args),
        % Process...
        {ok, Result}
    end,
    Schema).
```

### Add a New Resource

```erlang
erlmcp_server:add_resource(Server, <<"myapp://resource">>,
    fun(Uri) ->
        % Return resource content
        {ok, <<"content">>}
    end).
```

### Update Documentation

1. Edit relevant `.md` file in `docs/`
2. Keep examples up to date
3. Update API reference when changing public APIs
4. Update architecture docs for design changes

## Performance Development

### Profile Your Code
```bash
make profile
```

Uses recon to identify slow functions.

### Memory Leaks
```bash
make console

% In shell:
observer:start().
% Watch memory growth in Observer
```

### Benchmark
Create benchmark in `benches/`:

```erlang
-module(benchmark_module).

% Run with: rebar3 bench
benchmark() ->
    {timeout, 60, fun() ->
        [run_test() || _ <- lists:seq(1, 10000)]
    end}.
```

## Commits & Pull Requests

### Commit Message Format
```
type(scope): description

Longer explanation if needed.

Fixes #123
```

Types: `feat`, `fix`, `docs`, `test`, `refactor`, `perf`, `ci`

### Before Creating PR
```bash
# Full validation
make workspace-check

# All tests pass
make workspace-test

# No warnings
make workspace-lint

# Code formatted
make format
```

### PR Checklist
- [ ] Tests pass locally
- [ ] Coverage adequate (80%+)
- [ ] Lint passes
- [ ] Documentation updated
- [ ] Commit messages clear
- [ ] No breaking changes documented

## Useful Resources

### Erlang/OTP
- [Erlang Documentation](https://www.erlang.org/doc/)
- [OTP Design Principles](https://www.erlang.org/doc/design_principles/)
- [EUnit Testing](https://www.erlang.org/doc/apps/eunit/chapter.html)
- [Common Test](https://www.erlang.org/doc/apps/common_test/introduction.html)

### Tools
- [rebar3 User Guide](https://rebar3.org/)
- [Dialyzer User Guide](https://www.erlang.org/doc/apps/dialyzer/)
- [recon for debugging](https://ferd.github.io/recon/)

### MCP Protocol
- [Model Context Protocol](https://modelcontextprotocol.io/)
- [MCP Spec](https://spec.modelcontextprotocol.io/)

## Getting Help

1. Check existing issues on GitHub
2. Review test cases for examples
3. Read architecture docs
4. Ask in discussions
5. Create detailed issue with reproduction steps

## Next Steps

- **To deploy**: See [DEPLOYMENT.md](DEPLOYMENT.md)
- **To understand build**: See [BUILD_SYSTEM.md](BUILD_SYSTEM.md)
- **To understand architecture**: See [ARCHITECTURE_OVERVIEW.md](ARCHITECTURE_OVERVIEW.md)
- **For examples**: See [examples/README.md](../examples/README.md)

---

**Last Updated**: 2026-01-26
**Status**: Ready for development
**Erlang/OTP Version**: 25+
**rebar3 Version**: 3.22+
