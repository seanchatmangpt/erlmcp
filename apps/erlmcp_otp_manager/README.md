# erlmcp_otp_manager

OTP Build Manager - Compiles Erlang/OTP from source with real-time progress tracking.

## Overview

`erlmcp_otp_manager` provides a gen_server-based solution for building Erlang/OTP from source. It implements the OTP behavioral patterns from `docs/otp-patterns.md` with full supervision, graceful error handling, and detailed build receipts.

## Architecture

### Core Module: erlmcp_otp_builder

A gen_server that manages the complete OTP build lifecycle:

```
┌─────────────────────────────────────────────────┐
│                                                 │
│  erlmcp_otp_builder (gen_server)                │
│                                                 │
│  Phases:                                        │
│    1. Configure (./configure --prefix=...)      │
│    2. Build (make -j<CPU_COUNT>)                │
│    3. Install (make install)                    │
│                                                 │
│  Features:                                      │
│    - Line-by-line output capture                │
│    - Progress estimation                        │
│    - Graceful cancellation                      │
│    - Detailed build receipts                    │
│    - 600s total timeout                         │
│                                                 │
└─────────────────────────────────────────────────┘
```

## API

### Starting the Builder

```erlang
{ok, Builder} = erlmcp_otp_builder:start_link().
```

### Building OTP from Source

```erlang
Result = erlmcp_otp_builder:build(Builder, #{
    source_dir => "/path/to/otp/source",
    prefix => "/opt/erlang/28.3.1"
}).
```

**Returns:**
- `{ok, Prefix, Receipt}` - Build succeeded
- `{error, Reason}` - Build failed

### Receipt Structure

```erlang
#{
    timestamp := integer(),         % Start time (milliseconds)
    duration := integer(),          % Total duration (milliseconds)
    configure_output := binary(),   % Configure phase output
    build_output := binary(),       % Build phase output
    install_output := binary(),     % Install phase output
    build_log := binary()          % Complete build log
}
```

### Cancelling a Build

```erlang
ok = erlmcp_otp_builder:cancel(Builder).
```

### Checking Status

```erlang
{ok, Status} = erlmcp_otp_builder:get_status(Builder).
```

**Status Map:**
```erlang
#{
    phase := configure | make | install | idle,
    source_dir := file:filename_all() | undefined,
    prefix := file:filename_all() | undefined,
    elapsed_ms := integer(),
    cancelled := boolean()
}
```

## Build Process

### Phase 1: Configure (120s timeout)

```bash
./configure --prefix=<PREFIX> \
            --enable-threads \
            --disable-wx \
            --disable-odbc
```

**Progress Indicators:**
- `checking for...` - Configuration checks
- `creating Makefile` - Configuration complete

### Phase 2: Build (480s timeout)

```bash
make -j<CPU_COUNT>
```

Uses all available CPU cores (`erlang:system_info(schedulers_online)`).

**Progress Indicators:**
- `CC <file>` - Compiling C files
- `LD <target>` - Linking binaries
- `GEN <file>` - Generating files

### Phase 3: Install (60s timeout)

```bash
make install
```

**Progress Indicators:**
- `Installing to <PREFIX>` - Installation in progress
- `Installation complete` - Installation finished

## Error Handling

### Build Failures

```erlang
{error, {configure_failed, ExitCode, Output}}
{error, {make_failed, ExitCode, Output}}
{error, {install_failed, ExitCode, Output}}
```

### Timeouts

```erlang
{error, {configure_timeout, PartialOutput}}
{error, {make_timeout, PartialOutput}}
{error, {install_timeout, PartialOutput}}
```

### Cancellation

```erlang
{error, cancelled}
```

### Invalid Input

```erlang
{error, {source_dir_not_found, Path}}
{error, build_in_progress}
```

## OTP Compliance

### gen_server Callbacks

All 6 required callbacks implemented:

1. `init/1` - Initialize state with CPU count detection
2. `handle_call/3` - Synchronous build/status requests
3. `handle_cast/2` - Asynchronous cancellation
4. `handle_info/2` - Port output and phase transitions
5. `terminate/2` - Graceful port cleanup
6. `code_change/3` - Hot code upgrade support

### Supervision

The builder is designed to be supervised:

```erlang
ChildSpec = #{
    id => erlmcp_otp_builder,
    start => {erlmcp_otp_builder, start_link, []},
    restart => transient,
    shutdown => 5000,
    type => worker,
    modules => [erlmcp_otp_builder]
}.
```

### Let-It-Crash Philosophy

- **No defensive programming** - Invalid states crash immediately
- **Port monitoring** - Automatically cleaned up on failure
- **Supervisor restart** - Builder can be restarted independently
- **Isolated failures** - One build failure doesn't affect other builders

## Testing

### Chicago TDD Approach

All tests use real processes (no mocks):

```erlang
%% Real builder process
{ok, Builder} = erlmcp_otp_builder:start_link(),

%% Real filesystem operations
create_mock_configure_script(SourceDir, "echo 'configure'"),

%% Real port execution
Result = erlmcp_otp_builder:build(Builder, #{...}).
```

### Test Coverage

- **Basic Operations**: Start/stop, status queries
- **Error Cases**: Missing directories, invalid scripts, timeouts
- **Concurrency**: Multiple builders running in parallel
- **Cancellation**: Cancel during each phase
- **Performance**: Build duration tracking

### Running Tests

```bash
# Single module tests
rebar3 eunit --module=erlmcp_otp_builder_tests

# All tests
rebar3 eunit --app=erlmcp_otp_manager

# With coverage
rebar3 eunit --cover --app=erlmcp_otp_manager
```

## Performance

### Timeouts

| Phase | Timeout | Typical Duration |
|-------|---------|------------------|
| Configure | 120s | 30-60s |
| Build | 480s | 300-400s |
| Install | 60s | 10-20s |
| **Total** | **600s** | **340-480s** |

### Resource Usage

- **CPU**: `make -j<N>` uses N cores (typically 4-8)
- **Memory**: ~500MB during build phase
- **Disk**: ~1.5GB for source + build artifacts
- **Install**: ~200MB for installation

### Concurrent Builds

Multiple builders can run simultaneously:

```erlang
%% Start 3 builders for parallel builds
Builders = [erlmcp_otp_builder:start_link() || _ <- lists:seq(1, 3)],

%% Each builder is independent
lists:foreach(fun({ok, Builder}) ->
    spawn(fun() ->
        erlmcp_otp_builder:build(Builder, #{...})
    end)
end, Builders).
```

**Note**: Concurrent builds are CPU-intensive. Limit to `CPU_COUNT / 4` builders.

## Integration with SessionStart Hook

The `erlmcp_otp_builder` can be integrated into `.claude/hooks/SessionStart.sh`:

```bash
# Use Erlang-based builder instead of shell scripts
erl -noshell -eval '
    application:ensure_all_started(erlmcp_otp_manager),
    {ok, Builder} = erlmcp_otp_builder:start_link(),
    Result = erlmcp_otp_builder:build(Builder, #{
        source_dir => "/tmp/otp_src",
        prefix => "/home/user/.erlmcp/otp-28.3.1"
    }),
    io:format("Build result: ~p~n", [Result]),
    init:stop().
'
```

## Examples

### Basic Build

```erlang
%% Start builder
{ok, Builder} = erlmcp_otp_builder:start_link(),

%% Build OTP 28.3.1
{ok, Prefix, Receipt} = erlmcp_otp_builder:build(Builder, #{
    source_dir => "/tmp/otp-OTP-28.3.1",
    prefix => "/opt/erlang/28.3.1"
}),

%% Check duration
Duration = maps:get(duration, Receipt),
io:format("Build completed in ~p seconds~n", [Duration div 1000]).
```

### Build with Cancellation

```erlang
%% Start builder
{ok, Builder} = erlmcp_otp_builder:start_link(),

%% Start build in background
spawn(fun() ->
    Result = erlmcp_otp_builder:build(Builder, #{...}),
    io:format("Build result: ~p~n", [Result])
end),

%% Wait a bit, then cancel
timer:sleep(5000),
erlmcp_otp_builder:cancel(Builder).
```

### Monitoring Build Progress

```erlang
%% Monitor builder status
monitor_build(Builder) ->
    {ok, Status} = erlmcp_otp_builder:get_status(Builder),
    io:format("Phase: ~p, Elapsed: ~pms~n", [
        maps:get(phase, Status),
        maps:get(elapsed_ms, Status)
    ]),
    case maps:get(phase, Status) of
        idle -> ok;
        _ ->
            timer:sleep(1000),
            monitor_build(Builder)
    end.
```

## Future Enhancements

1. **Progress Streaming**: Publish progress events to gproc
2. **Parallel Builds**: Optimize for multi-core systems
3. **Incremental Builds**: Detect and reuse previous builds
4. **Build Caching**: Cache successful builds by source hash
5. **Distributed Builds**: Coordinate builds across multiple nodes

## See Also

- `docs/otp-patterns.md` - OTP design patterns
- `.claude/hooks/SessionStart.sh` - OTP installation automation
- `apps/erlmcp_core/src/erlmcp_server.erl` - Reference gen_server implementation
