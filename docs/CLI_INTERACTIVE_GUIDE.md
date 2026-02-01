# erlmcp CLI Interactive Mode Guide

Complete guide to using erlmcp in interactive mode with the Erlang REPL. Learn available commands, shortcuts, and how to manage servers and connections.

**Table of Contents**:
- [Starting Interactive Mode](#starting-interactive-mode)
- [REPL Basics](#repl-basics)
- [Server Management](#server-management)
- [Working with Connections](#working-with-connections)
- [Querying System State](#querying-system-state)
- [Example Walkthrough](#example-walkthrough)
- [Tips & Tricks](#tips--tricks)

## Starting Interactive Mode

### Basic startup

```bash
erlmcp start
```

This compiles the project and launches an interactive Erlang shell:

```
======================================================================
STARTING ERLMCP CLUSTER
======================================================================
🔨 Compiling... OK
🚀 Starting Erlang shell... OK
📡 Launching erlmcp application...

Erlang/OTP 28 [erts-14.0]

1>
```

The `1>` prompt indicates you're ready to enter Erlang commands.

### Exiting the REPL

```erlang
1> q().
ok
```

Or use `Ctrl+D` (Ctrl+Z on Windows).

## REPL Basics

### Erlang command structure

```erlang
% Simple function call
Module:Function().

% With arguments
Module:Function(Arg1, Arg2).

% Variable assignment
1> X = 42.
42

% Using variables
2> Y = X + 8.
50

% Lists
3> List = [1,2,3,4,5].
[1,2,3,4,5]

% List operations
4> lists:map(fun(N) -> N*2 end, List).
[2,4,6,8,10]
```

### Essential REPL commands

```erlang
% Get help
1> help().

% List all modules
2> code:all_loaded().

% Get module documentation
3> help(erlmcp_server).

% List available functions in module
4> erlmcp_server:module_info(exports).

% Exit REPL
5> q().
ok
```

### Working with results

```erlang
% Previous result is bound to v(N) where N is command number
1> 10 + 20.
30

2> v(1) + 5.    % v(1) refers to result from command 1
35

3> 1 + 2 + 3 + 4 + 5.
15

4> v(-1).       % -1 refers to last command
15
```

### Multi-line entries

For complex code, use a period to continue:

```erlang
1> Func = fun(X) ->
1>   case X of
1>     positive when X > 0 -> "positive";
1>     zero when X =:= 0 -> "zero";
1>     _ -> "negative"
1>   end
1> end.
#Fun<erl_eval.42.126501267>

2> Func(5).
"positive"
```

## Server Management

### Starting a server

```erlang
% Start a new server instance
1> {ok, Pid} = erlmcp_server:start_link(my_server, #{}).
{ok,<0.123.0>}

% The returned Pid is the server's process ID
2> Pid.
<0.123.0>
```

### Server configuration options

```erlang
% Basic configuration
1> Config = #{
1>   transport => stdio,
1>   port => 8080,
1>   timeout => 5000
1> }.
#{transport => stdio, port => 8080, timeout => 5000}

% Start with config
2> {ok, Pid} = erlmcp_server:start_link(server_1, Config).
{ok,<0.124.0>}
```

### Stopping a server

```erlang
% Get server PID
1> Pid = erlmcp_registry:lookup(my_server).
<0.123.0>

% Stop it
2> erlmcp_server:stop(Pid).
ok

% Or check if still running
3> erlang:is_process_alive(Pid).
false
```

### Listing all servers

```erlang
% Show all registered servers
1> erlmcp_registry:list().
[{my_server, <0.123.0>},
 {server_1, <0.124.0>},
 {test_server, <0.125.0>}]
```

## Working with Connections

### Creating a client

```erlang
% Create client process
1> {ok, Client} = erlmcp_client:start_link(client_1, #{
1>   server_pid => ServerPid
1> }).
{ok,<0.130.0>}
```

### Sending messages

```erlang
% Send a request
1> erlmcp_client:request(Client, #{
1>   method => <<"tools/list">>,
1>   params => #{}
1> }).
{ok, {ResponseCode, ResponseBody}}

% Send and wait for response (with timeout)
2> erlmcp_client:request(Client, Request, 5000).
{ok, Response}
```

### Managing connections

```erlang
% List all active connections
1> erlmcp_registry:list_connections().
[{client_1, <0.130.0>},
 {client_2, <0.131.0>}]

% Check connection status
2> erlmcp_client:status(Client).
{ok, connected}

% Close connection
3> erlmcp_client:stop(Client).
ok
```

## Querying System State

### Process information

```erlang
% Interactive process listing (graphical)
1> i().

% Shows all running processes
% PID            Name/Initial Call                Time Rcvd
% <0.0.0>        init                               0
% <0.1.0>        erl_prim_loader                    0
% ...

% Show info about specific process
2> erlang:process_info(<0.123.0>).
[{registered_name,my_server},
 {status,running},
 {reductions,1543},
 {messages,0},
 {links,[<0.1.0>]},
 ...]
```

### System information

```erlang
% Total process count
1> erlang:system_info(process_count).
52

% Total running processes
2> length(erlang:processes()).
52

% Memory usage
3> erlang:memory().
[{total, 12345678},
 {processes, 2345678},
 {processes_used, 2100000},
 ...]

% VM uptime (milliseconds)
4> erlang:monotonic_time(millisecond).
12345

% Atom table
5> erlang:system_info(atom_count).
1234
```

### Application status

```erlang
% List running applications
1> application:which_applications().
[{erlmcp_transports,[],[]},
 {erlmcp_core,[],[]},
 {stdlib,[],[]},
 {kernel,[],[]},
 ...]

% Check specific application status
2> application:info(erlmcp_core).
{error, undefined}  % OR
[{description, "erlmcp Core MCP Implementation"},
 {vsn, "3.0.0"},
 ...]
```

## Example Walkthrough

### Complete session: Create server, add resource, handle requests

```erlang
% ========================================
% Step 1: Start a server
% ========================================

Erlang/OTP 28 [erts-14.0]

1> {ok, Server} = erlmcp_server:start_link(demo_server, #{
1>   transport => stdio
1> }).
{ok,<0.126.0>}

% ========================================
% Step 2: Define a resource
% ========================================

2> Resource = #{
2>   name => <<"file_contents">>,
2>   uri => <<"file:///etc/passwd">>,
2>   description => <<"System password file">>
2> }.
#{name => <<"file_contents">>, ...}

% ========================================
% Step 3: Register resource with server
% ========================================

3> erlmcp_server:add_resource(Server, Resource).
ok

% ========================================
% Step 4: List resources
% ========================================

4> erlmcp_server:list_resources(Server).
[#{name => <<"file_contents">>,
   uri => <<"file:///etc/passwd">>,
   description => <<"System password file">>}]

% ========================================
% Step 5: Create client to test
% ========================================

5> {ok, Client} = erlmcp_client:start_link(demo_client, #{
5>   server_pid => Server
5> }).
{ok,<0.127.0>}

% ========================================
% Step 6: List resources via client
% ========================================

6> erlmcp_client:request(Client, #{
6>   method => <<"resources/list">>,
6>   params => #{}
6> }).
{ok, {200, #{resources => [
  #{name => <<"file_contents">>, ...}
]}}}

% ========================================
% Step 7: Query system state
% ========================================

7> erlmcp_registry:list().
[{demo_server, <0.126.0>},
 {demo_client, <0.127.0>}]

8> erlang:memory().
[{total, 15234567},
 {processes, 3456789},
 ...]

% ========================================
% Step 8: Clean up
% ========================================

9> erlmcp_server:stop(Server).
ok

10> erlmcp_client:stop(Client).
ok

11> erlmcp_registry:list().
[]

% ========================================
% Step 9: Exit REPL
% ========================================

12> q().
ok
```

## Tips & Tricks

### Command history

```erlang
% Press up/down arrows to navigate history
% (Terminal-dependent, works in most modern shells)

% View command history
1> erl_kernel_errors:get_buffer().  % Not standard

% Or save to file periodically
```

### Performance monitoring

```erlang
% Check reductions (CPU work done)
1> erlang:process_info(<0.126.0>, reductions).
{reductions, 12345}

% Monitor message queue
2> erlang:process_info(<0.126.0>, message_queue_len).
{message_queue_len, 0}

% Get all stats
3> erlang:process_info(<0.126.0>).
[{registered_name, demo_server},
 {status, running},
 {reductions, 12345},
 {messages, 0},
 {heap_size, 4000},
 {stack_size, 20},
 {total_heap_size, 5000},
 ...]
```

### Debugging with trace

```erlang
% Enable tracing for specific module
1> dbg:tracer().
{ok,<0.126.0>}

2> dbg:tpl(erlmcp_server, []),
   dbg:tp(erlmcp_server, []),
   dbg:p(all, c).
% c = function calls

3> % Now all erlmcp_server calls are traced

% Later, stop tracing
4> dbg:stop_clear().
ok
```

### Testing with minimal setup

```erlang
% Start just the core without transports
1> application:start(kernel).
ok

2> application:start(stdlib).
ok

3> application:start(erlmcp_core).
ok

% Now test core functionality without I/O
4> {ok, Srv} = erlmcp_server:start_link(test, #{}).
{ok,<0.100.0>}

% Test clean
5> erlmcp_server:stop(Srv).
ok

6> application:stop(erlmcp_core).
ok
```

### Measuring execution time

```erlang
% Simple timing
1> T0 = erlang:monotonic_time(),
1> erlmcp_server:start_link(test, #{}),
1> T1 = erlang:monotonic_time(),
1> (T1 - T0) / 1_000_000.  % Convert to milliseconds
4.567  % 4.567 milliseconds

% More detailed
2> timer:tc(erlmcp_server, start_link, [test, #{}]).
{4567, {ok,<0.130.0>}}  % {microseconds, result}
```

### Working with large data

```erlang
% Create test data
1> Data = lists:seq(1, 100000).
[1,2,3,...]  % Huge list

% Memory used
2> erlang:memory(processes).
15234567  % bytes

% Process in chunks
3> Chunks = [ lists:sublist(Data, I, 1000) ||
3>   I <- lists:seq(1, length(Data), 1000) ].
% Process each chunk separately

% Or use mapreduce pattern
4> lists:foldr(fun(X, Acc) -> Acc + X end, 0, Data).
5000050000
```

### Exploring module structure

```erlang
% List all functions in module
1> erlmcp_server:module_info(exports).
[{start_link, 2},
 {stop, 1},
 {add_resource, 2},
 ...]

% Get module attributes
2> erlmcp_server:module_info(attributes).
[{vsn, [123456789]}]

% Check if function exists
3> code:ensure_loaded(erlmcp_server),
   erlang:function_exported(erlmcp_server, start_link, 2).
true

% Get source file
4> code:where_is_file(erlmcp_server).
"/path/to/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl"
```

### Network debugging

```erlang
% Check if TCP/HTTP transports can bind
1> {ok, Socket} = gen_tcp:listen(8080, []).
{ok, <0.124.0>}

2> gen_tcp:close(Socket).
ok

% Check open ports
3> erlang:system_info(ports).  % Not standard

% Use netstat from shell instead
% Ctrl+Z to suspend REPL
% run: netstat -ltn | grep 8080
% then fg to resume
```

### Stress testing

```erlang
% Create many processes
1> Pids = [erlmcp_client:start_link(
1>   list_to_atom("client_" ++ integer_to_list(I)), #{})
1> || I <- lists:seq(1, 1000)].
[{ok,<0.200.0>}, {ok,<0.201.0>}, ...]

% Check system under load
2> erlang:system_info(process_count).
1050

3> erlang:memory().
[{total, 45234567}, ...]

% Clean up
4> lists:foreach(
4>   fun({ok, Pid}) -> erlmcp_client:stop(Pid) end, Pids).
ok

5> erlang:system_info(process_count).
50  % Back to normal
```

### Common patterns

#### Safe function calls with guards

```erlang
% Call only if process exists
1> case erlang:is_process_alive(Pid) of
1>   true -> erlmcp_server:stop(Pid);
1>   false -> already_dead
1> end.
ok
```

#### Error handling

```erlang
% Try/catch pattern
1> try
1>   erlmcp_server:start_link(test, #{})
1> catch
1>   error:E -> {error, E};
1>   exit:E -> {exit, E}
1> end.
{ok,<0.130.0>}

% Result wrapping
2> case Result of
2>   {ok, Val} -> process(Val);
2>   {error, Reason} -> handle_error(Reason)
2> end.
```

#### Working with maps

```erlang
% Create map
1> Config = #{name => test, port => 8080}.
#{name => test, port => 8080}

% Access values
2> maps:get(name, Config).
test

3> maps:get(nonexistent, Config, default).
default

% Update values
4> maps:put(port, 9090, Config).
#{name => test, port => 9090}

% Merge maps
5> maps:merge(Config, #{ssl => true}).
#{name => test, port => 8080, ssl => true}
```

## Advanced REPL Features

### Observer (GUI process inspector)

```erlang
% Start observer GUI
1> observer:start().
ok

% This opens a graphical window showing:
% - Processes and their state
% - Memory usage
% - Performance graphs
% - Module information
```

### Code hot-reloading

```erlang
% Compile modified code
1> c(erlmcp_server).
{ok, erlmcp_server}  % If no errors

% Reload all modules
2> l(erlmcp_server).
{module, erlmcp_server}

% Check if new version loaded
3> erlmcp_server:module_info(vsn).
[123456790]  % New version number
```

### Debugger

```erlang
% Start debugger
1> debugger:start().
ok

% Set breakpoint
2> int:ni(erlmcp_server).
{module,erlmcp_server}

3> int:break(erlmcp_server, 50).  % Line 50
ok

% Continue execution with: int:continue(AttachLevel)
```

## Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `Ctrl+C` | Interrupt current operation |
| `Ctrl+D` / `Ctrl+Z` | Exit REPL |
| `Ctrl+A` | Go to start of line |
| `Ctrl+E` | Go to end of line |
| `Ctrl+K` | Delete from cursor to end |
| `Ctrl+U` | Delete from start to cursor |
| Up/Down arrows | Navigate command history |

## See Also

- [CLI_REFERENCE.md](CLI_REFERENCE.md) - Command reference guide
- [DIAGNOSTICS_GUIDE.md](DIAGNOSTICS_GUIDE.md) - Advanced debugging
- [Erlang/OTP Documentation](https://erlang.org/doc/) - Official Erlang docs
