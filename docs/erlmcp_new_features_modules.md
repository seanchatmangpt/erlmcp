# erlmcp_new_features Module Summary

This document provides a comprehensive overview of the restored modules in `apps/erlmcp_new_features/src/`. These modules provide advanced functionality for distributed systems, message processing, and workflow orchestration within the erlmcp ecosystem.

---

## 1. erlmcp_mcp_relay.erl

### Purpose
The MCP Relay Server acts as a proxy server that forwards JSON-RPC 2.0 requests to multiple backend MCP servers. It provides load distribution, health monitoring, and dynamic backend management.

### Key Features
- **Dynamic Backend Management**: Register, remove, and enable/disable backends at runtime
- **Load Distribution**: Weighted round-robin algorithm for optimal load distribution
- **Health Monitoring**: Backend health tracking and automatic failover
- **Request Correlation**: Track pending requests and handle timeouts
- **Fault Tolerance**: Comprehensive error handling and logging

### API Functions

#### Server Management
```erlang
start_link() -> {ok, pid()} | {error, term()}
start_link(Options) -> {ok, pid()} | {error, term()}
```

#### Request Relay
```erlang
relay_request(Request, Timeout) -> {ok, response()} | {error, term()}
relay_request(BackendId, Request, Timeout) -> {ok, response()} | {error, term()}
```

#### Backend Management
```erlang
add_backend(BackendId, Config) -> ok | {error, term()}
remove_backend(BackendId) -> ok | {error, term()}
list_backends() -> {ok, [{backend_id(), backend_config()}]}
get_backend_status(BackendId) -> {ok, backend_config()} | {error, not_found}
set_backend_enabled(BackendId, Enabled) -> ok | {error, term()}
```

### Data Types
```erlang
-type backend_id() :: atom() | binary().
-type backend_url() :: binary() | string().
-type backend_config() :: #{
    url => backend_url(),
    enabled => boolean(),
    timeout => pos_integer(),
    weight => non_neg_integer(),
    healthy => boolean()
}.

-type request() :: #{
    jsonrpc := binary(),
    method := binary(),
    id => term(),
    params => map() | [term()]
}.

-type response() :: #{
    jsonrpc := binary(),
    result := term(),
    id := term()
} | #{
    jsonrpc := binary(),
    error => map(),
    id := term()
}.
```

### Dependencies
- `gen_server` (OTP)
- `logger` (kernel)
- `httpc` (inets)
- `jsone` (JSON encoding/decoding)

### Configuration
- Default request timeout: 5000ms
- Weighted round-robin selection
- Backend health checking enabled by default

---

## 2. erlmcp_distributed_lock.erl

### Purpose
A distributed locking system with TTL (Time-To-Live) support and priority-based queuing. Provides safe concurrent access to shared resources across distributed Erlang nodes.

### Key Features
- **Distributed Locking**: Safe resource locking across multiple Erlang nodes
- **TTL Support**: Automatic lock expiration to prevent deadlocks
- **Priority Queuing**: Priority-based lock request handling
- **Auto-Cleanup**: Automatic cleanup when processes crash
- **Non-blocking Operations**: Support for immediate and wait-based acquisition

### API Functions

#### Server Management
```erlang
start_link() -> {ok, pid()} | {error, term()}
start_link(Opts) -> {ok, pid()} | {error, term()}
```

#### Lock Operations
```erlang
acquire(LockName) -> {ok, reference()} | {error, term()}
acquire(LockName, Options) -> {ok, reference()} | {error, term()}
release(LockRef) -> ok | {error, term()}
```

#### Status Monitoring
```erlang
status() -> #{binary() => map()}
status(LockName) -> {ok, map()} | {error, not_found}
```

### Data Types
```erlang
-type lock_name() :: binary().
-type lock_options() :: #{
    ttl_ms => pos_integer(),           % Default: 30000ms
    wait_timeout_ms => pos_integer(),   % Default: 30000ms
    retry_interval_ms => pos_integer(), % Default: 500ms
    auto_extend => boolean(),          % Not implemented yet
    priority => integer()              % Default: 0 (higher = more urgent)
}.
```

### Internal Record Structure
```erlang
-record(lock, {
    name :: binary(),
    owner :: {node(), pid()},
    acquired_at :: integer(),
    expires_at :: integer()
}).

-record(lock_request, {
    request_id :: reference(),
    requester :: pid(),
    requested_at :: integer(),
    priority = 0 :: integer()
}).
```

### Operation Flow
1. **Immediate Acquisition**: Lock available → acquire immediately
2. **Expired Lock**: Check TTL → remove and grant to next requester
3. **Contention**: Queue request by priority → grant on timeout or release
4. **Process Death**: Automatic cleanup of owned locks
5. **Timeout Handling**: Remove expired locks and queued requests

### Dependencies
- `gen_server` (OTP)
- `logger` (kernel)
- `timer` (kernel)

### Default Values
- TTL: 30 seconds
- Wait timeout: 30 seconds
- Retry interval: 500ms
- Priority: 0 (lower values = lower priority)

---

## 3. erlmcp_message_queue.erl

### Purpose
A priority-based message queue with multiple storage backends, supporting acknowledgments, retries, and dead letter handling.

### Key Features
- **Multiple Storage Backends**: In-memory (queue) and ETS table storage
- **Priority Processing**: 0-9 priority levels (higher = more urgent)
- **Acknowledgment System**: ACK/NACK with configurable timeouts
- **Retry Mechanism**: Exponential backoff on failed deliveries
- **Dead Letter Queue**: Automatic handling of max-attempt messages
- **Statistics Tracking**: Comprehensive queue metrics

### API Functions

#### Server Management
```erlang
start_link(Name) -> {ok, pid()} | {error, term()}
start_link(Name, Config) -> {ok, pid()} | {error, term()}
stop(QueuePid) -> ok
```

#### Message Operations
```erlang
enqueue(QueuePid, Payload) -> {ok, message_id()} | {error, term()}
enqueue(QueuePid, Payload, Priority) -> {ok, message_id()} | {error, term()}
dequeue(QueuePid) -> {ok, message_id(), payload(), reference()} | {error, empty}
dequeue(QueuePid, WorkerPid) -> {ok, message_id(), payload(), reference()} | {error, empty}
```

#### Delivery Handling
```erlang
acknowledge(QueuePid, DeliveryRef) -> ok | {error, term()}
nack(QueuePid, DeliveryRef) -> ok | {error, term()}
nack(QueuePid, DeliveryRef, Requeue) -> ok | {error, term()}
```

#### Monitoring
```erlang
get_stats(QueuePid) -> {ok, queue_stats()}
```

### Data Types
```erlang
-type queue_name() :: binary().
-type message_id() :: binary().
-type payload() :: term().
-type priority() :: 0..9.

-type message() :: #{
    id => message_id(),
    payload => payload(),
    priority => priority(),
    attempts => non_neg_integer(),
    max_attempts => non_neg_integer(),
    created_at => integer(),
    expires_at => integer() | undefined
}.

-type delivery() :: #{
    message => message(),
    delivery_id => reference(),
    delivered_at => integer(),
    worker_pid => pid()
}.

-type queue_config() :: #{
    max_size => non_neg_integer(),        % Default: 10000
    storage_backend => storage_backend(), % Default: ets
    ack_timeout_ms => pos_integer(),      % Default: 30000
    retry_backoff_ms => pos_integer(),   % Default: 1000
    dead_letter_threshold => pos_integer() % Default: 5
}.

-type queue_stats() :: #{
    pending => non_neg_integer(),
    delivered => non_neg_integer(),
    acknowledged => non_neg_integer(),
    dead_lettered => non_neg_integer()
}.
```

### Storage Backends
- **Memory**: Uses `queue` module, FIFO behavior
- **ETS**: Uses ordered_set, priority-based insertion

### Message Processing Flow
1. **Enqueue**: Insert with priority, check size limits
2. **Dequeue**: Remove highest priority message, track delivery
3. **ACK/NACK**: Mark as completed or retry
4. **Timeout Handling**: Auto-requeue on ACK timeout
5. **Retry**: Exponential backoff for failed messages
6. **Dead Letter**: Move to DLQ after max attempts

### Dependencies
- `gen_server` (OTP)
- `logger` (kernel)
- `queue` (stdlib)
- `ets` (stdlib)

---

## 4. erlmcp_workflow_engine.erl

### Purpose
A workflow orchestration engine that executes DAG (Directed Acyclic Graph) based workflows with support for parallel and sequential steps, conditional execution, and retry logic.

### Key Features
- **DAG-based Workflow**: Support for complex dependency graphs
- **Multiple Step Types**: Tool, Parallel, Sequence, Conditional, Loop, Delay
- **Retry Logic**: Configurable retry with exponential backoff
- **Cancellation Support**: Stop running workflows
- **State Tracking**: Monitor execution status and progress
- **Isolated Execution**: Each workflow runs in separate process

### API Functions

#### Server Management
```erlang
start_link() -> {ok, pid()} | {error, term()}
```

#### Workflow Management
```erlang
define_workflow(Workflow) -> ok | {error, term()}
execute_workflow(WorkflowId, InputData) -> {ok, execution_id()} | {error, term()}
list_workflows() -> {ok, [workflow_id()]}
```

#### Execution Control
```erlang
cancel_execution(ExecutionId) -> ok | {error, term()}
get_execution_status(ExecutionId) -> {ok, execution()} | {error, not_found}
```

### Data Types
```erlang
-type workflow_id() :: binary().
-type step_id() :: binary().
-type execution_id() :: binary().
-type status() :: pending | running | completed | failed | cancelled.

-type step() :: #{
    id => step_id(),
    type => tool | parallel | sequence | conditional | loop | delay,
    tool_name => binary(),
    tool_arguments => map(),
    child_steps => [step()],
    max_retries => non_neg_integer(),
    retry_backoff_ms => non_neg_integer(),
    timeout_sec => pos_integer()
}.

-type transition() :: #{
    from => step_id(),
    to => step_id(),
    condition => success | failure | {value_equals, term()} | {expression, binary()}
}.

-type workflow() :: #{
    id => workflow_id(),
    steps => [step()],
    transitions => [transition()]
}.

-type execution() :: #{
    execution_id => execution_id(),
    workflow_id => workflow_id(),
    status => status(),
    started_at => erlang:timestamp(),
    completed_at => erlang:timestamp() | undefined,
    input_data => map(),
    output_data => map() | undefined,
    error_message => binary() | undefined,
    executor_pid => pid() | undefined
}.
```

### Step Types
1. **Tool**: Execute a tool with specified arguments
2. **Parallel**: Execute multiple child steps concurrently
3. **Sequence**: Execute child steps sequentially
4. **Conditional**: Execute based on condition evaluation
5. **Loop**: Execute child steps multiple times
6. **Delay**: Pause execution for specified time

### Workflow Execution Flow
1. **Define**: Validate and store workflow definition
2. **Execute**: Spawn executor process, monitor execution
3. **Execute Steps**: Process initial steps (no dependencies)
4. **Handle Dependencies**: Use transitions for step sequencing
5. **Process Results**: Aggregate outputs and handle errors
6. **Complete**: Update execution status and notify

### Configuration Example
```erlang
Workflow = #{
    id => <<"data_processing">>,
    steps => [
        #{
            id => <<"extract">>,
            type => tool,
            tool_name => <<"file_reader">>,
            tool_arguments => #{path => <<"data.csv">>},
            max_retries => 3,
            timeout_sec => 30
        },
        #{
            id => <<"transform_parallel">>,
            type => parallel,
            child_steps => [
                #{id => <<"clean">>, type => tool, tool_name => <<"data_cleaner">>},
                #{id => <<"validate">>, type => tool, tool_name => <<"data_validator">>}
            ]
        },
        #{
            id => <<"load">>,
            type => tool,
            tool_name => <<"database_writer">>,
            tool_arguments => #{table => <<"processed_data">>},
            timeout_sec => 60
        }
    ],
    transitions => [
        #{from => <<"extract">>, to => <<"transform_parallel">>, condition => success},
        #{from => <<"transform_parallel">>, to => <<"load">>, condition => success}
    ]
}
```

### Dependencies
- `gen_server` (OTP)
- `sets` (stdlib)
- `timer` (kernel)
- `crypto` (stdlib)

---

## Integration and Dependencies

### Module Dependencies
```
erlmcp_mcp_relay.erl → httpc, jsone
erlmcp_distributed_lock.erl → gen_server, logger
erlmcp_message_queue.erl → gen_server, logger, queue, ets
erlmcp_workflow_engine.erl → gen_server, sets, timer, crypto
```

### Common Patterns
- **gen_server Behavior**: All modules implement gen_server for state management
- **Process Monitoring**: Monitors processes for cleanup and error handling
- **Configuration Maps**: All modules use maps for configuration with sensible defaults
- **Logging**: Comprehensive logging for debugging and monitoring
- **Error Handling**: Consistent error handling across all modules

### Performance Characteristics
- **MCP Relay**: O(n) backend selection, O(1) request forwarding
- **Distributed Lock**: O(1) acquisition, O(log n) priority queuing
- **Message Queue**: O(1) enqueue/dequeue (ETS), O(n) timeout checking
- **Workflow Engine**: O(n) step validation, O(1) state monitoring

### Use Cases
- **MCP Relay**: Load balancing, backend service proxying
- **Distributed Lock**: Database operations, file locking, resource coordination
- **Message Queue**: Task queuing, asynchronous processing, event handling
- **Workflow Engine**: ETL processes, multi-step data processing, automation

These modules provide a robust foundation for building distributed systems with erlmcp, offering enterprise-grade reliability, scalability, and observability.