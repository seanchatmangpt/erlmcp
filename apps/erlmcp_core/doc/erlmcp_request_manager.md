# erlmcp_request_manager - Sophisticated Request Management System

## Overview

`erlmcp_request_manager` is an enterprise-grade request management module that provides sophisticated tracking, prioritization, and processing of MCP (Model Context Protocol) requests. Built using Chicago TDD methodology with comprehensive test coverage.

## Features

### Core Functionality
- **Request Tracking**: Complete lifecycle management with detailed status tracking
- **Priority-Based Processing**: Priority queue ensuring important requests get processed first
- **Timeout Management**: Configurable timeouts with automatic expiration handling
- **Request Cancellation**: Support for cancelling individual or multiple requests
- **Concurrent Processing**: Handles multiple concurrent requests with configurable limits

### Advanced Features
- **Result Caching**: Intelligent caching with TTL to improve performance
- **Request Deduplication**: Automatic detection and prevention of duplicate requests
- **Comprehensive Monitoring**: Real-time metrics and monitoring capabilities
- **Error Recovery**: Robust error handling and recovery mechanisms
- **Client Process Monitoring**: Automatic cleanup when clients terminate

## Architecture

### State Management
The module uses ETS tables for efficient state management:

```erlang
% Main ETS tables
- requests:        % request_id -> #request_info{}
- cache:           % request_id -> #cache_entry{}
- priority_queue:   % priority -> queue of request_ids
- metrics:         % metrics counters
- monitors:        % request_id -> client_pid
```

### Data Types

```erlang
%% Request Information
-record(request_info, {
    id :: pos_integer(),                    % Unique request identifier
    client_pid :: pid(),                   % Client process
    method :: binary(),                     % MCP method
    params :: map(),                       % Request parameters
    priority :: pos_integer(),              % Priority (lower = higher priority)
    status :: pending | processing | completed | failed | cancelled | timeout,
    timestamp :: integer(),                % Creation timestamp
    timeout_ms :: pos_integer(),           % Timeout in milliseconds
    expiration :: integer(),               % Expiration timestamp
    result :: term() | undefined,          % Request result
    error :: term() | undefined,           % Error information
    retry_count :: non_neg_integer(),       % Retry attempts
    metadata :: map()                      % Additional metadata
}).

%% Cache Entry
-record(cache_entry, {
    key :: pos_integer(),                  % Request ID
    value :: term(),                       Cached result
    timestamp :: integer(),                % Cache creation time
    expiration :: integer()                % Cache expiration time
}).

%% Metrics
-record(metrics, {
    total_requests :: non_neg_integer(),    % Total requests received
    active_requests :: non_neg_integer(),   % Currently active requests
    pending_requests :: non_neg_integer(),  % Pending requests
    completed_requests :: non_neg_integer(),% Successfully completed
    failed_requests :: non_neg_integer(),   % Failed requests
    cancelled_requests :: non_neg_integer(),% Cancelled requests
    timeout_requests :: non_neg_integer(),   % Timeout requests
    cache_hits :: non_neg_integer(),        % Cache hits
    cache_misses :: non_neg_integer(),      % Cache misses
    average_processing_time :: float()      % Average processing time
}).
```

## API Reference

### Request Management

#### register_request/3
```erlang
-spec register_request(request_id(), request_data(), client_pid()) -> ok | {error, term()}.
```
Register a new request with default timeout.

#### register_request/4
```erlang
-spec register_request(request_id(), request_data(), client_pid(), pos_integer()) -> ok | {error, term()}.
```
Register a new request with custom timeout.

#### get_request/1
```erlang
-spec get_request(request_id()) -> {ok, request_info()} | {error, term()}.
```
Get request information with default timeout.

#### get_request/2
```erlang
-spec get_request(request_id(), timeout()) -> {ok, request_info()} | {error, term()}.
```
Get request information with custom timeout.

#### update_request_status/2
```erlang
-spec update_request_status(request_id(), request_status()) -> ok | {error, term()}.
```
Update request status.

#### update_request_status/3
```erlang
-spec update_request_status(request_id(), request_status(), map()) -> ok | {error, term()}.
```
Update request status with metadata.

#### complete_request/2
```erlang
-spec complete_request(request_id(), term()) -> ok | {error, term()}.
```
Complete a request with result.

#### complete_request/3
```erlang
-spec complete_request(request_id(), term(), map()) -> ok | {error, term()}.
```
Complete a request with result and metadata.

#### cancel_request/1
```erlang
-spec cancel_request(request_id()) -> ok | {error, term()}.
```
Cancel a request.

#### cancel_request/2
```erlang
-spec cancel_request(request_id(), map()) -> ok | {error, term()}.
```
Cancel a request with metadata.

#### remove_request/1
```erlang
-spec remove_request(request_id()) -> ok | {error, term()}.
```
Remove a request from the system.

### Batch Operations

#### register_requests/2
```erlang
-spec register_requests([{request_id(), request_data()}], client_pid()) -> ok | {error, term()}.
```
Register multiple requests.

#### register_requests/3
```erlang
-spec register_requests([{request_id(), request_data()}], client_pid(), pos_integer()) -> ok | {error, term()}.
```
Register multiple requests with custom timeout.

#### complete_requests/2
```erlang
-spec complete_requests([{request_id(), term()}]) -> ok | {error, term()}.
```
Complete multiple requests.

#### cancel_requests/1
```erlang
-spec cancel_requests([request_id()]) -> ok | {error, term()}.
```
Cancel multiple requests.

#### clear_all_requests/0
```erlang
-spec clear_all_requests() -> ok.
```
Clear all requests from the system.

### Priority and Queueing

#### get_priority_queue/0
```erlang
-spec get_priority_queue() -> [request_id()].
```
Get the priority queue (ordered by priority).

#### get_next_request/0
```erlang
-spec get_next_request() -> {ok, request_info()} | {error, term()}.
```
Get the next request from the priority queue.

#### get_next_request/1
```erlang
-spec get_next_request(timeout()) -> {ok, request_info()} | {error, term()}.
```
Get the next request with custom timeout.

### Caching and Deduplication

#### get_cached_result/1
```erlang
-spec get_cached_result(request_id()) -> {ok, term()} | {error, not_found}.
```
Get cached result for a request.

#### cache_result/2
```erlang
-spec cache_result(request_id(), term()) -> ok | {error, term()}.
```
Cache a result for a request.

#### clear_cache/0
```erlang
-spec clear_cache() -> ok.
```
Clear all cached results.

#### is_duplicate_request/3
```erlang
-spec is_duplicate_request(request_id(), request_data(), client_pid()) -> boolean() | {error, term()}.
```
Check if a request is a duplicate.

### Monitoring and Metrics

#### get_metrics/0
```erlang
-spec get_metrics() -> metrics().
```
Get current metrics.

#### get_active_requests/0
```erlang
-spec get_active_requests() -> [request_info()].
```
Get active requests (pending and processing).

#### get_pending_requests/0
```erlang
-spec get_pending_requests() -> [request_info()].
```
Get pending requests.

#### get_completed_requests/0
```erlang
-spec get_completed_requests() -> [request_info()].
```
Get completed requests.

#### get_failed_requests/0
```erlang
-spec get_failed_requests() -> [request_info()].
```
Get failed requests.

### Configuration

#### set_default_timeout/1
```erlang
-spec set_default_timeout(pos_integer()) -> ok.
```
Set default timeout for requests.

#### get_default_timeout/0
```erlang
-spec get_default_timeout() -> pos_integer().
```
Get default timeout.

#### set_max_concurrent/1
```erlang
-spec set_max_concurrent(pos_integer()) -> ok.
```
Set maximum concurrent requests.

#### get_max_concurrent/0
```erlang
-spec get_max_concurrent() -> pos_integer().
```
Get maximum concurrent requests.

#### set_cache_ttl/1
```erlang
-spec set_cache_ttl(pos_integer()) -> ok.
```
Set cache TTL.

#### get_cache_ttl/0
```erlang
-spec get_cache_ttl() -> pos_integer().
```
Get cache TTL.

## Usage Examples

### Basic Request Management
```erlang
%% Register a new request
RequestData = #{
    method => <<"tools/call">>,
    params => #{name => "file_search", arguments => #{path => "/tmp"}},
    priority => 5,
    metadata => #{user_id => "12345"}
},
ok = erlmcp_request_manager:register_request(123, RequestData, self(), 30000).

%% Get request information
{ok, RequestInfo} = erlmcp_request_manager:get_request(123).

%% Update status to processing
ok = erlmcp_request_manager:update_request_status(123, processing).

%% Complete the request
Result = #{success => true, data => ["/tmp/file1.txt", "/tmp/file2.txt"]},
ok = erlmcp_request_manager:complete_request(123, Result).
```

### Priority Processing
```erlang
%% Register requests with different priorities
HighPriorityRequest = #{method => <<"tools/call">>, params => #{}, priority => 1},
LowPriorityRequest = #{method => <<"tools/call">>, params => #{}, priority => 10},

ok = erlmcp_request_manager:register_request(1, HighPriorityRequest, self(), 30000),
ok = erlmcp_request_manager:register_request(2, LowPriorityRequest, self(), 30000).

%% Get next request (should be high priority)
{ok, NextRequest} = erlmcp_request_manager:get_next_request(),
%% Request ID 1 should be returned first
```

### Batch Operations
```erlang
%% Register multiple requests at once
Requests = [
    {101, #{method => <<"tools/call">>, params => #{action => "list"}}},
    {102, #{method => <<"tools/call">>, params => #{action => "get"}}}
],
ok = erlmcp_request_manager:register_requests(Requests, self(), 30000).

%% Complete multiple requests
Results = [
    {101, #{success => true, data => [1, 2, 3]}},
    {102, #{success => true, data => <<"file content">>}}
],
ok = erlmcp_request_manager:complete_requests(Results).
```

### Caching and Deduplication
```erlang
%% Check for duplicate requests
IsDuplicate = erlmcp_request_manager:is_duplicate_request(123, RequestData, self()),
case IsDuplicate of
    false ->
        %% Register the request
        ok = erlmcp_request_manager:register_request(123, RequestData, self(), 30000);
    true ->
        %% Skip duplicate
        ok
end.

%% Cache and retrieve results
ok = erlmcp_request_manager:cache_result(123, #{result => "cached data"}),
{ok, CachedData} = erlmcp_request_manager:get_cached_result(123).
```

### Monitoring and Metrics
```erlang
%% Get current metrics
Metrics = erlmcp_request_manager:get_metrics(),
io:format("Total requests: ~p~n", [Metrics#metrics.total_requests]),
io:format("Completed requests: ~p~n", [Metrics#metrics.completed_requests]),
io:format("Cache hits: ~p~n", [Metrics#metrics.cache_hits]).

%% Monitor active requests
ActiveRequests = erlmcp_request_manager:get_active_requests(),
io:format("Active requests: ~p~n", [length(ActiveRequests)]).
```

## Configuration

The module accepts the following configuration options:

```erlang
Config = #{
    default_timeout => 30000,      % Default timeout in milliseconds
    max_concurrent => 100,         % Maximum concurrent requests
    cache_ttl => 300000,           % Cache TTL in milliseconds (5 minutes)
    cleanup_interval => 60000      % Cleanup interval in milliseconds (1 minute)
},
ok = erlmcp_request_manager:start_link(Config).
```

## Error Handling

The module provides comprehensive error handling:

### Common Error Codes
- `{error, invalid_request_id}` - Invalid request ID format
- `{error, duplicate_request}` - Request already exists
- `{error, max_concurrent_limit}` - Maximum concurrent requests reached
- `{error, not_found}` - Request not found
- `{error, timeout}` - Request timeout
- `{error, cancelled}` - Request cancelled
- `{error, database_error}` - Database operation failed

### Error Recovery
- Automatic cleanup of expired requests
- Client process monitoring and cleanup
- Graceful handling of system overload

## Performance Considerations

### Optimization Features
- **ETS Tables**: Fast in-memory storage with concurrent access
- **Priority Queues**: Efficient priority-based processing
- **Caching**: Reduces redundant processing
- **Batch Operations**: Minimize round-trips for bulk operations

### Monitoring and Metrics
The module provides comprehensive metrics for performance monitoring:
- Request processing times
- Cache hit/miss ratios
- Error rates
- Queue lengths
- Resource utilization

## Testing

The module includes comprehensive test coverage with Chicago TDD methodology:

### Test Coverage
- **100% API coverage**: All public functions tested
- **Error conditions**: All error scenarios tested
- **Concurrent operations**: Multi-process scenarios tested
- **Performance**: Load testing for high-volume scenarios
- **Integration**: Integration with other erlmcp components

### Running Tests
```erl
% Run all tests
erlmcp_request_manager_tests:request_manager_test_().

% Run specific test categories
eunit:test(erlmcp_request_manager_tests).
```

## Dependencies

- Erlang/OTP 26+
- `eunit` (for testing)
- `erlmcp.hrl` (for type definitions and constants)

## Integration

The module integrates seamlessly with the erlmcp ecosystem:

- **MCP Protocol**: Full compliance with MCP 2025-11-25 specification
- **Supervisor Integration**: Proper supervision and lifecycle management
- **Monitoring Integration**: Compatible with erlmcp monitoring systems
- **Security**: Follows erlmcp security best practices

## Monitoring and Observability

### Built-in Metrics
The module tracks comprehensive metrics:

- Request counts (total, active, completed, failed, cancelled, timeout)
- Cache performance (hits, misses, effectiveness)
- Processing times (average, percentiles)
- Queue performance (length, wait times)

### Integration with erlmcp Monitoring
- Metrics are automatically exported to erlmcp monitoring systems
- Compatible with observability tools like OpenTelemetry
- Real-time monitoring through API endpoints

## Future Enhancements

### Planned Features
- **Request Prioritization**: Dynamic priority adjustment based on load
- **Request Routing**: Intelligent routing based on request characteristics
- **Circuit Breakers**: Integration with erlmcp circuit breaker system
- **Rate Limiting**: Per-client and global rate limiting
- **Distributed Processing**: Multi-node request distribution

### Performance Optimizations
- **Lazy Loading**: On-demand resource allocation
- **Batch Optimization**: Intelligent batching of similar requests
- **Cache Strategies**: Advanced cache invalidation and warming
- **Connection Pooling**: Efficient resource reuse

## Troubleshooting

### Common Issues

#### Request Not Found
```erlang
% Check if request exists
case erlmcp_request_manager:get_request(RequestId) of
    {error, not_found} ->
        % Request may have expired or been cancelled
        io:format("Request ~p not found~n", [RequestId]);
    {ok, RequestInfo} ->
        % Request exists
        ok
end.
```

#### Concurrent Limit Reached
```erlang
% Check current metrics
Metrics = erlmcp_request_manager:get_metrics(),
MaxConcurrent = erlmcp_request_manager:get_max_concurrent(),

if Metrics#metrics.active_requests >= MaxConcurrent ->
    % System at capacity, consider scaling or reducing load
    io:format("System at capacity: ~p/~p concurrent requests~n",
              [Metrics#metrics.active_requests, MaxConcurrent]);
true ->
    % System has capacity
    ok
end.
```

#### Cache Performance
```erlang
% Check cache effectiveness
Metrics = erlmcp_request_manager:get_metrics(),
CacheHitRate = case (Metrics#metrics.cache_hits + Metrics#metrics.cache_misses) of
    0 -> 0;
    Total -> Metrics#metrics.cache_hits / Total * 100
end,
io:format("Cache hit rate: ~.2f%~n", [CacheHitRate]).
```

## License

This module is part of the erlmcp project and is licensed under the Apache 2.0 license.