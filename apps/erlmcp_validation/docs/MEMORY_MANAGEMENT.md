# Memory Management for Validation Framework

## Overview

The `erlmcp_memory_manager` module provides comprehensive memory management for the MCP specification validation framework.

## Features

1. **LRU Specification Cache** - Automatic eviction of least recently used specs
2. **Memory Monitoring** - Continuous tracking of memory usage and pressure
3. **Garbage Collection Optimization** - Targeted GC for validation processes
4. **Memory Pressure Management** - Automatic cleanup based on usage levels

## Quick Start

```erlang
%% Start memory manager
{ok, Pid} = erlmcp_memory_manager:start_link().

%% Cache a specification
{ok, SpecId} = erlmcp_memory_manager:cache_spec(ParsedSpec).

%% Retrieve cached specification
{ok, CachedSpec} = erlmcp_memory_manager:get_cached_spec(SpecId).

%% Check memory pressure
Pressure = erlmcp_memory_manager:check_memory_pressure().
%% => low | medium | high | critical
```

## API Reference

### Cache Management
- `cache_spec/1,2` - Cache parsed specifications
- `get_cached_spec/1` - Retrieve cached specs
- `purge_cache/0,1` - Clear cache entries

### Memory Monitoring
- `get_memory_usage/0` - Get current memory statistics
- `check_memory_pressure/0` - Check pressure level

### Memory Optimization
- `optimize_memory_usage/1` - Optimize based on pressure level
- `force_garbage_collection/0` - Force garbage collection

## Configuration

```erlang
{ok, Pid} = erlmcp_memory_manager:start_link(#{
    max_cache_size => 100,           % Maximum cached specs
    max_spec_memory => 100000000,     % 100MB per spec
    memory_limit => 2000000000        % 2GB total limit
}).
```
