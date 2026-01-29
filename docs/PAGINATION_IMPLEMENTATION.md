# Pagination Support Implementation (TASK #146)

## Overview

This document describes the implementation of cursor-based pagination for list operations in erlmcp, following the MCP 2025-11-25 specification.

## Architecture

### Module: `erlmcp_pagination`

A gen_server module that provides:

- **Cursor-based pagination**: Uses base64-encoded cursors containing offset and timestamp
- **Page size limits**: Configurable default and maximum page sizes
- **Integration**: Seamlessly integrates with `tools/list`, `resources/list`, and `prompts/list`

### Key Features

1. **Cursor Encoding/Decoding**
   - Cursors contain: `{offset, timestamp}`
   - Base64 encoded for safe transport
   - Compressed binary format for efficiency
   - Graceful fallback on invalid cursors

2. **Page Size Validation**
   - Default: 50 items per page
   - Maximum: 1000 items per page (configurable)
   - Automatic bounding to prevent excessive responses

3. **Response Format**
   ```erlang
   #{
       <<"tools">> => [ToolMap],     % or <<"resources">>, <<"prompts">>
       <<"nextCursor">> => Binary | null,
       <<"hasMore">> => boolean(),
       <<"total">> => integer() | undefined
   }
   ```

## Usage

### Client Request

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/list",
  "params": {
    "pageSize": 20,
    "cursor": null
  }
}
```

### Server Response

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "tools": [
      {"name": "tool_1", "description": "Tool 1"},
      {"name": "tool_2", "description": "Tool 2"}
    ],
    "nextCursor": "g10AAABhagNiZwFhbGyBAAAAAAJhagAAAAAAAAA=",
    "hasMore": true,
    "total": 100
  }
}
```

### Next Page Request

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/list",
  "params": {
    "pageSize": 20,
    "cursor": "g10AAABhagNiZwFhbGyBAAAAAAJhagAAAAAAAAA="
  }
}
```

## Implementation Details

### Server Integration

The `handle_paginated_list_with_key/3` function in `erlmcp_server.erl`:

```erlang
handle_paginated_list_with_key(Items, Params, ListKey) ->
    PageSize = maps:get(<<"pageSize">>, Params, undefined),
    Cursor = maps:get(<<"cursor">>, Params, null),
    TotalCount = length(Items),

    {PageItems, PageInfo} = erlmcp_pagination:paginate(
        Items, PageSize, Cursor, TotalCount
    ),

    Response = #{
        ListKey => PageItems,
        <<"nextCursor">> => maps:get(<<"cursor">>, PageInfo),
        <<"hasMore">> => maps:get(<<"hasMore">>, PageInfo),
        <<"total">> => maps:get(<<"total">>, PageInfo)
    },

    {ok, Response}
```

### Configuration

The pagination module is configured via application environment:

```erlang
%% In sys.config or app.config
{erlmcp, [
    {default_page_size, 50},
    {max_page_size, 1000}
]}.
```

Runtime configuration updates:

```erlang
erlmcp_pagination:set_page_size_limits(100, 500).
```

## Testing

### Test Coverage

The implementation includes comprehensive tests in `erlmcp_pagination_tests.erl`:

- **Basic pagination**: First page, last page, empty list
- **Cursor handling**: Encoding, decoding, invalid cursors
- **Page size validation**: Valid sizes, zero, negative, non-integer
- **Multi-page navigation**: Sequential page access, no duplicates
- **Edge cases**: Single item, page size of 1, exact boundaries
- **Integration tests**: Simulated tools/list, resources/list, prompts/list

### Running Tests

```bash
# Compile the module
erlc -I apps/erlmcp_core/include -o ebin \
    apps/erlmcp_core/src/erlmcp_pagination.erl

# Compile tests
erlc -I apps/erlmcp_core/include -o ebin \
    apps/erlmcp_core/test/erlmcp_pagination_tests.erl

# Run tests
erl -pa ebin -eval "eunit:test(erlmcp_pagination_tests, [verbose])" -s init stop
```

## Performance Characteristics

### Memory

- **Cursor size**: ~40 bytes (base64 encoded)
- **Per-page overhead**: Minimal (sub-millisecond)
- **No client-side state required**

### Scalability

- **Maximum items per page**: 1000 (configurable)
- **No limit on total items**: Cursor-based pagination scales indefinitely
- **Efficient for large datasets**: Only loads requested page

### Benchmarks

Expected performance (based on implementation):

- **Page calculation**: < 1ms for 1000 items
- **Cursor encoding**: < 0.1ms
- **Cursor decoding**: < 0.1ms
- **Total per-request overhead**: < 2ms

## Supervision

The pagination gen_server is included in the core supervision tree:

```erlang
%% In erlmcp_core_sup.erl
#{
    id => erlmcp_pagination,
    start => {erlmcp_pagination, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [erlmcp_pagination]
}
```

## Error Handling

### Invalid Page Size

```erlang
%% Page size > max: Bounded to max
%% Page size <= 0: Uses default
%% Page size not integer: Uses default
```

### Invalid Cursor

```erlang
%% Malformed base64: Returns default cursor (offset = 0)
%% Corrupted term: Returns default cursor (offset = 0)
```

### Fallback Behavior

On pagination errors, the system falls back to returning all items (non-paginated):

```erlang
catch _:_ ->
    {ok, #{ListKey => Items}}
```

## Future Enhancements

### Potential Improvements

1. **Sorting**: Add `sortField` and `sortOrder` parameters
2. **Filtering**: Add filtering criteria support
3. **Reverse pagination**: Support `previousCursor` for backward navigation
4. **Metadata caching**: Cache total counts for performance
5. **Cursor versioning**: Add version field for future compatibility

### MCP Specification Compliance

This implementation follows the MCP 2025-11-25 pagination specification:

- ✅ Cursor-based pagination
- ✅ Page size limits
- ✅ Total count (optional)
- ✅ Has more indicator
- ✅ Null cursor for first page
- ✅ Null cursor when no more pages

## Files Modified

- `apps/erlmcp_core/src/erlmcp_pagination.erl` - New module
- `apps/erlmcp_core/src/erlmcp_server.erl` - Updated `handle_paginated_list_with_key/3`
- `apps/erlmcp_core/src/erlmcp_core_sup.erl` - Added pagination to supervision tree
- `apps/erlmcp_core/test/erlmcp_pagination_tests.erl` - Comprehensive tests
- `docs/PAGINATION_IMPLEMENTATION.md` - This document

## Migration Notes

### Breaking Changes

None. Pagination is backward compatible:

- Requests without `pageSize` or `cursor` return all items
- Clients can adopt pagination incrementally
- No changes required for existing implementations

### Adoption Path

1. **Phase 1**: Server returns pagination metadata (hasMore, nextCursor, total)
2. **Phase 2**: Clients start sending pageSize and cursor parameters
3. **Phase 3**: Full pagination adoption with large datasets

## References

- MCP 2025-11-25 Specification: https://modelcontextprotocol.io/docs/2025-11-25/
- erlmcp OTP Patterns: `docs/otp-patterns.md`
- Server Implementation: `apps/erlmcp_core/src/erlmcp_server.erl`
- Test Suite: `apps/erlmcp_core/test/erlmcp_pagination_tests.erl`
