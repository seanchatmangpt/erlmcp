# Batch Request Integration Guide

## Quick Start

Batch request support is now available in erlmcp. Use these functions to process batches:

### API Functions

```erlang
%% Detect if message is batch
is_batch_request(Json :: binary()) -> boolean()

%% Decode batch into list of messages
decode_batch(Json :: binary()) -> {ok, [Message]} | {error, Reason}

%% Encode list of messages as batch
encode_batch(Messages :: [Message]) -> binary()
```

### Types

```erlang
-type json_rpc_message() ::
    #json_rpc_request{}
    | #json_rpc_response{}
    | #json_rpc_notification{}.

-type batch_request() :: [json_rpc_message()].

-type batch_decode_result() ::
    {ok, batch_request()}
    | {error, {atom(), term()}}.
```

## Usage Pattern

### 1. Check if Batch

```erlang
case erlmcp_json_rpc:is_batch_request(JsonData) of
    true  -> process_batch(JsonData);
    false -> process_single(JsonData)
end
```

### 2. Decode Batch

```erlang
{ok, Messages} = erlmcp_json_rpc:decode_batch(JsonData),

%% Messages is list of:
%% - #json_rpc_request{id=Id, method=Method, params=Params}
%% - #json_rpc_notification{method=Method, params=Params}
```

### 3. Process Each Message

```erlang
process_batch(Messages, State) ->
    Responses = [process_message(Msg, State) || Msg <- Messages],
    %% Filter out notifications (which generate no response)
    ValidResponses = lists:filter(fun has_id/1, Responses),
    ValidResponses
end.

has_id(#json_rpc_response{id = null}) -> false;
has_id(#json_rpc_response{id = undefined}) -> false;
has_id(#json_rpc_response{}) -> true.
```

### 4. Encode Response Batch

```erlang
ResponseJson = erlmcp_json_rpc:encode_batch(Responses)
```

## Real World Example

### Client sends batch

```json
[
  {"jsonrpc": "2.0", "id": 1, "method": "resources/list"},
  {"jsonrpc": "2.0", "id": 2, "method": "tools/list"},
  {"jsonrpc": "2.0", "method": "ping"}
]
```

### Server processes

```erlang
handle_transport_data(Data, State) ->
    case erlmcp_json_rpc:is_batch_request(Data) of
        true ->
            {ok, Messages} = erlmcp_json_rpc:decode_batch(Data),
            Responses = process_batch_messages(Messages, State),
            BatchJson = erlmcp_json_rpc:encode_batch(Responses),
            send_transport_response(BatchJson);
        false ->
            {ok, Message} = erlmcp_json_rpc:decode_message(Data),
            Response = process_single_message(Message, State),
            send_transport_response(Response)
    end.

process_batch_messages(Messages, State) ->
    [process_single_message(Msg, State) || Msg <- Messages].

process_single_message(#json_rpc_request{id = Id, method = Method, params = Params}, State) ->
    Result = call_method(Method, Params, State),
    #json_rpc_response{id = Id, result = Result};
process_single_message(#json_rpc_notification{method = Method, params = Params}, State) ->
    %% Notifications don't generate responses
    handle_notification(Method, Params, State),
    undefined.
```

### Server responds

```json
[
  {"jsonrpc": "2.0", "id": 1, "result": {...resources...}},
  {"jsonrpc": "2.0", "id": 2, "result": {...tools...}}
]
```

Note: No response for notification (id=null)

## Error Handling

### Empty Batch
```erlang
{error, {invalid_request, empty_batch}} =
    erlmcp_json_rpc:decode_batch(<<"[]">>)
```

### Invalid JSON
```erlang
{error, {parse_error, invalid_json}} =
    erlmcp_json_rpc:decode_batch(<<"invalid">>)
```

### Single Request Error
If one request in batch has error, others still process:

```erlang
%% Batch with invalid request
Batch = [
    #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"m1">>},
    #{<<"id">> => 2, <<"method">> => <<"m2">>}  %% Missing jsonrpc
],

%% Still gets first message processed
{ok, [Msg1]} = erlmcp_json_rpc:decode_batch(jsx:encode(Batch))
```

## Performance Tips

### 1. Batch Size
- Recommended: 2-50 requests per batch
- Maximum: Limited by message size (default 16MB)
- Optimal: 5-10 requests balances latency/throughput

### 2. Response Filtering
```erlang
%% Efficient notification filtering
Responses = lists:filtermap(fun
    (#json_rpc_notification{}) -> false;
    (Response) -> {true, Response}
end, AllResponses)
```

### 3. Large Batches
For batches > 100 requests:
- Process in concurrent chunks
- Use separate processes per transport
- Monitor response time

## Integration Checklist

- [ ] Transport layer detects batch (is_batch_request/1)
- [ ] Batch decoding implemented (decode_batch/1)
- [ ] Message processing loop handles both types
- [ ] Response filtering for notifications
- [ ] Response batch encoding (encode_batch/1)
- [ ] Error handling for empty batches
- [ ] Tests added for batch processing
- [ ] Performance validated for expected batch sizes

## Testing Batch Support

```bash
# Run batch tests
rebar3 eunit --module=erlmcp_batch_request_tests -v

# Test with manual batch
cat > test_batch.erl << 'EOF'
test_batch() ->
    Batch = [
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"m1">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"method">> => <<"m2">>}
    ],
    Json = jsx:encode(Batch),
    {ok, Messages} = erlmcp_json_rpc:decode_batch(Json),
    io:format("Decoded ~w messages~n", [length(Messages)]).
EOF

erl -pa _build/default/lib/*/ebin -noshell -s erlmcp_json_rpc -eval "test_batch(), halt()."
```

## Troubleshooting

### Issue: `decode_batch` returns error for valid batch
**Solution**: Ensure JSON is valid array, not object. Use `is_batch_request/1` first.

### Issue: Batch response order incorrect
**Solution**: encode_batch/1 preserves input order. Check request processing order.

### Issue: Notifications in response array
**Solution**: Filter notifications before encode_batch/1. Notifications have no id.

### Issue: Performance degradation with large batches
**Solution**: Process batches <= 50 requests. Split larger batches.

## Migration from Single Messages

### Before: Single Message Handling
```erlang
{ok, Message} = erlmcp_json_rpc:decode_message(Data),
Response = erlmcp_json_rpc:encode_response(Id, Result)
```

### After: Batch-Aware Handler
```erlang
case erlmcp_json_rpc:is_batch_request(Data) of
    true ->
        {ok, Messages} = erlmcp_json_rpc:decode_batch(Data),
        Responses = [process_msg(M) || M <- Messages],
        erlmcp_json_rpc:encode_batch(Responses);
    false ->
        {ok, Message} = erlmcp_json_rpc:decode_message(Data),
        erlmcp_json_rpc:encode_response(Id, process_msg(Message))
end
```

## Specification Compliance

See `/Users/sac/erlmcp/docs/GAP_43_BATCH_REQUEST_HANDLING.md` for detailed specification compliance.

## Support

For issues or questions:
1. Check test cases: `test/erlmcp_batch_request_tests.erl`
2. Review integration guide above
3. Check specification docs

---

**Feature**: Batch Request Handling (Gap #43)
**Status**: Production Ready
**Last Updated**: 2026-01-27
