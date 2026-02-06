# Error Handling Patterns

## Overview

This document provides comprehensive patterns and best practices for handling errors in erlmcp applications. Proper error handling is critical for building robust MCP servers and clients.

---

## Error Response Format

All erlmcp errors follow the JSON-RPC 2.0 error response format:

```erlang
%% Success response
{ok, Result}

%% Error response
{error, Reason}
```

Where `Reason` can be:

- `{jsonrpc_error, Code, Message, Data}` - JSON-RPC protocol error
- `{transport_error, Reason}` - Transport layer error
- `{timeout, Milliseconds}` - Request timeout
- `{validation_error, Details}` - Input validation error
- `atom()` - System error (enoent, eacces, etc.)

---

## Server-Side Error Handling

### Tool Handler Errors

**Pattern 1: Explicit Error Returns**

```erlang
erlmcp_server:add_tool(Server, #{
    name => <<"divide">>,
    description => <<"Divide two numbers">>,
    input_schema => #{
        type => object,
        properties => #{
            a => #{type => number},
            b => #{type => number}
        },
        required => [a, b]
    },
    handler => fun(Args) ->
        A = maps:get(<<"a">>, Args),
        B = maps:get(<<"b">>, Args),

        case B of
            0 ->
                %% Return error result
                #{
                    content => [#{
                        type => text,
                        text => <<"Error: Division by zero">>
                    }],
                    isError => true
                };
            _ ->
                %% Return success result
                Result = A / B,
                #{
                    content => [#{
                        type => text,
                        text => float_to_binary(Result, [{decimals, 4}])
                    }]
                }
        end
    end
}).
```

**Pattern 2: Exception Handling**

```erlang
erlmcp_server:add_tool(Server, #{
    name => <<"read_file">>,
    description => <<"Read a file">>,
    handler => fun(Args) ->
        Path = maps:get(<<"path">>, Args),

        try
            {ok, Content} = file:read_file(Path),
            #{content => [#{type => text, text => Content}]}
        catch
            error:{badmatch, {error, enoent}} ->
                #{
                    content => [#{
                        type => text,
                        text => <<"File not found: ", Path/binary>>
                    }],
                    isError => true
                };
            error:{badmatch, {error, eacces}} ->
                #{
                    content => [#{
                        type => text,
                        text => <<"Permission denied: ", Path/binary>>
                    }],
                    isError => true
                };
            Class:Reason:Stacktrace ->
                logger:error("Tool execution failed: ~p:~p~n~p",
                             [Class, Reason, Stacktrace]),
                #{
                    content => [#{
                        type => text,
                        text => <<"Internal error occurred">>
                    }],
                    isError => true
                }
        end
    end
}).
```

**Pattern 3: Result Tuples**

```erlang
erlmcp_server:add_tool(Server, #{
    name => <<"query_database">>,
    description => <<"Query database">>,
    handler => fun(Args) ->
        Query = maps:get(<<"query">>, Args),

        case execute_query(Query) of
            {ok, Rows} ->
                #{content => [#{
                    type => text,
                    text => jsx:encode(Rows)
                }]};
            {error, {invalid_sql, Msg}} ->
                #{
                    content => [#{
                        type => text,
                        text => <<"Invalid SQL: ", Msg/binary>>
                    }],
                    isError => true
                };
            {error, timeout} ->
                #{
                    content => [#{
                        type => text,
                        text => <<"Query timeout">>
                    }],
                    isError => true
                };
            {error, Reason} ->
                #{
                    content => [#{
                        type => text,
                        text => io_lib:format("Database error: ~p", [Reason])
                    }],
                    isError => true
                }
        end
    end
}).
```

### Resource Handler Errors

```erlang
erlmcp_server:add_resource(Server, #{
    uri => <<"file:///config/app.json">>,
    name => <<"Application Config">>,
    handler => fun(Uri) ->
        Path = uri_to_path(Uri),

        try
            case file:read_file(Path) of
                {ok, Content} ->
                    %% Validate JSON
                    case jsx:decode(Content, [return_maps]) of
                        Config when is_map(Config) ->
                            #{content => [#{type => text, text => Content}]};
                        _ ->
                            #{
                                content => [#{
                                    type => text,
                                    text => <<"Invalid JSON format">>
                                }],
                                isError => true
                            }
                    end;
                {error, enoent} ->
                    #{
                        content => [#{
                            type => text,
                            text => <<"Resource not found">>
                        }],
                        isError => true
                    };
                {error, Reason} ->
                    #{
                        content => [#{
                            type => text,
                            text => io_lib:format("Read error: ~p", [Reason])
                        }],
                        isError => true
                    }
            end
        catch
            error:badarg ->
                #{
                    content => [#{
                        type => text,
                        text => <<"Invalid JSON content">>
                    }],
                    isError => true
                }
        end
    end
}).
```

### Validation Errors

```erlang
%% Custom validation in handler
erlmcp_server:add_tool(Server, #{
    name => <<"create_user">>,
    description => <<"Create a new user">>,
    input_schema => #{
        type => object,
        properties => #{
            email => #{type => string},
            age => #{type => integer}
        },
        required => [email, age]
    },
    handler => fun(Args) ->
        Email = maps:get(<<"email">>, Args),
        Age = maps:get(<<"age">>, Args),

        %% Additional validation beyond JSON schema
        case validate_user_input(Email, Age) of
            ok ->
                case create_user(Email, Age) of
                    {ok, UserId} ->
                        #{content => [#{
                            type => text,
                            text => <<"User created: ", UserId/binary>>
                        }]};
                    {error, duplicate_email} ->
                        #{
                            content => [#{
                                type => text,
                                text => <<"Email already exists">>
                            }],
                            isError => true
                        }
                end;
            {error, {invalid_email, Msg}} ->
                #{
                    content => [#{
                        type => text,
                        text => <<"Invalid email: ", Msg/binary>>
                    }],
                    isError => true
                };
            {error, {invalid_age, Msg}} ->
                #{
                    content => [#{
                        type => text,
                        text => <<"Invalid age: ", Msg/binary>>
                    }],
                    isError => true
                }
        end
    end
}).

validate_user_input(Email, Age) ->
    case validate_email(Email) of
        ok ->
            case Age of
                N when N >= 0, N =< 150 -> ok;
                _ -> {error, {invalid_age, <<"Age must be between 0 and 150">>}}
            end;
        {error, Reason} ->
            {error, {invalid_email, Reason}}
    end.
```

---

## Client-Side Error Handling

### Basic Pattern

```erlang
case erlmcp_client:call_tool(Client, <<"my_tool">>, Args) of
    {ok, Result} ->
        handle_success(Result);
    {error, {jsonrpc_error, Code, Message, Data}} ->
        handle_jsonrpc_error(Code, Message, Data);
    {error, {transport_error, Reason}} ->
        handle_transport_error(Reason);
    {error, {timeout, Timeout}} ->
        handle_timeout(Timeout);
    {error, Reason} ->
        handle_unknown_error(Reason)
end.
```

### Comprehensive Error Handler

```erlang
-module(mcp_error_handler).
-export([handle_error/1, retry_on_error/3]).

handle_error({jsonrpc_error, Code, Message, Data}) ->
    case Code of
        -32700 ->
            logger:error("Parse error: ~s", [Message]),
            {error, parse_error};

        -32600 ->
            logger:error("Invalid request: ~s", [Message]),
            {error, invalid_request};

        -32601 ->
            logger:warning("Method not found: ~s", [Message]),
            {error, method_not_found};

        -32602 ->
            logger:warning("Invalid params: ~s", [Message]),
            {error, invalid_params};

        -32002 ->
            ToolName = maps:get(<<"tool">>, Data, <<"unknown">>),
            logger:warning("Tool not found: ~s", [ToolName]),
            {error, {tool_not_found, ToolName}};

        -32009 ->
            logger:warning("Request timeout after ~p ms", [Data]),
            {error, timeout};

        -32010 ->
            RetryAfter = maps:get(<<"retry_after">>, Data, 60),
            logger:warning("Rate limited, retry after ~p seconds", [RetryAfter]),
            {error, {rate_limited, RetryAfter}};

        -32051 ->
            logger:error("Authentication failed: ~s", [Message]),
            {error, authentication_failed};

        _ ->
            logger:error("JSON-RPC error ~p: ~s~nData: ~p",
                        [Code, Message, Data]),
            {error, {jsonrpc_error, Code}}
    end;

handle_error({transport_error, closed}) ->
    logger:error("Connection closed"),
    {error, connection_closed};

handle_error({transport_error, Reason}) ->
    logger:error("Transport error: ~p", [Reason]),
    {error, {transport_error, Reason}};

handle_error({timeout, Timeout}) ->
    logger:warning("Request timeout after ~p ms", [Timeout]),
    {error, timeout};

handle_error(Reason) ->
    logger:error("Unknown error: ~p", [Reason]),
    {error, {unknown, Reason}}.

%% Retry with exponential backoff
retry_on_error(Fun, MaxRetries, InitialDelay) ->
    retry_on_error(Fun, MaxRetries, InitialDelay, 1).

retry_on_error(Fun, MaxRetries, Delay, Attempt) when Attempt =< MaxRetries ->
    case Fun() of
        {ok, Result} ->
            {ok, Result};
        {error, {rate_limited, RetryAfter}} ->
            logger:info("Rate limited, waiting ~p seconds", [RetryAfter]),
            timer:sleep(RetryAfter * 1000),
            retry_on_error(Fun, MaxRetries, Delay, Attempt + 1);
        {error, timeout} when Attempt < MaxRetries ->
            logger:info("Timeout on attempt ~p/~p, retrying in ~p ms",
                       [Attempt, MaxRetries, Delay]),
            timer:sleep(Delay),
            retry_on_error(Fun, MaxRetries, Delay * 2, Attempt + 1);
        {error, {transport_error, _}} when Attempt < MaxRetries ->
            logger:info("Transport error on attempt ~p/~p, retrying in ~p ms",
                       [Attempt, MaxRetries, Delay]),
            timer:sleep(Delay),
            retry_on_error(Fun, MaxRetries, Delay * 2, Attempt + 1);
        {error, Reason} ->
            logger:error("Failed after ~p attempts: ~p", [Attempt, Reason]),
            {error, {max_retries, Reason}}
    end;

retry_on_error(_Fun, MaxRetries, _Delay, Attempt) ->
    {error, {max_retries_exceeded, Attempt, MaxRetries}}.
```

### Pattern Matching on Errors

```erlang
call_tool_safely(Client, ToolName, Args) ->
    case erlmcp_client:call_tool(Client, ToolName, Args) of
        {ok, #{<<"content">> := Content, <<"isError">> := true}} ->
            %% Tool executed but returned an error
            ErrorMsg = extract_error_message(Content),
            {error, {tool_error, ErrorMsg}};

        {ok, #{<<"content">> := Content}} ->
            %% Tool succeeded
            {ok, Content};

        {error, {jsonrpc_error, -32002, _, _}} ->
            %% Tool not found, try alternative
            call_alternative_tool(Client, Args);

        {error, {jsonrpc_error, -32009, _, _}} ->
            %% Timeout, retry with progress token
            retry_with_progress(Client, ToolName, Args);

        {error, Reason} ->
            {error, Reason}
    end.

extract_error_message([#{<<"text">> := Text} | _]) -> Text;
extract_error_message(_) -> <<"Unknown error">>.
```

### Graceful Degradation

```erlang
get_data_with_fallback(Client) ->
    %% Try primary source
    case erlmcp_client:read_resource(Client, <<"mcp://primary/data">>) of
        {ok, Data} ->
            {ok, Data};
        {error, _} ->
            %% Fallback to secondary source
            logger:warning("Primary source failed, trying secondary"),
            case erlmcp_client:read_resource(Client, <<"mcp://secondary/data">>) of
                {ok, Data} ->
                    {ok, Data};
                {error, _} ->
                    %% Fallback to cached data
                    logger:warning("Secondary source failed, using cache"),
                    get_cached_data()
            end
    end.
```

---

## Transport-Specific Error Handling

### stdio Transport

```erlang
%% stdio errors are typically fatal
start_stdio_client() ->
    case erlmcp_client:start_link(#{
        transport => stdio,
        capabilities => #{},
        client_info => #{name => <<"client">>, version => <<"1.0.0">>}
    }) of
        {ok, Client} ->
            {ok, Client};
        {error, {transport_error, Reason}} ->
            logger:error("Failed to start stdio client: ~p", [Reason]),
            {error, Reason}
    end.
```

### HTTP Transport

```erlang
%% HTTP transport can have various network errors
call_http_tool(Client, ToolName, Args) ->
    case erlmcp_client:call_tool(Client, ToolName, Args) of
        {ok, Result} ->
            {ok, Result};
        {error, {transport_error, econnrefused}} ->
            logger:error("Server not available"),
            {error, server_unavailable};
        {error, {transport_error, timeout}} ->
            logger:warning("HTTP request timeout"),
            {error, timeout};
        {error, {transport_error, {http_error, 503}}} ->
            logger:warning("Server unavailable (503)"),
            {error, service_unavailable};
        {error, Reason} ->
            {error, Reason}
    end.
```

### WebSocket Transport

```erlang
%% WebSocket can disconnect unexpectedly
-module(ws_client_supervisor).
-behaviour(gen_server).

init([Config]) ->
    process_flag(trap_exit, true),
    case start_ws_client(Config) of
        {ok, Client} ->
            {ok, #{client => Client, config => Config}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_info({'EXIT', Client, Reason}, #{client := Client, config := Config} = State) ->
    logger:warning("WebSocket client crashed: ~p, reconnecting...", [Reason]),
    timer:sleep(5000),
    case start_ws_client(Config) of
        {ok, NewClient} ->
            {noreply, State#{client => NewClient}};
        {error, _} ->
            timer:sleep(5000),
            {stop, reconnection_failed, State}
    end.

start_ws_client(Config) ->
    erlmcp_client:start_link(#{
        transport => ws,
        transport_opts => Config,
        capabilities => #{},
        client_info => #{name => <<"ws-client">>, version => <<"1.0.0">>}
    }).
```

---

## Error Logging Best Practices

### Structured Logging

```erlang
%% Good: Structured logging with context
handle_tool_error(ToolName, Args, Error) ->
    logger:error("Tool execution failed",
        #{
            tool => ToolName,
            args_hash => crypto:hash(sha256, term_to_binary(Args)),
            error => Error,
            timestamp => os:system_time(millisecond)
        }
    ).

%% Bad: Unstructured logging
handle_tool_error(ToolName, Args, Error) ->
    io:format("Error in ~p with ~p: ~p~n", [ToolName, Args, Error]).
```

### Sensitive Data Sanitization

```erlang
%% Sanitize arguments before logging
sanitize_args(Args) ->
    maps:fold(fun(Key, Value, Acc) ->
        case is_sensitive_key(Key) of
            true -> Acc#{Key => <<"***REDACTED***">>};
            false -> Acc#{Key => Value}
        end
    end, #{}, Args).

is_sensitive_key(Key) ->
    SensitiveKeys = [
        <<"password">>, <<"secret">>, <<"token">>,
        <<"api_key">>, <<"private_key">>, <<"credential">>
    ],
    lists:member(Key, SensitiveKeys).

log_tool_call(ToolName, Args) ->
    logger:info("Tool called",
        #{
            tool => ToolName,
            args => sanitize_args(Args)
        }
    ).
```

---

## Testing Error Scenarios

### Unit Tests

```erlang
-module(error_handling_tests).
-include_lib("eunit/include/eunit.hrl").

division_by_zero_test() ->
    {ok, Server} = start_test_server(),
    {ok, Result} = erlmcp_server:call_tool(Server, <<"divide">>, #{
        <<"a">> => 10,
        <<"b">> => 0
    }),
    ?assertMatch(#{<<"isError">> := true}, Result).

tool_not_found_test() ->
    {ok, Client} = start_test_client(),
    ?assertMatch(
        {error, {jsonrpc_error, -32002, _, _}},
        erlmcp_client:call_tool(Client, <<"nonexistent">>, #{})
    ).

timeout_handling_test() ->
    {ok, Client} = start_test_client(),
    ?assertMatch(
        {error, {timeout, _}},
        erlmcp_client:call_tool(Client, <<"slow_tool">>, #{}, #{timeout => 100})
    ).

retry_logic_test() ->
    %% Mock function that fails twice then succeeds
    Counter = persistent_term:get(test_counter, 0),
    MockFun = fun() ->
        Count = persistent_term:get(test_counter, 0),
        persistent_term:put(test_counter, Count + 1),
        case Count of
            0 -> {error, timeout};
            1 -> {error, timeout};
            _ -> {ok, success}
        end
    end,

    ?assertMatch({ok, success}, mcp_error_handler:retry_on_error(MockFun, 3, 100)).
```

---

## See Also

- [Error Codes Reference](error-codes.md) - Complete error code documentation
- [JSON-RPC Reference](json-rpc-reference.md) - Protocol specification
- [Client API Reference](client-api.md) - Client API documentation
- [Server API Reference](server-api.md) - Server API documentation
