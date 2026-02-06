# erlmcp Server API Reference

## Overview

The erlmcp server API provides functions for creating MCP servers that expose tools, resources, and prompts to clients. Servers handle client connections, validate requests, execute handlers, and send notifications.

---

## Server Module: erlmcp_server

### start_link/2

Start a new MCP server.

**Signature:**
```erlang
start_link(ServerName, Capabilities) -> {ok, Pid} | {error, Reason}
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| ServerName | atom() | Yes | Server name for registration |
| Capabilities | map() | Yes | Server capabilities |

**Capabilities Fields:**

| Field | Type | Description |
|-------|------|-------------|
| tools | map() | Tool capabilities |
| resources | map() | Resource capabilities |
| prompts | map() | Prompt capabilities |
| roots | map() | Root directory capabilities (v3.0) |
| logging | map() | Logging capabilities |
| experimental | map() | Experimental features |

**Example:**

```erlang
%% Start server with all capabilities
{ok, Server} = erlmcp_server:start_link(my_server, #{
    tools => #{
        listChanged => true
    },
    resources => #{
        subscribe => true,
        listChanged => true
    },
    prompts => #{
        listChanged => true
    },
    roots => #{
        listChanged => true
    }
}).

%% Start server with minimal capabilities
{ok, Server} = erlmcp_server:start_link(simple_server, #{
    tools => #{}
}).
```

---

## Tool Management

### add_tool/2

Register a tool with the server.

**Signature:**
```erlang
add_tool(Server, ToolSpec) -> ok | {error, Reason}
```

**ToolSpec Fields:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| name | binary() | Yes | Unique tool name |
| description | binary() | Yes | Human-readable description |
| input_schema | map() | No | JSON Schema for input validation |
| handler | function() | Yes | Tool handler function |
| timeout | integer() | No | Handler timeout in milliseconds (default: 30000) |
| meta | map() | No | Additional metadata |

**Handler Signature:**
```erlang
Handler :: fun((Arguments :: map()) -> Result :: map())
```

**Handler Result Format:**
```erlang
#{
    content => [Content],
    isError => boolean()  %% optional, default false
}
```

**Content Types:**
```erlang
%% Text content
#{type => text, text => Binary}

%% Image content
#{type => image, data => Base64Binary, mimeType => MimeType}

%% Resource content
#{type => resource, resource => #{uri => Uri, text => Text, mimeType => MimeType}}
```

**Example:**

```erlang
%% Simple echo tool
erlmcp_server:add_tool(Server, #{
    name => <<"echo">>,
    description => <<"Echo back the input text">>,
    input_schema => #{
        type => object,
        properties => #{
            text => #{type => string}
        },
        required => [text]
    },
    handler => fun(Args) ->
        Text = maps:get(<<"text">>, Args),
        #{content => [#{
            type => text,
            text => <<"Echo: ", Text/binary>>
        }]}
    end
}).

%% Calculator tool with error handling
erlmcp_server:add_tool(Server, #{
    name => <<"calculate">>,
    description => <<"Perform arithmetic operations">>,
    input_schema => #{
        type => object,
        properties => #{
            a => #{type => number},
            b => #{type => number},
            op => #{
                type => string,
                enum => [<<"add">>, <<"subtract">>, <<"multiply">>, <<"divide">>]
            }
        },
        required => [a, b, op]
    },
    handler => fun(Args) ->
        A = maps:get(<<"a">>, Args),
        B = maps:get(<<"b">>, Args),
        Op = maps:get(<<"op">>, Args),

        try
            Result = case Op of
                <<"add">> -> A + B;
                <<"subtract">> -> A - B;
                <<"multiply">> -> A * B;
                <<"divide">> when B =/= 0 -> A / B;
                <<"divide">> -> error(division_by_zero)
            end,
            #{content => [#{
                type => text,
                text => float_to_binary(Result, [{decimals, 4}])
            }]}
        catch
            error:division_by_zero ->
                #{
                    content => [#{
                        type => text,
                        text => <<"Error: Division by zero">>
                    }],
                    isError => true
                }
        end
    end
}).

%% Tool with progress reporting
erlmcp_server:add_tool(Server, #{
    name => <<"long_task">>,
    description => <<"A long-running task with progress updates">>,
    handler => fun(Args) ->
        ProgressToken = maps:get(<<"_progressToken">>, Args, undefined),

        Steps = 10,
        lists:foreach(fun(Step) ->
            timer:sleep(1000),
            case ProgressToken of
                undefined -> ok;
                Token ->
                    erlmcp_server:report_progress(Server, Token,
                        Step, Steps,
                        <<"Processing step ", (integer_to_binary(Step))/binary, "...">>
                    )
            end
        end, lists:seq(1, Steps)),

        #{content => [#{
            type => text,
            text => <<"Task completed!">>
        }]}
    end,
    timeout => 15000
}).

%% Tool with async execution
erlmcp_server:add_tool(Server, #{
    name => <<"async_task">>,
    description => <<"An asynchronous task">>,
    handler => fun(Args) ->
        Parent = self(),
        spawn(fun() ->
            timer:sleep(5000),
            Result = #{content => [#{type => text, text => <<"Done">>}]},
            Parent ! {async_result, Result}
        end),

        receive
            {async_result, Result} -> Result
        after 10000 ->
            #{
                content => [#{type => text, text => <<"Timeout">>}],
                isError => true
            }
        end
    end
}).
```

### remove_tool/2

Remove a tool from the server.

**Signature:**
```erlang
remove_tool(Server, ToolName) -> ok | {error, Reason}
```

**Example:**

```erlang
ok = erlmcp_server:remove_tool(Server, <<"old_tool">>).

%% Notify clients of tool list change
erlmcp_server:notify_tools_changed(Server).
```

### list_tools/1

Get list of all registered tools.

**Signature:**
```erlang
list_tools(Server) -> {ok, Tools}
```

**Example:**

```erlang
{ok, Tools} = erlmcp_server:list_tools(Server),
lists:foreach(fun(Tool) ->
    Name = maps:get(name, Tool),
    io:format("Tool: ~s~n", [Name])
end, Tools).
```

### report_progress/5

Report progress for a long-running operation.

**Signature:**
```erlang
report_progress(Server, ProgressToken, Progress, Total, Message) -> ok
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| Server | pid() | Server process |
| ProgressToken | binary() | Progress token from request |
| Progress | number() | Current progress value |
| Total | number() | Total progress value |
| Message | binary() | Progress message |

**Example:**

```erlang
%% In tool handler
ProgressToken = maps:get(<<"_progressToken">>, Args, undefined),

case ProgressToken of
    undefined ->
        %% No progress reporting requested
        process_without_progress();
    Token ->
        %% Report progress at different stages
        erlmcp_server:report_progress(Server, Token, 0, 100, <<"Starting...">>),

        do_work_step_1(),
        erlmcp_server:report_progress(Server, Token, 25, 100, <<"Step 1 complete">>),

        do_work_step_2(),
        erlmcp_server:report_progress(Server, Token, 50, 100, <<"Step 2 complete">>),

        do_work_step_3(),
        erlmcp_server:report_progress(Server, Token, 75, 100, <<"Step 3 complete">>),

        finalize(),
        erlmcp_server:report_progress(Server, Token, 100, 100, <<"Complete!">>)
end
```

---

## Resource Management

### add_resource/2

Register a static resource with the server.

**Signature:**
```erlang
add_resource(Server, ResourceSpec) -> ok | {error, Reason}
```

**ResourceSpec Fields:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| uri | binary() | Yes | Unique resource URI |
| name | binary() | Yes | Human-readable name |
| description | binary() | No | Resource description |
| mime_type | binary() | No | MIME type of content |
| handler | function() | Yes | Resource handler function |
| annotations | map() | No | Additional annotations |

**Handler Signature:**
```erlang
Handler :: fun((Uri :: binary()) -> Result :: map())
```

**Example:**

```erlang
%% Static resource
erlmcp_server:add_resource(Server, #{
    uri => <<"mcp://config">>,
    name => <<"Configuration">>,
    description => <<"Server configuration">>,
    mime_type => <<"application/json">>,
    handler => fun(_Uri) ->
        Config = #{
            version => <<"3.0.0">>,
            features => [<<"tools">>, <<"resources">>, <<"prompts">>]
        },
        #{content => [#{
            type => text,
            text => jsx:encode(Config)
        }]}
    end
}).

%% Dynamic resource
erlmcp_server:add_resource(Server, #{
    uri => <<"mcp://status">>,
    name => <<"Server Status">>,
    mime_type => <<"application/json">>,
    handler => fun(_Uri) ->
        Status = #{
            uptime => erlang:statistics(wall_clock),
            processes => erlang:system_info(process_count),
            memory => erlang:memory(total)
        },
        #{content => [#{
            type => text,
            text => jsx:encode(Status)
        }]}
    end
}).

%% File-based resource
erlmcp_server:add_resource(Server, #{
    uri => <<"file:///logs/app.log">>,
    name => <<"Application Log">>,
    mime_type => <<"text/plain">>,
    handler => fun(Uri) ->
        Path = binary_to_list(Uri),
        case file:read_file(Path) of
            {ok, Content} ->
                #{content => [#{
                    type => text,
                    text => Content
                }]};
            {error, Reason} ->
                #{
                    content => [#{
                        type => text,
                        text => io_lib:format("Error reading file: ~p", [Reason])
                    }],
                    isError => true
                }
        end
    end
}).
```

### add_resource_template/2

Register a resource URI template for dynamic resource access.

**Signature:**
```erlang
add_resource_template(Server, TemplateSpec) -> ok | {error, Reason}
```

**TemplateSpec Fields:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| uri_template | binary() | Yes | URI template with variables |
| name | binary() | Yes | Human-readable name |
| description | binary() | No | Template description |
| mime_type | binary() | No | MIME type of content |
| handler | function() | Yes | Template handler function |

**Handler Signature:**
```erlang
Handler :: fun((UriTemplate :: binary(), Variables :: [{atom(), binary()}]) -> Result :: map())
```

**Example:**

```erlang
%% Log file template
erlmcp_server:add_resource_template(Server, #{
    uri_template => <<"logs://{date}">>,
    name => <<"Daily Logs">>,
    description => <<"Access logs by date (YYYY-MM-DD)">>,
    mime_type => <<"text/plain">>,
    handler => fun(_Template, Variables) ->
        Date = proplists:get_value(date, Variables),
        Filename = <<"/var/log/app-", Date/binary, ".log">>,

        case file:read_file(Filename) of
            {ok, Content} ->
                #{content => [#{
                    type => text,
                    text => Content
                }]};
            {error, enoent} ->
                #{content => [#{
                    type => text,
                    text => <<"No log file for ", Date/binary>>
                }]};
            {error, Reason} ->
                #{
                    content => [#{
                        type => text,
                        text => io_lib:format("Error: ~p", [Reason])
                    }],
                    isError => true
                }
        end
    end
}).

%% User profile template
erlmcp_server:add_resource_template(Server, #{
    uri_template => <<"user://{user_id}/profile">>,
    name => <<"User Profile">>,
    mime_type => <<"application/json">>,
    handler => fun(_Template, Variables) ->
        UserId = proplists:get_value(user_id, Variables),
        case get_user_profile(UserId) of
            {ok, Profile} ->
                #{content => [#{
                    type => text,
                    text => jsx:encode(Profile)
                }]};
            {error, not_found} ->
                #{
                    content => [#{
                        type => text,
                        text => <<"User not found">>
                    }],
                    isError => true
                }
        end
    end
}).
```

### remove_resource/2

Remove a resource from the server.

**Signature:**
```erlang
remove_resource(Server, Uri) -> ok | {error, Reason}
```

### notify_resource_updated/2

Notify subscribed clients that a resource has changed.

**Signature:**
```erlang
notify_resource_updated(Server, Uri) -> ok
```

**Example:**

```erlang
%% Update resource and notify subscribers
update_config(Server, NewConfig) ->
    save_config(NewConfig),
    erlmcp_server:notify_resource_updated(Server, <<"mcp://config">>).
```

### notify_resources_changed/1

Notify clients that the resource list has changed.

**Signature:**
```erlang
notify_resources_changed(Server) -> ok
```

---

## Prompt Management

### add_prompt/2

Register a prompt template with the server.

**Signature:**
```erlang
add_prompt(Server, PromptSpec) -> ok | {error, Reason}
```

**PromptSpec Fields:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| name | binary() | Yes | Unique prompt name |
| description | binary() | No | Prompt description |
| arguments | list() | No | Prompt argument specifications |
| handler | function() | Yes | Prompt handler function |

**Argument Specification:**
```erlang
#{
    name => binary(),
    description => binary(),
    required => boolean()
}
```

**Handler Signature:**
```erlang
Handler :: fun((Arguments :: map()) -> Result :: map())
```

**Handler Result Format:**
```erlang
#{
    description => binary(),
    messages => [Message]
}
```

**Message Format:**
```erlang
#{
    role => user | assistant | system,
    content => Content
}
```

**Example:**

```erlang
%% Simple prompt
erlmcp_server:add_prompt(Server, #{
    name => <<"greeting">>,
    description => <<"Generate a greeting prompt">>,
    arguments => [
        #{
            name => <<"name">>,
            description => <<"Person's name">>,
            required => true
        }
    ],
    handler => fun(Args) ->
        Name = maps:get(<<"name">>, Args),
        #{
            description => <<"A friendly greeting">>,
            messages => [
                #{
                    role => user,
                    content => #{
                        type => text,
                        text => <<"Say hello to ", Name/binary, "!">>
                    }
                }
            ]
        }
    end
}).

%% Code generation prompt
erlmcp_server:add_prompt(Server, #{
    name => <<"write_code">>,
    description => <<"Generate a code writing prompt">>,
    arguments => [
        #{
            name => <<"language">>,
            description => <<"Programming language">>,
            required => true
        },
        #{
            name => <<"task">>,
            description => <<"What the code should do">>,
            required => true
        },
        #{
            name => <<"style">>,
            description => <<"Coding style preferences">>,
            required => false
        }
    ],
    handler => fun(Args) ->
        Language = maps:get(<<"language">>, Args),
        Task = maps:get(<<"task">>, Args),
        Style = maps:get(<<"style">>, Args, <<"standard">>),

        Prompt = <<"Write ", Language/binary, " code to: ", Task/binary,
                   ". Use ", Style/binary, " coding style.">>,

        #{
            description => <<"Code generation prompt">>,
            messages => [
                #{
                    role => system,
                    content => #{
                        type => text,
                        text => <<"You are an expert programmer.">>
                    }
                },
                #{
                    role => user,
                    content => #{
                        type => text,
                        text => Prompt
                    }
                }
            ]
        }
    end
}).
```

### remove_prompt/2

Remove a prompt from the server.

**Signature:**
```erlang
remove_prompt(Server, PromptName) -> ok | {error, Reason}
```

### notify_prompts_changed/1

Notify clients that the prompt list has changed.

**Signature:**
```erlang
notify_prompts_changed(Server) -> ok
```

---

## Root Management (v3.0)

### add_root/2

Add a root directory to the server's workspace.

**Signature:**
```erlang
add_root(Server, RootSpec) -> ok | {error, Reason}
```

**RootSpec:**
```erlang
#{
    uri => binary(),
    name => binary()
}
```

### remove_root/2

Remove a root directory.

**Signature:**
```erlang
remove_root(Server, Uri) -> ok | {error, Reason}
```

### list_roots/1

Get list of all roots.

**Signature:**
```erlang
list_roots(Server) -> {ok, Roots}
```

### notify_roots_changed/1

Notify clients that the roots list has changed.

**Signature:**
```erlang
notify_roots_changed(Server) -> ok
```

---

## Notification Methods

### send_notification/2

Send a custom notification to all connected clients.

**Signature:**
```erlang
send_notification(Server, Notification) -> ok
```

**Notification Format:**
```erlang
#{
    method => binary(),
    params => map()
}
```

**Example:**

```erlang
%% Send custom notification
erlmcp_server:send_notification(Server, #{
    method => <<"notifications/message">>,
    params => #{
        level => info,
        logger => <<"my-server">>,
        data => #{
            message => <<"Server restarting...">>
        }
    }
}).

%% Broadcast event to all clients
broadcast_event(Server, EventType, EventData) ->
    erlmcp_server:send_notification(Server, #{
        method => <<"notifications/message">>,
        params => #{
            level => info,
            data => #{
                event => EventType,
                data => EventData,
                timestamp => os:system_time(millisecond)
            }
        }
    }).
```

### notify_tools_changed/1

Notify clients that the tool list has changed (if capability enabled).

**Signature:**
```erlang
notify_tools_changed(Server) -> ok
```

---

## Server Configuration

### set_config/2

Update server configuration.

**Signature:**
```erlang
set_config(Server, Config) -> ok
```

**Config Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| max_connections | integer() | 1000 | Maximum concurrent connections |
| request_timeout | integer() | 30000 | Default request timeout (ms) |
| max_request_size | integer() | 16777216 | Maximum request size (16MB) |
| log_level | atom() | info | Logging level |
| enable_metrics | boolean() | true | Enable metrics collection |

### get_config/1

Get current server configuration.

**Signature:**
```erlang
get_config(Server) -> {ok, Config}
```

---

## Supervision and Lifecycle

### stop/1

Gracefully stop the server.

**Signature:**
```erlang
stop(Server) -> ok
```

**Example:**

```erlang
%% Graceful shutdown
erlmcp_server:send_notification(Server, #{
    method => <<"notifications/message">>,
    params => #{
        level => warning,
        data => #{message => <<"Server shutting down...">>}
    }
}),

timer:sleep(1000),
erlmcp_server:stop(Server).
```

---

## Complete Server Example

```erlang
-module(example_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Start MCP server
    {ok, Server} = erlmcp_server:start_link(example_server, #{
        tools => #{listChanged => true},
        resources => #{subscribe => true, listChanged => true},
        prompts => #{}
    }),

    %% Register tools
    register_tools(Server),

    %% Register resources
    register_resources(Server),

    %% Register prompts
    register_prompts(Server),

    %% Start transports
    start_transports(Server),

    {ok, #{server => Server}}.

register_tools(Server) ->
    %% Echo tool
    erlmcp_server:add_tool(Server, #{
        name => <<"echo">>,
        description => <<"Echo back the input">>,
        input_schema => #{
            type => object,
            properties => #{
                text => #{type => string}
            },
            required => [text]
        },
        handler => fun(Args) ->
            Text = maps:get(<<"text">>, Args),
            #{content => [#{type => text, text => Text}]}
        end
    }),

    %% Time tool
    erlmcp_server:add_tool(Server, #{
        name => <<"get_time">>,
        description => <<"Get current time">>,
        handler => fun(_Args) ->
            {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
            Time = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                  [Y, M, D, H, Min, S]),
            #{content => [#{type => text, text => list_to_binary(Time)}]}
        end
    }).

register_resources(Server) ->
    %% Server info resource
    erlmcp_server:add_resource(Server, #{
        uri => <<"mcp://info">>,
        name => <<"Server Info">>,
        mime_type => <<"application/json">>,
        handler => fun(_Uri) ->
            Info = #{
                name => <<"example-server">>,
                version => <<"1.0.0">>,
                started_at => os:system_time(second)
            },
            #{content => [#{type => text, text => jsx:encode(Info)}]}
        end
    }).

register_prompts(Server) ->
    erlmcp_server:add_prompt(Server, #{
        name => <<"analyze">>,
        description => <<"Analyze something">>,
        arguments => [
            #{name => <<"topic">>, description => <<"Topic to analyze">>, required => true}
        ],
        handler => fun(Args) ->
            Topic = maps:get(<<"topic">>, Args),
            #{
                description => <<"Analysis prompt">>,
                messages => [
                    #{
                        role => user,
                        content => #{type => text, text => <<"Analyze: ", Topic/binary>>}
                    }
                ]
            }
        end
    }).

start_transports(Server) ->
    %% Start stdio transport
    {ok, _Stdio} = erlmcp_transport_stdio:start_link(Server),

    %% Start HTTP transport
    {ok, _Http} = erlmcp_transport_http:start_link(Server, [
        {port, 8765}
    ]).

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
```

---

## See Also

- [Client API Reference](client-api.md) - Client-side API documentation
- [JSON-RPC Reference](json-rpc-reference.md) - Protocol specification
- [Error Codes](error-codes.md) - Error handling
- [Integration Examples](integration-examples.md) - Real-world examples
