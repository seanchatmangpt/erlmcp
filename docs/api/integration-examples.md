# erlmcp Integration Examples

This document provides real-world integration patterns and examples for using erlmcp in production scenarios.

---

## Table of Contents

1. [Database Integration](#database-integration)
2. [File System Access](#file-system-access)
3. [HTTP API Integration](#http-api-integration)
4. [Message Queue Integration](#message-queue-integration)
5. [Cloud Service Integration](#cloud-service-integration)
6. [Streaming Data](#streaming-data)
7. [Multi-Server Deployment](#multi-server-deployment)

---

## Database Integration

### PostgreSQL Integration

```erlang
-module(mcp_postgres).
-export([start/0, add_database_tools/1]).

start() ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(epgsql),  %% PostgreSQL client

    Capabilities = #mcp_server_capabilities{
        tools = #mcp_tools_capability{},
        resources = #mcp_resources_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(postgres_server, Capabilities),
    add_database_tools(Server),
    {ok, Server}.

add_database_tools(Server) ->
    %% Query tool
    erlmcp_server:add_tool(Server, #{
        name => <<"db_query">>,
        description => <<"Execute a SQL query">>,
        input_schema => #{
            type => object,
            properties => #{
                query => #{type => string},
                params => #{type => array}
            },
            required => [query]
        },
        handler => fun(Args) ->
            Query = maps:get(<<"query">>, Args),
            Params = maps:get(<<"params">>, Args, []),
            case execute_query(Query, Params) of
                {ok, Rows} ->
                    #{content => [#{
                        type => text,
                        text => jsx:encode(Rows)
                    }]};
                {error, Reason} ->
                    #{content => [#{
                        type => text,
                        text => io_lib:format("Query error: ~p", [Reason])
                    }], isError => true}
            end
        end
    }),

    %% Table list resource
    erlmcp_server:add_resource(Server, #{
        uri => <<"db://tables">>,
        name => <<"Database Tables">>,
        description => <<"List all tables">>,
        mime_type => <<"application/json">>,
        handler => fun(_Uri) ->
            {ok, Tables} = list_tables(),
            #{content => [#{
                type => text,
                text => jsx:encode(Tables)
            }]}
        end
    }).

execute_query(Query, Params) ->
    {ok, Conn} = epgsql:connect("localhost", "user", "pass", [
        {database, "mydb"}
    ]),
    try
        {ok, _Cols, Rows} = epgsql:equery(Conn, Query, Params),
        {ok, [[RowValue || {_, RowValue} <- lists:flatten(Row)] || Row <- Rows]}
    after
        epgsql:close(Conn)
    end.

list_tables() ->
    {ok, Conn} = epgsql:connect("localhost", "user", "pass", [
        {database, "mydb"}
    ]),
    try
        {ok, _, Rows} = epgsql:equery(Conn, "SELECT tablename FROM pg_tables WHERE schemaname = 'public'"),
        {ok, [Tablename || {Tablename} <- Rows]}
    after
        epgsql:close(Conn)
    end.
```

### Redis Integration

```erlang
-module(mcp_redis).
-export([start/0]).

start() ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(eredis),

    Capabilities = #mcp_server_capabilities{
        tools = #mcp_tools_capability{},
        resources = #mcp_resources_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(redis_server, Capabilities),

    %% GET command
    erlmcp_server:add_tool(Server, #{
        name => <<"redis_get">>,
        description => <<"Get value from Redis">>,
        input_schema => #{
            type => object,
            properties => #{
                key => #{type => string}
            },
            required => [key]
        },
        handler => fun(Args) ->
            Key = maps:get(<<"key">>, Args),
            {ok, Conn} = eredis:start_link("localhost", 6379),
            Result = case eredis:q(Conn, ["GET", Key]) of
                {ok, undefined} -> {error, not_found};
                {ok, Value} -> {ok, Value};
                {error, Reason} -> {error, Reason}
            end,
            eredis:stop(Conn),
            format_result(Result)
        end
    }),

    %% SET command
    erlmcp_server:add_tool(Server, #{
        name => <<"redis_set">>,
        description => <<"Set value in Redis">>,
        input_schema => #{
            type => object,
            properties => #{
                key => #{type => string},
                value => #{type => string}
            },
            required => [key, value]
        },
        handler => fun(Args) ->
            Key = maps:get(<<"key">>, Args),
            Value = maps:get(<<"value">>, Args),
            {ok, Conn} = eredis:start_link("localhost", 6379),
            Result = eredis:q(Conn, ["SET", Key, Value]),
            eredis:stop(Conn),
            format_result(Result)
        end
    }).

format_result({ok, _}) ->
    #{content => [{type => text, text => <<"OK">>}]};
format_result({error, Reason}) ->
    #{content => [{type => text, text => io_lib:format("Error: ~p", [Reason])}], isError => true}.
```

---

## File System Access

### Secure File Browser

```erlang
-module(mcp_filesystem).
-export([start/0]).

-define(ALLOWED_PATHS, [
    "/Users/user/projects",
    "/var/log/app"
]).
-define(MAX_FILE_SIZE, 10485760).  %% 10MB

start() ->
    application:ensure_all_started(erlmcp_core),

    Capabilities = #mcp_server_capabilities{
        tools = #mcp_tools_capability{},
        resources = #mcp_resources_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(filesystem_server, Capabilities),

    %% List directory tool
    erlmcp_server:add_tool(Server, #{
        name => <<"fs_list">>,
        description => <<"List directory contents">>,
        input_schema => #{
            type => object,
            properties => #{
                path => #{type => string}
            },
            required => [path]
        },
        handler => fun(Args) ->
            Path = maps:get(<<"path">>, Args),
            case validate_path(Path) of
                {ok, SafePath} ->
                    case file:list_dir(SafePath) of
                        {ok, Files} ->
                            #{content => [{type => text, text => jsx:encode(Files)}]};
                        {error, Reason} ->
                            #{content => [{type => text, text => io_lib:format("Error: ~p", [Reason])}], isError => true}
                    end;
                {error, Reason} ->
                    #{content => [{type => text, text => Reason}], isError => true}
            end
        end
    }),

    %% Read file resource
    erlmcp_server:add_resource(Server, #{
        uri_template => <<"file://{path}">>,
        name => <<"File Read">>,
        description => <<"Read file contents">>,
        handler => fun(_Uri, [{path, Path}]) ->
            case validate_path(Path) of
                {ok, SafePath} ->
                    case file:read_file(SafePath) of
                        {ok, Content} when byte_size(Content) =< ?MAX_FILE_SIZE ->
                            #{content => [{type => text, text => Content}]};
                        {ok, _Content} ->
                            #{content => [{type => text, text => <<"File too large">>}], isError => true};
                        {error, Reason} ->
                            #{content => [{type => text, text => io_lib:format("Read error: ~p", [Reason])}], isError => true}
                    end;
                {error, Reason} ->
                    #{content => [{type => text, text => Reason}], isError => true}
            end
        end
    }).

validate_path(Path) ->
    %% Ensure path is within allowed directories
    SafePath = filename:absname(Path),
    case lists:any(fun(Allowed) ->
        string:prefix(SafePath, Allowed) =/= nomatch
    end, ?ALLOWED_PATHS) of
        true -> {ok, SafePath};
        false -> {error, <<"Access denied: path not in allowed directories">>}
    end.
```

---

## HTTP API Integration

### API Gateway

```erlang
-module(mcp_http_client).
-export([start/0]).

start() ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(hackney),  %% HTTP client

    Capabilities = #mcp_server_capabilities{
        tools = #mcp_tools_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(http_client_server, Capabilities),

    %% Generic HTTP GET tool
    erlmcp_server:add_tool(Server, #{
        name => <<"http_get">>,
        description => <<"Make an HTTP GET request">>,
        input_schema => #{
            type => object,
            properties => #{
                url => #{type => string},
                headers => #{type => object}
            },
            required => [url]
        },
        handler => fun(Args) ->
            Url = maps:get(<<"url">>, Args),
            Headers = maps:get(<<"headers">>, Args, #{}),
            case hackney:get(Url, maps:to_list(Headers)) of
                {ok, StatusCode, RespHeaders, ClientRef} ->
                    {ok, Body} = hackney:body(ClientRef),
                    #{content => [{type => text, text => jsx:encode(#{
                        status => StatusCode,
                        headers => RespHeaders,
                        body => Body
                    })}]};
                {error, Reason} ->
                    #{content => [{type => text, text => io_lib:format("HTTP error: ~p", [Reason])}], isError => true}
            end
        end
    }),

    %% Pre-configured API call tool
    erlmcp_server:add_tool(Server, #{
        name => <<"get_user">>,
        description => <<"Get user from API">>,
        input_schema => #{
            type => object,
            properties => #{
                user_id => #{type => string}
            },
            required => [user_id]
        },
        handler => fun(Args) ->
            UserId = maps:get(<<"user_id">>, Args),
            ApiKey = get_secret(<<"api_key">>),  %% From secrets manager
            Url = <<"https://api.example.com/users/", UserId/binary>>,

            case hackney:get(Url, [
                {<<"Authorization">>, <<"Bearer ", ApiKey/binary>>},
                {<<"Accept">>, <<"application/json">>}
            ]) of
                {ok, 200, _, ClientRef} ->
                    {ok, Body} = hackney:body(ClientRef),
                    #{content => [{type => text, text => Body}]};
                {ok, StatusCode, _, ClientRef} ->
                    {ok, Body} = hackney:body(ClientRef),
                    #{content => [{type => text, text => Body}], isError => true};
                {error, Reason} ->
                    #{content => [{type => text, text => io_lib:format("API error: ~p", [Reason])}], isError => true}
            end
        end
    }).

get_secret(Key) ->
    {ok, Value} = erlmcp_secrets:get_secret(Key),
    Value.
```

---

## Message Queue Integration

### RabbitMQ Consumer

```erlang
-module(mcp_rabbitmq).
-export([start/0]).

start() ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(amqp_client),

    Capabilities = #mcp_server_capabilities{
        tools = #mcp_tools_capability{},
        resources => #mcp_resources_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(rabbitmq_server, Capabilities),

    %% Publish message tool
    erlmcp_server:add_tool(Server, #{
        name => <<"mq_publish">>,
        description => <<"Publish message to RabbitMQ">>,
        input_schema => #{
            type => object,
            properties => #{
                exchange => #{type => string},
                routing_key => #{type => string},
                message => #{type => string}
            },
            required => [exchange, routing_key, message]
        },
        handler => fun(Args) ->
            Exchange = maps:get(<<"exchange">>, Args),
            RoutingKey = maps:get(<<"routing_key">>, Args),
            Message = maps:get(<<"message">>, Args),

            {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
            {ok, Channel} = amqp_connection:open_channel(Connection),

            Publish = #'basic.publish'{
                exchange = Exchange,
                routing_key = RoutingKey
            },
            amqp_channel:call(Channel, Publish, #amqp_msg{payload = Message}),

            amqp_channel:close(Channel),
            amqp_connection:close(Connection),

            #{content => [{type => text, text => <<"Message published">>}]}
        end
    }),

    %% Queue messages resource
    erlmcp_server:add_resource(Server, #{
        uri_template => <<"mq://{queue}/messages">>,
        name => <<"Queue Messages">>,
        description => <<"Get messages from queue">>,
        handler => fun(_Uri, [{queue, QueueName}]) ->
            {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
            {ok, Channel} = amqp_connection:open_channel(Connection),

            #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{
                queue = QueueName
            }),

            {ok, _Count, Messages} = amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue}),

            amqp_channel:close(Channel),
            amqp_connection:close(Connection),

            #{content => [{type => text, text => jsx:encode(Messages)}]}
        end
    }).
```

---

## Cloud Service Integration

### AWS S3 Integration

```erlang
-module(mcp_s3).
-export([start/0]).

start() ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlcloud),  %% AWS SDK

    Capabilities = #mcp_server_capabilities{
        tools => #mcp_tools_capability{},
        resources => #mcp_resources_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(s3_server, Capabilities),

    %% List buckets tool
    erlmcp_server:add_tool(Server, #{
        name => <<"s3_list_buckets">>,
        description => <<"List S3 buckets">>,
        input_schema => #{
            type => object,
            properties => #{}
        },
        handler => fun(_Args) ->
            case erlcloud_s3:list_buckets() of
                {ok, Buckets} ->
                    #{content => [{type => text, text => jsx:encode(Buckets)}]};
                {error, Reason} ->
                    #{content => [{type => text, text => io_lib:format("S3 error: ~p", [Reason])}], isError => true}
            end
        end
    }),

    %% Get object resource
    erlmcp_server:add_resource(Server, #{
        uri_template => <<"s3://{bucket}/{key}">>,
        name => <<"S3 Object">>,
        description => <<"Get S3 object">>,
        handler => fun(_Uri, [{bucket, Bucket}, {key, Key}]) ->
            case erlcloud_s3:get_object(Bucket, Key) of
                {ok, [{content, Content}]} ->
                    #{content => [{type => text, text => Content}]};
                {error, Reason} ->
                    #{content => [{type => text, text => io_lib:format("S3 error: ~p", [Reason])}], isError => true}
            end
        end
    }).
```

---

## Streaming Data

### Real-Time Log Streaming

```erlang
-module(mcp_log_streamer).
-export([start/0]).

start() ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),

    Capabilities = #mcp_server_capabilities{
        tools => #mcp_tools_capability{},
        resources => #mcp_resources_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(log_server, Capabilities),

    %% Start SSE transport for streaming
    {ok, _SSE} = erlmcp_transport_sse:start_link(Server, [
        {port, 8765},
        {path, "/logs/stream"}
    ]),

    %% Subscribe to logs tool
    erlmcp_server:add_tool(Server, #{
        name => <<"subscribe_logs">>,
        description => <<"Subscribe to log stream">>,
        input_schema => #{
            type => object,
            properties => #{
                filter => #{type => string}
            }
        },
        handler => fun(Args) ->
            Filter = maps:get(<<"filter">>, Args, <<"">>),
            Pid = self(),
            spawn(fun() -> stream_logs(Pid, Filter) end),
            #{content => [{type => text, text => <<"Log streaming started">>}]}
        end
    }),

    %% Register notification handler
    erlmcp_server:register_notification_handler(Server, fun log_notifier/1),

    {ok, Server}.

stream_logs(SubscriberPid, Filter) ->
    %% In real implementation, this would tail a log file
    %% or subscribe to a log service
    erlmcp_server:send_notification(SubscriberPid, #{
        method => <<"notifications/message">>,
        params => #{
            level => info,
            data => #{
                message => <<"Sample log message">>,
                timestamp => os:system_time(millisecond)
            }
        }
    }),
    timer:sleep(1000),
    stream_logs(SubscriberPid, Filter).

log_notifier(Notification) ->
    %% Forward notifications to all subscribers
    %% In real implementation, track subscribers and filter
    ok.
```

---

## Multi-Server Deployment

### Distributed MCP Cluster

```erlang
-module(mcp_cluster).
-export([start/0]).

start() ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),
    application:ensure_all_started(erlmcp_observability),

    %% Connect to cluster
    case net_adm:ping('mcp-node1@host1') of
        pong -> ok;
        pang -> logger:warning("Failed to connect to mcp-node1")
    end,

    %% Start distributed server
    Capabilities = #mcp_server_capabilities{
        tools => #mcp_tools_capability{},
        resources => #mcp_resources_capability{},
        prompts => #mcp_prompts_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(cluster_server, Capabilities),

    %% Register with distributed registry
    erlmcp_registry:register_node(node(), Server),

    %% Start HTTP transport with load balancing
    {ok, _HTTP} = erlmcp_transport_http:start_link(Server, [
        {port, 8765},
        {ip, {0, 0, 0, 0}},
        {load_balancing, round_robin}
    ]),

    %% Start health monitoring
    erlmcp_health:start_monitor(Server, #{
        check_interval => 5000,
        fail_threshold => 3,
        nodes => [
            'mcp-node1@host1',
            'mcp-node2@host2',
            'mcp-node3@host3'
        ]
    }),

    {ok, Server}.
```

---

## Docker Deployment

### Dockerfile

```dockerfile
FROM erlang:28-alpine AS builder

WORKDIR /app
COPY rebar.config rebar.lock ./
RUN rebar3 get-deps

COPY . .
RUN rebar3 compile
RUN rebar3 release

FROM alpine:latest

RUN apk add --no-cache openssl ncurses-libs

WORKDIR /app
COPY --from=builder /app/_build/prod/rel/erlmcp ./

EXPOSE 8765

CMD ["bin/erlmcp", "start"]
```

### docker-compose.yml

```yaml
version: '3.8'

services:
  erlmcp:
    build: .
    ports:
      - "8765:8765"
    environment:
      - ERLMCP_TRANSPORT_HTTP_PORT=8765
      - ERLMCP_AUTH_ENABLED=false
      - ERLMCP_OTEL_ENABLED=true
    volumes:
      - ./priv/secrets:/app/priv/secrets
      - ./logs:/app/logs
    depends_on:
      - postgres
      - redis

  postgres:
    image: postgres:16
    environment:
      POSTGRES_DB: erlmcp
      POSTGRES_USER: erlmcp
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
    volumes:
      - postgres_data:/var/lib/postgresql/data

  redis:
    image: redis:7-alpine
    volumes:
      - redis_data:/data

volumes:
  postgres_data:
  redis_data:
```

---

## Kubernetes Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp
spec:
  replicas: 3
  selector:
    matchLabels:
      app: erlmcp
  template:
    metadata:
      labels:
        app: erlmcp
    spec:
      containers:
      - name: erlmcp
        image: erlmcp:3.0.0
        ports:
        - containerPort: 8765
        env:
        - name: ERLMCP_TRANSPORT_HTTP_PORT
          value: "8765"
        - name: ERLMCP_AUTH_ENABLED
          value: "true"
        - name: ERLMCP_AUTH_JWT_SECRET
          valueFrom:
            secretKeyRef:
              name: erlmcp-secrets
              key: jwt-secret
        volumeMounts:
        - name: secrets
          mountPath: /app/priv/secrets
          readOnly: true
        livenessProbe:
          httpGet:
            path: /health
            port: 8765
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /health
            port: 8765
          initialDelaySeconds: 10
          periodSeconds: 5
      volumes:
      - name: secrets
        secret:
          secretName: erlmcp-secrets

---
apiVersion: v1
kind: Service
metadata:
  name: erlmcp
spec:
  selector:
    app: erlmcp
  ports:
  - protocol: TCP
    port: 8765
    targetPort: 8765
  type: LoadBalancer
```

---

## Monitoring Integration

### Prometheus Metrics

```erlang
%% Enable Prometheus metrics endpoint
{erlmcp_observability, [
    {prometheus, #{
        enabled => true,
        port => 9090,
        path => "/metrics",
        labels => [
            {service, "erlmcp"},
            {version, "3.0.0"}
        ]
    }}
]}.
```

### Grafana Dashboard

```json
{
  "dashboard": {
    "title": "erlmcp Metrics",
    "panels": [
      {
        "title": "Requests per Second",
        "targets": [
          {
            "expr": "rate(erlmcp_requests_total[1m])"
          }
        ]
      },
      {
        "title": "Error Rate",
        "targets": [
          {
            "expr": "rate(erlmcp_errors_total[5m])"
          }
        ]
      },
      {
        "title": "Active Connections",
        "targets": [
          {
            "expr": "erlmcp_connections_active"
          }
        ]
      }
    ]
  }
}
```
