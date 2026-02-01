#!/usr/bin/env bash
###############################################################################
# HTTP + SSE MCP Server - Start erlmcp server with HTTP and SSE transports
#
# USAGE:
#   ./scripts/run/http-sse-server.sh [PORT]
#   OR
#   make run-http-sse
#
# ARGUMENTS:
#   PORT - HTTP port to listen on (default: 3000)
#
# DESCRIPTION:
#   Starts an MCP (Model Context Protocol) server that communicates via
#   both HTTP POST requests and Server-Sent Events (SSE). This enables
#   real-time server-to-client notifications and streaming updates.
#
# FEATURES:
#   - HTTP endpoint: http://localhost:3000/mcp (JSON-RPC 2.0)
#   - SSE endpoint: http://localhost:3000/events (Server-Sent Events)
#   - Includes example tools: calculate, long_running_task
#   - Includes example resources: mcp://config, mcp://counter
#   - Includes example prompts: generate_report
#   - Real-time notifications: resource updates, progress reports
#   - Supports secrets management with automatic injection
#
# CONFIGURATION:
#   Environment variables:
#     HTTP_PORT     - Override default port (default: 3000)
#     HTTP_HOST     - Bind address (default: localhost)
#
# EXAMPLE HTTP REQUESTS:
#   # Initialize
#   curl -X POST http://localhost:3000/mcp \
#     -H "Content-Type: application/json" \
#     -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"clientInfo":{"name":"test","version":"1.0"}}}'
#
#   # Subscribe to events (SSE)
#   curl -N http://localhost:3000/events
#
#   # Call long-running tool (receives progress via SSE)
#   curl -X POST http://localhost:3000/mcp \
#     -H "Content-Type: application/json" \
#     -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"long_running_task","arguments":{"duration":5}}}'
#
# SSE EVENTS:
#   The /events endpoint streams Server-Sent Events in this format:
#     event: resource_updated
#     data: {"uri":"mcp://config","timestamp":1234567890}
#
#     event: progress
#     data: {"token":"task-123","progress":0.5,"message":"Halfway done"}
#
# SEE ALSO:
#   - examples/mcp_complete/example.erl
#   - examples/mcp_complete/README.md
#   - docs/transports.md
#
###############################################################################

set -e

# Configuration
HTTP_PORT="${1:-${HTTP_PORT:-3000}}"
HTTP_HOST="${HTTP_HOST:-localhost}"

# Navigate to project root (script is in scripts/run/)
cd "$(dirname "$0")/../.."
PROJECT_ROOT="$(pwd)"

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "  ERLMCP HTTP + SSE SERVER"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Compile if needed
if [ ! -d "_build/default/lib" ]; then
    echo "Compiling erlmcp (first run)..."
    TERM=dumb rebar3 compile
    echo "✓ Compilation complete"
    echo ""
fi

echo "Starting HTTP + SSE MCP server..."
echo ""
echo "Configuration:"
echo "  • HTTP endpoint: http://${HTTP_HOST}:${HTTP_PORT}/mcp"
echo "  • SSE endpoint:  http://${HTTP_HOST}:${HTTP_PORT}/events"
echo "  • Transport: HTTP POST + Server-Sent Events"
echo "  • Protocol: MCP 2025-11-25"
echo ""
echo "Server capabilities:"
echo "  • Resources: mcp://config, mcp://counter, mcp://status"
echo "  • Tools: calculate, long_running_task"
echo "  • Prompts: generate_report"
echo "  • Features: real-time notifications, progress updates, secrets"
echo ""
echo "Example HTTP request:"
echo "  curl -X POST http://${HTTP_HOST}:${HTTP_PORT}/mcp \\"
echo "    -H 'Content-Type: application/json' \\"
echo "    -d '{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"clientInfo\":{\"name\":\"test\",\"version\":\"1.0\"}}}'"
echo ""
echo "Example SSE subscription:"
echo "  curl -N http://${HTTP_HOST}:${HTTP_PORT}/events"
echo ""
echo "For more examples and documentation:"
echo "  → examples/mcp_complete/README.md"
echo ""
echo "Press Ctrl+C to stop the server"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Create a temporary Erlang module to start HTTP+SSE server
cat > /tmp/erlmcp_sse_runner_$$.erl << 'ERLEOF'
-module(erlmcp_sse_runner).
-export([start/1]).

start([PortStr]) ->
    Port = list_to_integer(atom_to_list(PortStr)),

    %% Start applications
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_transports),

    %% Start resource subscriptions manager
    {ok, _} = erlmcp_resource_subscriptions:start_link(),

    %% Start secrets manager
    {ok, _} = erlmcp_secrets:start_link(#{
        backend => local_encrypted,
        ttl_seconds => 300,
        storage_path => "/tmp/erlmcp_secrets_sse.enc",
        encryption_key_path => "/tmp/erlmcp_master_sse.key"
    }),

    erlmcp_secrets:set_secret(<<"api_key">>, <<"sk-demo-sse-key-67890">>),

    %% Start MCP server with HTTP + SSE transport
    {ok, Server} = erlmcp_server:start_link(
        <<"http-sse-mcp-server">>,
        #{
            transport => http_server,
            http_port => Port,
            sse_enabled => true,
            capabilities => #{
                resources => true,
                tools => true,
                prompts => true,
                logging => true,
                resources_subscription => true
            }
        }
    ),

    io:format("✓ HTTP server listening on http://localhost:~w/mcp~n", [Port]),
    io:format("✓ SSE endpoint available at http://localhost:~w/events~n", [Port]),
    io:format("✓ Ready to accept MCP requests and stream events~n~n"),

    %% Add resources
    ok = erlmcp_server:add_resource(
        Server,
        <<"mcp://config">>,
        <<"Server Configuration">>,
        fun(_Uri) ->
            jsx:encode(#{
                version => <<"2.1.0">>,
                transport => <<"http+sse">>,
                port => Port,
                capabilities => [<<"resources">>, <<"tools">>, <<"prompts">>, <<"sse">>]
            })
        end,
        <<"application/json">>
    ),

    ok = erlmcp_server:add_resource(
        Server,
        <<"mcp://counter">>,
        <<"Request Counter">>,
        fun(_Uri) ->
            Count = case get(request_count) of
                undefined -> 0;
                C -> C
            end,
            put(request_count, Count + 1),
            jsx:encode(#{
                count => Count,
                timestamp => erlang:system_time(second)
            })
        end,
        <<"application/json">>
    ),

    ok = erlmcp_server:add_resource(
        Server,
        <<"mcp://status">>,
        <<"Server Status (live updates via SSE)">>,
        fun(_Uri) ->
            jsx:encode(#{
                status => <<"healthy">>,
                uptime_seconds => erlang:system_time(second),
                connections => 0
            })
        end,
        <<"application/json">>
    ),

    %% Add tools
    ok = erlmcp_server:add_tool(
        Server,
        <<"calculate">>,
        <<"Perform arithmetic calculations">>,
        fun(Args) ->
            #{<<"a">> := A, <<"b">> := B, <<"op">> := Op} = Args,
            Result = case Op of
                <<"add">> -> A + B;
                <<"subtract">> -> A - B;
                <<"multiply">> -> A * B;
                <<"divide">> when B =/= 0 -> A / B;
                _ -> error
            end,
            float_to_binary(Result, [{decimals, 2}])
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"a">> => #{<<"type">> => <<"number">>},
                <<"b">> => #{<<"type">> => <<"number">>},
                <<"op">> => #{
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"add">>, <<"subtract">>, <<"multiply">>, <<"divide">>]
                }
            },
            <<"required">> => [<<"a">>, <<"b">>, <<"op">>]
        }
    ),

    ok = erlmcp_server:add_tool(
        Server,
        <<"long_running_task">>,
        <<"Execute a long-running task with progress updates">>,
        fun(Args) ->
            Duration = maps:get(<<"duration">>, Args, 5),
            ProgressToken = maps:get(<<"_progressToken">>, Args, <<"task-", (integer_to_binary(erlang:unique_integer()))/binary>>),

            %% Simulate long-running work with progress updates
            Steps = 5,
            lists:foreach(fun(Step) ->
                Progress = Step / Steps,
                Message = io_lib:format("Step ~w/~w complete", [Step, Steps]),
                erlmcp_server:report_progress(Server, ProgressToken, Progress, iolist_to_binary(Message)),
                timer:sleep(Duration * 1000 div Steps)
            end, lists:seq(1, Steps)),

            jsx:encode(#{
                status => <<"completed">>,
                duration_seconds => Duration,
                steps => Steps
            })
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"duration">> => #{
                    <<"type">> => <<"number">>,
                    <<"description">> => <<"Task duration in seconds">>
                }
            }
        }
    ),

    %% Add prompts
    ok = erlmcp_server:add_prompt(
        Server,
        <<"generate_report">>,
        <<"Generate a structured report">>,
        fun(Args) ->
            Type = maps:get(<<"type">>, Args, <<"summary">>),
            [#{
                <<"role">> => <<"user">>,
                <<"content">> => #{
                    <<"type">> => <<"text">>,
                    <<"text">> => <<"Generate a ", Type/binary, " report with real-time progress updates.">>
                }
            }]
        end,
        [
            #{
                <<"name">> => <<"type">>,
                <<"description">> => <<"Report type">>,
                <<"required">> => false
            }
        ]
    ),

    %% Spawn a process to periodically update the status resource (demonstrates SSE notifications)
    spawn(fun() -> update_status_loop(Server, 10000) end),

    %% Keep running
    receive
        stop -> ok
    end.

%% Periodically update status resource to trigger SSE notifications
update_status_loop(Server, Interval) ->
    timer:sleep(Interval),
    %% Notify subscribers that mcp://status was updated
    erlmcp_server:notify_resource_updated(Server, <<"mcp://status">>),
    update_status_loop(Server, Interval).
ERLEOF

# Trap Ctrl+C to cleanup
trap "echo ''; echo 'Shutting down...'; rm -f /tmp/erlmcp_sse_runner_$$.erl /tmp/erlmcp_sse_runner_$$.beam /tmp/erlmcp_secrets_sse.enc /tmp/erlmcp_master_sse.key; exit 0" INT TERM

# Start the server
erl -pa _build/default/lib/*/ebin \
    -noshell \
    -eval "c('/tmp/erlmcp_sse_runner_$$.erl'), erlmcp_sse_runner:start(['$HTTP_PORT'])."

# Cleanup
rm -f /tmp/erlmcp_sse_runner_$$.erl /tmp/erlmcp_sse_runner_$$.beam /tmp/erlmcp_secrets_sse.enc /tmp/erlmcp_master_sse.key
