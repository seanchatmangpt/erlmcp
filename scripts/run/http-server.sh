#!/usr/bin/env bash
###############################################################################
# HTTP MCP Server - Start erlmcp server with HTTP transport
#
# USAGE:
#   ./scripts/run/http-server.sh [PORT]
#   OR
#   make run-http
#
# ARGUMENTS:
#   PORT - HTTP port to listen on (default: 3000)
#
# DESCRIPTION:
#   Starts an MCP (Model Context Protocol) server that communicates via
#   HTTP POST requests. This is useful for web-based integrations, testing,
#   and scenarios where stdio transport is not suitable.
#
# FEATURES:
#   - Listens on http://localhost:3000 (or specified port)
#   - Accepts JSON-RPC 2.0 POST requests to /mcp endpoint
#   - Includes example tools: calculate, fetch_external_data
#   - Includes example resources: mcp://config, mcp://counter
#   - Includes example prompts: generate_report
#   - Supports secrets management with automatic injection
#
# CONFIGURATION:
#   Environment variables:
#     HTTP_PORT     - Override default port (default: 3000)
#     HTTP_HOST     - Bind address (default: localhost)
#
# EXAMPLE REQUESTS:
#   # Initialize
#   curl -X POST http://localhost:3000/mcp \
#     -H "Content-Type: application/json" \
#     -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"clientInfo":{"name":"test","version":"1.0"}}}'
#
#   # List tools
#   curl -X POST http://localhost:3000/mcp \
#     -H "Content-Type: application/json" \
#     -d '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}'
#
#   # Call tool
#   curl -X POST http://localhost:3000/mcp \
#     -H "Content-Type: application/json" \
#     -d '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"calculate","arguments":{"a":10,"b":5,"op":"multiply"}}}'
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
echo "  ERLMCP HTTP SERVER"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Compile if needed
if [ ! -d "_build/default/lib" ]; then
    echo "Compiling erlmcp (first run)..."
    TERM=dumb rebar3 compile
    echo "✓ Compilation complete"
    echo ""
fi

echo "Starting HTTP MCP server..."
echo ""
echo "Configuration:"
echo "  • Endpoint: http://${HTTP_HOST}:${HTTP_PORT}/mcp"
echo "  • Transport: HTTP POST (JSON-RPC 2.0)"
echo "  • Protocol: MCP 2025-11-25"
echo ""
echo "Server capabilities:"
echo "  • Resources: mcp://config, mcp://counter, mcp://credentials"
echo "  • Tools: calculate, fetch_external_data"
echo "  • Prompts: generate_report"
echo "  • Features: secrets management, progress reporting"
echo ""
echo "Example request:"
echo "  curl -X POST http://${HTTP_HOST}:${HTTP_PORT}/mcp \\"
echo "    -H 'Content-Type: application/json' \\"
echo "    -d '{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"clientInfo\":{\"name\":\"test\",\"version\":\"1.0\"}}}'"
echo ""
echo "For more examples and documentation:"
echo "  → examples/mcp_complete/README.md"
echo "  → http://${HTTP_HOST}:${HTTP_PORT}/mcp (send POST requests)"
echo ""
echo "Press Ctrl+C to stop the server"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Create a temporary Erlang module to start HTTP server on port 3000
cat > /tmp/erlmcp_http_runner_$$.erl << 'ERLEOF'
-module(erlmcp_http_runner).
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
        storage_path => "/tmp/erlmcp_secrets.enc",
        encryption_key_path => "/tmp/erlmcp_master.key"
    }),

    %% Store a demo secret
    erlmcp_secrets:set_secret(<<"api_key">>, <<"sk-demo-key-12345">>),

    %% Start MCP server with HTTP transport
    {ok, Server} = erlmcp_server:start_link(
        <<"http-mcp-server">>,
        #{
            transport => http_server,
            http_port => Port,
            capabilities => #{
                resources => true,
                tools => true,
                prompts => true,
                logging => true
            }
        }
    ),

    io:format("✓ HTTP server listening on http://localhost:~w/mcp~n", [Port]),
    io:format("✓ Ready to accept MCP requests~n~n"),

    %% Add resources
    ok = erlmcp_server:add_resource(
        Server,
        <<"mcp://config">>,
        <<"Server Configuration">>,
        fun(_Uri) ->
            jsx:encode(#{
                version => <<"2.1.0">>,
                transport => <<"http">>,
                port => Port,
                capabilities => [<<"resources">>, <<"tools">>, <<"prompts">>]
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
        <<"fetch_external_data">>,
        <<"Fetch data from external API">>,
        fun(_Args) ->
            {ok, Key} = erlmcp_secrets:get_secret(<<"api_key">>),
            jsx:encode(#{
                status => success,
                message => <<"Simulated API response">>,
                authenticated => true,
                timestamp => erlang:system_time(second)
            })
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"endpoint">> => #{<<"type">> => <<"string">>}
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
                    <<"text">> => <<"Generate a ", Type/binary, " report.">>
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

    %% Keep running
    receive
        stop -> ok
    end.
ERLEOF

# Trap Ctrl+C to cleanup
trap "echo ''; echo 'Shutting down...'; rm -f /tmp/erlmcp_http_runner_$$.erl /tmp/erlmcp_http_runner_$$.beam /tmp/erlmcp_secrets.enc /tmp/erlmcp_master.key; exit 0" INT TERM

# Start the server
erl -pa _build/default/lib/*/ebin \
    -noshell \
    -eval "c('/tmp/erlmcp_http_runner_$$.erl'), erlmcp_http_runner:start(['$HTTP_PORT'])."

# Cleanup
rm -f /tmp/erlmcp_http_runner_$$.erl /tmp/erlmcp_http_runner_$$.beam /tmp/erlmcp_secrets.enc /tmp/erlmcp_master.key
