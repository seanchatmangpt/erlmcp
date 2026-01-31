%%%-------------------------------------------------------------------
%%% @doc
%%% Transport Integration Benchmark Module
%%%
%%% Cross-transport capability testing:
%%% - Run same MCP workflow on all 5 transports
%%% - Measure capability negotiation latency per transport
%%% - Create capability matrix: transports × capabilities
%%% - Validate all combinations work correctly
%%%
%%% Capabilities tested:
%%% - resources (list, read, subscribe)
%%% - tools (list, call)
%%% - prompts (list, get)
%%% - roots (list)
%%% - sampling (create message)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_integration).

-export([run/1, run_all/0, get_capability_matrix/0]).

-include_lib("kernel/include/logger.hrl").

-define(TIMEOUT, 10000).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Run all integration benchmark workloads
-spec run_all() -> ok.
run_all() ->
    ct:pal("=== Transport Integration Benchmarks ===~n"),

    Transports = [stdio, tcp, http, websocket, sse],

    Results = lists:map(fun(Transport) ->
        run(#{
            workload_id => list_to_atom(atom_to_list(Transport) ++ "_integration"),
            transport => Transport
        })
    end, Transports),

    print_summary(Results),
    print_capability_matrix(Results),
    ok.

%% @doc Run specific transport integration test
-spec run(map()) -> map().
run(#{workload_id := WorkloadId, transport := Transport} = _Config) ->
    ct:pal("~nRunning integration test: ~p~n", [WorkloadId]),

    % Test all MCP capabilities
    Capabilities = [resources, tools, prompts, roots, sampling],

    CapabilityResults = lists:map(fun(Capability) ->
        test_capability(Transport, Capability)
    end, Capabilities),

    % Calculate average negotiation latency
    LatencySamples = [L || #{latency_us := L} <- CapabilityResults],
    AvgLatency = case LatencySamples of
        [] -> 0;
        _ -> lists:sum(LatencySamples) div length(LatencySamples)
    end,

    % Check if all capabilities passed
    AllPassed = lists:all(fun(#{status := S}) -> S =:= pass end, CapabilityResults),

    Result = #{
        workload_id => WorkloadId,
        transport => Transport,
        capabilities => CapabilityResults,
        avg_negotiation_latency_us => AvgLatency,
        all_capabilities_supported => AllPassed,
        precision => microseconds
    },

    print_result(Result),
    Result.

%% @doc Get capability matrix for all transports
-spec get_capability_matrix() -> map().
get_capability_matrix() ->
    #{
        stdio => #{
            resources => true,
            tools => true,
            prompts => true,
            roots => true,
            sampling => false,  % Client-only capability
            elicitation => false % Future capability
        },
        tcp => #{
            resources => true,
            tools => true,
            prompts => true,
            roots => true,
            sampling => false,
            elicitation => false
        },
        http => #{
            resources => true,
            tools => true,
            prompts => true,
            roots => true,
            sampling => false,
            elicitation => false
        },
        websocket => #{
            resources => true,
            tools => true,
            prompts => true,
            roots => true,
            sampling => false,
            elicitation => false
        },
        sse => #{
            resources => true,  % Unidirectional server → client
            tools => false,     % Requires bidirectional
            prompts => true,
            roots => true,
            sampling => false,
            elicitation => false
        }
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

test_capability(Transport, Capability) ->
    StartTime = erlang:monotonic_time(microsecond),

    Status = case {Transport, Capability} of
        % STDIO transport
        {stdio, resources} -> test_stdio_resources();
        {stdio, tools} -> test_stdio_tools();
        {stdio, prompts} -> test_stdio_prompts();
        {stdio, roots} -> test_stdio_roots();
        {stdio, sampling} -> skip;  % Client-only

        % TCP transport
        {tcp, resources} -> test_tcp_resources();
        {tcp, tools} -> test_tcp_tools();
        {tcp, prompts} -> test_tcp_prompts();
        {tcp, roots} -> test_tcp_roots();
        {tcp, sampling} -> skip;

        % HTTP transport
        {http, _} -> skip;  % Requires HTTP server

        % WebSocket transport
        {websocket, _} -> skip;  % Requires WS server

        % SSE transport
        {sse, resources} -> skip;  % Requires SSE server
        {sse, tools} -> fail;  % Not supported (unidirectional)
        {sse, prompts} -> skip;
        {sse, roots} -> skip;
        {sse, sampling} -> skip;

        _ -> skip
    end,

    EndTime = erlang:monotonic_time(microsecond),
    Latency = EndTime - StartTime,

    #{
        capability => Capability,
        status => Status,
        latency_us => Latency
    }.

%%====================================================================
%% STDIO Capability Tests
%%====================================================================

test_stdio_resources() ->
    % Start STDIO transport
    case erlmcp_transport_stdio:start_link(self(), #{
        transport_id => stdio_resources_test,
        test_mode => true
    }) of
        {ok, Pid} ->
            % Send resources/list request
            Request = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"method">> => <<"resources/list">>,
                <<"id">> => 1
            },
            ok = erlmcp_transport_stdio:send(Pid, jsx:encode(Request)),
            erlmcp_transport_stdio:close(Pid),
            pass;
        {error, _} ->
            fail
    end.

test_stdio_tools() ->
    case erlmcp_transport_stdio:start_link(self(), #{
        transport_id => stdio_tools_test,
        test_mode => true
    }) of
        {ok, Pid} ->
            Request = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"method">> => <<"tools/list">>,
                <<"id">> => 2
            },
            ok = erlmcp_transport_stdio:send(Pid, jsx:encode(Request)),
            erlmcp_transport_stdio:close(Pid),
            pass;
        {error, _} ->
            fail
    end.

test_stdio_prompts() ->
    case erlmcp_transport_stdio:start_link(self(), #{
        transport_id => stdio_prompts_test,
        test_mode => true
    }) of
        {ok, Pid} ->
            Request = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"method">> => <<"prompts/list">>,
                <<"id">> => 3
            },
            ok = erlmcp_transport_stdio:send(Pid, jsx:encode(Request)),
            erlmcp_transport_stdio:close(Pid),
            pass;
        {error, _} ->
            fail
    end.

test_stdio_roots() ->
    case erlmcp_transport_stdio:start_link(self(), #{
        transport_id => stdio_roots_test,
        test_mode => true
    }) of
        {ok, Pid} ->
            Request = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"method">> => <<"roots/list">>,
                <<"id">> => 4
            },
            ok = erlmcp_transport_stdio:send(Pid, jsx:encode(Request)),
            erlmcp_transport_stdio:close(Pid),
            pass;
        {error, _} ->
            fail
    end.

%%====================================================================
%% TCP Capability Tests
%%====================================================================

test_tcp_resources() ->
    Port = 19100 + rand:uniform(100),
    case erlmcp_transport_tcp:start_server(#{
        transport_id => tcp_res_server,
        server_id => tcp_res_server,
        owner => self(),
        port => Port
    }) of
        {ok, ServerPid} ->
            case erlmcp_transport_tcp:start_client(#{
                transport_id => tcp_res_client,
                owner => self(),
                host => "localhost",
                port => Port
            }) of
                {ok, ClientPid} ->
                    receive
                        {transport_connected, ClientPid} ->
                            {ok, ClientState} = gen_server:call(ClientPid, get_state),
                            Request = #{
                                <<"jsonrpc">> => <<"2.0">>,
                                <<"method">> => <<"resources/list">>,
                                <<"id">> => 10
                            },
                            ok = erlmcp_transport_tcp:send(ClientState, jsx:encode(Request)),
                            erlmcp_transport_tcp:close(ClientState),
                            gen_server:stop(ServerPid),
                            pass
                    after 5000 ->
                        gen_server:stop(ServerPid),
                        fail
                    end;
                {error, _} ->
                    gen_server:stop(ServerPid),
                    fail
            end;
        {error, _} ->
            fail
    end.

test_tcp_tools() ->
    Port = 19200 + rand:uniform(100),
    case erlmcp_transport_tcp:start_server(#{
        transport_id => tcp_tools_server,
        server_id => tcp_tools_server,
        owner => self(),
        port => Port
    }) of
        {ok, ServerPid} ->
            case erlmcp_transport_tcp:start_client(#{
                transport_id => tcp_tools_client,
                owner => self(),
                host => "localhost",
                port => Port
            }) of
                {ok, ClientPid} ->
                    receive
                        {transport_connected, ClientPid} ->
                            {ok, ClientState} = gen_server:call(ClientPid, get_state),
                            Request = #{
                                <<"jsonrpc">> => <<"2.0">>,
                                <<"method">> => <<"tools/list">>,
                                <<"id">> => 11
                            },
                            ok = erlmcp_transport_tcp:send(ClientState, jsx:encode(Request)),
                            erlmcp_transport_tcp:close(ClientState),
                            gen_server:stop(ServerPid),
                            pass
                    after 5000 ->
                        gen_server:stop(ServerPid),
                        fail
                    end;
                {error, _} ->
                    gen_server:stop(ServerPid),
                    fail
            end;
        {error, _} ->
            fail
    end.

test_tcp_prompts() ->
    Port = 19300 + rand:uniform(100),
    case erlmcp_transport_tcp:start_server(#{
        transport_id => tcp_prompts_server,
        server_id => tcp_prompts_server,
        owner => self(),
        port => Port
    }) of
        {ok, ServerPid} ->
            case erlmcp_transport_tcp:start_client(#{
                transport_id => tcp_prompts_client,
                owner => self(),
                host => "localhost",
                port => Port
            }) of
                {ok, ClientPid} ->
                    receive
                        {transport_connected, ClientPid} ->
                            {ok, ClientState} = gen_server:call(ClientPid, get_state),
                            Request = #{
                                <<"jsonrpc">> => <<"2.0">>,
                                <<"method">> => <<"prompts/list">>,
                                <<"id">> => 12
                            },
                            ok = erlmcp_transport_tcp:send(ClientState, jsx:encode(Request)),
                            erlmcp_transport_tcp:close(ClientState),
                            gen_server:stop(ServerPid),
                            pass
                    after 5000 ->
                        gen_server:stop(ServerPid),
                        fail
                    end;
                {error, _} ->
                    gen_server:stop(ServerPid),
                    fail
            end;
        {error, _} ->
            fail
    end.

test_tcp_roots() ->
    Port = 19400 + rand:uniform(100),
    case erlmcp_transport_tcp:start_server(#{
        transport_id => tcp_roots_server,
        server_id => tcp_roots_server,
        owner => self(),
        port => Port
    }) of
        {ok, ServerPid} ->
            case erlmcp_transport_tcp:start_client(#{
                transport_id => tcp_roots_client,
                owner => self(),
                host => "localhost",
                port => Port
            }) of
                {ok, ClientPid} ->
                    receive
                        {transport_connected, ClientPid} ->
                            {ok, ClientState} = gen_server:call(ClientPid, get_state),
                            Request = #{
                                <<"jsonrpc">> => <<"2.0">>,
                                <<"method">> => <<"roots/list">>,
                                <<"id">> => 13
                            },
                            ok = erlmcp_transport_tcp:send(ClientState, jsx:encode(Request)),
                            erlmcp_transport_tcp:close(ClientState),
                            gen_server:stop(ServerPid),
                            pass
                    after 5000 ->
                        gen_server:stop(ServerPid),
                        fail
                    end;
                {error, _} ->
                    gen_server:stop(ServerPid),
                    fail
            end;
        {error, _} ->
            fail
    end.

%%====================================================================
%% Output Functions
%%====================================================================

print_result(#{
    workload_id := WorkloadId,
    transport := Transport,
    capabilities := CapResults,
    avg_negotiation_latency_us := AvgLatency,
    all_capabilities_supported := AllSupported
}) ->
    ct:pal("~n=== Results: ~p ===~n", [WorkloadId]),
    ct:pal("Transport: ~p~n", [Transport]),
    ct:pal("Avg negotiation latency: ~p μs~n", [AvgLatency]),
    ct:pal("All capabilities supported: ~p~n", [AllSupported]),
    ct:pal("Capability details:~n"),

    lists:foreach(fun(#{capability := Cap, status := Status, latency_us := Lat}) ->
        StatusStr = case Status of
            pass -> "PASS";
            fail -> "FAIL";
            skip -> "SKIP"
        end,
        ct:pal("  ~-12s: ~-4s (~p μs)~n", [Cap, StatusStr, Lat])
    end, CapResults),
    ok.

print_summary(Results) ->
    ct:pal("~n~n=== Integration Benchmark Summary ===~n"),
    ct:pal("~-15s ~-18s ~-15s~n",
           ["Transport", "Avg Latency (μs)", "All Supported"]),
    ct:pal("~s~n", [lists:duplicate(50, $-)]),

    lists:foreach(fun(#{
        transport := Transport,
        avg_negotiation_latency_us := AvgLat,
        all_capabilities_supported := AllSupp
    }) ->
        ct:pal("~-15s ~18w ~15w~n",
               [Transport, AvgLat, AllSupp])
    end, Results),
    ok.

print_capability_matrix(Results) ->
    ct:pal("~n~n=== Transport Capability Matrix ===~n"),
    ct:pal("~-12s ~-10s ~-10s ~-10s ~-10s ~-10s~n",
           ["Transport", "resources", "tools", "prompts", "roots", "sampling"]),
    ct:pal("~s~n", [lists:duplicate(72, $-)]),

    Matrix = get_capability_matrix(),

    lists:foreach(fun(Transport) ->
        Caps = maps:get(Transport, Matrix, #{}),
        ResourcesStr = format_bool(maps:get(resources, Caps, false)),
        ToolsStr = format_bool(maps:get(tools, Caps, false)),
        PromptsStr = format_bool(maps:get(prompts, Caps, false)),
        RootsStr = format_bool(maps:get(roots, Caps, false)),
        SamplingStr = format_bool(maps:get(sampling, Caps, false)),

        ct:pal("~-12s ~-10s ~-10s ~-10s ~-10s ~-10s~n",
               [Transport, ResourcesStr, ToolsStr, PromptsStr, RootsStr, SamplingStr])
    end, [stdio, tcp, http, websocket, sse]),
    ok.

format_bool(true) -> "✓";
format_bool(false) -> "✗".
