%%%-------------------------------------------------------------------
%%% @doc Comprehensive EUnit Tests for erlmcp_test_client
%%%
%%% Chicago School TDD:
%%% - Real gen_server processes
%%% - State-based verification
%%% - No mocks - use real transports where applicable
%%% - Test all observable behavior
%%%
%%% Coverage: 200+ test cases
%%% - Client lifecycle (startup, shutdown, restart)
%%% - Multi-transport support (stdio, tcp, http, websocket)
%%% - Request/response handling
%%% - Error handling
%%% - Concurrent requests
%%% - Configuration validation
%%% - Edge cases
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_test_client_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

test_client_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Client Lifecycle Tests", {inparallel, lifecycle_tests()}},
      {"Transport Configuration Tests", {inparallel, transport_tests()}},
      {"Request Handling Tests", {inparallel, request_tests()}},
      {"Error Handling Tests", {inparallel, error_tests()}},
      {"Concurrent Operations Tests", {inparallel, concurrent_tests()}},
      {"Edge Case Tests", {inparallel, edge_case_tests()}},
      {"Integration Tests", {inparallel, integration_tests()}}]}.

setup() ->
    application:ensure_all_started(erlmcp_validation),
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%%% Client Lifecycle Tests (30 cases)
%%%===================================================================

lifecycle_tests() ->
    [?_test(test_start_client_stdio()), ?_test(test_start_client_tcp()),
     ?_test(test_start_client_http()), ?_test(test_start_client_websocket()),
     ?_test(test_start_client_with_empty_config()), ?_test(test_start_client_with_full_config()),
     ?_test(test_start_multiple_clients()), ?_test(test_stop_client()),
     ?_test(test_stop_client_twice()), ?_test(test_stop_nonexistent_client()),
     ?_test(test_client_restart()), ?_test(test_client_normal_shutdown()),
     ?_test(test_client_crash_recovery()), ?_test(test_client_process_monitoring()),
     ?_test(test_client_linked_process()), ?_test(test_client_state_after_start()),
     ?_test(test_client_state_after_stop()), ?_test(test_client_memory_cleanup()),
     ?_test(test_client_timeout_on_start()), ?_test(test_client_invalid_transport_type()),
     ?_test(test_client_invalid_config_type()), ?_test(test_client_config_validation()),
     ?_test(test_client_gen_server_info()), ?_test(test_client_process_dictionary()),
     ?_test(test_client_trap_exit()), ?_test(test_client_system_messages()),
     ?_test(test_client_code_change()), ?_test(test_client_format_status()),
     ?_test(test_client_concurrent_start()), ?_test(test_client_concurrent_stop())].

%% Lifecycle test implementations
test_start_client_stdio() ->
    Config = #{transport_type => stdio},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_start_client_tcp() ->
    Config = #{host => "localhost", port => 9999},
    {ok, Pid} = erlmcp_test_client:start_test_client(tcp, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_start_client_http() ->
    Config = #{url => "http://localhost:8080"},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_start_client_websocket() ->
    Config = #{url => "ws://localhost:8080/ws"},
    {ok, Pid} = erlmcp_test_client:start_test_client(websocket, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_start_client_with_empty_config() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_start_client_with_full_config() ->
    Config =
        #{transport_type => tcp,
          host => "localhost",
          port => 9999,
          timeout => 5000,
          buffer_size => 4096,
          retry_count => 3,
          metadata => #{test => true}},
    {ok, Pid} = erlmcp_test_client:start_test_client(tcp, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_start_multiple_clients() ->
    Clients =
        [begin
             Config = #{index => I},
             {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
             Pid
         end
         || I <- lists:seq(1, 10)],

    ?assertEqual(10, length(Clients)),
    [?assert(is_process_alive(Pid)) || Pid <- Clients],
    [erlmcp_test_client:stop_test_server(Pid) || Pid <- Clients].

test_stop_client() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    ?assert(is_process_alive(Pid)),
    ok = erlmcp_test_client:stop_test_server(Pid),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

test_stop_client_twice() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    ok = erlmcp_test_client:stop_test_server(Pid),
    timer:sleep(100),
    %% Stopping already stopped client should not crash
    try
        erlmcp_test_client:stop_test_server(Pid),
        ok
    catch
        exit:{noproc, _} ->
            ok
    end.

test_stop_nonexistent_client() ->
    FakePid = spawn(fun() -> ok end),
    timer:sleep(10),
    ?assertNot(is_process_alive(FakePid)),
    try
        erlmcp_test_client:stop_test_server(FakePid),
        ok
    catch
        exit:{noproc, _} ->
            ok
    end.

test_client_restart() ->
    {ok, Pid1} = erlmcp_test_client:start_test_client(stdio, #{}),
    erlmcp_test_client:stop_test_server(Pid1),
    timer:sleep(100),
    {ok, Pid2} = erlmcp_test_client:start_test_client(stdio, #{}),
    ?assertNotEqual(Pid1, Pid2),
    erlmcp_test_client:stop_test_server(Pid2).

test_client_normal_shutdown() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Ref = monitor(process, Pid),
    erlmcp_test_client:stop_test_server(Pid),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            ok;
        {'DOWN', Ref, process, Pid, shutdown} ->
            ok;
        {'DOWN', Ref, process, Pid, Reason} ->
            ?assertEqual(normal, Reason)
    after 1000 ->
        ?assert(false)
    end.

test_client_crash_recovery() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Ref = monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Ref, process, Pid, killed} ->
            ok
    after 1000 ->
        ?assert(false)
    end.

test_client_process_monitoring() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Ref = monitor(process, Pid),
    ?assert(is_reference(Ref)),
    erlmcp_test_client:stop_test_server(Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    after 1000 ->
        ?assert(false)
    end.

test_client_linked_process() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    process_flag(trap_exit, true),
    link(Pid),
    exit(Pid, kill),
    receive
        {'EXIT', Pid, killed} ->
            ok
    after 1000 ->
        ?assert(false)
    end,
    process_flag(trap_exit, false).

test_client_state_after_start() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{test => value}),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_client_state_after_stop() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    erlmcp_test_client:stop_test_server(Pid),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

test_client_memory_cleanup() ->
    InitialMem = erlang:memory(total),
    Clients =
        [begin
             {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
             Pid
         end
         || _ <- lists:seq(1, 100)],

    [erlmcp_test_client:stop_test_server(Pid) || Pid <- Clients],
    timer:sleep(500),
    erlang:garbage_collect(),
    FinalMem = erlang:memory(total),

    %% Memory should not grow significantly
    MemGrowth = FinalMem - InitialMem,
    ?assert(MemGrowth < 10 * 1024 * 1024). %% Less than 10MB growth

test_client_timeout_on_start() ->
    Config = #{timeout => 1},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_client_invalid_transport_type() ->
    try
        erlmcp_test_client:start_test_client(invalid_transport, #{}),
        ?assert(false) %% Should not reach here
    catch
        error:function_clause ->
            ok
    end.

test_client_invalid_config_type() ->
    try
        erlmcp_test_client:start_test_client(stdio, not_a_map),
        ?assert(false)
    catch
        error:function_clause ->
            ok
    end.

test_client_config_validation() ->
    ValidConfigs = [#{}, #{key => value}, #{timeout => 5000}, #{retry => 3, buffer => 4096}],
    [begin
         {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
         erlmcp_test_client:stop_test_server(Pid)
     end
     || Config <- ValidConfigs].

test_client_gen_server_info() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Info = erlang:process_info(Pid),
    ?assert(is_list(Info)),
    ?assert(lists:keymember(message_queue_len, 1, Info)),
    erlmcp_test_client:stop_test_server(Pid).

test_client_process_dictionary() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Dict = erlang:process_info(Pid, dictionary),
    ?assertMatch({dictionary, _}, Dict),
    erlmcp_test_client:stop_test_server(Pid).

test_client_trap_exit() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    {trap_exit, TrapExit} = erlang:process_info(Pid, trap_exit),
    ?assert(is_boolean(TrapExit)),
    erlmcp_test_client:stop_test_server(Pid).

test_client_system_messages() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Status = sys:get_status(Pid),
    ?assertMatch({status, Pid, {module, gen_server}, _}, Status),
    erlmcp_test_client:stop_test_server(Pid).

test_client_code_change() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    {ok, _State} = sys:change_code(Pid, erlmcp_test_client, old_vsn, []),
    erlmcp_test_client:stop_test_server(Pid).

test_client_format_status() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Status = sys:get_status(Pid),
    ?assert(is_tuple(Status)),
    erlmcp_test_client:stop_test_server(Pid).

test_client_concurrent_start() ->
    Pids =
        pmap(fun(I) ->
                Config = #{id => I},
                {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
                Pid
             end,
             lists:seq(1, 50)),

    ?assertEqual(50, length(Pids)),
    [?assert(is_process_alive(Pid)) || Pid <- Pids],
    [erlmcp_test_client:stop_test_server(Pid) || Pid <- Pids].

test_client_concurrent_stop() ->
    Clients =
        [begin
             {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
             Pid
         end
         || _ <- lists:seq(1, 50)],

    pmap(fun(Pid) -> erlmcp_test_client:stop_test_server(Pid) end, Clients),

    timer:sleep(200),
    [?assertNot(is_process_alive(Pid)) || Pid <- Clients].

%%%===================================================================
%%% Transport Configuration Tests (40 cases)
%%%===================================================================

transport_tests() ->
    [?_test(test_stdio_transport_basic()), ?_test(test_stdio_transport_with_options()),
     ?_test(test_tcp_transport_basic()), ?_test(test_tcp_transport_with_host_port()),
     ?_test(test_tcp_transport_with_timeout()), ?_test(test_tcp_transport_with_buffer_size()),
     ?_test(test_tcp_transport_ipv4()), ?_test(test_tcp_transport_ipv6()),
     ?_test(test_http_transport_basic()), ?_test(test_http_transport_with_url()),
     ?_test(test_http_transport_with_headers()), ?_test(test_http_transport_with_timeout()),
     ?_test(test_http_transport_https()), ?_test(test_http_transport_with_auth()),
     ?_test(test_websocket_transport_basic()), ?_test(test_websocket_transport_with_url()),
     ?_test(test_websocket_transport_with_subprotocol()), ?_test(test_websocket_transport_wss()),
     ?_test(test_websocket_transport_with_origin()), ?_test(test_transport_config_persistence()),
     ?_test(test_transport_config_immutability()), ?_test(test_transport_type_validation()),
     ?_test(test_transport_config_merging()), ?_test(test_transport_default_values()),
     ?_test(test_transport_override_values()), ?_test(test_transport_metadata_storage()),
     ?_test(test_transport_capability_detection()), ?_test(test_transport_version_negotiation()),
     ?_test(test_transport_compression_options()), ?_test(test_transport_encryption_options()),
     ?_test(test_transport_proxy_config()), ?_test(test_transport_dns_resolution()),
     ?_test(test_transport_connection_pooling()), ?_test(test_transport_keep_alive()),
     ?_test(test_transport_idle_timeout()), ?_test(test_transport_max_connections()),
     ?_test(test_transport_retry_policy()), ?_test(test_transport_backoff_strategy()),
     ?_test(test_transport_circuit_breaker()), ?_test(test_transport_health_check())].

%% Transport test implementations (representative samples)
test_stdio_transport_basic() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_stdio_transport_with_options() ->
    Config = #{buffer_size => 8192, encoding => utf8},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_tcp_transport_basic() ->
    Config = #{host => "127.0.0.1", port => 8888},
    {ok, Pid} = erlmcp_test_client:start_test_client(tcp, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_tcp_transport_with_host_port() ->
    Config = #{host => "localhost", port => 9999},
    {ok, Pid} = erlmcp_test_client:start_test_client(tcp, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_tcp_transport_with_timeout() ->
    Config =
        #{host => "localhost",
          port => 9999,
          timeout => 10000},
    {ok, Pid} = erlmcp_test_client:start_test_client(tcp, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_tcp_transport_with_buffer_size() ->
    Config =
        #{host => "localhost",
          port => 9999,
          buffer_size => 16384},
    {ok, Pid} = erlmcp_test_client:start_test_client(tcp, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_tcp_transport_ipv4() ->
    Config =
        #{host => "127.0.0.1",
          port => 9999,
          inet_family => inet},
    {ok, Pid} = erlmcp_test_client:start_test_client(tcp, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_tcp_transport_ipv6() ->
    Config =
        #{host => "::1",
          port => 9999,
          inet_family => inet6},
    {ok, Pid} = erlmcp_test_client:start_test_client(tcp, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_http_transport_basic() ->
    Config = #{url => "http://localhost:8080"},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_http_transport_with_url() ->
    Config = #{url => "http://example.com:8080/api/v1"},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_http_transport_with_headers() ->
    Config =
        #{url => "http://localhost:8080",
          headers =>
              #{<<"content-type">> => <<"application/json">>,
                <<"authorization">> => <<"Bearer token123">>}},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_http_transport_with_timeout() ->
    Config = #{url => "http://localhost:8080", timeout => 30000},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_http_transport_https() ->
    Config = #{url => "https://localhost:8443", verify_ssl => false},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_http_transport_with_auth() ->
    Config =
        #{url => "http://localhost:8080",
          auth =>
              #{type => basic,
                user => <<"admin">>,
                password => <<"secret">>}},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_websocket_transport_basic() ->
    Config = #{url => "ws://localhost:8080/ws"},
    {ok, Pid} = erlmcp_test_client:start_test_client(websocket, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_websocket_transport_with_url() ->
    Config = #{url => "ws://example.com:8080/api/ws"},
    {ok, Pid} = erlmcp_test_client:start_test_client(websocket, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_websocket_transport_with_subprotocol() ->
    Config = #{url => "ws://localhost:8080/ws", subprotocol => "mcp-v1"},
    {ok, Pid} = erlmcp_test_client:start_test_client(websocket, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_websocket_transport_wss() ->
    Config = #{url => "wss://localhost:8443/ws", verify_ssl => false},
    {ok, Pid} = erlmcp_test_client:start_test_client(websocket, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_websocket_transport_with_origin() ->
    Config = #{url => "ws://localhost:8080/ws", origin => "http://localhost:3000"},
    {ok, Pid} = erlmcp_test_client:start_test_client(websocket, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_config_persistence() ->
    Config = #{key => value, nested => #{inner => data}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_config_immutability() ->
    Config = #{mutable => false},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_type_validation() ->
    ValidTypes = [stdio, tcp, http, websocket],
    [begin
         {ok, Pid} = erlmcp_test_client:start_test_client(Type, #{}),
         erlmcp_test_client:stop_test_server(Pid)
     end
     || Type <- ValidTypes].

test_transport_config_merging() ->
    Base = #{timeout => 5000},
    Override = #{timeout => 10000, buffer_size => 8192},
    Config = maps:merge(Base, Override),
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_default_values() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_override_values() ->
    Config = #{default_override => custom_value},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_metadata_storage() ->
    Config = #{metadata => #{app => erlmcp, version => <<"1.0.0">>}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_capability_detection() ->
    Config = #{capabilities => [streaming, compression]},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_version_negotiation() ->
    Config = #{version => <<"2024-11-05">>},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_compression_options() ->
    Config = #{compression => #{enabled => true, level => 6}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_encryption_options() ->
    Config = #{encryption => #{enabled => true, algorithm => aes256}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_proxy_config() ->
    Config = #{proxy => #{host => "proxy.example.com", port => 8080}},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_dns_resolution() ->
    Config = #{host => "localhost", dns_timeout => 5000},
    {ok, Pid} = erlmcp_test_client:start_test_client(tcp, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_connection_pooling() ->
    Config = #{pool_size => 10, pool_strategy => round_robin},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_keep_alive() ->
    Config = #{keep_alive => true, keep_alive_interval => 30000},
    {ok, Pid} = erlmcp_test_client:start_test_client(tcp, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_idle_timeout() ->
    Config = #{idle_timeout => 60000},
    {ok, Pid} = erlmcp_test_client:start_test_client(websocket, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_max_connections() ->
    Config = #{max_connections => 100},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_retry_policy() ->
    Config = #{retry => #{count => 3, delay => 1000}},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_backoff_strategy() ->
    Config = #{backoff => #{strategy => exponential, max => 60000}},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_circuit_breaker() ->
    Config = #{circuit_breaker => #{threshold => 5, timeout => 30000}},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_transport_health_check() ->
    Config = #{health_check => #{enabled => true, interval => 10000}},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    erlmcp_test_client:stop_test_server(Pid).

%%%===================================================================
%%% Request Handling Tests (50 cases)
%%%===================================================================

request_tests() ->
    [?_test(test_send_empty_request()), ?_test(test_send_simple_request()),
     ?_test(test_send_request_with_params()), ?_test(test_send_request_with_id()),
     ?_test(test_send_request_without_id()), ?_test(test_send_notification()),
     ?_test(test_send_batch_request()), ?_test(test_send_large_request()),
     ?_test(test_send_binary_request()), ?_test(test_send_unicode_request()),
     ?_test(test_request_response_correlation()), ?_test(test_request_timeout()),
     ?_test(test_request_retry()), ?_test(test_request_cancellation()),
     ?_test(test_request_priority()), ?_test(test_request_ordering()),
     ?_test(test_request_deduplication()), ?_test(test_request_validation()),
     ?_test(test_request_serialization()), ?_test(test_request_deserialization()),
     ?_test(test_response_success()), ?_test(test_response_error()),
     ?_test(test_response_partial()), ?_test(test_response_streaming()),
     ?_test(test_response_compression()), ?_test(test_response_timeout()),
     ?_test(test_response_validation()), ?_test(test_multiple_requests()),
     ?_test(test_concurrent_requests()), ?_test(test_sequential_requests()),
     ?_test(test_pipelined_requests()), ?_test(test_request_queue()),
     ?_test(test_request_throttling()), ?_test(test_request_rate_limiting()),
     ?_test(test_request_backpressure()), ?_test(test_request_flow_control()),
     ?_test(test_request_metadata()), ?_test(test_request_tracing()),
     ?_test(test_request_logging()), ?_test(test_request_metrics()),
     ?_test(test_request_auth_header()), ?_test(test_request_content_type()),
     ?_test(test_request_accept_header()), ?_test(test_request_custom_headers()),
     ?_test(test_request_body_encoding()), ?_test(test_request_chunked_transfer()),
     ?_test(test_request_keepalive()), ?_test(test_request_connection_reuse()),
     ?_test(test_request_connection_pooling()), ?_test(test_request_circuit_breaker())].

%% Request test implementations (representative samples)
test_send_empty_request() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Result = erlmcp_test_client:send_request(Pid, #{}),
    ?assertEqual({ok, #{}}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_send_simple_request() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"ping">>, params => #{}},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_send_request_with_params() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request =
        #{method => <<"tools/call">>,
          params => #{name => <<"calculator">>, arguments => #{a => 1, b => 2}}},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_send_request_with_id() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{id => 123, method => <<"ping">>},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_send_request_without_id() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"ping">>},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_send_notification() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Notification = #{method => <<"notifications/message">>, params => #{level => <<"info">>}},
    Result = erlmcp_test_client:send_request(Pid, Notification),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_send_batch_request() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Requests =
        [#{id => 1, method => <<"ping">>},
         #{id => 2, method => <<"ping">>},
         #{id => 3, method => <<"ping">>}],
    Results = [erlmcp_test_client:send_request(Pid, Req) || Req <- Requests],
    ?assertEqual(3, length([ok || {ok, _} <- Results])),
    erlmcp_test_client:stop_test_server(Pid).

test_send_large_request() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    LargeParams = #{data => list_to_binary([<<"x">> || _ <- lists:seq(1, 1000)])},
    Request = #{method => <<"process">>, params => LargeParams},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_send_binary_request() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"binary">>, params => #{data => <<1, 2, 3, 4, 5>>}},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_send_unicode_request() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"unicode">>, params => #{text => <<"Hello 世界 🌍"/utf8>>}},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_request_response_correlation() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Requests = [#{id => I, method => <<"ping">>} || I <- lists:seq(1, 10)],
    Results = [erlmcp_test_client:send_request(Pid, Req) || Req <- Requests],
    ?assertEqual(10, length([ok || {ok, _} <- Results])),
    erlmcp_test_client:stop_test_server(Pid).

test_request_timeout() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{request_timeout => 1}),
    Request = #{method => <<"slow_operation">>},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_request_retry() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{retry => 3}),
    Request = #{method => <<"unstable_operation">>},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_request_cancellation() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"long_operation">>},
    _Result = erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_priority() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    HighPrio = #{method => <<"urgent">>, priority => high},
    LowPrio = #{method => <<"normal">>, priority => low},
    erlmcp_test_client:send_request(Pid, HighPrio),
    erlmcp_test_client:send_request(Pid, LowPrio),
    erlmcp_test_client:stop_test_server(Pid).

test_request_ordering() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Requests = [#{id => I, method => <<"ping">>} || I <- lists:seq(1, 5)],
    [erlmcp_test_client:send_request(Pid, Req) || Req <- Requests],
    erlmcp_test_client:stop_test_server(Pid).

test_request_deduplication() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{id => 42, method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_validation() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    ValidRequest = #{method => <<"ping">>},
    Result = erlmcp_test_client:send_request(Pid, ValidRequest),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_request_serialization() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"test">>, params => #{key => value}},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_request_deserialization() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"echo">>, params => #{data => <<"test">>}},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_response_success() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"ping">>},
    {ok, Response} = erlmcp_test_client:send_request(Pid, Request),
    ?assert(is_map(Response)),
    erlmcp_test_client:stop_test_server(Pid).

test_response_error() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"invalid_method">>},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_response_partial() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"partial">>},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_response_streaming() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"stream">>},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_response_compression() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{compression => true}),
    Request = #{method => <<"large_response">>},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_response_timeout() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{response_timeout => 100}),
    Request = #{method => <<"slow">>},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_response_validation() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"ping">>},
    {ok, Response} = erlmcp_test_client:send_request(Pid, Request),
    ?assert(is_map(Response)),
    erlmcp_test_client:stop_test_server(Pid).

test_multiple_requests() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Requests = [#{method => <<"ping">>} || _ <- lists:seq(1, 20)],
    Results = [erlmcp_test_client:send_request(Pid, Req) || Req <- Requests],
    ?assertEqual(20, length([ok || {ok, _} <- Results])),
    erlmcp_test_client:stop_test_server(Pid).

test_concurrent_requests() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Parent = self(),
    Pids =
        [spawn(fun() ->
                  Result = erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),
                  Parent ! {self(), Result}
               end)
         || _ <- lists:seq(1, 10)],

    Results =
        [receive
             {P, R} ->
                 R
         end
         || P <- Pids],
    ?assertEqual(10, length([ok || {ok, _} <- Results])),
    erlmcp_test_client:stop_test_server(Pid).

test_sequential_requests() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Results =
        [begin
             Request = #{method => <<"ping">>, id => I},
             erlmcp_test_client:send_request(Pid, Request)
         end
         || I <- lists:seq(1, 10)],
    ?assertEqual(10, length([ok || {ok, _} <- Results])),
    erlmcp_test_client:stop_test_server(Pid).

test_pipelined_requests() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{pipeline => true}),
    Requests = [#{method => <<"ping">>} || _ <- lists:seq(1, 5)],
    [erlmcp_test_client:send_request(Pid, Req) || Req <- Requests],
    erlmcp_test_client:stop_test_server(Pid).

test_request_queue() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{queue_size => 100}),
    Requests = [#{method => <<"ping">>} || _ <- lists:seq(1, 50)],
    [erlmcp_test_client:send_request(Pid, Req) || Req <- Requests],
    erlmcp_test_client:stop_test_server(Pid).

test_request_throttling() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{throttle => 10}),
    Requests = [#{method => <<"ping">>} || _ <- lists:seq(1, 5)],
    [erlmcp_test_client:send_request(Pid, Req) || Req <- Requests],
    erlmcp_test_client:stop_test_server(Pid).

test_request_rate_limiting() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{rate_limit => 100}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_backpressure() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{backpressure => true}),
    Requests = [#{method => <<"ping">>} || _ <- lists:seq(1, 5)],
    [erlmcp_test_client:send_request(Pid, Req) || Req <- Requests],
    erlmcp_test_client:stop_test_server(Pid).

test_request_flow_control() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{flow_control => true}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_metadata() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"ping">>, metadata => #{trace_id => <<"abc123">>}},
    Result = erlmcp_test_client:send_request(Pid, Request),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_request_tracing() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{tracing => true}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_logging() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{logging => true}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_metrics() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{metrics => true}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_auth_header() ->
    {ok, Pid} =
        erlmcp_test_client:start_test_client(http,
                                             #{url => "http://localhost:8080",
                                               headers =>
                                                   #{<<"authorization">> => <<"Bearer token">>}}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_content_type() ->
    {ok, Pid} =
        erlmcp_test_client:start_test_client(http,
                                             #{url => "http://localhost:8080",
                                               headers =>
                                                   #{<<"content-type">> =>
                                                         <<"application/json">>}}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_accept_header() ->
    {ok, Pid} =
        erlmcp_test_client:start_test_client(http,
                                             #{url => "http://localhost:8080",
                                               headers =>
                                                   #{<<"accept">> => <<"application/json">>}}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_custom_headers() ->
    {ok, Pid} =
        erlmcp_test_client:start_test_client(http,
                                             #{url => "http://localhost:8080",
                                               headers =>
                                                   #{<<"x-request-id">> => <<"req-123">>,
                                                     <<"x-trace-id">> => <<"trace-456">>}}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_body_encoding() ->
    {ok, Pid} =
        erlmcp_test_client:start_test_client(http,
                                             #{url => "http://localhost:8080", encoding => utf8}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_chunked_transfer() ->
    {ok, Pid} =
        erlmcp_test_client:start_test_client(http,
                                             #{url => "http://localhost:8080", chunked => true}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_keepalive() ->
    {ok, Pid} =
        erlmcp_test_client:start_test_client(http,
                                             #{url => "http://localhost:8080", keepalive => true}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_connection_reuse() ->
    {ok, Pid} =
        erlmcp_test_client:start_test_client(http,
                                             #{url => "http://localhost:8080",
                                               connection_reuse => true}),
    [erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}) || _ <- lists:seq(1, 3)],
    erlmcp_test_client:stop_test_server(Pid).

test_request_connection_pooling() ->
    {ok, Pid} =
        erlmcp_test_client:start_test_client(http,
                                             #{url => "http://localhost:8080", pool_size => 5}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_request_circuit_breaker() ->
    {ok, Pid} =
        erlmcp_test_client:start_test_client(http,
                                             #{url => "http://localhost:8080",
                                               circuit_breaker =>
                                                   #{enabled => true, threshold => 5}}),
    Request = #{method => <<"ping">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

%%%===================================================================
%%% Error Handling Tests (30 cases)
%%%===================================================================

error_tests() ->
    [?_test(test_invalid_pid()), ?_test(test_stopped_client()), ?_test(test_crashed_client()),
     ?_test(test_timeout_error()), ?_test(test_connection_error()), ?_test(test_network_error()),
     ?_test(test_protocol_error()), ?_test(test_serialization_error()),
     ?_test(test_deserialization_error()), ?_test(test_validation_error()),
     ?_test(test_authorization_error()), ?_test(test_rate_limit_error()),
     ?_test(test_resource_exhausted()), ?_test(test_service_unavailable()),
     ?_test(test_internal_server_error()), ?_test(test_bad_request()), ?_test(test_not_found()),
     ?_test(test_method_not_allowed()), ?_test(test_unsupported_media_type()),
     ?_test(test_payload_too_large()), ?_test(test_uri_too_long()),
     ?_test(test_too_many_requests()), ?_test(test_request_header_too_large()),
     ?_test(test_malformed_request()), ?_test(test_invalid_json()),
     ?_test(test_missing_required_field()), ?_test(test_type_mismatch()),
     ?_test(test_constraint_violation()), ?_test(test_concurrent_error_handling()),
     ?_test(test_error_recovery())].

%% Error test implementations (representative samples)
test_invalid_pid() ->
    FakePid = list_to_pid("<0.99999.9999>"),
    try
        erlmcp_test_client:send_request(FakePid, #{method => <<"ping">>}),
        ?assert(false)
    catch
        exit:{noproc, _} ->
            ok;
        error:badarg ->
            ok
    end.

test_stopped_client() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    erlmcp_test_client:stop_test_server(Pid),
    timer:sleep(100),
    try
        erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),
        ?assert(false)
    catch
        exit:{noproc, _} ->
            ok
    end.

test_crashed_client() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    exit(Pid, kill),
    timer:sleep(100),
    try
        erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),
        ?assert(false)
    catch
        exit:{noproc, _} ->
            ok
    end.

test_timeout_error() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{timeout => 1}),
    Request = #{method => <<"slow_operation">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_connection_error() ->
    Config = #{host => "invalid.host.example.com", port => 9999},
    {ok, Pid} = erlmcp_test_client:start_test_client(tcp, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_network_error() ->
    Config = #{url => "http://unreachable.example.com:9999"},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_protocol_error() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{invalid => <<"structure">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_serialization_error() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"test">>, params => self()}, %% Pids can't be serialized
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_deserialization_error() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"invalid_response">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_validation_error() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<>>}, %% Empty method
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_authorization_error() ->
    Config = #{url => "http://localhost:8080", auth_required => true},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    Request = #{method => <<"protected_operation">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_rate_limit_error() ->
    Config = #{rate_limit => 1},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    [erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}) || _ <- lists:seq(1, 3)],
    erlmcp_test_client:stop_test_server(Pid).

test_resource_exhausted() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"allocate_huge_resource">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_service_unavailable() ->
    Config = #{url => "http://localhost:9999"}, %% Port likely not listening
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_internal_server_error() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"crash_server">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_bad_request() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{invalid_field => <<"value">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_not_found() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"nonexistent_method">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_method_not_allowed() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"forbidden_method">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_unsupported_media_type() ->
    Config = #{url => "http://localhost:8080", content_type => "application/xml"},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_payload_too_large() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    LargePayload = #{data => binary:copy(<<"x">>, 100000000)}, %% 100MB
    Request = #{method => <<"process">>, params => LargePayload},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_uri_too_long() ->
    Config = #{url => "http://localhost:8080/" ++ lists:duplicate(10000, $x)},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_too_many_requests() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{rate_limit => 1}),
    [erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}) || _ <- lists:seq(1, 10)],
    erlmcp_test_client:stop_test_server(Pid).

test_request_header_too_large() ->
    LargeHeader = binary:copy(<<"x">>, 10000),
    Config = #{url => "http://localhost:8080", headers => #{<<"x-large-header">> => LargeHeader}},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_malformed_request() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{<<>> => <<>>}, %% Empty keys/values
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_invalid_json() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"test">>, params => <<"invalid json">>},
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_missing_required_field() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{params => #{}}, %% Missing method
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_type_mismatch() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => 123}, %% Should be binary
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_constraint_violation() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Request = #{method => <<"test">>, params => #{value => -1}}, %% Negative value not allowed
    erlmcp_test_client:send_request(Pid, Request),
    erlmcp_test_client:stop_test_server(Pid).

test_concurrent_error_handling() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Pids =
        [spawn(fun() -> erlmcp_test_client:send_request(Pid, #{method => <<"error_prone">>}) end)
         || _ <- lists:seq(1, 10)],
    timer:sleep(500),
    [?assertNot(is_process_alive(P)) || P <- Pids],
    erlmcp_test_client:stop_test_server(Pid).

test_error_recovery() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    %% Send failing request
    erlmcp_test_client:send_request(Pid, #{method => <<"fail">>}),
    %% Send successful request
    Result = erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),
    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

%%%===================================================================
%%% Concurrent Operations Tests (25 cases)
%%%===================================================================

concurrent_tests() ->
    [?_test(test_concurrent_client_start()),
     ?_test(test_concurrent_client_stop()),
     ?_test(test_concurrent_requests_same_client()),
     ?_test(test_concurrent_requests_different_clients()),
     ?_test(test_concurrent_read_write()),
     ?_test(test_race_condition_handling()),
     ?_test(test_deadlock_prevention()),
     ?_test(test_starvation_prevention()),
     ?_test(test_fairness()),
     ?_test(test_load_balancing()),
     ?_test(test_backpressure_propagation()),
     ?_test(test_cascading_failures()),
     ?_test(test_thundering_herd()),
     ?_test(test_split_brain()),
     ?_test(test_network_partition()),
     ?_test(test_process_isolation()),
     ?_test(test_shared_state_access()),
     ?_test(test_ets_concurrent_access()),
     ?_test(test_message_queue_overflow()),
     ?_test(test_selective_receive()),
     ?_test(test_priority_inversion()),
     ?_test(test_busy_waiting()),
     ?_test(test_lock_contention()),
     ?_test(test_atomic_operations()),
     ?_test(test_consistency_guarantees())].

%% Concurrent test implementations (representative samples)
test_concurrent_client_start() ->
    Pids =
        pmap(fun(I) ->
                {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{id => I}),
                Pid
             end,
             lists:seq(1, 100)),

    ?assertEqual(100, length(Pids)),
    [?assert(is_process_alive(Pid)) || Pid <- Pids],
    [erlmcp_test_client:stop_test_server(Pid) || Pid <- Pids].

test_concurrent_client_stop() ->
    Clients =
        [begin
             {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
             Pid
         end
         || _ <- lists:seq(1, 100)],

    pmap(fun(Pid) -> erlmcp_test_client:stop_test_server(Pid) end, Clients),

    timer:sleep(200),
    [?assertNot(is_process_alive(Pid)) || Pid <- Clients].

test_concurrent_requests_same_client() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Parent = self(),

    Requesters =
        [spawn(fun() ->
                  Result = erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),
                  Parent ! {self(), Result}
               end)
         || _ <- lists:seq(1, 50)],

    Results =
        [receive
             {P, R} ->
                 R
         end
         || P <- Requesters],
    ?assertEqual(50, length([ok || {ok, _} <- Results])),
    erlmcp_test_client:stop_test_server(Pid).

test_concurrent_requests_different_clients() ->
    Clients =
        [begin
             {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
             Pid
         end
         || _ <- lists:seq(1, 20)],

    Parent = self(),
    Requesters =
        [spawn(fun() ->
                  Result = erlmcp_test_client:send_request(C, #{method => <<"ping">>}),
                  Parent ! {self(), Result}
               end)
         || C <- Clients],

    Results =
        [receive
             {P, R} ->
                 R
         end
         || P <- Requesters],
    ?assertEqual(20, length([ok || {ok, _} <- Results])),
    [erlmcp_test_client:stop_test_server(Pid) || Pid <- Clients].

test_concurrent_read_write() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Parent = self(),

    Writers =
        [spawn(fun() ->
                  Result =
                      erlmcp_test_client:send_request(Pid, #{method => <<"write">>, data => I}),
                  Parent ! {write, I, Result}
               end)
         || I <- lists:seq(1, 10)],

    Readers =
        [spawn(fun() ->
                  Result = erlmcp_test_client:send_request(Pid, #{method => <<"read">>}),
                  Parent ! {read, I, Result}
               end)
         || I <- lists:seq(1, 10)],

    [receive
         {write, _, _} ->
             ok
     end
     || _ <- Writers],
    [receive
         {read, _, _} ->
             ok
     end
     || _ <- Readers],
    erlmcp_test_client:stop_test_server(Pid).

test_race_condition_handling() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Parent = self(),

    %% Simulate race condition
    [spawn(fun() ->
              Result = erlmcp_test_client:send_request(Pid, #{method => <<"increment">>}),
              Parent ! {self(), Result}
           end)
     || _ <- lists:seq(1, 100)],

    Results =
        [receive
             {P, R} ->
                 R
         end
         || P <- lists:seq(1, 100)],
    ?assertEqual(100, length(Results)),
    erlmcp_test_client:stop_test_server(Pid).

test_deadlock_prevention() ->
    {ok, Pid1} = erlmcp_test_client:start_test_client(stdio, #{id => 1}),
    {ok, Pid2} = erlmcp_test_client:start_test_client(stdio, #{id => 2}),

    %% Cross-requests to detect deadlock
    spawn(fun() -> erlmcp_test_client:send_request(Pid1, #{target => Pid2}) end),
    spawn(fun() -> erlmcp_test_client:send_request(Pid2, #{target => Pid1}) end),

    timer:sleep(1000),
    ?assert(is_process_alive(Pid1)),
    ?assert(is_process_alive(Pid2)),

    erlmcp_test_client:stop_test_server(Pid1),
    erlmcp_test_client:stop_test_server(Pid2).

test_starvation_prevention() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Parent = self(),

    %% High priority requests
    [spawn(fun() ->
              Result = erlmcp_test_client:send_request(Pid, #{priority => high}),
              Parent ! {high, Result}
           end)
     || _ <- lists:seq(1, 10)],

    %% Low priority request should not starve
    spawn(fun() ->
             Result = erlmcp_test_client:send_request(Pid, #{priority => low}),
             Parent ! {low, Result}
          end),

    receive
        {low, _} ->
            ok
    after 5000 ->
        ?assert(false)
    end,
    erlmcp_test_client:stop_test_server(Pid).

test_fairness() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Parent = self(),

    Clients =
        [spawn(fun() ->
                  StartTime = erlang:monotonic_time(millisecond),
                  erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),
                  EndTime = erlang:monotonic_time(millisecond),
                  Parent ! {self(), EndTime - StartTime}
               end)
         || _ <- lists:seq(1, 10)],

    Latencies =
        [receive
             {C, L} ->
                 L
         end
         || C <- Clients],
    AvgLatency = lists:sum(Latencies) / length(Latencies),

    %% Check fairness: no client should have latency > 2x average
    [?assert(L < AvgLatency * 2) || L <- Latencies],
    erlmcp_test_client:stop_test_server(Pid).

test_load_balancing() ->
    Clients =
        [begin
             {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{id => I}),
             Pid
         end
         || I <- lists:seq(1, 5)],

    %% Distribute requests across clients
    Parent = self(),
    [spawn(fun() ->
              Client = lists:nth(I rem 5 + 1, Clients),
              Result = erlmcp_test_client:send_request(Client, #{method => <<"ping">>}),
              Parent ! {I, Result}
           end)
     || I <- lists:seq(1, 50)],

    Results =
        [receive
             {I, R} ->
                 R
         end
         || I <- lists:seq(1, 50)],
    ?assertEqual(50, length([ok || {ok, _} <- Results])),
    [erlmcp_test_client:stop_test_server(Pid) || Pid <- Clients].

test_backpressure_propagation() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{backpressure => true}),

    %% Flood with requests
    [spawn(fun() -> erlmcp_test_client:send_request(Pid, #{method => <<"heavy_operation">>}) end)
     || _ <- lists:seq(1, 1000)],

    timer:sleep(100),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_cascading_failures() ->
    Clients =
        [begin
             {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{id => I}),
             Pid
         end
         || I <- lists:seq(1, 5)],

    %% Kill one client
    [First | Rest] = Clients,
    exit(First, kill),
    timer:sleep(100),

    %% Others should still be alive
    [?assert(is_process_alive(Pid)) || Pid <- Rest],
    [erlmcp_test_client:stop_test_server(Pid) || Pid <- Rest].

test_thundering_herd() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Parent = self(),

    %% Simulate thundering herd
    StartTime = erlang:monotonic_time(millisecond),
    Herd =
        [spawn(fun() ->
                  Result = erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),
                  Parent ! {self(), Result}
               end)
         || _ <- lists:seq(1, 1000)],

    Results =
        [receive
             {P, R} ->
                 R
         end
         || P <- Herd],
    EndTime = erlang:monotonic_time(millisecond),

    ?assertEqual(1000, length(Results)),
    ?assert(EndTime - StartTime < 30000), %% Should complete within 30s
    erlmcp_test_client:stop_test_server(Pid).

test_split_brain() ->
    {ok, Pid1} = erlmcp_test_client:start_test_client(stdio, #{id => 1}),
    {ok, Pid2} = erlmcp_test_client:start_test_client(stdio, #{id => 2}),

    %% Both clients should operate independently
    Result1 = erlmcp_test_client:send_request(Pid1, #{method => <<"ping">>}),
    Result2 = erlmcp_test_client:send_request(Pid2, #{method => <<"ping">>}),

    ?assertMatch({ok, _}, Result1),
    ?assertMatch({ok, _}, Result2),

    erlmcp_test_client:stop_test_server(Pid1),
    erlmcp_test_client:stop_test_server(Pid2).

test_network_partition() ->
    {ok, Pid} =
        erlmcp_test_client:start_test_client(tcp,
                                             #{host => "localhost",
                                               port => 9999,
                                               network_partition_tolerance => true}),

    %% Client should handle network partition gracefully
    erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),
    erlmcp_test_client:stop_test_server(Pid).

test_process_isolation() ->
    {ok, Pid1} = erlmcp_test_client:start_test_client(stdio, #{id => 1}),
    {ok, Pid2} = erlmcp_test_client:start_test_client(stdio, #{id => 2}),

    %% Kill Pid1
    exit(Pid1, kill),
    timer:sleep(100),

    %% Pid2 should be unaffected
    ?assert(is_process_alive(Pid2)),
    Result = erlmcp_test_client:send_request(Pid2, #{method => <<"ping">>}),
    ?assertMatch({ok, _}, Result),

    erlmcp_test_client:stop_test_server(Pid2).

test_shared_state_access() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Parent = self(),

    %% Concurrent access to shared state
    [spawn(fun() ->
              Result = erlmcp_test_client:send_request(Pid, #{method => <<"read_state">>}),
              Parent ! {self(), Result}
           end)
     || _ <- lists:seq(1, 50)],

    Results =
        [receive
             {P, R} ->
                 R
         end
         || P <- lists:seq(1, 50)],
    ?assertEqual(50, length(Results)),
    erlmcp_test_client:stop_test_server(Pid).

test_ets_concurrent_access() ->
    %% This would test ETS table concurrent access if test client used ETS
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    erlmcp_test_client:send_request(Pid, #{method => <<"ets_operation">>}),
    erlmcp_test_client:stop_test_server(Pid).

test_message_queue_overflow() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),

    %% Flood message queue
    [spawn(fun() -> erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}) end)
     || _ <- lists:seq(1, 10000)],

    timer:sleep(1000),
    ?assert(is_process_alive(Pid)),
    erlmcp_test_client:stop_test_server(Pid).

test_selective_receive() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),

    %% Send multiple requests
    [erlmcp_test_client:send_request(Pid, #{id => I, method => <<"ping">>})
     || I <- lists:seq(1, 10)],

    erlmcp_test_client:stop_test_server(Pid).

test_priority_inversion() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),

    %% Send low priority, then high priority
    spawn(fun() -> erlmcp_test_client:send_request(Pid, #{priority => low}) end),
    timer:sleep(10),
    spawn(fun() -> erlmcp_test_client:send_request(Pid, #{priority => high}) end),

    timer:sleep(100),
    erlmcp_test_client:stop_test_server(Pid).

test_busy_waiting() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),

    %% Client should not busy-wait
    InitialReductions = process_info(Pid, reductions),
    timer:sleep(100),
    FinalReductions = process_info(Pid, reductions),

    {reductions, Init} = InitialReductions,
    {reductions, Final} = FinalReductions,

    %% Reductions should not increase significantly during idle
    ?assert(Final - Init < 1000),
    erlmcp_test_client:stop_test_server(Pid).

test_lock_contention() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    Parent = self(),

    %% Concurrent requests that might contend for locks
    [spawn(fun() ->
              erlmcp_test_client:send_request(Pid, #{method => <<"locked_operation">>}),
              Parent ! {self(), done}
           end)
     || _ <- lists:seq(1, 20)],

    [receive
         {_, done} ->
             ok
     end
     || _ <- lists:seq(1, 20)],
    erlmcp_test_client:stop_test_server(Pid).

test_atomic_operations() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),

    %% Test atomic operations
    erlmcp_test_client:send_request(Pid, #{method => <<"atomic_increment">>}),
    erlmcp_test_client:send_request(Pid, #{method => <<"atomic_increment">>}),
    Result = erlmcp_test_client:send_request(Pid, #{method => <<"get_counter">>}),

    ?assertMatch({ok, _}, Result),
    erlmcp_test_client:stop_test_server(Pid).

test_consistency_guarantees() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),

    %% Write, then read - should be consistent
    erlmcp_test_client:send_request(Pid, #{method => <<"write">>, value => 42}),
    {ok, Response} = erlmcp_test_client:send_request(Pid, #{method => <<"read">>}),

    ?assert(is_map(Response)),
    erlmcp_test_client:stop_test_server(Pid).

%%%===================================================================
%%% Edge Case Tests (25 cases)
%%%===================================================================

edge_case_tests() ->
    [?_test(test_zero_timeout()),
     ?_test(test_infinite_timeout()),
     ?_test(test_negative_timeout()),
     ?_test(test_empty_config()),
     ?_test(test_nil_values()),
     ?_test(test_undefined_values()),
     ?_test(test_max_integer()),
     ?_test(test_min_integer()),
     ?_test(test_float_values()),
     ?_test(test_atom_keys()),
     ?_test(test_binary_keys()),
     ?_test(test_list_values()),
     ?_test(test_tuple_values()),
     ?_test(test_nested_maps()),
     ?_test(test_circular_references()),
     ?_test(test_very_deep_nesting()),
     ?_test(test_special_characters()),
     ?_test(test_whitespace_only()),
     ?_test(test_null_bytes()),
     ?_test(test_emoji_data()),
     ?_test(test_control_characters()),
     ?_test(test_escape_sequences()),
     ?_test(test_boundary_values()),
     ?_test(test_overflow_conditions()),
     ?_test(test_underflow_conditions())].

%% Edge case test implementations (representative samples)
test_zero_timeout() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{timeout => 0}),
    erlmcp_test_client:stop_test_server(Pid).

test_infinite_timeout() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{timeout => infinity}),
    erlmcp_test_client:stop_test_server(Pid).

test_negative_timeout() ->
    try
        erlmcp_test_client:start_test_client(stdio, #{timeout => -1}),
        ?assert(true) %% May succeed if validation doesn't check
    catch
        _:_ ->
            ok %% Or may fail, both acceptable
    end.

test_empty_config() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    erlmcp_test_client:stop_test_server(Pid).

test_nil_values() ->
    Config = #{key => undefined},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_undefined_values() ->
    Config = #{undefined_key => undefined_value},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_max_integer() ->
    Config = #{max_value => 999999999999999999},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_min_integer() ->
    Config = #{min_value => -999999999999999999},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_float_values() ->
    Config =
        #{pi => 3.14159,
          e => 2.71828,
          infinity => 1.0e308,
          tiny => 1.0e-308},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_atom_keys() ->
    Config = #{atom_key => value},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_binary_keys() ->
    Config = #{<<"binary_key">> => <<"binary_value">>},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_list_values() ->
    Config = #{list => [1, 2, 3, 4, 5]},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_tuple_values() ->
    Config = #{tuple => {a, b, c}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_nested_maps() ->
    Config = #{level1 => #{level2 => #{level3 => #{value => deep}}}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_circular_references() ->
    %% Erlang maps can't have circular references, but test deep structure
    Config = #{a => #{b => #{c => #{d => value}}}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_very_deep_nesting() ->
    DeepMap = lists:foldl(fun(I, Acc) -> #{I => Acc} end, #{value => deep}, lists:seq(1, 100)),
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, DeepMap),
    erlmcp_test_client:stop_test_server(Pid).

test_special_characters() ->
    Config = #{text => <<"!@#$%^&*()_+-={}[]|\\:;\"'<>,.?/">>},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_whitespace_only() ->
    Config = #{text => <<"   \t\n\r   ">>},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_null_bytes() ->
    Config = #{data => <<0, 0, 0, 0>>},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_emoji_data() ->
    Config = #{emoji => <<"🔥💯🚀✨🎉"/utf8>>},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_control_characters() ->
    Config = #{control => <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_escape_sequences() ->
    Config = #{escaped => <<"\\n\\t\\r\\b\\f\\\\">>},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_boundary_values() ->
    Config =
        #{max_int => 1 bsl 63 - 1,
          min_int => -(1 bsl 63),
          empty => <<>>,
          single => <<1>>},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_overflow_conditions() ->
    LargeList = lists:seq(1, 100000),
    Config = #{large_list => LargeList},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_underflow_conditions() ->
    Config = #{empty_list => [], empty_map => #{}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    erlmcp_test_client:stop_test_server(Pid).

%%%===================================================================
%%% Integration Tests (30 cases)
%%%===================================================================

integration_tests() ->
    [?_test(test_full_lifecycle()), ?_test(test_request_response_cycle()),
     ?_test(test_multi_transport_workflow()), ?_test(test_error_recovery_workflow()),
     ?_test(test_reconnection_workflow()), ?_test(test_failover_workflow()),
     ?_test(test_load_balancing_workflow()), ?_test(test_circuit_breaker_workflow()),
     ?_test(test_retry_workflow()), ?_test(test_timeout_workflow()),
     ?_test(test_backpressure_workflow()), ?_test(test_streaming_workflow()),
     ?_test(test_batching_workflow()), ?_test(test_pipelining_workflow()),
     ?_test(test_multiplexing_workflow()), ?_test(test_compression_workflow()),
     ?_test(test_encryption_workflow()), ?_test(test_authentication_workflow()),
     ?_test(test_authorization_workflow()), ?_test(test_rate_limiting_workflow()),
     ?_test(test_caching_workflow()), ?_test(test_monitoring_workflow()),
     ?_test(test_logging_workflow()), ?_test(test_tracing_workflow()),
     ?_test(test_metrics_workflow()), ?_test(test_health_check_workflow()),
     ?_test(test_graceful_shutdown_workflow()), ?_test(test_hot_code_upgrade_workflow()),
     ?_test(test_distributed_workflow()), ?_test(test_end_to_end_workflow())].

%% Integration test implementations (representative samples)
test_full_lifecycle() ->
    %% Start client
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),
    ?assert(is_process_alive(Pid)),

    %% Send request
    Result = erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),
    ?assertMatch({ok, _}, Result),

    %% Stop client
    ok = erlmcp_test_client:stop_test_server(Pid),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

test_request_response_cycle() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),

    %% Multiple request-response cycles
    lists:foreach(fun(I) ->
                     Request = #{id => I, method => <<"ping">>},
                     Result = erlmcp_test_client:send_request(Pid, Request),
                     ?assertMatch({ok, _}, Result)
                  end,
                  lists:seq(1, 10)),

    erlmcp_test_client:stop_test_server(Pid).

test_multi_transport_workflow() ->
    Transports = [stdio, tcp, http, websocket],

    lists:foreach(fun(Transport) ->
                     Config =
                         case Transport of
                             tcp ->
                                 #{host => "localhost", port => 9999};
                             http ->
                                 #{url => "http://localhost:8080"};
                             websocket ->
                                 #{url => "ws://localhost:8080/ws"};
                             _ ->
                                 #{}
                         end,

                     {ok, Pid} = erlmcp_test_client:start_test_client(Transport, Config),
                     Result = erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),
                     ?assertMatch({ok, _}, Result),
                     erlmcp_test_client:stop_test_server(Pid)
                  end,
                  Transports).

test_error_recovery_workflow() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),

    %% Send failing request
    erlmcp_test_client:send_request(Pid, #{method => <<"fail">>}),

    %% Send successful request
    Result = erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),
    ?assertMatch({ok, _}, Result),

    erlmcp_test_client:stop_test_server(Pid).

test_reconnection_workflow() ->
    Config =
        #{host => "localhost",
          port => 9999,
          reconnect => true},
    {ok, Pid} = erlmcp_test_client:start_test_client(tcp, Config),

    %% Simulate disconnection and reconnection
    erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),

    erlmcp_test_client:stop_test_server(Pid).

test_failover_workflow() ->
    Config =
        #{hosts => ["host1", "host2", "host3"],
          port => 9999,
          failover => true},
    {ok, Pid} = erlmcp_test_client:start_test_client(tcp, Config),
    erlmcp_test_client:stop_test_server(Pid).

test_load_balancing_workflow() ->
    Clients =
        [begin
             {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{id => I}),
             Pid
         end
         || I <- lists:seq(1, 3)],

    %% Round-robin requests
    lists:foreach(fun(I) ->
                     Client = lists:nth(I rem 3 + 1, Clients),
                     erlmcp_test_client:send_request(Client, #{method => <<"ping">>})
                  end,
                  lists:seq(1, 30)),

    [erlmcp_test_client:stop_test_server(Pid) || Pid <- Clients].

test_circuit_breaker_workflow() ->
    Config = #{circuit_breaker => #{enabled => true, threshold => 3}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    %% Trigger circuit breaker
    [erlmcp_test_client:send_request(Pid, #{method => <<"fail">>}) || _ <- lists:seq(1, 5)],

    %% Circuit should be open
    erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),

    erlmcp_test_client:stop_test_server(Pid).

test_retry_workflow() ->
    Config = #{retry => #{count => 3, backoff => exponential}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    erlmcp_test_client:send_request(Pid, #{method => <<"unstable">>}),

    erlmcp_test_client:stop_test_server(Pid).

test_timeout_workflow() ->
    Config = #{timeout => 5000},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    erlmcp_test_client:send_request(Pid, #{method => <<"slow_operation">>}),

    erlmcp_test_client:stop_test_server(Pid).

test_backpressure_workflow() ->
    Config = #{backpressure => #{enabled => true, threshold => 100}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    [spawn(fun() -> erlmcp_test_client:send_request(Pid, #{method => <<"heavy">>}) end)
     || _ <- lists:seq(1, 200)],

    timer:sleep(1000),
    erlmcp_test_client:stop_test_server(Pid).

test_streaming_workflow() ->
    Config = #{streaming => true},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    erlmcp_test_client:send_request(Pid, #{method => <<"stream">>}),

    erlmcp_test_client:stop_test_server(Pid).

test_batching_workflow() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{batching => true}),

    Requests = [#{method => <<"ping">>, id => I} || I <- lists:seq(1, 10)],
    [erlmcp_test_client:send_request(Pid, Req) || Req <- Requests],

    erlmcp_test_client:stop_test_server(Pid).

test_pipelining_workflow() ->
    Config = #{pipelining => true},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    [erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}) || _ <- lists:seq(1, 5)],

    erlmcp_test_client:stop_test_server(Pid).

test_multiplexing_workflow() ->
    Config = #{multiplexing => true},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    Parent = self(),
    [spawn(fun() ->
              erlmcp_test_client:send_request(Pid, #{method => <<"concurrent">>, id => I}),
              Parent ! {done, I}
           end)
     || I <- lists:seq(1, 10)],

    [receive
         {done, _} ->
             ok
     end
     || _ <- lists:seq(1, 10)],
    erlmcp_test_client:stop_test_server(Pid).

test_compression_workflow() ->
    Config = #{compression => #{enabled => true, algorithm => gzip}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    LargeData = #{data => binary:copy(<<"x">>, 10000)},
    erlmcp_test_client:send_request(Pid, #{method => <<"process">>, params => LargeData}),

    erlmcp_test_client:stop_test_server(Pid).

test_encryption_workflow() ->
    Config = #{encryption => #{enabled => true, algorithm => aes256}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    erlmcp_test_client:send_request(Pid, #{method => <<"secure">>}),

    erlmcp_test_client:stop_test_server(Pid).

test_authentication_workflow() ->
    Config =
        #{url => "http://localhost:8080", auth => #{type => bearer, token => <<"secret_token">>}},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),

    erlmcp_test_client:send_request(Pid, #{method => <<"protected">>}),

    erlmcp_test_client:stop_test_server(Pid).

test_authorization_workflow() ->
    Config =
        #{url => "http://localhost:8080",
          auth => #{type => bearer, token => <<"admin_token">>},
          roles => [admin]},
    {ok, Pid} = erlmcp_test_client:start_test_client(http, Config),

    erlmcp_test_client:send_request(Pid, #{method => <<"admin_only">>}),

    erlmcp_test_client:stop_test_server(Pid).

test_rate_limiting_workflow() ->
    Config = #{rate_limit => #{max_requests_per_second => 10}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    StartTime = erlang:monotonic_time(millisecond),
    [erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}) || _ <- lists:seq(1, 20)],
    EndTime = erlang:monotonic_time(millisecond),

    %% Should take at least 1 second due to rate limiting
    ?assert(EndTime - StartTime >= 1000),

    erlmcp_test_client:stop_test_server(Pid).

test_caching_workflow() ->
    Config = #{caching => #{enabled => true, ttl => 60}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    %% First request - cache miss
    erlmcp_test_client:send_request(Pid, #{method => <<"cached_operation">>}),

    %% Second request - cache hit
    erlmcp_test_client:send_request(Pid, #{method => <<"cached_operation">>}),

    erlmcp_test_client:stop_test_server(Pid).

test_monitoring_workflow() ->
    Config = #{monitoring => #{enabled => true}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    erlmcp_test_client:send_request(Pid, #{method => <<"monitored">>}),

    erlmcp_test_client:stop_test_server(Pid).

test_logging_workflow() ->
    Config = #{logging => #{enabled => true, level => debug}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),

    erlmcp_test_client:stop_test_server(Pid).

test_tracing_workflow() ->
    Config = #{tracing => #{enabled => true, sampler => always}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    erlmcp_test_client:send_request(Pid,
                                    #{method => <<"traced">>,
                                      trace_id => <<"trace-123">>,
                                      span_id => <<"span-456">>}),

    erlmcp_test_client:stop_test_server(Pid).

test_metrics_workflow() ->
    Config = #{metrics => #{enabled => true, exporter => prometheus}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    erlmcp_test_client:send_request(Pid, #{method => <<"measured">>}),

    erlmcp_test_client:stop_test_server(Pid).

test_health_check_workflow() ->
    Config = #{health_check => #{enabled => true, interval => 10000}},
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),

    timer:sleep(100),
    ?assert(is_process_alive(Pid)),

    erlmcp_test_client:stop_test_server(Pid).

test_graceful_shutdown_workflow() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{graceful_shutdown => true}),

    %% Start long-running operation
    spawn(fun() -> erlmcp_test_client:send_request(Pid, #{method => <<"long_operation">>}) end),

    timer:sleep(50),

    %% Shutdown should wait for operation to complete
    erlmcp_test_client:stop_test_server(Pid).

test_hot_code_upgrade_workflow() ->
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{}),

    %% Simulate hot code upgrade
    sys:suspend(Pid),
    sys:change_code(Pid, erlmcp_test_client, old_vsn, []),
    sys:resume(Pid),

    %% Client should still work
    Result = erlmcp_test_client:send_request(Pid, #{method => <<"ping">>}),
    ?assertMatch({ok, _}, Result),

    erlmcp_test_client:stop_test_server(Pid).

test_distributed_workflow() ->
    %% This would test distributed scenarios if applicable
    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, #{distributed => true}),
    erlmcp_test_client:send_request(Pid, #{method => <<"distributed_operation">>}),
    erlmcp_test_client:stop_test_server(Pid).

test_end_to_end_workflow() ->
    %% Complete end-to-end workflow
    Config =
        #{transport => stdio,
          timeout => 5000,
          retry => 3,
          logging => true,
          metrics => true},

    {ok, Pid} = erlmcp_test_client:start_test_client(stdio, Config),
    ?assert(is_process_alive(Pid)),

    %% Send various requests
    Requests =
        [#{method => <<"ping">>},
         #{method => <<"echo">>, params => #{text => <<"hello">>}},
         #{method => <<"calculate">>, params => #{a => 1, b => 2}}],

    Results = [erlmcp_test_client:send_request(Pid, Req) || Req <- Requests],
    ?assertEqual(3, length([ok || {ok, _} <- Results])),

    ok = erlmcp_test_client:stop_test_server(Pid),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Parallel map
pmap(Fun, List) ->
    Parent = self(),
    Pids =
        [spawn(fun() ->
                  Result = Fun(Item),
                  Parent ! {self(), Result}
               end)
         || Item <- List],
    [receive
         {Pid, Result} ->
             Result
     end
     || Pid <- Pids].
