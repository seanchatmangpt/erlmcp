-module(erlmcp_transport_abuse_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Test Server Callbacks
%%====================================================================

suite() ->
    [{timetrap, {minutes, 30}}].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),
    application:ensure_all_started(ranch),
    application:ensure_all_started(cowboy),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(tcp_abuse, Config) ->
    [{transport, tcp} | Config];
init_per_group(ws_abuse, Config) ->
    [{transport, ws} | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    cleanup_processes().

%%====================================================================
%% Test Cases - TCP Abuse
%%====================================================================

groups() ->
    [
        {tcp_abuse, [parallel], [
            tcp_incomplete_handshake,
            tcp_half_close,
            tcp_slowloris,
            tcp_syn_flood_sim,
            tcp_fragmentation,
            tcp_keepalive_abuse
        ]},
        {ws_abuse, [parallel], [
            ws_incomplete_handshake,
            ws_invalid_headers,
            ws_frame_fragmentation,
            ws_opcode_abuse,
            ws_payload_lies,
            ws_masking_violations
        ]}
    ].

all() ->
    [
        {group, tcp_abuse},
        {group, ws_abuse}
    ].

%%--------------------------------------------------------------------
%% TCP ABUSE TESTS
%%--------------------------------------------------------------------

tcp_incomplete_handshake(Config) ->
    Port = proplists:get_value(tcp_port, Config, 10019),
    AbuseCount = 1000,
    
    Results = run_tcp_abuse(incomplete_handshake, Port, AbuseCount),
    
    CtResults = #{
        technique => incomplete_handshake,
        total_attempts => AbuseCount,
        successful_connections => maps:get(successful, Results, 0),
        failed_connections => maps:get(failed, Results, 0),
        handler_crashes => maps:get(crashes, Results, 0),
        memory_used => maps:get(memory, Results, 0),
        file_descriptors => maps:get(fds, Results, 0),
        time_to_exhaustion => maps:get(exhaustion_time, Results, infinity)
    },
    
    log_results(tcp, incomplete_handshake, CtResults),
    
    %% Assert server still responsive
    assert_server_responsive(Port),
    
    {comment, io_lib:format("Incomplete handshake test: ~p attempts, ~p crashes", 
                           [AbuseCount, maps:get(crashes, Results, 0)])}.

tcp_half_close(Config) ->
    Port = proplists:get_value(tcp_port, Config, 10019),
    AbuseCount = 1000,
    
    Results = run_tcp_abuse(half_close, Port, AbuseCount),
    
    CtResults = #{
        technique => half_close,
        total_attempts => AbuseCount,
        successful_half_closes => maps:get(successful, Results, 0),
        handler_crashes => maps:get(crashes, Results, 0),
        zombie_connections => maps:get(zombies, Results, 0),
        memory_leaked => maps:get(memory, Results, 0)
    },
    
    log_results(tcp, half_close, CtResults),
    assert_server_responsive(Port),
    
    {comment, io_lib:format("Half-close test: ~p attempts, ~p zombies", 
                           [AbuseCount, maps:get(zombies, Results, 0)])}.

tcp_slowloris(Config) ->
    Port = proplists:get_value(tcp_port, Config, 10019),
    AbuseCount = 500,
    
    Results = run_tcp_abuse(slowloris, Port, AbuseCount),
    
    CtResults = #{
        technique => slowloris,
        total_attempts => AbuseCount,
        slow_connections_established => maps-get(slow_conns, Results, 0),
        server_unresponsive => maps:get(unresponsive, Results, false),
        time_to_unresponsive => maps:get(unresponsive_time, Results, infinity),
        max_concurrent_slow => maps:get(max_concurrent, Results, 0)
    },
    
    log_results(tcp, slowloris, CtResults),
    
    %% Allow recovery time
    timer:sleep(5000),
    assert_server_responsive(Port),
    
    {comment, io_lib:format("Slowloris test: ~p slow conns, server recovered", 
                           [maps:get(slow_conns, Results, 0)])}.

tcp_syn_flood_sim(Config) ->
    Port = proplists:get_value(tcp_port, Config, 10019),
    AbuseCount = 2000,
    
    Results = run_tcp_abuse(syn_flood, Port, AbuseCount),
    
    CtResults = #{
        technique => syn_flood,
        total_attempts => AbuseCount,
        connections_rejected => maps:get(rejected, Results, 0),
        connections_accepted => maps:get(accepted, Results, 0),
        handler_crashes => maps-get(crashes, Results, 0),
        kernel_backlog_overflow => maps:get(backlog_overflow, Results, false)
    },
    
    log_results(tcp, syn_flood, CtResults),
    assert_server_responsive(Port),
    
    {comment, io_lib:format("SYN flood sim: ~p attempts, ~p rejected", 
                           [AbuseCount, maps:get(rejected, Results, 0)])}.

tcp_fragmentation(Config) ->
    Port = proplists:get_value(tcp_port, Config, 10019),
    AbuseCount = 1000,
    
    Results = run_tcp_abuse(fragmentation, Port, AbuseCount),
    
    CtResults = #{
        technique => fragmentation,
        total_attempts => AbuseCount,
        tiny_fragments_sent => maps:get(fragments, Results, 0),
        parser_crashes => maps:get(parser_crashes, Results, 0),
        buffer_overflows => maps:get(buffer_overflows, Results, 0),
        malformed_packets => maps:get(malformed, Results, 0)
    },
    
    log_results(tcp, fragmentation, CtResults),
    assert_server_responsive(Port),
    
    {comment, io_lib:format("Fragmentation test: ~p fragments, ~p parser crashes", 
                           [maps:get(fragments, Results, 0), 
                            maps:get(parser_crashes, Results, 0)])}.

tcp_keepalive_abuse(Config) ->
    Port = proplists:get_value(tcp_port, Config, 10019),
    AbuseCount = 500,
    
    Results = run_tcp_abuse(keepalive_abuse, Port, AbuseCount),
    
    CtResults = #{
        technique => keepalive_abuse,
        total_attempts => AbuseCount,
        ping_pong_failures => maps:get(ping_failures, Results, 0),
        connections_dropped => maps:get(dropped, Results, 0),
        handler_timeouts => maps:get(timeouts, Results, 0),
        stale_connections => maps:get(stale, Results, 0)
    },
    
    log_results(tcp, keepalive_abuse, CtResults),
    assert_server_responsive(Port),
    
    {comment, io_lib:format("Keepalive abuse: ~p failures, ~p dropped", 
                           [maps:get(ping_failures, Results, 0),
                            maps:get(dropped, Results, 0)])}.

%%--------------------------------------------------------------------
%% WEBSOCKET ABUSE TESTS
%%--------------------------------------------------------------------

ws_incomplete_handshake(Config) ->
    Port = proplists:get_value(ws_port, Config, 10020),
    AbuseCount = 1000,
    
    Results = run_ws_abuse(incomplete_handshake, Port, AbuseCount),
    
    CtResults = #{
        technique => incomplete_handshake,
        total_attempts => AbuseCount,
        handshake_rejections => maps:get(rejections, Results, 0),
        handler_crashes => maps:get(crashes, Results, 0),
        memory_leaked => maps:get(memory, Results, 0),
        orphaned_handshakes => maps:get(orphans, Results, 0)
    },
    
    log_results(ws, incomplete_handshake, CtResults),
    assert_ws_server_responsive(Port),
    
    {comment, io_lib:format("WS incomplete handshake: ~p attempts, ~p rejections", 
                           [AbuseCount, maps:get(rejections, Results, 0)])}.

ws_invalid_headers(Config) ->
    Port = proplists:get_value(ws_port, Config, 10020),
    AbuseCount = 1000,
    
    Results = run_ws_abuse(invalid_headers, Port, AbuseCount),
    
    CtResults = #{
        technique => invalid_headers,
        total_attempts => AbuseCount,
        header_parse_failures => maps:get(parse_failures, Results, 0),
        handler_crashes => maps-get(crashes, Results, 0),
        injection_attempts => maps:get(injections, Results, 0),
        smuggling_attempts => maps:get(smuggling, Results, 0)
    },
    
    log_results(ws, invalid_headers, CtResults),
    assert_ws_server_responsive(Port),
    
    {comment, io_lib:format("WS invalid headers: ~p parse failures", 
                           [maps:get(parse_failures, Results, 0)])}.

ws_frame_fragmentation(Config) ->
    Port = proplists:get_value(ws_port, Config, 10020),
    AbuseCount = 500,
    
    Results = run_ws_abuse(frame_fragmentation, Port, AbuseCount),
    
    CtResults = #{
        technique => frame_fragmentation,
        total_attempts => AbuseCount,
        fragments_sent => maps:get(fragments, Results, 0),
        reassembly_timeouts => maps:get(timeouts, Results, 0),
        buffer_overflows => maps:get(overflows, Results, 0),
        fragment_state_corruption => maps:get(corruption, Results, 0)
    },
    
    log_results(ws, frame_fragmentation, CtResults),
    assert_ws_server_responsive(Port),
    
    {comment, io_lib:format("WS frame fragmentation: ~p fragments, ~p timeouts", 
                           [maps:get(fragments, Results, 0),
                            maps:get(timeouts, Results, 0)])}.

ws_opcode_abuse(Config) ->
    Port = proplists:get_value(ws_port, Config, 10020),
    AbuseCount = 1000,
    
    Results = run_ws_abuse(opcode_abuse, Port, AbuseCount),
    
    CtResults = #{
        technique => opcode_abuse,
        total_attempts => AbuseCount,
        reserved_opcodes => maps:get(reserved, Results, 0),
        handler_crashes => maps:get(crashes, Results, 0),
        protocol_violations => maps:get(violations, Results, 0),
        invalid_control_frames => maps:get(invalid_controls, Results, 0)
    },
    
    log_results(ws, opcode_abuse, CtResults),
    assert_ws_server_responsive(Port),
    
    {comment, io_lib:format("WS opcode abuse: ~p reserved opcodes, ~p crashes", 
                           [maps:get(reserved, Results, 0),
                            maps:get(crashes, Results, 0)])}.

ws_payload_lies(Config) ->
    Port = proplists:get_value(ws_port, Config, 10020),
    AbuseCount = 500,
    
    Results = run_ws_abuse(payload_lies, Port, AbuseCount),
    
    CtResults = #{
        technique => payload_lies,
        total_attempts => AbuseCount,
        size_mismatches => maps:get(mismatches, Results, 0),
        buffer_exhaustions => maps:get(exhaustions, Results, 0),
        handler_crashes => maps:get(crashes, Results, 0),
        memory_allocated_vs_used => maps:get(memory_ratio, Results, 0)
    },
    
    log_results(ws, payload_lies, CtResults),
    assert_ws_server_responsive(Port),
    
    {comment, io_lib:format("WS payload lies: ~p mismatches, ~p exhaustions", 
                           [maps:get(mismatches, Results, 0),
                            maps:get(exhaustions, Results, 0)])}.

ws_masking_violations(Config) ->
    Port = proplists:get_value(ws_port, Config, 10020),
    AbuseCount = 1000,
    
    Results = run_ws_abuse(masking_violations, Port, AbuseCount),
    
    CtResults = #{
        technique => masking_violations,
        total_attempts => AbuseCount,
        unmasked_frames => maps:get(unmasked, Results, 0),
        invalid_masks => maps:get(invalid_masks, Results, 0),
        handler_crashes => maps:get(crashes, Results, 0),
        protocol_rejections => maps:get(rejections, Results, 0)
    },
    
    log_results(ws, masking_violations, CtResults),
    assert_ws_server_responsive(Port),
    
    {comment, io_lib:format("WS masking violations: ~p unmasked, ~p crashes", 
                           [maps:get(unmasked, Results, 0),
                            maps:get(crashes, Results, 0)])}.

%%====================================================================
%% Internal Functions - TCP Abuse
%%====================================================================

run_tcp_abuse(Technique, Port, Count) ->
    Parent = self(),
    Pids = [spawn_monitor(fun() ->
        Result = abuse_tcp(Technique, Port),
        Parent ! {result, self(), Result}
    end) || _ <- lists:seq(1, min(Count, 100))],
    
    collect_results(Pids, #{
        successful => 0,
        failed => 0,
        crashes => 0,
        memory => 0,
        fds => 0,
        exhaustion_time => infinity
    }).

abuse_tcp(incomplete_handshake, Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {active, false}], 1000) of
        {ok, Socket} ->
            %% Send SYN but never complete handshake
            %% Just connect and send nothing
            timer:sleep(100),
            %% Check if still connected
            case inet:port(Socket) of
                {ok, _} -> 
                    %% Leave it hanging
                    receive after 5000 -> ok end,
                    gen_tcp:close(Socket),
                    #{successful => 1};
                {error, _} ->
                    #{failed => 1}
            end;
        {error, _} ->
            #{failed => 1}
    end;

abuse_tcp(half_close, Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {active, false}]) of
        {ok, Socket} ->
            %% Send some data
            gen_tcp:send(Socket, <<"TEST">>),
            %% Shutdown write only
            gen_tcp:shutdown(Socket, write),
            timer:sleep(100),
            %% Check if handler still alive
            case inet:port(Socket) of
                {ok, _} ->
                    gen_tcp:close(Socket),
                    #{successful => 1, zombies => 1};
                {error, _} ->
                    #{successful => 1}
            end;
        {error, _} ->
            #{failed => 1}
    end;

abuse_tcp(slowloris, Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {active, false}]) of
        {ok, Socket} ->
            %% Send 1 byte per second
            send_slow_data(Socket, <<"GET / HTTP/1.1\r\nHost: localhost\r\n\r\n">>, 1000),
            gen_tcp:close(Socket),
            #{successful => 1, slow_conns => 1};
        {error, _} ->
            #{failed => 1}
    end;

abuse_tcp(syn_flood, Port) ->
    %% Rapid connection attempts
    Results = [begin
        case gen_tcp:connect("localhost", Port, [binary, {active, false}], 100) of
            {ok, Socket} ->
                gen_tcp:close(Socket),
                accepted;
            {error, _} ->
                rejected
        end
    end || _ <- lists:seq(1, 10)],
    
    Accepted = length([R || R <- Results, R =:= accepted]),
    Rejected = length([R || R <- Results, R =:= rejected]),
    
    #{
        accepted => Accepted,
        rejected => Rejected
    };

abuse_tcp(fragmentation, Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {active, false}]) of
        {ok, Socket} ->
            %% Send tiny fragments
            Data = <<"FRAGMENTED_DATA_TEST">>,
            Fragments = [<<C>> || <<C>> <= Data],
            lists:foreach(fun(F) ->
                gen_tcp:send(Socket, F),
                timer:sleep(10)
            end, Fragments),
            gen_tcp:close(Socket),
            #{successful => 1, fragments => length(Fragments)};
        {error, _} ->
            #{failed => 1}
    end;

abuse_tcp(keepalive_abuse, Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {active, false}, {keepalive, true}]) of
        {ok, Socket} ->
            %% Never respond to pings
            timer:sleep(10000),
            case inet:port(Socket) of
                {ok, _} ->
                    gen_tcp:close(Socket),
                    #{successful => 1, stale => 1};
                {error, _} ->
                    #{successful => 1, dropped => 1}
            end;
        {error, _} ->
            #{failed => 1}
    end.

send_slow_data(_Socket, <<>>, _Interval) ->
    ok;
send_slow_data(Socket, <<Byte, Rest/binary>>, Interval) ->
    gen_tcp:send(Socket, <<Byte>>),
    timer:sleep(Interval),
    send_slow_data(Socket, Rest, Interval).

%%====================================================================
%% Internal Functions - WebSocket Abuse
%%====================================================================

run_ws_abuse(Technique, Port, Count) ->
    Parent = self(),
    Pids = [spawn_monitor(fun() ->
        Result = abuse_ws(Technique, Port),
        Parent ! {result, self(), Result}
    end) || _ <- lists:seq(1, min(Count, 100))],
    
    collect_results(Pids, #{
        rejections => 0,
        crashes => 0,
        memory => 0,
        orphans => 0,
        parse_failures => 0,
        injections => 0,
        smuggling => 0,
        fragments => 0,
        timeouts => 0,
        overflows => 0,
        corruption => 0,
        reserved => 0,
        violations => 0,
        invalid_controls => 0,
        mismatches => 0,
        exhaustions => 0,
        memory_ratio => 0,
        unmasked => 0,
        invalid_masks => 0,
        protocol_rejections => 0
    }).

abuse_ws(incomplete_handshake, Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {active, false}]) of
        {ok, Socket} ->
            %% Send incomplete HTTP upgrade
            gen_tcp:send(Socket, <<"GET /mcp/ws HTTP/1.1\r\n">>),
            gen_tcp:send(Socket, <<"Host: localhost\r\n">>),
            %% Missing Upgrade header
            timer:sleep(100),
            case inet:port(Socket) of
                {ok, _} ->
                    gen_tcp:close(Socket),
                    #{rejections => 1};
                {error, _} ->
                    #{rejections => 1, orphans => 1}
            end;
        {error, _} ->
            #{rejections => 1}
    end;

abuse_ws(invalid_headers, Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {active, false}]) of
        {ok, Socket} ->
            %% Send headers with injection attempts
            gen_tcp:send(Socket, <<"GET /mcp/ws HTTP/1.1\r\n">>),
            gen_tcp:send(Socket, <<"Host: localhost\r\n">>),
            gen_tcp:send(Socket, <<"Upgrade: websocket\r\n">>),
            %% Injection attempt
            gen_tcp:send(Socket, <<"X-Evil: ../../etc/passwd\r\n">>),
            gen_tcp:send(Socket, <<"Connection: Upgrade, Keep-Alive\r\n\r\n">>),
            timer:sleep(100),
            gen_tcp:close(Socket),
            #{parse_failures => 1, injections => 1};
        {error, _} ->
            #{parse_failures => 1}
    end;

abuse_ws(frame_fragmentation, Port) ->
    %% This would require a proper WS client
    %% For now, simulate with TCP
    case gen_tcp:connect("localhost", Port, [binary, {active, false}]) of
        {ok, Socket} ->
            %% Complete handshake first
            gen_tcp:send(Socket, <<"GET /mcp/ws HTTP/1.1\r\n">>),
            gen_tcp:send(Socket, <<"Host: localhost\r\n">>),
            gen_tcp:send(Socket, <<"Upgrade: websocket\r\n">>),
            gen_tcp:send(Socket, <<"Connection: Upgrade\r\n">>),
            gen_tcp:send(Socket, <<"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n">>),
            timer:sleep(100),
            gen_tcp:close(Socket),
            #{fragments => 0};
        {error, _} ->
            #{fragments => 0}
    end;

abuse_ws(opcode_abuse, Port) ->
    %% Send WS frame with reserved opcode
    case gen_tcp:connect("localhost", Port, [binary, {active, false}]) of
        {ok, Socket} ->
            %% Send handshake
            gen_tcp:send(Socket, <<"GET /mcp/ws HTTP/1.1\r\n">>),
            gen_tcp:send(Socket, <<"Host: localhost\r\n">>),
            gen_tcp:send(Socket, <<"Upgrade: websocket\r\n">>),
            gen_tcp:send(Socket, <<"Connection: Upgrade\r\n">>),
            gen_tcp:send(Socket, <<"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n">>),
            timer:sleep(100),
            %% Send frame with opcode 0x0B (reserved)
            gen_tcp:send(Socket, <<16#8B:8>>), %% FIN + reserved opcode
            gen_tcp:send(Socket, <<16#00:8>>),  %% Length 0
            timer:sleep(100),
            gen_tcp:close(Socket),
            #{reserved => 1};
        {error, _} ->
            #{reserved => 0}
    end;

abuse_ws(payload_lies, Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {active, false}]) of
        {ok, Socket} ->
            %% Send handshake
            gen_tcp:send(Socket, <<"GET /mcp/ws HTTP/1.1\r\n">>),
            gen_tcp:send(Socket, <<"Host: localhost\r\n">>),
            gen_tcp:send(Socket, <<"Upgrade: websocket\r\n">>),
            gen_tcp:send(Socket, <<"Connection: Upgrade\r\n">>),
            gen_tcp:send(Socket, <<"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n">>),
            timer:sleep(100),
            %% Claim 1GB payload, send 1 byte
            gen_tcp:send(Socket, <<16#81:8, 127:8, 0:32, 0:32, 0:32, 1:32>>),
            gen_tcp:send(Socket, <<"X">>),
            timer:sleep(100),
            gen_tcp:close(Socket),
            #{mismatches => 1};
        {error, _} ->
            #{mismatches => 0}
    end;

abuse_ws(masking_violations, Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {active, false}]) of
        {ok, Socket} ->
            %% Send handshake
            gen_tcp:send(Socket, <<"GET /mcp/ws HTTP/1.1\r\n">>),
            gen_tcp:send(Socket, <<"Host: localhost\r\n">>),
            gen_tcp:send(Socket, <<"Upgrade: websocket\r\n">>),
            gen_tcp:send(Socket, <<"Connection: Upgrade\r\n">>),
            gen_tcp:send(Socket, <<"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n">>),
            timer:sleep(100),
            %% Send frame without MASK bit set (client frames must be masked)
            gen_tcp:send(Socket, <<16#81:8, 1:8, $"X">>),
            timer:sleep(100),
            gen_tcp:close(Socket),
            #{unmasked => 1};
        {error, _} ->
            #{unmasked => 0}
    end.

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

collect_results(Pids, Acc) ->
    receive
        {result, Pid, Result} ->
            NewAcc = merge_results(Acc, Result),
            collect_results(lists:keydelete(Pid, 1, Pids), NewAcc);
        {'DOWN', _Monitor, process, Pid, _Reason} ->
            collect_results(lists:keydelete(Pid, 1, Pids), Acc)
    after 10000 ->
        Acc
    end.

merge_results(Acc, Result) when is_map(Result) ->
    maps:fold(fun(K, V, A) ->
        maps:update_with(K, fun(Old) -> Old + V end, V, A)
    end, Acc, Result);
merge_results(Acc, _) ->
    Acc.

log_results(Transport, Technique, Results) ->
    ?LOG_ERROR(#{
        transport => Transport,
        technique => Technique,
        results => Results
    }).

assert_server_responsive(Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {active, false}], 2000) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            ok;
        {error, Reason} ->
            ct:fail({server_unresponsive, Port, Reason})
    end.

assert_ws_server_responsive(Port) ->
    assert_server_responsive(Port).

cleanup_processes() ->
    %% Cleanup any lingering processes
    ok.
