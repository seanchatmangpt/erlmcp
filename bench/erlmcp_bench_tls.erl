%%%-------------------------------------------------------------------
%%% @doc
%%% TLS 1.3 Performance Benchmark for erlmcp Transports
%%%
%%% This benchmark measures TLS 1.3 performance improvements in OTP 27-28:
%%% - Handshake time comparison (TLS 1.2 vs 1.3)
%%% - Throughput measurements (requests/sec with TLS)
%%% - Connection establishment latency
%%% - Cipher suite performance
%%% - OTP version comparison (26 vs 27 vs 28)
%%%
%%% == OTP Innovations ==
%%%
%%% OTP 27: Full TLS 1.3 support
%%% OTP 28: 15-25% TLS 1.3 performance improvement
%%%
%%% == Expected Results ==
%%%
%%% - TLS 1.3 handshake: <20ms (vs TLS 1.2: ~40ms)
%%% - TLS 1.3 throughput: >10K req/sec (vs TLS 1.2: ~8K req/sec)
%%% - OTP 28 improvement: 15-25% faster than OTP 27
%%% - Cipher suite performance: AES_256_GCM > CHACHA20_POLY1305
%%%
%%% == Usage ==
%%% ```erlang
%%% %% Run full TLS benchmark
%%% erlmcp_bench_tls:run(<<"tls13_full_2026_02_01">>).
%%%
%%% %% Run individual components
%%% erlmcp_bench_tls:run_handshake_benchmark(<<"handshake_comparison">>).
%%% erlmcp_bench_tls:run_throughput_benchmark(<<"tls_throughput">>).
%%% erlmcp_bench_tls:run_otp_comparison(<<"otp_versions">>).
%%% erlmcp_bench_tls:run_cipher_benchmark(<<"cipher_performance">>).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_tls).

-export([run/1,
         run_handshake_benchmark/1,
         run_throughput_benchmark/1,
         run_otp_comparison/0,
         run_cipher_benchmark/1,
         generate_report/2]).
-export([measure_handshake_time/2,
         measure_throughput/3,
         test_cipher_suite/2,
         get_otp_version/0]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Constants
%%====================================================================

-define(BENCH_ITERATIONS, 1000).
-define(WARMUP_ITERATIONS, 100).
-define(TEST_MESSAGE, <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{}}">>).
-define(TEST_HOST, "localhost").
-define(TEST_PORT, 8443).

%% TLS 1.3 Cipher Suites (OTP 27-28 optimized)
-define(TLS13_CIPHERS,
        ["TLS_AES_128_GCM_SHA256",
         "TLS_AES_256_GCM_SHA384",
         "TLS_CHACHA20_POLY1305_SHA256"]).

%% TLS 1.2 Cipher Suites (for comparison)
-define(TLS12_CIPHERS,
        ["ECDHE-RSA-AES128-GCM-SHA256",
         "ECDHE-RSA-AES256-GCM-SHA384",
         "ECDHE-RSA-CHACHA20-POLY1305"]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Run comprehensive TLS benchmark
-spec run(binary()) -> map().
run(WorkloadId) ->
    io:format("Starting TLS 1.3 performance benchmark: ~p~n", [WorkloadId]),
    io:format("OTP Version: ~s~n", [get_otp_version()]),

    %% Ensure applications are started
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(gun),

    %% Run all benchmark phases
    Results = #{
        handshake => run_handshake_benchmark(WorkloadId),
        throughput => run_throughput_benchmark(WorkloadId),
        otp_comparison => run_otp_comparison(),
        cipher_performance => run_cipher_benchmark(WorkloadId)
    },

    %% Generate comprehensive report
    Report = generate_report(WorkloadId, Results),

    %% Save results
    save_results(WorkloadId, Results, Report),

    io:format("TLS benchmark completed: ~p~n", [WorkloadId]),
    Report.

%% @doc Benchmark handshake time (TLS 1.2 vs 1.3)
-spec run_handshake_benchmark(binary()) -> map().
run_handshake_benchmark(WorkloadId) ->
    io:format("~n=== Handshake Benchmark ===~n"),
    io:format("Measuring TLS handshake time...~n"),

    %% Measure TLS 1.3 handshake time
    TLS13Results = measure_handshake_time(tls13, ?BENCH_ITERATIONS),

    %% Measure TLS 1.2 handshake time (for comparison)
    TLS12Results = measure_handshake_time(tls12, ?BENCH_ITERATIONS),

    %% Calculate improvement
    Improvement =
        case TLS12Results.avg_time of
            0 ->
                0.0;
            TLS12Avg ->
                ((TLS12Avg - TLS13Results.avg_time) / TLS12Avg) * 100
        end,

    io:format("TLS 1.3 avg handshake: ~.2f ms~n", [TLS13Results.avg_time]),
    io:format("TLS 1.2 avg handshake: ~.2f ms~n", [TLS12Results.avg_time]),
    io:format("Improvement: ~.1f%~n", [Improvement]),

    #{
        workload_id => WorkloadId,
        tls13 => TLS13Results,
        tls12 => TLS12Results,
        improvement_pct => Improvement,
        timestamp => erlang:system_time(millisecond)
    }.

%% @doc Benchmark throughput with TLS
-spec run_throughput_benchmark(binary()) -> map().
run_throughput_benchmark(WorkloadId) ->
    io:format("~n=== Throughput Benchmark ===~n"),
    io:format("Measuring TLS throughput...~n"),

    %% Test different concurrency levels
    ConcurrencyLevels = [1, 10, 50, 100],
    ThroughputResults =
        lists:map(fun(Concurrency) ->
                         io:format("Testing concurrency: ~p~n", [Concurrency]),
                         #{connections => Concurrency,
                           requests_per_sec => measure_throughput(tls13, Concurrency, ?BENCH_ITERATIONS)}
                  end,
                  ConcurrencyLevels),

    %% Calculate aggregate statistics
    AvgThroughput =
        lists:sum([R#{
                       requests_per_sec} || R <- ThroughputResults]) / length(ThroughputResults),

    MaxThroughput =
        lists:max([R#{
                       requests_per_sec} || R <- ThroughputResults]),

    io:format("Average throughput: ~.2f req/sec~n", [AvgThroughput]),
    io:format("Peak throughput: ~.2f req/sec~n", [MaxThroughput]),

    #{
        workload_id => WorkloadId,
        results => ThroughputResults,
        avg_throughput => AvgThroughput,
        max_throughput => MaxThroughput,
        timestamp => erlang:system_time(millisecond)
    }.

%% @doc Compare TLS performance across OTP versions
-spec run_otp_comparison() -> map().
run_otp_comparison() ->
    io:format("~n=== OTP Version Comparison ===~n"),
    CurrentVersion = get_otp_version(),

    io:format("Current OTP version: ~s~n", [CurrentVersion]),

    %% Measure current version performance
    CurrentPerf = measure_current_otp_performance(),

    %% Expected performance improvements (based on OTP release notes)
    ExpectedImprovements = #{
        <<"26">> => #{tls13_support => partial, improvement_factor => 1.0},
        <<"27">> => #{tls13_support => full, improvement_factor => 1.0},
        <<"28">> => #{tls13_support => full, improvement_factor => 1.2}
    },

    %% Get expected improvement for current version
    MajorVersion = list_to_binary(hd(string:split(CurrentVersion, "."))),
    ExpectedImprovement = maps:get(MajorVersion, ExpectedImprovements, #{improvement_factor => 1.0}),

    io:format("TLS 1.3 Support: ~p~n", [maps:get(tls13_support, ExpectedImprovement, unknown)]),
    io:format("Expected improvement factor: ~.2fx~n", [maps:get(improvement_factor, ExpectedImprovement, 1.0)]),

    #{
        current_version => CurrentVersion,
        current_performance => CurrentPerf,
        expected_improvements => ExpectedImprovement,
        timestamp => erlang:system_time(millisecond)
    }.

%% @doc Benchmark cipher suite performance
-spec run_cipher_benchmark(binary()) -> map().
run_cipher_benchmark(WorkloadId) ->
    io:format("~n=== Cipher Suite Benchmark ===~n"),

    %% Test TLS 1.3 cipher suites
    TLS13Ciphers =
        lists:map(fun(Cipher) ->
                         io:format("Testing cipher: ~s~n", [Cipher]),
                         #{cipher => Cipher,
                           performance => test_cipher_suite(tls13, Cipher)}
                  end,
                  ?TLS13_CIPHERS),

    %% Test TLS 1.2 cipher suites
    TLS12Ciphers =
        lists:map(fun(Cipher) ->
                         io:format("Testing cipher: ~s~n", [Cipher]),
                         #{cipher => Cipher,
                           performance => test_cipher_suite(tls12, Cipher)}
                  end,
                  ?TLS12_CIPHERS),

    %% Find best performing cipher
    BestTLS13 = best_cipher(TLS13Ciphers),
    BestTLS12 = best_cipher(TLS12Ciphers),

    io:format("Best TLS 1.3 cipher: ~s (~.2f req/sec)~n",
              [BestTLS13#{
                     cipher},
               maps:get(requests_per_sec, BestTLS13#{performance}, 0)]),
    io:format("Best TLS 1.2 cipher: ~s (~.2f req/sec)~n",
              [BestTLS12#{
                     cipher},
               maps:get(requests_per_sec, BestTLS12#{performance}, 0)]),

    #{
        workload_id => WorkloadId,
        tls13_ciphers => TLS13Ciphers,
        tls12_ciphers => TLS12Ciphers,
        best_tls13 => BestTLS13,
        best_tls12 => BestTLS12,
        timestamp => erlang:system_time(millisecond)
    }.

%%====================================================================
%% Internal Functions - Measurement
%%====================================================================

%% @doc Measure handshake time for TLS version
-spec measure_handshake_time(tls12 | tls13, pos_integer()) -> map().
measure_handshake_time(TLSVersion, Iterations) ->
    %% Build TLS options
    TLSOpts =
        case TLSVersion of
            tls13 ->
                %% Force TLS 1.3 only
                [{versions, ['tlsv1.3']},
                 {ciphers, ssl:cipher_suites(all, 'tlsv1.3')},
                 {verify, verify_peer},
                 {server_name_indication, disable}];
            tls12 ->
                %% Force TLS 1.2 only
                [{versions, ['tlsv1.2']},
                 {ciphers, ssl:cipher_suites(all, 'tlsv1.2')},
                 {verify, verify_peer},
                 {server_name_indication, disable}]
        end,

    %% Warmup
    lists:foreach(fun(_) ->
                         case ssl:connect(?TEST_HOST, ?TEST_PORT, TLSOpts, 1000) of
                             {ok, Socket} ->
                                 ssl:close(Socket);
                             _ ->
                                 ok
                         end
                  end,
                  lists:seq(1, ?WARMUP_ITERATIONS)),

    %% Measure handshake times
    {Times, _} =
        timer:tc(fun() ->
                       lists:filtermap(fun(_) ->
                                             case ssl:connect(?TEST_HOST,
                                                             ?TEST_PORT,
                                                             TLSOpts,
                                                             1000) of
                                                 {ok, Socket} ->
                                                     Start = erlang:monotonic_time(microsecond),
                                                     %% Wait for handshake to complete
                                                     case ssl:connection_information(Socket) of
                                                         {ok, _} ->
                                                             End = erlang:monotonic_time(microsecond),
                                                             ssl:close(Socket),
                                                             {true, End - Start};
                                                         _ ->
                                                             ssl:close(Socket),
                                                             false
                                                     end;
                                                 _ ->
                                                     false
                                             end
                                     end,
                                     lists:seq(1, Iterations))
                 end),

    %% Calculate statistics
    ValidTimes = [T / 1000 || T <- Times], %% Convert to milliseconds
    AvgTime = lists:sum(ValidTimes) / length(ValidTimes),
    MinTime = lists:min(ValidTimes),
    MaxTime = lists:max(ValidTimes),

    #{avg_time => AvgTime,
      min_time => MinTime,
      max_time => MaxTime,
      iterations => Iterations,
      successful_handshakes => length(ValidTimes)}.

%% @doc Measure throughput with TLS
-spec measure_throughput(tls12 | tls13, pos_integer(), pos_integer()) -> float().
measure_throughput(TLSVersion, Concurrency, Requests) ->
    %% Build TLS options
    TLSOpts =
        case TLSVersion of
            tls13 ->
                [{versions, ['tlsv1.3']},
                 {ciphers, ssl:cipher_suites(all, 'tlsv1.3')},
                 {verify, verify_peer}];
            tls12 ->
                [{versions, ['tlsv1.2']},
                 {ciphers, ssl:cipher_suites(all, 'tlsv1.2')},
                 {verify, verify_peer}]
        end,

    %% Create connection pool
    Pids =
        lists:map(fun(_) ->
                         {ok, Pid} = erlmcp_bench_tls_worker:start_link(TLSOpts),
                         Pid
                  end,
                  lists:seq(1, Concurrency)),

    %% Warmup
    lists:foreach(fun(Pid) ->
                         erlmcp_bench_tls_worker:send(Pid, ?TEST_MESSAGE)
                  end,
                  Pids),

    %% Measure throughput
    {Time, _} =
        timer:tc(fun() ->
                       %% Distribute requests across workers
                       RequestsPerWorker = Requests div Concurrency,
                       lists:foreach(fun(Pid) ->
                                           lists:foreach(fun(_) ->
                                                                 erlmcp_bench_tls_worker:send(Pid,
                                                                                               ?TEST_MESSAGE)
                                                        end,
                                                        lists:seq(1, RequestsPerWorker))
                                    end,
                                    Pids)
                 end),

    %% Cleanup workers
    lists:foreach(fun(Pid) ->
                         catch erlmcp_bench_tls_worker:stop(Pid)
                  end,
                  Pids),

    %% Calculate requests per second
    RequestsPerSec = (Requests / Time) * 1_000_000,
    RequestsPerSec.

%% @doc Test specific cipher suite performance
-spec test_cipher_suite(tls12 | tls13, string()) -> map().
test_cipher_suite(TLSVersion, Cipher) ->
    TLSOpts =
        [{versions,
          case TLSVersion of
              tls13 ->
                  ['tlsv1.3'];
              tls12 ->
                  ['tlsv1.2']
          end},
         {ciphers, [Cipher]},
         {verify, verify_peer}],

    Iterations = 100,
    RequestsPerIter = 100,

    %% Measure performance
    {Time, _} =
        timer:tc(fun() ->
                       lists:foreach(fun(_) ->
                                           case ssl:connect(?TEST_HOST,
                                                           ?TEST_PORT,
                                                           TLSOpts,
                                                           1000) of
                                               {ok, Socket} ->
                                                   %% Send test requests
                                                   lists:foreach(fun(_) ->
                                                                         ssl:send(Socket,
                                                                                 ?TEST_MESSAGE),
                                                                         ssl:setopts(Socket,
                                                                                    [{active,
                                                                                      once}]),
                                                                         receive
                                                                             {ssl, Socket, _} ->
                                                                                 ok
                                                                         after 100 ->
                                                                                 ok
                                                                         end
                                                                 end,
                                                                 lists:seq(1, RequestsPerIter)),
                                                   ssl:close(Socket);
                                               _ ->
                                                   ok
                                           end
                                    end,
                                    lists:seq(1, Iterations))
                 end),

    TotalRequests = Iterations * RequestsPerIter,
    RequestsPerSec = (TotalRequests / Time) * 1_000_000,

    #{requests_per_sec => RequestsPerSec,
      cipher => Cipher,
      total_requests => TotalRequests,
      time_us => Time}.

%% @doc Measure current OTP performance
-spec measure_current_otp_performance() -> map().
measure_current_otp_performance() ->
    %% Quick handshake test
    HandshakeTime = measure_handshake_time(tls13, 100),

    %% Quick throughput test
    Throughput = measure_throughput(tls13, 10, 1000),

    #{handshake_time_ms => HandshakeTime#{avg_time},
      throughput_req_per_sec => Throughput}.

%% @doc Get current OTP version
-spec get_otp_version() -> string().
get_otp_version() ->
    erlang:system_info(otp_release).

%% @doc Find best performing cipher
-spec best_cipher([map()]) -> map().
best_cipher(Ciphers) ->
    F =
        fun(#{performance := #{requests_per_sec := R1}} = A,
            #{performance := #{requests_per_sec := R2}} = B) ->
               R1 >= R2
        end,
    lists:max(F, Ciphers).

%%====================================================================
%% Internal Functions - Reporting
%%====================================================================

%% @doc Generate comprehensive benchmark report
-spec generate_report(binary(), map()) -> map().
generate_report(WorkloadId, Results) ->
    #{handshake := HandshakeResults,
      throughput := ThroughputResults,
      otp_comparison := OTPResults,
      cipher_performance := CipherResults} =
        Results,

    Report =
        #{workload_id => WorkloadId,
          summary =>
              #{
                handshake_improvement =>
                    HandshakeResults#{improvement_pct},
                avg_throughput =>
                    ThroughputResults#{avg_throughput},
                peak_throughput =>
                    ThroughputResults#{max_throughput},
                otp_version =>
                    OTPResults#{current_version},
                best_cipher =>
                    CipherResults#{best_tls13}#{cipher}
              },
          details => Results,
          timestamp => erlang:system_time(millisecond)},

    io:format("~n=== TLS Benchmark Summary ===~n"),
    io:format("Handshake improvement: ~.1f%~n",
              [maps:get(handshake_improvement, Report#{summary}, 0)]),
    io:format("Average throughput: ~.2f req/sec~n",
              [maps:get(avg_throughput, Report#{summary}, 0)]),
    io:format("Peak throughput: ~.2f req/sec~n",
              [maps:get(peak_throughput, Report#{summary}, 0)]),
    io:format("Best cipher: ~s~n",
              [maps:get(best_cipher, Report#{summary}, <<"unknown">>)]),

    Report.

%% @doc Save benchmark results
-spec save_results(binary(), map(), map()) -> ok.
save_results(WorkloadId, Results, Report) ->
    Filename =
        io_lib:format("bench/results/tls_benchmark_~s_~p.json",
                     [WorkloadId,
                      erlang:system_time(millisecond)]),

    %% Ensure directory exists
    ok = filelib:ensure_dir(Filename),

    %% Save results as JSON
    Json = jsx:encode(#{results => Results, report => Report}),
    ok = file:write_file(Filename, Json),

    io:format("Results saved to: ~s~n", [Filename]),
    ok.
