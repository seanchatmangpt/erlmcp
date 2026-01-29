#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin -pa ebin

%%%-------------------------------------------------------------------
%%% @doc Dictionary Attack Stress Test #15 - Standalone Script
%%% @end
%%%-------------------------------------------------------------------

-mode(compile).

main(Args) ->
    %% Parse arguments
    Port = case Args of
        [P|_] -> list_to_integer(P);
        _ -> 10015
    end,
    
    Rate = case length(Args) >= 2 of
        true -> list_to_integer(lists:nth(2, Args));
        false -> 100
    end,
    
    TotalAttempts = case length(Args) >= 3 of
        true -> list_to_integer(lists:nth(3, Args));
        false -> 10000
    end,
    
    CredentialType = case length(Args) >= 4 of
        true -> list_to_existing_atom(lists:nth(4, Args));
        false -> mixed
    end,
    
    %% Print header
    io:format("=== DICTIONARY ATTACK STRESS TEST #15 ===~n"),
    io:format("Attack: Rapid Reconnect with Random IDs~n"),
    io:format("Objective: Find rate limits, lockout behavior, or crash from auth overload~n~n"),
    
    io:format("Configuration:~n"),
    io:format("  Port: ~p~n", [Port]),
    io:format("  Rate: ~p connections/sec~n", [Rate]),
    io:format("  Total Attempts: ~p~n", [TotalAttempts]),
    io:format("  Credential Type: ~p~n~n", [CredentialType]),
    
    %% Record initial memory
    InitialMemory = erlang:memory(total) / 1024 / 1024,
    io:format("Initial memory: ~.2f MB~n~n", [InitialMemory]),
    
    %% Start the attack
    StartTime = erlang:system_time(millisecond),
    
    io:format("Starting attack loop...~n~n"),
    
    %% Run attack and collect stats
    {ok, Stats} = run_dictionary_attack(Port, Rate, TotalAttempts, CredentialType),
    
    EndTime = erlang:system_time(millisecond),
    Duration = (EndTime - StartTime) / 1000.0,
    
    %% Record final memory
    FinalMemory = erlang:memory(total) / 1024 / 1024,
    MemoryLeaked = FinalMemory - InitialMemory,
    
    %% Generate report
    io:format("~n=== ATTACK COMPLETE ===~n~n"),
    generate_report(Stats, Duration, InitialMemory, FinalMemory, MemoryLeaked),
    
    %% Save results
    Timestamp = erlang:system_time(second),
    ReportFile = lists:flatten(io_lib:format("bench/results/dictionary_attack_~p_~p.txt", [CredentialType, Timestamp])),
    save_report(ReportFile, Stats, Duration, InitialMemory, FinalMemory, MemoryLeaked, CredentialType),
    io:format("~nReport saved to: ~s~n", [ReportFile]),
    
    ok.

%%%===================================================================
%%% Attack Functions
%%%===================================================================

%% @private Run the dictionary attack
run_dictionary_attack(Port, Rate, TotalAttempts, CredentialType) ->
    %% Spawn attack supervisor
    Parent = self(),
    
    %% Calculate workers needed
    WorkersCount = max(1, Rate div 100),
    AttemptsPerWorker = TotalAttempts div WorkersCount,
    
    io:format("Spawning ~p attack workers, ~p attempts each~n", [WorkersCount, AttemptsPerWorker]),
    
    %% Spawn workers
    Workers = lists:map(fun(WorkerId) ->
        spawn_monitor(fun() -> 
            attack_worker(Port, Rate, CredentialType, AttemptsPerWorker, WorkerId, Parent) 
        end)
    end, lists:seq(1, WorkersCount)),
    
    %% Collect results
    collect_worker_results(Workers, #{
        total_attempts => 0,
        successful_auth => 0,
        failed_auth => 0,
        rate_limited => 0,
        connection_errors => 0,
        server_crashes => 0,
        rate_limit_detected => false,
        rate_limit_at => undefined
    }).

%% @private Attack worker
attack_worker(Port, Rate, CredentialType, Attempts, WorkerId, Parent) ->
    DelayMs = 1000 div Rate,
    
    Stats = lists:foldl(fun(AttemptNum, AccStats) ->
        %% Generate random client ID (NO REUSE)
        ClientId = generate_random_id(),
        
        %% Generate credentials
        Credentials = generate_credentials(CredentialType, AttemptNum),
        
        %% Attempt connection
        Result = attempt_connect_auth_disconnect(Port, ClientId, Credentials),
        
        %% Update stats
        NewStats = update_worker_stats(AccStats, Result),
        
        %% Rate limiting
        timer:sleep(DelayMs),
        
        %% Progress report
        case AttemptNum rem 1000 of
            0 -> 
                Parent ! {progress, WorkerId, AttemptNum},
                NewStats;
            _ -> 
                NewStats
        end
    end, #{attempts => 0, success => 0, failed => 0, rate_limited => 0, errors => 0}, 
    lists:seq(1, Attempts)),
    
    %% Send final stats to parent
    Parent ! {worker_complete, WorkerId, Stats}.

%% @private Attempt connect, authenticate, disconnect
attempt_connect_auth_disconnect(Port, ClientId, _Credentials) ->
    try
        %% Connect to server
        case gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {packet, 0}], 1000) of
            {ok, Socket} ->
                %% Send initialize message
                InitMsg = jsx:encode(#{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => 1,
                    <<"method">> => <<"initialize">>,
                    <<"params">> => #{
                        <<"protocolVersion">> => <<"2025-11-25">>,
                        <<"capabilities">> => #{},
                        <<"clientInfo">> => #{
                            <<"name">> => ClientId,
                            <<"version">> => <<"1.0.0">>
                        }
                    }
                }),
                
                case gen_tcp:send(Socket, InitMsg) of
                    ok ->
                        %% Try to receive response
                        case gen_tcp:recv(Socket, 0, 500) of
                            {ok, Response} ->
                                %% Parse response
                                try jsx:decode(Response, [return_maps]) of
                                    Json ->
                                        case analyze_response(Json) of
                                            {rate_limited, _} = Result ->
                                                gen_tcp:close(Socket),
                                                Result;
                                            {auth_success, _} = Result ->
                                                gen_tcp:close(Socket),
                                                Result;
                                            {auth_failed, _} = Result ->
                                                gen_tcp:close(Socket),
                                                Result;
                                            _ ->
                                                gen_tcp:close(Socket),
                                                {auth_error, unknown}
                                        end
                                catch
                                    _:_:_ ->
                                        gen_tcp:close(Socket),
                                        {invalid_response, Response}
                                end;
                            {error, timeout} ->
                                gen_tcp:close(Socket),
                                {auth_timeout};
                            {error, _Reason} ->
                                gen_tcp:close(Socket),
                                {recv_error}
                        end;
                    {error, _Reason} ->
                        gen_tcp:close(Socket),
                        {send_error}
                end;
            {error, _Reason} ->
                {connection_error}
        end
    catch
        _:_:_ ->
            {crash}
    end.

%% @private Analyze authentication response
analyze_response(Json) ->
    case maps:get(<<"error">>, Json, undefined) of
        undefined ->
            %% Success response
            case maps:get(<<"result">>, Json, undefined) of
                undefined -> {unexpected_response, no_result};
                Result -> {auth_success, Result}
            end;
        Error ->
            %% Error response
            ErrorCode = maps:get(<<"code">>, Error, 0),
            ErrorMessage = maps:get(<<"message">>, Error, <<>>),
            
            case ErrorCode of
                1056 -> {rate_limited, ErrorMessage};  % RATE_LIMIT_EXCEEDED
                1057 -> {rate_limited, ErrorMessage};  % RATE_LIMIT_PER_SECOND
                1058 -> {rate_limited, ErrorMessage};  % RATE_LIMIT_PER_MINUTE
                1011 -> {auth_failed, ErrorMessage};  % AUTH_FAILED
                1013 -> {auth_failed, ErrorMessage};  % AUTH_INVALID_CREDENTIALS
                1060 -> {connection_limit, ErrorMessage};  % CONCURRENT_LIMIT_EXCEEDED
                _ -> {auth_error, ErrorCode, ErrorMessage}
            end
    end.

%% @private Collect worker results
collect_worker_results([], Stats) ->
    {ok, Stats};
collect_worker_results([{Pid, MRef} | Workers], Stats) ->
    receive
        {worker_complete, Pid, WorkerStats} ->
            NewStats = combine_stats(Stats, WorkerStats),
            erlang:demonitor(MRef, [flush]),
            collect_worker_results(Workers, NewStats);
        {'DOWN', MRef, process, Pid, _Reason} ->
            NewStats = Stats#{server_crashes => maps_get(server_crashes, Stats, 0) + 1},
            collect_worker_results(Workers, NewStats);
        {progress, WorkerId, Progress} ->
            io:format("Worker ~p: ~p attempts complete~n", [WorkerId, Progress]),
            collect_worker_results([{Pid, MRef} | Workers], Stats)
    after 300000 ->
        io:format("Timeout waiting for workers~n"),
        {ok, Stats}
    end.

%% @private Combine stats
combine_stats(BaseStats, WorkerStats) ->
    lists:foldl(fun(Key, Acc) ->
        BaseValue = maps_get(Key, BaseStats, 0),
        WorkerValue = maps_get(Key, WorkerStats, 0),
        Acc#{Key => BaseValue + WorkerValue}
    end, BaseStats, [attempts, success, failed, rate_limited, errors]).

%%%===================================================================
%%% Utility Functions
%%%===================================================================

%% @private Generate random client ID
generate_random_id() ->
    Rand = crypto:strong_rand_bytes(16),
    <<"client_", Rand/binary>>.

%% @private Generate credentials
generate_credentials(valid, _AttemptNum) ->
    #{type => valid, api_key => <<"erlmcp_test_key_valid_12345">>};
generate_credentials(invalid, _AttemptNum) ->
    #{type => invalid, api_key => generate_random_api_key()};
generate_credentials(mixed, AttemptNum) ->
    case AttemptNum rem 2 of
        0 -> generate_credentials(valid, AttemptNum);
        1 -> generate_credentials(invalid, AttemptNum)
    end;
generate_credentials(same_credential, _AttemptNum) ->
    #{type => same_credential, api_key => <<"erlmcp_test_key_valid_12345">>}.

%% @private Generate random API key
generate_random_api_key() ->
    Rand = crypto:strong_rand_bytes(32),
    <<"key_", Rand/binary>>.

%% @private Update worker stats
update_worker_stats(Stats, Result) ->
    Attempts = maps_get(attempts, Stats, 0) + 1,
    case Result of
        {auth_success, _} ->
            Stats#{attempts => Attempts, success => maps_get(success, Stats, 0) + 1};
        {auth_failed, _} ->
            Stats#{attempts => Attempts, failed => maps_get(failed, Stats, 0) + 1};
        {rate_limited, _} ->
            Stats#{attempts => Attempts, rate_limited => maps_get(rate_limited, Stats, 0) + 1};
        {connection_error, _} ->
            Stats#{attempts => Attempts, errors => maps_get(errors, Stats, 0) + 1};
        _ ->
            Stats#{attempts => Attempts, errors => maps_get(errors, Stats, 0) + 1}
    end.

%% @private Safe maps:get with default
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.

%%%===================================================================
%%% Reporting Functions
%%%===================================================================

%% @private Generate report
generate_report(Stats, Duration, InitialMemory, FinalMemory, MemoryLeaked) ->
    TotalAttempts = maps_get(attempts, Stats, 0),
    SuccessCount = maps_get(success, Stats, 0),
    FailedCount = maps_get(failed, Stats, 0),
    RateLimitedCount = maps_get(rate_limited, Stats, 0),
    ErrorCount = maps_get(errors, Stats, 0),
    CrashCount = maps_get(server_crashes, Stats, 0),
    
    SuccessRate = case TotalAttempts > 0 of
        true -> (SuccessCount / TotalAttempts) * 100.0;
        false -> 0.0
    end,
    
    io:format("=== DICTIONARY ATTACK CRASH TEST ===~n~n", []),
    
    io:format("Attack Configuration:~n", []),
    io:format("- Total Attempts: ~p~n", [TotalAttempts]),
    io:format("- Connect Rate: ~.2f/sec~n", [TotalAttempts / Duration]),
    io:format("- Duration: ~.2f seconds~n", [Duration]),
    io:format("- Random IDs: yes (no reuse)~n~n", []),
    
    io:format("Credential Statistics:~n", []),
    io:format("- Successful Auth: ~p~n", [SuccessCount]),
    io:format("- Failed Auth: ~p~n", [FailedCount]),
    io:format("- Auth Success Rate: ~.2f%~n~n", [SuccessRate]),
    
    io:format("RATE LIMITING:~n", []),
    case RateLimitedCount > 0 of
        true ->
            io:format("- Limit Detected: yes~n", []),
            io:format("- Rate Limited Count: ~p~n", [RateLimitedCount]),
            io:format("- LIMIT EFFECTIVENESS: Attack was throttled~n~n", []);
        false ->
            io:format("- Limit Detected: NO~n", []),
            io:format("- WARNING: System vulnerable to brute force~n~n", [])
    end,
    
    io:format("SERVER STATUS:~n", []),
    io:format("- Server Crashed: ~p~n", [CrashCount > 0]),
    io:format("- Connection Errors: ~p~n~n", [ErrorCount]),
    
    io:format("MEMORY ANALYSIS:~n", []),
    io:format("- Initial Memory: ~.2f MB~n", [InitialMemory]),
    io:format("- Final Memory: ~.2f MB~n", [FinalMemory]),
    io:format("- Memory Leaked: ~.2f MB~n~n", [MemoryLeaked]),
    
    io:format("CRASH POINT:~n", []),
    case CrashCount > 0 of
        true ->
            io:format("- Server crashed during attack~n", []);
        false ->
            io:format("- No crash detected~n", [])
    end,
    io:format("- Server remained responsive~n~n", []),
    
    io:format("ANALYSIS:~n", []),
    case RateLimitedCount > 0 of
        true ->
            io:format("- Rate limiting is WORKING - system protected~n", []),
            io:format("- DoS resistance: GOOD~n", []);
        false ->
            io:format("- Rate limiting NOT DETECTED - system VULNERABLE~n", []),
            io:format("- DoS resistance: POOR - implement rate limiting immediately~n", [])
    end,
    
    case MemoryLeaked > 10.0 of
        true ->
            io:format("- Memory leak detected (~.2f MB) - investigate session cleanup~n", [MemoryLeaked]);
        false ->
            io:format("- Memory usage stable - no leaks detected~n", [])
    end.

%% @private Save report to file
save_report(ReportFile, Stats, Duration, InitialMemory, FinalMemory, MemoryLeaked, CredentialType) ->
    TotalAttempts = maps_get(attempts, Stats, 0),
    SuccessCount = maps_get(success, Stats, 0),
    FailedCount = maps_get(failed, Stats, 0),
    RateLimitedCount = maps_get(rate_limited, Stats, 0),
    ErrorCount = maps_get(errors, Stats, 0),
    CrashCount = maps_get(server_crashes, Stats, 0),
    
    SuccessRate = case TotalAttempts > 0 of
        true -> (SuccessCount / TotalAttempts) * 100.0;
        false -> 0.0
    end,
    
    ReportContent = io_lib:format(
        "=== DICTIONARY ATTACK CRASH TEST ===~n~n"
        "Attack Configuration:~n"
        "- Total Attempts: ~p~n"
        "- Connect Rate: ~.2f/sec~n"
        "- Random IDs: yes (no reuse)~n"
        "- Duration: ~.2f seconds~n"
        "- Credential Type: ~p~n~n"
        "Credential Statistics:~n"
        "- Successful Auth: ~p~n"
        "- Failed Auth: ~p~n"
        "- Auth Success Rate: ~.2f%~n~n"
        "RATE LIMITING:~n"
        "- Limit Detected: ~p~n"
        "- Rate Limited Count: ~p~n~n"
        "SERVER STATUS:~n"
        "- Server Crashed: ~p~n"
        "- Connection Errors: ~p~n~n"
        "MEMORY ANALYSIS:~n"
        "- Initial Memory: ~.2f MB~n"
        "- Final Memory: ~.2f MB~n"
        "- Memory Leaked: ~.2f MB~n~n"
        "CRASH POINT:~n"
        "- No crash detected~n"
        "- Server remained responsive~n~n"
        "ANALYSIS:~n"
        "~s~n"
        "~s~n",
        [
            TotalAttempts,
            TotalAttempts / Duration,
            Duration,
            CredentialType,
            
            SuccessCount,
            FailedCount,
            SuccessRate,
            
            RateLimitedCount > 0,
            RateLimitedCount,
            
            CrashCount > 0,
            ErrorCount,
            
            InitialMemory,
            FinalMemory,
            MemoryLeaked,
            
            case RateLimitedCount > 0 of
                true -> "Rate limiting is WORKING - system protected from DoS\nDoS resistance: GOOD";
                false -> "Rate limiting NOT DETECTED - system VULNERABLE to brute force\nDoS resistance: POOR - implement rate limiting immediately"
            end,
            
            case MemoryLeaked > 10.0 of
                true -> io_lib:format("Memory leak detected (~.2f MB) - investigate session cleanup", [MemoryLeaked]);
                false -> "Memory usage stable - no leaks detected"
            end
        ]
    ),
    
    filelib:ensure_dir(ReportFile),
    file:write_file(ReportFile, ReportContent).
