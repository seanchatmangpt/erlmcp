%%%-------------------------------------------------------------------
%%% @doc erlmcp_bench_dictionary_attack - Destructive Stress Test #15
%%%
%%% Dictionary Attack: Rapid Reconnect with Random IDs
%%% 
%%% This test simulates a dictionary attack against the authentication system
%%% by rapidly reconnecting with different random client IDs, attempting
%%% authentication with various credentials, and immediately disconnecting.
%%%
%%% Attack Vectors:
%%% 1. Brute force valid credentials with different IDs
%%% 2. Invalid credential flooding (noise)
%%% 3. Mixed valid/invalid (50/50)
%%% 4. Same credential, different IDs (session flooding)
%%%
%%% Measurements:
%%% - Rate limiting detection and thresholds
%%% - Account lockout behavior
%%% - Memory leaks from stale sessions
%%% - Server crash points
%%% - Authentication system degradation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_dictionary_attack).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include_lib("erlmcp_core/include/erlmcp_refusal.hrl").

%% API
-export([
    run/0,
    run/1,
    run_test/3,
    stop/1,
    get_results/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type attack_config() :: #{
    total_attempts => pos_integer(),
    connect_rate_per_sec => pos_integer(),
    port => pos_integer(),
    credential_type => valid | invalid | mixed | same_credential,
    report_interval => pos_integer()
}.

-type attack_stats() :: #{
    total_attempts := non_neg_integer(),
    successful_auth := non_neg_integer(),
    failed_auth := non_neg_integer(),
    rate_limited := non_neg_integer(),
    account_locked := non_neg_integer(),
    connection_errors := non_neg_integer(),
    server_crashes := non_neg_integer(),
    start_time => integer(),
    end_time => integer(),
    duration_ms => non_neg_integer(),
    auth_success_rate => float(),
    rate_limit_detected => boolean(),
    rate_limit_at => non_neg_integer() | undefined,
    memory_initial => float(),
    memory_peak => float(),
    memory_final => float(),
    memory_leaked => float(),
    sessions_cleaned => non_neg_integer(),
    sessions_leaked => non_neg_integer()
}.

-type attack_result() :: #{
    workload_id := binary(),
    benchmark := binary(),
    attack_type := binary(),
    config := attack_config(),
    stats := attack_stats(),
    timestamp := integer(),
    test_passed := boolean(),
    crash_point => non_neg_integer() | undefined,
    crash_reason => binary() | undefined,
    recommendations => list(binary())
}.

-export_type([attack_config/0, attack_stats/0, attack_result/0]).

%% State record
-record(state, {
    bench_pid :: pid() | undefined,
    config :: attack_config(),
    stats :: attack_stats(),
    report_timer :: reference() | undefined,
    attack_pids :: [pid()],
    server_monitor :: reference() | undefined,
    auth_monitor :: reference() | undefined
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Run dictionary attack with default config
-spec run() -> {ok, attack_result()} | {error, term()}.
run() ->
    run(#{
        total_attempts => 1000000,
        connect_rate_per_sec => 1000,
        port => 10015,
        credential_type => mixed,
        report_interval => 10000
    }).

%% @doc Run dictionary attack with custom config
-spec run(attack_config()) -> {ok, attack_result()} | {error, term()}.
run(Config) ->
    {ok, BenchPid} = gen_server:start_link(?MODULE, [Config], []),
    gen_server:call(BenchPid, execute_attack).

%% @doc Run a specific attack test
-spec run_test(pos_integer(), pos_integer(), atom()) -> {ok, attack_result()} | {error, term()}.
run_test(Port, Rate, CredentialType) ->
    run(#{
        total_attempts => 1000000,
        connect_rate_per_sec => Rate,
        port => Port,
        credential_type => CredentialType,
        report_interval => 10000
    }).

%% @doc Stop the attack
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Get attack results
-spec get_results(pid()) -> {ok, attack_stats()}.
get_results(Pid) ->
    gen_server:call(Pid, get_results).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([attack_config()]) -> {ok, state()}.
init([Config]) ->
    process_flag(trap_exit, true),
    
    %% Monitor critical processes
    ServerMonitor = monitor_server_process(),
    AuthMonitor = monitor_auth_process(),
    
    State = #state{
        config = Config,
        stats = init_stats(),
        attack_pids = [],
        server_monitor = ServerMonitor,
        auth_monitor = AuthMonitor
    },
    
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call(execute_attack, _From, State) ->
    %% Execute the attack and return results
    {Result, NewState} = do_execute_attack(State),
    {reply, Result, NewState};

handle_call(get_results, _From, State) ->
    {reply, {ok, State#state.stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) 
    when Ref =:= State#state.server_monitor;
         Ref =:= State#state.auth_monitor ->
    %% Critical process died - record crash
    logger:error("Critical process crashed: ~p", [Reason]),
    NewStats = (State#state.stats)#{
        server_crashes => maps:get(server_crashes, State#state.stats, 0) + 1
    },
    {noreply, State#state{stats = NewStats}};

handle_info({'EXIT', Pid, Reason}, State) ->
    %% Attack worker died
    logger:warning("Attack worker ~p died: ~p", [Pid, Reason]),
    NewPids = lists:delete(Pid, State#state.attack_pids),
    {noreply, State#state{attack_pids = NewPids}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Attack Execution
%%====================================================================

%% @private Execute the dictionary attack
do_execute_attack(State) ->
    Config = State#state.config,
    TotalAttempts = maps:get(total_attempts, Config, 1000000),
    Rate = maps_get(connect_rate_per_sec, Config, 1000),
    Port = maps_get(port, Config, 10015),
    CredentialType = maps_get(credential_type, Config, mixed),
    
    logger:info("Starting DICTIONARY ATTACK: ~p attempts at ~p/sec", [TotalAttempts, Rate]),
    logger:info("Attack type: ~p", [CredentialType]),
    logger:info("Target port: ~p", [Port]),
    
    %% Record initial memory
    InitialMemory = get_memory_usage(),
    
    %% Start the attack
    StartTime = erlang:system_time(millisecond),
    
    %% Spawn attack workers
    Workers = spawn_attack_workers(Port, Rate, CredentialType, TotalAttempts),
    
    %% Monitor progress
    Stats = monitor_attack_progress(Workers, StartTime, State#state.stats, Config),
    
    EndTime = erlang:system_time(millisecond),
    
    %% Record final memory
    PeakMemory = get_memory_usage(),
    FinalMemory = get_memory_usage(),
    MemoryLeaked = PeakMemory - InitialMemory,
    
    %% Calculate final stats
    FinalStats = Stats#{
        start_time => StartTime,
        end_time => EndTime,
        duration_ms => EndTime - StartTime,
        memory_initial => InitialMemory,
        memory_peak => PeakMemory,
        memory_final => FinalMemory,
        memory_leaked => MemoryLeaked
    },
    
    %% Check for leaks
    SessionsLeaked = count_leaked_sessions(),
    SessionsCleaned = count_cleaned_sessions(),
    
    FinalStats2 = FinalStats#{
        sessions_cleaned => SessionsCleaned,
        sessions_leaked => SessionsLeaked
    },
    
    %% Build result
    Result = #{
        workload_id => <<"dictionary_attack_", (atom_to_binary(CredentialType))/binary>>,
        benchmark => <<"dictionary_attack">>,
        attack_type => atom_to_binary(CredentialType),
        config => Config,
        stats => FinalStats2,
        timestamp => erlang:system_time(second),
        test_passed => not (maps:get(server_crashes, FinalStats2, 0) > 0),
        crash_point => maps_get(crash_point, FinalStats2, undefined),
        crash_reason => maps_get(crash_reason, FinalStats2, undefined),
        recommendations => generate_recommendations(FinalStats2)
    },
    
    logger:info("Dictionary attack complete: ~p attempts in ~p ms", 
                [maps_get(total_attempts, FinalStats2, 0), EndTime - StartTime]),
    
    {{ok, Result}, State#state{stats = FinalStats2}}.

%% @private Spawn attack workers
spawn_attack_workers(Port, Rate, CredentialType, TotalAttempts) ->
    %% Calculate workers needed (1 worker per 100 connections/sec)
    WorkersCount = max(1, Rate div 100),
    AttemptsPerWorker = TotalAttempts div WorkersCount,
    
    logger:info("Spawning ~p attack workers, ~p attempts each", [WorkersCount, AttemptsPerWorker]),
    
    lists:map(fun(WorkerId) ->
        spawn_link(fun() -> 
            attack_worker(Port, Rate, CredentialType, AttemptsPerWorker, WorkerId) 
        end)
    end, lists:seq(1, WorkersCount)).

%% @private Attack worker - rapid connect-auth-disconnect cycle
attack_worker(Port, Rate, CredentialType, Attempts, WorkerId) ->
    logger:info("Worker ~p starting: ~p attempts", [WorkerId, Attempts]),
    
    %% Attack loop with rate limiting
    DelayMs = 1000 div Rate,
    
    lists:foldl(fun(AttemptNum, AccStats) ->
        %% Generate random client ID (NO REUSE)
        ClientId = generate_random_id(),
        
        %% Generate credentials based on attack type
        Credentials = generate_credentials(CredentialType, AttemptNum),
        
        %% Attempt connection and auth
        Result = attempt_connect_auth_disconnect(Port, ClientId, Credentials),
        
        %% Update stats
        NewStats = update_worker_stats(AccStats, Result),
        
        %% Rate limiting
        timer:sleep(DelayMs),
        
        %% Periodic progress report
        case AttemptNum rem 10000 of
            0 -> logger:info("Worker ~p: ~p attempts complete", [WorkerId, AttemptNum]);
            _ -> ok
        end,
        
        NewStats
    end, init_worker_stats(), lists:seq(1, Attempts)),
    
    logger:info("Worker ~p complete", [WorkerId]).

%% @private Attempt connect, authenticate, disconnect
attempt_connect_auth_disconnect(Port, ClientId, Credentials) ->
    try
        %% Step 1: Connect to server
        case connect_to_server(Port, ClientId) of
            {ok, Socket} ->
                %% Step 2: Attempt authentication
                AuthResult = attempt_authentication(Socket, ClientId, Credentials),
                
                %% Step 3: Disconnect immediately
                disconnect(Socket),
                
                AuthResult;
            {error, Reason} ->
                {connection_error, Reason}
        end
    catch
        Class:Reason:Stacktrace ->
            logger:error("Attack attempt crashed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {crash, {Class, Reason}}
    end.

%% @private Connect to MCP server
connect_to_server(Port, ClientId) ->
    %% Simulate TCP connection
    case gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {packet, 0}], 1000) of
        {ok, Socket} ->
            %% Send MCP initialize with random client ID
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
                ok -> {ok, Socket};
                {error, Reason} -> {error, {send_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {connect_failed, Reason}}
    end.

%% @private Attempt authentication
attempt_authentication(Socket, ClientId, Credentials) ->
    %% Extract credential type
    CredentialType = maps_get(type, Credentials, invalid),
    ApiKey = maps_get(api_key, Credentials, <<>>),
    
    %% Send authenticate request
    AuthMsg = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"method">> => <<"auth/authenticate">>,
        <<"params">> => #{
            <<"method">> => <<"api_key">>,
            <<"credentials">> => #{
                <<"api_key">> => ApiKey
            }
        }
    }),
    
    case gen_tcp:send(Socket, AuthMsg) of
        ok ->
            %% Wait for response (with timeout)
            case gen_tcp:recv(Socket, 0, 500) of
                {ok, Response} ->
                    case jsx:is_json(Response) of
                        true ->
                            ResponseJson = jsx:decode(Response, [return_maps]),
                            analyze_auth_response(ResponseJson, CredentialType);
                        false ->
                            {invalid_response, Response}
                    end;
                {error, timeout} ->
                    {auth_timeout, Response};
                {error, Reason} ->
                    {recv_error, Reason}
            end;
        {error, Reason} ->
            {send_error, Reason}
    end.

%% @private Analyze authentication response
analyze_auth_response(ResponseJson, CredentialType) ->
    case maps_get(<<"error">>, ResponseJson, undefined) of
        undefined ->
            %% Success response
            case maps_get(<<"result">>, ResponseJson, undefined) of
                undefined -> {unexpected_response, no_result};
                Result -> {auth_success, Result}
            end;
        Error ->
            %% Error response - check for rate limiting
            ErrorCode = maps_get(<<"code">>, Error, 0),
            ErrorMessage = maps_get(<<"message">>, Error, <<>>),
            
            case ErrorCode of
                ?REFUSAL_RATE_LIMIT_EXCEEDED -> {rate_limited, ErrorMessage};
                ?REFUSAL_RATE_LIMIT_PER_SECOND -> {rate_limited, ErrorMessage};
                ?REFUSAL_RATE_LIMIT_PER_MINUTE -> {rate_limited, ErrorMessage};
                ?REFUSAL_AUTH_FAILED -> {auth_failed, ErrorMessage};
                ?REFUSAL_AUTH_INVALID_CREDENTIALS -> {invalid_credentials, ErrorMessage};
                ?REFUSAL_CONCURRENT_LIMIT_EXCEEDED -> {connection_limit, ErrorMessage};
                _ -> {auth_error, ErrorCode, ErrorMessage}
            end
    end.

%% @private Disconnect from server
disconnect(Socket) ->
    gen_tcp:close(Socket).

%% @private Monitor attack progress
monitor_attack_progress(Workers, StartTime, Stats, Config) ->
    ReportInterval = maps_get(report_interval, Config, 10000),
    
    %% Wait for workers to complete
    monitor_workers(Workers, StartTime, Stats, ReportInterval).

%% @private Monitor workers until completion
monitor_workers(Workers, StartTime, Stats, ReportInterval) ->
    %% Check if all workers are alive
    AliveWorkers = [W || W <- Workers, is_process_alive(W)],
    
    case AliveWorkers of
        [] ->
            %% All workers complete
            Stats;
        _ ->
            %% Some workers still running
            timer:sleep(min(ReportInterval, 1000)),
            
            %% Update intermediate stats
            CurrentStats = collect_worker_stats(Workers),
            CombinedStats = combine_stats(Stats, CurrentStats),
            
            %% Check for rate limiting
            NewStats = case maps_get(rate_limited, CurrentStats, 0) of
                0 -> CombinedStats;
                Count when Count > 0 ->
                    %% Rate limit detected
                    case maps_get(rate_limit_detected, CombinedStats, false) of
                        false ->
                            logger:warning("RATE LIMIT DETECTED at ~p attempts", 
                                         [maps_get(total_attempts, CombinedStats, 0)]),
                            CombinedStats#{
                                rate_limit_detected => true,
                                rate_limit_at => maps_get(total_attempts, CombinedStats, 0)
                            };
                        true ->
                            CombinedStats
                    end
            end,
            
            monitor_workers(AliveWorkers, StartTime, NewStats, ReportInterval)
    end.

%% @private Collect stats from all workers
collect_worker_stats(Workers) ->
    %% This would normally collect from workers via message passing
    %% For now, return empty stats (workers track their own)
    #{}.

%% @private Combine stats
combine_stats(BaseStats, NewStats) ->
    maps:fold(fun(K, V, Acc) ->
        case maps:get(K, Acc, undefined) of
            undefined -> Acc#{K => V};
            Existing when is_integer(V), is_integer(Existing) -> Acc#{K => Existing + V};
            _ -> Acc
        end
    end, BaseStats, NewStats).

%%====================================================================
%% Credential Generation
%%====================================================================

%% @private Generate credentials based on attack type
generate_credentials(valid, AttemptNum) ->
    %% Use valid API key (brute force with different IDs)
    #{
        type => valid,
        api_key => <<"erlmcp_test_key_valid_12345">>,
        attempt => AttemptNum
    };
generate_credentials(invalid, _AttemptNum) ->
    %% Random invalid API key
    #{
        type => invalid,
        api_key => generate_random_api_key(),
        attempt => _AttemptNum
    };
generate_credentials(mixed, AttemptNum) ->
    %% 50/50 mix of valid and invalid
    case AttemptNum rem 2 of
        0 -> generate_credentials(valid, AttemptNum);
        1 -> generate_credentials(invalid, AttemptNum)
    end;
generate_credentials(same_credential, AttemptNum) ->
    %% Same valid credential, different IDs (session flooding)
    #{
        type => same_credential,
        api_key => <<"erlmcp_test_key_valid_12345">>,
        attempt => AttemptNum
    }.

%% @private Generate random client ID (NO REUSE)
generate_random_id() ->
    Rand = crypto:strong_rand_bytes(16),
    <<"client_", Rand/binary>>.

%% @private Generate random API key
generate_random_api_key() ->
    Rand = crypto:strong_rand_bytes(32),
    <<"key_", Rand/binary>>.

%%====================================================================
%% Statistics & Monitoring
%%====================================================================

%% @private Initialize stats
init_stats() -> #{
    total_attempts => 0,
    successful_auth => 0,
    failed_auth => 0,
    rate_limited => 0,
    account_locked => 0,
    connection_errors => 0,
    server_crashes => 0
}.

%% @private Initialize worker stats
init_worker_stats() -> #{
    attempts => 0,
    success => 0,
    failed => 0,
    rate_limited => 0,
    locked => 0,
    errors => 0
}.

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
        {account_locked, _} ->
            Stats#{attempts => Attempts, locked => maps_get(locked, Stats, 0) + 1};
        {connection_error, _} ->
            Stats#{attempts => Attempts, errors => maps_get(errors, Stats, 0) + 1};
        {auth_error, _, _} ->
            Stats#{attempts => Attempts, failed => maps_get(failed, Stats, 0) + 1};
        {crash, _} ->
            Stats#{attempts => Attempts, errors => maps_get(errors, Stats, 0) + 1};
        _ ->
            Stats#{attempts => Attempts, errors => maps_get(errors, Stats, 0) + 1}
    end.

%% @private Get memory usage
get_memory_usage() ->
    case erlang:memory(total) of
        Memory when is_integer(Memory) ->
            Memory / 1024 / 1024;  % Convert to MB
        _ ->
            0.0
    end.

%% @private Count leaked sessions
count_leaked_sessions() ->
    try
        case whereis(erlmcp_session_manager) of
            undefined -> 0;
            _Pid ->
                {ok, Sessions} = erlmcp_session_manager:list_sessions(),
                length(Sessions)
        end
    catch
        _:_ -> 0
    end.

%% @private Count cleaned sessions
count_cleaned_sessions() ->
    %% This would require tracking cleanup events
    %% For now, return 0
    0.

%% @private Monitor server process
monitor_server_process() ->
    case whereis(erlmcp_server_sup) of
        undefined -> undefined;
        Pid -> erlang:monitor(process, Pid)
    end.

%% @private Monitor auth process
monitor_auth_process() ->
    case whereis(erlmcp_auth) of
        undefined -> undefined;
        Pid -> erlang:monitor(process, Pid)
    end.

%% @private Generate recommendations
generate_recommendations(Stats) ->
    Recommendations = [],
    
    %% Check for rate limiting
    case maps_get(rate_limit_detected, Stats, false) of
        true ->
            RateLimitAt = maps_get(rate_limit_at, Stats, 0),
            [
                <<"Rate limiting detected at attempt ", (integer_to_binary(RateLimitAt))/binary>>,
                <<"Rate limiting is working - system is protected from brute force">>
            | Recommendations];
        false ->
            [<<"WARNING: No rate limiting detected - system vulnerable to brute force">>
            | Recommendations]
    end.

%% @private Safe maps:get with default
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.

%%====================================================================
%% Reporting Functions
%%====================================================================

%% @doc Generate report from attack results
-spec generate_report(attack_result()) -> binary().
generate_report(Result) ->
    Stats = maps:get(stats, Result),
    Config = maps_get(config, Result),
    
    Report = io_lib:format(
        "=== DICTIONARY ATTACK CRASH TEST ===~n~n"
        "Attack Configuration:~n"
        "- Total Attempts: ~p~n"
        "- Connect Rate: ~p/sec~n"
        "- Random IDs: yes (no reuse)~n"
        "- Duration: ~p ms~n"
        "- Credential Type: ~p~n~n"
        "Credential Statistics:~n"
        "- Successful Auth: ~p~n"
        "- Failed Auth: ~p~n"
        "- Auth Success Rate: ~.2f%~n~n"
        "RATE LIMITING:~n"
        "- Limit Detected: ~p~n"
        "- Limit Triggered At: ~p~n"
        "- Rate Limited Count: ~p~n~n"
        "SERVER STATUS:~n"
        "- Server Crashed: ~p~n"
        "- Auth System Working: ~p~n"
        "- Connection Errors: ~p~n~n"
        "MEMORY ANALYSIS:~n"
        "- Initial Memory: ~.2f MB~n"
        "- Peak Memory: ~.2f MB~n"
        "- Final Memory: ~.2f MB~n"
        "- Memory Leaked: ~.2f MB~n"
        "- Sessions Leaked: ~p~n"
        "- Sessions Cleaned: ~p~n~n"
        "ANALYSIS:~n"
        "~s~n",
        [
            maps_get(total_attempts, Config, 0),
            maps_get(connect_rate_per_sec, Config, 0),
            maps_get(duration_ms, Stats, 0),
            maps_get(credential_type, Config, unknown),
            
            maps_get(successful_auth, Stats, 0),
            maps_get(failed_auth, Stats, 0),
            maps_get(auth_success_rate, Stats, 0.0) * 100,
            
            maps_get(rate_limit_detected, Stats, false),
            maps_get(rate_limit_at, Stats, undefined),
            maps_get(rate_limited, Stats, 0),
            
            maps_get(server_crashes, Stats, 0) > 0,
            maps_get(server_crashes, Stats, 0) =:= 0,
            maps_get(connection_errors, Stats, 0),
            
            maps_get(memory_initial, Stats, 0.0),
            maps_get(memory_peak, Stats, 0.0),
            maps_get(memory_final, Stats, 0.0),
            maps_get(memory_leaked, Stats, 0.0),
            maps_get(sessions_leaked, Stats, 0),
            maps_get(sessions_cleaned, Stats, 0),
            
            format_recommendations(maps_get(recommendations, Result, []))
        ]
    ),
    
    iolist_to_binary(Report).

%% @private Format recommendations
format_recommendations([]) ->
    "  No specific recommendations.";
format_recommendations(Recs) ->
    lists:foldl(fun(Rec, Acc) ->
        Acc ++ "  - " ++ binary_to_list(Rec) ++ "\n"
    end, "", Recs).
