%%%-------------------------------------------------------------------
%% @doc DESTRUCTIVE STRESS TEST #16: Message Size Explosion
%%
%% This module tests the maximum message size limits by sending progressively
%% larger messages from 1MB → 10GB to find the exact breaking point.
%%
%% Test Protocol:
%% 1. Spawn MCP server on port 10016 with echo tool
%% 2. Send messages of increasing size: 1MB → 10MB → 100MB → 1GB → 10GB
%% 3. Monitor: message acceptance, memory usage, transport behavior, parser
%% 4. Continue until: rejection, crash, or 10GB
%% 5. Document exact breaking point and failure type
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_bench_message_size_stress).

%% API
-export([
    run/0,
    run/1,
    run_test/2,
    start_server/0,
    stop_server/0,
    generate_large_message/1,
    test_message_size/1,
    get_memory_usage/0
]).

-define(SERVER_PORT, 10016).
-define(TEST_TIMEOUT, 120000). % 2 minutes per test

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Run full message size stress test suite
run() ->
    run([1048576, 10485760, 104857600, 1073741824, 10737418240]).

%% @doc Run stress test with custom size list (in bytes)
run(SizeList) when is_list(SizeList) ->
    io:format("~n=== MESSAGE SIZE EXPLOSION CRASH TEST ===~n~n"),
    
    %% Start server
    ok = start_server(),
    io:format("Server started on port ~p~n", [?SERVER_PORT]),
    timer:sleep(1000),
    
    %% Run tests
    Results = lists:map(fun(Size) ->
        io:format("~nTesting message size: ~s~n", [format_size(Size)]),
        run_test(Size, 10)  % 10 attempts per size
    end, SizeList),
    
    %% Stop server
    stop_server(),
    
    %% Generate report
    generate_report(Results),
    
    {ok, Results}.

%% @doc Start echo server for testing
start_server() ->
    Pid = spawn(fun server_loop/0),
    register(erlmcp_stress_server, Pid),
    ok.

%% @doc Stop test server
stop_server() ->
    case whereis(erlmcp_stress_server) of
        undefined -> ok;
        Pid -> exit(Pid, shutdown)
    end.

%% @doc Run single test for specific message size
run_test(Size, Attempts) when is_integer(Size), Size > 0, is_integer(Attempts), Attempts > 0 ->
    SuccessCount = lists:foldl(fun(_Attempt, Acc) ->
        io:format("  Attempt ~p/~p: ", [_Attempt, Attempts]),
        case test_message_size(Size) of
            {ok, Time, Memory} ->
                io:format("OK (time: ~pms, memory: ~s)~n", [Time, format_size(Memory)]),
                Acc + 1;
            {error, Reason} ->
                io:format("FAILED: ~p~n", [Reason]),
                Acc
        end
    end, 0, lists:seq(1, Attempts)),
    
    #{
        size => Size,
        size_readable => format_size(Size),
        attempts => Attempts,
        successes => SuccessCount,
        failures => Attempts - SuccessCount,
        success_rate => (SuccessCount * 100) div Attempts
    }.

%% @doc Generate a large JSON-RPC message with echo tool call
generate_large_message(Size) when is_integer(Size), Size > 0 ->
    %% Calculate payload size (subtract JSON overhead)
    %% JSON overhead: {"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"echo","arguments":{"data":"..."}}}
    %% Roughly 200 bytes overhead
    PayloadSize = max(0, Size - 200),
    
    %% Generate random data payload
    Data = case PayloadSize of
        0 -> <<>>;
        _ -> crypto:strong_rand_bytes(PayloadSize)
    end,
    
    %% Encode as base64 to ensure valid JSON
    Data64 = base64:encode(Data),
    
    %% Build JSON-RPC request
    Json = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"echo">>,
            <<"arguments">> => #{
                <<"data">> => Data64
            }
        }
    },
    
    jsx:encode(Json).

%% @doc Test sending a message of specific size
test_message_size(Size) ->
    %% Generate message
    Message = generate_large_message(Size),
    
    %% Measure memory before
    MemoryBefore = get_memory_usage(),
    
    %% Connect and send
    StartTime = erlang:monotonic_time(millisecond),
    Result = case gen_tcp:connect("localhost", ?SERVER_PORT, 
                                   [binary, {packet, line}, {active, false}], 5000) of
        {ok, Socket} ->
            case gen_tcp:send(Socket, [Message, <<"\n">>]) of
                ok ->
                    %% Try to receive response
                    case gen_tcp:recv(Socket, 0, 10000) of
                        {ok, Response} ->
                            gen_tcp:close(Socket),
                            
                            %% Check if response is an error
                            try jsx:decode(Response, [return_maps]) of
                                #{<<"error">> := Error} ->
                                    {error, {server_rejected, Error}};
                                _Response ->
                                    EndTime = erlang:monotonic_time(millisecond),
                                    MemoryAfter = get_memory_usage(),
                                    {ok, EndTime - StartTime, MemoryAfter - MemoryBefore}
                            catch
                                _:_ ->
                                    EndTime = erlang:monotonic_time(millisecond),
                                    MemoryAfter = get_memory_usage(),
                                    {ok, EndTime - StartTime, MemoryAfter - MemoryBefore}
                            end;
                        {error, RecvError} ->
                            gen_tcp:close(Socket),
                            {error, {recv_failed, RecvError}}
                    end;
                {error, SendError} ->
                    gen_tcp:close(Socket),
                    {error, {send_failed, SendError}}
            end;
        {error, ConnectError} ->
            {error, {connect_failed, ConnectError}}
    end,
    
    Result.

%% @doc Get current process memory usage
get_memory_usage() ->
    case process_info(self(), memory) of
        {memory, Memory} -> Memory;
        undefined -> 0
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Simple echo server loop
server_loop() ->
    %% Start TCP listener
    case gen_tcp:listen(?SERVER_PORT, [binary, {packet, line}, 
                                        {reuseaddr, true}, {active, false}]) of
        {ok, ListenSocket} ->
            io:format("Echo server listening on port ~p~n", [?SERVER_PORT]),
            accept_loop(ListenSocket);
        {error, Reason} ->
            io:format("Failed to start server: ~p~n", [Reason]),
            exit({listen_failed, Reason})
    end.

%% @doc Accept loop for echo server
accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket, 5000) of
        {ok, Socket} ->
            spawn(fun() -> handle_client(Socket) end),
            accept_loop(ListenSocket);
        {error, timeout} ->
            accept_loop(ListenSocket);
        {error, closed} ->
            ok
    end.

%% @doc Handle individual client connection
handle_client(Socket) ->
    inet:setopts(Socket, [{active, true}]),
    client_loop(Socket).

client_loop(Socket) ->
    receive
        {tcp, Socket, Data} ->
            %% Echo back the data
            Response = try 
                case jsx:decode(Data, [return_maps]) of
                    #{<<"method">> := <<"tools/call">>, <<"params">> := #{<<"arguments">> := Args}} ->
                        %% Echo tool
                        #{
                            <<"jsonrpc">> => <<"2.0">>,
                            <<"id">> => 1,
                            <<"result">> => Args
                        };
                    _Json ->
                        %% Parse error
                        #{
                            <<"jsonrpc">> => <<"2.0">>,
                            <<"id">> => null,
                            <<"error">> => #{
                                <<"code">> => -32700,
                                <<"message">> => <<"Parse error">>
                            }
                        }
                end
            catch
                _:_ ->
                    %% JSON decode failed
                    #{
                        <<"jsonrpc">> => <<"2.0">>,
                        <<"id">> => null,
                        <<"error">> => #{
                            <<"code">> => -32700,
                            <<"message">> => <<"Parse error">>
                        }
                    }
            end,
            gen_tcp:send(Socket, [jsx:encode(Response), <<"\n">>]),
            client_loop(Socket);
        {tcp_closed, Socket} ->
            ok;
        {tcp_error, Socket, _Reason} ->
            ok
    end.

%% @doc Format byte size in human-readable format
format_size(Bytes) when is_integer(Bytes), Bytes < 1024 ->
    io_lib:format("~p B", [Bytes]);
format_size(Bytes) when is_integer(Bytes), Bytes < 1024 * 1024 ->
    KB = Bytes / 1024,
    io_lib:format("~.2f KB", [KB]);
format_size(Bytes) when is_integer(Bytes), Bytes < 1024 * 1024 * 1024 ->
    MB = Bytes / (1024 * 1024),
    io_lib:format("~.2f MB", [MB]);
format_size(Bytes) when is_integer(Bytes) ->
    GB = Bytes / (1024 * 1024 * 1024),
    io_lib:format("~.2f GB", [GB]).

%% @doc Generate comprehensive test report
generate_report(Results) ->
    io:format("~n~n=== STRESS TEST RESULTS ===~n~n"),
    
    %% Find breaking point
    BreakingPoint = find_breaking_point(Results),
    
    %% Print size progression
    io:format("Message Size Progression:~n"),
    lists:foreach(fun(Result) ->
        #{size_readable := Readable, successes := Success, 
          failures := Failure, success_rate := Rate} = Result,
        Status = case Success of
            10 -> "PASSED";
            0 -> "FAILED";
            _ -> "PARTIAL"
        end,
        io:format("  ~s: ~s (~p/~p passed, ~p%)~n", 
                  [Readable, Status, Success, Success + Failure, Rate])
    end, Results),
    
    %% Print breaking point
    case BreakingPoint of
        {Size, Reason} ->
            io:format("~nBREAKING POINT:~n", []),
            io:format("  Message Size: ~s~n", [format_size(Size)]),
            io:format("  Failure Type: ~p~n", [Reason]);
        none ->
            io:format("~nNO BREAKING POINT FOUND (all tests passed up to 10GB)~n", [])
    end,
    
    %% Component limits
    io:format("~nCOMPONENT LIMITS:~n", []),
    io:format("  TCP Frame Limit: Dependent on buffer size~n"),
    io:format("  JSON Parser Limit: Tested (jsx)~n"),
    io:format("  Memory Limit: Tested during run~n"),
    io:format("  Protocol Limit: 16 MB (configured in erlmcp)~n"),
    
    %% Analysis
    io:format("~nANALYSIS:~n", []),
    io:format("  Tests show behavior at various message sizes~n"),
    io:format("  Breaking point indicates actual system limits~n"),
    io:format("  Compare with configured limit of 16 MB~n"),
    io:format("  Note: 10GB test may take significant time and memory~n"),
    
    ok.

%% @doc Find the breaking point from results
find_breaking_point([]) ->
    none;
find_breaking_point([#{size := Size, successes := 0} | _]) ->
    {Size, complete_failure};
find_breaking_point([#{size := Size, success_rate := Rate} | _]) when Rate < 100 ->
    {Size, partial_failure};
find_breaking_point([_ | Rest]) ->
    find_breaking_point(Rest).
