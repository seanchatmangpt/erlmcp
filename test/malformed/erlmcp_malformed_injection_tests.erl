%%%-------------------------------------------------------------------
%%% @doc Malformed Data Injection Stress Test Suite
%%% @purpose Find parser vulnerabilities, crash triggers, and DoS vectors
%%% @author erlang-performance agent
%%% @version 1.0.0
%%%-------------------------------------------------------------------
-module(erlmcp_malformed_injection_tests).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Suite Entry Point
%%%===================================================================

%% Run all malformed injection tests
run_all_tests() ->
    io:format("~n=== MALFORMED DATA INJECTION CRASH TEST ===~n~n"),
    
    Port = 10011,
    case start_test_server(Port) of
        {ok, Pid} ->
            io:format("Test server started on port ~p~n", [Port]),
            timer:sleep(1000),
            
            Results = run_all_categories(Port),
            
            stop_test_server(Pid),
            
            report_results(Results),
            Results;
        {error, Reason} ->
            io:format("Failed to start server: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Run all payload categories
run_all_categories(Port) ->
    Categories = [
        {invalid_utf8, fun test_invalid_utf8/1},
        {truncated_json, fun test_truncated_json/1},
        {invalid_values, fun test_invalid_values/1},
        {boundary_violations, fun test_boundary_violations/1},
        {protocol_violations, fun test_protocol_violations/1},
        {pathological_cases, fun test_pathological_cases/1}
    ],
    
    lists:map(fun({Category, TestFun}) ->
        io:format("~n--- Testing ~p ---~n", [Category]),
        
        Result = try
            TestFun(Port)
        catch
            Type:Error:Stacktrace ->
                io:format("Category ~p CRASHED: ~p:~p~nStack: ~p~n", 
                          [Category, Type, Error, Stacktrace]),
                #{category => Category, 
                  status => category_crash,
                  error => {Type, Error},
                  stacktrace => Stacktrace}
        end,
        
        {Category, Result}
    end, Categories).

%%%===================================================================
%%% Test Server Management
%%%===================================================================

start_test_server(Port) ->
    case gen_tcp:listen(Port, [
        binary,
        {active, false},
        {packet, 0},
        {reuseaddr, true}
    ]) of
        {ok, ListenSocket} ->
            Pid = spawn_link(fun() -> server_loop(ListenSocket) end),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop_test_server(Pid) ->
    exit(Pid, shutdown).

server_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket, 1000) of
        {ok, Socket} ->
            spawn(fun() -> handle_connection(Socket) end),
            server_loop(ListenSocket);
        {error, timeout} ->
            server_loop(ListenSocket);
        {error, closed} ->
            ok
    end.

handle_connection(Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} ->
            Response = try
                case jsx:is_json(Data) of
                    true ->
                        jsx:encode(#{status => ok});
                    false ->
                        jsx:encode(#{error => invalid_json})
                end
            catch
                _:_ ->
                    jsx:encode(#{error => parse_error})
            end,
            gen_tcp:send(Socket, Response),
            gen_tcp:close(Socket);
        {error, _} ->
            gen_tcp:close(Socket)
    end.

%% Connect to test server
connect(Port) ->
    case gen_tcp:connect("localhost", Port, [
        binary, 
        {active, false},
        {packet, 0}
    ], 5000) of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} -> {error, Reason}
    end.

%% Send payload and get response
send_payload(Socket, Payload) ->
    case gen_tcp:send(Socket, Payload) of
        ok ->
            case gen_tcp:recv(Socket, 0, 1000) of
                {ok, Data} -> {ok, Data};
                {error, timeout} -> timeout;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

%% Check if server is still alive
is_server_alive(Port) ->
    case connect(Port) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            true;
        _ ->
            false
    end.

%%%===================================================================
%%% Category 1: Invalid UTF-8
%%%===================================================================

test_invalid_utf8(Port) ->
    Payloads = [
        % Null bytes
        <<0>>,                           % Single null byte
        <<0, 0, 0>>,                     % Multiple null bytes
        
        % Incomplete sequences
        <<16#C0>>,                       % Incomplete 2-byte
        <<16#E0>>,                       % Incomplete 3-byte
        <<16#F0>>,                       % Incomplete 4-byte
        <<16#C0, 16#80>>,                % Partial 2-byte
        <<16#E0, 16#80>>,                % Partial 3-byte
        <<16#F0, 16#80>>,                % Partial 4-byte
        
        % Overlong encoding
        <<16#C0, 16#80>>,                % Overlong NULL
        <<16#C1, 16#80>>,                % Overlong space
        <<16#E0, 16#80, 16#80>>,         % Overlong 3-byte
        <<16#F0, 16#80, 16#80, 16#80>>,  % Overlong 4-byte
        
        % Invalid continuation bytes
        <<16#80>>,                       % Single continuation
        <<16#C0, 16#40>>,                % Invalid second byte
        <<16#E0, 16#40, 16#80>>,         % Invalid second byte
        <<16#F0, 16#40, 16#80, 16#80>>,  % Invalid second byte
        
        % Surrogate pairs (invalid in UTF-8)
        <<16#ED, 16#A0, 16#80>>,         % U+D800
        <<16#ED, 16#BF, 16#BF>>,         % U+DBFF
        <<16#ED, 16#B0, 16#80>>,         % U+DC00
        <<16#ED, 16#9F, 16#BF>>,         % U+DFFF
        
        % Beyond Unicode range
        <<16#F4, 16#90, 16#80, 16#80>>,  % U+110000 (beyond max)
        <<16#F4, 16#BF, 16#BF, 16#BF>>, % U+10FFFF+1
        
        % Mixed valid/invalid
        <<"{", 16#80, 16#C0, 16#E0, "}">>,
        <<"\"test", 16#ED, 16#A0, 16#80, "\"">>
    ],
    
    test_payloads(Port, Payloads).

%%%===================================================================
%%% Category 2: Truncated JSON
%%%===================================================================

test_truncated_json(Port) ->
    Valid = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{}}">>,
    
    Payloads = [
        % Truncate at various positions
        binary:part(Valid, {0, byte_size(Valid) - 1}),
        binary:part(Valid, {0, byte_size(Valid) - 5}),
        binary:part(Valid, {0, byte_size(Valid) - 10}),
        binary:part(Valid, {0, 1}),
        binary:part(Valid, {0, 2}),
        
        % Cut off mid-string
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"te">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{}">>,
        
        % Cut off mid-number
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{}}\"">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":123">>,
        
        % Cut off mid-escape
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\u">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\u00">>,
        
        % Missing brackets
        <<"{\"jsonrpc\":\"2.0\",\"id\":1}">>,
        <<"\"id\":1,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{}">>,
        
        % Empty payloads
        <<"{}">>,
        <<"{ }">>,
        <<"[]">>,
        <<"[ ]">>,
        <<"\"\"">>,
        
        % Only whitespace
        <<" ">>,
        <<"\n">>,
        <<"\t">>,
        <<"  \n\t  ">>
    ],
    
    test_payloads(Port, Payloads).

%%%===================================================================
%%% Category 3: Invalid Values
%%%===================================================================

test_invalid_values(Port) ->
    Payloads = [
        % JSON-RPC with invalid numeric values
        <<"{\"jsonrpc\":\"2.0\",\"id\":NaN,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":Infinity,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":-Infinity,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":-0,\"method\":\"test\",\"params\":{}}">>,
        
        % Invalid string escapes
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\x00\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\xZZ\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\uZZZZ\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\uD800\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\uDC00\",\"params\":{}}">>,
        
        % Control characters in strings
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\n\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\r\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\t\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\x00\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\x01\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\\x1F\",\"params\":{}}">>,
        
        % Invalid types in fields
        <<"{\"jsonrpc\":2.0,\"id\":1,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":true,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":{},\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":[],\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":[]}">>,
        
        % Duplicate keys
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"id\":2,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"method\":\"fail\",\"params\":{}}">>,
        <<"{\"params\":{},\"params\":[],\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
        
        % Numbers with leading zeros (technically invalid)
        <<"{\"jsonrpc\":\"2.0\",\"id\":01,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":001,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":-01,\"method\":\"test\",\"params\":{}}">>,
        
        % Invalid numbers
        <<"{\"jsonrpc\":\"2.0\",\"id\":1.2.3,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1e,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1e+,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1e-,\"method\":\"test\",\"params\":{}}">>
    ],
    
    test_payloads(Port, Payloads).

%%%===================================================================
%%% Category 4: Boundary Violations
%%%===================================================================

test_boundary_violations(Port) ->
    Payloads = [
        % Maximum integers
        <<"{\"jsonrpc\":\"2.0\",\"id\":9223372036854775807,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":-9223372036854775808,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":18446744073709551615,\"method\":\"test\",\"params\":{}}">>,
        
        % Negative lengths in arrays/objects
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"length\":-1}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"size\":-999}}">>,
        
        % Empty structures everywhere
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":[]}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"\":{}}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"a\":[]}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"a\":{}}}">>,
        
        % Deeply nested empty structures
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"a\":{\"b\":{\"c\":{}}}}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":[[[]]]}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"a\":[{\"b\":[]}]}}">>,
        
        % Arrays with negative indices
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":[-1]}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":[-999]}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":[1,2,-3,4]}">>,
        
        % Zero values
        <<"{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"value\":0}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":[0]}">>,
        
        % Extremely large numbers (overflow potential)
        <<"{\"jsonrpc\":\"2.0\",\"id\":9999999999999999999999999999999999999999,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":-9999999999999999999999999999999999999999,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"big\":9999999999999999999999999999999999999999}}">>
    ],
    
    test_payloads(Port, Payloads).

%%%===================================================================
%%% Category 5: Protocol Violations
%%%===================================================================

test_protocol_violations(Port) ->
    Payloads = [
        % Wrong JSON-RPC version
        <<"{\"jsonrpc\":\"1.0\",\"id\":1,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"3.0\",\"id\":1,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2\",\"id\":1,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":null,\"id\":1,\"method\":\"test\",\"params\":{}}">>,
        
        % Missing required fields
        <<"{\"id\":1,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1}">>,
        <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\"}">>,
        <<"{}">>,
        
        % Invalid field types
        <<"{\"jsonrpc\":null,\"id\":1,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":null,\"params\":{}}">>,
        
        % Extra unexpected fields
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{},\"extra\":\"field\"}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{},\"unknown\":123}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{},\"_private\":\"value\"}">>,
        
        % Wrong message types
        <<"{\"jsonrpc\":\"2.0\",\"result\":{},\"id\":1}">>,
        <<"{\"jsonrpc\":\"2.0\",\"error\":{},\"id\":1}">>,
        <<"{\"jsonrpc\":\"2.0\",\"result\":{},\"error\":{},\"id\":1}">>,
        
        % Batch protocol violations
        <<"[{}]">>,
        <<"[{\"jsonrpc\":\"2.0\",\"method\":\"test\"}]">>,
        <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"},{\"jsonrpc\":\"2.0\",\"method\":\"test\"}]">>,
        <<"[]">>,
        
        % Invalid ID formats
        <<"{\"jsonrpc\":\"2.0\",\"id\":1.5,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":true,\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":{},\"method\":\"test\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":[],\"method\":\"test\",\"params\":{}}">>
    ],
    
    test_payloads(Port, Payloads).

%%%===================================================================
%%% Category 6: Pathological Cases
%%%===================================================================

test_pathological_cases(Port) ->
    Payloads = [
        % Extremely long strings (1KB)
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"",(lists:duplicate(1024, $a))/binary, "\",\"params\":{}}">>,
        
        % Deeply nested structures (100 levels)
        build_deep_nested(100, object),
        build_deep_nested(100, array),
        
        % Wide arrays
        build_wide_array(100),
        
        % Wide objects
        build_wide_object(100),
        
        % Unicode edge cases
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"\\u0000\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"\\uD834\\uDD1E\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"\\uFFFF\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"\\uFFFE\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"\\uFEFF\",\"params\":{}}">>,
        
        % Emoji and complex Unicode
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"😀\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"🚀🌟\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"Test©®™\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"тест\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"测试\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"परीक्षण\",\"params\":{}}">>,
        
        % Special characters in method name
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test/with/slashes\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test.with.dots\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test:with:colons\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test-with-dashes\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test_with_underscores\",\"params\":{}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test with spaces\",\"params\":{}}">>,
        
        % Params with null values
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"value\":null}}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":[null]}">>,
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":null}">>,
        
        % Mixed line endings
        <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"value\":\"line1\\nline2\\rline3\\r\\nline4\"}}">>,
        
        % Very long escape sequences
        build_long_escapes(50)
    ],
    
    test_payloads(Port, Payloads).

%% Helper: Build deeply nested structure
build_deep_nested(Levels, Type) when Levels > 0 ->
    Inner = build_deep_nested(Levels - 1, Type),
    case Type of
        object -> <<"{\"a\":", Inner/binary, "}">>;
        array -> <<"[", Inner/binary, "]">>
    end;
build_deep_nested(0, _) ->
    <<"1">>.

%% Helper: Build wide array
build_wide_array(Count) ->
    Elements = lists:map(fun(_) -> <<"1,">> end, lists:seq(1, Count)),
    <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":[",
      (iolist_to_binary(Elements))/binary, "]}">>.

%% Helper: Build wide object
build_wide_object(Count) ->
    Pairs = lists:map(fun(N) ->
        Key = list_to_binary("key" ++ integer_to_list(N)),
        <<Key/binary, ":1,">>
    end, lists:seq(1, Count)),
    <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{",
      (iolist_to_binary(Pairs))/binary, "\"end\":1}}">>.

%% Helper: Build long escape sequences
build_long_escapes(Count) ->
    Escapes = lists:duplicate(Count, <<"\\\\n">>),
    <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"str\":\"",
      (iolist_to_binary(Escapes))/binary, "\"}}">>.

%%%===================================================================
%%% Generic Payload Testing
%%%===================================================================

test_payloads(Port, Payloads) ->
    TestFun = fun(Payload, Acc) ->
        Index = maps:get(count, Acc, 0) + 1,
        
        case connect(Port) of
            {ok, Socket} ->
                Result = test_single_payload(Socket, Payload, Index),
                gen_tcp:close(Socket),
                
                ServerAlive = is_server_alive(Port),
                
                Acc#{
                    count => Index,
                    results => [Result | maps:get(results, Acc, [])],
                    crashes => maps:get(crashes, Acc, 0) + is_crash(Result, ServerAlive),
                    errors => maps:get(errors, Acc, 0) + is_error(Result),
                    accepted => maps:get(accepted, Acc, 0) + is_accepted(Result)
                };
            {error, Reason} ->
                io:format("  [~p] Connection failed: ~p~n", [Index, Reason]),
                Acc#{
                    count => Index,
                    connection_failures => maps:get(connection_failures, Acc, 0) + 1
                }
        end
    end,
    
    lists:foldl(TestFun, #{}, Payloads).

%% Test a single payload
test_single_payload(Socket, Payload, Index) ->
    Hex = binary_to_hex(Payload),
    Desc = describe_payload(Payload),
    
    try
        Result = send_payload(Socket, Payload),
        
        case Result of
            {ok, Response} ->
                io:format("  [~p] ACCEPTED~n    Payload: ~s~n    Response: ~p~n", 
                          [Index, Desc, Response]),
                #{payload => Desc, hex => Hex, response => Response, status => accepted};
            timeout ->
                io:format("  [~p] TIMEOUT~n    Payload: ~s~n", [Index, Desc]),
                #{payload => Desc, hex => Hex, response => timeout, status => timeout};
            {error, Reason} ->
                io:format("  [~p] ERROR: ~p~n    Payload: ~s~n", [Index, Reason, Desc]),
                #{payload => Desc, hex => Hex, error => Reason, status => error}
        end
    catch
        Type:Error:Stacktrace ->
            io:format("  [~p] EXCEPTION: ~p:~p~n    Payload: ~s~n    Stack: ~p~n", 
                      [Index, Type, Error, Desc, Stacktrace]),
            #{payload => Desc, hex => Hex, error => {Type, Error}, 
              stacktrace => Stacktrace, status => exception}
    end.

%% Describe payload for logging
describe_payload(Payload) when byte_size(Payload) > 100 ->
    <<First:100/binary, _/binary>> = Payload,
    lists:flatten(io_lib:format("~s... (~p bytes)", [First, byte_size(Payload)]));
describe_payload(Payload) ->
    binary_to_list(Payload).

%% Convert binary to hex string
binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [B]) || <<B>> <= Bin]).

%% Determine if result is a crash
is_crash(Result, ServerAlive) ->
    case {Result, ServerAlive} of
        {#{status := exception}, false} -> 1;
        {#{status := error}, false} -> 1;
        {#{status := timeout}, false} -> 1;
        _ -> 0
    end.

%% Determine if result is an error
is_error(#{status := Status}) when Status =:= error; Status =:= exception; Status =:= timeout ->
    1;
is_error(_) ->
    0.

%% Determine if result was accepted
is_accepted(#{status := accepted}) ->
    1;
is_accepted(_) ->
    0.

%%%===================================================================
%%% Results Reporting
%%%===================================================================

report_results(Results) ->
    io:format("~n~n=== MALFORMED DATA INJECTION TEST RESULTS ===~n~n"),
    
    lists:foreach(fun({Category, Result}) ->
        report_category_results(Category, Result)
    end, Results),
    
    report_summary(Results),
    report_crash_details(Results).

report_category_results(Category, Result) when is_map(Result) ->
    Count = maps:get(count, Result, 0),
    Crashes = maps:get(crashes, Result, 0),
    Errors = maps:get(errors, Result, 0),
    Accepted = maps:get(accepted, Result, 0),
    
    io:format("~p:~n", [Category]),
    io:format("  Total Tested: ~p~n", [Count]),
    io:format("  Crashes: ~p~n", [Crashes]),
    io:format("  Errors: ~p~n", [Errors]),
    io:format("  Accepted: ~p~n", [Accepted]),
    
    case Crashes of
        0 -> io:format("  Status: SAFE ✓~n");
        _ -> io:format("  Status: VULNERABLE ⚠~n")
    end,
    
    io:format("~n");
report_category_results(Category, #{status := category_crash, error := Error}) ->
    io:format("~p: CRASHED during testing~n", [Category]),
    io:format("  Error: ~p~n", [Error]),
    io:format("  Status: VULNERABLE ⚠~n~n").

report_summary(Results) ->
    io:format("=== SUMMARY ===~n~n"),
    
    {Total, TotalCrashes, TotalErrors, TotalAccepted} = 
        lists:foldl(fun({_Category, Result}, {T, C, E, A}) ->
            case is_map(Result) of
                true ->
                    {T + maps:get(count, Result, 0),
                     C + maps:get(crashes, Result, 0),
                     E + maps:get(errors, Result, 0),
                     A + maps:get(accepted, Result, 0)};
                false ->
                    {T, C, E, A}
            end
        end, {0, 0, 0, 0}, Results),
    
    io:format("Payload Categories Tested: ~p~n", length(Results)),
    io:format("Total Payloads Tested: ~p~n", [Total]),
    io:format("Total Crashes: ~p~n", [TotalCrashes]),
    io:format("Total Errors: ~p~n", [TotalErrors]),
    io:format("Total Accepted: ~p~n", [TotalAccepted]),
    
    AcceptRate = case Total of
        0 -> 0;
        _ -> (TotalAccepted * 100) div Total
    end,
    io:format("Acceptance Rate: ~p%~n", [AcceptRate]),
    
    case TotalCrashes of
        0 -> io:format("~n✓ NO CRASHES - Parser appears robust~n");
        _ -> io:format("~n⚠ VULNERABILITIES DETECTED - See crash details below~n")
    end.

report_crash_details(Results) ->
    Crashes = lists:filtermap(fun({_Category, Result}) ->
        case Result of
            #{results := ResultsList} when is_list(ResultsList) ->
                CrashResults = [R || R <- ResultsList, is_crash_result(R)],
                case CrashResults of
                    [] -> false;
                    _ -> {true, {_Category, CrashResults}}
                end;
            _ ->
                false
        end
    end, Results),
    
    case Crashes of
        [] ->
            ok;
        _ ->
            io:format("~n~n=== CRASH TRIGGERS ===~n~n"),
            
            lists:foreach(fun({Category, CrashResults}) ->
                io:format("~p crashes:~n", [Category]),
                
                lists:foreach(fun(#{payload := Desc, hex := Hex, response := Response}) ->
                    io:format("~n  Payload: ~s~n", [Desc]),
                    io:format("  Hex: ~s~n", [Hex]),
                    io:format("  Response: ~p~n", [Response])
                end, CrashResults),
                
                io:format("~n")
            end, Crashes),
            
            io:format("~n=== VULNERABILITIES ===~n~n"),
            
            TotalCrashes = lists:foldl(fun({_Category, CrashResults}, Acc) ->
                Acc + length(CrashResults)
            end, 0, Crashes),
            
            io:format("Critical Crashes: ~p~n", [TotalCrashes]),
            io:format("Memory Corruption: Unknown (requires instrumentation)~n"),
            io:format("DoS Vectors: ~p~n", [length(Crashes)]),
            io:format("Security Issues: See individual crashes above~n")
    end.

is_crash_result(#{status := Status}) when Status =:= exception; 
                                          Status =:= error; 
                                          Status =:= timeout ->
    true;
is_crash_result(_) ->
    false.
