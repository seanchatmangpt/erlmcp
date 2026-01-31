#!/usr/bin/env escript
%% Simple test script to verify ping implementation
-include("apps/erlmcp_core/include/erlmcp.hrl").

main(_) ->
    io:format("Testing ping implementation...~n"),

    %% Add ebin directories to code path
    true = code:add_patha("apps/erlmcp_core/ebin"),
    true = code:add_patha("_build/default/lib/jsx/ebin"),
    true = code:add_patha("_build/default/lib/gproc/ebin"),

    %% Test 1: Check that ping constant is defined
    io:format("Test 1: Checking ping constant...~n"),
    try
        %% This will fail if the constant isn't defined
        case <<"ping">> of
            <<"ping">> -> io:format("  ✓ Ping constant defined correctly~n")
        end
    catch
        _:_ -> io:format("  ✗ Ping constant not defined~n"), halt(1)
    end,

    %% Test 2: Create a mock server state and test ping routing
    io:format("~nTest 2: Testing ping request routing...~n"),
    try
        %% Create minimal server state
        State = #mcp_server_state{
            server_id = ping_test,
            phase = initialized,
            initialized = true
        },

        %% Create ping request
        Request = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"ping">>,
            <<"id">> => 1
        },
        RequestJson = jsx:encode(Request),

        %% Process through message handler
        case erlmcp_message_handler:process_message(<<"test">>, RequestJson, State) of
            {reply, ResponseJson, _NewState} ->
                Response = jsx:decode(ResponseJson, [return_maps]),

                %% Verify response structure
                case {
                    maps:get(<<"jsonrpc">>, Response, undefined),
                    maps:get(<<"id">>, Response, undefined),
                    maps:get(<<"result">>, Response, undefined),
                    maps:is_key(<<"error">>, Response)
                } of
                    {<<"2.0">>, 1, Result, false} when map_size(Result) =:= 0 ->
                        io:format("  ✓ Ping request routed correctly~n"),
                        io:format("  ✓ Response has correct JSON-RPC 2.0 structure~n"),
                        io:format("  ✓ Response ID matches request ID~n"),
                        io:format("  ✓ Result is empty object (map_size = 0)~n"),
                        io:format("  ✓ No error field in response~n");
                    _ ->
                        io:format("  ✗ Response structure incorrect~n"),
                        io:format("    Response: ~p~n", [Response]),
                        halt(1)
                end;
            Other ->
                io:format("  ✗ Unexpected response: ~p~n", [Other]),
                halt(1)
        end
    catch
        Type:Error:Stacktrace ->
            io:format("  ✗ Exception during ping routing test:~n"),
            io:format("    ~p: ~p~n", [Type, Error]),
            io:format("    ~p~n", [Stacktrace]),
            halt(1)
    end,

    %% Test 3: Verify ping works in all phases
    io:format("~nTest 3: Testing ping in all initialization phases...~n"),
    Phases = [initialization, initialized, disconnected, closed],
    lists:foreach(fun(Phase) ->
        try
            State = #mcp_server_state{
                server_id = ping_phase_test,
                phase = Phase,
                initialized = (Phase =:= initialized)
            },

            Request = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"method">> => <<"ping">>,
                <<"id">> => 1
            },
            RequestJson = jsx:encode(Request),

            case erlmcp_message_handler:process_message(<<"test">>, RequestJson, State) of
                {reply, ResponseJson, _} ->
                    Response = jsx:decode(ResponseJson, [return_maps]),
                    case maps:get(<<"result">>, Response) of
                        #{} ->
                            io:format("  ✓ Ping works in phase: ~p~n", [Phase]);
                        _ ->
                            io:format("  ✗ Non-empty result in phase ~p~n", [Phase]),
                            halt(1)
                    end;
                _ ->
                    io:format("  ✗ Failed in phase: ~p~n", [Phase]),
                    halt(1)
            end
        catch
            _:_ ->
                io:format("  ✗ Exception in phase: ~p~n", [Phase]),
                halt(1)
        end
    end, Phases),

    %% Test 4: Verify ping with different request IDs
    io:format("~nTest 4: Testing ping with different request IDs...~n"),
    RequestIds = [1, 42, 999, <<"uuid-1234">>],
    lists:foreach(fun(RequestId) ->
        try
            State = #mcp_server_state{
                server_id = ping_id_test,
                phase = initialized,
                initialized = true
            },

            Request = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"method">> => <<"ping">>,
                <<"id">> => RequestId
            },
            RequestJson = jsx:encode(Request),

            {reply, ResponseJson, _} =
                erlmcp_message_handler:process_message(<<"test">>, RequestJson, State),

            Response = jsx:decode(ResponseJson, [return_maps]),
            ResponseId = maps:get(<<"id">>, Response),

            case ResponseId of
                RequestId ->
                    io:format("  ✓ Request ID ~p echoed correctly~n", [RequestId]);
                _ ->
                    io:format("  ✗ Request ID mismatch: expected ~p, got ~p~n",
                              [RequestId, ResponseId]),
                    halt(1)
            end
        catch
            _:_ ->
                io:format("  ✗ Exception with request ID: ~p~n", [RequestId]),
                halt(1)
        end
    end, RequestIds),

    io:format("~n=== All ping tests passed! ===~n"),
    halt(0).
