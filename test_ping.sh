#!/bin/bash
# Test script to verify ping implementation

set -e

echo "=== Testing Ping Implementation ==="
echo ""

# Compile the message handler to ensure it compiles
echo "1. Compiling erlmcp_message_handler..."
erlc -I apps/erlmcp_core/include -o /tmp apps/erlmcp_core/src/erlmcp_message_handler.erl
echo "   ✓ Message handler compiled successfully"
echo ""

# Test that the ping method is routed correctly
echo "2. Testing ping routing..."
cat > /tmp/ping_test.erl <<'EOF'
-module(ping_test).
-include_lib("erlmcp.hrl").
-export([test/0]).

test() ->
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
    {reply, ResponseJson, _NewState} =
        erlmcp_message_handler:process_message(<<"test">>, RequestJson, State),

    Response = jsx:decode(ResponseJson, [return_maps]),

    %% Verify response structure
    #{
        <<"jsonrpc">> := <<"2.0">>,
        <<"id">> := 1,
        <<"result">> := Result
    } = Response,

    case map_size(Result) of
        0 -> io:format("   ✓ Ping returns empty object~n");
        _ -> io:format("   ✗ Result not empty: ~p~n", [Result]), halt(1)
    end,

    %% Test that error field is absent
    case maps:is_key(<<"error">>, Response) of
        false -> io:format("   ✓ No error field in response~n");
        true -> io:format("   ✗ Error field present in response~n"), halt(1)
    end,

    %% Test ping in all phases
    Phases = [initialization, initialized, disconnected, closed],
    lists:foreach(fun(Phase) ->
        PhaseState = State#mcp_server_state{phase = Phase},
        {reply, PhaseResponseJson, _} =
            erlmcp_message_handler:process_message(<<"test">>, RequestJson, PhaseState),
        PhaseResponse = jsx:decode(PhaseResponseJson, [return_maps]),
        case maps:get(<<"result">>, PhaseResponse) of
            #{} -> ok;
            _ -> halt(1)
        end
    end, Phases),
    io:format("   ✓ Ping works in all initialization phases~n"),

    %% Test with different request IDs
    RequestIds = [1, 42, 999, <<"uuid-1234">>],
    lists:foreach(fun(RequestId) ->
        IdRequest = Request#{<<"id">> => RequestId},
        IdRequestJson = jsx:encode(IdRequest),
        {reply, IdResponseJson, _} =
            erlmcp_message_handler:process_message(<<"test">>, IdRequestJson, State),
        IdResponse = jsx:decode(IdResponseJson, [return_maps]),
        case maps:get(<<"id">>, IdResponse) of
            RequestId -> ok;
            _ -> halt(1)
        end
    end, RequestIds),
    io:format("   ✓ Request ID echoed correctly~n"),

    io:format("~n=== All ping tests passed! ===~n"),
    halt(0).
EOF

# Compile and run the test
erlc -I apps/erlmcp_core/include -o /tmp /tmp/ping_test.erl
erl -noshell -pa /tmp -pa apps/erlmcp_core/ebin -pa _build/default/lib/jsx/ebin -s ping_test test

echo ""
echo "=== Ping implementation verified successfully! ==="
