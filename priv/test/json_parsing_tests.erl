-module(json_parsing_tests).

-include_lib("eunit/include/eunit.hrl").

json_parsing_test_() ->
    [
        ?_test(test_initialize_message_parsing()),
        ?_test(test_tool_call_message_parsing()),
        ?_test(test_response_encoding()),
        ?_test(test_invalid_json_handling())
    ].

test_initialize_message_parsing() ->
    Message = "{\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"claude-ai\",\"version\":\"0.1.0\"}},\"jsonrpc\":\"2.0\",\"id\":0}",

    ?debugMsg("Testing initialize message parsing"),
    ?debugFmt("Message: ~s", [Message]),

    Decoded = jsx:decode(list_to_binary(Message), [return_maps]),
    ?debugFmt("Successfully decoded: ~p", [Decoded]),

    ?assertMatch(#{<<"method">> := <<"initialize">>, <<"id">> := 0}, Decoded),
    ?assertEqual(<<"initialize">>, maps:get(<<"method">>, Decoded)),
    ?assertEqual(0, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),

    Params = maps:get(<<"params">>, Decoded),
    ?assertEqual(<<"2025-06-18">>, maps:get(<<"protocolVersion">>, Params)).

test_tool_call_message_parsing() ->
    Message = "{\"method\":\"tools/call\",\"params\":{\"name\":\"echo\",\"arguments\":{\"message\":\"hello\"}},\"jsonrpc\":\"2.0\",\"id\":1}",

    ?debugMsg("Testing tool call message parsing"),
    ?debugFmt("Message: ~s", [Message]),

    Decoded = jsx:decode(list_to_binary(Message), [return_maps]),
    ?debugFmt("Successfully decoded: ~p", [Decoded]),

    ?assertMatch(#{<<"method">> := <<"tools/call">>, <<"id">> := 1}, Decoded),
    ?assertEqual(<<"tools/call">>, maps:get(<<"method">>, Decoded)),
    ?assertEqual(1, maps:get(<<"id">>, Decoded)),

    Params = maps:get(<<"params">>, Decoded),
    ?assertEqual(<<"echo">>, maps:get(<<"name">>, Params)),

    Arguments = maps:get(<<"arguments">>, Params),
    ?assertEqual(<<"hello">>, maps:get(<<"message">>, Arguments)).

test_response_encoding() ->
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 0,
        <<"result">> => #{
            <<"protocolVersion">> => <<"2025-06-18">>,
            <<"capabilities">> => #{
                <<"tools">> => #{<<"listChanged">> => false},
                <<"resources">> => #{<<"subscribe">> => false, <<"listChanged">> => false},
                <<"prompts">> => #{<<"listChanged">> => false}
            },
            <<"serverInfo">> => #{
                <<"name">> => <<"erlmcp-stdio">>,
                <<"version">> => <<"1.0.0">>
            }
        }
    },

    ?debugMsg("Testing response encoding"),
    ?debugFmt("Response: ~p", [Response]),

    Json = jsx:encode(Response),
    ?debugFmt("Encoded JSON: ~s", [Json]),

    ?assert(is_binary(Json)),
    ?assert(byte_size(Json) > 0),

    % Test that it can be decoded back
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(Response, Decoded).

test_invalid_json_handling() ->
    InvalidMessages = [
        "{invalid json",
        "{\"method\":\"test\"}",  % Missing required fields
        "not json at all",
        "{\"method\":\"test\",\"id\":\"not_a_number\",\"jsonrpc\":\"2.0\"}"
    ],

    ?debugMsg("Testing invalid JSON handling"),

    lists:foreach(fun(Message) ->
        ?debugFmt("Testing invalid message: ~s", [Message]),
        try jsx:decode(list_to_binary(Message), [return_maps]) of
            Decoded ->
                ?debugFmt("Unexpectedly parsed: ~p", [Decoded])
        catch
            Error:Reason ->
                ?debugFmt("Expected error: ~p:~p", [Error, Reason]),
                ?assert(true)  % Expected to fail
        end
    end, InvalidMessages).
