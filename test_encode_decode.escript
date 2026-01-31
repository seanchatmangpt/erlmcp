#!/usr/bin/env escript
main(_) ->
    code:add_patha("_build/default/lib/erlmcp_core/ebin"),
    code:add_patha("_build/default/lib/jsx/ebin"),
    code:add_patha("_build/default/lib/jesse/ebin"),
    code:add_patha("_build/default/lib/gproc/ebin"),
    code:add_patha("_build/default/lib/erlmcp_message_size/ebin"),
    
    Id = <<128>>,
    Result = 0,
    
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    io:format("Encoded: ~p~n", [Encoded]),
    
    case erlmcp_json_rpc:decode_message(Encoded) of
        {ok, Decoded} ->
            io:format("Decoded: ~p~n", [Decoded]),
            io:format("Is tuple: ~p~n", [is_tuple(Decoded)]),
            io:format("Tuple size: ~p~n", [tuple_size(Decoded)]),
            io:format("Element 1: ~p~n", [element(1, Decoded)]),
            io:format("Element 2: ~p~n", [element(2, Decoded)]),
            io:format("Element 3: ~p~n", [element(3, Decoded)]),
            io:format("Element 4: ~p~n", [element(4, Decoded)]);
        Error ->
            io:format("Error: ~p~n", [Error])
    end.
