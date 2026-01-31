-module(test_simple_otel).
-export([test/0]).

test() ->
    io:format("Compilation test successful~n"),
    io:format("Module exports: ~p~n", [erlmcp_otel:module_info(exports)]),
    
    %% Check if new functions exist
    Exports = erlmcp_otel:module_info(exports),
    HasInject = lists:member({inject_rpc_span, 3}, Exports),
    HasLink = lists:member({link_span, 2}, Exports),
    HasPropagate = lists:member({propagate_baggage, 2}, Exports),
    
    io:format("~nNew functions:~n"),
    io:format("  inject_rpc_span/3: ~p~n", [HasInject]),
    io:format("  link_span/2: ~p~n", [HasLink]),
    io:format("  propagate_baggage/2: ~p~n", [HasPropagate]),
    
    case HasInject andalso HasLink andalso HasPropagate of
        true ->
            io:format("~n[PASS] All enhanced tracing functions present~n"),
            ok;
        false ->
            io:format("~n[FAIL] Some functions missing~n"),
            error
    end.
