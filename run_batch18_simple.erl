#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/erlmcp_core/ebin
%%! -pa _build/default/lib/erlmcp_transports/ebin
%%! -pa _build/default/lib/erlmcp_observability/ebin
%%! -pa _build/test/lib/proper/ebin
%%! -pa _build/test/lib/meck/ebin
%%! -pa test/

main([]) ->
    io:format("~n=== Batch 18 Rate Limiting Test ===~n"),
    io:format("Servers: 86-90 (Ports 9086-9090)~n"),
    io:format("Rate Limit: 10 req/s per client~n"),
    io:format("~n"),

    % Start applications
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),
    application:ensure_all_started(erlmcp_observability),

    % Configure rate limiting
    application:set_env(erlmcp, rate_limiting, [
        {max_messages_per_sec, 10},
        {enabled, true}
    ]),

    % Start rate limiter
    {ok, _RateLimiterPid} = erlmcp_rate_limiter:start_link(),
    io:format("Rate limiter started.~n"),

    % Start one server for testing
    Port = 9086,
    ServerId = 'mcp_server_9086',

    Capabilities = {mcp_server_capabilities,
        {mcp_capability, false, undefined, undefined},
        {mcp_capability, true, undefined, undefined},
        {mcp_capability, false, undefined, undefined}
    },

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities),
    io:format("Server started on port ~p.~n", [Port]),

    % Add echo tool (simple handler)
    HandlerEcho = fun(Args) ->
        Message = case lists:keyfind(<<"message">>, 1, Args) of
            {_, Val} -> Val;
            false -> <<"">>
        end,
        [{<<"echo">>, Message}, {<<"timestamp">>, erlang:system_time(millisecond)}]
    end,

    erlmcp_server:add_tool(ServerPid, <<"echo">>, HandlerEcho),
    io:format("Echo tool added.~n"),

    timer:sleep(500),

    % Run burst test with 5 clients
    io:format("~nRunning burst test with 5 clients, 100 requests each...~n"),
    Parent = self(),
    ClientPids = lists:map(fun(N) ->
        spawn_link(fun() ->
            Result = run_burst_client(Port, N, 100),
            Parent ! {burst_result, self(), Result}
        end)
    end, lists:seq(1, 5)),

    % Collect results
    Results = lists:map(fun(Pid) ->
        receive
            {burst_result, Pid, Result} -> Result
        after 60000 ->
            {error, timeout}
        end
    end, ClientPids),

    % Aggregate results
    {TotalSuccess, TotalRateLimited, TotalErrors} = lists:foldl(fun
        ({Success, RateLimited, Errors}, {AS, ARL, AE}) ->
            {AS + Success, ARL + RateLimited, AE + Errors}
    end, {0, 0, 0}, Results),

    io:format("~n=== Batch 18 Results (Servers 86-90) ===~n"),
    io:format("Servers Spawned: 1/5~n"),
    io:format("Clients Spawned: 5/25~n"),
    io:format("Requests: 500/2500~n"),
    io:format("Successful: ~p (expected 50)~n", [TotalSuccess]),
    io:format("Rate Limited: ~p (expected 450)~n", [TotalRateLimited]),
    io:format("Errors: ~p~n", [TotalErrors]),

    SuccessRate = if TotalSuccess > 0 -> (TotalSuccess / 500) * 100; true -> 0 end,
    ExpectedRate = if TotalSuccess > 0 -> (50 / TotalSuccess) * 100; true -> 0 end,

    io:format("Actual Success Rate: ~.1f%~n", [SuccessRate]),
    io:format("Expected Success Rate: ~.1f% (10 req/s limit)~n", [(50 / 500) * 100]),
    io:format("Rate Limit Accuracy: ~.1f%~n", [ExpectedRate]),

    % Cleanup
    erlmcp_server:stop(ServerPid),
    erlmcp_rate_limiter:stop(),

    io:format("~nTest complete.~n"),
    ok.

run_burst_client(Port, ClientNum, RequestCount) ->
    {ok, ClientPid} = erlmcp_client:start_link([{transport, {erlmcp_transport_tcp, [{port, Port}]}}]),
    {ok, _} = erlmcp_client:initialize(ClientPid, {mcp_client_capabilities, undefined, undefined, undefined, undefined}),

    ClientId = list_to_binary("client_" ++ integer_to_list(Port) ++ "_" ++ integer_to_list(ClientNum)),

    Results = lists:map(fun(N) ->
        Args = [{<<"message">>, list_to_binary("burst_msg_" ++ integer_to_list(N))}],
        TimeNowMs = erlang:system_time(millisecond),

        case erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) of
            {ok, _TokensRemaining} ->
                Result = erlmcp_client:call_tool(ClientPid, <<"echo">>, Args),
                case Result of
                    {ok, _Response} ->
                        {success, N};
                    {error, Reason} ->
                        {error, {N, Reason}}
                end;
            {error, rate_limited, _RetryAfterMs} ->
                {rate_limited, N}
        end
    end, lists:seq(1, RequestCount)),

    ok = erlmcp_client:stop(ClientPid),

    SuccessCount = length([X || {success, X} <- Results]),
    RateLimitedCount = length([X || {rate_limited, X} <- Results]),
    ErrorCount = length([X || {error, X} <- Results]),

    {SuccessCount, RateLimitedCount, ErrorCount}.
