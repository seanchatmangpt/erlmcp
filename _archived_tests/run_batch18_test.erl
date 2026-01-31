#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/erlmcp_core/ebin
%%! -pa _build/default/lib/erlmcp_transports/ebin
%%! -pa _build/default/lib/erlmcp_observability/ebin
%%! -pa _build/test/lib/proper/ebin
%%! -pa _build/test/lib/meck/ebin
%%! -pa test/

main([]) ->
    io:format("~n=== Batch 18 Rate Limiting Test Runner ===~n"),
    io:format("Servers: 86-90 (Ports 9086-9090)~n"),
    io:format("Rate Limit: 10 req/s per client~n"),
    io:format("Clients: 25 (5 per server)~n"),
    io:format("Requests: 100 per client (2500 total)~n"),
    io:format("~n"),

    % Start applications
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),
    application:ensure_all_started(erlmcp_observability),

    % Configure rate limiting
    application:set_env(erlmcp, rate_limiting, [
        {max_messages_per_sec, 10},
        {max_connections_per_sec, 10},
        {global_max_messages_per_sec, 10000},
        {max_tool_calls_per_sec, 10},
        {max_subscriptions_per_sec, 20},
        {bucket_refill_interval_ms, 100},
        {ddos_violation_threshold, 100},
        {ddos_block_duration_ms, 300000},
        {enabled, true}
    ]),

    % Start rate limiter
    {ok, _RateLimiterPid} = erlmcp_rate_limiter:start_link(),

    % Start servers
    ServerIds = lists:seq(86, 90),
    Ports = lists:seq(9086, 9090),

    ServerPids = lists:map(fun({Id, Port}) ->
        ServerId = list_to_atom("mcp_server_" ++ integer_to_list(Port)),
        Capabilities = #mcp_server_capabilities{
            resources = #mcp_capability{enabled = false},
            tools = #mcp_capability{enabled = true},
            prompts = #mcp_capability{enabled = false}
        },

        {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities),

        % Add echo tool
        HandlerEcho = fun(Args) ->
            Message = maps:get(<<"message">>, Args, <<"">>),
            [{<<"echo">>, Message}, {<<"timestamp">>, erlang:system_time(millisecond)}]
        end,
        EchoTool = #mcp_tool{
            name = <<"echo">>,
            description = <<"Echo tool for rate limit testing">>,
            input_schema => [{type, <<"object">>}]
        },
        erlmcp_server:add_tool(Pid, EchoTool, HandlerEcho),

        {Id, Pid, Port, ServerId}
    end, lists:zip(ServerIds, Ports)),

    timer:sleep(500),

    % Run burst test on one server
    {_, _, Port, _} = lists:nth(1, ServerPids),
    io:format("Running burst test on port ~p...~n", [Port]),

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
    io:format("Servers Spawned: 5/5~n"),
    io:format("Clients Spawned: 5/25~n"),
    io:format("Requests: 500/2500~n"),
    io:format("Successful: ~p (expected ~50)~n", [TotalSuccess]),
    io:format("Rate Limited: ~p (expected ~450)~n", [TotalRateLimited]),
    io:format("Errors: ~p~n", [TotalErrors]),
    io:format("Rate Limit Accuracy: ~.1f%~n", [if TotalSuccess > 0 -> (50 / TotalSuccess) * 100; true -> 0 end]),
    io:format("Success Rate: ~.1f% (within limit: 10 req/s)~n", [(TotalSuccess / 500) * 100]),

    % Cleanup
    lists:foreach(fun({_Id, Pid, _Port, _ServerId}) ->
        erlmcp_server:stop(Pid)
    end, ServerPids),
    erlmcp_rate_limiter:stop(),

    io:format("~nTest complete.~n"),
    ok.

run_burst_client(Port, ClientNum, RequestCount) ->
    {ok, ClientPid} = erlmcp_client:start_link(#{transport => {erlmcp_transport_tcp, [{port, Port}]}}),
    {ok, _} = erlmcp_client:initialize(ClientPid, #mcp_client_capabilities{}),

    ClientId = list_to_binary("client_" ++ integer_to_list(Port) ++ "_" ++ integer_to_list(ClientNum)),

    Results = lists:map(fun(N) ->
        Args = #{<<"message">> => list_to_binary("burst_msg_" ++ integer_to_list(N))},
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

    SuccessCount = length([R || {success, _} <- Results]),
    RateLimitedCount = length([R || {rate_limited, _} <- Results]),
    ErrorCount = length([R || {error, _} <- Results]),

    {SuccessCount, RateLimitedCount, ErrorCount}.
