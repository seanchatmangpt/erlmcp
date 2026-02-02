#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

%%%-------------------------------------------------------------------
%%% @doc
%%% Port Pool Example - Demonstrates port pooling usage
%%%
%%% This example shows how to use erlmcp_port_pool for:
%%% - Starting port pool
%%% - Checking out ports from pool
%%% - Returning ports to pool
%%% - Monitoring pool status
%%% @end
%%%-------------------------------------------------------------------

-mode(compile).
-include("erlmcp.hrl").

main([]) ->
    io:format("=== Port Pool Example ===~n~n"),

    %% Start port pool
    io:format("1. Starting port pool...~n"),
    PoolConfig = #{
        size => 5,
        max_overflow => 2
    },
    {ok, PoolPid} = erlmcp_port_pool:start_link(PoolConfig),
    io:format("   Pool started: ~p~n", [PoolPid]),
    io:format("   Config: size=~p, overflow=~p~n~n",
              [maps:get(size, PoolConfig), maps:get(max_overflow, PoolConfig)]),

    %% Get initial pool status
    io:format("2. Getting initial pool status...~n"),
    {ok, InitialStatus} = erlmcp_port_pool:pool_status(),
    io:format("   Pool size: ~p~n", [maps:get(size, InitialStatus)]),
    io:format("   Available: ~p~n", [maps:get(available, InitialStatus)]),
    io:format("   Overflow: ~p~n~n", [maps:get(overflow, InitialStatus)]),

    %% Checkout multiple ports
    io:format("3. Checking out ports from pool...~n"),
    Workers = lists:filtermap(fun(_) ->
        case erlmcp_port_pool:checkout_port() of
            {ok, WorkerPid} ->
                io:format("   Checked out: ~p~n", [WorkerPid]),
                {true, WorkerPid};
            {error, Reason} ->
                io:format("   Checkout failed: ~p~n", [Reason]),
                false
        end
    end, lists:seq(1, 3)),
    io:format("   Checked out ~p workers~n~n", [length(Workers)]),

    %% Get pool status after checkout
    io:format("4. Getting pool status after checkout...~n"),
    {ok, StatusAfterCheckout} = erlmcp_port_pool:pool_status(),
    io:format("   Available: ~p~n~n", [maps:get(available, StatusAfterCheckout)]),

    %% Get healthy ports
    io:format("5. Getting healthy ports...~n"),
    {ok, HealthyPids} = erlmcp_port_pool:healthy_ports(),
    io:format("   Healthy ports: ~p~n~n", [length(HealthyPids)]),

    %% Return all workers
    io:format("6. Returning workers to pool...~n"),
    lists:foreach(fun(WorkerPid) ->
        ok = erlmcp_port_pool:return_port(WorkerPid),
        io:format("   Returned: ~p~n", [WorkerPid])
    end, Workers),
    io:format("   All workers returned~n~n"),

    %% Get final pool status
    io:format("7. Getting final pool status...~n"),
    {ok, FinalStatus} = erlmcp_port_pool:pool_status(),
    io:format("   Available: ~p~n", [maps:get(available, FinalStatus)]),
    io:format("   Total requests: ~p~n~n", [maps:get(total_requests, FinalStatus)]),

    io:format("=== Example Complete ===~n"),
    init:stop().

main(["--help"]) ->
    io:format("Port Pool Example~n"),
    io:format("Usage: port_pool_example.erl~n~n"),
    io:format("Demonstrates port pooling for efficient resource reuse~n"),
    init:stop().

main(_) ->
    main([]).
