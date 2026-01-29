#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin -pa _build/test/lib/*/ebin

main(_) ->
    io:format("=== MCP Roundtrip Batch 11: Concurrent Request Tests ===~n"),
    io:format("Servers 51-55, Ports 9051-9055~n"),
    io:format("25 clients, 100 concurrent sets each, 10 parallel requests~n"),
    io:format("Total operations: 10000~n~n"),

    % Start applications
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_observability),
    application:ensure_all_started(erlmcp_transports),

    % Start 5 servers on ports 9051-9055
    ServerIds = lists:seq(51, 55),
    Ports = lists:seq(9051, 9055),

    io:format("Starting ~p servers on ports ~p-~p...~n", [length(ServerIds), hd(Ports), lists:last(Ports)]),

    ServerPids = lists:map(fun({Id, Port}) ->
        ServerName = list_to_atom("mcp_server_" ++ integer_to_list(Id)),

        Config = #{
            name => ServerName,
            transport => {erlmcp_transport_tcp, [{port, Port}]},
            tools => #{
                <<"echo">> => #{
                    description => <<"Echo tool">>,
                    input_schema => #{
                        type => object,
                        properties => #{<<"message">> => #{type => string}},
                        required => [<<"message">>]
                    }
                },
                <<"concurrent_test">> => #{
                    description => <<"Concurrent test tool">>,
                    input_schema => #{
                        type => object,
                        properties => #{
                            <<"request_id">> => #{type => string},
                            <<"timestamp">> => #{type => number}
                        },
                        required => [<<"request_id">>, <<"timestamp">>]
                    }
                }
            }
        },

        case erlmcp_server:start_link(ServerName, Config) of
            {ok, Pid} ->
                io:format("  Server ~p started on port ~p~n", [Id, Port]),
                {Id, Pid, Port};
            {error, Reason} ->
                io:format("  ERROR: Server ~p failed on port ~p: ~p~n", [Id, Port, Reason]),
                exit({server_start_failed, Id, Reason})
        end
    end, lists:zip(ServerIds, Ports)),

    timer:sleep(500),

    % Create 5 clients per server
    io:format("~nSpawning ~p clients (5 per server)...~n", [5 * length(ServerIds)]),

    ClientGroups = lists:map(fun({ServerId, _Pid, Port}) ->
        lists:map(fun(N) ->
            ClientName = list_to_atom("concurrent_client_" ++ integer_to_list(ServerId) ++
                                       "_" ++ integer_to_list(N)),
            {ok, Pid} = erlmcp_client:start_link(#{
                name => ClientName,
                transport => {erlmcp_transport_tcp, [{port, Port}]}
            }),
            Pid
        end, lists:seq(1, 5))
    end, ServerPids),

    io:format("All ~p clients spawned~n", [length(lists:flatten(ClientGroups))]),

    % Run concurrent tests
    io:format("~nRunning concurrent request tests...~n~n"),
    io:format("Test: 100 sets per client × 10 parallel requests = 1000 ops per client~n"),
    io:format("Total: 25 clients × 1000 ops = 25000 operations~n~n"),

    StartTime = erlang:monotonic_time(millisecond),

    % Run tests for each client
    AllResults = lists:map(fun({ServerId, _, _}) ->
        io:format("Testing server ~p...~n", [ServerId]),
        ServerClients = lists:nth(ServerId - 50, ClientGroups),

        lists:map(fun(ClientPid) ->
            run_concurrent_sets(ClientPid, ServerId)
        end, ServerClients)
    end, ServerPids),

    EndTime = erlang:monotonic_time(millisecond),
    TotalDuration = EndTime - StartTime,

    % Calculate results
    FlatResults = lists:flatten(AllResults),
    {TotalSets, TotalOps, TotalPassed, Errors} =
        lists:foldl(fun({_SetNum, Ops, Passed, SetErrors}, {AccSets, AccOps, AccPassed, AccErrors}) ->
            {AccSets + 1, AccOps + Ops, AccPassed + Passed, AccErrors ++ SetErrors}
        end, {0, 0, 0, []}, FlatResults),

    SuccessRate = if
        TotalOps > 0 -> (TotalPassed / TotalOps) * 100;
        true -> 0
    end,

    io:format("~n=== Batch 11 Results (Servers 51-55) ===~n"),
    io:format("Servers Spawned: ~p/~p~n", [length(ServerPids), length(ServerIds)]),
    io:format("Clients Spawned: ~p/~p~n", [length(lists:flatten(ClientGroups)), 5 * length(ServerIds)]),
    io:format("Concurrent Sets: ~p~n", [TotalSets]),
    io:format("Operations: ~p/~p~n", [TotalPassed, TotalOps]),
    io:format("Total Duration: ~p ms~n", [TotalDuration]),
    io:format("Avg Latency per Op: ~.2f ms~n", [TotalDuration / max(1, TotalOps)]),
    io:format("Success Rate: ~.2f%~n", [SuccessRate]),
    io:format("Errors: ~p~n", [length(Errors)]),

    % Cleanup
    io:format("~nCleaning up...~n"),
    lists:foreach(fun(Clients) ->
        lists:foreach(fun(Pid) -> erlmcp_client:stop(Pid) end, Clients)
    end, ClientGroups),
    lists:foreach(fun({Id, Pid, _}) ->
        erlmcp_server:stop(Pid),
        io:format("  Stopped server ~p~n", [Id])
    end, ServerPids),

    io:format("~n=== Test Complete ===~n"),
    halt(0).

run_concurrent_sets(ClientPid, ServerId) ->
    Sets = 100,
    ParallelReqs = 10,

    lists:map(fun(SetNum) ->
        Parent = self(),
        StartTime = erlang:monotonic_time(microsecond),

        Pids = lists:map(fun(N) ->
            spawn_monitor(fun() ->
                RequestId = list_to_binary("req_" ++ integer_to_list(ServerId) ++
                                           "_" ++ integer_to_list(SetNum) ++
                                           "_" ++ integer_to_list(N)),

                Request = #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => RequestId,
                    <<"method">> => <<"tools/call">>,
                    <<"params">> => #{
                        <<"name">> => <<"concurrent_test">>,
                        <<"arguments">> => #{
                            <<"request_id">> => RequestId,
                            <<"timestamp">> => erlang:system_time(microsecond)
                        }
                    }
                },

                Result = case erlmcp_client:send_request(ClientPid, Request) of
                    {ok, ReqId} ->
                        case erlmcp_client:wait_for_response(ClientPid, ReqId, 3000) of
                            {ok, #{<<"result">> := _}} -> {ok, RequestId};
                            {ok, Response} -> {error, {unexpected_response, Response}};
                            {error, Reason} -> {error, {response_error, Reason}}
                        end;
                    {error, Reason} ->
                        {error, {send_error, Reason}}
                end,

                Parent ! {self(), Result}
            end)
        end, lists:seq(1, ParallelReqs)),

        % Collect results
        Results = lists:map(fun({Pid, Ref}) ->
            receive
                {Pid, Result} ->
                    erlang:demonitor(Ref, [flush]),
                    Result
            after 4000 ->
                erlang:demonitor(Ref, [flush]),
                {error, timeout}
            end
        end, Pids),

        EndTime = erlang:monotonic_time(microsecond),
        SetLatency = (EndTime - StartTime) / 1000,

        Passed = length([ok || {ok, _} <- Results]),
        Errors = [E || {error, E} <- Results],

        if
            SetNum rem 10 =:= 0 ->
                io:format("  Set ~p/~p: ~p/~p passed, latency ~.2f ms~n",
                          [SetNum, Sets, Passed, ParallelReqs, SetLatency]);
            true -> ok
        end,

        {SetNum, ParallelReqs, Passed, Errors}
    end, lists:seq(1, Sets)).
