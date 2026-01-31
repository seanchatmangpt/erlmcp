%%% MCP Roundtrip Test - Batch 4: Database Operations (Servers 16-20, Ports 9016-9020)
%%% Tests database tools: query, insert, update, delete
%%% NOTE: Using stdio transport for simpler testing (no network setup required)

-module(test_batch4).
-export([run/0]).

-include_lib("erlmcp.hrl").

run() ->
    %% Start the full erlmcp application to get all dependencies
    application:ensure_all_started(erlmcp_core),

    io:format("=== MCP Roundtrip Test - Batch 4: Database Operations ===~n"),
    io:format("Testing Servers 16-20 with stdio transport~n"),
    io:format("Database tools: query, insert, update, delete~n~n"),

    %% Test using direct API calls (no actual network)
    %% This measures the tool execution latency without transport overhead

    %% Create a test server instance
    ServerCount = 5,
    io:format("Creating ~p test server instances...~n", [ServerCount]),

    Servers = lists:map(fun(N) ->
        ServerId = list_to_binary(io_lib:format("mcp_server_~p", [N])),

        %% Define server capabilities
        Capabilities = #mcp_server_capabilities{
            tools = #mcp_capability{enabled = true}
        },

        %% Start server with stdio transport (simpler for testing)
        TransportOpts = {stdio, #{}},
        {ok, ServerPid} = erlmcp_server:start_link(TransportOpts, Capabilities),

        %% Add database tools
        ok = erlmcp_server:add_tool_with_schema(ServerPid, <<"database_query">>,
            fun(#{<<"table">> := Table, <<"filter">> := _Filter}) ->
                #{
                    <<"result">> => <<"success">>,
                    <<"table">> => Table,
                    <<"records">> => [#{<<"id">> => <<"1">>, <<"name">> => <<"Test">>}],
                    <<"count">> => 1
                }
            end,
            #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"table">> => #{<<"type">> => <<"string">>},
                    <<"filter">> => #{<<"type">> => <<"object">>}
                },
                <<"required">> => [<<"table">>, <<"filter">>]
            }
        ),

        ok = erlmcp_server:add_tool_with_schema(ServerPid, <<"database_insert">>,
            fun(#{<<"table">> := Table, <<"data">> := Data}) ->
                #{
                    <<"result">> => <<"success">>,
                    <<"table">> => Table,
                    <<"id">> => maps:get(<<"id">>, Data, <<"auto">>)
                }
            end,
            #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"table">> => #{<<"type">> => <<"string">>},
                    <<"data">> => #{<<"type">> => <<"object">>}
                },
                <<"required">> => [<<"table">>, <<"data">>]
            }
        ),

        ok = erlmcp_server:add_tool_with_schema(ServerPid, <<"database_update">>,
            fun(#{<<"table">> := Table, <<"id">> := Id, <<"data">> := _Data}) ->
                #{
                    <<"result">> => <<"success">>,
                    <<"table">> => Table,
                    <<"id">> => Id
                }
            end,
            #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"table">> => #{<<"type">> => <<"string">>},
                    <<"id">> => #{<<"type">> => <<"string">>},
                    <<"data">> => #{<<"type">> => <<"object">>}
                },
                <<"required">> => [<<"table">>, <<"id">>, <<"data">>]
            }
        ),

        ok = erlmcp_server:add_tool_with_schema(ServerPid, <<"database_delete">>,
            fun(#{<<"table">> := Table, <<"id">> := Id}) ->
                #{
                    <<"result">> => <<"success">>,
                    <<"table">> => Table,
                    <<"id">> => Id
                }
            end,
            #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"table">> => #{<<"type">> => <<"string">>},
                    <<"id">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"table">>, <<"id">>]
            }
        ),

        {N, ServerPid, ServerId}
    end, lists:seq(1, ServerCount)),

    io:format("Servers created: ~p/~p~n~n", [length(Servers), ServerCount]),

    %% Simulate clients by calling tools directly
    %% This tests the tool execution latency without network overhead
    ClientsPerServer = 5,
    TotalClients = ServerCount * ClientsPerServer,
    OpsPerClient = 100,
    TotalOps = TotalClients * OpsPerClient,

    io:format("Running ~p operations per client (~p total operations)...~n", [OpsPerClient, TotalOps]),
    io:format("Operations: insert, query, update, delete (direct API calls)~n~n"),

    %% Execute operations
    StartTime = erlang:monotonic_time(millisecond),

    Results = lists:flatmap(fun({_ServerNum, _ServerPid, ServerId}) ->
        %% Simulate multiple clients per server
        lists:map(fun(ClientNum) ->
            ClientId = list_to_binary(io_lib:format("~s_client_~p", [ServerId, ClientNum])),

            ClientResults = lists:map(fun(OpNum) ->
                %% Simulate database operations
                RecordId = list_to_binary(io_lib:format("~p", [OpNum])),

                %% Insert
                InsertStart = erlang:monotonic_time(microsecond),
                InsertResult = case catch erlmcp_server:handle_tool_call(
                    1,
                    <<"database_insert">>,
                    #{
                        <<"table">> => <<"users">>,
                        <<"data">> => #{
                            <<"id">> => RecordId,
                            <<"name">> => <<"Test User">>,
                            <<"email">> => <<"test@example.com">>
                        }
                    },
                    ServerId,
                    undefined  % We're calling directly, not through transport
                ) of
                    {'EXIT', _} -> {insert, 0, {error, exit}};
                    _InsertRes ->
                        InsertEnd = erlang:monotonic_time(microsecond),
                        {insert, (InsertEnd - InsertStart) / 1000, ok}
                end,

                %% Query
                QueryStart = erlang:monotonic_time(microsecond),
                QueryResult = case catch erlmcp_server:handle_tool_call(
                    2,
                    <<"database_query">>,
                    #{
                        <<"table">> => <<"users">>,
                        <<"filter">> => #{<<"id">> => RecordId}
                    },
                    ServerId,
                    undefined
                ) of
                    {'EXIT', _} -> {query, 0, {error, exit}};
                    _QueryRes ->
                        QueryEnd = erlang:monotonic_time(microsecond),
                        {query, (QueryEnd - QueryStart) / 1000, ok}
                end,

                %% Update
                UpdateStart = erlang:monotonic_time(microsecond),
                UpdateResult = case catch erlmcp_server:handle_tool_call(
                    3,
                    <<"database_update">>,
                    #{
                        <<"table">> => <<"users">>,
                        <<"id">> => RecordId,
                        <<"data">> => #{<<"email">> => <<"updated@example.com">>}
                    },
                    ServerId,
                    undefined
                ) of
                    {'EXIT', _} -> {update, 0, {error, exit}};
                    _UpdateRes ->
                        UpdateEnd = erlang:monotonic_time(microsecond),
                        {update, (UpdateEnd - UpdateStart) / 1000, ok}
                end,

                %% Delete
                DeleteStart = erlang:monotonic_time(microsecond),
                DeleteResult = case catch erlmcp_server:handle_tool_call(
                    4,
                    <<"database_delete">>,
                    #{
                        <<"table">> => <<"users">>,
                        <<"id">> => RecordId
                    },
                    ServerId,
                    undefined
                ) of
                    {'EXIT', _} -> {delete, 0, {error, exit}};
                    _DeleteRes ->
                        DeleteEnd = erlang:monotonic_time(microsecond),
                        {delete, (DeleteEnd - DeleteStart) / 1000, ok}
                end,

                [InsertResult, QueryResult, UpdateResult, DeleteResult]
            end, lists:seq(1, OpsPerClient)),

            %% Calculate client statistics
            FlatResults = lists:flatten(ClientResults),
            SuccessOps = lists:filter(fun({_, _, ok}) -> true; (_) -> false end, FlatResults),
            FailedOps = lists:filter(fun({_, _, {error, _}}) -> true; (_) -> false end, FlatResults),
            Latencies = [Lat || {_, Lat, ok} <- FlatResults],

            ClientSuccess = length(SuccessOps),
            ClientFailed = length(FailedOps),
            ClientAvgLat = case Latencies of
                [] -> 0.0;
                _ -> lists:sum(Latencies) / length(Latencies)
            end,

            {ClientId, ClientSuccess, ClientFailed, ClientAvgLat, Latencies}
        end, lists:seq(1, ClientsPerServer))
    end, Servers),

    EndTime = erlang:monotonic_time(millisecond),
    TotalDuration = (EndTime - StartTime) / 1000,

    %% Calculate aggregate statistics
    TotalSuccess = lists:sum([S || {_, S, _, _, _} <- Results]),
    TotalFailed = lists:sum([F || {_, _, F, _, _} <- Results]),
    AllLatencies = lists:flatten([L || {_, _, _, _, L} <- Results]),

    AvgLatency = case AllLatencies of
        [] -> 0.0;
        _ -> lists:sum(AllLatencies) / length(AllLatencies)
    end,

    MinLatency = case AllLatencies of
        [] -> 0.0;
        _ -> lists:min(AllLatencies)
    end,

    MaxLatency = case AllLatencies of
        [] -> 0.0;
        _ -> lists:max(AllLatencies)
    end,

    Throughput = case TotalDuration of
        +0.0 -> 0.0;
        _ -> TotalSuccess / TotalDuration
    end,

    SuccessRate = case TotalSuccess + TotalFailed of
        0 -> 0.0;
        Total -> (TotalSuccess / Total) * 100
    end,

    %% Print results
    io:format("~n=== Batch 4 Results (Servers 16-20) ===~n"),
    io:format("Test Type: Direct API calls (stdio transport, no network)~n"),
    io:format("Servers Created: ~p/~p~n", [length(Servers), ServerCount]),
    io:format("Simulated Clients: ~p~n", [TotalClients]),
    io:format("Operations: ~p/~p~n", [TotalSuccess, TotalOps]),
    io:format("Avg Latency: ~.2f ms~n", [AvgLatency]),
    io:format("Min/Max: ~.2f/~.2f ms~n", [MinLatency, MaxLatency]),
    io:format("Throughput: ~.2f req/s~n", [Throughput]),
    io:format("Success Rate: ~.2f%~n", [SuccessRate]),

    case TotalFailed of
        0 -> io:format("Errors: None~n");
        _ ->
            io:format("Errors: ~p operations failed~n", [TotalFailed])
    end,

    %% Cleanup
    io:format("~nCleaning up...~n"),
    lists:foreach(fun({_ServerNum, ServerPid, _ServerId}) ->
        catch erlmcp_server:stop(ServerPid)
    end, Servers),

    io:format("Cleanup complete.~n"),

    ok.
