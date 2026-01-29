%%% MCP Roundtrip Test - Batch 4: Database Operations (Servers 16-20, Ports 9016-9020)
%%% Tests database tools: query, insert, update, delete

-module(erlmcp_batch4_db_ops_test).
-export([run/0]).

-include_lib("erlmcp.hrl").

run() ->
    io:format("=== MCP Roundtrip Test - Batch 4: Database Operations ===~n"),
    io:format("Testing Servers 16-20 (Ports 9016-9020)~n~n"),

    %% Spawn 5 servers (ports 9016-9020)
    ServerPorts = lists:seq(9016, 9020),
    ServerCount = length(ServerPorts),

    io:format("Starting ~p servers...~n", [ServerCount]),
    Servers = lists:map(fun(Port) ->
        ServerId = list_to_binary(io_lib:format("mcp_server_~p", [Port])),

        %% Define server capabilities
        Capabilities = #mcp_server_capabilities{
            tools = #mcp_capability{enabled = true}
        },

        %% Transport configuration
        TransportOpts = {tcp, #{port => Port}},

        %% Start the server (ranch listener is auto-started by transport)
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

        {Port, ServerPid, ServerId}
    end, ServerPorts),

    io:format("Servers spawned: ~p/~p~n~n", [length(Servers), ServerCount]),

    %% Wait for servers to be ready
    timer:sleep(500),

    %% Spawn 5 clients per server (25 total)
    ClientsPerServer = 5,
    TotalClients = ServerCount * ClientsPerServer,

    io:format("Spawning ~p clients (~p per server)...~n", [TotalClients, ClientsPerServer]),

    %% Spawn clients
    AllClients = lists:flatmap(fun({_Port, _ServerPid, ServerId}) ->
        lists:map(fun(N) ->
            ClientId = list_to_binary(io_lib:format("~s_client_~p", [ServerId, N])),

            %% Start client with TCP transport
            {ok, ClientPid} = erlmcp_client:start_link({tcp, #{
                host => "localhost",
                port => _Port
            }}),

            %% Initialize client
            ClientCapabilities = #mcp_client_capabilities{
                roots = #mcp_capability{enabled = false},
                sampling = #mcp_capability{enabled = false}
            },

            InitResult = erlmcp_client:initialize(ClientPid, ClientCapabilities),
            case InitResult of
                {ok, _ServerInfo} -> ok;
                {error, Reason} -> io:format("Client ~p init error: ~p~n", [ClientId, Reason])
            end,

            {_Port, ClientPid, ClientId}
        end, lists:seq(1, ClientsPerServer))
    end, Servers),

    io:format("Clients spawned: ~p/~p~n~n", [length(AllClients), TotalClients]),

    %% Run 100 operations per client (2500 total)
    OpsPerClient = 100,
    TotalOps = TotalClients * OpsPerClient,

    io:format("Running ~p operations per client (~p total)...~n", [OpsPerClient, TotalOps]),
    io:format("Operations: insert, query, update, delete~n~n"),

    %% Execute operations
    StartTime = erlang:monotonic_time(millisecond),

    Results = lists:map(fun({_Port, ClientPid, ClientId}) ->
        ClientResults = lists:map(fun(OpNum) ->
            %% Simulate database operations
            RecordId = list_to_binary(io_lib:format("~p", [OpNum])),

            %% Insert
            InsertStart = erlang:monotonic_time(microsecond),
            InsertResult = case erlmcp_client:call_tool(ClientPid, <<"database_insert">>, #{
                <<"table">> => <<"users">>,
                <<"data">> => #{
                    <<"id">> => RecordId,
                    <<"name">> => <<"Test User">>,
                    <<"email">> => <<"test@example.com">>
                }
            }) of
                {ok, _InsertResponse} ->
                    InsertEnd = erlang:monotonic_time(microsecond),
                    {insert, (InsertEnd - InsertStart) / 1000, ok};
                {error, InsertReason} ->
                    {insert, 0, {error, InsertReason}}
            end,

            %% Query
            QueryStart = erlang:monotonic_time(microsecond),
            QueryResult = case erlmcp_client:call_tool(ClientPid, <<"database_query">>, #{
                <<"table">> => <<"users">>,
                <<"filter">> => #{<<"id">> => RecordId}
            }) of
                {ok, _QueryResponse} ->
                    QueryEnd = erlang:monotonic_time(microsecond),
                    {query, (QueryEnd - QueryStart) / 1000, ok};
                {error, QueryReason} ->
                    {query, 0, {error, QueryReason}}
            end,

            %% Update
            UpdateStart = erlang:monotonic_time(microsecond),
            UpdateResult = case erlmcp_client:call_tool(ClientPid, <<"database_update">>, #{
                <<"table">> => <<"users">>,
                <<"id">> => RecordId,
                <<"data">> => #{<<"email">> => <<"updated@example.com">>}
            }) of
                {ok, _UpdateResponse} ->
                    UpdateEnd = erlang:monotonic_time(microsecond),
                    {update, (UpdateEnd - UpdateStart) / 1000, ok};
                {error, UpdateReason} ->
                    {update, 0, {error, UpdateReason}}
            end,

            %% Delete
            DeleteStart = erlang:monotonic_time(microsecond),
            DeleteResult = case erlmcp_client:call_tool(ClientPid, <<"database_delete">>, #{
                <<"table">> => <<"users">>,
                <<"id">> => RecordId
            }) of
                {ok, _DeleteResponse} ->
                    DeleteEnd = erlang:monotonic_time(microsecond),
                    {delete, (DeleteEnd - DeleteStart) / 1000, ok};
                {error, DeleteReason} ->
                    {delete, 0, {error, DeleteReason}}
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
            [] -> 0;
            _ -> lists:sum(Latencies) / length(Latencies)
        end,

        {ClientId, ClientSuccess, ClientFailed, ClientAvgLat, Latencies}
    end, AllClients),

    EndTime = erlang:monotonic_time(millisecond),
    TotalDuration = (EndTime - StartTime) / 1000,

    %% Calculate aggregate statistics
    TotalSuccess = lists:sum([S || {_, S, _, _, _} <- Results]),
    TotalFailed = lists:sum([F || {_, _, F, _, _} <- Results]),
    AllLatencies = lists:flatten([L || {_, _, _, _, L} <- Results]),

    AvgLatency = case AllLatencies of
        [] -> 0;
        _ -> lists:sum(AllLatencies) / length(AllLatencies)
    end,

    MinLatency = case AllLatencies of
        [] -> 0;
        _ -> lists:min(AllLatencies)
    end,

    MaxLatency = case AllLatencies of
        [] -> 0;
        _ -> lists:max(AllLatencies)
    end,

    Throughput = case TotalDuration of
        +0.0 -> 0;
        _ -> TotalSuccess / TotalDuration
    end,

    SuccessRate = case TotalSuccess + TotalFailed of
        0 -> 0.0;
        Total -> (TotalSuccess / Total) * 100
    end,

    %% Count query operations specifically
    QueryOps = [S || {_, S, _, _, _} <- Results],

    %% Print results
    io:format("~n=== Batch 4 Results (Servers 16-20) ===~n"),
    io:format("Servers Spawned: ~p/~p~n", [length(Servers), ServerCount]),
    io:format("Clients Spawned: ~p/~p~n", [length(AllClients), TotalClients]),
    io:format("Operations: ~p/~p~n", [TotalSuccess, TotalOps]),
    io:format("Avg Latency: ~.2f ms~n", [AvgLatency]),
    io:format("Min/Max: ~.2f/~.2f ms~n", [MinLatency, MaxLatency]),
    io:format("Throughput: ~.2f req/s~n", [Throughput]),
    io:format("Queries: ~p~n", [lists:sum(QueryOps)]),
    io:format("Success Rate: ~.2f%~n", [SuccessRate]),

    case TotalFailed of
        0 -> io:format("Errors: None~n");
        _ ->
            ErrorSamples = lists:sublist([C || {C, _, _, _, _} <- Results], 5),
            io:format("Errors: ~p (showing first 5 clients)~n", [ErrorSamples])
    end,

    %% Cleanup
    io:format("~nCleaning up...~n"),
    lists:foreach(fun({_Port, ClientPid, _ClientId}) ->
        %% Stop client
        catch erlmcp_client:stop(ClientPid)
    end, AllClients),

    lists:foreach(fun({_Port, ServerPid, _ServerId}) ->
        %% Stop server
        catch erlmcp_server:stop(ServerPid)
    end, Servers),

    io:format("Cleanup complete.~n"),

    ok.
