#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/erlmcp_core/ebin -pa _build/default/lib/erlmcp_transports/ebin -pa _build/default/lib/gproc/ebin -pa _build/default/lib/jsx/ebin -pa _build/default/lib/ranch/ebin

main(_) ->
    io:format("=== Batch 12: Large Payload Tests (Servers 56-60) ===~n~n"),

    % Start applications
    application:ensure_all_started(gproc),
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),

    io:format("Applications started~n"),

    % Test configuration
    ServerIds = lists:seq(56, 60),
    Ports = lists:seq(9056, 9060),
    ClientsPerServer = 5,
    TransfersPerSize = 20,  % Reduced for faster execution

    % Start servers
    io:format("Starting ~p servers on ports ~p...~n", [length(ServerIds), hd(Ports)]),
    ServerPids = start_servers(ServerIds, Ports, []),
    io:format("Servers spawned: ~p/~p~n", [length(ServerPids), length(ServerIds)]),

    % Test 1KB payloads
    io:format("~n=== Testing 1KB Payloads ===~n"),
    Payload1KB = generate_payload(1024),
    {Success1KB, Time1KB} = test_all_servers(ServerPids, Payload1KB, ClientsPerServer, TransfersPerSize),
    io:format("1KB: ~p transfers, ~p successful, avg latency: ~.2f ms~n",
              [length(ServerPids) * ClientsPerServer * TransfersPerSize,
               Success1KB, Time1KB / 1000]),

    % Test 100KB payloads
    io:format("~n=== Testing 100KB Payloads ===~n"),
    Payload100KB = generate_payload(102400),
    {Success100KB, Time100KB} = test_all_servers(ServerPids, Payload100KB, ClientsPerServer, TransfersPerSize),
    io:format("100KB: ~p transfers, ~p successful, avg latency: ~.2f ms~n",
              [length(ServerPids) * ClientsPerServer * TransfersPerSize,
               Success100KB, Time100KB / 1000]),

    % Test 1MB payloads
    io:format("~n=== Testing 1MB Payloads ===~n"),
    Payload1MB = generate_payload(1048576),
    {Success1MB, Time1MB} = test_all_servers(ServerPids, Payload1MB, ClientsPerServer, TransfersPerSize),
    io:format("1MB: ~p transfers, ~p successful, avg latency: ~.2f ms~n",
              [length(ServerPids) * ClientsPerServer * TransfersPerSize,
               Success1MB, Time1MB / 1000]),

    % Calculate totals
    TotalTransfers = 3 * length(ServerPids) * ClientsPerServer * TransfersPerSize,
    TotalSuccess = Success1KB + Success100KB + Success1MB,
    SuccessRate = (TotalSuccess / TotalTransfers) * 100,
    TotalDataMB = (Success1KB * 1 + Success100KB * 100 + Success1MB * 1024) / 1024,

    io:format("~n=== Batch 12 Results ===~n"),
    io:format("Servers Spawned: ~p/~p~n", [length(ServerPids), length(ServerIds)]),
    io:format("Clients per Server: ~p~n", [ClientsPerServer]),
    io:format("Total Clients: ~p~n", [length(ServerPids) * ClientsPerServer]),
    io:format("Transfers: ~p/~p~n", [TotalSuccess, TotalTransfers]),
    io:format("1KB Avg Latency: ~.2f ms~n", [Time1KB / 1000]),
    io:format("100KB Avg Latency: ~.2f ms~n", [Time100KB / 1000]),
    io:format("1MB Avg Latency: ~.2f ms~n", [Time1MB / 1000]),
    io:format("Total Data: ~.2f MB~n", [TotalDataMB]),
    io:format("Success Rate: ~.2f%~n", [SuccessRate]),

    % Stop servers
    stop_servers(ServerPids),

    io:format("~n=== Test Complete ===~n").

%%%-------------------------------------------------------------------
%%% Server Functions
%%%-------------------------------------------------------------------

start_servers([], [], Acc) ->
    lists:reverse(Acc);
start_servers([Id | Ids], [Port | Ports], Acc) ->
    Config = #{
        name => list_to_atom("mcp_server_" ++ integer_to_list(Id)),
        transport => {erlmcp_transport_tcp, [{port, Port}]},
        tools => #{
            <<"echo_data">> => #{
                description => <<"Echo data back">>,
                input_schema => #{
                    type => object,
                    properties => #{
                        <<"data">> => #{type => string},
                        <<"size">> => #{type => number}
                    },
                    required => [<<"data">>]
                }
            }
        }
    },

    case erlmcp_server:start_link(Config) of
        {ok, Pid} ->
            timer:sleep(100),
            start_servers(Ids, Ports, [{Pid, Port} | Acc]);
        {error, Reason} ->
            io:format("Failed to start server on port ~p: ~p~n", [Port, Reason]),
            start_servers(Ids, Ports, Acc)
    end.

stop_servers(ServerPids) ->
    lists:foreach(fun({Pid, _Port}) ->
        case is_process_alive(Pid) of
            true -> erlmcp_server:stop(Pid);
            false -> ok
        end
    end, ServerPids).

%%%-------------------------------------------------------------------
%%% Test Functions
%%%-------------------------------------------------------------------

test_all_servers(ServerPids, Payload, ClientsPerServer, TransfersPerClient) ->
    Results = lists:map(fun({ServerPid, Port}) ->
        test_server(ServerPid, Port, Payload, ClientsPerServer, TransfersPerClient)
    end, ServerPids),

    TotalSuccess = lists:sum([S || {S, _} <- Results]),
    TotalTime = lists:sum([T || {_, T} <- Results]),
    AvgTime = case length(Results) of
        0 -> 0;
        _ -> TotalTime div length(Results)
    end,

    {TotalSuccess, AvgTime}.

test_server(ServerPid, Port, Payload, ClientCount, TransfersPerClient) ->
    % Spawn clients
    Clients = lists:map(fun(N) ->
        ClientName = list_to_atom("client_" ++ integer_to_list(Port) ++ "_" ++ integer_to_list(N)),
        {ok, Pid} = erlmcp_client:start_link(#{
            name => ClientName,
            transport => {erlmcp_transport_tcp, [{port, Port}]}
        }),
        Pid
    end, lists:seq(1, ClientCount)),

    % Run transfers
    Results = lists:map(fun(ClientPid) ->
        run_client_transfers(ClientPid, Payload, TransfersPerClient)
    end, Clients),

    % Cleanup clients
    lists:foreach(fun(Pid) -> erlmcp_client:stop(Pid) end, Clients),

    % Aggregate results
    TotalSuccess = lists:sum([S || {S, _} <- Results]),
    TotalTime = lists:sum([T || {_, T} <- Results]),
    AvgTime = case TotalSuccess of
        0 -> 0;
        _ -> TotalTime div TotalSuccess
    end,

    {TotalSuccess, AvgTime}.

run_client_transfers(ClientPid, Payload, Count) ->
    run_client_transfers(ClientPid, Payload, Count, 0, 0).

run_client_transfers(_ClientPid, _Payload, 0, SuccessAcc, TimeAcc) ->
    {SuccessAcc, TimeAcc};
run_client_transfers(ClientPid, Payload, Count, SuccessAcc, TimeAcc) ->
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => generate_request_id(),
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"echo_data">>,
            <<"arguments">> => #{
                <<"data">> => Payload,
                <<"size">> => byte_size(Payload)
            }
        }
    },

    StartTime = erlang:monotonic_time(microsecond),
    Result = case erlmcp_client:send_request(ClientPid, Request) of
        {ok, RequestId} ->
            case erlmcp_client:wait_for_response(ClientPid, RequestId, 30000) of
                {ok, #{<<"result">> := _}} -> ok;
                _ -> error
            end;
        _ -> error
    end,
    EndTime = erlang:monotonic_time(microsecond),

    case Result of
        ok ->
            run_client_transfers(ClientPid, Payload, Count - 1,
                                SuccessAcc + 1, TimeAcc + (EndTime - StartTime));
        error ->
            run_client_transfers(ClientPid, Payload, Count - 1,
                                SuccessAcc, TimeAcc)
    end.

%%%-------------------------------------------------------------------
%%% Helper Functions
%%%-------------------------------------------------------------------

generate_payload(Size) ->
    Pattern = <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789">>,
    PatternSize = byte_size(Pattern),
    Repeats = (Size div PatternSize) + 1,
    Base = binary:copy(<<Pattern/binary, Pattern/binary>>, Repeats),
    binary:part(Base, 0, Size).

generate_request_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    Id.
