#!/usr/bin/env escript
%% -*- erlang -*-
%% Test script for gcp_simulator_server gen_server conversion (standalone)

main(_) ->
    %% Add ebin directories
    EbinDirs = filelib:wildcard("_build/default/lib/*/ebin"),
    [true = code:add_pathz(Dir) || Dir <- EbinDirs],

    io:format("Testing gcp_simulator_server as gen_server...~n~n"),

    %% Start the gen_server directly (without full app)
    {ok, Pid} = gcp_simulator_server:start_link(),
    io:format("✓ Started gcp_simulator_server: ~p~n", [Pid]),

    %% Verify it's registered
    true = is_process_alive(Pid),
    Pid = whereis(gcp_simulator_server),
    io:format("✓ Server is registered and alive~n~n"),

    %% Test basic operations
    io:format("Testing Compute Engine operations...~n"),
    Result1 = gcp_simulator_server:create_compute_instance(<<"test-instance">>, <<"e2-micro">>, <<"us-central1-a">>),
    io:format("✓ Create instance result: ~s~n", [Result1]),

    Result2 = gcp_simulator_server:list_compute_instances(),
    io:format("✓ List instances: ~s~n", [Result2]),

    Result3 = gcp_simulator_server:get_compute_instance(<<"test-instance">>),
    io:format("✓ Get instance: ~p~n", [Result3]),

    io:format("~nTesting Cloud Storage operations...~n"),
    Result4 = gcp_simulator_server:create_storage_bucket(<<"test-bucket">>, <<"US">>),
    io:format("✓ Create bucket result: ~s~n", [Result4]),

    Result5 = gcp_simulator_server:list_storage_buckets(),
    io:format("✓ List buckets: ~s~n", [Result5]),

    io:format("~nTesting Cloud Functions operations...~n"),
    Result6 = gcp_simulator_server:deploy_cloud_function(<<"test-function">>, <<"python39">>, <<"handler">>),
    io:format("✓ Deploy function result: ~s~n", [Result6]),

    Result7 = gcp_simulator_server:list_cloud_functions(),
    io:format("✓ List functions: ~s~n", [Result7]),

    io:format("~nTesting simulator status...~n"),
    Result8 = gcp_simulator_server:get_simulator_status(),
    io:format("✓ Status: ~s~n", [Result8]),

    %% Stop the server
    io:format("~nStopping server...~n"),
    ok = gcp_simulator_server:stop(),
    timer:sleep(100),  % Give it time to clean up

    %% Verify it's gone
    undefined = whereis(gcp_simulator_server),
    io:format("✓ Server stopped and unregistered~n~n"),

    io:format("=== ALL TESTS PASSED ===~n"),
    io:format("The gcp_simulator_server is now a proper gen_server!~n"),

    halt(0).
