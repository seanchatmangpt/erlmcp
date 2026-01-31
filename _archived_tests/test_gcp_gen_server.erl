#!/usr/bin/env escript
%% -*- erlang -*-
%% Test script for gcp_simulator_server gen_server conversion

main(_) ->
    %% Add ebin directories
    EbinDirs = filelib:wildcard("_build/default/lib/*/ebin"),
    [true = code:add_pathz(Dir) || Dir <- EbinDirs],

    %% Start applications
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_transports),

    %% Start the gen_server
    {ok, Pid} = gcp_simulator_server:start_link(),
    io:format("Started gcp_simulator_server: ~p~n", [Pid]),

    %% Test basic operations
    Result1 = gcp_simulator_server:create_compute_instance(<<"test-instance">>, <<"e2-micro">>, <<"us-central1-a">>),
    io:format("Create instance result: ~s~n", [Result1]),

    Result2 = gcp_simulator_server:list_compute_instances(),
    io:format("List instances: ~s~n", [Result2]),

    Result3 = gcp_simulator_server:get_simulator_status(),
    io:format("Status: ~s~n", [Result3]),

    %% Test bucket creation
    Result4 = gcp_simulator_server:create_storage_bucket(<<"test-bucket">>, <<"US">>),
    io:format("Create bucket result: ~s~n", [Result4]),

    Result5 = gcp_simulator_server:list_storage_buckets(),
    io:format("List buckets: ~s~n", [Result5]),

    %% Test function deployment
    Result6 = gcp_simulator_server:deploy_cloud_function(<<"test-function">>, <<"python39">>, <<"handler">>),
    io:format("Deploy function result: ~s~n", [Result6]),

    Result7 = gcp_simulator_server:list_cloud_functions(),
    io:format("List functions: ~s~n", [Result7]),

    %% Stop the server
    gcp_simulator_server:stop(),
    io:format("Server stopped successfully~n"),

    halt(0).
