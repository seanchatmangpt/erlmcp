#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% ErlMCP Integration Test Runner (escript version)
%%
%% Usage: ./test_runner.escript
%% Or:    escript test_runner.escript

main(_Args) ->
    %% Add code paths
    code:add_path("_build/default/lib/jsx/ebin"),
    code:add_path("_build/default/lib/erlmcp/ebin"),
    code:add_path("test"),
    
    %% Start required applications
    application:start(sasl),
    application:start(jsx),
    
    %% Run the integration test
    try
        erlmcp_integration_test:run(),
        halt(0)
    catch
        Class:Error:Stack ->
            io:format("Critical Error: ~p:~p~n~p~n", [Class, Error, Stack]),
            halt(1)
    end.