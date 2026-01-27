#!/usr/bin/env escript
%%! -pa _build/test/lib/*/ebin -pa _build/default/lib/*/ebin

%%%-------------------------------------------------------------------
%%% @doc Mock Services Verification Script
%%%
%%% Quick verification that all mock services can start, respond to
%%% requests, and shutdown cleanly.
%%%
%%% Usage:
%%%   escript test/integration/verify_mock_services.erl
%%%
%%% @end
%%%-------------------------------------------------------------------

main([]) ->
    io:format("~n=== TCPS Mock Services Verification ===~n~n"),

    %% Test 1: Start all services
    io:format("1. Starting all mock services...~n"),
    case tcps_mock_services:start_all() of
        {ok, Services} ->
            io:format("   ✓ All services started successfully~n"),
            io:format("   Services: ~p~n", [maps:keys(Services)]),
            io:format("   Ports: ~p~n~n", [maps:get(ports, Services)]),

            %% Test 2: Inject test data
            io:format("2. Injecting test data...~n"),
            test_data_injection(),
            io:format("   ✓ Test data injected successfully~n~n"),

            %% Test 3: Verify service calls
            io:format("3. Testing service calls...~n"),
            test_service_calls(),
            io:format("   ✓ Service calls working correctly~n~n"),

            %% Test 4: Reset state
            io:format("4. Testing state reset...~n"),
            ok = tcps_mock_services:reset_all(),
            io:format("   ✓ State reset successfully~n~n"),

            %% Test 5: Stop services
            io:format("5. Stopping all services...~n"),
            ok = tcps_mock_services:stop_all(Services),
            io:format("   ✓ All services stopped successfully~n~n"),

            io:format("=== All Tests Passed ===~n~n"),
            halt(0);

        {error, Reason} ->
            io:format("   ✗ Failed to start services: ~p~n", [Reason]),
            halt(1)
    end;

main(_Args) ->
    io:format("Usage: escript verify_mock_services.erl~n"),
    halt(1).

test_data_injection() ->
    %% Inject GitHub issue
    ok = tcps_mock_services:inject_github_issue(#{
        repo => <<"test/repo">>,
        number => 1234,
        title => <<"Test Issue">>,
        labels => [<<"bug">>, <<"test">>]
    }),

    %% Inject CVE advisory
    ok = tcps_mock_services:inject_cve_advisory(#{
        cve_id => <<"CVE-2024-TEST">>,
        severity => high,
        cvss_score => 7.5
    }),

    %% Inject marketplace feature
    ok = tcps_mock_services:inject_marketplace_feature(#{
        id => <<"feature-test-001">>,
        title => <<"Test Feature">>,
        upvotes => 42
    }),

    ok.

test_service_calls() ->
    %% Test GitHub calls
    GitHubCalls = tcps_mock_services:get_github_calls(),
    io:format("   - GitHub calls: ~p~n", [length(GitHubCalls)]),

    %% Test Marketplace calls
    MarketplaceCalls = tcps_mock_services:get_marketplace_calls(),
    io:format("   - Marketplace calls: ~p~n", [length(MarketplaceCalls)]),

    %% Test CVE calls
    CveCalls = tcps_mock_services:get_cve_calls(),
    io:format("   - CVE calls: ~p~n", [length(CveCalls)]),

    %% Test OTLP spans
    OtlpSpans = tcps_mock_services:get_otlp_spans(),
    io:format("   - OTLP spans: ~p~n", [length(OtlpSpans)]),

    ok.
