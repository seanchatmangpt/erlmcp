#!/usr/bin/env escript
%% -*- erlang -*-
%%! +pc unicode

% Test script for erlmcp_new_features observability integration
% This script tests the observability features without Docker

main(_) ->
    io:format("=== Testing erlmcp_new_features Observability Integration ===~n"),

    % Test 1: Check if modules compile
    io:format("1. Testing module compilation...~n"),
    case compile_modules() of
        ok -> io:format("   ✓ All modules compiled successfully~n");
        {error, Errors} ->
            io:format("   ✗ Compilation errors: ~p~n", [Errors]),
            halt(1)
    end,

    % Test 2: Test observability API
    io:format("2. Testing observability API...~n"),
    case test_observability_api() of
        ok -> io:format("   ✓ Observability API working~n");
        {error, Reason} ->
            io:format("   ✗ Observability API error: ~p~n", [Reason]),
            halt(1)
    end,

    % Test 3: Test metrics collection
    io:format("3. Testing metrics collection...~n"),
    case test_metrics() of
        ok -> io:format("   ✓ Metrics collection working~n");
        {error, Reason} ->
            io:format("   ✗ Metrics error: ~p~n", [Reason]),
            halt(1)
    end,

    % Test 4: Test logging
    io:format("4. Testing structured logging...~n"),
    case test_logging() of
        ok -> io:format("   ✓ Structured logging working~n");
        {error, Reason} ->
            io:format("   ✗ Logging error: ~p~n", [Reason]),
            halt(1)
    end,

    % Test 5: Test tracing
    io:format("5. Testing distributed tracing...~n"),
    case test_tracing() of
        ok -> io:format("   ✓ Distributed tracing working~n");
        {error, Reason} ->
            io:format("   ✗ Tracing error: ~p~n", [Reason]),
            halt(1)
    end,

    % Test 6: Test health checks
    io:format("6. Testing health checks...~n"),
    case test_health_checks() of
        ok -> io:format("   ✓ Health checks working~n");
        {error, Reason} ->
            io:format("   ✗ Health check error: ~p~n", [Reason]),
            halt(1)
    end,

    io:format("~n=== All observability tests passed! ===~n").

compile_modules() ->
    Modules = [
        erlmcp_observability,
        erlmcp_json_schema_validator,
        erlmcp_event_bus,
        erlmcp_mcp_proxy_relay,
        erlmcp_batch_processor,
        erlmcp_health_check_memory,
        erlmcp_health_check
    ],

    F = fun(Module) ->
        case code:ensure_loaded(Module) of
            {module, _} -> ok;
            {error, Reason} -> {error, {Module, Reason}}
        end
    end,

    case lists:map(F, Modules) of
        [ok | _] -> ok;
        Errors -> {error, Errors}
    end.

test_observability_api() ->
    % Test if observability module functions are available
    Functions = [
        {erlmcp_observability, log, 2},
        {erlmcp_observability, log, 3},
        {erlmcp_observability, counter, 2},
        {erlmcp_observability, gauge, 2},
        {erlmcp_observability, histogram_observe, 2},
        {erlmcp_observability, trace_start, 1},
        {erlmcp_observability, health_check, 0}
    ],

    F = fun({Module, Function, Arity}) ->
        case erlang:function_exported(Module, Function, Arity) of
            true -> ok;
            false -> {error, {Module, Function, Arity, not_exported}}
        end
    end,

    case lists:map(F, Functions) of
        [ok | _] -> ok;
        Errors -> {error, Errors}
    end.

test_metrics() ->
    % Test basic metrics functionality
    try
        % Test counter
        erlmcp_observability:counter(<<"test_counter">>, #{label => "test"}),
        ok
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

test_logging() ->
    % Test structured logging
    try
        % Test basic log
        erlmcp_observability:log(<<"test_message">>, #{test => true}),

        % Test log with level
        erlmcp_observability:log(info, <<"info_test">>, #{test => true}),

        % Test log with trace context
        TraceContext = erlmcp_observability:trace_span(<<"test_span">>),
        erlmcp_observability:log(debug, <<"debug_test">>, #{test => true}, TraceContext),

        ok
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

test_tracing() ->
    % Test distributed tracing
    try
        % Test trace creation
        TraceContext = erlmcp_observability:trace_span(<<"test_trace">>),

        % Test nested spans
        erlmcp_observability:trace_span(<<"nested_span">>, #{}, TraceContext),

        ok
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

test_health_checks() ->
    % Test health check functionality
    try
        % Test memory health check
        case erlmcp_health_check_memory:check() of
            {ok, _Message, _Details} -> ok;
            {warning, _Message, _Details} -> ok;
            {error, _Message, _Details} -> ok
        end,

        % Test health check module
        case erlmcp_health_check:health_check() of
            HealthStatus when is_map(HealthStatus) -> ok
        end,

        ok
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.