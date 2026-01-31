#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin -pa bench

main(["tcp"]) ->
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_transports),
    Result = erlmcp_bench_resource_leak:run_tcp_socket_leak(),
    io:format("~n~nTest complete.~n");
main(["ets"]) ->
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_transports),
    Result = erlmcp_bench_resource_leak:run_ets_table_leak(),
    io:format("~n~nTest complete.~n");
main(["process"]) ->
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_transports),
    Result = erlmcp_bench_resource_leak:run_process_leak(),
    io:format("~n~nTest complete.~n");
main(["binary"]) ->
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_transports),
    Result = erlmcp_bench_resource_leak:run_binary_leak(),
    io:format("~n~nTest complete.~n");
main(["mailbox"]) ->
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_transports),
    Result = erlmcp_bench_resource_leak:run_mailbox_leak(),
    io:format("~n~nTest complete.~n");
main(["full"]) ->
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_transports),
    Result = erlmcp_bench_resource_leak:run_full_leak_test(),
    io:format("~n~nTest complete.~n");
main(["all"]) ->
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_transports),
    {ok, Results} = erlmcp_bench_resource_leak:run_all(),
    io:format("~n~nAll tests complete.~n");
main(_) ->
    io:format("Usage: run_resource_leak_test.erl [tcp|ets|process|binary|mailbox|full|all]~n"),
    io:format("  tcp     - TCP socket leak test~n"),
    io:format("  ets     - ETS table leak test~n"),
    io:format("  process - Process leak test~n"),
    io:format("  binary  - Binary leak test~n"),
    io:format("  mailbox - Mailbox leak test~n"),
    io:format("  full    - Full leak test (all types)~n"),
    io:format("  all     - Run all leak tests~n").
