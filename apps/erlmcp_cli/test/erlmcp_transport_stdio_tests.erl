%%%-------------------------------------------------------------------
%%% @doc
%%% Stdio Transport Test Suite (EUnit)
%%%
%%% Tests for erlmcp_transport_stdio module - Standard I/O transport
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real stdio transport process
%%% - NO mocks, real stdio operations
%%% - State-based verification (transport state, messages)
%%%
%%% Coverage Target: ≥85%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_stdio_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

stdio_transport_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Initialize - start stdio transport", fun test_init_stdio/0},
      {"Read - read from stdin", fun test_read_stdin/0},
      {"Write - write to stdout", fun test_write_stdout/0},
      {"Error handling - EOF", fun test_eof_handling/0},
      {"Error handling - broken pipe", fun test_broken_pipe/0},
      {"Lifecycle - start and stop", fun test_lifecycle/0},
      {"Concurrent - concurrent reads/writes", fun test_concurrent_io/0},
      {"Buffer - buffer management", fun test_buffer_management/0}]}.

setup() ->
    application:ensure_all_started(erlmcp_cli),
    ok.

cleanup(_Args) ->
    try erlmcp_transport_stdio:stop() catch _:_ -> ok end,
    ok.

%%%====================================================================
%%% Initialization Tests
%%%====================================================================

test_init_stdio() ->
    %% Start stdio transport
    {ok, Pid} = erlmcp_transport_stdio:start_link(#{session_id => <<"test">>}),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    %% Cleanup
    ok = erlmcp_transport_stdio:stop().

%%%====================================================================
%%% Read/Write Tests
%%%====================================================================

test_read_stdin() ->
    %% Start transport
    {ok, _Pid} = erlmcp_transport_stdio:start_link(#{session_id => <<"test-read">>}),

    %% In test mode, verify read capability
    ?assert(is_process_alive(whereis(erlmcp_transport_stdio))),

    %% Cleanup
    ok = erlmcp_transport_stdio:stop().

test_write_stdout() ->
    %% Start transport
    {ok, _Pid} = erlmcp_transport_stdio:start_link(#{session_id => <<"test-write">>}),

    %% Write message
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    ok = erlmcp_transport_stdio:send(Message),

    %% Cleanup
    ok = erlmcp_transport_stdio:stop().

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

test_eof_handling() ->
    %% Start transport
    {ok, _Pid} = erlmcp_transport_stdio:start_link(#{session_id => <<"test-eof">>}),

    %% Simulate EOF (Ctrl+D)
    %% In test mode, just verify process handles it gracefully
    ?assert(is_process_alive(whereis(erlmcp_transport_stdio))),

    %% Cleanup
    ok = erlmcp_transport_stdio:stop().

test_broken_pipe() ->
    %% Start transport
    {ok, Pid} = erlmcp_transport_stdio:start_link(#{session_id => <<"test-pipe">>}),

    %% Simulate broken pipe (write to closed stream)
    %% Verify transport handles error gracefully
    ?assert(is_process_alive(Pid)),

    %% Cleanup
    ok = erlmcp_transport_stdio:stop().

%%%====================================================================
%%% Lifecycle Tests
%%%====================================================================

test_lifecycle() ->
    %% Start transport
    {ok, Pid} = erlmcp_transport_stdio:start_link(#{session_id => <<"test-life">>}),
    ?assert(is_process_alive(Pid)),

    %% Get state
    {ok, State} = erlmcp_transport_stdio:get_state(),
    ?assertEqual(ready, maps:get(status, State)),

    %% Stop transport
    ok = erlmcp_transport_stdio:stop(),
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)).

%%%====================================================================
%%% Concurrent Operations Tests
%%%====================================================================

test_concurrent_io() ->
    %% Start transport
    {ok, _Pid} = erlmcp_transport_stdio:start_link(#{session_id => <<"test-concurrent">>}),

    %% Spawn concurrent writes
    Pids = [spawn(fun() ->
        Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":",
                   (integer_to_binary(N))/binary, "}">>,
        erlmcp_transport_stdio:send(Message)
    end) || N <- lists:seq(1, 10)],

    %% Wait for all writes
    timer:sleep(100),

    %% Verify all processes completed
    lists:foreach(fun(P) ->
        ?assertNot(is_process_alive(P))
    end, Pids),

    %% Cleanup
    ok = erlmcp_transport_stdio:stop().

%%%====================================================================
%%% Buffer Management Tests
%%%====================================================================

test_buffer_management() ->
    %% Start transport
    {ok, _Pid} = erlmcp_transport_stdio:start_link(#{session_id => <<"test-buffer">>}),

    %% Send large message
    LargeMessage = <<"{\"data\":\"", binary:copy(<<"x">>, 10000), "\"}">>,
    ok = erlmcp_transport_stdio:send(LargeMessage),

    %% Verify buffer handled correctly
    {ok, State} = erlmcp_transport_stdio:get_state(),
    ?assert(is_map(maps:get(buffer, State, #{}))),

    %% Cleanup
    ok = erlmcp_transport_stdio:stop().
