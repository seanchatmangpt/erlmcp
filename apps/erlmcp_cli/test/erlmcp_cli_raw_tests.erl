%%%-------------------------------------------------------------------
%%% @doc EUnit tests for erlmcp_cli_raw (real exported API).
%%%
%%% Rewritten from pseudo-code that ended in ?FORALL/3 property tests
%%% without a proper include (the proper library is not a repo
%%% dependency). The character/buffer/history behavior specs mapped
%%% cleanly onto the real module, so they are preserved as real
%%% state-based tests; the property section is replaced by a
%%% deterministic sampled version.
%%%
%%% Real API under test (apps/erlmcp_cli/src/erlmcp_cli_raw.erl):
%%%   start_link/0, start_link/1, stop/0, start_raw_session/0,
%%%   send_char/1, inspect_context/0, cancel_operation/0,
%%%   get_stats/0, get_state/0, execute_command/1
%%%
%%% WARNING (real-collaborator note): start_link/1 enables kernel
%%% terminal raw mode on stdin. Under a test runner without a tty the
%%% lifecycle tests below surface that as a real error, not a fake
%%% pass.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_raw_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

buffer_accumulates_printable_chars_test() ->
    {ok, _} = erlmcp_cli_raw:start_link(#{test_mode => true}),
    lists:foreach(fun(C) -> ok = erlmcp_cli_raw:send_char(C) end,
                  "hello"),
    {ok, State} = erlmcp_cli_raw:get_state(),
    ?assertMatch(#{buffer := <<"hello">>}, State),
    ok = erlmcp_cli_raw:stop().

backspace_removes_last_char_test() ->
    {ok, _} = erlmcp_cli_raw:start_link(#{test_mode => true}),
    lists:foreach(fun(C) -> ok = erlmcp_cli_raw:send_char(C) end, "he"),
    ok = erlmcp_cli_raw:send_char(8), %% backspace
    {ok, State} = erlmcp_cli_raw:get_state(),
    ?assertMatch(#{buffer := <<"h">>}, State),
    ok = erlmcp_cli_raw:stop().

carriage_return_submits_or_resets_buffer_test() ->
    {ok, _} = erlmcp_cli_raw:start_link(#{test_mode => true}),
    lists:foreach(fun(C) -> ok = erlmcp_cli_raw:send_char(C) end, "ping"),
    ok = erlmcp_cli_raw:send_char(13), %% CR
    {ok, State} = erlmcp_cli_raw:get_state(),
    %% After submit the input buffer is emptied (command moved to history
    %% or executed); assert the buffer did not keep growing.
    Buffer = maps:get(buffer, State),
    ?assert(byte_size(Buffer) < 4),
    ok = erlmcp_cli_raw:stop().

get_state_shape_test() ->
    {ok, _} = erlmcp_cli_raw:start_link(#{test_mode => true}),
    {ok, State} = erlmcp_cli_raw:get_state(),
    ?assert(is_map(State)),
    ?assert(maps:is_key(buffer, State)),
    ?assert(maps:is_key(history, State)),
    ok = erlmcp_cli_raw:stop().

get_stats_returns_map_test() ->
    {ok, _} = erlmcp_cli_raw:start_link(#{test_mode => true}),
    ?assertMatch({ok, _}, erlmcp_cli_raw:get_stats()),
    ok = erlmcp_cli_raw:stop().

inspect_context_returns_map_test() ->
    {ok, _} = erlmcp_cli_raw:start_link(#{test_mode => true}),
    ?assertMatch({ok, _}, erlmcp_cli_raw:inspect_context()),
    ok = erlmcp_cli_raw:stop().

stop_kills_registered_process_test() ->
    {ok, _} = erlmcp_cli_raw:start_link(#{test_mode => true}),
    ?assert(is_pid(whereis(erlmcp_cli_raw))),
    ok = erlmcp_cli_raw:stop(),
    timer:sleep(50),
    ?assertEqual(undefined, whereis(erlmcp_cli_raw)).

execute_command_returns_result_or_error_test() ->
    %% execute_command/1 against the live server: result must be a
    %% tagged tuple, never a crash of the caller.
    {ok, _} = erlmcp_cli_raw:start_link(#{test_mode => true}),
    Result = erlmcp_cli_raw:execute_command(<<"ping">>),
    ?assert(is_tuple(Result)),
    ?assert(lists:member(element(1, Result), [ok, error])),
    ok = erlmcp_cli_raw:stop().

%% Deterministic replacement for the broken ?FORALL property
%% prop_char_sequence: fixed sample of printable ASCII.
char_sequence_sample_property_test() ->
    Samples = ["abc", "a b c", "~!@#$%^&*()_+", [32], "zz"],
    lists:foreach(fun(Chars) ->
                          {ok, _} = erlmcp_cli_raw:start_link(#{test_mode => true}),
                          lists:foreach(fun(C) -> ok = erlmcp_cli_raw:send_char(C) end, Chars),
                          {ok, State} = erlmcp_cli_raw:get_state(),
                          ?assertEqual(iolist_to_binary(Chars), maps:get(buffer, State)),
                          ok = erlmcp_cli_raw:stop()
                  end, Samples).
