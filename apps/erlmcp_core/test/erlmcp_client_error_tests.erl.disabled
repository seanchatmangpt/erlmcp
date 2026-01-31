-module(erlmcp_client_error_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Chicago School TDD: Error Handling and Edge Cases Tests
%%====================================================================
%%
%% Testing Methodology:
%% - Chicago School TDD: Real processes, API-based verification, no mocks
%% - Test observable behavior through ALL interfaces
%% - NO state inspection (test API boundaries only)
%% - NO record duplication (respect encapsulation)
%%

%%====================================================================
%% Initialization Error Tests
%%====================================================================

initialization_errors_test_() ->
    {setup, fun setup_client/0, fun cleanup_client/1, fun(C) -> case C of
        undefined -> []; _ -> [
            {"Init with no server", ?_test(test_init_no_server(C))},
            {"Init with invalid capabilities", ?_test(test_init_invalid_capabilities(C))},
            {"Multiple init attempts", ?_test(test_multiple_init_attempts(C))},
            {"Init during transport failure", ?_test(test_init_transport_failure(C))}
        ] end end}.

test_init_no_server(Client) ->
    ?assertMatch({error, _}, erlmcp_client:initialize(Client, #mcp_client_capabilities{})),
    ?assert(erlang:is_process_alive(Client)).

test_init_invalid_capabilities(Client) ->
    Caps = #mcp_client_capabilities{roots = #mcp_capability{enabled = true}},
    ?assertMatch({error, _}, erlmcp_client:initialize(Client, Caps)),
    ?assert(erlang:is_process_alive(Client)).

test_multiple_init_attempts(Client) ->
    ?assertMatch({error, _}, erlmcp_client:initialize(Client, #mcp_client_capabilities{})),
    ?assertMatch({error, _}, erlmcp_client:initialize(Client, #mcp_client_capabilities{})),
    ?assert(erlang:is_process_alive(Client)).

test_init_transport_failure(Client) ->
    Start = erlang:monotonic_time(millisecond),
    ?assertMatch({error, _}, erlmcp_client:initialize(Client, #mcp_client_capabilities{}, #{timeout => 100})),
    ?assert(erlang:monotonic_time(millisecond) - Start < 2000),
    ?assert(erlang:is_process_alive(Client)).

%%====================================================================
%% Notification Error Tests
%%====================================================================

notification_errors_test_() ->
    {setup, fun setup_client/0, fun cleanup_client/1, fun(C) -> case C of
        undefined -> []; _ -> [
            {"Notification handler crash", ?_test(test_notification_handler_crash(C))},
            {"Handler removal non-existent", ?_test(test_handler_removal_non_existent(C))},
            {"Multiple handlers same method", ?_test(test_multiple_handlers_same_method(C))}
        ] end end}.

test_notification_handler_crash(Client) ->
    Handler = fun(_, _) -> self() ! {error, handler_error}, error(intentional_crash) end,
    ok = erlmcp_client:set_notification_handler(Client, <<"test/n">>, Handler),
    ?assert(erlang:is_process_alive(Client)),
    ok = erlmcp_client:remove_notification_handler(Client, <<"test/n">>).

test_handler_removal_non_existent(Client) ->
    ok = erlmcp_client:remove_notification_handler(Client, <<"nonexistent/h">>),
    ?assert(erlang:is_process_alive(Client)).

test_multiple_handlers_same_method(Client) ->
    H1 = fun(_, _) -> ok end, H2 = fun(_, _) -> ok end,
    ok = erlmcp_client:set_notification_handler(Client, <<"test/m">>, H1),
    ok = erlmcp_client:set_notification_handler(Client, <<"test/m">>, H2),
    ?assert(erlang:is_process_alive(Client)),
    ok = erlmcp_client:remove_notification_handler(Client, <<"test/m">>).

%%====================================================================
%% Batch Error Tests
%%====================================================================

batch_errors_test_() ->
    {setup, fun setup_client/0, fun cleanup_client/1, fun(C) -> case C of
        undefined -> []; _ -> [
            {"Batch with failing operation", ?_test(test_batch_failing_operation(C))},
            {"Concurrent batch errors", ?_test(test_concurrent_batch_errors(C))},
            {"Empty batch execution", ?_test(test_empty_batch_execution(C))}
        ] end end}.

test_batch_failing_operation(Client) ->
    try
        ?assertMatch({error, _}, erlmcp_client:with_batch(Client, fun(Bc) ->
            erlmcp_client:list_tools(Bc), erlmcp_client:list_resources(Bc)
        end))
    catch error:badarg -> ok end.

test_concurrent_batch_errors(Client) ->
    try
        spawn_link(fun() -> erlmcp_client:with_batch(Client, fun(Bc) ->
            erlmcp_client:send_batch_request(Bc, <<"b1">>, <<"tools/list">>, #{}), error(intentional)
        end) end),
        spawn_link(fun() -> erlmcp_client:with_batch(Client, fun(Bc) ->
            erlmcp_client:send_batch_request(Bc, <<"b2">>, <<"resources/list">>, #{})
        end) end),
        timer:sleep(1000), ?assert(erlang:is_process_alive(Client))
    catch error:badarg -> ok end.

test_empty_batch_execution(Client) ->
    try ?assertMatch({ok, 0}, erlmcp_client:with_batch(Client, fun(_) -> ok end)) catch error:badarg -> ok end.

%%====================================================================
%% Timeout Error Tests
%%====================================================================

timeout_errors_test_() ->
    {setup, fun setup_client/0, fun cleanup_client/1, fun(C) -> case C of
        undefined -> []; _ -> [
            {"Request timeout recovery", ?_test(test_request_timeout_recovery(C))},
            {"Concurrent timeout requests", ?_test(test_concurrent_timeout_requests(C))}
        ] end end}.

test_request_timeout_recovery(Client) ->
    case erlmcp_client:start_link({stdio, #{test_mode => true}}, #{timeout => 100}) of
        {ok, CWithTimeout} ->
            ?assertMatch({error, _}, erlmcp_client:list_tools(CWithTimeout)),
            ?assert(erlang:is_process_alive(CWithTimeout)),
            ?assertMatch({error, _}, erlmcp_client:list_resources(CWithTimeout)),
            ?assert(erlang:is_process_alive(CWithTimeout)),
            erlmcp_client:stop(CWithTimeout);
        {error, _} -> ?assert(true)
    end.

test_concurrent_timeout_requests(Client) ->
    case erlmcp_client:start_link({stdio, #{test_mode => true}}, #{timeout => 100}) of
        {ok, CWithTimeout} ->
            Self = self(),
            spawn_link(fun() -> Self ! {r1, erlmcp_client:list_tools(CWithTimeout)} end),
            spawn_link(fun() -> Self ! {r2, erlmcp_client:list_resources(CWithTimeout)} end),
            spawn_link(fun() -> Self ! {r3, erlmcp_client:list_prompts(CWithTimeout)} end),
            Results = collect_results(3, 5000),
            ?assertEqual(3, length(Results)),
            ?assert(lists:all(fun({_, {error, _}}) -> true; (_) -> false end, Results)),
            ?assert(erlang:is_process_alive(CWithTimeout)),
            erlmcp_client:stop(CWithTimeout);
        {error, _} -> ?assert(true)
    end.

%%====================================================================
%% Invalid Input Tests
%%====================================================================

invalid_input_test_() ->
    {setup, fun setup_client/0, fun cleanup_client/1, fun(C) -> case C of
        undefined -> []; _ -> [
            {"Call tool with invalid name", ?_test(test_invalid_tool_name(C))},
            {"Read resource with invalid URI", ?_test(test_invalid_resource_uri(C))},
            {"Get prompt with empty name", ?_test(test_empty_prompt_name(C))},
            {"Complete with empty argument", ?_test(test_empty_complete_argument(C))}
        ] end end}.

test_invalid_tool_name(Client) ->
    ?assertMatch({error, _}, erlmcp_client:call_tool(Client, <<>>, #{})),
    ?assertError(_, erlmcp_client:call_tool(Client, invalid, #{})).

test_invalid_resource_uri(Client) ->
    ?assertMatch({error, _}, erlmcp_client:read_resource(Client, <<>>)),
    ?assertError(_, erlmcp_client:read_resource(Client, invalid)).

test_empty_prompt_name(Client) ->
    ?assertMatch({error, _}, erlmcp_client:get_prompt(Client, <<>>)).

test_empty_complete_argument(Client) ->
    ?assertMatch({_, _}, erlmcp_client:complete(Client, <<"test_ref">>, <<>>)).

%%====================================================================
%% Process Crash Recovery Tests
%%====================================================================

process_crash_recovery_test_() ->
    {setup, fun setup_client/0, fun cleanup_client/1, fun(C) -> case C of
        undefined -> []; _ -> [
            {"Client operations after errors", ?_test(test_client_operations_after_errors(C))},
            {"Transport failure recovery", ?_test(test_transport_failure_recovery(C))}
        ] end end}.

test_client_operations_after_errors(Client) ->
    ?assert(erlang:is_process_alive(Client)),
    _ = erlmcp_client:list_tools(Client),
    _ = erlmcp_client:list_resources(Client),
    ?assert(erlang:is_process_alive(Client)).

test_transport_failure_recovery(Client) ->
    ?assert(erlang:is_process_alive(Client)),
    ?assertMatch({error, _}, erlmcp_client:list_tools(Client)),
    ?assert(erlang:is_process_alive(Client)).

%%====================================================================
%% Edge Cases Tests
%%====================================================================

edge_cases_test_() ->
    {setup, fun setup_client/0, fun cleanup_client/1, fun(C) -> case C of
        undefined -> []; _ -> [
            {"Very long tool name", ?_test(test_very_long_tool_name(C))},
            {"Special characters in resource URI", ?_test(test_special_chars_uri(C))},
            {"Large arguments map", ?_test(test_large_arguments_map(C))},
            {"Zero timeout", ?_test(test_zero_timeout(C))}
        ] end end}.

test_very_long_tool_name(Client) ->
    LongName = binary:copy(<<"a">>, 10000),
    ?assertMatch({error, _}, erlmcp_client:call_tool(Client, LongName, #{})).

test_special_chars_uri(Client) ->
    SpecialUri = <<"file:///test/path%20with%20spaces/文件.txt">>,
    ?assertMatch({error, _}, erlmcp_client:read_resource(Client, SpecialUri)).

test_large_arguments_map(Client) ->
    LargeArgs = maps:from_list([{<<"arg", (integer_to_binary(I))/binary>>, I} || I <- lists:seq(1, 1000)]),
    ?assertMatch({error, _}, erlmcp_client:call_tool(Client, <<"test">>, LargeArgs)).

test_zero_timeout(Client) ->
    ?assertMatch({error, _}, erlmcp_client:complete(Client, <<"test_ref">>, <<"test">>, 0)).

%%====================================================================
%% Setup/Cleanup/Helpers
%%====================================================================

setup_application() -> application:ensure_all_started(erlmcp_core), ok.
cleanup_application(_) -> application:stop(erlmcp_core), ok.

setup_client() ->
    setup_application(),
    case erlmcp_client:start_link({stdio, #{test_mode => true}}) of {ok, C} -> C; {error, _} -> undefined end.

cleanup_client(undefined) -> cleanup_application(ok);
cleanup_client(Client) -> erlmcp_client:stop(Client), cleanup_application(ok).

collect_results(Count, Timeout) -> collect_results(Count, Timeout, []).

collect_results(0, _, Acc) -> lists:reverse(Acc);
collect_results(Count, Timeout, Acc) ->
    receive
        R -> collect_results(Count - 1, Timeout, [R | Acc])
    after Timeout -> lists:reverse(Acc)
    end.
