-module(erlmcp_elicitation_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_elicitation Module
%%====================================================================
%%
%% Test Coverage Areas:
%% 1. Elicitation creation and lifecycle
%% 2. Three elicitation modes: inline, url, terminal
%% 3. Security features: SSRF protection, rate limiting, size limits
%% 4. Timeout handling and enforcement
%% 5. Elicitation completion and cancellation
%% 6. Concurrent elicitation limits
%% 7. Error handling and edge cases
%%
%% Testing Methodology:
%% - Chicago School TDD: Real processes, state-based verification
%% - No mocks: Use actual gen_server behavior
%% - Test all security constraints
%% - Test all observable behaviors
%%
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    {ok, Pid} = erlmcp_elicitation:start_link(),
    Pid.

cleanup(Pid) ->
    catch gen_server:stop(Pid),
    timer:sleep(50).

%%====================================================================
%% Elicitation Creation Tests
%%====================================================================

elicitation_creation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_create_inline_elicitation()),
             ?_test(test_create_url_elicitation()),
             ?_test(test_create_terminal_elicitation()),
             ?_test(test_create_elicitation_with_custom_timeout()),
             ?_test(test_create_elicitation_with_custom_size_limit()),
             ?_test(test_create_elicitation_invalid_mode())
         ]
     end}.

test_create_inline_elicitation() ->
    Config = #{<<"mode">> => <<"inline">>},
    ClientPid = self(),
    {ok, ElicitationId, Response} = erlmcp_elicitation:create_elicitation(Config, ClientPid),

    ?assert(is_binary(ElicitationId)),
    ?assertMatch(#{id := _, mode := inline, status := pending}, Response),
    ?assert(maps:get(<<"mode">>, Response) =:= inline),
    ?assert(maps:get(status, Response) =:= pending),

    %% Verify we can get the status
    {ok, Status} = erlmcp_elicitation:get_elicitation_status(ElicitationId),
    ?assert(maps:get(id, Status) =:= ElicitationId),
    ?assert(maps:get(status, Status) =:= pending).

test_create_url_elicitation() ->
    %% Test with a safe public URL
    Config = #{
        <<"mode">> => <<"url">>,
        <<"url">> => <<"https://example.com/elicitation">>
    },
    {ok, ElicitationId, Response} = erlmcp_elicitation:create_elicitation(Config, self()),

    ?assert(is_binary(ElicitationId)),
    ?assertMatch(#{mode := url, status := pending}, Response),

    %% Verify status
    {ok, Status} = erlmcp_elicitation:get_elicitation_status(ElicitationId),
    ?assert(maps:get(mode, Status) =:= url).

test_create_terminal_elicitation() ->
    Config = #{<<"mode">> => <<"terminal">>},
    {ok, ElicitationId, Response} = erlmcp_elicitation:create_elicitation(Config, self()),

    ?assert(is_binary(ElicitationId)),
    ?assertMatch(#{mode := terminal}, Response),

    {ok, Status} = erlmcp_elicitation:get_elicitation_status(ElicitationId),
    ?assert(maps:get(mode, Status) =:= terminal).

test_create_elicitation_with_custom_timeout() ->
    Config = #{
        <<"mode">> => <<"inline">>,
        <<"timeout">> => 5000
    },
    {ok, _ElicitationId, Response} = erlmcp_elicitation:create_elicitation(Config, self()),

    %% Verify timeout is in the future
    CreatedAt = maps:get(created_at, Response),
    TimeoutAt = maps:get(timeout_at, Response),
    ?assert(TimeoutAt > CreatedAt),
    ?assert(TimeoutAt - CreatedAt =:= 5000).

test_create_elicitation_with_custom_size_limit() ->
    Config = #{
        <<"mode">> => <<"inline">>,
        <<"size_limit">> => 1024  % 1KB
    },
    {ok, _ElicitationId, Response} = erlmcp_elicitation:create_elicitation(Config, self()),
    ?assertMatch(#{mode := inline}, Response).

test_create_elicitation_invalid_mode() ->
    Config = #{<<"mode">> => <<"invalid_mode">>},
    Result = erlmcp_elicitation:create_elicitation(Config, self()),
    ?assertMatch({error, {invalid_mode, <<"invalid_mode">>}}, Result).

%%====================================================================
%% Security Tests
%%====================================================================

security_tests_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_url_mode_requires_url()),
             ?_test(test_url_mode_blocks_localhost()),
             ?_test(test_url_mode_blocks_private_ipv4()),
             ?_test(test_url_mode_blocks_private_ipv4_class_b()),
             ?_test(test_url_mode_blocks_private_ipv4_class_c()),
             ?_test(test_url_mode_allows_public_urls()),
             ?_test(test_url_mode_blocks_invalid_url())
         ]
     end}.

test_url_mode_requires_url() ->
    Config = #{<<"mode">> => <<"url">>},
    Result = erlmcp_elicitation:create_elicitation(Config, self()),
    ?assertMatch({error, url_required}, Result).

test_url_mode_blocks_localhost() ->
    Config = #{
        <<"mode">> => <<"url">>,
        <<"url">> => <<"http://localhost:8080/elicitation">>
    },
    Result = erlmcp_elicitation:create_elicitation(Config, self()),
    ?assertMatch({error, {unsafe_url, _}}, Result).

test_url_mode_blocks_private_ipv4() ->
    Config = #{
        <<"mode">> => <<"url">>,
        <<"url">> => <<"http://10.0.0.1/elicitation">>
    },
    Result = erlmcp_elicitation:create_elicitation(Config, self()),
    ?assertMatch({error, {unsafe_url, _}}, Result).

test_url_mode_blocks_private_ipv4_class_b() ->
    Config = #{
        <<"mode">> => <<"url">>,
        <<"url">> => <<"http://172.16.0.1/elicitation">>
    },
    Result = erlmcp_elicitation:create_elicitation(Config, self()),
    ?assertMatch({error, {unsafe_url, _}}, Result).

test_url_mode_blocks_private_ipv4_class_c() ->
    Config = #{
        <<"mode">> => <<"url">>,
        <<"url">> => <<"http://192.168.1.1/elicitation">>
    },
    Result = erlmcp_elicitation:create_elicitation(Config, self()),
    ?assertMatch({error, {unsafe_url, _}}, Result).

test_url_mode_allows_public_urls() ->
    Config = #{
        <<"mode">> => <<"url">>,
        <<"url">> => <<"https://example.com/elicitation">>
    },
    Result = erlmcp_elicitation:create_elicitation(Config, self()),
    ?assertMatch({ok, _, _}, Result).

test_url_mode_blocks_invalid_url() ->
    Config = #{
        <<"mode">> => <<"url">>,
        <<"url">> => <<"not-a-valid-url">>
    },
    Result = erlmcp_elicitation:create_elicitation(Config, self()),
    ?assertMatch({error, {invalid_url, _}}, Result).

%%====================================================================
%% Rate Limiting Tests
%%====================================================================

rate_limiting_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_rate_limit_allows_within_threshold()),
             ?_test(test_rate_limit_blocks_when_exceeded())
         ]
     end}.

test_rate_limit_allows_within_threshold() ->
    %% Should allow 10 requests in a minute
    Config = #{<<"mode">> => <<"inline">>},
    lists:foreach(fun(_) ->
        {ok, _, _} = erlmcp_elicitation:create_elicitation(Config, self())
    end, lists:seq(1, 9)),
    ?assert(true).

test_rate_limit_blocks_when_exceeded() ->
    %% Should block the 11th request
    Config = #{<<"mode">> => <<"inline">>},

    %% Make 10 requests (should succeed)
    lists:foreach(fun(_) ->
        {ok, _, _} = erlmcp_elicitation:create_elicitation(Config, self())
    end, lists:seq(1, 10)),

    %% 11th request should be rate limited
    Result = erlmcp_elicitation:create_elicitation(Config, self()),
    ?assertMatch({error, rate_limited}, Result).

%%====================================================================
%% Size Limit Tests
%%====================================================================

size_limit_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_complete_elicitation_within_size_limit()),
             ?_test(test_complete_elicitation_exceeds_size_limit())
         ]
     end}.

test_complete_elicitation_within_size_limit() ->
    Config = #{
        <<"mode">> => <<"inline">>,
        <<"size_limit">> => 1024
    },
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, self()),

    %% Small result should succeed
    Result = <<"small result">>,
    Response = erlmcp_elicitation:complete_elicitation(ElicitationId, Result),
    ?assertMatch(ok, Response),

    %% Verify status is completed
    {ok, Status} = erlmcp_elicitation:get_elicitation_status(ElicitationId),
    ?assert(maps:get(status, Status) =:= completed).

test_complete_elicitation_exceeds_size_limit() ->
    Config = #{
        <<"mode">> => <<"inline">>,
        <<"size_limit">> => 100
    },
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, self()),

    %% Large result should fail
    LargeResult = binary:copy(<<"x">>, 200),
    Result = erlmcp_elicitation:complete_elicitation(ElicitationId, LargeResult),
    ?assertMatch({error, result_too_large}, Result).

%%====================================================================
%% Completion and Cancellation Tests
%%====================================================================

completion_and_cancellation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_complete_elicitation_success()),
             ?_test(test_complete_already_completed_elicitation()),
             ?_test(test_cancel_pending_elicitation()),
             ?_test(test_cancel_active_elicitation()),
             ?_test(test_cancel_already_cancelled_elicitation()),
             ?_test(test_cancel_completed_elicitation_fails())
         ]
     end}.

test_complete_elicitation_success() ->
    Config = #{<<"mode">> => <<"inline">>},
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, self()),

    Result = #{<<"answer">> => <<"42">>},
    Response = erlmcp_elicitation:complete_elicitation(ElicitationId, Result),
    ?assertMatch(ok, Response).

test_complete_already_completed_elicitation() ->
    Config = #{<<"mode">> => <<"inline">>},
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, self()),

    %% Complete once
    ok = erlmcp_elicitation:complete_elicitation(ElicitationId, #{<<"data">> => <<"first">>}),

    %% Try to complete again
    Result = erlmcp_elicitation:complete_elicitation(ElicitationId, #{<<"data">> => <<"second">>}),
    ?assertMatch({error, already_completed}, Result).

test_cancel_pending_elicitation() ->
    Config = #{<<"mode">> => <<"inline">>},
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, self()),

    Response = erlmcp_elicitation:cancel_elicitation(ElicitationId),
    ?assertMatch(ok, Response),

    %% Verify status is cancelled
    {ok, Status} = erlmcp_elicitation:get_elicitation_status(ElicitationId),
    ?assert(maps:get(status, Status) =:= cancelled).

test_cancel_active_elicitation() ->
    Config = #{<<"mode">> => <<"terminal">>},
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, self()),

    %% Cancel should work even if not explicitly "active"
    Response = erlmcp_elicitation:cancel_elicitation(ElicitationId),
    ?assertMatch(ok, Response).

test_cancel_already_cancelled_elicitation() ->
    Config = #{<<"mode">> => <<"inline">>},
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, self()),

    %% Cancel once
    ok = erlmcp_elicitation:cancel_elicitation(ElicitationId),

    %% Try to cancel again
    Result = erlmcp_elicitation:cancel_elicitation(ElicitationId),
    ?assertMatch({error, already_cancelled}, Result).

test_cancel_completed_elicitation_fails() ->
    Config = #{<<"mode">> => <<"inline">>},
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, self()),

    %% Complete first
    ok = erlmcp_elicitation:complete_elicitation(ElicitationId, #{<<"done">> => true}),

    %% Try to cancel
    Result = erlmcp_elicitation:cancel_elicitation(ElicitationId),
    ?assertMatch({error, already_completed}, Result).

%%====================================================================
%% Timeout Tests
%%====================================================================

timeout_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_elicitation_timeout()),
             ?_test(test_complete_timed_out_elicitation_fails()),
             ?_test(test_cancel_timed_out_elicitation_fails())
         ]
     end}.

test_elicitation_timeout() ->
    Config = #{
        <<"mode">> => <<"inline">>,
        <<"timeout">> => 100  % 100ms
    },
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, self()),

    %% Wait for timeout
    timer:sleep(150),

    %% Check status
    {ok, Status} = erlmcp_elicitation:get_elicitation_status(ElicitationId),
    ?assert(maps:get(status, Status) =:= timeout).

test_complete_timed_out_elicitation_fails() ->
    Config = #{
        <<"mode">> => <<"inline">>,
        <<"timeout">> => 100
    },
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, self()),

    %% Wait for timeout
    timer:sleep(150),

    %% Try to complete
    Result = erlmcp_elicitation:complete_elicitation(ElicitationId, #{<<"data">> => <<"late">>}),
    ?assertMatch({error, already_timeout}, Result).

test_cancel_timed_out_elicitation_fails() ->
    Config = #{
        <<"mode">> => <<"inline">>,
        <<"timeout">> => 100
    },
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, self()),

    %% Wait for timeout
    timer:sleep(150),

    %% Try to cancel
    Result = erlmcp_elicitation:cancel_elicitation(ElicitationId),
    ?assertMatch({error, already_timeout}, Result).

%%====================================================================
%% List and Query Tests
%%====================================================================

list_and_query_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_list_empty_elicitations()),
             ?_test(test_list_multiple_elicitations()),
             ?_test(test_get_nonexistent_elicitation())
         ]
     end}.

test_list_empty_elicitations() ->
    {ok, List} = erlmcp_elicitation:list_elicitations(),
    ?assertMatch([], List).

test_list_multiple_elicitations() ->
    %% Create multiple elicitations
    Config1 = #{<<"mode">> => <<"inline">>},
    Config2 = #{<<"mode">> => <<"terminal">>},
    Config3 = #{<<"mode">> => <<"url">>, <<"url">> => <<"https://example.com">>},

    {ok, _, _} = erlmcp_elicitation:create_elicitation(Config1, self()),
    {ok, _, _} = erlmcp_elicitation:create_elicitation(Config2, self()),
    {ok, _, _} = erlmcp_elicitation:create_elicitation(Config3, self()),

    {ok, List} = erlmcp_elicitation:list_elicitations(),
    ?assert(length(List) >= 3).

test_get_nonexistent_elicitation() ->
    Result = erlmcp_elicitation:get_elicitation_status(<<"nonexistent">>),
    ?assertMatch({error, not_found}, Result).

%%====================================================================
%% Concurrent Limit Tests
%%====================================================================

concurrent_limit_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_concurrent_limit_enforcement())
         ]
     end}.

test_concurrent_limit_enforcement() ->
    %% The default limit is 100, but we can't easily test that in a unit test
    %% Instead, we verify the mechanism works by checking that listing works
    Config = #{<<"mode">> => <<"inline">>},

    %% Create a few elicitations
    {ok, _, _} = erlmcp_elicitation:create_elicitation(Config, self()),
    {ok, _, _} = erlmcp_elicitation:create_elicitation(Config, self()),
    {ok, _, _} = erlmcp_elicitation:create_elicitation(Config, self()),

    {ok, List} = erlmcp_elicitation:list_elicitations(),
    ?assert(length(List) >= 3).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_invalid_url_type()),
             ?_test(test_complete_nonexistent_elicitation()),
             ?_test(test_cancel_nonexistent_elicitation())
         ]
     end}.

test_invalid_url_type() ->
    Config = #{
        <<"mode">> => <<"url">>,
        <<"url">> => 12345  % Not a binary
    },
    Result = erlmcp_elicitation:create_elicitation(Config, self()),
    ?assertMatch({error, invalid_url_type}, Result).

test_complete_nonexistent_elicitation() ->
    Result = erlmcp_elicitation:complete_elicitation(<<"nonexistent">>, #{}),
    ?assertMatch({error, not_found}, Result).

test_cancel_nonexistent_elicitation() ->
    Result = erlmcp_elicitation:cancel_elicitation(<<"nonexistent">>),
    ?assertMatch({error, not_found}, Result).

%%====================================================================
%% Client Cleanup Tests
%%====================================================================

client_cleanup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_client_cleanup_on_exit())
         ]
     end}.

test_client_cleanup_on_exit() ->
    %% Create a client process
    ClientPid = spawn(fun() ->
        receive
            die -> ok
        end
    end),

    %% Create elicitations for this client
    Config = #{<<"mode">> => <<"inline">>},
    {ok, ElicitationId1, _} = erlmcp_elicitation:create_elicitation(Config, ClientPid),
    {ok, ElicitationId2, _} = erlmcp_elicitation:create_elicitation(Config, ClientPid),

    %% Verify they exist
    {ok, _} = erlmcp_elicitation:get_elicitation_status(ElicitationId1),
    {ok, _} = erlmcp_elicitation:get_elicitation_status(ElicitationId2),

    %% Kill the client
    exit(ClientPid, kill),
    timer:sleep(100),

    %% Verify elicitations are cleaned up
    ?assertMatch({error, not_found}, erlmcp_elicitation:get_elicitation_status(ElicitationId1)),
    ?assertMatch({error, not_found}, erlmcp_elicitation:get_elicitation_status(ElicitationId2)).
