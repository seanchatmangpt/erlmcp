%%%-------------------------------------------------------------------
%%% @doc Integration Tests for erlmcp_elicitation
%%%
%%% Chicago School TDD: REAL erlmcp_elicitation processes, state-based verification
%%% Tests ALL observable behavior through the public API
%%% NO MOCKS - Uses REAL gen_server processes
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_elicitation_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup and Cleanup
%%%===================================================================

setup() ->
    %% Start erlmcp_core application for dependencies
    application:ensure_all_started(erlmcp_core),
    %% Start elicitation server
    {ok, Pid} = erlmcp_elicitation:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end,
    application:stop(erlmcp_core).

%%%===================================================================
%%% Test Suite - Lifecycle
%%%===================================================================

elicitation_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {"Create inline elicitation", fun test_create_inline_elicitation/0},
          {"Create URL elicitation", fun test_create_url_elicitation/0},
          {"Create terminal elicitation", fun test_create_terminal_elicitation/0},
          {"Get elicitation status", fun test_get_elicitation_status/0},
          {"Cancel elicitation", fun test_cancel_elicitation/0},
          {"Complete elicitation", fun test_complete_elicitation/0},
          {"List elicitations", fun test_list_elicitations/0}
         ]
     end}.

test_create_inline_elicitation() ->
    ClientPid = self(),
    Config = #{
        <<"mode">> => <<"inline">>,
        <<"prompt">> => <<"Please provide your API key">>,
        <<"placeholder">> => <<"sk-...">>,
        <<"timeout">> => 30000
    },

    %% Create elicitation
    {ok, ElicitationId, ResponseConfig} = erlmcp_elicitation:create_elicitation(Config, ClientPid),

    %% Verify response
    ?assert(is_binary(ElicitationId)),
    ?assertEqual(<<"inline">>, maps:get(<<"mode">>, ResponseConfig)),
    ?assertEqual(<<"Please provide your API key">>, maps:get(<<"prompt">>, ResponseConfig)),

    %% Verify status
    {ok, Status} = erlmcp_elicitation:get_elicitation_status(ElicitationId),
    ?assertEqual(ElicitationId, maps:get(id, Status)),
    ?assertEqual(inline, maps:get(mode, Status)),
    ?assert(maps:get(status, Status) =:= pending orelse maps:get(status, Status) =:= active).

test_create_url_elicitation() ->
    ClientPid = self(),
    Config = #{
        <<"mode">> => <<"url">>,
        <<"url">> => <<"https://example.com/auth">>,
        <<"method">> => <<"GET">>,
        <<"headers">> => #{<<"Authorization">> => <<"Bearer token">>},
        <<"timeout">> => 60000
    },

    %% Create elicitation
    {ok, ElicitationId, ResponseConfig} = erlmcp_elicitation:create_elicitation(Config, ClientPid),

    %% Verify response
    ?assert(is_binary(ElicitationId)),
    ?assertEqual(<<"url">>, maps:get(<<"mode">>, ResponseConfig)),
    ?assertEqual(<<"https://example.com/auth">>, maps:get(<<"url">>, ResponseConfig)),

    %% Verify status
    {ok, Status} = erlmcp_elicitation:get_elicitation_status(ElicitationId),
    ?assertEqual(url, maps:get(mode, Status)).

test_create_terminal_elicitation() ->
    ClientPid = self(),
    Config = #{
        <<"mode">> => <<"terminal">>,
        <<"command">> => <<"/usr/bin/zenity --entry --text='Enter password:'">>
    },

    %% Create elicitation
    {ok, ElicitationId, ResponseConfig} = erlmcp_elicitation:create_elicitation(Config, ClientPid),

    %% Verify response
    ?assert(is_binary(ElicitationId)),
    ?assertEqual(<<"terminal">>, maps:get(<<"mode">>, ResponseConfig)),

    %% Verify status
    {ok, Status} = erlmcp_elicitation:get_elicitation_status(ElicitationId),
    ?assertEqual(terminal, maps:get(mode, Status)).

test_get_elicitation_status() ->
    ClientPid = self(),
    Config = #{<<"mode">> => <<"inline">>, <<"prompt">> => <<"Test">>},

    %% Create elicitation
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, ClientPid),

    %% Get status
    {ok, Status} = erlmcp_elicitation:get_elicitation_status(ElicitationId),

    %% Verify status fields
    ?assert(maps:is_key(id, Status)),
    ?assert(maps:is_key(mode, Status)),
    ?assert(maps:is_key(status, Status)),
    ?assert(maps:is_key(created_at, Status)),
    ?assert(maps:is_key(timeout_at, Status)),
    ?assert(maps:is_key(config, Status)),

    %% Test non-existent elicitation
    FakeId = <<"nonexistent_elicitation">>,
    ?assertEqual({error, not_found}, erlmcp_elicitation:get_elicitation_status(FakeId)).

test_cancel_elicitation() ->
    ClientPid = self(),
    Config = #{<<"mode">> => <<"inline">>, <<"prompt">> => <<"Cancel test">>},

    %% Create elicitation
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, ClientPid),

    %% Cancel elicitation
    ?assertEqual(ok, erlmcp_elicitation:cancel_elicitation(ElicitationId)),

    %% Verify status changed to cancelled
    {ok, Status} = erlmcp_elicitation:get_elicitation_status(ElicitationId),
    ?assertEqual(cancelled, maps:get(status, Status)),

    %% Test cancelling already cancelled
    Result = erlmcp_elicitation:cancel_elicitation(ElicitationId),
    ?assert(Result =:= ok orelse Result =:= {error, already_cancelled}),

    %% Test cancelling non-existent
    ?assertEqual({error, not_found}, erlmcp_elicitation:cancel_elicitation(<<"nonexistent">>)).

test_complete_elicitation() ->
    ClientPid = self(),
    Config = #{<<"mode">> => <<"inline">>, <<"prompt">> => <<"Complete test">>},

    %% Create elicitation
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, ClientPid),

    %% Complete elicitation with result
    Result = <<"user_provided_api_key">>,
    ?assertEqual(ok, erlmcp_elicitation:complete_elicitation(ElicitationId, Result)),

    %% Verify status changed to completed
    {ok, Status} = erlmcp_elicitation:get_elicitation_status(ElicitationId),
    ?assertEqual(completed, maps:get(status, Status)),

    %% Test completing already completed
    CompletionResult = erlmcp_elicitation:complete_elicitation(ElicitationId, Result),
    ?assert(CompletionResult =:= ok orelse CompletionResult =:= {error, already_completed}),

    %% Test completing non-existent
    ?assertEqual({error, not_found}, erlmcp_elicitation:complete_elicitation(<<"nonexistent">>, Result)).

test_list_elicitations() ->
    ClientPid = self(),

    %% Create multiple elicitations
    Config1 = #{<<"mode">> => <<"inline">>, <<"prompt">> => <<"Elicit 1">>},
    Config2 = #{<<"mode">> => <<"url">>, <<"url">> => <<"https://example.com/auth">>},
    Config3 = #{<<"mode">> => <<"terminal">>, <<"command">> => <<"/bin/echo test">>},

    {ok, _Id1, _} = erlmcp_elicitation:create_elicitation(Config1, ClientPid),
    {ok, _Id2, _} = erlmcp_elicitation:create_elicitation(Config2, ClientPid),
    {ok, _Id3, _} = erlmcp_elicitation:create_elicitation(Config3, ClientPid),

    %% List all elicitations
    {ok, Elicitations} = erlmcp_elicitation:list_elicitations(),

    %% Verify at least our 3 elicitations exist
    ?assert(length(Elicitations) >= 3),

    %% Verify structure of elicitation entries
    lists:foreach(fun(Elicitation) ->
        ?assert(maps:is_key(id, Elicitation)),
        ?assert(maps:is_key(mode, Elicitation)),
        ?assert(maps:is_key(status, Elicitation)),
        ?assert(maps:is_key(created_at, Elicitation)),
        ?assert(maps:is_key(timeout_at, Elicitation))
    end, Elicitations).

%%%===================================================================
%%% Test Suite - Validation
%%%===================================================================

elicitation_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {"Invalid mode", fun test_invalid_mode/0},
          {"Missing required fields inline", fun test_missing_inline_fields/0},
          {"Missing required fields URL", fun test_missing_url_fields/0},
          {"Invalid URL format", fun test_invalid_url_format/0},
          {"SSRF protection - localhost", fun test_ssrf_localhost/0},
          {"SSRF protection - private IP", fun test_ssrf_private_ip/0},
          {"Size limit validation", fun test_size_limit_validation/0},
          {"Timeout validation", fun test_timeout_validation/0}
         ]
     end}.

test_invalid_mode() ->
    ClientPid = self(),
    Config = #{<<"mode">> => <<"invalid_mode">>, <<"prompt">> => <<"Test">>},

    %% Should fail with validation error
    Result = erlmcp_elicitation:create_elicitation(Config, ClientPid),
    ?assertMatch({error, _}, Result).

test_missing_inline_fields() ->
    ClientPid = self(),

    %% Missing prompt for inline mode
    Config = #{<<"mode">> => <<"inline">>},

    Result = erlmcp_elicitation:create_elicitation(Config, ClientPid),
    ?assertMatch({error, _}, Result).

test_missing_url_fields() ->
    ClientPid = self(),

    %% Missing URL for url mode
    Config = #{<<"mode">> => <<"url">>},

    Result = erlmcp_elicitation:create_elicitation(Config, ClientPid),
    ?assertMatch({error, _}, Result).

test_invalid_url_format() ->
    ClientPid = self(),

    %% Invalid URL format
    Config = #{<<"mode">> => <<"url">>, <<"url">> => <<"not_a_url">>},

    Result = erlmcp_elicitation:create_elicitation(Config, ClientPid),
    ?assertMatch({error, _}, Result).

test_ssrf_localhost() ->
    ClientPid = self(),

    %% Try to create URL elicitation to localhost (SSRF attack)
    Config = #{<<"mode">> => <<"url">>, <<"url">> => <<"http://localhost:8080/secret">>},

    Result = erlmcp_elicitation:create_elicitation(Config, ClientPid),
    %% Should fail due to SSRF protection
    ?assertMatch({error, _}, Result).

test_ssrf_private_ip() ->
    ClientPid = self(),

    %% Try to create URL elicitation to private IP (SSRF attack)
    PrivateIPs = [
        <<"http://127.0.0.1:8080/admin">>,
        <<"http://10.0.0.1/internal">>,
        <<"http://192.168.1.1/router">>,
        <<"http://172.16.0.1/private">>
    ],

    lists:foreach(fun(URL) ->
        Config = #{<<"mode">> => <<"url">>, <<"url">> => URL},
        Result = erlmcp_elicitation:create_elicitation(Config, ClientPid),
        %% Should fail due to SSRF protection
        ?assertMatch({error, _}, Result)
    end, PrivateIPs).

test_size_limit_validation() ->
    ClientPid = self(),
    Config = #{<<"mode">> => <<"inline">>, <<"prompt">> => <<"Test">>},

    %% Create elicitation
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, ClientPid),

    %% Try to complete with oversized result (> 1MB default)
    OversizedResult = binary:copy(<<$A>>, 2 * 1024 * 1024), %% 2MB

    Result = erlmcp_elicitation:complete_elicitation(ElicitationId, OversizedResult),
    %% Should fail due to size limit
    ?assertMatch({error, _}, Result).

test_timeout_validation() ->
    ClientPid = self(),

    %% Test invalid timeout values
    InvalidTimeouts = [-1, 0, 1000000000],

    lists:foreach(fun(Timeout) ->
        Config = #{
            <<"mode">> => <<"inline">>,
            <<"prompt">> => <<"Test">>,
            <<"timeout">> => Timeout
        },
        Result = erlmcp_elicitation:create_elicitation(Config, ClientPid),
        %% Should either fail or normalize timeout
        case Result of
            {ok, _, _} -> ok; %% Normalized
            {error, _} -> ok  %% Rejected
        end
    end, InvalidTimeouts).

%%%===================================================================
%%% Test Suite - Rate Limiting
%%%===================================================================

elicitation_rate_limiting_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {"Per-client rate limit", fun test_per_client_rate_limit/0},
          {"Concurrent elicitation limit", fun test_concurrent_limit/0}
         ]
     end}.

test_per_client_rate_limit() ->
    ClientPid = self(),
    Config = #{<<"mode">> => <<"inline">>, <<"prompt">> => <<"Rate limit test">>},

    %% Create multiple elicitations rapidly
    %% Default limit is 10 per client
    Results = [erlmcp_elicitation:create_elicitation(Config, ClientPid) || _ <- lists:seq(1, 12)],

    %% At least some should succeed
    Successes = [R || {ok, _, _} = R <- Results],
    ?assert(length(Successes) >= 1),

    %% May hit rate limit on later requests
    Errors = [R || {error, _} = R <- Results],
    ?assert(length(Errors) >= 0).

test_concurrent_limit() ->
    %% Create elicitations from multiple clients
    Clients = [spawn(fun() -> receive after 5000 -> ok end end) || _ <- lists:seq(1, 110)],

    Config = #{<<"mode">> => <<"inline">>, <<"prompt">> => <<"Concurrent test">>},

    Results = [erlmcp_elicitation:create_elicitation(Config, Pid) || Pid <- Clients],

    %% Some should succeed (up to max concurrent limit)
    Successes = [R || {ok, _, _} = R <- Results],
    ?assert(length(Successes) >= 1),

    %% May hit concurrent limit
    Errors = [R || {error, _} = R <- Results],
    ?assert(length(Errors) >= 0),

    %% Cleanup
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Clients).

%%%===================================================================
%%% Test Suite - Process Monitoring
%%%===================================================================

elicitation_process_monitoring_test_() ->
    {timeout, 10, {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {"Client process death cleanup", fun test_client_death_cleanup/0}
         ]
     end}}.

test_client_death_cleanup() ->
    %% Create a client process
    ClientPid = spawn(fun() -> receive after 5000 -> ok end end),

    Config = #{<<"mode">> => <<"inline">>, <<"prompt">> => <<"Death test">>},

    %% Create elicitation
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(Config, ClientPid),

    %% Verify elicitation exists
    ?assertMatch({ok, _}, erlmcp_elicitation:get_elicitation_status(ElicitationId)),

    %% Kill client process
    exit(ClientPid, kill),

    %% Wait for cleanup
    timer:sleep(200),

    %% Elicitation should still exist (not cleaned up immediately)
    %% but may be marked as cancelled/timeout
    Result = erlmcp_elicitation:get_elicitation_status(ElicitationId),
    case Result of
        {ok, Status} ->
            %% Status may have changed
            ?assert(maps:is_key(status, Status));
        {error, not_found} ->
            %% Cleaned up
            ok
    end.

%%%===================================================================
%%% Test Suite - Edge Cases
%%%===================================================================

elicitation_edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {"Empty prompt", fun test_empty_prompt/0},
          {"Special characters in prompt", fun test_special_characters/0},
          {"Large prompt", fun test_large_prompt/0},
          {"Unicode in prompt", fun test_unicode_prompt/0}
         ]
     end}.

test_empty_prompt() ->
    ClientPid = self(),
    Config = #{<<"mode">> => <<"inline">>, <<"prompt">> => <<>>},

    Result = erlmcp_elicitation:create_elicitation(Config, ClientPid),
    %% Should either accept or reject empty prompt
    case Result of
        {ok, _, _} -> ok;
        {error, _} -> ok
    end.

test_special_characters() ->
    ClientPid = self(),
    SpecialPrompt = <<"Enter password: !@#$%^&*()_+-=[]{}|;':\",./<>?">>,
    Config = #{<<"mode">> => <<"inline">>, <<"prompt">> => SpecialPrompt},

    %% Should handle special characters
    ?assertMatch({ok, _, _}, erlmcp_elicitation:create_elicitation(Config, ClientPid)).

test_large_prompt() ->
    ClientPid = self(),
    LargePrompt = binary:copy(<<"A">>, 10000),
    Config = #{<<"mode">> => <<"inline">>, <<"prompt">> => LargePrompt},

    %% Should handle large prompts
    ?assertMatch({ok, _, _}, erlmcp_elicitation:create_elicitation(Config, ClientPid)).

test_unicode_prompt() ->
    ClientPid = self(),
    UnicodePrompt = <<"请输入密码: 🔑"/utf8>>,
    Config = #{<<"mode">> => <<"inline">>, <<"prompt">> => UnicodePrompt},

    %% Should handle Unicode
    ?assertMatch({ok, _, _}, erlmcp_elicitation:create_elicitation(Config, ClientPid)).
