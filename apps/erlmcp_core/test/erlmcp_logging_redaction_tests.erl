%%%-------------------------------------------------------------------
%%% @doc
%%% FM-08: Secret Redaction Layer - Unit Tests
%%%
%%% Comprehensive test coverage for credential redaction in logging:
%%% - Bearer token redaction
%%% - Session ID redaction
%%% - JWT payload redaction
%%% - API key redaction
%%% - Password redaction
%%% - Nested structure redaction
%%% - Multiple secrets in same message
%%% - Non-secrets preservation (no over-redaction)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_logging_redaction_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup/Teardown
%%%===================================================================

redaction_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"Bearer tokens redacted in all positions", fun test_bearer_tokens/0},
         {"Session IDs redacted", fun test_session_ids/0},
         {"JWT payloads redacted", fun test_jwt_payloads/0},
         {"API keys redacted", fun test_api_keys/0},
         {"Passwords redacted", fun test_passwords/0},
         {"Nested structures fully redacted", fun test_nested_structures/0},
         {"Multiple secrets in same message redacted", fun test_multiple_secrets/0},
         {"Non-secrets preserved", fun test_non_secrets_preserved/0},
         {"AWS credentials redacted", fun test_aws_credentials/0},
         {"OAuth tokens redacted", fun test_oauth_tokens/0},
         {"Config disabled skips redaction", fun test_disabled_redaction/0},
         {"Custom patterns work", fun test_custom_patterns/0},
         {"List structures redacted", fun test_list_redaction/0},
         {"Tuple structures redacted", fun test_tuple_redaction/0},
         {"Binary messages redacted", fun test_binary_redaction/0},
         {"Secret keys detected correctly", fun test_secret_key_detection/0}
     ]}.

setup() ->
    % Start application to load config
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    application:stop(erlmcp),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc Test 1: Bearer tokens redacted in all positions
test_bearer_tokens() ->
    % JWT Bearer token in Authorization header
    Input1 = <<"Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c">>,
    Result1 = erlmcp_logging_redactor:redact_message(Input1),
    ?assertNotEqual(Input1, Result1),
    ?assertEqual(nomatch, binary:match(Result1, <<"eyJhbGci">>)),
    ?assertNotEqual(nomatch, binary:match(Result1, <<"***REDACTED***">>)),

    % Multiple Bearer tokens
    Input2 = <<"Auth1: Bearer token1.payload1.sig1 Auth2: Bearer token2.payload2.sig2">>,
    Result2 = erlmcp_logging_redactor:redact_message(Input2),
    ?assertEqual(nomatch, binary:match(Result2, <<"token1">>)),
    ?assertEqual(nomatch, binary:match(Result2, <<"token2">>)),

    % Bearer in middle of message
    Input3 = <<"Request failed with Bearer abc.def.ghi from client 192.168.1.1">>,
    Result3 = erlmcp_logging_redactor:redact_message(Input3),
    ?assertEqual(nomatch, binary:match(Result3, <<"abc.def.ghi">>)),
    ?assertNotEqual(nomatch, binary:match(Result3, <<"192.168.1.1">>)),  % IP preserved

    ok.

%% @doc Test 2: Session IDs redacted
test_session_ids() ->
    % UUID format session ID
    Input1 = <<"session_id: 123e4567-e89b-12d3-a456-426614174000">>,
    Result1 = erlmcp_logging_redactor:redact_message(Input1),
    ?assertEqual(nomatch, binary:match(Result1, <<"123e4567">>)),

    % Multiple UUIDs
    Input2 = <<"Session 550e8400-e29b-41d4-a716-446655440000 and 6ba7b810-9dad-11d1-80b4-00c04fd430c8">>,
    Result2 = erlmcp_logging_redactor:redact_message(Input2),
    ?assertEqual(nomatch, binary:match(Result2, <<"550e8400">>)),
    ?assertEqual(nomatch, binary:match(Result2, <<"6ba7b810">>)),

    % Session ID in map
    Input3 = #{<<"session_id">> => <<"abc-123-def-456">>},
    Result3 = erlmcp_logging_redactor:redact_data(Input3),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"session_id">>, Result3)),

    ok.

%% @doc Test 3: JWT payloads redacted (base64 patterns)
test_jwt_payloads() ->
    % Base64 JWT pattern (xxx.yyy.zzz)
    Input1 = <<"JWT: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0In0.SflKxw">>,
    Result1 = erlmcp_logging_redactor:redact_message(Input1),
    ?assertEqual(nomatch, binary:match(Result1, <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9">>)),

    % JWT in map
    Input2 = #{<<"jwt">> => <<"header.payload.signature">>},
    Result2 = erlmcp_logging_redactor:redact_data(Input2),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"jwt">>, Result2)),

    ok.

%% @doc Test 4: API keys redacted
test_api_keys() ->
    % Long alphanumeric API key
    Input1 = <<"API_KEY=sk_live_1234567890abcdefghijklmnopqrstuvwxyz">>,
    Result1 = erlmcp_logging_redactor:redact_message(Input1),
    ?assertEqual(nomatch, binary:match(Result1, <<"sk_live_1234567890abcdefghijklmnopqrstuvwxyz">>)),

    % API key in map
    Input2 = #{
        <<"api_key">> => <<"sk_test_abcd1234">>,
        <<"username">> => <<"alice">>
    },
    Result2 = erlmcp_logging_redactor:redact_data(Input2),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"api_key">>, Result2)),
    ?assertEqual(<<"alice">>, maps:get(<<"username">>, Result2)),

    % apikey variant
    Input3 = #{<<"apikey">> => <<"secret123">>},
    Result3 = erlmcp_logging_redactor:redact_data(Input3),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"apikey">>, Result3)),

    ok.

%% @doc Test 5: Passwords redacted
test_passwords() ->
    % Password in map
    Input1 = #{
        <<"password">> => <<"secret123!@#">>,
        <<"email">> => <<"alice@example.com">>
    },
    Result1 = erlmcp_logging_redactor:redact_data(Input1),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"password">>, Result1)),
    ?assertEqual(<<"alice@example.com">>, maps:get(<<"email">>, Result1)),

    % Atom keys
    Input2 = #{
        password => <<"my_password">>,
        username => <<"bob">>
    },
    Result2 = erlmcp_logging_redactor:redact_data(Input2),
    ?assertEqual(<<"***REDACTED***">>, maps:get(password, Result2)),
    ?assertEqual(<<"bob">>, maps:get(username, Result2)),

    ok.

%% @doc Test 6: Nested structures fully redacted
test_nested_structures() ->
    % Deeply nested map
    Input = #{
        <<"user">> => #{
            <<"password">> => <<"secret123">>,
            <<"email">> => <<"alice@example.com">>,
            <<"profile">> => #{
                <<"api_key">> => <<"sk_1234">>,
                <<"display_name">> => <<"Alice">>
            }
        },
        <<"auth">> => #{
            <<"token">> => <<"jwt_token_here">>,
            <<"session_id">> => <<"session_123">>
        },
        <<"metadata">> => #{
            <<"ip">> => <<"192.168.1.1">>,
            <<"user_agent">> => <<"Mozilla/5.0">>
        }
    },

    Result = erlmcp_logging_redactor:redact_data(Input),

    % Check nested redactions
    User = maps:get(<<"user">>, Result),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"password">>, User)),
    ?assertEqual(<<"alice@example.com">>, maps:get(<<"email">>, User)),

    Profile = maps:get(<<"profile">>, User),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"api_key">>, Profile)),
    ?assertEqual(<<"Alice">>, maps:get(<<"display_name">>, Profile)),

    Auth = maps:get(<<"auth">>, Result),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"token">>, Auth)),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"session_id">>, Auth)),

    Metadata = maps:get(<<"metadata">>, Result),
    ?assertEqual(<<"192.168.1.1">>, maps:get(<<"ip">>, Metadata)),
    ?assertEqual(<<"Mozilla/5.0">>, maps:get(<<"user_agent">>, Metadata)),

    ok.

%% @doc Test 7: Multiple secrets in same message redacted
test_multiple_secrets() ->
    % Multiple secret types in one message
    Input = <<"User logged in with password=secret123, api_key=sk_live_abc123, session_id=550e8400-e29b-41d4-a716-446655440000, Bearer eyJhbGci.payload.sig">>,
    Result = erlmcp_logging_redactor:redact_message(Input),

    % Verify all secrets redacted
    ?assertEqual(nomatch, binary:match(Result, <<"secret123">>)),
    ?assertEqual(nomatch, binary:match(Result, <<"sk_live_abc123">>)),
    ?assertEqual(nomatch, binary:match(Result, <<"550e8400">>)),
    ?assertEqual(nomatch, binary:match(Result, <<"eyJhbGci">>)),

    % Verify non-secrets preserved
    ?assertNotEqual(nomatch, binary:match(Result, <<"User logged in">>)),

    ok.

%% @doc Test 8: Non-secrets preserved (no over-redaction)
test_non_secrets_preserved() ->
    % Regular data should not be redacted
    Input1 = #{
        <<"username">> => <<"alice">>,
        <<"email">> => <<"alice@example.com">>,
        <<"ip_address">> => <<"192.168.1.1">>,
        <<"timestamp">> => 1234567890,
        <<"message">> => <<"User authenticated successfully">>
    },
    Result1 = erlmcp_logging_redactor:redact_data(Input1),
    ?assertEqual(Input1, Result1),

    % Binary messages without secrets
    Input2 = <<"Processing request from 192.168.1.1 at 2024-01-01T12:00:00Z">>,
    Result2 = erlmcp_logging_redactor:redact_message(Input2),
    ?assertEqual(Input2, Result2),

    ok.

%% @doc Test 9: AWS credentials redacted
test_aws_credentials() ->
    % AWS Access Key ID pattern
    Input1 = <<"AWS_ACCESS_KEY_ID=AKIAIOSFODNN7EXAMPLE">>,
    Result1 = erlmcp_logging_redactor:redact_message(Input1),
    ?assertEqual(nomatch, binary:match(Result1, <<"AKIAIOSFODNN7EXAMPLE">>)),

    % AWS Secret Access Key pattern
    Input2 = <<"AWS_SECRET_ACCESS_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
    Result2 = erlmcp_logging_redactor:redact_message(Input2),
    ?assertEqual(nomatch, binary:match(Result2, <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>)),

    ok.

%% @doc Test 10: OAuth tokens redacted
test_oauth_tokens() ->
    % OAuth token in map
    Input1 = #{
        <<"access_token">> => <<"ya29.a0AfH6SMBx...">>>,
        <<"refresh_token">> => <<"1//0gHZ9...">>
    },
    Result1 = erlmcp_logging_redactor:redact_data(Input1),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"access_token">>, Result1)),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"refresh_token">>, Result1)),

    % Google OAuth pattern in binary
    Input2 = <<"token=ya29.a0AfH6SMBxVdja3hL9jKlM...">>,
    Result2 = erlmcp_logging_redactor:redact_message(Input2),
    ?assertEqual(nomatch, binary:match(Result2, <<"ya29.a0AfH6SMBxVdja3hL9jKlM">>)),

    ok.

%% @doc Test 11: Config disabled skips redaction
test_disabled_redaction() ->
    Input = <<"password=secret123 api_key=sk_live_abc">>,
    Config = #{enabled => false},
    Result = erlmcp_logging_redactor:redact_message(Input, Config),
    ?assertEqual(Input, Result),

    InputData = #{<<"password">> => <<"secret">>},
    ResultData = erlmcp_logging_redactor:redact_data(InputData, Config),
    ?assertEqual(InputData, ResultData),

    ok.

%% @doc Test 12: Custom patterns work
test_custom_patterns() ->
    % Custom pattern for credit card numbers
    CustomConfig = #{
        enabled => true,
        patterns => [{credit_card, <<"\\d{4}-\\d{4}-\\d{4}-\\d{4}">>}],
        replacement => <<"[CARD]">>,
        secret_keys => []
    },

    Input = <<"Payment with card 1234-5678-9012-3456">>,
    Result = erlmcp_logging_redactor:redact_message(Input, CustomConfig),
    ?assertEqual(nomatch, binary:match(Result, <<"1234-5678-9012-3456">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"[CARD]">>)),

    ok.

%% @doc Test 13: List structures redacted
test_list_redaction() ->
    % List with mixed content
    Input = [
        <<"password: secret123">>,
        <<"username: alice">>,
        #{<<"api_key">> => <<"sk_1234">>},
        <<"Bearer eyJhbGci.payload.sig">>
    ],

    Config = erlmcp_logging_redactor:default_patterns(),
    Result = erlmcp_logging_redactor:redact_message(Input),

    [First, Second, Third, Fourth] = Result,
    ?assertEqual(nomatch, binary:match(First, <<"secret123">>)),
    ?assertNotEqual(nomatch, binary:match(Second, <<"alice">>)),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"api_key">>, Third)),
    ?assertEqual(nomatch, binary:match(Fourth, <<"eyJhbGci">>)),

    ok.

%% @doc Test 14: Tuple structures redacted
test_tuple_redaction() ->
    % Tuple with secrets
    Input = {<<"password">>, <<"secret123">>, <<"Bearer token.here.now">>},
    Result = erlmcp_logging_redactor:redact_message(Input),

    ?assertMatch({_, _, _}, Result),
    {First, Second, Third} = Result,
    ?assertEqual(<<"password">>, First),  % Key name preserved
    ?assertEqual(nomatch, binary:match(Second, <<"secret123">>)),
    ?assertEqual(nomatch, binary:match(Third, <<"token.here.now">>)),

    ok.

%% @doc Test 15: Binary messages redacted
test_binary_redaction() ->
    % Various binary formats
    Input1 = <<"Authorization: Bearer eyJhbGci">>,
    Result1 = erlmcp_logging_redactor:redact_message(Input1),
    ?assertNotEqual(Input1, Result1),

    Input2 = <<"Regular log message without secrets">>,
    Result2 = erlmcp_logging_redactor:redact_message(Input2),
    ?assertEqual(Input2, Result2),

    ok.

%% @doc Test 16: Secret keys detected correctly
test_secret_key_detection() ->
    % Binary keys
    ?assert(erlmcp_logging_redactor:is_secret_key(<<"password">>)),
    ?assert(erlmcp_logging_redactor:is_secret_key(<<"api_key">>)),
    ?assert(erlmcp_logging_redactor:is_secret_key(<<"session_id">>)),
    ?assert(erlmcp_logging_redactor:is_secret_key(<<"access_token">>)),

    % Atom keys
    ?assert(erlmcp_logging_redactor:is_secret_key(password)),
    ?assert(erlmcp_logging_redactor:is_secret_key(api_key)),
    ?assert(erlmcp_logging_redactor:is_secret_key(secret)),

    % Non-secret keys
    ?assertNot(erlmcp_logging_redactor:is_secret_key(<<"username">>)),
    ?assertNot(erlmcp_logging_redactor:is_secret_key(<<"email">>)),
    ?assertNot(erlmcp_logging_redactor:is_secret_key(username)),

    ok.

%%%===================================================================
%%% Integration Tests (with erlmcp_logging)
%%%===================================================================

integration_test_() ->
    {foreach,
     fun integration_setup/0,
     fun integration_cleanup/1,
     [
         {"Logging integration redacts secrets", fun test_logging_integration/0},
         {"Redaction preserves log structure", fun test_log_structure_preserved/0}
     ]}.

integration_setup() ->
    application:ensure_all_started(erlmcp),
    {ok, Pid} = erlmcp_logging:start_link(),
    ClientPid = spawn(fun() -> receive stop -> ok end end),
    erlmcp_logging:create_client_buffer(ClientPid),
    {Pid, ClientPid}.

integration_cleanup({Pid, ClientPid}) ->
    ClientPid ! stop,
    gen_server:stop(Pid),
    application:stop(erlmcp),
    ok.

test_logging_integration() ->
    {_LoggingPid, ClientPid} = get_test_context(),

    % Log message with secrets
    Message = <<"User logged in with Bearer eyJhbGci.payload.sig">>,
    Data = #{
        <<"password">> => <<"secret123">>,
        <<"username">> => <<"alice">>
    },

    erlmcp_logging:log(ClientPid, info, <<"auth">>, Message, Data),

    % Give it time to process
    timer:sleep(50),

    % Retrieve logs
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{}),
    ?assertNotEqual([], Logs),

    [Entry | _] = Logs,
    LoggedMessage = maps:get(<<"message">>, Entry),
    LoggedData = maps:get(<<"data">>, Entry),

    % Verify secrets redacted
    ?assertEqual(nomatch, binary:match(LoggedMessage, <<"eyJhbGci">>)),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"password">>, LoggedData)),
    ?assertEqual(<<"alice">>, maps:get(<<"username">>, LoggedData)),

    ok.

test_log_structure_preserved() ->
    {_LoggingPid, ClientPid} = get_test_context(),

    % Log with nested structure
    Data = #{
        <<"user">> => #{
            <<"id">> => 123,
            <<"password">> => <<"secret">>
        },
        <<"timestamp">> => erlang:system_time(millisecond)
    },

    erlmcp_logging:log(ClientPid, warning, <<"test">>, <<"Test message">>, Data),
    timer:sleep(50),

    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{}),
    [Entry | _] = Logs,

    % Verify structure preserved
    ?assertNotEqual(undefined, maps:get(<<"timestamp">>, Entry)),
    ?assertEqual(<<"warning">>, maps:get(<<"level">>, Entry)),
    ?assertEqual(<<"test">>, maps:get(<<"component">>, Entry)),

    LoggedData = maps:get(<<"data">>, Entry),
    User = maps:get(<<"user">>, LoggedData),
    ?assertEqual(123, maps:get(<<"id">>, User)),
    ?assertEqual(<<"***REDACTED***">>, maps:get(<<"password">>, User)),

    ok.

%% Helper to get test context from eunit process dictionary
get_test_context() ->
    erlang:get(test_context).

integration_setup() ->
    application:ensure_all_started(erlmcp),
    {ok, Pid} = erlmcp_logging:start_link(),
    ClientPid = spawn(fun() -> receive stop -> ok end end),
    erlmcp_logging:create_client_buffer(ClientPid),
    Context = {Pid, ClientPid},
    erlang:put(test_context, Context),
    Context.
