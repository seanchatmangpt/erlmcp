%%%-------------------------------------------------------------------
%%% @doc
%%% FM-08: CI Secret Scan Suite
%%%
%%% CI gate that scans logs for exposed secrets.
%%% Blocks build if any secret patterns are detected in logs.
%%%
%%% This suite:
%%% 1. Runs various operations that generate logs
%%% 2. Collects all log output
%%% 3. Scans for secret patterns (JWT, Bearer, session IDs, API keys)
%%% 4. FAILS if any secrets found (quality gate)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(test_secret_scan_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_no_jwt_patterns_in_logs/1,
    test_no_session_ids_in_logs/1,
    test_no_bearer_tokens_in_logs/1,
    test_no_aws_keys_in_logs/1,
    test_no_api_keys_in_logs/1,
    test_comprehensive_secret_scan/1,
    test_redaction_under_load/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        test_no_jwt_patterns_in_logs,
        test_no_session_ids_in_logs,
        test_no_bearer_tokens_in_logs,
        test_no_aws_keys_in_logs,
        test_no_api_keys_in_logs,
        test_comprehensive_secret_scan,
        test_redaction_under_load
    ].

init_per_suite(Config) ->
    % Start application
    {ok, _Apps} = application:ensure_all_started(erlmcp),

    % Start logging server
    {ok, LogPid} = erlmcp_logging:start_link(),

    [{log_pid, LogPid} | Config].

end_per_suite(Config) ->
    LogPid = ?config(log_pid, Config),
    gen_server:stop(LogPid),
    application:stop(erlmcp),
    ok.

init_per_testcase(_TestCase, Config) ->
    % Create fresh client for each test
    ClientPid = spawn(fun() -> receive stop -> ok end end),
    erlmcp_logging:create_client_buffer(ClientPid),
    [{client_pid, ClientPid} | Config].

end_per_testcase(_TestCase, Config) ->
    ClientPid = ?config(client_pid, Config),
    ClientPid ! stop,
    timer:sleep(50),  % Allow cleanup
    ok.

%%%===================================================================
%%% Test Cases - Secret Pattern Detection
%%%===================================================================

%% @doc CI Gate: No JWT patterns in logs
test_no_jwt_patterns_in_logs(Config) ->
    ClientPid = ?config(client_pid, Config),

    % Log messages that SHOULD contain redacted JWTs
    erlmcp_logging:log(ClientPid, info, <<"auth">>,
        <<"Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U">>,
        undefined),

    erlmcp_logging:log(ClientPid, debug, <<"auth">>,
        <<"JWT token: header.payload.signature">>,
        undefined),

    timer:sleep(100),

    % Retrieve all logs
    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{level => debug}),

    % Scan for JWT patterns
    JWTPattern = <<"eyJ[A-Za-z0-9_\\-]+\\.[A-Za-z0-9_\\-]+\\.[A-Za-z0-9_\\-]+">>,
    Base64JWTPattern = <<"[A-Za-z0-9_\\-]+\\.[A-Za-z0-9_\\-]+\\.[A-Za-z0-9_\\-]+">>,

    Violations = scan_logs_for_pattern(Logs, [JWTPattern, Base64JWTPattern]),

    case Violations of
        [] ->
            ct:pal("✅ PASS: No JWT patterns found in logs"),
            ok;
        _ ->
            ct:fail("❌ FAIL: JWT patterns detected in logs: ~p", [Violations])
    end.

%% @doc CI Gate: No session IDs in logs
test_no_session_ids_in_logs(Config) ->
    ClientPid = ?config(client_pid, Config),

    % Log with session ID that should be redacted
    SessionData = #{
        <<"session_id">> => <<"550e8400-e29b-41d4-a716-446655440000">>,
        <<"user">> => <<"alice">>
    },

    erlmcp_logging:log(ClientPid, info, <<"session">>,
        <<"User session established: 123e4567-e89b-12d3-a456-426614174000">>,
        SessionData),

    timer:sleep(100),

    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{}),

    % UUID pattern
    UUIDPattern = <<"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}">>,

    Violations = scan_logs_for_pattern(Logs, [UUIDPattern]),

    case Violations of
        [] ->
            ct:pal("✅ PASS: No session IDs found in logs"),
            ok;
        _ ->
            ct:fail("❌ FAIL: Session IDs detected in logs: ~p", [Violations])
    end.

%% @doc CI Gate: No Bearer tokens in logs
test_no_bearer_tokens_in_logs(Config) ->
    ClientPid = ?config(client_pid, Config),

    % Log Bearer token
    erlmcp_logging:log(ClientPid, warning, <<"auth">>,
        <<"Authentication failed for Bearer sk_live_abcdefghijklmnopqrstuvwxyz1234567890">>,
        undefined),

    timer:sleep(100),

    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{}),

    % Bearer pattern
    BearerPattern = <<"Bearer\\s+[A-Za-z0-9_\\-\\.]+">>,

    Violations = scan_logs_for_pattern(Logs, [BearerPattern]),

    case Violations of
        [] ->
            ct:pal("✅ PASS: No Bearer tokens found in logs"),
            ok;
        _ ->
            ct:fail("❌ FAIL: Bearer tokens detected in logs: ~p", [Violations])
    end.

%% @doc CI Gate: No AWS keys in logs
test_no_aws_keys_in_logs(Config) ->
    ClientPid = ?config(client_pid, Config),

    % Log AWS credentials
    erlmcp_logging:log(ClientPid, error, <<"aws">>,
        <<"AWS error: AKIAIOSFODNN7EXAMPLE / wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
        undefined),

    timer:sleep(100),

    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{}),

    % AWS patterns
    AWSAccessKeyPattern = <<"AKIA[0-9A-Z]{16}">>,
    AWSSecretPattern = <<"[A-Za-z0-9/+=]{40}">>,

    Violations = scan_logs_for_pattern(Logs, [AWSAccessKeyPattern, AWSSecretPattern]),

    case Violations of
        [] ->
            ct:pal("✅ PASS: No AWS keys found in logs"),
            ok;
        _ ->
            ct:fail("❌ FAIL: AWS keys detected in logs: ~p", [Violations])
    end.

%% @doc CI Gate: No API keys in logs
test_no_api_keys_in_logs(Config) ->
    ClientPid = ?config(client_pid, Config),

    % Log API key
    APIKeyData = #{
        <<"api_key">> => <<"sk_live_51234567890abcdefghijklmnopqrstuvwxyz">>,
        <<"apikey">> => <<"pk_test_1234567890abcdefghij">>
    },

    erlmcp_logging:log(ClientPid, info, <<"api">>,
        <<"API request with key: sk_prod_abcdefghijklmnopqrstuvwxyz123456">>,
        APIKeyData),

    timer:sleep(100),

    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{}),

    % Long alphanumeric API key pattern (32+ chars)
    APIKeyPattern = <<"[A-Za-z0-9_\\-]{32,}">>,

    Violations = scan_logs_for_pattern(Logs, [APIKeyPattern]),

    case Violations of
        [] ->
            ct:pal("✅ PASS: No API keys found in logs"),
            ok;
        _ ->
            ct:fail("❌ FAIL: API keys detected in logs: ~p", [Violations])
    end.

%% @doc CI Gate: Comprehensive secret scan
test_comprehensive_secret_scan(Config) ->
    ClientPid = ?config(client_pid, Config),

    % Log various secret types
    erlmcp_logging:log(ClientPid, info, <<"auth">>,
        <<"Multi-secret: Bearer eyJhbGci.payload.sig password=secret123 session=550e8400-e29b-41d4-a716-446655440000">>,
        undefined),

    SecretData = #{
        <<"password">> => <<"my_secret_password_123!">>,
        <<"secret">> => <<"api_secret_xyz">>,
        <<"access_token">> => <<"ya29.a0AfH6SMBx...">>,
        <<"refresh_token">> => <<"1//0gHZ9...">>,
        <<"client_secret">> => <<"client_secret_abc123">>,
        <<"private_key">> => <<"-----BEGIN PRIVATE KEY-----\nMIIEv...">>
    },

    erlmcp_logging:log(ClientPid, debug, <<"secrets">>,
        <<"Processing secrets">>,
        SecretData),

    timer:sleep(100),

    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{level => debug}),

    % All secret patterns
    Patterns = [
        <<"Bearer\\s+[A-Za-z0-9_\\-\\.]+">>,  % Bearer tokens
        <<"eyJ[A-Za-z0-9_\\-]+\\.[A-Za-z0-9_\\-]+">>,  % JWT
        <<"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}">>,  % UUID
        <<"AKIA[0-9A-Z]{16}">>,  % AWS Access Key
        <<"ya29\\.[0-9A-Za-z\\-_]+">>,  % Google OAuth
        <<"-----BEGIN PRIVATE KEY-----">>,  % Private key header
        <<"password[\"']?\\s*[=:]\\s*[\"']?[^\\s\"'\\*]{6,}">>,  % Password assignments
        <<"secret[\"']?\\s*[=:]\\s*[\"']?[^\\s\"'\\*]{6,}">>  % Secret assignments
    ],

    Violations = scan_logs_for_pattern(Logs, Patterns),

    case Violations of
        [] ->
            ct:pal("✅ PASS: Comprehensive scan - no secrets found in logs"),
            ok;
        _ ->
            ct:fail("❌ FAIL: Secrets detected in comprehensive scan: ~p", [Violations])
    end.

%% @doc CI Gate: Redaction works under load
test_redaction_under_load(Config) ->
    ClientPid = ?config(client_pid, Config),

    % Generate 100 log entries with secrets
    lists:foreach(fun(N) ->
        Message = list_to_binary(io_lib:format("Request ~p with Bearer token_~p_abc.def.ghi", [N, N])),
        Data = #{
            <<"request_id">> => N,
            <<"api_key">> => list_to_binary(io_lib:format("sk_live_~p_1234567890", [N])),
            <<"session_id">> => list_to_binary(io_lib:format("550e8400-e29b-~4.10.0B-a716-446655440000", [N]))
        },
        erlmcp_logging:log(ClientPid, info, <<"load_test">>, Message, Data)
    end, lists:seq(1, 100)),

    timer:sleep(500),  % Allow processing

    {ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{}),

    % Verify we got logs
    ?assert(length(Logs) > 0),

    % Scan for any secret patterns
    Patterns = [
        <<"token_\\d+_abc\\.def\\.ghi">>,  % Our fake tokens
        <<"sk_live_\\d+_1234567890">>,  % Our fake API keys
        <<"550e8400-e29b-[0-9a-fA-F]{4}-a716-446655440000">>  % Our session IDs
    ],

    Violations = scan_logs_for_pattern(Logs, Patterns),

    case Violations of
        [] ->
            ct:pal("✅ PASS: Redaction works under load - ~p logs, 0 secrets", [length(Logs)]),
            ok;
        _ ->
            ct:fail("❌ FAIL: Secrets leaked under load: ~p violations in ~p logs", [length(Violations), length(Logs)])
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Scan logs for secret patterns
%% Returns list of violations: [{Pattern, LogEntry}]
-spec scan_logs_for_pattern([map()], [binary()]) -> [{binary(), map()}].
scan_logs_for_pattern(Logs, Patterns) ->
    lists:foldl(fun(Log, Acc) ->
        Message = maps:get(<<"message">>, Log, <<>>),
        Data = maps:get(<<"data">>, Log, undefined),

        % Check message for patterns
        MessageViolations = lists:foldl(fun(Pattern, MAcc) ->
            case re:run(Message, Pattern, []) of
                {match, _} ->
                    [{Pattern, Log} | MAcc];
                nomatch ->
                    MAcc
            end
        end, [], Patterns),

        % Check data for patterns (serialize to binary first)
        DataViolations = case Data of
            undefined ->
                [];
            _ when is_map(Data) ->
                DataBin = jsx:encode(Data),
                lists:foldl(fun(Pattern, DAcc) ->
                    case re:run(DataBin, Pattern, []) of
                        {match, _} ->
                            [{Pattern, Log} | DAcc];
                        nomatch ->
                            DAcc
                    end
                end, [], Patterns);
            _ ->
                []
        end,

        MessageViolations ++ DataViolations ++ Acc
    end, [], Logs).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Pretty print violations for debugging
print_violations([]) ->
    ok;
print_violations(Violations) ->
    ct:pal("~n=== SECRET VIOLATIONS DETECTED ===~n"),
    lists:foreach(fun({Pattern, Log}) ->
        ct:pal("Pattern: ~s~n", [Pattern]),
        ct:pal("Message: ~s~n", [maps:get(<<"message">>, Log, <<>>)]),
        ct:pal("Data: ~p~n~n", [maps:get(<<"data">>, Log, undefined)])
    end, Violations).
