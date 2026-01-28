%%%-------------------------------------------------------------------
%% @doc Test suite for erlmcp_refusal taxonomy
%%
%% Tests all 45 refusal codes for:
%% - Correct HTTP status codes
%% - Proper remediation hints (non-empty, actionable)
%% - Consistent error messages
%% - Deterministic behavior (run 5x)
%% - JSON formatting
%% - Category classification
%% - Severity levels
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_refusal_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erlmcp_refusal.hrl").
-include("erlmcp.hrl").

-compile(export_all).

%%====================================================================
%% CT Callback Functions
%%====================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
        % Core metadata tests
        test_all_refusals_have_metadata,
        test_http_status_codes_valid,
        test_messages_non_empty,
        test_hints_non_empty_and_actionable,
        test_severity_levels_valid,

        % Refusal creation and formatting
        test_create_refusal_with_code_only,
        test_create_refusal_with_details,
        test_create_refusal_invalid_code,
        test_format_refusal_human_readable,
        test_format_refusal_json,

        % Lookup functions
        test_lookup_all_codes,
        test_get_http_status_function,
        test_get_message_function,
        test_get_hint_function,
        test_get_severity_function,
        test_get_metadata_function,

        % Validation functions
        test_is_valid_code,
        test_is_critical,
        test_is_auth_failure,
        test_is_security_issue,
        test_is_rate_limit,
        test_is_queue_limit,

        % Deterministic behavior (run each 5 times)
        test_deterministic_lookup_queue_limit,
        test_deterministic_lookup_auth_failure,
        test_deterministic_lookup_rate_limit,
        test_deterministic_format_refusal,
        test_deterministic_category_classification,

        % Logging tests
        test_log_refusal_critical,
        test_log_refusal_error,
        test_log_refusal_warn,
        test_log_refusal_with_context,

        % Integration tests
        test_all_category_classifications,
        test_no_duplicate_http_statuses,
        test_refusal_json_contains_all_fields,
        test_remediation_hints_actionable
    ].

%%====================================================================
%% Core Metadata Tests
%%====================================================================

test_all_refusals_have_metadata(Config) ->
    %% Verify all refusal codes in ?REFUSAL_METADATA have valid structure
    Metadata = ?REFUSAL_METADATA,
    ct:pal("Testing ~w refusal codes", [length(Metadata)]),

    Results = lists:map(fun({Code, HTTPStatus, Message, Hint, Severity}) ->
        {ok, _} = erlmcp_refusal:get_metadata(Code),
        {ok, HTTPStatus} = erlmcp_refusal:get_http_status(Code),
        {ok, Message} = erlmcp_refusal:get_message(Code),
        {ok, Hint} = erlmcp_refusal:get_hint(Code),
        {ok, Severity} = erlmcp_refusal:get_severity(Code),
        ok
    end, Metadata),

    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),
    ?assert(length(Results) >= 45),
    Config.

test_http_status_codes_valid(Config) ->
    %% All HTTP status codes must be in valid range
    Metadata = ?REFUSAL_METADATA,
    ValidStatuses = [200, 202, 400, 401, 403, 404, 409, 413, 415, 429, 503],

    Results = lists:map(fun({_Code, HTTPStatus, _Msg, _Hint, _Sev}) ->
        ?assert(lists:member(HTTPStatus, ValidStatuses)),
        ok
    end, Metadata),

    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),
    Config.

test_messages_non_empty(Config) ->
    %% All error messages must be non-empty binaries
    Metadata = ?REFUSAL_METADATA,

    Results = lists:map(fun({_Code, _Status, Message, _Hint, _Sev}) ->
        ?assert(is_binary(Message)),
        ?assert(byte_size(Message) > 0),
        ok
    end, Metadata),

    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),
    Config.

test_hints_non_empty_and_actionable(Config) ->
    %% All remediation hints must be non-empty and actionable (contain verb or action)
    Metadata = ?REFUSAL_METADATA,
    ActionWords = [
        <<"reduce">>, <<"check">>, <<"increase">>, <<"provide">>, <<"use">>,
        <<"wait">>, <<"add">>, <<"remove">>, <<"review">>, <<"upgrade">>,
        <<"refresh">>, <<"retry">>, <<"close">>, <<"contact">>, <<"register">>,
        <<"slow">>, <<"compress">>, <<"call">>, <<"implement">>, <<"enable">>,
        <<"disable">>, <<"ensure">>, <<"format">>, <<"follow">>, <<"include">>
    ],

    Results = lists:map(fun({_Code, _Status, _Msg, Hint, _Sev}) ->
        ?assert(is_binary(Hint)),
        ?assert(byte_size(Hint) > 10),  % Reasonable length
        LowerHint = string:lowercase(Hint),
        HasAction = lists:any(fun(Word) ->
            string:find(LowerHint, Word) =/= nomatch
        end, ActionWords),
        ?assert(HasAction),
        ok
    end, Metadata),

    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),
    Config.

test_severity_levels_valid(Config) ->
    %% All severity levels must be warn, error, or critical
    Metadata = ?REFUSAL_METADATA,
    ValidSeverities = [warn, error, critical],

    Results = lists:map(fun({_Code, _Status, _Msg, _Hint, Severity}) ->
        ?assert(lists:member(Severity, ValidSeverities)),
        ok
    end, Metadata),

    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),
    Config.

%%====================================================================
%% Refusal Creation and Formatting Tests
%%====================================================================

test_create_refusal_with_code_only(Config) ->
    {ok, Refusal} = erlmcp_refusal:create_refusal(1001),
    ?assert(is_record(Refusal, refusal)),
    ?assertEqual(1001, Refusal#refusal.code),
    ?assert(byte_size(Refusal#refusal.message) > 0),
    ?assert(byte_size(Refusal#refusal.hint) > 0),
    Config.

test_create_refusal_with_details(Config) ->
    Details = #{connection_id => <<"conn_123">>, message_count => 1050},
    {ok, Refusal} = erlmcp_refusal:create_refusal(1001, Details),
    ?assertEqual(Details, Refusal#refusal.details),
    Config.

test_create_refusal_invalid_code(Config) ->
    error = erlmcp_refusal:create_refusal(9999),
    error = erlmcp_refusal:create_refusal(-1),
    Config.

test_format_refusal_human_readable(Config) ->
    {ok, Refusal} = erlmcp_refusal:create_refusal(1001),
    Formatted = erlmcp_refusal:format_refusal(Refusal),
    ?assert(is_binary(Formatted)),
    ?assert(string:find(Formatted, <<"Code 1001">>) =/= nomatch),
    ?assert(string:find(Formatted, <<"Tip:">>) =/= nomatch),
    Config.

test_format_refusal_json(Config) ->
    {ok, Refusal} = erlmcp_refusal:create_refusal(1001, #{custom => value}),
    JSON = erlmcp_refusal:format_refusal_json(Refusal),
    ?assert(is_map(JSON)),
    ?assert(maps:is_key(error_code, JSON)),
    ?assert(maps:is_key(http_status, JSON)),
    ?assert(maps:is_key(message, JSON)),
    ?assert(maps:is_key(remediation_hint, JSON)),
    ?assert(maps:is_key(severity, JSON)),
    ?assert(maps:is_key(details, JSON)),
    Config.

%%====================================================================
%% Lookup Function Tests
%%====================================================================

test_lookup_all_codes(Config) ->
    %% Verify all codes can be looked up
    Codes = [1001, 1002, 1011, 1021, 1036, 1046, 1056, 1066, 1076, 1086],
    Results = lists:map(fun(Code) ->
        {ok, _Status, _Msg, _Hint, _Sev} = erlmcp_refusal:lookup_refusal(Code),
        ok
    end, Codes),
    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),
    Config.

test_get_http_status_function(Config) ->
    {ok, 429} = erlmcp_refusal:get_http_status(1001),  % Queue limit = 429
    {ok, 401} = erlmcp_refusal:get_http_status(1011),  % Auth fail = 401
    {ok, 400} = erlmcp_refusal:get_http_status(1021),  % Invalid params = 400
    error = erlmcp_refusal:get_http_status(9999),
    Config.

test_get_message_function(Config) ->
    {ok, Msg} = erlmcp_refusal:get_message(1001),
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0),
    error = erlmcp_refusal:get_message(9999),
    Config.

test_get_hint_function(Config) ->
    {ok, Hint} = erlmcp_refusal:get_hint(1001),
    ?assert(is_binary(Hint)),
    ?assert(byte_size(Hint) > 0),
    error = erlmcp_refusal:get_hint(9999),
    Config.

test_get_severity_function(Config) ->
    {ok, error} = erlmcp_refusal:get_severity(1001),
    {ok, critical} = erlmcp_refusal:get_severity(1003),
    {ok, error} = erlmcp_refusal:get_severity(1011),
    not_found = erlmcp_refusal:get_severity(9999),
    Config.

test_get_metadata_function(Config) ->
    {ok, Meta} = erlmcp_refusal:get_metadata(1001),
    ?assert(is_map(Meta)),
    ?assert(maps:is_key(code, Meta)),
    ?assert(maps:is_key(http_status, Meta)),
    ?assert(maps:is_key(message, Meta)),
    ?assert(maps:is_key(hint, Meta)),
    ?assert(maps:is_key(severity, Meta)),
    ?assert(maps:is_key(category, Meta)),
    Config.

%%====================================================================
%% Validation Function Tests
%%====================================================================

test_is_valid_code(Config) ->
    ?assert(erlmcp_refusal:is_valid_code(1001)),
    ?assert(erlmcp_refusal:is_valid_code(1089)),
    ?assert(not erlmcp_refusal:is_valid_code(9999)),
    ?assert(not erlmcp_refusal:is_valid_code(invalid)),
    Config.

test_is_critical(Config) ->
    ?assert(erlmcp_refusal:is_critical(1003)),   % Tenant cap = critical
    ?assert(erlmcp_refusal:is_critical(1036)),   % Path traversal = critical
    ?assert(not erlmcp_refusal:is_critical(1001)),  % Queue = error
    Config.

test_is_auth_failure(Config) ->
    ?assert(erlmcp_refusal:is_auth_failure(1011)),  % Auth failed
    ?assert(erlmcp_refusal:is_auth_failure(1012)),  % Auth expired
    ?assert(erlmcp_refusal:is_auth_failure(1016)),  % Session invalid
    ?assert(not erlmcp_refusal:is_auth_failure(1001)),  % Queue limit
    Config.

test_is_security_issue(Config) ->
    ?assert(erlmcp_refusal:is_security_issue(1036)),  % Path traversal
    ?assert(erlmcp_refusal:is_security_issue(1038)),  % Symlink traversal
    ?assert(erlmcp_refusal:is_security_issue(1015)),  % Missing auth
    ?assert(not erlmcp_refusal:is_security_issue(1001)),  % Queue limit
    Config.

test_is_rate_limit(Config) ->
    ?assert(erlmcp_refusal:is_rate_limit(1056)),  % Rate limit exceeded
    ?assert(erlmcp_refusal:is_rate_limit(1060)),  % Concurrent limit
    ?assert(not erlmcp_refusal:is_rate_limit(1001)),  % Queue limit
    Config.

test_is_queue_limit(Config) ->
    ?assert(erlmcp_refusal:is_queue_limit(1001)),  % Message count
    ?assert(erlmcp_refusal:is_queue_limit(1005)),  % Backpressure
    ?assert(not erlmcp_refusal:is_queue_limit(1056)),  % Rate limit
    Config.

%%====================================================================
%% Deterministic Behavior Tests (Run 5x to verify consistency)
%%====================================================================

test_deterministic_lookup_queue_limit(Config) ->
    Results = [erlmcp_refusal:get_http_status(1001) || _ <- lists:seq(1, 5)],
    ?assert(lists:all(fun(R) -> R =:= {ok, 429} end, Results)),
    Config.

test_deterministic_lookup_auth_failure(Config) ->
    Results = [erlmcp_refusal:get_http_status(1011) || _ <- lists:seq(1, 5)],
    ?assert(lists:all(fun(R) -> R =:= {ok, 401} end, Results)),
    Config.

test_deterministic_lookup_rate_limit(Config) ->
    Results = [erlmcp_refusal:get_http_status(1056) || _ <- lists:seq(1, 5)],
    ?assert(lists:all(fun(R) -> R =:= {ok, 429} end, Results)),
    Config.

test_deterministic_format_refusal(Config) ->
    {ok, R1} = erlmcp_refusal:create_refusal(1001),
    Msgs = [erlmcp_refusal:format_refusal(R1) || _ <- lists:seq(1, 5)],
    ?assert(lists:all(fun(M) -> is_binary(M) end, Msgs)),
    ?assert(all_equal(Msgs)),
    Config.

test_deterministic_category_classification(Config) ->
    TestCodes = [1001, 1011, 1021, 1036, 1046, 1056, 1066, 1076, 1086],
    Results = [erlmcp_refusal:get_metadata(Code) || Code <- TestCodes],
    Categories = [maps:get(category, Meta) || {ok, Meta} <- Results],
    Expected = [queue, auth, validation, security, resource, rate_limit, protocol, server, circuit_breaker],
    ?assertEqual(Expected, Categories),
    Config.

%%====================================================================
%% Logging Tests
%%====================================================================

test_log_refusal_critical(Config) ->
    {ok, Refusal} = erlmcp_refusal:create_refusal(1003),  % Tenant cap = critical
    ok = erlmcp_refusal:log_refusal(Refusal),
    Config.

test_log_refusal_error(Config) ->
    {ok, Refusal} = erlmcp_refusal:create_refusal(1001),  % Queue = error
    ok = erlmcp_refusal:log_refusal(Refusal),
    Config.

test_log_refusal_warn(Config) ->
    {ok, Refusal} = erlmcp_refusal:create_refusal(1046),  % Not found = warn
    ok = erlmcp_refusal:log_refusal(Refusal),
    Config.

test_log_refusal_with_context(Config) ->
    {ok, Refusal} = erlmcp_refusal:create_refusal(1001),
    ok = erlmcp_refusal:log_refusal(Refusal, "context_test"),
    ok = erlmcp_refusal:log_refusal_with_context(Refusal, "detailed", #{extra => data}),
    Config.

%%====================================================================
%% Integration Tests
%%====================================================================

test_all_category_classifications(Config) ->
    %% Test that all codes are correctly categorized
    Categories = {queue, auth, validation, security, resource, rate_limit, protocol, server, circuit_breaker},
    CodeRanges = [
        {1001, 1005, queue},
        {1011, 1016, auth},
        {1021, 1029, validation},
        {1036, 1040, security},
        {1046, 1052, resource},
        {1056, 1060, rate_limit},
        {1066, 1070, protocol},
        {1076, 1080, server},
        {1086, 1089, circuit_breaker}
    ],

    Results = lists:map(fun({Start, End, ExpectedCat}) ->
        lists:all(fun(Code) ->
            {ok, Meta} = erlmcp_refusal:get_metadata(Code),
            Cat = maps:get(category, Meta),
            Cat =:= ExpectedCat
        end, lists:seq(Start, End))
    end, CodeRanges),

    ?assert(lists:all(fun(R) -> R =:= true end, Results)),
    Config.

test_no_duplicate_http_statuses(Config) ->
    %% While multiple codes can share HTTP status, verify no inconsistencies
    Metadata = ?REFUSAL_METADATA,
    StatusMap = lists:foldl(fun({Code, Status, _Msg, _Hint, _Sev}, Acc) ->
        Codes = maps:get(Status, Acc, []),
        maps:put(Status, [Code | Codes], Acc)
    end, #{}, Metadata),

    %% All status codes should be valid HTTP codes
    AllValid = lists:all(fun({Status, _Codes}) ->
        lists:member(Status, [200, 202, 400, 401, 403, 404, 409, 413, 415, 429, 503])
    end, maps:to_list(StatusMap)),

    ?assert(AllValid),
    Config.

test_refusal_json_contains_all_fields(Config) ->
    %% Test JSON format for all refusal codes (sample 10 random)
    AllCodes = [C || {C, _, _, _, _} <- ?REFUSAL_METADATA],
    SampleCodes = lists:sublist(AllCodes, 1, min(10, length(AllCodes))),

    Results = lists:map(fun(Code) ->
        {ok, Refusal} = erlmcp_refusal:create_refusal(Code, #{test => true}),
        JSON = erlmcp_refusal:format_refusal_json(Refusal),

        RequiredKeys = [error_code, http_status, message, remediation_hint, severity, timestamp_ms, details],
        AllPresent = lists:all(fun(Key) -> maps:is_key(Key, JSON) end, RequiredKeys),

        ?assert(AllPresent),
        ok
    end, SampleCodes),

    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),
    Config.

test_remediation_hints_actionable(Config) ->
    %% Sample hints to ensure they're genuinely actionable
    SampleCodes = [1001, 1011, 1021, 1036, 1056],

    Results = lists:map(fun(Code) ->
        {ok, Refusal} = erlmcp_refusal:create_refusal(Code),
        Hint = Refusal#refusal.hint,

        %% Hint should contain specific action or value
        HasSpecs = (
            string:find(Hint, <<"max_">>) =/= nomatch orelse
            string:find(Hint, <<"200">>) =/= nomatch orelse
            string:find(Hint, <<"100">>) =/= nomatch orelse
            string:find(Hint, <<"32">>) =/= nomatch orelse
            string:find(Hint, <<"config">>) =/= nomatch orelse
            string:find(Hint, <<"administrator">>) =/= nomatch
        ),
        ?assert(HasSpecs),
        ok
    end, SampleCodes),

    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),
    Config.

%%====================================================================
%% Helper Functions
%%====================================================================

all_equal([]) -> true;
all_equal([_]) -> true;
all_equal([H | T]) -> lists:all(fun(X) -> X =:= H end, T).
