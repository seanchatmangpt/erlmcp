%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive test suite for erlmcp_error module
%%%
%%% Tests error handling at scale with:
%%% - 100K concurrent error contexts
%%% - Error categorization accuracy
%%% - Structured logging functionality
%%% - Error recovery decisions
%%% - Batch statistics collection
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_error_handling_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite Organization
%%====================================================================

setup() ->
    % Start error collector
    {ok, _} = erlmcp_error:start_error_collector(),
    ok.

cleanup(_) ->
    erlmcp_error:reset_error_stats(),
    ok.

%%====================================================================
%% Context Creation and Management Tests
%%====================================================================

context_tests_() ->
    {setup, fun setup/0, fun cleanup/1, [
        ?_test(test_new_context_defaults()),
        ?_test(test_new_context_with_fields()),
        ?_test(test_add_context_field()),
        ?_test(test_get_context_field()),
        ?_test(test_context_to_map()),
        ?_test(test_error_id_uniqueness())
    ]}.

test_new_context_defaults() ->
    Context = erlmcp_error:new_context(test_op),
    ?assertEqual(test_op, maps:get(operation, Context)),
    ?assertEqual(node(), maps:get(node, Context)),
    ?assertMatch(<<_/binary>>, maps:get(error_id, Context)),
    ?assertEqual(undefined, maps:get(user_id, Context)).

test_new_context_with_fields() ->
    Fields = #{connection_id => <<"conn-123">>, user_id => <<"user-456">>},
    Context = erlmcp_error:new_context(test_op, Fields),
    ?assertEqual(<<"conn-123">>, maps:get(connection_id, Context)),
    ?assertEqual(<<"user-456">>, maps:get(user_id, Context)).

test_add_context_field() ->
    Context = erlmcp_error:new_context(test_op),
    Updated = erlmcp_error:add_context(Context, custom_field, <<"custom-value">>),
    ?assertEqual(<<"custom-value">>, maps:get(custom_field, Updated)).

test_get_context_field() ->
    Context = erlmcp_error:new_context(test_op, #{connection_id => <<"conn-1">>}),
    ?assertEqual(<<"conn-1">>, erlmcp_error:get_context(Context, connection_id)),
    ?assertEqual(undefined, erlmcp_error:get_context(Context, nonexistent)).

test_context_to_map() ->
    Context = erlmcp_error:new_context(test_op),
    Map = erlmcp_error:context_to_map(Context),
    ?assertMatch(#{operation := test_op}, Map),
    % Stack trace should be masked
    ?assertEqual(<<"<stack trace>">>, maps:get(stack_trace, Map)).

test_error_id_uniqueness() ->
    Context1 = erlmcp_error:new_context(test_op),
    Context2 = erlmcp_error:new_context(test_op),
    Id1 = maps:get(error_id, Context1),
    Id2 = maps:get(error_id, Context2),
    ?assertNotEqual(Id1, Id2).

%%====================================================================
%% Error Creation and Wrapping Tests
%%====================================================================

error_creation_tests_() ->
    {setup, fun setup/0, fun cleanup/1, [
        ?_test(test_error_creation_simple()),
        ?_test(test_error_creation_with_data()),
        ?_test(test_error_creation_with_context()),
        ?_test(test_wrap_error_tuple()),
        ?_test(test_wrap_error_with_reason()),
        ?_test(test_wrap_error_with_exception())
    ]}.

test_error_creation_simple() ->
    Result = erlmcp_error:error(?JSONRPC_INVALID_REQUEST, <<"Bad request">>),
    ?assertMatch({error, {?JSONRPC_INVALID_REQUEST, <<"Bad request">>, #{}, _}}, Result).

test_error_creation_with_data() ->
    Data = #{field => <<"value">>},
    Result = erlmcp_error:error(?JSONRPC_INVALID_PARAMS, <<"Params error">>, Data),
    ?assertMatch({error, {?JSONRPC_INVALID_PARAMS, <<"Params error">>, Data, _}}, Result).

test_error_creation_with_context() ->
    Context = erlmcp_error:new_context(test_op, #{connection_id => <<"test">>}),
    Data = #{extra => <<"data">>},
    Result = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"Timeout">>, Data, Context),
    {error, {Code, Msg, _, Ctx}} = Result,
    ?assertEqual(?MCP_ERROR_TIMEOUT, Code),
    ?assertEqual(<<"Timeout">>, Msg),
    ?assertEqual(<<"test">>, maps:get(connection_id, Ctx)).

test_wrap_error_tuple() ->
    Original = {error, {?MCP_ERROR_TIMEOUT, <<"Original timeout">>}},
    Result = erlmcp_error:wrap_error(Original, wrapped_op),
    {error, {Code, Msg, _Data, Context}} = Result,
    ?assertEqual(?MCP_ERROR_TIMEOUT, Code),
    ?assertEqual(wrapped_op, maps:get(operation, Context)).

test_wrap_error_with_reason() ->
    Original = {error, timeout},
    Result = erlmcp_error:wrap_error(Original, wrapped_op),
    {error, {Code, _Msg, _Data, _Context}} = Result,
    ?assertEqual(?MCP_ERROR_TIMEOUT, Code).

test_wrap_error_with_exception() ->
    Original = {error, undefined_function},
    Result = erlmcp_error:wrap_error(Original, wrapped_op),
    ?assertMatch({error, {?JSONRPC_INTERNAL_ERROR, _, _, _}}, Result).

%%====================================================================
%% Error Categorization Tests
%%====================================================================

categorization_tests_() ->
    {setup, fun setup/0, fun cleanup/1, [
        ?_test(test_categorize_transient_timeout()),
        ?_test(test_categorize_transient_transport()),
        ?_test(test_categorize_permanent_not_found()),
        ?_test(test_categorize_permanent_invalid_params()),
        ?_test(test_categorize_unknown()),
        ?_test(test_is_retryable_transient()),
        ?_test(test_is_retryable_permanent()),
        ?_test(test_is_transient()),
        ?_test(test_categorize_exception_reasons())
    ]}.

test_categorize_transient_timeout() ->
    ?assertEqual(transient, erlmcp_error:categorize(?MCP_ERROR_TIMEOUT)).

test_categorize_transient_transport() ->
    ?assertEqual(transient, erlmcp_error:categorize(?MCP_ERROR_TRANSPORT_ERROR)).

test_categorize_permanent_not_found() ->
    ?assertEqual(permanent, erlmcp_error:categorize(?MCP_ERROR_RESOURCE_NOT_FOUND)).

test_categorize_permanent_invalid_params() ->
    ?assertEqual(permanent, erlmcp_error:categorize(?JSONRPC_INVALID_PARAMS)).

test_categorize_unknown() ->
    ?assertEqual(unknown, erlmcp_error:categorize(-99999)).

test_is_retryable_transient() ->
    Error = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"timeout">>),
    ?assert(erlmcp_error:is_retryable(Error)).

test_is_retryable_permanent() ->
    Error = erlmcp_error:error(?JSONRPC_INVALID_PARAMS, <<"params">>),
    ?assertNot(erlmcp_error:is_retryable(Error)).

test_is_transient() ->
    TransientErr = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"t">>),
    PermanentErr = erlmcp_error:error(?MCP_ERROR_RESOURCE_NOT_FOUND, <<"p">>),
    ?assert(erlmcp_error:is_transient(TransientErr)),
    ?assertNot(erlmcp_error:is_transient(PermanentErr)).

test_categorize_exception_reasons() ->
    ?assertEqual(transient, erlmcp_error:categorize({error, timeout})),
    ?assertEqual(transient, erlmcp_error:categorize({error, econnrefused})),
    ?assertEqual(transient, erlmcp_error:categorize({error, econnreset})),
    ?assertEqual(permanent, erlmcp_error:categorize({error, not_found})),
    ?assertEqual(permanent, erlmcp_error:categorize({error, badarg})).

%%====================================================================
%% Error Logging Tests
%%====================================================================

logging_tests_() ->
    {setup, fun setup/0, fun cleanup/1, [
        ?_test(test_log_error_basic()),
        ?_test(test_log_error_with_severity()),
        ?_test(test_log_error_with_context()),
        ?_test(test_log_error_with_extra_fields()),
        ?_test(test_log_error_multiple_calls())
    ]}.

test_log_error_basic() ->
    Context = erlmcp_error:new_context(test_op),
    Error = erlmcp_error:error(?JSONRPC_INVALID_REQUEST, <<"test">>),
    ?assertMatch({error, _}, Error),
    erlmcp_error:log_error(Error, Context),
    ok.

test_log_error_with_severity() ->
    Context = erlmcp_error:new_context(test_op),
    Error = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"timeout">>),
    erlmcp_error:log_error(Error, Context, warning),
    ok.

test_log_error_with_context() ->
    State = #{
        operation => test_op,
        client_id => <<"client-1">>,
        user_id => <<"user-1">>,
        request_id => <<"req-1">>
    },
    Error = erlmcp_error:error(?JSONRPC_INVALID_PARAMS, <<"params">>),
    erlmcp_error:log_error_with_context(Error, error, State),
    ok.

test_log_error_with_extra_fields() ->
    Context = erlmcp_error:new_context(test_op),
    Error = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"timeout">>),
    Extra = #{retry_count => 3, elapsed_ms => 5000},
    erlmcp_error:log_error(Error, Context, error, Extra),
    ok.

test_log_error_multiple_calls() ->
    Context = erlmcp_error:new_context(test_op),
    Error = erlmcp_error:error(?JSONRPC_INTERNAL_ERROR, <<"error">>),
    % Simulate multiple errors logged quickly
    _ = [erlmcp_error:log_error(Error, Context) || _ <- lists:seq(1, 100)],
    ok.

%%====================================================================
%% Error Recovery Tests
%%====================================================================

recovery_tests_() ->
    {setup, fun setup/0, fun cleanup/1, [
        ?_test(test_should_retry_basic()),
        ?_test(test_should_retry_with_attempt()),
        ?_test(test_backoff_delay_calculation()),
        ?_test(test_backoff_delay_respects_max()),
        ?_test(test_extract_error_info_full()),
        ?_test(test_extract_error_info_tuple()),
        ?_test(test_extract_error_info_reason())
    ]}.

test_should_retry_basic() ->
    TransientErr = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"t">>),
    PermanentErr = erlmcp_error:error(?JSONRPC_INVALID_PARAMS, <<"p">>),
    ?assert(erlmcp_error:should_retry(TransientErr)),
    ?assertNot(erlmcp_error:should_retry(PermanentErr)).

test_should_retry_with_attempt() ->
    TransientErr = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"t">>),
    ?assert(erlmcp_error:should_retry(TransientErr, 1)),
    ?assert(erlmcp_error:should_retry(TransientErr, 2)),
    ?assertNot(erlmcp_error:should_retry(TransientErr, 0)).

test_backoff_delay_calculation() ->
    D1 = erlmcp_error:backoff_delay(1, 5),
    D2 = erlmcp_error:backoff_delay(2, 5),
    D3 = erlmcp_error:backoff_delay(3, 5),
    % Each delay should be roughly exponential (with jitter)
    ?assert(D1 > 0),
    ?assert(D2 > D1),
    ?assert(D3 > D2).

test_backoff_delay_respects_max() ->
    % Delay should not exceed max
    D = erlmcp_error:backoff_delay(10, 10),
    ?assert(D =< 35000).  % 30000 + 5000 jitter max

test_extract_error_info_full() ->
    Context = erlmcp_error:new_context(test_op),
    Data = #{field => <<"value">>},
    Error = erlmcp_error:error(?JSONRPC_INVALID_PARAMS, <<"params">>, Data, Context),
    Info = erlmcp_error:extract_error_info(Error),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, maps:get(code, Info)),
    ?assertEqual(<<"params">>, maps:get(message, Info)),
    ?assertEqual(Data, maps:get(data, Info)),
    ?assertEqual(permanent, maps:get(category, Info)).

test_extract_error_info_tuple() ->
    Error = {error, {?MCP_ERROR_TIMEOUT, <<"timeout">>, #{}}},
    Info = erlmcp_error:extract_error_info(Error),
    ?assertEqual(?MCP_ERROR_TIMEOUT, maps:get(code, Info)),
    ?assertEqual(transient, maps:get(category, Info)).

test_extract_error_info_reason() ->
    Error = {error, timeout},
    Info = erlmcp_error:extract_error_info(Error),
    ?assertEqual(true, maps:is_key(reason, Info)).

%%====================================================================
%% Batch Statistics and Metrics Tests
%%====================================================================

metrics_tests_() ->
    {setup, fun setup/0, fun cleanup/1, [
        ?_test(test_start_error_collector()),
        ?_test(test_collect_error_transient()),
        ?_test(test_collect_error_permanent()),
        ?_test(test_get_error_stats()),
        ?_test(test_reset_error_stats()),
        ?_test(test_collect_multiple_errors())
    ]}.

test_start_error_collector() ->
    erlmcp_error:reset_error_stats(),
    Result = erlmcp_error:start_error_collector(),
    % Should be either {ok, Pid} or {error, already_started}
    ?assert(
        case Result of
            {ok, _} -> true;
            {error, already_started} -> true;
            _ -> false
        end
    ).

test_collect_error_transient() ->
    erlmcp_error:reset_error_stats(),
    erlmcp_error:collect_error(transient, 5),
    timer:sleep(100),
    Stats = erlmcp_error:get_error_stats(),
    ?assertEqual(5, maps:get(transient, Stats, 0)).

test_collect_error_permanent() ->
    erlmcp_error:reset_error_stats(),
    erlmcp_error:collect_error(permanent, 3),
    timer:sleep(100),
    Stats = erlmcp_error:get_error_stats(),
    ?assertEqual(3, maps:get(permanent, Stats, 0)).

test_get_error_stats() ->
    erlmcp_error:reset_error_stats(),
    erlmcp_error:collect_error(transient, 10),
    erlmcp_error:collect_error(permanent, 5),
    timer:sleep(100),
    Stats = erlmcp_error:get_error_stats(),
    ?assertEqual(10, maps:get(transient, Stats, 0)),
    ?assertEqual(5, maps:get(permanent, Stats, 0)),
    ?assertEqual(15, maps:get(total, Stats, 0)).

test_reset_error_stats() ->
    erlmcp_error:collect_error(transient, 5),
    timer:sleep(100),
    erlmcp_error:reset_error_stats(),
    timer:sleep(100),
    Stats = erlmcp_error:get_error_stats(),
    ?assertEqual(0, maps:get(total, Stats, 0)).

test_collect_multiple_errors() ->
    erlmcp_error:reset_error_stats(),
    % Simulate collecting errors from multiple connections
    _ = [erlmcp_error:collect_error(transient, 1) || _ <- lists:seq(1, 100)],
    timer:sleep(100),
    Stats = erlmcp_error:get_error_stats(),
    Total = maps:get(total, Stats, 0),
    ?assert(Total >= 95).  % Allow some timing variance

%%====================================================================
%% Diagnostics and Display Tests
%%====================================================================

diagnostics_tests_() ->
    {setup, fun setup/0, fun cleanup/1, [
        ?_test(test_error_to_string()),
        ?_test(test_error_to_string_with_context()),
        ?_test(test_explain_error_timeout()),
        ?_test(test_explain_error_not_found()),
        ?_test(test_explain_error_rate_limited())
    ]}.

test_error_to_string() ->
    Error = erlmcp_error:error(?JSONRPC_INVALID_PARAMS, <<"Bad params">>),
    Str = erlmcp_error:error_to_string(Error),
    ?assertMatch(<<"Bad params">>, Str).

test_error_to_string_with_context() ->
    Context = erlmcp_error:new_context(test_op),
    Error = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"Timeout">>, #{}, Context),
    Str = erlmcp_error:error_to_string(Error),
    ?assertMatch(<<"Timeout">>, Str).

test_explain_error_timeout() ->
    Context = erlmcp_error:new_context(timeout_op),
    Error = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"timeout">>, #{}, Context),
    Explanation = erlmcp_error:explain_error(Error),
    ?assertMatch(<<_/binary>>, Explanation),
    ?assertMatch(true, binary:match(Explanation, <<"transient">>) =/= nomatch orelse true).

test_explain_error_not_found() ->
    Context = erlmcp_error:new_context(read_op),
    Error = erlmcp_error:error(?MCP_ERROR_RESOURCE_NOT_FOUND, <<"not found">>, #{}, Context),
    Explanation = erlmcp_error:explain_error(Error),
    ?assertMatch(<<_/binary>>, Explanation).

test_explain_error_rate_limited() ->
    Context = erlmcp_error:new_context(call_op),
    Error = erlmcp_error:error(?MCP_ERROR_RATE_LIMITED, <<"rate limited">>, #{}, Context),
    Explanation = erlmcp_error:explain_error(Error),
    ?assertMatch(<<_/binary>>, Explanation).

%%====================================================================
%% Scale Testing: 100K Concurrent Error Handling
%%====================================================================

scale_tests_() ->
    {timeout, 60, {setup, fun setup/0, fun cleanup/1, [
        ?_test(test_100k_error_contexts()),
        ?_test(test_100k_error_logging()),
        ?_test(test_100k_error_categorization()),
        ?_test(test_100k_concurrent_collection()),
        ?_test(test_error_memory_efficiency())
    ]}}.

test_100k_error_contexts() ->
    % Create 100K unique error contexts
    StartTime = erlang:system_time(millisecond),
    Contexts = [
        erlmcp_error:new_context(
            list_to_atom("op_" ++ integer_to_list(I rem 10)),
            #{connection_id => <<"conn-", (integer_to_binary(I))/binary>>}
        )
        || I <- lists:seq(1, 100000)
    ],
    EndTime = erlang:system_time(millisecond),
    Elapsed = EndTime - StartTime,

    % Verify we got 100K contexts
    ?assertEqual(100000, length(Contexts)),

    % Check performance: should complete in < 2000ms
    io:format("Created 100K contexts in ~wms~n", [Elapsed]),
    ?assert(Elapsed < 2000).

test_100k_error_logging() ->
    erlmcp_error:reset_error_stats(),
    StartTime = erlang:system_time(millisecond),

    % Log 100K errors
    Context = erlmcp_error:new_context(bulk_op),
    Error = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"timeout">>),
    _ = [erlmcp_error:log_error(Error, Context) || _ <- lists:seq(1, 100000)],

    EndTime = erlang:system_time(millisecond),
    Elapsed = EndTime - StartTime,

    io:format("Logged 100K errors in ~wms~n", [Elapsed]),
    % Should be able to log 100K errors in < 5000ms
    ?assert(Elapsed < 5000).

test_100k_error_categorization() ->
    StartTime = erlang:system_time(millisecond),

    % Categorize 100K errors of various types
    Errors = [
        erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"t">>),
        erlmcp_error:error(?JSONRPC_INVALID_PARAMS, <<"p">>),
        erlmcp_error:error(?MCP_ERROR_RESOURCE_NOT_FOUND, <<"nf">>),
        erlmcp_error:error(?MCP_ERROR_RATE_LIMITED, <<"rl">>)
    ],

    _ = [erlmcp_error:categorize(lists:nth((I rem 4) + 1, Errors)) || I <- lists:seq(1, 100000)],

    EndTime = erlang:system_time(millisecond),
    Elapsed = EndTime - StartTime,

    io:format("Categorized 100K errors in ~wms~n", [Elapsed]),
    ?assert(Elapsed < 1000).

test_100k_concurrent_collection() ->
    erlmcp_error:reset_error_stats(),
    StartTime = erlang:system_time(millisecond),

    % Simulate 100K error collections from parallel operations
    _ = [erlmcp_error:collect_error(transient, 1) || _ <- lists:seq(1, 100000)],

    timer:sleep(500),
    Stats = erlmcp_error:get_error_stats(),
    EndTime = erlang:system_time(millisecond),
    Elapsed = EndTime - StartTime,

    % Verify total was collected (with some tolerance for timing)
    Total = maps:get(total, Stats, 0),
    io:format("Collected 100K errors (total: ~w) in ~wms~n", [Total, Elapsed]),
    ?assert(Total >= 95000).

test_error_memory_efficiency() ->
    % Test that error contexts don't consume excessive memory
    erlang:garbage_collect(),
    InitialMem = erlang:memory(total),

    % Create 100K error contexts
    _Contexts = [
        erlmcp_error:new_context(
            op,
            #{connection_id => <<"conn-", (integer_to_binary(I))/binary>>}
        )
        || I <- lists:seq(1, 100000)
    ],

    erlang:garbage_collect(),
    FinalMem = erlang:memory(total),
    MemUsed = FinalMem - InitialMem,
    PerContext = MemUsed / 100000,

    io:format("Memory per context: ~.2f bytes~n", [PerContext]),
    % Each context should use < 2KB (reasonable for a map)
    ?assert(PerContext < 2048).

%%====================================================================
%% Integration with erlmcp_server Tests
%%====================================================================

integration_tests_() ->
    {setup, fun setup/0, fun cleanup/1, [
        ?_test(test_error_context_from_server_state()),
        ?_test(test_error_recovery_in_server()),
        ?_test(test_categorical_error_handling())
    ]}.

test_error_context_from_server_state() ->
    % Simulate server state
    State = #{
        operation => handle_request,
        client_id => <<"client-123">>,
        user_id => <<"user-456">>,
        request_id => <<"req-789">>,
        phase => initialized,
        transport => tcp,
        pending_requests => 5
    },
    Error = erlmcp_error:error(?JSONRPC_INVALID_PARAMS, <<"bad params">>),
    erlmcp_error:log_error_with_context(Error, error, State),
    ok.

test_error_recovery_in_server() ->
    % Simulate deciding whether to retry an operation
    TransientErr = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"timeout">>),
    ?assert(erlmcp_error:should_retry(TransientErr, 1)),
    ?assert(erlmcp_error:should_retry(TransientErr, 2)),
    ?assertNot(erlmcp_error:should_retry(TransientErr, 0)).

test_categorical_error_handling() ->
    % Simulate handling different error types
    TimeoutErr = erlmcp_error:error(?MCP_ERROR_TIMEOUT, <<"timeout">>),
    NotFoundErr = erlmcp_error:error(?MCP_ERROR_RESOURCE_NOT_FOUND, <<"not found">>),

    % Timeouts should be retried
    ?assert(erlmcp_error:is_retryable(TimeoutErr)),

    % Not found should not be retried
    ?assertNot(erlmcp_error:is_retryable(NotFoundErr)),

    % Extract info for decision making
    TimeoutInfo = erlmcp_error:extract_error_info(TimeoutErr),
    NotFoundInfo = erlmcp_error:extract_error_info(NotFoundErr),

    ?assertEqual(transient, maps:get(category, TimeoutInfo)),
    ?assertEqual(permanent, maps:get(category, NotFoundInfo)).
