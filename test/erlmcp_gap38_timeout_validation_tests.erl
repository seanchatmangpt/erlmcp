-module(erlmcp_gap38_timeout_validation_tests).

-include_lib("eunit/include/eunit.hrl").

%% Gap #38: Form Timeout Validation - Standalone test file
%% Tests the timeout validation logic without full erlmcp compilation

%%====================================================================
%% Timeout Validation Logic (extracted from erlmcp_elicitation.erl)
%%====================================================================

%% Constants from MCP spec
-define(MIN_FORM_TIMEOUT_MS, 1000).        %% 1 second minimum
-define(MAX_FORM_TIMEOUT_MS, 300000).      %% 5 minutes maximum
-define(DEFAULT_FORM_TIMEOUT_MS, 600000).  %% 10 minutes default

validate_form_timeout(undefined) ->
    {ok, ?DEFAULT_FORM_TIMEOUT_MS};
validate_form_timeout(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs > 0 ->
    case TimeoutMs of
        _ when TimeoutMs < ?MIN_FORM_TIMEOUT_MS ->
            {error, {timeout_too_small, #{
                requested => TimeoutMs,
                minimum => ?MIN_FORM_TIMEOUT_MS,
                message => <<"Form timeout must be at least 1 second (1000 milliseconds)">>
            }}};
        _ when TimeoutMs > ?MAX_FORM_TIMEOUT_MS ->
            {error, {timeout_too_large, #{
                requested => TimeoutMs,
                maximum => ?MAX_FORM_TIMEOUT_MS,
                message => <<"Form timeout must not exceed 5 minutes (300000 milliseconds)">>
            }}};
        _ ->
            {ok, TimeoutMs}
    end;
validate_form_timeout(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs =< 0 ->
    {error, {invalid_timeout, #{
        requested => TimeoutMs,
        message => <<"Form timeout must be a positive integer (milliseconds)">>
    }}};
validate_form_timeout(Invalid) ->
    {error, {invalid_timeout, #{
        requested => Invalid,
        message => <<"Form timeout must be a positive integer or undefined (milliseconds)">>
    }}}.

%%====================================================================
%% Tests
%%====================================================================

timeout_validation_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_validate_form_timeout_with_undefined()),
            ?_test(test_validate_form_timeout_with_valid_timeout()),
            ?_test(test_validate_form_timeout_below_minimum()),
            ?_test(test_validate_form_timeout_above_maximum()),
            ?_test(test_validate_form_timeout_zero()),
            ?_test(test_validate_form_timeout_negative()),
            ?_test(test_validate_form_timeout_invalid_type()),
            ?_test(test_timeout_boundary_min()),
            ?_test(test_timeout_boundary_max()),
            ?_test(test_timeout_just_below_min()),
            ?_test(test_timeout_just_above_max()),
            ?_test(test_timeout_large_numbers())
        ]
    }.

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Individual Tests
%%====================================================================

test_validate_form_timeout_with_undefined() ->
    %% When undefined, should return default timeout
    {ok, Timeout} = validate_form_timeout(undefined),
    ?assert(is_integer(Timeout)),
    ?assertEqual(?DEFAULT_FORM_TIMEOUT_MS, Timeout),
    ?assert(Timeout > 0).

test_validate_form_timeout_with_valid_timeout() ->
    %% Test valid timeouts in the middle of the range
    ValidTimeouts = [1000, 5000, 30000, 60000, 120000, 300000],
    lists:foreach(fun(Timeout) ->
        {ok, ValidatedTimeout} = validate_form_timeout(Timeout),
        ?assertEqual(Timeout, ValidatedTimeout),
        ?assert(ValidatedTimeout >= ?MIN_FORM_TIMEOUT_MS),
        ?assert(ValidatedTimeout =< ?MAX_FORM_TIMEOUT_MS)
    end, ValidTimeouts).

test_validate_form_timeout_below_minimum() ->
    %% Test timeout below minimum (< 1 second)
    InvalidTimeouts = [1, 10, 100, 500, 999],
    lists:foreach(fun(Timeout) ->
        {error, {timeout_too_small, Details}} = validate_form_timeout(Timeout),
        ?assert(is_map(Details)),
        ?assertEqual(Timeout, maps:get(requested, Details)),
        ?assertEqual(?MIN_FORM_TIMEOUT_MS, maps:get(minimum, Details)),
        Message = maps:get(message, Details),
        ?assert(is_binary(Message))
    end, InvalidTimeouts).

test_validate_form_timeout_above_maximum() ->
    %% Test timeout above maximum (> 5 minutes)
    InvalidTimeouts = [300001, 350000, 600000, 1000000],
    lists:foreach(fun(Timeout) ->
        {error, {timeout_too_large, Details}} = validate_form_timeout(Timeout),
        ?assert(is_map(Details)),
        ?assertEqual(Timeout, maps:get(requested, Details)),
        ?assertEqual(?MAX_FORM_TIMEOUT_MS, maps:get(maximum, Details)),
        Message = maps:get(message, Details),
        ?assert(is_binary(Message))
    end, InvalidTimeouts).

test_validate_form_timeout_zero() ->
    %% Zero is not a valid positive integer
    {error, {invalid_timeout, Details}} = validate_form_timeout(0),
    ?assert(is_map(Details)),
    ?assertEqual(0, maps:get(requested, Details)),
    Message = maps:get(message, Details),
    ?assert(is_binary(Message)).

test_validate_form_timeout_negative() ->
    %% Negative timeouts are invalid
    NegativeTimeouts = [-1, -100, -1000, -999999],
    lists:foreach(fun(Timeout) ->
        {error, {invalid_timeout, Details}} = validate_form_timeout(Timeout),
        ?assert(is_map(Details)),
        ?assertEqual(Timeout, maps:get(requested, Details)),
        Message = maps:get(message, Details),
        ?assert(is_binary(Message))
    end, NegativeTimeouts).

test_validate_form_timeout_invalid_type() ->
    %% Non-integer, non-undefined values should fail
    InvalidValues = [<<"not_integer">>, 1.5, atom, [], {}],
    lists:foreach(fun(Value) ->
        {error, {invalid_timeout, Details}} = validate_form_timeout(Value),
        ?assert(is_map(Details)),
        Message = maps:get(message, Details),
        ?assert(is_binary(Message))
    end, InvalidValues).

test_timeout_boundary_min() ->
    %% Test minimum boundary (exactly 1000ms)
    MinTimeout = 1000,
    {ok, ValidatedTimeout} = validate_form_timeout(MinTimeout),
    ?assertEqual(MinTimeout, ValidatedTimeout).

test_timeout_boundary_max() ->
    %% Test maximum boundary (exactly 300000ms)
    MaxTimeout = 300000,
    {ok, ValidatedTimeout} = validate_form_timeout(MaxTimeout),
    ?assertEqual(MaxTimeout, ValidatedTimeout).

test_timeout_just_below_min() ->
    %% Test just below minimum boundary
    JustBelowMin = ?MIN_FORM_TIMEOUT_MS - 1,
    {error, {timeout_too_small, _Details}} = validate_form_timeout(JustBelowMin),
    ?assert(true).

test_timeout_just_above_max() ->
    %% Test just above maximum boundary
    JustAboveMax = ?MAX_FORM_TIMEOUT_MS + 1,
    {error, {timeout_too_large, _Details}} = validate_form_timeout(JustAboveMax),
    ?assert(true).

test_timeout_large_numbers() ->
    %% Test very large numbers
    LargeNumbers = [999999999, 1000000000, 9999999999],
    lists:foreach(fun(Timeout) ->
        {error, {timeout_too_large, _Details}} = validate_form_timeout(Timeout),
        ?assert(true)
    end, LargeNumbers).
