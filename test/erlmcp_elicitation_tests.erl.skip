-module(erlmcp_elicitation_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    {ok, _} = erlmcp_elicitation:start_link(),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

elicitation_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_create_form()),
            ?_test(test_create_url_elicitation()),
            ?_test(test_get_elicitation()),
            ?_test(test_submit_response()),
            ?_test(test_cancel_elicitation()),
            ?_test(test_form_expiry()),
            ?_test(test_concurrent_elicitations()),
            ?_test(test_invalid_elicitation_id()),

            %% Gap #38: Form Timeout Validation Tests
            ?_test(test_validate_form_timeout_with_undefined()),
            ?_test(test_validate_form_timeout_with_valid_timeout()),
            ?_test(test_validate_form_timeout_below_minimum()),
            ?_test(test_validate_form_timeout_above_maximum()),
            ?_test(test_validate_form_timeout_zero()),
            ?_test(test_validate_form_timeout_negative()),
            ?_test(test_validate_form_timeout_invalid_type()),
            ?_test(test_create_form_with_custom_timeout()),
            ?_test(test_create_form_with_invalid_timeout()),
            ?_test(test_create_url_with_custom_timeout()),
            ?_test(test_create_url_with_invalid_timeout()),
            ?_test(test_get_form_timeout_config()),
            ?_test(test_form_expiration_with_custom_timeout()),
            ?_test(test_timeout_boundary_min()),
            ?_test(test_timeout_boundary_max())
        ]
    }.

%%====================================================================
%% Individual Tests
%%====================================================================

test_create_form() ->
    ServerPid = self(),
    Title = <<"Personal Information">>,
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>},
            <<"email">> => #{<<"type">> => <<"string">>, <<"format">> => <<"email">>}
        }
    },

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema),
    ?assert(is_binary(ElicitationId)),
    ?assert(byte_size(ElicitationId) > 0).

test_create_url_elicitation() ->
    ServerPid = self(),
    Title = <<"Approve Action">>,
    Url = <<"https://example.com/approve">>,

    {ok, ElicitationId} = erlmcp_elicitation:create_url_elicitation(ServerPid, Title, Url),
    ?assert(is_binary(ElicitationId)).

test_get_elicitation() ->
    ServerPid = self(),
    Title = <<"Test Form">>,
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"answer">> => #{<<"type">> => <<"string">>}
        }
    },

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema),
    {ok, Elicitation} = erlmcp_elicitation:get_elicitation(ElicitationId),

    ?assert(is_map(Elicitation)),
    ?assert(maps:get(<<"id">>, Elicitation) =:= ElicitationId),
    ?assert(maps:get(<<"title">>, Elicitation) =:= Title),
    ?assert(maps:get(<<"type">>, Elicitation) =:= <<"form">>).

test_submit_response() ->
    ServerPid = self(),
    Title = <<"Test Form">>,
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"answer">> => #{<<"type">> => <<"string">>}
        }
    },

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema),

    Response = #{<<"answer">> => <<"test response">>},
    {ok, SubmittedId} = erlmcp_elicitation:submit_response(ElicitationId, Response),

    ?assert(SubmittedId =:= ElicitationId),

    {ok, UpdatedElicitation} = erlmcp_elicitation:get_elicitation(ElicitationId),
    ?assert(maps:get(<<"status">>, UpdatedElicitation) =:= <<"completed">>).

test_cancel_elicitation() ->
    ServerPid = self(),
    Title = <<"Test Form">>,
    Schema = #{
        <<"type">> => <<"object">>
    },

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema),
    {ok, CancelledId} = erlmcp_elicitation:cancel_elicitation(ElicitationId),

    ?assert(CancelledId =:= ElicitationId),

    {ok, UpdatedElicitation} = erlmcp_elicitation:get_elicitation(ElicitationId),
    ?assert(maps:get(<<"status">>, UpdatedElicitation) =:= <<"cancelled">>).

test_form_expiry() ->
    ServerPid = self(),
    Title = <<"Test Form">>,
    Schema = #{
        <<"type">> => <<"object">>
    },

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema),
    {ok, Elicitation} = erlmcp_elicitation:get_elicitation(ElicitationId),

    CreatedAt = maps:get(<<"createdAt">>, Elicitation),
    ExpiresAt = maps:get(<<"expiresAt">>, Elicitation),

    ?assert(ExpiresAt > CreatedAt),
    ?assert(ExpiresAt - CreatedAt =:= 600000).  %% 10 minutes

test_concurrent_elicitations() ->
    ServerPid = self(),
    Title = <<"Concurrent Test">>,
    Schema = #{<<"type">> => <<"object">>},

    Ids = lists:map(
        fun(_) ->
            {ok, Id} = erlmcp_elicitation:create_form(ServerPid, Title, Schema),
            Id
        end,
        lists:seq(1, 5)
    ),

    ?assert(length(Ids) =:= 5),
    ?assert(length(lists:usort(Ids)) =:= 5).  %% All IDs unique

test_invalid_elicitation_id() ->
    InvalidId = <<"invalid_id_does_not_exist">>,
    {error, not_found} = erlmcp_elicitation:get_elicitation(InvalidId),

    ?assert(true).

%%====================================================================
%% Gap #38: Form Timeout Validation Tests
%%====================================================================

test_validate_form_timeout_with_undefined() ->
    %% When undefined, should return default timeout
    {ok, Timeout} = erlmcp_elicitation:validate_form_timeout(undefined),
    ?assert(is_integer(Timeout)),
    ?assert(Timeout > 0),
    %% Default should be reasonable (between 1s and 5m)
    ?assert(Timeout >= 1000),
    ?assert(Timeout =< 600000).

test_validate_form_timeout_with_valid_timeout() ->
    %% Test valid timeouts in the middle of the range
    ValidTimeouts = [1000, 5000, 30000, 60000, 120000, 300000],
    lists:foreach(fun(Timeout) ->
        {ok, ValidatedTimeout} = erlmcp_elicitation:validate_form_timeout(Timeout),
        ?assertEqual(Timeout, ValidatedTimeout)
    end, ValidTimeouts).

test_validate_form_timeout_below_minimum() ->
    %% Test timeout below minimum (< 1 second)
    InvalidTimeouts = [1, 10, 100, 500, 999],
    lists:foreach(fun(Timeout) ->
        {error, {timeout_too_small, Details}} = erlmcp_elicitation:validate_form_timeout(Timeout),
        ?assert(is_map(Details)),
        ?assertEqual(Timeout, maps:get(requested, Details)),
        ?assertMatch(Msg when is_binary(Msg), maps:get(message, Details))
    end, InvalidTimeouts).

test_validate_form_timeout_above_maximum() ->
    %% Test timeout above maximum (> 5 minutes)
    InvalidTimeouts = [300001, 350000, 600000, 1000000],
    lists:foreach(fun(Timeout) ->
        {error, {timeout_too_large, Details}} = erlmcp_elicitation:validate_form_timeout(Timeout),
        ?assert(is_map(Details)),
        ?assertEqual(Timeout, maps:get(requested, Details)),
        ?assertMatch(Msg when is_binary(Msg), maps:get(message, Details))
    end, InvalidTimeouts).

test_validate_form_timeout_zero() ->
    %% Zero is not a valid positive integer
    {error, {invalid_timeout, Details}} = erlmcp_elicitation:validate_form_timeout(0),
    ?assert(is_map(Details)),
    ?assertEqual(0, maps:get(requested, Details)).

test_validate_form_timeout_negative() ->
    %% Negative timeouts are invalid
    NegativeTimeouts = [-1, -100, -1000],
    lists:foreach(fun(Timeout) ->
        {error, {invalid_timeout, Details}} = erlmcp_elicitation:validate_form_timeout(Timeout),
        ?assert(is_map(Details)),
        ?assertEqual(Timeout, maps:get(requested, Details))
    end, NegativeTimeouts).

test_validate_form_timeout_invalid_type() ->
    %% Non-integer, non-undefined values should fail
    InvalidValues = [<<"not_integer">>, 1.5, atom, [], {}],
    lists:foreach(fun(Value) ->
        {error, {invalid_timeout, Details}} = erlmcp_elicitation:validate_form_timeout(Value),
        ?assert(is_map(Details))
    end, InvalidValues).

test_create_form_with_custom_timeout() ->
    %% Create form with explicit valid timeout
    ServerPid = self(),
    Title = <<"Test Form with Timeout">>,
    Schema = #{<<"type">> => <<"object">>},
    ValidTimeout = 30000,  % 30 seconds

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema, ValidTimeout),
    ?assert(is_binary(ElicitationId)),

    %% Verify the timeout is stored correctly
    {ok, Elicitation} = erlmcp_elicitation:get_elicitation(ElicitationId),
    ?assertEqual(ValidTimeout, maps:get(<<"timeoutMs">>, Elicitation)).

test_create_form_with_invalid_timeout() ->
    %% Create form with invalid timeout should fail
    ServerPid = self(),
    Title = <<"Test Form with Invalid Timeout">>,
    Schema = #{<<"type">> => <<"object">>},
    InvalidTimeout = 500,  % Too small (< 1000ms)

    {error, {timeout_too_small, _Details}} = erlmcp_elicitation:create_form(ServerPid, Title, Schema, InvalidTimeout),
    ?assert(true).

test_create_url_with_custom_timeout() ->
    %% Create URL elicitation with explicit valid timeout
    ServerPid = self(),
    Title = <<"Test URL with Timeout">>,
    Url = <<"https://example.com/approve">>,
    ValidTimeout = 60000,  % 60 seconds

    {ok, ElicitationId} = erlmcp_elicitation:create_url_elicitation(ServerPid, Title, Url, ValidTimeout),
    ?assert(is_binary(ElicitationId)),

    %% Verify the timeout is stored correctly
    {ok, Elicitation} = erlmcp_elicitation:get_elicitation(ElicitationId),
    ?assertEqual(ValidTimeout, maps:get(<<"timeoutMs">>, Elicitation)).

test_create_url_with_invalid_timeout() ->
    %% Create URL elicitation with invalid timeout should fail
    ServerPid = self(),
    Title = <<"Test URL with Invalid Timeout">>,
    Url = <<"https://example.com/approve">>,
    InvalidTimeout = 600001,  % Too large (> 300000ms)

    {error, {timeout_too_large, _Details}} = erlmcp_elicitation:create_url_elicitation(ServerPid, Title, Url, InvalidTimeout),
    ?assert(true).

test_get_form_timeout_config() ->
    %% Test getting timeout configuration
    Config = erlmcp_elicitation:get_form_timeout_config(),
    ?assert(is_map(Config)),
    ?assert(maps:is_key(default_ms, Config)),
    ?assert(maps:is_key(min_ms, Config)),
    ?assert(maps:is_key(max_ms, Config)),
    ?assert(maps:is_key(config_source, Config)),

    Default = maps:get(default_ms, Config),
    Min = maps:get(min_ms, Config),
    Max = maps:get(max_ms, Config),

    %% Validate relationships
    ?assert(Default >= Min),
    ?assert(Max >= Min),
    ?assert(Max >= 1000),
    ?assert(Min >= 1000).

test_form_expiration_with_custom_timeout() ->
    %% Create form with specific timeout and verify expiration time
    ServerPid = self(),
    Title = <<"Expiring Form">>,
    Schema = #{<<"type">> => <<"object">>},
    CustomTimeout = 15000,  % 15 seconds

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema, CustomTimeout),
    {ok, Elicitation} = erlmcp_elicitation:get_elicitation(ElicitationId),

    CreatedAt = maps:get(<<"createdAt">>, Elicitation),
    ExpiresAt = maps:get(<<"expiresAt">>, Elicitation),
    TimeoutMs = maps:get(<<"timeoutMs">>, Elicitation),

    %% Verify expiration calculation
    ?assertEqual(CustomTimeout, TimeoutMs),
    ?assertEqual(CustomTimeout, ExpiresAt - CreatedAt).

test_timeout_boundary_min() ->
    %% Test minimum boundary (exactly 1000ms)
    ServerPid = self(),
    Title = <<"Min Boundary Test">>,
    Schema = #{<<"type">> => <<"object">>},
    MinTimeout = 1000,

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema, MinTimeout),
    ?assert(is_binary(ElicitationId)),

    {ok, Elicitation} = erlmcp_elicitation:get_elicitation(ElicitationId),
    ?assertEqual(MinTimeout, maps:get(<<"timeoutMs">>, Elicitation)).

test_timeout_boundary_max() ->
    %% Test maximum boundary (exactly 300000ms)
    ServerPid = self(),
    Title = <<"Max Boundary Test">>,
    Schema = #{<<"type">> => <<"object">>},
    MaxTimeout = 300000,

    {ok, ElicitationId} = erlmcp_elicitation:create_form(ServerPid, Title, Schema, MaxTimeout),
    ?assert(is_binary(ElicitationId)),

    {ok, Elicitation} = erlmcp_elicitation:get_elicitation(ElicitationId),
    ?assertEqual(MaxTimeout, maps:get(<<"timeoutMs">>, Elicitation)).
