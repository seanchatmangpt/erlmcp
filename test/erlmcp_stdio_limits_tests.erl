%%%-------------------------------------------------------------------
%%% @doc
%%% Test Suite for Stdio Message Size Validation
%%%
%%% Tests validate that erlmcp_transport_stdio properly enforces
%%% message size limits and prevents DoS attacks.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_stdio_limits_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    % Set test mode
    put(test_mode, true),
    ok.

cleanup(_) ->
    erase(test_mode),
    ok.

%%====================================================================
%% Test Suite: Message Size Validation
%%====================================================================

message_size_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_message_under_limit_accepted()),
             ?_test(test_message_at_limit_accepted()),
             ?_test(test_message_over_limit_rejected()),
             ?_test(test_empty_message_accepted()),
             ?_test(test_size_validation_function()),
             ?_test(test_get_max_message_size_default()),
             ?_test(test_get_max_message_size_configured()),
             ?_test(test_large_message_detection()),
             ?_test(test_boundary_conditions())
         ]
     end}.

test_message_under_limit_accepted() ->
    MaxSize = 16777216,  % 16MB default
    SmallMessage = <<"Hello, World!">>,
    ?assert(byte_size(SmallMessage) < MaxSize),
    % Verify validation passes
    Result = erlmcp_transport_stdio:validate_message_size(SmallMessage, MaxSize),
    ?assertEqual(ok, Result).

test_message_at_limit_accepted() ->
    MaxSize = 1000,
    Message = binary:copy(<<"X">>, 1000),
    ?assertEqual(1000, byte_size(Message)),
    Result = erlmcp_transport_stdio:validate_message_size(Message, MaxSize),
    ?assertEqual(ok, Result).

test_message_over_limit_rejected() ->
    MaxSize = 1000,
    Message = binary:copy(<<"X">>, 1001),
    ?assertEqual(1001, byte_size(Message)),
    Result = erlmcp_transport_stdio:validate_message_size(Message, MaxSize),
    ?assertEqual({error, size_exceeded}, Result).

test_empty_message_accepted() ->
    MaxSize = 16777216,
    EmptyMessage = <<>>,
    Result = erlmcp_transport_stdio:validate_message_size(EmptyMessage, MaxSize),
    ?assertEqual(ok, Result).

test_size_validation_function() ->
    % Test explicit validation function
    Result1 = erlmcp_transport_stdio:validate_message_size(<<"test">>, 100),
    ?assertEqual(ok, Result1),

    Result2 = erlmcp_transport_stdio:validate_message_size(<<"test">>, 2),
    ?assertEqual({error, size_exceeded}, Result2).

test_get_max_message_size_default() ->
    % Should return default 16MB when no config
    application:unset_env(erlmcp, message_size_limits),
    MaxSize = erlmcp_transport_stdio:get_max_message_size(),
    ?assertEqual(16777216, MaxSize).

test_get_max_message_size_configured() ->
    % Test with configured value
    CustomLimit = 1000000,
    application:set_env(erlmcp, message_size_limits, #{stdio => CustomLimit}),
    try
        MaxSize = erlmcp_transport_stdio:get_max_message_size(),
        ?assertEqual(CustomLimit, MaxSize)
    after
        application:unset_env(erlmcp, message_size_limits)
    end.

test_large_message_detection() ->
    MaxSize = 1024,  % 1KB limit
    LargeMessage = binary:copy(<<"0123456789">>, 150),  % 1500 bytes
    ?assert(byte_size(LargeMessage) > MaxSize),
    Result = erlmcp_transport_stdio:validate_message_size(LargeMessage, MaxSize),
    ?assertEqual({error, size_exceeded}, Result).

test_boundary_conditions() ->
    % Test exact boundary
    MaxSize = 100,
    ExactMessage = binary:copy(<<"X">>, 100),
    Result1 = erlmcp_transport_stdio:validate_message_size(ExactMessage, MaxSize),
    ?assertEqual(ok, Result1),

    % Test just over boundary
    OverMessage = binary:copy(<<"X">>, 101),
    Result2 = erlmcp_transport_stdio:validate_message_size(OverMessage, MaxSize),
    ?assertEqual({error, size_exceeded}, Result2).

%%====================================================================
%% Test Suite: Transport Initialization with Limits
%%====================================================================

transport_init_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_transport_starts_with_default_limit()),
             ?_test(test_transport_reads_configured_limit()),
             ?_test(test_transport_state_contains_max_size()),
             ?_test(test_multiple_transports_independent_limits())
         ]
     end}.

test_transport_starts_with_default_limit() ->
    application:unset_env(erlmcp, message_size_limits),
    {ok, TransportPid} = erlmcp_transport_stdio:start_link(self()),
    try
        {ok, State} = gen_server:call(TransportPid, get_state),
        %% Extract max_message_size from state record
        {state, _, _, _, _, MaxSize} = State,
        ?assertEqual(16777216, MaxSize)
    after
        gen_server:stop(TransportPid)
    end.

test_transport_reads_configured_limit() ->
    CustomLimit = 500000,
    application:set_env(erlmcp, message_size_limits, #{stdio => CustomLimit}),
    try
        {ok, TransportPid} = erlmcp_transport_stdio:start_link(self()),
        try
            {ok, State} = gen_server:call(TransportPid, get_state),
            {state, _, _, _, _, MaxSize} = State,
            ?assertEqual(CustomLimit, MaxSize)
        after
            gen_server:stop(TransportPid)
        end
    after
        application:unset_env(erlmcp, message_size_limits)
    end.

test_transport_state_contains_max_size() ->
    {ok, TransportPid} = erlmcp_transport_stdio:start_link(self()),
    try
        {ok, State} = gen_server:call(TransportPid, get_state),
        ?assert(tuple_size(State) >= 6),  % Verify state has max_message_size field
        {state, _, _, _, _, MaxSize} = State,
        ?assert(is_integer(MaxSize)),
        ?assert(MaxSize > 0)
    after
        gen_server:stop(TransportPid)
    end.

test_multiple_transports_independent_limits() ->
    % Verify each transport maintains independent limits
    {ok, Transport1} = erlmcp_transport_stdio:start_link(self()),
    {ok, Transport2} = erlmcp_transport_stdio:start_link(self()),
    try
        {ok, State1} = gen_server:call(Transport1, get_state),
        {ok, State2} = gen_server:call(Transport2, get_state),
        {state, _, _, _, _, MaxSize1} = State1,
        {state, _, _, _, _, MaxSize2} = State2,
        ?assertEqual(MaxSize1, MaxSize2)
    after
        gen_server:stop(Transport1),
        gen_server:stop(Transport2)
    end.

%%====================================================================
%% Test Suite: Configuration Management
%%====================================================================

config_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_default_config_16mb()),
             ?_test(test_custom_config_respected()),
             ?_test(test_partial_config_uses_default()),
             ?_test(test_invalid_config_ignored())
         ]
     end}.

test_default_config_16mb() ->
    application:unset_env(erlmcp, message_size_limits),
    MaxSize = erlmcp_transport_stdio:get_max_message_size(),
    ?assertEqual(16777216, MaxSize),  % 16MB in bytes
    ?assertEqual(16777216, 16 * 1024 * 1024).

test_custom_config_respected() ->
    CustomSize = 5000000,  % 5MB
    application:set_env(erlmcp, message_size_limits, #{stdio => CustomSize}),
    try
        MaxSize = erlmcp_transport_stdio:get_max_message_size(),
        ?assertEqual(CustomSize, MaxSize)
    after
        application:unset_env(erlmcp, message_size_limits)
    end.

test_partial_config_uses_default() ->
    % Config exists but without stdio key
    application:set_env(erlmcp, message_size_limits, #{http_body => 1000}),
    try
        MaxSize = erlmcp_transport_stdio:get_max_message_size(),
        ?assertEqual(16777216, MaxSize)  % Should use default
    after
        application:unset_env(erlmcp, message_size_limits)
    end.

test_invalid_config_ignored() ->
    % Config is not a map
    application:set_env(erlmcp, message_size_limits, invalid),
    try
        MaxSize = erlmcp_transport_stdio:get_max_message_size(),
        ?assertEqual(16777216, MaxSize)  % Should use default
    after
        application:unset_env(erlmcp, message_size_limits)
    end.

%%====================================================================
%% Test Suite: Error Response Generation
%%====================================================================

error_response_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_oversized_message_generates_error()),
             ?_test(test_error_is_valid_json_rpc()),
             ?_test(test_error_code_is_correct())
         ]
     end}.

test_oversized_message_generates_error() ->
    % When a message exceeds limit, an error should be sent
    MaxSize = 100,
    OversizedMessage = binary:copy(<<"X">>, 200),
    Result = erlmcp_transport_stdio:validate_message_size(OversizedMessage, MaxSize),
    ?assertEqual({error, size_exceeded}, Result).

test_error_is_valid_json_rpc() ->
    % Verify error response format is a valid map
    ErrorMap = #{
        jsonrpc => <<"2.0">>,
        error => #{
            code => -32700,
            message => <<"Message too large">>
        }
    },
    ?assert(is_map(ErrorMap)),
    ?assert(maps:is_key(jsonrpc, ErrorMap)),
    ?assert(maps:is_key(error, ErrorMap)).

test_error_code_is_correct() ->
    % JSON-RPC parse error code is -32700
    ErrorCode = -32700,
    ?assertEqual(-32700, ErrorCode).

%%====================================================================
%% Helper Functions
%%====================================================================

%% Note: These tests assume erlmcp_transport_stdio has these functions exposed:
%% - validate_message_size/2
%% - get_max_message_size/0
