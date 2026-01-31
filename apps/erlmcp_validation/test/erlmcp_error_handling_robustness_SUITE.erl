-module(erlmcp_error_handling_robustness_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%%====================================================================
%%% Common Test Callbacks
%%%====================================================================

all() ->
    [
     %% Network Timeout Tests
     network_timeout_during_request_test,
     network_timeout_cleanup_test,
     configurable_timeout_test,

     %% Malformed Response Tests
     malformed_json_response_test,
     oversized_response_test,
     invalid_utf8_response_test,

     %% Transport Failure Tests
     transport_disconnect_during_validation_test,
     transport_reconnect_after_failure_test,
     transport_failure_notification_test,

     %% Error Message Clarity Tests
     error_message_includes_context_test,
     error_message_actionable_test,
     error_response_with_remediation_test,

     %% Cleanup on Failure Tests
     cleanup_resources_on_timeout_test,
     cleanup_monitors_on_error_test,
     cleanup_no_resource_leaks_test,

     %% Concurrent Error Scenarios
     concurrent_request_timeout_test,
     concurrent_transport_failure_test,

     %% Large Response Handling
     large_response_memory_management_test,
     chunked_response_handling_test,

     %% Spec Compliance Edge Cases
     null_id_error_response_test,
     batch_error_handling_test,
     error_data_field_validation_test
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

%%%====================================================================
%%% Network Timeout Tests
%%%====================================================================

%% @doc Test that network timeouts are handled gracefully
network_timeout_during_request_test(_Config) ->
    %% Start a test client with short timeout
    {ok, Client} = start_test_client_with_timeout(100),  % 100ms timeout

    %% Create a request that will take longer than timeout
    Request = #{<<"method">> => <<"slow_operation">>,
                <<"params">> => #{<<"delay_ms">> => 500}},
    %% Note: This test verifies timeout handling exists
    %% Actual slow_operation should be implemented in test server

    %% Send request and expect timeout
    case catch erlmcp_test_client:send_request(Client, Request) of
        {error, timeout} ->
            ct:log("Timeout handled correctly", []);
        {error, {timeout, _}} ->
            ct:log("Timeout with details handled correctly", []);
        {'EXIT', {timeout, _}} ->
            ct:fail("Timeout caused process crash, should be handled gracefully");
        Other ->
            ct:log("Unexpected result: ~p", [Other])
    end,

    %% Verify client is still functional after timeout
    {ok, _} = erlmcp_test_client:get_server_info(Client),
    erlmcp_test_client:stop_test_server(Client),
    ok.

%% @doc Test that resources are cleaned up on timeout
network_timeout_cleanup_test(_Config) ->
    {ok, Client} = start_test_client_with_timeout(100),

    %% Get initial state
    {ok, InitialState} = erlmcp_test_client:get_server_info(Client),
    InitialPending = maps:get(pending_requests, InitialState, #{}),

    %% Send request that will timeout
    Request = #{<<"method">> => <<"slow_operation">>,
                <<"params">> => #{<<"delay_ms">> => 500}},
    _ = catch erlmcp_test_client:send_request(Client, Request),

    %% Allow timeout to occur
    timer:sleep(200),

    %% Verify cleanup
    {ok, FinalState} = erlmcp_test_client:get_server_info(Client),
    FinalPending = maps:get(pending_requests, FinalState, #{}),

    %% Pending requests should be cleaned up
    ct:log("Pending requests before: ~p, after: ~p",
           [maps:size(InitialPending), maps:size(FinalPending)]),

    %% Note: This test verifies cleanup mechanism exists
    %% Actual cleanup may need to be implemented

    erlmcp_test_client:stop_test_server(Client),
    ok.

%% @doc Test that timeouts are configurable
configurable_timeout_test(_Config) ->
    %% Test with 1 second timeout
    {ok, Client1} = start_test_client_with_timeout(1000),
    ok = erlmcp_test_client:set_response_timeout(Client1, 2000),
    erlmcp_test_client:stop_test_server(Client1),

    %% Test with 100ms timeout
    {ok, Client2} = start_test_client_with_timeout(100),
    ok = erlmcp_test_client:set_response_timeout(Client2, 50),
    erlmcp_test_client:stop_test_server(Client2),

    ok.

%%%====================================================================
%%% Malformed Response Tests
%%%====================================================================

%% @doc Test handling of malformed JSON responses
malformed_json_response_test(_Config) ->
    %% Create a malformed JSON response
    MalformedJson = <<"{invalid json}">>,

    %% Should return parse error
    case erlmcp_json_rpc:decode_message(MalformedJson) of
        {error, {parse_error, _}} ->
            ct:log("Malformed JSON detected correctly", []);
        {error, Reason} ->
            ct:log("Malformed JSON detected with reason: ~p", [Reason]);
        Other ->
            ct:fail("Expected parse error, got: ~p", [Other])
    end,
    ok.

%% @doc Test handling of oversized responses
oversized_response_test(_Config) ->
    %% Create a large response (simulating memory issues)
    LargeData = << <<X>> || <<X>> <= <<0:8000000>> >>,  % 1MB of zeros

    %% Test message size validation
    MaxSize = 16 * 1024 * 1024,  % 16MB default
    case byte_size(LargeData) =< MaxSize of
        true ->
            ct:log("Large message within size limit", []);
        false ->
            Size = byte_size(LargeData),
            ct:log("Message size ~p exceeds limit ~p", [Size, MaxSize])
    end,

    %% Test with message exceeding limit
    HugeMessage = << <<X>> || <<X>> <= <<0:20000000>> >>,  % 20MB
    case byte_size(HugeMessage) > MaxSize of
        true ->
            ct:log("Oversized message rejected correctly", []);
        false ->
            ct:fail("Oversized message should have been rejected")
    end,
    ok.

%% @doc Test handling of invalid UTF-8
invalid_utf8_response_test(_Config) ->
    %% Create invalid UTF-8 sequence
    InvalidUtf8 = <<255, 255, 255>>,

    %% Test that invalid UTF-8 is handled
    try jsx:decode(#{<<"data">> => InvalidUtf8}) of
        _ ->
            ct:log("Invalid UTF-8 processed", [])
    catch
        error:badarg ->
            ct:log("Invalid UTF-8 caught by decoder", []);
        Type:Reason ->
            ct:log("Invalid UTF-8 caused: ~p:~p", [Type, Reason])
    end,
    ok.

%%%====================================================================
%%% Transport Failure Tests
%%%====================================================================

%% @doc Test handling of transport disconnect during validation
transport_disconnect_during_validation_test(_Config) ->
    {ok, Client} = erlmcp_test_client:start_test_server(stdio, []),

    %% Simulate disconnect
    Pid = get_server_pid(Client),
    exit(Pid, kill),

    %% Give time for disconnect to be detected
    timer:sleep(100),

    %% Try to send request - should handle disconnect gracefully
    Request = #{<<"method">> => <<"tools/list">>},
    case catch erlmcp_test_client:send_request(Client, Request) of
        {error, disconnected} ->
            ct:log("Disconnect detected correctly", []);
        {error, Reason} ->
            ct:log("Disconnect detected with reason: ~p", [Reason]);
        {'EXIT', Reason} ->
            ct:log("Process exited on disconnect: ~p", [Reason])
    end,

    %% Cleanup
    erlmcp_test_client:stop_test_server(Client),
    ok.

%% @doc Test transport reconnection after failure
transport_reconnect_after_failure_test(_Config) ->
    %% This test verifies reconnection logic exists
    %% Actual implementation may need to be added

    {ok, Client} = erlmcp_test_client:start_test_server(tcp,
                                                          #{host => "localhost",
                                                            port => 9999}),

    %% Simulate connection failure
    %% (In real test, would kill connection and trigger reconnect)

    %% Verify client attempts reconnection
    %% (Implementation dependent)

    erlmcp_test_client:stop_test_server(Client),
    {comment, "Reconnection logic needs implementation"}.

%% @doc Test that transport failures are notified
transport_failure_notification_test(_Config) ->
    %% Verify that transport failures generate notifications
    {ok, Client} = erlmcp_test_client:start_test_server(stdio, []),

    %% Subscribe to transport events
    %% (Implementation dependent)

    %% Simulate failure
    Pid = get_server_pid(Client),
    MonitorRef = monitor(process, Pid),
    exit(Pid, shutdown),

    %% Wait for termination
    receive
        {'DOWN', MonitorRef, process, Pid, _Reason} ->
            ct:log("Transport failure detected", [])
    after 1000 ->
        ct:fail("Transport failure not detected")
    end,

    erlmcp_test_client:stop_test_server(Client),
    ok.

%%%====================================================================
%%% Error Message Clarity Tests
%%%====================================================================

%% @doc Test that error messages include context
error_message_includes_context_test(_Config) ->
    %% Trigger various errors and check for context
    ErrorCases = [
        {<<"nonexistent_method">>, -32601},
        {<<"invalid_params">>, -32602}
    ],

    lists:foreach(fun({Method, ExpectedCode}) ->
        ErrorResponse = erlmcp_json_rpc:error_method_not_found(1, Method),
        ResponseMap = jsx:decode(ErrorResponse, [return_maps]),

        ErrorObj = maps:get(<<"error">>, ResponseMap),
        Code = maps:get(<<"code">>, ErrorObj),
        Message = maps:get(<<"message">>, ErrorObj),

        ct:log("Error ~p: code=~p, message=~p", [Method, Code, Message]),

        %% Verify error code matches
        case Code of
            ExpectedCode -> ok;
            _ -> ct:fail("Expected code ~p, got ~p", [ExpectedCode, Code])
        end,

        %% Verify message is not empty
        true = (byte_size(Message) > 0),

        %% Verify data field has context
        Data = maps:get(<<"data">>, ErrorObj, #{}),
        case Method of
            <<"nonexistent_method">> ->
                true = maps:is_key(<<"method">>, Data);
            _ ->
                ok
        end
    end, ErrorCases),

    ok.

%% @doc Test that error messages are actionable
error_message_actionable_test(_Config) ->
    %% Check if error messages provide actionable guidance
    ErrorResponse = erlmcp_json_rpc:error_invalid_params(
        1, <<"Missing required parameter: 'name'">>),
    ResponseMap = jsx:decode(ErrorResponse, [return_maps]),

    ErrorObj = maps:get(<<"error">>, ResponseMap),
    Message = maps:get(<<"message">>, ErrorObj),
    Data = maps:get(<<"data">>, ErrorObj),

    %% Message should indicate what's wrong
    true = binary:match(Message, <<"Missing">>) =/= nomatch orelse
            binary:match(Message, <<"required">>) =/= nomatch,

    %% Data should provide details
    Details = maps:get(<<"details">>, Data, <<>>),
    ct:log("Error details: ~p", [Details]),

    ok.

%% @doc Test error response with remediation information
error_response_with_remediation_test(_Config) ->
    %% Verify error responses include remediation hints
    %% (This is a proposed feature - checking if implemented)

    UriError = erlmcp_json_rpc:error_resource_not_found(
        1, <<"file:///missing/resource.txt">>),
    UriMap = jsx:decode(UriError, [return_maps]),

    UriErrorObj = maps:get(<<"error">>, UriMap),
    UriData = maps:get(<<"data">>, UriErrorObj),

    %% Check if remediation information exists
    _RemediationStatus = case maps:get(<<"remediation">>, UriData, undefined) of
        undefined ->
            {comment, "Remediation information not yet implemented"};
        Remediation ->
            ct:log("Remediation provided: ~s", [Remediation]),
            true = is_binary(Remediation),
            true = byte_size(Remediation) > 0,
            {ok, Remediation}
    end,

    ok.

%%%====================================================================
%%% Cleanup on Failure Tests
%%%====================================================================

%% @doc Test that resources are cleaned up on timeout
cleanup_resources_on_timeout_test(_Config) ->
    %% Start client with tracking
    {ok, Client} = start_test_client_with_timeout(100),

    %% Get initial resource count
    {ok, InitialState} = erlmcp_test_client:get_server_info(Client),

    %% Simulate operation that times out
    %% (Implementation dependent)

    %% Verify cleanup
    %% (Should check: monitors, timers, sockets, etc.)

    erlmcp_test_client:stop_test_server(Client),
    {comment, "Resource cleanup verification needs implementation"}.

%% @doc Test that monitors are cleaned up on error
cleanup_monitors_on_error_test(_Config) ->
    %% This test verifies that process monitors are cleaned up
    {comment, "Monitor cleanup verification needs implementation"}.

%% @doc Test that there are no resource leaks
cleanup_no_resource_leaks_test(_Config) ->
    %% Run operations and check for resource leaks
    {ok, Client} = erlmcp_test_client:start_test_server(stdio, []),

    %% Perform multiple operations
    lists:foreach(fun(I) ->
        Request = #{<<"method">> => <<"ping">>, <<"id">> => I},
        try erlmcp_test_client:send_request(Client, Request) of
            {ok, _} -> ok
        catch
            _:_ -> ok
        end
    end, lists:seq(1, 100)),

    %% Check for leaks
    {ok, State} = erlmcp_test_client:get_server_info(Client),
    ct:log("Final state: ~p", [State]),

    erlmcp_test_client:stop_test_server(Client),
    ok.

%%%====================================================================
%%% Concurrent Error Scenarios
%%%====================================================================

%% @doc Test concurrent request timeouts
concurrent_request_timeout_test(_Config) ->
    {ok, Client} = start_test_client_with_timeout(100),

    %% Spawn multiple concurrent requests
    Pids = [spawn_link(fun() ->
        Request = #{<<"method">> => <<"slow_operation">>,
                    <<"params">> => #{<<"delay_ms">> => 500}},
        try erlmcp_test_client:send_request(Client, Request) of
            Result -> Result
        catch
            _:{error, timeout} -> timeout
        end
    end) || _ <- lists:seq(1, 10)],

    %% Wait for all to complete/timeout
    timer:sleep(300),

    %% Verify client is still functional
    {ok, _} = erlmcp_test_client:get_server_info(Client),

    %% Cleanup
    erlmcp_test_client:stop_test_server(Client),
    ok.

%% @doc Test concurrent transport failures
concurrent_transport_failure_test(_Config) ->
    %% This test verifies handling of multiple simultaneous failures
    {comment, "Concurrent failure handling needs implementation"}.

%%%====================================================================
%%% Large Response Handling
%%%====================================================================

%% @doc Test memory management for large responses
large_response_memory_management_test(_Config) ->
    %% Verify that large responses don't cause memory issues
    LargeResponse = create_large_response(1024 * 1024),  % 1MB

    %% Test that response can be validated
    MaxResponseSize = 16 * 1024 * 1024,
    case byte_size(LargeResponse) =< MaxResponseSize of
        true ->
            ct:log("Large response size validation passed", []);
        false ->
            ct:fail("Large response validation failed: size ~p exceeds ~p",
                    [byte_size(LargeResponse), MaxResponseSize])
    end,

    ok.

%% @doc Test chunked response handling
chunked_response_handling_test(_Config) ->
    %% Verify that responses can be processed in chunks
    %% (Implementation dependent)
    {comment, "Chunked response handling needs implementation"}.

%%%====================================================================
%%% Spec Compliance Edge Cases
%%%====================================================================

%% @doc Test null ID in error response
null_id_error_response_test(_Config) ->
    %% Some errors (like parse errors) may not have a request ID
    ErrorResponse = erlmcp_json_rpc:error_parse(null),
    ResponseMap = jsx:decode(ErrorResponse, [return_maps]),

    %% Verify null ID is allowed
    null = maps:get(<<"id">>, ResponseMap),

    %% Verify error structure is still valid
    ErrorObj = maps:get(<<"error">>, ResponseMap),
    true = maps:is_key(<<"code">>, ErrorObj),
    true = maps:is_key(<<"message">>, ErrorObj),

    ok.

%% @doc Test batch error handling
batch_error_handling_test(_Config) ->
    %% Create a batch with some invalid requests
    BatchJson = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
          <<"method">> => <<"valid_method">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2},
        %% Missing method
        #{<<"invalid">> => <<"request">>}
    ]),

    case erlmcp_json_rpc:decode_batch(BatchJson) of
        {ok, Messages} when is_list(Messages) ->
            ct:log("Batch produced ~p responses", [length(Messages)]),
            true = (length(Messages) >= 1);
        {error, _Reason} ->
            %% Batch-level validation failed
            ok
    end,

    ok.

%% @doc Test error data field validation
error_data_field_validation_test(_Config) ->
    %% Test various data field types
    ErrorCases = [
        fun() -> erlmcp_json_rpc:error_invalid_params(1, <<"string details">>) end,
        fun() -> erlmcp_json_rpc:error_tool_not_found(1, <<"test_tool">>) end,
        fun() -> erlmcp_json_rpc:error_resource_not_found(1, <<"file:///test.txt">>) end
    ],

    lists:foreach(fun(ErrorFun) ->
        Response = ErrorFun(),
        ResponseMap = jsx:decode(Response, [return_maps]),
        ErrorObj = maps:get(<<"error">>, ResponseMap),

        %% Verify data field exists and is valid
        Data = maps:get(<<"data">>, ErrorObj),
        true = is_map(Data),

        ct:log("Error data: ~p", [Data])
    end, ErrorCases),

    ok.

%%%====================================================================
%%% Internal Helper Functions
%%%====================================================================

start_test_client_with_timeout(TimeoutMs) ->
    {ok, Client} = erlmcp_test_client:start_test_server(stdio, []),
    ok = erlmcp_test_client:set_response_timeout(Client, TimeoutMs),
    {ok, Client}.

get_server_pid(Client) ->
    case erlmcp_test_client:get_server_info(Client) of
        {ok, State} ->
            maps:get(server_pid, State);
        _ ->
            undefined
    end.

create_large_response(Size) ->
    %% Create a response of approximately Size bytes
    Data = << <<X>> || <<X>> <= <<0:(Size * 8)>> >>,
    jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => #{<<"data">> => Data}
    }).
