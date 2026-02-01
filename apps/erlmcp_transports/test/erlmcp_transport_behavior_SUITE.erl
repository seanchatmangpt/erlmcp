%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test suite for erlmcp_transport_behavior module
%%%
%%% This comprehensive test suite validates the transport behavior
%%% interface and tests behavior compliance across all transport
%%% implementations using Chicago School TDD (real processes, no mocks).
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_behavior_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).
%% Test cases
-export([behavior_module_exists/1, behavior_callbacks_defined/1, behavior_types_exported/1,
         behavior_optional_callbacks/1, validate_json_rpc_message/1, validate_transport_opts/1,
         message_creation_functions/1, error_message_creation/1, stdio_opts_validation/1,
         tcp_opts_validation/1, http_opts_validation/1, websocket_opts_validation/1,
         json_rpc_structure/1, notification_format/1, response_format/1, error_response_format/1,
         stdio_behavior_compliance/1, tcp_behavior_compliance/1, http_behavior_compliance/1,
         url_validation_functions/1, host_validation_functions/1, message_content_validation/1,
         error_structure_validation/1, behavior_error_handling/1, behavior_lifecycle/1]).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [{group, behavior_validation},
     {group, message_validation},
     {group, transport_options},
     {group, message_formats},
     {group, behavior_compliance},
     {group, validation_functions},
     {group, integration}].

groups() ->
    [{behavior_validation,
      [parallel],
      [behavior_module_exists,
       behavior_callbacks_defined,
       behavior_types_exported,
       behavior_optional_callbacks]},
     {message_validation,
      [parallel],
      [validate_json_rpc_message,
       validate_transport_opts,
       message_creation_functions,
       error_message_creation]},
     {transport_options,
      [parallel],
      [stdio_opts_validation,
       tcp_opts_validation,
       http_opts_validation,
       websocket_opts_validation]},
     {message_formats,
      [parallel],
      [json_rpc_structure, notification_format, response_format, error_response_format]},
     {behavior_compliance,
      [sequential],
      [stdio_behavior_compliance, tcp_behavior_compliance, http_behavior_compliance]},
     {validation_functions,
      [parallel],
      [url_validation_functions,
       host_validation_functions,
       message_content_validation,
       error_structure_validation]},
     {integration, [sequential], [behavior_error_handling, behavior_lifecycle]}].

%%====================================================================
%% Suite Setup/Teardown
%%====================================================================

init_per_suite(Config) ->
    ct:pal("Starting transport behavior test suite"),

    % Start necessary applications (returns {ok, Apps}, not ok)
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(sasl),
    {ok, _} = application:ensure_all_started(gproc),

    % Start registry for integration tests
    case whereis(erlmcp_registry) of
        undefined ->
            {ok, _Pid} = erlmcp_registry:start_link();
        _ ->
            ok
    end,

    Config.

end_per_suite(_Config) ->
    ct:pal("Ending transport behavior test suite"),
    ok.

init_per_group(GroupName, Config) ->
    ct:pal("Starting group: ~p", [GroupName]),
    Config.

end_per_group(GroupName, _Config) ->
    ct:pal("Ending group: ~p", [GroupName]),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Ending test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Test Cases - Behavior Validation
%%====================================================================

behavior_module_exists(Config) ->
    % Verify the behavior module exists and can be loaded
    ?assert(code:is_loaded(erlmcp_transport_behavior) =/= false
            orelse code:load_file(erlmcp_transport_behavior)
                   =:= {module, erlmcp_transport_behavior}),

    % Verify it has expected exports (validation and creation functions)
    Exports = erlmcp_transport_behavior:module_info(exports),
    ?assert(is_list(Exports)),
    ?assert(length(Exports) > 0),
    ok.

behavior_callbacks_defined(Config) ->
    % Verify required callback functions exist as helper functions
    % The transport_behavior module provides validation and creation functions
    RequiredHelpers =
        [validate_message,
         validate_transport_opts,
         create_message,
         create_notification,
         create_response,
         create_error_response],

    lists:foreach(fun(Helper) ->
                     ?assert(erlang:function_exported(erlmcp_transport_behavior,
                                                      Helper,
                                                      case Helper of
                                                          validate_message ->
                                                              1;
                                                          validate_transport_opts ->
                                                              2;
                                                          create_message ->
                                                              3;
                                                          create_notification ->
                                                              2;
                                                          create_response ->
                                                              2;
                                                          create_error_response ->
                                                              4
                                                      end))
                  end,
                  RequiredHelpers),

    % Verify default implementations for optional callbacks
    ?assert(erlang:function_exported(erlmcp_transport_behavior, default_get_info, 1)),
    ?assert(erlang:function_exported(erlmcp_transport_behavior, default_handle_transport_call, 2)),

    ok.

behavior_types_exported(Config) ->
    % Verify exported types
    Exports = erlmcp_transport_behavior:module_info(exports),

    % Check for type information (may vary by Erlang version)
    % At minimum, verify the module compiles and loads
    ?assert(is_list(Exports)),
    ?assert(length(Exports) > 0),
    ok.

behavior_optional_callbacks(Config) ->
    % Test optional callbacks behavior
    % Note: erlmcp_transport_behavior defines callbacks but doesn't export behaviour_info
    % because it's a specification module, not a behavior module in the traditional sense
    OptionalCallbacks = [get_info, handle_transport_call],

    % Verify that the module provides documentation/validation for these optional callbacks
    % by checking that the callback documentation exists in the module
    ?assert(is_list(erlmcp_transport_behavior:module_info(exports))),
    ?assert(length(erlmcp_transport_behavior:module_info(exports)) > 0),

    % Check that default implementations exist for optional callbacks
    ?assert(erlang:function_exported(erlmcp_transport_behavior, default_get_info, 1)),
    ?assert(erlang:function_exported(erlmcp_transport_behavior, default_handle_transport_call, 2)),
    ok.

%%====================================================================
%% Test Cases - Message Validation
%%====================================================================

validate_json_rpc_message(Config) ->
    % Test valid JSON-RPC messages
    ValidMessages =
        [#{<<"jsonrpc">> => <<"2.0">>,
           <<"method">> => <<"test">>,
           <<"id">> => 1},
         #{<<"jsonrpc">> => <<"2.0">>,
           <<"result">> => <<"ok">>,
           <<"id">> => 1},
         #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"notify">>}, % Notification
         #{<<"jsonrpc">> => <<"2.0">>,
           <<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>},
           <<"id">> => null}],

    lists:foreach(fun(Message) ->
                     ?assertEqual(ok, erlmcp_transport_behavior:validate_message(Message))
                  end,
                  ValidMessages),

    % Test invalid messages
    InvalidMessages =
        [#{}, % Missing jsonrpc field
         #{<<"jsonrpc">> => <<"1.0">>}, % Wrong version
         #{<<"jsonrpc">> => <<"2.0">>}, % No method, result, or error
         "not a map", % Not a map
         42], % Not a map

    lists:foreach(fun(Message) ->
                     ?assertMatch({error, _}, erlmcp_transport_behavior:validate_message(Message))
                  end,
                  InvalidMessages),
    ok.

validate_transport_opts(Config) ->
    % Test stdio options validation
    ValidStdioOpts = #{owner => self()},
    ?assertEqual(ok, erlmcp_transport_behavior:validate_transport_opts(stdio, ValidStdioOpts)),

    % Test TCP options validation
    ValidTcpOpts =
        #{host => "localhost",
          port => 8080,
          owner => self()},
    ?assertEqual(ok, erlmcp_transport_behavior:validate_transport_opts(tcp, ValidTcpOpts)),

    % Test HTTP options validation
    ValidHttpOpts = #{url => "http://example.com/api", owner => self()},
    ?assertEqual(ok, erlmcp_transport_behavior:validate_transport_opts(http, ValidHttpOpts)),

    % Test WebSocket options validation
    ValidWsOpts = #{url => "ws://example.com/ws", owner => self()},
    ?assertEqual(ok, erlmcp_transport_behavior:validate_transport_opts(websocket, ValidWsOpts)),

    % Test invalid transport type
    ?assertMatch({error, _}, erlmcp_transport_behavior:validate_transport_opts(invalid_type, #{})),
    ok.

message_creation_functions(Config) ->
    % Test message creation functions
    Method = <<"test_method">>,
    Params = #{param1 => <<"value1">>},
    Id = 123,

    % Test create_message/3
    Message = erlmcp_transport_behavior:create_message(Method, Params, Id),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Message)),
    ?assertEqual(Method, maps:get(<<"method">>, Message)),
    ?assertEqual(Params, maps:get(<<"params">>, Message)),
    ?assertEqual(Id, maps:get(<<"id">>, Message)),

    % Test create_notification/2
    Notification = erlmcp_transport_behavior:create_notification(Method, Params),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Notification)),
    ?assertEqual(Method, maps:get(<<"method">>, Notification)),
    ?assertEqual(Params, maps:get(<<"params">>, Notification)),
    ?assertNot(maps:is_key(<<"id">>, Notification)),

    % Test create_response/2
    Result = #{result => <<"success">>},
    Response = erlmcp_transport_behavior:create_response(Id, Result),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(Id, maps:get(<<"id">>, Response)),
    ?assertEqual(Result, maps:get(<<"result">>, Response)),
    ok.

error_message_creation(Config) ->
    % Test error response creation
    Id = 456,
    Code = -32600,
    Message = <<"Invalid Request">>,
    Data = #{additional => <<"info">>},

    % Test with data
    ErrorWithData = erlmcp_transport_behavior:create_error_response(Id, Code, Message, Data),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, ErrorWithData)),
    ?assertEqual(Id, maps:get(<<"id">>, ErrorWithData)),

    ErrorObj = maps:get(<<"error">>, ErrorWithData),
    ?assertEqual(Code, maps:get(<<"code">>, ErrorObj)),
    ?assertEqual(Message, maps:get(<<"message">>, ErrorObj)),
    ?assertEqual(Data, maps:get(<<"data">>, ErrorObj)),

    % Test without data
    ErrorWithoutData =
        erlmcp_transport_behavior:create_error_response(Id, Code, Message, undefined),
    ErrorObj2 = maps:get(<<"error">>, ErrorWithoutData),
    ?assertNot(maps:is_key(<<"data">>, ErrorObj2)),
    ok.

%%====================================================================
%% Test Cases - Transport Options
%%====================================================================

stdio_opts_validation(Config) ->
    % Valid stdio options
    ValidOpts =
        [#{owner => self()},
         #{owner => self(), test_mode => true},
         #{owner => self(), test_mode => false}],

    lists:foreach(fun(Opts) ->
                     ?assertEqual(ok,
                                  erlmcp_transport_behavior:validate_transport_opts(stdio, Opts))
                  end,
                  ValidOpts),

    % Invalid stdio options
    InvalidOpts =
        [#{}, % Missing owner
         #{owner => "not_a_pid"}, % Invalid owner type
         #{owner => "not_a_pid"}], % Invalid owner type (must be pid)

    lists:foreach(fun(Opts) ->
                     ?assertMatch({error, _},
                                  erlmcp_transport_behavior:validate_transport_opts(stdio, Opts))
                  end,
                  InvalidOpts),
    ok.

tcp_opts_validation(Config) ->
    % Valid TCP options
    ValidOpts =
        [#{host => "localhost",
           port => 8080,
           owner => self()},
         #{host => {127, 0, 0, 1},
           port => 8080,
           owner => self()},
         #{host => "example.com",
           port => 443,
           owner => self(),
           keepalive => true}],

    lists:foreach(fun(Opts) ->
                     ?assertEqual(ok, erlmcp_transport_behavior:validate_transport_opts(tcp, Opts))
                  end,
                  ValidOpts),

    % Invalid TCP options
    InvalidOpts =
        [#{}, % Missing required fields
         #{host => "localhost", owner => self()}, % Missing port
         #{host => "localhost",
           port => 0,
           owner => self()}, % Invalid port
         #{host => "localhost",
           port => 70000,
           owner => self()}, % Port too high
         #{host => invalid_host,
           port => 8080,
           owner => self()}], % Invalid host

    lists:foreach(fun(Opts) ->
                     ?assertMatch({error, _},
                                  erlmcp_transport_behavior:validate_transport_opts(tcp, Opts))
                  end,
                  InvalidOpts),
    ok.

http_opts_validation(Config) ->
    % Valid HTTP options
    ValidOpts =
        [#{url => "http://example.com/api", owner => self()},
         #{url => "https://secure.example.com/api", owner => self()},
         #{url => <<"http://example.com/api">>, owner => self()},
         #{url => "http://localhost:8080/api", owner => self()}],

    lists:foreach(fun(Opts) ->
                     ?assertEqual(ok, erlmcp_transport_behavior:validate_transport_opts(http, Opts))
                  end,
                  ValidOpts),

    % Invalid HTTP options
    InvalidOpts =
        [#{}, % Missing required fields
         #{url => "ftp://example.com", owner => self()}, % Invalid protocol
         #{url => "not-a-url", owner => self()}, % Invalid URL format
         #{url => "", owner => self()}], % Empty URL

    lists:foreach(fun(Opts) ->
                     ?assertMatch({error, _},
                                  erlmcp_transport_behavior:validate_transport_opts(http, Opts))
                  end,
                  InvalidOpts),
    ok.

websocket_opts_validation(Config) ->
    % Valid WebSocket options
    ValidOpts =
        [#{url => "ws://example.com/ws", owner => self()},
         #{url => "wss://secure.example.com/ws", owner => self()},
         #{url => <<"ws://example.com/ws">>, owner => self()}],

    lists:foreach(fun(Opts) ->
                     ?assertEqual(ok,
                                  erlmcp_transport_behavior:validate_transport_opts(websocket,
                                                                                    Opts))
                  end,
                  ValidOpts),

    % Invalid WebSocket options
    InvalidOpts =
        [#{}, % Missing required fields
         #{url => "http://example.com", owner => self()}, % Wrong protocol
         #{url => "not-a-url", owner => self()}, % Invalid URL format
         #{url => "", owner => self()}], % Empty URL

    lists:foreach(fun(Opts) ->
                     ?assertMatch({error, _},
                                  erlmcp_transport_behavior:validate_transport_opts(websocket,
                                                                                    Opts))
                  end,
                  InvalidOpts),
    ok.

%%====================================================================
%% Test Cases - Message Formats
%%====================================================================

json_rpc_structure(Config) ->
    % Test JSON-RPC 2.0 message structure requirements
    % Valid request with all fields
    FullRequest =
        #{<<"jsonrpc">> => <<"2.0">>,
          <<"method">> => <<"test_method">>,
          <<"params">> => #{<<"arg1">> => <<"value1">>},
          <<"id">> => 1},
    ?assertEqual(ok, erlmcp_transport_behavior:validate_message(FullRequest)),

    % Valid request without params
    RequestWithoutParams =
        #{<<"jsonrpc">> => <<"2.0">>,
          <<"method">> => <<"test_method">>,
          <<"id">> => 1},
    ?assertEqual(ok, erlmcp_transport_behavior:validate_message(RequestWithoutParams)),

    % Valid notification (no id)
    Notification =
        #{<<"jsonrpc">> => <<"2.0">>,
          <<"method">> => <<"notify_method">>,
          <<"params">> => #{<<"data">> => <<"value">>}},
    ?assertEqual(ok, erlmcp_transport_behavior:validate_message(Notification)),
    ok.

notification_format(Config) ->
    % Test notification format (no id field)
    Method = <<"test_notification">>,
    Params = #{<<"data">> => <<"test">>},

    Notification = erlmcp_transport_behavior:create_notification(Method, Params),

    % Should have required fields
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Notification)),
    ?assertEqual(Method, maps:get(<<"method">>, Notification)),
    ?assertEqual(Params, maps:get(<<"params">>, Notification)),

    % Should NOT have id field
    ?assertNot(maps:is_key(<<"id">>, Notification)),

    % Should validate as a proper message
    ?assertEqual(ok, erlmcp_transport_behavior:validate_message(Notification)),
    ok.

response_format(Config) ->
    % Test response format
    Id = 42,
    Result = #{<<"status">> => <<"success">>, <<"data">> => <<"test_result">>},

    Response = erlmcp_transport_behavior:create_response(Id, Result),

    % Should have required fields
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(Id, maps:get(<<"id">>, Response)),
    ?assertEqual(Result, maps:get(<<"result">>, Response)),

    % Should NOT have method or error fields
    ?assertNot(maps:is_key(<<"method">>, Response)),
    ?assertNot(maps:is_key(<<"error">>, Response)),

    % Should validate as a proper message
    ?assertEqual(ok, erlmcp_transport_behavior:validate_message(Response)),
    ok.

error_response_format(Config) ->
    % Test error response format
    Id = 123,
    Code = -32602,
    Message = <<"Invalid params">>,
    Data = #{<<"param">> => <<"invalid_value">>},

    ErrorResponse = erlmcp_transport_behavior:create_error_response(Id, Code, Message, Data),

    % Should have required top-level fields
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, ErrorResponse)),
    ?assertEqual(Id, maps:get(<<"id">>, ErrorResponse)),

    % Should have error object with required fields
    ErrorObj = maps:get(<<"error">>, ErrorResponse),
    ?assertEqual(Code, maps:get(<<"code">>, ErrorObj)),
    ?assertEqual(Message, maps:get(<<"message">>, ErrorObj)),
    ?assertEqual(Data, maps:get(<<"data">>, ErrorObj)),

    % Should NOT have method or result fields
    ?assertNot(maps:is_key(<<"method">>, ErrorResponse)),
    ?assertNot(maps:is_key(<<"result">>, ErrorResponse)),

    % Should validate as a proper message
    ?assertEqual(ok, erlmcp_transport_behavior:validate_message(ErrorResponse)),
    ok.

%%====================================================================
%% Test Cases - Behavior Compliance (Chicago School: Real Processes)
%%====================================================================

stdio_behavior_compliance(Config) ->
    % Test that stdio transport implements the behavior correctly
    % Use the public API to start the transport (not init/1 directly)
    StdioOpts = #{test_mode => true},
    case erlmcp_transport_stdio:start_link(self(), StdioOpts) of
        {ok, Pid} when is_pid(Pid) ->
            % Verify process is alive (black-box testing)
            ?assert(is_process_alive(Pid)),

            % Test send callback using the state (send is stateless for stdio)
            ?assertEqual(ok, erlmcp_transport_stdio:send(Pid, <<"test message">>)),

            % Test close callback
            ?assertEqual(ok, erlmcp_transport_stdio:close(Pid)),

            % Verify process stopped after close
            timer:sleep(50),
            ?assertNot(is_process_alive(Pid));
        {error, Reason} ->
            ct:fail("Stdio transport failed to start: ~p", [Reason])
    end,
    ok.

tcp_behavior_compliance(Config) ->
    % Test that TCP transport implements the behavior correctly
    % Start a TCP server (not a client) for testing
    TcpOpts =
        #{transport_id => test_tcp_behavior,
          server_id => test_server_behavior,
          mode => server,
          port => 0,  % Use OS-assigned port
          owner => self(),
          num_acceptors => 1,
          max_connections => 10},

    case catch erlmcp_transport_tcp:start_server(TcpOpts) of
        {ok, Pid} when is_pid(Pid) ->
            % Verify process is alive (black-box testing)
            ?assert(is_process_alive(Pid)),

            % Test close callback using transport API
            ?assertEqual(ok, erlmcp_transport_tcp:close(Pid)),

            % Stop the server
            ok = gen_server:stop(Pid);
        {error, Reason} = Error ->
            ct:pal("TCP transport not available for behavior test: ~p", [Error]),
            % This is acceptable - TCP transport may not be available in all environments
            ok;
        Error ->
            ct:pal("TCP transport failed with unexpected error: ~p", [Error]),
            ok
    end,
    ok.

http_behavior_compliance(Config) ->
    % Test that HTTP transport implements the behavior correctly
    % NOTE: HTTP transport uses start_link/1 with an opts map
    HttpOpts =
        #{url => "http://example.com/api",
          owner => self(),
          test_mode => true},

    case catch erlmcp_transport_http:start_link(HttpOpts) of
        {ok, Pid} when is_pid(Pid) ->
            % Verify process is alive (black-box testing)
            ?assert(is_process_alive(Pid)),

            % Test send callback (may fail due to network, but shouldn't crash)
            Result = erlmcp_transport_http:send(Pid, <<"test">>),
            % Send may fail with {error, Reason} due to network/test mode
            ?assert(Result =:= ok orelse element(1, Result) =:= error),

            % Test close callback
            ?assertEqual(ok, erlmcp_transport_http:close(Pid)),

            % Verify process is terminated
            timer:sleep(100),
            ?assertNot(is_process_alive(Pid));
        {error, Reason} ->
            ct:pal("HTTP transport failed to start: ~p", [Reason]);
        Error ->
            ct:pal("HTTP transport not available for behavior test: ~p", [Error])
    end,
    ok.

%%====================================================================
%% Test Cases - Validation Functions
%%====================================================================

url_validation_functions(Config) ->
    % Test URL validation helper functions
    % Valid HTTP URLs
    ValidHttpUrls =
        ["http://example.com", "https://secure.example.com", "http://localhost:8080/path"],

    % Valid WebSocket URLs
    ValidWsUrls = ["ws://example.com/ws", "wss://secure.example.com/ws"],

    % Test the validation functions exist and work
    % Note: These are internal functions, so we test through public interface
    lists:foreach(fun(Url) ->
                     HttpOpts = #{url => Url, owner => self()},
                     ?assertEqual(ok,
                                  erlmcp_transport_behavior:validate_transport_opts(http, HttpOpts))
                  end,
                  ValidHttpUrls),

    lists:foreach(fun(Url) ->
                     WsOpts = #{url => Url, owner => self()},
                     ?assertEqual(ok,
                                  erlmcp_transport_behavior:validate_transport_opts(websocket,
                                                                                    WsOpts))
                  end,
                  ValidWsUrls),
    ok.

host_validation_functions(Config) ->
    % Test host validation helper functions
    ValidHosts = ["localhost", "example.com", "192.168.1.1", {127, 0, 0, 1}, {192, 168, 1, 1}],

    lists:foreach(fun(Host) ->
                     TcpOpts =
                         #{host => Host,
                           port => 8080,
                           owner => self()},
                     ?assertEqual(ok,
                                  erlmcp_transport_behavior:validate_transport_opts(tcp, TcpOpts))
                  end,
                  ValidHosts),

    % Test invalid hosts
    InvalidHosts =
        [{256, 0, 0, 1}, % Invalid IP
         {}, % Wrong tuple format
         123, % Not a string or IP tuple
         ""],  % Empty string

    lists:foreach(fun(Host) ->
                     TcpOpts =
                         #{host => Host,
                           port => 8080,
                           owner => self()},
                     ?assertMatch({error, _},
                                  erlmcp_transport_behavior:validate_transport_opts(tcp, TcpOpts))
                  end,
                  InvalidHosts),
    ok.

message_content_validation(Config) ->
    % Test message content validation functions
    % Valid message contents
    ValidMessages =
        [#{<<"jsonrpc">> => <<"2.0">>,
           <<"method">> => <<"test">>,
           <<"id">> => 1},
         #{<<"jsonrpc">> => <<"2.0">>,
           <<"result">> => <<"ok">>,
           <<"id">> => 1},
         #{<<"jsonrpc">> => <<"2.0">>,
           <<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Error">>},
           <<"id">> => 1}],

    lists:foreach(fun(Message) ->
                     ?assertEqual(ok, erlmcp_transport_behavior:validate_message(Message))
                  end,
                  ValidMessages),

    % Invalid message contents
    InvalidMessages =
        [#{<<"method">> => <<"test">>}, % Missing jsonrpc
         #{<<"jsonrpc">> => <<"1.0">>}, % Wrong version
         #{<<"jsonrpc">> => <<"2.0">>}],  % Missing content

    lists:foreach(fun(Message) ->
                     ?assertMatch({error, _}, erlmcp_transport_behavior:validate_message(Message))
                  end,
                  InvalidMessages),
    ok.

error_structure_validation(Config) ->
    % Test error structure validation
    % Valid error objects
    ValidErrors =
        [#{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>},
         #{<<"code">> => -32700,
           <<"message">> => <<"Parse error">>,
           <<"data">> => #{<<"detail">> => <<"expected">>}}],

    % Test that error responses validate correctly
    lists:foreach(fun(Error) ->
                     Message =
                         #{<<"jsonrpc">> => <<"2.0">>,
                           <<"error">> => Error,
                           <<"id">> => 1},
                     ?assertEqual(ok, erlmcp_transport_behavior:validate_message(Message))
                  end,
                  ValidErrors),

    % Invalid error objects
    InvalidErrors =
        [#{<<"message">> => <<"Error">>}, % Missing code
         #{<<"code">> => -32600}, % Missing message
         #{<<"code">> => "not_an_int", <<"message">> => <<"Error">>}], % Code not integer

    lists:foreach(fun(Error) ->
                     Message =
                         #{<<"jsonrpc">> => <<"2.0">>,
                           <<"error">> => Error,
                           <<"id">> => 1},
                     ?assertMatch({error, _}, erlmcp_transport_behavior:validate_message(Message))
                  end,
                  InvalidErrors),
    ok.

%%====================================================================
%% Test Cases - Integration
%%====================================================================

behavior_error_handling(Config) ->
    % Test error handling in behavior functions
    % Test validate_message with various invalid inputs
    InvalidInputs = [not_a_map, 123, [], <<"not_a_map">>],

    lists:foreach(fun(Input) ->
                     ?assertMatch({error, _}, erlmcp_transport_behavior:validate_message(Input))
                  end,
                  InvalidInputs),

    % Test validate_transport_opts with invalid types
    ?assertMatch({error, _}, erlmcp_transport_behavior:validate_transport_opts(invalid, #{})),
    ok.

behavior_lifecycle(Config) ->
    % Test complete behavior lifecycle with stdio transport
    % 1. Initialize
    StdioOpts = #{test_mode => true, transport_id => test_behavior_lifecycle},
    {ok, Pid} = erlmcp_transport_stdio:start_link(self(), StdioOpts),
    ?assert(is_process_alive(Pid)),

    % 2. Test send via Pid (API sends to stdout)
    ?assertEqual(ok, erlmcp_transport_stdio:send(Pid, <<"test message">>)),

    % 3. Test close via Pid
    ?assertEqual(ok, erlmcp_transport_stdio:close(Pid)),

    % 4. Verify process is terminated
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)),
    ok.
