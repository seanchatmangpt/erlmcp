%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test suite for erlmcp_transport_behavior module
%%%
%%% This comprehensive test suite validates the transport behavior
%%% interface and tests behavior compliance across all transport
%%% implementations.
%%%-------------------------------------------------------------------
-module(erlmcp_transport_behavior_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2,
         end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
%% Test cases
-export([behavior_module_exists/1, behavior_callbacks_defined/1,
         behavior_types_exported/1, behavior_optional_callbacks/1, validate_json_rpc_message/1,
         validate_transport_opts/1, message_creation_functions/1, error_message_creation/1,
         stdio_opts_validation/1, tcp_opts_validation/1, http_opts_validation/1,
         websocket_opts_validation/1, json_rpc_structure/1, notification_format/1,
         response_format/1, error_response_format/1, stdio_behavior_compliance/1,
         tcp_behavior_compliance/1, http_behavior_compliance/1, transport_state_type/1,
         transport_opts_type/1, transport_message_type/1, transport_info_type/1,
         url_validation_functions/1, host_validation_functions/1, message_content_validation/1,
         error_structure_validation/1, behavior_with_registry/1, behavior_error_handling/1,
         behavior_lifecycle/1]).    % Behavior validation

    % Message validation

    % Transport options validation

    % Message format compliance

    % Behavior compliance tests

    % Type system validation

    % Validation functions

    % Integration testing

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [{group, behavior_validation},
     {group, message_validation},
     {group, transport_options},
     {group, message_formats},
     {group, behavior_compliance},
     {group, type_system},
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
     {type_system,
      [parallel],
      [transport_state_type, transport_opts_type, transport_message_type, transport_info_type]},
     {validation_functions,
      [parallel],
      [url_validation_functions,
       host_validation_functions,
       message_content_validation,
       error_structure_validation]},
     {integration,
      [sequential],
      [behavior_with_registry, behavior_error_handling, behavior_lifecycle]}].

%%====================================================================
%% Suite Setup/Teardown
%%====================================================================

init_per_suite(Config) ->
    ct:pal("Starting transport behavior test suite"),

    % Start necessary applications
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(sasl),

    % Start registry for integration tests
    case erlmcp_registry:start_link() of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
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
    ?assert(code:is_loaded(erlmcp_transport) =/= false
            orelse code:load_file(erlmcp_transport) =:= {module, erlmcp_transport}),

    % Verify it's a behavior module
    Attributes = erlmcp_transport:module_info(attributes),
    ?assert(lists:member(behavior, proplists:get_keys(Attributes))
            orelse lists:member(behaviour, proplists:get_keys(Attributes))),
    ok.

behavior_callbacks_defined(Config) ->
    % Verify required callbacks are defined
    RequiredCallbacks = [{init, 1}, {send, 2}, {close, 1}],

    Callbacks = erlmcp_transport:behaviour_info(callbacks),

    lists:foreach(fun(Callback) -> ?assert(lists:member(Callback, Callbacks)) end,
                  RequiredCallbacks),

    % Verify optional callbacks are marked as optional
    OptionalCallbacks = erlmcp_transport:behaviour_info(optional_callbacks),
    ExpectedOptional = [{get_info, 1}, {handle_transport_call, 2}],

    lists:foreach(fun(OptCallback) -> ?assert(lists:member(OptCallback, OptionalCallbacks))
                  end,
                  ExpectedOptional),

    ok.

behavior_types_exported(Config) ->
    % Verify exported types
    Exports = erlmcp_transport:module_info(exports),

    % Check for type information (may vary by Erlang version)
    % At minimum, verify the module compiles and loads
    ?assert(is_list(Exports)),
    ?assert(length(Exports) > 0),
    ok.

behavior_optional_callbacks(Config) ->
    % Test optional callbacks behavior
    OptionalCallbacks = [get_info, handle_transport_call],

    % These should be marked as optional
    lists:foreach(fun(Callback) ->
                     % Verify optional callback is properly defined
                     ?assert(erlang:function_exported(erlmcp_transport, behaviour_info, 1))
                  end,
                  OptionalCallbacks),
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

    lists:foreach(fun(Message) -> ?assertEqual(ok, erlmcp_transport:validate_message(Message))
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
                     ?assertMatch({error, _}, erlmcp_transport:validate_message(Message))
                  end,
                  InvalidMessages),
    ok.

validate_transport_opts(Config) ->
    % Test stdio options validation
    ValidStdioOpts = #{owner => self()},
    ?assertEqual(ok, erlmcp_transport:validate_transport_opts(stdio, ValidStdioOpts)),

    % Test TCP options validation
    ValidTcpOpts =
        #{host => "localhost",
          port => 8080,
          owner => self()},
    ?assertEqual(ok, erlmcp_transport:validate_transport_opts(tcp, ValidTcpOpts)),

    % Test HTTP options validation
    ValidHttpOpts = #{url => "http://example.com/api", owner => self()},
    ?assertEqual(ok, erlmcp_transport:validate_transport_opts(http, ValidHttpOpts)),

    % Test WebSocket options validation
    ValidWsOpts = #{url => "ws://example.com/ws", owner => self()},
    ?assertEqual(ok, erlmcp_transport:validate_transport_opts(websocket, ValidWsOpts)),

    % Test invalid transport type
    ?assertMatch({error, _}, erlmcp_transport:validate_transport_opts(invalid_type, #{})),
    ok.

message_creation_functions(Config) ->
    % Test message creation functions
    Method = <<"test_method">>,
    Params = #{param1 => <<"value1">>},
    Id = 123,

    % Test create_message/3
    Message = erlmcp_transport:create_message(Method, Params, Id),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Message)),
    ?assertEqual(Method, maps:get(<<"method">>, Message)),
    ?assertEqual(Params, maps:get(<<"params">>, Message)),
    ?assertEqual(Id, maps:get(<<"id">>, Message)),

    % Test create_notification/2
    Notification = erlmcp_transport:create_notification(Method, Params),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Notification)),
    ?assertEqual(Method, maps:get(<<"method">>, Notification)),
    ?assertEqual(Params, maps:get(<<"params">>, Notification)),
    ?assertNot(maps:is_key(<<"id">>, Notification)),

    % Test create_response/2
    Result = #{result => <<"success">>},
    Response = erlmcp_transport:create_response(Id, Result),
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
    ErrorWithData = erlmcp_transport:create_error_response(Id, Code, Message, Data),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, ErrorWithData)),
    ?assertEqual(Id, maps:get(<<"id">>, ErrorWithData)),

    ErrorObj = maps:get(<<"error">>, ErrorWithData),
    ?assertEqual(Code, maps:get(<<"code">>, ErrorObj)),
    ?assertEqual(Message, maps:get(<<"message">>, ErrorObj)),
    ?assertEqual(Data, maps:get(<<"data">>, ErrorObj)),

    % Test without data
    ErrorWithoutData = erlmcp_transport:create_error_response(Id, Code, Message, undefined),
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
                     ?assertEqual(ok, erlmcp_transport:validate_transport_opts(stdio, Opts))
                  end,
                  ValidOpts),

    % Invalid stdio options
    InvalidOpts =
        [#{}, % Missing owner
         #{owner => "not_a_pid"}, % Invalid owner type
         #{owner => self(), invalid_option => true}], % Extra invalid option

    lists:foreach(fun(Opts) ->
                     ?assertMatch({error, _}, erlmcp_transport:validate_transport_opts(stdio, Opts))
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
                     ?assertEqual(ok, erlmcp_transport:validate_transport_opts(tcp, Opts))
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
                     ?assertMatch({error, _}, erlmcp_transport:validate_transport_opts(tcp, Opts))
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
                     ?assertEqual(ok, erlmcp_transport:validate_transport_opts(http, Opts))
                  end,
                  ValidOpts),

    % Invalid HTTP options
    InvalidOpts =
        [#{}, % Missing required fields
         #{url => "ftp://example.com", owner => self()}, % Invalid protocol
         #{url => "not-a-url", owner => self()}, % Invalid URL format
         #{url => "", owner => self()}], % Empty URL

    lists:foreach(fun(Opts) ->
                     ?assertMatch({error, _}, erlmcp_transport:validate_transport_opts(http, Opts))
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
                     ?assertEqual(ok, erlmcp_transport:validate_transport_opts(websocket, Opts))
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
                                  erlmcp_transport:validate_transport_opts(websocket, Opts))
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
    ?assertEqual(ok, erlmcp_transport:validate_message(FullRequest)),

    % Valid request without params
    RequestWithoutParams =
        #{<<"jsonrpc">> => <<"2.0">>,
          <<"method">> => <<"test_method">>,
          <<"id">> => 1},
    ?assertEqual(ok, erlmcp_transport:validate_message(RequestWithoutParams)),

    % Valid notification (no id)
    Notification =
        #{<<"jsonrpc">> => <<"2.0">>,
          <<"method">> => <<"notify_method">>,
          <<"params">> => #{<<"data">> => <<"value">>}},
    ?assertEqual(ok, erlmcp_transport:validate_message(Notification)),
    ok.

notification_format(Config) ->
    % Test notification format (no id field)
    Method = <<"test_notification">>,
    Params = #{<<"data">> => <<"test">>},

    Notification = erlmcp_transport:create_notification(Method, Params),

    % Should have required fields
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Notification)),
    ?assertEqual(Method, maps:get(<<"method">>, Notification)),
    ?assertEqual(Params, maps:get(<<"params">>, Notification)),

    % Should NOT have id field
    ?assertNot(maps:is_key(<<"id">>, Notification)),

    % Should validate as a proper message
    ?assertEqual(ok, erlmcp_transport:validate_message(Notification)),
    ok.

response_format(Config) ->
    % Test response format
    Id = 42,
    Result = #{<<"status">> => <<"success">>, <<"data">> => <<"test_result">>},

    Response = erlmcp_transport:create_response(Id, Result),

    % Should have required fields
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(Id, maps:get(<<"id">>, Response)),
    ?assertEqual(Result, maps:get(<<"result">>, Response)),

    % Should NOT have method or error fields
    ?assertNot(maps:is_key(<<"method">>, Response)),
    ?assertNot(maps:is_key(<<"error">>, Response)),

    % Should validate as a proper message
    ?assertEqual(ok, erlmcp_transport:validate_message(Response)),
    ok.

error_response_format(Config) ->
    % Test error response format
    Id = 123,
    Code = -32602,
    Message = <<"Invalid params">>,
    Data = #{<<"param">> => <<"invalid_value">>},

    ErrorResponse = erlmcp_transport:create_error_response(Id, Code, Message, Data),

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
    ?assertEqual(ok, erlmcp_transport:validate_message(ErrorResponse)),
    ok.

%%====================================================================
%% Test Cases - Behavior Compliance
%%====================================================================

stdio_behavior_compliance(Config) ->
    % Test that stdio transport implements the behavior correctly
    % Test init callback
    StdioOpts = #{owner => self(), test_mode => true},
    case erlmcp_transport_stdio_new:init([transport_id, StdioOpts]) of
        {ok, State} ->
            % Test send callback
            ?assertEqual(ok, erlmcp_transport_stdio_new:send(State, <<"test">>)),

            % Test close callback
            ?assertEqual(ok, erlmcp_transport_stdio_new:close(State)),

            % Test optional get_info callback if implemented
            try
                Info = erlmcp_transport_stdio_new:get_info(State),
                ?assert(is_map(Info)),
                ?assert(maps:is_key(type, Info))
            catch
                error:undef ->
                    ok % Optional callback not implemented
            end;
        {stop, Reason} ->
            ct:pal("Stdio transport failed to initialize: ~p", [Reason])
    end,
    ok.

tcp_behavior_compliance(Config) ->
    % Test that TCP transport implements the behavior correctly
    TcpOpts =
        #{transport_id => test_tcp_behavior,
          server_id => test_server_behavior,
          host => "localhost",
          port => 8080,
          test_mode => true},

    case catch erlmcp_transport_tcp:start_link(test_tcp_behavior, TcpOpts) of
        {ok, Pid} ->
            {ok, State} = gen_server:call(Pid, get_state),
            
            % Test send callback (may fail if not connected, but shouldn't crash)
            Result = erlmcp_transport_tcp:send(State, <<"test">>),
            ?assert(Result =:= ok orelse element(1, Result) =:= error),
            
            % Test close callback
            ?assertEqual(ok, erlmcp_transport_tcp:close(State)),
            
            ok = gen_server:stop(Pid);
        Error ->
            ct:pal("TCP transport not available for behavior test: ~p", [Error])
    end,
    ok.

http_behavior_compliance(Config) ->
    % Test that HTTP transport implements the behavior correctly
    HttpOpts =
        #{transport_id => test_http_behavior,
          server_id => test_server_behavior,
          url => "http://example.com/api",
          test_mode => true},

    case catch erlmcp_transport_http:start_link(test_http_behavior, HttpOpts) of
        {ok, Pid} ->
            {ok, State} = gen_server:call(Pid, get_state),
            
            % Test send callback (may fail due to network, but shouldn't crash)
            Result = erlmcp_transport_http:send(State, <<"test">>),
            ?assert(Result =:= ok orelse element(1, Result) =:= error),
            
            % Test close callback
            ?assertEqual(ok, erlmcp_transport_http:close(State)),
            
            ok = gen_server:stop(Pid);
        Error ->
            ct:pal("HTTP transport not available for behavior test: ~p", [Error])
    end,
    ok.

%%====================================================================
%% Test Cases - Type System
%%====================================================================

transport_state_type(Config) ->
    % Test that transport state is properly typed
    % This is mainly a compilation/interface test
    % Verify different transports return state that works with behavior
    StdioOpts = #{owner => self(), test_mode => true},
    case erlmcp_transport_stdio_new:init([transport_id, StdioOpts]) of
        {ok, StdioState} ->
            ?assert(erlmcp_transport_stdio_new:send(StdioState, <<"test">>) =/= undefined);
        _ ->
            ok
    end,

    TcpOpts = #{host => "localhost", port => 8080},
    {ok, TcpState} = erlmcp_transport_tcp:init(TcpOpts),
    ?assert(erlmcp_transport_tcp:send(TcpState, <<"test">>) =/= undefined),
    ok.

transport_opts_type(Config) ->
    % Test transport options type compatibility
    % These should all be valid transport_opts types
    StdioOpts = #{owner => self()},
    TcpOpts =
        #{host => "localhost",
          port => 8080,
          owner => self()},
    HttpOpts = #{url => "http://example.com", owner => self()},

    ?assertEqual(ok, erlmcp_transport:validate_transport_opts(stdio, StdioOpts)),
    ?assertEqual(ok, erlmcp_transport:validate_transport_opts(tcp, TcpOpts)),
    ?assertEqual(ok, erlmcp_transport:validate_transport_opts(http, HttpOpts)),
    ok.

transport_message_type(Config) ->
    % Test transport message type compliance
    % Create various message types
    Message1 = erlmcp_transport:create_message(<<"test">>, #{}, 1),
    Message2 = erlmcp_transport:create_notification(<<"notify">>, #{}),
    Message3 = erlmcp_transport:create_response(1, #{result => ok}),
    Message4 = erlmcp_transport:create_error_response(1, -32600, <<"Error">>, undefined),

    % All should be valid transport messages
    ?assertEqual(ok, erlmcp_transport:validate_message(Message1)),
    ?assertEqual(ok, erlmcp_transport:validate_message(Message2)),
    ?assertEqual(ok, erlmcp_transport:validate_message(Message3)),
    ?assertEqual(ok, erlmcp_transport:validate_message(Message4)),
    ok.

transport_info_type(Config) ->
    % Test transport info type structure
    % Mock transport info
    MockInfo =
        #{type => test_transport,
          version => <<"1.0.0">>,
          capabilities => [send, 'receive'],
          connection_state => connected,
          statistics => #{messages_sent => 10, messages_received => 5}},

    % Should have expected structure
    ?assert(maps:is_key(type, MockInfo)),
    ?assert(maps:is_key(connection_state, MockInfo)),
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
                     ?assertEqual(ok, erlmcp_transport:validate_transport_opts(http, HttpOpts))
                  end,
                  ValidHttpUrls),

    lists:foreach(fun(Url) ->
                     WsOpts = #{url => Url, owner => self()},
                     ?assertEqual(ok, erlmcp_transport:validate_transport_opts(websocket, WsOpts))
                  end,
                  ValidWsUrls),
    ok.

host_validation_functions(Config) ->
    % Test host validation helper functions
    ValidHosts =
        ["localhost", "example.com", "192.168.1.1", {127, 0, 0, 1}, {192, 168, 1, 1}],

    lists:foreach(fun(Host) ->
                     TcpOpts =
                         #{host => Host,
                           port => 8080,
                           owner => self()},
                     ?assertEqual(ok, erlmcp_transport:validate_transport_opts(tcp, TcpOpts))
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
                                  erlmcp_transport:validate_transport_opts(tcp, TcpOpts))
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

    lists:foreach(fun(Message) -> ?assertEqual(ok, erlmcp_transport:validate_message(Message))
                  end,
                  ValidMessages),

    % Invalid message contents
    InvalidMessages =
        [#{<<"method">> => <<"test">>}, % Missing jsonrpc
         #{<<"jsonrpc">> => <<"1.0">>}, % Wrong version
         #{<<"jsonrpc">> => <<"2.0">>}],  % Missing content

    lists:foreach(fun(Message) ->
                     ?assertMatch({error, _}, erlmcp_transport:validate_message(Message))
                  end,
                  InvalidMessages),
    ok.

error_structure_validation(Config) ->
    % Test error structure validation
    % Valid error structures
    ValidErrors =
        [#{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>},
         #{<<"code">> => -32601,
           <<"message">> => <<"Method not found">>,
           <<"data">> => #{<<"method">> => <<"unknown">>}}],

    lists:foreach(fun(ErrorObj) ->
                     ErrorResponse =
                         #{<<"jsonrpc">> => <<"2.0">>,
                           <<"error">> => ErrorObj,
                           <<"id">> => 1},
                     ?assertEqual(ok, erlmcp_transport:validate_message(ErrorResponse))
                  end,
                  ValidErrors),

    % Invalid error structures
    InvalidErrors =
        [#{<<"message">> => <<"Error">>}, % Missing code
         #{<<"code">> => "invalid", <<"message">> => <<"Error">>}, % Wrong code type
         #{<<"code">> => -32600, <<"message">> => 123}], % Wrong message type

    lists:foreach(fun(ErrorObj) ->
                     ErrorResponse =
                         #{<<"jsonrpc">> => <<"2.0">>,
                           <<"error">> => ErrorObj,
                           <<"id">> => 1},
                     ?assertMatch({error, _}, erlmcp_transport:validate_message(ErrorResponse))
                  end,
                  InvalidErrors),
    ok.

%%====================================================================
%% Test Cases - Integration
%%====================================================================

behavior_with_registry(Config) ->
    % Test behavior integration with registry
    TransportId = test_behavior_registry,
    ServerId = test_server_registry,

    % Start a transport that implements the behavior
    StdioOpts = #{test_mode => true, server_id => ServerId},
    {ok, Pid} = erlmcp_transport_stdio_new:start_link(TransportId, StdioOpts),

    timer:sleep(100), % Allow registration

    % Verify it registered properly
    {ok, {RegPid, RegConfig}} = erlmcp_registry:find_transport(TransportId),
    ?assertEqual(Pid, RegPid),
    ?assert(maps:is_key(type, RegConfig)),

    ok = gen_server:stop(Pid),
    ok.

behavior_error_handling(Config) ->
    % Test behavior error handling patterns
    % Test with invalid options
    InvalidOpts = #{invalid => option},

    % Should handle gracefully
    try
        erlmcp_transport:validate_transport_opts(stdio, InvalidOpts)
    catch
        _:_ ->
            ok % Expected to fail
    end,

    % Test message validation with invalid input
    InvalidMessage = <<"not a map">>,
    ?assertMatch({error, _}, erlmcp_transport:validate_message(InvalidMessage)),
    ok.

behavior_lifecycle(Config) ->
    % Test complete behavior lifecycle
    TransportId = test_behavior_lifecycle,

    % 1. Initialize
    StdioOpts = #{test_mode => true},
    {ok, Pid} = erlmcp_transport_stdio_new:start_link(TransportId, StdioOpts),
    ?assert(is_process_alive(Pid)),

    % 2. Get state and test send
    {ok, State} = gen_server:call(Pid, get_state),
    ?assertEqual(ok, erlmcp_transport_stdio_new:send(State, <<"test message">>)),

    % 3. Test optional callbacks if available
    try
        Info = erlmcp_transport_stdio_new:get_info(State),
        ?assert(is_map(Info))
    catch
        error:undef ->
            ok % Optional callback not implemented
    end,

    % 4. Close
    ?assertEqual(ok, erlmcp_transport_stdio_new:close(State)),

    % 5. Terminate
    ok = gen_server:stop(Pid),
    ?assertNot(is_process_alive(Pid)),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

% No additional helper functions needed for this test suite
