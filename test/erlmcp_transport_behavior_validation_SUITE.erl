%%%-------------------------------------------------------------------
%%% @doc
%%% Test suite for enhanced transport behavior interface validation
%%% 
%%% This suite tests the enhanced validation functions and message 
%%% type detection capabilities of the transport behavior interface.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_behavior_validation_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/erlmcp.hrl").

%% Common Test exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).

%% Test exports
-export([
    test_validate_json_rpc_request/1,
    test_validate_json_rpc_response/1,
    test_validate_json_rpc_notification/1,
    test_extract_message_type/1,
    test_message_type_predicates/1,
    test_enhanced_message_examples/1
]).

%%====================================================================
%% Common Test Functions
%%====================================================================

all() ->
    [
        test_validate_json_rpc_request,
        test_validate_json_rpc_response,
        test_validate_json_rpc_notification,
        test_extract_message_type,
        test_message_type_predicates,
        test_enhanced_message_examples
    ].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_validate_json_rpc_request(_Config) ->
    %% Valid request
    ValidRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{}
    },
    ?assertEqual(ok, erlmcp_transport:validate_json_rpc_request(ValidRequest)),
    
    %% Request without ID (should fail)
    NoIdRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"initialize">>
    },
    ?assertMatch({error, missing_request_id}, 
                 erlmcp_transport:validate_json_rpc_request(NoIdRequest)),
    
    %% Request without method (should fail)
    NoMethodRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1
    },
    ?assertMatch({error, missing_method}, 
                 erlmcp_transport:validate_json_rpc_request(NoMethodRequest)),
    
    %% Request with invalid method type (should fail)
    InvalidMethodRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => initialize  % atom instead of binary
    },
    ?assertMatch({error, {invalid_method_type, initialize}}, 
                 erlmcp_transport:validate_json_rpc_request(InvalidMethodRequest)).

test_validate_json_rpc_response(_Config) ->
    %% Valid success response
    ValidSuccessResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => #{<<"version">> => <<"1.0">>}
    },
    ?assertEqual(ok, erlmcp_transport:validate_json_rpc_response(ValidSuccessResponse)),
    
    %% Valid error response
    ValidErrorResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"error">> => #{
            <<"code">> => -32602,
            <<"message">> => <<"Invalid params">>
        }
    },
    ?assertEqual(ok, erlmcp_transport:validate_json_rpc_response(ValidErrorResponse)),
    
    %% Response without ID (should fail)
    NoIdResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => #{<<"ok">> => true}
    },
    ?assertMatch({error, missing_response_id}, 
                 erlmcp_transport:validate_json_rpc_response(NoIdResponse)),
    
    %% Response with both result and error (should fail)
    BothResultAndError = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => #{},
        <<"error">> => #{<<"code">> => -1, <<"message">> => <<"Error">>}
    },
    ?assertMatch({error, both_result_and_error_present}, 
                 erlmcp_transport:validate_json_rpc_response(BothResultAndError)).

test_validate_json_rpc_notification(_Config) ->
    %% Valid notification
    ValidNotification = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"initialized">>,
        <<"params">> => #{}
    },
    ?assertEqual(ok, erlmcp_transport:validate_json_rpc_notification(ValidNotification)),
    
    %% Notification without method (should fail)
    NoMethodNotification = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"params">> => #{}
    },
    ?assertMatch({error, missing_method}, 
                 erlmcp_transport:validate_json_rpc_notification(NoMethodNotification)),
    
    %% Notification with ID (should fail - notifications must not have ID)
    NotificationWithId = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialized">>
    },
    ?assertMatch({error, notification_cannot_have_id}, 
                 erlmcp_transport:validate_json_rpc_notification(NotificationWithId)).

test_extract_message_type(_Config) ->
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"ping">>
    },
    ?assertEqual(request, erlmcp_transport:extract_message_type(Request)),
    
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => <<"pong">>
    },
    ?assertEqual(response, erlmcp_transport:extract_message_type(Response)),
    
    ErrorResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"error">> => #{<<"code">> => -1, <<"message">> => <<"Error">>}
    },
    ?assertEqual(error, erlmcp_transport:extract_message_type(ErrorResponse)),
    
    Notification = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"ping">>
    },
    ?assertEqual(notification, erlmcp_transport:extract_message_type(Notification)),
    
    InvalidMessage = #{<<"invalid">> => true},
    ?assertEqual(unknown, erlmcp_transport:extract_message_type(InvalidMessage)).

test_message_type_predicates(_Config) ->
    Request = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"test">>},
    Response = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => #{}},
    ErrorResponse = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"error">> => #{<<"code">> => -1, <<"message">> => <<"test">>}},
    Notification = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"test">>},
    
    %% Test is_request/1
    ?assert(erlmcp_transport:is_request(Request)),
    ?assertNot(erlmcp_transport:is_request(Response)),
    ?assertNot(erlmcp_transport:is_request(ErrorResponse)),
    ?assertNot(erlmcp_transport:is_request(Notification)),
    
    %% Test is_response/1
    ?assertNot(erlmcp_transport:is_response(Request)),
    ?assert(erlmcp_transport:is_response(Response)),
    ?assertNot(erlmcp_transport:is_response(ErrorResponse)),
    ?assertNot(erlmcp_transport:is_response(Notification)),
    
    %% Test is_error_response/1
    ?assertNot(erlmcp_transport:is_error_response(Request)),
    ?assertNot(erlmcp_transport:is_error_response(Response)),
    ?assert(erlmcp_transport:is_error_response(ErrorResponse)),
    ?assertNot(erlmcp_transport:is_error_response(Notification)),
    
    %% Test is_notification/1
    ?assertNot(erlmcp_transport:is_notification(Request)),
    ?assertNot(erlmcp_transport:is_notification(Response)),
    ?assertNot(erlmcp_transport:is_notification(ErrorResponse)),
    ?assert(erlmcp_transport:is_notification(Notification)).

test_enhanced_message_examples(_Config) ->
    %% Test the documented examples from the enhanced interface
    InitializeRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{}
    },
    ?assertEqual(ok, erlmcp_transport:validate_json_rpc_request(InitializeRequest)),
    ?assert(erlmcp_transport:is_request(InitializeRequest)),
    
    InitializeResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => #{<<"version">> => <<"1.0">>}
    },
    ?assertEqual(ok, erlmcp_transport:validate_json_rpc_response(InitializeResponse)),
    ?assert(erlmcp_transport:is_response(InitializeResponse)),
    
    ErrorResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"error">> => #{
            <<"code">> => -32602,
            <<"message">> => <<"Invalid params">>
        }
    },
    ?assertEqual(ok, erlmcp_transport:validate_json_rpc_response(ErrorResponse)),
    ?assert(erlmcp_transport:is_error_response(ErrorResponse)),
    
    InitializedNotification = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"initialized">>,
        <<"params">> => #{}
    },
    ?assertEqual(ok, erlmcp_transport:validate_json_rpc_notification(InitializedNotification)),
    ?assert(erlmcp_transport:is_notification(InitializedNotification)).