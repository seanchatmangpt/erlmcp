%%%-------------------------------------------------------------------
%%% @doc Refusal Code API Tests
%%%
%%% Chicago School TDD tests for erlmcp_refusal API functions.
%%% Tests observable behavior through public API only.
%%% NO internal state inspection or record duplication.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_refusal_api_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp_refusal.hrl").

%%%====================================================================
%%% API Function Tests
%%%====================================================================

get_message_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [?_test(test_get_message_success()), ?_test(test_get_message_unknown_code())] end}.

test_get_message_success() ->
    {ok, Message} = erlmcp_refusal:get_message(1001),
    ?assertEqual(<<"Queue capacity exceeded">>, Message).

test_get_message_unknown_code() ->
    Result = erlmcp_refusal:get_message(9999),
    ?assertEqual({error, unknown_code}, Result).

get_metadata_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [?_test(test_get_metadata_success()), ?_test(test_get_metadata_unknown_code())] end}.

test_get_metadata_success() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(1001),
    ?assertEqual(1001, Code),
    ?assertEqual(429, Status),
    ?assertEqual(<<"Queue capacity exceeded">>, Message),
    ?assert(is_binary(Hint)),
    ?assert(byte_size(Hint) > 0),
    ?assertEqual(error, Severity).

test_get_metadata_unknown_code() ->
    Result = erlmcp_refusal:get_metadata(9999),
    ?assertEqual({error, unknown_code}, Result).

format_refusal_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [?_test(test_format_refusal_basic()), ?_test(test_format_refusal_unknown_code())]
     end}.

test_format_refusal_basic() ->
    Formatted = erlmcp_refusal:format_refusal(1001),
    ?assert(is_binary(Formatted)),
    ?assertNotEqual(0, byte_size(Formatted)),
    ?assert(<<"Queue capacity exceeded">> =< Formatted).

test_format_refusal_unknown_code() ->
    Formatted = erlmcp_refusal:format_refusal(9999),
    ?assert(is_binary(Formatted)),
    ?assertNotEqual(0, byte_size(Formatted)),
    ?assert(<<"Unknown error code">> =< Formatted).

format_refusal_with_details_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(test_format_refusal_with_details_empty()),
         ?_test(test_format_refusal_with_details_populated())]
     end}.

test_format_refusal_with_details_empty() ->
    Formatted = erlmcp_refusal:format_refusal(1001, #{}),
    ?assert(is_binary(Formatted)),
    ?assertNotEqual(0, byte_size(Formatted)),
    %% Empty details should behave same as no details
    FormattedNoDetails = erlmcp_refusal:format_refusal(1001),
    ?assertEqual(FormattedNoDetails, Formatted).

test_format_refusal_with_details_populated() ->
    Details = #{<<"connection_id">> => <<"conn_123">>, <<"count">> => 1050},
    Formatted = erlmcp_refusal:format_refusal(1001, Details),
    ?assert(is_binary(Formatted)),
    ?assertNotEqual(0, byte_size(Formatted)),
    ?assert(<<"Queue capacity exceeded">> =< Formatted),
    ?assert(<<"Details">> =< Formatted).

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.
