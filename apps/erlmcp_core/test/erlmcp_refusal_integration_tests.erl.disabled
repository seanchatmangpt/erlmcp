%%%-------------------------------------------------------------------
%%% @doc Refusal Code Integration Tests
%%%
%%% Chicago School TDD tests for erlmcp_refusal integration with JSON-RPC.
%%% Tests observable behavior through public API only.
%%% NO internal state inspection or record duplication.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_refusal_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp_refusal.hrl").

%%%====================================================================
%%% JSON-RPC Integration Tests
%%%====================================================================

jsonrpc_error_response_format_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_jsonrpc_error_response_structure()),
          ?_test(test_jsonrpc_error_with_refusal_code())
         ]
     end}.

test_jsonrpc_error_response_structure() ->
    %% Verify refusal codes can be used in JSON-RPC error responses
    {ok, Code, Status, Message, Hint, _Severity} = erlmcp_refusal:get_metadata(1001),
    ErrorObj = #{
        <<"code">> => Code,
        <<"message">> => Message,
        <<"data">> => #{
            <<"http_status">> => Status,
            <<"remediation_hint">> => Hint
        }
    },
    ?assertEqual(1001, maps:get(<<"code">>, ErrorObj)),
    ?assertEqual(<<"Queue capacity exceeded">>, maps:get(<<"message">>, ErrorObj)),
    ?assertEqual(429, maps:get(<<"http_status">>, maps:get(<<"data">>, ErrorObj))).

test_jsonrpc_error_with_refusal_code() ->
    %% Verify refusal codes integrate with JSON-RPC encoding
    {ok, Code, Status, Message, Hint, _Severity} = erlmcp_refusal:get_metadata(1048),
    JsonError = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message,
            <<"data">> => #{
                <<"http_status">> => Status,
                <<"remediation_hint">> => Hint,
                <<"refusal_code">> => Code
            }
        }
    },
    Encoded = jsx:encode(JsonError),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(1048, maps:get(<<"code">>, maps:get(<<"error">>, Decoded))),
    ?assertEqual(<<"Tool not found">>, maps:get(<<"message">>, maps:get(<<"error">>, Decoded))).

%%%====================================================================
%%% Category Distribution Tests
%%%====================================================================

category_distribution_test() ->
    %% Verify all codes belong to expected categories
    QueueCodes = [1001, 1002, 1003, 1004, 1005],
    AuthCodes = [1011, 1012, 1013, 1014, 1015, 1016],
    ValidationCodes = [1021, 1022, 1023, 1024, 1025, 1026, 1027, 1028, 1029],
    SecurityCodes = [1036, 1037, 1038, 1039, 1040],
    ResourceCodes = [1046, 1047, 1048, 1049, 1050, 1051, 1052],

    AllCodes = QueueCodes ++ AuthCodes ++ ValidationCodes ++ SecurityCodes ++ ResourceCodes,

    %% Verify all codes return valid metadata
    lists:foreach(fun(Code) ->
        {ok, _, _, _, _, _} = erlmcp_refusal:get_metadata(Code)
    end, AllCodes),

    ?assertEqual(32, length(AllCodes)).

severity_distribution_test() ->
    %% Verify severity levels are appropriate for categories
    CriticalCodes = [1003, 1004, 1036, 1038],
    WarnCodes = [1005, 1046, 1047, 1048, 1049, 1050, 1051, 1052],

    %% Verify critical codes
    lists:foreach(fun(Code) ->
        {ok, _, _, _, _, Severity} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(critical, Severity)
    end, CriticalCodes),

    %% Verify warn codes
    lists:foreach(fun(Code) ->
        {ok, _, _, _, _, Severity} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(warn, Severity)
    end, WarnCodes).

http_status_distribution_test() ->
    %% Verify HTTP status codes are appropriate for categories
    Status429Codes = [1001, 1002, 1003],
    Status401Codes = [1011, 1012, 1013, 1015, 1016],
    Status403Codes = [1014],
    Status404Codes = [1046, 1048, 1050],
    Status409Codes = [1047, 1049, 1051, 1052],
    Status415Codes = [1024],
    Status400Codes = [1021, 1022, 1023, 1025, 1026, 1027, 1028, 1029, 1036, 1037, 1038, 1039, 1040],

    %% Verify 429 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(429, Status)
    end, Status429Codes),

    %% Verify 401 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(401, Status)
    end, Status401Codes),

    %% Verify 403 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(403, Status)
    end, Status403Codes),

    %% Verify 404 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(404, Status)
    end, Status404Codes),

    %% Verify 409 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(409, Status)
    end, Status409Codes),

    %% Verify 415 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(415, Status)
    end, Status415Codes),

    %% Verify 400 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(400, Status)
    end, Status400Codes).

%%%====================================================================
%%% Edge Case Tests
%%%====================================================================

edge_case_boundary_codes_test() ->
    %% Test first and last codes in ranges
    {ok, _, _, _, _, _} = erlmcp_refusal:get_metadata(1001), % First
    {ok, _, _, _, _, _} = erlmcp_refusal:get_metadata(1052), % Last in resource category
    ok.

edge_case_unknown_code_test() ->
    %% Test code outside range
    ?assertEqual({error, unknown_code}, erlmcp_refusal:get_metadata(1000)),
    ?assertEqual({error, unknown_code}, erlmcp_refusal:get_metadata(1090)),
    ?assertEqual({error, unknown_code}, erlmcp_refusal:get_metadata(0)),
    ?assertEqual({error, unknown_code}, erlmcp_refusal:get_metadata(-1)),
    ok.

edge_case_message_content_test() ->
    %% Verify all defined codes have non-empty binary messages
    DefinedCodes = [1001, 1002, 1003, 1004, 1005,
                    1011, 1012, 1013, 1014, 1015, 1016,
                    1021, 1022, 1023, 1024, 1025, 1026, 1027, 1028, 1029,
                    1036, 1037, 1038, 1039, 1040,
                    1046, 1047, 1048, 1049, 1050, 1051, 1052],
    ValidCodes = [C || C <- DefinedCodes,
        case erlmcp_refusal:get_metadata(C) of
            {ok, _, _, Message, _, _} when is_binary(Message), byte_size(Message) > 0 -> true;
            _ -> false
        end],
    ?assertEqual(32, length(ValidCodes)).

edge_case_hint_content_test() ->
    %% Verify all defined codes have non-empty binary hints
    DefinedCodes = [1001, 1002, 1003, 1004, 1005,
                    1011, 1012, 1013, 1014, 1015, 1016,
                    1021, 1022, 1023, 1024, 1025, 1026, 1027, 1028, 1029,
                    1036, 1037, 1038, 1039, 1040,
                    1046, 1047, 1048, 1049, 1050, 1051, 1052],
    ValidCodes = [C || C <- DefinedCodes,
        case erlmcp_refusal:get_metadata(C) of
            {ok, _, _, _, Hint, _} when is_binary(Hint), byte_size(Hint) > 0 -> true;
            _ -> false
        end],
    ?assertEqual(32, length(ValidCodes)).

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.
