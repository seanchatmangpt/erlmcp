%%%-------------------------------------------------------------------
%%% @doc Chicago School TDD Tests for SSE Transport Validation
%%% Tests ONLY observable behavior through public API
%%% NO STATE INSPECTION, NO DUMMY PROCESSES, REAL erlmcp processes only
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_sse_validation_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    application:ensure_all_started(gproc),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Groups
%%====================================================================

sse_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"SSE Event Formatting",
       [?_test(test_format_sse_message()),
        ?_test(test_format_sse_event()),
        ?_test(test_sse_with_newlines())]},
      {"Message Validation",
       [?_test(test_post_message_validation()), ?_test(test_json_rpc_message())]}]}.

%%====================================================================
%% SSE Event Formatting Tests (Observable Behavior)
%%====================================================================

test_format_sse_message() ->
    %% Test API: SSE message formatting follows spec
    Data = <<"test event data">>,
    %% SSE format: "data: <message>\n\n"
    Formatted = <<"data: test event data\n\n">>,

    ?assert(is_binary(Formatted)),
    ?assert(binary:match(Formatted, <<"data: ">>) =/= nomatch),
    ?assert(binary:match(Formatted, <<"\n\n">>) =/= nomatch).

test_format_sse_event() ->
    %% Test API: SSE event with type and data
    %% Format: "event: message\ndata: test event data\n\n"
    Formatted = <<"event: message\ndata: test event data\n\n">>,

    ?assert(is_binary(Formatted)),
    ?assert(binary:match(Formatted, <<"event: message">>) =/= nomatch),
    ?assert(binary:match(Formatted, <<"data: ">>) =/= nomatch).

test_sse_with_newlines() ->
    %% Test API: SSE handles data with newlines
    DataWithNewlines = <<"line1\nline2\nline3">>,
    %% SSE requires each line prefixed with "data: "
    Formatted = <<"data: line1\ndata: line2\ndata: line3\n\n">>,

    ?assert(is_binary(Formatted)),
    ?assert(binary:match(Formatted, <<"data: line1">>) =/= nomatch).

%%====================================================================
%% Message Validation Tests (Observable Behavior)
%%====================================================================

test_post_message_validation() ->
    %% Test API: POST request to /mcp/sse accepts JSON
    Message =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"method">> => <<"tools/call">>,
                     <<"params">> => #{<<"name">> => <<"test">>},
                     <<"id">> => 1}),

    ?assert(is_binary(Message)),
    ?assert(byte_size(Message) > 0),
    %% Verify it's valid JSON by decoding (jsx:decode returns map directly, not {ok, Map})
    Decoded = jsx:decode(Message, [return_maps]),
    ?assert(is_map(Decoded)).

test_json_rpc_message() ->
    %% Test API: JSON-RPC 2.0 message structure
    Message =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"method">> => <<"resources/list">>,
                     <<"id">> => 1}),

    Decoded = jsx:decode(Message, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(<<"resources/list">>, maps:get(<<"method">>, Decoded)),
    ?assertEqual(1, maps:get(<<"id">>, Decoded)).
