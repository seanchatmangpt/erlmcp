-module(erlmcp_ws_compression_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define state record for testing (matches erlmcp_transport_ws internal state)
-record(state,
        {transport_id :: binary(),
         registry_pid :: pid(),
         connection_info :: map(),
         session_id :: binary(),
         ping_timer :: reference() | undefined,
         fragment_buffer :: binary() | undefined,
         fragment_start_time :: integer() | undefined,
         max_message_size :: integer(),
         strict_delimiter_check :: boolean(),
         validate_utf8 :: boolean(),
         frame_buffer_size :: integer(),
         frame_buffer_used :: integer(),
         backpressure_state :: atom(),
         backpressure_timer :: reference() | undefined,
         messages_pending :: non_neg_integer(),
         bytes_buffered :: non_neg_integer(),
         compression_enabled = false :: boolean(),
         compress_zstream :: term() | undefined,
         decompress_zstream :: term() | undefined,
         server_max_window_bits = 15 :: integer(),
         client_max_window_bits = 15 :: integer(),
         server_no_context_takeover = false :: boolean(),
         client_no_context_takeover = false :: boolean(),
         messages_received = 0 :: non_neg_integer(),
         messages_sent = 0 :: non_neg_integer(),
         bytes_received = 0 :: non_neg_integer(),
         bytes_sent = 0 :: non_neg_integer(),
         ping_count = 0 :: non_neg_integer(),
         pong_count = 0 :: non_neg_integer(),
         connection_start_time :: integer()}).

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Groups
%%====================================================================

compression_negotiation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Extension Header Parsing",
       [?_test(test_parse_empty_extensions()),
        ?_test(test_parse_permessage_deflate()),
        ?_test(test_parse_permessage_deflate_with_params()),
        ?_test(test_parse_multiple_extensions())]},
      {"Parameter Negotiation",
       [?_test(test_server_max_window_bits_negotiation()),
        ?_test(test_client_max_window_bits_negotiation()),
        ?_test(test_server_no_context_takeover()),
        ?_test(test_client_no_context_takeover())]},
      {"Compression/Decompression",
       [?_test(test_compress_decompress_roundtrip()),
        ?_test(test_compress_json_data())]}]}.

%%====================================================================
%% Extension Header Parsing Tests
%%====================================================================

test_parse_empty_extensions() ->
    %% No extensions offered
    ConnectionInfo = #{headers => #{}},
    Result = erlmcp_transport_ws:negotiate_compression(ConnectionInfo),
    ?assertEqual(false, maps:get(enabled, Result, false)).

test_parse_permessage_deflate() ->
    %% Basic permessage-deflate offer
    ConnectionInfo =
        #{headers =>
              #{<<"sec-websocket-extensions">> => <<"permessage-deflate">>}},
    Result = erlmcp_transport_ws:negotiate_compression(ConnectionInfo),
    ?assertEqual(true, maps:get(enabled, Result, false)).

test_parse_permessage_deflate_with_params() ->
    %% permessage-deflate with parameters
    Extensions = <<"permessage-deflate; server_max_window_bits=15; "
                   "client_max_window_bits=15; client_no_context_takeover">>,
    ConnectionInfo =
        #{headers =>
              #{<<"sec-websocket-extensions">> => Extensions}},
    Result = erlmcp_transport_ws:negotiate_compression(ConnectionInfo),
    ?assertEqual(true, maps:get(enabled, Result, false)),
    ?assertEqual(15, maps:get(server_max_window_bits, Result, 15)),
    ?assertEqual(15, maps:get(client_max_window_bits, Result, 15)),
    ?assertEqual(true, maps:get(client_no_context_takeover, Result, false)).

test_parse_multiple_extensions() ->
    %% Multiple extensions offered
    Extensions = <<"permessage-deflate; client_max_window_bits=15, "
                   "another-extension; param=value">>,
    ConnectionInfo =
        #{headers =>
              #{<<"sec-websocket-extensions">> => Extensions}},
    Result = erlmcp_transport_ws:negotiate_compression(ConnectionInfo),
    ?assertEqual(true, maps:get(enabled, Result, false)).

%%====================================================================
%% Parameter Negotiation Tests
%%====================================================================

test_server_max_window_bits_negotiation() ->
    %% Server window bits negotiation
    Extensions = <<"permessage-deflate; server_max_window_bits=14">>,
    ConnectionInfo =
        #{headers =>
              #{<<"sec-websocket-extensions">> => Extensions}},
    Result = erlmcp_transport_ws:negotiate_compression(ConnectionInfo),
    ?assertEqual(true, maps:get(enabled, Result, false)),
    ServerWin = maps:get(server_max_window_bits, Result, 15),
    ?assert(ServerWin >= 8 andalso ServerWin =< 15).

test_client_max_window_bits_negotiation() ->
    %% Client window bits negotiation
    Extensions = <<"permessage-deflate; client_max_window_bits=14">>,
    ConnectionInfo =
        #{headers =>
              #{<<"sec-websocket-extensions">> => Extensions}},
    Result = erlmcp_transport_ws:negotiate_compression(ConnectionInfo),
    ?assertEqual(true, maps:get(enabled, Result, false)),
    ClientWin = maps:get(client_max_window_bits, Result, 15),
    ?assert(ClientWin >= 8 andalso ClientWin =< 15).

test_server_no_context_takeover() ->
    %% Server no context takeover flag
    Extensions = <<"permessage-deflate; server_no_context_takeover">>,
    ConnectionInfo =
        #{headers =>
              #{<<"sec-websocket-extensions">> => Extensions}},
    Result = erlmcp_transport_ws:negotiate_compression(ConnectionInfo),
    ?assertEqual(true, maps:get(enabled, Result, false)),
    ?assertEqual(true, maps:get(server_no_context_takeover, Result, false)).

test_client_no_context_takeover() ->
    %% Client no context takeover flag
    Extensions = <<"permessage-deflate; client_no_context_takeover">>,
    ConnectionInfo =
        #{headers =>
              #{<<"sec-websocket-extensions">> => Extensions}},
    Result = erlmcp_transport_ws:negotiate_compression(ConnectionInfo),
    ?assertEqual(true, maps:get(enabled, Result, false)),
    ?assertEqual(true, maps:get(client_no_context_takeover, Result, false)).

%%====================================================================
%% Compression/Decompression Tests
%%====================================================================

test_compress_decompress_roundtrip() ->
    %% Test compression and decompression roundtrip
    Original = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,

    %% Create compression and decompression streams
    CompressZ = zlib:open(),
    zlib:deflateInit(CompressZ, default, -15),

    DecompressZ = zlib:open(),
    zlib:inflateInit(DecompressZ, -15),

    %% Create test state using the helper function
    State = erlmcp_transport_ws:test_state(),
    State1 = State#state{compression_enabled = true,
                         compress_zstream = CompressZ,
                         decompress_zstream = DecompressZ,
                         server_max_window_bits = 15,
                         client_max_window_bits = 15,
                         server_no_context_takeover = false,
                         client_no_context_takeover = false},

    %% Compress
    {ok, Compressed} = erlmcp_transport_ws:compress_frame(Original, State1),

    %% Verify compressed data is different
    ?assertNotEqual(Original, Compressed),

    %% Decompress
    {ok, Decompressed} = erlmcp_transport_ws:decompress_frame(Compressed, State1),

    %% Should get original data back
    ?assertEqual(Original, Decompressed),

    %% Clean up
    zlib:close(CompressZ),
    zlib:close(DecompressZ).

test_compress_json_data() ->
    %% Test with realistic JSON-RPC data
    JsonData = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                           <<"method">> => <<"tools/call">>,
                           <<"params">> => #{<<"name">> => <<"test_tool">>,
                                            <<"arguments">> =>
                                                [<<"arg1">>, <<"arg2">>, <<"arg3">>]},
                           <<"id">> => 42}),

    CompressZ = zlib:open(),
    zlib:deflateInit(CompressZ, default, -15),

    DecompressZ = zlib:open(),
    zlib:inflateInit(DecompressZ, -15),

    State = erlmcp_transport_ws:test_state(),
    State1 = State#state{compression_enabled = true,
                         compress_zstream = CompressZ,
                         decompress_zstream = DecompressZ,
                         server_max_window_bits = 15,
                         client_max_window_bits = 15,
                         server_no_context_takeover = false,
                         client_no_context_takeover = false},

    %% Compress and decompress
    {ok, Compressed} = erlmcp_transport_ws:compress_frame(JsonData, State1),
    ?assertNotEqual(JsonData, Compressed),

    {ok, Decompressed} = erlmcp_transport_ws:decompress_frame(Compressed, State1),

    ?assertEqual(JsonData, Decompressed),

    %% Clean up
    zlib:close(CompressZ),
    zlib:close(DecompressZ).

%%====================================================================
%% Error Handling Tests
%%====================================================================

compression_error_handling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_decompress_invalid_data()),
      ?_test(test_compress_empty_data())]}.

test_decompress_invalid_data() ->
    %% Attempt to decompress invalid data
    InvalidData = <<255, 255, 255, 255>>,

    DecompressZ = zlib:open(),
    zlib:inflateInit(DecompressZ, -15),

    State = erlmcp_transport_ws:test_state(),
    State1 = State#state{compression_enabled = true,
                         decompress_zstream = DecompressZ,
                         client_max_window_bits = 15,
                         client_no_context_takeover = false},

    %% Should return error tuple
    Result = erlmcp_transport_ws:decompress_frame(InvalidData, State1),
    ?assertMatch({error, _}, Result),

    %% Clean up
    zlib:close(DecompressZ).

test_compress_empty_data() ->
    %% Compress empty data
    EmptyData = <<>>,

    CompressZ = zlib:open(),
    zlib:deflateInit(CompressZ, default, -15),

    State = erlmcp_transport_ws:test_state(),
    State1 = State#state{compression_enabled = true,
                         compress_zstream = CompressZ,
                         server_max_window_bits = 15,
                         server_no_context_takeover = false},

    %% Should handle empty data gracefully
    {ok, Compressed} = erlmcp_transport_ws:compress_frame(EmptyData, State1),
    ?assert(is_binary(Compressed)),

    %% Clean up
    zlib:close(CompressZ).
