%%%-------------------------------------------------------------------
%%% @doc
%%% DNS-SD (mDNS) Discovery Tests
%%%
%%% Tests for mDNS/Bonjour discovery implementation.
%%% Tests real UDP sockets, real mDNS packet parsing.
%%%
%%% Chicago School TDD: Tests use real processes, no mocks.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_discovery_dns_sd_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Unit Tests - mDNS Query Building
%%====================================================================

%% @doc Test building mDNS query for _erlmcp._tcp.local
mdns_query_build_test() ->
    Service = <<"_erlmcp._tcp.local">>,
    Query = erlmcp_transport_discovery:build_mdns_query(Service),

    % Verify packet is at least minimum size (header 12 + question)
    ?assert(byte_size(Query) > 20),

    % Check mDNS header: Transaction ID = 0
    <<TransactionId:16, _Rest/binary>> = Query,
    ?assertEqual(0, TransactionId).

%% @doc Test mDNS query has correct flags
mdns_query_flags_test() ->
    Service = <<"_erlmcp._tcp.local">>,
    Query = erlmcp_transport_discovery:build_mdns_query(Service),

    % Skip transaction ID
    <<_TransactionId:16, Flags:16, _Rest/binary>> = Query,

    % Flags should be 0x0100 (standard query, recursion desired)
    ?assertEqual(16#0100, Flags).

%% @doc Test mDNS query has one question
mdns_query_question_count_test() ->
    Service = <<"_erlmcp._tcp.local">>,
    Query = erlmcp_transport_discovery:build_mdns_query(Service),

    % Parse header
    <<_TransactionId:16, _Flags:16, Questions:16, _Rest/binary>> = Query,

    % Should have exactly 1 question
    ?assertEqual(1, Questions).

%%====================================================================
%% Unit Tests - mDNS Response Parsing
%%====================================================================

%% @doc Test parsing valid mDNS response with PTR record
mdns_response_parse_ptr_test() ->
    % Build a minimal mDNS response
    % Header: ID=0, Flags=0x8400 (response), Q=0, A=1, Auth=0, Add=0
    Header = <<0:16, 16#8400:16, 0:16, 1:16, 0:16, 0:16>>,

    % Answer: compressed name, TYPE=PTR (12), CLASS=IN (1), TTL=3600, RDLENGTH=10
    Target = <<"target.host">>,
    Answer = <<16#C0:8, 16#0C:8, 12:16, 1:16, 3600:32, (byte_size(Target)):16,
              Target/binary>>,

    Packet = <<Header/binary, Answer/binary>>,

    {ok, Transports} = erlmcp_transport_discovery:parse_mdns_response(Packet),

    % Should create at least one transport from PTR record
    ?assert(maps:size(Transports) >= 1),

    % Verify transport has expected fields
    TransportId = hd(maps:keys(Transports)),
    Config = maps:get(TransportId, Transports),
    ?assertEqual(dns_sd, maps:get(discovered_via, Config)).

%% @doc Test parsing mDNS response with no answers
mdns_response_parse_no_answers_test() ->
    % Header: ID=0, Flags=0x8400 (response), Q=0, A=0
    Header = <<0:16, 16#8400:16, 0:16, 0:16, 0:16, 0:16>>,

    Packet = <<Header/binary>>,

    {ok, Transports} = erlmcp_transport_discovery:parse_mdns_response(Packet),

    % Should return empty map
    ?assertEqual(0, maps:size(Transports)).

%% @doc Test parsing empty packet
mdns_response_parse_empty_test() ->
    Packet = <<>>,

    {ok, Transports} = erlmcp_transport_discovery:parse_mdns_response(Packet),

    % Should handle gracefully
    ?assertEqual(0, maps:size(Transports)).

%%====================================================================
%% Integration Tests - UDP Socket Operations
%%====================================================================

%% @doc Test opening UDP socket for mDNS
udp_socket_open_test() ->
    % Open a UDP socket (same parameters as mDNS discovery)
    {ok, Socket} = gen_udp:open(0, [
        binary,
        {active, false},
        {reuseaddr, true}
    ]),

    % Verify socket is open
    ?assert(is_port(Socket)),

    % Clean close
    ok = gen_udp:close(Socket).

%% @doc Test sending data to multicast address
udp_send_multicast_test() ->
    {ok, Socket} = gen_udp:open(0, [
        binary,
        {active, false},
        {reuseaddr, true}
    ]),

    % Send test packet to mDNS multicast address
    Packet = <<0, 1, 2, 3>>,
    Result = gen_udp:send(Socket, {224,0,0,251}, 5353, Packet),

    % Should succeed (even if no one is listening)
    ?assertEqual(ok, Result),

    gen_udp:close(Socket).

%% @doc Test UDP timeout
udp_recv_timeout_test() ->
    {ok, Socket} = gen_udp:open(0, [
        binary,
        {active, false}
    ]),

    % Try to receive with short timeout
    Result = gen_udp:recv(Socket, 1500, 100),

    % Should timeout (no data sent)
    ?assertMatch({error, timeout}, Result),

    gen_udp:close(Socket).

%% @doc Test broadcast socket option
udp_broadcast_socket_test() ->
    {ok, Socket} = gen_udp:open(0, [
        binary,
        {active, false},
        {broadcast, true}
    ]),

    % Send to broadcast address
    Packet = <<"{\"test\": true}">>,
    Result = gen_udp:send(Socket, {255,255,255,255}, 9900, Packet),

    ?assertEqual(ok, Result),

    gen_udp:close(Socket).
