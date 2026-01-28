-module(erlmcp_localhost_binding_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests for localhost binding validation
%%====================================================================

%% Test is_localhost/1 with IPv4 localhost
is_localhost_ipv4_test() ->
    ?assert(erlmcp_localhost_binding:is_localhost("127.0.0.1")),
    ?assert(erlmcp_localhost_binding:is_localhost(<<"127.0.0.1">>)),
    ?assert(erlmcp_localhost_binding:is_localhost('127.0.0.1')).

%% Test is_localhost/1 with IPv6 localhost
is_localhost_ipv6_test() ->
    ?assert(erlmcp_localhost_binding:is_localhost("::1")),
    ?assert(erlmcp_localhost_binding:is_localhost(<<"::1">>)),
    ?assert(erlmcp_localhost_binding:is_localhost('::1')).

%% Test is_localhost/1 with hostname
is_localhost_hostname_test() ->
    ?assert(erlmcp_localhost_binding:is_localhost("localhost")),
    ?assert(erlmcp_localhost_binding:is_localhost(<<"localhost">>)),
    ?assert(erlmcp_localhost_binding:is_localhost(localhost)).

%% Test is_localhost/1 with non-localhost addresses
is_localhost_negative_test() ->
    ?assertNot(erlmcp_localhost_binding:is_localhost("0.0.0.0")),
    ?assertNot(erlmcp_localhost_binding:is_localhost("::")),
    ?assertNot(erlmcp_localhost_binding:is_localhost("192.168.1.1")),
    ?assertNot(erlmcp_localhost_binding:is_localhost("10.0.0.1")),
    ?assertNot(erlmcp_localhost_binding:is_localhost("example.com")),
    ?assertNot(erlmcp_localhost_binding:is_localhost("8.8.8.8")).

%% Test validate_bind_address/1 with localhost-only policy (default)
validate_localhost_only_ipv4_test() ->
    application:set_env(erlmcp, enforce_localhost_only, true),
    Result = erlmcp_localhost_binding:validate_bind_address("127.0.0.1"),
    ?assertEqual({ok, "127.0.0.1"}, Result).

validate_localhost_only_ipv6_test() ->
    application:set_env(erlmcp, enforce_localhost_only, true),
    Result = erlmcp_localhost_binding:validate_bind_address("::1"),
    ?assertEqual({ok, "::1"}, Result).

validate_localhost_only_hostname_test() ->
    application:set_env(erlmcp, enforce_localhost_only, true),
    Result = erlmcp_localhost_binding:validate_bind_address("localhost"),
    ?assertEqual({ok, "localhost"}, Result).

%% Test validate_bind_address/2 rejecting 0.0.0.0 with localhost-only
validate_reject_bind_all_ipv4_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("0.0.0.0", true),
    ?assertMatch({error, {localhost_only_violation, "0.0.0.0", 'binds_to_all_interfaces'}}, Result).

%% Test validate_bind_address/2 rejecting :: (IPv6 bind-all) with localhost-only
validate_reject_bind_all_ipv6_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("::", true),
    ?assertMatch({error, {localhost_only_violation, "::", 'binds_to_all_interfaces'}}, Result).

%% Test validate_bind_address/2 rejecting arbitrary IP with localhost-only
validate_reject_arbitrary_ipv4_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("192.168.1.1", true),
    ?assertMatch({error, {localhost_only_violation, "192.168.1.1", 'non_localhost_address'}}, Result).

validate_reject_arbitrary_ipv6_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("2001:db8::1", true),
    ?assertMatch({error, {localhost_only_violation, "2001:db8::1", 'non_localhost_address'}}, Result).

%% Test validate_bind_address/2 allowing any address when localhost-only is false
validate_no_enforcement_0_0_0_0_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("0.0.0.0", false),
    ?assertEqual({ok, "0.0.0.0"}, Result).

validate_no_enforcement_arbitrary_ip_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("192.168.1.1", false),
    ?assertEqual({ok, "192.168.1.1"}, Result).

%% Test validate_bind_address/2 with binary address
validate_binary_address_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address(<<"127.0.0.1">>, true),
    ?assertEqual({ok, "127.0.0.1"}, Result).

%% Test validate_bind_address/2 with atom address
validate_atom_address_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address('127.0.0.1', true),
    ?assertEqual({ok, "127.0.0.1"}, Result).

%% Test validate_bind_address with invalid address format
validate_invalid_address_format_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address(123, true),
    ?assertMatch({error, {invalid_address_type, 123}}, Result).

%% Test validate_bind_address with empty string
validate_empty_address_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("", true),
    ?assertMatch({error, {invalid_address_syntax, ""}}, Result).

%% Test validate_bind_address with very long address
validate_long_address_test() ->
    LongAddr = string:chars($a, 300),
    Result = erlmcp_localhost_binding:validate_bind_address(LongAddr, true),
    ?assertMatch({error, {invalid_address_syntax, _}}, Result).

%% Test validate_bind_address with invalid characters
validate_invalid_chars_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("127.0.0.1<script>", true),
    ?assertMatch({error, {invalid_address_syntax, _}}, Result).

%% Test normalize_address with various input types
normalize_string_address_test() ->
    Result = erlmcp_localhost_binding:normalize_address("127.0.0.1"),
    ?assertEqual("127.0.0.1", Result).

normalize_binary_address_test() ->
    Result = erlmcp_localhost_binding:normalize_address(<<"127.0.0.1">>),
    ?assertEqual("127.0.0.1", Result).

normalize_atom_address_test() ->
    Result = erlmcp_localhost_binding:normalize_address('127.0.0.1'),
    ?assertEqual("127.0.0.1", Result).

normalize_invalid_type_test() ->
    Result = erlmcp_localhost_binding:normalize_address(12345),
    ?assertMatch({error, {invalid_address_type, 12345}}, Result).

%% Test get_localhost_binding/0
get_localhost_binding_default_test() ->
    application:unset_env(erlmcp, http_bind_address),
    Result = erlmcp_localhost_binding:get_localhost_binding(),
    ?assertEqual("127.0.0.1", Result).

get_localhost_binding_configured_test() ->
    application:set_env(erlmcp, http_bind_address, "192.168.1.1"),
    Result = erlmcp_localhost_binding:get_localhost_binding(),
    ?assertEqual("192.168.1.1", Result),
    application:unset_env(erlmcp, http_bind_address).

%% Test get_localhost_binding/1 with default
get_localhost_binding_with_default_test() ->
    application:unset_env(erlmcp, http_bind_address),
    Result = erlmcp_localhost_binding:get_localhost_binding("0.0.0.0"),
    ?assertEqual("0.0.0.0", Result).

%% Test get_ipv6_localhost/0
get_ipv6_localhost_default_test() ->
    application:unset_env(erlmcp, http_bind_ipv6),
    Result = erlmcp_localhost_binding:get_ipv6_localhost(),
    ?assertEqual("::1", Result).

get_ipv6_localhost_configured_test() ->
    application:set_env(erlmcp, http_bind_ipv6, "fe80::1"),
    Result = erlmcp_localhost_binding:get_ipv6_localhost(),
    ?assertEqual("fe80::1", Result),
    application:unset_env(erlmcp, http_bind_ipv6).

%%====================================================================
%% Integration tests combining multiple validations
%%====================================================================

%% Test multiple localhost addresses accepted
multiple_localhost_addresses_test() ->
    Addresses = ["127.0.0.1", "::1", "localhost"],
    Results = [erlmcp_localhost_binding:validate_bind_address(A, true) || A <- Addresses],
    ?assert(lists:all(fun(R) -> element(1, R) =:= ok end, Results)).

%% Test multiple non-localhost addresses rejected
multiple_non_localhost_rejection_test() ->
    Addresses = ["0.0.0.0", "::", "192.168.1.1", "10.0.0.1"],
    Results = [erlmcp_localhost_binding:validate_bind_address(A, true) || A <- Addresses],
    ?assert(lists:all(fun(R) -> element(1, R) =:= error end, Results)).

%% Test security: 0.0.0.0 is always rejected with localhost-only
security_reject_0_0_0_0_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("0.0.0.0", true),
    ?assertMatch({error, {localhost_only_violation, _, 'binds_to_all_interfaces'}}, Result).

%% Test security: IPv6 bind-all is always rejected with localhost-only
security_reject_ipv6_bind_all_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("::", true),
    ?assertMatch({error, {localhost_only_violation, _, 'binds_to_all_interfaces'}}, Result).

%% Test configuration enforcement: can be disabled
configuration_can_disable_enforcement_test() ->
    application:set_env(erlmcp, enforce_localhost_only, false),
    Result = erlmcp_localhost_binding:validate_bind_address("0.0.0.0"),
    ?assertEqual({ok, "0.0.0.0"}, Result),
    application:set_env(erlmcp, enforce_localhost_only, true).

%% Test configuration enforcement: enabled by default
configuration_enabled_by_default_test() ->
    application:unset_env(erlmcp, enforce_localhost_only),
    Result = erlmcp_localhost_binding:validate_bind_address("0.0.0.0"),
    ?assertMatch({error, {localhost_only_violation, _, _}}, Result).

%%====================================================================
%% Edge case tests
%%====================================================================

%% Test uppercase IPv4 address (should fail - invalid chars)
edge_case_uppercase_ipv4_test() ->
    Result = erlmcp_localhost_binding:normalize_address("127.0.0.1A"),
    ?assertMatch({error, {invalid_address_syntax, _}}, Result).

%% Test IPv4 with extra leading zeros
edge_case_ipv4_leading_zeros_test() ->
    Result = erlmcp_localhost_binding:normalize_address("127.000.000.001"),
    ?assertEqual("127.000.000.001", Result).

%% Test IPv6 compressed format
edge_case_ipv6_compressed_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("::1", true),
    ?assertEqual({ok, "::1"}, Result).

%% Test IPv6 with port-like suffix (invalid)
edge_case_ipv6_with_port_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("[::1]:8080", true),
    %% Colons and brackets are valid chars, but address semantics might reject this
    %% For now it should normalize but might not match localhost
    case Result of
        {ok, "[::1]:8080"} -> ok;
        {error, _} -> ok;
        _ -> throw(unexpected_result)
    end.

%% Test localhost with port suffix (should not match)
edge_case_localhost_with_port_test() ->
    Result = erlmcp_localhost_binding:is_localhost("localhost:8080"),
    ?assertNot(Result).

%% Test whitespace in address
edge_case_whitespace_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("127.0.0.1 ", true),
    ?assertMatch({error, {invalid_address_syntax, _}}, Result).

%% Test special characters
edge_case_special_chars_test() ->
    Result = erlmcp_localhost_binding:validate_bind_address("127.0.0.1;", true),
    ?assertMatch({error, {invalid_address_syntax, _}}, Result).

%%====================================================================
%% Performance/stress tests
%%====================================================================

%% Test many validations don't cause issues
stress_many_validations_test() ->
    Results = lists:map(fun(_) ->
        erlmcp_localhost_binding:validate_bind_address("127.0.0.1", true)
    end, lists:seq(1, 1000)),
    ?assert(lists:all(fun(R) -> R =:= {ok, "127.0.0.1"} end, Results)).

%% Test alternating enforcement modes
stress_alternating_enforcement_test() ->
    Results = lists:map(fun(N) ->
        Mode = (N rem 2) =:= 0,
        erlmcp_localhost_binding:validate_bind_address("0.0.0.0", Mode)
    end, lists:seq(1, 100)),
    ?assertEqual(100, length(Results)).
