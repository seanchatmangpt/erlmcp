-module(erlmcp_config_validation_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases for Transport Configuration Validation
%%====================================================================

%%--------------------------------------------------------------------
%% STDIO Transport Validation Tests
%%--------------------------------------------------------------------

validate_stdio_minimal_config_test() ->
    Config = #{type => stdio},
    ?assertEqual(ok, erlmcp:validate_transport_config(Config)).

validate_stdio_full_config_test() ->
    Config = #{
        type => stdio,
        server_id => test_server,
        buffer_size => 8192,
        read_timeout => 10000
    },
    ?assertEqual(ok, erlmcp:validate_transport_config(Config)).

validate_stdio_invalid_buffer_size_test() ->
    Config = #{type => stdio, buffer_size => -1},
    {error, {validation_error, {invalid_buffer_size, -1}}} =
        erlmcp:validate_transport_config(Config).

validate_stdio_invalid_timeout_test() ->
    Config = #{type => stdio, read_timeout => -100},
    {error, {validation_error, {invalid_timeout, read_timeout, -100}}} =
        erlmcp:validate_transport_config(Config).

validate_stdio_unknown_field_test() ->
    Config = #{type => stdio, unknown_field => value},
    {error, {validation_error, {unknown_fields, [unknown_field]}}} =
        erlmcp:validate_transport_config(Config).

%%--------------------------------------------------------------------
%% TCP Transport Validation Tests
%%--------------------------------------------------------------------

validate_tcp_minimal_config_test() ->
    Config = #{
        type => tcp,
        host => <<"localhost">>,
        port => 8080
    },
    ?assertEqual(ok, erlmcp:validate_transport_config(Config)).

validate_tcp_full_config_test() ->
    Config = #{
        type => tcp,
        host => <<"example.com">>,
        port => 9000,
        server_id => test_server,
        keepalive => true,
        connect_timeout => 5000,
        recv_timeout => 30000,
        send_timeout => 5000,
        nodelay => true,
        buffer_size => 4096,
        max_connections => 100
    },
    ?assertEqual(ok, erlmcp:validate_transport_config(Config)).

validate_tcp_missing_host_test() ->
    Config = #{type => tcp, port => 8080},
    {error, {validation_error, {missing_required_fields, [host]}}} =
        erlmcp:validate_transport_config(Config).

validate_tcp_missing_port_test() ->
    Config = #{type => tcp, host => <<"localhost">>},
    {error, {validation_error, {missing_required_fields, [port]}}} =
        erlmcp:validate_transport_config(Config).

validate_tcp_invalid_port_test() ->
    Config = #{type => tcp, host => <<"localhost">>, port => 70000},
    {error, {validation_error, {invalid_port, 70000}}} =
        erlmcp:validate_transport_config(Config).

validate_tcp_invalid_host_test() ->
    Config = #{type => tcp, host => <<>>, port => 8080},
    {error, {validation_error, {invalid_host, <<>>}}} =
        erlmcp:validate_transport_config(Config).

validate_tcp_string_host_test() ->
    Config = #{type => tcp, host => "localhost", port => 8080},
    ?assertEqual(ok, erlmcp:validate_transport_config(Config)).

validate_tcp_invalid_keepalive_test() ->
    Config = #{type => tcp, host => <<"localhost">>, port => 8080, keepalive => "true"},
    {error, {validation_error, {invalid_boolean, keepalive, "true"}}} =
        erlmcp:validate_transport_config(Config).

validate_tcp_infinity_timeout_test() ->
    Config = #{type => tcp, host => <<"localhost">>, port => 8080, recv_timeout => infinity},
    ?assertEqual(ok, erlmcp:validate_transport_config(Config)).

%%--------------------------------------------------------------------
%% HTTP Transport Validation Tests
%%--------------------------------------------------------------------

validate_http_minimal_config_test() ->
    Config = #{
        type => http,
        url => <<"https://example.com/api">>
    },
    ?assertEqual(ok, erlmcp:validate_transport_config(Config)).

validate_http_full_config_test() ->
    Config = #{
        type => http,
        url => <<"https://api.example.com/mcp">>,
        server_id => test_server,
        method => <<"POST">>,
        headers => #{<<"Content-Type">> => <<"application/json">>},
        timeout => 60000,
        max_redirects => 3,
        verify_ssl => true,
        compression => true
    },
    ?assertEqual(ok, erlmcp:validate_transport_config(Config)).

validate_http_missing_url_test() ->
    Config = #{type => http},
    {error, {validation_error, {missing_required_fields, [url]}}} =
        erlmcp:validate_transport_config(Config).

validate_http_invalid_url_test() ->
    Config = #{type => http, url => <<"ftp://example.com">>},
    {error, {validation_error, {invalid_url, <<"ftp://example.com">>}}} =
        erlmcp:validate_transport_config(Config).

validate_http_valid_method_atom_test() ->
    Config = #{type => http, url => <<"http://example.com">>, method => post},
    ?assertEqual(ok, erlmcp:validate_transport_config(Config)).

validate_http_valid_method_binary_test() ->
    Config = #{type => http, url => <<"http://example.com">>, method => <<"GET">>},
    ?assertEqual(ok, erlmcp:validate_transport_config(Config)).

validate_http_invalid_method_test() ->
    Config = #{type => http, url => <<"http://example.com">>, method => <<"INVALID">>},
    {error, {validation_error, {invalid_http_method, <<"INVALID">>}}} =
        erlmcp:validate_transport_config(Config).

validate_http_headers_as_map_test() ->
    Config = #{
        type => http,
        url => <<"http://example.com">>,
        headers => #{<<"X-Custom">> => <<"value">>}
    },
    ?assertEqual(ok, erlmcp:validate_transport_config(Config)).

validate_http_headers_as_list_test() ->
    Config = #{
        type => http,
        url => <<"http://example.com">>,
        headers => [{<<"X-Custom">>, <<"value">>}, {<<"X-Another">>, <<"value2">>}]
    },
    ?assertEqual(ok, erlmcp:validate_transport_config(Config)).

validate_http_invalid_headers_test() ->
    Config = #{type => http, url => <<"http://example.com">>, headers => "invalid"},
    {error, {validation_error, {invalid_headers, "invalid"}}} =
        erlmcp:validate_transport_config(Config).

validate_http_string_url_test() ->
    Config = #{type => http, url => "https://example.com"},
    ?assertEqual(ok, erlmcp:validate_transport_config(Config)).

validate_http_invalid_max_redirects_test() ->
    Config = #{type => http, url => <<"http://example.com">>, max_redirects => -1},
    {error, {validation_error, {invalid_max_redirects, -1}}} =
        erlmcp:validate_transport_config(Config).

%%--------------------------------------------------------------------
%% General Validation Tests
%%--------------------------------------------------------------------

validate_missing_type_test() ->
    Config = #{host => <<"localhost">>, port => 8080},
    {error, {validation_error, missing_required_field_type}} =
        erlmcp:validate_transport_config(Config).

validate_unsupported_type_test() ->
    Config = #{type => websocket},
    {error, {validation_error, {unsupported_transport_type, websocket}}} =
        erlmcp:validate_transport_config(Config).

validate_not_a_map_test() ->
    {error, {validation_error, config_must_be_map}} =
        erlmcp:validate_transport_config("not a map").

validate_empty_map_test() ->
    {error, {validation_error, missing_required_field_type}} =
        erlmcp:validate_transport_config(#{}).
