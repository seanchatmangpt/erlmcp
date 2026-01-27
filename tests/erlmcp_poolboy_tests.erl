%%%-------------------------------------------------------------------
%%% @doc Tests for poolboy integration in erlmcp
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_poolboy_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% Test pool_name/1 function
pool_name_test() ->
    ?assertEqual(erlmcp_stdio_pool, erlmcp:pool_name(stdio)),
    ?assertEqual(erlmcp_tcp_pool, erlmcp:pool_name(tcp)),
    ?assertEqual(erlmcp_http_pool, erlmcp:pool_name(http)),

    % Test dynamic pool name generation
    CustomPool = erlmcp:pool_name(custom_type),
    ?assert(is_atom(CustomPool)).

%% Test worker_module/1 function
worker_module_test() ->
    ?assertEqual(erlmcp_transport_stdio_worker, erlmcp:worker_module(stdio)),
    ?assertEqual(erlmcp_transport_tcp_worker, erlmcp:worker_module(tcp)),
    ?assertEqual(erlmcp_transport_http_worker, erlmcp:worker_module(http)),
    ?assertEqual(erlmcp_transport_worker, erlmcp:worker_module(unknown)).

%% Test transport config validation for stdio
validate_stdio_config_test() ->
    % Valid config
    ValidConfig = #{
        type => stdio,
        buffer_size => 4096,
        read_timeout => 5000
    },
    ?assertEqual(ok, erlmcp:validate_transport_config(ValidConfig)),

    % Minimal valid config
    MinimalConfig = #{type => stdio},
    ?assertEqual(ok, erlmcp:validate_transport_config(MinimalConfig)),

    % Invalid buffer size
    InvalidBuffer = #{type => stdio, buffer_size => -1},
    ?assertMatch({error, {validation_error, _}},
        erlmcp:validate_transport_config(InvalidBuffer)),

    % Invalid timeout
    InvalidTimeout = #{type => stdio, read_timeout => -1000},
    ?assertMatch({error, {validation_error, _}},
        erlmcp:validate_transport_config(InvalidTimeout)).

%% Test transport config validation for tcp
validate_tcp_config_test() ->
    % Valid config
    ValidConfig = #{
        type => tcp,
        host => <<"localhost">>,
        port => 8080,
        keepalive => true,
        connect_timeout => 5000
    },
    ?assertEqual(ok, erlmcp:validate_transport_config(ValidConfig)),

    % Missing required field (host)
    MissingHost = #{type => tcp, port => 8080},
    ?assertMatch({error, {validation_error, {missing_required_fields, _}}},
        erlmcp:validate_transport_config(MissingHost)),

    % Missing required field (port)
    MissingPort = #{type => tcp, host => <<"localhost">>},
    ?assertMatch({error, {validation_error, {missing_required_fields, _}}},
        erlmcp:validate_transport_config(MissingPort)),

    % Invalid port (too high)
    InvalidPort = #{type => tcp, host => <<"localhost">>, port => 99999},
    ?assertMatch({error, {validation_error, _}},
        erlmcp:validate_transport_config(InvalidPort)),

    % Invalid port (too low)
    InvalidPort2 = #{type => tcp, host => <<"localhost">>, port => 0},
    ?assertMatch({error, {validation_error, _}},
        erlmcp:validate_transport_config(InvalidPort2)),

    % Invalid host (empty)
    InvalidHost = #{type => tcp, host => <<>>, port => 8080},
    ?assertMatch({error, {validation_error, _}},
        erlmcp:validate_transport_config(InvalidHost)).

%% Test transport config validation for http
validate_http_config_test() ->
    % Valid config
    ValidConfig = #{
        type => http,
        url => <<"https://api.example.com">>,
        method => get,
        timeout => 30000
    },
    ?assertEqual(ok, erlmcp:validate_transport_config(ValidConfig)),

    % Valid with http URL
    ValidHttp = #{type => http, url => <<"http://example.com">>},
    ?assertEqual(ok, erlmcp:validate_transport_config(ValidHttp)),

    % Missing required field (url)
    MissingUrl = #{type => http},
    ?assertMatch({error, {validation_error, {missing_required_fields, _}}},
        erlmcp:validate_transport_config(MissingUrl)),

    % Invalid URL (no protocol)
    InvalidUrl = #{type => http, url => <<"example.com">>},
    ?assertMatch({error, {validation_error, _}},
        erlmcp:validate_transport_config(InvalidUrl)),

    % Invalid HTTP method
    InvalidMethod = #{type => http, url => <<"http://example.com">>, method => invalid},
    ?assertMatch({error, {validation_error, _}},
        erlmcp:validate_transport_config(InvalidMethod)),

    % Valid HTTP methods
    lists:foreach(fun(Method) ->
        Config = #{type => http, url => <<"http://example.com">>, method => Method},
        ?assertEqual(ok, erlmcp:validate_transport_config(Config))
    end, [get, post, put, delete, patch, head, options]).

%% Test validation with missing type field
validate_missing_type_test() ->
    Config = #{host => <<"localhost">>, port => 8080},
    ?assertMatch({error, {validation_error, missing_required_field_type}},
        erlmcp:validate_transport_config(Config)).

%% Test validation with unsupported transport type
validate_unsupported_type_test() ->
    Config = #{type => websocket, url => <<"ws://example.com">>},
    ?assertMatch({error, {validation_error, {unsupported_transport_type, _}}},
        erlmcp:validate_transport_config(Config)).

%% Test validation with non-map config
validate_non_map_test() ->
    ?assertMatch({error, {validation_error, config_must_be_map}},
        erlmcp:validate_transport_config([])),
    ?assertMatch({error, {validation_error, config_must_be_map}},
        erlmcp:validate_transport_config(invalid)).

%% Test pool status when pool doesn't exist
pool_status_not_found_test() ->
    % Ensure pool doesn't exist
    _ = erlmcp:stop_connection_pool(tcp),

    Result = erlmcp:pool_status(tcp),
    ?assertMatch({error, {pool_not_found, erlmcp_tcp_pool}}, Result).

%% Test unknown fields detection
validate_unknown_fields_test() ->
    Config = #{
        type => tcp,
        host => <<"localhost">>,
        port => 8080,
        unknown_field => value,
        another_unknown => 123
    },
    ?assertMatch({error, {validation_error, {unknown_fields, _}}},
        erlmcp:validate_transport_config(Config)).

%% Test timeout validation
validate_timeout_values_test() ->
    % Valid timeout (positive integer)
    ?assertEqual(ok, erlmcp:validate_transport_config(#{
        type => stdio,
        read_timeout => 5000
    })),

    % Valid timeout (infinity)
    ?assertEqual(ok, erlmcp:validate_transport_config(#{
        type => tcp,
        host => <<"localhost">>,
        port => 8080,
        connect_timeout => infinity
    })),

    % Invalid timeout (negative)
    ?assertMatch({error, {validation_error, _}},
        erlmcp:validate_transport_config(#{
            type => stdio,
            read_timeout => -1
        })),

    % Invalid timeout (zero)
    ?assertMatch({error, {validation_error, _}},
        erlmcp:validate_transport_config(#{
            type => stdio,
            read_timeout => 0
        })).

%% Test boolean field validation
validate_boolean_fields_test() ->
    % Valid boolean
    ?assertEqual(ok, erlmcp:validate_transport_config(#{
        type => tcp,
        host => <<"localhost">>,
        port => 8080,
        keepalive => true,
        nodelay => false
    })),

    % Invalid boolean
    ?assertMatch({error, {validation_error, _}},
        erlmcp:validate_transport_config(#{
            type => tcp,
            host => <<"localhost">>,
            port => 8080,
            keepalive => "true"  % String instead of boolean
        })).

%% Test header validation for HTTP
validate_http_headers_test() ->
    % Valid headers (map)
    ?assertEqual(ok, erlmcp:validate_transport_config(#{
        type => http,
        url => <<"http://example.com">>,
        headers => #{<<"Content-Type">> => <<"application/json">>}
    })),

    % Valid headers (list of tuples)
    ?assertEqual(ok, erlmcp:validate_transport_config(#{
        type => http,
        url => <<"http://example.com">>,
        headers => [{<<"Content-Type">>, <<"application/json">>}]
    })),

    % Invalid headers (not map or list)
    ?assertMatch({error, {validation_error, _}},
        erlmcp:validate_transport_config(#{
            type => http,
            url => <<"http://example.com">>,
            headers => "invalid"
        })).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Setup function for tests that need cleanup
setup() ->
    % Clean up any existing pools
    lists:foreach(fun erlmcp:stop_connection_pool/1, [tcp, http, stdio]),
    ok.

%% Teardown function
teardown(_) ->
    % Clean up any pools created during test
    lists:foreach(fun erlmcp:stop_connection_pool/1, [tcp, http, stdio]),
    ok.
