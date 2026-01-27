#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/erlmcp/ebin

-module(validation_test_examples).
-mode(compile).

main(_) ->
    io:format("~n=== Transport Configuration Validation Examples ===~n~n"),

    %% Test STDIO validation
    io:format("1. Valid STDIO config:~n"),
    StdioConfig = #{type => stdio, server_id => test_server, buffer_size => 8192},
    io:format("   Config: ~p~n", [StdioConfig]),
    io:format("   Result: ~p~n~n", [erlmcp:validate_transport_config(StdioConfig)]),

    io:format("2. Invalid STDIO config (negative buffer):~n"),
    InvalidStdio = #{type => stdio, buffer_size => -1},
    io:format("   Config: ~p~n", [InvalidStdio]),
    io:format("   Result: ~p~n~n", [erlmcp:validate_transport_config(InvalidStdio)]),

    %% Test TCP validation
    io:format("3. Valid TCP config:~n"),
    TcpConfig = #{
        type => tcp,
        host => <<"localhost">>,
        port => 8080,
        keepalive => true,
        connect_timeout => 5000
    },
    io:format("   Config: ~p~n", [TcpConfig]),
    io:format("   Result: ~p~n~n", [erlmcp:validate_transport_config(TcpConfig)]),

    io:format("4. Invalid TCP config (missing port):~n"),
    InvalidTcp = #{type => tcp, host => <<"localhost">>},
    io:format("   Config: ~p~n", [InvalidTcp]),
    io:format("   Result: ~p~n~n", [erlmcp:validate_transport_config(InvalidTcp)]),

    io:format("5. Invalid TCP config (bad port):~n"),
    InvalidTcpPort = #{type => tcp, host => <<"localhost">>, port => 70000},
    io:format("   Config: ~p~n", [InvalidTcpPort]),
    io:format("   Result: ~p~n~n", [erlmcp:validate_transport_config(InvalidTcpPort)]),

    %% Test HTTP validation
    io:format("6. Valid HTTP config:~n"),
    HttpConfig = #{
        type => http,
        url => <<"https://api.example.com/mcp">>,
        method => <<"POST">>,
        headers => #{<<"Content-Type">> => <<"application/json">>},
        timeout => 30000
    },
    io:format("   Config: ~p~n", [HttpConfig]),
    io:format("   Result: ~p~n~n", [erlmcp:validate_transport_config(HttpConfig)]),

    io:format("7. Invalid HTTP config (bad URL):~n"),
    InvalidHttp = #{type => http, url => <<"ftp://example.com">>},
    io:format("   Config: ~p~n", [InvalidHttp]),
    io:format("   Result: ~p~n~n", [erlmcp:validate_transport_config(InvalidHttp)]),

    io:format("8. Invalid HTTP config (bad method):~n"),
    InvalidHttpMethod = #{type => http, url => <<"http://example.com">>, method => <<"INVALID">>},
    io:format("   Config: ~p~n", [InvalidHttpMethod]),
    io:format("   Result: ~p~n~n", [erlmcp:validate_transport_config(InvalidHttpMethod)]),

    %% Test general validation
    io:format("9. Missing type field:~n"),
    NoType = #{host => <<"localhost">>, port => 8080},
    io:format("   Config: ~p~n", [NoType]),
    io:format("   Result: ~p~n~n", [erlmcp:validate_transport_config(NoType)]),

    io:format("10. Unknown transport type:~n"),
    UnknownType = #{type => websocket},
    io:format("   Config: ~p~n", [UnknownType]),
    io:format("   Result: ~p~n~n", [erlmcp:validate_transport_config(UnknownType)]),

    io:format("=== All validation examples completed ===~n").
