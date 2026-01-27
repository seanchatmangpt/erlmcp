-module(erlmcp_setup).

-export([start_stdio_setup/2, start_tcp_setup/3, start_http_setup/3,
         start_transport_enhanced/3, start_transport_with_retry/4]).

-spec start_stdio_setup(binary(), map()) -> {ok, pid()} | {error, term()}.
start_stdio_setup(ServerId, Config) ->
  erlmcp:start_stdio_setup(ServerId, Config).

-spec start_tcp_setup(binary(), map(), map()) -> {ok, pid()} | {error, term()}.
start_tcp_setup(ServerId, ServerConfig, TcpConfig) ->
  erlmcp:start_tcp_setup(ServerId, ServerConfig, TcpConfig).

-spec start_http_setup(binary(), map(), map()) -> {ok, pid()} | {error, term()}.
start_http_setup(ServerId, ServerConfig, HttpConfig) ->
  erlmcp:start_http_setup(ServerId, ServerConfig, HttpConfig).

-spec start_transport_enhanced(binary(), atom(), map()) -> {ok, term()} | {error, term()}.
start_transport_enhanced(TransportId, Type, Config) ->
  erlmcp:start_transport_enhanced(TransportId, Type, Config).

-spec start_transport_with_retry(binary(), atom(), map(), integer()) -> {ok, term()} | {error, term()}.
start_transport_with_retry(TransportId, Type, Config, MaxRetries) ->
  erlmcp:start_transport_with_retry(TransportId, Type, Config, MaxRetries).
