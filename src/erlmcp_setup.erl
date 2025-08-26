-module(erlmcp_setup).

-export([start_stdio_setup/2, start_tcp_setup/3, start_http_setup/3,
         start_transport_enhanced/3, start_transport_with_retry/4]).

start_stdio_setup(ServerId, Config) ->
  erlmcp:start_stdio_setup(ServerId, Config).

start_tcp_setup(ServerId, ServerConfig, TcpConfig) ->
  erlmcp:start_tcp_setup(ServerId, ServerConfig, TcpConfig).

start_http_setup(ServerId, ServerConfig, HttpConfig) ->
  erlmcp:start_http_setup(ServerId, ServerConfig, HttpConfig).

start_transport_enhanced(TransportId, Type, Config) ->
  erlmcp:start_transport_enhanced(TransportId, Type, Config).

start_transport_with_retry(TransportId, Type, Config, MaxRetries) ->
  erlmcp:start_transport_with_retry(TransportId, Type, Config, MaxRetries).
