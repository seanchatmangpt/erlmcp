-module(erlmcp_transport_api).

-export([start_transport/2, start_transport/3, stop_transport/1, list_transports/0,
         ensure_transport_supervisor/0]).

%% Thin wrappers to existing erlmcp APIs for safe incremental cutover

start_transport(TransportId, Type) ->
  erlmcp:start_transport(TransportId, Type).

start_transport(TransportId, Type, Config) ->
  erlmcp:start_transport(TransportId, Type, Config).

stop_transport(TransportId) ->
  erlmcp:stop_transport(TransportId).

list_transports() ->
  erlmcp:list_transports().

ensure_transport_supervisor() ->
  erlmcp:ensure_transport_supervisor().
