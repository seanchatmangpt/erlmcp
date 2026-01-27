-module(erlmcp_transport_api).

-export([start_transport/2, start_transport/3, stop_transport/1, list_transports/0,
         ensure_transport_supervisor/0]).

%% Thin wrappers to existing erlmcp APIs for safe incremental cutover

-spec start_transport(binary(), atom()) -> {ok, pid()} | {error, term()}.
start_transport(TransportId, Type) ->
  erlmcp:start_transport(TransportId, Type).

-spec start_transport(binary(), atom(), map()) -> {ok, pid()} | {error, term()}.
start_transport(TransportId, Type, Config) ->
  erlmcp:start_transport(TransportId, Type, Config).

-spec stop_transport(binary()) -> ok | {error, term()}.
stop_transport(TransportId) ->
  erlmcp:stop_transport(TransportId).

-spec list_transports() -> [binary()].
list_transports() ->
  erlmcp:list_transports().

-spec ensure_transport_supervisor() -> {ok, pid()} | {error, term()}.
ensure_transport_supervisor() ->
  erlmcp:ensure_transport_supervisor().
