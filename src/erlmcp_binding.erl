-module(erlmcp_binding).

-export([bind_transport_to_server/2, unbind_transport/1, get_transport_binding_info/1,
         list_transport_bindings/0, validate_transport_binding/2, get_enhanced_transport_status/1,
         audit_transport_bindings/0]).

-spec bind_transport_to_server(binary(), binary()) -> {ok, binary()} | {error, term()}.
bind_transport_to_server(TransportId, ServerId) ->
  erlmcp:bind_transport_to_server(TransportId, ServerId).

-spec unbind_transport(binary()) -> ok | {error, term()}.
unbind_transport(TransportId) ->
  erlmcp:unbind_transport(TransportId).

-spec get_transport_binding_info(binary()) -> {ok, map()} | {error, term()}.
get_transport_binding_info(TransportId) ->
  erlmcp:get_transport_binding_info(TransportId).

-spec list_transport_bindings() -> {ok, [map()]} | {error, term()}.
list_transport_bindings() ->
  erlmcp:list_transport_bindings().

-spec validate_transport_binding(binary(), binary()) -> boolean().
validate_transport_binding(TransportId, ServerId) ->
  erlmcp:validate_transport_binding(TransportId, ServerId).

-spec get_enhanced_transport_status(binary()) -> {ok, map()} | {error, term()}.
get_enhanced_transport_status(TransportId) ->
  erlmcp:get_enhanced_transport_status(TransportId).

-spec audit_transport_bindings() -> {ok, map()} | {error, term()}.
audit_transport_bindings() ->
  erlmcp:audit_transport_bindings().
