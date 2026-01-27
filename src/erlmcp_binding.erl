-module(erlmcp_binding).

-export([bind_transport_to_server/2, unbind_transport/1, get_transport_binding_info/1,
         list_transport_bindings/0, validate_transport_binding/2, get_enhanced_transport_status/1,
         audit_transport_bindings/0]).

bind_transport_to_server(TransportId, ServerId) ->
  erlmcp:bind_transport_to_server(TransportId, ServerId).

unbind_transport(TransportId) ->
  erlmcp:unbind_transport(TransportId).

get_transport_binding_info(TransportId) ->
  erlmcp:get_transport_binding_info(TransportId).

list_transport_bindings() ->
  erlmcp:list_transport_bindings().

validate_transport_binding(TransportId, ServerId) ->
  erlmcp:validate_transport_binding(TransportId, ServerId).

get_enhanced_transport_status(TransportId) ->
  erlmcp:get_enhanced_transport_status(TransportId).

audit_transport_bindings() ->
  erlmcp:audit_transport_bindings().
