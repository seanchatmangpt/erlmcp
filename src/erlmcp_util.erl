-module(erlmcp_util).

-export([create_transport_id/2, get_process_status/1, list_supported_transport_types/0,
         get_config_examples/0]).

create_transport_id(ServerId, TypeBin) ->
  erlmcp:create_transport_id(ServerId, TypeBin).

get_process_status(Pid) ->
  erlmcp:get_process_status(Pid).

list_supported_transport_types() ->
  erlmcp:list_supported_transport_types().

get_config_examples() ->
  erlmcp:get_config_examples().
