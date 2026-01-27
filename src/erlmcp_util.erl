-module(erlmcp_util).

-export([create_transport_id/2, get_process_status/1, list_supported_transport_types/0,
         get_config_examples/0]).

-spec create_transport_id(binary(), binary()) -> {ok, binary()} | {error, term()}.
create_transport_id(ServerId, TypeBin) ->
  erlmcp:create_transport_id(ServerId, TypeBin).

-spec get_process_status(pid()) -> {ok, atom()} | {error, term()}.
get_process_status(Pid) ->
  erlmcp:get_process_status(Pid).

-spec list_supported_transport_types() -> [atom()].
list_supported_transport_types() ->
  erlmcp:list_supported_transport_types().

-spec get_config_examples() -> map().
get_config_examples() ->
  erlmcp:get_config_examples().
