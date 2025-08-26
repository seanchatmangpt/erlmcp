-module(erlmcp_validation).

-export([validate_transport_config/1, validate_transport_config/2, get_config_schema/1,
         initialize/0]).

validate_transport_config(Config) ->
  erlmcp:validate_transport_config(Config).

validate_transport_config(Type, Config) ->
  erlmcp:validate_transport_config(Type, Config).

get_config_schema(Type) ->
  erlmcp:get_config_schema(Type).

initialize() ->
  erlmcp:initialize_config_validation().
