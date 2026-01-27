-module(erlmcp_validation).

-export([validate_transport_config/1, validate_transport_config/2, get_config_schema/1,
         initialize/0]).

-spec validate_transport_config(map()) -> ok | {error, term()}.
validate_transport_config(Config) ->
  erlmcp:validate_transport_config(Config).

-spec validate_transport_config(atom(), map()) -> ok | {error, term()}.
validate_transport_config(Type, Config) ->
  erlmcp:validate_transport_config(Type, Config).

-spec get_config_schema(atom()) -> {ok, map()} | {error, atom()}.
get_config_schema(Type) ->
  erlmcp:get_config_schema(Type).

-spec initialize() -> ok.
initialize() ->
  erlmcp:initialize_config_validation().
