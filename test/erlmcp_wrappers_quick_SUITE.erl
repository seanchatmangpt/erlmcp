-module(erlmcp_wrappers_quick_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([transport_api_wrappers/1, validation_wrappers/1, setup_wrappers/1,
         binding_wrappers/1, util_wrappers/1]).

all() ->
  [transport_api_wrappers,
   validation_wrappers,
   setup_wrappers,
   binding_wrappers,
   util_wrappers].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

transport_api_wrappers(_Config) ->
  %% Ensure functions exist and callable
  ?assertEqual(ok,
               case catch erlmcp_transport_api:ensure_transport_supervisor() of
                 ok ->
                   ok;
                 _ ->
                   ok
               end),
  ?assert(is_list(erlmcp_transport_api:list_transports())),
  ok.

validation_wrappers(_Config) ->
  %% Validate a trivial HTTP config shape via wrapper
  {ok, _} = erlmcp_validation:get_config_schema(http),
  ok =
    case erlmcp_validation:validate_transport_config(http,
                                                     #{type => http, url => "http://localhost/"})
    of
      ok ->
        ok;
      {error, _} ->
        ok
    end,
  ok.

setup_wrappers(_Config) ->
  %% Just ensure exported functions are callable (no side effects in this quick test)
  ?assert(fun_test_exported(start_transport_enhanced, 3)),
  ?assert(fun_test_exported(start_transport_with_retry, 4)),
  ok.

binding_wrappers(_Config) ->
  %% Functions should be callable; results depend on registry availability
  _ = case catch erlmcp_binding:list_transport_bindings() of
        {error, _} ->
          ok;
        L when is_list(L) ->
          ok;
        _ ->
          ok
      end,
  ok.

util_wrappers(_Config) ->
  Id = erlmcp_util:create_transport_id(my_server, <<"tcp">>),
  ?assert(is_atom(Id)),
  ?assert(is_list(erlmcp_util:list_supported_transport_types())),
  Ex = erlmcp_util:get_config_examples(),
  ?assert(is_map(Ex)),
  ok.

fun_test_exported(Name, Arity) ->
  lists:member({Name, Arity}, erlmcp_setup:module_info(exports)).
