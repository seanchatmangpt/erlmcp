-module(erlmcp_quick_wins_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([stdio_exports/1, stdio_info_shape/1, stdio_registry_registration/1]).

all() ->
  [stdio_exports, stdio_info_shape, stdio_registry_registration].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

stdio_exports(_Config) ->
  Exports = erlmcp_transport_stdio_new:module_info(exports),
  ?assert(lists:member({start_link, 2}, Exports)),
  ?assert(lists:member({send, 2}, Exports)),
  ?assert(lists:member({close, 1}, Exports)),
  ?assert(lists:member({get_info, 1}, Exports)).

stdio_info_shape(_Config) ->
  {ok, Pid} = erlmcp_transport_stdio_new:start_link(stdio_qw, #{test_mode => true}),
  Info = erlmcp_transport_stdio_new:get_info(Pid),
  ?assertEqual(stdio, maps:get(type, Info, undefined)),
  ?assert(maps:is_key(version, Info)),
  ?assert(maps:is_key(capabilities, Info)),
  ?assert(maps:is_key(connection_state, Info)),
  ?assert(maps:is_key(statistics, Info)),
  ok = erlmcp_transport_stdio_new:close(Pid).

stdio_registry_registration(_Config) ->
  case whereis(erlmcp_registry) of
    undefined ->
      {ok, _} = erlmcp_registry:start_link();
    _ ->
      ok
  end,
  {ok, Pid} = erlmcp_transport_stdio_new:start_link(stdio_reg_qw, #{test_mode => true}),
  timer:sleep(100),
  case erlmcp_registry:find_transport(stdio_reg_qw) of
    {ok, {RegPid, RegConfig}} ->
      ?assertEqual(Pid, RegPid),
      ?assert(maps:is_key(type, RegConfig));
    {error, not_found} ->
      ct:fail("Transport not registered in registry")
  end,
  ok = erlmcp_transport_stdio_new:close(Pid),
  ok.
