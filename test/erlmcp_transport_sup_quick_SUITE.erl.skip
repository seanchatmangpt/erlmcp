-module(erlmcp_transport_sup_quick_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([start_stop_stdio/1, restart_stdio/1]).
%% internal helpers
-export([wait_for_status/2]).

all() ->
  [start_stop_stdio, restart_stdio].

init_per_suite(Config) ->
  case whereis(erlmcp_transport_sup) of
    undefined ->
      {ok, _} = erlmcp_transport_sup:start_link();
    _ ->
      ok
  end,
  Config.

end_per_suite(_Config) ->
  ok.

start_stop_stdio(_Config) ->
  TId = stdio_sup_qw,
  {ok, Pid} = erlmcp_transport_sup:start_child(TId, stdio, #{test_mode => true}),
  ?assert(is_pid(Pid)),
  timer:sleep(100),
  ok = wait_for_status(TId, running),
  ok = erlmcp_transport_sup:stop_child(TId),
  ok = wait_for_status(TId, stopped).

restart_stdio(_Config) ->
  TId = stdio_sup_restart_qw,
  {ok, _} = erlmcp_transport_sup:start_child(TId, stdio, #{test_mode => true}),
  ok = wait_for_status(TId, running),
  {ok, NewPid} = erlmcp_transport_sup:restart_transport(TId),
  ?assert(is_pid(NewPid)),
  ok = erlmcp_transport_sup:stop_child(TId),
  ok = wait_for_status(TId, stopped).

wait_for_status(TransportId, Expected) ->
  wait_for_status(TransportId, Expected, 100).

wait_for_status(_TransportId, _Expected, 0) ->
  ct:fail("timeout waiting for expected status");
wait_for_status(TransportId, Expected, N) when N > 0 ->
  case erlmcp_transport_sup:get_child_status(TransportId) of
    {ok, Expected} ->
      ok;
    _ ->
      timer:sleep(50),
      wait_for_status(TransportId, Expected, N - 1)
  end.
