-module(erlmcp_transport_http_quick_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([http_start_test_mode/1, http_info_shape/1, http_transport_calls/1]).

all() ->
  [http_start_test_mode, http_info_shape, http_transport_calls].

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

%% Start HTTP in test_mode and ensure pid
http_start_test_mode(_Config) ->
  TId = http_qw_1,
  {ok, Pid} =
    erlmcp_transport_sup:start_child(TId,
                                     http,
                                     #{url => "http://localhost/", test_mode => true}),
  ?assert(is_pid(Pid)),
  ok = erlmcp_transport_sup:stop_child(TId).

%% Verify get_info shape in test mode
http_info_shape(_Config) ->
  TId = http_qw_2,
  {ok, Pid} =
    erlmcp_transport_sup:start_child(TId,
                                     http,
                                     #{url => "http://localhost/", test_mode => true}),
  Info = erlmcp_transport_http:get_info(Pid),
  ?assertEqual(http, maps:get(type, Info, undefined)),
  ?assert(maps:is_key(status, Info)),
  ?assert(maps:is_key(statistics, Info)),
  ok = erlmcp_transport_sup:stop_child(TId).

%% Exercise transport calls
http_transport_calls(_Config) ->
  TId = http_qw_3,
  {ok, Pid} =
    erlmcp_transport_sup:start_child(TId,
                                     http,
                                     #{url => "http://localhost/", test_mode => true}),
  {ok, Url} = gen_server:call(Pid, {transport_call, get_url}),
  ?assertEqual("http://localhost/", Url),
  {error, unknown_request} = gen_server:call(Pid, {transport_call, unknown}),
  ok = erlmcp_transport_sup:stop_child(TId).
