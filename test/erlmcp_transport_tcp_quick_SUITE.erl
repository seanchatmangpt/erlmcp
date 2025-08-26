-module(erlmcp_transport_tcp_quick_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([tcp_start_test_mode/1, tcp_info_shape/1, tcp_transport_calls/1]).

all() ->
  [tcp_start_test_mode, tcp_info_shape, tcp_transport_calls].

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

%% Start TCP in test_mode and ensure pid
tcp_start_test_mode(_Config) ->
  TId = tcp_qw_1,
  {ok, Pid} =
    erlmcp_transport_sup:start_child(TId,
                                     tcp,
                                     #{host => "127.0.0.1",
                                       port => 65535,
                                       test_mode => true}),
  ?assert(is_pid(Pid)),
  ok = erlmcp_transport_sup:stop_child(TId).

%% Verify get_info shape in test mode
tcp_info_shape(_Config) ->
  TId = tcp_qw_2,
  {ok, Pid} =
    erlmcp_transport_sup:start_child(TId,
                                     tcp,
                                     #{host => "127.0.0.1",
                                       port => 65535,
                                       test_mode => true}),
  Info = erlmcp_transport_tcp:get_info(Pid),
  ?assertEqual(tcp, maps:get(type, Info, undefined)),
  ?assert(maps:is_key(status, Info)),
  ?assert(maps:is_key(statistics, Info)),
  ok = erlmcp_transport_sup:stop_child(TId).

%% Exercise transport calls
tcp_transport_calls(_Config) ->
  TId = tcp_qw_3,
  {ok, Pid} =
    erlmcp_transport_sup:start_child(TId,
                                     tcp,
                                     #{host => "127.0.0.1",
                                       port => 65535,
                                       test_mode => true}),
  {reply, {ok, _Sock}, _} = gen_server:call(Pid, {transport_call, get_socket}),
  {reply, {ok, Conn}, _} = gen_server:call(Pid, {transport_call, get_connection}),
  ?assert(maps:is_key(host, Conn)),
  {reply, {ok, Stats}, _} = gen_server:call(Pid, {transport_call, get_stats}),
  ?assert(maps:is_key(messages_sent, Stats)),
  {reply, ok, _} = gen_server:call(Pid, {transport_call, reconnect}),
  ok = erlmcp_transport_sup:stop_child(TId).
