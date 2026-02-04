-module(erlmcp_mcp_proxy_relay_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup
%%%===================================================================

setup() ->
    {ok, Pid} = erlmcp_mcp_proxy_relay:start_link(),
    Pid.

cleanup(_Pid) ->
    ok.

%%%===================================================================
%%% Test Generators
%%%===================================================================

mcp_proxy_relay_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun add_upstream_test/0,
      fun remove_upstream_test/0,
      fun list_upstreams_test/0,
      fun forward_request_test/0,
      fun get_stats_test/0
     ]}.

%%%===================================================================
%%% Individual Tests
%%%===================================================================

add_upstream_test() ->
    {"add_upstream adds new upstream", fun() ->
        ?assertEqual(ok, erlmcp_mcp_proxy_relay:add_upstream(test1, <<"http://localhost:8080">>)),
        ?assertEqual(ok, erlmcp_mcp_proxy_relay:add_upstream(test2, <<"http://example.com">>))
    end}.

remove_upstream_test() ->
    {"remove_upstream removes existing upstream", fun() ->
        ok = erlmcp_mcp_proxy_relay:add_upstream(temp, <<"http://temp.com">>),
        ?assertEqual(ok, erlmcp_mcp_proxy_relay:remove_upstream(temp))
    end}.

list_upstreams_test() ->
    {"list_upstreams returns all upstreams", fun() ->
        ok = erlmcp_mcp_proxy_relay:add_upstream(list_test1, <<"http://test1.com">>),
        Upstreams = erlmcp_mcp_proxy_relay:list_upstreams(),
        ?assert(is_list(Upstreams)),
        ?assert(length(Upstreams) > 0)
    end}.

forward_request_test() ->
    {"forward_request forwards to upstream", fun() ->
        ok = erlmcp_mcp_proxy_relay:add_upstream(forward_test, <<"http://forward.com">>),
        Request = #{jsonrpc => <<"2.0">>, id => 1, method => <<"test_method">>},
        Result = erlmcp_mcp_proxy_relay:forward_request(Request),
        ?assertMatch({ok, _}, Result)
    end}.

get_stats_test() ->
    {"get_stats returns statistics", fun() ->
        Stats = erlmcp_mcp_proxy_relay:get_stats(),
        ?assert(is_map(Stats)),
        ?assert(maps:is_key(forwarded, Stats)),
        ?assert(maps:is_key(errors, Stats))
    end}.
