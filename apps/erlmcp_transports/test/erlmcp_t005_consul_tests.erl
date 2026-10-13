%%%-------------------------------------------------------------------
%%% @doc
%%% T005 — Consul service discovery: meck-based unit tests
%%%
%%% All three Consul operations are tested by mocking httpc:request
%%% so that no live Consul agent is required.
%%%
%%% The meck callbacks send messages back to the test process so that
%%% captured URLs and methods are visible across process boundaries.
%%%
%%% Tests assert:
%%%   - Correct URL construction (host/port from application env)
%%%   - Correct HTTP method per operation (PUT register, PUT deregister,
%%%     GET catalog/health)
%%%   - Correct JSON payload structure for register
%%%   - Correct parsing of a Consul catalog response into endpoints
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_t005_consul_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Setup / Teardown
%%%===================================================================

setup() ->
    application:ensure_all_started(inets),
    meck:new(httpc, [unstick, passthrough]),
    ok.

teardown(_) ->
    meck:unload(httpc),
    application:unset_env(erlmcp_transports, consul_host),
    application:unset_env(erlmcp_transports, consul_port),
    ok.

consul_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun t005_register_url/0,
      fun t005_register_method_is_put/0,
      fun t005_register_payload_contains_name/0,
      fun t005_register_returns_ok_on_200/0,
      fun t005_register_returns_error_on_500/0,
      fun t005_deregister_url/0,
      fun t005_deregister_returns_ok_on_200/0,
      fun t005_discover_url/0,
      fun t005_discover_parses_empty_list/0,
      fun t005_discover_parses_single_service/0
     ]}.

%%%===================================================================
%%% Helpers
%%%===================================================================

set_consul_env() ->
    application:set_env(erlmcp_transports, consul_host, "consul.test"),
    application:set_env(erlmcp_transports, consul_port, 8501).

start_server() ->
    catch gen_server:stop(erlmcp_transport_discovery),
    {ok, Pid} = erlmcp_transport_discovery:start_link(),
    Pid.

stop_server(Pid) ->
    catch gen_server:stop(Pid),
    ok.

%% Receive one httpc_call message from the meck mock within 2 s.
recv_call() ->
    receive
        {httpc_call, Method, URL, Body} -> {Method, URL, Body}
    after 2000 -> error(no_httpc_call)
    end.

%% Receive one httpc_get message (no body in GET requests).
recv_get() ->
    receive
        {httpc_get, URL} -> URL
    after 2000 -> error(no_httpc_get)
    end.

%%%===================================================================
%%% Register tests
%%%===================================================================

t005_register_url() ->
    set_consul_env(),
    Self = self(),
    meck:expect(httpc, request,
        fun(Method, {URL, _H, _CT, Body}, _HO, _O) ->
            Self ! {httpc_call, Method, URL, Body},
            {ok, {{"HTTP/1.1", 200, "OK"}, [], <<>>}}
        end),

    Pid = start_server(),
    erlmcp_transport_discovery:register_consul(<<"url-svc">>, #{port => 9090, host => "10.0.0.5"}),
    stop_server(Pid),

    {_Method, URL, _Body} = recv_call(),
    ?assertNotEqual(nomatch,
        string:find(URL, "consul.test:8501/v1/agent/service/register")).

t005_register_method_is_put() ->
    Self = self(),
    meck:expect(httpc, request,
        fun(Method, {URL, _H, _CT, Body}, _HO, _O) ->
            Self ! {httpc_call, Method, URL, Body},
            {ok, {{"HTTP/1.1", 200, "OK"}, [], <<>>}}
        end),

    Pid = start_server(),
    erlmcp_transport_discovery:register_consul(<<"method-svc">>, #{port => 80, host => "h"}),
    stop_server(Pid),

    {Method, _URL, _Body} = recv_call(),
    ?assertEqual(put, Method).

t005_register_payload_contains_name() ->
    Self = self(),
    meck:expect(httpc, request,
        fun(Method, {URL, _H, _CT, Body}, _HO, _O) ->
            Self ! {httpc_call, Method, URL, Body},
            {ok, {{"HTTP/1.1", 200, "OK"}, [], <<>>}}
        end),

    SvcName = <<"payload-svc">>,
    Pid = start_server(),
    erlmcp_transport_discovery:register_consul(SvcName, #{port => 80, host => "h"}),
    stop_server(Pid),

    {_Method, _URL, Body} = recv_call(),
    ?assert(is_binary(Body) andalso byte_size(Body) > 0),
    ?assertNotEqual(nomatch, binary:match(Body, SvcName)).

t005_register_returns_ok_on_200() ->
    meck:expect(httpc, request,
        fun(put, _, _, _) -> {ok, {{"HTTP/1.1", 200, "OK"}, [], <<>>}} end),

    Pid = start_server(),
    Result = erlmcp_transport_discovery:register_consul(<<"ok-svc">>, #{port => 80, host => "h"}),
    stop_server(Pid),
    ?assertEqual(ok, Result).

t005_register_returns_error_on_500() ->
    meck:expect(httpc, request,
        fun(put, _, _, _) -> {ok, {{"HTTP/1.1", 500, "Err"}, [], <<>>}} end),

    Pid = start_server(),
    Result = erlmcp_transport_discovery:register_consul(<<"err-svc">>, #{port => 80, host => "h"}),
    stop_server(Pid),
    ?assertMatch({error, {consul_http_error, 500}}, Result).

%%%===================================================================
%%% Deregister tests
%%%===================================================================

t005_deregister_url() ->
    set_consul_env(),
    Self = self(),
    meck:expect(httpc, request,
        fun(Method, {URL, _H, _CT, _Body}, _HO, _O) ->
            Self ! {httpc_call, Method, URL, <<>>},
            {ok, {{"HTTP/1.1", 200, "OK"}, [], <<>>}}
        end),

    Pid = start_server(),
    erlmcp_transport_discovery:unregister_consul(<<"my-svc">>),
    stop_server(Pid),

    {_Method, URL, _} = recv_call(),
    ?assertNotEqual(nomatch,
        string:find(URL, "consul.test:8501/v1/agent/service/deregister/my-svc")).

t005_deregister_returns_ok_on_200() ->
    meck:expect(httpc, request,
        fun(put, _, _, _) -> {ok, {{"HTTP/1.1", 200, "OK"}, [], <<>>}} end),

    Pid = start_server(),
    Result = erlmcp_transport_discovery:unregister_consul(<<"dereg-svc">>),
    stop_server(Pid),
    ?assertEqual(ok, Result).

%%%===================================================================
%%% Discover tests
%%%===================================================================

t005_discover_url() ->
    set_consul_env(),
    Self = self(),
    meck:expect(httpc, request,
        fun(get, {URL, _H}, _HO, _O) ->
            Self ! {httpc_get, URL},
            {ok, {{"HTTP/1.1", 200, "OK"}, [], <<"[]">>}}
        end),

    Pid = start_server(),
    erlmcp_transport_discovery:discover_services(<<"my-svc">>, consul),
    stop_server(Pid),

    URL = recv_get(),
    %% URL must reference the health/service endpoint.
    ?assertNotEqual(nomatch,
        string:find(URL, "consul.test:8501/v1/health/service/my-svc")).

t005_discover_parses_empty_list() ->
    meck:expect(httpc, request,
        fun(get, _, _, _) -> {ok, {{"HTTP/1.1", 200, "OK"}, [], <<"[]">>}} end),

    Pid = start_server(),
    Result = erlmcp_transport_discovery:discover_services(<<"empty-svc">>, consul),
    stop_server(Pid),
    ?assertMatch({ok, []}, Result).

t005_discover_parses_single_service() ->
    case code:is_loaded(jsx) of
        false ->
            {skip, no_jsx};
        {file, _} ->
            Body = jsx:encode([
                #{<<"Service">> => #{
                    <<"Address">> => <<"10.1.2.3">>,
                    <<"Port">> => 4242
                }}
            ]),
            meck:expect(httpc, request,
                fun(get, _, _, _) ->
                    {ok, {{"HTTP/1.1", 200, "OK"}, [], Body}}
                end),

            Pid = start_server(),
            {ok, Endpoints} = erlmcp_transport_discovery:discover_services(<<"one-svc">>, consul),
            stop_server(Pid),

            ?assertEqual(1, length(Endpoints)),
            [EP] = Endpoints,
            ?assertEqual("10.1.2.3", maps:get(host, EP)),
            ?assertEqual(4242, maps:get(port, EP))
    end.
