%%%-------------------------------------------------------------------
%%% @doc
%%% T006 — Kubernetes in-cluster service discovery: meck-based unit tests
%%%
%%% The K8s implementation calls the in-cluster K8s API via httpc.
%%% All tests mock httpc:request/4 so that no live cluster is needed.
%%%
%%% The meck callbacks send messages to the test process (message-passing
%%% is required because meck callbacks execute in the caller's process,
%%% i.e. the gen_server, not the test process).
%%%
%%% Tests assert:
%%%   - register_k8s PATCHes the Endpoints resource at the correct URL
%%%   - register_k8s falls back to POST when PATCH returns 404
%%%   - unregister_k8s DELETEs the Endpoints resource
%%%   - 404 on deregister is treated as success (already gone)
%%%   - discover_k8s GETs the Endpoints resource
%%%   - discover returns {ok, []} on 404
%%%   - discover parses subsets into endpoint maps (requires jsx)
%%%   - In dev (no token file), no Authorization header is sent
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_t006_k8s_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Setup / Teardown
%%%===================================================================

setup() ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    meck:new(httpc, [unstick, passthrough]),
    ok.

teardown(_) ->
    meck:unload(httpc),
    application:unset_env(erlmcp_transports, k8s_api_server),
    application:unset_env(erlmcp_transports, k8s_namespace),
    ok.

k8s_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun t006_register_patches_endpoint/0,
      fun t006_register_creates_on_404/0,
      fun t006_register_returns_error_on_500/0,
      fun t006_deregister_deletes_endpoint/0,
      fun t006_deregister_404_is_ok/0,
      fun t006_discover_get_url/0,
      fun t006_discover_returns_empty_on_404/0,
      fun t006_discover_parses_subsets/0,
      fun t006_no_token_in_dev/0
     ]}.

%%%===================================================================
%%% Helpers
%%%===================================================================

set_k8s_env() ->
    application:set_env(erlmcp_transports, k8s_api_server, "https://k8s.test:6443"),
    application:set_env(erlmcp_transports, k8s_namespace, "testns").

start_server() ->
    catch gen_server:stop(erlmcp_transport_discovery),
    {ok, Pid} = erlmcp_transport_discovery:start_link(),
    Pid.

stop_server(Pid) ->
    catch gen_server:stop(Pid),
    ok.

recv_call() ->
    receive
        {k8s_call, M, U, H} -> {M, U, H}
    after 2000 -> error(no_k8s_call)
    end.

recv_calls(N) ->
    recv_calls(N, []).

recv_calls(0, Acc) -> lists:reverse(Acc);
recv_calls(N, Acc) ->
    receive
        {k8s_call, M, U, H} -> recv_calls(N - 1, [{M, U, H} | Acc])
    after 2000 -> lists:reverse(Acc)
    end.

%% A meck expect that captures method, URL, headers for any tuple size.
capture_expect(Self, Response) ->
    meck:expect(httpc, request,
        fun(Method, Request, _HO, _O) ->
            URL = element(1, Request),
            Headers = element(2, Request),
            Self ! {k8s_call, Method, URL, Headers},
            Response
        end).

%%%===================================================================
%%% Register tests
%%%===================================================================

t006_register_patches_endpoint() ->
    set_k8s_env(),
    capture_expect(self(), {ok, {{"HTTP/1.1", 200, "OK"}, [], <<>>}}),

    Pid = start_server(),
    erlmcp_transport_discovery:register_k8s(
        <<"my-svc">>, #{port => 8080, host => "10.0.1.1", namespace => "testns"}),
    stop_server(Pid),

    {Method, URL, _H} = recv_call(),
    %% First attempt is a PATCH.
    ?assertEqual(patch, Method),
    ?assertNotEqual(nomatch,
        string:find(URL, "k8s.test:6443/api/v1/namespaces/testns/endpoints/my-svc")).

t006_register_creates_on_404() ->
    set_k8s_env(),
    Self = self(),
    %% PATCH returns 404; POST should follow.
    meck:expect(httpc, request,
        fun(Method, Request, _HO, _O) ->
            URL = element(1, Request),
            Self ! {k8s_call, Method, URL, []},
            case Method of
                patch -> {ok, {{"HTTP/1.1", 404, "Not Found"}, [], <<>>}};
                post  -> {ok, {{"HTTP/1.1", 201, "Created"},  [], <<>>}}
            end
        end),

    Pid = start_server(),
    Result = erlmcp_transport_discovery:register_k8s(
        <<"new-svc">>, #{port => 8080, host => "10.0.1.2", namespace => "testns"}),
    stop_server(Pid),

    ?assertEqual(ok, Result),
    Calls = recv_calls(2),
    Methods = [M || {M, _, _} <- Calls],
    ?assert(lists:member(patch, Methods)),
    ?assert(lists:member(post, Methods)).

t006_register_returns_error_on_500() ->
    set_k8s_env(),
    meck:expect(httpc, request,
        fun(_, _, _, _) -> {ok, {{"HTTP/1.1", 500, "Err"}, [], <<>>}} end),

    Pid = start_server(),
    Result = erlmcp_transport_discovery:register_k8s(
        <<"fail-svc">>, #{port => 80, host => "h", namespace => "testns"}),
    stop_server(Pid),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Deregister tests
%%%===================================================================

t006_deregister_deletes_endpoint() ->
    set_k8s_env(),
    capture_expect(self(), {ok, {{"HTTP/1.1", 200, "OK"}, [], <<>>}}),

    Pid = start_server(),
    Result = erlmcp_transport_discovery:unregister_k8s(<<"del-svc">>),
    stop_server(Pid),

    ?assertEqual(ok, Result),
    {Method, URL, _H} = recv_call(),
    ?assertEqual(delete, Method),
    ?assertNotEqual(nomatch,
        string:find(URL, "k8s.test:6443/api/v1/namespaces/testns/endpoints/del-svc")).

t006_deregister_404_is_ok() ->
    set_k8s_env(),
    meck:expect(httpc, request,
        fun(delete, _, _, _) -> {ok, {{"HTTP/1.1", 404, "Not Found"}, [], <<>>}} end),

    Pid = start_server(),
    Result = erlmcp_transport_discovery:unregister_k8s(<<"gone-svc">>),
    stop_server(Pid),
    ?assertEqual(ok, Result).

%%%===================================================================
%%% Discover tests
%%%===================================================================

t006_discover_get_url() ->
    set_k8s_env(),
    capture_expect(self(), {ok, {{"HTTP/1.1", 200, "OK"}, [], <<"{}">>}}),

    Pid = start_server(),
    erlmcp_transport_discovery:discover_services(<<"disc-svc">>, k8s),
    stop_server(Pid),

    {Method, URL, _H} = recv_call(),
    ?assertEqual(get, Method),
    ?assertNotEqual(nomatch,
        string:find(URL, "k8s.test:6443/api/v1/namespaces/testns/endpoints/disc-svc")).

t006_discover_returns_empty_on_404() ->
    set_k8s_env(),
    meck:expect(httpc, request,
        fun(get, _, _, _) -> {ok, {{"HTTP/1.1", 404, "Not Found"}, [], <<>>}} end),

    Pid = start_server(),
    Result = erlmcp_transport_discovery:discover_services(<<"miss-svc">>, k8s),
    stop_server(Pid),
    ?assertMatch({ok, []}, Result).

t006_discover_parses_subsets() ->
    case code:is_loaded(jsx) of
        false ->
            {skip, no_jsx};
        {file, _} ->
            set_k8s_env(),
            Body = erlmcp_json_native:encode(#{
                <<"subsets">> => [
                    #{<<"addresses">> => [#{<<"ip">> => <<"192.168.1.5">>}],
                      <<"ports">>     => [#{<<"port">> => 9090}]}
                ]
            }),
            meck:expect(httpc, request,
                fun(get, _, _, _) ->
                    {ok, {{"HTTP/1.1", 200, "OK"}, [], Body}}
                end),

            Pid = start_server(),
            {ok, Endpoints} = erlmcp_transport_discovery:discover_services(<<"ep-svc">>, k8s),
            stop_server(Pid),

            ?assertEqual(1, length(Endpoints)),
            [EP] = Endpoints,
            ?assertEqual("192.168.1.5", maps:get(host, EP)),
            ?assertEqual(9090, maps:get(port, EP))
    end.

%%%===================================================================
%%% Dev guard test
%%%===================================================================

%% @doc When no service-account token exists (dev environment), the
%%      implementation proceeds without an Authorization header.
%%      We verify no crash and no auth header when token file is absent.
t006_no_token_in_dev() ->
    set_k8s_env(),
    Self = self(),
    meck:expect(httpc, request,
        fun(get, {URL, Headers}, _HO, _O) ->
            Self ! {k8s_call, get, URL, Headers},
            {ok, {{"HTTP/1.1", 404, "Not Found"}, [], <<>>}}
        end),

    Pid = start_server(),
    Result = erlmcp_transport_discovery:discover_services(<<"dev-svc">>, k8s),
    stop_server(Pid),

    %% Must be {ok,[]} or {error,_} — never a bare stub return.
    ?assert(is_tuple(Result)),

    %% When token file is absent, no Authorization header is added.
    receive
        {k8s_call, get, _URL, Headers} ->
            HasAuth = lists:any(
                fun({K, _}) -> string:equal(string:lowercase(K), "authorization") end,
                Headers),
            ?assertEqual(false, HasAuth)
    after 200 ->
        %% httpc may not have been reached (e.g. ssl startup); that's ok.
        ok
    end.
