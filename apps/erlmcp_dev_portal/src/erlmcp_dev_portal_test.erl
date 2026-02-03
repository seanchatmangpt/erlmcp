-module(erlmcp_dev_portal_test).

-behaviour(cowboy_handler).

%% API exports
-export([init/2, handle/2, terminate/3]).

%%====================================================================
%% API Functions
%%====================================================================

init(Req, _Opts) ->
    %% Initialize test state
    State = #{},
    {ok, Req, State}.

handle(Req, State) ->
    %% Handle test requests
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    case Method of
        <<"GET">> ->
            handle_get_test(Path, Req, State);
        <<"POST">> ->
            handle_post_test(Path, Req, State);
        _ ->
            cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

handle_get_test(<<"/test">>, Req, State) ->
    %% Return test home page
    {ok, Body} = render_test_home(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);

handle_get_test(<<"/test/", ApiId/binary>>, Req, State) ->
    %% Return test page for specific API
    case erlmcp_api_management:get_api(binary_to_list(ApiId)) of
        {ok, Api} ->
            {ok, Body} = render_api_test_page(Api),
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"API not found">>, Req)
    end;

handle_get_test(<<"/test/run">>, Req, State) ->
    %% Run API tests
    Result = run_api_tests(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Result), Req);

handle_get_test(Path, Req, State) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

handle_post_test(<<"/test/run">>, Req, State) ->
    %% Execute custom API test
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    TestConfig = jsx:decode(Body, [{labels, atom}]),

    %% Execute test
    Result = execute_api_test(TestConfig),

    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Result), Req);

handle_post_test(Path, Req, State) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

render_test_home() ->
    %% Render test home page HTML
    Html = <<
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>API Testing - erlmcp</title></head>"
        "<body>"
        "<h1>API Testing</h1>"
        "<div class='api-selector'>"
        "<h2>Select API to Test</h2>"
        "<select id='api-select'>"
        "<option value=''>Select an API</option>"
        "</select>"
        "</div>"
        "<div class='test-runner' style='display: none;'>"
        "<h2>Test Runner</h2>"
        "<div id='test-configuration'></div>"
        "<button id='run-tests'>Run Tests</button>"
        "<div id='test-results'></div>"
        "</div>"
        "<div class='manual-test' style='display: none;'>"
        "<h2>Manual Test</h2>"
        "<div id='manual-test-form'></div>"
        "<div id='manual-results'></div>"
        "</div>"
        "<script>"
        "fetch('/apis/list')"
        "  .then(response => response.json())"
        "  .then(data => {"
        "    const select = document.getElementById('api-select');"
        "    data.apis.forEach(api => {"
        "      const option = document.createElement('option');"
        "      option.value = api.id;"
        "      option.textContent = api.name;"
        "      select.appendChild(option);"
        "    });"
        "  });"
        ""
        "document.getElementById('api-select').addEventListener('change', function() {"
        "  const apiId = this.value;"
        "  if (apiId) {"
        "    fetch(`/test/${apiId}`)"
        "      .then(response => response.text())"
        "      .then(html => {"
        "        document.querySelector('.test-runner').style.display = 'block';"
        "        document.querySelector('.manual-test').style.display = 'block';"
        "      });"
        "  } else {"
        "    document.querySelector('.test-runner').style.display = 'none';"
        "    document.querySelector('.manual-test').style.display = 'none';"
        "  }"
        "});"
        "</script>"
        "</body>"
        "</html>"
    >>,
    {ok, Html}.

render_api_test_page(Api) ->
    %% Render API test page HTML
    ApiId = Api#api_definition.id,
    Routes = erlmcp_api_management:list_routes(ApiId),

    %% Generate test configuration
    TestConfig = generate_test_config(Api, Routes),

    Html = list_to_binary([
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>API Testing - ", ApiId, "</title></head>"
        "<body>"
        "<h1>API Testing - ", ApiId, "</h1>"
        "<div class='test-info'>"
        "<h2>API Information</h2>"
        "<p><strong>Name:</strong> ", Api#api_definition.name, "</p>"
        "<p><strong>Version:</strong> ", Api#api_definition.version, "</p>"
        "<p><strong>Description:</strong> ", Api#api_definition.description, "</p>"
        "</div>"
        "<div class='test-routes'>"
        "<h2>Test Routes</h2>"
        "<div id='routes'></div>"
        "</div>"
        "<div class='test-results'>"
        "<h2>Test Results</h2>"
        "<div id='results'></div>"
        "</div>"
        "<script>"
        "const routes = ", jsx:encode(Routes), ";"
        "const routesDiv = document.getElementById('routes');"
        "routes.routes.forEach(route => {"
        "  const div = document.createElement('div');"
        "  div.className = 'route-test';"
        "  div.innerHTML = `"
        "<h3>${route.method} ${route.path}</h3>"
        "<p>${route.description}</p>"
        "<button onclick='testRoute(\"${route.method}\", \"${route.path}\")'>Test</button>`;"
        "  routesDiv.appendChild(div);"
        "});"
        ""
        "function testRoute(method, path) {"
        "  fetch('/test/run', {"
        "    method: 'POST',"
        "    headers: {'Content-Type': 'application/json'},"
        "    body: JSON.stringify({"
        "      api_id: '", ApiId, "',"
        "      method: method,"
        "      path: path"
        "    })"
        "  })"
        "    .then(response => response.json())"
        "    .then(result => {"
        "      const resultsDiv = document.getElementById('results');"
        "      const resultDiv = document.createElement('div');"
        "      resultDiv.className = 'result-item';"
        "      resultDiv.innerHTML = `<h4>${method} ${path}</h4><pre>${JSON.stringify(result, null, 2)}</pre>`;"
        "      resultsDiv.appendChild(resultDiv);"
        "    });"
        "}"
        "</script>"
        "</body>"
        "</html>"
    ]),
    {ok, Html}.

generate_test_config(Api, Routes) ->
    %% Generate test configuration for API
    #{
        api => Api,
        routes => Routes,
        tests => generate_tests(Routes),
        preconditions => generate_preconditions()
    }.

generate_tests(Routes) ->
    %% Generate test scenarios for each route
    lists:map(fun(Route) ->
        #{
            route => Route,
            scenarios => [
                #{
                    name => "Successful request",
                    request => build_request(Route),
                    expected => #{status => 200}
                },
                #{
                    name => "Invalid request",
                    request => build_invalid_request(Route),
                    expected => #{status => 400}
                }
            ]
        }
    end, Routes).

build_request(Route) ->
    %% Build valid test request
    #{
        method => Route#api_route.method,
        path => Route#api_route.path,
        headers => #{
            <<"content-type">> => <<"application/json">>,
            <<"x-api-key">> => <<"test-key">>
        }
    }.

build_invalid_request(Route) ->
    %% Build invalid test request
    #{
        method => Route#api_route.method,
        path => Route#api_route_path ++ "/invalid",
        headers => #{
            <<"content-type">> => <<"application/json">>
        }
    }.

generate_preconditions() ->
    %% Generate test preconditions
    [
        #{
            name => "API key exists",
            check => fun() -> check_api_key_exists() end
        },
        #{
            name => "Service available",
            check => fun() -> check_service_available() end
        }
    ].

run_api_tests() ->
    %% Run all API tests
    Result = execute_test_suite(),

    #{
        success => true,
        total_tests => maps:get(total, Result, 0),
        passed => maps:get(passed, Result, 0),
        failed => maps:get(failed, Result, 0),
        duration => maps:get(duration, Result, 0),
        details => Result
    }.

execute_test_suite() ->
    %% Execute complete test suite
    TestCases = get_all_test_cases(),

    Results = lists:foldl(fun(TestCase, Acc) ->
        case execute_test(TestCase) of
            {passed, _} -> Acc#{passed => maps:get(passed, Acc, 0) + 1};
            {failed, _} -> Acc#{failed => maps:get(failed, Acc, 0) + 1}
        end
    end, #{total => length(TestCases)}, TestCases),

    Results#{duration => get_execution_time()}.

execute_api_test(TestConfig) ->
    %% Execute custom API test
    ApiId = proplists:get_value(api_id, TestConfig),
    Method = proplists:get_value(method, TestConfig, <<"GET">>),
    Path = proplists:get_value(path, TestConfig),

    %% Execute test
    Result = erlmcp_client:call_tool(#{
        api_id => ApiId,
        method => Method,
        path => Path
    }),

    #{
        success => true,
        api_id => ApiId,
        method => Method,
        path => Path,
        result => Result
    }.