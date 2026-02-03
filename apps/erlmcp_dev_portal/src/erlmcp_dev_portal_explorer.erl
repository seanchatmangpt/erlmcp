-module(erlmcp_dev_portal_explorer).

-behaviour(cowboy_handler).

%% API exports
-export([init/2, handle/2, terminate/3]).

%%====================================================================
%% API Functions
%%====================================================================

init(Req, _Opts) ->
    %% Initialize explorer state
    State = #{},
    {ok, Req, State}.

handle(Req, State) ->
    %% Handle API explorer requests
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    case Method of
        <<"GET">> ->
            handle_get_explorer(Path, Req, State);
        <<"POST">> ->
            handle_post_explorer(Path, Req, State);
        _ ->
            cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

handle_get_explorer(<<"/explorer">>, Req, State) ->
    %% Return explorer home page
    {ok, Body} = render_explorer_home(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);

handle_get_explorer(<<"/explorer/", ApiId/binary>>, Req, State) ->
    %% Return API explorer for specific API
    case erlmcp_api_management:get_api(binary_to_list(ApiId)) of
        {ok, Api} ->
            {ok, Body} = render_api_explorer(Api),
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"API not found">>, Req)
    end;

handle_get_explorer(Path, Req, State) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

handle_post_explorer(<<"/explorer/test">>, Req, State) ->
    %% Handle API test execution
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    TestParams = jsx:decode(Body, [{labels, atom}]),

    %% Execute API test
    Result = execute_api_test(TestParams),

    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Result), Req);

handle_post_explorer(Path, Req, State) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

render_explorer_home() ->
    %% Render explorer home page HTML
    Html = <<
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>API Explorer - erlmcp</title></head>"
        "<body>"
        "<h1>API Explorer</h1>"
        "<div class='api-list'>"
        "<h2>Available APIs</h2>"
        "<div id='apis'></div>"
        "</div>"
        "<div class='explorer'>"
        "<h2>API Explorer</h2>"
        "<select id='api-select'>"
        "<option value=''>Select an API</option>"
        "</select>"
        "<div id='api-docs'></div>"
        "</div>"
        "<script>"
        "fetch('/apis/list')"
        "  .then(response => response.json())"
        "  .then(data => {"
        "    const apiList = document.getElementById('apis');"
        "    const apiSelect = document.getElementById('api-select');"
        "    data.apis.forEach(api => {"
        "      const item = document.createElement('div');"
        "      item.innerHTML = `<h3>${api.name}</h3><p>${api.description}</p>`;"
        "      apiList.appendChild(item);"
        "      "
        "      const option = document.createElement('option');"
        "      option.value = api.id;"
        "      option.textContent = api.name;"
        "      apiSelect.appendChild(option);"
        "    });"
        "  });"
        ""
        "document.getElementById('api-select').addEventListener('change', function() {"
        "  const apiId = this.value;"
        "  if (apiId) {"
        "    fetch(`/explorer/${apiId}`)"
        "      .then(response => response.text())"
        "      .then(html => {"
        "        document.getElementById('api-docs').innerHTML = html;"
        "      });"
        "  }"
        "});"
        "</script>"
        "</body>"
        "</html>"
    >>,
    {ok, Html}.

render_api_explorer(Api) ->
    %% Render API explorer HTML
    ApiId = Api#api_definition.id,
    Html = list_to_binary([
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>", ApiId, " - API Explorer</title></head>"
        "<body>"
        "<h1>API Explorer - ", ApiId, "</h1>"
        "<div class='api-info'>"
        "<h2>API Information</h2>"
        "<p><strong>Name:</strong> ", Api#api_definition.name, "</p>"
        "<p><strong>Version:</strong> ", Api#api_definition.version, "</p>"
        "<p><strong>Description:</strong> ", Api#api_definition.description, "</p>"
        "</div>"
        "<div class='endpoints'>"
        "<h2>Endpoints</h2>"
        "<div id='endpoints'></div>"
        "</div>"
        "<div class='tester'>"
        "<h2>API Tester</h2>"
        "<div id='tester'></div>"
        "</div>"
        "<script>"
        "fetch(`/apis/", ApiId, "/routes`)"
        "  .then(response => response.json())"
        "  .then(routes => {"
        "    const endpoints = document.getElementById('endpoints');"
        "    routes.routes.forEach(route => {"
        "      const div = document.createElement('div');"
        "      div.className = 'endpoint';"
        "      div.innerHTML = `<h3>${route.method} ${route.path}</h3>"
        "<p>${route.description}</p>`;"
        "      endpoints.appendChild(div);"
        "    });"
        "  });"
        ""
        "document.getElementById('tester').innerHTML = `"
        "<form id='test-form'>"
        "<select id='method'>"
        "<option value='GET'>GET</option>"
        "<option value='POST'>POST</option>"
        "<option value='PUT'>PUT</option>"
        "<option value='DELETE'>DELETE</option>"
        "</select>"
        "<input type='text' id='path' placeholder='Path'>"
        "<button type='submit'>Test API</button>"
        "</form>"
        "<div id='response'></div>`;"
        ""
        "document.getElementById('test-form').addEventListener('submit', function(e) {"
        "  e.preventDefault();"
        "  const method = document.getElementById('method').value;"
        "  const path = document.getElementById('path').value;"
        "  "
        "  fetch('/explorer/test', {"
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
        "      document.getElementById('response').innerHTML = "
        "`<pre>${JSON.stringify(result, null, 2)}</pre>`;"
        "    });"
        "});"
        "</script>"
        "</body>"
        "</html>"
    ]),
    {ok, Html}.

execute_api_test(TestParams) ->
    %% Execute API test
    ApiId = proplists:get_value(api_id, TestParams),
    Method = proplists:get_value(method, TestParams, <<"GET">>),
    Path = proplists:get_value(path, TestParams),

    %% Build test request
    TestReq = build_test_request(ApiId, Method, Path),

    %% Execute test
    Result = erlmcp_client:call_tool(TestReq),

    %% Format result
    #{
        success => true,
        api_id => ApiId,
        method => Method,
        path => Path,
        result => Result
    }.

build_test_request(ApiId, Method, Path) ->
    %% Build test request
    #{
        <<"api_id">> => ApiId,
        <<"method">> => Method,
        <<"path">> => Path,
        <<"headers">> => #{
            <<"content-type">> => <<"application/json">>,
            <<"x-api-key">> => <<"test-api-key">>
        }
    }.