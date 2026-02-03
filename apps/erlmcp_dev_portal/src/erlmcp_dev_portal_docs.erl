-module(erlmcp_dev_portal_docs).

-behaviour(cowboy_handler).

%% API exports
-export([init/2, handle/2, terminate/3]).

%%====================================================================
%% API Functions
%%====================================================================

init(Req, _Opts) ->
    %% Initialize docs state
    State = #{},
    {ok, Req, State}.

handle(Req, State) ->
    %% Handle documentation requests
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    case Method of
        <<"GET">> ->
            handle_get_docs(Path, Req, State);
        _ ->
            cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

handle_get_docs(<<"/docs">>, Req, State) ->
    %% Return documentation home page
    {ok, Body} = render_docs_home(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);

handle_get_docs(<<"/docs/", ApiId/binary>>, Req, State) ->
    %% Return API documentation for specific API
    case erlmcp_api_management:get_api(binary_to_list(ApiId)) of
        {ok, Api} ->
            {ok, Body} = render_api_docs(Api),
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"API not found">>, Req)
    end;

handle_get_docs(<<"/docs/", ApiId/binary, "/", Version/binary>>, Req, State) ->
    %% Return API documentation for specific version
    case erlmcp_api_management:get_api_version(binary_to_list(ApiId), binary_to_list(Version)) of
        {ok, Api} ->
            {ok, Body} = render_api_docs(Api),
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"API version not found">>, Req)
    end;

handle_get_docs(Path, Req, State) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

render_docs_home() ->
    %% Render documentation home page HTML
    Html = <<
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>API Documentation - erlmcp</title></head>"
        "<body>"
        "<h1>API Documentation</h1>"
        "<div class='api-list'>"
        "<h2>Available APIs</h2>"
        "<div id='apis'></div>"
        "</div>"
        "<div class='search'>"
        "<h2>Search Documentation</h2>"
        "<input type='text' id='search' placeholder='Search APIs...'>"
        "<div id='search-results'></div>"
        "</div>"
        "<script>"
        "fetch('/apis/list')"
        "  .then(response => response.json())"
        "  .then(data => {"
        "    const apiList = document.getElementById('apis');"
        "    data.apis.forEach(api => {"
        "      const item = document.createElement('div');"
        "      item.className = 'api-item';"
        "      item.innerHTML = `"
        "<h3>${api.name}</h3>"
        "<p>${api.description}</p>"
        "<a href='/docs/${api.id}'>View Documentation</a>`;"
        "      apiList.appendChild(item);"
        "    });"
        "  });"
        ""
        "document.getElementById('search').addEventListener('input', function(e) {"
        "  const query = e.target.value.toLowerCase();"
        "  const items = document.querySelectorAll('.api-item');"
        "  items.forEach(item => {"
        "    const text = item.textContent.toLowerCase();"
        "    item.style.display = text.includes(query) ? '' : 'none';"
        "  });"
        "});"
        "</script>"
        "</body>"
        "</html>"
    >>,
    {ok, Html}.

render_api_docs(Api) ->
    %% Render API documentation HTML
    ApiId = Api#api_definition.id,
    Routes = erlmcp_api_management:list_routes(ApiId),

    %% Generate OpenAPI spec
    {ok, OpenAPI} = erlmcp_api_openapi:generate_openapi(ApiId),

    Html = list_to_binary([
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>", ApiId, " - API Documentation</title>"
        "<link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/swagger-ui/4.1.3/swagger-ui.min.css'>"
        "</head>"
        "<body>"
        "<h1>API Documentation - ", ApiId, "</h1>"
        "<div id='swagger-ui'></div>"
        "<script src='https://cdnjs.cloudflare.com/ajax/libs/swagger-ui/4.1.3/swagger-ui-bundle.min.js'></script>"
        "<script>"
        "const spec = ", jsx:encode(OpenAPI), ";"
        "SwaggerUIBundle({"
        "  url: '',"
        "  spec: spec,"
        "  dom_id: '#swagger-ui',"
        "  presets: ["
        "    SwaggerUIBundle.presets.apis,"
        "    SwaggerUIBundle.SwaggerUIStandalonePreset"
        "  ],"
        "  layout: 'StandaloneLayout',"
        "  deepLinking: true,"
        "  showExtensions: true,"
        "  showCommonExtensions: true"
        "});"
        "</script>"
        "</body>"
        "</html>"
    ]),
    {ok, Html}.