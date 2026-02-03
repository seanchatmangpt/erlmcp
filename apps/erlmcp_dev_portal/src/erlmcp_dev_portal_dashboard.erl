-module(erlmcp_dev_portal_dashboard).

-behaviour(cowboy_handler).

%% API exports
-export([init/2, handle/2, terminate/3]).

%%====================================================================
%% API Functions
%%====================================================================

init(Req, _Opts) ->
    %% Initialize dashboard state
    State = #{},
    {ok, Req, State}.

handle(Req, State) ->
    %% Handle dashboard requests
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    case Method of
        <<"GET">> ->
            handle_get_dashboard(Path, Req, State);
        _ ->
            cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

handle_get_dashboard(<<"/dashboard">>, Req, State) ->
    %% Return dashboard home page
    {ok, Body} = render_dashboard_home(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);

handle_get_dashboard(<<"/dashboard/keys">>, Req, State) ->
    %% Return API keys management page
    {ok, Body} = render_api_keys_page(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);

handle_get_dashboard(<<"/dashboard/apps">>, Req, State) ->
    %% Return applications management page
    {ok, Body} = render_apps_page(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);

handle_get_dashboard(Path, Req, State) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

render_dashboard_home() ->
    %% Render dashboard home page HTML
    Html = <<
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>Developer Dashboard - erlmcp</title></head>"
        "<body>"
        "<h1>Developer Dashboard</h1>"
        "<div class='stats'>"
        "<h2>API Statistics</h2>"
        "<div id='stats'></div>"
        "</div>"
        "<div class='recent-activity'>"
        "<h2>Recent Activity</h2>"
        "<div id='activity'></div>"
        "</div>"
        "<div class='quick-links'>"
        "<h2>Quick Links</h2>"
        "<ul>"
        "<li><a href='/dashboard/keys'>Manage API Keys</a></li>"
        "<li><a href='/dashboard/apps'>Applications</a></li>"
        "<li><a href='/docs'>API Documentation</a></li>"
        "<li><a href='/explorer'>API Explorer</a></li>"
        "</ul>"
        "</div>"
        "<script>"
        "fetch('/dashboard/stats')"
        "  .then(response => response.json())"
        "  .then(data => {"
        "    const stats = document.getElementById('stats');"
        "    stats.innerHTML = `"
        "<div class='stat-card'>"
        "<h3>Total APIs</h3>"
        "<div class='stat-value'>${data.total_apis}</div>"
        "</div>"
        "<div class='stat-card'>"
        "<h3>Total Calls</h3>"
        "<div class='stat-value'>${data.total_calls}</div>"
        "</div>"
        "<div class='stat-card'>"
        "<h3>Active Keys</h3>"
        "<div class='stat-value'>${data.active_keys}</div>"
        "</div>`;"
        "  });"
        ""
        "fetch('/dashboard/activity')"
        "  .then(response => response.json())"
        "  .then(data => {"
        "    const activity = document.getElementById('activity');"
        "    data.activities.forEach(activity => {"
        "      const item = document.createElement('div');"
        "      item.className = 'activity-item';"
        "      item.innerHTML = `<strong>${activity.type}</strong> - ${activity.description} (${activity.timestamp})`;"
        "      activity.appendChild(item);"
        "    });"
        "  });"
        "</script>"
        "</body>"
        "</html>"
    >>,
    {ok, Html}.

render_api_keys_page() ->
    %% Render API keys management page
    Html = <<
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>API Keys - erlmcp</title></head>"
        "<body>"
        "<h1>API Keys Management</h1>"
        "<div class='api-keys'>"
        "<h2>Your API Keys</h2>"
        "<div id='keys'></div>"
        "<button id='create-key'>Create New API Key</button>"
        "</div>"
        "<div class='key-template' style='display: none;'>"
        "<h3>API Key Created</h3>"
        "<div id='key-display'></div>"
        "<p>Store this key securely. It won't be shown again.</p>"
        "</div>"
        "<script>"
        "fetch('/dashboard/keys/list')"
        "  .then(response => response.json())"
        "  .then(keys => {"
        "    const keysDiv = document.getElementById('keys');"
        "    keys.forEach(key => {"
        "      const keyDiv = document.createElement('div');"
        "      keyDiv.className = 'key-item';"
        "      keyDiv.innerHTML = `"
        "<div class='key-info'>"
        "<strong>${key.name}</strong>"
        "<p>Created: ${key.created}</p>"
        "<p>Last Used: ${key.last_used || 'Never'}</p>"
        "<p>Usage: ${key.usage}/${key.limit}</p>"
        "</div>"
        "<div class='key-actions'>"
        "<button onclick='deleteKey(\"${key.id}\")'>Delete</button>"
        "</div>`;"
        "      keysDiv.appendChild(keyDiv);"
        "    });"
        "  });"
        ""
        "document.getElementById('create-key').addEventListener('click', function() {"
        "  fetch('/dashboard/keys/create', {"
        "    method: 'POST',"
        "    headers: {'Content-Type': 'application/json'},"
        "    body: JSON.stringify({name: 'New API Key'})"
        "  })"
        "    .then(response => response.json())"
        "    .then(key => {"
        "      const template = document.querySelector('.key-template');"
        "      document.getElementById('key-display').innerHTML = `"
        "<pre>${key.key}</pre>`;"
        "      template.style.display = 'block';"
        "    });"
        "});"
        ""
        "function deleteKey(keyId) {"
        "  if (confirm('Are you sure you want to delete this API key?')) {"
        "    fetch(`/dashboard/keys/${keyId}`, { method: 'DELETE' })"
        "      .then(response => response.json())"
        "      .then(result => {"
        "        if (result.success) {"
        "          location.reload();"
        "        }"
        "      });"
        "  }"
        "}"
        "</script>"
        "</body>"
        "</html>"
    >>,
    {ok, Html}.

render_apps_page() ->
    %% Render applications management page
    Html = <<
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>Applications - erlmcp</title></head>"
        "<body>"
        "<h1>Applications</h1>"
        "<div class='apps'>"
        "<h2>Your Applications</h2>"
        "<div id='apps'></div>"
        "<button id='create-app'>Create New Application</button>"
        "</div>"
        "<script>"
        "fetch('/dashboard/apps/list')"
        "  .then(response => response.json())"
        "  .then(apps => {"
        "    const appsDiv = document.getElementById('apps');"
        "    apps.forEach(app => {"
        "      const appDiv = document.createElement('div');"
        "      appDiv.className = 'app-item';"
        "      appDiv.innerHTML = `"
        "<div class='app-info'>"
        "<h3>${app.name}</h3>"
        "<p>Created: ${app.created}</p>"
        "<p>Status: ${app.status}</p>"
        "<p>API Keys: ${app.api_keys}</p>"
        "</div>"
        "<div class='app-actions'>"
        "<button onclick='viewApp(\"${app.id}\")'>View</button>"
        "<button onclick='deleteApp(\"${app.id}\")'>Delete</button>"
        "</div>`;"
        "      appsDiv.appendChild(appDiv);"
        "    });"
        "  });"
        ""
        "document.getElementById('create-app').addEventListener('click', function() {"
        "  const name = prompt('Enter application name:');"
        "  if (name) {"
        "    fetch('/dashboard/apps/create', {"
        "      method: 'POST',"
        "      headers: {'Content-Type': 'application/json'},"
        "      body: JSON.stringify({name: name})"
        "    })"
        "      .then(response => response.json())"
        "      .then(app => {"
        "        location.reload();"
        "      });"
        "  }"
        "});"
        ""
        "function viewApp(appId) {"
        "  window.location.href = `/dashboard/apps/${appId}`;"
        "}"
        ""
        "function deleteApp(appId) {"
        "  if (confirm('Are you sure you want to delete this application?')) {"
        "    fetch(`/dashboard/apps/${appId}`, { method: 'DELETE' })"
        "      .then(response => response.json())"
        "      .then(result => {"
        "        if (result.success) {"
        "          location.reload();"
        "        }"
        "      });"
        "  }"
        "}"
        "</script>"
        "</body>"
        "</html>"
    >>,
    {ok, Html}.