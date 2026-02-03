-module(erlmcp_dev_portal_home).

-behaviour(cowboy_handler).

%% API exports
-export([init/2, handle/2, terminate/3]).

%%====================================================================
%% API Functions
%%====================================================================

init(Req, _Opts) ->
    %% Initialize home state
    State = #{},
    {ok, Req, State}.

handle(Req, State) ->
    %% Handle home page requests
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    case Method of
        <<"GET">> ->
            handle_get_home(Path, Req, State);
        _ ->
            cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

handle_get_home(<<"/">>, Req, State) ->
    %% Return home page
    {ok, Body} = render_home_page(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);

handle_get_home(<<"/index.html">>, Req, State) ->
    %% Return home page (alternative path)
    {ok, Body} = render_home_page(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);

handle_get_home(Path, Req, State) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

render_home_page() ->
    %% Render home page HTML
    Html = <<
        "<!DOCTYPE html>"
        "<html>"
        "<head>"
        "<title>erlmcp Developer Portal</title>"
        "<style>"
        "body {"
        "  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;"
        "  margin: 0;"
        "  padding: 0;"
        "  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);"
        "  min-height: 100vh;"
        "}"
        ".container {"
        "  max-width: 1200px;"
        "  margin: 0 auto;"
        "  padding: 20px;"
        "}"
        ".header {"
        "  text-align: center;"
        "  color: white;"
        "  padding: 40px 0;"
        "}"
        ".header h1 {"
        "  font-size: 3rem;"
        "  margin: 0;"
        "  text-shadow: 2px 2px 4px rgba(0,0,0,0.3);"
        "}"
        ".header p {"
        "  font-size: 1.2rem;"
        "  margin: 10px 0 0 0;"
        "  opacity: 0.9;"
        "}"
        .portal-grid {"
        "  display: grid;"
        "  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));"
        "  gap: 20px;"
        "  margin-top: 40px;"
        "}"
        .portal-card {"
        "  background: white;"
        "  border-radius: 10px;"
        "  padding: 30px;"
        "  box-shadow: 0 4px 6px rgba(0,0,0,0.1);"
        "  transition: transform 0.2s;"
        "}"
        .portal-card:hover {"
        "  transform: translateY(-5px);"
        "}"
        .portal-card h2 {"
        "  color: #333;"
        "  margin: 0 0 10px 0;"
        "}"
        .portal-card p {"
        "  color: #666;"
        "  margin: 0 0 20px 0;"
        "  line-height: 1.6;"
        "}"
        .portal-card a {"
        "  display: inline-block;"
        "  background: #667eea;"
        "  color: white;"
        "  padding: 10px 20px;"
        "  border-radius: 5px;"
        "  text-decoration: none;"
        "  font-weight: bold;"
        "}"
        .portal-card a:hover {"
        "  background: #5a67d8;"
        "}"
        .stats {"
        "  display: grid;"
        "  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));"
        "  gap: 20px;"
        "  margin: 40px 0;"
        "}"
        .stat-card {"
        "  background: rgba(255,255,255,0.1);"
        "  backdrop-filter: blur(10px);"
        "  border-radius: 10px;"
        "  padding: 20px;"
        "  text-align: center;"
        "  color: white;"
        "}"
        .stat-card h3 {"
        "  font-size: 2rem;"
        "  margin: 0 0 10px 0;"
        "}"
        .stat-card p {"
        "  margin: 0;"
        "  opacity: 0.9;"
        "}"
        .footer {"
        "  text-align: center;"
        "  color: white;"
        "  padding: 40px 0;"
        "  opacity: 0.8;"
        "}"
        "@media (max-width: 768px) {"
        "  .header h1 {"
        "    font-size: 2rem;"
        "  }"
        "  .portal-grid {"
        "    grid-template-columns: 1fr;"
        "  }"
        "}"
        "</style>"
        "</head>"
        "<body>"
        "<div class='container'>"
        "<div class='header'>"
        "<h1>erlmcp Developer Portal</h1>"
        "<p>Your gateway to enterprise API management and development</p>"
        "</div>"

        "<div class='stats'>"
        "<div class='stat-card'>"
        "<h3>1,234</h3>"
        "<p>Active APIs</p>"
        "</div>"
        "<div class='stat-card'>"
        "<h3>56K</h3>"
        <p>Daily Requests</p>"
        "</div>"
        "<div class='stat-card'>"
        "<h3>98%</h3>"
        "<p>Uptime</p>"
        "</div>"
        "<div class='stat-card'>"
        "<h3>24/7</h3>"
        "<p>Support</p>"
        "</div>"
        "</div>"

        "<div class='portal-grid'>"
        "<div class='portal-card'>"
        "<h2>📚 API Documentation</h2>"
        "<p>Browse comprehensive documentation for all APIs, including tutorials, guides, and reference materials.</p>"
        "<a href='/docs'>Explore Docs</a>"
        "</div>"

        "<div class='portal-card'>"
        "<h2>🔍 API Explorer</h2>"
        <p>Interactive API explorer to test endpoints, view schemas, and understand API behavior.</p>"
        "<a href='/explorer'>Start Exploring</a>"
        "</div>"

        "<div class='portal-card'>"
        "<h2>📊 Developer Dashboard</h2>"
        "<p>Monitor your API usage, manage keys, and track performance from your personal dashboard.</p>"
        "<a href='/dashboard'>Open Dashboard</a>"
        "</div>"

        "<div class='portal-card'>"
        "<h2>🧪 API Testing</h2>"
        "<p>Test your APIs with our comprehensive testing suite. Create custom tests and validate responses.</p>"
        "<a href='/test'>Start Testing</a>"
        "</div>"

        "<div class='portal-card'>"
        "<h2>💬 Community</h2>"
        <p>Join our developer community, ask questions, share knowledge, and connect with other developers.</p>"
        "<a href='/community'>Join Community</a>"
        "</div>"

        "<div class='portal-card'>"
        "<h2>🛟 Support</h2>"
        <p>Get help when you need it. Create support tickets, browse documentation, or contact our team.</p>"
        "<a href='/support'>Get Support</a>"
        "</div>"

        "<div class='portal-card'>"
        "<h2>📈 Analytics</h2>"
        <p>Dive deep into API usage analytics, performance metrics, and usage patterns.</p>"
        "<a href='/analytics'>View Analytics</a>"
        "</div>"

        "<div class='portal-card'>"
        "<h2>🔐 Authentication</h2>"
        <p>Manage your account, login, or register for a new developer account.</p>"
        "<a href='/login'>Sign In / Sign Up</a>"
        "</div>"
        "</div>"

        "<div class='footer'>"
        "<p>© 2026 erlmcp - Enterprise API Management Platform</p>"
        "<p>Powered by Erlang/OTP | Built for Scale and Reliability</p>"
        "</div>"
        "</div>"

        "<script>"
        "// Track page view"
        "fetch('/analytics/pageview', {"
        "  method: 'POST',"
        "  headers: {'Content-Type': 'application/json'},"
        "  body: JSON.stringify({"
        "    page: 'home',"
        "    timestamp: Date.now()"
        "  })"
        "}).catch(() => {});"

        "// Load portal stats"
        "fetch('/portal/stats')"
        "  .then(response => response.json())"
        "  .then(stats => {"
        "    document.querySelectorAll('.stat-card h3').forEach((el, index) => {"
        "      if (stats[index]) {"
        "        el.textContent = stats[index].value;"
        "      }"
        "    });"
        "  });"
        "</script>"
        "</body>"
        "</html>"
    >>,
    {ok, Html}.