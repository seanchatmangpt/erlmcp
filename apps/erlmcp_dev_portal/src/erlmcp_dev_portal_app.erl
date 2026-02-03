-module(erlmcp_dev_portal_app).

-behaviour(application).

%% Application callbacks
-export([start/2]).

%%====================================================================
%% API functions
%%====================================================================

start(_Type, _Args) ->
    %% Initialize developer portal configuration
    case erlmcp_dev_portal_config:init() of
        ok ->
            %% Cowboy HTTP router for developer portal endpoints
            Dispatch = cowboy_router:compile([
                {'_', [
                    %% Portal Home
                    {"/", erlmcp_dev_portal_home, []},
                    {"/index.html", erlmcp_dev_portal_home, []},

                    %% API Documentation
                    {"/docs", erlmcp_dev_portal_docs, []},
                    {"/docs/:api_id", erlmcp_dev_portal_docs, []},
                    {"/docs/:api_id/:version", erlmcp_dev_portal_docs, []},
                    {"/docs/:api_id/:version/:endpoint", erlmcp_dev_portal_docs, []},

                    %% API Explorer
                    {"/explorer", erlmcp_dev_portal_explorer, []},
                    {"/explorer/:api_id", erlmcp_dev_portal_explorer, []},

                    %% Developer Dashboard
                    {"/dashboard", erlmcp_dev_portal_dashboard, []},
                    {"/dashboard/keys", erlmcp_dev_portal_dashboard, []},
                    {"/dashboard/apps", erlmcp_dev_portal_dashboard, []},

                    %% API Testing
                    {"/test", erlmcp_dev_portal_test, []},
                    {"/test/:api_id", erlmcp_dev_portal_test, []},
                    {"/test/run", erlmcp_dev_portal_test, []},

                    %% Authentication
                    {"/login", erlmcp_dev_portal_auth, []},
                    {"/register", erlmcp_dev_portal_auth, []},
                    {"/logout", erlmcp_dev_portal_auth, []},

                    %% Developer Community
                    {"/community", erlmcp_dev_portal_community, []},
                    {"/community/forums", erlmcp_dev_portal_community, []},
                    {"/community/tutorials", erlmcp_dev_portal_community, []},

                    %% Support
                    {"/support", erlmcp_dev_portal_support, []},
                    {"/support/tickets", erlmcp_dev_portal_support, []},
                    {"/support/docs", erlmcp_dev_portal_support, []},

                    %% Static Assets
                    {"/assets/*path", cowboy_static, {
                        root, "/Users/sac/erlmcp/apps/erlmcp_dev_portal/assets"
                    }},

                    %% Health checks
                    {"/health", erlmcp_dev_portal_health, []}
                ]}
            ]),

            %% Start HTTP server with developer portal configuration
            {ok, _} = cowboy:start_clear(
                erlmcp_dev_portal,
                [{port, 3000}],
                #{env => #{dispatch => Dispatch}},
                erlmcp_dev_portal_protocol
            ),

            %% Start developer portal supervisor
            erlmcp_dev_portal_sup:start_link();
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

prep_stop(_State) ->
    %% Perform graceful shutdown of developer portal components
    erlmcp_dev_portal:graceful_shutdown(),
    ok.