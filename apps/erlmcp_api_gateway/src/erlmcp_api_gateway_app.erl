-module(erlmcp_api_gateway_app).
-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).

start(_Type, _Args) ->
    %% Initialize API gateway configuration
    case erlmcp_api_gateway_config:init() of
        ok ->
            %% Cowboy HTTP router for API gateway endpoints
            Dispatch = cowboy_router:compile([
                {'_', [
                    %% API Management endpoints
                    {"/apis", erlmcp_api_gateway_routes, []},
                    {"/apis/:api_id", erlmcp_api_gateway_routes, []},
                    {"/apis/:api_id/routes", erlmcp_api_gateway_routes, []},
                    {"/apis/:api_id/routes/:route_id", erlmcp_api_gateway_routes, []},
                    {"/apis/:api_id/versions", erlmcp_api_gateway_routes, []},
                    {"/apis/:api_id/versions/:version", erlmcp_api_gateway_routes, []},

                    %% Consumer Management
                    {"/consumers", erlmcp_api_gateway_routes, []},
                    {"/consumers/:consumer_id", erlmcp_api_gateway_routes, []},
                    {"/consumers/:consumer_id/keys", erlmcp_api_gateway_routes, []},
                    {"/consumers/:consumer_id/apps", erlmcp_api_gateway_routes, []},

                    %% Security & Authentication
                    {"/auth", erlmcp_api_gateway_auth, []},
                    {"/auth/login", erlmcp_api_gateway_auth, []},
                    {"/auth/validate", erlmcp_api_gateway_auth, []},
                    {"/auth/refresh", erlmcp_api_gateway_auth, []},

                    %% Plugin Management
                    {"/plugins", erlmcp_api_gateway_routes, []},
                    {"/plugins/:plugin_id", erlmcp_api_gateway_routes, []},
                    {"/plugins/:plugin_id/config", erlmcp_api_gateway_routes, []},

                    %% Rate Limiting
                    {"/ratelimit", erlmcp_api_gateway_ratelimit, []},
                    {"/ratelimit/:policy_id", erlmcp_api_gateway_ratelimit, []},

                    %% Analytics & Monitoring
                    {"/analytics", erlmcp_api_gateway_analytics, []},
                    {"/analytics/apis", erlmcp_api_gateway_analytics, []},
                    {"/analytics/consumers", erlmcp_api_gateway_analytics, []},
                    {"/analytics/health", erlmcp_api_gateway_analytics, []},

                    %% Developer Portal
                    {"/docs", erlmcp_api_gateway_docs, []},
                    {"/docs/:api_id", erlmcp_api_gateway_docs, []},
                    {"/docs/:api_id/:version", erlmcp_api_gateway_docs, []},

                    %% Health checks
                    {"/health", erlmcp_api_gateway_health, []},
                    {"/health/details", erlmcp_api_gateway_health, []},
                    {"/ready", erlmcp_api_gateway_health, []},
                    {"/readyz", erlmcp_api_gateway_health, []}
                ]}
            ]),

            %% Start HTTP server with enterprise-grade configuration
            {ok, _} = cowboy:start_clear(
                erlmcp_api_gateway,
                [{port, 8080}, {max_connections, 10000}, {num_acceptors, 100}],
                #{env => #{dispatch => Dispatch}},
                erlmcp_api_gateway_protocol
            ),

            %% Start API gateway supervisor
            erlmcp_api_gateway_sup:start_link();
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

prep_stop(_State) ->
    %% Perform graceful shutdown of API gateway components
    erlmcp_api_gateway:graceful_shutdown(),
    ok.