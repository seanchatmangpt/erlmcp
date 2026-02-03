-module(erlmcp_dev_portal).

%% Public API
-export([start_link/0, stop/0, graceful_shutdown/0]).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    %% Start the dev portal application
    erlmcp_dev_portal_app:start_link().

stop() ->
    %% Stop the dev portal application
    application:stop(erlmcp_dev_portal).

graceful_shutdown() ->
    %% Perform graceful shutdown
    erlmcp_dev_portal_health:check_overall_health(),
    erlmcp_dev_portal_config:reload_config(),
    erlmcp_dev_portal_assets:cleanup_temp_files(),
    ok.