-module(erlmcp_siem_supervisor).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init(term()) -> {ok, {supervisor:supervisor_specs(), [supervisor:child_spec()]}} | ignore.
init(_Args) ->
    %% SIEM Integration child processes
    Children = [
        %% Splunk Integration
        {splunk_forwarder,
         {erlmcp_siem_splunk, start_link, []},
         permanent, 5000, worker,
         [erlmcp_siem_splunk]},

        %% QRadar Integration
        {qradar_integration,
         {erlmcp_siem_qradar, start_link, []},
         permanent, 5000, worker,
         [erlmcp_siem_qradar]},

        %% ArcSight Integration
        {arcsight_integration,
         {erlmcp_siem_arcsight, start_link, []},
         permanent, 5000, worker,
         [erlmcp_siem_arcsight]},

        %% Microsoft Sentinel Integration
        {sentinel_integration,
         {erlmcp_siem_sentinel, start_link, []},
         permanent, 5000, worker,
         [erlmcp_siem_sentinel]},

        %% Generic SIEM Forwarder
        {siem_forwarder,
         {erlmcp_siem_generic, start_link, []},
         permanent, 5000, worker,
         [erlmcp_siem_generic]},

        %% SIEM Connection Pool
        {siem_connection_pool,
         {erlmcp_siem_pool, start_link, []},
         permanent, 5000, worker,
         [erlmcp_siem_pool]},

        %% SIEM Configuration Manager
        {siem_config_manager,
         {erlmcp_siem_config, start_link, []},
         permanent, 5000, worker,
         [erlmcp_siem_config]},

        %% SIEM Event Formatter
        {siem_event_formatter,
         {erlmcp_siem_formatter, start_link, []},
         permanent, 5000, worker,
         [erlmcp_siem_formatter]}
    ],

    {ok, {{one_for_one, 10, 3600}, Children}}.