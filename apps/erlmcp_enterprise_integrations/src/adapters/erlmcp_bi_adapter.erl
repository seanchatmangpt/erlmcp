%% @doc Business Intelligence Adapter
%% Integrates with BI tools (Tableau, Power BI)
-module(erlmcp_bi_adapter).

-behaviour(gen_server).

-export([start_link/0, publish_dataset/3, create_dashboard/3, generate_report/3,
         schedule_refresh/3, get_permissions/2, grant_permissions/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type provider() :: tableau | powerbi.
-type dataset() :: map().
-type dashboard() :: map().
-type report() :: map().
-type refresh_schedule() :: map().

-record(state, {
    provider :: provider(),
    config :: map(),
    connection :: pid() | undefined,
    workspace_id :: binary(),
    datasets :: map(),
    dashboards :: map(),
    metrics :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Publish a dataset to the BI platform
-spec publish_dataset(binary(), dataset(), map()) -> {ok, binary()} | {error, term()}.
publish_dataset(DatasetName, Data, Context) ->
    gen_server:call(?MODULE, {publish_dataset, DatasetName, Data, Context}).

%% Create a new dashboard
-spec create_dashboard(binary(), map(), map()) -> {ok, binary()} | {error, term()}.
create_dashboard(DashboardName, Config, Context) ->
    gen_server:call(?MODULE, {create_dashboard, DashboardName, Config, Context}).

%% Generate a report
-spec generate_report(binary(), binary(), map()) -> {ok, binary()} | {error, term()}.
generate_report(ReportName, Dataset, Context) ->
    gen_server:call(?MODULE, {generate_report, ReportName, Dataset, Context}).

%% Schedule a refresh for a dataset
-spec schedule_refresh(binary(), binary(), refresh_schedule()) -> {ok, binary()} | {error, term()}.
schedule_refresh(DatasetName, DatasetId, Schedule) ->
    gen_server:call(?MODULE, {schedule_refresh, DatasetName, DatasetId, Schedule}).

%% Get permissions for a dataset/dashboard
-spec get_permissions(binary(), binary()) -> {ok, map()} | {error, term()}.
get_permissions(ResourceId, ResourceType) ->
    gen_server:call(?MODULE, {get_permissions, ResourceId, ResourceType}).

%% Grant permissions to users/groups
-spec grant_permissions(binary(), binary(), map()) -> {ok, binary()} | {error, term()}.
grant_permissions(ResourceId, ResourceType, Permissions) ->
    gen_server:call(?MODULE, {grant_permissions, ResourceId, ResourceType, Permissions}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize state with configuration
    Config = load_config(),
    Provider = maps:get(provider, Config, undefined),

    State = #state{
        provider = Provider,
        config = Config,
        connection = undefined,
        workspace_id = maps:get(workspace_id, Config, <<"default">>),
        datasets = #{},
        dashboards = #{},
        metrics = #{}
    },

    %% Initialize provider-specific connection
    case Provider of
        tableau ->
            tableau_connection:start(State);
        powerbi ->
            powerbi_connection:start(State);
        _ ->
            throw({invalid_provider, Provider})
    end,

    {ok, State}.

handle_call({publish_dataset, DatasetName, Data, Context}, _From, State) ->
    try
        %% Publish dataset based on provider
        case State#state.provider of
            tableau -> publish_tableau_dataset(DatasetName, Data, Context, State);
            powerbi -> publish_powerbi_dataset(DatasetName, Data, Context, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to publish dataset: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({create_dashboard, DashboardName, Config, Context}, _From, State) ->
    try
        %% Create dashboard based on provider
        case State#state.provider of
            tableau -> create_tableau_dashboard(DashboardName, Config, Context, State);
            powerbi -> create_powerbi_dashboard(DashboardName, Config, Context, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to create dashboard: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({generate_report, ReportName, Dataset, Context}, _From, State) ->
    try
        %% Generate report based on provider
        case State#state.provider of
            tableau -> generate_tableau_report(ReportName, Dataset, Context, State);
            powerbi -> generate_powerbi_report(ReportName, Dataset, Context, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to generate report: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({schedule_refresh, DatasetName, DatasetId, Schedule}, _From, State) ->
    try
        %% Schedule refresh based on provider
        case State#state.provider of
            tableau -> schedule_tableau_refresh(DatasetName, DatasetId, Schedule, State);
            powerbi -> schedule_powerbi_refresh(DatasetName, DatasetId, Schedule, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to schedule refresh: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({get_permissions, ResourceId, ResourceType}, _From, State) ->
    try
        %% Get permissions based on provider
        case State#state.provider of
            tableau -> get_tableau_permissions(ResourceId, ResourceType, State);
            powerbi -> get_powerbi_permissions(ResourceId, ResourceType, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to get permissions: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({grant_permissions, ResourceId, ResourceType, Permissions}, _From, State) ->
    try
        %% Grant permissions based on provider
        case State#state.provider of
            tableau -> grant_tableau_permissions(ResourceId, ResourceType, Permissions, State);
            powerbi -> grant_powerbi_permissions(ResourceId, ResourceType, Permissions, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to grant permissions: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Clean up resources
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

load_config() ->
    %% Load BI provider configuration
    case application:get_env(erlmcp_enterprise_integrations, bi_config) of
        undefined -> default_bi_config();
        {ok, Config} -> Config
    end.

default_bi_config() ->
    #{
        provider => tableau,
        endpoint => "https://your-site.tableau.com",
        api_version => "3.12",
        site_url => "default",
        username => undefined,
        password => undefined,
        personal_access_token => undefined,
        timeout => 30000
    }.

%% Tableau Integration
publish_tableau_dataset(DatasetName, Data, Context, State) ->
    %% In a real implementation, this would extract data from the provided source
    %% and publish it to Tableau Server/Online
    Endpoint = "https://" ++ binary_to_list(maps:get(site_url, State#state.config)) ++
               ".tableau.com/api/" ++
               maps:get(api_version, State#state.config) ++
               "/sites/" ++ binary_to_list(maps:get(workspace_id, State#state.config)) ++
               "/datasources",
    Headers = tableau_headers(State),
    DataSource = tableau_data_source_config(DatasetName, Data, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(DataSource)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            DataSourceId = proplists:get_value(<<"id">>, Response),
            UpdatedState = State#state{datasets = maps:put(DatasetName, DataSourceId, State#state.datasets)},
            {ok, DataSourceId, UpdatedState};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {dataset_publish_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

create_tableau_dashboard(DashboardName, Config, Context, State) ->
    Endpoint = "https://" ++ binary_to_list(maps:get(site_url, State#state.config)) ++
               ".tableau.com/api/" ++
               maps:get(api_version, State#state.config) ++
               "/sites/" ++ binary_to_list(maps:get(workspace_id, State#state.config)) ++
               "/workbooks",
    Headers = tableau_headers(State),
    Workbook = tableau_workbook_config(DashboardName, Config, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(Workbook)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            WorkbookId = proplists:get_value(<<"id">>, Response),
            UpdatedState = State#state{dashboards = maps:put(DashboardName, WorkbookId, State#state.dashboards)},
            {ok, WorkbookId, UpdatedState};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {dashboard_create_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

generate_tableau_report(ReportName, Dataset, Context, State) ->
    %% Create a new view/worksheet from the dataset
    Endpoint = "https://" ++ binary_to_list(maps:get(site_url, State#state.config)) ++
               ".tableau.com/api/" ++
               maps:get(api_version, State#state.config) ++
               "/sites/" ++ binary_to_list(maps:get(workspace_id, State#state.config)) ++
               "/views",
    Headers = tableau_headers(State),
    View = tableau_view_config(ReportName, Dataset, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(View)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            ViewId = proplists:get_value(<<"id">>, Response),
            {ok, ViewId};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {report_generation_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

schedule_tableau_refresh(DatasetName, DatasetId, Schedule, State) ->
    Endpoint = "https://" ++ binary_to_list(maps:get(site_url, State#state.config)) ++
               ".tableau.com/api/" ++
               maps:get(api_version, State#state.config) ++
               "/sites/" ++ binary_to_list(maps:get(workspace_id, State#state.config)) ++
               "/datasources/" ++ binary_to_list(DatasetId) ++
               "/refreshes",
    Headers = tableau_headers(State),
    RefreshJob = tableau_refresh_job_config(Schedule, State),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(RefreshJob)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            JobId = proplists:get_value(<<"id">>, Response),
            {ok, JobId};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {refresh_schedule_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

get_tableau_permissions(ResourceId, ResourceType, State) ->
    Endpoint = "https://" ++ binary_to_list(maps:get(site_url, State#state.config)) ++
               ".tableau.com/api/" ++
               maps:get(api_version, State#state.config) ++
               "/sites/" ++ binary_to_list(maps:get(workspace_id, State#state.config)) ++
               "/" ++ binary_to_list(ResourceType) ++
               "/" ++ binary_to_list(ResourceId) ++
               "/permissions",
    Headers = tableau_headers(State),

    case httpc:request(get, {Endpoint, Headers},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(ResponseBody, [{labels, binary}]};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {permissions_get_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

grant_tableau_permissions(ResourceId, ResourceType, Permissions, State) ->
    Endpoint = "https://" ++ binary_to_list(maps:get(site_url, State#state.config)) ++
               ".tableau.com/api/" ++
               maps:get(api_version, State#state.config) ++
               "/sites/" ++ binary_to_list(maps:get(workspace_id, State#state.config)) ++
               "/" ++ binary_to_list(ResourceType) ++
               "/" ++ binary_to_list(ResourceId) ++
               "/permissions",
    Headers = tableau_headers(State),
    PermissionUpdate = tableau_permission_update(Permissions, State),

    case httpc:request(put, {Endpoint, Headers, "application/json", jsx:encode(PermissionUpdate)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, <<"permissions_updated">>};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {permissions_grant_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

%% Power BI Integration
publish_powerbi_dataset(DatasetName, Data, Context, State) ->
    %% In a real implementation, this would extract data from the provided source
    %% and publish it to Power BI service
    Endpoint = "https://api.powerbi.com/v1.0/myorg/groups/" ++
               binary_to_list(maps:get(workspace_id, State#state.config)) ++
               "/datasets",
    Headers = powerbi_headers(State),
    Dataset = powerbi_dataset_config(DatasetName, Data, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(Dataset)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            DatasetId = proplists:get_value(<<"id">>, Response),
            UpdatedState = State#state{datasets = maps:put(DatasetName, DatasetId, State#state.datasets)},
            {ok, DatasetId, UpdatedState};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {dataset_publish_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

create_powerbi_dashboard(DashboardName, Config, Context, State) ->
    Endpoint = "https://api.powerbi.com/v1.0/myorg/groups/" ++
               binary_to_list(maps:get(workspace_id, State#state.config)) ++
               "/dashboards",
    Headers = powerbi_headers(State),
    Dashboard = powerbi_dashboard_config(DashboardName, Config, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(Dashboard)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            DashboardId = proplists:get_value(<<"id">>, Response),
            UpdatedState = State#state{dashboards = maps:put(DashboardName, DashboardId, State#state.dashboards)},
            {ok, DashboardId, UpdatedState};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {dashboard_create_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

generate_powerbi_report(ReportName, Dataset, Context, State) ->
    %% Create a new report (PBIX file) from the dataset
    Endpoint = "https://api.powerbi.com/v1.0/myorg/groups/" ++
               binary_to_list(maps:get(workspace_id, State#state.config)) ++
               "/reports",
    Headers = powerbi_headers(State),
    ReportConfig = powerbi_report_config(ReportName, Dataset, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(ReportConfig)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            ReportId = proplists:get_value(<<"id">>, Response),
            {ok, ReportId};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {report_generation_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

schedule_powerbi_refresh(DatasetName, DatasetId, Schedule, State) ->
    Endpoint = "https://api.powerbi.com/v1.0/myorg/groups/" ++
               binary_to_list(maps:get(workspace_id, State#state.config)) ++
               "/datasets/" ++ binary_to_list(DatasetId) ++
               "/refreshes",
    Headers = powerbi_headers(State),
    RefreshSchedule = powerbi_refresh_schedule(Schedule, State),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(RefreshSchedule)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 202, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            JobId = proplists:get_value(<<"id">>, Response),
            {ok, JobId};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {refresh_schedule_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

get_powerbi_permissions(ResourceId, ResourceType, State) ->
    Endpoint = "https://api.powerbi.com/v1.0/myorg/groups/" ++
               binary_to_list(maps:get(workspace_id, State#state.config)) ++
               "/" ++ binary_to_list(ResourceType) ++
               "/" ++ binary_to_list(ResourceId) ++
               "/permissions",
    Headers = powerbi_headers(State),

    case httpc:request(get, {Endpoint, Headers},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(ResponseBody, [{labels, binary}]};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {permissions_get_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

grant_powerbi_permissions(ResourceId, ResourceType, Permissions, State) ->
    Endpoint = "https://api.powerbi.com/v1.0/myorg/groups/" ++
               binary_to_list(maps:get(workspace_id, State#state.config)) ++
               "/" ++ binary_to_list(ResourceType) ++
               "/" ++ binary_to_list(ResourceId) ++
               "/permissions",
    Headers = powerbi_headers(State),
    PermissionUpdate = powerbi_permission_update(Permissions, State),

    case httpc:request(put, {Endpoint, Headers, "application/json", jsx:encode(PermissionUpdate)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, <<"permissions_updated">>};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {permissions_grant_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

%% Helper Functions
tableau_headers(State) =>
    case maps:get(personal_access_token, State#state.config) of
        undefined ->
            Auth = base64:encode(<<(maps:get(username, State#state.config))/binary,
                                   ":",
                                   (maps:get(password, State#state.config))/binary>>),
            #{<<"X-Tableau-Auth">> => Auth,
              <<"Content-Type">> => <<"application/json">>};
        Token ->
            #{<<"X-Tableau-Auth">> => Token,
              <<"Content-Type">> => <<"application/json">>}
    end.

tableau_data_source_config(DatasetName, Data, Context) =>
    #{
        datasource => #{
            name => DatasetName,
            description => proplists:get_value(description, Context, "erlmcp dataset"),
            serverUrl => proplists:get_value(server_url, Context, "http://localhost"),
            username => proplists:get_value(username, Context, undefined),
            password => proplists:get_value(password, Context, undefined),
            embeddedCredentials => proplists:get_value(embedded_credentials, Context, true)
        }
    }.

tableau_workbook_config(DashboardName, Config, Context) =>
    #{
        workbook => #{
            name => DashboardName,
            description => proplists:get_value(description, Context, "erlmcp dashboard"),
            connections => proplists:get_value(connections, Context, []),
            datasources => proplists:get_value(datasources, Context, []),
            views => proplists:get_value(views, Context, [])
        }
    }.

tableau_view_config(ReportName, Dataset, Context) =>
    #{
        view => #{
            name => ReportName,
            dataSource => Dataset,
            worksheetType => proplists:get_value(worksheet_type, Context, "table"),
            filters => proplists:get_value(filters, Context, []),
            options => proplists:get_value(options, Context, #{})
        }
    }.

tableau_refresh_job_config(Schedule, State) =>
    #{
        refreshType => proplists:get_value(type, Schedule, "full"),
        priority => proplists:get_value(priority, Schedule, "normal"),
        schedule => proplists:get_value(schedule, Schedule, undefined),
        parameterValues => proplists:get_value(parameters, Schedule, [])
    }.

tableau_permission_update(Permissions, State) =>
    #{
        permissions => #{
            granteeCapabilities => Permissions,
            propagate => proplists:get_value(propagate, Permissions, true)
        }
    }.

powerbi_headers(State) =>
    #{
        <<"Authorization">> => <<"Bearer ", (maps:get(personal_access_token, State#state.config))/binary>>,
        <<"Content-Type">> => <<"application/json">>,
        <<"Accept">> => <<"application/json">>
    }.

powerbi_dataset_config(DatasetName, Data, Context) =>
    #{
        name => DatasetName,
        defaultMode => proplists:get_value(default_mode, Context, "import"),
        tables => proplists:get_value(tables, Context, []),
        relationships => proplists:get_value(relationships, Context, []),
        refreshPolicy => proplists:get_value(refresh_policy, Context, #{
            enabled => true,
            refresh => [proplists:get_value(refresh_type, Context, "Full")]
        })
    }.

powerbi_dashboard_config(DashboardName, Config, Context) =>
    #{
        name => DashboardName,
        description => proplists:get_value(description, Context, "erlmcp dashboard"),
        tiles => proplists:get_value(tiles, Context, []),
        embedUrl => proplists:get_value(embed_url, Context, undefined),
        isDefault => proplists:get_value(is_default, Context, false)
    }.

powerbi_report_config(ReportName, Dataset, Context) =>
    #{
        name => ReportName,
        description => proplists:get_value(description, Context, "erlmcp report"),
        sourceModel => Dataset,
        viewProjections => proplists:get_value(view_projections, Context, []),
        layout => proplists:get_value(layout, Context, "mobile")
    }.

powerbi_refresh_schedule(Schedule, State) =>
    #{
        type => proplists:get_value(type, Schedule, "Scheduled"),
        enabled => proplists:get_value(enabled, Schedule, true),
        endTime => proplists:get_value(end_time, Schedule, undefined),
        localTime => proplists:get_value(local_time, Schedule, undefined),
        refreshFrequency => proplists:get_value(frequency, Schedule, "Daily"),
        startTime => proplists:get_value(start_time, Schedule, undefined),
        timezones => proplists:get_value(timezones, Schedule, []),
        value => proplists:get_value(value, Schedule, undefined)
    }.

powerbi_permission_update(Permissions, State) =>
    #{
        principal => #{
            id => proplists:get_value(id, Permissions),
            displayName => proplists:get_value(display_name, Permissions),
            email => proplists:get_value(email, Permissions)
        },
        role => proplists:get_value(role, Permissions)
    }.