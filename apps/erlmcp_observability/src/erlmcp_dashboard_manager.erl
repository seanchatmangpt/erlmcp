%%%-------------------------------------------------------------------
%%% @doc
%%% Dashboard Manager for erlmcp Monitoring Stack
%%%
%%% Manages the creation, storage, and retrieval of monitoring dashboards
%%% for Prometheus, Grafana, and other monitoring platforms.
%%%
%%% Features:
%%% - Dashboard template management
%%% - Dynamic dashboard generation
%%% - Multi-platform support (Prometheus, Grafana, etc.)
%%% - Dashboard versioning
%%% - Sharing and collaboration
%%% - Performance optimization
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_dashboard_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         create_dashboard/2, get_dashboard/1,
         update_dashboard/2, delete_dashboard/1,
         list_dashboards/0, clone_dashboard/2,
         export_dashboard/1, import_dashboard/2,
         get_dashboard_stats/0,
         get_prometheus_alert_rules/0,
         get_grafana_dashboards/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Records
-record(dashboard_template, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    platform :: prometheus | grafana | custom,
    template :: map(),
    variables :: [binary()],
    version :: binary(),
    created_at :: integer(),
    updated_at :: integer(),
    author :: binary()
}).

-record(dashboard, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    platform :: prometheus | grafana | custom,
    data :: map(),
    template_id :: binary(),
    version :: binary(),
    created_at :: integer(),
    updated_at :: integer(),
    variables :: map(),
    shared :: boolean(),
    access_control :: [binary()]
}).

-record(prometheus_alert_rule, {
    name :: binary(),
    expr :: binary(),
    duration :: binary(),
    labels :: map(),
    annotations :: map(),
    enabled :: boolean()
}).

-record(dashboard_stats, {
    total_dashboards :: integer(),
    dashboards_by_platform :: map(),
    shared_dashboards :: integer(),
    recent_access :: map(),
    storage_usage :: integer()
}).

-record(state, {
    templates :: #{binary() => #dashboard_template{}},
    dashboards :: #{binary() => #dashboard{}},
    stats :: #dashboard_stats{},
    storage_limit :: integer(),
    version_control :: boolean(),
    auto_export :: boolean(),
    export_interval :: pos_integer(),
    export_ref :: reference() | undefined,
    last_export :: integer() | undefined
}).

-define(STORAGE_LIMIT, 10737418240).  % 10GB
-define(EXPORT_INTERVAL, 3600000).  % 1 hour
-define(DEFAULT_TEMPLATE_VERSION, "1.0.0").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the dashboard manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start with configuration
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @brief Create a new dashboard
-spec create_dashboard(binary(), map()) -> ok.
create_dashboard(Name, Config) ->
    gen_server:call(?MODULE, {create_dashboard, Name, Config}).

%% @brief Get dashboard by ID
-spec get_dashboard(binary()) -> map() | undefined.
get_dashboard(Id) ->
    gen_server:call(?MODULE, {get_dashboard, Id}).

%% @brief Update existing dashboard
-spec update_dashboard(binary(), map()) -> ok.
update_dashboard(Id, Config) ->
    gen_server:call(?MODULE, {update_dashboard, Id, Config}).

%% @brief Delete a dashboard
-spec delete_dashboard(binary()) -> ok.
delete_dashboard(Id) ->
    gen_server:call(?MODULE, {delete_dashboard, Id}).

%% @brief List all dashboards
-spec list_dashboards() -> [map()].
list_dashboards() ->
    gen_server:call(?MODULE, list_dashboards).

%% @brief Clone an existing dashboard
-spec clone_dashboard(binary(), binary()) -> ok.
clone_dashboard(SourceId, NewName) ->
    gen_server:call(?MODULE, {clone_dashboard, SourceId, NewName}).

%% @brief Export dashboard
-spec export_dashboard(binary()) -> map().
export_dashboard(Id) ->
    gen_server:call(?MODULE, {export_dashboard, Id}).

%% @brief Import dashboard
-spec import_dashboard(binary(), map()) -> ok.
import_dashboard(Id, Data) ->
    gen_server:call(?MODULE, {import_dashboard, Id, Data}).

%% @brief Get dashboard statistics
-spec get_dashboard_stats() -> map().
get_dashboard_stats() ->
    gen_server:call(?MODULE, get_dashboard_stats).

%% @brief Get Prometheus alert rules
-spec get_prometheus_alert_rules() -> [#prometheus_alert_rule{}].
get_prometheus_alert_rules() ->
    gen_server:call(?MODULE, get_prometheus_alert_rules).

%% @brief Get Grafana dashboards
-spec get_grafana_dashboards() -> [map()].
get_grafana_dashboards() ->
    gen_server:call(?MODULE, get_grafana_dashboards).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Config) ->
    process_flag(trap_exit, true),

    %% Initialize with default templates
    DefaultTemplates = load_default_templates(),

    %% Initialize state
    State0 = #state{
        templates = DefaultTemplates,
        dashboards = #{},
        stats = #dashboard_stats{
            total_dashboards = 0,
            dashboards_by_platform = #{prometheus => 0, grafana => 0, custom => 0},
            shared_dashboards = 0,
            recent_access = #{},
            storage_usage = 0
        },
        storage_limit = maps:get(storage_limit, Config, ?STORAGE_LIMIT),
        version_control = maps:get(version_control, Config, true),
        auto_export = maps:get(auto_export, Config, false),
        export_interval = maps:get(export_interval, Config, ?EXPORT_INTERVAL),
        export_ref = undefined,
        last_export = undefined
    },

    %% Start auto-export if enabled
    State = case State0#state.auto_export of
        true ->
            ExportRef = erlang:send_after(State0#state.export_interval, self(), auto_export),
            State0#state{export_ref = ExportRef};
        false ->
            State0
    end,

    {ok, State}.

handle_call({create_dashboard, Name, Config}, _From, State) ->
    %% Generate unique ID
    DashboardId = generate_dashboard_id(),

    %% Extract platform and template
    Platform = maps:get(platform, Config, prometheus),
    TemplateId = maps:get(template_id, Config),

    %% Get template
    Template = case TemplateId of
        undefined ->
            create_default_template(Platform);
        _ ->
            maps:get(TemplateId, State#state.templates, create_default_template(Platform))
    end,

    %% Get variables
    Variables = maps:get(variables, Config, #{}),

    %% Generate dashboard data
    DashboardData = generate_dashboard_data(Template#dashboard_template.template, Variables),

    %% Create dashboard record
    Dashboard = #dashboard{
        id = DashboardId,
        name = Name,
        description = maps:get(description, Config, ""),
        platform = Platform,
        data = DashboardData,
        template_id = TemplateId,
        version = ?DEFAULT_TEMPLATE_VERSION,
        created_at = erlang:system_time(millisecond),
        updated_at = erlang:system_time(millisecond),
        variables = Variables,
        shared = maps:get(shared, Config, false),
        access_control = maps:get(access_control, Config, [])
    },

    %% Store dashboard
    Dashboards = maps:put(DashboardId, Dashboard, State#state.dashboards),

    %% Update stats
    UpdatedStats = update_dashboard_stats(Dashboard, State#state.stats),

    ?LOG_INFO("Created dashboard: ~s (~s)", [Name, Platform]),

    {reply, DashboardId, State#state{
        dashboards = Dashboards,
        stats = UpdatedStats
    }};

handle_call({get_dashboard, Id}, _From, State) ->
    Dashboard = maps:get(Id, State#state.dashboards, undefined),
    case Dashboard of
        undefined -> {reply, undefined, State};
        _ ->
            %% Update access time
            UpdatedStats = update_access_stats(Id, State#state.stats),
            {reply, Dashboard, State#state{stats = UpdatedStats}}
    end;

handle_call({update_dashboard, Id, Config}, _From, State) ->
    %% Get existing dashboard
    Dashboard = maps:get(Id, State#state.dashboards, undefined),

    case Dashboard of
        undefined ->
            {reply, {error, not_found}, State};
        _ ->
            %% Update dashboard
            UpdatedDashboard = Dashboard#dashboard{
                name = maps:get(name, Config, Dashboard#dashboard.name),
                description = maps:get(description, Config, Dashboard#dashboard.description),
                variables = maps:get(variables, Config, Dashboard#dashboard.variables),
                shared = maps:get(shared, Config, Dashboard#dashboard.shared),
                access_control = maps:get(access_control, Config, Dashboard#dashboard.access_control),
                updated_at = erlang:system_time(millisecond)
            },

            %% Regenerate data if variables changed
            NewDashboard = case maps:size(maps:diff(Dashboard#dashboard.variables, UpdatedDashboard#dashboard.variables)) of
                0 -> UpdatedDashboard;
                _ ->
                    Template = maps:get(UpdatedDashboard#dashboard.template_id, State#state.templates),
                    NewData = generate_dashboard_data(Template#dashboard_template.template, UpdatedDashboard#dashboard.variables),
                    UpdatedDashboard#dashboard{data = NewData}
            end,

            %% Store updated dashboard
            Dashboards = maps:put(Id, NewDashboard, State#state.dashboards),

            %% Update stats
            UpdatedStats = update_dashboard_stats(NewDashboard, State#state.stats),

            ?LOG_INFO("Updated dashboard: ~s", [Id]),

            {reply, ok, State#state{
                dashboards = Dashboards,
                stats = UpdatedStats
            }}
    end;

handle_call({delete_dashboard, Id}, _From, State) ->
    %% Get dashboard to delete
    Dashboard = maps:get(Id, State#state.dashboards, undefined),

    case Dashboard of
        undefined ->
            {reply, {error, not_found}, State};
        _ ->
            %% Remove dashboard
            Dashboards = maps:remove(Id, State#state.dashboards),

            %% Update stats
            UpdatedStats = decrement_dashboard_stats(Dashboard, State#state.stats),

            ?LOG_INFO("Deleted dashboard: ~s", [Id]),

            {reply, ok, State#state{
                dashboards = Dashboards,
                stats = UpdatedStats
            }}
    end;

handle_call(list_dashboards, _From, State) ->
    %% Convert dashboards to list format
    DashboardList = maps:values(State#state.dashboards),
    {reply, DashboardList, State};

handle_call({clone_dashboard, SourceId, NewName}, _From, State) ->
    %% Get source dashboard
    SourceDashboard = maps:get(SourceId, State#state.dashboards, undefined),

    case SourceDashboard of
        undefined ->
            {reply, {error, not_found}, State};
        _ ->
            %% Create clone
            CloneId = generate_dashboard_id(),
            CloneDashboard = SourceDashboard#dashboard{
                id = CloneId,
                name = NewName,
                created_at = erlang:system_time(millisecond),
                updated_at = erlang:system_time(millisecond)
            },

            %% Store clone
            Dashboards = maps:put(CloneId, CloneDashboard, State#state.dashboards),

            %% Update stats
            UpdatedStats = update_dashboard_stats(CloneDashboard, State#state.stats),

            ?LOG_INFO("Cloned dashboard: ~s -> ~s", [SourceId, CloneId]),

            {reply, CloneId, State#state{
                dashboards = Dashboards,
                stats = UpdatedStats
            }}
    end;

handle_call({export_dashboard, Id}, _From, State) ->
    Dashboard = maps:get(Id, State#state.dashboards, undefined),

    case Dashboard of
        undefined ->
            {reply, {error, not_found}, State};
        _ ->
            ExportData = export_dashboard_data(Dashboard),
            {reply, ExportData, State}
    end;

handle_call({import_dashboard, Id, Data}, _From, State) ->
    %% Validate import data
    case validate_import_data(Data) of
        invalid ->
            {reply, {error, invalid_data}, State};
        _ ->
            %% Import dashboard
            ImportedDashboard = import_dashboard_data(Id, Data),
            Dashboards = maps:put(Id, ImportedDashboard, State#state.dashboards),

            %% Update stats
            UpdatedStats = update_dashboard_stats(ImportedDashboard, State#state.stats),

            ?LOG_INFO("Imported dashboard: ~s", [Id]),

            {reply, ok, State#state{
                dashboards = Dashboards,
                stats = UpdatedStats
            }}
    end;

handle_call(get_dashboard_stats, _From, State) ->
    {reply, State#state.stats, State};

handle_call(get_prometheus_alert_rules, _From, State) ->
    %% Get all Prometheus dashboards and extract alert rules
    PrometheusDashboards = [D || D <- maps:values(State#state.dashboards), D#dashboard.platform =:= prometheus],
    AlertRules = lists:foldl(fun(Dashboard, Acc) ->
        extract_alert_rules(Dashboard) ++ Acc
    end, [], PrometheusDashboards),

    {reply, AlertRules, State};

handle_call(get_grafana_dashboards, _From, State) ->
    %% Get all Grafana dashboards
    GrafanaDashboards = [D || D <- maps:values(State#state.dashboards), D#dashboard.platform =:= grafana],

    %% Format for Grafana API
    FormattedDashboards = lists:map(fun(Dashboard) ->
        #{
            id => Dashboard#dashboard.id,
            title => Dashboard#dashboard.name,
            description => Dashboard#dashboard.description,
            panels => format_grafana_panels(Dashboard#dashboard.data),
            tags => ["erlmcp", "managed"]
        }
    end, GrafanaDashboards),

    {reply, FormattedDashboards, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(auto_export, State) ->
    %% Auto-export all dashboards
    export_all_dashboards(State),

    %% Schedule next export
    ExportRef = erlang:send_after(State#state.export_interval, self(), auto_export),

    {noreply, State#state{export_ref = ExportRef, last_export = erlang:system_time(millisecond)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel export timer
    case State#state.export_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @brief Create default template
create_default_template(Platform) ->
    #dashboard_template{
        id = generate_template_id(),
        name = <<(atom_to_binary(Platform))/binary, "_default">>,
        description = "Default template for " ++ atom_to_list(Platform),
        platform = Platform,
        template = get_platform_template(Platform),
        variables = get_platform_variables(Platform),
        version = ?DEFAULT_TEMPLATE_VERSION,
        created_at = erlang:system_time(millisecond),
        updated_at = erlang:system_time(millisecond),
        author = "system"
    }.

%% @brief Get platform-specific template
get_platform_template(prometheus) ->
    #{panels => [
        #{
            title => "System Overview",
            type => "graph",
            metrics => ["erlmcp_memory_total_bytes", "erlmcp_process_count"]
        },
        #{
            title => "Error Rate",
            type => "graph",
            metrics => ["rate(erlmcp_errors_total[5m])"]
        }
    ]};

get_platform_template(grafana) ->
    #{
        title => "erlmcp Monitoring",
        panels => [
            #{
                title => "Requests",
                type => "graph",
                targets => [
                    #{
                        expr => "rate(erlmcp_requests_total[5m])",
                        legendFormat => "Requests"
                    }
                ]
            }
        ]
    };

get_platform_template(custom) ->
    #{panels => []}.

%% @brief Get platform variables
get_platform_variables(prometheus) ->
    [<<"duration">>, <<"resolution">>];

get_platform_variables(grafana) ->
    [<<"dashboardId">>, <<"timeRange">>];

get_platform_variables(custom) ->
    [].

%% @brief Generate dashboard data from template
generate_dashboard_data(Template, Variables) ->
    %% Apply variables to template
    ProcessedTemplate = apply_variables(Template, Variables),

    %% Add metadata
    ProcessedTemplate#{
        generated_at => erlang:system_time(millisecond),
        variables => Variables
    }.

%% @brief Apply variables to template
apply_variables(Template, Variables) ->
    %% This would apply the actual variable substitutions
    Template.

%% @brief Export dashboard data
export_dashboard_data(Dashboard) ->
    #{
        id => Dashboard#dashboard.id,
        name => Dashboard#dashboard.name,
        description => Dashboard#dashboard.description,
        platform => Dashboard#dashboard.platform,
        data => Dashboard#dashboard.data,
        template_id => Dashboard#dashboard.template_id,
        version => Dashboard#dashboard.version,
        variables => Dashboard#dashboard.variables,
        created_at => Dashboard#dashboard.created_at,
        updated_at => Dashboard#dashboard.updated_at,
        shared => Dashboard#dashboard.shared
    }.

%% @brief Import dashboard data
import_dashboard_data(Id, Data) ->
    DashboardId = maps:get(id, Data, Id),

    #dashboard{
        id = DashboardId,
        name = maps:get(name, Data),
        description = maps:get(description, Data),
        platform = maps:get(platform, Data),
        data = maps:get(data, Data),
        template_id = maps:get(template_id, Data),
        version = maps:get(version, Data, ?DEFAULT_TEMPLATE_VERSION),
        created_at = maps:get(created_at, Data, erlang:system_time(millisecond)),
        updated_at = erlang:system_time(millisecond),
        variables = maps:get(variables, Data, #{}),
        shared = maps:get(shared, Data, false),
        access_control = maps:get(access_control, Data, [])
    }.

%% @brief Validate import data
validate_import_data(Data) ->
    %% Basic validation - would need more comprehensive checks
    RequiredFields = [id, name, platform, data],
    lists:all(fun(Field) -> maps:is_key(Field, Data) end, RequiredFields).

%% @brief Extract alert rules from dashboard
extract_alert_rules(Dashboard) ->
    case Dashboard#dashboard.platform of
        prometheus ->
            extract_prometheus_alerts(Dashboard#dashboard.data);
        _ ->
            []
    end.

%% @brief Extract Prometheus alerts
extract_prometheus_alerts(Data) ->
    %% This would extract alert rules from Prometheus dashboard data
    [].

%% @brief Format panels for Grafana
format_grafana_panels(Data) ->
    case maps:get(panels, Data, undefined) of
        undefined -> [];
        Panels -> lists:map(fun(Panel) ->
            #{
                id => generate_panel_id(),
                title => maps:get(title, Panel),
                type => maps:get(type, Panel),
                targets => maps:get(metrics, Panel)
            }
        end, Panels)
    end.

%% @brief Update dashboard statistics
update_dashboard_stats(Dashboard, Stats) ->
    Platform = Dashboard#dashboard.platform,

    %% Update total count
    Total = Stats#dashboard_stats.total_dashboards + 1,

    %% Update platform count
    PlatformCountMap = maps:update_with(
        Platform,
        fun(C) -> C + 1 end,
        1,
        Stats#dashboard_stats.dashboards_by_platform
    ),

    %% Update shared count
    SharedCount = case Dashboard#dashboard.shared of
        true -> Stats#dashboard_stats.shared_dashboards + 1;
        false -> Stats#dashboard_stats.shared_dashboards
    end,

    %% Update storage usage (estimate)
    StorageEstimate = estimate_dashboard_size(Dashboard),
    TotalStorage = Stats#dashboard_stats.storage_usage + StorageEstimate,

    Stats#dashboard_stats{
        total_dashboards = Total,
        dashboards_by_platform = PlatformCountMap,
        shared_dashboards = SharedCount,
        storage_usage = TotalStorage
    }.

%% @brief Decrement dashboard statistics
decrement_dashboard_stats(Dashboard, Stats) ->
    Platform = Dashboard#dashboard.platform,

    %% Update total count
    Total = max(Stats#dashboard_stats.total_dashboards - 1, 0),

    %% Update platform count
    PlatformCountMap = maps:update_with(
        Platform,
        fun(C) -> max(C - 1, 0) end,
        Stats#dashboard_stats.dashboards_by_platform
    ),

    %% Update shared count
    SharedCount = case Dashboard#dashboard.shared of
        true -> max(Stats#dashboard_stats.shared_dashboards - 1, 0);
        false -> Stats#dashboard_stats.shared_dashboards
    end,

    %% Update storage usage
    StorageEstimate = estimate_dashboard_size(Dashboard),
    TotalStorage = max(Stats#dashboard_stats.storage_usage - StorageEstimate, 0),

    Stats#dashboard_stats{
        total_dashboards = Total,
        dashboards_by_platform = PlatformCountMap,
        shared_dashboards = SharedCount,
        storage_usage = TotalStorage
    }.

%% @brief Update access statistics
update_access_stats(DashboardId, Stats) ->
    %% Update recent access time
    Now = erlang:system_time(millisecond),
    UpdatedAccess = maps:put(DashboardId, Now, Stats#dashboard_stats.recent_access),

    %% Keep only last 100 access records
    FinalAccess = if map_size(UpdatedAccess) > 100 ->
            Sorted = lists:sort(fun({_K1, V1}, {_K2, V2}) -> V1 > V2 end, maps:to_list(UpdatedAccess)),
            Truncated = lists:sublist(Sorted, 100),
            maps:from_list(Truncated);
       true ->
            UpdatedAccess
    end,

    Stats#dashboard_stats{recent_access = FinalAccess}.

%% @brief Estimate dashboard size
estimate_dashboard_size(Dashboard) ->
    %% Simple size estimation based on data size
    Size = size(term_to_binary(Dashboard#dashboard.data)),
    Size + 1024.  % Add metadata estimate

%% @brief Auto-export all dashboards
export_all_dashboards(State) ->
    ?LOG_INFO("Auto-exporting ~p dashboards", [map_size(State#state.dashboards)]),

    %% This would export to cloud storage or backup location
    ok.

%% @brief Load default templates
load_default_templates() ->
    maps:from_list([
        {<<"prometheus_overview">>, #dashboard_template{
            id = <<"prometheus_overview">>,
            name = "Prometheus System Overview",
            description = "Default Prometheus dashboard for erlmcp system monitoring",
            platform = prometheus,
            template = get_platform_template(prometheus),
            variables = get_platform_variables(prometheus),
            version = "1.0.0",
            created_at = erlang:system_time(millisecond),
            updated_at = erlang:system_time(millisecond),
            author = "system"
        }},
        {<<"grafana_erlmcp">>, #dashboard_template{
            id = <<"grafana_erlmcp">>,
            name = "erlmcp Grafana Dashboard",
            description = "Grafana dashboard for erlmcp metrics and traces",
            platform = grafana,
            template = get_platform_template(grafana),
            variables = get_platform_variables(grafana),
            version = "1.0.0",
            created_at = erlang:system_time(millisecond),
            updated_at = erlang:system_time(millisecond),
            author = "system"
        }}
    ]).

%% @brief Generate unique dashboard ID
generate_dashboard_id() ->
    Id = crypto:strong_rand_bytes(8),
    integer_to_binary(binary:decode_unsigned(Id), 16).

%% @brief Generate unique template ID
generate_template_id() ->
    Id = crypto:strong_rand_bytes(8),
    integer_to_binary(binary:decode_unsigned(Id), 16).

%% @brief Generate unique panel ID
generate_panel_id() ->
    Id = crypto:strong_rand_bytes(4),
    integer_to_binary(binary:decode_unsigned(Id), 16).