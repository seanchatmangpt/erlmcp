%% @doc Cloud Platform Adapter
%% Integrates with AWS, Azure, GCP for cloud services
-module(erlmcp_cloud_adapter).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    platform :: aws | azure | gcp,
    config :: map(),
    connection :: pid() | undefined,
    services :: map(),
    regions :: [binary()],
    resources :: map()
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    Platform = maps:get(platform, Config),
    gen_server:start_link({local, cloud_name(Platform)}, ?MODULE, [Platform, Config], []).

-spec stop(Ref :: pid() | atom()) -> ok.
stop(Ref) ->
    gen_server:stop(Ref).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom(), map()]) -> {ok, #state{}} | {stop, term()}.
init([Platform, Config]) ->
    process_flag(trap_exit, true),
    State = #state{platform = Platform, config = Config, services = #{}, regions = [], resources = #{}},

    %% Initialize connection
    case init_cloud_connection(Platform, Config) of
        {ok, Connection} ->
            {ok, State#state{connection = Connection}};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(create_resource, {From, ResourceConfig}, State) ->
    case create_resource(State, ResourceConfig) of
        {ok, ResourceId} ->
            Metrics = update_metric(State#state.metrics, resources_created, 1),
            {reply, {ok, ResourceId}, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_resource, {From, ResourceId}, State) ->
    case get_resource(State, ResourceId) of
        {ok, Resource} ->
            {reply, {ok, Resource}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(list_resources, {From, ResourceType}, State) ->
    case list_resources(State, ResourceType) of
        {ok, Resources} ->
            {reply, {ok, Resources}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(delete_resource, {From, ResourceId}, State) ->
    case delete_resource(State, ResourceId) of
        ok ->
            Metrics = update_metric(State#state.metrics, resources_deleted, 1),
            Resources = maps:remove(ResourceId, State#state.resources),
            {reply, ok, State#state{metrics = Metrics, resources = Resources}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(deploy_service, {From, ServiceConfig}, State) ->
    case deploy_service(State, ServiceConfig) of
        {ok, ServiceId} ->
            Metrics = update_metric(State#state.metrics, services_deployed, 1),
            Services = maps:put(ServiceId, ServiceConfig, State#state.services),
            {reply, {ok, ServiceId}, State#state{metrics = Metrics, services = Services}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_service_status, {From, ServiceId}, State) ->
    case get_service_status(State, ServiceId) of
        {ok, Status} ->
            {reply, {ok, Status}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(scaling_operation, {From, Operation}, State) ->
    case perform_scaling_operation(State, Operation) of
        ok ->
            Metrics = update_metric(State#state.metrics, scaling_operations, 1),
            {reply, ok, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_cost_metrics, {From, Period}, State) ->
    case get_cost_metrics(State, Period) of
        {ok, Metrics} ->
            {reply, {ok, Metrics}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_iam_policies, _From, State) ->
    case list_iam_policies(State) of
        {ok, Policies} ->
            {reply, {ok, Policies}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(create_iam_policy, {From, PolicyConfig}, State) ->
    case create_iam_policy(State, PolicyConfig) of
        {ok, PolicyId} ->
            {reply, {ok, PolicyId}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({resource_created, Resource}, State) ->
    %% Update resource registry
    Resources = maps:put(Resource#id, Resource, State#state.resources),
    {noreply, State#state{resources = Resources}};

handle_cast({service_deployed, Service}, State) ->
    %% Update service registry
    Services = maps:put(Service#id, Service, State#state.services),
    {noreply, State#state{services = Services}};

handle_cast({region_updated, Regions}, State) ->
    %% Update region list
    {noreply, State#state{regions = Regions}};

handle_cast({cost_alert, Alert}, State) ->
    %% Handle cost alerts
    case handle_cost_alert(State, Alert) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Failed to handle cost alert: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast({update_config, NewConfig}, State) ->
    case reconnect_cloud(State, NewConfig) of
        {ok, NewState} ->
            {noreply, NewState#state{config = NewConfig}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to reconnect: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info({resource_event, Event}, State) ->
    case handle_resource_event(Event, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Failed to handle resource event: ~p", [Reason]),
            {noreply, State}
    end;

handle_info({service_event, Event}, State) ->
    case handle_service_event(Event, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Failed to handle service event: ~p", [Reason]),
            {noreply, State}
    end;

handle_info(connection_lost, Platform) ->
    ?LOG_WARNING("Lost connection to ~p, attempting reconnect", [Platform]),
    case reconnect_cloud(State, State#state.config) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Reconnect failed: ~p", [Reason]),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.connection of
        undefined -> ok;
        Connection -> erlmcp_cloud_connection:close(Connection)
    end.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec cloud_name(atom()) -> atom().
cloud_name(Platform) ->
    list_to_atom("cloud_" ++ atom_to_list(Platform) ++ "_adapter").

-spec init_cloud_connection(atom(), map()) -> {ok, pid()} | {error, term()}.
init_cloud_connection(aws, Config) ->
    erlmcp_aws_client:start(Config);
init_cloud_connection(azure, Config) ->
    erlmcp_azure_client:start(Config);
init_cloud_connection(gcp, Config) ->
    erlmcp_gcp_client:start(Config).

-spec create_resource(#state{}, map()) -> {ok, binary()} | {error, term()}.
create_resource(State, ResourceConfig) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.platform of
                aws -> erlmcp_aws_client:create_resource(Connection, ResourceConfig);
                azure -> erlmcp_azure_client:create_resource(Connection, ResourceConfig);
                gcp -> erlmcp_gcp_client:create_resource(Connection, ResourceConfig)
            end
    end.

-spec get_resource(#state{}, binary()) -> {ok, map()} | {error, term()}.
get_resource(State, ResourceId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.platform of
                aws -> erlmcp_aws_client:get_resource(Connection, ResourceId);
                azure -> erlmcp_azure_client.get_resource(Connection, ResourceId);
                gcp -> erlmcp_gcp_client.get_resource(Connection, ResourceId)
            end
    end.

-spec list_resources(#state{}, binary()) -> {ok, [map()]} | {error, term()}.
list_resources(State, ResourceType) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.platform of
                aws -> erlmcp_aws_client:list_resources(Connection, ResourceType);
                azure -> erlmcp_azure_client.list_resources(Connection, ResourceType);
                gcp -> erlmcp_gcp_client.list_resources(Connection, ResourceType)
            end
    end.

-spec delete_resource(#state{}, binary()) -> ok | {error, term()}.
delete_resource(State, ResourceId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.platform of
                aws -> erlmcp_aws_client:delete_resource(Connection, ResourceId);
                azure -> erlmcp_azure_client.delete_resource(Connection, ResourceId);
                gcp -> erlmcp_gcp_client.delete_resource(Connection, ResourceId)
            end
    end.

-spec deploy_service(#state{}, map()) -> {ok, binary()} | {error, term()}.
deploy_service(State, ServiceConfig) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.platform of
                aws -> erlmcp_aws_client:deploy_service(Connection, ServiceConfig);
                azure -> erlmcp_azure_client.deploy_service(Connection, ServiceConfig);
                gcp -> erlmcp_gcp_client.deploy_service(Connection, ServiceConfig)
            end
    end.

-spec get_service_status(#state{}, binary()) -> {ok, map()} | {error, term()}.
get_service_status(State, ServiceId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.platform of
                aws -> erlmcp_aws_client:get_service_status(Connection, ServiceId);
                azure -> erlmcp_azure_client.get_service_status(Connection, ServiceId);
                gcp -> erlmcp_gcp_client.get_service_status(Connection, ServiceId)
            end
    end.

-spec perform_scaling_operation(#state{}, map()) -> ok | {error, term()}.
perform_scaling_operation(State, Operation) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.platform of
                aws -> erlmcp_aws_client:scaling_operation(Connection, Operation);
                azure -> erlmcp_azure_client.scaling_operation(Connection, Operation);
                gcp -> erlmcp_gcp_client.scaling_operation(Connection, Operation)
            end
    end.

-spec get_cost_metrics(#state{}, binary()) -> {ok, map()} | {error, term()}.
get_cost_metrics(State, Period) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.platform of
                aws -> erlmcp_aws_client.get_cost_metrics(Connection, Period);
                azure -> erlmcp_azure_client.get_cost_metrics(Connection, Period);
                gcp -> erlmcp_gcp_client.get_cost_metrics(Connection, Period)
            end
    end.

-spec list_iam_policies(#state{}) -> {ok, [map()]} | {error, term()}.
list_iam_policies(State) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.platform of
                aws -> erlmcp_aws_client:list_iam_policies(Connection);
                azure -> erlmcp_azure_client.list_iam_policies(Connection);
                gcp -> erlmcp_gcp_client.list_iam_policies(Connection)
            end
    end.

-spec create_iam_policy(#state{}, map()) -> {ok, binary()} | {error, term()}.
create_iam_policy(State, PolicyConfig) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.platform of
                aws -> erlmcp_aws_client:create_iam_policy(Connection, PolicyConfig);
                azure -> erlmcp_azure_client.create_iam_policy(Connection, PolicyConfig);
                gcp -> erlmcp_gcp_client.create_iam_policy(Connection, PolicyConfig)
            end
    end.

-spec handle_resource_event(map(), #state{}) -> {ok, #state{}} | {error, term()}.
handle_resource_event(Event, State) ->
    case Event of
        {resource_created, ResourceId, Resource} ->
            Resources = maps:put(ResourceId, Resource, State#state.resources),
            {ok, State#state{resources = Resources}};
        {resource_deleted, ResourceId} ->
            Resources = maps:remove(ResourceId, State#state.resources),
            {ok, State#state{resources = Resources}};
        _ ->
            {ok, State}
    end.

-spec handle_service_event(map(), #state{}) -> {ok, #state{}} | {error, term()}.
handle_service_event(Event, State) ->
    case Event of
        {service_deployed, ServiceId, Service} ->
            Services = maps:put(ServiceId, Service, State#state.services),
            {ok, State#state{services = Services}};
        {service_deleted, ServiceId} ->
            Services = maps:remove(ServiceId, State#state.services),
            {ok, State#state{services = Services}};
        _ ->
            {ok, State}
    end.

-spec handle_cost_alert(#state{}, map()) -> {ok, #state{}} | {error, term()}.
handle_cost_alert(State, Alert) ->
    case Alert#threshold of
        Threshold when Threshold > 10000 ->
            %% High cost alert - notify and potentially scale down
            erlmcp_cloud_notifier:notify(high_cost, Alert),
            {ok, State};
        Threshold when Threshold > 5000 ->
            %% Medium cost alert - log and monitor
            erlmcp_cloud_notifier:notify(medium_cost, Alert),
            {ok, State};
        _ ->
            {ok, State}
    end.

-spec reconnect_cloud(#state{}, map()) -> {ok, #state{}} | {error, term()}.
reconnect_cloud(State, NewConfig) ->
    case State#state.connection of
        undefined ->
            case init_cloud_connection(State#state.platform, NewConfig) of
                {ok, Connection} ->
                    {ok, State#state{connection = Connection}};
                {error, Reason} ->
                    {error, Reason}
            end;
        OldConnection ->
            erlmcp_cloud_connection:close(OldConnection),
            case init_cloud_connection(State#state.platform, NewConfig) of
                {ok, Connection} ->
                    {ok, State#state{connection = Connection}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec update_metric(map(), atom(), integer()) -> map().
update_metric(Metrics, Key, Inc) ->
    Current = maps:get(Key, Metrics, 0),
    Metrics#{Key => Current + Inc}.