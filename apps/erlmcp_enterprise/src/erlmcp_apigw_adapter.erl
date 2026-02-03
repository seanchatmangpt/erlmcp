%% @doc API Gateway Adapter
%% Integrates with Kong, Apigee for API management
-module(erlmcp_apigw_adapter).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    gateway :: kong | apigee,
    config :: map(),
    connection :: pid() | undefined,
    apis :: map(),
    routes :: map(),
    plugins :: map(),
    ratelimit :: map()
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    Gateway = maps:get(gateway, Config),
    gen_server:start_link({local, apigw_name(Gateway)}, ?MODULE, [Gateway, Config], []).

-spec stop(Ref :: pid() | atom()) -> ok.
stop(Ref) ->
    gen_server:stop(Ref).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom(), map()]) -> {ok, #state{}} | {stop, term()}.
init([Gateway, Config]) ->
    process_flag(trap_exit, true),
    State = #state{gateway = Gateway, config = Config, apis = #{}, routes = #{}, plugins = #{}, ratelimit = #{}},

    %% Initialize connection
    case init_apigw_connection(Gateway, Config) of
        {ok, Connection} ->
            {ok, State#state{connection = Connection}};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(create_api, {From, ApiConfig}, State) ->
    case create_api(State, ApiConfig) of
        {ok, ApiId} ->
            Metrics = update_metric(State#state.metrics, apis_created, 1),
            {reply, {ok, ApiId}, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_api, {From, ApiId}, State) ->
    case get_api(State, ApiId) of
        {ok, Api} ->
            {reply, {ok, Api}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(list_apis, _From, State) ->
    {reply, {ok, maps:keys(State#state.apis)}, State};

handle_call(create_route, {From, ApiId, RouteConfig}, State) ->
    case create_route(State, ApiId, RouteConfig) of
        {ok, RouteId} ->
            Metrics = update_metric(State#state.metrics, routes_created, 1),
            {reply, {ok, RouteId}, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_route, {From, RouteId}, State) ->
    case get_route(State, RouteId) of
        {ok, Route} ->
            {reply, {ok, Route}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(list_routes, {From, ApiId}, State) ->
    case list_routes(State, ApiId) of
        {ok, Routes} ->
            {reply, {ok, Routes}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(add_plugin, {From, ApiId, PluginConfig}, State) ->
    case add_plugin(State, ApiId, PluginConfig) of
        {ok, PluginId} ->
            Metrics = update_metric(State#state.metrics, plugins_added, 1),
            {reply, {ok, PluginId}, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(remove_plugin, {From, ApiId, PluginId}, State) ->
    case remove_plugin(State, ApiId, PluginId) of
        ok ->
            Metrics = update_metric(State#state.metrics, plugins_removed, 1),
            {reply, ok, State#state{metrics = Metrics};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(configure_ratelimit, {From, ApiId, RateConfig}, State) ->
    case configure_ratelimit(State, ApiId, RateConfig) of
        ok ->
            Metrics = update_metric(State#state.metrics, ratelimits_configured, 1),
            {reply, ok, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_api_stats, {From, ApiId}, State) ->
    case get_api_stats(State, ApiId) of
        {ok, Stats} ->
            {reply, {ok, Stats}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_usage_metrics, {From, ApiId}, State) ->
    case get_usage_metrics(State, ApiId) of
        {ok, Metrics} ->
            {reply, {ok, Metrics}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({api_created, Api}, State) ->
    %% Update API registry
    Apis = maps:put(Api#id, Api, State#state.apis),
    {noreply, State#state{apis = Apis}};

handle_cast({route_created, Route}, State) ->
    %% Update route registry
    Routes = maps:put(Route#id, Route, State#state.routes),
    {noreply, State#state{routes = Routes}};

handle_cast({plugin_added, Plugin}, State) ->
    %% Update plugin registry
    Plugins = maps:put(Plugin#id, Plugin, State#state.plugins),
    {noreply, State#state{plugins = Plugins}};

handle_cast({usage_update, Usage}, State) ->
    %% Update usage metrics
    ApiId = Usage#api_id,
    CurrentUsage = maps:get(ApiId, State#state.ratelimit, 0),
    Ratelimit = maps:put(ApiId, CurrentUsage + Usage#count, State#state.ratelimit),
    {noreply, State#state{ratelimit = Ratelimit}};

handle_cast({update_config, NewConfig}, State) ->
    case reconnect_apigw(State, NewConfig) of
        {ok, NewState} ->
            {noreply, NewState#state{config = NewConfig}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to reconnect: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info({api_event, Event}, State) ->
    case handle_api_event(Event, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Failed to handle API event: ~p", [Reason]),
            {noreply, State}
    end;

handle_info({rate_limit_exceeded, ApiId}, State) ->
    ?LOG_WARNING("Rate limit exceeded for API: ~p", [ApiId]),
    case handle_rate_limit_exceeded(State, ApiId) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Failed to handle rate limit: ~p", [Reason]),
            {noreply, State}
    end;

handle_info(connection_lost, Gateway) ->
    ?LOG_WARNING("Lost connection to ~p, attempting reconnect", [Gateway]),
    case reconnect_apigw(State, State#state.config) of
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
        Connection -> erlmcp_apigw_connection:close(Connection)
    end.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec apigw_name(atom()) -> atom().
apigw_name(Gateway) ->
    list_to_atom("apigw_" ++ atom_to_list(Gateway) ++ "_adapter").

-spec init_apigw_connection(atom(), map()) -> {ok, pid()} | {error, term()}.
init_apigw_connection(kong, Config) ->
    erlmcp_kong_client:start(Config);
init_apigw_connection(apigee, Config) ->
    erlmcp_apigee_client:start(Config).

-spec create_api(#state{}, map()) -> {ok, binary()} | {error, term()}.
create_api(State, ApiConfig) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.gateway of
                kong -> erlmcp_kong_client:create_api(Connection, ApiConfig);
                apigee -> erlmcp_apigee_client:create_api(Connection, ApiConfig)
            end
    end.

-spec get_api(#state{}, binary()) -> {ok, map()} | {error, term()}.
get_api(State, ApiId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.gateway of
                kong -> erlmcp_kong_client:get_api(Connection, ApiId);
                apigee -> erlmcp_apigee_client:get_api(Connection, ApiId)
            end
    end.

-spec create_route(#state{}, binary(), map()) -> {ok, binary()} | {error, term()}.
create_route(State, ApiId, RouteConfig) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.gateway of
                kong -> erlmcp_kong_client:create_route(Connection, ApiId, RouteConfig);
                apigee -> erlmcp_apigee_client.create_route(Connection, ApiId, RouteConfig)
            end
    end.

-spec get_route(#state{}, binary()) -> {ok, map()} | {error, term()}.
get_route(State, RouteId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.gateway of
                kong -> erlmcp_kong_client:get_route(Connection, RouteId);
                apigee -> erlmcp_apigee_client.get_route(Connection, RouteId)
            end
    end.

-spec list_routes(#state{}, binary()) -> {ok, [map()]} | {error, term()}.
list_routes(State, ApiId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.gateway of
                kong -> erlmcp_kong_client:list_routes(Connection, ApiId);
                apigee -> erlmcp_apigee_client.list_routes(Connection, ApiId)
            end
    end.

-spec add_plugin(#state{}, binary(), map()) -> {ok, binary()} | {error, term()}.
add_plugin(State, ApiId, PluginConfig) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.gateway of
                kong -> erlmcp_kong_client:add_plugin(Connection, ApiId, PluginConfig);
                apigee -> erlmcp_apigee_client.add_plugin(Connection, ApiId, PluginConfig)
            end
    end.

-spec remove_plugin(#state{}, binary(), binary()) -> ok | {error, term()}.
remove_plugin(State, ApiId, PluginId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.gateway of
                kong -> erlmcp_kong_client:remove_plugin(Connection, ApiId, PluginId);
                apigee -> erlmcp_apigee_client.remove_plugin(Connection, ApiId, PluginId)
            end
    end.

-spec configure_ratelimit(#state{}, binary(), map()) -> ok | {error, term()}.
configure_ratelimit(State, ApiId, RateConfig) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.gateway of
                kong -> erlmcp_kong_client:configure_ratelimit(Connection, ApiId, RateConfig);
                apigee -> erlmcp_apigee_client.configure_ratelimit(Connection, ApiId, RateConfig)
            end
    end.

-spec get_api_stats(#state{}, binary()) -> {ok, map()} | {error, term()}.
get_api_stats(State, ApiId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.gateway of
                kong -> erlmcp_kong_client:get_api_stats(Connection, ApiId);
                apigee -> erlmcp_apigee_client.get_api_stats(Connection, ApiId)
            end
    end.

-spec get_usage_metrics(#state{}, binary()) -> {ok, map()} | {error, term()}.
get_usage_metrics(State, ApiId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.gateway of
                kong -> erlmcp_kong_client:get_usage_metrics(Connection, ApiId);
                apigee -> erlmcp_apigee_client.get_usage_metrics(Connection, ApiId)
            end
    end.

-spec handle_api_event(map(), #state{}) -> {ok, #state{}} | {error, term()}.
handle_api_event(Event, State) ->
    case Event of
        {api_updated, ApiId, NewConfig} ->
            %% Update API configuration
            Apis = maps:put(ApiId, NewConfig, State#state.apis),
            {ok, State#state{apis = Apis}};
        {route_updated, RouteId, NewConfig} ->
            %% Update route configuration
            Routes = maps:put(RouteId, NewConfig, State#state.routes),
            {ok, State#state{routes = Routes}};
        _ ->
            {ok, State}
    end.

-spec handle_rate_limit_exceeded(#state{}, binary()) -> {ok, #state{}} | {error, term()}.
handle_rate_limit_exceeded(State, ApiId) ->
    %% Implement rate limit handling strategy
    case maps:get(ApiId, State#state.ratelimit, 0) of
        Count when Count > 1000 ->
            %% Notify and potentially throttle
            erlmcp_apigw_notifier:notify(ApiId, rate_limit_exceeded, Count),
            {ok, State};
        _ ->
            {ok, State}
    end.

-spec reconnect_apigw(#state{}, map()) -> {ok, #state{}} | {error, term()}.
reconnect_apigw(State, NewConfig) ->
    case State#state.connection of
        undefined ->
            case init_apigw_connection(State#state.gateway, NewConfig) of
                {ok, Connection} ->
                    {ok, State#state{connection = Connection}};
                {error, Reason} ->
                    {error, Reason}
            end;
        OldConnection ->
            erlmcp_apigw_connection:close(OldConnection),
            case init_apigw_connection(State#state.gateway, NewConfig) of
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