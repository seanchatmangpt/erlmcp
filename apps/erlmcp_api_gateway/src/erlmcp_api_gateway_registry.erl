-module(erlmcp_api_gateway_registry).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    create_api/1, get_api/1, update_api/1, delete_api/1, list_apis/0,
    create_route/2, get_route/2, update_route/2, delete_route/2, list_routes/1,
    create_consumer/1, get_consumer/1, update_consumer/1, delete_consumer/1, list_consumers/0,
    create_plugin/1, get_plugin/1, update_plugin/1, delete_plugin/1, list_plugins/0
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{apis => #{}, routes => #{}, consumers => #{}, plugins => #{}}}.

create_api(Api) ->
    gen_server:call(?MODULE, {create_api, Api}).

get_api(ApiId) ->
    gen_server:call(?MODULE, {get_api, ApiId}).

update_api(Api) ->
    gen_server:call(?MODULE, {update_api, Api}).

delete_api(ApiId) ->
    gen_server:call(?MODULE, {delete_api, ApiId}).

list_apis() ->
    gen_server:call(?MODULE, list_apis).

create_route(ApiId, Route) ->
    gen_server:call(?MODULE, {create_route, ApiId, Route}).

get_route(ApiId, RouteId) ->
    gen_server:call(?MODULE, {get_route, ApiId, RouteId}).

update_route(ApiId, Route) ->
    gen_server:call(?MODULE, {update_route, ApiId, Route}).

delete_route(ApiId, RouteId) ->
    gen_server:call(?MODULE, {delete_route, ApiId, RouteId}).

list_routes(ApiId) ->
    gen_server:call(?MODULE, {list_routes, ApiId}).

create_consumer(Consumer) ->
    gen_server:call(?MODULE, {create_consumer, Consumer}).

get_consumer(ConsumerId) ->
    gen_server:call(?MODULE, {get_consumer, ConsumerId}).

update_consumer(Consumer) ->
    gen_server:call(?MODULE, {update_consumer, Consumer}).

delete_consumer(ConsumerId) ->
    gen_server:call(?MODULE, {delete_consumer, ConsumerId}).

list_consumers() ->
    gen_server:call(?MODULE, list_consumers).

create_plugin(Plugin) ->
    gen_server:call(?MODULE, {create_plugin, Plugin}).

get_plugin(PluginId) ->
    gen_server:call(?MODULE, {get_plugin, PluginId}).

update_plugin(Plugin) ->
    gen_server:call(?MODULE, {update_plugin, Plugin}).

delete_plugin(PluginId) ->
    gen_server:call(?MODULE, {delete_plugin, PluginId}).

list_plugins() ->
    gen_server:call(?MODULE, list_plugins).

handle_call({create_api, Api}, _From, State) ->
    ApiId = maps:get(id, Api),
    NewApis = maps:put(ApiId, Api, State#{apis}),
    {reply, {ok, Api}, NewApis};

handle_call({get_api, ApiId}, _From, State) ->
    case maps:find(ApiId, State#{apis}) of
        {ok, Api} ->
            {reply, {ok, Api}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({update_api, Api}, _From, State) ->
    ApiId = maps:get(id, Api),
    NewApis = maps:put(ApiId, Api, State#{apis}),
    {reply, {ok, Api}, NewApis};

handle_call({delete_api, ApiId}, _From, State) ->
    NewApis = maps:remove(ApiId, State#{apis}),
    NewRoutes = maps:remove(ApiId, State#{routes}),
    {reply, ok, NewApis#{routes => NewRoutes}};

handle_call(list_apis, _From, State) ->
    Apis = maps:values(State#{apis}),
    {reply, Apis, State};

handle_call({create_route, ApiId, Route}, _From, State) ->
    RouteId = maps:get(id, Route),
    Routes = case maps:find(ApiId, State#{routes}) of
        {ok, ApiRoutes} ->
            maps:put(RouteId, Route, ApiRoutes);
        error ->
            maps:put(RouteId, Route, #{})
    end,
    NewRoutes = maps:put(ApiId, Routes, State#{routes}),
    {reply, {ok, Route}, NewRoutes#{routes => NewRoutes}};

handle_call({get_route, ApiId, RouteId}, _From, State) ->
    case maps:find(ApiId, State#{routes}) of
        {ok, Routes} ->
            case maps:find(RouteId, Routes) of
                {ok, Route} ->
                    {reply, {ok, Route}, State};
                error ->
                    {reply, {error, not_found}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({update_route, ApiId, Route}, _From, State) ->
    RouteId = maps:get(id, Route),
    Routes = case maps:find(ApiId, State#{routes}) of
        {ok, ApiRoutes} ->
            maps:put(RouteId, Route, ApiRoutes);
        error ->
            maps:put(RouteId, Route, #{})
    end,
    NewRoutes = maps:put(ApiId, Routes, State#{routes}),
    {reply, {ok, Route}, NewRoutes#{routes => NewRoutes}};

handle_call({delete_route, ApiId, RouteId}, _From, State) ->
    NewRoutes = case maps:find(ApiId, State#{routes}) of
        {ok, Routes} ->
            NewApiRoutes = maps:remove(RouteId, Routes),
            case maps:size(NewApiRoutes) of
                0 -> maps:remove(ApiId, State#{routes});
                _ -> maps:put(ApiId, NewApiRoutes, State#{routes})
            end;
        error ->
            State#{routes}
    end,
    {reply, ok, NewRoutes};

handle_call({list_routes, ApiId}, _From, State) ->
    case maps:find(ApiId, State#{routes}) of
        {ok, Routes} ->
            RoutesList = maps:values(Routes),
            {reply, RoutesList, State};
        error ->
            {reply, [], State}
    end;

handle_call({create_consumer, Consumer}, _From, State) ->
    ConsumerId = maps:get(id, Consumer),
    NewConsumers = maps:put(ConsumerId, Consumer, State#{consumers}),
    {reply, {ok, Consumer}, NewConsumers};

handle_call({get_consumer, ConsumerId}, _From, State) ->
    case maps:find(ConsumerId, State#{consumers}) of
        {ok, Consumer} ->
            {reply, {ok, Consumer}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({update_consumer, Consumer}, _From, State) ->
    ConsumerId = maps:get(id, Consumer),
    NewConsumers = maps:put(ConsumerId, Consumer, State#{consumers}),
    {reply, {ok, Consumer}, NewConsumers};

handle_call({delete_consumer, ConsumerId}, _From, State) ->
    NewConsumers = maps:remove(ConsumerId, State#{consumers}),
    {reply, ok, NewConsumers};

handle_call(list_consumers, _From, State) ->
    Consumers = maps:values(State#{consumers}),
    {reply, Consumers, State};

handle_call({create_plugin, Plugin}, _From, State) ->
    PluginId = maps:get(id, Plugin),
    NewPlugins = maps:put(PluginId, Plugin, State#{plugins}),
    {reply, {ok, Plugin}, NewPlugins};

handle_call({get_plugin, PluginId}, _From, State) ->
    case maps:find(PluginId, State#{plugins}) of
        {ok, Plugin} ->
            {reply, {ok, Plugin}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({update_plugin, Plugin}, _From, State) ->
    PluginId = maps:get(id, Plugin),
    NewPlugins = maps:put(PluginId, Plugin, State#{plugins}),
    {reply, {ok, Plugin}, NewPlugins};

handle_call({delete_plugin, PluginId}, _From, State) ->
    NewPlugins = maps:remove(PluginId, State#{plugins}),
    {reply, ok, NewPlugins};

handle_call(list_plugins, _From, State) ->
    Plugins = maps:values(State#{plugins}),
    {reply, Plugins, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.