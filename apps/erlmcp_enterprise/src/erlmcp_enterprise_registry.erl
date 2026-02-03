%% @doc Enterprise Registry
%% Central registry for enterprise services and resources
-module(erlmcp_enterprise_registry).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([register_service/2, unregister_service/1, get_service/1, list_services/1]).
-export([register_resource/2, unregister_resource/1, get_resource/1, list_resources/1]).
-export([register_adapter/2, unregister_adapter/1, get_adapter/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    services :: map(),  % Name -> Service info
    resources :: map(),  % Name -> Resource info
    adapters :: map(),  % Type -> Adapter info
    metrics :: map()
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-spec register_service(Name :: binary(), Info :: map()) -> ok.
register_service(Name, Info) ->
    gen_server:call(?MODULE, {register_service, Name, Info}).

-spec unregister_service(Name :: binary()) -> ok.
unregister_service(Name) ->
    gen_server:call(?MODULE, {unregister_service, Name}).

-spec get_service(Name :: binary()) -> {ok, map()} | {error, not_found}.
get_service(Name) ->
    gen_server:call(?MODULE, {get_service, Name}).

-spec list_services(Type :: atom()) -> [binary()].
list_services(Type) ->
    gen_server:call(?MODULE, {list_services, Type}).

-spec register_resource(Name :: binary(), Info :: map()) -> ok.
register_resource(Name, Info) ->
    gen_server:call(?MODULE, {register_resource, Name, Info}).

-spec unregister_resource(Name :: binary()) -> ok.
unregister_resource(Name) ->
    gen_server:call(?MODULE, {unregister_resource, Name}).

-spec get_resource(Name :: binary()) -> {ok, map()} | {error, not_found}.
get_resource(Name) ->
    gen_server:call(?MODULE, {get_resource, Name}).

-spec list_resources(Type :: atom()) -> [binary()].
list_resources(Type) ->
    gen_server:call(?MODULE, {list_resources, Type}).

-spec register_adapter(Type :: atom(), Info :: map()) -> ok.
register_adapter(Type, Info) ->
    gen_server:call(?MODULE, {register_adapter, Type, Info}).

-spec unregister_adapter(Type :: atom()) -> ok.
unregister_adapter(Type) ->
    gen_server:call(?MODULE, {unregister_adapter, Type}).

-spec get_adapter(Type :: atom()) -> {ok, map()} | {error, not_found}.
get_adapter(Type) ->
    gen_server:call(?MODULE, {get_adapter, Type}).

-spec list_adapters() -> [atom()].
list_adapters() ->
    gen_server:call(?MODULE, {list_adapters}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),

    State = #state{
        services = #{},
        resources = #{},
        adapters = #{},
        metrics = #{}
    },

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call({register_service, Name, Info}, _From, State) ->
    %% Register a new service
    Services = maps:put(Name, Info, State#state.services),
    Metrics = update_metric(State#state.metrics, services_registered, 1),

    %% Notify interested parties
    erlmcp_enterprise_bus:publish(service_registered, {Name, Info}),

    {reply, ok, State#state{services = Services, metrics = Metrics}};

handle_call({unregister_service, Name}, _From, State) ->
    %% Unregister a service
    case maps:find(Name, State#state.services) of
        {ok, Info} ->
            Services = maps:remove(Name, State#state.services),
            Metrics = update_metric(State#state.metrics, services_unregistered, 1),

            %% Notify interested parties
            erlmcp_enterprise_bus:publish(service_unregistered, {Name, Info}),

            {reply, ok, State#state{services = Services, metrics = Metrics}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_service, Name}, _From, State) ->
    case maps:find(Name, State#state.services) of
        {ok, Info} ->
            {reply, {ok, Info}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({list_services, Type}, _From, State) ->
    %% Filter services by type
    Services = maps:keys(maps:filter(fun(_, Info) ->
        maps:get(type, Info, undefined) =:= Type
    end, State#state.services)),
    {reply, {ok, Services}, State};

handle_call({register_resource, Name, Info}, _From, State) ->
    %% Register a new resource
    Resources = maps:put(Name, Info, State#state.resources),
    Metrics = update_metric(State#state.metrics, resources_registered, 1),

    %% Notify interested parties
    erlmcp_enterprise_bus:publish(resource_registered, {Name, Info}),

    {reply, ok, State#state{resources = Resources, metrics = Metrics}};

handle_call({unregister_resource, Name}, _From, State) ->
    %% Unregister a resource
    case maps:find(Name, State#state.resources) of
        {ok, Info} ->
            Resources = maps:remove(Name, State#state.resources),
            Metrics = update_metric(State#state.metrics, resources_unregistered, 1),

            %% Notify interested parties
            erlmcp_enterprise_bus:publish(resource_unregistered, {Name, Info}),

            {reply, ok, State#state{resources = Resources, metrics = Metrics}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_resource, Name}, _From, State) ->
    case maps:find(Name, State#state.resources) of
        {ok, Info} ->
            {reply, {ok, Info}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({list_resources, Type}, _From, State) ->
    %% Filter resources by type
    Resources = maps:keys(maps:filter(fun(_, Info) ->
        maps:get(type, Info, undefined) =:= Type
    end, State#state.resources)),
    {reply, {ok, Resources}, State};

handle_call({register_adapter, Type, Info}, _From, State) ->
    %% Register a new adapter
    Adapters = maps:put(Type, Info, State#state.adapters),
    Metrics = update_metric(State#state.metrics, adapters_registered, 1),

    %% Notify interested parties
    erlmcp_enterprise_bus:publish(adapter_registered, {Type, Info}),

    {reply, ok, State#state{adapters = Adapters, metrics = Metrics}};

handle_call({unregister_adapter, Type}, _From, State) ->
    %% Unregister an adapter
    case maps:find(Type, State#state.adapters) of
        {ok, Info} ->
            Adapters = maps:remove(Type, State#state.adapters),
            Metrics = update_metric(State#state.metrics, adapters_unregistered, 1),

            %% Notify interested parties
            erlmcp_enterprise_bus:publish(adapter_unregistered, {Type, Info}),

            {reply, ok, State#state{adapters = Adapters, metrics = Metrics}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_adapter, Type}, _From, State) ->
    case maps:find(Type, State#state.adapters) of
        {ok, Info} ->
            {reply, {ok, Info}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({list_adapters}, _From, State) ->
    {reply, {ok, maps:keys(State#state.adapters)}, State};

handle_call(get_metrics, _From, State) ->
    {reply, {ok, State#state.metrics}, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec update_metric(map(), atom(), integer()) -> map().
update_metric(Metrics, Key, Inc) ->
    Current = maps:get(Key, Metrics, 0),
    Metrics#{Key => Current + Inc}.