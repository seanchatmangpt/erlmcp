%% @doc Enterprise Integration Adapter
%% Dynamic adapter for enterprise system integrations
-module(erlmcp_enterprise_adapter).
-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    type :: atom(),  % adapter type (identity, monitoring, logging, etc.)
    config :: map(),
    connection :: pid() | undefined,
    metrics :: map()
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(Type :: atom(), Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Type, Config) ->
    gen_server:start_link({local, adapter_name(Type)}, ?MODULE, [Type, Config], []).

-spec stop(Ref :: pid() | atom()) -> ok.
stop(Ref) ->
    gen_server:stop(Ref).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom(), map()]) -> {ok, #state{}} | {stop, term()}.
init([Type, Config]) ->
    process_flag(trap_exit, true),
    State = #state{type = Type, config = Config, metrics = #{}},

    %% Initialize adapter based on type
    case initialize_adapter(Type, Config) of
        {ok, Connection} ->
            {ok, State#state{connection = Connection}};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(get_status, _From, State) ->
    Status = get_adapter_status(State),
    {reply, Status, State};

handle_call(get_metrics, _From, State) ->
    {reply, State#state.metrics, State};

handle_call(config, _From, State) ->
    {reply, State#state.config, State};

handle_call({test_connection, TestParams}, _From, State) ->
    Result = test_adapter_connection(State, TestParams),
    {reply, Result, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({send_data, Data}, State) ->
    case send_adapter_data(State, Data) of
        ok ->
            Metrics = update_metric(State#state.metrics, messages_sent, 1),
            {noreply, State#state{metrics = Metrics}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to send data: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast({update_config, NewConfig}, State) ->
    case reconfigure_adapter(State, NewConfig) of
        ok ->
            {noreply, State#state{config = NewConfig}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to reconfigure adapter: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info({adapter_data, Data}, State) ->
    %% Process incoming data from adapter
    Metrics = update_metric(State#state.metrics, messages_received, 1),
    notify_system(Data),
    {noreply, State#state{metrics = Metrics}};

handle_info({adapter_error, Error}, State) ->
    ?LOG_ERROR("Adapter error: ~p", [Error]),
    %% Attempt to recover or notify monitoring
    case handle_adapter_error(State, Error) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, _} ->
            {stop, adapter_error, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    cleanup_adapter(State).

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec adapter_name(atom()) -> atom().
adapter_name(Type) ->
    list_to_atom("adapter_" ++ atom_to_list(Type) ++ "_worker").

-spec initialize_adapter(atom(), map()) -> {ok, pid()} | {error, term()}.
initialize_adapter(identity, Config) ->
    erlmcp_identity_adapter:init(Config);
initialize_adapter(monitoring, Config) ->
    erlmcp_monitoring_adapter:init(Config);
initialize_adapter(logging, Config) ->
    erlmcp_logging_adapter:init(Config);
initialize_adapter(business_intel, Config) ->
    erlmcp_bizintel_adapter:init(Config);
initialize_adapter(service_bus, Config) ->
    erlmcp_servicebus_adapter:init(Config);
initialize_adapter(data_warehouse, Config) ->
    erlmcp_data_adapter:init(Config);
initialize_adapter(devops, Config) ->
    erlmcp_devops_adapter:init(Config);
initialize_adapter(api_gateway, Config) ->
    erlmcp_apigw_adapter:init(Config);
initialize_adapter(cloud, Config) ->
    erlmcp_cloud_adapter:init(Config);
initialize_adapter(security, Config) ->
    erlmcp_security_adapter:init(Config);
initialize_adapter(config_mgmt, Config) ->
    erlmcp_config_adapter:init(Config);
initialize_adapter(container, Config) ->
    erlmcp_container_adapter:init(Config);
initialize_adapter(Other, _Config) ->
    {error, {unsupported_adapter, Other}}.

-spec get_adapter_status(#state{}) -> map().
get_adapter_status(State) ->
    #{
        type => State#state.type,
        connected => State#state.connection =/= undefined,
        metrics => State#state.metrics,
        config => State#state.config
    }.

-spec send_adapter_data(#state{}, term()) -> ok | {error, term()}.
send_adapter_data(State, Data) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            erlmcp_enterprise_connection:send(Connection, Data)
    end.

-spec test_adapter_connection(#state{}, map()) -> {ok, map()} | {error, term()}.
test_adapter_connection(State, TestParams) ->
    erlmcp_enterprise_testing:test_adapter(State#state.type, State#state.config, TestParams).

-spec reconfigure_adapter(#state{}, map()) -> ok | {error, term()}.
reconfigure_adapter(State, NewConfig) ->
    case initialize_adapter(State#state.type, NewConfig) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec handle_adapter_error(#state{}, term()) -> {ok, #state{}} | {error, term()}.
handle_adapter_error(State, Error) ->
    %% Implement error recovery strategy
    case Error of
        connection_lost ->
            %% Attempt to reconnect
            case initialize_adapter(State#state.type, State#state.config) of
                {ok, Connection} ->
                    {ok, State#state{connection = Connection}};
                {error, _} ->
                    {error, recovery_failed}
            end;
        _ ->
            {error, unknown_error}
    end.

-spec notify_system(term()) -> ok.
notify_system(Data) ->
    %% Publish to enterprise bus
    erlmcp_enterprise_bus:publish(adapter_data, Data).

-spec update_metric(map(), atom(), integer()) -> map().
update_metric(Metrics, Key, Inc) ->
    Current = maps:get(Key, Metrics, 0),
    Metrics#{Key => Current + Inc}.

-spec cleanup_adapter(#state{}) -> ok.
cleanup_adapter(State) ->
    case State#state.connection of
        undefined ->
            ok;
        Connection ->
            erlmcp_enterprise_connection:close(Connection)
    end.