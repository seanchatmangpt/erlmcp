%% @doc Enterprise Integration Gateway
%% Central hub for all enterprise integrations with routing, transformation, and protocol translation
-module(erlmcp_integration_gateway).

-behaviour(gen_server).

-export([start_link/0, route_message/2, transform_message/3, translate_protocol/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type integration_type() :: identity | monitoring | logging | bi | esb | dwh |
                           devops | api | cloud | security | config | container.
-type protocol_type() :: json | xml | soap | rest | grpc | jms | amqp.
-type routing_key() :: binary().
-type transformation_rule() :: map().
-type protocol_config() :: map().

-record(state, {
    registry :: map(), % Integration type to adapter mappings
    transformers :: map(), % Protocol transformation rules
    routes :: map(), % Message routing rules
    metrics :: map() % Performance metrics
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Route a message through the integration gateway
-spec route_message(routing_key(), map()) -> {ok, pid()} | {error, term()}.
route_message(Key, Message) ->
    gen_server:call(?MODULE, {route_message, Key, Message}).

%% Transform a message between protocols
-spec transform_message(map(), protocol_type(), protocol_type()) -> {ok, map()} | {error, term()}.
transform_message(Message, FromProtocol, ToProtocol) ->
    gen_server:call(?MODULE, {transform_message, Message, FromProtocol, ToProtocol}).

%% Translate between protocols
-spec translate_protocol(map(), protocol_config()) -> {ok, map()} | {error, term()}.
translate_protocol(Message, Config) ->
    gen_server:call(?MODULE, {translate_protocol, Message, Config}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize state with empty registries
    State = #state{
        registry = #{},
        transformers = load_transformation_rules(),
        routes = load_routing_rules(),
        metrics = #{}
    },

    %% Register all integration adapters
    register_integration_adapters(State),

    %% Start message routing loop
    erlmcp_gateway_router:start(),

    {ok, State}.

handle_call({route_message, Key, Message}, _From, State) ->
    try
        %% Find the appropriate integration adapter
        IntegrationType = determine_integration_type(Key),
        AdapterPid = find_adapter(IntegrationType, State),

        %% Route the message
        Response = erlmcp_adapter:send(AdapterPid, Message),

        %% Update metrics
        NewMetrics = update_metrics(State#state.metrics, route, 1),

        {reply, {ok, Response}, State#state{metrics = NewMetrics}}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to route message: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({transform_message, Message, FromProtocol, ToProtocol}, _From, State) ->
    try
        %% Get transformation rule
        Rule = get_transformation_rule(FromProtocol, ToProtocol, State),

        %% Apply transformation
        Transformed = apply_transformation(Message, Rule),

        %% Update metrics
        NewMetrics = update_metrics(State#state.metrics, transform, 1),

        {reply, {ok, Transformed}, State#state{metrics = NewMetrics}}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to transform message: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({translate_protocol, Message, Config}, _From, State) ->
    try
        %% Translate protocol
        Translated = translate_data(Message, Config),

        %% Update metrics
        NewMetrics = update_metrics(State#state.metrics, translate, 1),

        {reply, {ok, Translated}, State#state{metrics = NewMetrics}}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to translate protocol: ~p:~p", [Error, Reason]),
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

register_integration_adapters(State) ->
    %% Register all available integration adapters
    Adapters = [
        {identity, erlmcp_identity_adapter},
        {monitoring, erlmcp_monitoring_adapter},
        {logging, erlmcp_logging_adapter},
        {bi, erlmcp_bi_adapter},
        {esb, erlmcp_esb_adapter},
        {dwh, erlmcp_dwh_adapter},
        {devops, erlmcp_devops_adapter},
        {api, erlmcp_api_adapter},
        {cloud, erlmcp_cloud_adapter},
        {security, erlmcp_security_adapter},
        {config, erlmcp_config_adapter},
        {container, erlmcp_container_adapter}
    ],

    lists:foreach(fun({Type, Module}) ->
        case erlmcp_adapter_registry:register(Type, Module) of
            ok -> ?LOG_INFO("Registered ~p adapter", [Type]);
            {error, Reason} -> ?LOG_ERROR("Failed to register ~p adapter: ~p", [Type, Reason])
        end
    end, Adapters),

    State.

determine_integration_type(Key) ->
    %% Parse the routing key to determine integration type
    case binary:split(Key, <<":">>) of
        [<<"identity">>, _] -> identity;
        [<<"monitoring">>, _] -> monitoring;
        [<<"logging">>, _] -> logging;
        [<<"bi">>, _] -> bi;
        [<<"esb">>, _] -> esb;
        [<<"dwh">>, _] -> dwh;
        [<<"devops">>, _] -> devops;
        [<<"api">>, _] -> api;
        [<<"cloud">>, _] -> cloud;
        [<<"security">>, _] -> security;
        [<<"config">>, _] -> config;
        [<<"container">>, _] -> container;
        _ -> throw({unknown_integration_key, Key})
    end.

find_adapter(IntegrationType, State) ->
    %% Find the appropriate adapter for the integration type
    case erlmcp_adapter_registry:get_pid(IntegrationType) of
        {ok, Pid} -> Pid;
        {error, not_found} -> throw({adapter_not_found, IntegrationType})
    end.

load_transformation_rules() ->
    %% Load protocol transformation rules from configuration
    #{json_to_xml => json_to_xml_rule(),
      xml_to_json => xml_to_json_rule(),
      soap_to_rest => soap_to_rest_rule(),
      rest_to_grpc => rest_to_grpc_rule()}.

load_routing_rules() ->
    %% Load message routing rules
    #{default => default_routing_rule(),
      high_priority => high_priority_routing_rule(),
      batch => batch_routing_rule()}.

get_transformation_rule(FromProtocol, ToProtocol, State) ->
    %% Get the appropriate transformation rule
    Key = list_to_binary([atom_to_list(FromProtocol), "_to_", atom_to_list(ToProtocol)]),
    maps:get(Key, State#state.transformers, default_transformation_rule()).

apply_transformation(Message, Rule) ->
    %% Apply the transformation rule to the message
    case maps:get(type, Rule, simple) of
        simple -> apply_simple_transformation(Message, Rule);
        complex -> apply_complex_transformation(Message, Rule)
    end.

translate_data(Message, Config) ->
    %% Translate data according to protocol configuration
    Protocol = maps:get(protocol, Config, json),
    Schema = maps:get(schema, Config, undefined),

    %% Apply schema validation if specified
    case Schema of
        undefined -> Message;
        _ -> validate_and_transform(Message, Schema, Protocol)
    end.

update_metrics(Metrics, Type, Inc) ->
    %% Update performance metrics
    Key = atom_to_binary(Type, utf8),
    maps:update_with(Key, fun(V) -> V + Inc end, Inc, Metrics).

%%====================================================================
%% Transformation Rules
%%====================================================================

json_to_xml_rule() ->
    #{type => simple,
      root => <<"data">>,
      mapping => #{}}.

xml_to_json_rule() ->
    #{type => simple,
      root => <<"data">>,
      mapping => #{}}.

soap_to_rest_rule() ->
    #{type => complex,
      mapping => #{<<"Envelope">> => <<"payload">>},
      headers => #{<<"SOAPAction">> => <<"action">>}}.

rest_to_grpc_rule() ->
    #{type => complex,
      mapping => #{},
      proto_file => <<"service.proto">>}.

default_transformation_rule() ->
    #{type => simple,
      mapping => #{}}.

%%====================================================================
%% Routing Rules
%%====================================================================

default_routing_rule() ->
    #{priority => normal,
      timeout => 5000,
      retries => 3}.

high_priority_routing_rule() ->
    #{priority => high,
      timeout => 1000,
      retries => 1}.

batch_routing_rule() ->
    #{priority => low,
      timeout => 30000,
      retries => 5,
      batch_size => 100}.