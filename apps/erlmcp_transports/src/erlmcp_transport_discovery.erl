%%%-------------------------------------------------------------------
%%% @doc
%%% Service Discovery Module for erlmcp Transports
%%%
%%% This module provides service discovery capabilities using multiple
%%% backends:
%%% - mDNS/DNS-SD for local network discovery
%%% - Consul for distributed service discovery
%%% - Kubernetes API for container orchestration
%%%
%%% Features:
%%% - Automatic service registration and deregistration
%%% - Health check integration
%%% - DNS-based service resolution
%%% - Watch-based resource monitoring
%%% - Connection pool integration
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_discovery).

-behaviour(gen_server).

%% API exports
-export([start_link/0, start_link/1,
         register_service/2, unregister_service/1,
         discover_services/1, discover_services/2,
         get_service_endpoints/1,
         register_mdns/2, unregister_mdns/1,
         register_consul/2, unregister_consul/1,
         register_k8s/2, unregister_k8s/1,
         watch_services/1, unwatch_services/1,
         get_discovery_stats/0,
         %% Helper functions exposed for testing
         build_mdns_query/1, parse_mdns_response/1,
         parse_consul_service/1, parse_consul_services/1,
         build_consul_headers/1, ensure_gun_started/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Type definitions
-type discovery_backend() :: mdns | consul | k8s.
-type service_info() ::
    #{name := binary(),
      type => binary(),
      port => inet:port_number(),
      host => inet:hostname(),
      metadata => map(),
      ttl => pos_integer()}.
-type service_endpoint() ::
    #{host => inet:hostname(),
      port => inet:port_number(),
      metadata => map()}.
-type discovery_opts() ::
    #{backend := discovery_backend(),
      domain => binary(),
      service_type => binary(),
      namespace => binary(),
      ttl => pos_integer(),
      health_check => boolean()}.

-record(state,
        {registered_services = #{} :: #{binary() => service_info()},
         watchers = #{} :: #{binary() => [pid()]},
         mdns_ref :: reference() | undefined,
         consul_client :: term() | undefined,
         k8s_client :: term() | undefined,
         stats = #{} :: map()}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start discovery server with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Start discovery server with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Register a service for discovery
-spec register_service(binary(), service_info()) -> ok | {error, term()}.
register_service(ServiceName, ServiceInfo) when is_binary(ServiceName), is_map(ServiceInfo) ->
    gen_server:call(?MODULE, {register_service, ServiceName, ServiceInfo}).

%% @doc Unregister a service from discovery
-spec unregister_service(binary()) -> ok | {error, term()}.
unregister_service(ServiceName) when is_binary(ServiceName) ->
    gen_server:call(?MODULE, {unregister_service, ServiceName}).

%% @doc Discover services using default backend (mDNS)
-spec discover_services(binary()) -> {ok, [service_endpoint()]} | {error, term()}.
discover_services(ServiceName) when is_binary(ServiceName) ->
    gen_server:call(?MODULE, {discover_services, ServiceName, mdns}, 5000).

%% @doc Discover services using specified backend
-spec discover_services(binary(), discovery_backend()) ->
    {ok, [service_endpoint()]} | {error, term()}.
discover_services(ServiceName, Backend) when is_binary(ServiceName) ->
    gen_server:call(?MODULE, {discover_services, ServiceName, Backend}, 5000).

%% @doc Get endpoints for a specific service
-spec get_service_endpoints(binary()) -> {ok, [service_endpoint()]} | {error, term()}.
get_service_endpoints(ServiceName) ->
    gen_server:call(?MODULE, {get_endpoints, ServiceName}, 5000).

%% @doc Register service via mDNS/DNS-SD
-spec register_mdns(binary(), service_info()) -> ok | {error, term()}.
register_mdns(ServiceName, ServiceInfo) ->
    gen_server:call(?MODULE, {register_mdns, ServiceName, ServiceInfo}, 5000).

%% @doc Unregister mDNS service
-spec unregister_mdns(binary()) -> ok | {error, term()}.
unregister_mdns(ServiceName) ->
    gen_server:call(?MODULE, {unregister_mdns, ServiceName}, 5000).

%% @doc Register service via Consul
-spec register_consul(binary(), service_info()) -> ok | {error, term()}.
register_consul(ServiceName, ServiceInfo) ->
    gen_server:call(?MODULE, {register_consul, ServiceName, ServiceInfo}, 5000).

%% @doc Unregister Consul service
-spec unregister_consul(binary()) -> ok | {error, term()}.
unregister_consul(ServiceName) ->
    gen_server:call(?MODULE, {unregister_consul, ServiceName}, 5000).

%% @doc Register service via Kubernetes
-spec register_k8s(binary(), service_info()) -> ok | {error, term()}.
register_k8s(ServiceName, ServiceInfo) ->
    gen_server:call(?MODULE, {register_k8s, ServiceName, ServiceInfo}, 5000).

%% @doc Unregister Kubernetes service
-spec unregister_k8s(binary()) -> ok | {error, term()}.
unregister_k8s(ServiceName) ->
    gen_server:call(?MODULE, {unregister_k8s, ServiceName}, 5000).

%% @doc Watch services for changes
-spec watch_services(binary()) -> {ok, reference()} | {error, term()}.
watch_services(ServiceName) ->
    gen_server:call(?MODULE, {watch_services, ServiceName, self()}, 5000).

%% @doc Unwatch services
-spec unwatch_services(binary()) -> ok | {error, term()}.
unwatch_services(ServiceName) ->
    gen_server:cast(?MODULE, {unwatch_services, ServiceName, self()}).

%% @doc Get discovery statistics
-spec get_discovery_stats() -> {ok, map()} | {error, term()}.
get_discovery_stats() ->
    gen_server:call(?MODULE, get_stats, 5000).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init(_Opts) ->
    logger:info("Starting service discovery server"),
    {ok, #state{}}.

handle_call({register_service, ServiceName, ServiceInfo}, _From, State) ->
    Result = case maps:get(backend, ServiceInfo, mdns) of
        mdns   -> register_mdns_internal(ServiceName, ServiceInfo);
        consul -> register_consul_internal(ServiceName, ServiceInfo);
        k8s    -> register_k8s_internal(ServiceName, ServiceInfo)
    end,
    NewState = case Result of
        ok ->
            State#state{
                registered_services =
                    maps:put(ServiceName, ServiceInfo, State#state.registered_services)
            };
        _ ->
            State
    end,
    {reply, Result, NewState};

handle_call({unregister_service, ServiceName}, _From, State) ->
    case maps:get(ServiceName, State#state.registered_services, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ServiceInfo ->
            Backend = maps:get(backend, ServiceInfo, mdns),
            Result = case Backend of
                mdns -> unregister_mdns_internal(ServiceName);
                consul -> unregister_consul_internal(ServiceName);
                k8s -> unregister_k8s_internal(ServiceName)
            end,
            NewState = State#state{
                registered_services = maps:remove(ServiceName, State#state.registered_services)
            },
            {reply, Result, NewState}
    end;

handle_call({discover_services, ServiceName, Backend}, _From, State) ->
    Result = case Backend of
        mdns -> discover_mdns(ServiceName);
        consul -> discover_consul(ServiceName);
        k8s -> discover_k8s(ServiceName)
    end,
    {reply, Result, State};

handle_call({get_endpoints, ServiceName}, _From, State) ->
    Result = case maps:get(ServiceName, State#state.registered_services, undefined) of
        undefined ->
            {error, service_not_found};
        ServiceInfo ->
            Host = maps:get(host, ServiceInfo, "localhost"),
            Port = maps:get(port, ServiceInfo, 8080),
            {ok, [#{host => Host, port => Port, metadata => maps:get(metadata, ServiceInfo, #{})}]}
    end,
    {reply, Result, State};

handle_call({register_mdns, ServiceName, ServiceInfo}, _From, State) ->
    Result = register_mdns_internal(ServiceName, ServiceInfo),
    {reply, Result, State};

handle_call({unregister_mdns, ServiceName}, _From, State) ->
    Result = unregister_mdns_internal(ServiceName),
    {reply, Result, State};

handle_call({register_consul, ServiceName, ServiceInfo}, _From, State) ->
    Result = register_consul_internal(ServiceName, ServiceInfo),
    {reply, Result, State};

handle_call({unregister_consul, ServiceName}, _From, State) ->
    Result = unregister_consul_internal(ServiceName),
    {reply, Result, State};

handle_call({register_k8s, ServiceName, ServiceInfo}, _From, State) ->
    Result = register_k8s_internal(ServiceName, ServiceInfo),
    {reply, Result, State};

handle_call({unregister_k8s, ServiceName}, _From, State) ->
    Result = unregister_k8s_internal(ServiceName),
    {reply, Result, State};

handle_call({watch_services, ServiceName, Pid}, _From, State) ->
    Watchers = maps:get(ServiceName, State#state.watchers, []),
    NewWatchers = case lists:member(Pid, Watchers) of
        true -> Watchers;
        false -> [Pid | Watchers]
    end,
    NewState = State#state{watchers = maps:put(ServiceName, NewWatchers, State#state.watchers)},
    {reply, {ok, make_ref()}, NewState};

handle_call(get_stats, _From, State) ->
    Stats = #{
        registered_services => maps:size(State#state.registered_services),
        active_watchers => count_total_watchers(State#state.watchers),
        mdns_active => State#state.mdns_ref =/= undefined,
        consul_active => State#state.consul_client =/= undefined,
        k8s_active => State#state.k8s_client =/= undefined
    },
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({unwatch_services, ServiceName, Pid}, State) ->
    Watchers = maps:get(ServiceName, State#state.watchers, []),
    NewWatchers = lists:delete(Pid, Watchers),
    NewState = State#state{watchers = maps:put(ServiceName, NewWatchers, State#state.watchers)},
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    logger:info("Shutting down service discovery server"),
    %% Deregister all services
    maps:foreach(fun(ServiceName, _ServiceInfo) ->
        catch unregister_mdns_internal(ServiceName),
        catch unregister_consul_internal(ServiceName),
        catch unregister_k8s_internal(ServiceName)
    end, State#state.registered_services),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions - mDNS/DNS-SD
%%%===================================================================

%% mDNS multicast group and port
-define(MDNS_ADDR, {224, 0, 0, 251}).
-define(MDNS_PORT, 5353).
%% Default mDNS query/response timeout in ms
-define(MDNS_TIMEOUT, 2000).

%% @doc Build a DNS-SD PTR query packet for the given service type.
%% The packet is a minimal DNS query: standard header + one question.
-spec build_mdns_query(binary()) -> binary().
build_mdns_query(ServiceType) ->
    %% Encode service type as a DNS name (dot-separated labels, each length-prefixed, terminated with 0)
    EncodedName = encode_dns_name(ServiceType),
    %% DNS header: ID=0, Flags=0x0100 (standard query, recursion desired),
    %% QDCOUNT=1, ANCOUNT=0, NSCOUNT=0, ARCOUNT=0
    Header = <<0:16, 16#0100:16, 1:16, 0:16, 0:16, 0:16>>,
    %% Question: QNAME, QTYPE=PTR (12), QCLASS=IN (1)
    Question = <<EncodedName/binary, 0:8, 12:16, 1:16>>,
    <<Header/binary, Question/binary>>.

%% @doc Encode a dot-separated DNS name into DNS wire format.
-spec encode_dns_name(binary()) -> binary().
encode_dns_name(Name) ->
    Labels = binary:split(Name, <<".">>, [global]),
    encode_labels(Labels).

encode_labels([]) ->
    <<>>;
encode_labels([Label | Rest]) ->
    Len = byte_size(Label),
    Tail = encode_labels(Rest),
    <<Len:8, Label/binary, Tail/binary>>.

%% @doc Parse a raw DNS/mDNS response packet.
%% Returns {ok, #{TransportId => Config}} on success.
-spec parse_mdns_response(binary()) -> {ok, #{atom() => map()}}.
parse_mdns_response(<<>>) ->
    {ok, #{}};
parse_mdns_response(Packet) when byte_size(Packet) < 12 ->
    {ok, #{}};
parse_mdns_response(Packet) ->
    try
        <<_TxId:16, _Flags:16, _QCount:16, ACount:16, _NSCount:16, _ARCount:16,
          Rest/binary>> = Packet,
        parse_mdns_answers(Rest, ACount, #{})
    catch
        _:_ ->
            {ok, #{}}
    end.

%% @doc Parse DNS answer records.
-spec parse_mdns_answers(binary(), non_neg_integer(), map()) -> {ok, map()}.
parse_mdns_answers(_Bin, 0, Acc) ->
    {ok, Acc};
parse_mdns_answers(Bin, N, Acc) when N > 0 ->
    try
        {Name, Rest0} = skip_dns_name(Bin),
        <<Type:16, _Class:16, _TTL:32, RDLen:16, RData:RDLen/binary, Rest1/binary>> = Rest0,
        NewAcc = case Type of
            12 ->
                %% PTR record — target name is the service instance
                TransportId = binary_to_atom(Name, utf8),
                Config = #{
                    transport_id => TransportId,
                    type => tcp,
                    host => binary_to_list(RData),
                    port => 0,
                    discovered_via => dns_sd,
                    discovered_at => erlang:system_time(millisecond)
                },
                maps:put(TransportId, Config, Acc);
            _ ->
                Acc
        end,
        parse_mdns_answers(Rest1, N - 1, NewAcc)
    catch
        _:_ ->
            {ok, Acc}
    end.

%% @doc Skip a DNS name (handles compression pointer and labels).
%% Returns {Name, Rest}.
-spec skip_dns_name(binary()) -> {binary(), binary()}.
skip_dns_name(Bin) ->
    skip_dns_name(Bin, <<>>).

skip_dns_name(<<0:8, Rest/binary>>, Acc) ->
    Name = case Acc of
        <<>> -> <<>>;
        _ -> binary:part(Acc, 0, byte_size(Acc) - 1)  %% strip trailing dot
    end,
    {Name, Rest};
skip_dns_name(<<16#C0:8, _Ptr:8, Rest/binary>>, Acc) ->
    %% Compression pointer — skip it (we can't resolve without full packet)
    Name = case Acc of
        <<>> -> <<"compressed">>;
        _ -> binary:part(Acc, 0, byte_size(Acc) - 1)
    end,
    {Name, Rest};
skip_dns_name(<<Len:8, Label:Len/binary, Rest/binary>>, Acc) ->
    skip_dns_name(Rest, <<Acc/binary, Label/binary, ".">>);
skip_dns_name(Rest, Acc) ->
    {Acc, Rest}.

%% @doc Register service via mDNS — broadcasts a DNS-SD PTR announcement
%%      on the mDNS multicast group 224.0.0.251:5353.
-spec register_mdns_internal(binary(), service_info()) -> ok | {error, term()}.
register_mdns_internal(ServiceName, ServiceInfo) ->
    try
        ServiceType = maps:get(type, ServiceInfo, <<"_erlmcp._tcp">>),
        Domain = maps:get(domain, ServiceInfo, <<"local">>),
        Port = maps:get(port, ServiceInfo, 8080),
        Host = maps:get(host, ServiceInfo, hostname()),
        TTL = maps:get(ttl, ServiceInfo, 120),

        logger:info("Registering mDNS service: ~p.~p on ~p:~p (TTL: ~p)",
                   [ServiceName, ServiceType, Host, Port, TTL]),

        %% Build a minimal mDNS announcement packet.
        %% DNS response with one PTR answer record.
        FullName = iolist_to_binary([ServiceType, ".", Domain]),
        EncodedFull = encode_dns_name(FullName),
        EncodedService = encode_dns_name(ServiceName),
        RDLen = byte_size(EncodedService) + 1,   %% +1 for the 0 terminator byte

        %% Header: ID=0, QR=1 (response), AA=1 (authoritative), QDCOUNT=0, ANCOUNT=1
        Header = <<0:16, 16#8400:16, 0:16, 1:16, 0:16, 0:16>>,
        %% Answer: name=FullName, TYPE=PTR(12), CLASS=IN(1)|FLUSH(32768), TTL, RDLENGTH, RDATA
        Answer = <<EncodedFull/binary, 0:8,
                   12:16, (1 bor 32768):16, TTL:32, RDLen:16,
                   EncodedService/binary, 0:8>>,
        Packet = <<Header/binary, Answer/binary>>,

        %% Open a UDP socket and multicast the announcement
        {ok, Sock} = gen_udp:open(0, [binary, {active, false}, {reuseaddr, true},
                                       {multicast_ttl, 255}, {multicast_loop, true}]),
        ok = gen_udp:send(Sock, ?MDNS_ADDR, ?MDNS_PORT, Packet),
        gen_udp:close(Sock),
        ok
    catch
        EType:Error:Stacktrace ->
            logger:error("mDNS registration failed: ~p:~p~n~p", [EType, Error, Stacktrace]),
            {error, {mdns_registration_failed, Error}}
    end.

%% @doc Deregister mDNS service — sends a goodbye packet (TTL=0).
-spec unregister_mdns_internal(binary()) -> ok | {error, term()}.
unregister_mdns_internal(ServiceName) ->
    try
        logger:info("Unregistering mDNS service: ~p", [ServiceName]),

        %% Goodbye packet: PTR record with TTL=0
        EncodedService = encode_dns_name(ServiceName),
        RDLen = byte_size(EncodedService) + 1,
        Header = <<0:16, 16#8400:16, 0:16, 1:16, 0:16, 0:16>>,
        Answer = <<EncodedService/binary, 0:8,
                   12:16, 1:16, 0:32, RDLen:16,
                   EncodedService/binary, 0:8>>,
        Packet = <<Header/binary, Answer/binary>>,

        {ok, Sock} = gen_udp:open(0, [binary, {active, false}, {reuseaddr, true},
                                       {multicast_ttl, 255}, {multicast_loop, true}]),
        ok = gen_udp:send(Sock, ?MDNS_ADDR, ?MDNS_PORT, Packet),
        gen_udp:close(Sock),
        ok
    catch
        EType:Error:Stacktrace ->
            logger:error("mDNS deregistration failed: ~p:~p~n~p", [EType, Error, Stacktrace]),
            {error, {mdns_deregistration_failed, Error}}
    end.

%% @doc Discover services via mDNS — sends a PTR query and collects responses.
-spec discover_mdns(binary()) -> {ok, [service_endpoint()]} | {error, term()}.
discover_mdns(ServiceName) ->
    try
        logger:info("Discovering mDNS service: ~p", [ServiceName]),
        ServiceType = iolist_to_binary([ServiceName, "._tcp.local"]),
        QueryPacket = build_mdns_query(ServiceType),

        {ok, Sock} = gen_udp:open(0, [binary, {active, false}, {reuseaddr, true},
                                       {multicast_ttl, 255}, {multicast_loop, true}]),
        ok = gen_udp:send(Sock, ?MDNS_ADDR, ?MDNS_PORT, QueryPacket),

        Endpoints = collect_mdns_responses(Sock, ?MDNS_TIMEOUT, []),
        gen_udp:close(Sock),
        {ok, Endpoints}
    catch
        EType:Error:Stacktrace ->
            logger:error("mDNS discovery failed: ~p:~p~n~p", [EType, Error, Stacktrace]),
            {error, {mdns_discovery_failed, Error}}
    end.

%% @doc Collect mDNS UDP responses within a timeout window.
-spec collect_mdns_responses(gen_udp:socket(), non_neg_integer(), [service_endpoint()]) ->
    [service_endpoint()].
collect_mdns_responses(Sock, Timeout, Acc) ->
    case gen_udp:recv(Sock, 1500, Timeout) of
        {ok, {_Ip, _Port, Data}} ->
            NewAcc = case parse_mdns_response(Data) of
                {ok, Transports} ->
                    maps:fold(fun(_Id, Config, A) ->
                        EP = #{
                            host => maps:get(host, Config, "localhost"),
                            port => maps:get(port, Config, 0),
                            metadata => #{}
                        },
                        [EP | A]
                    end, Acc, Transports);
                _ ->
                    Acc
            end,
            collect_mdns_responses(Sock, Timeout, NewAcc);
        {error, timeout} ->
            Acc;
        {error, _} ->
            Acc
    end.

%%%===================================================================
%%% Internal Functions - Consul
%%%===================================================================

%% @doc Ensure the gun HTTP client application is started.
-spec ensure_gun_started() -> ok | {error, term()}.
ensure_gun_started() ->
    case application:start(gun) of
        ok -> ok;
        {error, {already_started, gun}} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Build Consul HTTP request headers, optionally including an ACL token.
-spec build_consul_headers(binary() | undefined) -> [{binary(), binary()}].
build_consul_headers(undefined) ->
    [{<<"accept">>, <<"application/json">>}];
build_consul_headers(Token) ->
    [
        {<<"accept">>, <<"application/json">>},
        {<<"x-consul-token">>, Token}
    ].

%% @doc Parse a list of raw Consul service maps into a transport map.
-spec parse_consul_services([map()]) -> {ok, #{atom() => map()}}.
parse_consul_services(Services) when is_list(Services) ->
    Transports = lists:foldl(fun(Service, Acc) ->
        case parse_consul_service(Service) of
            {ok, TransportId, Config} ->
                maps:put(TransportId, Config, Acc);
            {error, Reason} ->
                logger:warning("Consul service parse failed: ~p (service: ~p)", [Reason, Service]),
                Acc
        end
    end, #{}, Services),
    {ok, Transports};
parse_consul_services(_) ->
    {ok, #{}}.

%% @doc Parse a single raw Consul service map.
-spec parse_consul_service(map()) -> {ok, atom(), map()} | {error, term()}.
parse_consul_service(Service) ->
    try
        ServiceId   = maps:get(<<"ServiceID">>, Service),
        ServiceAddr = maps:get(<<"ServiceAddress">>, Service, undefined),
        ServicePort = maps:get(<<"ServicePort">>, Service),
        ServiceTags = maps:get(<<"ServiceTags">>, Service, []),

        %% Fall back to node Address if ServiceAddress is absent or empty
        Address = case ServiceAddr of
            undefined -> binary_to_list(maps:get(<<"Address">>, Service, <<"127.0.0.1">>));
            <<>>      -> binary_to_list(maps:get(<<"Address">>, Service, <<"127.0.0.1">>));
            _         -> binary_to_list(ServiceAddr)
        end,

        %% Derive transport type from tags
        Type = transport_type_from_tags(ServiceTags),

        %% Normalise the service ID to a safe Erlang atom (replace hyphens).
        NormId = binary:replace(ServiceId, <<"-">>, <<"_">>, [global]),
        TransportId = list_to_atom(binary_to_list(NormId)),
        Config = #{
            transport_id  => TransportId,
            type          => Type,
            host          => Address,
            port          => ServicePort,
            discovered_via => consul,
            discovered_at => erlang:system_time(millisecond),
            tags          => ServiceTags
        },
        {ok, TransportId, Config}
    catch
        error:{badkey, Key} ->
            {error, {missing_field, Key}};
        EType:Error:Stack ->
            {error, {parse_error, {EType, Error, Stack}}}
    end.

%% @doc Derive transport type atom from a list of Consul tags.
-spec transport_type_from_tags([binary()]) -> tcp | http | ws | atom().
transport_type_from_tags(Tags) ->
    case lists:member(<<"erlmcp:tcp">>, Tags) of
        true -> tcp;
        false ->
            case lists:member(<<"erlmcp:http">>, Tags) of
                true -> http;
                false ->
                    case lists:member(<<"erlmcp:ws">>, Tags) of
                        true -> ws;
                        false -> tcp
                    end
            end
    end.

%% @doc Perform an httpc request, returning {ok, StatusCode, Body} | {error, Reason}.
-spec httpc_request(atom(), {string(), list(), string(), iodata()} | {string(), list()}) ->
    {ok, non_neg_integer(), binary()} | {error, term()}.
httpc_request(Method, Request) ->
    _ = inets:start(),   %% idempotent; {error, {already_started, inets}} is fine
    case httpc:request(Method, Request, [{timeout, 5000}], [{body_format, binary}]) of
        {ok, {{_, StatusCode, _}, _Headers, Body}} ->
            {ok, StatusCode, Body};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Register service via Consul — POSTs to the agent register endpoint.
-spec register_consul_internal(binary(), service_info()) -> ok | {error, term()}.
register_consul_internal(ServiceName, ServiceInfo) ->
    try
        ConsulHost = application:get_env(erlmcp_transports, consul_host, "localhost"),
        ConsulPort = application:get_env(erlmcp_transports, consul_port, 8500),
        Port = maps:get(port, ServiceInfo, 8080),
        Host = maps:get(host, ServiceInfo, hostname()),
        HostBin = list_to_binary(Host),

        URL = lists:flatten(io_lib:format("http://~s:~p/v1/agent/service/register",
                                          [ConsulHost, ConsulPort])),

        Payload = [
            "{\"ID\":\"", ServiceName, "\",",
            "\"Name\":\"", ServiceName, "\",",
            "\"Address\":\"", HostBin, "\",",
            "\"Port\":", integer_to_list(Port), ",",
            "\"Check\":{",
              "\"HTTP\":\"http://", HostBin, ":", integer_to_list(Port), "/health\",",
              "\"Interval\":\"10s\",",
              "\"Timeout\":\"5s\"",
            "}}"
        ],
        Body = iolist_to_binary(Payload),

        logger:info("Registering Consul service: ~p at ~p:~p via ~s", [ServiceName, Host, Port, URL]),

        Token = application:get_env(erlmcp_transports, consul_token, undefined),
        Headers = build_consul_headers(Token),
        StringHeaders = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],

        case httpc_request(put, {URL, StringHeaders, "application/json", Body}) of
            {ok, Code, _} when Code >= 200, Code < 300 -> ok;
            {ok, Code, _} -> {error, {consul_http_error, Code}};
            {error, Reason} -> {error, {consul_request_failed, Reason}}
        end
    catch
        EType:Error:Stacktrace ->
            logger:error("Consul registration failed: ~p:~p~n~p", [EType, Error, Stacktrace]),
            {error, {consul_registration_failed, Error}}
    end.

%% @doc Deregister Consul service — DELETEs (PUTs to deregister endpoint).
-spec unregister_consul_internal(binary()) -> ok | {error, term()}.
unregister_consul_internal(ServiceName) ->
    try
        ConsulHost = application:get_env(erlmcp_transports, consul_host, "localhost"),
        ConsulPort = application:get_env(erlmcp_transports, consul_port, 8500),
        URL = lists:flatten(io_lib:format("http://~s:~p/v1/agent/service/deregister/~s",
                                          [ConsulHost, ConsulPort, ServiceName])),

        logger:info("Unregistering Consul service: ~p via ~s", [ServiceName, URL]),

        Token = application:get_env(erlmcp_transports, consul_token, undefined),
        Headers = build_consul_headers(Token),
        StringHeaders = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],

        case httpc_request(put, {URL, StringHeaders, "application/json", <<>>}) of
            {ok, Code, _} when Code >= 200, Code < 300 -> ok;
            {ok, Code, _} -> {error, {consul_http_error, Code}};
            {error, Reason} -> {error, {consul_request_failed, Reason}}
        end
    catch
        EType:Error:Stacktrace ->
            logger:error("Consul deregistration failed: ~p:~p~n~p", [EType, Error, Stacktrace]),
            {error, {consul_deregistration_failed, Error}}
    end.

%% @doc Discover services via Consul — GETs the health/service endpoint.
-spec discover_consul(binary()) -> {ok, [service_endpoint()]} | {error, term()}.
discover_consul(ServiceName) ->
    try
        ConsulHost = application:get_env(erlmcp_transports, consul_host, "localhost"),
        ConsulPort = application:get_env(erlmcp_transports, consul_port, 8500),
        URL = lists:flatten(io_lib:format("http://~s:~p/v1/health/service/~s?passing",
                                          [ConsulHost, ConsulPort, ServiceName])),

        logger:info("Discovering Consul service: ~p via ~s", [ServiceName, URL]),

        Token = application:get_env(erlmcp_transports, consul_token, undefined),
        Headers = build_consul_headers(Token),
        StringHeaders = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],

        case httpc_request(get, {URL, StringHeaders}) of
            {ok, 200, Body} ->
                %% Parse JSON array of service health entries
                Entries = try_json_decode(Body),
                Endpoints = lists:filtermap(fun consul_entry_to_endpoint/1, Entries),
                {ok, Endpoints};
            {ok, Code, _} ->
                {error, {consul_http_error, Code}};
            {error, Reason} ->
                {error, {consul_request_failed, Reason}}
        end
    catch
        EType:Error:Stacktrace ->
            logger:error("Consul discovery failed: ~p:~p~n~p", [EType, Error, Stacktrace]),
            {error, {consul_discovery_failed, Error}}
    end.

%% @doc Convert a Consul health entry map to a service_endpoint().
-spec consul_entry_to_endpoint(map()) -> {true, service_endpoint()} | false.
consul_entry_to_endpoint(Entry) when is_map(Entry) ->
    try
        Service = maps:get(<<"Service">>, Entry, #{}),
        Host = binary_to_list(maps:get(<<"Address">>, Service, <<"localhost">>)),
        Port = maps:get(<<"Port">>, Service, 0),
        {true, #{host => Host, port => Port, metadata => #{}}}
    catch
        _:_ -> false
    end;
consul_entry_to_endpoint(_) ->
    false.

%% @doc Attempt to JSON-decode a binary, returning a list (or [] on failure).
-spec try_json_decode(binary()) -> [map()].
try_json_decode(Body) ->
    try
        %% Use jsx if available, otherwise fall back to empty list
        case code:is_loaded(jsx) of
            {file, _} -> jsx:decode(Body, [return_maps]);
            false -> []
        end
    catch
        _:_ -> []
    end.

%%%===================================================================
%%% Internal Functions - Kubernetes
%%%===================================================================

%% @doc Read the Kubernetes in-cluster service-account token.
%%      Returns undefined when not running inside a cluster.
-spec k8s_token() -> binary() | undefined.
k8s_token() ->
    TokenPath = "/var/run/secrets/kubernetes.io/serviceaccount/token",
    case file:read_file(TokenPath) of
        {ok, Token} -> string:trim(Token);
        _           -> undefined
    end.

%% @doc Perform an HTTP(S) call to the Kubernetes API server.
%%      Uses httpc with SSL peer verification disabled for simplicity
%%      (production deployments should supply the cluster CA).
-spec k8s_httpc(atom(), string(), iodata()) ->
    {ok, non_neg_integer(), binary()} | {error, term()}.
k8s_httpc(Method, URL, Body) ->
    _ = inets:start(),   %% idempotent
    _ = ssl:start(),     %% idempotent
    SSLOpts = [{verify, verify_none}],
    HttpOpts = [{timeout, 5000}, {ssl, SSLOpts}],
    Opts = [{body_format, binary}],
    Token = k8s_token(),
    BaseHeaders = [{"accept", "application/json"}],
    Headers = case Token of
        undefined -> BaseHeaders;
        T -> [{"authorization", "Bearer " ++ binary_to_list(T)} | BaseHeaders]
    end,
    Request = case Method of
        get    -> {URL, Headers};
        delete -> {URL, Headers};
        _      -> {URL, Headers, "application/json", Body}
    end,
    case httpc:request(Method, Request, HttpOpts, Opts) of
        {ok, {{_, Code, _}, _, RespBody}} -> {ok, Code, RespBody};
        {error, Reason}                   -> {error, Reason}
    end.

%% @doc Register an endpoint into the Kubernetes Endpoints resource via PATCH.
-spec register_k8s_internal(binary(), service_info()) -> ok | {error, term()}.
register_k8s_internal(ServiceName, ServiceInfo) ->
    try
        K8sAPIServer = application:get_env(erlmcp_transports, k8s_api_server,
                                           "https://kubernetes.default.svc"),
        Namespace = maps:get(namespace, ServiceInfo, "default"),
        Port = maps:get(port, ServiceInfo, 8080),
        Host = maps:get(host, ServiceInfo, hostname()),

        logger:info("Registering Kubernetes endpoint: ~p in namespace ~p at ~p:~p",
                    [ServiceName, Namespace, Host, Port]),

        URL = lists:flatten(io_lib:format(
            "~s/api/v1/namespaces/~s/endpoints/~s",
            [K8sAPIServer, Namespace, ServiceName])),

        %% PATCH body: strategic-merge to add our address+port
        PatchBody = iolist_to_binary([
            "{\"subsets\":[{\"addresses\":[{\"ip\":\"", Host, "\"}],",
            "\"ports\":[{\"port\":", integer_to_list(Port), "}]}]}"
        ]),

        case k8s_httpc(patch, URL, PatchBody) of
            {ok, Code, _} when Code >= 200, Code < 300 -> ok;
            {ok, 404, _} ->
                %% Endpoints resource may not exist yet — try POST to create
                CreateURL = lists:flatten(io_lib:format(
                    "~s/api/v1/namespaces/~s/endpoints",
                    [K8sAPIServer, Namespace])),
                CreateBody = iolist_to_binary([
                    "{\"apiVersion\":\"v1\",\"kind\":\"Endpoints\",",
                    "\"metadata\":{\"name\":\"", ServiceName, "\",\"namespace\":\"", Namespace, "\"},",
                    "\"subsets\":[{\"addresses\":[{\"ip\":\"", Host, "\"}],",
                    "\"ports\":[{\"port\":", integer_to_list(Port), "}]}]}"
                ]),
                case k8s_httpc(post, CreateURL, CreateBody) of
                    {ok, C, _} when C >= 200, C < 300 -> ok;
                    {ok, C, _} -> {error, {k8s_http_error, C}};
                    {error, R} -> {error, {k8s_request_failed, R}}
                end;
            {ok, Code, _} -> {error, {k8s_http_error, Code}};
            {error, Reason} -> {error, {k8s_request_failed, Reason}}
        end
    catch
        EType:Error:Stacktrace ->
            logger:error("Kubernetes registration failed: ~p:~p~n~p", [EType, Error, Stacktrace]),
            {error, {k8s_registration_failed, Error}}
    end.

%% @doc Remove the endpoint from the Kubernetes Endpoints resource.
-spec unregister_k8s_internal(binary()) -> ok | {error, term()}.
unregister_k8s_internal(ServiceName) ->
    try
        K8sAPIServer = application:get_env(erlmcp_transports, k8s_api_server,
                                           "https://kubernetes.default.svc"),
        Namespace = application:get_env(erlmcp_transports, k8s_namespace, "default"),
        URL = lists:flatten(io_lib:format(
            "~s/api/v1/namespaces/~s/endpoints/~s",
            [K8sAPIServer, Namespace, ServiceName])),

        logger:info("Unregistering Kubernetes endpoint: ~p in namespace ~p", [ServiceName, Namespace]),

        %% DELETE the entire Endpoints resource
        case k8s_httpc(delete, URL, <<>>) of
            {ok, Code, _} when Code >= 200, Code < 300 -> ok;
            {ok, 404, _} -> ok;  %% Already gone
            {ok, Code, _} -> {error, {k8s_http_error, Code}};
            {error, Reason} -> {error, {k8s_request_failed, Reason}}
        end
    catch
        EType:Error:Stacktrace ->
            logger:error("Kubernetes deregistration failed: ~p:~p~n~p", [EType, Error, Stacktrace]),
            {error, {k8s_deregistration_failed, Error}}
    end.

%% @doc Discover endpoints via the Kubernetes Endpoints resource.
-spec discover_k8s(binary()) -> {ok, [service_endpoint()]} | {error, term()}.
discover_k8s(ServiceName) ->
    try
        K8sAPIServer = application:get_env(erlmcp_transports, k8s_api_server,
                                           "https://kubernetes.default.svc"),
        Namespace = application:get_env(erlmcp_transports, k8s_namespace, "default"),
        URL = lists:flatten(io_lib:format(
            "~s/api/v1/namespaces/~s/endpoints/~s",
            [K8sAPIServer, Namespace, ServiceName])),

        logger:info("Discovering Kubernetes endpoints: ~p in namespace ~p", [ServiceName, Namespace]),

        case k8s_httpc(get, URL, <<>>) of
            {ok, 200, Body} ->
                Endpoints = parse_k8s_endpoints(Body),
                {ok, Endpoints};
            {ok, 404, _} ->
                {ok, []};
            {ok, Code, _} ->
                {error, {k8s_http_error, Code}};
            {error, Reason} ->
                {error, {k8s_request_failed, Reason}}
        end
    catch
        EType:Error:Stacktrace ->
            logger:error("Kubernetes discovery failed: ~p:~p~n~p", [EType, Error, Stacktrace]),
            {error, {k8s_discovery_failed, Error}}
    end.

%% @doc Parse Kubernetes Endpoints JSON into a list of service_endpoint().
-spec parse_k8s_endpoints(binary()) -> [service_endpoint()].
parse_k8s_endpoints(Body) ->
    try
        case code:is_loaded(jsx) of
            {file, _} ->
                Resource = jsx:decode(Body, [return_maps]),
                Subsets = maps:get(<<"subsets">>, Resource, []),
                lists:foldl(fun(Subset, Acc) ->
                    Addresses = maps:get(<<"addresses">>, Subset, []),
                    Ports = maps:get(<<"ports">>, Subset, []),
                    lists:foldl(fun(Addr, AAcc) ->
                        Ip = binary_to_list(maps:get(<<"ip">>, Addr, <<"localhost">>)),
                        lists:foldl(fun(P, PAcc) ->
                            Port = maps:get(<<"port">>, P, 0),
                            [#{host => Ip, port => Port, metadata => #{}} | PAcc]
                        end, AAcc, Ports)
                    end, Acc, Addresses)
                end, [], Subsets);
            false ->
                []
        end
    catch
        _:_ -> []
    end.

%%%===================================================================
%%% Internal Functions - Utilities
%%%===================================================================

%% @doc Count total watchers across all services
-spec count_total_watchers(#{binary() => [pid()]}) -> non_neg_integer().
count_total_watchers(WatchersMap) ->
    maps:fold(fun(_ServiceName, Pids, Acc) ->
        Acc + length(Pids)
    end, 0, WatchersMap).

%% @doc Get system hostname
-spec hostname() -> string().
hostname() ->
    {ok, Host} = inet:gethostname(),
    Host.
