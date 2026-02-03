-module(erlmcp_api_gateway_routes).

-export([init/2, allowed_methods/2, content_types_accepted/2, content_types_provided/2,
         handle_post/2, handle_get/2, handle_put/2, handle_delete/2]).

-include("erlmcp_api_gateway.hrl").

%%====================================================================
%% Cowboy2 Callbacks
%%====================================================================

init(Req, _Opts) ->
    %% Initialize request context
    Context = #{
        start_time => erlang:system_time(millisecond),
        request_id => generate_request_id(),
        trace_id => generate_trace_id()
    },
    {cowboy_rest, Req, Context}.

allowed_methods(Req, State) ->
    %% Define allowed HTTP methods
    Methods = [
        <<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>
    ],
    {[Methods], Req, State}.

content_types_accepted(Req, State) ->
    %% Define accepted content types
    Types = [
        {<<"application/json">>, handle_json}
    ],
    {Types, Req, State}.

content_types_provided(Req, State) ->
    %% Define provided content types
    Types = [
        {<<"application/json">>, return_json}
    ],
    {Types, Req, State}.

%%====================================================================
%% HTTP Method Handlers
%%====================================================================

handle_post(Req, State) ->
    Path = cowboy_req:path(Req),
    ContentType = cowboy_req:content_type(Req),

    case handle_api_request(post, Path, ContentType, Req, State) of
        {Method, Status, Headers, Body, NewState} ->
            cowboy_req:reply(Status, Headers, Body, Req, NewState)
    end.

handle_get(Req, State) ->
    Path = cowboy_req:path(Req),

    case handle_api_request(get, Path, undefined, Req, State) of
        {Method, Status, Headers, Body, NewState} ->
            cowboy_req:reply(Status, Headers, Body, Req, NewState)
    end.

handle_put(Req, State) ->
    Path = cowboy_req:path(Req),
    ContentType = cowboy_req:content_type(Req),

    case handle_api_request(put, Path, ContentType, Req, State) of
        {Method, Status, Headers, Body, NewState} ->
            cowboy_req:reply(Status, Headers, Body, Req, NewState)
    end.

handle_delete(Req, State) ->
    Path = cowboy_req:path(Req),

    case handle_api_request(delete, Path, undefined, Req, State) of
        {Method, Status, Headers, Body, NewState} ->
            cowboy_req:reply(Status, Headers, Body, Req, NewState)
    end.

%%====================================================================
%% Request Processing
%%====================================================================

handle_api_request(Method, Path, ContentType, Req, State) ->
    %% Extract API key from headers
    ApiKey = extract_api_key(Req),

    %% Authenticate and authorize
    case authenticate_request(ApiKey, Req, State) of
        {ok, ConsumerId} ->
            %% Apply rate limiting
            case check_rate_limit(ConsumerId, Path, State) of
                {allowed, Remaining} ->
                    %% Process the request
                    process_request(Method, Path, ContentType, Req, State#{consumer_id => ConsumerId});
                {denied, ResetTime} ->
                    rate_limit_exceeded(ResetTime, State)
            end;
        {error, Reason} ->
            unauthorized(Reason, State)
    end.

process_request(Method, Path, ContentType, Req, State) ->
    %% Route to appropriate handler based on path
    case Path of
        <<"/apis">> ->
            handle_apis_request(Method, ContentType, Req, State);
        <<"/apis/", ApiId/binary>> ->
            handle_api_request(Method, ApiId, ContentType, Req, State);
        <<"/apis/", ApiId/binary, "/routes">> ->
            handle_api_routes_request(Method, ApiId, ContentType, Req, State);
        <<"/apis/", ApiId/binary, "/routes/", RouteId/binary>> ->
            handle_api_route_request(Method, ApiId, RouteId, ContentType, Req, State);
        <<"/consumers">> ->
            handle_consumers_request(Method, ContentType, Req, State);
        <<"/consumers/", ConsumerId/binary>> ->
            handle_consumer_request(Method, ConsumerId, ContentType, Req, State);
        _ ->
            not_found(State)
    end.

handle_json(Req, State) ->
    Method = cowboy_req:method(Req),
    handle_method(Method, Req, State).

handle_method(<<"GET">>, Req, State) ->
    Path = cowboy_req:path(Req),
    case Path of
        <<"/apis">> ->
            list_apis(Req, State);
        <<"/apis/", ApiId/binary>> ->
            get_api(Req, State, ApiId);
        <<"/apis/", ApiId/binary, "/routes">> ->
            list_routes(Req, State, ApiId);
        <<"/consumers">> ->
            list_consumers(Req, State);
        _ ->
            cowboy_req:reply(404, #{}, <<"Not Found">>, Req)
    end;

handle_method(<<"POST">>, Req, State) ->
    Path = cowboy_req:path(Req),
    case Path of
        <<"/apis">> ->
            create_api(Req, State);
        <<"/consumers">> ->
            create_consumer(Req, State);
        <<"/plugins">> ->
            create_plugin(Req, State);
        _ ->
            cowboy_req:reply(404, #{}, <<"Not Found">>, Req)
    end;

handle_method(<<"PUT">>, Req, State) ->
    Path = cowboy_req:path(Req),
    case Path of
        <<"/apis/", ApiId/binary>> ->
            update_api(Req, State, ApiId);
        _ ->
            cowboy_req:reply(404, #{}, <<"Not Found">>, Req)
    end;

handle_method(<<"DELETE">>, Req, State) ->
    Path = cowboy_req:path(Req),
    case Path of
        <<"/apis/", ApiId/binary>> ->
            delete_api(Req, State, ApiId);
        <<"/consumers/", ConsumerId/binary>> ->
            delete_consumer(Req, State, ConsumerId);
        _ ->
            cowboy_req:reply(404, #{}, <<"Not Found">>, Req)
    end.

%%====================================================================
%% API Management Handlers
%%====================================================================

list_apis(Req, State) ->
   Apis = erlmcp_api_management:list_apis(),
    Response = jsx:encode(#{<<"apis">> => Apis}),
    cowboy_req:reply(200, #{}, Response, Req, State).

get_api(Req, State, ApiId) ->
    case erlmcp_api_management:get_api(binary_to_list(ApiId)) of
        {ok, Api} ->
            Response = jsx:encode(#{<<"api">> => Api}),
            cowboy_req:reply(200, #{}, Response, Req, State);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"API not found">>, Req, State)
    end.

create_api(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    ApiSpec = jsx:decode(Body, [{return_maps, true}]),

    %% Validate API definition
    case erlmcp_api_management:validate_api_definition(ApiSpec) of
        {ok, ValidatedApi} ->
            %% Create API
            case erlmcp_api_management:create_api(ValidatedApi) of
                {ok, ApiId} ->
                    Response = erlmcp_api_management:get_api(ApiId),
                    cowboy_req:reply(201, #{<<"location">> => <<"/apis/", (list_to_binary(ApiId))/binary>>},
                                    jsx:encode(#{<<"api">> => Response}), Req2, State);
                {error, Reason} ->
                    bad_request(Reason, State)
            end;
        {error, Reason} ->
            bad_request(Reason, State)
    end.

list_routes(Req, State, ApiId) ->
    Routes = erlmcp_api_management:list_routes(binary_to_list(ApiId)),
    Response = jsx:encode(#{<<"routes">> => Routes}),
    cowboy_req:reply(200, #{}, Response, Req, State).

list_consumers(Req, State) ->
    Consumers = erlmcp_api_management:list_consumers(),
    Response = jsx:encode(#{<<"consumers">> => Consumers}),
    cowboy_req:reply(200, #{}, Response, Req, State).

create_consumer(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    ConsumerSpec = jsx:decode(Body, [{return_maps, true}]),

    Consumer = #{
        id => uuid:uuid_to_list(uuid:uuid4()),
        name => maps:get(<<"name">>, ConsumerSpec),
        email => maps:get(<<"email">>, ConsumerSpec),
        status => maps:get(<<"status">>, ConsumerSpec, <<"active">>),
        rate_limit => maps:get(<<"rate_limit">>, ConsumerSpec, #{<<"requests">> => 1000, <<"window">> => <<"1m">>}),
        metadata => maps:get(<<"metadata">>, ConsumerSpec, #{}),
        created_at => erlang:system_time(millisecond)
    },

    case erlmcp_api_management:create_consumer(Consumer) of
        {ok, ConsumerId} ->
            Response = erlmcp_api_management:get_consumer(ConsumerId),
            cowboy_req:reply(201, #{<<"location">> => <<"/consumers/", (list_to_binary(ConsumerId))/binary>>},
                            jsx:encode(#{<<"consumer">> => Response}), Req2, State);
        {error, Reason} ->
            bad_request(Reason, State)
    end.

create_plugin(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    PluginSpec = jsx:decode(Body, [{return_maps, true}]),

    Plugin = #{
        id => uuid:uuid_to_list(uuid:uuid4()),
        name => maps:get(<<"name">>, PluginSpec),
        version => maps:get(<<"version">>, PluginSpec, <<"1.0.0">>),
        config => maps:get(<<"config">>, PluginSpec, #{}),
        enabled => maps:get(<<"enabled">>, PluginSpec, true),
        hooks => maps:get(<<"hooks">>, PluginSpec, []),
        created_at => erlang:system_time(millisecond)
    },

    case erlmcp_api_management:create_plugin(Plugin) of
        {ok, PluginId} ->
            Response = erlmcp_api_management:get_plugin(PluginId),
            cowboy_req:reply(201, #{<<"location">> => <<"/plugins/", (list_to_binary(PluginId))/binary>>},
                            jsx:encode(#{<<"plugin">> => Response}), Req2, State);
        {error, Reason} ->
            bad_request(Reason, State)
    end.

update_api(Req, State, ApiId) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    UpdateSpec = jsx:decode(Body, [{return_maps, true}]),

    case erlmcp_api_management:get_api(binary_to_list(ApiId)) of
        {ok, Api} ->
            UpdatedApi = maps:merge(Api, UpdateSpec),
            case erlmcp_api_management:update_api(ApiId, UpdatedApi) of
                {ok, UpdatedApiData} ->
                    Response = jsx:encode(#{<<"api">> => UpdatedApiData}),
                    cowboy_req:reply(200, #{}, Response, Req2, State);
                {error, Reason} ->
                    bad_request(Reason, State)
            end;
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"API not found">>, Req2, State)
    end.

delete_api(Req, State, ApiId) ->
    case erlmcp_api_management:delete_api(binary_to_list(ApiId)) of
        ok ->
            cowboy_req:reply(204, #{}, <<>>, Req, State);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"API not found">>, Req, State)
    end.

delete_consumer(Req, State, ConsumerId) ->
    case erlmcp_api_management:delete_consumer(binary_to_list(ConsumerId)) of
        ok ->
            cowboy_req:reply(204, #{}, <<>>, Req, State);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"Consumer not found">>, Req, State)
    end.

%%====================================================================
%% Utility Functions
%%====================================================================

extract_api_key(Req) ->
    %% Try API key from header
    case cowboy_req:header(?API_KEY_HEADER, Req) of
        undefined ->
            %% Try from authorization header
            case cowboy_req:header(?AUTHORIZATION_HEADER, Req) of
                <<"Bearer ", ApiKey/binary>> ->
                    ApiKey;
                _ ->
                    undefined
            end;
        ApiKey ->
            ApiKey
    end.

authenticate_request(ApiKey, Req, State) ->
    if
        ApiKey =/= undefined ->
            case erlmcp_api_management:validate_api_key(ApiKey) of
                {ok, ConsumerId} ->
                    {ok, ConsumerId};
                {error, invalid_key} ->
                    {error, invalid_api_key};
                {error, expired} ->
                    {error, expired_api_key};
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            {error, missing_api_key}
    end.

check_rate_limit(ConsumerId, Path, State) ->
    case erlmcp_api_management:check_rate_limit(ConsumerId, Path) of
        {allowed, Remaining} ->
            {allowed, Remaining};
        {denied, ResetTime} ->
            {denied, ResetTime}
    end.

add_response_headers(State) ->
    %% Add common headers
    Headers = #{
        <<"content-type">> => <<"application/json">>,
        <<"server">> => ?API_GATEWAY_VERSION,
        <<"connection">> => <<"keep-alive">>,
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
        <<"access-control-allow-headers">> => <<"X-API-Key, Authorization, Content-Type">>
    },

    %% Add tracing headers
    case maps:get(trace_id, State, undefined) of
        TraceId when TraceId =/= undefined ->
            maps:put(<<"X-Trace-ID">>, TraceId, Headers);
        _ ->
            Headers
    end.

%%====================================================================
%% Response Handlers
%%====================================================================

return_json(Req, State) ->
    cowboy_req:reply(200, add_response_headers(State), <<>>, Req, State).

not_found(State) ->
    {get, 404, add_response_headers(State),
        jsx:encode(#{error => <<"API endpoint not found">>}),
        State}.

bad_request(Reason, State) ->
    {post, 400, add_response_headers(State),
        jsx:encode(#{error => Reason}),
        State}.

unauthorized(Reason, State) ->
    {post, 401, add_response_headers(State),
        jsx:encode(#{error => <<"Unauthorized">>, reason => Reason}),
        State}.

rate_limit_exceeded(ResetTime, State) ->
    Response = #{
        error => <<"Rate limit exceeded">>,
        reset_time => ResetTime,
        retry_after => trunc((ResetTime - erlang:system_time(millisecond)) / 1000)
    },
    {post, 429, add_response_headers(State),
        jsx:encode(Response),
        State}.

generate_request_id() ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(16))).

generate_trace_id() ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(32))).