-module(erlmcp_api_management).

-behaviour(gen_server).

%% API exports
-export([start_link/0, list_apis/0, get_api/1, create_api/1, update_api/2, delete_api/1,
         list_consumers/0, get_consumer/1, create_consumer/1, update_consumer/2, delete_consumer/1,
         list_routes/1, get_route/2, create_route/2, update_route/3, delete_route/2,
         list_plugins/0, get_plugin/1, create_plugin/1, update_plugin/2, delete_plugin/1,
         validate_api_key/1, check_rate_limit/2, get_analytics/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp_api_gateway.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

list_apis() ->
    gen_server:call(?MODULE, list_apis).

get_api(ApiId) ->
    gen_server:call(?MODULE, {get_api, ApiId}).

create_api(ApiDefinition) ->
    gen_server:call(?MODULE, {create_api, ApiDefinition}).

update_api(ApiId, ApiDefinition) ->
    gen_server:call(?MODULE, {update_api, ApiId, ApiDefinition}).

delete_api(ApiId) ->
    gen_server:call(?MODULE, {delete_api, ApiId}).

list_consumers() ->
    gen_server:call(?MODULE, list_consumers).

get_consumer(ConsumerId) ->
    gen_server:call(?MODULE, {get_consumer, ConsumerId}).

create_consumer(Consumer) ->
    gen_server:call(?MODULE, {create_consumer, Consumer}).

update_consumer(ConsumerId, Consumer) ->
    gen_server:call(?MODULE, {update_consumer, ConsumerId, Consumer}).

delete_consumer(ConsumerId) ->
    gen_server:call(?MODULE, {delete_consumer, ConsumerId}).

list_routes(ApiId) ->
    gen_server:call(?MODULE, {list_routes, ApiId}).

get_route(ApiId, RouteId) ->
    gen_server:call(?MODULE, {get_route, ApiId, RouteId}).

create_route(ApiId, Route) ->
    gen_server:call(?MODULE, {create_route, ApiId, Route}).

update_route(ApiId, RouteId, Route) ->
    gen_server:call(?MODULE, {update_route, ApiId, RouteId, Route}).

delete_route(ApiId, RouteId) ->
    gen_server:call(?MODULE, {delete_route, ApiId, RouteId}).

list_plugins() ->
    gen_server:call(?MODULE, list_plugins).

get_plugin(PluginId) ->
    gen_server:call(?MODULE, {get_plugin, PluginId}).

create_plugin(Plugin) ->
    gen_server:call(?MODULE, {create_plugin, Plugin}).

update_plugin(PluginId, Plugin) ->
    gen_server:call(?MODULE, {update_plugin, PluginId, Plugin}).

delete_plugin(PluginId) ->
    gen_server:call(?MODULE, {delete_plugin, PluginId}).

validate_api_key(ApiKey) ->
    gen_server:call(?MODULE, {validate_api_key, ApiKey}).

check_rate_limit(ConsumerId, Path) ->
    gen_server:call(?MODULE, {check_rate_limit, ConsumerId, Path}).

get_analytics(TimeRange) ->
    gen_server:call(?MODULE, {get_analytics, TimeRange}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize database tables
    init_tables(),

    %% Load configuration from environment or defaults
    Config = load_config(),

    %% Start rate limiter process
    start_rate_limiter(),

    %% Start analytics collector
    start_analytics_collector(),

    State = #{
        apis => load_apis(),
        consumers => load_consumers(),
        routes => load_routes(),
        plugins => load_plugins(),
        config => Config
    },

    {ok, State}.

handle_call(list_apis, _From, State) ->
    Apis = maps:get(apis, State),
    {reply, {ok, Apis}, State};

handle_call({get_api, ApiId}, _From, State) ->
    case maps:find(ApiId, maps:get(apis, State)) of
        {ok, Api} ->
            {reply, {ok, Api}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({create_api, ApiDefinition}, _From, State) ->
    %% Validate API definition
    case validate_api_definition(ApiDefinition) of
        {ok, ValidatedApi} ->
            ApiId = generate_id(),
            Api = ValidatedApi#{
                id => ApiId,
                created_at => erlang:system_time(millisecond),
                updated_at => erlang:system_time(millisecond)
            },

            %% Store API
            NewApis = maps:put(ApiId, Api, maps:get(apis, State)),
            NewState = State#{apis => NewApis},

            %% Persist to database
            save_api(Api),

            {reply, {ok, ApiId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({update_api, ApiId, ApiDefinition}, _From, State) ->
    case maps:find(ApiId, maps:get(apis, State)) of
        {ok, ExistingApi} ->
            %% Validate API definition
            case validate_api_definition(ApiDefinition) of
                {ok, ValidatedApi} ->
                    UpdatedApi = maps:merge(ExistingApi, ValidatedApi)#{
                        updated_at => erlang:system_time(millisecond)
                    },

                    %% Store updated API
                    NewApis = maps:put(ApiId, UpdatedApi, maps:get(apis, State)),
                    NewState = State#{apis => NewApis},

                    %% Persist to database
                    save_api(UpdatedApi),

                    {reply, {ok, UpdatedApi}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_api, ApiId}, _From, State) ->
    case maps:find(ApiId, maps:get(apis, State)) of
        {ok, _Api} ->
            %% Delete API
            NewApis = maps:remove(ApiId, maps:get(apis, State)),
            NewState = State#{apis => NewApis},

            %% Delete from database
            delete_api_from_db(ApiId),

            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(list_consumers, _From, State) ->
    Consumers = maps:get(consumers, State),
    {reply, {ok, Consumers}, State};

handle_call({get_consumer, ConsumerId}, _From, State) ->
    case maps:find(ConsumerId, maps:get(consumers, State)) of
        {ok, Consumer} ->
            {reply, {ok, Consumer}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({create_consumer, Consumer}, _From, State) ->
    ConsumerId = generate_id(),
    ConsumerWithId = Consumer#{
        id => ConsumerId,
        created_at => erlang:system_time(millisecond),
        updated_at => erlang:system_time(millisecond)
    },

    %% Store consumer
    NewConsumers = maps:put(ConsumerId, ConsumerWithId, maps:get(consumers, State)),
    NewState = State#{consumers => NewConsumers},

    %% Persist to database
    save_consumer(ConsumerWithId),

    {reply, {ok, ConsumerId}, NewState};

handle_call({update_consumer, ConsumerId, Consumer}, _From, State) ->
    case maps:find(ConsumerId, maps:get(consumers, State)) of
        {ok, ExistingConsumer} ->
            UpdatedConsumer = maps:merge(ExistingConsumer, Consumer)#{
                updated_at => erlang:system_time(millisecond)
            },

            %% Store updated consumer
            NewConsumers = maps:put(ConsumerId, UpdatedConsumer, maps:get(consumers, State)),
            NewState = State#{consumers => NewConsumers},

            %% Persist to database
            save_consumer(UpdatedConsumer),

            {reply, {ok, UpdatedConsumer}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_consumer, ConsumerId}, _From, State) ->
    case maps:find(ConsumerId, maps:get(consumers, State)) of
        {ok, _Consumer} ->
            %% Delete consumer
            NewConsumers = maps:remove(ConsumerId, maps:get(consumers, State)),
            NewState = State#{consumers => NewConsumers},

            %% Delete from database
            delete_consumer_from_db(ConsumerId),

            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({list_routes, ApiId}, _From, State) ->
    Routes = maps:get(routes, State),
    ApiRoutes = maps:filter(fun(RouteId, _Route) ->
        case RouteId of
            <<ApiId/binary, _/binary>> -> true;
            _ -> false
        end
    end, Routes),

    {reply, {ok, ApiRoutes}, State};

handle_call({create_route, ApiId, Route}, _From, State) ->
    RouteId = generate_id(ApiId),
    RouteWithId = Route#{
        id => RouteId,
        api_id => ApiId,
        created_at => erlang:system_time(millisecond),
        updated_at => erlang:system_time(millisecond)
    },

    %% Store route
    NewRoutes = maps:put(RouteId, RouteWithId, maps:get(routes, State)),
    NewState = State#{routes => NewRoutes},

    %% Persist to database
    save_route(RouteWithId),

    {reply, {ok, RouteId}, NewState};

handle_call({validate_api_key, ApiKey}, _From, State) ->
    %% Find consumer with this API key
    Consumers = maps:get(consumers, State),
    case find_consumer_by_api_key(ApiKey, Consumers) of
        {ok, ConsumerId} ->
            {reply, {ok, ConsumerId}, State};
        {error, not_found} ->
            {reply, {error, invalid_key}, State};
        {error, expired} ->
            {reply, {error, expired}, State}
    end;

handle_call({check_rate_limit, ConsumerId, Path}, _From, State) ->
    %% Get rate limit configuration
    Config = maps:get(config, State),
    RateLimits = maps:get(rate_limits, Config),

    %% Get consumer rate limit
    Consumer = maps:get(ConsumerId, maps:get(consumers, State), #{}),
    ConsumerRateLimit = maps:get(rate_limit, Consumer, maps:get(default, RateLimits)),

    %% Check rate limit
    case is_rate_limit_allowed(ConsumerId, Path, ConsumerRateLimit) of
        {allowed, Remaining} ->
            {reply, {allowed, Remaining}, State};
        {denied, ResetTime} ->
            {reply, {denied, ResetTime}, State}
    end;

handle_call({get_analytics, TimeRange}, _From, State) ->
    %% Get analytics data
    Analytics = get_analytics_data(TimeRange),
    {reply, {ok, Analytics}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

init_tables() ->
    %% Create mnesia tables for high availability
    mnesia:create_table(api_definitions, [
        {attributes, record_info(fields, api_definition)},
        {disc_copies, [node()]},
        {type, set}
    ]),

    mnesia:create_table(consumers, [
        {attributes, record_info(fields, consumer)},
        {disc_copies, [node()]},
        {type, set}
    ]),

    mnesia:create_table(routes, [
        {attributes, record_info(fields, api_route)},
        {disc_copies, [node()]},
        {type, set}
    ]),

    mnesia:create_table(plugins, [
        {attributes, record_info(fields, plugin)},
        {disc_copies, [node()]},
        {type, set}
    ]),

    mnesia:create_table(analytics_events, [
        {attributes, record_info(fields, analytics_event)},
        {disc_copies, [node()]},
        {type, set}
    ]).

load_config() ->
    %% Load configuration from environment or use defaults
    #{
        rate_limits => #{
            default => #{
                requests_per_minute => 1000,
                requests_per_hour => 10000,
                requests_per_day => 100000
            }
        },
        analytics => #{
            enabled => true,
            retention => 86400000 %% 24 hours
        }
    }.

load_apis() ->
    %% Load APIs from database
    F = fun() -> mnesia:match_object(#api_definition{_ = '_'}) end,
    case mnesia:transaction(F) of
        {atomic, Apis} ->
            lists:foldl(fun(Api, Acc) ->
                maps:put(Api#api_definition.id, Api, Acc)
            end, #{}, Apis);
        {aborted, _} ->
            #{}
    end.

load_consumers() ->
    %% Load consumers from database
    F = fun() -> mnesia:match_object(#consumer{_ = '_'}) end,
    case mnesia:transaction(F) of
        {atomic, Consumers} ->
            lists:foldl(fun(Consumer, Acc) ->
                maps:put(Consumer#consumer.id, Consumer, Acc)
            end, #{}, Consumers);
        {aborted, _} ->
            #{}
    end.

load_routes() ->
    %% Load routes from database
    F = fun() -> mnesia:match_object(#api_route{_ = '_'}) end,
    case mnesia:transaction(F) of
        {atomic, Routes} ->
            lists:foldl(fun(Route, Acc) ->
                maps:put(Route#api_route.id, Route, Acc)
            end, #{}, Routes);
        {aborted, _} ->
            #{}
    end.

load_plugins() ->
    %% Load plugins from database
    F = fun() -> mnesia:match_object(#plugin{_ = '_'}) end,
    case mnesia:transaction(F) of
        {atomic, Plugins} ->
            lists:foldl(fun(Plugin, Acc) ->
                maps:put(Plugin#plugin.id, Plugin, Acc)
            end, #{}, Plugins);
        {aborted, _} ->
            #{}
    end.

save_api(Api) ->
    %% Save API to database
    F = fun() ->
        mnesia:write(Api#api_definition{
            updated_at = erlang:system_time(millisecond)
        })
    end,
    mnesia:transaction(F).

save_consumer(Consumer) ->
    %% Save consumer to database
    F = fun() ->
        mnesia:write(Consumer#consumer{
            updated_at = erlang:system_time(millisecond)
        })
    end,
    mnesia:transaction(F).

save_route(Route) ->
    %% Save route to database
    F = fun() ->
        mnesia:write(Route#api_route{
            updated_at = erlang:system_time(millisecond)
        })
    end,
    mnesia:transaction(F).

delete_api_from_db(ApiId) ->
    %% Delete API from database
    F = fun() ->
        mnesia:delete({api_definition, ApiId})
    end,
    mnesia:transaction(F).

delete_consumer_from_db(ConsumerId) ->
    %% Delete consumer from database
    F = fun() ->
        mnesia:delete({consumer, ConsumerId})
    end,
    mnesia:transaction(F).

find_consumer_by_api_key(ApiKey, Consumers) ->
    %% Find consumer by API key
    case maps:find(ApiKey, Consumers) of
        {ok, ConsumerId} ->
            Consumer = maps:get(ConsumerId, Consumers),
            case maps:get(status, Consumer) of
                active ->
                    {ok, ConsumerId};
                _ ->
                    {error, not_found}
            end;
        error ->
            {error, not_found}
    end.

is_rate_limit_allowed(ConsumerId, Path, RateLimit) ->
    %% Check if request is allowed based on rate limit
    Now = erlang:system_time(millisecond),

    %% Get current request count
    CurrentCount = get_request_count(ConsumerId, Path, Now),

    %% Extract rate limit values
    PerMinute = maps:get(requests_per_minute, RateLimit, 1000),
    PerHour = maps:get(requests_per_hour, RateLimit, 10000),
    PerDay = maps:get(requests_per_day, RateLimit, 100000),

    %% Check against limits
    if
        CurrentCount >= PerMinute ->
            {denied, get_minute_reset(Now)};
        CurrentCount >= PerHour ->
            {denied, get_hour_reset(Now)};
        CurrentCount >= PerDay ->
            {denied, get_day_reset(Now)};
        true ->
            {allowed, PerMinute - CurrentCount}
    end.

get_request_count(ConsumerId, Path, Now) ->
    %% Get request count from cache or analytics
    %% This is a simplified version - in production you'd use Redis or similar
    0. %% Placeholder for demonstration

get_minute_reset(Now) ->
    %% Get timestamp for minute reset
    MinuteStart = Now - (Now rem 60000),
    MinuteStart + 60000.

get_hour_reset(Now) ->
    %% Get timestamp for hour reset
    HourStart = Now - (Now rem 3600000),
    HourStart + 3600000.

get_day_reset(Now) ->
    %% Get timestamp for day reset
    DaysSinceEpoch = Now div 86400000,
    DaysSinceEpoch * 86400000 + 86400000.

generate_id() ->
    %% Generate unique ID
    binary_to_list(base64:encode(crypto:strong_rand_bytes(16))).

generate_id(Prefix) ->
    %% Generate ID with prefix
    <<Prefix/binary, "_", (generate_id())/binary>>.

start_rate_limiter() ->
    %% Start rate limiter process
    erlmcp_api_rate_limiter:start_link().

start_analytics_collector() ->
    %% Start analytics collector process
    erlmcp_api_analytics:start_link().

get_analytics_data(TimeRange) ->
    %% Get analytics data for given time range
    %% This is a simplified version - in production you'd query the database
    #{
        total_requests => 0,
        error_rate => 0.0,
        average_response_time => 0,
        top_apis => [],
        top_consumers => []
    }.

validate_api_definition(ApiDefinition) ->
    %% Validate API definition
    RequiredFields = [name, version, description, base_path],

    %% Check required fields
    case check_required_fields(RequiredFields, ApiDefinition) of
        ok ->
            %% Validate version format
            case validate_version_format(maps:get(version, ApiDefinition)) of
                ok ->
                    {ok, ApiDefinition};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Field} ->
            {error, {missing_field, Field}}
    end.

check_required_fields(Fields, Map) ->
    check_required_fields(Fields, Map, ok).

check_required_fields([], _Map, Result) ->
    Result;

check_required_fields([Field|Rest], Map, Result) ->
    case Result of
        ok ->
            case maps:is_key(Field, Map) of
                true ->
                    check_required_fields(Rest, Map, ok);
                false ->
                    check_required_fields(Rest, Map, {error, Field})
            end;
        _ ->
            check_required_fields(Rest, Map, Result)
    end.

validate_version_format(Version) when is_binary(Version) ->
    %% Simple version validation (SemVer)
    case re:run(Version, "^[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9]+(\.[0-9]+)*)?$") of
        match ->
            ok;
        nomatch ->
            {error, invalid_version_format}
    end;

validate_version_format(_) ->
    {error, invalid_version_format}.