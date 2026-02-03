-ifndef(ERLMCP_API_GATEWAY_HRL).
-define(ERLMCP_API_GATEWAY_HRL, 1).

%% API Gateway Constants
-define(API_GATEWAY_VERSION, <<"3.0.0">>).
-define(DEFAULT_PORT, 8080).
-define(DEFAULT_SSL_PORT, 8443).
-define(DEFAULT_MAX_CONNECTIONS, 10000).
-define(DEFAULT_TIMEOUT, 30000).

%% Error Codes
-define(ERR_API_NOT_FOUND, -40001).
-define(ERR_API_ALREADY_EXISTS, -40002).
-define(ERR_ROUTE_NOT_FOUND, -40003).
-define(ERR_CONSUMER_NOT_FOUND, -40004).
-defineERR_AUTHENTICATION_FAILED, -40005).
-define(ERR_AUTHORIZATION_FAILED, -40006).
-define(ERR_RATE_LIMIT_EXCEEDED, -40007).
-defineERR_INVALID_API_KEY, -40008).
-defineERR_INVALID_CONFIGURATION, -40009).
-defineERR_GATEWAY_SERVICE_UNAVAILABLE, -40010).

%% Rate Limit Headers
-define(RATE_LIMIT_LIMIT, <<"X-RateLimit-Limit">>).
-define(RATE_LIMIT_REMAINING, <<"X-RateLimit-Remaining">>).
-define(RATE_LIMIT_RESET, <<"X-RateLimit-Reset">>).
-define(RATE_LIMIT_LIMIT_MINUTE, <<"X-RateLimit-Limit-Minute">>).
-define(RATE_LIMIT_REMAINING_MINUTE, <<"X-RateLimit-Remaining-Minute">>).

%% Security Headers
-define(API_KEY_HEADER, <<"X-API-Key">>).
-define(AUTHORIZATION_HEADER, <<"Authorization">>).
-define(API_VERSION_HEADER, <<"X-API-Version">>).

%% Database Records
-record(api_definition, {
    id :: binary(),
    name :: binary(),
    version :: binary(),
    description :: binary(),
    base_path :: binary(),
    protocols :: [binary()],
    servers :: [map()],
    contact :: map(),
    license :: map(),
    tags :: [binary()],
    created_at :: integer(),
    updated_at :: integer(),
    status :: active | inactive | deprecated,
    documentation :: map()
}).

-record(api_route, {
    id :: binary(),
    api_id :: binary(),
    path :: binary(),
    method :: binary(),
    target :: binary(),
    upstream_url :: binary(),
    strip_path :: boolean(),
    preserve_host :: boolean(),
    enabled :: boolean(),
    rate_limit :: map(),
    authentication :: map(),
    authorization :: map(),
    plugins :: [binary()],
    config :: map(),
    created_at :: integer(),
    updated_at :: integer()
}).

-record(api_version, {
    id :: binary(),
    api_id :: binary(),
    version :: binary(),
    semver :: binary(),
    status :: active | deprecated | retired,
    created_at :: integer(),
    updated_at :: integer(),
    deprecated_on :: binary() | null,
    retirement_date :: binary() | null,
    documentation_url :: binary()
}).

-record(consumer, {
    id :: binary(),
    name :: binary(),
    email :: binary(),
    status :: active | suspended | deleted,
    api_keys :: [binary()],
    applications :: [map()],
    rate_limit :: map(),
    authentication :: map(),
    metadata :: map(),
    created_at :: integer(),
    updated_at :: integer(),
    last_active :: integer()
}).

-record(consumer_key, {
    id :: binary(),
    consumer_id :: binary(),
    key :: binary(),
    secret :: binary(),
    permissions :: [binary()],
    scopes :: [binary()],
    created_at :: integer(),
    expires_at :: integer(),
    last_used :: integer()
}).

-record(plugin, {
    id :: binary(),
    name :: binary(),
    version :: binary(),
    enabled :: boolean(),
    config :: map(),
    hooks :: [binary()],
    dependencies :: [binary()],
    created_at :: integer(),
    updated_at :: integer()
}).

-record(analytics_event, {
    id :: binary(),
    api_id :: binary(),
    route_id :: binary(),
    consumer_id :: binary(),
    event_type :: binary(),
    timestamp :: integer(),
    request_id :: binary(),
    ip_address :: binary(),
    user_agent :: binary(),
    response_time :: integer(),
    status_code :: integer(),
    request_size :: integer(),
    response_size :: integer(),
    error_message :: binary()
}).

-record(api_gateway_config, {
    id :: binary(),
    config :: map(),
    rate_limits :: map(),
    security_policies :: map(),
    analytics_config :: map(),
    updated_at :: integer()
}).

-endif.