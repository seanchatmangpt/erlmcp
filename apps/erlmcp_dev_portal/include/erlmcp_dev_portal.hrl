%%====================================================================
%% Record Definitions for Developer Portal
%%====================================================================

%% User record
-record(user, {
    id :: binary(),
    name :: binary(),
    email :: binary(),
    password_hash :: binary(),
    created :: integer(),
    last_login :: integer() | undefined,
    api_keys :: [map()],
    preferences :: map()
}).

%% API Key record
-record(api_key, {
    id :: binary(),
    user_id :: binary(),
    name :: binary(),
    key_hash :: binary(),
    permissions :: [binary()],
    created :: integer(),
    expires :: integer() | undefined,
    last_used :: integer() | undefined,
    usage_count :: integer(),
    rate_limit :: map()
}).

%% Application record
-record(application, {
    id :: binary(),
    user_id :: binary(),
    name :: binary(),
    description :: binary(),
    api_keys :: [binary()],
    created :: integer(),
    last_updated :: integer(),
    status :: active | suspended | deleted,
    metadata :: map()
}).

%% Forum Post record
-record(forum_post, {
    id :: binary(),
    title :: binary(),
    content :: binary(),
    author :: binary(),
    category :: binary(),
    created :: integer(),
    updated :: integer(),
    views :: integer(),
    replies :: [#forum_reply{}],
    tags :: [binary()],
    status :: open | closed | locked
}).

%% Forum Reply record
-record(forum_reply, {
    id :: binary(),
    post_id :: binary(),
    content :: binary(),
    author :: binary(),
    created :: integer(),
    parent_reply :: binary() | undefined
}).

%% Tutorial record
-record(tutorial, {
    id :: binary(),
    title :: binary(),
    content :: binary(),
    author :: binary(),
    category :: binary(),
    created :: integer(),
    updated :: integer(),
    views :: integer(),
    rating :: float(),
    tags :: [binary()],
    status :: draft | published | archived
}).

%% Support Ticket record
-record(support_ticket, {
    id :: binary(),
    user_id :: binary(),
    subject :: binary(),
    description :: binary(),
    category :: binary(),
    status :: open | in_progress | resolved | closed,
    priority :: low | normal | high | critical,
    created :: integer(),
    updated :: integer(),
    replies :: [#support_reply{}]
}).

%% Support Reply record
-record(support_reply, {
    id :: binary(),
    ticket_id :: binary(),
    content :: binary(),
    author :: binary(),
    created :: integer(),
    type :: internal | public
}).

%% Analytics record
-record(analytics_event, {
    id :: binary(),
    type :: binary(),
    user_id :: binary() | undefined,
    api_id :: binary() | undefined,
    data :: map(),
    timestamp :: integer(),
    metadata :: map()
}).

%% Configuration record
-record(dev_portal_config, {
    id :: atom(),
    config :: map(),
    timestamp :: integer()
}).

%%====================================================================
%% Constants
%%====================================================================

-define(DEFAULT_SESSION_TIMEOUT, 86400). % 24 hours
-define(DEFAULT_RATE_LIMIT, 100). % requests per minute
-define(DEFAULT_BURST_LIMIT, 200). % burst requests
-define(DEFAULT_CACHE_TTL, 3600). % 1 hour
-define(DEFAULT_PAGE_SIZE, 20).
-define(MAX_PAGE_SIZE, 100).

%% Error codes
-define(ERROR_NOT_FOUND, 404).
-define(ERROR_UNAUTHORIZED, 401).
-define(ERROR_FORBIDDEN, 403).
-define(ERROR_BAD_REQUEST, 400).
-define(ERROR_INTERNAL, 500).
-define(ERROR_RATE_LIMITED, 429).

%% Status codes
-define(STATUS_ACTIVE, active).
-define(STATUS_SUSPENDED, suspended).
-define(STATUS_DELETED, deleted).
-define(STATUS_OPEN, open).
-define(STATUS_CLOSED, closed).

%%====================================================================
%% Macros
%%====================================================================

-define(IS_VALID_EMAIL(Email),
    re:run(Email, "^[^@]+@[^@]+\\.[^@]+$") =/= nomatch).

-define(IS_VALID_PASSWORD(Password),
    length(Password) >= 8).

-define(GENERATE_ID(),
    uuid:to_string(uuid:new())).

-define(TIMESTAMP(),
    erlang:system_time(second)).

-define(LOG(Format, Args),
    error_logger:info_msg("[~w] " ++ Format, [?MODULE | Args])).

-define(LOG_ERROR(Format, Args),
    error_logger:error_msg("[~w] " ++ Format, [?MODULE | Args])).