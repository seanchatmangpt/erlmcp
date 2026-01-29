%%%-------------------------------------------------------------------
%% @doc erlmcp_auth.hrl - Authentication and Authorization Definitions
%%
%% This header defines common records, types, and macros for
%% authentication and authorization across erlmcp modules.
%%
%% @end
%%%-------------------------------------------------------------------

-ifndef(ERLMCP_AUTH_HRL).
-define(ERLMCP_AUTH_HRL, 1).

%%====================================================================
%% Authentication Rate Limiter Configuration
%%====================================================================

-define(DEFAULT_AUTH_MAX_ATTEMPTS_PER_SECOND, 10).
-define(DEFAULT_AUTH_WINDOW_MS, 1000).
-define(DEFAULT_AUTH_MAX_FAILURES, 5).
-define(DEFAULT_AUTH_BLOCK_DURATION_MS, 300000). %% 5 minutes
-define(DEFAULT_AUTH_BACKOFF_LEVELS, [0, 1000, 2000, 4000, 8000, 16000]).
-define(DEFAULT_AUTH_CLEANUP_INTERVAL_MS, 60000). %% 1 minute

%%====================================================================
%% Authentication Method Types
%%====================================================================

-type auth_method() :: api_key | jwt | oauth2 | mtls.
-type auth_token() :: binary().
-type user_id() :: binary().
-type session_id() :: binary().
-type role() :: binary().
-type permission() :: binary().
-type resource() :: binary().

%%====================================================================
%% Session Record
%%====================================================================

-record(session, {
    session_id :: session_id(),
    user_id :: user_id(),
    roles :: [role()],
    permissions :: [permission()],
    auth_method :: auth_method(),
    created_at :: integer(),
    expires_at :: integer(),
    metadata :: map()
}).

-type session() :: #session{}.

%%====================================================================
%% Rate Limiter State Records
%%====================================================================

-record(rate_limit_state, {
    count :: non_neg_integer(),
    window_start :: integer() %% milliseconds
}).

-record(block_state, {
    reason :: term(),
    blocked_until :: integer(), %% milliseconds
    failure_count :: non_neg_integer(),
    backoff_level :: 0..5
}).

-record(client_stats, {
    client_id :: binary() | undefined,
    ip_address :: inet:ip_address() | undefined,
    total_attempts :: non_neg_integer(),
    successful_auths :: non_neg_integer(),
    failed_auths :: non_neg_integer(),
    rate_limited_count :: non_neg_integer(),
    blocked_count :: non_neg_integer(),
    current_backoff_level :: 0..5,
    last_attempt_at :: integer() | undefined
}).

%%====================================================================
%% Authentication Result Macros
%%====================================================================

-define(AUTH_SUCCESS(SessionId), {ok, SessionId}).
-define(AUTH_FAILED(Reason), {error, auth_failed, Reason}).
-define(AUTH_RATE_LIMITED, {error, rate_limited}).
-define(AUTH_BLOCKED(Reason), {error, blocked, Reason}).
-define(AUTH_INVALID_CREDENTIALS, {error, invalid_credentials}).
-define(AUTH_TOKEN_EXPIRED, {error, token_expired}).
-define(AUTH_SESSION_INVALID, {error, invalid_session}).

%%====================================================================
%% Convenience Macros
%%====================================================================

-define(IS_AUTH_SUCCESS(Result), element(1, Result) =:= ok).
-define(IS_AUTH_FAILURE(Result), element(1, Result) =:= error).

-endif.
