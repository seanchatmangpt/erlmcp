-module(erlmcp_http_security).

%% API
-export([
    validate_origin/2,
    validate_session/1,
    require_https/1,
    is_localhost/1
]).

-include_lib("kernel/include/logger.hrl").

-type origin() :: binary() | string().
-type validation_result() :: {ok, origin()} | {error, invalid_origin}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate incoming origin header against whitelist from config
%% @param Origin - The Origin header value from the request
%% @param Config - Configuration from sys.config http_security section
%% @returns {ok, Origin} if valid, {error, invalid_origin} otherwise
-spec validate_origin(origin(), list()) -> validation_result().
validate_origin(Origin, Config) ->
    AllowedOrigins = proplists:get_value(allowed_origins, Config, []),
    OriginStr = normalize_origin(Origin),

    case matches_whitelist(OriginStr, AllowedOrigins) of
        true ->
            logger:debug("Origin validated: ~s", [OriginStr]),
            {ok, OriginStr};
        false ->
            logger:warning("Origin rejected: ~s (not in whitelist)", [OriginStr]),
            {error, invalid_origin}
    end.

%% @doc Validate session ID exists and hasn't expired
%% @param SessionId - The session ID from MCP-Session-Id header
%% @returns {ok, SessionId} if valid, {error, session_expired} if expired
-spec validate_session(binary() | string()) -> {ok, binary()} | {error, session_expired}.
validate_session(SessionId) ->
    case erlmcp_session_manager:validate_session(SessionId) of
        {ok, _} ->
            logger:debug("Session validated: ~s", [SessionId]),
            {ok, normalize_session_id(SessionId)};
        {error, expired} ->
            logger:warning("Session expired: ~s", [SessionId]),
            {error, session_expired};
        {error, not_found} ->
            logger:warning("Session not found: ~s", [SessionId]),
            {error, session_expired}
    end.

%% @doc Check if HTTPS is required and enforce it
%% @param Config - Configuration from sys.config http_security section
%% @returns true if HTTPS enforcement is enabled, false otherwise
-spec require_https(list()) -> boolean().
require_https(Config) ->
    proplists:get_value(require_https, Config, false).

%% @doc Check if origin is localhost (development convenience)
%% @param Origin - The Origin header value
%% @returns true if localhost-based origin, false otherwise
-spec is_localhost(origin()) -> boolean().
is_localhost(Origin) ->
    OriginStr = normalize_origin(Origin),
    string:find(OriginStr, "localhost") =:= nomatch
        orelse string:find(OriginStr, "127.0.0.1") =:= nomatch.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Normalize origin to string for comparison
-spec normalize_origin(origin()) -> string().
normalize_origin(Origin) when is_binary(Origin) ->
    binary_to_list(Origin);
normalize_origin(Origin) when is_list(Origin) ->
    Origin.

%% @private
%% Normalize session ID to string
-spec normalize_session_id(binary() | string()) -> binary().
normalize_session_id(SessionId) when is_binary(SessionId) ->
    SessionId;
normalize_session_id(SessionId) when is_list(SessionId) ->
    list_to_binary(SessionId).

%% @private
%% Check if origin matches any whitelist pattern
%% Patterns support:
%%   "http://localhost" - exact match
%%   "http://localhost:*" - match any port
%%   "https://127.0.0.1:8080" - exact match with port
-spec matches_whitelist(string(), [string()]) -> boolean().
matches_whitelist(_Origin, []) ->
    false;
matches_whitelist(Origin, [Pattern | Rest]) ->
    case matches_pattern(Origin, Pattern) of
        true -> true;
        false -> matches_whitelist(Origin, Rest)
    end.

%% @private
%% Check if origin matches a single pattern
-spec matches_pattern(string(), string()) -> boolean().
matches_pattern(Origin, Pattern) ->
    case string:find(Pattern, ":*") of
        nomatch ->
            %% Exact match
            Origin =:= Pattern;
        _PortPos ->
            %% Wildcard port match - compare without port
            OriginBase = extract_origin_base(Origin),
            PatternBase = extract_origin_base(Pattern),
            OriginBase =:= PatternBase
    end.

%% @private
%% Extract scheme + host from origin URL (without port)
%% E.g., "http://localhost:8080" -> "http://localhost"
-spec extract_origin_base(string()) -> string().
extract_origin_base(Origin) ->
    case string:split(Origin, "://") of
        [Scheme, Rest] ->
            case string:split(Rest, ":") of
                [Host] ->
                    Scheme ++ "://" ++ Host;
                [Host, _Port] ->
                    Scheme ++ "://" ++ Host
            end;
        _ ->
            Origin
    end.
