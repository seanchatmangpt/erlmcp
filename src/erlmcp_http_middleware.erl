-module(erlmcp_http_middleware).

%% API
-export([
    validate_request/2,
    extract_session_header/1,
    inject_session_header/2
]).

-include_lib("kernel/include/logger.hrl").

-type request() :: #{
    method := atom(),
    path := string(),
    headers := [{string(), string()}],
    body => binary()
}.

-type response() :: #{
    status := non_neg_integer(),
    headers := [{string(), string()}],
    body => binary()
}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate incoming HTTP request for security compliance
%% @param Request - HTTP request map with method, path, headers, body
%% @param Config - HTTP security configuration
%% @returns {ok, {SessionId, ValidatedRequest}} | {error, ErrorCode, ErrorMsg}
-spec validate_request(request(), list()) ->
    {ok, {binary(), request()}} |
    {error, 400 | 403 | 404, string()}.
validate_request(Request, Config) ->
    %% Step 1: Validate Origin header
    case extract_origin_header(Request) of
        undefined ->
            logger:warning("Missing Origin header in request"),
            {error, 403, "Missing Origin header"};
        Origin ->
            case erlmcp_http_security:validate_origin(Origin, Config) of
                {error, invalid_origin} ->
                    logger:warning("Invalid origin: ~s", [Origin]),
                    {error, 403, "Invalid origin"};
                {ok, _} ->
                    %% Step 2: Check if this is initialization request
                    case is_init_request(Request) of
                        true ->
                            %% Initialize - no session required
                            {ok, {undefined, Request}};
                        false ->
                            %% Regular request - session required
                            validate_session_request(Request, Config)
                    end
            end
    end.

%% @doc Extract session ID from MCP-Session-Id header
%% @param Request - HTTP request map
%% @returns SessionId or undefined
-spec extract_session_header(request()) -> binary() | undefined.
extract_session_header(#{headers := Headers}) ->
    case lists:keyfind("mcp-session-id", 1, Headers) of
        {_, SessionId} -> SessionId;
        false -> undefined
    end;
extract_session_header(_) ->
    undefined.

%% @doc Inject session ID into response headers
%% @param Response - HTTP response map
%% @param SessionId - Session ID to inject
%% @returns Updated response with session header
-spec inject_session_header(response(), binary()) -> response().
inject_session_header(#{headers := Headers} = Response, SessionId) ->
    NewHeaders = lists:keystore("mcp-session-id", 1, Headers, {"mcp-session-id", SessionId}),
    Response#{headers => NewHeaders};
inject_session_header(#{} = Response, SessionId) ->
    Response#{
        headers => [{"mcp-session-id", SessionId}]
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Extract Origin header from request
-spec extract_origin_header(request()) -> string() | undefined.
extract_origin_header(#{headers := Headers}) ->
    case lists:keyfind("origin", 1, Headers) of
        {_, Origin} -> Origin;
        false ->
            %% Some clients use referer instead
            case lists:keyfind("referer", 1, Headers) of
                {_, Referer} -> Referer;
                false -> undefined
            end
    end;
extract_origin_header(_) ->
    undefined.

%% @private
%% Check if request is initialization request (creates new session)
%% Init requests typically go to /initialize or use specific method
-spec is_init_request(request()) -> boolean().
is_init_request(#{path := Path}) ->
    %% Match initialization paths
    case string:find(Path, "/initialize") of
        nomatch -> false;
        _ -> true
    end;
is_init_request(_) ->
    false.

%% @private
%% Validate session for non-init requests
-spec validate_session_request(request(), list()) ->
    {ok, {binary(), request()}} |
    {error, 400 | 404, string()}.
validate_session_request(Request, _Config) ->
    case extract_session_header(Request) of
        undefined ->
            logger:warning("Missing session header in request"),
            {error, 400, "Missing MCP-Session-Id header"};
        SessionId ->
            case erlmcp_session_manager:validate_session(SessionId) of
                {ok, _} ->
                    {ok, {SessionId, Request}};
                {error, expired} ->
                    logger:warning("Session expired: ~s", [SessionId]),
                    {error, 404, "Session expired"};
                {error, not_found} ->
                    logger:warning("Session not found: ~s", [SessionId]),
                    {error, 404, "Session not found"}
            end
    end.
