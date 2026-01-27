-module(erlmcp_origin_validator).

%% API for origin validation - DNS rebinding protection
-export([
    validate_origin/2,
    validate_origin/3,
    matches_origin_pattern/2,
    get_default_allowed_origins/0,
    is_origin_allowed/2
]).

-include_lib("kernel/include/logger.hrl").

-type origin() :: binary() | string().
-type allowed_origins() :: [origin()].
-type validation_result() :: {ok, origin()} | {error, forbidden}.

%%====================================================================
%% Constants
%%====================================================================

%% Default safe origins for localhost development
-define(DEFAULT_ALLOWED_ORIGINS, [
    <<"http://127.0.0.1:*">>,
    <<"http://localhost:*">>,
    <<"http://[::1]:*">>,
    <<"https://127.0.0.1:*">>,
    <<"https://localhost:*">>,
    <<"https://[::1]:*">>
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate Origin header against configured whitelist
%% Implements DNS rebinding attack protection by validating Origin header
%% on all HTTP requests against a configured whitelist.
%%
%% Returns {ok, Origin} if valid, {error, forbidden} if invalid.
%% Missing Origin header is treated as OK (same-origin request).
%%
%% @param OriginHeader - The Origin header value from request (or undefined)
%% @param AllowedOrigins - List of allowed origins from config
%% @returns {ok, Origin} | {error, forbidden}
-spec validate_origin(term(), allowed_origins()) -> validation_result().
validate_origin(undefined, _AllowedOrigins) ->
    %% Origin header not present - OK for same-origin requests
    logger:debug("Origin header missing (same-origin request)"),
    {ok, <<"same-origin">>};
validate_origin(Origin, AllowedOrigins) ->
    case is_origin_allowed(Origin, AllowedOrigins) of
        true ->
            OriginStr = ensure_string(Origin),
            logger:debug("Origin validated: ~s", [OriginStr]),
            {ok, ensure_binary(Origin)};
        false ->
            OriginStr = ensure_string(Origin),
            logger:warning("Origin rejected - DNS rebinding attack prevented: ~s", [OriginStr]),
            {error, forbidden}
    end.

%% @doc Validate origin with config lookup
%% Fetches allowed_origins from configuration and validates.
%% @param OriginHeader - The Origin header value from request
%% @param AllowedOrigins - List of allowed origins
%% @param Config - Configuration dict/map (optional)
%% @returns {ok, Origin} | {error, forbidden}
-spec validate_origin(term(), allowed_origins(), map() | list()) -> validation_result().
validate_origin(Origin, _Default, Config) when is_map(Config) ->
    AllowedOrigins = maps:get(allowed_origins, Config, get_default_allowed_origins()),
    validate_origin(Origin, AllowedOrigins);
validate_origin(Origin, _Default, Config) when is_list(Config) ->
    AllowedOrigins = proplists:get_value(allowed_origins, Config, get_default_allowed_origins()),
    validate_origin(Origin, AllowedOrigins).

%% @doc Check if origin matches any allowed pattern
%% Supports exact match and wildcard ports:
%%   "http://localhost" - exact match
%%   "http://localhost:*" - any port
%%   "http://localhost:3000" - exact port match
%%
%% @param Origin - The origin to check
%% @param AllowedOrigins - List of allowed patterns
%% @returns true if origin matches any pattern, false otherwise
-spec is_origin_allowed(origin(), allowed_origins()) -> boolean().
is_origin_allowed(_Origin, []) ->
    false;
is_origin_allowed(Origin, [Pattern | Rest]) ->
    case matches_origin_pattern(Origin, Pattern) of
        true -> true;
        false -> is_origin_allowed(Origin, Rest)
    end.

%% @doc Check if origin matches a specific pattern
%% Supports:
%%   Exact match: "http://localhost:8080" = "http://localhost:8080"
%%   Wildcard port: "http://localhost:*" matches "http://localhost:3000"
%%
%% @param Origin - The origin to check
%% @param Pattern - The pattern to match against
%% @returns true if matches, false otherwise
-spec matches_origin_pattern(origin(), origin()) -> boolean().
matches_origin_pattern(Origin, Pattern) ->
    OriginStr = ensure_string(Origin),
    PatternStr = ensure_string(Pattern),

    case string:find(PatternStr, ":*") of
        nomatch ->
            %% Exact match - case sensitive for URL components
            OriginStr =:= PatternStr;
        _PortPos ->
            %% Wildcard port - compare scheme + host only
            match_without_port(OriginStr, PatternStr)
    end.

%% @doc Get default safe origins for localhost
%% Default whitelist allows only localhost addresses (127.0.0.1, ::1, localhost)
%% on any port for both HTTP and HTTPS.
%%
%% @returns List of default allowed origins
-spec get_default_allowed_origins() -> allowed_origins().
get_default_allowed_origins() ->
    [ensure_string(O) || O <- ?DEFAULT_ALLOWED_ORIGINS].

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% @private
%% Ensure value is a string
-spec ensure_string(term()) -> string().
ensure_string(S) when is_list(S) ->
    S;
ensure_string(B) when is_binary(B) ->
    binary_to_list(B);
ensure_string(A) when is_atom(A) ->
    atom_to_list(A);
ensure_string(X) ->
    io_lib:format("~p", [X]).

%% @private
%% Ensure value is binary
-spec ensure_binary(term()) -> binary().
ensure_binary(B) when is_binary(B) ->
    B;
ensure_binary(S) when is_list(S) ->
    list_to_binary(S);
ensure_binary(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
ensure_binary(X) ->
    iolist_to_binary(io_lib:format("~p", [X])).

%% @private
%% Match origin and pattern ignoring port
%% Extracts scheme://host and compares
-spec match_without_port(string(), string()) -> boolean().
match_without_port(Origin, Pattern) ->
    OriginBase = extract_scheme_host(Origin),
    PatternBase = extract_scheme_host(Pattern),
    OriginBase =:= PatternBase.

%% @private
%% Extract scheme://host from URL, removing port and path
%% Examples:
%%   "http://localhost:8080" -> "http://localhost"
%%   "https://127.0.0.1:443/path" -> "https://127.0.0.1"
%%   "http://[::1]:8080" -> "http://[::1]"
-spec extract_scheme_host(string()) -> string().
extract_scheme_host(Url) ->
    case string:split(Url, "://") of
        [Scheme, Rest] ->
            HostPart = case string:split(Rest, "/") of
                [H | _] -> H;
                [H] -> H
            end,
            %% Handle IPv6 addresses in brackets
            case extract_host_from_hostport(HostPart) of
                {IPv6Host, _Port} ->
                    case string:find(IPv6Host, ":") of
                        nomatch ->
                            Scheme ++ "://" ++ IPv6Host;
                        _ ->
                            Scheme ++ "://" ++ IPv6Host
                    end;
                {Host, _Port} ->
                    Scheme ++ "://" ++ Host;
                Host ->
                    Scheme ++ "://" ++ Host
            end;
        _ ->
            Url
    end.

%% @private
%% Extract host from host:port combination
%% Handles IPv6 addresses like [::1]:8080
-spec extract_host_from_hostport(string()) -> {string(), integer()} | string().
extract_host_from_hostport(HostPort) ->
    %% Handle IPv6 [::1]:port format
    case string:find(HostPort, "]:") of
        nomatch ->
            %% Not IPv6 with port
            case string:split(HostPort, ":") of
                [Host] ->
                    Host;
                [Host, PortStr] ->
                    try
                        Port = list_to_integer(PortStr),
                        {Host, Port}
                    catch
                        _:_ -> HostPort
                    end
            end;
        _Pos ->
            %% IPv6 with port
            case string:split(HostPort, "]:") of
                [BracketHost, PortStr] ->
                    Host = BracketHost ++ "]",
                    try
                        Port = list_to_integer(PortStr),
                        {Host, Port}
                    catch
                        _:_ -> HostPort
                    end;
                _ ->
                    HostPort
            end
    end.
