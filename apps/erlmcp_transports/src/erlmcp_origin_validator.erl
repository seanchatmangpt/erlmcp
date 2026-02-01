%%%-------------------------------------------------------------------
%%% @doc erlmcp_origin_validator - Origin Validation for DNS Rebinding Protection
%%%
%%% Validates Origin headers to prevent DNS rebinding attacks.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_origin_validator).

%% API exports
-export([
    validate_origin/2,
    get_default_allowed_origins/0
]).

%% Types
-type origin() :: binary() | undefined.
-type origins() :: [binary()].
-type validation_result() :: {ok, binary()} | {error, forbidden}.

-export_type([origin/0, origins/0, validation_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate Origin header against allowed origins
%% Armstrong Principle: Secure by default - undefined origins are REJECTED
%% unless explicitly allowed in the whitelist.
-spec validate_origin(origin(), origins()) -> validation_result().
validate_origin(undefined, AllowedOrigins) ->
    %% SECURITY FIX (P0): Reject undefined origins by default
    %% Armstrong principle: "make unsafe defaults unrepresentable"
    %% Previous behavior allowed ANY undefined origin (DNS rebinding attack vector)
    %% New behavior: undefined is only allowed if explicitly in whitelist
    case lists:member(<<"undefined">>, AllowedOrigins) orelse
         lists:member(<<"null">>, AllowedOrigins) of
        true ->
            logger:debug("Origin validation: undefined origin explicitly allowed by whitelist"),
            {ok, <<"undefined">>};
        false ->
            logger:warning("Origin validation DENIED: undefined origin not in allowed list (DNS rebinding protection)"),
            log_security_violation(<<"undefined">>, AllowedOrigins),
            {error, forbidden}
    end;
validate_origin(Origin, AllowedOrigins) when is_binary(Origin), is_list(AllowedOrigins) ->
    case is_origin_allowed(Origin, AllowedOrigins) of
        true ->
            logger:info("Origin validation SUCCESS: ~s (allowed)", [Origin]),
            {ok, Origin};
        false ->
            logger:warning("Origin validation DENIED: ~s (DNS rebinding attack vector - not in allowed list: ~p)",
                [Origin, AllowedOrigins]),
            %% Log security violation for audit trail
            log_security_violation(Origin, AllowedOrigins),
            {error, forbidden}
    end.

%% @doc Get default allowed origins from configuration
%% SECURITY FIX (P0): Armstrong principle - empty list by default
%% Origins must be explicitly whitelisted by configuration.
%% Previous behavior: Permissive localhost list (development convenience)
%% New behavior: Empty list (production security)
%%
%% To enable localhost for development, configure explicitly:
%%   application:set_env(erlmcp, allowed_origins, [
%%       <<"http://localhost:8080">>, <<"http://127.0.0.1:8080">>
%%   ]).
-spec get_default_allowed_origins() -> origins().
get_default_allowed_origins() ->
    %% SECURITY: Empty by default - explicit whitelist required
    %% Read from application environment if configured
    case application:get_env(erlmcp, allowed_origins, undefined) of
        undefined ->
            %% No configuration - return empty list (deny all)
            logger:warning("No origin whitelist configured - all origins will be rejected. "
                          "Set 'allowed_origins' in app config to allow specific origins."),
            [];
        Origins when is_list(Origins) ->
            Origins;
        InvalidConfig ->
            logger:error("Invalid allowed_origins configuration: ~p (expected list)", [InvalidConfig]),
            []
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Check if origin is in allowed list
is_origin_allowed(_Origin, []) ->
    false;
is_origin_allowed(Origin, [AllowedOrigin | Rest]) ->
    case match_origin(Origin, AllowedOrigin) of
        true ->
            true;
        false ->
            is_origin_allowed(Origin, Rest)
    end.

%% @private Match origin against allowed origin
match_origin(Origin, AllowedOrigin) when is_binary(Origin), is_binary(AllowedOrigin) ->
    %% Exact match
    case Origin of
        AllowedOrigin ->
            true;
        _ ->
            %% Try with wildcard matching
            match_wildcard_origin(Origin, AllowedOrigin)
    end.

%% @private Match origin with wildcard support
match_wildcard_origin(_Origin, <<"*">>) ->
    true;
match_wildcard_origin(Origin, <<"*.", Rest/binary>>) ->
    %% Match any subdomain
    case binary:match(Origin, Rest) of
        {Pos, _} when Pos > 0 ->
            %% Check if Rest starts after a dot
            Before = binary:part(Origin, {0, Pos}),
            case binary:match(Before, <<".">>) of
                nomatch ->
                    false;
                _ ->
                    %% Check if Rest matches the end
                    SuffixSize = byte_size(Rest),
                    OriginSize = byte_size(Origin),
                    case OriginSize >= Pos + SuffixSize of
                        true ->
                            EndPart = binary:part(Origin, {OriginSize - SuffixSize, SuffixSize}),
                            EndPart =:= Rest;
                        false ->
                            false
                    end
            end;
        _ ->
            false
    end;
match_wildcard_origin(_Origin, _AllowedOrigin) ->
    false.

%%====================================================================
%% Security Logging Functions
%%====================================================================

%% @private Log security violation for audit trail
-spec log_security_violation(binary(), origins()) -> ok.
log_security_violation(Origin, AllowedOrigins) ->
    %% Log to error_logger for security audit
    logger:error("SECURITY VIOLATION - Origin not allowed: ~s~n"
                 "Allowed origins: ~p~n"
                 "Attack type: Potential DNS rebinding~n"
                 "Action: Request rejected with 403 Forbidden~n"
                 "Timestamp: ~s",
                 [Origin, AllowedOrigins, format_timestamp()]),

    %% Could also send to external security monitoring system here
    %% For example: send_to_siem(Origin, AllowedOrigins)

    ok.

%% @private Format timestamp for logging
-spec format_timestamp() -> binary().
format_timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w UTC",
        [Year, Month, Day, Hour, Minute, Second])).
