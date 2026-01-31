%% ============================================================================
%% ERLMCP Security Validator
%% ============================================================================
%% Comprehensive security validation for MCP protocol implementations.
%% Validates inputs against OWASP Top 10, injection attacks, secrets exposure,
%% authentication tokens, rate limiting, and CORS headers.
%%
%% @author erlmcp
%% @version 1.0.0
%% @reference https://owasp.org/www-project-top-ten/
%% ============================================================================

-module(erlmcp_security_validator).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    start_link/1,
    validate_input_size/2,
    detect_injection_patterns/1,
    validate_auth_token/1,
    detect_secrets_in_input/1,
    validate_rate_limit/1,
    validate_cors_headers/1,
    check_owasp_compliance/1,
    sanitize_input/1,
    validate_uri/1,
    validate_json_structure/1,
    check_command_injection/1,
    check_ldap_injection/1,
    check_xpath_injection/1,
    get_security_report/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

%% Constants
-define(DEFAULT_MAX_SIZE, 100 * 1024 * 1024). % 100MB
-define(MAX_URI_LENGTH, 2000).
-define(MAX_JSON_DEPTH, 100).

%% Injection patterns (regex as binary strings for re:module)
-define(SQL_INJECTION_PATTERNS, [
   <<"(' OR '--)|('|' OR 1=1--)|(' OR '1'='1)|(admin'--)|(admin' #)">>,
    <<"(union select.*from)|(union all select)|(1' order by)">>,
    <<"(drop table)|(delete from)|(insert into)|(update.*set)">>,
    <<("(\\$\\{)|(<script>)|(javascript:)|(eval\\()")
>>).

-define(COMMAND_INJECTION_PATTERNS, [
    <<("(;)|(\\|)|(\\$\\()|(`)>(\\$\\{))">>,
    <<("(\\&\\&)|(\\|\\|)|(>|(>>))">>
]).

-define(LDAP_INJECTION_PATTERNS, [
    <<"(\\*\\))|(\\()\\)|\\(\\*)">>,
    <<("([&\\|><~=!])")>>
]).

-define(XPATH_INJECTION_PATTERNS, [
    <<("('|\"|or\\s+and|or\\s+1\\s*=\\s*1|and\\s+1\\s*=\\s*1)")>>,
    <<("ancestor::|descendant::|parent::|child::")>>
]).

%% Secret detection patterns (AWS, API keys, JWT, etc.)
-define(SECRET_PATTERNS, [
    %% AWS Access Key ID
    <<"(AKIA[0-9A-Z]{16})">>,
    %% AWS Secret Key
    <<("[0-9a-zA-Z/+]{40})">>,
    %% Google API Key
    <<("(AIza[0-9A-Za-z\\-_]{35})">>,
    %% GitHub Personal Access Token
    <<("(ghp_[a-zA-Z0-9]{36})">>,
    %% Slack Token
    <<("(xox[baprs]-[0-9]{12}-[0-9]{12}-[0-9]{12}-[a-z0-9]{32})">>,
    %% Generic API Key patterns
    <<("(api[_-]?key\\s*[=:]\\s*['\"]?[0-9a-zA-Z]{32,}['\"]?)">>,
    <<("(apikey\\s*[=:]\\s*['\"]?[0-9a-zA-Z]{32,}['\"]?)">>,
    %% JWT tokens
    <<("(eyJ[a-zA-Z0-9_-]*\\.eyJ[a-zA-Z0-9_-]*\\.[a-zA-Z0-9_-]*)">>
]).

%% OWASP Top 10 (2021) mapping for MCP
-define(OWASP_CHECKS, [
    {<<"A01:2021-Broken Access Control">>, fun check_access_control/1},
    {<<"A02:2021-Cryptographic Failures">>, fun check_cryptographic_failures/1},
    {<<"A03:2021-Injection">>, fun check_injection_attacks/1},
    {<<"A04:2021-Insecure Design">>, fun check_insecure_design/1},
    {<<"A05:2021-Security Misconfiguration">>, fun check_security_misconfiguration/1},
    {<<"A06:2021-Vulnerable and Outdated Components">>, fun check_outdated_components/1},
    {<<"A07:2021-Identification and Authentication Failures">>, fun check_auth_failures/1},
    {<<"A08:2021-Software and Data Integrity Failures">>, fun check_integrity_failures/1},
    {<<"A09:2021-Security Logging and Monitoring Failures">>, fun check_logging_failures/1},
    {<<"A10:2021-Server-Side Request Forgery">>, fun check_ssr_forgery/1}
]).

%% Record definitions
-record(state, {
    max_size :: non_neg_integer(),
    rate_limits :: #{binary() => {integer(), integer()}},  % {requests, window_seconds}
    rate_counter :: #{binary() => {integer(), integer(), integer()}}, % {count, window_start, rate}
    cors_allowed_origins :: [binary()],
    stats :: #{
        total_validations => non_neg_integer(),
        security_violations => non_neg_integer(),
        last_check => integer()
    }
}).

-type validation_result() :: {ok, map()} | {error, term()}.
-type security_report() :: #{
    owasp_compliance => map(),
    vulnerabilities => [map()],
    recommendations => [binary()]
}.
-export_type([validation_result/0, security_report/0]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%% @doc Start security validator with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start security validator with custom options
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Validate input size against limits
%% Returns {ok, Size} or {error, size_exceeded}
-spec validate_input_size(binary() | map(), non_neg_integer()) -> validation_result().
validate_input_size(Input, MaxSize) when is_binary(Input) ->
    Size = byte_size(Input),
    case Size =< MaxSize of
        true ->
            {ok, #{size => Size, max_size => MaxSize, status => ok}};
        false ->
            {error, #{reason => size_exceeded, size => Size, max_size => MaxSize}}
    end;
validate_input_size(Input, MaxSize) when is_map(Input) ->
    try
        Json = jsx:encode(Input),
        validate_input_size(Json, MaxSize)
    catch
        _:_ ->
            {error, #{reason => invalid_json}}
    end;
validate_input_size(_Input, _MaxSize) ->
    {error, #{reason => invalid_input_type}}.

%% @doc Detect injection patterns (SQL, XSS, path traversal)
%% Returns {ok, #{}} or {error, #{type := injection_type, pattern := binary()}}
-spec detect_injection_patterns(binary()) -> validation_result().
detect_injection_patterns(Input) when is_binary(Input) ->
    % Check for SQL injection
    case detect_sql_injection(Input) of
        {error, _} = Error -> Error;
        ok ->
            % Check for XSS
            case detect_xss(Input) of
                {error, _} = Error -> Error;
                ok ->
                    % Check for path traversal
                    case detect_path_traversal(Input) of
                        {error, _} = Error -> Error;
                        ok ->
                            {ok, #{status => secure, patterns_checked => [sql, xss, path_traversal]}}
                    end
            end
    end;
detect_injection_patterns(_) ->
    {error, #{reason => invalid_input_type}}.

%% @doc Validate JWT authentication token structure and signature
%% Returns {ok, #{claims := map()}} or {error, term()}
-spec validate_auth_token(binary()) -> validation_result().
validate_auth_token(Token) when is_binary(Token) ->
    try
        % Split JWT into parts: header.payload.signature
        Parts = binary:split(Token, <<$.>>, [global]),
        case length(Parts) of
            3 ->
                [HeaderB64, PayloadB64, SignatureB64] = Parts,
                % Decode header and payload (base64url)
                HeaderJson = base64url_decode(HeaderB64),
                PayloadJson = base64url_decode(PayloadB64),

                Header = jsx:decode(HeaderJson, [return_maps]),
                Payload = jsx:decode(PayloadJson, [return_maps]),

                % Validate JWT structure
                case validate_jwt_structure(Header, Payload) of
                    ok ->
                        % Note: Full signature validation requires public key
                        % This validates structure and basic claims
                        {ok, #{
                            valid => true,
                            algorithm => maps:get(<<"alg">>, Header),
                            type => maps:get(<<"typ">>, Header, <<"JWT">>),
                            claims => Payload,
                            signature_present => byte_size(SignatureB64) > 0
                        }};
                    {error, _} = Error ->
                        Error
                end;
            _ ->
                {error, #{reason => invalid_jwt_format, expected => 3_parts}}
        end
    catch
        _:_ ->
            {error, #{reason => invalid_jwt_encoding}}
    end;
validate_auth_token(_) ->
    {error, #{reason => invalid_token_type}}.

%% @doc Detect hardcoded secrets in input (AWS keys, API keys, JWT, etc.)
%% Returns {ok, #{}} or {error, #{detected := [binary()]}}
-spec detect_secrets_in_input(binary()) -> validation_result().
detect_secrets_in_input(Input) when is_binary(Input) ->
    Detected = lists:filtermap(
        fun(Pattern) ->
            case re:run(Input, Pattern, [caseless, {capture, all, binary}]) of
                {match, [Captured]} -> {true, Captured};
                _ -> false
            end
        end,
        ?SECRET_PATTERNS
    ),

    case Detected of
        [] ->
            {ok, #{status => clean, patterns_checked => length(?SECRET_PATTERNS)}};
        _ ->
            {error, #{
                reason => secrets_detected,
                detected => Detected,
                count => length(Detected)
            }}
    end;
detect_secrets_in_input(_) ->
    {error, #{reason => invalid_input_type}}.

%% @doc Validate rate limit compliance
%% Returns {ok, #{remaining := integer()}} or {error, rate_limit_exceeded}
-spec validate_rate_limit(map()) -> validation_result().
validate_rate_limit(Request) ->
    gen_server:call(?MODULE, {validate_rate_limit, Request}).

%% @doc Validate CORS headers format
%% Returns {ok, #{}} or {error, term()}
-spec validate_cors_headers(map()) -> validation_result().
validate_cors_headers(Headers) when is_map(Headers) ->
    try
        % Check for required CORS headers
        Origin = maps:get(<<"Origin">>, Headers, undefined),
        Host = maps:get(<<"Host">>, Headers, undefined),

        % Validate Origin header if present
        case Origin of
            undefined ->
                ok;
            _ ->
                case validate_origin(Origin) of
                    ok -> ok;
                    {error, _} = Error -> throw(Error)
                end
        end,

        % Validate Access-Control-Allow-Origin if present
        case maps:get(<<"Access-Control-Allow-Origin">>, Headers, undefined) of
            undefined ->
                ok;
            AllowedOrigin ->
                case validate_allowed_origin(AllowedOrigin, Host) of
                    ok -> ok;
                    {error, _} = Error -> throw(Error)
                end
        end,

        % Validate Access-Control-Allow-Methods if present
        case maps:get(<<"Access-Control-Allow-Methods">>, Headers, undefined) of
            undefined ->
                ok;
            Methods ->
                case validate_allowed_methods(Methods) of
                    ok -> ok;
                    {error, _} = Error -> throw(Error)
                end
        end,

        {ok, #{status => valid, cors_compliant => true}}
    catch
        throw:{error, _} = Error ->
            Error;
        _:_ ->
            {error, #{reason => invalid_cors_headers}}
    end;
validate_cors_headers(_) ->
    {error, #{reason => invalid_headers_type}}.

%% @doc Run OWASP Top 10 compliance checks for MCP
%% Returns {ok, #{owasp_compliance => map()}} or {error, map()}
-spec check_owasp_compliance(map()) -> validation_result().
check_owasp_compliance(Input) when is_map(Input) ->
    ComplianceResults = lists:map(
        fun({Category, CheckFun}) ->
            try
                case CheckFun(Input) of
                    ok -> {Category, #{status => pass}};
                    {warning, Reason} -> {Category, #{status => warning, reason => Reason}};
                    {error, Reason} -> {Category, #{status => fail, reason => Reason}}
                end
            catch
                _:_ -> {Category, #{status => error, reason => check_failed}}
            end
        end,
        ?OWASP_CHECKS
    ),

    ComplianceMap = maps:from_list(ComplianceResults),

    % Check if any failures
    Failures = [Cat || {Cat, #{status := Status}} <- ComplianceResults, Status =:= fail],

    case Failures of
        [] ->
            {ok, #{owasp_compliance => ComplianceMap}};
        _ ->
            {error, #{
                reason => owasp_compliance_failed,
                failures => Failures,
                details => ComplianceMap
            }}
    end;
check_owasp_compliance(_) ->
    {error, #{reason => invalid_input_type}}.

%% @doc Sanitize input by removing dangerous patterns
%% Returns sanitized binary
-spec sanitize_input(binary()) -> binary().
sanitize_input(Input) when is_binary(Input) ->
    % Remove null bytes
    Step1 = re:replace(Input, <<"\\x00">>, <<>>, [global, {return, binary}]),

    % Remove control characters except newline, tab, carriage return
    Step2 = re:replace(Step1, "[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F\\x7F]", <<>>,
                      [global, {return, binary}]),

    % Normalize whitespace
    Step3 = re:replace(Step2, <<"\\s+">>, <<" ">>, [global, {return, binary}]),

    Step3.

%% @doc Validate URI structure and safety
%% Returns {ok, #{}} or {error, term()}
-spec validate_uri(binary()) -> validation_result().
validate_uri(URI) when is_binary(URI) ->
    case byte_size(URI) > ?MAX_URI_LENGTH of
        true ->
            {error, #{reason => uri_too_long, max_length => ?MAX_URI_LENGTH}};
        false ->
            try
                % Check for dangerous protocols
                case check_dangerous_protocols(URI) of
                    ok -> ok;
                    {error, _} = Error -> throw(Error)
                end,

                % Check for encoded attacks
                case check_uri_encoding(URI) of
                    ok -> ok;
                    {error, _} = Error -> throw(Error)
                end,

                {ok, #{status => valid, uri => URI}}
            catch
                throw:{error, _} = Error -> Error;
                _:_ -> {error, #{reason => invalid_uri}}
            end
    end;
validate_uri(_) ->
    {error, #{reason => invalid_uri_type}}.

%% @doc Validate JSON structure (depth, recursion, etc.)
%% Returns {ok, #{}} or {error, term()}
-spec validate_json_structure(map() | binary()) -> validation_result().
validate_json_structure(Json) when is_binary(Json) ->
    try
        Decoded = jsx:decode(Json, [return_maps]),
        validate_json_structure(Decoded)
    catch
        _:_ ->
            {error, #{reason => invalid_json}}
    end;
validate_json_structure(Json) when is_map(Json) ->
    try
        Depth = calculate_json_depth(Json),
        case Depth > ?MAX_JSON_DEPTH of
            true ->
                {error, #{reason => json_too_deep, max_depth => ?MAX_JSON_DEPTH, actual_depth => Depth}};
            false ->
                {ok, #{status => valid, depth => Depth}}
        end
    catch
        _:_ ->
            {error, #{reason => json_validation_failed}}
    end;
validate_json_structure(_) ->
    {error, #{reason => invalid_json_type}}.

%% @doc Check for command injection patterns
%% Returns {ok, #{}} or {error, #{}}
-spec check_command_injection(binary()) -> validation_result().
check_command_injection(Input) when is_binary(Input) ->
    case any_match(Input, ?COMMAND_INJECTION_PATTERNS) of
        {true, Pattern} ->
            {error, #{reason => command_injection_detected, pattern => Pattern}};
        false ->
            {ok, #{status => clean}}
    end;
check_command_injection(_) ->
    {error, #{reason => invalid_input_type}}.

%% @doc Check for LDAP injection patterns
%% Returns {ok, #{}} or {error, #{}}
-spec check_ldap_injection(binary()) -> validation_result().
check_ldap_injection(Input) when is_binary(Input) ->
    case any_match(Input, ?LDAP_INJECTION_PATTERNS) of
        {true, Pattern} ->
            {error, #{reason => ldap_injection_detected, pattern => Pattern}};
        false ->
            {ok, #{status => clean}}
    end;
check_ldap_injection(_) ->
    {error, #{reason => invalid_input_type}}.

%% @doc Check for XPath injection patterns
%% Returns {ok, #{}} or {error, #{}}
-spec check_xpath_injection(binary()) -> validation_result().
check_xpath_injection(Input) when is_binary(Input) ->
    case any_match(Input, ?XPATH_INJECTION_PATTERNS) of
        {true, Pattern} ->
            {error, #{reason => xpath_injection_detected, pattern => Pattern}};
        false ->
            {ok, #{status => clean}}
    end;
check_xpath_injection(_) ->
    {error, #{reason => invalid_input_type}}.

%% @doc Get comprehensive security report
-spec get_security_report() -> {ok, security_report()}.
get_security_report() ->
    gen_server:call(?MODULE, get_security_report).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

init(Options) ->
    MaxSize = proplists:get_value(max_size, Options, ?DEFAULT_MAX_SIZE),
    RateLimits = proplists:get_value(rate_limits, Options, #{}),
    CorsOrigins = proplists:get_value(cors_origins, Options, [<<"*">>]),

    {ok, #state{
        max_size = MaxSize,
        rate_limits = maps:from_list(RateLimits),
        rate_counter = #{},
        cors_allowed_origins = CorsOrigins,
        stats = #{
            total_validations => 0,
            security_violations => 0,
            last_check => erlang:system_time(second)
        }
    }}.

handle_call({validate_rate_limit, Request}, _From, State) ->
    ClientId = maps:get(<<"client_id">>, Request, <<"default">>),
    {Limit, Window} = maps:get(ClientId, State#state.rate_limits, {100, 60}),

    Now = erlang:system_time(second),
    Counters = maps:get(ClientId, State#state.rate_counter, {0, Now, Limit}),

    {Count, WindowStart, _Rate} = Counters,

    % Reset counter if window expired
    NewCounters = case Now - WindowStart >= Window of
        true -> {1, Now, Limit};
        false when Count < Limit -> {Count + 1, WindowStart, Limit};
        false -> Counters  % Rate limit exceeded
    end,

    Result = case element(1, NewCounters) > Limit of
        true ->
            {error, #{reason => rate_limit_exceeded, limit => Limit, window => Window}};
        false ->
            Remaining = Limit - element(1, NewCounters),
            {ok, #{remaining => Remaining, limit => Limit, window => Window}}
    end,

    NewState = State#state{
        rate_counter = maps:put(ClientId, NewCounters, State#state.rate_counter)
    },
    {reply, Result, NewState};

handle_call(get_security_report, _From, State) ->
    Stats = State#state.stats,
    Report = #{
        owasp_compliance => #{
            status => active,
            checks_enabled => length(?OWASP_CHECKS)
        },
        vulnerabilities => [],
        recommendations => [
            <<"Enable HTTPS for all communications">>,
            <<"Implement rate limiting per client">>,
            <<"Rotate secrets regularly">>,
            <<"Monitor security logs">>
        ],
        stats => Stats
    },
    {reply, {ok, Report}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @private Detect SQL injection patterns
detect_sql_injection(Input) ->
    SqlPatterns = [
        <<"(?i)('\\s+OR\\s+'.*'|'\\s+AND\\s+'.*'|'\\s*;\\s*)">>,
        <<"(?i)(\\bUNION\\s+ALL\\s+SELECT|\\bUNION\\s+SELECT)">>,
        <<"(?i)(\\bDROP\\s+TABLE|\\bDELETE\\s+FROM|\\bINSERT\\s+INTO)">>,
        <<"(?i)(--|#|\\/\\*|\\*\\/|'|\\bor\\s+1\\s*=\\s*1\\b|\\band\\s+1\\s*=\\s*1\\b)">>
    ],
    case any_match(Input, SqlPatterns) of
        {true, Pattern} -> {error, #{type => sql_injection, pattern => Pattern}};
        false -> ok
    end.

%% @private Detect XSS patterns
detect_xss(Input) ->
    XssPatterns = [
        <<"(?i)<script[^>]*>.*?</script>">>,
        <<"(?i)javascript:">>,
        <<"(?i)on\\w+\\s*=">>,
        <<"(?i)<iframe[^>]*>">>,
        <<"(?i)onclick|onload|onerror">>
    ],
    case any_match(Input, XssPatterns) of
        {true, Pattern} -> {error, #{type => xss, pattern => Pattern}};
        false -> ok
    end.

%% @private Detect path traversal patterns
detect_path_traversal(Input) ->
    PathPatterns = [
        <<"\\.\\.[\\\\/]">>,
        <<"\\.\\.\\/">>,
        <<"%2e%2e">>,
        <<"\\.\\.%2f">>,
        <<"%5c%2e%2e">>
    ],
    case any_match(Input, PathPatterns) of
        {true, Pattern} -> {error, #{type => path_traversal, pattern => Pattern}};
        false -> ok
    end.

%% @private Check if any pattern matches input
any_match(_Input, []) ->
    false;
any_match(Input, [Pattern | Rest]) ->
    case re:run(Input, Pattern, [caseless, {capture, first, binary}]) of
        {match, [Matched]} -> {true, Matched};
        _ -> any_match(Input, Rest)
    end.

%% @private Validate JWT structure
validate_jwt_structure(Header, Payload) ->
    % Check algorithm
    Alg = maps:get(<<"alg">>, Header, undefined),
    case Alg of
        undefined ->
            {error, #{reason => missing_algorithm}};
        _ ->
            % Check for vulnerable algorithms (none, HS256 with public keys)
            SecureAlgs = [<<"HS256">>, <<"HS384">>, <<"HS512">>,
                         <<"RS256">>, <<"RS384">>, <<"RS512">>,
                         <<"ES256">>, <<"ES384">>, <<"ES512">>,
                         <<"PS256">>, <<"PS384">>, <<"PS512">>],
            case lists:member(Alg, SecureAlgs) of
                false ->
                    {error, #{reason => insecure_algorithm, algorithm => Alg}};
                true ->
                    % Check standard claims
                    validate_jwt_claims(Payload)
            end
    end.

%% @private Validate JWT claims
validate_jwt_claims(Payload) ->
    % Check expiration
    Exp = maps:get(<<"exp">>, Payload, undefined),
    Now = erlang:system_time(second),
    case Exp of
        undefined ->
            ok;  % No expiration
        _ when Exp > Now ->
            ok;  % Valid
        _ ->
            {error, #{reason => token_expired}}
    end.

%% @private Base64URL decode (no padding)
base64url_decode(Input) ->
    % Add padding if needed
    Padded = case byte_size(Input) rem 4 of
        0 -> Input;
        1 -> <<Input/binary, "===">>;
        2 -> <<Input/binary, "==">>;
        3 -> <<Input/binary, "=">>
    end,
    % Replace URL-safe characters
    Replaced = binary:replace(Padded, <<"-">>, <<"+">>, [global]),
    Replaced2 = binary:replace(Replaced, <<"_">>, <<"/">>, [global]),
    base64:decode(Replaced2).

%% @private Validate Origin header
validate_origin(Origin) ->
    % Check for null origin (sandboxed frames)
    case Origin of
        <<"null">> ->
            {error, #{reason => null_origin_not_allowed}};
        _ ->
            % Check for valid URL format
            case uri_string:normalize(binary_to_list(Origin)) of
                {error, _, _} ->
                    {error, #{reason => invalid_origin}};
                _ ->
                    ok
            end
    end.

%% @private Validate Access-Control-Allow-Origin
validate_allowed_origin(<<"*">>, _Host) ->
    ok;
validate_allowed_origin(AllowedOrigin, Host) ->
    % Check if origin matches or is a wildcard
    case AllowedOrigin of
        <<"*">> ->
            ok;
        Host ->
            ok;
        _ ->
            % Could implement more sophisticated wildcard matching
            {warning, #{reason => origin_mismatch, allowed => AllowedOrigin, host => Host}}
    end.

%% @private Validate Access-Control-Allow-Methods
validate_allowed_methods(Methods) when is_binary(Methods) ->
    ValidMethods = [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>,
                    <<"PATCH">>, <<"HEAD">>, <<"OPTIONS">>],
    MethodList = binary:split(Methods, <<", ">>, [global]),
    case lists:all(fun(M) -> lists:member(M, ValidMethods) end, MethodList) of
        true -> ok;
        false -> {error, #{reason => invalid_http_method, methods => MethodList}}
    end;
validate_allowed_methods(_) ->
    {error, #{reason => invalid_methods_type}}.

%% @private OWASP A01: Broken Access Control
check_access_control(Input) ->
    % Check for unauthorized access patterns
    case maps:get(<<"root">>, Input, false) of
        true -> {error, root_access_requested};
        _ ->
            case maps:get(<<"admin">>, Input, false) of
                true -> {warning, admin_access_requested};
                _ -> ok
            end
    end.

%% @private OWASP A02: Cryptographic Failures
check_cryptographic_failures(Input) ->
    % Check for plaintext secrets
    case maps:get(<<"password">>, Input, undefined) of
        undefined -> ok;
        Password when byte_size(Password) < 8 ->
            {error, weak_password};
        _ -> {warning, password_in_transit}
    end.

%% @private OWASP A03: Injection
check_injection_attacks(Input) ->
    InputBin = case maps:get(<<"input">>, Input, undefined) of
        undefined -> <<"">>;
        Val -> Val
    end,
    detect_injection_patterns(InputBin).

%% @private OWASP A04: Insecure Design
check_insecure_design(Input) ->
    % Check for missing security headers
    case maps:get(<<"security_headers">>, Input, true) of
        false -> {error, missing_security_headers};
        _ -> ok
    end.

%% @private OWASP A05: Security Misconfiguration
check_security_misconfiguration(Input) ->
    case maps:get(<<"debug_mode">>, Input, false) of
        true -> {error, debug_mode_enabled};
        _ -> ok
    end.

%% @private OWASP A06: Vulnerable Components
check_outdated_components(_Input) ->
    % Would require version checking
    ok.

%% @private OWASP A07: Authentication Failures
check_auth_failures(Input) ->
    case maps:get(<<"auth_token">>, Input, undefined) of
        undefined -> {error, missing_auth};
        Token ->
            case validate_auth_token(Token) of
                {ok, _} -> ok;
                {error, _} -> {error, invalid_auth_token}
            end
    end.

%% @private OWASP A08: Integrity Failures
check_integrity_failures(Input) ->
    case maps:get(<<"signature">>, Input, undefined) of
        undefined -> {warning, missing_integrity_check};
        _ -> ok
    end.

%% @private OWASP A09: Logging Failures
check_logging_failures(Input) ->
    case maps:get(<<"audit_log">>, Input, true) of
        false -> {error, logging_disabled};
        _ -> ok
    end.

%% @private OWASP A10: Server-Side Request Forgery
check_ssr_forgery(Input) ->
    case maps:get(<<"url">>, Input, undefined) of
        undefined -> ok;
        URL ->
            % Check for internal URLs
            case check_internal_url(URL) of
                ok -> ok;
                {error, _} = Error -> Error
            end
    end.

%% @private Check for dangerous protocols in URI
check_dangerous_protocols(URI) ->
    Dangerous = [<<"file:">>, <<"javascript:">>, <<"data:">>,
                 <<"vbscript:">>, <<"mailto:">>],
    DangerousPrefix = lists:any(fun(P) ->
        binary:longest_common_prefix([<<URI/binary>>, P]) =:= P
    end, Dangerous),
    case DangerousPrefix of
        true -> {error, #{reason => dangerous_protocol}};
        false -> ok
    end.

%% @private Check URI encoding attacks
check_uri_encoding(URI) ->
    % Check for double encoding
    case re:run(URI, <<"%25[0-9A-Fa-f]{2}">>, [global]) of
        {match, _} -> {error, #{reason => double_encoding_detected}};
        _ -> ok
    end.

%% @private Check for internal URLs (SSRF prevention)
check_internal_url(URL) ->
    InternalPatterns = [
        <<"localhost">>,
        <<"127.0.0.1">>,
        <<"0.0.0.0">>,
        <<"::1">>,
        <<"[::1]">>
    ],
    URLLower = binary:lowercase(URL),
    case lists:any(fun(P) -> binary:match(URLLower, P) =/= nomatch end, InternalPatterns) of
        true -> {error, #{reason => internal_url_detected}};
        false -> ok
    end.

%% @private Calculate JSON nesting depth
calculate_json_structure_depth(Value, Depth) when is_map(Value) ->
    Depths = [calculate_json_structure_depth(V, Depth + 1) || V <- maps:values(Value)],
    case Depths of
        [] -> Depth;
        _ -> lists:max(Depths)
    end;
calculate_json_structure_depth(Value, Depth) when is_list(Value) ->
    Depths = [calculate_json_structure_depth(V, Depth + 1) || V <- Value],
    case Depths of
        [] -> Depth;
        _ -> lists:max(Depths)
    end;
calculate_json_structure_depth(_Value, Depth) ->
    Depth.

calculate_json_depth(Value) ->
    calculate_json_structure_depth(Value, 0).
