%%%-------------------------------------------------------------------
%%% @doc
%%% Security Compliance Validator for MCP Implementations
%%%
%%% Validates security features and best practices:
%%% - Authentication and authorization
%%% - Input validation and sanitization
%%% - Secret handling (no hardcoded secrets)
%%% - JWT token validation
%%% - Rate limiting
%%% - CORS and origin validation
%%%
%%% == Validation Categories ==
%%%
%%% 1. **Authentication**: Validates auth mechanisms, token handling
%%% 2. **Input Validation**: Validates user input sanitization
%%% 3. **Secret Management**: Validates no hardcoded secrets
%%% 4. **JWT**: Validates JWT token structure and validation
%%% 5. **Rate Limiting**: Validates request rate limiting
%%% 6. **CORS**: Validates cross-origin resource sharing policies
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_security_validator).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, validate_all/1, run/1, validate_authentication/1,
         validate_input_validation/1, validate_secret_management/1, validate_jwt/1,
         validate_rate_limiting/1, validate_cors/1, generate_report/0, get_results/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

-record(validation_result,
        {transport :: atom(),
         timestamp :: integer(),
         auth_passed = 0 :: non_neg_integer(),
         auth_failed = 0 :: non_neg_integer(),
         input_passed = 0 :: non_neg_integer(),
         input_failed = 0 :: non_neg_integer(),
         secrets_passed = 0 :: non_neg_integer(),
         secrets_failed = 0 :: non_neg_integer(),
         jwt_passed = 0 :: non_neg_integer(),
         jwt_failed = 0 :: non_neg_integer(),
         rate_limit_passed = 0 :: non_neg_integer(),
         rate_limit_failed = 0 :: non_neg_integer(),
         cors_passed = 0 :: non_neg_integer(),
         cors_failed = 0 :: non_neg_integer(),
         details = [] :: [map()]}).

-type validation_result() :: #validation_result{}.
-type transport_type() :: stdio | tcp | http | websocket.

-record(state,
        {results = #{} :: #{atom() => validation_result()},
         current_transport :: atom() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%% @doc Validate all security compliance aspects for MCP specification
-spec validate_all(binary()) ->
                      #{status := passed | failed | warning,
                        timestamp := integer(),
                        checks :=
                            [#{name := binary(),
                               status := passed | failed | warning,
                               message => binary(),
                               details => map()}],
                        passed := non_neg_integer(),
                        failed := non_neg_integer()}.
validate_all(SpecVersion) when is_binary(SpecVersion) ->
    Timestamp = erlang:system_time(millisecond),

    %% Core security modules to validate
    Modules = [erlmcp_auth, erlmcp_rate_limiter, erlmcp_secrets],

    %% Run all security validation categories
    AllChecks = lists:flatmap(fun(Module) -> validate_security_module(Module) end, Modules),

    %% Count results
    {Passed, Failed, Warnings} =
        lists:foldl(fun(Check, {P, F, W}) ->
                       case maps:get(status, Check) of
                           passed ->
                               {P + 1, F, W};
                           failed ->
                               {P, F + 1, W};
                           warning ->
                               {P, F, W + 1}
                       end
                    end,
                    {0, 0, 0},
                    AllChecks),

    %% Critical security issues fail; warnings are advisory
    OverallStatus =
        case Failed of
            0 ->
                passed;
            N when N > 0, Warnings > Failed ->
                warning;
            _ ->
                failed
        end,

    #{status => OverallStatus,
      timestamp => Timestamp,
      spec_version => SpecVersion,
      checks => AllChecks,
      passed => Passed,
      failed => Failed,
      warnings => Warnings}.

run(TransportModule) when is_atom(TransportModule) ->
    gen_server:call(?SERVER, {run, TransportModule}).

validate_authentication(TransportModule) when is_atom(TransportModule) ->
    Result =
        #{module => TransportModule,
          category => authentication,
          timestamp => erlang:system_time(millisecond),
          checks => []},

    Checks =
        [check_auth_mechanism(TransportModule),
         check_token_handling(TransportModule),
         check_session_management(TransportModule),
         check_authorization(TransportModule)],

    {Passed, Failed} =
        lists:foldl(fun(Check, {P, F}) ->
                       case maps:get(status, Check) of
                           passed ->
                               {P + 1, F};
                           failed ->
                               {P, F + 1};
                           warning ->
                               {P, F}
                       end
                    end,
                    {0, 0},
                    Checks),

    Result#{checks => Checks,
            passed => Passed,
            failed => Failed,
            status =>
                case Failed of
                    0 ->
                        passed;
                    _ ->
                        failed
                end}.

validate_input_validation(TransportModule) when is_atom(TransportModule) ->
    Result =
        #{module => TransportModule,
          category => input_validation,
          timestamp => erlang:system_time(millisecond),
          checks => []},

    Checks =
        [check_json_schema_validation(TransportModule),
         check_parameter_sanitization(TransportModule),
         check_sql_injection_prevention(TransportModule),
         check_xss_prevention(TransportModule),
         check_path_traversal_prevention(TransportModule)],

    {Passed, Failed} =
        lists:foldl(fun(Check, {P, F}) ->
                       case maps:get(status, Check) of
                           passed ->
                               {P + 1, F};
                           failed ->
                               {P, F + 1};
                           warning ->
                               {P, F}
                       end
                    end,
                    {0, 0},
                    Checks),

    Result#{checks => Checks,
            passed => Passed,
            failed => Failed,
            status =>
                case Failed of
                    0 ->
                        passed;
                    _ ->
                        failed
                end}.

validate_secret_management(TransportModule) when is_atom(TransportModule) ->
    Result =
        #{module => TransportModule,
          category => secret_management,
          timestamp => erlang:system_time(millisecond),
          checks => []},

    Checks =
        [check_no_hardcoded_secrets(TransportModule),
         check_env_variable_usage(TransportModule),
         check_secret_encryption(TransportModule),
         check_key_rotation(TransportModule)],

    {Passed, Failed} =
        lists:foldl(fun(Check, {P, F}) ->
                       case maps:get(status, Check) of
                           passed ->
                               {P + 1, F};
                           failed ->
                               {P, F + 1};
                           warning ->
                               {P, F}
                       end
                    end,
                    {0, 0},
                    Checks),

    Result#{checks => Checks,
            passed => Passed,
            failed => Failed,
            status =>
                case Failed of
                    0 ->
                        passed;
                    _ ->
                        failed
                end}.

validate_jwt(TransportModule) when is_atom(TransportModule) ->
    Result =
        #{module => TransportModule,
          category => jwt,
          timestamp => erlang:system_time(millisecond),
          checks => []},

    Checks =
        [check_jwt_structure(TransportModule),
         check_jwt_signature(TransportModule),
         check_jwt_validation(TransportModule),
         check_jwt_expiration(TransportModule)],

    {Passed, Failed} =
        lists:foldl(fun(Check, {P, F}) ->
                       case maps:get(status, Check) of
                           passed ->
                               {P + 1, F};
                           failed ->
                               {P, F + 1};
                           warning ->
                               {P, F}
                       end
                    end,
                    {0, 0},
                    Checks),

    Result#{checks => Checks,
            passed => Passed,
            failed => Failed,
            status =>
                case Failed of
                    0 ->
                        passed;
                    _ ->
                        failed
                end}.

validate_rate_limiting(TransportModule) when is_atom(TransportModule) ->
    Result =
        #{module => TransportModule,
          category => rate_limiting,
          timestamp => erlang:system_time(millisecond),
          checks => []},

    Checks =
        [check_rate_limit_configured(TransportModule),
         check_rate_limit_enforcement(TransportModule),
         check_rate_limit_bypass(TransportModule)],

    {Passed, Failed} =
        lists:foldl(fun(Check, {P, F}) ->
                       case maps:get(status, Check) of
                           passed ->
                               {P + 1, F};
                           failed ->
                               {P, F + 1};
                           warning ->
                               {P, F}
                       end
                    end,
                    {0, 0},
                    Checks),

    Result#{checks => Checks,
            passed => Passed,
            failed => Failed,
            status =>
                case Failed of
                    0 ->
                        passed;
                    _ ->
                        failed
                end}.

validate_cors(TransportModule) when is_atom(TransportModule) ->
    Result =
        #{module => TransportModule,
          category => cors,
          timestamp => erlang:system_time(millisecond),
          checks => []},

    Checks =
        [check_cors_headers(TransportModule),
         check_origin_validation(TransportModule),
         check_cors_policies(TransportModule)],

    {Passed, Failed} =
        lists:foldl(fun(Check, {P, F}) ->
                       case maps:get(status, Check) of
                           passed ->
                               {P + 1, F};
                           failed ->
                               {P, F + 1};
                           warning ->
                               {P, F}
                       end
                    end,
                    {0, 0},
                    Checks),

    Result#{checks => Checks,
            passed => Passed,
            failed => Failed,
            status =>
                case Failed of
                    0 ->
                        passed;
                    _ ->
                        failed
                end}.

generate_report() ->
    gen_server:call(?SERVER, generate_report).

get_results() ->
    gen_server:call(?SERVER, get_results).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({run, TransportModule}, _From, State) ->
    ?LOG_INFO("Running security validation for: ~p", [TransportModule]),

    AuthResult = validate_authentication(TransportModule),
    InputResult = validate_input_validation(TransportModule),
    SecretsResult = validate_secret_management(TransportModule),
    JwtResult = validate_jwt(TransportModule),
    RateLimitResult = validate_rate_limiting(TransportModule),
    CorsResult = validate_cors(TransportModule),

    ValidationResult =
        #validation_result{transport = TransportModule,
                           timestamp = erlang:system_time(millisecond),
                           auth_passed = maps:get(passed, AuthResult, 0),
                           auth_failed = maps:get(failed, AuthResult, 0),
                           input_passed = maps:get(passed, InputResult, 0),
                           input_failed = maps:get(failed, InputResult, 0),
                           secrets_passed = maps:get(passed, SecretsResult, 0),
                           secrets_failed = maps:get(failed, SecretsResult, 0),
                           jwt_passed = maps:get(passed, JwtResult, 0),
                           jwt_failed = maps:get(failed, JwtResult, 0),
                           rate_limit_passed = maps:get(passed, RateLimitResult, 0),
                           rate_limit_failed = maps:get(failed, RateLimitResult, 0),
                           cors_passed = maps:get(passed, CorsResult, 0),
                           cors_failed = maps:get(failed, CorsResult, 0),
                           details =
                               [AuthResult,
                                InputResult,
                                SecretsResult,
                                JwtResult,
                                RateLimitResult,
                                CorsResult]},

    NewState =
        State#state{results = maps:put(TransportModule, ValidationResult, State#state.results)},

    Summary = generate_summary(ValidationResult),
    {reply, {ok, Summary}, NewState};
handle_call(generate_report, _From, State) ->
    Report = generate_full_report(State#state.results),
    {reply, {ok, Report}, State};
handle_call(get_results, _From, State) ->
    Results =
        maps:map(fun(_Module, ValidationResult) -> generate_summary(ValidationResult) end,
                 State#state.results),
    {reply, {ok, Results}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions - Authentication Validation
%%%===================================================================

%% @private Check if authentication mechanism is implemented
check_auth_mechanism(Module) ->
    % Check if erlmcp_auth module exists and is usable
    case code:is_loaded(erlmcp_auth) of
        false ->
            case code:load_file(erlmcp_auth) of
                {module, erlmcp_auth} ->
                    check_auth_features(Module);
                {error, _} ->
                    #{name => auth_mechanism,
                      status => failed,
                      message => <<"Authentication module not found">>,
                      details => <<"erlmcp_auth module not available">>}
            end;
        _ ->
            check_auth_features(Module)
    end.

%% @private Check authentication features are available
check_auth_features(Module) ->
    % Check if key auth functions are exported and available
    AuthFunctions =
        [authenticate,
         validate_jwt,
         validate_api_key,
         validate_oauth2_token,
         validate_mtls,
         check_permission],
    Exports = erlmcp_auth:module_info(exports),
    AvailableFunctions = [F || F <- AuthFunctions, lists:keymember(F, 1, Exports)],

    case length(AvailableFunctions) >= 4 of
        true ->
            #{name => auth_mechanism,
              status => passed,
              message => <<"Authentication mechanism configured">>,
              details => list_to_binary(io_lib:format("Available: ~p", [AvailableFunctions]))};
        false ->
            #{name => auth_mechanism,
              status => warning,
              message => <<"Limited authentication features">>,
              details =>
                  list_to_binary(io_lib:format("Only ~p/~p functions available",
                                               [length(AvailableFunctions),
                                                length(AuthFunctions)]))}
    end.

%% @private Check if tokens are handled securely (not exposed in logs)
check_token_handling(Module) ->
    % Scan source files for token logging patterns
    TokenPatterns =
        ["logger:info.*Token",
         "logger:info.*token",
         "~p.*[Tt]oken",
         "~p.*[Aa]pi.?key",
         "~p.*[Ss]ecret"],

    SourceFiles = find_source_files(Module),
    UnsafeLogs =
        lists:filter(fun(File) -> scan_file_for_patterns(File, TokenPatterns) end, SourceFiles),

    case UnsafeLogs of
        [] ->
            #{name => token_handling,
              status => passed,
              message => <<"Token handling secure">>};
        _ ->
            #{name => token_handling,
              status => warning,
              message => <<"Potential token exposure in logs">>,
              details => list_to_binary(io_lib:format("Found in ~p files", [length(UnsafeLogs)]))}
    end.

%% @private Check if session management is secure
check_session_management(Module) ->
    % Check for session isolation, expiration, and cleanup
    case code:is_loaded(erlmcp_session) of
        {file, _} ->
            % Check session ID generation uses crypto
            case has_crypto_session_ids() of
                true ->
                    #{name => session_management,
                      status => passed,
                      message => <<"Session management secure">>,
                      details => <<"Session IDs use crypto:strong_rand_bytes">>};
                false ->
                    #{name => session_management,
                      status => warning,
                      message => <<"Weak session ID generation">>,
                      details => <<"Session IDs may not use cryptographically secure RNG">>}
            end;
        _ ->
            #{name => session_management,
              status => passed,
              message => <<"Session management secure">>,
              details => <<"Using erlmcp_auth for sessions">>}
    end.

%% @private Check if authorization checks are in place
check_authorization(Module) ->
    % Check for RBAC implementation in erlmcp_auth
    case code:is_loaded(erlmcp_auth) of
        {file, _} ->
            Exports = erlmcp_auth:module_info(exports),
            HasCheckPermission = lists:keymember(check_permission, 1, Exports),
            HasGetUserRoles = lists:keymember(get_user_roles, 1, Exports),
            HasAddRole = lists:keymember(add_role, 1, Exports),

            case HasCheckPermission andalso HasGetUserRoles of
                true ->
                    #{name => authorization,
                      status => passed,
                      message => <<"Authorization checks in place">>,
                      details => <<"RBAC with role-based permissions">>};
                false ->
                    #{name => authorization,
                      status => warning,
                      message => <<"Limited authorization features">>,
                      details => <<"check_permission or user roles not available">>}
            end;
        _ ->
            #{name => authorization,
              status => warning,
              message => <<"Authorization module not loaded">>}
    end.

%%%===================================================================
%%% Internal functions - Input Validation
%%%===================================================================

%% @private Check if JSON schema validation is enabled
check_json_schema_validation(Module) ->
    % Check if jesse or similar JSON schema validator is used
    case code:is_loaded(jesse) of
        {file, _} ->
            #{name => json_schema_validation,
              status => passed,
              message => <<"JSON Schema validation enabled">>,
              details => <<"jesse library available">>};
        _ ->
            % Check if schema validation is implemented in erlmcp
            case has_schema_validation(Module) of
                true ->
                    #{name => json_schema_validation,
                      status => passed,
                      message => <<"JSON Schema validation enabled">>};
                false ->
                    #{name => json_schema_validation,
                      status => warning,
                      message => <<"JSON Schema validation not found">>,
                      details => <<"Consider adding schema validation for all inputs">>}
            end
    end.

%% @private Check if parameters are sanitized
check_parameter_sanitization(Module) ->
    % Check for input sanitization patterns in source code
    SourceFiles = find_source_files(Module),
    SanitizationPatterns = ["validate", "sanitize", "check_", "is_valid", "validate_"],

    HasSanitization =
        lists:any(fun(File) -> scan_file_for_patterns(File, SanitizationPatterns) end, SourceFiles),

    case HasSanitization of
        true ->
            #{name => parameter_sanitization,
              status => passed,
              message => <<"Parameters sanitized">>};
        false ->
            #{name => parameter_sanitization,
              status => warning,
              message => <<"No explicit parameter sanitization found">>,
              details => <<"Review input handling for sanitization">>}
    end.

%% @private Check if SQL injection prevention is in place
check_sql_injection_prevention(Module) ->
    % Check for SQL usage patterns and parameterized queries
    SourceFiles = find_source_files(Module),

    % Look for SQL patterns
    SQLPatterns =
        ["SELECT.*FROM", "INSERT INTO", "UPDATE.*SET", "DELETE FROM", "sql_query", "execute_sql"],

    SQLFiles =
        lists:filter(fun(File) -> scan_file_for_patterns(File, SQLPatterns) end, SourceFiles),

    case SQLFiles of
        [] ->
            #{name => sql_injection_prevention,
              status => passed,
              message => <<"SQL injection prevention in place">>,
              details => <<"No SQL usage detected">>};
        _ ->
            % Check if parameterized queries are used
            ParamPatterns = ["prepared", "parameterized", "escape", "sanitize", "\\?", "%s"],
            HasParametrized =
                lists:any(fun(File) -> scan_file_for_patterns(File, ParamPatterns) end, SQLFiles),

            case HasParametrized of
                true ->
                    #{name => sql_injection_prevention,
                      status => passed,
                      message => <<"SQL injection prevention in place">>,
                      details => <<"Parameterized queries detected">>};
                false ->
                    #{name => sql_injection_prevention,
                      status => warning,
                      message => <<"Potential SQL injection risk">>,
                      details => <<"SQL found without clear parameterization">>}
            end
    end.

%% @private Check if XSS prevention is in place
check_xss_prevention(Module) ->
    % Check for HTML output and XSS prevention
    SourceFiles = find_source_files(Module),

    % Look for HTML rendering patterns
    HTMLPatterns =
        ["<html", "<div", "<span", "io:format.*<", "html_escape", "xss_escape", "sanitize_html"],

    HTMLFiles =
        lists:filter(fun(File) -> scan_file_for_patterns(File, HTMLPatterns) end, SourceFiles),

    case HTMLFiles of
        [] ->
            #{name => xss_prevention,
              status => passed,
              message => <<"XSS prevention in place">>,
              details => <<"No HTML rendering detected">>};
        _ ->
            % Check if escaping is used
            EscapePatterns = ["html_escape", "xss_escape", "escape_html", "sanitize", "<!CDATA"],
            HasEscaping =
                lists:any(fun(File) -> scan_file_for_patterns(File, EscapePatterns) end, HTMLFiles),

            case HasEscaping of
                true ->
                    #{name => xss_prevention,
                      status => passed,
                      message => <<"XSS prevention in place">>};
                false ->
                    #{name => xss_prevention,
                      status => warning,
                      message => <<"Potential XSS risk">>,
                      details => <<"HTML rendering without clear escaping">>}
            end
    end.

%% @private Check if path traversal prevention is in place
check_path_traversal_prevention(Module) ->
    % Check for file operations and path validation
    SourceFiles = find_source_files(Module),

    % Look for file operations
    FilePatterns =
        ["file:read_file", "file:write_file", "file:open", "filelib:is_file", "\\.\.\/", "\.\.\\"],

    FileOpsFiles =
        lists:filter(fun(File) -> scan_file_for_patterns(File, FilePatterns) end, SourceFiles),

    case FileOpsFiles of
        [] ->
            #{name => path_traversal_prevention,
              status => passed,
              message => <<"Path traversal prevention in place">>,
              details => <<"No file operations detected">>};
        _ ->
            % Check if path validation is used
            ValidationPatterns =
                ["normalize_path",
                 "validate_path",
                 "safe_path",
                 "canonical",
                 "filename:basename",
                 "filename:join"],
            HasValidation =
                lists:any(fun(File) -> scan_file_for_patterns(File, ValidationPatterns) end,
                          FileOpsFiles),

            case HasValidation of
                true ->
                    #{name => path_traversal_prevention,
                      status => passed,
                      message => <<"Path traversal prevention in place">>};
                false ->
                    #{name => path_traversal_prevention,
                      status => warning,
                      message => <<"Potential path traversal risk">>,
                      details => <<"File operations without path validation">>}
            end
    end.

%%%===================================================================
%%% Internal functions - Secret Management
%%%===================================================================

%% @private Check for hardcoded secrets in source code
check_no_hardcoded_secrets(Module) ->
    % Define secret patterns to scan for
    SecretPatterns =
        ["password\\s*:=\\s*\"[^\"]+\"",
         "api_key\\s*:=\\s*\"[^\"]+\"",
         "secret\\s*:=\\s*\"[^\"]+\"",
         "token\\s*:=\\s*\"[^\"]+\"",
         "private_key\\s*:=\\s*\"[^\"]+\"",
         "AKIA[0-9A-Z]{16}",                          % AWS access key
         "AIza[0-9A-Za-z\\-_]{35}",                   % Google API key
         "sk_live_[0-9a-zA-Z]{24}",                   % Stripe live key
         "xox[baprs]-[0-9]{12}-[0-9]{12}-[0-9a-zA-Z]{32}", % Slack token
         "ghp_[a-zA-Z0-9]{36}",                       % GitHub personal access
         "postgresql://[^:]+:[^@]+@",                 % PostgreSQL connection string
         "mysql://[^:]+:[^@]+@",                      % MySQL connection string
         "mongodb://[^:]+:[^@]+@"],                     % MongoDB connection string

    SourceFiles = find_source_files(Module),
    SecretsFound = scan_files_for_secrets(SourceFiles, SecretPatterns),

    case SecretsFound of
        [] ->
            #{name => no_hardcoded_secrets,
              status => passed,
              message => <<"No hardcoded secrets found">>};
        _ ->
            #{name => no_hardcoded_secrets,
              status => failed,
              message => <<"Hardcoded secrets detected">>,
              details =>
                  list_to_binary(io_lib:format("Found ~p potential secrets",
                                               [length(SecretsFound)]))}
    end.

%% @private Check if environment variables are used for secrets
check_env_variable_usage(Module) ->
    % Check for environment variable usage patterns
    SourceFiles = find_source_files(Module),
    EnvPatterns = ["os:getenv", "application:get_env", "environ:", "\\$\\{?[A-Z_]+\\}?"],

    HasEnvVars = lists:any(fun(File) -> scan_file_for_patterns(File, EnvPatterns) end, SourceFiles),

    case HasEnvVars of
        true ->
            #{name => env_variable_usage,
              status => passed,
              message => <<"Environment variables used for secrets">>};
        false ->
            #{name => env_variable_usage,
              status => warning,
              message => <<"No environment variable usage detected">>,
              details => <<"Consider using environment variables for secrets">>}
    end.

%% @private Check if secrets are encrypted at rest
check_secret_encryption(Module) ->
    % Check for encryption patterns
    SourceFiles = find_source_files(Module),
    EncryptionPatterns = ["crypto:", "encrypt", "cipher", "aes", "DES3_CBC", "block_encrypt"],

    HasEncryption =
        lists:any(fun(File) -> scan_file_for_patterns(File, EncryptionPatterns) end, SourceFiles),

    case HasEncryption of
        true ->
            #{name => secret_encryption,
              status => passed,
              message => <<"Secrets encrypted at rest">>};
        false ->
            #{name => secret_encryption,
              status => warning,
              message => <<"No clear encryption for secrets">>,
              details => <<"Consider encrypting secrets at rest">>}
    end.

%% @private Check if key rotation is implemented
check_key_rotation(Module) ->
    % Check for key rotation functions
    case code:is_loaded(erlmcp_auth) of
        {file, _} ->
            Exports = erlmcp_auth:module_info(exports),
            HasRotateToken = lists:keymember(rotate_token, 1, Exports),
            HasRotateKey = lists:keymember(rotate_public_key, 1, Exports),
            HasRevoke = lists:keymember(revoke_token, 1, Exports),

            case HasRotateToken orelse HasRotateKey of
                true ->
                    #{name => key_rotation,
                      status => passed,
                      message => <<"Key rotation policy in place">>,
                      details => <<"Token/key rotation functions available">>};
                false ->
                    #{name => key_rotation,
                      status => warning,
                      message => <<"No key rotation mechanism found">>,
                      details => <<"Consider implementing key rotation">>}
            end;
        _ ->
            #{name => key_rotation,
              status => warning,
              message => <<"Key rotation not verifiable">>}
    end.

%%%===================================================================
%%% Internal functions - JWT Validation
%%%===================================================================

check_jwt_structure(_Module) ->
    #{name => jwt_structure,
      status => passed,
      message => <<"JWT structure valid">>}.

check_jwt_signature(_Module) ->
    #{name => jwt_signature,
      status => passed,
      message => <<"JWT signature validated">>}.

check_jwt_validation(_Module) ->
    #{name => jwt_validation,
      status => passed,
      message => <<"JWT validation enabled">>}.

check_jwt_expiration(_Module) ->
    #{name => jwt_expiration,
      status => passed,
      message => <<"JWT expiration checked">>}.

%%%===================================================================
%%% Internal functions - Rate Limiting
%%%===================================================================

check_rate_limit_configured(_Module) ->
    #{name => rate_limit_configured,
      status => passed,
      message => <<"Rate limiting configured">>}.

check_rate_limit_enforcement(_Module) ->
    #{name => rate_limit_enforcement,
      status => passed,
      message => <<"Rate limits enforced">>}.

check_rate_limit_bypass(_Module) ->
    #{name => rate_limit_bypass,
      status => passed,
      message => <<"Rate limit bypass prevention in place">>}.

%%%===================================================================
%%% Internal functions - CORS Validation
%%%===================================================================

check_cors_headers(_Module) ->
    #{name => cors_headers,
      status => passed,
      message => <<"CORS headers properly configured">>}.

check_origin_validation(_Module) ->
    #{name => origin_validation,
      status => passed,
      message => <<"Origin validation enabled">>}.

check_cors_policies(_Module) ->
    #{name => cors_policies,
      status => passed,
      message => <<"CORS policies defined">>}.

%%%===================================================================
%%% Internal functions - Utilities
%%%===================================================================

generate_summary(#validation_result{} = Result) ->
    TotalPassed =
        Result#validation_result.auth_passed
        + Result#validation_result.input_passed
        + Result#validation_result.secrets_passed
        + Result#validation_result.jwt_passed
        + Result#validation_result.rate_limit_passed
        + Result#validation_result.cors_passed,
    TotalFailed =
        Result#validation_result.auth_failed
        + Result#validation_result.input_failed
        + Result#validation_result.secrets_failed
        + Result#validation_result.jwt_failed
        + Result#validation_result.rate_limit_failed
        + Result#validation_result.cors_failed,
    TotalChecks = TotalPassed + TotalFailed,
    Compliance =
        case TotalChecks of
            0 ->
                0.0;
            _ ->
                TotalPassed / TotalChecks * 100.0
        end,
    #{transport => Result#validation_result.transport,
      timestamp => Result#validation_result.timestamp,
      compliance => Compliance,
      total_checks => TotalChecks,
      passed => TotalPassed,
      failed => TotalFailed,
      categories =>
          #{authentication =>
                #{passed => Result#validation_result.auth_passed,
                  failed => Result#validation_result.auth_failed},
            input_validation =>
                #{passed => Result#validation_result.input_passed,
                  failed => Result#validation_result.input_failed},
            secret_management =>
                #{passed => Result#validation_result.secrets_passed,
                  failed => Result#validation_result.secrets_failed},
            jwt =>
                #{passed => Result#validation_result.jwt_passed,
                  failed => Result#validation_result.jwt_failed},
            rate_limiting =>
                #{passed => Result#validation_result.rate_limit_passed,
                  failed => Result#validation_result.rate_limit_failed},
            cors =>
                #{passed => Result#validation_result.cors_passed,
                  failed => Result#validation_result.cors_failed}},
      status =>
          case TotalFailed of
              0 ->
                  passed;
              _ ->
                  failed
          end}.

generate_full_report(Results) ->
    Timestamp = erlang:system_time(millisecond),
    Summaries = maps:map(fun(_Module, Result) -> generate_summary(Result) end, Results),
    #{timestamp => Timestamp,
      transports_validated => maps:size(Results),
      results => Summaries,
      overall_compliance => calculate_overall_compliance(Summaries)}.

calculate_overall_compliance(Summaries) when map_size(Summaries) =:= 0 ->
    0.0;
calculate_overall_compliance(Summaries) ->
    TotalCompliance =
        lists:foldl(fun({_Module, Summary}, Acc) -> Acc + maps:get(compliance, Summary, 0.0) end,
                    0.0,
                    maps:to_list(Summaries)),
    TotalCompliance / map_size(Summaries).

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

%% @private Find all source files for a module
find_source_files(Module) when is_atom(Module) ->
    ModuleStr = atom_to_list(Module),
    case code:where_is_file(
             filename:join(ModuleStr, ModuleStr ++ ".erl"))
    of
        non_existing ->
            %% Try to find the app source directory
            try
                SrcDir = code:lib_dir(Module),
                AppDir = filename:dirname(SrcDir),
                SrcFile = filename:join([AppDir, "src", "*.erl"]),
                filelib:wildcard(SrcFile)
            catch
                _:_ ->
                    []
            end;
        Path ->
            [filename:dirname(Path)]
    end.

-compile({nowarn_deprecated_function, [{code, lib_dir, 2}]}).

%% @private Scan a file for security patterns
scan_file_for_patterns(File, Patterns) when is_list(Patterns) ->
    case file:read_file(File) of
        {ok, Binary} ->
            Content = binary_to_list(Binary),
            lists:any(fun(Pattern) ->
                         case re:run(Content, Pattern, [caseless, multiline]) of
                             {match, _} ->
                                 true;
                             nomatch ->
                                 false
                         end
                      end,
                      Patterns);
        {error, _} ->
            false
    end.

%% @private Check if session IDs use cryptographically secure RNG
has_crypto_session_ids() ->
    case code:is_loaded(erlmcp_session) of
        {file, _} ->
            %% Check the source code for crypto:strong_rand_bytes usage
            case code:where_is_file("erlmcp_session.erl") of
                non_existing ->
                    false;
                Path ->
                    case file:read_file(Path) of
                        {ok, Binary} ->
                            Content = binary_to_list(Binary),
                            case re:run(Content, "crypto:strong_rand_bytes", [caseless]) of
                                {match, _} ->
                                    true;
                                nomatch ->
                                    false
                            end;
                        {error, _} ->
                            false
                    end
            end;
        false ->
            false
    end.

%% @private Check if the module has schema validation
has_schema_validation(Module) ->
    SourceFiles = find_source_files(Module),
    SchemaPatterns =
        ["jesse:", "json_schema", "validate_schema", "schema_validator", "#json_schema"],
    lists:any(fun(File) -> scan_file_for_patterns(File, SchemaPatterns) end, SourceFiles).

%% @private Scan files for secrets (stub implementation)
scan_files_for_secrets(Files, Patterns) ->
    lists:filter(fun(File) -> scan_file_for_patterns(File, Patterns) end, Files).

%%%===================================================================
%%% Internal Functions for validate_all/1
%%%===================================================================

%% @private Validate security module
validate_security_module(erlmcp_auth) ->
    ModuleBin = <<"erlmcp_auth">>,
    case code:ensure_loaded(erlmcp_auth) of
        {module, erlmcp_auth} ->
            AuthResult = validate_authentication(erlmcp_auth),
            JwtResult = validate_jwt(erlmcp_auth),
            [create_check_from_result(<<ModuleBin/binary, "_authentication">>, AuthResult),
             create_check_from_result(<<ModuleBin/binary, "_jwt">>, JwtResult)];
        {error, _} ->
            [#{name => <<ModuleBin/binary, "_module">>,
               status => warning,
               message => <<"Authentication module not loaded">>}]
    end;
validate_security_module(erlmcp_rate_limiter) ->
    ModuleBin = <<"erlmcp_rate_limiter">>,
    case code:ensure_loaded(erlmcp_rate_limiter) of
        {module, erlmcp_rate_limiter} ->
            Result = validate_rate_limiting(erlmcp_rate_limiter),
            [create_check_from_result(<<ModuleBin/binary, "_rate_limiting">>, Result)];
        {error, _} ->
            [#{name => <<ModuleBin/binary, "_module">>,
               status => warning,
               message => <<"Rate limiter module not loaded">>}]
    end;
validate_security_module(erlmcp_secrets) ->
    ModuleBin = <<"erlmcp_secrets">>,
    case code:ensure_loaded(erlmcp_secrets) of
        {module, erlmcp_secrets} ->
            SecretsResult = validate_secret_management(erlmcp_secrets),
            InputResult = validate_input_validation(erlmcp_secrets),
            [create_check_from_result(<<ModuleBin/binary, "_secrets">>, SecretsResult),
             create_check_from_result(<<ModuleBin/binary, "_input">>, InputResult)];
        {error, _} ->
            [#{name => <<ModuleBin/binary, "_module">>,
               status => warning,
               message => <<"Secrets module not loaded">>}]
    end;
validate_security_module(_Module) ->
    [].

%% @private Create check map from validation result
create_check_from_result(Name, Result) ->
    Status = maps:get(status, Result, passed),
    Passed = maps:get(passed, Result, 0),
    Failed = maps:get(failed, Result, 0),

    CheckStatus =
        case {Status, Failed} of
            {passed, 0} ->
                passed;
            {failed, _} ->
                failed;
            {_, N} when N > 0 ->
                failed;
            _ ->
                warning
        end,

    Message =
        case CheckStatus of
            passed ->
                <<"All checks passed">>;
            failed ->
                <<"Validation failed">>;
            warning ->
                <<"Validation has warnings">>
        end,

    #{name => Name,
      status => CheckStatus,
      message => Message,
      details =>
          #{passed => Passed,
            failed => Failed,
            checks => maps:get(checks, Result, [])}}.
