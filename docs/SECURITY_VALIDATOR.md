# Security Validator - Security Compliance Validation

## Purpose and Scope

The `erlmcp_security_validator` module validates security features and best practices for MCP implementations. It ensures:

- Authentication and authorization mechanisms are in place
- Input validation and sanitization prevents injection attacks
- Secrets are managed securely (no hardcoded values)
- JWT tokens are validated properly
- Rate limiting prevents abuse
- CORS policies are configured correctly

## Architecture

```
erlmcp_security_validator (gen_server)
├── Authentication Validation    - Auth mechanisms, tokens, sessions
├── Input Validation            - Sanitization, injection prevention
├── Secret Management           - No hardcoded secrets, env vars
├── JWT Validation              - Structure, signature, expiration
├── Rate Limiting               - Configuration, enforcement
└── CORS Validation             - Headers, origin, policies
```

## API Reference

### Server Management

#### start_link/0

Start the security validator gen_server.

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
```

**Example:**
```erlang
{ok, Pid} = erlmcp_security_validator:start_link().
```

#### run/1

Run full security validation for a transport module.

```erlang
-spec run(atom()) -> {ok, map()}.
```

**Parameters:**
- `TransportModule` - Atom name of transport module

**Returns:**
- `{ok, Summary}` - Map with validation results

**Example:**
```erlang
{ok, Summary} = erlmcp_security_validator:run(erlmcp_transport_stdio).
% Returns: #{transport => ..., compliance => 95.0, ...}
```

### Authentication Validation

#### validate_authentication/1

Validate authentication mechanisms.

```erlang
-spec validate_authentication(atom()) -> map().
```

**Checks Performed:**

| Check | Description |
|-------|-------------|
| `auth_mechanism` | `erlmcp_auth` module available |
| `token_handling` | Tokens not exposed in logs |
| `session_management` | Session IDs use crypto RNG |
| `authorization` | RBAC functions available |

**Example:**
```erlang
Result = erlmcp_security_validator:validate_authentication(erlmcp_transport_stdio).
% Returns: #{
%   module => erlmcp_transport_stdio,
%   category => authentication,
%   checks => [...],
%   passed => 3,
%   failed => 1,
%   status => failed
% }
```

### Input Validation

#### validate_input_validation/1

Validate input sanitization and injection prevention.

```erlang
-spec validate_input_validation(atom()) -> map().
```

**Checks Performed:**

| Check | Description |
|-------|-------------|
| `json_schema_validation` | JSON schema validation enabled |
| `parameter_sanitization` | Input sanitization patterns found |
| `sql_injection_prevention` | SQL usage with parameterized queries |
| `xss_prevention` | HTML output escaped |
| `path_traversal_prevention` | File operations use path validation |

**Example:**
```erlang
Result = erlmcp_security_validator:validate_input_validation(erlmcp_transport_http).
% Returns: #{checks => [...], status => passed}
```

### Secret Management

#### validate_secret_management/1

Validate secret handling practices.

```erlang
-spec validate_secret_management(atom()) -> map().
```

**Checks Performed:**

| Check | Description |
|-------|-------------|
| `no_hardcoded_secrets` | No secrets in source code |
| `env_variable_usage` | Environment variables used |
| `secret_encryption` | Secrets encrypted at rest |
| `key_rotation` | Key rotation available |

**Secret Patterns Detected:**
```
password := "..."         % Direct password
api_key := "..."          % API key
secret := "..."           % Secret value
AKIA[0-9A-Z]{16}         % AWS access key
sk_live_[0-9a-zA-Z]{24}  % Stripe key
postgresql://...         % Connection strings
```

**Example:**
```erlang
Result = erlmcp_security_validator:validate_secret_management(erlmcp_transport_stdio).
% If secrets found:
% #{status => failed, checks => [
%   #{name => no_hardcoded_secrets, status => failed,
%     message => <<"Hardcoded secrets detected">>}
% ]}
```

### JWT Validation

#### validate_jwt/1

Validate JWT handling.

```erlang
-spec validate_jwt(atom()) -> map().
```

**Checks Performed:**

| Check | Description |
|-------|-------------|
| `jwt_structure` | JWT has header.payload.signature |
| `jwt_signature` | Signatures are validated |
| `jwt_validation` | Validation functions present |
| `jwt_expiration` | Expiration checked |

**Example:**
```erlang
Result = erlmcp_security_validator:validate_jwt(erlmcp_transport_http).
% Returns: #{checks => [...], status => passed}
```

### Rate Limiting

#### validate_rate_limiting/1

Validate rate limiting implementation.

```erlang
-spec validate_rate_limiting(atom()) -> map().
```

**Checks Performed:**

| Check | Description |
|-------|-------------|
| `rate_limit_configured` | Rate limiter configured |
| `rate_limit_enforcement` | Limits are enforced |
| `rate_limit_bypass` | No bypass possible |

**Example:**
```erlang
Result = erlmcp_security_validator:validate_rate_limiting(erlmcp_transport_http).
% Returns: #{checks => [...], status => passed}
```

### CORS Validation

#### validate_cors/1

Validate CORS configuration.

```erlang
-spec validate_cors(atom()) -> map().
```

**Checks Performed:**

| Check | Description |
|-------|-------------|
| `cors_headers` | CORS headers set correctly |
| `origin_validation` | Origin validation enabled |
| `cors_policies` | Policies defined |

**Example:**
```erlang
Result = erlmcp_security_validator:validate_cors(erlmcp_transport_http).
% Returns: #{checks => [...], status => passed}
```

### Report Generation

#### generate_report/0

Generate full security validation report.

```erlang
-spec generate_report() -> {ok, map()}.
```

**Example:**
```erlang
{ok, Report} = erlmcp_security_validator:generate_report().
% Returns: #{
%   timestamp => ...,
%   transports_validated => N,
%   results => #{...},
%   overall_compliance => 92.5
% }
```

#### get_results/0

Get current validation results.

```erlang
-spec get_results() -> {ok, map()}.
```

## Usage Examples

### Running Full Security Validation

```erlang
%% Start the validator
{ok, _Pid} = erlmcp_security_validator:start_link(),

%% Run validation for each transport
Transports = [
    erlmcp_transport_stdio,
    erlmcp_transport_tcp,
    erlmcp_transport_http,
    erlmcp_transport_ws
],

Results = lists:map(fun(Transport) ->
    {ok, Summary} = erlmcp_security_validator:run(Transport),
    {Transport, Summary}
end, Transports),

%% Generate report
{ok, Report} = erlmcp_security_validator:generate_report().

%% Print overall compliance
#{overall_compliance := Compliance} = Report,
io:format("Overall Security Compliance: ~.1f%~n", [Compliance]).
```

### Checking Specific Security Categories

```erlang
%% Check only authentication
AuthResult = erlmcp_security_validator:validate_authentication(Module),
case maps:get(status, AuthResult) of
    passed -> io:format("Authentication: OK~n");
    failed -> io:format("Authentication: FAILED~n")
end.

%% Check only secret management
SecretsResult = erlmcp_security_validator:validate_secret_management(Module),
case maps:get(status, SecretsResult) of
    passed -> io:format("Secrets: OK~n");
    failed -> io:format("Secrets: FAILED~n")
end.
```

### Integrating with CI/CD

```erlang
%% In a validation script
run_security_validation() ->
    {ok, _} = application:ensure_all_started(erlmcp_validation),

    %% Run validation
    {ok, Report} = erlmcp_security_validator:generate_report(),

    %% Check compliance threshold
    #{overall_compliance := Compliance} = Report,
    case Compliance >= 80.0 of
        true ->
            io:format("Security validation PASSED (~.1f%)~n", [Compliance]),
            halt(0);
        false ->
            io:format("Security validation FAILED (~.1f%)~n", [Compliance]),
            halt(1)
    end.
```

## Testing Guidance

### Unit Tests for Security Checks

```erlang
%% Test authentication validation
authentication_check_test() ->
    {ok, _} = erlmcp_security_validator:start_link(),

    Result = erlmcp_security_validator:validate_authentication(erlmcp_transport_stdio),

    % Verify structure
    ?assert(is_map(Result)),
    ?assert(maps:is_key(category, Result)),
    ?assertEqual(<<"authentication">>, maps:get(category, Result)),

    % Verify status is one of: passed, failed, warning
    Status = maps:get(status, Result),
    ?assert(lists:member(Status, [passed, failed, warning])).

%% Test secret detection
hardcoded_secrets_test() ->
    %% Create a test file with secrets
    TestCode = "
        password := \"secret123\",
        api_key := \"AKIA1234567890ABCDEFGHI\",
    ",

    %% The scanner should detect these patterns
    %% (Actual implementation scans real source files)
    ok.
```

### Property-Based Tests

```erlang
%% Property: All validated modules return valid check structure
prop_security_validation_structure() ->
    ?FORALL(Module, oneof([
        erlmcp_transport_stdio,
        erlmcp_transport_tcp,
        erlmcp_transport_http
    ]),
        begin
            Result = erlmcp_security_validator:validate_authentication(Module),
            is_map(Result) andalso
            maps:is_key(checks, Result) andalso
            maps:is_key(passed, Result) andalso
            maps:is_key(failed, Result) andalso
            lists:member(maps:get(status, Result), [passed, failed, warning])
        end).
```

## Troubleshooting

### Common Security Issues

#### Hardcoded Secrets Detected

**Symptom:**
```
#{name => no_hardcoded_secrets, status => failed,
  message => <<"Hardcoded secrets detected">>}
```

**Solution:**
1. Search source files for detected patterns
2. Move secrets to environment variables
3. Use `application:get_env/3` to retrieve secrets

```erlang
%% Instead of:
password = "hardcoded_password"

%% Use:
password = case application:get_env(myapp, password) of
    {ok, Pwd} -> Pwd;
    undefined -> os:getenv("APP_PASSWORD")
end.
```

#### Token Exposure in Logs

**Symptom:**
```
#{name => token_handling, status => warning,
  message => <<"Potential token exposure in logs">>}
```

**Solution:**
1. Review logging statements
2. Redact sensitive fields before logging
3. Use structured logging with filtering

```erlang
%% Bad:
logger:info("Token: ~p", [Token]),

%% Good:
logger:info("Token: ~s", [redact_token(Token)]),
redact_token(<<>>) -> <<>>;
redact_token(Token) when byte_size(Token) > 8 ->
    Head = binary:part(Token, 0, 4),
    Tail = binary:part(Token, byte_size(Token) -4, 4),
    <<Head/binary, "****", Tail/binary>>.
```

#### Missing SQL Injection Prevention

**Symptom:**
```
#{name => sql_injection_prevention, status => warning,
  message => <<"SQL found without clear parameterization">>}
```

**Solution:**
1. Use parameterized queries
2. Avoid string concatenation for SQL
3. Use proper escaping functions

```erlang
%% Bad:
Sql = io_lib:format("SELECT * FROM users WHERE name='~s'", Name),

%% Good:
Sql = <<"SELECT * FROM users WHERE name=$1">>,
pgsql:equery(Sql, [Name]).
```

## Security Best Practices

### Authentication

1. **Use strong session IDs**: Generate with `crypto:strong_rand_bytes/1`
2. **Monitor sessions**: Track session lifecycle and terminate inactive ones
3. **Implement RBAC**: Role-based access control for fine-grained permissions

### Input Validation

1. **Validate all inputs**: Never trust client data
2. **Use JSON Schema**: Validate against schema with jesse
3. **Sanitize output**: Escape HTML, validate URIs

### Secret Management

1. **Never hardcode secrets**: Use environment variables
2. **Encrypt at rest**: Use crypto for sensitive data
3. **Rotate keys**: Implement key rotation mechanism
4. **Use vault services**: Consider external secret management

### Rate Limiting

1. **Per-IP limits**: Prevent DoS from single sources
2. **Per-user limits**: Prevent abuse from authenticated users
3. **Burst handling**: Allow bursts within sustained limits

### CORS

1. **Whitelist origins**: Only allow known origins
2. **Validate headers**: Check Origin header explicitly
3. **Preflight requests**: Handle OPTIONS correctly

## Related Documentation

- [MCP_SPEC_VALIDATION.md](MCP_SPEC_VALIDATION.md) - Validation overview
- [erlmcp_auth](../apps/erlmcp_core/src/erlmcp_auth.erl) - Authentication implementation
- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
