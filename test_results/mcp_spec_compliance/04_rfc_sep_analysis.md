# MCP RFC & SEP Analysis Report

**Analysis Date:** 2026-01-30
**Report Version:** 1.0
**Scope:** Analysis of SEP-1034, SEP-1036, SEP-1330, PR #797, SEP-835, SEP-985, SEP-991 and their impact on erlmcp implementation

## Executive Summary

This report analyzes 7 key MCP Specification Enhancement Proposals (SEPs) and 1 major RFC that define the future of the Model Context Protocol. The SEPs introduce significant enhancements to elicitation, authorization, client registration, and schema handling capabilities. Most notably, SEP-1036 (URL Mode Elicitation) enables secure out-of-band interactions, SEP-991 introduces URL-based client registration, and SEP-835 enhances authorization with incremental scope consent. These changes provide substantial improvements for enterprise deployments, security, and user experience while maintaining backward compatibility.

## RFC Requirement Catalog

### SEP-1034: Default Values for Primitive Types (Final)

**Status:** Final (2025-07-22)
**Track:** Standards Track
**Implementation Complexity:** Low

#### Requirements
- Extend all elicitation primitive schemas to support optional `default` fields:
  - `StringSchema`: `default?: string`
  - `NumberSchema`: `default?: number`
  - `EnumSchema`: `default?: string` (must be one of enum values)
  - `BooleanSchema`: Already supported (no change)
- Default values must match schema type
- Clients that support defaults SHOULD pre-populate form fields
- Clients that don't support defaults MAY ignore the field

#### Use Cases
- Pre-populated email forms with common recipients
- Number inputs with sensible defaults (e.g., "1" for quantity)
- Enum selections with most common option pre-selected
- Improved user experience for repetitive tasks

#### Backward Compatibility
- Fully backward compatible
- Existing implementations continue to work unchanged
- Adoption is optional and incremental

#### Security Considerations
- No new security concerns
- Default values are visible to users
- Client control over data submission maintained

### SEP-1036: URL Mode Elicitation (Final)

**Status:** Final (2025-07-22)
**Track:** Standards Track
**Implementation Complexity:** High

#### Requirements
- Introduce two elicitation modes:
  - `form` mode (existing behavior, now named)
  - `url` mode (new out-of-band interactions)
- Clients MUST declare supported modes:
  ```json
  {
    "capabilities": {
      "elicitation": {
        "form": {},
        "url": {}
      }
    }
  }
  ```
- URL mode requests include:
  - `mode: "url"`
  - `url: string` (HTTPS URL for user interaction)
  - `elicitationId: string` (unique identifier)
  - `message: string` (explanation for user)
- Support completion notifications via `notifications/elicitation/complete`
- Return `URLElicitationRequiredError` (code -32042) when URL mode required

#### Use Cases
- Secure credential collection (API keys, passwords)
- External OAuth flows (3rd-party authorization)
- Payment processing with PCI compliance
- Any interaction requiring browser security boundaries

#### Security Requirements
- SSRF prevention: Validate URLs before fetching
- Protocol restrictions: HTTPS only
- Domain validation: Display target domains to users
- Identity verification: Bind elicitation to user sessions
- Rate limiting: Prevent abuse of metadata fetches

#### Backward Compatibility
- Breaking change: Capability declaration required
- Migration path: Support form mode initially, add URL mode incrementally
- Existing form elicitation continues with mode parameter addition

#### Implementation Impact
- Clients need secure browser contexts
- Servers need elicitation state management
- Clear trust boundaries between client and server

### SEP-1330: Enhanced Enum Schema (Final)

**Status:** Final (2025-08-11)
**Track:** Standards Track
**Implementation Complexity:** Medium

#### Requirements
- Deprecate non-standard `enumNames` property in favor of JSON Schema-compliant patterns
- Support multiple enum schema types:
  - `LegacyEnumSchema`: Current approach (deprecated but supported)
  - `UntitledSingleSelectEnumSchema`: Plain enum without titles
  - `TitledSingleSelectEnumSchema`: Single select with display names using `oneOf` + `const`
  - `UntitledMultiSelectEnumSchema`: Multiple select without titles
  - `TitledMultiSelectEnumSchema`: Multiple select with display names
- Extend `ElicitResult` to support `string[]` for multi-select enums
- Add `minItems` and `maxItems` for multi-select constraints

#### Schema Examples
**Single-Select with Titles:**
```json
{
  "type": "string",
  "oneOf": [
    { "const": "#FF0000", "title": "Red" },
    { "const": "#00FF00", "title": "Green" },
    { "const": "#0000FF", "title": "Blue" }
  ],
  "default": "#00FF00"
}
```

**Multi-Select with Titles:**
```json
{
  "type": "array",
  "title": "Color Selection",
  "minItems": 1,
  "maxItems": 3,
  "items": {
    "oneOf": [
      { "const": "#FF0000", "title": "Red" },
      { "const": "#00FF00", "title": "Green" },
      { "const": "#0000FF", "title": "Blue" }
    ]
  },
  "default": ["#00FF00"]
}
```

#### Backward Compatibility
- Legacy schema continues to work (marked as deprecated)
- Incremental adoption possible
- Client implementations can support legacy and new formats

#### Standards Compliance
- Aligns with JSON Schema 2020-12 specification
- Works with existing JSON Schema validators
- Improves interoperability across implementations

### PR #797: OpenID Connect Discovery (Final)

**Status:** Final (2025-11-25)
**Implementation Complexity:** Low

#### Requirements
- Enhance authorization server discovery with OpenID Connect Discovery 1.0 support
- Integrate with existing OAuth 2.0 flows
- Provide standardized mechanism for discovery
- Add icons as additional metadata for tools, resources, resource templates, and prompts

#### Implementation Impact
- Servers can advertise OIDC discovery endpoint
- Clients can use standard OIDC discovery patterns
- Simplifies integration with existing identity providers
- Enables consistent metadata handling

### SEP-835: Incremental Scope Consent (Final)

**Status:** Final (2025-10-03)
**Track:** Standards Track
**Implementation Complexity:** Medium

#### Requirements
- Enhanced scope selection strategy with priority-based approach:
  1. `WWW-Authenticate` scope parameter (immediate, contextual)
  2. `scopes_supported` in Protected Resource Metadata (fallback)
- Principle of least privilege guidance
- Enhanced error handling:
  - Standardized 403 `insufficient_scope` responses with `resource_metadata`
  - Scope upgrade flow with differentiated client behavior:
    - Interactive clients SHOULD attempt upgrade
    - Client credentials clients MAY attempt or abort immediately
- Security enhancements:
  - New Scope Minimization security section
  - Fine-grained scopes guidance
  - Progressive access patterns support

#### Use Cases
- Dynamic scope upgrades during operation
- Reduced permission prompts
- More secure access patterns
- Better user experience for incremental authorization

#### Backward Compatibility
- All changes are additive or clarifications
- Existing implementations continue to work
- No breaking changes to protocol messages

#### Client Differentiation
- **Interactive clients** (authorization_code): Full upgrade flow capability
- **Client credentials clients**: Limited upgrade options, can abort immediately

### SEP-985: RFC 9728 Protected Resource Metadata (Final)

**Status:** Final (2025-07-16)
**Track:** Standards Track
**Implementation Complexity:** Low

#### Requirements
- Align OAuth 2.0 Protected Resource Metadata handling with RFC 9728
- Make WWW-Authenticate header optional with fallback to `.well-known` endpoint
- Update flow:
  1. Attempt MCP request without token
  2. If 401 received: Check WWW-Authenticate header for `resource_metadata`
  3. If header missing/invalid: Fallback to `/.well-known/oauth-protected-resource`
  4. Use discovered metadata to build authorization server URL

#### Benefits
- More flexible deployment models
- Reduced communication overhead in distributed environments
- Easier MCP adoption in complex infrastructure
- Maintains existing capabilities while adding flexibility

#### Security Considerations
- No reduction in security
- Fallback mechanism is optional
- Servers SHOULD still return WWW-Authenticate headers when feasible

### SEP-991: OAuth Client ID Metadata Documents (Final)

**Status:** Final (2025-07-07)
**Track:** Standards Track
**Implementation Complexity:** High

#### Requirements
- Adopt OAuth Client ID Metadata Documents as SHOULD (better default)
- Change Dynamic Client Registration (DCR) to MAY
- URL-based client registration where:
  - `client_id` is HTTPS URL pointing to metadata document
  - Server fetches and validates metadata before authorization
- Support flexible trust policies:
  - Open servers: Accept any HTTPS client_id
  - Protected servers: Restrict to trusted domains or specific clients
- No client pre-coordination required

#### Metadata Document Format
```json
{
  "client_id": "https://app.example.com/oauth/client-metadata.json",
  "client_name": "Example MCP Client",
  "client_uri": "https://app.example.com",
  "logo_uri": "https://app.example.com/logo.png",
  "redirect_uris": [
    "http://127.0.0.1:3000/callback",
    "http://localhost:3000/callback"
  ],
  "grant_types": ["authorization_code"],
  "response_types": ["code"],
  "token_endpoint_auth_method": "none"
}
```

#### Security Requirements
- SSRF prevention: Validate URLs and IPs
- Rate limiting: Prevent abuse of metadata fetches
- Caching: Respect HTTP cache headers (max 24h recommended)
- Redirect URI validation: Ensure match with metadata document

#### Use Cases
- No pre-existing relationship scenarios
- Developer-friendly onboarding
- Server-controlled trust without pre-coordination
- Stable, auditable client identifiers

#### Risks and Mitigations
- **Localhost impersonation**: Additional warnings for localhost-only clients
- **SSRF**: URL validation, rate limiting, timeout requests
- **DDoS**: Aggressive caching, bandwidth considerations
- **Implementation burden**: Low infrastructure requirement (static JSON file)

## Experimental Feature Roadmap

### Phase 1: Core Enhancements (Q1 2026)
- **SEP-1034**: Default values for primitive types
  - Implementation priority: High
  - Impact: User experience improvement
  - Dependencies: None

- **SEP-1330**: Enhanced enum schema support
  - Implementation priority: High
  - Impact: Developer experience improvement
  - Dependencies: Schema validation updates

### Phase 2: Authorization Improvements (Q2 2026)
- **SEP-835**: Incremental scope consent
  - Implementation priority: Medium
  - Impact: Security and UX improvement
  - Dependencies: OAuth flow enhancements

- **PR #797**: OpenID Connect Discovery
  - Implementation priority: Medium
  - Impact: Identity provider integration
  - Dependencies: Authorization server updates

### Phase 3: Advanced Capabilities (Q3 2026)
- **SEP-991**: Client ID Metadata Documents
  - Implementation priority: Medium
  - Impact: Client registration simplification
  - Dependencies: Metadata fetching infrastructure

- **SEP-1036**: URL Mode Elicitation
  - Implementation priority: Low (complex)
  - Impact: Secure out-of-band interactions
  - Dependencies: Secure browser contexts, elicitation state management

### Phase 4: Optimization (Q4 2026)
- **SEP-985**: RFC 9728 alignment
  - Implementation priority: Low
  - Impact: Deployment flexibility
  - Dependencies: Protected resource metadata handling

## Implementation Guide for Erlang

### Schema Updates (apps/erlmcp_core/src/)

#### 1. Elicitation Schema Definitions
```erlang
%% apps/erlmcp_core/src/erlmcp_schemas.erl
-record(string_schema, {
    type = string :: string(),
    title :: maybe_binary(),
    description :: maybe_binary(),
    min_length :: maybe_integer(),
    max_length :: maybe_integer(),
    format :: maybe_binary(),  % email | uri | date | date-time
    default :: maybe_binary()  %% NEW: SEP-1034
}).

-record(number_schema, {
    type = number :: number | integer,
    title :: maybe_binary(),
    description :: maybe_binary(),
    minimum :: maybe_integer(),
    maximum :: maybe_integer(),
    default :: maybe_integer()  %% NEW: SEP-1034
}).

-record(enum_schema, {
    type = string :: string(),
    title :: maybe_binary(),
    description :: maybe_binary(),
    enum :: [binary()],  %% For legacy compatibility
    enum_names :: maybe_binary(),  %% LEGACY: To be deprecated
    default :: maybe_binary(),  %% NEW: SEP-1034
    one_of :: maybe_list(),  %% NEW: SEP-1330
    multi_select :: boolean()  %% NEW: SEP-1330
}).
```

#### 2. Elicitation Mode Support
```erlang
%% apps/erlmcp_core/src/erlmcp_elicitation.erl
-record(elicitation_request, {
    mode :: form | url,  %% NEW: SEP-1036
    message :: binary(),
    requested_schema :: map(),
    url :: maybe_binary(),  %% NEW: SEP-1036
    elicitation_id :: binary()  %% NEW: SEP-1036
}).

-record(elicitation_response, {
    action :: accept | decline | cancel,
    content :: map()  %% Extended for string[] arrays: SEP-1330
}).
```

### Authorization Updates (apps/erlmcp_core/src/)

#### 1. Scope Management
```erlang
%% apps/erlmcp_core/src/erlmcp_auth.erl
-record(scope_strategy, {
    priority :: www_authenticate | scopes_supported,
    current_scopes :: [binary()],
    required_scopes :: [binary()],
    resource_metadata :: maybe(map())  %% SEP-835
}).

%% Handle incremental scope consent
-spec handle_scope_upgrade(binary(), map(), list()) -> {ok, binary()} | {error, term()}.
```

#### 2. Client Registration
```erlang
%% apps/erlmcp_core/src/erlmcp_oauth.erl
-record(client_metadata, {
    client_id :: binary(),  %% Can be URL for SEP-991
    client_name :: binary(),
    redirect_uris :: [binary()],
    token_endpoint_auth_method :: binary(),
    jwks_uri :: maybe_binary()  %% For private_key_jwt
}).

%% Fetch and validate client metadata
-spec fetch_client_metadata(binary()) -> {ok, map()} | {error, term()}.
```

### Transport Layer Updates (apps/erlmcp_transports/src/)

#### 1. URL Validation
```erlang
%% apps/erlmcp_transports/src/erlmcp_transport_validation.erl
%% SSRF protection for URL mode elicitation
-spec validate_url(binary()) -> ok | {error, term()}.
```

#### 2. Metadata Fetching
```erlang
%% apps/erlmcp_transports/src/erlmcp_oauth_fetcher.erl
%% For SEP-991 and SEP-985
-spec fetch_metadata(binary(), map()) -> {ok, map()} | {error, term()}.
```

### State Management

#### 1. Elicitation State
```erlang
%% Track URL mode elicitations
-record(elicitation_state, {
    id :: binary(),
    mode :: form | url,
    created :: integer(),
    expires :: integer(),
    user_id :: binary(),
    metadata :: map()
}).
```

#### 2. Client Metadata Cache
```erlang
%% Cache client metadata with TTL
-record(client_metadata_cache, {
    url :: binary(),
    metadata :: map(),
    expires :: integer(),
    last_fetched :: integer()
}).
```

### Migration Path

#### Step 1: Schema Updates (Low Risk)
- Implement SEP-1034 (default values)
- Add backward compatibility for existing schemas
- Update validation logic

#### Step 2: Enhanced Enum Support (Medium Risk)
- Implement SEP-1330 schema variants
- Add legacy schema support (deprecated but functional)
- Update client compatibility layer

#### Step 3: Authorization Enhancements (Medium Risk)
- Implement SEP-835 scope upgrade flows
- Add SEP-985 fallback logic
- Update OpenID Connect discovery

#### Step 4: Client Registration (High Risk)
- Implement SEP-991 metadata fetching
- Add URL-based client validation
- Update OAuth flow handling

#### Step 5: URL Mode Elicitation (High Risk)
- Implement SEP-1036 URL mode
- Add secure browser context handling
- Update elicitation state management

## Security Considerations

### SEP-1034 (Default Values)
- **Risk**: Default values might expose sensitive information
- **Mitigation**: Follow existing security guidelines against sensitive data
- **Implementation**: Validate default values against security policies

### SEP-1036 (URL Mode Elicitation)
- **Risk**: SSRF attacks via malicious URLs
- **Mitigation**: Strict URL validation, IP allowlisting
- **Implementation**: Rate limiting, request timeout, size limits

- **Risk**: Phishing through malicious URLs
- **Mitigation**: Clear domain display, user confirmation
- **Implementation**: Security warnings for unknown domains

### SEP-1330 (Enhanced Enums)
- **Risk**: Schema complexity increases attack surface
- **Mitigation**: Input validation, schema whitelisting
- **Implementation**: Strict JSON Schema validation

### SEP-835 (Incremental Scope)
- **Risk**: Scope upgrade attacks
- **Mitigation**: Rate limiting, user confirmation
- **Implementation**: Audit logging, consent tracking

### SEP-985 (Protected Resource Metadata)
- **Risk**: Metadata tampering
- **Mitigation**: HTTPS validation, certificate pinning
- **Implementation**: Cache validation, integrity checks

### SEP-991 (Client ID Metadata)
- **Risk**: Metadata abuse in DDoS attacks
- **Mitigation**: Request throttling, caching
- **Implementation**: Size limits, timeout controls

## Testing Requirements

### Unit Tests
- Schema validation for all new formats
- URL validation and security checks
- OAuth flow edge cases
- Cache behavior and TTL handling

### Integration Tests
- End-to-end elicitation flows
- Authorization with incremental scope upgrades
- Client registration via metadata documents
- Error handling and fallback scenarios

### Security Tests
- SSRF vulnerability testing
- Phishing resistance validation
- Rate limiting effectiveness
- Cache poisoning prevention

### Performance Tests
- Metadata fetching under load
- Elicitation state management scalability
- OAuth flow performance impact
- Memory usage with large schemas

## Performance Considerations

### SEP-1034
- Minimal performance impact
- Default value validation adds minimal overhead

### SEP-1036
- Metadata fetching adds latency
- Caching strategy critical for performance
- Parallel processing for multiple elicitations

### SEP-1330
- Schema complexity increases validation time
- Pre-compilation of schemas recommended
- Caching of schema definitions

### SEP-835
- Scope upgrade flow adds round trips
- Client differentiation requires additional logic
- Caching of scope decisions improves performance

### SEP-985
- Fallback mechanism adds complexity
- Discovery caching reduces overhead
- Parallel metadata fetching improves performance

### SEP-991
- Metadata fetching is network-intensive
- Aggressive caching recommended
- CDN support for metadata endpoints

## Conclusion

The analyzed SEPs and RFC represent significant advancements in the MCP ecosystem:

### Key Benefits
1. **Enhanced Security**: URL mode elicitation, incremental scope consent, and metadata-based registration
2. **Improved UX**: Default values, enhanced enum handling, and flexible client registration
3. **Enterprise Ready**: OIDC integration, fine-grained scope management, and deployment flexibility
4. **Standards Compliance**: JSON Schema 2020-12, RFC 9728 alignment

### Implementation Priority
1. **High Priority**: SEP-1034 (default values), SEP-1330 (enhanced enums)
2. **Medium Priority**: SEP-835 (incremental scope), PR #797 (OIDC)
3. **Low Priority**: SEP-985 (flexible metadata), SEP-991 (client metadata), SEP-1036 (URL mode)

### Migration Strategy
- Implement incrementally with backward compatibility
- Use feature flags for gradual rollout
- Provide clear documentation for each enhancement
- Maintain existing behavior during transition

### Final Recommendations
1. Start with SEP-1034 and SEP-1330 for immediate user experience improvements
2. Plan for SEP-835 and PR #797 to enhance authorization capabilities
3. Consider SEP-991 for improved client onboarding in enterprise environments
4. Implement SEP-1036 last due to its complexity and security requirements
5. Always maintain backward compatibility throughout the migration

This analysis provides a comprehensive foundation for implementing these important MCP enhancements in erlmcp, balancing innovation with stability and security.