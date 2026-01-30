# Erlang Validator Template Generator

## Overview

This directory contains Tera templates for generating Erlang validation modules for MCP components. The template system creates gen_server-based validators with comprehensive validation capabilities including JSON Schema validation, semantic rules, and security checks.

## Files

- **validator.erl.tera** - Main template for generating validation modules
- **validator_examples.yaml** - Configuration examples for different component types
- **README.md** - This file

## Features

The validator template generates modules with:

1. **JSON Schema Validation**
   - Integration with `erlmcp_schema_validator` and jesse
   - Automatic error formatting with field-level details
   - Support for custom schemas per component type

2. **Semantic Validation Rules**
   - Configurable via ontology-driven SPARQL queries
   - Component-specific constraints (description length, version format, etc.)
   - Extensible validation rule framework

3. **Security Validation**
   - URI canonicalization to prevent injection attacks
   - Path traversal detection (../,  %2e%2e/, etc.)
   - Length limits and format validation

4. **Error Formatting**
   - Detailed field-level error messages
   - JSON-RPC 2.0 compliant error codes
   - Integration with MCP error code system

5. **Integration Points**
   - Works with existing `erlmcp_schema_validator` pool
   - Compatible with current validation patterns
   - Follows OTP gen_server best practices

## Critical Gap Fix

This template addresses **Gap #42: Tool Argument Validation** by generating validators that check arguments **BEFORE** handler invocation. This prevents:
- Invalid arguments reaching tool handlers
- Unvalidated input causing handler failures
- Missing required arguments being silently ignored

## Usage

### 1. Basic Usage

Generate a validator module using configuration:

```bash
# Using ggen tool (when implemented)
ggen generate \
  --template templates/ggen/erlang/validator.erl.tera \
  --config validator_examples.yaml \
  --output apps/erlmcp_core/src/erlmcp_tool_argument_validator.erl
```

### 2. SPARQL Integration

Extract validation rules from ontology:

```sparql
PREFIX mcp: <http://erlmcp.org/ontology#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?rule ?description ?constraint
WHERE {
  ?rule a mcp:ValidationRule ;
        mcp:appliesTo mcp:Tool ;
        rdfs:comment ?description ;
        mcp:constraint ?constraint .
}
```

Transform SPARQL results to template variables:

```erlang
ValidationRules = [
    #{
        name => <<"description_length">>,
        description => <<"Validate tool description does not exceed maximum length">>,
        constraint => <<"byte_size(Description) =< 10000">>
    },
    % ... more rules
]
```

### 3. Integration with Existing Code

#### Tool Argument Validation

```erlang
%% In erlmcp_server.erl or tool handler
handle_tool_call(ToolName, Args, State) ->
    case erlmcp_tool_argument_validator:validate_tool_arguments(Args, ToolDef) of
        ok ->
            %% Execute tool handler
            execute_tool_handler(ToolName, Args, State);
        {error, {Code, Message, Data}} ->
            %% Return validation error
            {error, Code, Message, Data}
    end.
```

#### Resource URI Validation

```erlang
%% In resource handler
handle_resource_read(Uri, State) ->
    case erlmcp_resource_uri_validator:validate_uri(Uri) of
        ok ->
            %% Read resource
            read_resource(Uri, State);
        {error, Reason} ->
            {error, ?MCP_ERROR_INVALID_URI, <<"Invalid resource URI">>, Reason}
    end.
```

#### Prompt Argument Validation

```erlang
%% In prompt handler
handle_prompt_get(PromptName, Args, State) ->
    case erlmcp_prompt_argument_validator_v2:validate_prompt_arguments(Args, PromptDef) of
        ok ->
            %% Execute prompt handler
            execute_prompt_handler(PromptName, Args, State);
        {error, {Code, Message, Data}} ->
            {error, Code, Message, Data}
    end.
```

## Template Variables

### Required Variables

| Variable | Type | Description | Example |
|----------|------|-------------|---------|
| `component` | string | Component type (tool, resource, prompt, task) | `"tool"` |
| `module_name` | string | Generated module name | `"erlmcp_tool_validator"` |
| `validation_type` | string | Type of validation (argument, structure, semantic) | `"argument"` |

### Optional Variables

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `validation_rules` | array | `[]` | List of semantic validation rules |
| `schema` | map | `undefined` | JSON Schema for component |
| `description_max_length` | integer | `10000` | Maximum description length |
| `uri_validation_enabled` | boolean | `false` | Enable URI validation |
| `path_traversal_check` | boolean | `false` | Enable path traversal detection |
| `error_code_prefix` | string | `"MCP_ERROR_"` | Error code prefix |
| `includes_schema_validator` | boolean | `true` | Include schema validator integration |
| `timestamp` | string | (current) | Generation timestamp |
| `generator_version` | string | `"1.0.0"` | Generator version |

## Component Types

### Tool Validator

**Purpose**: Validate tool definitions and arguments before handler invocation.

**Features**:
- Description length validation (default: 10,000 chars)
- Semantic version validation (X.Y.Z format)
- JSON Schema validation for input arguments
- Metadata structure validation

**Example**: See `tool_validator` in `validator_examples.yaml`

### Resource Validator

**Purpose**: Validate resource URIs and prevent security vulnerabilities.

**Features**:
- URI canonicalization (RFC 3986)
- Path traversal prevention (../, %2e%2e/, etc.)
- URI length limits (default: 2048 chars)
- Scheme validation

**Example**: See `resource_validator` in `validator_examples.yaml`

### Prompt Validator

**Purpose**: Validate prompt definitions and arguments.

**Features**:
- Required argument validation
- JSON Schema validation for input arguments
- Description length validation
- Argument type checking

**Example**: See `prompt_validator` in `validator_examples.yaml`

### Task Validator

**Purpose**: Validate task definitions (NEW in MCP 2025-11-25).

**Features**:
- Task ID format validation
- Status enum validation
- Timestamp format validation (ISO 8601)
- Result/error structure validation

**Example**: See `task_validator` in `validator_examples.yaml`

## Validation Rules from Ontology

Validation rules can be extracted from the ontology using SPARQL queries. Each rule should define:

1. **Name**: Unique identifier for the rule
2. **Description**: Human-readable explanation
3. **Constraint**: Erlang expression or validation logic

Example validation rule structure:

```yaml
validation_rules:
  - name: description_length
    description: "Validate tool description does not exceed maximum length"
    constraint: "byte_size(Description) =< 10000"
  - name: input_schema_structure
    description: "Validate tool input schema is valid JSON Schema"
    constraint: "jesse:validate_schema(InputSchema)"
```

## Error Codes

The template uses standard JSON-RPC 2.0 and MCP error codes:

| Code | Constant | Description |
|------|----------|-------------|
| -32602 | `JSONRPC_INVALID_PARAMS` | Invalid parameters |
| -32001 | `MCP_ERROR_RESOURCE_NOT_FOUND` | Resource not found |
| -32002 | `MCP_ERROR_TOOL_NOT_FOUND` | Tool not found |
| -32011 | `MCP_ERROR_TOOL_DESCRIPTION_TOO_LONG` | Tool description too long |
| -32022 | `MCP_ERROR_INVALID_URI` | Invalid URI |
| -32034 | `MCP_ERROR_INVALID_TOOL_ARGUMENTS` | Invalid tool arguments |

See `apps/erlmcp_core/include/erlmcp.hrl` for complete error code list.

## Testing Generated Validators

After generating a validator module:

1. **Compile the module**:
   ```bash
   rebar3 compile
   ```

2. **Run unit tests**:
   ```bash
   rebar3 eunit --module=<module_name>_tests
   ```

3. **Check Dialyzer**:
   ```bash
   rebar3 dialyzer
   ```

4. **Verify xref**:
   ```bash
   rebar3 xref
   ```

## Example: Generating Tool Validator

```bash
# 1. Create configuration file
cat > tool_validator_config.yaml <<EOF
component: tool
module_name: erlmcp_tool_argument_validator
validation_type: argument
description_max_length: 10000
includes_schema_validator: true
error_code_prefix: MCP_ERROR_TOOL_
validation_rules:
  - name: description_length
    description: "Validate tool description length"
  - name: version_format
    description: "Validate semantic version format"
EOF

# 2. Generate validator
ggen generate \
  --template validator.erl.tera \
  --config tool_validator_config.yaml \
  --output apps/erlmcp_core/src/erlmcp_tool_argument_validator.erl

# 3. Compile and test
rebar3 compile
rebar3 eunit --module=erlmcp_tool_argument_validator_tests
```

## Integration with MCP Server

Add generated validators to the MCP server supervision tree:

```erlang
%% In erlmcp_core_sup.erl
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },

    ChildSpecs = [
        %% Schema validator pool
        #{
            id => erlmcp_schema_validator_pool,
            start => {erlmcp_schema_validator_pool, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Tool argument validator
        #{
            id => erlmcp_tool_argument_validator,
            start => {erlmcp_tool_argument_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Resource URI validator
        #{
            id => erlmcp_resource_uri_validator,
            start => {erlmcp_resource_uri_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

## Performance Considerations

1. **Schema Validation**: Uses pooled workers to avoid blocking
2. **Caching**: Consider caching validation results for frequently-used schemas
3. **Timeouts**: Default 5000ms call timeout for validation operations
4. **Monitoring**: Track validation stats in gen_server state

## Extending the Template

To add new validation rules:

1. Add rule to `validation_rules` in configuration
2. Implement rule logic in template's `validate_<component>_semantics/1`
3. Add corresponding error codes to `erlmcp.hrl`
4. Update tests to cover new rules

## References

- MCP Specification: [MCP 2025-11-25](https://spec.modelcontextprotocol.io/2025-11-25/)
- JSON-RPC 2.0: [JSON-RPC Specification](https://www.jsonrpc.org/specification)
- Jesse JSON Schema: [Jesse GitHub](https://github.com/for-GET/jesse)
- Tera Templates: [Tera Documentation](https://tera.netlify.app/)

## Support

For issues or questions:
- Check existing validation patterns in `apps/erlmcp_core/src/erlmcp_*_validator.erl`
- Review MCP compliance analysis in `docs/mcp-compliance/`
- See OTP patterns in `docs/otp-patterns.md`
