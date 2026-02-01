# [Feature Name] User Guide Template

**Version**: 3.0.0
**Last Updated**: YYYY-MM-DD
**Maintainer**: [Name]

---

## Overview

[Brief description of what this feature does and who it is for. Include the problem it solves and the value it provides.]

**Target Audience**: [Specify: developers, operators, architects, etc.]

---

## Prerequisites

Before using this feature, ensure you have:

- [ ] [Requirement 1 with version]
- [ ] [Requirement 2 with version]
- [ ] [Requirement 3 with version]
- [ ] [Any specific configuration]

---

## Quick Start

Get started with [Feature Name] in 5 minutes:

```bash
# Step 1: [Action]
[Command or code]

# Step 2: [Action]
[Command or code]

# Step 3: [Action]
[Command or code]

# Verify
[Verification command]
```

**Expected Output**: [What you should see]

---

## Concepts

### [Concept 1]

[Clear explanation of the first key concept]

- [Aspect 1]: [Description]
- [Aspect 2]: [Description]

### [Concept 2]

[Clear explanation of the second key concept]

**Relationship to Concept 1**: [How they relate]

### Glossary

| Term | Definition |
|------|------------|
| [Term 1] | [Definition] |
| [Term 2] | [Definition] |

---

## Usage

### Basic Usage

[Simplest use case with complete example]

```erlang
% Complete, runnable example
-module(example).
-export([run/0]).

run() ->
    % Step 1
    Result1 = do_something(),
    % Step 2
    Result2 = do_something_else(Result1),
    % Step 3
    finalize(Result2).
```

**Expected Result**: [What happens]

### Advanced Usage

[More complex scenarios]

#### Scenario 1: [Title]

[Description of when to use this scenario]

```erlang
% Complete example for scenario 1
```

**Parameters**:
- `Param1`: [Description and type]
- `Param2`: [Description and type]

**Returns**: [Type and description]

#### Scenario 2: [Title]

[Description of when to use this scenario]

```erlang
% Complete example for scenario 2
```

---

## Configuration

### Required Configuration

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `param1` | `type()` | `default` | Description |
| `param2` | `type()` | `default` | Description |

### Optional Configuration

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `opt_param1` | `type()` | `default` | Description |
| `opt_param2` | `type()` | `undefined` | Description |

### Configuration Examples

#### Example 1: Minimal Configuration

```erlang
%% sys.config
{erlmcp, [
    {feature_name, [
        {param1, value1}
    ]}
]}.
```

#### Example 2: Full Configuration

```erlang
%% sys.config
{erlmcp, [
    {feature_name, [
        {param1, value1},
        {param2, value2},
        {opt_param1, value3}
    ]}
]}.
```

#### Environment Variables

```bash
# Set via environment
export ERLMCP_FEATURE_PARAM1=value1
export ERLMCP_FEATURE_PARAM2=value2
```

---

## API Reference

### Function: `function_name/1`

```erlang
-spec function_name(InputType) -> {ok, OutputType} | {error, Reason}.
```

**Description**: [What the function does]

**Parameters**:
- `Input`: [Type and description]

**Returns**:
- `{ok, Output}`: [Description of success case]
- `{error, Reason}`: [Description of error cases]

**Example**:

```erlang
case function_name(Input) of
    {ok, Result} ->
        handle_success(Result);
    {error, Reason} ->
        handle_error(Reason)
end.
```

---

## Best Practices

### Do's

- [Practice 1]: [Why this is good]
- [Practice 2]: [Why this is good]
- [Practice 3]: [Why this is good]

### Don'ts

- [Anti-pattern 1]: [Why to avoid]
- [Anti-pattern 2]: [Why to avoid]
- [Anti-pattern 3]: [Why to avoid]

### Performance Considerations

[Performance characteristics and optimization tips]

---

## Testing

### Unit Testing

```erlang
% Example test
feature_test() ->
    Input = setup_input(),
    {ok, Output} = feature:process(Input),
    ?assertEqual(Expected, Output).
```

### Integration Testing

```erlang
% Example integration test
feature_integration_test() ->
    {ok, Pid} = feature:start_link(Config),
    {ok, Result} = feature:operation(Pid, Input),
    ?assertMatch({ok, _}, Result).
```

---

## Troubleshooting

### Problem: [Issue Title]

**Symptoms**:
- [Symptom 1]
- [Symptom 2]

**Diagnosis**:
[How to identify the issue]

**Solution**:
```erlang
% Code fix or configuration change
```

### Problem: [Another Issue Title]

**Symptoms**:
- [Symptom 1]

**Solution**:
[Step-by-step resolution]

---

## Examples

See the `examples/` directory for complete working examples:

- `examples/simple/` - [Description]
- `examples/advanced/` - [Description]
- `examples/integration/` - [Description]

---

## References

### Related Documentation

- [Link to related doc 1]
- [Link to related doc 2]

### External References

- [External link 1]
- [External link 2]

---

## Changelog

### Version 3.0.0 (YYYY-MM-DD)
- [Change 1]
- [Change 2]

### Version 2.x.x
- See CHANGELOG.md for full history

---

## Support

For issues, questions, or contributions:

- **Documentation Issues**: [Link to issue tracker]
- **Feature Requests**: [Link to feature request tracker]
- **Questions**: [Link to discussions or forum]

---

**Status**: [Draft | Review | Published]
**Review Date**: [YYYY-MM-DD]
**Next Review**: [YYYY-MM-DD]
