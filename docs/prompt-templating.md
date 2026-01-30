# Prompt String Templating Engine

## Overview

The erlmcp prompt templating engine provides Mustache-based template interpolation for MCP Prompts API. This eliminates the need for manual string construction in prompt handlers, making prompts more maintainable and enterprise-ready.

## Features

- **Mustache Syntax**: Standard `{{variable}}` interpolation
- **Sections**: Support for lists and conditionals with `{{#section}}...{{/section}}`
- **Inverted Sections**: `{{^section}}...{{/section}}` for negation
- **Validation**: Pre-flight template syntax validation
- **Auto-Detection**: Automatic template rendering when syntax is detected
- **Type Support**: Handles binary, string, integer, float, boolean, and map values
- **Nested Data**: Supports nested maps with dot notation (e.g., `{{user.name}}`)

## Module: erlmcp_prompt_template

### API Functions

#### render/2

```erlang
-spec render(template(), variables()) -> binary().
```

Render a template with variables. Throws `{template_render_error, Reason}` on failure.

**Example:**
```erlang
Template = <<"Hello {{name}}!">>,
Variables = #{<<"name">> => <<"World">>},
Result = erlmcp_prompt_template:render(Template, Variables).
% Result: <<"Hello World!">>
```

#### render_safe/2

```erlang
-spec render_safe(template(), variables()) -> {ok, binary()} | {error, term()}.
```

Safe version of render/2 that returns a result tuple instead of throwing.

**Example:**
```erlang
case erlmcp_prompt_template:render_safe(Template, Variables) of
    {ok, Result} -> handle_success(Result);
    {error, Reason} -> handle_error(Reason)
end.
```

#### validate/1

```erlang
-spec validate(template()) -> ok | {error, term()}.
```

Validate template syntax without rendering.

**Example:**
```erlang
case erlmcp_prompt_template:validate(<<"Hello {{name}}!">>) of
    ok -> io:format("Template is valid~n");
    {error, Reason} -> io:format("Invalid: ~p~n", [Reason])
end.
```

#### has_template_syntax/1

```erlang
-spec has_template_syntax(binary() | string()) -> boolean().
```

Check if text contains template syntax ({{...}}).

**Example:**
```erlang
HasSyntax = erlmcp_prompt_template:has_template_syntax(<<"Hello {{name}}">>).
% HasSyntax: true
```

## Integration with MCP Server

The templating engine is automatically integrated with `erlmcp_server` prompt handling. When a prompt handler returns a result, the server automatically:

1. Detects if the result contains template syntax (`{{...}}`)
2. Renders templates with the provided arguments
3. Returns the rendered result to the client

### Automatic Template Rendering

**Before (manual string construction):**
```erlang
Handler = fun(#{<<"topic">> := Topic, <<"style">> := Style}) ->
    <<"Write a ", Style/binary, " essay about ", Topic/binary>>
end.
```

**After (with templating):**
```erlang
Handler = fun(_Args) ->
    <<"Write a {{style}} essay about {{topic}}">>
end.
```

The arguments are automatically passed to the template renderer.

### Adding Prompts with Templates

```erlang
%% Simple template-based prompt
ok = erlmcp_server:add_prompt_with_args(Server, <<"essay_prompt">>,
    fun(_Args) ->
        % Return template string - will be auto-rendered
        <<"Write a {{style}} essay about {{topic}}">>
    end,
    [
        #mcp_prompt_argument{
            name = <<"topic">>,
            description = <<"Essay topic">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"style">>,
            description = <<"Writing style">>,
            required = false
        }
    ]).
```

### Complex Templates with Sections

```erlang
%% Code review prompt with rules section
ok = erlmcp_server:add_prompt_with_args(Server, <<"code_review">>,
    fun(_Args) ->
        [#{
            <<"role">> => <<"system">>,
            <<"content">> => #{
                <<"type">> => <<"text">>,
                <<"text">> => <<"You are reviewing {{language}} code in {{style}} style.\n"
                               "Review rules:\n"
                               "{{#rules}}• {{rule}}\n{{/rules}}">>
            }
        }]
    end,
    [
        #mcp_prompt_argument{name = <<"language">>, required = true},
        #mcp_prompt_argument{name = <<"style">>, required = true},
        #mcp_prompt_argument{name = <<"rules">>, required = false}
    ]).
```

**Client Request:**
```json
{
    "method": "prompts/get",
    "params": {
        "name": "code_review",
        "arguments": {
            "language": "Erlang",
            "style": "OTP",
            "rules": [
                {"rule": "Check gen_server callbacks"},
                {"rule": "Verify supervision tree"},
                {"rule": "Ensure error handling"}
            ]
        }
    }
}
```

**Server Response:**
```json
{
    "messages": [{
        "role": "system",
        "content": {
            "type": "text",
            "text": "You are reviewing Erlang code in OTP style.\nReview rules:\n• Check gen_server callbacks\n• Verify supervision tree\n• Ensure error handling\n"
        }
    }]
}
```

## Template Syntax Reference

### Variables

```
{{variable}}
```

Replaced with the value of `variable` from the variables map.

### Sections (Lists)

```
{{#items}}
  Item: {{name}}
{{/items}}
```

Repeats the section for each element in the `items` list.

### Sections (Booleans)

```
{{#show}}
  This is visible when show is true
{{/show}}
```

Shows content when `show` is truthy (true, non-empty string, non-empty list).

### Inverted Sections

```
{{^items}}
  No items available
{{/items}}
```

Shows content when `items` is falsy (false, empty list, undefined).

### Nested Data

```
{{user.name}}
{{user.email}}
```

Access nested map values using dot notation.

### Comments

```
{{! This is a comment and won't be rendered }}
```

## Error Handling

### Template Rendering Errors

When a template fails to render, the server returns:

```json
{
    "error": {
        "code": -32044,
        "message": "Template rendering failed: <reason>"
    }
}
```

Error code `-32044` is `MCP_ERROR_PROMPT_RENDER_FAILED`.

### Validation Errors

Template validation errors are caught during `add_prompt` operations:

```erlang
case erlmcp_prompt_template:validate(Template) of
    ok ->
        % Add prompt
        ok;
    {error, {parse_error, Reason}} ->
        % Handle invalid template
        {error, {invalid_template, Reason}}
end.
```

## Best Practices

### 1. Validate Templates Early

```erlang
% Validate templates when adding prompts
add_safe_prompt(Server, Name, Template, Args) ->
    case erlmcp_prompt_template:validate(Template) of
        ok ->
            Handler = fun(_) -> Template end,
            erlmcp_server:add_prompt_with_args(Server, Name, Handler, Args);
        {error, Reason} ->
            {error, {invalid_template, Name, Reason}}
    end.
```

### 2. Provide Defaults for Optional Variables

```erlang
Handler = fun(Args) ->
    % Ensure optional variables have defaults
    Variables = maps:merge(#{
        <<"style">> => <<"formal">>,
        <<"length">> => <<"medium">>
    }, Args),
    erlmcp_prompt_template:render(Template, Variables)
end.
```

### 3. Use Explicit Template Rendering

For complex logic, explicitly call the template renderer:

```erlang
Handler = fun(Args) ->
    % Complex logic
    Template = case maps:get(<<"type">>, Args) of
        <<"essay">> -> <<"Write an essay about {{topic}}>>;
        <<"summary">> -> <<"Summarize {{topic}}>>;
        _ -> <<"Discuss {{topic}}">>
    end,
    erlmcp_prompt_template:render(Template, Args)
end.
```

### 4. Handle Missing Variables

```erlang
% bbmustache renders missing variables as empty strings
% Validate required fields before rendering
validate_and_render(Template, Args, Required) ->
    case check_required_fields(Args, Required) of
        ok ->
            erlmcp_prompt_template:render(Template, Args);
        {error, Missing} ->
            error({missing_required_fields, Missing})
    end.
```

## Performance Considerations

### Template Caching

Templates are parsed on each render. For high-frequency prompts, consider:

```erlang
% Pre-parse template in module initialization
-define(CACHED_TEMPLATE, bbmustache:parse_binary(<<"...">>)).

render_cached(Variables) ->
    bbmustache:compile(?CACHED_TEMPLATE, Variables, [
        {key_type, atom},
        {escape_fun, fun(X) -> X end}
    ]).
```

### Large Templates

For large templates (>10KB), consider:
- Breaking into smaller sections
- Using resource links for static content
- Streaming responses for very large outputs

## Testing

### Unit Tests

```erlang
-include_lib("eunit/include/eunit.hrl").

render_simple_test() ->
    Template = <<"Hello {{name}}!">>,
    Variables = #{<<"name">> => <<"World">>},
    ?assertEqual(<<"Hello World!">>,
                 erlmcp_prompt_template:render(Template, Variables)).

render_missing_variable_test() ->
    Template = <<"Hello {{name}}!">>,
    Variables = #{},
    % Missing variables render as empty string
    ?assertEqual(<<"Hello !">>,
                 erlmcp_prompt_template:render(Template, Variables)).
```

### Integration Tests

See `apps/erlmcp_core/test/erlmcp_prompt_template_tests.erl` for comprehensive test suite.

## Examples

See `examples/prompt_template_example.erl` for runnable examples demonstrating:
- Simple variable interpolation
- Multiple variables
- Essay writing prompts
- Code review prompts with sections
- Template validation
- Server integration patterns

## Migration Guide

### From Manual String Construction

**Before:**
```erlang
fun(#{<<"lang">> := Lang, <<"style">> := Style}) ->
    <<"Write ", Lang/binary, " code in ", Style/binary, " style">>
end
```

**After:**
```erlang
fun(_Args) ->
    <<"Write {{lang}} code in {{style}} style">>
end
```

### From io_lib:format

**Before:**
```erlang
fun(#{<<"count">> := Count, <<"item">> := Item}) ->
    iolist_to_binary(io_lib:format("You have ~p ~s", [Count, Item]))
end
```

**After:**
```erlang
fun(_Args) ->
    <<"You have {{count}} {{item}}">>
end
```

## Dependencies

- **bbmustache** (1.12.2): Mustache template engine for Erlang
  - Already included in rebar.config dependencies
  - No additional installation required

## See Also

- [MCP Protocol Documentation](protocol.md)
- [Prompts API Reference](api-reference.md#prompts)
- [bbmustache Documentation](https://github.com/soranoba/bbmustache)
- [Mustache Specification](https://mustache.github.io/mustache.5.html)
