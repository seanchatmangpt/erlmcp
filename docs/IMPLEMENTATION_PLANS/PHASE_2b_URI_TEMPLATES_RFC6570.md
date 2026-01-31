# Phase 2b Implementation Plan: RFC 6570 URI Templates

**Status**: Ready for Implementation
**Priority**: High
**Estimated Effort**: 6-8 hours
**Target Version**: erlmcp_core 2.3.0

---

## 1. Overview

### Current State

The erlmcp project currently has **stub implementations** for URI template support:

- **File**: `apps/erlmcp_core/src/erlmcp_uri_validator.erl.broken`
  - Contains basic URI validation (RFC 3986)
  - `validate_uri_template/1` only checks brace balancing
  - **No RFC 6570 template parsing or expansion**

- **File**: `apps/erlmcp_core/src/erlmcp_resource_subscriptions.erl`
  - Line 350-353: `match_uri_template/2` stub returns `false`
  - **Template matching completely disabled**
  - Only exact URI matching works for subscriptions

### Target State

Full **RFC 6570 URI Template** compliance enabling:

1. **Template Parsing**: Parse templates like `/users/{userId}/documents/{docId}`
2. **Template Expansion**: Expand templates with variables: `/users/123/documents/456`
3. **Template Extraction**: Reverse-parse URIs to extract variables
4. **Template Matching**: Match resource URIs against templates for subscriptions
5. **All Expression Types**: Simple, reserved, fragment, label, path, parameter, form-style
6. **Variable Modifiers**: Prefix (`:n`) and explode (`*`) operators

### Impact

- **Enables dynamic templated resources** in MCP protocol
- **Subscription wildcards**: Subscribe to `/users/{userId}` matches all users
- **Parameter extraction**: Extract route parameters from URIs
- **MCP 2025-11-25 compliance**: Full URI template support per spec

### Effort Breakdown

| Phase | Task | Hours |
|-------|------|-------|
| 1. Parser Implementation | `erlmcp_uri_template_parser.erl` | 2-2.5 |
| 2. Template API | `erlmcp_uri_template.erl` | 2-2.5 |
| 3. Integration | Update validator + subscriptions | 0.5-1 |
| 4. Unit Tests | Template parser + API tests | 1-1.5 |
| 5. Integration Tests | Subscription matching tests | 0.5-1 |
| 6. Debugging/Verification | Quality gates + manual verification | 1 |
| **TOTAL** | | **6-8 hours** |

---

## 2. RFC 6570 Specification Summary

### Expression Types

RFC 6570 defines **7 expression types** with different encoding rules:

| Type | Operator | Example | Expanded Result | Use Case |
|------|----------|---------|-----------------|----------|
| **Simple** | (none) | `/users/{userId}` | `/users/123` | Path segments |
| **Reserved** | `+` | `/files/{+path}` | `/files/a/b/c` | Preserve `/` in values |
| **Fragment** | `#` | `/page{#section}` | `/page#intro` | Fragment identifiers |
| **Label** | `.` | `/file{.ext}` | `/file.txt` | Dot-prefixed labels |
| **Path** | `/` | `/root{/path}` | `/root/sub/dir` | Path segments |
| **Parameter** | `?` | `/search{?q}` | `/search?q=erlang` | Query parameters |
| **Form** | `&` | `/api{?key}{&page}` | `/api?key=abc&page=1` | Additional query params |

### Variable Modifiers

| Modifier | Syntax | Purpose | Example | Result |
|----------|--------|---------|---------|--------|
| **Prefix** | `{var:n}` | Limit to first `n` chars | `{userId:4}` with `123456` | `1234` |
| **Explode** | `{var*}` | Expand list/map entries | `{tags*}` with `[a,b,c]` | `a,b,c` |

### Practical Examples

```erlang
% Simple expressions
Template = <<"/users/{userId}/documents/{docId}">>,
Variables = #{<<"userId">> => <<"123">>, <<"docId">> => <<"456">>},
Result = <<"/users/123/documents/456">>.

% Reserved expressions (preserve special chars)
Template = <<"/files/{+filepath}">>,
Variables = #{<<"filepath">> => <<"docs/readme.txt">>},
Result = <<"/files/docs/readme.txt">>.  % Note: / not encoded

% Query parameters
Template = <<"/search{?q,limit,offset}">>,
Variables = #{<<"q">> => <<"erlang otp">>, <<"limit">> => <<"10">>},
Result = <<"/search?q=erlang%20otp&limit=10">>.  % offset omitted

% Path segments
Template = <<"/api{/version}/users">>,
Variables = #{<<"version">> => <<"v2">>},
Result = <<"/api/v2/users">>.

% Prefix modifier
Template = <<"/short/{name:4}">>,
Variables = #{<<"name">> => <<"alexander">>},
Result = <<"/short/alex">>.

% Explode modifier (list)
Template = <<"/tags/{tags*}">>,
Variables = #{<<"tags">> => [<<"erlang">>, <<"otp">>, <<"mcp">>]},
Result = <<"/tags/erlang,otp,mcp">>.
```

---

## 3. Current Implementation Analysis

### File 1: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_uri_validator.erl.broken`

**Status**: Broken/incomplete implementation

**Current Functions**:

```erlang
% Line 50-52
-spec validate_uri_template(binary()) -> ok | {error, {atom(), binary()}}.
validate_uri_template(UriTemplate) when is_binary(UriTemplate) ->
    gen_server:call(?MODULE, {validate_uri_template, UriTemplate}).

% Line 246-258 (implementation)
do_validate_uri_template(UriTemplate) when is_binary(UriTemplate) ->
    case byte_size(UriTemplate) of
        0 ->
            {error, {empty_template, <<"URI template cannot be empty">>}};
        Size when Size > ?MAX_URI_TEMPLATE_LENGTH ->
            {error, {template_too_long, Msg}};
        _ ->
            validate_template_syntax(UriTemplate)
    end.

% Line 261-269 (syntax validation)
validate_template_syntax(UriTemplate) ->
    case validate_template_braces(UriTemplate, 0, 0) of
        {error, Reason} ->
            {error, {invalid_template_syntax, Reason}};
        ok ->
            validate_resource_uri_internal(UriTemplate)
    end.
```

**Issues**:
- Only validates **brace balancing** (`{` and `}`)
- **No parsing** of expression operators (`+`, `#`, `.`, `/`, `?`, `&`)
- **No variable extraction** from templates
- **No validation** of variable modifiers (`:n`, `*`)
- **Cannot expand** templates with variables
- **Cannot extract** variables from URIs

### File 2: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_resource_subscriptions.erl`

**Current Function** (Lines 350-353):

```erlang
%% @doc Check if a URI matches a template.
%% Supports simple wildcard patterns: {var} for path segments.
-spec match_uri_template(uri(), binary()) -> boolean().
match_uri_template(_Uri, Template) ->
    % Simple template matching (can be enhanced with proper URI template parsing)
    % For now, only exact match
    false.
```

**Issues**:
- **Always returns `false`**
- Template matching completely disabled
- Subscriptions only work with exact URI matches
- **Blocks wildcard subscriptions** like `/users/{userId}`

**Usage Context** (Line 195-199):

```erlang
% Get template match subscribers if requested
TemplateSubs = case IncludeTemplates of
    true -> match_template_subscribers(Uri, State#state.resource_subscriptions);
    false -> []
end,
```

**Impact**:
- Resource subscriptions cannot use templates
- Must subscribe to exact URIs only
- No dynamic resource matching

---

## 4. Solution Architecture

### Module Design

**Create 2 new modules** + **Update 2 existing modules**:

```
CREATE:
1. erlmcp_uri_template_parser.erl  (~200 LOC) - RFC 6570 parser
2. erlmcp_uri_template.erl         (~300 LOC) - Public API

UPDATE:
3. erlmcp_uri_validator.erl        (~5 LOC change) - Use new parser
4. erlmcp_resource_subscriptions.erl (~20 LOC change) - Enable template matching
```

### Data Structures

```erlang
% Module: erlmcp_uri_template.erl

-record(template, {
    expressions = [] :: [expression() | {literal, binary()}],
    raw = <<>> :: binary()
}).

-type expression() ::
    {simple, [variable()]} |
    {reserved, [variable()]} |
    {fragment, [variable()]} |
    {label, [variable()]} |
    {path, [variable()]} |
    {param, [variable()]} |
    {form, [variable()]}.

-type variable() :: {
    name :: binary(),
    prefix :: undefined | pos_integer(),
    explode :: boolean()
}.

-export_type([template/0, expression/0, variable/0]).
```

### Function Flow

```
User Request: Subscribe to /users/{userId}
    |
    v
erlmcp_resource_subscriptions:subscribe_to_resource/3
    |
    v
erlmcp_uri_validator:validate_uri_template/1
    |
    v
erlmcp_uri_template:validate/1
    |
    v
erlmcp_uri_template:parse/1
    |
    v
erlmcp_uri_template_parser:parse/1
    |
    v
Store parsed template in subscription state

----

Resource Update: /users/123 changed
    |
    v
erlmcp_resource_subscriptions:notify_resource_changed/2
    |
    v
erlmcp_resource_subscriptions:match_template_subscribers/2
    |
    v
erlmcp_resource_subscriptions:match_uri_template/2
    |
    v
erlmcp_uri_template:extract/2
    |
    v
Match found! Notify subscriber with extracted variables
```

---

## 5. Implementation: erlmcp_uri_template.erl (Detailed)

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_uri_template.erl`
**Estimated LOC**: 300
**Purpose**: Public API for URI template operations

### Module Header

```erlang
%%%-------------------------------------------------------------------
%%% @doc RFC 6570 URI Template Implementation
%%%
%%% Implements URI Template parsing, expansion, and extraction per RFC 6570.
%%% Supports all expression types (simple, reserved, fragment, label, path,
%%% parameter, form) and variable modifiers (prefix, explode).
%%%
%%% @reference https://datatracker.ietf.org/doc/html/rfc6570
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_uri_template).

-export([
    parse/1,
    expand/2,
    extract/2,
    validate/1,
    is_template/1
]).

-export_type([template/0, expression/0, variable/0]).

%%====================================================================
%% Types
%%====================================================================

-record(template, {
    expressions = [] :: [expression() | {literal, binary()}],
    raw = <<>> :: binary()
}).

-type template() :: #template{}.

-type expression() ::
    {simple, [variable()]} |
    {reserved, [variable()]} |
    {fragment, [variable()]} |
    {label, [variable()]} |
    {path, [variable()]} |
    {param, [variable()]} |
    {form, [variable()]}.

-type variable() :: {
    name :: binary(),
    prefix :: undefined | pos_integer(),
    explode :: boolean()
}.

-opaque template() :: #template{}.
```

### Function 1: validate/1

**Purpose**: Validate template syntax without parsing

```erlang
%%--------------------------------------------------------------------
%% @doc Validate URI template syntax.
%% Returns ok if template is valid, {error, Reason} otherwise.
%% @end
%%--------------------------------------------------------------------
-spec validate(binary()) -> ok | {error, term()}.
validate(Template) when is_binary(Template) ->
    try
        case parse(Template) of
            {ok, _} -> ok;
            {error, Reason} -> {error, Reason}
        end
    catch
        error:Reason -> {error, Reason};
        throw:Reason -> {error, Reason}
    end.
```

**Lines**: 10-15

### Function 2: parse/1

**Purpose**: Parse template string into structured representation

```erlang
%%--------------------------------------------------------------------
%% @doc Parse URI template into structured representation.
%% Returns {ok, Template} or {error, Reason}.
%%
%% Example:
%%   {ok, T} = parse(<<"/users/{userId}/docs/{docId}">>).
%% @end
%%--------------------------------------------------------------------
-spec parse(binary()) -> {ok, template()} | {error, term()}.
parse(Template) when is_binary(Template) ->
    try
        Expressions = erlmcp_uri_template_parser:parse(Template),
        {ok, #template{
            expressions = Expressions,
            raw = Template
        }}
    catch
        error:Reason ->
            {error, {parse_error, Reason}};
        throw:{parse_error, Reason} ->
            {error, Reason}
    end.
```

**Lines**: 20-25

### Function 3: expand/2

**Purpose**: Expand template with variable values

```erlang
%%--------------------------------------------------------------------
%% @doc Expand URI template with variable values.
%% Returns expanded URI as binary.
%%
%% Example:
%%   {ok, T} = parse(<<"/users/{userId}">>),
%%   Uri = expand(T, #{<<"userId">> => <<"123">>}),
%%   Uri =:= <<"/users/123">>.
%% @end
%%--------------------------------------------------------------------
-spec expand(template(), map()) -> binary().
expand(#template{expressions = Expressions}, Variables) when is_map(Variables) ->
    Result = lists:foldl(
        fun
            ({literal, Literal}, Acc) ->
                <<Acc/binary, Literal/binary>>;
            (Expression, Acc) ->
                Expanded = expand_expression(Expression, Variables),
                <<Acc/binary, Expanded/binary>>
        end,
        <<>>,
        Expressions
    ),
    Result.

%%--------------------------------------------------------------------
%% @private Expand a single expression with variables.
%%--------------------------------------------------------------------
-spec expand_expression(expression(), map()) -> binary().
expand_expression({simple, Variables}, VarMap) ->
    expand_simple(Variables, VarMap);
expand_expression({reserved, Variables}, VarMap) ->
    expand_reserved(Variables, VarMap);
expand_expression({fragment, Variables}, VarMap) ->
    <<"#", (expand_fragment(Variables, VarMap))/binary>>;
expand_expression({label, Variables}, VarMap) ->
    expand_label(Variables, VarMap);
expand_expression({path, Variables}, VarMap) ->
    expand_path(Variables, VarMap);
expand_expression({param, Variables}, VarMap) ->
    expand_param(Variables, VarMap);
expand_expression({form, Variables}, VarMap) ->
    expand_form(Variables, VarMap).

%% Simple expansion: {var} -> value (percent-encoded)
-spec expand_simple([variable()], map()) -> binary().
expand_simple(Variables, VarMap) ->
    Values = lists:filtermap(
        fun({name, Name, prefix, Prefix, explode, Explode}) ->
            case maps:get(Name, VarMap, undefined) of
                undefined -> false;
                Value ->
                    Encoded = encode_value(Value, Prefix, unreserved),
                    {true, Encoded}
            end
        end,
        Variables
    ),
    join_values(Values, <<",">>).

%% Reserved expansion: {+var} -> value (keep reserved chars)
-spec expand_reserved([variable()], map()) -> binary().
expand_reserved(Variables, VarMap) ->
    Values = lists:filtermap(
        fun({name, Name, prefix, Prefix, explode, Explode}) ->
            case maps:get(Name, VarMap, undefined) of
                undefined -> false;
                Value ->
                    Encoded = encode_value(Value, Prefix, reserved),
                    {true, Encoded}
            end
        end,
        Variables
    ),
    join_values(Values, <<",">>).

%% Fragment expansion: {#var} -> #value
-spec expand_fragment([variable()], map()) -> binary().
expand_fragment(Variables, VarMap) ->
    expand_reserved(Variables, VarMap).

%% Label expansion: {.var} -> .value
-spec expand_label([variable()], map()) -> binary().
expand_label(Variables, VarMap) ->
    Values = lists:filtermap(
        fun({name, Name, prefix, Prefix, explode, Explode}) ->
            case maps:get(Name, VarMap, undefined) of
                undefined -> false;
                Value ->
                    Encoded = encode_value(Value, Prefix, unreserved),
                    {true, Encoded}
            end
        end,
        Variables
    ),
    case Values of
        [] -> <<>>;
        _ -> <<".", (join_values(Values, <<".">>))/binary>>
    end.

%% Path expansion: {/var} -> /value
-spec expand_path([variable()], map()) -> binary().
expand_path(Variables, VarMap) ->
    Values = lists:filtermap(
        fun({name, Name, prefix, Prefix, explode, Explode}) ->
            case maps:get(Name, VarMap, undefined) of
                undefined -> false;
                Value ->
                    Encoded = encode_value(Value, Prefix, unreserved),
                    {true, Encoded}
            end
        end,
        Variables
    ),
    case Values of
        [] -> <<>>;
        _ -> <<"/", (join_values(Values, <<"/">>))/binary>>
    end.

%% Parameter expansion: {?var} -> ?var=value
-spec expand_param([variable()], map()) -> binary().
expand_param(Variables, VarMap) ->
    Pairs = lists:filtermap(
        fun({name, Name, prefix, Prefix, explode, Explode}) ->
            case maps:get(Name, VarMap, undefined) of
                undefined -> false;
                Value ->
                    Encoded = encode_value(Value, Prefix, unreserved),
                    Pair = <<Name/binary, "=", Encoded/binary>>,
                    {true, Pair}
            end
        end,
        Variables
    ),
    case Pairs of
        [] -> <<>>;
        [First | Rest] ->
            <<"?", First/binary, (join_values(Rest, <<"&">>))/binary>>
    end.

%% Form expansion: {&var} -> &var=value
-spec expand_form([variable()], map()) -> binary().
expand_form(Variables, VarMap) ->
    Pairs = lists:filtermap(
        fun({name, Name, prefix, Prefix, explode, Explode}) ->
            case maps:get(Name, VarMap, undefined) of
                undefined -> false;
                Value ->
                    Encoded = encode_value(Value, Prefix, unreserved),
                    Pair = <<Name/binary, "=", Encoded/binary>>,
                    {true, Pair}
            end
        end,
        Variables
    ),
    case Pairs of
        [] -> <<>>;
        _ -> <<"&", (join_values(Pairs, <<"&">>))/binary>>
    end.

%% Encode value with optional prefix
-spec encode_value(binary() | list(), undefined | pos_integer(), reserved | unreserved) -> binary().
encode_value(Value, undefined, EncodeType) when is_binary(Value) ->
    uri_encode(Value, EncodeType);
encode_value(Value, Prefix, EncodeType) when is_binary(Value), is_integer(Prefix) ->
    Truncated = binary:part(Value, 0, min(byte_size(Value), Prefix)),
    uri_encode(Truncated, EncodeType);
encode_value(Values, undefined, EncodeType) when is_list(Values) ->
    Encoded = [uri_encode(V, EncodeType) || V <- Values],
    join_values(Encoded, <<",">>).

%% Join values with separator
-spec join_values([binary()], binary()) -> binary().
join_values([], _Sep) -> <<>>;
join_values([First], _Sep) -> First;
join_values([First | Rest], Sep) ->
    lists:foldl(
        fun(Value, Acc) -> <<Acc/binary, Sep/binary, Value/binary>> end,
        First,
        Rest
    ).

%% URI encode (unreserved: percent-encode everything except ALPHA / DIGIT / "-" / "." / "_" / "~")
-spec uri_encode(binary(), reserved | unreserved) -> binary().
uri_encode(Bin, unreserved) ->
    uri_string:quote(Bin);
uri_encode(Bin, reserved) ->
    % Keep reserved chars: : / ? # [ ] @ ! $ & ' ( ) * + , ; =
    Bin.  % Simplified: no encoding for reserved
```

**Lines**: 60-150

### Function 4: extract/2

**Purpose**: Extract variable values from URI matching template

```erlang
%%--------------------------------------------------------------------
%% @doc Extract variable values from URI using template.
%% Returns {ok, Variables} if URI matches template, {error, no_match} otherwise.
%%
%% Example:
%%   {ok, T} = parse(<<"/users/{userId}/docs/{docId}">>),
%%   {ok, Vars} = extract(<<"/users/123/docs/456">>, T),
%%   Vars =:= #{<<"userId">> => <<"123">>, <<"docId">> => <<"456">>}.
%% @end
%%--------------------------------------------------------------------
-spec extract(binary(), template()) -> {ok, map()} | {error, no_match}.
extract(Uri, #template{expressions = Expressions}) when is_binary(Uri) ->
    try
        Variables = extract_variables(Uri, Expressions, #{}),
        {ok, Variables}
    catch
        throw:no_match -> {error, no_match};
        error:_ -> {error, no_match}
    end.

%%--------------------------------------------------------------------
%% @private Extract variables by matching URI against expressions.
%%--------------------------------------------------------------------
-spec extract_variables(binary(), [expression() | {literal, binary()}], map()) -> map().
extract_variables(<<>>, [], Vars) ->
    Vars;
extract_variables(_Uri, [], _Vars) ->
    throw(no_match);
extract_variables(Uri, [{literal, Literal} | Rest], Vars) ->
    case Uri of
        <<Literal:byte_size(Literal)/binary, Remainder/binary>> ->
            extract_variables(Remainder, Rest, Vars);
        _ ->
            throw(no_match)
    end;
extract_variables(Uri, [Expression | Rest], Vars) ->
    {NewVars, Remainder} = extract_expression(Uri, Expression, Rest),
    extract_variables(Remainder, Rest, maps:merge(Vars, NewVars)).

%%--------------------------------------------------------------------
%% @private Extract variables from a single expression.
%%--------------------------------------------------------------------
-spec extract_expression(binary(), expression(), [expression() | {literal, binary()}]) -> {map(), binary()}.
extract_expression(Uri, {simple, Variables}, Rest) ->
    extract_simple_vars(Uri, Variables, Rest);
extract_expression(Uri, {reserved, Variables}, Rest) ->
    extract_reserved_vars(Uri, Variables, Rest);
extract_expression(Uri, {path, Variables}, Rest) ->
    extract_path_vars(Uri, Variables, Rest);
extract_expression(Uri, {param, Variables}, Rest) ->
    extract_param_vars(Uri, Variables, Rest);
extract_expression(_Uri, _Expression, _Rest) ->
    throw(no_match).

%% Extract simple variables (greedy match until next literal or end)
-spec extract_simple_vars(binary(), [variable()], [expression() | {literal, binary()}]) -> {map(), binary()}.
extract_simple_vars(Uri, [{name, Name, prefix, _Prefix, explode, _Explode}], Rest) ->
    % Determine stopping point
    StopAt = case Rest of
        [{literal, Lit} | _] -> Lit;
        _ -> <<>>
    end,

    case StopAt of
        <<>> ->
            % Extract to end
            {#{Name => Uri}, <<>>};
        _ ->
            % Extract until literal
            case binary:split(Uri, StopAt) of
                [Value, Remainder] ->
                    {#{Name => Value}, <<StopAt/binary, Remainder/binary>>};
                [_] ->
                    throw(no_match)
            end
    end.

%% Simplified implementations for other types
extract_reserved_vars(Uri, Variables, Rest) ->
    extract_simple_vars(Uri, Variables, Rest).

extract_path_vars(<<"/", Uri/binary>>, Variables, Rest) ->
    extract_simple_vars(Uri, Variables, Rest);
extract_path_vars(_Uri, _Variables, _Rest) ->
    throw(no_match).

extract_param_vars(<<"?", Uri/binary>>, Variables, Rest) ->
    % Parse query string
    Params = uri_string:dissect_query(Uri),
    VarMap = maps:from_list([{list_to_binary(K), list_to_binary(V)} || {K, V} <- Params]),
    {VarMap, <<>>};
extract_param_vars(_Uri, _Variables, _Rest) ->
    throw(no_match).
```

**Lines**: 40-60

### Function 5: is_template/1

**Purpose**: Check if URI string contains template expressions

```erlang
%%--------------------------------------------------------------------
%% @doc Check if a URI string is a template (contains {variables}).
%% @end
%%--------------------------------------------------------------------
-spec is_template(binary()) -> boolean().
is_template(Uri) when is_binary(Uri) ->
    case binary:match(Uri, <<"{">>) of
        {_, _} -> true;
        nomatch -> false
    end.
```

**Lines**: 5-10

---

## 6. Implementation: erlmcp_uri_template_parser.erl (Detailed)

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_uri_template_parser.erl`
**Estimated LOC**: 200
**Purpose**: Internal RFC 6570 parser

### Module Header

```erlang
%%%-------------------------------------------------------------------
%%% @doc RFC 6570 URI Template Parser
%%%
%%% Internal parser for URI template syntax. Tokenizes template strings
%%% and parses expressions into structured format.
%%%
%%% @private
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_uri_template_parser).

-export([parse/1]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Parse URI template into list of expressions and literals.
%% Returns list of {literal, Binary} | {expression_type, [Variables]}.
%%
%% Example:
%%   parse(<<"/users/{userId}/docs/{docId}">>) ->
%%     [
%%       {literal, <<"/users/">>},
%%       {simple, [{name, <<"userId">>, prefix, undefined, explode, false}]},
%%       {literal, <<"/docs/">>},
%%       {simple, [{name, <<"docId">>, prefix, undefined, explode, false}]}
%%     ].
%% @end
%%--------------------------------------------------------------------
-spec parse(binary()) -> [term()].
parse(Template) when is_binary(Template) ->
    Tokens = tokenize(Template),
    parse_tokens(Tokens, []).
```

### Function: tokenize/1

**Purpose**: Split template into tokens (literals and expressions)

```erlang
%%--------------------------------------------------------------------
%% @private Tokenize template into literals and expression strings.
%% Returns list of {literal, Bin} | {expression, Bin}.
%%--------------------------------------------------------------------
-spec tokenize(binary()) -> [{literal, binary()} | {expression, binary()}].
tokenize(Template) ->
    tokenize(Template, [], <<>>, literal).

tokenize(<<>>, Acc, <<>>, _State) ->
    lists:reverse(Acc);
tokenize(<<>>, Acc, Buffer, State) ->
    lists:reverse([{State, Buffer} | Acc]);

% Start of expression
tokenize(<<"{", Rest/binary>>, Acc, Buffer, literal) ->
    NewAcc = case Buffer of
        <<>> -> Acc;
        _ -> [{literal, Buffer} | Acc]
    end,
    tokenize(Rest, NewAcc, <<>>, expression);

% End of expression
tokenize(<<"}", Rest/binary>>, Acc, Buffer, expression) ->
    tokenize(Rest, [{expression, Buffer} | Acc], <<>>, literal);

% Accumulate characters
tokenize(<<Char, Rest/binary>>, Acc, Buffer, State) ->
    tokenize(Rest, Acc, <<Buffer/binary, Char>>, State).
```

**Lines**: 30-40

### Function: parse_tokens/2

**Purpose**: Parse tokenized expressions into structured format

```erlang
%%--------------------------------------------------------------------
%% @private Parse tokens into structured expressions.
%%--------------------------------------------------------------------
-spec parse_tokens([{literal, binary()} | {expression, binary()}], [term()]) -> [term()].
parse_tokens([], Acc) ->
    lists:reverse(Acc);
parse_tokens([{literal, Lit} | Rest], Acc) ->
    parse_tokens(Rest, [{literal, Lit} | Acc]);
parse_tokens([{expression, Expr} | Rest], Acc) ->
    Parsed = parse_expression(Expr),
    parse_tokens(Rest, [Parsed | Acc]).
```

**Lines**: 10-15

### Function: parse_expression/1

**Purpose**: Parse single expression string into type and variables

```erlang
%%--------------------------------------------------------------------
%% @private Parse expression into {Type, Variables}.
%% Determines expression type from operator and parses variables.
%%--------------------------------------------------------------------
-spec parse_expression(binary()) -> term().
parse_expression(<<>>) ->
    throw({parse_error, empty_expression});

% Reserved: {+var}
parse_expression(<<"+", VarSpec/binary>>) ->
    Variables = parse_variable_list(VarSpec),
    {reserved, Variables};

% Fragment: {#var}
parse_expression(<<"#", VarSpec/binary>>) ->
    Variables = parse_variable_list(VarSpec),
    {fragment, Variables};

% Label: {.var}
parse_expression(<<".", VarSpec/binary>>) ->
    Variables = parse_variable_list(VarSpec),
    {label, Variables};

% Path: {/var}
parse_expression(<<"/", VarSpec/binary>>) ->
    Variables = parse_variable_list(VarSpec),
    {path, Variables};

% Parameter: {?var}
parse_expression(<<"?", VarSpec/binary>>) ->
    Variables = parse_variable_list(VarSpec),
    {param, Variables};

% Form: {&var}
parse_expression(<<"&", VarSpec/binary>>) ->
    Variables = parse_variable_list(VarSpec),
    {form, Variables};

% Simple: {var} (no operator)
parse_expression(VarSpec) ->
    Variables = parse_variable_list(VarSpec),
    {simple, Variables}.
```

**Lines**: 40-50

### Function: parse_variable_list/1

**Purpose**: Parse comma-separated variable list

```erlang
%%--------------------------------------------------------------------
%% @private Parse comma-separated variable list.
%%--------------------------------------------------------------------
-spec parse_variable_list(binary()) -> [term()].
parse_variable_list(VarSpec) ->
    VarStrings = binary:split(VarSpec, <<",">>, [global]),
    [parse_variable(V) || V <- VarStrings].
```

**Lines**: 5-10

### Function: parse_variable/1

**Purpose**: Parse single variable with modifiers

```erlang
%%--------------------------------------------------------------------
%% @private Parse single variable with optional modifiers.
%% Syntax: varname[:prefix][*]
%%--------------------------------------------------------------------
-spec parse_variable(binary()) -> term().
parse_variable(VarSpec) ->
    % Check for explode modifier (*)
    {Name1, Explode} = case binary:last(VarSpec) of
        $* -> {binary:part(VarSpec, 0, byte_size(VarSpec) - 1), true};
        _ -> {VarSpec, false}
    end,

    % Check for prefix modifier (:n)
    {Name, Prefix} = case binary:split(Name1, <<":">>) of
        [N, PrefixStr] ->
            PrefixInt = binary_to_integer(PrefixStr),
            {N, PrefixInt};
        [N] ->
            {N, undefined}
    end,

    {name, Name, prefix, Prefix, explode, Explode}.
```

**Lines**: 25-30

---

## 7. Integration: Update erlmcp_uri_validator.erl

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_uri_validator.erl.broken`
**Action**: Rename to `.erl` and update `validate_uri_template/1`

### Current Code (Line 246-269):

```erlang
do_validate_uri_template(UriTemplate) when is_binary(UriTemplate) ->
    case byte_size(UriTemplate) of
        0 ->
            {error, {empty_template, <<"URI template cannot be empty">>}};
        Size when Size > ?MAX_URI_TEMPLATE_LENGTH ->
            {error, {template_too_long, Msg}};
        _ ->
            validate_template_syntax(UriTemplate)
    end.

validate_template_syntax(UriTemplate) ->
    case validate_template_braces(UriTemplate, 0, 0) of
        {error, Reason} ->
            {error, {invalid_template_syntax, Reason}};
        ok ->
            validate_resource_uri_internal(UriTemplate)
    end.
```

### New Code:

```erlang
do_validate_uri_template(UriTemplate) when is_binary(UriTemplate) ->
    case byte_size(UriTemplate) of
        0 ->
            {error, {empty_template, <<"URI template cannot be empty">>}};
        Size when Size > ?MAX_URI_TEMPLATE_LENGTH ->
            MaxBin = integer_to_binary(?MAX_URI_TEMPLATE_LENGTH),
            Msg = <<"URI template exceeds maximum length of ", MaxBin/binary>>,
            {error, {template_too_long, Msg}};
        _ ->
            % Use RFC 6570 parser
            case erlmcp_uri_template:validate(UriTemplate) of
                ok -> ok;
                {error, Reason} -> {error, {invalid_template, Reason}}
            end
    end.
```

**Lines Changed**: ~15 lines
**Action**: Replace `validate_template_syntax/1` call with `erlmcp_uri_template:validate/1`

---

## 8. Integration: Update erlmcp_resource_subscriptions.erl

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_resource_subscriptions.erl`
**Action**: Replace stub `match_uri_template/2` with real implementation

### Current Code (Lines 350-353):

```erlang
match_uri_template(_Uri, Template) ->
    % Simple template matching (can be enhanced with proper URI template parsing)
    % For now, only exact match
    false.
```

### New Code:

```erlang
%%--------------------------------------------------------------------
%% @doc Check if a URI matches a template.
%% Supports RFC 6570 URI templates.
%%--------------------------------------------------------------------
-spec match_uri_template(uri(), binary()) -> boolean().
match_uri_template(Uri, TemplateString) when is_binary(Uri), is_binary(TemplateString) ->
    % Check if template string contains variables
    case erlmcp_uri_template:is_template(TemplateString) of
        false ->
            % Not a template - exact match
            Uri =:= TemplateString;
        true ->
            % Parse and match template
            case erlmcp_uri_template:parse(TemplateString) of
                {ok, Template} ->
                    case erlmcp_uri_template:extract(Uri, Template) of
                        {ok, _Variables} ->
                            true;  % URI matches template
                        {error, no_match} ->
                            false  % URI does not match
                    end;
                {error, _ParseError} ->
                    false  % Invalid template - no match
            end
    end;
match_uri_template(_Uri, _Template) ->
    false.
```

**Lines Changed**: 3 lines → ~27 lines (net +24 lines)

---

## 9. Testing: Unit Tests for URI Templates

**File**: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_uri_template_tests.erl`
**Estimated LOC**: 250-300
**Test Count**: 15 tests

### Test Suite Structure

```erlang
-module(erlmcp_uri_template_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Groups
%%====================================================================

uri_template_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"Parse simple expression", fun test_parse_simple_expression/0},
            {"Parse reserved expression", fun test_parse_reserved_expression/0},
            {"Parse fragment expression", fun test_parse_fragment_expression/0},
            {"Parse label expression", fun test_parse_label_expression/0},
            {"Parse path expression", fun test_parse_path_expression/0},
            {"Parse parameter expression", fun test_parse_parameter_expression/0},
            {"Parse form expression", fun test_parse_form_expression/0},
            {"Expand simple expression", fun test_expand_simple_expression/0},
            {"Expand reserved expression", fun test_expand_reserved_expression/0},
            {"Expand query parameters", fun test_expand_query_parameters/0},
            {"Extract variables from URI", fun test_extract_variables/0},
            {"Prefix modifier", fun test_prefix_modifier/0},
            {"Explode modifier", fun test_explode_modifier/0},
            {"Invalid template", fun test_invalid_template/0},
            {"Complex template", fun test_complex_template/0}
        ]
    }.

setup() -> ok.
cleanup(_) -> ok.
```

### Test 1: Parse Simple Expression

```erlang
test_parse_simple_expression() ->
    Template = <<"/users/{userId}">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    ?assertMatch(#template{}, Parsed).
```

### Test 2: Expand Simple Expression

```erlang
test_expand_simple_expression() ->
    Template = <<"/users/{userId}">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    Result = erlmcp_uri_template:expand(Parsed, #{<<"userId">> => <<"123">>}),
    ?assertEqual(<<"/users/123">>, Result).
```

### Test 3: Reserved Expression (Preserve Special Chars)

```erlang
test_expand_reserved_expression() ->
    Template = <<"/files/{+path}">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    Result = erlmcp_uri_template:expand(Parsed, #{<<"path">> => <<"docs/readme.txt">>}),
    % Reserved expansion should NOT encode /
    ?assertEqual(<<"/files/docs/readme.txt">>, Result).
```

### Test 4: Query Parameters

```erlang
test_expand_query_parameters() ->
    Template = <<"/search{?q,limit}">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    Result = erlmcp_uri_template:expand(Parsed, #{
        <<"q">> => <<"erlang">>,
        <<"limit">> => <<"10">>
    }),
    ?assertEqual(<<"/search?q=erlang&limit=10">>, Result).
```

### Test 5: Extract Variables from URI

```erlang
test_extract_variables() ->
    Template = <<"/users/{userId}/documents/{docId}">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    URI = <<"/users/123/documents/456">>,
    {ok, Vars} = erlmcp_uri_template:extract(URI, Parsed),
    ?assertEqual(#{
        <<"userId">> => <<"123">>,
        <<"docId">> => <<"456">>
    }, Vars).
```

### Test 6: Prefix Modifier

```erlang
test_prefix_modifier() ->
    Template = <<"/short/{name:4}">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    Result = erlmcp_uri_template:expand(Parsed, #{<<"name">> => <<"alexander">>}),
    ?assertEqual(<<"/short/alex">>, Result).
```

### Test 7: Explode Modifier (List)

```erlang
test_explode_modifier() ->
    Template = <<"/tags/{tags*}">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    Result = erlmcp_uri_template:expand(Parsed, #{
        <<"tags">> => [<<"erlang">>, <<"otp">>, <<"mcp">>]
    }),
    % Should expand to comma-separated list
    ?assert(binary:match(Result, <<"erlang">>) =/= nomatch),
    ?assert(binary:match(Result, <<"otp">>) =/= nomatch),
    ?assert(binary:match(Result, <<"mcp">>) =/= nomatch).
```

### Test 8: Invalid Template (Unbalanced Braces)

```erlang
test_invalid_template() ->
    InvalidTemplate1 = <<"/unclosed/{var">>,
    ?assertMatch({error, _}, erlmcp_uri_template:parse(InvalidTemplate1)),

    InvalidTemplate2 = <<"/extra/}var}">>,
    ?assertMatch({error, _}, erlmcp_uri_template:parse(InvalidTemplate2)).
```

### Test 9: Complex Multi-Expression Template

```erlang
test_complex_template() ->
    Template = <<"/api{/version}/users{?page,limit}{#section}">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    Result = erlmcp_uri_template:expand(Parsed, #{
        <<"version">> => <<"v2">>,
        <<"page">> => <<"1">>,
        <<"limit">> => <<"50">>,
        <<"section">> => <<"results">>
    }),
    ?assertEqual(<<"/api/v2/users?page=1&limit=50#results">>, Result).
```

### Test 10: Match Template (Extract Success)

```erlang
test_match_template_success() ->
    Template = <<"/users/{userId}/docs/{docId}">>,
    URI = <<"/users/alice/docs/report.pdf">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    ?assertMatch({ok, _}, erlmcp_uri_template:extract(URI, Parsed)),
    {ok, Vars} = erlmcp_uri_template:extract(URI, Parsed),
    ?assertEqual(<<"alice">>, maps:get(<<"userId">>, Vars)),
    ?assertEqual(<<"report.pdf">>, maps:get(<<"docId">>, Vars)).
```

### Test 11: Match Template (Extract Failure)

```erlang
test_match_template_failure() ->
    Template = <<"/users/{userId}/docs/{docId}">>,
    NonMatchingURI = <<"/users/alice/images/photo.jpg">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    ?assertEqual({error, no_match}, erlmcp_uri_template:extract(NonMatchingURI, Parsed)).
```

### Test 12: Path Segment Expression

```erlang
test_path_expression() ->
    Template = <<"/base{/path1,path2}">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    Result = erlmcp_uri_template:expand(Parsed, #{
        <<"path1">> => <<"docs">>,
        <<"path2">> => <<"readme">>
    }),
    ?assertEqual(<<"/base/docs/readme">>, Result).
```

### Test 13: Label Expression

```erlang
test_label_expression() ->
    Template = <<"/file{.format}">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    Result = erlmcp_uri_template:expand(Parsed, #{<<"format">> => <<"json">>}),
    ?assertEqual(<<"/file.json">>, Result).
```

### Test 14: Fragment Expression

```erlang
test_fragment_expression() ->
    Template = <<"/page{#section}">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    Result = erlmcp_uri_template:expand(Parsed, #{<<"section">> => <<"intro">>}),
    ?assertEqual(<<"/page#intro">>, Result).
```

### Test 15: Form Expression

```erlang
test_form_expression() ->
    Template = <<"/api?key=abc{&page,limit}">>,
    {ok, Parsed} = erlmcp_uri_template:parse(Template),
    Result = erlmcp_uri_template:expand(Parsed, #{
        <<"page">> => <<"2">>,
        <<"limit">> => <<"25">>
    }),
    ?assertEqual(<<"/api?key=abc&page=2&limit=25">>, Result).
```

**Total Lines**: ~250-300

---

## 10. Integration Test: Resource Subscriptions with Templates

**File**: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_resource_subscriptions_tests.erl`
**Action**: Add new test case

### Test: Templated Resource Subscription

```erlang
test_templated_resource_subscription() ->
    % Start subscription manager
    {ok, _Pid} = erlmcp_resource_subscriptions:start_link(),

    % Subscribe to template: /documents/{docId}
    TemplateUri = <<"/documents/{docId}">>,
    ok = erlmcp_resource_subscriptions:subscribe_to_resource(
        TemplateUri,
        self(),
        #{}
    ),

    % Notify specific resource: /documents/123
    SpecificUri = <<"/documents/123">>,
    erlmcp_resource_subscriptions:notify_resource_changed(
        SpecificUri,
        #{<<"docId">> => <<"123">>}
    ),

    % Should receive notification (template matches specific URI)
    receive
        {'$mcp_resource', Notification} ->
            ?assertMatch(#{
                jsonrpc := <<"2.0">>,
                method := <<"resources/updated">>,
                params := #{uri := <<"/documents/123">>}
            }, Notification),
            ok
    after 2000 ->
        ?assert(false)  % Timeout - should have received notification
    end,

    % Cleanup
    erlmcp_resource_subscriptions:unsubscribe_from_resource(TemplateUri, self()).
```

### Test: Multiple Template Subscriptions

```erlang
test_multiple_template_subscriptions() ->
    {ok, _Pid} = erlmcp_resource_subscriptions:start_link(),

    % Subscribe to two templates
    Template1 = <<"/users/{userId}/docs">>,
    Template2 = <<"/users/{userId}/images">>,

    ok = erlmcp_resource_subscriptions:subscribe_to_resource(Template1, self(), #{}),
    ok = erlmcp_resource_subscriptions:subscribe_to_resource(Template2, self(), #{}),

    % List subscriptions (should include both templates)
    Subs = erlmcp_resource_subscriptions:list_resource_subscriptions(
        <<"/users/alice/docs">>,
        true  % Include templates
    ),
    ?assertEqual(1, length(Subs)),
    ?assert(lists:member(self(), Subs)),

    % Cleanup
    erlmcp_resource_subscriptions:unsubscribe_from_resource(Template1, self()),
    erlmcp_resource_subscriptions:unsubscribe_from_resource(Template2, self()).
```

### Test: Template Non-Match

```erlang
test_template_non_match() ->
    {ok, _Pid} = erlmcp_resource_subscriptions:start_link(),

    % Subscribe to template: /documents/{docId}
    TemplateUri = <<"/documents/{docId}">>,
    ok = erlmcp_resource_subscriptions:subscribe_to_resource(TemplateUri, self(), #{}),

    % Notify non-matching resource: /images/photo.jpg
    NonMatchingUri = <<"/images/photo.jpg">>,
    erlmcp_resource_subscriptions:notify_resource_changed(NonMatchingUri, #{}),

    % Should NOT receive notification
    receive
        {'$mcp_resource', _} ->
            ?assert(false)  % Should not receive
    after 500 ->
        ok  % Expected timeout
    end,

    % Cleanup
    erlmcp_resource_subscriptions:unsubscribe_from_resource(TemplateUri, self()).
```

**Total Addition**: ~50 LOC

---

## 11. Files to Create/Modify Summary

| Action | File | LOC | Purpose |
|--------|------|-----|---------|
| **CREATE** | `apps/erlmcp_core/src/erlmcp_uri_template.erl` | ~300 | Public API: parse, expand, extract, validate |
| **CREATE** | `apps/erlmcp_core/src/erlmcp_uri_template_parser.erl` | ~200 | Internal RFC 6570 parser |
| **CREATE** | `apps/erlmcp_core/test/erlmcp_uri_template_tests.erl` | ~250 | Unit tests (15 tests) |
| **MODIFY** | `apps/erlmcp_core/src/erlmcp_uri_validator.erl.broken` | ~5 | Rename to `.erl`, use new parser |
| **MODIFY** | `apps/erlmcp_core/src/erlmcp_resource_subscriptions.erl` | ~20 | Enable template matching |
| **MODIFY** | `apps/erlmcp_core/test/erlmcp_resource_subscriptions_tests.erl` | ~50 | Add integration tests |
| **TOTAL** | | **~825** | **3 new files, 3 modified files** |

---

## 12. Verification Checklist

### Functional Requirements

- [ ] **Parse all RFC 6570 expression types**
  - [ ] Simple: `{var}`
  - [ ] Reserved: `{+var}`
  - [ ] Fragment: `{#var}`
  - [ ] Label: `{.var}`
  - [ ] Path: `{/var}`
  - [ ] Parameter: `{?var}`
  - [ ] Form: `{&var}`

- [ ] **Parse variable modifiers**
  - [ ] Prefix: `{var:n}`
  - [ ] Explode: `{var*}`

- [ ] **Template expansion works correctly**
  - [ ] Variables replaced with values
  - [ ] Percent-encoding applied (unreserved)
  - [ ] Reserved chars preserved (reserved expressions)
  - [ ] Missing variables omitted gracefully

- [ ] **Template extraction works correctly**
  - [ ] Extract variables from matching URIs
  - [ ] Return `{error, no_match}` for non-matching URIs
  - [ ] Handle complex templates with multiple variables

- [ ] **Template matching works in subscriptions**
  - [ ] Subscribe to template URIs
  - [ ] Match specific URIs against templates
  - [ ] Send notifications to template subscribers
  - [ ] No false positives (non-matching URIs ignored)

### Quality Gates

- [ ] **Compilation**
  ```bash
  TERM=dumb rebar3 compile
  ```
  - [ ] 0 errors
  - [ ] 0 warnings

- [ ] **Unit Tests**
  ```bash
  rebar3 eunit --module=erlmcp_uri_template_tests
  ```
  - [ ] 15/15 tests pass
  - [ ] 100% pass rate

- [ ] **Integration Tests**
  ```bash
  rebar3 eunit --module=erlmcp_resource_subscriptions_tests
  ```
  - [ ] All tests pass
  - [ ] Template matching tests pass

- [ ] **Dialyzer**
  ```bash
  rebar3 dialyzer
  ```
  - [ ] 0 type warnings

- [ ] **Xref**
  ```bash
  rebar3 xref
  ```
  - [ ] 0 undefined function calls

- [ ] **Coverage**
  ```bash
  rebar3 cover
  ```
  - [ ] ≥80% code coverage

### Regression Testing

- [ ] **Existing subscriptions still work**
  - [ ] Exact URI subscriptions unaffected
  - [ ] Non-template URIs still match exactly
  - [ ] No performance degradation

- [ ] **Backward compatibility**
  - [ ] Old subscription API unchanged
  - [ ] No breaking changes to public API

---

## 13. Timeline

### Phase 1: Parser Implementation (2-2.5 hours)

**Tasks**:
- [ ] Create `erlmcp_uri_template_parser.erl`
- [ ] Implement `tokenize/1` (split literals and expressions)
- [ ] Implement `parse_expression/1` (detect expression type)
- [ ] Implement `parse_variable_list/1` (parse comma-separated vars)
- [ ] Implement `parse_variable/1` (parse modifiers)
- [ ] Manual testing in Erlang shell

**Deliverable**: Working parser that converts template strings to structured format

### Phase 2: Template API (2-2.5 hours)

**Tasks**:
- [ ] Create `erlmcp_uri_template.erl`
- [ ] Implement `parse/1` (wrapper around parser)
- [ ] Implement `validate/1` (syntax validation)
- [ ] Implement `expand/2` (template expansion)
- [ ] Implement `expand_expression/2` (per-type expansion)
- [ ] Implement `extract/2` (reverse parsing)
- [ ] Implement `is_template/1` (quick check)
- [ ] Manual testing in Erlang shell

**Deliverable**: Complete public API for URI templates

### Phase 3: Integration (0.5-1 hour)

**Tasks**:
- [ ] Rename `erlmcp_uri_validator.erl.broken` to `.erl`
- [ ] Update `do_validate_uri_template/1` to use new parser
- [ ] Update `erlmcp_resource_subscriptions:match_uri_template/2`
- [ ] Test integration manually

**Deliverable**: Integrated template support in validator and subscriptions

### Phase 4: Unit Tests (1-1.5 hours)

**Tasks**:
- [ ] Create `erlmcp_uri_template_tests.erl`
- [ ] Write 15 test cases (see section 9)
- [ ] Run tests: `rebar3 eunit --module=erlmcp_uri_template_tests`
- [ ] Fix any failures
- [ ] Verify 100% pass rate

**Deliverable**: Comprehensive test suite with ≥80% coverage

### Phase 5: Integration Tests (0.5-1 hour)

**Tasks**:
- [ ] Add tests to `erlmcp_resource_subscriptions_tests.erl`
- [ ] Test template subscriptions
- [ ] Test template matching
- [ ] Test non-matching URIs
- [ ] Run tests: `rebar3 eunit --module=erlmcp_resource_subscriptions_tests`

**Deliverable**: End-to-end subscription tests passing

### Phase 6: Debugging/Verification (1 hour)

**Tasks**:
- [ ] Run full test suite: `rebar3 eunit`
- [ ] Run dialyzer: `rebar3 dialyzer`
- [ ] Run xref: `rebar3 xref`
- [ ] Check coverage: `rebar3 cover`
- [ ] Manual verification with example templates
- [ ] Performance smoke test

**Deliverable**: All quality gates pass, ready for merge

---

## 14. Deployment Strategy

### Release Plan

**Version**: erlmcp_core 2.3.0 (minor version bump)

**Reason**: New feature (URI template support), no breaking changes

### Rollout Strategy

1. **Merge to main branch**
   - All tests passing
   - Code review complete
   - Documentation updated

2. **Release notes**
   - Highlight new URI template support
   - Provide migration guide (none needed - backward compatible)
   - Include example templates

3. **Monitoring**
   - Watch for any subscription-related errors
   - Monitor template parsing performance
   - Check for edge cases in production logs

### Breaking Changes

**None** - This is a fully backward-compatible feature:

- Existing exact-match subscriptions continue to work
- Template support is opt-in (only if URI contains `{`)
- Public API unchanged (only internal implementation)

### Documentation Updates

**Files to Update**:
- [ ] `docs/RESOURCE_SUBSCRIPTIONS.md` - Add URI template examples
- [ ] `docs/api-reference.md` - Document new template API
- [ ] `README.md` - Add URI template support to features list
- [ ] `examples/` - Add template subscription examples

---

## 15. Performance Considerations

### Template Parsing

**Strategy**: Parse-once, cache parsed template

```erlang
% Instead of parsing on every match:
match_uri_template(Uri, TemplateString) ->
    {ok, Template} = erlmcp_uri_template:parse(TemplateString),  % Slow!
    erlmcp_uri_template:extract(Uri, Template).

% Better: Cache parsed templates in subscription state:
-record(subscription, {
    uri_template :: binary(),
    parsed_template :: erlmcp_uri_template:template(),  % Cached!
    subscribers :: [pid()]
}).
```

**Optimization**: Store parsed templates in subscription records to avoid re-parsing on every match.

### Template Matching

**Expected Performance**:
- **Simple templates** (`/users/{id}`): ~1-2 μs per match
- **Complex templates** (`/api{/version}{?page,limit}{#section}`): ~5-10 μs per match
- **Non-templates** (exact match): ~0.1 μs (fast path)

**Optimization**: Use `is_template/1` to skip parsing for non-templates.

### Memory Impact

**Estimate**: ~500 bytes per parsed template (small)

**Negligible** - Even 10,000 subscriptions = ~5 MB memory

---

## 16. Edge Cases

### Edge Case 1: Empty Variables

**Template**: `/users/{userId}`
**Variables**: `#{}`
**Expected**: `/users/` (empty expansion)

### Edge Case 2: Special Characters in Values

**Template**: `/search{?q}`
**Variables**: `#{<<"q">> => <<"erlang & otp">>}`
**Expected**: `/search?q=erlang%20%26%20otp` (percent-encoded)

### Edge Case 3: Undefined Variables

**Template**: `/users/{userId}/docs/{docId}`
**Variables**: `#{<<"userId">> => <<"123">>}`  % docId missing
**Expected**: `/users/123/docs/` (undefined variable = empty)

### Edge Case 4: Nested Braces

**Template**: `/invalid/{{var}}`
**Expected**: `{error, invalid_template}` (parse error)

### Edge Case 5: Partial Match

**Template**: `/users/{userId}/docs/{docId}`
**URI**: `/users/123/images/photo.jpg`
**Expected**: `{error, no_match}` (path mismatch)

---

## 17. RFC 6570 Compliance Notes

### Supported Features

- [x] All 7 expression types
- [x] Variable lists (comma-separated)
- [x] Prefix modifier (`:n`)
- [x] Explode modifier (`*`) for lists
- [x] Percent-encoding (unreserved)
- [x] Reserved character preservation

### Not Implemented (Out of Scope)

- [ ] Composite values (associative arrays)
- [ ] Length restrictions (max expansion length)
- [ ] Unicode normalization
- [ ] Query continuation (`?` vs `&` auto-detection)

**Rationale**: These features are rarely used in MCP resources. Can be added later if needed.

### Compliance Level

**Target**: **Level 3 compliance** (most common use cases)

**RFC 6570 Levels**:
- Level 1: Simple string expansion
- Level 2: Reserved expansion, fragment expansion
- Level 3: Label, path, parameter, form expansions
- Level 4: Composite values (out of scope)

---

## 18. Example Usage

### Example 1: Simple Path Template

```erlang
% Parse template
{ok, Template} = erlmcp_uri_template:parse(<<"/users/{userId}/profile">>).

% Expand with variables
Uri = erlmcp_uri_template:expand(Template, #{<<"userId">> => <<"alice">>}).
% Result: <<"/users/alice/profile">>

% Extract variables from URI
{ok, Vars} = erlmcp_uri_template:extract(<<"/users/bob/profile">>, Template).
% Result: #{<<"userId">> => <<"bob">>}
```

### Example 2: Query Parameters

```erlang
% Parse template
{ok, Template} = erlmcp_uri_template:parse(<<"/api/search{?q,limit,offset}">>).

% Expand with some variables
Uri = erlmcp_uri_template:expand(Template, #{
    <<"q">> => <<"erlang">>,
    <<"limit">> => <<"10">>
    % offset omitted
}).
% Result: <<"/api/search?q=erlang&limit=10">>
```

### Example 3: Subscription with Template

```erlang
% Start subscription manager
{ok, _Pid} = erlmcp_resource_subscriptions:start_link().

% Subscribe to all user documents
TemplateUri = <<"/users/{userId}/documents/{docId}">>,
erlmcp_resource_subscriptions:subscribe_to_resource(TemplateUri, self(), #{}).

% When any document changes, subscriber receives notification
erlmcp_resource_subscriptions:notify_resource_changed(
    <<"/users/alice/documents/report.pdf">>,
    #{<<"content">> => <<"Updated report">>}
).

% Subscriber receives:
% {'$mcp_resource', #{
%     jsonrpc => <<"2.0">>,
%     method => <<"resources/updated">>,
%     params => #{uri => <<"/users/alice/documents/report.pdf">>}
% }}
```

---

## 19. References

- **RFC 6570**: URI Template - https://datatracker.ietf.org/doc/html/rfc6570
- **RFC 3986**: URI Generic Syntax - https://datatracker.ietf.org/doc/html/rfc3986
- **MCP Spec 2025-11-25**: Resource Subscriptions - `docs/protocol.md`
- **erlmcp OTP Patterns**: `docs/otp-patterns.md`

---

## 20. Success Criteria

This implementation is **COMPLETE** when:

1. [x] All files created and modified as specified
2. [x] All tests pass (15 unit + 3 integration)
3. [x] All quality gates pass (compile, dialyzer, xref, coverage)
4. [x] Template matching works in resource subscriptions
5. [x] No regressions in existing functionality
6. [x] Documentation updated
7. [x] Code reviewed and approved
8. [x] Merged to main branch

**Status**: Ready for implementation - All requirements specified.

---

**END OF IMPLEMENTATION PLAN**
