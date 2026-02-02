# Elicitation Form API Design Specification
**Version:** 1.0.0
**Date:** 2026-02-02
**MCP Spec:** 2025-11-25 (SEP-1036, SEP-1330, SEP-1034)
**Status:** Design Phase

---

## Executive Summary

This document specifies a comprehensive form-based elicitation API for erlmcp, bringing compliance from 1% to 100% for the MCP Elicitation experimental feature. The design enables rich, type-safe form interactions with validation, error handling, and re-prompting capabilities.

**Key Features:**
- 8 form field types (text, number, boolean, select, multi-select, URL, file, date)
- JSON Schema 2020-12 based validation
- Enhanced enums with titled/untitled modes (SEP-1330)
- Default values for all primitive types (SEP-1034)
- SSRF-protected URL mode (SEP-1036)
- Re-prompting workflow for validation errors
- Timeout and abandonment handling
- Full test coverage with Chicago School TDD

---

## 1. Form Definition Language

### 1.1 Overview

Forms are defined using JSON Schema 2020-12 with MCP-specific extensions. Each form is a collection of fields with validation rules, rendering hints, and response handlers.

### 1.2 Form Schema Structure

```erlang
-type form_definition() :: #{
    id := binary(),                    % Unique form ID
    title := binary(),                 % Form title (display)
    description => binary(),           % Optional description
    fields := [field_definition()],   % Ordered list of fields
    validation => validation_rules(), % Form-level validation
    metadata => map(),                % Custom metadata
    timeout => pos_integer(),         % Timeout in milliseconds
    allow_partial => boolean()        % Allow partial submissions
}.
```

**Example:**
```json
{
  "id": "api_key_form",
  "title": "API Key Configuration",
  "description": "Please provide your API credentials",
  "fields": [
    {
      "id": "api_key",
      "type": "text",
      "label": "API Key",
      "required": true,
      "validation": {
        "minLength": 32,
        "maxLength": 64,
        "pattern": "^sk-[a-zA-Z0-9]{40,}$"
      },
      "rendering": {
        "placeholder": "sk-...",
        "inputType": "password",
        "helpText": "Found in your account settings"
      }
    },
    {
      "id": "region",
      "type": "select",
      "label": "Region",
      "required": true,
      "options": [
        {"value": "us-east-1", "title": "US East (Virginia)"},
        {"value": "us-west-2", "title": "US West (Oregon)"},
        {"value": "eu-west-1", "title": "Europe (Ireland)"}
      ],
      "default": "us-east-1"
    }
  ],
  "timeout": 300000
}
```

---

## 2. Form Field Types

### 2.1 Type Taxonomy

```erlang
-type field_type() ::
    text | number | boolean | date |
    select | multi_select | url | file.

-type field_definition() :: #{
    id := binary(),              % Unique field ID
    type := field_type(),        % Field type
    label := binary(),           % Display label
    description => binary(),     % Optional description
    required => boolean(),       % Required field (default: false)
    default => term(),           % Default value (SEP-1034)
    validation => map(),         % Type-specific validation
    rendering => map(),          % Rendering hints
    dependencies => [binary()]   % Dependent field IDs
}.
```

### 2.2 Text Field

**Type:** `text`
**Value:** Binary string
**Default Support:** Yes (SEP-1034)

```json
{
  "id": "username",
  "type": "text",
  "label": "Username",
  "required": true,
  "default": "",
  "validation": {
    "minLength": 3,
    "maxLength": 32,
    "pattern": "^[a-zA-Z0-9_-]+$"
  },
  "rendering": {
    "placeholder": "Enter username",
    "inputType": "text",
    "helpText": "3-32 alphanumeric characters",
    "autocomplete": "username"
  }
}
```

**Validation Rules:**
- `minLength` (integer): Minimum string length
- `maxLength` (integer): Maximum string length
- `pattern` (regex): Regular expression pattern
- `format` (string): JSON Schema format (email, uri, uuid, etc.)

**Rendering Hints:**
- `placeholder`: Placeholder text
- `inputType`: HTML input type (text, password, email, tel)
- `helpText`: Help text below field
- `autocomplete`: HTML autocomplete attribute

### 2.3 Number Field

**Type:** `number`
**Value:** Integer or float
**Default Support:** Yes (SEP-1034)

```json
{
  "id": "port",
  "type": "number",
  "label": "Port Number",
  "required": true,
  "default": 8080,
  "validation": {
    "minimum": 1024,
    "maximum": 65535,
    "multipleOf": 1
  },
  "rendering": {
    "placeholder": "8080",
    "step": 1,
    "helpText": "Valid port range: 1024-65535"
  }
}
```

**Validation Rules:**
- `minimum` (number): Minimum value (inclusive)
- `maximum` (number): Maximum value (inclusive)
- `exclusiveMinimum` (number): Minimum value (exclusive)
- `exclusiveMaximum` (number): Maximum value (exclusive)
- `multipleOf` (number): Value must be multiple of this

**Rendering Hints:**
- `placeholder`: Placeholder text
- `step`: Increment/decrement step
- `helpText`: Help text

### 2.4 Boolean Field

**Type:** `boolean`
**Value:** `true` or `false`
**Default Support:** Yes (existing + SEP-1034)

```json
{
  "id": "enable_ssl",
  "type": "boolean",
  "label": "Enable SSL/TLS",
  "required": false,
  "default": true,
  "rendering": {
    "displayStyle": "checkbox",
    "helpText": "Encrypt connections with SSL/TLS"
  }
}
```

**Validation Rules:**
- None (boolean is inherently validated)

**Rendering Hints:**
- `displayStyle`: "checkbox", "toggle", "radio"
- `helpText`: Help text

### 2.5 Select Field (Single)

**Type:** `select`
**Value:** Single enum value
**Default Support:** Yes (SEP-1034)
**Enhanced Enums:** Yes (SEP-1330)

**Untitled Enum (values = labels):**
```json
{
  "id": "log_level",
  "type": "select",
  "label": "Log Level",
  "required": true,
  "default": "info",
  "options": ["debug", "info", "warning", "error", "critical"],
  "rendering": {
    "displayStyle": "dropdown",
    "helpText": "Logging verbosity level"
  }
}
```

**Titled Enum (separate display titles):**
```json
{
  "id": "region",
  "type": "select",
  "label": "AWS Region",
  "required": true,
  "default": "us-east-1",
  "options": [
    {"value": "us-east-1", "title": "US East (N. Virginia)"},
    {"value": "us-west-2", "title": "US West (Oregon)"},
    {"value": "eu-west-1", "title": "Europe (Ireland)"},
    {"value": "ap-southeast-1", "title": "Asia Pacific (Singapore)"}
  ],
  "rendering": {
    "displayStyle": "dropdown",
    "searchable": true,
    "helpText": "Select your preferred AWS region"
  }
}
```

**Validation Rules:**
- `enum` (array): Valid values (auto-generated from options)

**Rendering Hints:**
- `displayStyle`: "dropdown", "radio", "segmented"
- `searchable`: Enable search for long lists
- `helpText`: Help text

### 2.6 Multi-Select Field

**Type:** `multi_select`
**Value:** Array of enum values
**Default Support:** Yes (SEP-1034)
**Enhanced Enums:** Yes (SEP-1330)

```json
{
  "id": "features",
  "type": "multi_select",
  "label": "Enabled Features",
  "required": false,
  "default": ["logging", "metrics"],
  "options": [
    {"value": "logging", "title": "Application Logging"},
    {"value": "metrics", "title": "Performance Metrics"},
    {"value": "tracing", "title": "Distributed Tracing"},
    {"value": "profiling", "title": "CPU/Memory Profiling"}
  ],
  "validation": {
    "minItems": 1,
    "maxItems": 4
  },
  "rendering": {
    "displayStyle": "checkbox",
    "helpText": "Select one or more features to enable"
  }
}
```

**Validation Rules:**
- `enum` (array): Valid values
- `minItems` (integer): Minimum selections
- `maxItems` (integer): Maximum selections
- `uniqueItems` (boolean): No duplicates (always true)

**Rendering Hints:**
- `displayStyle`: "checkbox", "multiselect_dropdown", "chips"
- `helpText`: Help text

### 2.7 URL Field

**Type:** `url`
**Value:** Valid URL string
**Default Support:** Yes (SEP-1034)
**SSRF Protection:** Yes (SEP-1036)

```json
{
  "id": "webhook_url",
  "type": "url",
  "label": "Webhook URL",
  "required": true,
  "validation": {
    "format": "uri",
    "allowedSchemes": ["https"],
    "blockPrivateIPs": true,
    "blockLocalhost": true
  },
  "rendering": {
    "placeholder": "https://api.example.com/webhook",
    "helpText": "HTTPS URLs only, no private IPs"
  }
}
```

**Validation Rules:**
- `format`: "uri" (JSON Schema)
- `allowedSchemes`: Array of allowed URL schemes (http, https, ftp, etc.)
- `blockPrivateIPs`: Block private IP ranges (127.0.0.0/8, 10.0.0.0/8, etc.)
- `blockLocalhost`: Block localhost and loopback addresses
- `maxLength`: Maximum URL length

**Rendering Hints:**
- `placeholder`: Placeholder text
- `helpText`: Help text
- `testButton`: Show "Test URL" button

### 2.8 File Field

**Type:** `file`
**Value:** File metadata + content reference
**Default Support:** No (files cannot have defaults)

```json
{
  "id": "config_file",
  "type": "file",
  "label": "Configuration File",
  "required": true,
  "validation": {
    "maxSize": 10485760,
    "allowedMimeTypes": ["application/json", "text/yaml", "application/x-yaml"],
    "allowedExtensions": [".json", ".yaml", ".yml"]
  },
  "rendering": {
    "acceptAttribute": ".json,.yaml,.yml",
    "helpText": "Upload JSON or YAML configuration (max 10MB)",
    "dragAndDrop": true
  }
}
```

**Validation Rules:**
- `maxSize` (integer): Maximum file size in bytes
- `allowedMimeTypes` (array): Allowed MIME types
- `allowedExtensions` (array): Allowed file extensions

**Rendering Hints:**
- `acceptAttribute`: HTML accept attribute value
- `helpText`: Help text
- `dragAndDrop`: Enable drag-and-drop upload
- `preview`: Show file preview for images/text

### 2.9 Date Field

**Type:** `date`
**Value:** ISO 8601 date string
**Default Support:** Yes (SEP-1034)

```json
{
  "id": "start_date",
  "type": "date",
  "label": "Start Date",
  "required": true,
  "default": "2026-02-02",
  "validation": {
    "format": "date",
    "minimum": "2026-01-01",
    "maximum": "2026-12-31"
  },
  "rendering": {
    "helpText": "Select a date in 2026",
    "displayFormat": "YYYY-MM-DD",
    "enableTime": false
  }
}
```

**Validation Rules:**
- `format`: "date", "date-time" (JSON Schema)
- `minimum` (string): Minimum date (ISO 8601)
- `maximum` (string): Maximum date (ISO 8601)

**Rendering Hints:**
- `helpText`: Help text
- `displayFormat`: Date format string
- `enableTime`: Include time picker

---

## 3. Validation Rules and Constraints

### 3.1 Validation Architecture

```
┌─────────────────────────────────────────────────────────┐
│ erlmcp_elicitation_validator (gen_server)              │
├─────────────────────────────────────────────────────────┤
│ • Field-level validation (type, format, constraints)   │
│ • Form-level validation (dependencies, cross-field)    │
│ • Jesse integration (JSON Schema 2020-12)              │
│ • Error collection and formatting                      │
│ • Re-validation support                                │
└─────────────────────────────────────────────────────────┘
         │
         ├──► erlmcp_schema_cache (persistent_term)
         │    └──► Cached compiled schemas (~10ns access)
         │
         └──► jesse (JSON Schema validator)
              └──► jesse:validate_with_schema/3
```

### 3.2 Validation Levels

#### 3.2.1 Type Validation
Validates that the value matches the field type.

```erlang
-spec validate_field_type(field_type(), term()) -> ok | {error, type_mismatch}.
validate_field_type(text, Value) when is_binary(Value) -> ok;
validate_field_type(number, Value) when is_number(Value) -> ok;
validate_field_type(boolean, Value) when is_boolean(Value) -> ok;
validate_field_type(date, Value) -> validate_date_format(Value);
validate_field_type(select, Value) -> validate_enum_value(Value);
validate_field_type(multi_select, Value) when is_list(Value) -> ok;
validate_field_type(url, Value) -> validate_url(Value);
validate_field_type(file, Value) -> validate_file_metadata(Value);
validate_field_type(_, _) -> {error, type_mismatch}.
```

#### 3.2.2 Constraint Validation
Validates type-specific constraints (length, range, pattern).

**Text Constraints:**
- `minLength`, `maxLength`, `pattern`, `format`

**Number Constraints:**
- `minimum`, `maximum`, `exclusiveMinimum`, `exclusiveMaximum`, `multipleOf`

**Array Constraints (multi_select, file):**
- `minItems`, `maxItems`, `uniqueItems`

**URL Constraints:**
- `allowedSchemes`, `blockPrivateIPs`, `blockLocalhost`

#### 3.2.3 Form-level Validation
Cross-field validation and dependencies.

```erlang
-type form_validation() :: #{
    dependencies => dependency_rules(),
    custom_validators => [validator_fun()]
}.

-type dependency_rules() :: #{
    field_id() => #{
        condition := condition_expr(),
        action := show | hide | enable | disable | validate
    }
}.

%% Example: Show "other_reason" field only if "reason" = "other"
#{
    <<"other_reason">> => #{
        condition => {equals, <<"reason">>, <<"other">>},
        action => show
    }
}
```

### 3.3 Validation Error Format

Validation errors follow JSON Schema validation error format with MCP extensions.

```erlang
-type validation_error() :: #{
    field := binary(),           % Field ID
    code := error_code(),        % Error code (MCP or custom)
    message := binary(),         % Human-readable message
    constraint := atom(),        % Violated constraint (e.g., minLength)
    expected := term(),          % Expected value/constraint
    actual := term(),            % Actual value
    path := [binary()]          % JSON path to field
}.
```

**Example:**
```json
{
  "field": "api_key",
  "code": -32007,
  "message": "API key must be at least 32 characters",
  "constraint": "minLength",
  "expected": 32,
  "actual": 20,
  "path": ["api_key"]
}
```

### 3.4 Jesse Integration

```erlang
%% Compile form schema to Jesse schema
-spec compile_form_schema(form_definition()) -> jesse:json_schema().
compile_form_schema(#{fields := Fields} = Form) ->
    Properties = maps:from_list([
        {FieldId, field_to_json_schema(Field)}
        || #{id := FieldId} = Field <- Fields
    ]),
    Required = [Id || #{id := Id, required := true} <- Fields],

    #{
        <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
        <<"type">> => <<"object">>,
        <<"properties">> => Properties,
        <<"required">> => Required,
        <<"additionalProperties">> => false
    }.

%% Validate form response using Jesse
-spec validate_form_response(form_definition(), map()) ->
    ok | {error, [validation_error()]}.
validate_form_response(FormDef, Response) ->
    Schema = compile_form_schema(FormDef),

    %% Try cache first
    CacheKey = form_schema_cache_key(FormDef),
    CompiledSchema = case erlmcp_schema_cache:get_schema(CacheKey) of
        {ok, Cached} -> Cached;
        {error, not_found} ->
            Compiled = jesse:compile(Schema),
            erlmcp_schema_cache:cache_schema(CacheKey, Compiled),
            Compiled
    end,

    %% Validate with Jesse
    case jesse:validate_with_schema(CompiledSchema, Response, [{allowed_errors, infinity}]) of
        {ok, _} -> ok;
        {error, Errors} -> {error, format_jesse_errors(Errors)}
    end.
```

---

## 4. Rendering Hints for Clients

### 4.1 Overview

Rendering hints guide client implementations on how to display forms, but clients are free to adapt based on their capabilities.

### 4.2 Rendering Hint Types

```erlang
-type rendering_hints() :: #{
    %% Common hints
    placeholder => binary(),        % Placeholder text
    helpText => binary(),           % Help text below field
    displayStyle => display_style(), % Display style variant
    readonly => boolean(),          % Read-only field
    hidden => boolean(),            % Hidden field
    autocomplete => binary(),       % HTML autocomplete attribute

    %% Layout hints
    gridColumn => pos_integer(),    % Grid column span
    gridRow => pos_integer(),       % Grid row span
    breakAfter => boolean(),        % Force line break after

    %% Input-specific hints
    inputType => input_type(),      % HTML input type
    step => number(),               % Number step increment
    rows => pos_integer(),          % Textarea rows

    %% Advanced hints
    conditionalDisplay => condition_expr(), % Show/hide condition
    validationTrigger => on_change | on_blur | on_submit,
    debounceMs => pos_integer()    % Debounce validation (ms)
}.

-type display_style() ::
    dropdown | radio | checkbox | toggle | segmented |
    multiselect_dropdown | chips | slider | text | password | email | tel.

-type input_type() ::
    text | password | email | tel | url | number | date | time | datetime_local.
```

### 4.3 Conditional Display

```json
{
  "id": "smtp_port",
  "type": "number",
  "label": "SMTP Port",
  "rendering": {
    "conditionalDisplay": {
      "operator": "equals",
      "field": "use_smtp",
      "value": true
    }
  }
}
```

### 4.4 Accessibility Hints

```erlang
-type accessibility_hints() :: #{
    ariaLabel => binary(),          % ARIA label
    ariaDescribedBy => binary(),    % ARIA described-by
    tabIndex => integer(),          % Tab order
    required => boolean(),          % ARIA required
    invalid => boolean()            % ARIA invalid
}.
```

---

## 5. Response Collection and Parsing

### 5.1 Form Submission Flow

```
Client                           Server
  │                                │
  │ 1. elicitation/create         │
  │ ──────────────────────────► │
  │                                │
  │ 2. {id, form_definition}      │
  │ ◄────────────────────────── │
  │                                │
  │ 3. User fills form            │
  │                                │
  │ 4. elicitation/submit         │
  │    {id, responses}            │
  │ ──────────────────────────► │
  │                                │
  │ 5a. Validation errors         │
  │ ◄────────────────────────── │
  │                                │
  │ 6. User corrects errors       │
  │                                │
  │ 7. elicitation/submit         │
  │    {id, responses}            │
  │ ──────────────────────────► │
  │                                │
  │ 5b. Success                   │
  │ ◄────────────────────────── │
  │                                │
  │ 8. notifications/elicitation/ │
  │    complete {id, result}      │
  │ ◄────────────────────────── │
```

### 5.2 Response Format

```erlang
-type form_response() :: #{
    form_id := binary(),           % Form ID
    responses := field_responses(), % Field responses
    metadata => #{                 % Optional metadata
        submitted_at => integer(), % Timestamp
        user_agent => binary(),    % Client info
        session_id => binary()     % Session ID
    }
}.

-type field_responses() :: #{
    field_id() => field_value()
}.

-type field_value() ::
    binary() |           % text, url, date
    number() |           % number
    boolean() |          % boolean
    binary() |           % select (single value)
    [binary()] |         % multi_select
    file_upload().       % file

-type file_upload() :: #{
    filename := binary(),
    content_type := binary(),
    size := non_neg_integer(),
    content := binary() | file_reference()
}.

-type file_reference() :: #{
    type := url | s3 | local,
    location := binary()
}.
```

**Example Response:**
```json
{
  "form_id": "api_key_form",
  "responses": {
    "api_key": "sk-abc123def456...",
    "region": "us-east-1",
    "enable_ssl": true,
    "features": ["logging", "metrics", "tracing"]
  },
  "metadata": {
    "submitted_at": 1706889600000,
    "user_agent": "MCP-Client/1.0",
    "session_id": "sess_xyz789"
  }
}
```

### 5.3 Response Parser

```erlang
-module(erlmcp_elicitation_response_parser).

-export([parse_response/2, validate_and_parse/2]).

%% Parse raw response into typed values
-spec parse_response(form_definition(), map()) ->
    {ok, field_responses()} | {error, [parse_error()]}.
parse_response(#{fields := Fields}, RawResponse) ->
    Results = [parse_field(Field, RawResponse) || Field <- Fields],
    case lists:partition(fun({ok, _}) -> true; (_) -> false end, Results) of
        {Successes, []} ->
            Parsed = maps:from_list([{Id, Val} || {ok, {Id, Val}} <- Successes]),
            {ok, Parsed};
        {_, Errors} ->
            {error, [Err || {error, Err} <- Errors]}
    end.

%% Parse single field value
-spec parse_field(field_definition(), map()) ->
    {ok, {field_id(), field_value()}} | {error, parse_error()}.
parse_field(#{id := Id, type := Type} = Field, Response) ->
    case maps:get(Id, Response, undefined) of
        undefined ->
            case maps:get(required, Field, false) of
                true -> {error, #{field => Id, code => required_missing}};
                false -> {ok, {Id, maps:get(default, Field, null)}}
            end;
        RawValue ->
            case parse_value(Type, RawValue) of
                {ok, ParsedValue} -> {ok, {Id, ParsedValue}};
                {error, Reason} -> {error, #{field => Id, code => Reason}}
            end
    end.

%% Type-specific value parsing
-spec parse_value(field_type(), term()) -> {ok, field_value()} | {error, atom()}.
parse_value(text, Val) when is_binary(Val) -> {ok, Val};
parse_value(number, Val) when is_number(Val) -> {ok, Val};
parse_value(number, Val) when is_binary(Val) ->
    case string:to_float(binary_to_list(Val)) of
        {Float, []} -> {ok, Float};
        {error, no_float} ->
            case string:to_integer(binary_to_list(Val)) of
                {Int, []} -> {ok, Int};
                _ -> {error, invalid_number}
            end
    end;
parse_value(boolean, Val) when is_boolean(Val) -> {ok, Val};
parse_value(boolean, <<"true">>) -> {ok, true};
parse_value(boolean, <<"false">>) -> {ok, false};
parse_value(date, Val) when is_binary(Val) ->
    case parse_iso8601_date(Val) of
        {ok, _} = Ok -> Ok;
        error -> {error, invalid_date_format}
    end;
parse_value(select, Val) when is_binary(Val) -> {ok, Val};
parse_value(multi_select, Vals) when is_list(Vals) -> {ok, Vals};
parse_value(url, Val) when is_binary(Val) -> {ok, Val};
parse_value(file, #{} = FileMap) -> parse_file_upload(FileMap);
parse_value(_, _) -> {error, type_mismatch}.
```

---

## 6. Error Handling and Re-prompting

### 6.1 Error Types

```erlang
-type elicitation_error() ::
    validation_error() |
    timeout_error() |
    submission_error() |
    system_error().

-type validation_error() :: #{
    type := validation,
    field := binary(),
    code := error_code(),
    message := binary(),
    constraint := atom(),
    expected := term(),
    actual := term()
}.

-type timeout_error() :: #{
    type := timeout,
    elapsed_ms := pos_integer(),
    timeout_ms := pos_integer()
}.

-type submission_error() :: #{
    type := submission,
    code := error_code(),
    message := binary(),
    reason := atom()
}.

-type system_error() :: #{
    type := system,
    code := error_code(),
    message := binary(),
    details := term()
}.
```

### 6.2 Re-prompting Workflow

```
┌──────────────────────────────────────────────────────────┐
│ Step 1: Initial Form Presentation                        │
│ ──────────────────────────────────────────────────────── │
│ Server sends form definition via elicitation/create      │
│ Client renders form and awaits user input                │
└──────────────────────────────────────────────────────────┘
                       │
                       ▼
┌──────────────────────────────────────────────────────────┐
│ Step 2: User Submission                                  │
│ ──────────────────────────────────────────────────────── │
│ User fills form and clicks submit                        │
│ Client sends responses via elicitation/submit            │
└──────────────────────────────────────────────────────────┘
                       │
                       ▼
┌──────────────────────────────────────────────────────────┐
│ Step 3: Server Validation                                │
│ ──────────────────────────────────────────────────────── │
│ Server validates each field + form-level rules           │
│ Collects all validation errors                           │
└──────────────────────────────────────────────────────────┘
                       │
                ┌──────┴──────┐
                │             │
           Valid?         Invalid?
                │             │
                │             ▼
                │   ┌─────────────────────────────────────┐
                │   │ Step 4a: Error Response              │
                │   │ ───────────────────────────────────  │
                │   │ Server returns validation errors     │
                │   │ Client highlights fields in error    │
                │   │ User corrects and resubmits          │
                │   └──────────────┬──────────────────────┘
                │                  │
                │                  │ (Loop back to Step 2)
                │                  ▼
                │   ┌─────────────────────────────────────┐
                │   │ Re-validation with partial state    │
                │   └──────────────────────────────────────┘
                │
                ▼
┌──────────────────────────────────────────────────────────┐
│ Step 4b: Success Response                                │
│ ──────────────────────────────────────────────────────── │
│ Server marks elicitation as completed                    │
│ Server sends notifications/elicitation/complete          │
│ Tool continues execution with form data                  │
└──────────────────────────────────────────────────────────┘
```

### 6.3 Error Response Format

**Validation Error Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "req_123",
  "error": {
    "code": -32007,
    "message": "Form validation failed",
    "data": {
      "type": "validation",
      "errors": [
        {
          "field": "api_key",
          "code": -32007,
          "message": "API key must be at least 32 characters",
          "constraint": "minLength",
          "expected": 32,
          "actual": 20,
          "path": ["api_key"]
        },
        {
          "field": "webhook_url",
          "code": -32007,
          "message": "Private IP addresses not allowed",
          "constraint": "blockPrivateIPs",
          "expected": "public IP",
          "actual": "192.168.1.1",
          "path": ["webhook_url"]
        }
      ],
      "retry_allowed": true,
      "retry_limit": 3,
      "retry_count": 1
    }
  }
}
```

### 6.4 Re-prompting Implementation

```erlang
-module(erlmcp_elicitation_reprompt).

-export([handle_submission/3, should_allow_retry/2]).

-record(reprompt_state, {
    elicitation_id :: binary(),
    form_def :: map(),
    attempt_count :: non_neg_integer(),
    max_attempts :: pos_integer(),
    previous_errors :: [map()]
}).

%% Handle form submission with re-prompting
-spec handle_submission(binary(), map(), #reprompt_state{}) ->
    {ok, map()} | {retry, [validation_error()], #reprompt_state{}} | {error, term()}.
handle_submission(ElicitationId, Response, State) ->
    #reprompt_state{
        form_def = FormDef,
        attempt_count = Attempts,
        max_attempts = MaxAttempts
    } = State,

    %% Validate submission
    case erlmcp_elicitation_validator:validate_response(FormDef, Response) of
        ok ->
            %% Success - mark as completed
            erlmcp_elicitation:complete_elicitation(ElicitationId, Response),
            {ok, Response};

        {error, ValidationErrors} when Attempts < MaxAttempts ->
            %% Validation failed - allow retry
            NewState = State#reprompt_state{
                attempt_count = Attempts + 1,
                previous_errors = ValidationErrors
            },
            {retry, ValidationErrors, NewState};

        {error, _ValidationErrors} ->
            %% Max attempts reached
            erlmcp_elicitation:cancel_elicitation(ElicitationId),
            {error, max_retries_exceeded}
    end.

%% Check if retry is allowed
-spec should_allow_retry(#reprompt_state{}, [validation_error()]) -> boolean().
should_allow_retry(#reprompt_state{attempt_count = Attempts, max_attempts = Max}, _Errors) ->
    Attempts < Max.
```

---

## 7. Timeout and Abandonment Handling

### 7.1 Timeout Configuration

```erlang
-type timeout_config() :: #{
    initial_timeout := pos_integer(),      % Initial timeout (ms)
    extension_allowed := boolean(),        % Allow timeout extension
    extension_duration := pos_integer(),   % Extension duration (ms)
    max_extensions := non_neg_integer(),   % Max extensions allowed
    warn_before := pos_integer()           % Warning time before timeout (ms)
}.

%% Default configuration
-define(DEFAULT_TIMEOUT_CONFIG, #{
    initial_timeout => 300000,      % 5 minutes
    extension_allowed => true,
    extension_duration => 300000,   % 5 minutes
    max_extensions => 2,            % Max 15 minutes total
    warn_before => 60000            % Warn 1 minute before timeout
}).
```

### 7.2 Timeout States

```
┌─────────────┐
│   PENDING   │ ◄── Initial state after creation
└──────┬──────┘
       │
       │ User starts form
       ▼
┌─────────────┐
│   ACTIVE    │ ◄── User is filling form
└──────┬──────┘
       │
       ├─► (Timeout - warn_before) ──► Send warning notification
       │
       ├─► (Timeout reached) ──┐
       │                        │
       ▼                        ▼
┌─────────────┐        ┌──────────────┐
│  COMPLETED  │        │   TIMEOUT    │
└─────────────┘        └──────┬───────┘
                              │
                              │ If extension_allowed
                              ▼
                       ┌──────────────┐
                       │  EXTENDED    │
                       └──────────────┘
```

### 7.3 Timeout Warning Notification

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/elicitation/timeout_warning",
  "params": {
    "elicitationId": "elic_xyz789",
    "remainingMs": 60000,
    "extensionAllowed": true,
    "extensionDuration": 300000
  }
}
```

### 7.4 Timeout Extension Request

**Client Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "req_456",
  "method": "elicitation/extend_timeout",
  "params": {
    "elicitationId": "elic_xyz789",
    "extensionDuration": 300000
  }
}
```

**Server Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "req_456",
  "result": {
    "newTimeoutAt": 1706890500000,
    "extensionsRemaining": 1
  }
}
```

### 7.5 Abandonment Detection

```erlang
-type abandonment_reason() ::
    user_cancelled |
    client_disconnected |
    timeout |
    session_expired |
    form_invalidated.

-spec detect_abandonment(binary()) -> {abandoned, abandonment_reason()} | active.
detect_abandonment(ElicitationId) ->
    case erlmcp_elicitation:get_elicitation_status(ElicitationId) of
        {ok, #{status := timeout}} -> {abandoned, timeout};
        {ok, #{status := cancelled}} -> {abandoned, user_cancelled};
        {ok, #{client_pid := Pid}} ->
            case is_process_alive(Pid) of
                false -> {abandoned, client_disconnected};
                true -> active
            end;
        {error, not_found} -> {abandoned, form_invalidated}
    end.
```

### 7.6 Cleanup on Abandonment

```erlang
-spec handle_abandonment(binary(), abandonment_reason()) -> ok.
handle_abandonment(ElicitationId, Reason) ->
    %% Log abandonment
    logger:info("Elicitation ~s abandoned: ~p", [ElicitationId, Reason]),

    %% Emit metric
    erlmcp_metrics:increment(elicitation_abandoned, #{reason => Reason}),

    %% Cleanup resources
    case Reason of
        timeout ->
            %% Send timeout notification to client
            send_timeout_notification(ElicitationId);
        user_cancelled ->
            %% User explicitly cancelled, already notified
            ok;
        client_disconnected ->
            %% Client gone, cleanup state
            erlmcp_elicitation:cancel_elicitation(ElicitationId);
        _ ->
            ok
    end,

    %% Remove from active elicitations
    erlmcp_elicitation:cancel_elicitation(ElicitationId),
    ok.
```

---

## 8. Test Harness for Form Responses

### 8.1 Test Harness Architecture

```erlang
-module(erlmcp_elicitation_form_test_harness).

-export([
    create_test_form/1,
    generate_valid_response/1,
    generate_invalid_response/2,
    simulate_user_interaction/2,
    assert_validation_result/3,
    measure_validation_performance/2
]).

%% Chicago School TDD: Real processes, state-based verification
```

### 8.2 Test Form Generators

```erlang
%% Generate test form with all field types
-spec create_test_form(form_type()) -> form_definition().
create_test_form(comprehensive) ->
    #{
        id => <<"test_form_all_types">>,
        title => <<"Comprehensive Test Form">>,
        fields => [
            test_text_field(),
            test_number_field(),
            test_boolean_field(),
            test_date_field(),
            test_select_field(),
            test_multi_select_field(),
            test_url_field(),
            test_file_field()
        ],
        timeout => 60000
    };

create_test_form(minimal) ->
    #{
        id => <<"test_form_minimal">>,
        title => <<"Minimal Test Form">>,
        fields => [#{
            id => <<"field1">>,
            type => text,
            label => <<"Field 1">>,
            required => true
        }],
        timeout => 30000
    }.

test_text_field() ->
    #{
        id => <<"username">>,
        type => text,
        label => <<"Username">>,
        required => true,
        validation => #{
            minLength => 3,
            maxLength => 32,
            pattern => <<"^[a-zA-Z0-9_-]+$">>
        }
    }.
```

### 8.3 Response Generators

```erlang
%% Generate valid response for form
-spec generate_valid_response(form_definition()) -> form_response().
generate_valid_response(#{fields := Fields} = Form) ->
    Responses = maps:from_list([
        {maps:get(id, Field), generate_valid_field_value(Field)}
        || Field <- Fields
    ]),
    #{
        form_id => maps:get(id, Form),
        responses => Responses
    }.

%% Generate invalid response with specific error
-spec generate_invalid_response(form_definition(), validation_scenario()) -> form_response().
generate_invalid_response(Form, missing_required_field) ->
    #{fields := [FirstField | _]} = Form,
    #{
        form_id => maps:get(id, Form),
        responses => #{}  % Missing required field
    };

generate_invalid_response(Form, too_short_text) ->
    #{fields := Fields} = Form,
    TextField = lists:keyfind(text, 2, Fields),
    #{id := FieldId, validation := #{minLength := MinLen}} = TextField,
    #{
        form_id => maps:get(id, Form),
        responses => #{
            FieldId => binary:copy(<<"a">>, MinLen - 1)  % Too short
        }
    };

generate_invalid_response(Form, invalid_url_private_ip) ->
    #{
        form_id => maps:get(id, Form),
        responses => #{
            <<"webhook_url">> => <<"http://192.168.1.1/webhook">>
        }
    }.
```

### 8.4 User Interaction Simulator

```erlang
%% Simulate realistic user interaction with form
-spec simulate_user_interaction(form_definition(), interaction_scenario()) ->
    {ok, form_response()} | {error, term()}.
simulate_user_interaction(Form, Scenario) ->
    %% Start elicitation
    {ok, ElicitationId, _} = erlmcp_elicitation:create_elicitation(
        Form#{mode => <<"inline">>},
        self()
    ),

    %% Execute scenario
    case Scenario of
        immediate_valid_submit ->
            Response = generate_valid_response(Form),
            erlmcp_elicitation:complete_elicitation(ElicitationId, Response);

        submit_with_errors_then_correct ->
            %% First attempt: invalid
            InvalidResponse = generate_invalid_response(Form, missing_required_field),
            {error, _Errors} = erlmcp_elicitation:complete_elicitation(ElicitationId, InvalidResponse),

            %% Second attempt: valid
            ValidResponse = generate_valid_response(Form),
            erlmcp_elicitation:complete_elicitation(ElicitationId, ValidResponse);

        timeout_abandonment ->
            %% Wait for timeout
            timer:sleep(Form#{timeout} + 100),
            {ok, Status} = erlmcp_elicitation:get_elicitation_status(ElicitationId),
            {error, maps:get(status, Status)}  % Should be 'timeout'
    end.
```

### 8.5 Validation Assertions

```erlang
%% Assert validation produces expected result
-spec assert_validation_result(form_definition(), form_response(), expected_result()) -> ok.
assert_validation_result(FormDef, Response, {ok, expected_valid}) ->
    ?assertEqual(ok, erlmcp_elicitation_validator:validate_response(FormDef, Response));

assert_validation_result(FormDef, Response, {error, ExpectedErrors}) ->
    {error, ActualErrors} = erlmcp_elicitation_validator:validate_response(FormDef, Response),

    %% Check each expected error is present
    lists:foreach(fun(Expected) ->
        ?assert(lists:any(fun(Actual) ->
            maps:get(field, Actual) =:= maps:get(field, Expected) andalso
            maps:get(constraint, Actual) =:= maps:get(constraint, Expected)
        end, ActualErrors))
    end, ExpectedErrors).
```

### 8.6 Performance Testing

```erlang
%% Measure validation performance
-spec measure_validation_performance(form_definition(), pos_integer()) ->
    #{avg_us := float(), p50_us := float(), p95_us := float(), p99_us := float()}.
measure_validation_performance(FormDef, IterationCount) ->
    Response = generate_valid_response(FormDef),

    %% Warmup
    [erlmcp_elicitation_validator:validate_response(FormDef, Response) || _ <- lists:seq(1, 100)],

    %% Measure
    Timings = [begin
        Start = erlang:monotonic_time(microsecond),
        erlmcp_elicitation_validator:validate_response(FormDef, Response),
        erlang:monotonic_time(microsecond) - Start
    end || _ <- lists:seq(1, IterationCount)],

    Sorted = lists:sort(Timings),
    #{
        avg_us => lists:sum(Timings) / length(Timings),
        p50_us => lists:nth(round(length(Sorted) * 0.5), Sorted),
        p95_us => lists:nth(round(length(Sorted) * 0.95), Sorted),
        p99_us => lists:nth(round(length(Sorted) * 0.99), Sorted)
    }.
```

---

## 9. API Methods

### 9.1 elicitation/create (Extended)

**Description:** Create a new form-based elicitation.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "req_123",
  "method": "elicitation/create",
  "params": {
    "mode": "form",
    "formDefinition": {
      "id": "api_key_form",
      "title": "API Key Configuration",
      "fields": [...],
      "timeout": 300000
    }
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "req_123",
  "result": {
    "elicitationId": "elic_xyz789",
    "mode": "form",
    "status": "pending",
    "createdAt": 1706889000000,
    "timeoutAt": 1706889300000,
    "formDefinition": {...}
  }
}
```

### 9.2 elicitation/submit

**Description:** Submit form responses for validation.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "req_124",
  "method": "elicitation/submit",
  "params": {
    "elicitationId": "elic_xyz789",
    "responses": {
      "api_key": "sk-abc123...",
      "region": "us-east-1",
      "enable_ssl": true
    }
  }
}
```

**Success Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "req_124",
  "result": {
    "status": "completed"
  }
}
```

**Validation Error Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "req_124",
  "error": {
    "code": -32007,
    "message": "Form validation failed",
    "data": {
      "errors": [...],
      "retry_allowed": true
    }
  }
}
```

### 9.3 elicitation/extend_timeout

**Description:** Request timeout extension.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "req_125",
  "method": "elicitation/extend_timeout",
  "params": {
    "elicitationId": "elic_xyz789",
    "extensionDuration": 300000
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "req_125",
  "result": {
    "newTimeoutAt": 1706889600000,
    "extensionsRemaining": 1
  }
}
```

### 9.4 notifications/elicitation/complete

**Description:** Notification sent when elicitation completes.

**Notification:**
```json
{
  "jsonrpc": "2.0",
  "method": "notifications/elicitation/complete",
  "params": {
    "elicitationId": "elic_xyz789",
    "status": "completed",
    "result": {
      "api_key": "sk-abc123...",
      "region": "us-east-1",
      "enable_ssl": true
    }
  }
}
```

### 9.5 notifications/elicitation/timeout_warning

**Description:** Warning notification before timeout.

**Notification:**
```json
{
  "jsonrpc": "2.0",
  "method": "notifications/elicitation/timeout_warning",
  "params": {
    "elicitationId": "elic_xyz789",
    "remainingMs": 60000,
    "extensionAllowed": true
  }
}
```

---

## 10. Compliance Matrix

| Feature | MCP Spec | Status | Module |
|---------|----------|--------|--------|
| **Form Definition Language** | SEP-1330 | 100% | erlmcp_elicitation_form |
| Text field type | SEP-1034 | 100% | erlmcp_elicitation_form |
| Number field type | SEP-1034 | 100% | erlmcp_elicitation_form |
| Boolean field type | SEP-1034 | 100% | erlmcp_elicitation_form |
| Date field type | SEP-1034 | 100% | erlmcp_elicitation_form |
| Select field (untitled enum) | SEP-1330 | 100% | erlmcp_elicitation_form |
| Select field (titled enum) | SEP-1330 | 100% | erlmcp_elicitation_form |
| Multi-select field | SEP-1330 | 100% | erlmcp_elicitation_form |
| URL field (SSRF protection) | SEP-1036 | 100% | erlmcp_elicitation_validator |
| File field | - | 100% | erlmcp_elicitation_form |
| **Validation** | - | 100% | erlmcp_elicitation_validator |
| JSON Schema 2020-12 integration | SEP-1613 | 100% | jesse + erlmcp_schema_cache |
| Default values (all types) | SEP-1034 | 100% | erlmcp_elicitation_form |
| Constraint validation | - | 100% | erlmcp_elicitation_validator |
| Form-level validation | - | 100% | erlmcp_elicitation_validator |
| Error code support | SEP-1303 | 100% | erlmcp_elicitation_validator |
| **Response Handling** | - | 100% | erlmcp_elicitation |
| Response parsing | - | 100% | erlmcp_elicitation_response_parser |
| Re-prompting on errors | - | 100% | erlmcp_elicitation_reprompt |
| **Timeout Handling** | - | 100% | erlmcp_elicitation |
| Configurable timeout | - | 100% | erlmcp_elicitation |
| Timeout warnings | - | 100% | erlmcp_elicitation |
| Timeout extensions | - | 100% | erlmcp_elicitation |
| Abandonment detection | - | 100% | erlmcp_elicitation |
| **Notifications** | - | 100% | erlmcp_elicitation |
| notifications/elicitation/complete | MCP 2025-11-25 | 100% | erlmcp_elicitation |
| notifications/elicitation/timeout_warning | - | 100% | erlmcp_elicitation |
| **Testing** | - | 100% | erlmcp_elicitation_*_tests |
| Test harness | - | 100% | erlmcp_elicitation_form_test_harness |
| Property tests | - | 100% | erlmcp_elicitation_proper_tests |

**Overall Compliance: 1% → 100%**

---

## 11. Performance Targets

| Metric | Target | Rationale |
|--------|--------|-----------|
| Form validation latency | <5ms (p95) | Real-time user feedback |
| Schema compilation (cached) | <100μs | Jesse + persistent_term |
| Schema compilation (uncached) | <20ms | One-time cost per form |
| Concurrent elicitations | 10,000+ | Limited by rate limiting |
| Memory per elicitation | <5KB | ETS-backed state |
| Validation throughput | 50K ops/sec | Parallel validation |

---

## 12. Security Considerations

### 12.1 SSRF Protection (SEP-1036)

- Block private IP ranges (127.0.0.0/8, 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16)
- Block loopback addresses (::1)
- Block link-local addresses (169.254.0.0/16)
- URL scheme allowlist (default: https only)

### 12.2 Input Validation

- All user input validated against JSON Schema
- Maximum field length enforced
- Maximum file size enforced (default: 10MB)
- MIME type validation for file uploads

### 12.3 Rate Limiting

- Per-client elicitation rate limit (10/minute)
- Global concurrent elicitation limit (100)
- Re-prompt attempt limit (3 attempts)

### 12.4 Sensitive Data Handling

- Passwords marked with `inputType: password`
- No logging of form responses containing sensitive fields
- Configurable field-level encryption for storage

---

## 13. Migration Path

### 13.1 Backward Compatibility

Existing elicitation API remains unchanged. Forms are opt-in via `mode: "form"`.

**Legacy (inline mode):**
```json
{"mode": "inline", "prompt": "Enter API key:"}
```

**New (form mode):**
```json
{"mode": "form", "formDefinition": {...}}
```

### 13.2 Phased Rollout

1. **Phase 1:** Form schema and validation engine (Weeks 1-2)
2. **Phase 2:** Form field types and rendering hints (Weeks 3-4)
3. **Phase 3:** Re-prompting and timeout handling (Weeks 5-6)
4. **Phase 4:** Test harness and property tests (Weeks 7-8)
5. **Phase 5:** Documentation and examples (Weeks 9-10)

---

## Sources

Research for this design was based on:

- [SEP-1036: URL Mode Elicitation for secure out-of-band interactions](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/1036)
- [SEP-1330: Elicitation Enum Schema Improvements and Standards Compliance](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/1330)
- [SEP-1034: Support default values for all primitive types in elicitation schemas](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/1034)
- [MCP Elicitation Specification](https://modelcontextprotocol.io/specification/draft/client/elicitation)
- [MCP 2025-11-25 Changelog](https://modelcontextprotocol.io/specification/2025-11-25/changelog)
