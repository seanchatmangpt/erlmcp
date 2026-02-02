# Elicitation Form Schema Specification
**Version:** 1.0.0
**Date:** 2026-02-02
**JSON Schema Version:** 2020-12
**MCP Spec:** 2025-11-25

---

## 1. Overview

This document defines the formal JSON Schema for elicitation forms in erlmcp. All form definitions MUST conform to this schema to ensure type safety, validation correctness, and client compatibility.

### 1.1 Design Principles

1. **Standards Compliance**: Full JSON Schema 2020-12 compatibility
2. **Type Safety**: Strict typing for all field definitions
3. **Extensibility**: Custom extensions via `x-` prefix
4. **Backward Compatibility**: Supports legacy elicitation modes
5. **Client Hints**: Optional rendering hints for better UX

---

## 2. Root Schema: Form Definition

### 2.1 JSON Schema

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://erlmcp.org/schemas/elicitation/form-definition.json",
  "title": "Elicitation Form Definition",
  "description": "Schema for MCP elicitation form definitions",
  "type": "object",
  "required": ["id", "title", "fields"],
  "properties": {
    "id": {
      "type": "string",
      "description": "Unique form identifier",
      "pattern": "^[a-zA-Z0-9_-]+$",
      "minLength": 1,
      "maxLength": 128
    },
    "title": {
      "type": "string",
      "description": "Human-readable form title",
      "minLength": 1,
      "maxLength": 256
    },
    "description": {
      "type": "string",
      "description": "Optional form description",
      "maxLength": 2048
    },
    "fields": {
      "type": "array",
      "description": "Ordered list of form fields",
      "items": {"$ref": "#/$defs/field"},
      "minItems": 1,
      "maxItems": 100
    },
    "validation": {
      "$ref": "#/$defs/formValidation",
      "description": "Form-level validation rules"
    },
    "metadata": {
      "type": "object",
      "description": "Custom metadata",
      "additionalProperties": true
    },
    "timeout": {
      "type": "integer",
      "description": "Form timeout in milliseconds",
      "minimum": 1000,
      "maximum": 3600000,
      "default": 300000
    },
    "allowPartial": {
      "type": "boolean",
      "description": "Allow partial form submissions",
      "default": false
    },
    "version": {
      "type": "string",
      "description": "Form schema version (semver)",
      "pattern": "^\\d+\\.\\d+\\.\\d+$",
      "default": "1.0.0"
    }
  },
  "additionalProperties": false
}
```

### 2.2 Erlang Record

```erlang
-record(form_definition, {
    id :: binary(),
    title :: binary(),
    description = undefined :: binary() | undefined,
    fields :: [field_definition()],
    validation = #{} :: map(),
    metadata = #{} :: map(),
    timeout = 300000 :: pos_integer(),
    allow_partial = false :: boolean(),
    version = <<"1.0.0">> :: binary()
}).
```

---

## 3. Field Definition Schema

### 3.1 Base Field Schema

```json
{
  "$defs": {
    "field": {
      "type": "object",
      "required": ["id", "type", "label"],
      "properties": {
        "id": {
          "type": "string",
          "description": "Unique field identifier within form",
          "pattern": "^[a-zA-Z0-9_-]+$",
          "minLength": 1,
          "maxLength": 64
        },
        "type": {
          "type": "string",
          "enum": ["text", "number", "boolean", "date", "select", "multi_select", "url", "file"],
          "description": "Field type"
        },
        "label": {
          "type": "string",
          "description": "Display label",
          "minLength": 1,
          "maxLength": 256
        },
        "description": {
          "type": "string",
          "description": "Optional field description",
          "maxLength": 1024
        },
        "required": {
          "type": "boolean",
          "description": "Is field required?",
          "default": false
        },
        "default": {
          "description": "Default value (type-specific, SEP-1034)"
        },
        "validation": {
          "type": "object",
          "description": "Type-specific validation rules"
        },
        "rendering": {
          "$ref": "#/$defs/renderingHints",
          "description": "Client rendering hints"
        },
        "dependencies": {
          "type": "array",
          "description": "Field IDs this field depends on",
          "items": {"type": "string"},
          "uniqueItems": true
        }
      },
      "additionalProperties": false,
      "allOf": [
        {
          "if": {"properties": {"type": {"const": "text"}}},
          "then": {"$ref": "#/$defs/textField"}
        },
        {
          "if": {"properties": {"type": {"const": "number"}}},
          "then": {"$ref": "#/$defs/numberField"}
        },
        {
          "if": {"properties": {"type": {"const": "boolean"}}},
          "then": {"$ref": "#/$defs/booleanField"}
        },
        {
          "if": {"properties": {"type": {"const": "date"}}},
          "then": {"$ref": "#/$defs/dateField"}
        },
        {
          "if": {"properties": {"type": {"const": "select"}}},
          "then": {"$ref": "#/$defs/selectField"}
        },
        {
          "if": {"properties": {"type": {"const": "multi_select"}}},
          "then": {"$ref": "#/$defs/multiSelectField"}
        },
        {
          "if": {"properties": {"type": {"const": "url"}}},
          "then": {"$ref": "#/$defs/urlField"}
        },
        {
          "if": {"properties": {"type": {"const": "file"}}},
          "then": {"$ref": "#/$defs/fileField"}
        }
      ]
    }
  }
}
```

---

## 4. Type-Specific Field Schemas

### 4.1 Text Field

```json
{
  "$defs": {
    "textField": {
      "type": "object",
      "properties": {
        "default": {"type": "string"},
        "validation": {
          "type": "object",
          "properties": {
            "minLength": {"type": "integer", "minimum": 0},
            "maxLength": {"type": "integer", "minimum": 1},
            "pattern": {"type": "string", "format": "regex"},
            "format": {
              "type": "string",
              "enum": ["email", "uri", "uuid", "ipv4", "ipv6", "hostname", "date-time"]
            }
          },
          "additionalProperties": false
        },
        "rendering": {
          "type": "object",
          "properties": {
            "placeholder": {"type": "string"},
            "inputType": {
              "type": "string",
              "enum": ["text", "password", "email", "tel", "search"]
            },
            "helpText": {"type": "string"},
            "autocomplete": {"type": "string"},
            "rows": {"type": "integer", "minimum": 1, "maximum": 100}
          }
        }
      }
    }
  }
}
```

**Erlang Validation:**
```erlang
validate_text_field(#{type := text} = Field, Value) when is_binary(Value) ->
    Validation = maps:get(validation, Field, #{}),
    Checks = [
        check_min_length(Value, maps:get(minLength, Validation, 0)),
        check_max_length(Value, maps:get(maxLength, Validation, infinity)),
        check_pattern(Value, maps:get(pattern, Validation, undefined)),
        check_format(Value, maps:get(format, Validation, undefined))
    ],
    combine_results(Checks).
```

### 4.2 Number Field

```json
{
  "$defs": {
    "numberField": {
      "type": "object",
      "properties": {
        "default": {"type": "number"},
        "validation": {
          "type": "object",
          "properties": {
            "minimum": {"type": "number"},
            "maximum": {"type": "number"},
            "exclusiveMinimum": {"type": "number"},
            "exclusiveMaximum": {"type": "number"},
            "multipleOf": {"type": "number", "exclusiveMinimum": 0}
          },
          "additionalProperties": false
        },
        "rendering": {
          "type": "object",
          "properties": {
            "placeholder": {"type": "string"},
            "step": {"type": "number"},
            "helpText": {"type": "string"},
            "displayStyle": {"type": "string", "enum": ["input", "slider", "stepper"]}
          }
        }
      }
    }
  }
}
```

### 4.3 Boolean Field

```json
{
  "$defs": {
    "booleanField": {
      "type": "object",
      "properties": {
        "default": {"type": "boolean"},
        "validation": {
          "type": "object",
          "additionalProperties": false
        },
        "rendering": {
          "type": "object",
          "properties": {
            "helpText": {"type": "string"},
            "displayStyle": {"type": "string", "enum": ["checkbox", "toggle", "radio"]}
          }
        }
      }
    }
  }
}
```

### 4.4 Date Field

```json
{
  "$defs": {
    "dateField": {
      "type": "object",
      "properties": {
        "default": {"type": "string", "format": "date"},
        "validation": {
          "type": "object",
          "properties": {
            "format": {"type": "string", "enum": ["date", "date-time"]},
            "minimum": {"type": "string", "format": "date"},
            "maximum": {"type": "string", "format": "date"}
          },
          "additionalProperties": false
        },
        "rendering": {
          "type": "object",
          "properties": {
            "helpText": {"type": "string"},
            "displayFormat": {"type": "string"},
            "enableTime": {"type": "boolean", "default": false}
          }
        }
      }
    }
  }
}
```

### 4.5 Select Field (SEP-1330 Compliant)

#### 4.5.1 Untitled Enum

```json
{
  "$defs": {
    "selectField": {
      "type": "object",
      "required": ["options"],
      "properties": {
        "default": {"type": "string"},
        "options": {
          "oneOf": [
            {
              "type": "array",
              "description": "Untitled enum: values serve as both value and display text",
              "items": {"type": "string"},
              "minItems": 1,
              "uniqueItems": true
            },
            {
              "type": "array",
              "description": "Titled enum: separate display titles (SEP-1330)",
              "items": {
                "type": "object",
                "required": ["value", "title"],
                "properties": {
                  "value": {"type": "string"},
                  "title": {"type": "string"}
                }
              },
              "minItems": 1
            }
          ]
        },
        "validation": {
          "type": "object",
          "properties": {
            "enum": {
              "type": "array",
              "description": "Auto-generated from options",
              "items": {"type": "string"}
            }
          }
        },
        "rendering": {
          "type": "object",
          "properties": {
            "helpText": {"type": "string"},
            "displayStyle": {"type": "string", "enum": ["dropdown", "radio", "segmented"]},
            "searchable": {"type": "boolean", "default": false}
          }
        }
      }
    }
  }
}
```

**Example - Untitled:**
```json
{
  "id": "log_level",
  "type": "select",
  "label": "Log Level",
  "options": ["debug", "info", "warning", "error"],
  "default": "info"
}
```

**Example - Titled:**
```json
{
  "id": "region",
  "type": "select",
  "label": "AWS Region",
  "options": [
    {"value": "us-east-1", "title": "US East (N. Virginia)"},
    {"value": "us-west-2", "title": "US West (Oregon)"}
  ],
  "default": "us-east-1"
}
```

### 4.6 Multi-Select Field (SEP-1330 Compliant)

```json
{
  "$defs": {
    "multiSelectField": {
      "type": "object",
      "required": ["options"],
      "properties": {
        "default": {
          "type": "array",
          "items": {"type": "string"},
          "uniqueItems": true
        },
        "options": {
          "oneOf": [
            {
              "type": "array",
              "description": "Untitled multi-select enum",
              "items": {"type": "string"},
              "minItems": 1,
              "uniqueItems": true
            },
            {
              "type": "array",
              "description": "Titled multi-select enum (SEP-1330)",
              "items": {
                "type": "object",
                "required": ["value", "title"],
                "properties": {
                  "value": {"type": "string"},
                  "title": {"type": "string"}
                }
              },
              "minItems": 1
            }
          ]
        },
        "validation": {
          "type": "object",
          "properties": {
            "enum": {"type": "array", "items": {"type": "string"}},
            "minItems": {"type": "integer", "minimum": 0},
            "maxItems": {"type": "integer", "minimum": 1},
            "uniqueItems": {"type": "boolean", "const": true}
          }
        },
        "rendering": {
          "type": "object",
          "properties": {
            "helpText": {"type": "string"},
            "displayStyle": {"type": "string", "enum": ["checkbox", "multiselect_dropdown", "chips"]}
          }
        }
      }
    }
  }
}
```

### 4.7 URL Field (SEP-1036 SSRF Protected)

```json
{
  "$defs": {
    "urlField": {
      "type": "object",
      "properties": {
        "default": {"type": "string", "format": "uri"},
        "validation": {
          "type": "object",
          "properties": {
            "format": {"type": "string", "const": "uri"},
            "allowedSchemes": {
              "type": "array",
              "items": {"type": "string", "enum": ["http", "https", "ftp", "ftps", "ws", "wss"]},
              "default": ["https"]
            },
            "blockPrivateIPs": {"type": "boolean", "default": true},
            "blockLocalhost": {"type": "boolean", "default": true},
            "maxLength": {"type": "integer", "default": 2048}
          },
          "additionalProperties": false
        },
        "rendering": {
          "type": "object",
          "properties": {
            "placeholder": {"type": "string"},
            "helpText": {"type": "string"},
            "testButton": {"type": "boolean", "default": false}
          }
        }
      }
    }
  }
}
```

### 4.8 File Field

```json
{
  "$defs": {
    "fileField": {
      "type": "object",
      "properties": {
        "validation": {
          "type": "object",
          "properties": {
            "maxSize": {"type": "integer", "minimum": 1, "default": 10485760},
            "allowedMimeTypes": {
              "type": "array",
              "items": {"type": "string"},
              "minItems": 1
            },
            "allowedExtensions": {
              "type": "array",
              "items": {"type": "string", "pattern": "^\\.[a-zA-Z0-9]+$"},
              "minItems": 1
            }
          },
          "additionalProperties": false
        },
        "rendering": {
          "type": "object",
          "properties": {
            "acceptAttribute": {"type": "string"},
            "helpText": {"type": "string"},
            "dragAndDrop": {"type": "boolean", "default": true},
            "preview": {"type": "boolean", "default": false}
          }
        }
      }
    }
  }
}
```

---

## 5. Form Validation Schema

### 5.1 Form-Level Validation

```json
{
  "$defs": {
    "formValidation": {
      "type": "object",
      "properties": {
        "dependencies": {
          "$ref": "#/$defs/dependencyRules",
          "description": "Cross-field dependencies"
        },
        "customValidators": {
          "type": "array",
          "description": "Custom validation function names",
          "items": {"type": "string"}
        }
      },
      "additionalProperties": false
    },
    "dependencyRules": {
      "type": "object",
      "patternProperties": {
        "^[a-zA-Z0-9_-]+$": {
          "type": "object",
          "required": ["condition", "action"],
          "properties": {
            "condition": {"$ref": "#/$defs/conditionExpr"},
            "action": {
              "type": "string",
              "enum": ["show", "hide", "enable", "disable", "validate"]
            }
          }
        }
      },
      "additionalProperties": false
    },
    "conditionExpr": {
      "type": "object",
      "required": ["operator", "field"],
      "properties": {
        "operator": {
          "type": "string",
          "enum": ["equals", "not_equals", "contains", "gt", "lt", "gte", "lte", "regex", "and", "or"]
        },
        "field": {"type": "string"},
        "value": {}
      }
    }
  }
}
```

---

## 6. Rendering Hints Schema

```json
{
  "$defs": {
    "renderingHints": {
      "type": "object",
      "properties": {
        "placeholder": {"type": "string"},
        "helpText": {"type": "string"},
        "displayStyle": {"type": "string"},
        "readonly": {"type": "boolean", "default": false},
        "hidden": {"type": "boolean", "default": false},
        "autocomplete": {"type": "string"},
        "gridColumn": {"type": "integer", "minimum": 1, "maximum": 12},
        "gridRow": {"type": "integer", "minimum": 1},
        "breakAfter": {"type": "boolean", "default": false},
        "inputType": {"type": "string"},
        "step": {"type": "number"},
        "rows": {"type": "integer"},
        "conditionalDisplay": {"$ref": "#/$defs/conditionExpr"},
        "validationTrigger": {
          "type": "string",
          "enum": ["on_change", "on_blur", "on_submit"],
          "default": "on_blur"
        },
        "debounceMs": {"type": "integer", "minimum": 0, "default": 300}
      },
      "additionalProperties": true
    }
  }
}
```

---

## 7. Form Response Schema

### 7.1 Response Format

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://erlmcp.org/schemas/elicitation/form-response.json",
  "title": "Elicitation Form Response",
  "type": "object",
  "required": ["form_id", "responses"],
  "properties": {
    "form_id": {"type": "string"},
    "responses": {
      "type": "object",
      "patternProperties": {
        "^[a-zA-Z0-9_-]+$": {}
      }
    },
    "metadata": {
      "type": "object",
      "properties": {
        "submitted_at": {"type": "integer"},
        "user_agent": {"type": "string"},
        "session_id": {"type": "string"}
      }
    }
  },
  "additionalProperties": false
}
```

### 7.2 File Upload Format

```json
{
  "$defs": {
    "fileUpload": {
      "type": "object",
      "required": ["filename", "content_type", "size"],
      "properties": {
        "filename": {"type": "string"},
        "content_type": {"type": "string"},
        "size": {"type": "integer", "minimum": 0},
        "content": {
          "oneOf": [
            {"type": "string", "contentEncoding": "base64"},
            {"$ref": "#/$defs/fileReference"}
          ]
        }
      }
    },
    "fileReference": {
      "type": "object",
      "required": ["type", "location"],
      "properties": {
        "type": {"type": "string", "enum": ["url", "s3", "local"]},
        "location": {"type": "string"}
      }
    }
  }
}
```

---

## 8. Validation Error Schema

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://erlmcp.org/schemas/elicitation/validation-error.json",
  "title": "Validation Error",
  "type": "object",
  "required": ["field", "code", "message", "constraint"],
  "properties": {
    "field": {"type": "string"},
    "code": {"type": "integer"},
    "message": {"type": "string"},
    "constraint": {"type": "string"},
    "expected": {},
    "actual": {},
    "path": {
      "type": "array",
      "items": {"type": "string"}
    }
  },
  "additionalProperties": false
}
```

---

## 9. Erlang Type Definitions

### 9.1 Core Types

```erlang
%% Form definition
-type form_definition() :: #{
    id := binary(),
    title := binary(),
    description => binary(),
    fields := [field_definition()],
    validation => form_validation(),
    metadata => map(),
    timeout => pos_integer(),
    allow_partial => boolean(),
    version => binary()
}.

%% Field definition
-type field_definition() :: #{
    id := binary(),
    type := field_type(),
    label := binary(),
    description => binary(),
    required => boolean(),
    default => term(),
    validation => field_validation(),
    rendering => rendering_hints(),
    dependencies => [binary()]
}.

-type field_type() ::
    text | number | boolean | date |
    select | multi_select | url | file.

%% Validation
-type field_validation() :: map().
-type form_validation() :: #{
    dependencies => dependency_rules(),
    custom_validators => [binary()]
}.

-type dependency_rules() :: #{
    field_id() => #{
        condition := condition_expr(),
        action := show | hide | enable | disable | validate
    }
}.

-type condition_expr() :: #{
    operator := equals | not_equals | contains | gt | lt | gte | lte | regex | 'and' | 'or',
    field := binary(),
    value => term()
}.

%% Rendering
-type rendering_hints() :: #{
    placeholder => binary(),
    help_text => binary(),
    display_style => binary(),
    readonly => boolean(),
    hidden => boolean(),
    autocomplete => binary(),
    grid_column => pos_integer(),
    grid_row => pos_integer(),
    break_after => boolean(),
    input_type => binary(),
    step => number(),
    rows => pos_integer(),
    conditional_display => condition_expr(),
    validation_trigger => on_change | on_blur | on_submit,
    debounce_ms => non_neg_integer()
}.

%% Response
-type form_response() :: #{
    form_id := binary(),
    responses := #{field_id() => field_value()},
    metadata => #{
        submitted_at => integer(),
        user_agent => binary(),
        session_id => binary()
    }
}.

-type field_value() ::
    binary() | number() | boolean() | [binary()] | file_upload().

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

%% Validation error
-type validation_error() :: #{
    field := binary(),
    code := integer(),
    message := binary(),
    constraint := atom(),
    expected := term(),
    actual := term(),
    path => [binary()]
}.
```

---

## 10. Schema Compilation and Caching

### 10.1 Jesse Integration

```erlang
-module(erlmcp_elicitation_form_schema).

-export([compile_form_schema/1, validate_form_definition/1, validate_form_response/2]).

%% Compile form definition to Jesse schema
-spec compile_form_schema(form_definition()) -> jesse:json_schema().
compile_form_schema(#{fields := Fields} = Form) ->
    %% Build JSON Schema properties from fields
    Properties = maps:from_list([
        {FieldId, field_to_json_schema(Field)}
        || #{id := FieldId} = Field <- Fields
    ]),

    %% Collect required fields
    Required = [Id || #{id := Id, required := true} <- Fields],

    #{
        <<"$schema">> => <<"https://json-schema.org/draft/2020-12/schema">>,
        <<"type">> => <<"object">>,
        <<"properties">> => Properties,
        <<"required">> => Required,
        <<"additionalProperties">> => false
    }.

%% Convert field definition to JSON Schema property
-spec field_to_json_schema(field_definition()) -> map().
field_to_json_schema(#{type := text, validation := Validation}) ->
    Base = #{<<"type">> => <<"string">>},
    add_text_constraints(Base, Validation);

field_to_json_schema(#{type := number, validation := Validation}) ->
    Base = #{<<"type">> => <<"number">>},
    add_number_constraints(Base, Validation);

field_to_json_schema(#{type := boolean}) ->
    #{<<"type">> => <<"boolean">>};

field_to_json_schema(#{type := select, options := Options}) ->
    Enum = extract_enum_values(Options),
    #{<<"type">> => <<"string">>, <<"enum">> => Enum};

field_to_json_schema(#{type := multi_select, options := Options}) ->
    Enum = extract_enum_values(Options),
    #{
        <<"type">> => <<"array">>,
        <<"items">> => #{<<"type">> => <<"string">>, <<"enum">> => Enum},
        <<"uniqueItems">> => true
    };

field_to_json_schema(#{type := url}) ->
    #{<<"type">> => <<"string">>, <<"format">> => <<"uri">>};

field_to_json_schema(#{type := date}) ->
    #{<<"type">> => <<"string">>, <<"format">> => <<"date">>};

field_to_json_schema(#{type := file}) ->
    #{<<"type">> => <<"object">>}.

%% Validate form definition against meta-schema
-spec validate_form_definition(map()) -> ok | {error, [validation_error()]}.
validate_form_definition(FormDef) ->
    MetaSchema = load_form_definition_meta_schema(),
    case jesse:validate_with_schema(MetaSchema, FormDef) of
        {ok, _} -> ok;
        {error, Errors} -> {error, format_jesse_errors(Errors)}
    end.

%% Validate form response
-spec validate_form_response(form_definition(), map()) -> ok | {error, [validation_error()]}.
validate_form_response(FormDef, Response) ->
    %% Compile form schema
    Schema = compile_form_schema(FormDef),

    %% Check cache
    CacheKey = form_cache_key(FormDef),
    CompiledSchema = case erlmcp_schema_cache:get_schema(CacheKey) of
        {ok, Cached} -> Cached;
        {error, not_found} ->
            Compiled = jesse:compile(Schema),
            erlmcp_schema_cache:cache_schema(CacheKey, Compiled),
            Compiled
    end,

    %% Validate
    Responses = maps:get(responses, Response),
    case jesse:validate_with_schema(CompiledSchema, Responses) of
        {ok, _} -> ok;
        {error, Errors} -> {error, format_jesse_errors(Errors)}
    end.
```

---

## 11. Complete Example

### 11.1 Comprehensive Form Definition

```json
{
  "id": "oauth_config_form",
  "title": "OAuth 2.0 Configuration",
  "description": "Configure OAuth 2.0 authentication for your application",
  "version": "1.0.0",
  "timeout": 600000,
  "allowPartial": false,
  "fields": [
    {
      "id": "provider",
      "type": "select",
      "label": "OAuth Provider",
      "required": true,
      "default": "google",
      "options": [
        {"value": "google", "title": "Google OAuth 2.0"},
        {"value": "github", "title": "GitHub OAuth"},
        {"value": "microsoft", "title": "Microsoft Identity Platform"},
        {"value": "custom", "title": "Custom OAuth Provider"}
      ],
      "rendering": {
        "helpText": "Select your OAuth provider",
        "displayStyle": "dropdown"
      }
    },
    {
      "id": "client_id",
      "type": "text",
      "label": "Client ID",
      "required": true,
      "validation": {
        "minLength": 10,
        "maxLength": 256
      },
      "rendering": {
        "placeholder": "Enter your OAuth client ID",
        "helpText": "Found in your provider's developer console",
        "autocomplete": "off"
      }
    },
    {
      "id": "client_secret",
      "type": "text",
      "label": "Client Secret",
      "required": true,
      "validation": {
        "minLength": 10,
        "maxLength": 256
      },
      "rendering": {
        "placeholder": "Enter your OAuth client secret",
        "inputType": "password",
        "helpText": "Keep this secret secure",
        "autocomplete": "off"
      }
    },
    {
      "id": "custom_auth_url",
      "type": "url",
      "label": "Authorization URL",
      "required": false,
      "validation": {
        "format": "uri",
        "allowedSchemes": ["https"],
        "blockPrivateIPs": true,
        "blockLocalhost": false
      },
      "rendering": {
        "placeholder": "https://auth.example.com/oauth/authorize",
        "helpText": "Required for custom OAuth providers",
        "conditionalDisplay": {
          "operator": "equals",
          "field": "provider",
          "value": "custom"
        }
      },
      "dependencies": ["provider"]
    },
    {
      "id": "custom_token_url",
      "type": "url",
      "label": "Token URL",
      "required": false,
      "validation": {
        "format": "uri",
        "allowedSchemes": ["https"],
        "blockPrivateIPs": true
      },
      "rendering": {
        "placeholder": "https://auth.example.com/oauth/token",
        "helpText": "Required for custom OAuth providers",
        "conditionalDisplay": {
          "operator": "equals",
          "field": "provider",
          "value": "custom"
        }
      },
      "dependencies": ["provider"]
    },
    {
      "id": "scopes",
      "type": "multi_select",
      "label": "OAuth Scopes",
      "required": true,
      "default": ["openid", "profile", "email"],
      "options": [
        {"value": "openid", "title": "OpenID Connect"},
        {"value": "profile", "title": "User Profile"},
        {"value": "email", "title": "Email Address"},
        {"value": "offline_access", "title": "Offline Access (Refresh Token)"},
        {"value": "read:org", "title": "Read Organization Data"},
        {"value": "write:repo", "title": "Write Repository Access"}
      ],
      "validation": {
        "minItems": 1,
        "maxItems": 10
      },
      "rendering": {
        "helpText": "Select the OAuth scopes your app needs",
        "displayStyle": "checkbox"
      }
    },
    {
      "id": "enable_pkce",
      "type": "boolean",
      "label": "Enable PKCE",
      "required": false,
      "default": true,
      "rendering": {
        "displayStyle": "toggle",
        "helpText": "Proof Key for Code Exchange (recommended for security)"
      }
    },
    {
      "id": "redirect_uri",
      "type": "url",
      "label": "Redirect URI",
      "required": true,
      "validation": {
        "format": "uri",
        "allowedSchemes": ["http", "https"],
        "maxLength": 2048
      },
      "rendering": {
        "placeholder": "https://yourapp.com/oauth/callback",
        "helpText": "Must match exactly in your OAuth provider settings"
      }
    },
    {
      "id": "session_duration",
      "type": "number",
      "label": "Session Duration (hours)",
      "required": false,
      "default": 24,
      "validation": {
        "minimum": 1,
        "maximum": 168,
        "multipleOf": 1
      },
      "rendering": {
        "helpText": "How long should user sessions last? (1-168 hours)",
        "step": 1,
        "displayStyle": "stepper"
      }
    }
  ],
  "validation": {
    "dependencies": {
      "custom_auth_url": {
        "condition": {
          "operator": "equals",
          "field": "provider",
          "value": "custom"
        },
        "action": "validate"
      },
      "custom_token_url": {
        "condition": {
          "operator": "equals",
          "field": "provider",
          "value": "custom"
        },
        "action": "validate"
      }
    }
  },
  "metadata": {
    "category": "authentication",
    "tags": ["oauth", "security", "authentication"]
  }
}
```

---

## 12. Changelog

### Version 1.0.0 (2026-02-02)
- Initial schema specification
- JSON Schema 2020-12 compliance
- SEP-1034 default values support
- SEP-1330 enhanced enums support
- SEP-1036 SSRF-protected URL fields
- Complete Erlang type definitions
- Jesse integration with schema caching
