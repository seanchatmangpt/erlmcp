%%%-----------------------------------------------------------------------------
%%% @doc TCPS Configuration Reference Documentation
%%%
%%% Comprehensive reference for all TCPS configuration options including:
%%% - Quality gate thresholds
%%% - Kanban WIP limits
%%% - Andon severity levels
%%% - Receipt storage paths
%%% - RDF ontology namespaces
%%% - MCP server configuration
%%%
%%% Configuration Sources:
%%% - Application environment (sys.config)
%%% - rebar.config
%%% - Module defaults
%%% - Runtime configuration files
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_config_reference).

%% API exports
-export([
    generate_full_config_reference/0,
    get_quality_gates_config/0,
    get_kanban_config/0,
    get_andon_config/0,
    get_receipt_config/0,
    get_ontology_config/0,
    get_mcp_config/0,
    format_config_markdown/1,
    format_config_json/1,
    validate_config/2
]).

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type config_section() :: #{
    section := binary(),
    description := binary(),
    options := [config_option()]
}.

-type config_option() :: #{
    key := binary(),
    type := atom | string | integer | float | boolean | list | map,
    default := term(),
    description := binary(),
    required := boolean(),
    examples := [binary()],
    valid_values := [term()] | {range, term(), term()} | undefined,
    related := [binary()],
    since := binary() | undefined,
    deprecated := boolean(),
    environment_variable := binary() | undefined
}.

-export_type([config_section/0, config_option/0]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Generate complete configuration reference.
%%
%% @end
%%------------------------------------------------------------------------------
-spec generate_full_config_reference() -> [config_section()].
generate_full_config_reference() ->
    [
        get_quality_gates_config(),
        get_kanban_config(),
        get_andon_config(),
        get_receipt_config(),
        get_ontology_config(),
        get_mcp_config()
    ].

%%------------------------------------------------------------------------------
%% @doc Quality Gates Configuration Reference
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_quality_gates_config() -> config_section().
get_quality_gates_config() ->
    #{
        section => <<"quality_gates">>,
        description => <<"Quality gate thresholds for zero-defect delivery (Toyota Production System standards)">>,

        options => [
            #{
                key => <<"test_pass_rate">>,
                type => float,
                default => 0.95,
                description => <<"Minimum test pass rate required to pass test_execution gate. Tests must pass at this rate or higher.">>,
                required => false,
                examples => [
                    <<"0.95  # 95% pass rate (default)">>,
                    <<"0.90  # 90% pass rate (relaxed)">>,
                    <<"0.98  # 98% pass rate (strict)">>
                ],
                valid_values => {range, 0.0, 1.0},
                related => [<<"test_coverage">>, <<"defect_rate">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_TEST_PASS_RATE">>
            },

            #{
                key => <<"test_coverage">>,
                type => float,
                default => 0.80,
                description => <<"Minimum code coverage required to pass test_execution gate. Industry best practice is 80%.">>,
                required => false,
                examples => [
                    <<"0.80  # 80% coverage (default)">>,
                    <<"0.85  # 85% coverage">>,
                    <<"0.90  # 90% coverage (high quality)">>
                ],
                valid_values => {range, 0.0, 1.0},
                related => [<<"test_pass_rate">>, <<"quality_gate_pass_rate">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_TEST_COVERAGE">>
            },

            #{
                key => <<"quality_gate_pass_rate">>,
                type => float,
                default => 0.95,
                description => <<"Minimum percentage of quality gates that must pass. Toyota standard is 95%.">>,
                required => false,
                examples => [
                    <<"0.95  # 95% gates must pass (default)">>,
                    <<"1.00  # 100% gates must pass (strict)">>
                ],
                valid_values => {range, 0.0, 1.0},
                related => [<<"test_pass_rate">>, <<"first_pass_yield">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => undefined
            },

            #{
                key => <<"defect_rate">>,
                type => float,
                default => 0.05,
                description => <<"Maximum acceptable defect rate (Andon events per work order). Toyota target is 5% maximum.">>,
                required => false,
                examples => [
                    <<"0.05  # 5% defect rate (default)">>,
                    <<"0.03  # 3% defect rate (strict)">>,
                    <<"0.10  # 10% defect rate (relaxed)">>
                ],
                valid_values => {range, 0.0, 1.0},
                related => [<<"first_pass_yield">>, <<"test_pass_rate">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_DEFECT_RATE">>
            },

            #{
                key => <<"first_pass_yield">>,
                type => float,
                default => 0.90,
                description => <<"Minimum percentage of work orders that pass all gates on first attempt. Toyota standard is 90%.">>,
                required => false,
                examples => [
                    <<"0.90  # 90% first pass yield (default)">>,
                    <<"0.95  # 95% first pass yield (excellent)">>
                ],
                valid_values => {range, 0.0, 1.0},
                related => [<<"defect_rate">>, <<"quality_gate_pass_rate">>],
                since => <<"0.2.0">>,
                deprecated => false,
                environment_variable => undefined
            },

            #{
                key => <<"receipts_dir">>,
                type => string,
                default => <<"priv/receipts">>,
                description => <<"Directory path for storing quality gate receipt JSON files">>,
                required => false,
                examples => [
                    <<"\"priv/receipts\"  # Default (relative to app root)">>,
                    <<"\"/var/lib/tcps/receipts\"  # Absolute path">>,
                    <<"\"./receipts\"  # Current directory">>
                ],
                valid_values => undefined,
                related => [<<"tcps_receipt">>, <<"tcps_persistence">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_RECEIPTS_DIR">>
            },

            #{
                key => <<"gate_timeout_seconds">>,
                type => integer,
                default => 300,
                description => <<"Maximum time (seconds) allowed for a single gate execution before timeout">>,
                required => false,
                examples => [
                    <<"300   # 5 minutes (default)">>,
                    <<"600   # 10 minutes">>,
                    <<"1800  # 30 minutes (long tests)">>
                ],
                valid_values => {range, 1, 3600},
                related => [],
                since => <<"0.3.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_GATE_TIMEOUT">>
            }
        ]
    }.

%%------------------------------------------------------------------------------
%% @doc Kanban Configuration Reference
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_kanban_config() -> config_section().
get_kanban_config() ->
    #{
        section => <<"kanban">>,
        description => <<"Kanban WIP (Work In Progress) limits and Heijunka leveling configuration">>,

        options => [
            #{
                key => <<"wip_limit_reliability">>,
                type => integer,
                default => 5,
                description => <<"Maximum concurrent work items in reliability bucket (bugs, production issues)">>,
                required => false,
                examples => [
                    <<"5   # 5 items (default)">>,
                    <<"10  # 10 items (larger team)">>,
                    <<"3   # 3 items (strict flow)">>
                ],
                valid_values => {range, 1, 100},
                related => [<<"wip_limit_security">>, <<"wip_limit_cost">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_WIP_RELIABILITY">>
            },

            #{
                key => <<"wip_limit_security">>,
                type => integer,
                default => 5,
                description => <<"Maximum concurrent work items in security bucket (CVE, vulnerabilities)">>,
                required => false,
                examples => [
                    <<"5   # 5 items (default)">>,
                    <<"8   # 8 items (security-focused)">>,
                    <<"3   # 3 items (minimal WIP)">>
                ],
                valid_values => {range, 1, 100},
                related => [<<"wip_limit_reliability">>, <<"wip_limit_compliance">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_WIP_SECURITY">>
            },

            #{
                key => <<"wip_limit_cost">>,
                type => integer,
                default => 5,
                description => <<"Maximum concurrent work items in cost bucket (performance, resource optimization)">>,
                required => false,
                examples => [
                    <<"5   # 5 items (default)">>,
                    <<"7   # 7 items">>
                ],
                valid_values => {range, 1, 100},
                related => [<<"wip_limit_reliability">>, <<"wip_limit_compliance">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_WIP_COST">>
            },

            #{
                key => <<"wip_limit_compliance">>,
                type => integer,
                default => 5,
                description => <<"Maximum concurrent work items in compliance bucket (legal, regulatory)">>,
                required => false,
                examples => [
                    <<"5   # 5 items (default)">>,
                    <<"10  # 10 items (compliance-heavy)">>
                ],
                valid_values => {range, 1, 100},
                related => [<<"wip_limit_reliability">>, <<"wip_limit_security">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_WIP_COMPLIANCE">>
            },

            #{
                key => <<"max_consecutive_same_bucket">>,
                type => integer,
                default => 2,
                description => <<"Maximum consecutive work orders from same bucket in Heijunka schedule (prevents batching)">>,
                required => false,
                examples => [
                    <<"2  # Max 2 consecutive (default)">>,
                    <<"1  # Strict alternation">>,
                    <<"3  # Allow 3 consecutive">>
                ],
                valid_values => {range, 1, 10},
                related => [],
                since => <<"0.2.0">>,
                deprecated => false,
                environment_variable => undefined
            },

            #{
                key => <<"enable_heijunka_leveling">>,
                type => boolean,
                default => true,
                description => <<"Enable Heijunka production leveling algorithm to prevent work batching">>,
                required => false,
                examples => [
                    <<"true   # Enable (default)">>,
                    <<"false  # Disable (FIFO queue)">>
                ],
                valid_values => [true, false],
                related => [<<"max_consecutive_same_bucket">>],
                since => <<"0.2.0">>,
                deprecated => false,
                environment_variable => undefined
            }
        ]
    }.

%%------------------------------------------------------------------------------
%% @doc Andon Configuration Reference
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_andon_config() -> config_section().
get_andon_config() ->
    #{
        section => <<"andon">>,
        description => <<"Andon stop-the-line event configuration and severity levels">>,

        options => [
            #{
                key => <<"severity_critical">>,
                type => list,
                default => [shacl_violation, compilation_failure, non_determinism],
                description => <<"Failure types classified as critical severity (blocks all progression)">>,
                required => false,
                examples => [
                    <<"[shacl_violation, compilation_failure, non_determinism]  # Default">>,
                    <<"[shacl_violation, compilation_failure]  # Without non_determinism">>
                ],
                valid_values => undefined,
                related => [<<"severity_warning">>, <<"severity_info">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => undefined
            },

            #{
                key => <<"severity_warning">>,
                type => list,
                default => [test_failure, missing_receipt],
                description => <<"Failure types classified as warning severity (requires review but may allow progression)">>,
                required => false,
                examples => [
                    <<"[test_failure, missing_receipt]  # Default">>,
                    <<"[test_failure]  # Only test failures as warnings">>
                ],
                valid_values => undefined,
                related => [<<"severity_critical">>, <<"severity_info">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => undefined
            },

            #{
                key => <<"auto_resolve_timeout_hours">>,
                type => integer,
                default => 0,
                description => <<"Auto-resolve Andon events after N hours (0 = disabled, requires manual resolution)">>,
                required => false,
                examples => [
                    <<"0   # Disabled (default, manual resolution required)">>,
                    <<"24  # Auto-resolve after 24 hours">>,
                    <<"72  # Auto-resolve after 3 days">>
                ],
                valid_values => {range, 0, 720},
                related => [],
                since => <<"0.3.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_ANDON_AUTO_RESOLVE">>
            },

            #{
                key => <<"enable_dashboard_broadcast">>,
                type => boolean,
                default => true,
                description => <<"Broadcast Andon events to dashboard via SSE (Server-Sent Events)">>,
                required => false,
                examples => [
                    <<"true   # Enable dashboard updates (default)">>,
                    <<"false  # Disable dashboard broadcasts">>
                ],
                valid_values => [true, false],
                related => [],
                since => <<"0.2.0">>,
                deprecated => false,
                environment_variable => undefined
            },

            #{
                key => <<"receipts_storage_path">>,
                type => string,
                default => <<"priv/receipts/andon">>,
                description => <<"Directory for storing Andon event receipt JSON files">>,
                required => false,
                examples => [
                    <<"\"priv/receipts/andon\"  # Default">>,
                    <<"\"/var/lib/tcps/receipts/andon\"  # Production path">>
                ],
                valid_values => undefined,
                related => [<<"receipts_dir">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_ANDON_RECEIPTS_DIR">>
            }
        ]
    }.

%%------------------------------------------------------------------------------
%% @doc Receipt Configuration Reference
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_receipt_config() -> config_section().
get_receipt_config() ->
    #{
        section => <<"receipts">>,
        description => <<"Receipt generation and storage configuration for audit trail">>,

        options => [
            #{
                key => <<"storage_backend">>,
                type => atom,
                default => filesystem,
                description => <<"Receipt storage backend (filesystem, rdf, database)">>,
                required => false,
                examples => [
                    <<"filesystem  # Store as JSON files (default)">>,
                    <<"rdf         # Store in RDF ontology">>,
                    <<"database    # Store in database">>
                ],
                valid_values => [filesystem, rdf, database],
                related => [<<"storage_path">>, <<"enable_ontology_linking">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_RECEIPT_BACKEND">>
            },

            #{
                key => <<"storage_path">>,
                type => string,
                default => <<"priv/receipts">>,
                description => <<"Base directory for receipt storage (filesystem backend only)">>,
                required => false,
                examples => [
                    <<"\"priv/receipts\"  # Default">>,
                    <<"\"/var/lib/tcps/receipts\"  # Production">>,
                    <<"\"./receipts\"  # Current directory">>
                ],
                valid_values => undefined,
                related => [<<"storage_backend">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_RECEIPTS_PATH">>
            },

            #{
                key => <<"enable_ontology_linking">>,
                type => boolean,
                default => true,
                description => <<"Include RDF ontology references in receipts for semantic linking">>,
                required => false,
                examples => [
                    <<"true   # Include ontology refs (default)">>,
                    <<"false  # Plain JSON only">>
                ],
                valid_values => [true, false],
                related => [<<"ontology_base_uri">>],
                since => <<"0.2.0">>,
                deprecated => false,
                environment_variable => undefined
            },

            #{
                key => <<"compress_receipts">>,
                type => boolean,
                default => false,
                description => <<"Compress receipt JSON with gzip to save disk space">>,
                required => false,
                examples => [
                    <<"false  # No compression (default)">>,
                    <<"true   # Gzip compression">>
                ],
                valid_values => [true, false],
                related => [<<"storage_path">>],
                since => <<"0.3.0">>,
                deprecated => false,
                environment_variable => undefined
            },

            #{
                key => <<"retention_days">>,
                type => integer,
                default => 365,
                description => <<"Number of days to retain receipts before archival/deletion (0 = keep forever)">>,
                required => false,
                examples => [
                    <<"365  # 1 year (default)">>,
                    <<"90   # 90 days">>,
                    <<"0    # Keep forever">>
                ],
                valid_values => {range, 0, 3650},
                related => [],
                since => <<"0.3.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_RECEIPT_RETENTION">>
            }
        ]
    }.

%%------------------------------------------------------------------------------
%% @doc Ontology Configuration Reference
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_ontology_config() -> config_section().
get_ontology_config() ->
    #{
        section => <<"ontology">>,
        description => <<"RDF ontology and semantic web configuration">>,

        options => [
            #{
                key => <<"base_uri">>,
                type => string,
                default => <<"http://example.org/tcps/ontology#">>,
                description => <<"Base URI for TCPS RDF ontology namespace">>,
                required => false,
                examples => [
                    <<"\"http://example.org/tcps/ontology#\"  # Default">>,
                    <<"\"https://tcps.company.com/ontology#\"  # Production">>
                ],
                valid_values => undefined,
                related => [<<"enable_ontology_linking">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_ONTOLOGY_BASE_URI">>
            },

            #{
                key => <<"ttl_storage_path">>,
                type => string,
                default => <<"priv/ontology">>,
                description => <<"Directory for storing TTL (Turtle) ontology files">>,
                required => false,
                examples => [
                    <<"\"priv/ontology\"  # Default">>,
                    <<"\"/var/lib/tcps/ontology\"  # Production">>
                ],
                valid_values => undefined,
                related => [<<"base_uri">>],
                since => <<"0.1.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_ONTOLOGY_PATH">>
            },

            #{
                key => <<"enable_shacl_validation">>,
                type => boolean,
                default => true,
                description => <<"Enable SHACL (Shapes Constraint Language) validation for ontology conformance">>,
                required => false,
                examples => [
                    <<"true   # Enable SHACL validation (default)">>,
                    <<"false  # Disable validation">>
                ],
                valid_values => [true, false],
                related => [<<"shacl_shapes_path">>],
                since => <<"0.2.0">>,
                deprecated => false,
                environment_variable => undefined
            },

            #{
                key => <<"shacl_shapes_path">>,
                type => string,
                default => <<"priv/ontology/shapes">>,
                description => <<"Directory containing SHACL shape definitions for validation">>,
                required => false,
                examples => [
                    <<"\"priv/ontology/shapes\"  # Default">>,
                    <<"\"/etc/tcps/shapes\"  # System path">>
                ],
                valid_values => undefined,
                related => [<<"enable_shacl_validation">>],
                since => <<"0.2.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_SHACL_SHAPES">>
            }
        ]
    }.

%%------------------------------------------------------------------------------
%% @doc MCP Server Configuration Reference
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_mcp_config() -> config_section().
get_mcp_config() ->
    #{
        section => <<"mcp_server">>,
        description => <<"MCP (Model Context Protocol) server configuration for Diataxis documentation">>,

        options => [
            #{
                key => <<"enable_server">>,
                type => boolean,
                default => true,
                description => <<"Enable MCP server for serving Diataxis documentation">>,
                required => false,
                examples => [
                    <<"true   # Enable MCP server (default)">>,
                    <<"false  # Disable server">>
                ],
                valid_values => [true, false],
                related => [<<"server_port">>, <<"server_host">>],
                since => <<"0.4.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_MCP_ENABLE">>
            },

            #{
                key => <<"server_host">>,
                type => string,
                default => <<"localhost">>,
                description => <<"Host address for MCP server">>,
                required => false,
                examples => [
                    <<"\"localhost\"  # Local only (default)">>,
                    <<"\"0.0.0.0\"    # All interfaces">>,
                    <<"\"127.0.0.1\"  # Loopback only">>
                ],
                valid_values => undefined,
                related => [<<"server_port">>],
                since => <<"0.4.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_MCP_HOST">>
            },

            #{
                key => <<"server_port">>,
                type => integer,
                default => 8080,
                description => <<"TCP port for MCP server">>,
                required => false,
                examples => [
                    <<"8080  # Default port">>,
                    <<"3000  # Alternative port">>,
                    <<"9090  # Custom port">>
                ],
                valid_values => {range, 1024, 65535},
                related => [<<"server_host">>],
                since => <<"0.4.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_MCP_PORT">>
            },

            #{
                key => <<"cache_ttl_seconds">>,
                type => integer,
                default => 300,
                description => <<"Cache TTL (Time To Live) for generated documentation in seconds">>,
                required => false,
                examples => [
                    <<"300   # 5 minutes (default)">>,
                    <<"600   # 10 minutes">>,
                    <<"0     # Disable caching">>
                ],
                valid_values => {range, 0, 3600},
                related => [],
                since => <<"0.4.0">>,
                deprecated => false,
                environment_variable => <<"TCPS_MCP_CACHE_TTL">>
            }
        ]
    }.

%%%=============================================================================
%%% Formatting Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Format configuration section as Markdown.
%%
%% @end
%%------------------------------------------------------------------------------
-spec format_config_markdown(config_section()) -> binary().
format_config_markdown(#{section := Section, description := Desc, options := Options}) ->
    Header = [
        <<"## ">>, Section, <<" Configuration\n\n">>,
        Desc, <<"\n\n">>,
        <<"### Configuration Options\n\n">>
    ],

    OptionDocs = [format_option_markdown(O) || O <- Options],

    iolist_to_binary([Header, OptionDocs]).

format_option_markdown(#{key := Key, type := Type, default := Default,
                        description := Desc, examples := Examples}) ->
    DefaultStr = format_default(Default),
    TypeStr = atom_to_binary(Type, utf8),

    [
        <<"#### `">>, Key, <<"`\n\n">>,
        <<"**Type:** ">>, TypeStr, <<"\n\n">>,
        <<"**Default:** `">>, DefaultStr, <<"`\n\n">>,
        Desc, <<"\n\n">>,
        <<"**Examples:**\n">>,
        <<"```erlang\n">>,
        lists:join(<<"\n">>, Examples),
        <<"\n```\n\n">>,
        <<"---\n\n">>
    ].

format_default(Default) when is_atom(Default) ->
    atom_to_binary(Default, utf8);
format_default(Default) when is_binary(Default) ->
    Default;
format_default(Default) when is_integer(Default) ->
    integer_to_binary(Default);
format_default(Default) when is_float(Default) ->
    float_to_binary(Default, [{decimals, 2}]);
format_default(Default) when is_list(Default) ->
    iolist_to_binary(io_lib:format("~p", [Default]));
format_default(Default) ->
    iolist_to_binary(io_lib:format("~p", [Default])).

%%------------------------------------------------------------------------------
%% @doc Format configuration section as JSON.
%%
%% @end
%%------------------------------------------------------------------------------
-spec format_config_json([config_section()]) -> binary().
format_config_json(Sections) ->
    jsx:encode(Sections).

%%------------------------------------------------------------------------------
%% @doc Validate configuration value against option spec.
%%
%% @end
%%------------------------------------------------------------------------------
-spec validate_config(config_option(), term()) -> ok | {error, term()}.
validate_config(#{type := Type, valid_values := ValidValues}, Value) ->
    case validate_type(Type, Value) of
        ok ->
            validate_valid_values(ValidValues, Value);
        Error ->
            Error
    end.

validate_type(integer, V) when is_integer(V) -> ok;
validate_type(float, V) when is_float(V) -> ok;
validate_type(boolean, V) when is_boolean(V) -> ok;
validate_type(string, V) when is_binary(V) -> ok;
validate_type(atom, V) when is_atom(V) -> ok;
validate_type(list, V) when is_list(V) -> ok;
validate_type(map, V) when is_map(V) -> ok;
validate_type(Type, _V) -> {error, {invalid_type, Type}}.

validate_valid_values(undefined, _Value) ->
    ok;
validate_valid_values({range, Min, Max}, Value) when Value >= Min, Value =< Max ->
    ok;
validate_valid_values({range, Min, Max}, Value) ->
    {error, {out_of_range, Value, Min, Max}};
validate_valid_values(ValidList, Value) when is_list(ValidList) ->
    case lists:member(Value, ValidList) of
        true -> ok;
        false -> {error, {invalid_value, Value, ValidList}}
    end.
