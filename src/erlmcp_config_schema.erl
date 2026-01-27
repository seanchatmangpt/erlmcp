%% @doc Configuration Schema Module
%% Defines the configuration schema, validation rules, and type specifications
%% for the erlmcp configuration system.
%%
%% Features:
%% - JSON Schema-style validation
%% - Type checking and conversion
%% - Required/optional field validation
%% - Custom validation rules
%% - Migration support
-module(erlmcp_config_schema).

-export([
    validate/1,
    get_schema/0,
    get_defaults/0,
    migrate_schema/2,
    validate_type/3,
    format_validation_error/1
]).

-include("erlmcp.hrl").

%% Schema definition
-define(CONFIG_SCHEMA, #{
    type => object,
    properties => #{
        server_name => #{
            type => atom,
            required => false,
            default => erlmcp_server,
            description => "Server name for registration"
        },
        port => #{
            type => integer,
            required => false,
            default => 8080,
            minimum => 1,
            maximum => 65535,
            description => "Server port number"
        },
        max_connections => #{
            type => integer,
            required => false,
            default => 100,
            minimum => 1,
            description => "Maximum number of concurrent connections"
        },
        timeout => #{
            type => integer,
            required => false,
            default => 5000,
            minimum => 100,
            description => "Connection timeout in milliseconds"
        },
        acceptors => #{
            type => integer,
            required => false,
            default => 10,
            minimum => 1,
            description => "Number of acceptor processes"
        },
        socket_opts => #{
            type => list,
            required => false,
            default => [{reuseaddr, true}],
            description => "Socket options"
        },
        protocol_opts => #{
            type => list,
            required => false,
            default => [],
            description => "Protocol options"
        },
        transports => #{
            type => list,
            required => false,
            default => [
                {stdio, #{enabled => true, buffer_size => 1024, timeout => 5000}},
                {tcp, #{enabled => false, port => 8080, acceptors => 10}},
                {http, #{enabled => false, port => 8081, path => "/mcp"}}
            ],
            items => #{
                type => tuple,
                properties => [atom, map]
            },
            description => "Transport configurations"
        },
        log_level => #{
            type => atom,
            required => false,
            default => info,
            enum => [debug, info, warning, error],
            description => "Logging level"
        },
        log_file => #{
            type => [atom, string],
            required => false,
            default => undefined,
            description => "Log file path (undefined for console logging)"
        },
        metrics_enabled => #{
            type => boolean,
            required => false,
            default => true,
            description => "Enable metrics collection"
        },
        tracing_enabled => #{
            type => boolean,
            required => false,
            default => false,
            description => "Enable tracing"
        }
    },
    additional_properties => true
}).

%% Transport schemas
-define(STDIO_TRANSPORT_SCHEMA, #{
    type => object,
    properties => #{
        enabled => #{type => boolean, default => true},
        buffer_size => #{type => integer, minimum => 1, default => 1024},
        timeout => #{type => integer, minimum => 100, default => 5000},
        encoding => #{type => atom, enum => [utf8, latin1], default => utf8}
    }
}).

-define(TCP_TRANSPORT_SCHEMA, #{
    type => object,
    properties => #{
        enabled => #{type => boolean, default => false},
        port => #{type => integer, minimum => 1, maximum => 65535, default => 8080},
        acceptors => #{type => integer, minimum => 1, default => 10},
        socket_opts => #{type => list, default => [{reuseaddr, true}]},
        backlog => #{type => integer, minimum => 1, default => 1024}
    }
}).

-define(HTTP_TRANSPORT_SCHEMA, #{
    type => object,
    properties => #{
        enabled => #{type => boolean, default => false},
        port => #{type => integer, minimum => 1, maximum => 65535, default => 8081},
        path => #{type => string, default => "/mcp"},
        ssl_opts => #{type => list, default => []},
        cors_enabled => #{type => boolean, default => true}
    }
}).

%%--------------------------------------------------------------------
%% API Functions
%%--------------------------------------------------------------------

%% @doc Validate configuration against schema
-spec validate(map() | list()) -> ok | {error, term()}.
validate(Config) when is_list(Config) ->
    validate(maps:from_list(Config));
validate(Config) when is_map(Config) ->
    try
        validate_object(Config, ?CONFIG_SCHEMA)
    catch
        throw:Error -> Error;
        Class:Reason:Stacktrace ->
            {error, {validation_exception, Class, Reason, Stacktrace}}
    end;
validate(_Config) ->
    {error, {type_error, "Configuration must be a map or property list"}}.

%% @doc Get the complete configuration schema
-spec get_schema() -> map().
get_schema() ->
    ?CONFIG_SCHEMA.

%% @doc Get default configuration values
-spec get_defaults() -> map().
get_defaults() ->
    extract_defaults(?CONFIG_SCHEMA).

%% @doc Migrate schema from old version to new version
-spec migrate_schema(map(), string()) -> {ok, map()} | {error, term()}.
migrate_schema(Config, FromVersion) ->
    try
        case FromVersion of
            "1.0" -> migrate_from_1_0(Config);
            "2.0" -> migrate_from_2_0(Config);
            _ -> {ok, Config}
        end
    catch
        Class:Reason:Stacktrace ->
            {error, {migration_failed, Class, Reason, Stacktrace}}
    end.

%% @doc Validate a specific type
-spec validate_type(term(), atom(), map()) -> ok | {error, term()}.
validate_type(Value, Type, Constraints) ->
    try
        validate_value_type(Value, Type, Constraints)
    catch
        throw:Error -> Error
    end.

%% @doc Format validation error for human consumption
-spec format_validation_error(term()) -> string().
format_validation_error({type_error, Field, Expected, Got}) ->
    io_lib:format("Field '~p': expected ~p, got ~p", [Field, Expected, Got]);
format_validation_error({missing_required_field, Field}) ->
    io_lib:format("Missing required field: ~p", [Field]);
format_validation_error({enum_error, Field, Value, Allowed}) ->
    io_lib:format("Field '~p': value ~p not in allowed values ~p", [Field, Value, Allowed]);
format_validation_error({range_error, Field, Value, Min, Max}) ->
    io_lib:format("Field '~p': value ~p not in range [~p, ~p]", [Field, Value, Min, Max]);
format_validation_error({validation_failed, Reason}) ->
    io_lib:format("Validation failed: ~p", [Reason]);
format_validation_error(Error) ->
    io_lib:format("Validation error: ~p", [Error]).

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

%% @doc Validate object against schema
validate_object(Object, Schema) when is_map(Object), is_map(Schema) ->
    %% Check required fields and validate each property
    Properties = maps:get(properties, Schema, #{}),
    
    %% First pass: check required fields
    RequiredFields = get_required_fields(Properties),
    case check_required_fields(RequiredFields, Object, Properties) of
        ok -> 
            %% Second pass: validate all present fields
            validate_object_properties(Object, Properties);
        Error -> 
            throw(Error)
    end.

%% @doc Get list of required fields from schema properties
get_required_fields(Properties) ->
    maps:fold(fun(Field, FieldSchema, Acc) ->
        case maps:get(required, FieldSchema, false) of
            true -> [Field | Acc];
            false -> Acc
        end
    end, [], Properties).

%% @doc Check if required fields are present
check_required_fields([], _Object, _Properties) -> 
    ok;
check_required_fields([Field | Rest], Object, Properties) ->
    case maps:is_key(Field, Object) of
        true -> 
            check_required_fields(Rest, Object, Properties);
        false -> 
            %% Check if field has default value
            FieldSchema = maps:get(Field, Properties, #{}),
            case maps:is_key(default, FieldSchema) of
                true -> check_required_fields(Rest, Object, Properties);
                false -> {error, {missing_required_field, Field}}
            end
    end.

%% @doc Validate all object properties
validate_object_properties(Object, Properties) ->
    maps:fold(fun(Field, Value, ok) ->
        case maps:get(Field, Properties, undefined) of
            undefined ->
                %% Unknown field - allow if additional_properties is true
                ok;
            FieldSchema ->
                case validate_field_value(Field, Value, FieldSchema) of
                    ok -> ok;
                    Error -> throw(Error)
                end
        end;
    (_, _, Error) -> 
        throw(Error)
    end, ok, Object).

%% @doc Validate a field value against its schema
validate_field_value(Field, Value, FieldSchema) ->
    Type = maps:get(type, FieldSchema, any),
    case validate_value_type(Value, Type, FieldSchema) of
        ok -> 
            validate_constraints(Field, Value, FieldSchema);
        Error -> 
            Error
    end.

%% @doc Validate value type
validate_value_type(_Value, any, _Constraints) -> 
    ok;
validate_value_type(Value, atom, _Constraints) when is_atom(Value) -> 
    ok;
validate_value_type(Value, integer, _Constraints) when is_integer(Value) -> 
    ok;
validate_value_type(Value, float, _Constraints) when is_float(Value) -> 
    ok;
validate_value_type(Value, number, _Constraints) when is_number(Value) -> 
    ok;
validate_value_type(Value, boolean, _Constraints) when is_boolean(Value) -> 
    ok;
validate_value_type(Value, string, _Constraints) when is_list(Value) -> 
    ok;
validate_value_type(Value, binary, _Constraints) when is_binary(Value) -> 
    ok;
validate_value_type(Value, list, Constraints) when is_list(Value) -> 
    case maps:get(items, Constraints, undefined) of
        undefined -> ok;
        ItemSchema -> validate_list_items(Value, ItemSchema)
    end;
validate_value_type(Value, map, _Constraints) when is_map(Value) -> 
    ok;
validate_value_type(Value, object, Constraints) when is_map(Value) -> 
    %% Recursively validate nested object
    validate_object(Value, Constraints);
validate_value_type(Value, tuple, Constraints) when is_tuple(Value) ->
    validate_tuple(Value, Constraints);
validate_value_type(Value, Types, Constraints) when is_list(Types) ->
    %% Union type - value must match at least one type
    case lists:any(fun(Type) ->
        validate_value_type(Value, Type, Constraints) =:= ok
    end, Types) of
        true -> ok;
        false -> throw({error, {type_error, union, Types, typeof(Value)}})
    end;
validate_value_type(Value, Type, _Constraints) ->
    throw({error, {type_error, Type, typeof(Value), Value}}).

%% @doc Validate list items
validate_list_items([], _ItemSchema) -> 
    ok;
validate_list_items([Item | Rest], ItemSchema) ->
    case validate_list_item(Item, ItemSchema) of
        ok -> validate_list_items(Rest, ItemSchema);
        Error -> Error
    end.

validate_list_item(Item, #{type := tuple, properties := [Type1, Type2]}) when is_tuple(Item) ->
    case tuple_size(Item) of
        2 ->
            case {validate_value_type(element(1, Item), Type1, #{}),
                  validate_value_type(element(2, Item), Type2, #{})} of
                {ok, ok} -> ok;
                {Error, _} -> Error;
                {_, Error} -> Error
            end;
        _ ->
            throw({error, {type_error, "2-tuple", tuple_size(Item), Item}})
    end;
validate_list_item(Item, ItemSchema) ->
    Type = maps:get(type, ItemSchema, any),
    validate_value_type(Item, Type, ItemSchema).

%% @doc Validate tuple structure
validate_tuple(Tuple, Constraints) ->
    Properties = maps:get(properties, Constraints, []),
    Size = tuple_size(Tuple),
    case length(Properties) of
        0 -> ok; % No specific structure required
        Size -> validate_tuple_elements(Tuple, Properties, 1);
        Expected -> throw({error, {tuple_size_mismatch, Expected, Size}})
    end.

validate_tuple_elements(_Tuple, [], _Index) -> 
    ok;
validate_tuple_elements(Tuple, [Type | Rest], Index) ->
    Element = element(Index, Tuple),
    case validate_value_type(Element, Type, #{}) of
        ok -> validate_tuple_elements(Tuple, Rest, Index + 1);
        Error -> Error
    end.

%% @doc Validate field constraints
validate_constraints(Field, Value, Constraints) ->
    %% Check enum constraint
    case maps:get(enum, Constraints, undefined) of
        undefined -> ok;
        AllowedValues ->
            case lists:member(Value, AllowedValues) of
                true -> ok;
                false -> throw({error, {enum_error, Field, Value, AllowedValues}})
            end
    end,
    
    %% Check range constraints for numbers
    case is_number(Value) of
        true -> validate_number_constraints(Field, Value, Constraints);
        false -> ok
    end,
    
    %% Check length constraints for strings and lists
    case is_list(Value) orelse is_binary(Value) of
        true -> validate_length_constraints(Field, Value, Constraints);
        false -> ok
    end.

%% @doc Validate number constraints (min/max)
validate_number_constraints(Field, Value, Constraints) ->
    case maps:get(minimum, Constraints, undefined) of
        undefined -> ok;
        Min when Value >= Min -> ok;
        Min -> throw({error, {range_error, Field, Value, Min, infinity}})
    end,
    case maps:get(maximum, Constraints, undefined) of
        undefined -> ok;
        Max when Value =< Max -> ok;
        Max -> throw({error, {range_error, Field, Value, -infinity, Max}})
    end.

%% @doc Validate length constraints
validate_length_constraints(Field, Value, Constraints) ->
    Length = case is_binary(Value) of
        true -> byte_size(Value);
        false -> length(Value)
    end,
    case maps:get(min_length, Constraints, undefined) of
        undefined -> ok;
        MinLen when Length >= MinLen -> ok;
        MinLen -> throw({error, {length_error, Field, Length, MinLen, infinity}})
    end,
    case maps:get(max_length, Constraints, undefined) of
        undefined -> ok;
        MaxLen when Length =< MaxLen -> ok;
        MaxLen -> throw({error, {length_error, Field, Length, -infinity, MaxLen}})
    end.

%% @doc Get type of a value
typeof(Value) when is_atom(Value) -> atom;
typeof(Value) when is_integer(Value) -> integer;
typeof(Value) when is_float(Value) -> float;
typeof(Value) when is_boolean(Value) -> boolean;
typeof(Value) when is_list(Value) -> list;
typeof(Value) when is_binary(Value) -> binary;
typeof(Value) when is_map(Value) -> map;
typeof(Value) when is_tuple(Value) -> tuple;
typeof(Value) when is_pid(Value) -> pid;
typeof(Value) when is_reference(Value) -> reference;
typeof(Value) when is_function(Value) -> function;
typeof(_Value) -> unknown.

%% @doc Extract default values from schema
extract_defaults(Schema) ->
    Properties = maps:get(properties, Schema, #{}),
    maps:fold(fun(Field, FieldSchema, Acc) ->
        case maps:get(default, FieldSchema, undefined) of
            undefined -> Acc;
            Default -> maps:put(Field, Default, Acc)
        end
    end, #{}, Properties).

%% @doc Migrate configuration from version 1.0
migrate_from_1_0(Config) ->
    Mappings = #{
        name => server_name,
        tcp_port => port,
        connections => max_connections,
        conn_timeout => timeout
    },
    {ok, migrate_keys(Config, Mappings)}.

%% @doc Migrate configuration from version 2.0
migrate_from_2_0(Config) ->
    %% Version 2.0 is mostly compatible, just ensure transport format
    case maps:get(transports, Config, undefined) of
        undefined -> {ok, Config};
        Transports when is_list(Transports) -> {ok, Config};
        TransportsMap when is_map(TransportsMap) ->
            %% Convert map format to list format
            TransportsList = maps:to_list(TransportsMap),
            {ok, maps:put(transports, TransportsList, Config)}
    end.

%% @doc Apply key mappings for migration
migrate_keys(Config, Mappings) ->
    maps:fold(fun(OldKey, NewKey, Acc) ->
        case maps:get(OldKey, Config, undefined) of
            undefined -> Acc;
            Value -> 
                Acc1 = maps:remove(OldKey, Acc),
                maps:put(NewKey, Value, Acc1)
        end
    end, Config, Mappings).