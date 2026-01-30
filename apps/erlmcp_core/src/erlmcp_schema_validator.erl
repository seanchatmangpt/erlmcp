%%%-------------------------------------------------------------------
%%% @doc
%%% JSON Schema 2020-12 Validator for erlmcp
%%%
%%% Implements JSON Schema 2020-12 specification with MCP extensions:
%%% - SEP-1034: Default Values in Primitive Types
%%% - SEP-1330: Enhanced Enum Schema Support
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_schema_validator).
-behaviour(gen_server).

%% API exports
-export([
    start_link/1,
    validate/3,
    validate_async/3,
    validate_regex/2,
    validate_range/3,
    validate_dependencies/2,
    do_validate/2,
    apply_defaults/2,
    validate_type/2,
    validate_required/2,
    validate_properties/2,
    validate_enum/2,
    format_jesse_error/1,
    format_jesse_errors/1,
    format_path/1,
    format_error_message/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type validation_result() :: ok | {error, [validation_error()]}.
-type validation_error() :: #{
    path := [binary()],
    message := binary(),
    expected := term(),
    actual := term()
}.

-export_type([validation_result/0, validation_error/0]).

%% State record
-record(state, {
    worker_id :: integer()
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(term()) -> {ok, pid()} | {error, term()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec validate(pid(), map(), term()) -> validation_result().
validate(Worker, Schema, Data) ->
    gen_server:call(Worker, {validate, Schema, Data}, 5000).

-spec validate_async(pid(), map(), term()) -> ok.
validate_async(Worker, Schema, Data) ->
    gen_server:cast(Worker, {validate_async, Schema, Data, self()}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(term()) -> {ok, state()}.
init(_Args) ->
    WorkerId = erlang:unique_integer([positive]),
    {ok, #state{worker_id = WorkerId}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.

handle_call({validate, Schema, Data}, _From, State) ->
    Result = do_validate(Schema, Data),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast({validate_async, Schema, Data, ReplyTo}, State) ->
    Result = do_validate(Schema, Data),
    ReplyTo ! {validation_result, Result},
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Validation
%%====================================================================

%% @doc Validate data against JSON Schema using jesse
-spec do_validate(map(), term()) -> validation_result().
do_validate(Schema, Data) ->
    case jesse:validate_with_schema(Schema, Data, [{allowed_errors, infinity}]) of
        {ok, _} ->
            ok;
        {error, Errors} ->
            FormattedErrors = format_jesse_errors(Errors),
            {error, FormattedErrors}
    end.

%% @doc Apply default values from schema (SEP-1034)
-spec apply_defaults(term(), map()) -> term().
apply_defaults(Data, Schema) when is_map(Data), is_map(Schema) ->
    Type = maps:get(<<"type">>, Schema, undefined),

    case Type of
        <<"object">> ->
            apply_object_defaults(Data, Schema);
        <<"array">> ->
            apply_array_defaults(Data, Schema);
        _ ->
            apply_primitive_default(Data, Schema)
    end;
apply_defaults(Data, _Schema) ->
    Data.

%% @doc Apply defaults for object properties
apply_object_defaults(Data, Schema) ->
    Properties = maps:get(<<"properties">>, Schema, #{}),
    RequiredSet = sets:from_list(maps:get(<<"required">>, Schema, [])),

    % Apply default values to properties
    DataWithDefaults = maps:fold(fun(PropertyName, PropertySchema, Acc) ->
        IsRequired = sets:is_element(PropertyName, RequiredSet),
        case {maps:is_key(PropertyName, Data), IsRequired} of
            {false, false} ->
                % Optional property not in data, check for default
                case maps:get(<<"default">>, PropertySchema, undefined) of
                    undefined -> Acc;
                    Default -> maps:put(PropertyName, Default, Acc)
                end;
            {true, _} ->
                % Property exists, recursively apply defaults
                case maps:get(PropertyName, Data, undefined) of
                    undefined -> Acc;
                    Value ->
                        ValueWithDefaults = apply_defaults(Value, PropertySchema),
                        maps:put(PropertyName, ValueWithDefaults, Acc)
                end;
            {false, true} ->
                % Required property missing - don't add default
                Acc
        end
    end, Data, Properties),

    DataWithDefaults.

%% @doc Apply defaults for array items
apply_array_defaults(Data, Schema) when is_list(Data) ->
    ItemsSchema = maps:get(<<"items">>, Schema, undefined),
    DefaultItem = maps:get(<<"default">>, Schema, undefined),

    case {ItemsSchema, DefaultItem} of
        {undefined, undefined} ->
            Data;
        {_, undefined} ->
            % Apply defaults to each item
            [apply_defaults(Item, ItemsSchema) || Item <- Data];
        {undefined, _} ->
            % Array-level default (rare, but allowed in JSON Schema)
            case Data of
                [] -> DefaultItem;
                _ -> Data
            end
    end;
apply_array_defaults(Data, _Schema) ->
    Data.

%% @doc Apply default value for primitive types
apply_primitive_default(Data, Schema) ->
    case Data of
        undefined ->
            maps:get(<<"default">>, Schema, undefined);
        _ ->
            Data
    end.

%% @doc Validate type (JSON Schema 2020-12 core validation)
-spec validate_type(term(), binary() | [binary()]) -> ok | {error, binary()}.
validate_type(Value, ExpectedType) when is_binary(ExpectedType) ->
    validate_type(Value, [ExpectedType]);
validate_type(Value, ExpectedTypes) when is_list(ExpectedTypes) ->
    TypeMatches = lists:any(fun(ExpectedType) ->
        type_matches(Value, ExpectedType)
    end, ExpectedTypes),

    case TypeMatches of
        true -> ok;
        false ->
            ActualType = get_type(Value),
            {error, iolist_to_binary(io_lib:format(
                "Type mismatch: expected ~p, got ~s",
                [ExpectedTypes, ActualType]
            ))}
    end.

%% @doc Check if value matches type
type_matches(Value, <<"string">>) when is_binary(Value) -> true;
type_matches(Value, <<"number">>) when is_number(Value) -> true;
type_matches(Value, <<"integer">>) when is_integer(Value) -> true;
type_matches(Value, <<"boolean">>) when is_boolean(Value) -> true;
type_matches(Value, <<"null">>) when Value =:= null -> true;
type_matches(Value, <<"array">>) when is_list(Value) -> true;
type_matches(Value, <<"object">>) when is_map(Value) -> true;
type_matches(_Value, _Type) -> false.

%% @doc Get JSON type of value
get_type(Value) when is_binary(Value) -> <<"string">>;
get_type(Value) when is_integer(Value) -> <<"integer">>;
get_type(Value) when is_float(Value) -> <<"number">>;
get_type(Value) when is_boolean(Value) -> <<"boolean">>;
get_type(Value) when Value =:= null -> <<"null">>;
get_type(Value) when is_list(Value) -> <<"array">>;
get_type(Value) when is_map(Value) -> <<"object">>;
get_type(_Value) -> <<"unknown">>.

%% @doc Validate required fields (JSON Schema 2020-12)
-spec validate_required(map(), [binary()]) -> ok | {error, [binary()]}.
validate_required(Object, RequiredFields) when is_map(Object), is_list(RequiredFields) ->
    Missing = lists:filter(fun(Field) ->
        not maps:is_key(Field, Object)
    end, RequiredFields),

    case Missing of
        [] -> ok;
        _ -> {error, Missing}
    end.

%% @doc Validate object properties against schemas
-spec validate_properties(map(), map()) -> ok | {error, [validation_error()]}.
validate_properties(Object, PropertySchemas) when is_map(Object), is_map(PropertySchemas) ->
    Errors = lists:foldl(fun({PropertyName, PropertySchema}, Acc) ->
        case maps:get(PropertyName, Object, undefined) of
            undefined ->
                Acc;
            Value ->
                case do_validate(PropertySchema, Value) of
                    ok -> Acc;
                    {error, PropErrors} ->
                        FormattedErrors = lists:map(fun(Err) ->
                            Path = [PropertyName | maps:get(path, Err, [])],
                            Err#{path => Path}
                        end, PropErrors),
                        FormattedErrors ++ Acc
                end
        end
    end, [], maps:to_list(PropertySchemas)),

    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

%% @doc Validate enum values (SEP-1330: Enhanced Enum Schema Support)
-spec validate_enum(term(), [term()]) -> ok | {error, binary()}.
validate_enum(Value, EnumValues) when is_list(EnumValues) ->
    case lists:member(Value, EnumValues) of
        true -> ok;
        false ->
            {error, iolist_to_binary(io_lib:format(
                "Value ~p not in enum: ~p",
                [Value, EnumValues]
            ))}
    end.

%% @doc Format Jesse validation errors
-spec format_jesse_errors(term()) -> [validation_error()].
format_jesse_errors(Errors) when is_list(Errors) ->
    lists:map(fun format_jesse_error/1, Errors);
format_jesse_errors(Error) ->
    [format_jesse_error(Error)].

%% @doc Format single Jesse error (CORRECTED parameter order)
%% Jesse error format: {data_invalid, Schema, Error, Data, Path}
-spec format_jesse_error(term()) -> validation_error().
format_jesse_error({data_invalid, Schema, Error, Data, Path}) ->
    #{
        path => format_path(Path),
        message => format_error_message(Error),
        expected => Schema,
        actual => Data
    };
format_jesse_error({schema_invalid, Schema, Error}) ->
    #{
        path => [],
        message => format_error_message(Error),
        expected => Schema,
        actual => schema_error
    };
format_jesse_error({data_error, {parse_error, Reason}}) ->
    #{
        path => [],
        message => iolist_to_binary(io_lib:format("Parse error: ~p", [Reason])),
        expected => valid_json,
        actual => parse_error
    };
format_jesse_error({schema_error, {parse_error, Reason}}) ->
    #{
        path => [],
        message => iolist_to_binary(io_lib:format("Schema parse error: ~p", [Reason])),
        expected => valid_schema,
        actual => parse_error
    };
format_jesse_error(_Other) ->
    #{
        path => [],
        message => iolist_to_binary(io_lib:format("Unknown validation error: ~p", [_Other])),
        expected => undefined,
        actual => undefined
    }.

%% @doc Format path as list of binaries
-spec format_path([term()]) -> [binary()].
format_path(Path) ->
    lists:map(fun
        (P) when is_binary(P) -> P;
        (P) when is_atom(P) -> atom_to_binary(P, utf8);
        (P) when is_integer(P) -> integer_to_binary(P);
        (P) -> iolist_to_binary(io_lib:format("~p", [P]))
    end, Path).

%% @doc Format Jesse error message
-spec format_error_message(term()) -> binary().
format_error_message({missing_required_property, Property}) ->
    iolist_to_binary(io_lib:format("Missing required property: ~s", [Property]));
format_error_message({wrong_type, Expected}) ->
    iolist_to_binary(io_lib:format("Wrong type, expected: ~s", [Expected]));
format_error_message({not_in_enum, AllowedValues}) ->
    iolist_to_binary(io_lib:format("Value not in enum: ~p", [AllowedValues]));
format_error_message({not_unique, _}) ->
    <<"Array items must be unique">>;
format_error_message(wrong_length) ->
    <<"Array/string has wrong length">>;
format_error_message({wrong_length, Expected}) ->
    iolist_to_binary(io_lib:format("Wrong length, expected: ~p", [Expected]));
format_error_message(wrong_size) ->
    <<"Array/string has wrong size">>;
format_error_message({wrong_size, Expected}) ->
    iolist_to_binary(io_lib:format("Wrong size, expected: ~p", [Expected]));
format_error_message({missing_dependency, Dependency}) ->
    iolist_to_binary(io_lib:format("Missing dependency: ~s", [Dependency]));
format_error_message(no_match) ->
    <<"Pattern does not match">>;
format_error_message(no_extra_properties_allowed) ->
    <<"No extra properties allowed">>;
format_error_message(no_extra_items_allowed) ->
    <<"No extra items allowed">>;
format_error_message(not_allowed) ->
    <<"Value not allowed">>;
format_error_message(not_in_range) ->
    <<"Value not in allowed range">>;
format_error_message(not_divisible) ->
    <<"Value not divisible">>;
format_error_message(not_array) ->
    <<"Value is not an array">>;
format_error_message(wrong_format) ->
    <<"Value has wrong format">>;
format_error_message(too_many_properties) ->
    <<"Object has too many properties">>;
format_error_message(too_few_properties) ->
    <<"Object has too few properties">>;
format_error_message(all_schemas_not_valid) ->
    <<"All schemas failed validation">>;
format_error_message(any_schemas_not_valid) ->
    <<"No schemas validated">>;
format_error_message(not_multiple_of) ->
    <<"Value is not a multiple">>;
format_error_message(not_one_schema_valid) ->
    <<"No schema validated">>;
format_error_message(more_than_one_schema_valid) ->
    <<"More than one schema validated">>;
format_error_message(not_schema_valid) ->
    <<"Schema not valid">>;
format_error_message(validation_always_fails) ->
    <<"Validation always fails">>;
format_error_message(external) ->
    <<"External validation error">>;
format_error_message(not_found) ->
    <<"Resource not found">>;
format_error_message({ErrorType, Details}) when is_atom(ErrorType) ->
    iolist_to_binary(io_lib:format("Validation error (~p): ~p", [ErrorType, Details]));
format_error_message(Error) when is_atom(Error) ->
    iolist_to_binary(io_lib:format("Validation error: ~p", [Error]));
format_error_message(Error) ->
    iolist_to_binary(io_lib:format("Validation error: ~p", [Error])).

%%====================================================================
%% Custom Validators
%%====================================================================

%% @doc Validate string against regex pattern
-spec validate_regex(binary(), binary()) -> boolean().
validate_regex(Pattern, String) when is_binary(Pattern), is_binary(String) ->
    case re:compile(Pattern) of
        {ok, MP} ->
            case re:run(String, MP) of
                {match, _} -> true;
                nomatch -> false
            end;
        {error, _} ->
            false
    end.

%% @doc Validate number is in range [Min, Max]
-spec validate_range(number(), number(), number()) -> boolean().
validate_range(Value, Min, Max) when is_number(Value), is_number(Min), is_number(Max) ->
    Value >= Min andalso Value =< Max.

%% @doc Validate property dependencies (JSON Schema 2020-12)
-spec validate_dependencies(map(), map()) -> validation_result().
validate_dependencies(Data, Dependencies) when is_map(Data), is_map(Dependencies) ->
    Errors = maps:fold(fun(Property, RequiredProps, Acc) ->
        case maps:is_key(Property, Data) of
            true ->
                % Property exists, check dependencies
                lists:foldl(fun(RequiredProp, Acc2) ->
                    case maps:is_key(RequiredProp, Data) of
                        true -> Acc2;
                        false ->
                            Error = #{
                                path => [Property],
                                message => iolist_to_binary(
                                    io_lib:format("Property ~s requires ~s",
                                                  [Property, RequiredProp])),
                                expected => RequiredProp,
                                actual => undefined
                            },
                            [Error | Acc2]
                    end
                end, Acc, RequiredProps);
            false ->
                Acc
        end
    end, [], Dependencies),

    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.
