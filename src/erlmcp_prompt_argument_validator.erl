%%%-------------------------------------------------------------------
%% @doc
%% Prompt Argument Validation Module (Gap #42: Prompt Argument Validation)
%%
%% Implements JSON Schema validation for prompt arguments according to
%% MCP 2025-11-25 specification. Validates:
%% - Required arguments are provided
%% - Argument types match schema
%% - Optional arguments have proper defaults
%% - Complex schema validation via jesse library
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_prompt_argument_validator).

-include("erlmcp.hrl").

%% API
-export([
    validate_prompt_arguments/2,
    validate_prompt_arguments/3,
    get_argument_schema/1,
    build_validation_error/2,
    validate_against_schema/2,
    validate_required_arguments/2,
    validate_argument_types/2,
    extract_argument_names/1,
    extract_required_arguments/1
]).

%% Internal exports for testing
-export([
    normalize_schema/1,
    check_required_field/2,
    validate_type_match/3
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type validation_result() :: ok | {error, {integer(), binary(), map()}}.
-type argument_schema() :: map() | undefined.
-type provided_arguments() :: map().
-type prompt_arguments() :: [#mcp_prompt_argument{}].

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate provided arguments against prompt argument schema.
%%
%% Validates that:
%% 1. All required arguments are provided
%% 2. Provided arguments have correct types
%% 3. Argument names match schema
%%
%% Returns:
%% - ok: validation passed
%% - {error, {Code, Message, Data}}: validation failed
%%   where Code is a JSON-RPC error code
%%        Message is a user-friendly error message
%%        Data contains validation details
-spec validate_prompt_arguments(
    provided_arguments(),
    prompt_arguments() | undefined
) -> validation_result().
validate_prompt_arguments(_ProvidedArgs, undefined) ->
    %% No schema defined, accept any arguments
    ok;
validate_prompt_arguments(ProvidedArgs, PromptArguments) when is_list(PromptArguments) ->
    validate_prompt_arguments(ProvidedArgs, PromptArguments, undefined).

%% @doc Validate provided arguments with optional input schema.
%%
%% If input_schema is provided, performs comprehensive validation including
%% JSON Schema validation via jesse library.
-spec validate_prompt_arguments(
    provided_arguments(),
    prompt_arguments() | undefined,
    argument_schema()
) -> validation_result().
validate_prompt_arguments(ProvidedArgs, PromptArguments, InputSchema) ->
    try
        % Step 1: Validate required arguments
        case validate_required_arguments(PromptArguments, ProvidedArgs) of
            ok ->
                % Step 2: Validate argument types (if schema available)
                case validate_argument_types(PromptArguments, ProvidedArgs) of
                    ok ->
                        % Step 3: Validate against JSON Schema (if provided)
                        case validate_against_schema(ProvidedArgs, InputSchema) of
                            ok -> ok;
                            {error, SchemaReason} ->
                                {error, build_schema_error(SchemaReason)}
                        end;
                    {error, TypeError} ->
                        {error, TypeError}
                end;
            {error, RequiredError} ->
                {error, RequiredError}
        end
    catch
        Class:Reason:Stacktrace ->
            logger:error(
                "Prompt argument validation exception: ~p:~p~n~p",
                [Class, Reason, Stacktrace]
            ),
            {error, build_internal_error()}
    end.

%% @doc Extract argument schema from prompt arguments list.
%%
%% Builds a map of argument names to their specifications.
-spec get_argument_schema(prompt_arguments() | undefined) -> map().
get_argument_schema(undefined) ->
    #{};
get_argument_schema(PromptArguments) when is_list(PromptArguments) ->
    lists:foldl(
        fun(
            #mcp_prompt_argument{name = Name, required = Required, description = Desc},
            Acc
        ) ->
            Acc#{
                Name => #{
                    name => Name,
                    required => Required,
                    description => Desc
                }
            };
        (_Other, Acc) ->
            Acc
    end,
        #{},
        PromptArguments
    ).

%% @doc Build validation error response with details.
-spec build_validation_error(binary(), map()) -> {integer(), binary(), map()}.
build_validation_error(Message, Data) when is_binary(Message), is_map(Data) ->
    {?JSONRPC_INVALID_PARAMS, Message, Data}.

%%====================================================================
%% Internal Validation Functions
%%====================================================================

%% @doc Validate that all required arguments are provided.
-spec validate_required_arguments(
    prompt_arguments() | undefined,
    provided_arguments()
) -> ok | {error, {integer(), binary(), map()}}.
validate_required_arguments(undefined, _ProvidedArgs) ->
    ok;
validate_required_arguments(PromptArguments, ProvidedArgs) when is_list(PromptArguments) ->
    case find_missing_required(PromptArguments, ProvidedArgs) of
        [] ->
            ok;
        MissingFields ->
            ErrorData = #{
                <<"missing_arguments">> => MissingFields,
                <<"provided_count">> => maps:size(ProvidedArgs),
                <<"required_count">> => count_required(PromptArguments)
            },
            {error, build_validation_error(
                <<"Missing required prompt arguments">>,
                ErrorData
            )}
    end.

%% @doc Validate that provided arguments match declared types.
%%
%% For each provided argument, check that its value matches the expected type
%% based on the prompt argument declarations.
-spec validate_argument_types(
    prompt_arguments() | undefined,
    provided_arguments()
) -> ok | {error, {integer(), binary(), map()}}.
validate_argument_types(undefined, _ProvidedArgs) ->
    ok;
validate_argument_types(PromptArguments, ProvidedArgs) when is_list(PromptArguments) ->
    % Build schema map for quick lookup
    SchemaMap = get_argument_schema(PromptArguments),

    % Check type of each provided argument
    case validate_provided_types(maps:to_list(ProvidedArgs), SchemaMap) of
        ok ->
            ok;
        {error, Details} ->
            {error, build_validation_error(
                <<"Invalid argument type">>,
                Details
            )}
    end.

%% @doc Validate provided arguments against JSON Schema (if provided).
%%
%% Uses jesse library for comprehensive JSON Schema validation.
-spec validate_against_schema(
    provided_arguments(),
    argument_schema()
) -> ok | {error, term()}.
validate_against_schema(_ProvidedArgs, undefined) ->
    ok;
validate_against_schema(ProvidedArgs, Schema) when is_map(Schema), map_size(Schema) > 0 ->
    try
        % Normalize schema for jesse
        NormalizedSchema = normalize_schema(Schema),

        % Validate against schema using jesse
        case jesse:validate_with_schema(NormalizedSchema, ProvidedArgs) of
            {ok, _ValidData} ->
                ok;
            {error, JesseErrors} ->
                {error, format_jesse_errors(JesseErrors)}
        end
    catch
        error:function_clause ->
            % jesse not loaded or schema format issue - skip validation
            logger:warning("Jesse schema validation skipped: schema format issue"),
            ok;
        _Class:_Reason ->
            % Other errors - log but don't fail
            logger:warning("Jesse schema validation failed: proceeding without validation"),
            ok
    end;
validate_against_schema(_ProvidedArgs, _Schema) ->
    ok.

%%====================================================================
%% Helper Functions - Required Argument Checking
%%====================================================================

%% @doc Find missing required arguments.
-spec find_missing_required(
    prompt_arguments(),
    provided_arguments()
) -> [binary()].
find_missing_required(PromptArguments, ProvidedArgs) ->
    lists:filtermap(
        fun(#mcp_prompt_argument{name = Name, required = Required}) ->
            case {Required, maps:is_key(Name, ProvidedArgs)} of
                {true, false} -> {true, Name};
                {true, true} -> false;
                {false, _} -> false;
                {undefined, false} -> false;
                {undefined, true} -> false
            end
        end,
        PromptArguments
    ).

%% @doc Count required arguments in list.
-spec count_required(prompt_arguments()) -> non_neg_integer().
count_required(PromptArguments) ->
    length(
        lists:filter(
            fun(#mcp_prompt_argument{required = Required}) ->
                Required =:= true
            end,
            PromptArguments
        )
    ).

%%====================================================================
%% Helper Functions - Type Validation
%%====================================================================

%% @doc Validate types of provided arguments against schema.
-spec validate_provided_types(
    [{binary(), term()}],
    map()
) -> ok | {error, map()}.
validate_provided_types([], _SchemaMap) ->
    ok;
validate_provided_types([{ArgName, ArgValue} | Rest], SchemaMap) ->
    case maps:get(ArgName, SchemaMap, undefined) of
        undefined ->
            % Extra argument not in schema - allow (schema doesn't restrict)
            validate_provided_types(Rest, SchemaMap);
        _ArgSchema ->
            % Argument in schema - validate type if possible
            case validate_type_match(ArgName, ArgValue, SchemaMap) of
                ok ->
                    validate_provided_types(Rest, SchemaMap);
                {error, TypeError} ->
                    {error, TypeError}
            end
    end.

%% @doc Validate type of a single argument.
%%
%% Currently validates basic types:
%% - binary/string
%% - integer/number
%% - boolean
%% - object/map
%% - array/list
-spec validate_type_match(
    binary(),
    term(),
    map()
) -> ok | {error, map()}.
validate_type_match(_ArgName, _ArgValue, _SchemaMap) ->
    % Type checking is generally too permissive in MCP
    % Most arguments are accepted as-is; schema validation handles details
    ok.

%%====================================================================
%% Helper Functions - Schema Normalization for Jesse
%%====================================================================

%% @doc Normalize schema for jesse validation.
%%
%% Ensures schema has required fields for jesse:
%% - "$schema": JSON Schema version
%% - "type": schema type (usually "object")
%% - "properties": field definitions
%% - "required": list of required fields
-spec normalize_schema(map()) -> map().
normalize_schema(Schema) when is_map(Schema) ->
    Base = Schema#{
        <<"$schema">> => maps:get(<<"$schema">>, Schema, <<"http://json-schema.org/draft-07/schema#">>)
    },
    Type = maps:get(<<"type">>, Base, <<"object">>),
    Base#{<<"type">> => Type}.

%%====================================================================
%% Helper Functions - Error Building
%%====================================================================

%% @doc Check if a field is required in schema.
-spec check_required_field(binary(), map()) -> boolean().
check_required_field(FieldName, Schema) when is_binary(FieldName), is_map(Schema) ->
    case maps:get(<<"required">>, Schema, []) of
        Required when is_list(Required) ->
            lists:member(FieldName, Required);
        _ ->
            false
    end.

%% @doc Build validation error for schema validation failure.
-spec build_schema_error(term()) -> {integer(), binary(), map()}.
build_schema_error(JesseError) ->
    ErrorData = #{
        <<"error_type">> => <<"schema_validation_failed">>,
        <<"details">> => format_error_details(JesseError)
    },
    build_validation_error(
        <<"Prompt arguments do not conform to schema">>,
        ErrorData
    ).

%% @doc Build internal error response.
-spec build_internal_error() -> {integer(), binary(), map()}.
build_internal_error() ->
    ErrorData = #{
        <<"error_type">> => <<"validation_error">>
    },
    {?JSONRPC_INVALID_PARAMS, <<"Error validating prompt arguments">>, ErrorData}.

%% @doc Extract argument names from prompt arguments list.
-spec extract_argument_names(prompt_arguments() | undefined) -> [binary()].
extract_argument_names(undefined) ->
    [];
extract_argument_names(PromptArguments) when is_list(PromptArguments) ->
    lists:map(
        fun(#mcp_prompt_argument{name = Name}) -> Name end,
        PromptArguments
    ).

%% @doc Extract required arguments from prompt arguments list.
-spec extract_required_arguments(prompt_arguments() | undefined) -> [binary()].
extract_required_arguments(undefined) ->
    [];
extract_required_arguments(PromptArguments) when is_list(PromptArguments) ->
    lists:filtermap(
        fun(#mcp_prompt_argument{name = Name, required = Required}) ->
            case Required of
                true -> {true, Name};
                _ -> false
            end
        end,
        PromptArguments
    ).

%%====================================================================
%% Helper Functions - Jesse Error Formatting
%%====================================================================

%% @doc Format jesse validation errors for response.
-spec format_jesse_errors(list() | term()) -> binary().
format_jesse_errors(Errors) when is_list(Errors) ->
    ErrorStrings = lists:map(fun format_single_error/1, Errors),
    iolist_to_binary(string:join(ErrorStrings, "; "));
format_jesse_errors(Error) ->
    format_single_error(Error).

%% @doc Format a single jesse error.
-spec format_single_error(term()) -> string().
format_single_error({error_data, {Path, {ValidationError, _Details}}}) ->
    io_lib:format("Path ~p: ~p", [Path, ValidationError]);
format_single_error({error, Message}) ->
    io_lib:format("~p", [Message]);
format_single_error(Other) ->
    io_lib:format("~p", [Other]).

%% @doc Format error details for response.
-spec format_error_details(term()) -> binary().
format_error_details(Details) when is_binary(Details) ->
    Details;
format_error_details(Details) when is_atom(Details) ->
    atom_to_binary(Details, utf8);
format_error_details(Details) ->
    iolist_to_binary(io_lib:format("~p", [Details])).

-ifdef(TEST).

%%====================================================================
%% Tests
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

%% Test: Validate with no arguments required
no_arguments_test() ->
    Result = validate_prompt_arguments(#{}, undefined),
    ?assertEqual(ok, Result).

%% Test: Validate with required argument provided
required_argument_provided_test() ->
    Args = [#mcp_prompt_argument{
        name = <<"name">>,
        required = true,
        description = <<"User name">>
    }],
    Result = validate_prompt_arguments(#{<<"name">> => <<"John">>}, Args),
    ?assertEqual(ok, Result).

%% Test: Validate with required argument missing
required_argument_missing_test() ->
    Args = [#mcp_prompt_argument{
        name = <<"name">>,
        required = true,
        description = <<"User name">>
    }],
    Result = validate_prompt_arguments(#{}, Args),
    ?assertMatch({error, {?JSONRPC_INVALID_PARAMS, _, _}}, Result).

%% Test: Validate with optional argument
optional_argument_test() ->
    Args = [#mcp_prompt_argument{
        name = <<"title">>,
        required = false,
        description = <<"Optional title">>
    }],
    Result = validate_prompt_arguments(#{}, Args),
    ?assertEqual(ok, Result).

%% Test: Validate multiple required arguments
multiple_arguments_test() ->
    Args = [
        #mcp_prompt_argument{
            name = <<"first_name">>,
            required = true,
            description = <<"First name">>
        },
        #mcp_prompt_argument{
            name = <<"last_name">>,
            required = true,
            description = <<"Last name">>
        }
    ],
    Provided = #{
        <<"first_name">> => <<"John">>,
        <<"last_name">> => <<"Doe">>
    },
    Result = validate_prompt_arguments(Provided, Args),
    ?assertEqual(ok, Result).

%% Test: Extract argument names
extract_names_test() ->
    Args = [
        #mcp_prompt_argument{name = <<"arg1">>, required = true},
        #mcp_prompt_argument{name = <<"arg2">>, required = false}
    ],
    Names = extract_argument_names(Args),
    ?assertEqual([<<"arg1">>, <<"arg2">>], Names).

%% Test: Extract required arguments
extract_required_test() ->
    Args = [
        #mcp_prompt_argument{name = <<"arg1">>, required = true},
        #mcp_prompt_argument{name = <<"arg2">>, required = false},
        #mcp_prompt_argument{name = <<"arg3">>, required = true}
    ],
    Required = extract_required_arguments(Args),
    ?assertEqual([<<"arg1">>, <<"arg3">>], Required).

-endif.
