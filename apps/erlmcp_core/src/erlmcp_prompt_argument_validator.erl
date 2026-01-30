-module(erlmcp_prompt_argument_validator).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    validate_prompt_arguments/3,
    format_validation_error/1,
    format_validation_errors/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Types
-type validation_result() :: ok | {error, {integer(), binary(), map()}}.
-type validation_error() :: #{
    code := integer(),
    message := binary(),
    data := map()
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

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Validate prompt arguments against declared schema.
%% Gap #42 Implementation: Validates provided arguments against:
%% 1. JSON Schema (if input_schema is defined)
%% 2. Required vs optional arguments (from arguments list)
%% 3. Type checking for each argument
-spec validate_prompt_arguments(
    map(),
    [#mcp_prompt_argument{}] | undefined,
    map() | undefined
) -> validation_result().
validate_prompt_arguments(ProvidedArgs, PromptArguments, InputSchema) ->
    gen_server:call(?MODULE, {validate_prompt_arguments, ProvidedArgs, PromptArguments, InputSchema}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    WorkerId = erlang:unique_integer([positive]),
    {ok, #state{worker_id = WorkerId}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.

handle_call({validate_prompt_arguments, ProvidedArgs, PromptArguments, InputSchema}, _From, State) ->
    Result = do_validate(ProvidedArgs, PromptArguments, InputSchema),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
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

%% @doc Main validation orchestrator
-spec do_validate(
    map(),
    [#mcp_prompt_argument{}] | undefined,
    map() | undefined
) -> validation_result().
do_validate(ProvidedArgs, PromptArguments, InputSchema) ->
    %% Step 1: Validate JSON Schema if present (Gap #42)
    case validate_json_schema(ProvidedArgs, InputSchema) of
        ok ->
            %% Step 2: Validate required vs optional arguments
            validate_required_arguments(ProvidedArgs, PromptArguments);
        {error, _} = Error ->
            Error
    end.

%% @doc Validate arguments against JSON Schema using jesse
-spec validate_json_schema(map(), map() | undefined) -> validation_result().
validate_json_schema(_ProvidedArgs, undefined) ->
    ok;
validate_json_schema(ProvidedArgs, InputSchema) when is_map(InputSchema) ->
    try
        case jesse:validate_with_schema(InputSchema, ProvidedArgs, [{allowed_errors, infinity}]) of
            {ok, _} ->
                ok;
            {error, JesseErrors} ->
                FormattedErrors = format_jesse_errors(JesseErrors),
                {error, {?JSONRPC_INVALID_PARAMS, <<"Argument validation failed">>, #{
                    <<"validation_errors">> => FormattedErrors
                }}}
        end
    catch
        _:JessError ->
            logger:error("Jesse validation error: ~p", [JessError]),
            {error, {?JSONRPC_INVALID_PARAMS, <<"Schema validation failed">>, #{
                <<"schema_error">> => io_lib:format("~p", [JessError])
            }}}
    end.

%% @doc Validate required arguments are present
-spec validate_required_arguments(map(), [#mcp_prompt_argument{}] | undefined) -> validation_result().
validate_required_arguments(_ProvidedArgs, undefined) ->
    ok;
validate_required_arguments(ProvidedArgs, PromptArguments) when is_list(PromptArguments) ->
    MissingRequired = lists:filtermap(
        fun(#mcp_prompt_argument{name = Name, required = Required}) ->
            case Required of
                true ->
                    case maps:is_key(Name, ProvidedArgs) of
                        false -> {true, Name};
                        true -> false
                    end;
                false ->
                    false
            end
        end,
        PromptArguments
    ),

    case MissingRequired of
        [] ->
            ok;
        _ ->
            {error, {?JSONRPC_INVALID_PARAMS,
                <<"Missing required prompt arguments">>, #{
                    <<"missing_arguments">> => MissingRequired,
                    <<"provided_arguments">> => maps:keys(ProvidedArgs)
                }}}
    end.

%% @doc Format jesse validation errors
-spec format_jesse_errors(term()) -> [map()].
format_jesse_errors(Errors) when is_list(Errors) ->
    lists:map(fun format_jesse_error/1, Errors);
format_jesse_errors(Error) ->
    [format_jesse_error(Error)].

%% @doc Format a single jesse error
-spec format_jesse_error(term()) -> map().
format_jesse_error({data_invalid, _Schema, Error, _Data, Path}) ->
    #{
        <<"path">> => format_path(Path),
        <<"error">> => format_error_message(Error)
    };
format_jesse_error({schema_invalid, _Schema, Error}) ->
    #{
        <<"path">> => <<>>,
        <<"error">> => format_error_message(Error)
    };
format_jesse_error({data_error, {parse_error, Reason}}) ->
    #{
        <<"path">> => <<>>,
        <<"error">> => iolist_to_binary(io_lib:format("Parse error: ~p", [Reason]))
    };
format_jesse_error({schema_error, {parse_error, Reason}}) ->
    #{
        <<"path">> => <<>>,
        <<"error">> => iolist_to_binary(io_lib:format("Schema parse error: ~p", [Reason]))
    };
format_jesse_error(_Other) ->
    #{
        <<"path">> => <<>>,
        <<"error">> => <<"Unknown validation error">>
    }.

%% @doc Format JSON path for error messages
-spec format_path([term()]) -> binary().
format_path(Path) ->
    PathParts = lists:map(fun
        (P) when is_binary(P) -> P;
        (P) when is_atom(P) -> atom_to_binary(P, utf8);
        (P) when is_integer(P) -> integer_to_binary(P);
        (P) -> iolist_to_binary(io_lib:format("~p", [P]))
    end, Path),
    case PathParts of
        [] -> <<>>;
        Parts -> iolist_to_binary(["$." | lists:join(<<".">>, Parts)])
    end.

%% @doc Format jesse error message
-spec format_error_message(term()) -> binary().
format_error_message({missing_required_property, Property}) ->
    iolist_to_binary(io_lib:format("Missing required property: ~s", [Property]));
format_error_message({wrong_type, Expected}) ->
    iolist_to_binary(io_lib:format("Wrong type, expected: ~s", [Expected]));
format_error_message({not_in_enum, AllowedValues}) ->
    iolist_to_binary(io_lib:format("Value not in enum: ~p", [AllowedValues]));
format_error_message(not_unique) ->
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

%% @doc Format validation error for JSON-RPC response
-spec format_validation_error(validation_error()) -> validation_error().
format_validation_error(Error) ->
    Error.

%% @doc Format multiple validation errors for JSON-RPC response
-spec format_validation_errors([validation_error()]) -> [validation_error()].
format_validation_errors(Errors) ->
    Errors.
