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

-spec do_validate(map(), term()) -> validation_result().
do_validate(Schema, Data) ->
    % Use jesse for JSON Schema validation
    case jesse:validate_with_schema(Schema, Data, [{allowed_errors, infinity}]) of
        {ok, _} ->
            ok;
        {error, Errors} ->
            FormattedErrors = format_jesse_errors(Errors),
            {error, FormattedErrors}
    end.

-spec format_jesse_errors(term()) -> [validation_error()].
format_jesse_errors(Errors) when is_list(Errors) ->
    lists:map(fun format_jesse_error/1, Errors);
format_jesse_errors(Error) ->
    [format_jesse_error(Error)].

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

-spec format_path([term()]) -> [binary()].
format_path(Path) ->
    lists:map(fun
        (P) when is_binary(P) -> P;
        (P) when is_atom(P) -> atom_to_binary(P, utf8);
        (P) when is_integer(P) -> integer_to_binary(P);
        (P) -> iolist_to_binary(io_lib:format("~p", [P]))
    end, Path).

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

%% These can be extended for MCP-specific validation rules

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

-spec validate_range(number(), number(), number()) -> boolean().
validate_range(Value, Min, Max) when is_number(Value), is_number(Min), is_number(Max) ->
    Value >= Min andalso Value =< Max.

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
