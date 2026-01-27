%%%===================================================================
%%% Module: erlmcp_completion_context
%%% Purpose: Context-aware completions with argument reference resolution
%%%
%%% Provides:
%%% - Schema-driven completion generation
%%% - Argument reference resolution from completion context
%%% - Smart filtering based on partial input
%%% - Enum and pattern-based completions
%%%
%%% Design:
%%% - Stores completion context from previous arguments
%%% - Resolves references to already-completed arguments
%%% - Generates suggestions based on JSON Schema constraints
%%% - Supports partial completion with filtering
%%%===================================================================

-module(erlmcp_completion_context).

%% API exports
-export([
    complete/3,
    resolve_context/2,
    resolve_argument_reference/3,
    filter_completions/2,
    generate_completions_from_schema/3
]).

%% Type definitions
-type completion_request() :: #{
    name => binary(),
    partial_argument => binary(),
    arguments => map(),
    context => map()
}.

-type completion_response() :: #{
    values => [binary()],
    total => non_neg_integer(),
    hasMore => boolean()
}.

-type json_schema() :: map().

%% Error types
-type completion_error() :: {error, Reason :: term()}.

%%====================================================================
%% API Functions
%%====================================================================

%% Complete a tool argument with context
-spec complete(binary(), completion_request(), json_schema() | undefined) ->
    {ok, completion_response()} | completion_error().
complete(ToolName, Request, Schema) when is_binary(ToolName), is_map(Request) ->
    try
        PartialArg = maps:get(partial_argument, Request, <<"">>),
        Arguments = maps:get(arguments, Request, #{}),
        Context = maps:get(context, Request, #{}),

        % Resolve any references in context
        ResolvedContext = resolve_context(Arguments, Context),

        % Generate completions based on schema or context
        case Schema of
            undefined ->
                % No schema, use context-based completion
                generate_context_completions(PartialArg, ResolvedContext);
            SchemaMap ->
                % Schema-based completion with context
                generate_schema_completions(PartialArg, SchemaMap, ResolvedContext)
        end
    catch
        Class:Reason ->
            logger:error("Completion error in ~s: ~p:~p", [ToolName, Class, Reason]),
            {error, {completion_failed, Reason}}
    end.

%% Resolve context by replacing argument references
-spec resolve_context(map(), map()) -> map().
resolve_context(Arguments, Context) ->
    maps:fold(fun
        (Key, Value, Acc) when is_binary(Value), byte_size(Value) > 1 ->
            case Value of
                <<"$.", RefKey/binary>> ->
                    % Reference to another argument
                    case maps:get(RefKey, Arguments, undefined) of
                        undefined ->
                            logger:debug("Unresolved reference: ~s", [RefKey]),
                            Acc#{Key => undefined};
                        RefValue ->
                            Acc#{Key => RefValue}
                    end;
                _ ->
                    Acc#{Key => Value}
            end;
        (Key, Value, Acc) ->
            Acc#{Key => Value}
    end, #{}, Context).

%% Resolve a single argument reference
-spec resolve_argument_reference(binary(), map(), map()) ->
    {ok, term()} | {error, not_found}.
resolve_argument_reference(Reference, Arguments, _Context)
  when is_binary(Reference), byte_size(Reference) > 2 ->
    case Reference of
        <<"$.", RefKey/binary>> ->
            case maps:get(RefKey, Arguments, undefined) of
                undefined ->
                    {error, not_found};
                Value ->
                    {ok, Value}
            end;
        _ ->
            {error, invalid_reference}
    end;
resolve_argument_reference(_Reference, _Arguments, _Context) ->
    {error, invalid_reference}.

%% Filter completions based on partial input
-spec filter_completions([binary()], binary()) -> [binary()].
filter_completions(Completions, PartialArg) when is_binary(PartialArg) ->
    lists:filter(fun(Completion) ->
        case binary:match(Completion, PartialArg) of
            {0, _} -> true;  % Match at start
            _ -> false
        end
    end, Completions).

%% Generate completions from JSON schema
-spec generate_completions_from_schema(json_schema(), binary(), map()) ->
    [binary()].
generate_completions_from_schema(Schema, PartialArg, Context)
  when is_map(Schema), is_binary(PartialArg) ->
    case maps:get(<<"enum">>, Schema) of
        undefined ->
            generate_from_schema_type(Schema, PartialArg, Context);
        EnumValues when is_list(EnumValues) ->
            Binaries = lists:filtermap(fun
                (V) when is_binary(V) -> {true, V};
                (V) when is_atom(V) -> {true, atom_to_binary(V)};
                (_) -> false
            end, EnumValues),
            filter_completions(Binaries, PartialArg)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Generate completions based on context alone
-spec generate_context_completions(binary(), map()) ->
    {ok, completion_response()}.
generate_context_completions(PartialArg, Context) ->
    % Use context keys and values as completion hints
    ContextValues = lists:filtermap(fun
        ({_Key, undefined}) -> false;
        ({_Key, Value}) when is_binary(Value) ->
            {true, Value};
        ({_Key, Value}) when is_atom(Value) ->
            {true, atom_to_binary(Value)};
        (_) -> false
    end, maps:to_list(Context)),

    Filtered = filter_completions(ContextValues, PartialArg),
    {ok, #{
        values => lists:usort(Filtered),
        total => length(Filtered),
        hasMore => false
    }}.

%% Generate completions using schema with context
-spec generate_schema_completions(binary(), json_schema(), map()) ->
    {ok, completion_response()}.
generate_schema_completions(PartialArg, Schema, Context) ->
    Completions = generate_completions_from_schema(Schema, PartialArg, Context),
    {ok, #{
        values => Completions,
        total => length(Completions),
        hasMore => length(Completions) > 100
    }}.

%% Generate completions based on schema type
-spec generate_from_schema_type(json_schema(), binary(), map()) ->
    [binary()].
generate_from_schema_type(Schema, PartialArg, Context) ->
    SchemaType = maps:get(<<"type">>, Schema, undefined),

    case SchemaType of
        <<"string">> ->
            generate_string_completions(Schema, PartialArg, Context);
        <<"number">> ->
            generate_number_completions(Schema, PartialArg);
        <<"integer">> ->
            generate_integer_completions(Schema, PartialArg);
        <<"boolean">> ->
            generate_boolean_completions(PartialArg);
        <<"object">> ->
            generate_object_completions(Schema, PartialArg, Context);
        <<"array">> ->
            generate_array_completions(Schema, PartialArg, Context);
        _ ->
            []
    end.

%% String completions with pattern support
-spec generate_string_completions(json_schema(), binary(), map()) ->
    [binary()].
generate_string_completions(Schema, PartialArg, _Context) ->
    case maps:get(<<"pattern">>, Schema) of
        Pattern when is_binary(Pattern) ->
            % Try to generate examples matching pattern
            generate_pattern_examples(Pattern, PartialArg);
        undefined ->
            % Check for examples or default
            case maps:get(<<"examples">>, Schema) of
                Examples when is_list(Examples) ->
                    Binaries = lists:filtermap(fun
                        (E) when is_binary(E) -> {true, E};
                        (E) when is_atom(E) -> {true, atom_to_binary(E)};
                        (_) -> false
                    end, Examples),
                    filter_completions(Binaries, PartialArg);
                _ ->
                    []
            end
    end.

%% Number completions
-spec generate_number_completions(json_schema(), binary()) ->
    [binary()].
generate_number_completions(Schema, PartialArg) ->
    case {maps:get(<<"minimum">>, Schema), maps:get(<<"maximum">>, Schema)} of
        {Min, Max} when is_number(Min), is_number(Max) ->
            % Generate a few values in range
            Step = (Max - Min) / 5,
            Values = [Min + (Step * I) || I <- lists:seq(0, 4)],
            Strings = lists:map(fun(V) -> float_to_binary(V, [{decimals, 2}]) end, Values),
            filter_completions(Strings, PartialArg);
        _ ->
            []
    end.

%% Integer completions
-spec generate_integer_completions(json_schema(), binary()) ->
    [binary()].
generate_integer_completions(Schema, PartialArg) ->
    case {maps:get(<<"minimum">>, Schema), maps:get(<<"maximum">>, Schema)} of
        {Min, Max} when is_integer(Min), is_integer(Max), Max - Min < 100 ->
            % Generate all values in small range
            Values = lists:seq(Min, Max),
            Strings = lists:map(fun integer_to_binary/1, Values),
            filter_completions(Strings, PartialArg);
        {Min, Max} when is_integer(Min), is_integer(Max) ->
            % Generate sample values from large range
            Step = (Max - Min) div 5,
            Values = [Min + (Step * I) || I <- lists:seq(0, 4)] ++ [Max],
            Strings = lists:map(fun integer_to_binary/1, Values),
            filter_completions(Strings, PartialArg);
        _ ->
            []
    end.

%% Boolean completions
-spec generate_boolean_completions(binary()) -> [binary()].
generate_boolean_completions(PartialArg) ->
    filter_completions([<<"true">>, <<"false">>], PartialArg).

%% Object completions - properties as keys
-spec generate_object_completions(json_schema(), binary(), map()) ->
    [binary()].
generate_object_completions(Schema, PartialArg, _Context) ->
    case maps:get(<<"properties">>, Schema) of
        Props when is_map(Props) ->
            Keys = maps:keys(Props),
            Binaries = lists:filtermap(fun
                (K) when is_binary(K) -> {true, K};
                (K) when is_atom(K) -> {true, atom_to_binary(K)};
                (_) -> false
            end, Keys),
            filter_completions(Binaries, PartialArg);
        _ ->
            []
    end.

%% Array completions - items type completions
-spec generate_array_completions(json_schema(), binary(), map()) ->
    [binary()].
generate_array_completions(Schema, PartialArg, Context) ->
    case maps:get(<<"items">>, Schema) of
        ItemSchema when is_map(ItemSchema) ->
            generate_from_schema_type(ItemSchema, PartialArg, Context);
        _ ->
            []
    end.

%% Generate pattern examples
-spec generate_pattern_examples(binary(), binary()) -> [binary()].
generate_pattern_examples(Pattern, PartialArg) ->
    % Simple pattern matching for common patterns
    case Pattern of
        <<"^[a-zA-Z]+$">> ->
            Suggestions = [<<"alpha">>, <<"example">>, <<"test">>],
            filter_completions(Suggestions, PartialArg);
        <<"^[0-9]+$">> ->
            Suggestions = [<<"0">>, <<"1">>, <<"10">>, <<"100">>],
            filter_completions(Suggestions, PartialArg);
        <<"^[a-z_]+$">> ->
            Suggestions = [<<"snake_case">>, <<"example_name">>, <<"test_value">>],
            filter_completions(Suggestions, PartialArg);
        _ ->
            []
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

completion_context_test_() ->
    [
        ?_test(test_context_resolution()),
        ?_test(test_enum_completions()),
        ?_test(test_pattern_completions()),
        ?_test(test_schema_completions()),
        ?_test(test_reference_resolution()),
        ?_test(test_filtering()),
        ?_test(test_boolean_completions()),
        ?_test(test_number_completions()),
        ?_test(test_object_completions()),
        ?_test(test_array_completions())
    ].

test_context_resolution() ->
    Context = #{
        <<"author">> => <<"$.user">>,
        <<"manual">> => <<"John">>
    },
    Arguments = #{
        <<"user">> => <<"Alice">>,
        <<"date">> => <<"2024-01-01">>
    },

    Resolved = resolve_context(Arguments, Context),
    ?assertEqual(<<"Alice">>, maps:get(<<"author">>, Resolved)),
    ?assertEqual(<<"John">>, maps:get(<<"manual">>, Resolved)).

test_enum_completions() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"enum">> => [<<"red">>, <<"green">>, <<"blue">>]
    },

    {ok, Response} = complete(<<"color_picker">>, #{
        partial_argument => <<"r">>,
        arguments => #{},
        context => #{}
    }, Schema),

    Values = maps:get(values, Response),
    ?assert(lists:member(<<"red">>, Values)).

test_pattern_completions() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"pattern">> => <<"^[a-z_]+$">>
    },

    {ok, Response} = complete(<<"snake_case">>, #{
        partial_argument => <<"">>,
        arguments => #{},
        context => #{}
    }, Schema),

    Values = maps:get(values, Response),
    ?assert(length(Values) > 0).

test_schema_completions() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>},
            <<"age">> => #{<<"type">> => <<"integer">>},
            <<"email">> => #{<<"type">> => <<"string">>}
        }
    },

    {ok, Response} = complete(<<"user_form">>, #{
        partial_argument => <<"">>,
        arguments => #{},
        context => #{}
    }, Schema),

    Values = maps:get(values, Response),
    ?assert(lists:member(<<"name">>, Values)),
    ?assert(lists:member(<<"age">>, Values)),
    ?assert(lists:member(<<"email">>, Values)).

test_reference_resolution() ->
    {ok, Value} = resolve_argument_reference(<<"$.username">>, #{
        <<"username">> => <<"alice">>,
        <<"email">> => <<"alice@example.com">>
    }, #{}),

    ?assertEqual(<<"alice">>, Value),

    {error, not_found} = resolve_argument_reference(<<"$.nonexistent">>, #{
        <<"username">> => <<"alice">>
    }, #{}).

test_filtering() ->
    Completions = [<<"apple">>, <<"apricot">>, <<"banana">>, <<"orange">>],
    Filtered = filter_completions(Completions, <<"ap">>),

    ?assert(lists:member(<<"apple">>, Filtered)),
    ?assert(lists:member(<<"apricot">>, Filtered)),
    ?assertNot(lists:member(<<"banana">>, Filtered)).

test_boolean_completions() ->
    {ok, Response} = complete(<<"toggle">>, #{
        partial_argument => <<"">>,
        arguments => #{},
        context => #{}
    }, #{<<"type">> => <<"boolean">>}),

    Values = maps:get(values, Response),
    ?assert(lists:member(<<"true">>, Values)),
    ?assert(lists:member(<<"false">>, Values)).

test_number_completions() ->
    Schema = #{
        <<"type">> => <<"number">>,
        <<"minimum">> => 0.0,
        <<"maximum">> => 100.0
    },

    {ok, Response} = complete(<<"percentage">>, #{
        partial_argument => <<"">>,
        arguments => #{},
        context => #{}
    }, Schema),

    Values = maps:get(values, Response),
    ?assert(length(Values) > 0).

test_object_completions() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"firstName">> => #{<<"type">> => <<"string">>},
            <<"lastName">> => #{<<"type">> => <<"string">>},
            <<"age">> => #{<<"type">> => <<"integer">>}
        }
    },

    {ok, Response} = complete(<<"person">>, #{
        partial_argument => <<"first">>,
        arguments => #{},
        context => #{}
    }, Schema),

    Values = maps:get(values, Response),
    ?assert(lists:member(<<"firstName">>, Values)).

test_array_completions() ->
    Schema = #{
        <<"type">> => <<"array">>,
        <<"items">> => #{
            <<"type">> => <<"string">>,
            <<"enum">> => [<<"item1">>, <<"item2">>, <<"item3">>]
        }
    },

    {ok, Response} = complete(<<"items_list">>, #{
        partial_argument => <<"">>,
        arguments => #{},
        context => #{}
    }, Schema),

    Values = maps:get(values, Response),
    ?assert(length(Values) > 0).

-endif.
