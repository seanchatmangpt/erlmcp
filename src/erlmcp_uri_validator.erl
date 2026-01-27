%%% erlmcp_uri_validator.erl
%%% Gap #41: Resource URI Format Validation
-module(erlmcp_uri_validator).

-export([
    validate_uri/1,
    validate_uri_scheme/1,
    validate_uri_template/1,
    is_valid_uri_format/1,
    validate_resource_uri_on_registration/1,
    validate_subscription_uri/1,
    parse_uri_template_variables/1,
    substitute_template_variables/2,
    get_uri_scheme/1,
    is_absolute_uri/1,
    is_relative_uri/1
]).

-type uri() :: binary().
-type scheme() :: binary().
-type validation_result() :: ok | {error, {atom(), binary()}}.

%% Main validation functions
-spec validate_uri(uri()) -> validation_result().
validate_uri(Uri) when is_binary(Uri), byte_size(Uri) > 0 ->
    case is_valid_uri_format(Uri) of
        true -> ok;
        false -> {error, {invalid_uri_format, <<"URI format is invalid">>}}
    end;
validate_uri(Uri) when is_binary(Uri) ->
    {error, {empty_uri, <<"URI cannot be empty">>}};
validate_uri(_) ->
    {error, {invalid_uri_type, <<"URI must be binary">>}}.

-spec validate_uri_scheme(scheme()) -> validation_result().
validate_uri_scheme(Scheme) when is_binary(Scheme), byte_size(Scheme) > 0 ->
    ValidSchemes = [<<"file">>, <<"http">>, <<"https">>, <<"data">>,
        <<"ftp">>, <<"ftps">>, <<"ws">>, <<"wss">>, <<"custom">>],
    case lists:member(Scheme, ValidSchemes) of
        true -> ok;
        false -> {error, {unsupported_scheme, <<"Scheme not supported">>}}
    end;
validate_uri_scheme(Scheme) when is_binary(Scheme) ->
    {error, {empty_scheme, <<"Scheme cannot be empty">>}};
validate_uri_scheme(_) ->
    {error, {invalid_scheme_type, <<"Scheme must be binary">>}}.

-spec validate_uri_template(uri()) -> validation_result().
validate_uri_template(Template) when is_binary(Template), byte_size(Template) > 0 ->
    case validate_template_structure(Template) of
        ok ->
            CleanedTemplate = remove_template_variables(Template),
            case is_valid_uri_format(CleanedTemplate) of
                true -> ok;
                false -> {error, {invalid_template_base_format,
                    <<"Template base format is invalid">>}}
            end;
        Error -> Error
    end;
validate_uri_template(Template) when is_binary(Template) ->
    {error, {empty_template, <<"URI template cannot be empty">>}};
validate_uri_template(_) ->
    {error, {invalid_template_type, <<"URI template must be binary">>}}.

-spec is_valid_uri_format(uri()) -> boolean().
is_valid_uri_format(Uri) when is_binary(Uri), byte_size(Uri) > 0 ->
    case get_uri_scheme(Uri) of
        {ok, Scheme} ->
            case validate_uri_scheme(Scheme) of
                ok -> true;
                _ -> false
            end;
        error ->
            is_relative_uri(Uri)
    end;
is_valid_uri_format(_) ->
    false.

-spec validate_resource_uri_on_registration(uri()) -> validation_result().
validate_resource_uri_on_registration(Uri) when is_binary(Uri) ->
    validate_uri(Uri);
validate_resource_uri_on_registration(_) ->
    {error, {invalid_uri_type, <<"URI must be binary">>}}.

-spec validate_subscription_uri(uri()) -> validation_result().
validate_subscription_uri(Uri) when is_binary(Uri), byte_size(Uri) > 0 ->
    validate_uri(Uri);
validate_subscription_uri(Uri) when is_binary(Uri) ->
    {error, {empty_uri, <<"URI cannot be empty for subscription">>}};
validate_subscription_uri(_) ->
    {error, {invalid_uri_type, <<"URI must be binary">>}}.

-spec parse_uri_template_variables(uri()) -> [binary()].
parse_uri_template_variables(Template) when is_binary(Template) ->
    case extract_template_variables(Template, []) of
        Variables when is_list(Variables) -> lists:usort(Variables);
        _ -> []
    end;
parse_uri_template_variables(_) ->
    [].

-spec substitute_template_variables(uri(), map()) -> {ok, uri()} | {error, term()}.
substitute_template_variables(Template, Variables)
  when is_binary(Template), is_map(Variables) ->
    RequiredVars = parse_uri_template_variables(Template),
    case check_all_variables_provided(RequiredVars, Variables) of
        ok ->
            {ok, substitute_variables_in_template(Template, Variables)};
        {error, _} = VarError -> VarError
    end;
substitute_template_variables(_, _) ->
    {error, {invalid_arguments, <<"Template must be binary">>}}.

-spec get_uri_scheme(uri()) -> {ok, scheme()} | error.
get_uri_scheme(Uri) when is_binary(Uri) ->
    case binary:match(Uri, <<"://">>) of
        {Pos, _Len} ->
            Scheme = binary:part(Uri, 0, Pos),
            case is_valid_scheme_name(Scheme) of
                true -> {ok, Scheme};
                false -> error
            end;
        nomatch -> error
    end;
get_uri_scheme(_) ->
    error.

-spec is_absolute_uri(uri()) -> boolean().
is_absolute_uri(Uri) when is_binary(Uri) ->
    case get_uri_scheme(Uri) of
        {ok, _} -> true;
        error -> false
    end;
is_absolute_uri(_) ->
    false.

-spec is_relative_uri(uri()) -> boolean().
is_relative_uri(Uri) when is_binary(Uri), byte_size(Uri) > 0 ->
    StartsWithSlash = binary:match(Uri, <<"/">>) =:= {0, 1},
    StartsWithDotSlash = binary:match(Uri, <<"./">>) =:= {0, 2},
    StartsWithParent = binary:match(Uri, <<"../">>) =:= {0, 3},
    case {StartsWithSlash, StartsWithDotSlash, StartsWithParent} of
        {true, _, _} -> true;
        {_, true, _} -> true;
        {_, _, true} -> true;
        _ ->
            case get_uri_scheme(Uri) of
                error -> is_valid_relative_path(Uri);
                {ok, _} -> false
            end
    end;
is_relative_uri(_) ->
    false.

%% Internal functions
-spec validate_template_structure(uri()) -> validation_result().
validate_template_structure(Template) when is_binary(Template) ->
    case check_balanced_braces(Template) of
        ok -> check_valid_variable_names(Template);
        Error -> Error
    end.

-spec check_balanced_braces(uri()) -> validation_result().
check_balanced_braces(Template) when is_binary(Template) ->
    case count_braces(Template, 0) of
        0 -> ok;
        _ -> {error, {unbalanced_braces, <<"Unbalanced braces">>}}
    end.

-spec count_braces(binary(), integer()) -> integer().
count_braces(<<>>, Balance) ->
    Balance;
count_braces(<<${, Rest/binary>>, Balance) ->
    count_braces(Rest, Balance + 1);
count_braces(<<$}, Rest/binary>>, Balance) ->
    count_braces(Rest, Balance - 1);
count_braces(<<_, Rest/binary>>, Balance) ->
    count_braces(Rest, Balance).

-spec check_valid_variable_names(uri()) -> validation_result().
check_valid_variable_names(Template) ->
    case extract_template_variables(Template, []) of
        Variables when is_list(Variables) ->
            case lists:all(fun is_valid_variable_name/1, Variables) of
                true -> ok;
                false -> {error, {invalid_variable_names,
                    <<"Invalid variable names">>}}
            end;
        _ -> {error, {invalid_template, <<"Invalid template">>}}
    end.

-spec extract_template_variables(binary(), [binary()]) -> [binary()].
extract_template_variables(<<>>, Acc) ->
    Acc;
extract_template_variables(<<${, Rest/binary>>, Acc) ->
    case extract_variable_name(Rest, <<>>) of
        {ok, VarName, Remaining} ->
            extract_template_variables(Remaining, [VarName | Acc]);
        error -> Acc
    end;
extract_template_variables(<<_, Rest/binary>>, Acc) ->
    extract_template_variables(Rest, Acc).

-spec extract_variable_name(binary(), binary()) -> {ok, binary(), binary()} | error.
extract_variable_name(<<$}, Rest/binary>>, Acc) when byte_size(Acc) > 0 ->
    {ok, Acc, Rest};
extract_variable_name(<<C, Rest/binary>>, Acc) when
    (C >= $a andalso C =< $z) orelse
    (C >= $A andalso C =< $Z) orelse
    (C >= $0 andalso C =< $9) orelse
    C =:= $_ ->
    extract_variable_name(Rest, <<Acc/binary, C>>);
extract_variable_name(_, _) ->
    error.

-spec is_valid_variable_name(binary()) -> boolean().
is_valid_variable_name(VarName) when is_binary(VarName), byte_size(VarName) > 0 ->
    true;
is_valid_variable_name(_) ->
    false.

-spec is_valid_scheme_name(binary()) -> boolean().
is_valid_scheme_name(Scheme) when is_binary(Scheme), byte_size(Scheme) > 0 ->
    case Scheme of
        <<C, _/binary>> when
            (C >= $a andalso C =< $z) orelse
            (C >= $A andalso C =< $Z) ->
            lists:all(fun(Ch) ->
                (Ch >= $a andalso Ch =< $z) orelse
                (Ch >= $A andalso Ch =< $Z) orelse
                (Ch >= $0 andalso Ch =< $9) orelse
                Ch =:= $+ orelse Ch =:= $- orelse Ch =:= $.
            end, binary_to_list(Scheme));
        _ -> false
    end;
is_valid_scheme_name(_) ->
    false.

-spec is_valid_relative_path(binary()) -> boolean().
is_valid_relative_path(Path) when is_binary(Path), byte_size(Path) > 0 ->
    lists:any(fun(C) -> C =:= $/ end, binary_to_list(Path));
is_valid_relative_path(_) ->
    false.

-spec remove_template_variables(uri()) -> uri().
remove_template_variables(Template) when is_binary(Template) ->
    remove_template_variables(Template, <<>>).

-spec remove_template_variables(binary(), binary()) -> binary().
remove_template_variables(<<>>, Acc) ->
    Acc;
remove_template_variables(<<${, Rest/binary>>, Acc) ->
    case skip_to_closing_brace(Rest) of
        {ok, Remaining} ->
            remove_template_variables(Remaining, Acc);
        error ->
            Prefix = <<"${">>,
            <<Acc/binary, Prefix/binary>>
    end;
remove_template_variables(<<C, Rest/binary>>, Acc) ->
    remove_template_variables(Rest, <<Acc/binary, C>>).

-spec skip_to_closing_brace(binary()) -> {ok, binary()} | error.
skip_to_closing_brace(<<$}, Rest/binary>>) ->
    {ok, Rest};
skip_to_closing_brace(<<_C, Rest/binary>>) ->
    skip_to_closing_brace(Rest);
skip_to_closing_brace(<<>>) ->
    error.

-spec check_all_variables_provided([binary()], map()) -> ok | {error, term()}.
check_all_variables_provided(RequiredVars, ProvidedVars) ->
    MissingVars = lists:filter(fun(Var) ->
        not maps:is_key(Var, ProvidedVars)
    end, RequiredVars),
    case MissingVars of
        [] -> ok;
        Missing ->
            {error, {missing_variables,
                {<<"Missing variables">>, Missing}}}
    end.

-spec substitute_variables_in_template(uri(), map()) -> uri().
substitute_variables_in_template(Template, Variables) ->
    substitute_variables_in_template(Template, Variables, <<>>).

-spec substitute_variables_in_template(binary(), map(), binary()) -> binary().
substitute_variables_in_template(<<>>, _Variables, Acc) ->
    Acc;
substitute_variables_in_template(<<${, Rest/binary>>, Variables, Acc) ->
    case extract_variable_name(Rest, <<>>) of
        {ok, VarName, Remaining} ->
            VarValue = maps:get(VarName, Variables, VarName),
            substitute_variables_in_template(Remaining, Variables,
                <<Acc/binary, VarValue/binary>>);
        error ->
            substitute_variables_in_template(Rest, Variables, Acc)
    end;
substitute_variables_in_template(<<C, Rest/binary>>, Variables, Acc) ->
    substitute_variables_in_template(Rest, Variables, <<Acc/binary, C>>).
