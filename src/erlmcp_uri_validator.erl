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
    is_relative_uri/1,
    validate_path_traversal/1,
    check_malicious_patterns/1,
    validate_uri_security/1
]).

-type uri() :: binary().
-type scheme() :: binary().
-type validation_result() :: ok | {error, {atom(), binary()}}.

%% Main validation functions
-spec validate_uri(uri()) -> validation_result().
validate_uri(Uri) when is_binary(Uri), byte_size(Uri) > 0 ->
    case validate_uri_security(Uri) of
        {error, _} = SecurityErr ->
            SecurityErr;
        ok ->
            case is_valid_uri_format(Uri) of
                true -> ok;
                false -> {error, {invalid_uri_format, <<"URI format is invalid">>}}
            end
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

%%====================================================================
%% Security Validation Functions (v1.3.0)
%%====================================================================

%% @doc Comprehensive URI security validation
%% Rejects path traversal, encoding attacks, null bytes, and injection patterns
-spec validate_uri_security(uri()) -> ok | {error, {atom(), binary()}}.
validate_uri_security(Uri) when is_binary(Uri) ->
    case check_malicious_patterns(Uri) of
        ok ->
            case validate_path_traversal(Uri) of
                ok -> ok;
                {error, _} = Err -> Err
            end;
        {error, _} = Err -> Err
    end;
validate_uri_security(_) ->
    {error, {invalid_uri_type, <<"URI must be binary">>}}.

%% @doc Validate path traversal attacks
%% Rejects: ../, ..\, %2e%2e, encoded dots, null bytes, dot segments
-spec validate_path_traversal(uri()) -> ok | {error, {atom(), binary()}}.
validate_path_traversal(Uri) when is_binary(Uri) ->
    case contains_path_traversal_pattern(Uri) of
        false -> ok;
        true -> {error, {path_traversal_detected, <<"Path traversal pattern detected">>}}
    end.

%% @doc Check for malicious patterns: null bytes, command injection, etc.
-spec check_malicious_patterns(uri()) -> ok | {error, {atom(), binary()}}.
check_malicious_patterns(Uri) when is_binary(Uri) ->
    case contains_null_byte(Uri) of
        true ->
            {error, {null_byte_detected, <<"Null byte in URI">>}};
        false ->
            case contains_control_characters(Uri) of
                true ->
                    {error, {control_character_detected, <<"Control character in URI">>}};
                false ->
                    case contains_command_injection(Uri) of
                        true ->
                            {error, {command_injection_detected, <<"Command injection pattern">>}};
                        false -> ok
                    end
            end
    end.

%% @doc Check for path traversal patterns (including encoded variants)
-spec contains_path_traversal_pattern(binary()) -> boolean().
contains_path_traversal_pattern(Uri) ->
    %% Check for direct patterns
    case binary:match(Uri, [<<"../">>, <<"..\\">> ]) of
        nomatch ->
            %% Check for encoded patterns
            case contains_encoded_traversal(Uri) of
                true -> true;
                false ->
                    %% Check for Unicode-encoded dot sequences
                    case contains_unicode_encoded_traversal(Uri) of
                        true -> true;
                        false ->
                            %% Check for backslash traversal (Windows-style)
                            contains_backslash_traversal(Uri)
                    end
            end;
        _ -> true
    end.

%% @doc Check for various encoded traversal patterns
-spec contains_encoded_traversal(binary()) -> boolean().
contains_encoded_traversal(Uri) ->
    EncodedPatterns = [
        <<"%2e%2e">>,    % ..
        <<"%2E%2E">>,    % .. (uppercase)
        <<"%2e%2e%2f">>, % ../
        <<"%2E%2E%2F">>, % ../
        <<"%252e">>,     % Double-encoded %
        <<"%c0%ae">>,    % Unicode encoded /
        <<"%c1%9c">>,    % Unicode variant
        <<"%c0%9e">>,    % UTF-8 variant
        <<"%f0%90%80">>, % UTF-8 4-byte encoding
        <<"..%2f">>,     % .. with encoded /
        <<"..%5c">>,     % .. with encoded \
        <<"%2e%2e/">>    % Mixed encoding
    ],
    lists:any(fun(Pattern) ->
        binary:match(Uri, Pattern) =/= nomatch
    end, EncodedPatterns).

%% @doc Check for Unicode-encoded traversal patterns
-spec contains_unicode_encoded_traversal(binary()) -> boolean().
contains_unicode_encoded_traversal(Uri) ->
    UnicodePatterns = [
        <<"&#46;&#46;">>,  % HTML entity ..
        <<"&#x2e;&#x2e;">>,% Hex entity ..
        <<"&#46%2e;">>,    % Mixed entity and encoded
        <<"&#x2e%2e;">>    % Mixed hex entity
    ],
    lists:any(fun(Pattern) ->
        binary:match(Uri, Pattern) =/= nomatch
    end, UnicodePatterns).

%% @doc Check for backslash-based traversal (Windows)
-spec contains_backslash_traversal(binary()) -> boolean().
contains_backslash_traversal(Uri) ->
    case binary:match(Uri, <<"..\\">>) of
        nomatch ->
            case binary:match(Uri, <<"..\\">> ) of
                nomatch -> false;
                _ -> true
            end;
        _ -> true
    end.

%% @doc Check for null bytes in URI
-spec contains_null_byte(binary()) -> boolean().
contains_null_byte(Uri) ->
    case binary:match(Uri, <<0>>) of
        nomatch -> false;
        _ -> true
    end.

%% @doc Check for control characters (ASCII 0-31, 127)
-spec contains_control_characters(binary()) -> boolean().
contains_control_characters(Uri) ->
    check_bytes_range(Uri, 0, 31) orelse check_bytes_range(Uri, 127, 127).

%% @doc Check for command injection patterns
-spec contains_command_injection(binary()) -> boolean().
contains_command_injection(Uri) ->
    InjectionPatterns = [
        <<"&">>,    % Command chaining (but allow in query strings with validation)
        <<";">>,    % Command separator (strict)
        <<"|">>,    % Pipe (but allow in paths with validation)
        <<"<">>,    % Redirect
        <<">>">>,   % Append redirect
        <<"$(">>,   % Command substitution
        <<"${">>,   % Variable expansion (except our template format)
        <<"{">>,    % Template variable (if not in expected context)
        <<"`">>     % Backtick execution
    ],
    %% More lenient: only reject if combined with shell indicators
    case lists:any(fun(Pattern) ->
        binary:match(Uri, Pattern) =/= nomatch
    end, InjectionPatterns) of
        false -> false;
        true ->
            %% Additional context check for command patterns
            (binary:match(Uri, <<"sh ">>) =/= nomatch) or
            (binary:match(Uri, <<"bash ">>) =/= nomatch) or
            (binary:match(Uri, <<"cmd ">>) =/= nomatch) or
            (binary:match(Uri, <<"powershell">>) =/= nomatch)
    end.

%% @doc Check for bytes in specific range in binary
-spec check_bytes_range(binary(), non_neg_integer(), non_neg_integer()) -> boolean().
check_bytes_range(<<>>, _Min, _Max) ->
    false;
check_bytes_range(<<Byte, Rest/binary>>, Min, Max) ->
    case Byte >= Min andalso Byte =< Max of
        true -> true;
        false -> check_bytes_range(Rest, Min, Max)
    end.
