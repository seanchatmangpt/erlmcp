%%%====================================================================
%%% @doc Mustache-based Prompt Template Rendering Module
%%%
%%% Provides secure template rendering for MCP prompt templates with:
%%% - Mustache syntax: {{variable}}, {{#section}}, {{^inverted}}, {{/section}}
%%% - Security: Allowlist-only variable access, no code execution
%%% - Input validation: template syntax, variable names, size limits
%%%
%%% @end
%%%====================================================================
-module(erlmcp_prompt_template).

%% API
-export([compile/1, render/2, render_safe/2, validate/1, has_template_syntax/1]).

%%====================================================================
%% Constants & Macros
%%====================================================================

%% Security limits
-define(MAX_TEMPLATE_SIZE, 10240).      % 10KB
-define(MAX_VARIABLE_NAME_LEN, 64).
-define(MAX_VARIABLE_VALUE_LEN, 10240).  % 10KB
-define(MAX_NESTING_DEPTH, 5).
-define(MAX_OUTPUT_SIZE, 102400).       % 100KB
%% Regular expression for allowed variable names (allowlist only)
-define(ALLOWED_VAR_PATTERN, "^[a-zA-Z_][a-zA-Z0-9_]*$").
%% Error reasons
-define(ERR_TEMPLATE_TOO_LARGE, template_too_large).
-define(ERR_INVALID_VAR_NAME, invalid_variable_name).
-define(ERR_VAR_NAME_TOO_LONG, variable_name_too_long).
-define(ERR_VAR_VALUE_TOO_LARGE, variable_value_too_large).
-define(ERR_NESTING_TOO_DEEP, nesting_too_deep).
-define(ERR_OUTPUT_TOO_LARGE, output_too_large).
-define(ERR_INVALID_SYNTAX, invalid_template_syntax).

%%====================================================================
%% Type Definitions
%%====================================================================

-type template() :: binary().
-type compiled_template() :: binary().
-type variables() :: #{binary() => binary() | number() | boolean() | [term()]}.
-type render_result() :: {ok, binary()} | {error, term()}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Compile a template for faster rendering.
-spec compile(template()) -> {ok, compiled_template()} | {error, term()}.
compile(Template) when is_binary(Template) ->
    case validate_size_limits(Template) of
        ok ->
            case validate_template_syntax(Template) of
                ok ->
                    {ok, Template};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Render a pre-compiled template with variables.
-spec render(compiled_template(), variables()) -> render_result().
render(Template, Variables) when is_binary(Template), is_map(Variables) ->
    case validate_variables(Variables) of
        ok ->
            case render_with_bbmustache(Template, Variables) of
                {ok, Output} ->
                    case byte_size(Output) > ?MAX_OUTPUT_SIZE of
                        true ->
                            {error, {?ERR_OUTPUT_TOO_LARGE, byte_size(Output), ?MAX_OUTPUT_SIZE}};
                        false ->
                            {ok, Output}
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Compile and render a template in one step (safe).
-spec render_safe(template(), variables()) -> render_result().
render_safe(Template, Variables) ->
    case compile(Template) of
        {ok, Compiled} ->
            render(Compiled, Variables);
        {error, _} = Error ->
            Error
    end.

%% @doc Validate template syntax and security constraints.
-spec validate(template()) -> ok | {error, term()}.
validate(Template) ->
    case validate_size_limits(Template) of
        ok ->
            validate_template_syntax(Template);
        {error, _} = Error ->
            Error
    end.

%% @doc Check if a string contains Mustache template syntax.
-spec has_template_syntax(template()) -> boolean().
has_template_syntax(Template) when is_binary(Template) ->
    case binary:match(Template, <<"{{">>) of
        nomatch ->
            false;
        _ ->
            true
    end.

%%====================================================================
%% Internal Functions - Validation
%%====================================================================

%% @doc Validate template size limits.
-spec validate_size_limits(template()) -> ok | {error, term()}.
validate_size_limits(Template) ->
    Size = byte_size(Template),
    case Size > ?MAX_TEMPLATE_SIZE of
        true ->
            {error, {?ERR_TEMPLATE_TOO_LARGE, Size, ?MAX_TEMPLATE_SIZE}};
        false ->
            ok
    end.

%% @doc Validate template syntax (check for matching tags and nesting).
-spec validate_template_syntax(template()) -> ok | {error, term()}.
validate_template_syntax(Template) ->
    case validate_nesting_depth(Template, 0) of
        {ok, Depth} when Depth =< ?MAX_NESTING_DEPTH ->
            validate_unclosed_tags(Template);
        {ok, Depth} ->
            {error, {?ERR_NESTING_TOO_DEEP, Depth, ?MAX_NESTING_DEPTH}};
        {error, _} = Error ->
            Error
    end.

%% @doc Validate nesting depth of sections.
-spec validate_nesting_depth(template(), non_neg_integer()) ->
                                {ok, non_neg_integer()} | {error, term()}.
validate_nesting_depth(<<>>, Depth) ->
    {ok, Depth};
validate_nesting_depth(Template, Depth) ->
    case binary:match(Template, <<"{{#">>) of
        {Pos, _Len} when Pos < byte_size(Template) - 2 ->
            %% Extract everything after {{# to find the section name
            Before = binary:part(Template, 0, Pos),
            Rest = binary:part(Template, Pos + 2, byte_size(Template) - Pos - 2),
            case extract_section_name(Rest) of
                {ok, _Name, Remaining} ->
                    validate_nesting_depth(Remaining, Depth + 1);
                {error, _} = Error ->
                    Error
            end;
        nomatch ->
            {ok, Depth}
    end.

%% @doc Extract section name from template.
-spec extract_section_name(binary()) -> {ok, binary(), binary()} | {error, term()}.
extract_section_name(Template) ->
    case binary:split(Template, <<"}}">>) of
        [Name, Rest] ->
            TrimmedName = trim_whitespace(Name),
            {ok, TrimmedName, Rest};
        _ ->
            {error, {?ERR_INVALID_SYNTAX, unclosed_section_tag}}
    end.

%% @doc Validate that all tags are properly closed.
-spec validate_unclosed_tags(template()) -> ok | {error, term()}.
validate_unclosed_tags(Template) ->
    OpenTags = find_all_tags(Template, []),
    check_matching_tags(OpenTags).

%% @doc Find all opening and closing tags.
-spec find_all_tags(binary(), [{binary(), pos_integer()}]) -> [{binary(), pos_integer()}].
find_all_tags(<<>>, Acc) ->
    lists:reverse(Acc);
find_all_tags(Template, Acc) ->
    case binary:match(Template, <<"{{">>) of
        nomatch ->
            lists:reverse(Acc);
        {Pos, 2} ->
            Rest = binary:part(Template, Pos + 2, byte_size(Template) - Pos - 2),
            case parse_tag_name(Rest) of
                {ok, TagName, Remaining} ->
                    NewPos = Pos + 2,
                    find_all_tags(Remaining, [{TagName, NewPos} | Acc]);
                {error, _} ->
                    find_all_tags(Rest, Acc)
            end
    end.

%% @doc Parse tag name from template.
-spec parse_tag_name(binary()) -> {ok, binary(), binary()} | {error, term()}.
parse_tag_name(Template) ->
    case binary:split(Template, <<"}}">>) of
        [Tag, Rest] ->
            TrimmedTag = trim_whitespace(Tag),
            {ok, TrimmedTag, Rest};
        _ ->
            {error, {?ERR_INVALID_SYNTAX, unclosed_variable}}
    end.

%% @doc Check that opening and closing tags match.
-spec check_matching_tags([{binary(), pos_integer()}]) -> ok | {error, term()}.
check_matching_tags(Tags) ->
    check_matching_tags(Tags, []).

check_matching_tags([], []) ->
    ok;
check_matching_tags([], [_ | _]) ->
    {error, {?ERR_INVALID_SYNTAX, unclosed_section}};
check_matching_tags([{<<>>, _Pos} | Rest], Stack) ->
    check_matching_tags(Rest, Stack);
check_matching_tags([{<<"#", Name/binary>>, _Pos} | Rest], Stack) ->
    check_matching_tags(Rest, [Name | Stack]);
check_matching_tags([{<<"^", Name/binary>>, _Pos} | Rest], Stack) ->
    check_matching_tags(Rest, [Name | Stack]);
check_matching_tags([{<<"/", Name/binary>>, _Pos} | Rest], [Name | Stack]) ->
    check_matching_tags(Rest, Stack);
check_matching_tags([{<<"/", _Name/binary>>, _Pos} | _Rest], _Stack) ->
    {error, {?ERR_INVALID_SYNTAX, mismatched_closing_tag}};
check_matching_tags([{_Tag, _Pos} | Rest], Stack) ->
    check_matching_tags(Rest, Stack).

%% @doc Validate variable names and values.
-spec validate_variables(variables()) -> ok | {error, term()}.
validate_variables(Variables) when map_size(Variables) =:= 0 ->
    ok;
validate_variables(Variables) ->
    maps:fold(fun (Key, Value, ok) when is_binary(Key) ->
                      case validate_variable_name(Key) of
                          ok ->
                              validate_variable_value(Key, Value);
                          {error, _} = Error ->
                              Error
                      end;
                  (_Key, _Value, Acc) ->
                      Acc
              end,
              ok,
              Variables).

%% @doc Validate a single variable name (allowlist only).
-spec validate_variable_name(binary()) -> ok | {error, term()}.
validate_variable_name(Name) ->
    Len = byte_size(Name),
    case Len > ?MAX_VARIABLE_NAME_LEN of
        true ->
            {error, {?ERR_VAR_NAME_TOO_LONG, Name, Len, ?MAX_VARIABLE_NAME_LEN}};
        false ->
            case re:run(Name, ?ALLOWED_VAR_PATTERN, [{capture, none}]) of
                match ->
                    ok;
                nomatch ->
                    {error, {?ERR_INVALID_VAR_NAME, Name}}
            end
    end.

%% @doc Validate variable value size.
-spec validate_variable_value(binary(), term()) -> ok | {error, term()}.
validate_variable_value(Key, Value) when is_binary(Value) ->
    case byte_size(Value) > ?MAX_VARIABLE_VALUE_LEN of
        true ->
            {error, {?ERR_VAR_VALUE_TOO_LARGE, Key, byte_size(Value), ?MAX_VARIABLE_VALUE_LEN}};
        false ->
            ok
    end;
validate_variable_value(_Key, Value) when is_integer(Value); is_float(Value); is_boolean(Value) ->
    ok;
validate_variable_value(_Key, Value) when is_list(Value) ->
    %% Validate list items
    case validate_list_items(Value) of
        ok ->
            ok;
        {error, _} = Error ->
            Error
    end;
validate_variable_value(Key, _Value) ->
    {error, {?ERR_INVALID_VAR_NAME, Key}}.

%% @doc Validate list items (must be maps for section rendering).
-spec validate_list_items([term()]) -> ok | {error, term()}.
validate_list_items([]) ->
    ok;
validate_list_items([Item | Rest]) when is_map(Item) ->
    validate_list_items(Rest);
validate_list_items(_Item) ->
    {error, list_items_must_be_maps}.

%% @doc Trim whitespace from binary.
-spec trim_whitespace(binary()) -> binary().
trim_whitespace(Binary) ->
    TrimmedLeading = re:replace(Binary, "^\\s+", "", [{return, binary}]),
    re:replace(TrimmedLeading, "\\s+$", "", [{return, binary}]).

%%====================================================================
%% Internal Functions - Rendering with bbmustache
%%====================================================================

%% @doc Render template using bbmustache library with security constraints.
-spec render_with_bbmustache(template(), variables()) -> render_result().
render_with_bbmustache(Template, Variables) ->
    try
        %% Convert variables to bbmustache format (string keys required)
        MustacheData = convert_to_mustache_data(Variables),
        %% Render using bbmustache (render/2 takes template and data)
        Output = bbmustache:render(Template, MustacheData),
        {ok, iolist_to_binary(Output)}
    catch
        error:{badmatch, _} ->
            {error, {?ERR_INVALID_SYNTAX, render_failed}};
        Error:Reason ->
            {error, {render_error, Error, Reason}}
    end.

%% @doc Convert variables to bbmustache-compatible format.
%% bbmustache requires string keys, not binary keys.
-spec convert_to_mustache_data(variables()) -> map().
convert_to_mustache_data(Variables) ->
    maps:fold(fun(Key, Value, Acc) ->
                 %% Convert binary key to string (bbmustache requirement)
                 StringKey = binary_to_list(Key),
                 ConvertedValue = convert_value(Value),
                 maps:put(StringKey, ConvertedValue, Acc)
              end,
              #{},
              Variables).

%% @doc Convert a single value to bbmustache format.
-spec convert_value(term()) -> term().
convert_value(Value) when is_binary(Value); is_number(Value); is_boolean(Value) ->
    Value;
convert_value(Value) when is_list(Value) ->
    [convert_item(Item) || Item <- Value];
convert_value(_Value) ->
    undefined.

%% @doc Convert a list item for section rendering.
-spec convert_item(map()) -> map().
convert_item(Item) when is_map(Item) ->
    maps:fold(fun(Key, Value, Acc) ->
                 %% Convert binary key to string for nested maps
                 StringKey =
                     case is_binary(Key) of
                         true ->
                             binary_to_list(Key);
                         false ->
                             Key
                     end,
                 maps:put(StringKey, convert_value(Value), Acc)
              end,
              #{},
              Item);
convert_item(_Item) ->
    #{}.
