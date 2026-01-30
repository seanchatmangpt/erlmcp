-module(erlmcp_prompt_template).

%% API exports
-export([
    render/2,
    render_safe/2,
    validate/1,
    has_template_syntax/1
]).

-include("erlmcp.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type template() :: binary() | string().
-type variables() :: map().
-type render_result() :: {ok, binary()} | {error, term()}.

-export_type([template/0, variables/0, render_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Render a template with variables using Mustache syntax.
%% Throws an error if rendering fails - use render_safe/2 for error handling.
%%
%% Template Syntax (Mustache):
%%   - {{variable}} - Simple variable interpolation
%%   - {{#section}}...{{/section}} - Sections (for lists/booleans)
%%   - {{^inverted}}...{{/inverted}} - Inverted sections
%%   - {{! comment}} - Comments (ignored)
%%
%% Example:
%%   render(<<"Hello {{name}}!">>, #{<<"name">> => <<"World">>})
%%   => <<"Hello World!">>
%%
-spec render(template(), variables()) -> binary().
render(Template, Variables) when is_binary(Template), is_map(Variables) ->
    try
        % Convert map keys to atoms for bbmustache compatibility
        % bbmustache expects atom keys or string keys
        AtomVars = convert_keys_to_atoms(Variables),
        Result = bbmustache:render(Template, AtomVars, [
            {key_type, atom},
            {escape_fun, fun(X) -> X end}  % No HTML escaping
        ]),
        iolist_to_binary(Result)
    catch
        error:Reason ->
            error({template_render_error, Reason});
        throw:Reason ->
            error({template_render_error, Reason})
    end;
render(Template, Variables) when is_list(Template), is_map(Variables) ->
    render(list_to_binary(Template), Variables).

%% @doc Safe version of render/2 that returns {ok, Result} | {error, Reason}.
%% Use this when you want to handle errors without try/catch.
%%
-spec render_safe(template(), variables()) -> render_result().
render_safe(Template, Variables) ->
    try
        Result = render(Template, Variables),
        {ok, Result}
    catch
        error:{template_render_error, Reason} ->
            {error, Reason};
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% @doc Validate template syntax.
%% Returns ok if the template is valid, {error, Reason} otherwise.
%%
%% This performs a dry-run parse of the template to check for syntax errors
%% without actually rendering it.
%%
-spec validate(template()) -> ok | {error, term()}.
validate(Template) when is_binary(Template) ->
    try
        % Try to parse the template with empty variables
        % If it parses without error, the syntax is valid
        _ = bbmustache:parse_binary(Template),
        ok
    catch
        error:Reason ->
            {error, {parse_error, Reason}};
        throw:Reason ->
            {error, {parse_error, Reason}}
    end;
validate(Template) when is_list(Template) ->
    validate(list_to_binary(Template)).

%% @doc Check if a string contains template syntax ({{...}}).
%% Useful for detecting if auto-rendering should be applied.
%%
-spec has_template_syntax(binary() | string()) -> boolean().
has_template_syntax(Text) when is_binary(Text) ->
    case binary:match(Text, [<<"{{">>, <<"}}">>]) of
        nomatch -> false;
        _ -> true
    end;
has_template_syntax(Text) when is_list(Text) ->
    has_template_syntax(list_to_binary(Text)).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Convert map with binary or string keys to map with atom keys.
%% This is needed for bbmustache compatibility.
%% Handles nested maps recursively.
%%
-spec convert_keys_to_atoms(map()) -> map().
convert_keys_to_atoms(Map) when is_map(Map) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            AtomKey = key_to_atom(Key),
            NewValue = convert_value(Value),
            maps:put(AtomKey, NewValue, Acc)
        end,
        #{},
        Map
    ).

%% @doc Convert a key (binary, string, or atom) to an atom.
%% Uses existing atoms when possible to avoid atom exhaustion.
%%
-spec key_to_atom(binary() | string() | atom()) -> atom().
key_to_atom(Key) when is_atom(Key) ->
    Key;
key_to_atom(Key) when is_binary(Key) ->
    try
        % Try to use existing atom first
        binary_to_existing_atom(Key, utf8)
    catch
        error:badarg ->
            % Create new atom if it doesn't exist
            % Note: This could lead to atom exhaustion if user input is not controlled
            binary_to_atom(Key, utf8)
    end;
key_to_atom(Key) when is_list(Key) ->
    try
        list_to_existing_atom(Key)
    catch
        error:badarg ->
            list_to_atom(Key)
    end.

%% @doc Convert a value recursively.
%% Handles nested maps and lists of maps.
%%
-spec convert_value(term()) -> term().
convert_value(Value) when is_map(Value) ->
    convert_keys_to_atoms(Value);
convert_value(Value) when is_list(Value) ->
    % Check if it's a list of maps (for sections)
    case lists:all(fun is_map/1, Value) of
        true ->
            [convert_keys_to_atoms(V) || V <- Value];
        false ->
            Value
    end;
convert_value(Value) ->
    Value.
