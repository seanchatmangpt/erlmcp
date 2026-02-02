-module(erlmcp_tool_router).

-include("erlmcp.hrl").

%% Tool routing using OTP 28 PCRE2 regex patterns
%% Detects and routes tool invocations from AI model output

%% API exports
-export([detect_tool_call/1,
         detect_tool_call/2,
         route_to_tool/2,
         add_pattern/3,
         remove_pattern/1,
         list_patterns/0,
         compile_pattern/1,
         validate_pattern/1,
         extract_args/2]).

%% Type exports
-export_type([tool_pattern/0,
              route_result/0,
              pattern_name/0,
              capture_result/0]).

%%====================================================================
%% Types
%%====================================================================

-type pattern_name() :: binary().
-type regex_pattern() :: binary().
-type capture_map() :: #{binary() => binary()}.

-record(tool_pattern,
        {name :: pattern_name(),
         pattern :: regex_pattern(),
         handler :: module() | function(),
         options :: [term()]}).  % re:run/3 options

-type tool_pattern() :: #tool_pattern{}.
-type route_result() :: {ok, module(), binary(), capture_map()} |
                       {ok, function(), binary(), capture_map()} |
                       {error, unknown_tool | pattern_not_found | invalid_pattern}.

-type capture_result() :: {ok, capture_map()} | {error, no_match | invalid_capture}.

%%====================================================================
%% Constants - Built-in Tool Patterns
%%====================================================================

%% Calculator pattern: calculate(2 + 2)
-define(CALCULATOR_PATTERN,
        "calculate\\(\\s*(?<expr>[^)]+)\\s*\\)").

%% Search pattern: search for Erlang patterns
-define(SEARCH_PATTERN,
        "search\\s+for\\s+(?<query>[^.!?]+)[.!?]?").

%% Code execution pattern: execute erlang: io:format("Hello")
-define(CODE_EXEC_PATTERN,
        "execute\\s+(?<lang>\\w+)\\s*:\\s*(?<code>[^]+?)(?=\\s*(?:execute|search|calculate|$))").

%% Generic tool call pattern: tool_name(arg1, arg2)
-define(GENERIC_TOOL_PATTERN,
        "(?<tool>[a-z_][a-z0-9_]*)\\(\\s*(?<args>[^)]*)\\s*\\)").

%%====================================================================
%% Pattern Registry (ETS table)
%%====================================================================

-define(PATTERN_TABLE, erlmcp_tool_patterns).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Detect tool call in text using default built-in patterns
-spec detect_tool_call(binary()) -> route_result() | not_found.
detect_tool_call(Text) when is_binary(Text) ->
    Patterns = get_builtin_patterns(),
    detect_tool_call(Text, Patterns).

%% @doc Detect tool call in text using provided patterns
-spec detect_tool_call(binary(), [tool_pattern()]) -> route_result() | not_found.
detect_tool_call(Text, Patterns) when is_binary(Text), is_list(Patterns) ->
    case try_match_patterns(Text, Patterns) of
        {ok, Pattern, Captures} ->
            ToolName = maps:get(<<"tool">>, Captures, Pattern#tool_pattern.name),
            Args = maps:get(<<"args">>, Captures, <<>>),
            route_to_tool(ToolName, Args, Captures);
        nomatch ->
            not_found
    end.

%% @doc Route to specific tool handler by name
-spec route_to_tool(binary(), binary()) -> {ok, module() | function(), binary()} | {error, unknown_tool}.
route_to_tool(<<"calculator">>, Expression) ->
    {ok, erlmcp_tool_calculator, Expression};
route_to_tool(<<"search">>, Query) ->
    {ok, erlmcp_tool_search, Query};
route_to_tool(<<"execute">>, Code) ->
    {ok, erlmcp_tool_execute, Code};
route_to_tool(ToolName, _) ->
    {error, {unknown_tool, ToolName}}.

%% @doc Route tool with full capture context (internal)
-spec route_to_tool(binary(), binary(), capture_map()) -> route_result().
route_to_tool(ToolName, Args, Captures) ->
    case route_to_tool(ToolName, Args) of
        {ok, Handler, _} ->
            {ok, Handler, Args, Captures};
        {error, _} = Error ->
            Error
    end.

%% @doc Add custom pattern to router
-spec add_pattern(pattern_name(), regex_pattern(), module() | function()) ->
                         ok | {error, invalid_pattern | already_exists}.
add_pattern(Name, Pattern, Handler) when is_binary(Name), is_binary(Pattern) ->
    case validate_pattern(Pattern) of
        ok ->
            case ets:lookup(?PATTERN_TABLE, Name) of
                [] ->
                    ToolPattern = #tool_pattern{name = Name,
                                              pattern = Pattern,
                                              handler = Handler,
                                              options = [unicode, {capture, all_names, binary}]},
                    ets:insert(?PATTERN_TABLE, {Name, ToolPattern}),
                    ok;
                _ ->
                    {error, already_exists}
            end;
        Error ->
            Error
    end.

%% @doc Remove pattern from router
-spec remove_pattern(pattern_name()) -> ok | {error, pattern_not_found}.
remove_pattern(Name) when is_binary(Name) ->
    case ets:lookup(?PATTERN_TABLE, Name) of
        [] ->
            {error, pattern_not_found};
        _ ->
            ets:delete(?PATTERN_TABLE, Name),
            ok
    end.

%% @doc List all registered patterns
-spec list_patterns() -> [{pattern_name(), regex_pattern()}].
list_patterns() ->
    ets:fold(fun({Name, #tool_pattern{pattern = Pattern}}, Acc) ->
                    [{Name, Pattern} | Acc]
             end, [], ?PATTERN_TABLE).

%% @doc Compile PCRE2 regex pattern
-spec compile_pattern(binary()) -> {ok, re:mp()} | {error, term()}.
compile_pattern(Pattern) when is_binary(Pattern) ->
    try
        {ok, re:compile(Pattern, [unicode])}
    catch
        _:_:Stacktrace ->
            {error, {compile_error, Stacktrace}}
    end.

%% @doc Validate PCRE2 regex pattern syntax
-spec validate_pattern(binary()) -> ok | {error, invalid_pattern}.
validate_pattern(Pattern) when is_binary(Pattern) ->
    case re:compile(Pattern, [unicode]) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, {invalid_pattern, Reason}}
    end.

%% @doc Extract named captures from matched text
-spec extract_args(binary(), binary()) -> capture_result().
extract_args(Text, Pattern) when is_binary(Text), is_binary(Pattern) ->
    case re:run(Text, Pattern, [unicode, {capture, all_names, binary}]) of
        {match, Captures} when is_list(Captures) ->
            case extract_capture_names(Pattern) of
                {ok, Names} ->
                    CaptureMap = lists:foldl(fun(Name, Acc) ->
                                                   Index = get_capture_index(Pattern, Name),
                                                   Value = lists:nth(Index + 1, Captures),
                                                   maps:put(Name, Value, Acc)
                                           end, #{}, Names),
                    {ok, CaptureMap};
                {error, _} = Error ->
                    Error
            end;
        nomatch ->
            {error, no_match}
    end;
extract_args(_, _) ->
    {error, invalid_capture}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Get built-in tool patterns
-spec get_builtin_patterns() -> [tool_pattern()].
get_builtin_patterns() ->
    [#tool_pattern{name = <<"calculator">>,
                  pattern = list_to_binary(?CALCULATOR_PATTERN),
                  handler = erlmcp_tool_calculator,
                  options = [unicode, {capture, all_names, binary}]},
     #tool_pattern{name = <<"search">>,
                  pattern = list_to_binary(?SEARCH_PATTERN),
                  handler = erlmcp_tool_search,
                  options = [unicode, {capture, all_names, binary}]},
     #tool_pattern{name = <<"execute">>,
                  pattern = list_to_binary(?CODE_EXEC_PATTERN),
                  handler = erlmcp_tool_execute,
                  options = [unicode, {capture, all_names, binary}]}].

%% @doc Try to match text against patterns (first match wins)
-spec try_match_patterns(binary(), [tool_pattern()]) ->
                                {ok, tool_pattern(), capture_map()} | nomatch.
try_match_patterns(_Text, []) ->
    nomatch;
try_match_patterns(Text, [#tool_pattern{pattern = Pattern, options = Options} = ToolPattern | Rest]) ->
    case re:run(Text, Pattern, Options) of
        {match, Captures} when is_list(Captures) ->
            case extract_capture_names(Pattern) of
                {ok, Names} ->
                    CaptureMap = build_capture_map(Names, Captures),
                    {ok, ToolPattern, CaptureMap};
                {error, _} ->
                    try_match_patterns(Text, Rest)
            end;
        nomatch ->
            try_match_patterns(Text, Rest)
    end.

%% @doc Extract named capture group names from pattern
-spec extract_capture_names(binary()) -> {ok, [binary()]} | {error, term()}.
extract_capture_names(Pattern) ->
    case re:run(Pattern, <<"\\(\\?<([a-zA-Z_][a-zA-Z0-9_]*)>">>, [global, unicode, {capture, all_but_first, binary}]) of
        {match, Names} when is_list(Names) ->
            {ok, lists:flatten(Names)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get index of named capture group
-spec get_capture_index(binary(), binary()) -> pos_integer().
get_capture_index(Pattern, Name) ->
    NamedCaptures = "(?<" ++ binary_to_list(Name) ++ ">",
    {match, [{Start, _}]} = re:run(Pattern, list_to_binary(NamedCaptures), [unicode]),
    Count = count_captures_before(Pattern, Start),
    Count + 1.

%% @doc Count number of capture groups before position
-spec count_captures_before(binary(), non_neg_integer()) -> non_neg_integer().
count_captures_before(Pattern, Pos) ->
    Before = binary:part(Pattern, 0, Pos),
    {match, AllOpens} = re:run(Before, <<"\\((?!\\?)">>, [global, unicode]),
    {match, NamedOpens} = re:run(Before, <<"\\(\\?<">>, [global, unicode]),
    {match, NonCapturing} = re:run(Before, <<"\\(\\?">>, [global, unicode]),
    length(AllOpens) - length(NamedOpens) - length(NonCapturing).

%% @doc Build capture map from names and values
-spec build_capture_map([binary()], [binary()]) -> capture_map().
build_capture_map(Names, Values) ->
    lists:foldl(fun({Name, Value}, Acc) ->
                   maps:put(Name, Value, Acc)
                end, #{}, lists:zip(Names, Values)).

%%====================================================================
%% Initialization
%%====================================================================

%% @doc Initialize pattern table (called from application start)
-spec init() -> ok.
init() ->
    case ets:whereis(?PATTERN_TABLE) of
        undefined ->
            ets:new(?PATTERN_TABLE, [named_table, public, set]),
            ok;
        _ ->
            ok
    end.

%% @doc Stop pattern table (called from application stop)
-spec stop() -> ok.
stop() ->
    case ets:whereis(?PATTERN_TABLE) of
        undefined ->
            ok;
        _ ->
            ets:delete(?PATTERN_TABLE),
            ok
    end.
