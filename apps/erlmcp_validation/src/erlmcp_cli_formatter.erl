%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Formatter - ANSI color codes and pretty printing
%%%
%%% Provides color-coded output, table formatting, tree display,
%%% and JSON pretty-printing for the interactive CLI.
%%%
%%% == Features ==
%%%
%%% - ANSI color codes (green, red, yellow, blue, etc.)
%%% - Success/error/warning formatting
%%% - Table formatting for lists
%%% - Tree formatting for hierarchical data
%%% - JSON pretty printing
%%% - Box drawing for sections
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_formatter).

%% API exports
-export([color/2, success/1, error/1, warning/1, info/1, bold/1, underline/1, reset/0,
         format_table/2, format_tree/1, format_tree/2, format_json/1, format_list/1, format_list/2,
         format_box/1, format_box/2, format_status/1, format_result/1, format_connection_info/1,
         strip_colors/1, is_color_enabled/0, enable_colors/0, disable_colors/0, format/3, format/2,
         print/3, print/2, supports_format/1, format_yaml/1, format_csv/2, format_raw/1]).

                            % Color functions

    % Formatting functions

    % Status formatting

    % Utility

    % Structured output (new)

%% ANSI color codes
-define(RESET, "\e[0m").
-define(BOLD, "\e[1m").
-define(UNDERLINE, "\e[4m").
-define(BLACK, "\e[30m").
-define(RED, "\e[31m").
-define(GREEN, "\e[32m").
-define(YELLOW, "\e[33m").
-define(BLUE, "\e[34m").
-define(MAGENTA, "\e[35m").
-define(CYAN, "\e[36m").
-define(WHITE, "\e[37m").
-define(BRIGHT_BLACK, "\e[90m").
-define(BRIGHT_RED, "\e[91m").
-define(BRIGHT_GREEN, "\e[92m").
-define(BRIGHT_YELLOW, "\e[93m").
-define(BRIGHT_BLUE, "\e[94m").
-define(BRIGHT_MAGENTA, "\e[95m").
-define(BRIGHT_CYAN, "\e[96m").
-define(BRIGHT_WHITE, "\e[97m").
%% ETS table for color preference
-define(COLOR_TABLE, erlmcp_cli_colors).

%%====================================================================
%% API Functions - Colors
%%====================================================================

%% @doc Apply color to text
-spec color(atom(), iolist()) -> iolist().
color(Color, Text) ->
    case is_color_enabled() of
        true ->
            [color_code(Color), Text, ?RESET];
        false ->
            Text
    end.

%% @doc Format success message (green)
-spec success(iolist()) -> iolist().
success(Text) ->
    color(green, Text).

%% @doc Format error message (red)
-spec error(iolist()) -> iolist().
error(Text) ->
    color(red, Text).

%% @doc Format warning message (yellow)
-spec warning(iolist()) -> iolist().
warning(Text) ->
    color(yellow, Text).

%% @doc Format info message (blue)
-spec info(iolist()) -> iolist().
info(Text) ->
    color(blue, Text).

%% @doc Make text bold
-spec bold(iolist()) -> iolist().
bold(Text) ->
    case is_color_enabled() of
        true ->
            [?BOLD, Text, ?RESET];
        false ->
            Text
    end.

%% @doc Underline text
-spec underline(iolist()) -> iolist().
underline(Text) ->
    case is_color_enabled() of
        true ->
            [?UNDERLINE, Text, ?RESET];
        false ->
            Text
    end.

%% @doc Reset formatting
-spec reset() -> string().
reset() ->
    ?RESET.

%%====================================================================
%% API Functions - Formatting
%%====================================================================

%% @doc Format data as a table
-spec format_table([map()], [atom()]) -> iolist().
format_table([], _Columns) ->
    info("(empty)");
format_table(Rows, Columns) ->
    % Calculate column widths
    ColWidths = calculate_column_widths(Rows, Columns),

    % Build header
    Header = format_table_row_header(Columns, ColWidths, fun(H) -> bold(atom_to_list(H)) end),
    Separator = format_table_separator(ColWidths),

    % Build data rows
    DataRows = [format_table_row_data(Row, Columns, ColWidths) || Row <- Rows],

    [Header, "\n", Separator, "\n", string:join(DataRows, "\n")].

%% @doc Format hierarchical data as a tree
-spec format_tree(map()) -> iolist().
format_tree(Tree) ->
    format_tree(Tree, 0).

%% @doc Format tree with indentation level
-spec format_tree(map() | list(), non_neg_integer()) -> iolist().
format_tree(Tree, Indent) when is_map(Tree) ->
    maps:fold(fun(Key, Value, Acc) ->
                 Line =
                     [lists:duplicate(Indent, "  "),
                      "├─ ",
                      bold(format_key(Key)),
                      ": ",
                      format_tree_value(Value, Indent + 1),
                      "\n"],
                 [Acc, Line]
              end,
              [],
              Tree);
format_tree(List, Indent) when is_list(List) ->
    lists:map(fun(Item) ->
                 [lists:duplicate(Indent, "  "), "├─ ", format_tree_value(Item, Indent + 1), "\n"]
              end,
              List);
format_tree(Value, _Indent) ->
    io_lib:format("~p", [Value]).

%% @doc Format JSON with pretty printing
-spec format_json(map() | list()) -> iolist().
format_json(Data) ->
    try
        JSON = erlmcp_json_native:encode(Data, [space, {indent, 2}]),
        color(cyan, JSON)
    catch
        _:_ ->
            error(io_lib:format("~p", [Data]))
    end.

%% @doc Format a list with bullets
-spec format_list([term()]) -> iolist().
format_list(Items) ->
    format_list(Items, "•").

%% @doc Format a list with custom bullet
-spec format_list([term()], string()) -> iolist().
format_list(Items, Bullet) ->
    [[" ", Bullet, " ", format_list_item(Item), "\n"] || Item <- Items].

%% @doc Format text in a box
-spec format_box(iolist()) -> iolist().
format_box(Content) ->
    format_box(Content, #{}).

%% @doc Format text in a box with options
-spec format_box(iolist(), map()) -> iolist().
format_box(Content, Opts) ->
    Width = maps:get(width, Opts, 70),
    Title = maps:get(title, Opts, undefined),
    Color = maps:get(color, Opts, white),

    Lines =
        string:split(
            lists:flatten(Content), "\n", all),
    MaxLen = lists:max([string:length(L) || L <- Lines]),
    BoxWidth = max(Width, MaxLen + 4),

    TopBorder = color(Color, ["╔", lists:duplicate(BoxWidth - 2, "═"), "╗"]),
    BottomBorder = color(Color, ["╚", lists:duplicate(BoxWidth - 2, "═"), "╝"]),

    TitleLine =
        case Title of
            undefined ->
                [];
            T ->
                TitleText = io_lib:format(" ~s ", [T]),
                TitleLen =
                    string:length(
                        lists:flatten(TitleText)),
                LeftPad = (BoxWidth - TitleLen - 2) div 2,
                RightPad = BoxWidth - TitleLen - LeftPad - 2,
                [color(Color, "║"),
                 lists:duplicate(LeftPad, " "),
                 bold(TitleText),
                 lists:duplicate(RightPad, " "),
                 color(Color, "║"),
                 "\n"]
        end,

    ContentLines =
        [[color(Color, "║"), " ", pad_right(Line, BoxWidth - 4), " ", color(Color, "║"), "\n"]
         || Line <- Lines],

    [TopBorder, "\n", TitleLine, ContentLines, BottomBorder, "\n"].

%%====================================================================
%% API Functions - Status Formatting
%%====================================================================

%% @doc Format status (passed/failed/warning)
-spec format_status(atom()) -> iolist().
format_status(passed) ->
    success("✓ PASSED");
format_status(failed) ->
    error("✗ FAILED");
format_status(warning) ->
    warning("⚠ WARNING");
format_status(pending) ->
    info("◷ PENDING");
format_status(running) ->
    info("⟳ RUNNING");
format_status(Status) ->
    io_lib:format("~p", [Status]).

%% @doc Format a result map
-spec format_result(map()) -> iolist().
format_result(#{status := Status} = Result) ->
    StatusText = format_status(Status),
    Details = maps:without([status], Result),
    [StatusText, "\n", format_tree(Details)];
format_result(Result) ->
    format_tree(Result).

%% @doc Format connection information
-spec format_connection_info(map()) -> iolist().
format_connection_info(#{transport := Transport,
                         url := Url,
                         status := Status}) ->
    [bold("Connection: "),
     "\n",
     "  Transport: ",
     info(io_lib:format("~p", [Transport])),
     "\n",
     "  URL: ",
     info(Url),
     "\n",
     "  Status: ",
     format_status(Status),
     "\n"];
format_connection_info(Info) ->
    format_tree(Info).

%%====================================================================
%% API Functions - Utility
%%====================================================================

%% @doc Strip ANSI color codes from text
-spec strip_colors(iolist()) -> string().
strip_colors(Text) ->
    re:replace(
        lists:flatten(Text), "\e\\[[0-9;]*m", "", [global, {return, list}]).

%% @doc Check if colors are enabled
-spec is_color_enabled() -> boolean().
is_color_enabled() ->
    case ets:info(?COLOR_TABLE, name) of
        undefined ->
            % Default: check if terminal supports colors
            case os:getenv("TERM") of
                false ->
                    false;
                "dumb" ->
                    false;
                _ ->
                    true
            end;
        ?COLOR_TABLE ->
            case ets:lookup(?COLOR_TABLE, colors_enabled) of
                [{colors_enabled, Enabled}] ->
                    Enabled;
                [] ->
                    true
            end
    end.

%% @doc Enable colors
-spec enable_colors() -> ok.
enable_colors() ->
    ensure_table(),
    ets:insert(?COLOR_TABLE, {colors_enabled, true}),
    ok.

%% @doc Disable colors
-spec disable_colors() -> ok.
disable_colors() ->
    ensure_table(),
    ets:insert(?COLOR_TABLE, {colors_enabled, false}),
    ok.

%%====================================================================
%% API Functions - Structured Output
%%====================================================================

%% @doc Format data with specified format and options
-spec format(atom(), term(), map()) -> iolist().
format(json, Data, Opts) ->
    format_json_with_opts(Data, Opts);
format(yaml, Data, Opts) ->
    format_yaml_with_opts(Data, Opts);
format(csv, Data, Opts) ->
    format_csv_with_opts(Data, Opts);
format(table, Data, Opts) ->
    format_table_structured(Data, Opts);
format(raw, Data, _Opts) ->
    format_raw(Data);
format(_Format, Data, _Opts) ->
    io_lib:format("~p", [Data]).

%% @doc Format data with default options
-spec format(atom(), term()) -> iolist().
format(Format, Data) ->
    format(Format, Data, #{}).

%% @doc Print formatted output to stdout
-spec print(atom(), term(), map()) -> ok.
print(Format, Data, Opts) ->
    Output = format(Format, Data, Opts),
    Quiet = maps:get(quiet, Opts, false),
    case Quiet of
        true ->
            ok;
        false ->
            io:format("~ts~n", [Output])
    end.

%% @doc Print formatted output with default options
-spec print(atom(), term()) -> ok.
print(Format, Data) ->
    print(Format, Data, #{}).

%% @doc Check if format is supported
-spec supports_format(atom() | string() | binary()) -> boolean().
supports_format(json) ->
    true;
supports_format(yaml) ->
    true;
supports_format(csv) ->
    true;
supports_format(table) ->
    true;
supports_format(raw) ->
    true;
supports_format("json") ->
    true;
supports_format("yaml") ->
    true;
supports_format("csv") ->
    true;
supports_format("table") ->
    true;
supports_format("raw") ->
    true;
supports_format(<<"json">>) ->
    true;
supports_format(<<"yaml">>) ->
    true;
supports_format(<<"csv">>) ->
    true;
supports_format(<<"table">>) ->
    true;
supports_format(<<"raw">>) ->
    true;
supports_format(_) ->
    false.

%% @doc Format as YAML
-spec format_yaml(term()) -> iolist().
format_yaml(Data) ->
    format_yaml_with_opts(Data, #{}).

%% @doc Format as CSV
-spec format_csv(term(), map()) -> iolist().
format_csv(Data, Opts) ->
    format_csv_with_opts(Data, Opts).

%% @doc Format as raw (Erlang terms)
-spec format_raw(term()) -> iolist().
format_raw(Data) ->
    io_lib:format("~p", [Data]).

%%====================================================================
%% Internal Functions - Structured Output
%%====================================================================

%% @private Format JSON with options
-spec format_json_with_opts(term(), map()) -> iolist().
format_json_with_opts(Data, Opts) ->
    Verbose = maps:get(verbose, Opts, 3),
    Indent = maps:get(indent, Opts, 2),

    FilteredData = filter_by_verbosity(Data, Verbose),
    JsonData = normalize_for_json(FilteredData),

    case Indent of
        0 ->
            erlmcp_json_native:encode(JsonData);
        _ ->
            erlmcp_json_native:encode(JsonData, [{space, 1}, {indent, Indent}])
    end.

%% @private Normalize data for JSON encoding
-spec normalize_for_json(term()) -> term().
normalize_for_json(Data) when is_map(Data) ->
    maps:fold(fun(K, V, Acc) ->
                 Key = normalize_json_key(K),
                 Value = normalize_for_json(V),
                 Acc#{Key => Value}
              end,
              #{},
              Data);
normalize_for_json(Data) when is_list(Data) ->
    case io_lib:printable_unicode_list(Data) of
        true ->
            list_to_binary(Data);
        false ->
            [normalize_for_json(Item) || Item <- Data]
    end;
normalize_for_json(Data) when is_atom(Data) ->
    atom_to_binary(Data, utf8);
normalize_for_json(Data) when is_pid(Data) ->
    list_to_binary(pid_to_list(Data));
normalize_for_json(Data) when is_reference(Data) ->
    list_to_binary(ref_to_list(Data));
normalize_for_json(Data) when is_port(Data) ->
    list_to_binary(port_to_list(Data));
normalize_for_json(Data) when is_tuple(Data) ->
    list_to_binary(io_lib:format("~p", [Data]));
normalize_for_json(Data) when is_binary(Data); is_number(Data); is_boolean(Data) ->
    Data;
normalize_for_json(Data) ->
    list_to_binary(io_lib:format("~p", [Data])).

-spec normalize_json_key(term()) -> binary().
normalize_json_key(Key) when is_binary(Key) ->
    Key;
normalize_json_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
normalize_json_key(Key) when is_list(Key) ->
    list_to_binary(Key);
normalize_json_key(Key) ->
    list_to_binary(io_lib:format("~p", [Key])).

%% @private Format YAML with options
-spec format_yaml_with_opts(term(), map()) -> iolist().
format_yaml_with_opts(Data, Opts) ->
    Verbose = maps:get(verbose, Opts, 3),
    Indent = maps:get(indent, Opts, 2),

    FilteredData = filter_by_verbosity(Data, Verbose),
    format_yaml_internal(FilteredData, 0, Indent).

-spec format_yaml_internal(term(), non_neg_integer(), non_neg_integer()) -> iolist().
format_yaml_internal(Data, Level, Indent) when is_map(Data) ->
    IndentStr = lists:duplicate(Level * Indent, $ ),
    maps:fold(fun(K, V, Acc) ->
                 Key = format_yaml_key(K),
                 Value = format_yaml_value(V, Level + 1, Indent),
                 [Acc, IndentStr, Key, ": ", Value, "\n"]
              end,
              [],
              Data);
format_yaml_internal(Data, Level, Indent) when is_list(Data) ->
    case io_lib:printable_unicode_list(Data) of
        true ->
            io_lib:format("\"~s\"", [Data]);
        false ->
            IndentStr = lists:duplicate(Level * Indent, $ ),
            [[IndentStr, "- ", format_yaml_value(Item, Level + 1, Indent), "\n"] || Item <- Data]
    end;
format_yaml_internal(Data, _Level, _Indent) ->
    format_yaml_scalar(Data).

-spec format_yaml_value(term(), non_neg_integer(), non_neg_integer()) -> iolist().
format_yaml_value(Data, Level, Indent) when is_map(Data); is_list(Data) ->
    ["\n", format_yaml_internal(Data, Level, Indent)];
format_yaml_value(Data, _Level, _Indent) ->
    format_yaml_scalar(Data).

-spec format_yaml_key(term()) -> iolist().
format_yaml_key(Key) when is_binary(Key) ->
    Key;
format_yaml_key(Key) when is_atom(Key) ->
    atom_to_list(Key);
format_yaml_key(Key) when is_list(Key) ->
    Key;
format_yaml_key(Key) ->
    io_lib:format("~p", [Key]).

-spec format_yaml_scalar(term()) -> iolist().
format_yaml_scalar(Data) when is_binary(Data) ->
    io_lib:format("\"~s\"", [Data]);
format_yaml_scalar(Data) when is_atom(Data) ->
    atom_to_list(Data);
format_yaml_scalar(Data) when is_number(Data) ->
    io_lib:format("~p", [Data]);
format_yaml_scalar(Data) when is_boolean(Data) ->
    atom_to_list(Data);
format_yaml_scalar(Data) when is_pid(Data) ->
    io_lib:format("\"~s\"", [pid_to_list(Data)]);
format_yaml_scalar(Data) ->
    io_lib:format("\"~p\"", [Data]).

%% @private Format CSV with options
-spec format_csv_with_opts(term(), map()) -> iolist().
format_csv_with_opts(Data, Opts) when is_list(Data) ->
    Headers = maps:get(headers, Opts, []),

    HeaderRow =
        case Headers of
            [] ->
                [];
            _ ->
                [format_csv_row(Headers), "\n"]
        end,

    Rows = [format_csv_row(row_to_list(Row)) || Row <- Data],
    [HeaderRow, lists:join("\n", Rows)];
format_csv_with_opts(Data, Opts) when is_map(Data) ->
    format_csv_with_opts([Data], Opts);
format_csv_with_opts(Data, _Opts) ->
    io_lib:format("~p", [Data]).

-spec format_csv_row(list()) -> iolist().
format_csv_row(Row) ->
    Formatted = [format_csv_value(Value) || Value <- Row],
    lists:join(",", Formatted).

-spec format_csv_value(term()) -> iolist().
format_csv_value(Value) when is_binary(Value) ->
    escape_csv_value(binary_to_list(Value));
format_csv_value(Value) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
        true ->
            escape_csv_value(Value);
        false ->
            io_lib:format("\"~p\"", [Value])
    end;
format_csv_value(Value) when is_atom(Value) ->
    atom_to_list(Value);
format_csv_value(Value) when is_number(Value) ->
    io_lib:format("~p", [Value]);
format_csv_value(Value) ->
    io_lib:format("~p", [Value]).

-spec escape_csv_value(string()) -> iolist().
escape_csv_value(Value) ->
    case lists:any(fun(C) -> C =:= $, orelse C =:= $" orelse C =:= $\n end, Value) of
        true ->
            Escaped =
                lists:flatten([case C of
                                   $" ->
                                       "\"\"";
                                   _ ->
                                       C
                               end
                               || C <- Value]),
            [$", Escaped, $"];
        false ->
            Value
    end.

-spec row_to_list(term()) -> list().
row_to_list(Row) when is_map(Row) ->
    maps:values(Row);
row_to_list(Row) when is_list(Row) ->
    Row;
row_to_list(Row) when is_tuple(Row) ->
    tuple_to_list(Row);
row_to_list(Row) ->
    [Row].

%% @private Format table with structured output options
-spec format_table_structured(term(), map()) -> iolist().
format_table_structured(Data, Opts) when is_list(Data), Data =/= [] ->
    Color = maps:get(color, Opts, true),
    Headers = determine_headers(Data, maps:get(headers, Opts, [])),

    Rows = [row_to_list(Row) || Row <- Data],
    AllRows = [Headers | Rows],
    ColWidths = calculate_column_widths_list(AllRows),

    HeaderRow = format_table_row_list(Headers, ColWidths, Color, header),
    Separator = format_table_separator_list(ColWidths),
    DataRows = [format_table_row_list(Row, ColWidths, Color, data) || Row <- Rows],

    [HeaderRow, "\n", Separator, "\n", lists:join("\n", DataRows)];
format_table_structured(Data, Opts) when is_map(Data) ->
    format_table_structured([Data], Opts);
format_table_structured(Data, _Opts) ->
    io_lib:format("~p", [Data]).

-spec determine_headers(list(), list()) -> list().
determine_headers(_Data, Headers) when Headers =/= [] ->
    Headers;
determine_headers([First | _], []) when is_map(First) ->
    [atom_to_list(K) || K <- maps:keys(First)];
determine_headers([First | _], []) when is_tuple(First) ->
    [integer_to_list(I) || I <- lists:seq(1, tuple_size(First))];
determine_headers(_Data, []) ->
    ["Value"].

-spec calculate_column_widths_list(list()) -> [non_neg_integer()].
calculate_column_widths_list(Rows) ->
    case Rows of
        [] ->
            [];
        [FirstRow | _] ->
            NumCols = length(row_to_list(FirstRow)),
            InitWidths = lists:duplicate(NumCols, 0),

            lists:foldl(fun(Row, Widths) ->
                           RowList = row_to_list(Row),
                           lists:zipwith(fun(Value, CurrentWidth) ->
                                            ValueStr = format_table_value_str(Value),
                                            max(CurrentWidth, length(lists:flatten(ValueStr)))
                                         end,
                                         RowList,
                                         Widths)
                        end,
                        InitWidths,
                        Rows)
    end.

-spec format_table_row_list(list(), [non_neg_integer()], boolean(), header | data) -> iolist().
format_table_row_list(Row, ColWidths, Color, RowType) ->
    RowList = row_to_list(Row),
    FormattedCells =
        lists:zipwith(fun(Value, Width) ->
                         ValueStr = format_table_value_str(Value),
                         Padding = Width - length(lists:flatten(ValueStr)),
                         PaddedValue = [ValueStr, lists:duplicate(Padding, $ )],
                         case {Color, RowType} of
                             {true, header} ->
                                 bold(PaddedValue);
                             _ ->
                                 PaddedValue
                         end
                      end,
                      RowList,
                      ColWidths),

    [$|, lists:join(" | ", FormattedCells), $|].

-spec format_table_separator_list([non_neg_integer()]) -> iolist().
format_table_separator_list(ColWidths) ->
    Separators = [lists:duplicate(Width, $-) || Width <- ColWidths],
    [$|, lists:join("-+-", Separators), $|].

-spec format_table_value_str(term()) -> iolist().
format_table_value_str(Value) when is_binary(Value) ->
    binary_to_list(Value);
format_table_value_str(Value) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
        true ->
            Value;
        false ->
            io_lib:format("~p", [Value])
    end;
format_table_value_str(Value) when is_atom(Value) ->
    atom_to_list(Value);
format_table_value_str(Value) when is_number(Value) ->
    io_lib:format("~p", [Value]);
format_table_value_str(Value) when is_pid(Value) ->
    pid_to_list(Value);
format_table_value_str(Value) ->
    io_lib:format("~p", [Value]).

%% @private Filter data by verbosity level
-spec filter_by_verbosity(term(), 1..5) -> term().
filter_by_verbosity(Data, Verbosity) when is_map(Data) ->
    maps:filter(fun(Key, _Value) -> should_include_field(Key, Verbosity) end, Data);
filter_by_verbosity(Data, _Verbosity) ->
    Data.

-spec should_include_field(term(), 1..5) -> boolean().
should_include_field(_Key, 5) ->
    true;
should_include_field(Key, Level) ->
    Importance = field_importance(Key),
    Importance =< Level.

-spec field_importance(term()) -> 1..5.
field_importance(Key) when is_atom(Key) ->
    field_importance(atom_to_binary(Key, utf8));
field_importance(Key) when is_binary(Key) ->
    case Key of
        <<"status">> ->
            1;
        <<"error">> ->
            1;
        <<"message">> ->
            1;
        <<"result">> ->
            1;
        _ ->
            case binary:match(Key, [<<"time">>, <<"count">>, <<"total">>]) of
                nomatch ->
                    case binary:match(Key, [<<"details">>, <<"summary">>, <<"metrics">>]) of
                        nomatch ->
                            case binary:match(Key, [<<"debug">>, <<"trace">>, <<"stack">>]) of
                                nomatch ->
                                    5;
                                _ ->
                                    4
                            end;
                        _ ->
                            3
                    end;
                _ ->
                    2
            end
    end;
field_importance(_Key) ->
    3.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Get color code for atom
-spec color_code(atom()) -> string().
color_code(black) ->
    ?BLACK;
color_code(red) ->
    ?RED;
color_code(green) ->
    ?GREEN;
color_code(yellow) ->
    ?YELLOW;
color_code(blue) ->
    ?BLUE;
color_code(magenta) ->
    ?MAGENTA;
color_code(cyan) ->
    ?CYAN;
color_code(white) ->
    ?WHITE;
color_code(bright_black) ->
    ?BRIGHT_BLACK;
color_code(bright_red) ->
    ?BRIGHT_RED;
color_code(bright_green) ->
    ?BRIGHT_GREEN;
color_code(bright_yellow) ->
    ?BRIGHT_YELLOW;
color_code(bright_blue) ->
    ?BRIGHT_BLUE;
color_code(bright_magenta) ->
    ?BRIGHT_MAGENTA;
color_code(bright_cyan) ->
    ?BRIGHT_CYAN;
color_code(bright_white) ->
    ?BRIGHT_WHITE;
color_code(_) ->
    ?RESET.

%% @doc Calculate column widths for table
-spec calculate_column_widths([map()], [atom()]) -> map().
calculate_column_widths(Rows, Columns) ->
    InitWidths = maps:from_list([{Col, string:length(atom_to_list(Col))} || Col <- Columns]),
    lists:foldl(fun(Row, Widths) ->
                   maps:map(fun(Col, CurrentWidth) ->
                               Value = maps:get(Col, Row, ""),
                               ValueStr = format_cell_value(Value),
                               max(CurrentWidth, string:length(ValueStr))
                            end,
                            Widths)
                end,
                InitWidths,
                Rows).

%% @doc Format table row (header)
-spec format_table_row_header([atom()], map(), fun((atom()) -> iolist())) -> iolist().
format_table_row_header(Columns, ColWidths, Formatter) ->
    Cells = [pad_right(Formatter(Col), maps:get(Col, ColWidths)) || Col <- Columns],
    string:join(Cells, " │ ").

%% @doc Format table row (data)
-spec format_table_row_data(map(), [atom()], map()) -> iolist().
format_table_row_data(Row, Columns, ColWidths) ->
    Cells =
        [pad_right(format_cell_value(maps:get(Col, Row, "")), maps:get(Col, ColWidths))
         || Col <- Columns],
    string:join(Cells, " │ ").

%% @doc Format table separator
-spec format_table_separator(map()) -> iolist().
format_table_separator(ColWidths) ->
    Separators = [lists:duplicate(Width, "─") || Width <- maps:values(ColWidths)],
    string:join(Separators, "─┼─").

%% @doc Format cell value
-spec format_cell_value(term()) -> string().
format_cell_value(Value) when is_binary(Value) ->
    binary_to_list(Value);
format_cell_value(Value) when is_atom(Value) ->
    atom_to_list(Value);
format_cell_value(Value) when is_integer(Value) ->
    integer_to_list(Value);
format_cell_value(Value) when is_float(Value) ->
    float_to_list(Value, [{decimals, 2}]);
format_cell_value(Value) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
        true ->
            Value;
        false ->
            io_lib:format("~p", [Value])
    end;
format_cell_value(Value) ->
    io_lib:format("~p", [Value]).

%% @doc Pad string to the right
-spec pad_right(iolist(), non_neg_integer()) -> string().
pad_right(Str, Width) ->
    StrFlat = lists:flatten(Str),
    StrLen = string:length(StrFlat),
    if StrLen >= Width ->
           StrFlat;
       true ->
           StrFlat ++ lists:duplicate(Width - StrLen, $ )
    end.

%% @doc Format tree value
-spec format_tree_value(term(), non_neg_integer()) -> iolist().
format_tree_value(Value, Indent) when is_map(Value) ->
    ["\n", format_tree(Value, Indent)];
format_tree_value(Value, Indent) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
        true ->
            io_lib:format("~s", [Value]);
        false ->
            ["\n", format_tree(Value, Indent)]
    end;
format_tree_value(Value, _Indent) ->
    io_lib:format("~p", [Value]).

%% @doc Format map key
-spec format_key(term()) -> string().
format_key(Key) when is_atom(Key) ->
    atom_to_list(Key);
format_key(Key) when is_binary(Key) ->
    binary_to_list(Key);
format_key(Key) ->
    io_lib:format("~p", [Key]).

%% @doc Format list item
-spec format_list_item(term()) -> iolist().
format_list_item(Item) when is_binary(Item) ->
    Item;
format_list_item(Item) when is_list(Item) ->
    case io_lib:printable_unicode_list(Item) of
        true ->
            Item;
        false ->
            io_lib:format("~p", [Item])
    end;
format_list_item(Item) ->
    io_lib:format("~p", [Item]).

%% @doc Ensure ETS table exists
-spec ensure_table() -> ok.
ensure_table() ->
    case ets:info(?COLOR_TABLE, name) of
        undefined ->
            ets:new(?COLOR_TABLE, [named_table, public, set]),
            ok;
        ?COLOR_TABLE ->
            ok
    end.
