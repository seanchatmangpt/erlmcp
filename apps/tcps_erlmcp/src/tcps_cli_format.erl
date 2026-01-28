%%%-----------------------------------------------------------------------------
%%% @doc TCPS CLI Output Formatting
%%%
%%% Provides multiple output formats for TCPS CLI commands:
%%% - Table (pretty-printed ASCII tables)
%%% - JSON (machine-readable)
%%% - Markdown (documentation-friendly)
%%% - Colored output support
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_cli_format).
-compile({no_auto_import,[error/2]}).

-export([
    output/2,
    output/3,
    table/2,
    json/1,
    markdown/2,
    error/1,
    error/2,
    success/1,
    success/2,
    warning/1,
    warning/2,
    info/1,
    info/2,
    format_timestamp/1,
    format_duration/1,
    format_bytes/1,
    format_percentage/1
]).

-type format_type() :: table | json | markdown.
-type color() :: black | red | green | yellow | blue | magenta | cyan | white.

-export_type([format_type/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%% @doc Output data in the configured format
-spec output(Data :: term(), Format :: format_type()) -> ok.
output(Data, Format) ->
    output(Data, Format, #{}).

-spec output(Data :: term(), Format :: format_type(), Opts :: map()) -> ok.
output(Data, table, Opts) when is_list(Data) ->
    Headers = maps:get(headers, Opts, infer_headers(Data)),
    table(Headers, Data);
output(Data, table, Opts) when is_map(Data) ->
    table([Data], Opts);
output(Data, json, _Opts) ->
    json(Data);
output(Data, markdown, Opts) ->
    Headers = maps:get(headers, Opts, infer_headers([Data])),
    markdown(Headers, [Data]);
output(Data, Format, _Opts) ->
    io:format(standard_error, "Unknown format: ~p~n", [Format]),
    io:format("~p~n", [Data]).

%% @doc Format as ASCII table
-spec table(Headers :: [atom()], Data :: [map()]) -> ok.
table(Headers, Data) ->
    % Calculate column widths
    Widths = calculate_widths(Headers, Data),

    % Print header
    print_table_separator(Widths),
    print_table_row(Headers, Widths, header),
    print_table_separator(Widths),

    % Print rows
    lists:foreach(fun(Row) ->
        Values = [maps:get(H, Row, <<>>) || H <- Headers],
        print_table_row(Values, Widths, data)
    end, Data),

    print_table_separator(Widths),
    ok.

%% @doc Format as JSON
-spec json(Data :: term()) -> ok.
json(Data) ->
    JsonBin = jsx:encode(Data, [pretty]),
    io:format("~s~n", [JsonBin]).

%% @doc Format as Markdown table
-spec markdown(Headers :: [atom()], Data :: [map()]) -> ok.
markdown(Headers, Data) ->
    % Print header
    io:format("| ~s |~n", [string:join([atom_to_list(H) || H <- Headers], " | ")]),

    % Print separator
    Separators = lists:duplicate(length(Headers), "---"),
    io:format("| ~s |~n", [string:join(Separators, " | ")]),

    % Print rows
    lists:foreach(fun(Row) ->
        Values = [format_value(maps:get(H, Row, <<>>)) || H <- Headers],
        io:format("| ~s |~n", [string:join(Values, " | ")])
    end, Data),
    ok.

%% @doc Print error message
-spec error(Message :: string()) -> ok.
error(Message) ->
    error("~s", [Message]).

-spec error(Format :: string(), Args :: [term()]) -> ok.
error(Format, Args) ->
    case tcps_cli_config:get(color_output, true) of
        true ->
            io:format(standard_error, color(red, "Error: " ++ Format ++ "~n"), Args);
        false ->
            %% Using io:format instead of error/2 to avoid conflict with BIF
            io:format(standard_error, "Error: " ++ Format ++ "~n", Args)
    end.

%% @doc Print success message
-spec success(Message :: string()) -> ok.
success(Message) ->
    success("~s", [Message]).

-spec success(Format :: string(), Args :: [term()]) -> ok.
success(Format, Args) ->
    case tcps_cli_config:get(color_output, true) of
        true ->
            io:format(color(green, "✓ " ++ Format ++ "~n"), Args);
        false ->
            io:format("Success: " ++ Format ++ "~n", Args)
    end.

%% @doc Print warning message
-spec warning(Message :: string()) -> ok.
warning(Message) ->
    warning("~s", [Message]).

-spec warning(Format :: string(), Args :: [term()]) -> ok.
warning(Format, Args) ->
    case tcps_cli_config:get(color_output, true) of
        true ->
            io:format(standard_error, color(yellow, "Warning: " ++ Format ++ "~n"), Args);
        false ->
            io:format(standard_error, "Warning: " ++ Format ++ "~n", Args)
    end.

%% @doc Print info message
-spec info(Message :: string()) -> ok.
info(Message) ->
    info("~s", [Message]).

-spec info(Format :: string(), Args :: [term()]) -> ok.
info(Format, Args) ->
    case tcps_cli_config:get(verbose, false) of
        true -> io:format(Format ++ "~n", Args);
        false -> ok
    end.

%% @doc Format timestamp
-spec format_timestamp(integer()) -> binary().
format_timestamp(Milliseconds) when is_integer(Milliseconds) ->
    Seconds = Milliseconds div 1000,
    DateTime = calendar:system_time_to_universal_time(Seconds, second),
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                      [Year, Month, Day, Hour, Minute, Second])
    );
format_timestamp(_) ->
    <<"N/A">>.

%% @doc Format duration in human-readable form
-spec format_duration(Seconds :: number()) -> binary().
format_duration(Seconds) when Seconds < 60 ->
    iolist_to_binary(io_lib:format("~.1fs", [Seconds]));
format_duration(Seconds) when Seconds < 3600 ->
    Minutes = Seconds / 60,
    iolist_to_binary(io_lib:format("~.1fm", [Minutes]));
format_duration(Seconds) ->
    Hours = Seconds / 3600,
    iolist_to_binary(io_lib:format("~.1fh", [Hours])).

%% @doc Format bytes in human-readable form
-spec format_bytes(Bytes :: non_neg_integer()) -> binary().
format_bytes(Bytes) when Bytes < 1024 ->
    iolist_to_binary(io_lib:format("~pB", [Bytes]));
format_bytes(Bytes) when Bytes < 1024 * 1024 ->
    KB = Bytes / 1024,
    iolist_to_binary(io_lib:format("~.1fKB", [KB]));
format_bytes(Bytes) when Bytes < 1024 * 1024 * 1024 ->
    MB = Bytes / (1024 * 1024),
    iolist_to_binary(io_lib:format("~.1fMB", [MB]));
format_bytes(Bytes) ->
    GB = Bytes / (1024 * 1024 * 1024),
    iolist_to_binary(io_lib:format("~.1fGB", [GB])).

%% @doc Format percentage
-spec format_percentage(Value :: float()) -> binary().
format_percentage(Value) ->
    iolist_to_binary(io_lib:format("~.1f%", [Value])).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private Infer headers from data
-spec infer_headers([map()]) -> [atom()].
infer_headers([]) ->
    [];
infer_headers([First | _]) ->
    lists:sort(maps:keys(First)).

%% @private Calculate column widths
-spec calculate_widths([atom()], [map()]) -> [non_neg_integer()].
calculate_widths(Headers, Data) ->
    lists:map(fun(Header) ->
        HeaderLen = length(atom_to_list(Header)),
        DataLens = lists:map(fun(Row) ->
            Value = maps:get(Header, Row, <<>>),
            byte_size(format_value(Value))
        end, Data),
        lists:max([HeaderLen | DataLens])
    end, Headers).

%% @private Print table separator
-spec print_table_separator([non_neg_integer()]) -> ok.
print_table_separator(Widths) ->
    Parts = [lists:duplicate(W + 2, $-) || W <- Widths],
    io:format("+~s+~n", [string:join(Parts, "+")]).

%% @private Print table row
-spec print_table_row([term()], [non_neg_integer()], header | data) -> ok.
print_table_row(Values, Widths, Type) ->
    Formatted = lists:zipwith(fun(Val, Width) ->
        Str = case Type of
            header -> string:uppercase(atom_to_list(Val));
            data -> binary_to_list(format_value(Val))
        end,
        string:pad(Str, Width, trailing)
    end, Values, Widths),
    io:format("| ~s |~n", [string:join(Formatted, " | ")]).

%% @private Format value for display
-spec format_value(term()) -> binary().
format_value(V) when is_binary(V) -> V;
format_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
format_value(V) when is_integer(V) -> integer_to_binary(V);
format_value(V) when is_float(V) -> iolist_to_binary(io_lib:format("~.2f", [V]));
format_value(V) when is_list(V) ->
    case io_lib:printable_unicode_list(V) of
        true -> list_to_binary(V);
        false -> iolist_to_binary(io_lib:format("~p", [V]))
    end;
format_value(V) -> iolist_to_binary(io_lib:format("~p", [V])).

%% @private ANSI color codes
-spec color(color(), string()) -> string().
color(Color, Text) ->
    ColorCode = color_code(Color),
    lists:flatten(["\e[", integer_to_list(ColorCode), "m", Text, "\e[0m"]).

-spec color_code(color()) -> non_neg_integer().
color_code(black) -> 30;
color_code(red) -> 31;
color_code(green) -> 32;
color_code(yellow) -> 33;
color_code(blue) -> 34;
color_code(magenta) -> 35;
color_code(cyan) -> 36;
color_code(white) -> 37.
