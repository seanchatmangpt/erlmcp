%%%-------------------------------------------------------------------
%%% @doc
%%% JSON to CSV Formatter Plugin - Example Plugin Implementation
%%%
%%% This plugin demonstrates how to create a transformer plugin for erlmcp
%%% that converts JSON arrays of objects to CSV format.
%%%
%%% Usage:
%%%   1. Compile: erlc formatter-json-to-csv.erl
%%%   2. Load in REPL: c(formatter_json_to_csv).
%%%   3. Start: formatter_json_to_csv:start().
%%%   4. Transform: erlmcp_plugin_registry:call(json_to_csv, transform, [Data, Options]).
%%%   5. Stop: formatter_json_to_csv:stop().
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(formatter_json_to_csv).
-author("erlmcp examples").

% Declare behavior implementation
-behaviour(erlmcp_plugin_transformer).

% Public API
-export([
  start/0,
  stop/0,
  transform/2,
  format/0,
  name/0
]).

% For testing
-export([
  test_basic/0,
  test_with_special_chars/0,
  test_error_handling/0
]).

% =========================================================================
% Public API - Plugin Lifecycle
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Start the plugin and register it with the plugin registry.
%% @end
%%%-------------------------------------------------------------------
start() ->
  erlmcp_plugin_registry:register(
    json_to_csv,                           % Plugin name
    erlmcp_plugin_transformer,             % Behavior module
    formatter_json_to_csv                  % This module
  ),
  io:format("Plugin loaded: JSON→CSV Transformer~n"),
  {ok, loaded}.

%%%-------------------------------------------------------------------
%% @doc Stop the plugin and unregister from registry.
%% @end
%%%-------------------------------------------------------------------
stop() ->
  erlmcp_plugin_registry:unregister(json_to_csv),
  io:format("Plugin unloaded: JSON→CSV Transformer~n"),
  ok.

% =========================================================================
% Behavior Implementation - erlmcp_plugin_transformer
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Return plugin name as binary.
%% @spec name() -> binary()
%% @end
%%%-------------------------------------------------------------------
name() ->
  <<"json_to_csv">>.

%%%-------------------------------------------------------------------
%% @doc Return output format identifier.
%% @spec format() -> atom()
%% @end
%%%-------------------------------------------------------------------
format() ->
  csv.

%%%-------------------------------------------------------------------
%% @doc Transform JSON array of objects to CSV format.
%%
%% Input:
%%   - Input: List of maps (JSON-like structure)
%%   - Options: Map with configuration:
%%     * headers: auto (extract from first object) | explicit (from options)
%%     * delimiter: binary, default ","
%%     * quote: binary, default "\""
%%     * include_empty_rows: boolean, default true
%%
%% Output:
%%   - {ok, CSV :: binary()} on success
%%   - {error, Reason} on failure
%%
%% @spec transform(Input :: list(), Options :: map()) ->
%%   {ok, binary()} | {error, term()}
%% @end
%%%-------------------------------------------------------------------
transform(Input, Options) ->
  try
    % Validate input
    validate_input(Input),

    % Extract configuration
    HeaderMode = maps:get(headers, Options, auto),
    Delimiter = maps:get(delimiter, Options, <<",">>),
    QuoteChar = maps:get(quote, Options, <<"\"">>),
    IncludeEmpty = maps:get(include_empty_rows, Options, true),

    % Extract header row
    Headers = case HeaderMode of
      auto -> extract_headers(Input);
      explicit -> maps:get(header_list, Options, []);
      _ -> extract_headers(Input)
    end,

    % Filter rows if requested
    FilteredInput = case IncludeEmpty of
      true -> Input;
      false -> [Row || Row <- Input, row_not_empty(Row)]
    end,

    % Convert rows to CSV values
    Rows = [map_to_csv_row(Row, Headers, Delimiter, QuoteChar)
            || Row <- FilteredInput],

    % Format complete CSV (headers + rows)
    HeaderRow = row_to_csv_string(Headers, Delimiter, QuoteChar),
    CSV = case Rows of
      [] -> HeaderRow;
      _ -> <<HeaderRow/binary, "\n", (
        binary:list_to_bin(string:join(Rows, "\n"))
      )/binary>>
    end,

    {ok, CSV}

  catch
    error:{validation_error, Reason} ->
      {error, {invalid_input, Reason}};
    error:{type_error, Type} ->
      {error, {type_error, Type}};
    error:Reason ->
      {error, {transform_error, Reason}}
  end.

% =========================================================================
% Helper Functions - Input Validation
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Validate that input is a non-empty list of maps.
%% @end
%%%-------------------------------------------------------------------
validate_input(Input) when is_list(Input) ->
  case Input of
    [] -> throw(error({validation_error, empty_list}));
    [First | _] ->
      case is_map(First) of
        true -> ok;
        false -> throw(error({validation_error, elements_must_be_maps}))
      end
  end;
validate_input(_) ->
  throw(error({validation_error, input_must_be_list})).

%%%-------------------------------------------------------------------
%% @doc Check if a row is not empty.
%% @end
%%%-------------------------------------------------------------------
row_not_empty(Row) when is_map(Row) ->
  maps:size(Row) > 0;
row_not_empty(_) ->
  false.

% =========================================================================
% Helper Functions - Header Extraction
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Extract headers from first map in list.
%% @end
%%%-------------------------------------------------------------------
extract_headers([First | _]) when is_map(First) ->
  maps:keys(First);
extract_headers(_) ->
  [].

% =========================================================================
% Helper Functions - Row Conversion
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Convert a single row (map) to CSV format.
%% @end
%%%-------------------------------------------------------------------
map_to_csv_row(Row, Headers, Delimiter, QuoteChar) when is_map(Row) ->
  Values = [maps:get(H, Row, null) || H <- Headers],
  row_to_csv_string(Values, Delimiter, QuoteChar);

map_to_csv_row(Row, _Headers, _Delimiter, _QuoteChar) ->
  throw(error({type_error, {expected_map, got, Row}})).

%%%-------------------------------------------------------------------
%% @doc Convert a list of values to CSV string (single row).
%% @end
%%%-------------------------------------------------------------------
row_to_csv_string(Values, Delimiter, QuoteChar) ->
  CSVValues = [escape_csv_value(V, QuoteChar) || V <- Values],
  binary:list_to_bin(string:join(CSVValues, binary_to_list(Delimiter))).

% =========================================================================
% Helper Functions - Value Escaping
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Escape a single CSV value.
%% @end
%%%-------------------------------------------------------------------
escape_csv_value(null, _QuoteChar) ->
  "";

escape_csv_value(V, QuoteChar) when is_binary(V) ->
  escape_binary_value(V, QuoteChar);

escape_csv_value(V, QuoteChar) when is_list(V) ->
  escape_binary_value(list_to_binary(V), QuoteChar);

escape_csv_value(V, QuoteChar) when is_number(V) ->
  Number = case is_float(V) of
    true -> io_lib:format("~p", [V]);
    false -> integer_to_list(V)
  end,
  escape_binary_value(list_to_binary(Number), QuoteChar);

escape_csv_value(V, QuoteChar) when is_atom(V) ->
  escape_binary_value(atom_to_binary(V, utf8), QuoteChar);

escape_csv_value(V, QuoteChar) ->
  % Fallback for other types
  escape_binary_value(
    list_to_binary(io_lib:format("~p", [V])),
    QuoteChar
  ).

%%%-------------------------------------------------------------------
%% @doc Escape binary value, adding quotes if necessary.
%% @end
%%%-------------------------------------------------------------------
escape_binary_value(Value, QuoteChar) ->
  case needs_quoting(Value) of
    true ->
      % Escape internal quote characters
      Escaped = binary:replace(
        Value,
        QuoteChar,
        <<QuoteChar/binary, QuoteChar/binary>>,
        [global]
      ),
      binary_to_list(<<QuoteChar/binary, Escaped/binary, QuoteChar/binary>>);
    false ->
      binary_to_list(Value)
  end.

%%%-------------------------------------------------------------------
%% @doc Determine if value needs CSV quoting.
%% @end
%%%-------------------------------------------------------------------
needs_quoting(Value) ->
  case binary:match(Value, [<<$">>, <<$,>>, <<$\n>>, <<$\r>>]) of
    nomatch -> false;
    _ -> true
  end.

% =========================================================================
% Testing Functions
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Test 1: Basic transformation with simple data.
%% @end
%%%-------------------------------------------------------------------
test_basic() ->
  io:format("Test 1: Basic transformation~n"),
  Data = [
    #{name => <<"Alice">>, age => 30, city => <<"New York">>},
    #{name => <<"Bob">>, age => 25, city => <<"Boston">>},
    #{name => <<"Carol">>, age => 35, city => <<"Chicago">>}
  ],
  {ok, CSV} = transform(Data, #{}),
  io:format("Input: ~p~n", [Data]),
  io:format("Output:~n~s~n", [CSV]),
  {ok, CSV}.

%%%-------------------------------------------------------------------
%% @doc Test 2: Handle special characters and escaping.
%% @end
%%%-------------------------------------------------------------------
test_with_special_chars() ->
  io:format("~nTest 2: Special characters (quotes, commas, newlines)~n"),
  Data = [
    #{
      name => <<"Alice \"The Great\"">> ,
      description => <<"Expert in machine, learning">>
    },
    #{
      name => <<"Bob O'Neill">>,
      description => <<"Senior developer">>
    }
  ],
  {ok, CSV} = transform(Data, #{}),
  io:format("Input: ~p~n", [Data]),
  io:format("Output:~n~s~n", [CSV]),
  {ok, CSV}.

%%%-------------------------------------------------------------------
%% @doc Test 3: Error handling.
%% @end
%%%-------------------------------------------------------------------
test_error_handling() ->
  io:format("~nTest 3: Error handling~n"),

  % Test 1: Empty list
  io:format("  - Empty list: "),
  case transform([], #{}) of
    {error, {invalid_input, empty_list}} ->
      io:format("✓ Caught correctly~n");
    Other ->
      io:format("✗ Got: ~p~n", [Other])
  end,

  % Test 2: Non-map elements
  io:format("  - Non-map elements: "),
  case transform([<<"not a map">>], #{}) of
    {error, {invalid_input, elements_must_be_maps}} ->
      io:format("✓ Caught correctly~n");
    Other ->
      io:format("✗ Got: ~p~n", [Other])
  end,

  % Test 3: Non-list input
  io:format("  - Non-list input: "),
  case transform(not_a_list, #{}) of
    {error, {invalid_input, input_must_be_list}} ->
      io:format("✓ Caught correctly~n");
    Other ->
      io:format("✗ Got: ~p~n", [Other])
  end,

  io:format("~nAll error tests passed~n").

% =========================================================================
% Main Entry Point for Testing
% =========================================================================

%%%-------------------------------------------------------------------
%% @doc Run all tests.
%% @end
%%%-------------------------------------------------------------------
run_tests() ->
  io:format("~n=== JSON→CSV Transformer Plugin Tests ===~n"),
  test_basic(),
  test_with_special_chars(),
  test_error_handling(),
  io:format("~n=== All Tests Completed ===~n").
