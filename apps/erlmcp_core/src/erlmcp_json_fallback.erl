%%%-------------------------------------------------------------------
%%% @doc
%%% JSON Fallback Implementation
%%%
%%% This module provides a JSON fallback mechanism for when the
%%% native JSON module is not available (OTP < 27).
%%%
%%% Features:
%%%   - Automatic detection of native JSON availability
%%%   - Fallback to JSX when native JSON is not available
%%%   - Performance optimization for native JSON when available
%%%   - API compatibility with native JSON module
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_json_fallback).

%% API
-export([
    enable_fallback/0,
    disable_fallback/0,
    is_fallback_enabled/0,
    encode/1,
    decode/1,
    encode_pretty/1,
    decode_pretty/1,
    get_encoding_module/0,
    get_encoding_method/0
]).

%% Types
-type json_data() :: map() | list() | atom() | integer() | float() | binary().
-type json_error() :: {error, term()}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Enable JSON fallback mode
-spec enable_fallback() -> ok.
enable_fallback() ->
    application:set_env(erlmcp, json_fallback_enabled, true),
    application:set_env(erlmcp, json_library, jsx),
    logger:info("JSON fallback enabled - using JSX module"),
    ok.

%% @doc Disable JSON fallback mode
-spec disable_fallback() -> ok.
disable_fallback() ->
    case erlang:function_exported(json, encode, 1) of
        true ->
            application:set_env(erlmcp, json_fallback_enabled, false),
            application:set_env(erlmcp, json_library, native),
            logger:info("JSON fallback disabled - using native JSON module");
        false ->
            logger:warning("Cannot disable fallback - native JSON not available"),
            enable_fallback()
    end,
    ok.

%% @doc Check if fallback mode is enabled
-spec is_fallback_enabled() -> boolean().
is_fallback_enabled() ->
    application:get_env(erlmcp, json_fallback_enabled, false).

%% @doc Encode data to JSON (automatic module selection)
-spec encode(json_data()) -> binary().
encode(Data) ->
    case get_encoding_module() of
        jsx ->
            jsx:encode(Data);
        native ->
            json:encode(Data)
    end.

%% @doc Decode JSON data (automatic module selection)
-spec decode(binary()) -> map() | list().
decode(Binary) ->
    case get_encoding_module() of
        jsx ->
            jsx:decode(Binary, [return_maps]);
        native ->
            json:decode(Binary)
    end.

%% @doc Encode data to pretty JSON
-spec encode_pretty(json_data()) -> binary().
encode_pretty(Data) ->
    case get_encoding_module() of
        jsx ->
            jsx:encode(Data, [{indent, 2}]);
        native ->
            json:encode(Data, [{indent, 2}])
    end.

%% @doc Decode pretty JSON data
-spec decode_pretty(binary()) -> map() | list().
decode_pretty(Binary) ->
    decode(Binary).  % Pretty decoding same as normal for now

%% @doc Get current encoding module
-spec get_encoding_module() -> jsx | native.
get_encoding_module() ->
    case application:get_env(erlmcp, json_library, auto) of
        auto ->
            case erlang:function_exported(json, encode, 1) of
                true -> native;
                false -> jsx
            end;
        jsx ->
            jsx;
        native ->
            case erlang:function_exported(json, encode, 1) of
                true -> native;
                false -> jsx
            end
    end.

%% @doc Get current encoding method
-spec get_encoding_method() -> atom().
get_encoding_method() ->
    Module = get_encoding_module(),
    case Module of
        jsx -> jsx_encode;
        native -> native_json
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Check if native JSON is available
-spec has_native_json() -> boolean().
has_native_json() ->
    erlang:function_exported(json, encode, 1).

%% @private Check if JSX is available
-spec has_jsx() -> boolean().
has_jsx() ->
    erlang:function_exported(jsx, encode, 1).

%% @private Log encoding performance metrics
-spec log_encoding_performance(atom(), pos_integer()) -> ok.
log_encoding_performance(Module, Time) ->
    logger:debug("JSON encoding using ~s took ~p microseconds", [Module, Time]).