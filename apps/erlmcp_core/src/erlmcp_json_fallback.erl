%%%-------------------------------------------------------------------
%%% @doc
%%% JSON Fallback Implementation (DEPRECATED - Native JSON Only)
%%%
%%% This module provided fallback support for OTP < 27.
%%% Since erlmcp now requires OTP 28+, this module is deprecated
%%% and all functions now use the native json module directly.
%%%
%%% Migration Guide:
%%%   - Replace erlmcp_json_fallback:encode/1 with erlmcp_json_native:encode/1
%%%   - Replace erlmcp_json_fallback:decode/1 with erlmcp_json_native:decode/1
%%%   - Fallback mode is no longer supported
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_json_fallback).

%% API
-export([
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

%% @doc Encode data to JSON using native json module
%% Note: Fallback to jsx is no longer supported (requires OTP 28+)
-spec encode(json_data()) -> binary().
encode(Data) ->
    erlmcp_json_native:encode(Data).

%% @doc Decode JSON data using native json module
%% Note: Fallback to jsx is no longer supported (requires OTP 28+)
-spec decode(binary()) -> map() | list().
decode(Binary) ->
    erlmcp_json_native:decode(Binary).

%% @doc Encode data to pretty JSON
%% Note: Native JSON doesn't support pretty print options directly
%% For formatted output, use external tooling like jq or post-process
-spec encode_pretty(json_data()) -> binary().
encode_pretty(Data) ->
    %% Native json module doesn't have pretty print
    %% For formatted JSON, use external tools or implement custom formatter
    erlmcp_json_native:encode(Data).

%% @doc Decode pretty JSON data (same as regular decode)
-spec decode_pretty(binary()) -> map() | list().
decode_pretty(Binary) ->
    erlmcp_json_native:decode(Binary).

%% @doc Get current encoding module (always native now)
-spec get_encoding_module() -> native.
get_encoding_module() ->
    native.

%% @doc Get current encoding method (always native_json now)
-spec get_encoding_method() -> native_json.
get_encoding_method() ->
    native_json.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Check if native JSON is available (always true in OTP 28+)
-spec has_native_json() -> boolean().
has_native_json() ->
    erlang:function_exported(json, encode, 1).
