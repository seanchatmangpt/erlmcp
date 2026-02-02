-module(erlmcp_json_native).

%% @doc Native JSON encoding/decoding using Erlang OTP 27+ json module
%% No backward compatibility - OTP 27+ required
%% All decode operations return maps (not atoms) for object keys
%% All encode operations return binaries (not iolists) for compatibility
%%
%% This module provides a drop-in replacement for jsx with native JSON
%% from OTP 27+ (EEP-68). Benefits:
%% - No NIF dependency (pure Erlang)
%% - RFC 8259 compliant
%% - Faster than jsx for most operations
%% - Built into stdlib (no external dependency)

-export([encode/1, encode/2, decode/1, decode/2]).

%%====================================================================
%% Types
%%====================================================================

-type json_term() :: term().
-type encode_options() :: list().
-type decode_options() :: list().

-export_type([json_term/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Encode Erlang term to JSON binary
%% Equivalent to jsx:encode/1 but returns binary instead of iolist
%%
%% Examples:
%% ```
%% > erlmcp_json_native:encode(#{<<"foo">> => <<"bar">>}).
%% <<"{\"foo\":\"bar\"}">>
%% '''
-spec encode(json_term()) -> binary().
encode(Term) ->
    try
        iolist_to_binary(json:encode(Term))
    catch
        error:Reason ->
            error({encode_error, Reason})
    end.

%% @doc Encode Erlang term to JSON binary with options
%% Note: Native json module supports custom encoders, but we don't expose
%% that complexity here. Use json:encode/2 directly if needed.
%%
%% Options are currently unused (for API compatibility with jsx).
-spec encode(json_term(), encode_options()) -> binary().
encode(Term, _Opts) ->
    %% Native JSON doesn't have the same options as jsx
    %% We ignore options for simplicity
    try
        iolist_to_binary(json:encode(Term))
    catch
        error:Reason ->
            error({encode_error, Reason})
    end.

%% @doc Decode JSON binary to Erlang term
%% Always returns maps (not atoms) for object keys
%%
%% Examples:
%% ```
%% > erlmcp_json_native:decode(<<"{\"foo\":\"bar\"}">>).
%% #{<<"foo">> => <<"bar">>}
%% '''
-spec decode(binary()) -> json_term().
decode(Binary) when is_binary(Binary) ->
    try
        json:decode(Binary)
    catch
        error:badarg ->
            error({decode_error, invalid_json});
        error:Reason ->
            error({decode_error, Reason})
    end.

%% @doc Decode JSON binary to Erlang term with options
%%
%% Native JSON always returns maps, so options like [return_maps]
%% are ignored but accepted for API compatibility.
-spec decode(binary(), decode_options()) -> json_term().
decode(Binary, _Opts) when is_binary(Binary) ->
    %% Native JSON always returns maps (like jsx with [return_maps])
    %% Options are accepted but ignored for API compatibility
    try
        json:decode(Binary)
    catch
        error:badarg ->
            error({decode_error, invalid_json});
        error:Reason ->
            error({decode_error, Reason})
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% No internal functions - thin wrapper around native json module
