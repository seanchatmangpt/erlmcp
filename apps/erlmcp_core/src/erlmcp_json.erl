-module(erlmcp_json).
-moduledoc """
Native JSON encoding/decoding using the OTP 27+ `json` module (EEP-68).

No backward compatibility - OTP 28.3.1+ required. All decode operations
return maps (not atoms) for object keys via the `return_maps` option.
""" .

%% @doc Native JSON encoding/decoding using Erlang OTP 28.3.1+ json module
%% No backward compatibility - OTP 28.3.1+ required
%% All decode operations return maps (not atoms) for object keys via return_maps option

-export([encode/1, encode/2, decode/1, decode/2]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Encode Erlang term to JSON binary
-doc "
Encodes an Erlang term to a JSON binary.
" .
-spec encode(term()) -> binary().
encode(Term) ->
    try
        iolist_to_binary(json:encode(Term))
    catch
        error:Reason ->
            error({encode_error, Reason})
    end.

%% @doc Encode Erlang term to JSON binary with jsx-compatible options.
%% No callers pass encode options today; formatting-only jsx options
%% ({space, _}, {indent, _}, clean, implicit) are accepted and ignored;
%% any other option errors instead of silently changing semantics.
-doc "
Encodes an Erlang term to a JSON binary with jsx-compatible encode options.
" .
-spec encode(term(), list()) -> binary().
encode(Term, Opts) when is_list(Opts) ->
    case jsx_noop_filter(Opts) of
        [] ->
            encode(Term);
        Unsupported ->
            erlang:error({encode_error, {unsupported_options, Unsupported}})
    end;
encode(Term, Encoder) when is_function(Encoder, 2) ->
    try
        iolist_to_binary(json:encode(Term, Encoder))
    catch
        error:Reason ->
            erlang:error({encode_error, Reason})
    end.

%% @doc Decode JSON binary to Erlang term
%% Always returns maps (not atoms) for object keys (native json default)
-doc "
Decodes a JSON binary to Erlang terms; object keys become maps.
" .
-spec decode(binary()) -> term().
decode(Binary) when is_binary(Binary) ->
    try
        json:decode(Binary)
    catch
        error:Reason ->
            error({decode_error, Reason})
    end.

%% @doc Decode JSON binary to Erlang term with jsx-compatible options.
%% Supported: return_maps (native default), {labels, binary} (native default),
%% {labels, atom} (keys converted via binary_to_atom), {labels, existing_atom}.
%% Options may be passed as a bare list or an options-or-nil proplist.
-doc "
Decodes a JSON binary with decoder options (e.g. `label` for key conversion).
" .
-spec decode(binary(), list()) -> term().
decode(Binary, Opts) when is_binary(Binary), is_list(Opts) ->
    try
        case label_option(Opts) of
            binary -> json:decode(Binary);
            atom -> decode_with_labels(Binary, fun(K) -> binary_to_atom(K, utf8) end);
            existing_atom -> decode_with_labels(Binary, fun(K) -> binary_to_existing_atom(K, utf8) end)
        end
    catch
        error:Reason ->
            error({decode_error, Reason})
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec label_option(list()) -> binary | atom | existing_atom.
label_option(Opts) ->
    case [L || {labels, L} <- Opts] of
        [] -> binary;
        [L | _] -> L
    end.

jsx_encode_noops() ->
    [clean, implicit, escape_forward_slashes].

%% Formatting-only jsx options: {space, N}/{indent, N} have no semantic
%% effect on the JSON bytes; accepted and ignored.
jsx_noop_filter(Opts) ->
    Noops = jsx_encode_noops(),
    lists:filter(fun({space, _}) -> false;
                    ({indent, _}) -> false;
                    (O) -> not lists:member(O, Noops)
                 end, Opts).

-spec decode_with_labels(binary(), fun((binary()) -> atom())) -> term().
decode_with_labels(Binary, Converter) ->
    ObjectPush = fun(Key, Value, Acc) -> [{Converter(Key), Value} | Acc] end,
    {Result, _Acc, <<>>} = json:decode(Binary, [], #{object_push => ObjectPush}),
    Result.
