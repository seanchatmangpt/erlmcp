-module(erlmcp_json).

%% @doc Native JSON encoding/decoding using Erlang OTP 28.3.1+ json module
%% No backward compatibility - OTP 28.3.1+ required
%% All decode operations return maps (not atoms) for object keys via return_maps option

-export([encode/1, encode/2, decode/1, decode/2]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Encode Erlang term to JSON binary
-spec encode(term()) -> binary().
encode(Term) ->
    try
        json:encode(Term)
    catch
        error:Reason ->
            error({encode_error, Reason})
    end.

%% @doc Encode Erlang term to JSON binary with options
%% Options are passed directly to json:encode/2
-spec encode(term(), list()) -> binary().
encode(Term, Opts) ->
    try
        json:encode(Term, Opts)
    catch
        error:Reason ->
            error({encode_error, Reason})
    end.

%% @doc Decode JSON binary to Erlang term
%% Always returns maps (not atoms) for object keys
-spec decode(binary()) -> term().
decode(Binary) when is_binary(Binary) ->
    try
        json:decode(Binary, [return_maps])
    catch
        error:badarg ->
            error({decode_error, invalid_json});
        error:Reason ->
            error({decode_error, Reason})
    end.

%% @doc Decode JSON binary to Erlang term with options
%% Ensures return_maps is included for consistency
-spec decode(binary(), list()) -> term().
decode(Binary, Opts) when is_binary(Binary) ->
    FinalOpts = ensure_return_maps(Opts),
    try
        json:decode(Binary, FinalOpts)
    catch
        error:badarg ->
            error({decode_error, invalid_json});
        error:Reason ->
            error({decode_error, Reason})
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Ensure return_maps option is present in options list
-spec ensure_return_maps(list()) -> list().
ensure_return_maps(Opts) ->
    case lists:member(return_maps, Opts) of
        true ->
            Opts;
        false ->
            [return_maps | Opts]
    end.
