-module(erlmcp_json).

%% @doc Unified JSON interface for OTP 25+ compatibility
%% Provides a consistent API for JSON encoding/decoding across OTP versions:
%% - OTP 28+: Uses native json:encode/decode with return_maps option
%% - OTP 25-27: Uses jsx:encode/decode with return_maps option
%% Both implementations return maps (not atoms) for object keys.

-include("otp_compat.hrl").

-export([
    encode/1,
    encode/2,
    decode/1,
    decode/2
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Encode Erlang term to JSON binary
%% Uses native json module on OTP 28+, jsx on earlier versions
-spec encode(term()) -> binary().
-if(?OTP_RELEASE >= 28).
encode(Term) ->
    try json:encode(Term)
    catch
        error:Reason -> error({encode_error, Reason})
    end.
-else.
encode(Term) ->
    try jsx:encode(Term)
    catch
        error:Reason -> error({encode_error, Reason})
    end.
-endif.

%% @doc Encode Erlang term to JSON binary with options
%% Options are normalized across implementations for compatibility
-spec encode(term(), list()) -> binary().
-if(?OTP_RELEASE >= 28).
encode(Term, _Opts) ->
    %% Native json module doesn't need options for basic encoding
    try json:encode(Term)
    catch
        error:Reason -> error({encode_error, Reason})
    end.
-else.
encode(Term, Opts) ->
    try jsx:encode(Term, Opts)
    catch
        error:Reason -> error({encode_error, Reason})
    end.
-endif.

%% @doc Decode JSON binary to Erlang term
%% Always returns maps (not atoms) for object keys
-spec decode(binary()) -> term().
-if(?OTP_RELEASE >= 28).
decode(Binary) when is_binary(Binary) ->
    try json:decode(Binary, [return_maps])
    catch
        error:badarg -> error({decode_error, invalid_json});
        error:Reason -> error({decode_error, Reason})
    end.
-else.
decode(Binary) when is_binary(Binary) ->
    try jsx:decode(Binary, [return_maps])
    catch
        error:badarg -> error({decode_error, invalid_json});
        error:Reason -> error({decode_error, Reason})
    end.
-endif.

%% @doc Decode JSON binary to Erlang term with options
%% Options include [return_maps] by default for consistency
-spec decode(binary(), list()) -> term().
-if(?OTP_RELEASE >= 28).
decode(Binary, Opts) when is_binary(Binary) ->
    %% Ensure return_maps is included
    FinalOpts = ensure_return_maps(Opts),
    try json:decode(Binary, FinalOpts)
    catch
        error:badarg -> error({decode_error, invalid_json});
        error:Reason -> error({decode_error, Reason})
    end.
-else.
decode(Binary, Opts) when is_binary(Binary) ->
    %% Ensure return_maps is included
    FinalOpts = ensure_return_maps(Opts),
    try jsx:decode(Binary, FinalOpts)
    catch
        error:badarg -> error({decode_error, invalid_json});
        error:Reason -> error({decode_error, Reason})
    end.
-endif.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Ensure return_maps option is present in options list
-spec ensure_return_maps(list()) -> list().
ensure_return_maps(Opts) ->
    case lists:member(return_maps, Opts) of
        true -> Opts;
        false -> [return_maps | Opts]
    end.
