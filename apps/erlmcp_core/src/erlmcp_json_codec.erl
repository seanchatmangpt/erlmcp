%%%====================================================================
%%% erlmcp_json_codec - Native JSON Encoder
%%%====================================================================
%%% Using OTP 27+ native JSON module (EEP-68)
%%% Benefits: No NIF dependency, RFC 8259 compliant, faster than jsx
%%%
%%% Simplified from adaptive codec (jiffy/jsx) to native JSON only
%%% Native JSON is fast enough for all message sizes
%%%====================================================================

-module(erlmcp_json_codec).
-moduledoc """
Native JSON encoder facade (EEP-68, OTP 27+).

No NIF dependency, RFC 8259 compliant. Simplified from the adaptive
jiffy/jsx codec to native JSON only; native JSON is fast enough for all
message sizes.
""" .

-export([encode/1, decode/1]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Encode data using native OTP 27+ JSON
-doc "
Encodes an Erlang term to a JSON binary.
" .
-spec encode(term()) -> binary().
encode(Data) ->
    erlmcp_json_native:encode(Data).

%% @doc Decode JSON binary using native OTP 27+ JSON
-doc "
Decodes a JSON binary to Erlang terms.
" .
-spec decode(binary()) -> term().
decode(Binary) when is_binary(Binary) ->
    erlmcp_json_native:decode(Binary).
