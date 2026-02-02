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

-export([encode/1, decode/1]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Encode data using native OTP 27+ JSON
-spec encode(term()) -> binary().
encode(Data) ->
    erlmcp_json_native:encode(Data).

%% @doc Decode JSON binary using native OTP 27+ JSON
-spec decode(binary()) -> term().
decode(Binary) when is_binary(Binary) ->
    erlmcp_json_native:decode(Binary).
