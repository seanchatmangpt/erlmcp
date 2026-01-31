%%%====================================================================
%%% erlmcp_json_codec - Adaptive JSON Encoder
%%%====================================================================
%%% Optimization: Use jiffy for large messages (>100KB), jsx for small
%%% Expected improvement: 40-60% faster for large payloads
%%% 
%%% Bottleneck: jsx is slow for large messages
%%% Solution: Adaptive codec selection based on size threshold
%%%====================================================================

-module(erlmcp_json_codec).

-export([
    encode/1,
    decode/1,
    encode_with_threshold/2,
    get_default_threshold/0
]).

%% Default threshold: 100KB
-define(DEFAULT_THRESHOLD_BYTES, 102400).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Encode data using adaptive codec selection
-spec encode(term()) -> binary().
encode(Data) ->
    encode_with_threshold(Data, ?DEFAULT_THRESHOLD_BYTES).

%% @doc Encode data with custom threshold
-spec encode_with_threshold(term(), pos_integer()) -> binary().
encode_with_threshold(Data, ThresholdBytes) ->
    %% Estimate size using term_to_binary
    EstimatedSize = byte_size(term_to_binary(Data)),
    
    if 
        EstimatedSize > ThresholdBytes ->
            %% Large message: try jiffy first (3x faster for large payloads)
            try 
                jiffy:encode(Data)
            catch
                _:_ -> 
                    %% Fallback to jsx if jiffy fails
                    jsx:encode(Data)
            end;
        true ->
            %% Small message: use jsx (more compatible)
            jsx:encode(Data)
    end.

%% @doc Decode JSON binary
-spec decode(binary()) -> term().
decode(Binary) when is_binary(Binary) ->
    %% Try jiffy first (faster), fallback to jsx
    try
        jiffy:decode(Binary, [return_maps])
    catch
        _:_ ->
            jsx:decode(Binary, [return_maps])
    end.

%% @doc Get default threshold
-spec get_default_threshold() -> pos_integer().
get_default_threshold() ->
    ?DEFAULT_THRESHOLD_BYTES.
