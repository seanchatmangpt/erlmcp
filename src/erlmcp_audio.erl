%%% @doc
%%% erlmcp_audio - Audio Content Type Support for MCP 2025-11-25
%%%
%%% This module provides functions for handling audio content types in MCP responses,
%%% including base64 encoding, MIME type validation, and optional metadata support.
%%%
%%% Supports multiple audio formats:
%%% - audio/wav (WAV)
%%% - audio/mpeg (MP3)
%%% - audio/aac (AAC)
%%% - audio/flac (FLAC)
%%% - audio/ogg (OGG Vorbis)
%%% - audio/webm (WebM)
%%% - audio/opus (Opus)
%%%
%%% Gap #34: Audio Content Type Support from MCP 2025-11-25 Compliance Review
%%%
%%% @since 0.7.0
-module(erlmcp_audio).

-include("erlmcp.hrl").

%% API exports
-export([
    encode_audio_content/2,
    encode_audio_content_with_metadata/3,
    validate_audio_mime_type/1,
    encode_audio_base64/1,
    decode_audio_base64/1,
    is_valid_audio_format/1,
    supported_audio_formats/0
]).

%% Type definitions
-type audio_mime_type() :: binary().
-type audio_binary() :: binary().
-type base64_string() :: binary().
-type audio_metadata() :: #{
    duration => float(),
    sample_rate => pos_integer(),
    channels => pos_integer(),
    bitrate => pos_integer()
}.

-export_type([audio_mime_type/0, audio_binary/0, base64_string/0, audio_metadata/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%% @doc
%%% Encode raw audio binary data as base64 for JSON-RPC transport.
%%% Returns a map suitable for inclusion in MCP content blocks.
%%%
%%% Example:
%%%   AudioBinary = <<...raw audio bytes...>>,
%%%   Content = erlmcp_audio:encode_audio_content(AudioBinary, <<"audio/wav">>),
%%%   % Returns:
%%%   % #{
%%%   %     <<"type">> => <<"audio">>,
%%%   %     <<"data">> => <<"base64-encoded-data">>,
%%%   %     <<"mimeType">> => <<"audio/wav">>
%%%   % }
%%%
%%% @param AudioBinary The raw binary audio data
%%% @param MimeType The audio MIME type (e.g., <<"audio/wav">>, <<"audio/mp3">>)
%%% @return A map with audio content encoding, or {error, Reason}
%%% @end
-spec encode_audio_content(audio_binary(), audio_mime_type()) ->
    {ok, map()} | {error, {atom(), binary()}}.
encode_audio_content(AudioBinary, MimeType) when is_binary(AudioBinary), is_binary(MimeType) ->
    case validate_audio_mime_type(MimeType) of
        ok ->
            Base64Data = encode_audio_base64(AudioBinary),
            {ok, #{
                ?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_AUDIO,
                ?MCP_PARAM_DATA => Base64Data,
                ?MCP_PARAM_MIME_TYPE => MimeType
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%%% @doc
%%% Encode audio content with optional metadata (duration, sample rate, channels, bitrate).
%%%
%%% Example:
%%%   Metadata = #{
%%%       duration => 125.5,
%%%       sample_rate => 48000,
%%%       channels => 2,
%%%       bitrate => 320000
%%%   },
%%%   Content = erlmcp_audio:encode_audio_content_with_metadata(
%%%       AudioBinary,
%%%       <<"audio/mp3">>,
%%%       Metadata
%%%   ),
%%%
%%% @param AudioBinary The raw binary audio data
%%% @param MimeType The audio MIME type
%%% @param Metadata Optional audio metadata (duration, sample_rate, channels, bitrate)
%%% @return A map with audio content and metadata, or {error, Reason}
%%% @end
-spec encode_audio_content_with_metadata(audio_binary(), audio_mime_type(), audio_metadata()) ->
    {ok, map()} | {error, {atom(), binary()}}.
encode_audio_content_with_metadata(AudioBinary, MimeType, Metadata)
  when is_binary(AudioBinary), is_binary(MimeType), is_map(Metadata) ->
    case encode_audio_content(AudioBinary, MimeType) of
        {ok, BaseContent} ->
            EnrichedMetadata = extract_valid_metadata(Metadata),
            EnrichedContent = case EnrichedMetadata of
                EmptyMap when map_size(EmptyMap) =:= 0 ->
                    BaseContent;
                _ ->
                    BaseContent#{
                        ?MCP_PARAM_METADATA => EnrichedMetadata
                    }
            end,
            {ok, EnrichedContent};
        Error ->
            Error
    end.

%%% @doc
%%% Validate that a MIME type is a supported audio format.
%%%
%%% Supports:
%%% - audio/wav
%%% - audio/mpeg (and audio/mp3)
%%% - audio/aac
%%% - audio/flac
%%% - audio/ogg
%%% - audio/webm
%%% - audio/opus
%%%
%%% @param MimeType The MIME type to validate
%%% @return ok | {error, {unsupported_audio_format, MimeType}}
%%% @end
-spec validate_audio_mime_type(audio_mime_type()) ->
    ok | {error, {atom(), binary()}}.
validate_audio_mime_type(MimeType) when is_binary(MimeType) ->
    SupportedFormats = supported_audio_formats(),
    case lists:member(MimeType, SupportedFormats) of
        true ->
            ok;
        false ->
            {error, {unsupported_audio_format, MimeType}}
    end.

%%% @doc
%%% Check if a MIME type is a valid audio format.
%%% Returns true/false instead of ok/error.
%%%
%%% @param MimeType The MIME type to check
%%% @return true | false
%%% @end
-spec is_valid_audio_format(audio_mime_type()) -> boolean().
is_valid_audio_format(MimeType) when is_binary(MimeType) ->
    case validate_audio_mime_type(MimeType) of
        ok -> true;
        {error, _} -> false
    end.

%%% @doc
%%% Return the list of supported audio MIME types.
%%%
%%% @return List of supported audio MIME types
%%% @end
-spec supported_audio_formats() -> [audio_mime_type()].
supported_audio_formats() ->
    [
        ?MCP_MIME_AUDIO_WAV,
        ?MCP_MIME_AUDIO_MPEG,
        ?MCP_MIME_AUDIO_MP3,
        ?MCP_MIME_AUDIO_AAC,
        ?MCP_MIME_AUDIO_FLAC,
        ?MCP_MIME_AUDIO_OGG,
        ?MCP_MIME_AUDIO_WEBM,
        ?MCP_MIME_AUDIO_OPUS
    ].

%%% @doc
%%% Encode raw audio binary data as base64 for JSON transport.
%%% Uses Erlang's built-in base64 module.
%%%
%%% @param AudioBinary The raw binary audio data
%%% @return Base64-encoded binary suitable for JSON
%%% @end
-spec encode_audio_base64(audio_binary()) -> base64_string().
encode_audio_base64(AudioBinary) when is_binary(AudioBinary) ->
    base64:encode(AudioBinary).

%%% @doc
%%% Decode base64-encoded audio data back to binary.
%%% Inverse of encode_audio_base64/1.
%%%
%%% @param Base64Data Base64-encoded binary
%%% @return Decoded binary audio data or {error, Reason}
%%% @end
-spec decode_audio_base64(base64_string()) ->
    {ok, audio_binary()} | {error, atom()}.
decode_audio_base64(Base64Data) when is_binary(Base64Data) ->
    try
        {ok, base64:decode(Base64Data)}
    catch
        error:_ ->
            {error, invalid_base64}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%% Extract valid metadata fields from the provided metadata map.
%%% Only keeps fields that match the defined audio metadata spec.
%%%
%%% @param Metadata Input metadata map
%%% @return Filtered metadata map with only valid fields
%%% @end
-spec extract_valid_metadata(audio_metadata()) -> map().
extract_valid_metadata(Metadata) when is_map(Metadata) ->
    ValidFields = [
        duration,
        sample_rate,
        channels,
        bitrate
    ],
    maps:with(ValidFields, Metadata).
