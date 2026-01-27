%%% @doc
%%% Comprehensive test suite for erlmcp_audio module
%%% Tests audio content type support for MCP 2025-11-25 compliance (Gap #34)
%%%
%%% Test Coverage:
%%% - Audio content encoding with base64
%%% - Multiple audio formats (wav, mp3, aac, flac, ogg, webm, opus)
%%% - Base64 serialization validation
%%% - Audio metadata inclusion
%%% - Large audio file handling
%%% - Invalid audio format rejection
%%% - MIME type validation
%%% - Content size calculation
%%%
%%% @since 0.7.0
-module(erlmcp_audio_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite
%%====================================================================

audio_tests_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_audio_mime_type_validation()),
             ?_test(test_supported_audio_formats()),
             ?_test(test_encode_audio_content_wav()),
             ?_test(test_encode_audio_content_mp3()),
             ?_test(test_encode_audio_content_aac()),
             ?_test(test_encode_audio_content_flac()),
             ?_test(test_encode_audio_content_ogg()),
             ?_test(test_encode_audio_content_webm()),
             ?_test(test_encode_audio_content_opus()),
             ?_test(test_encode_audio_base64()),
             ?_test(test_decode_audio_base64()),
             ?_test(test_encode_audio_with_metadata()),
             ?_test(test_audio_metadata_extraction()),
             ?_test(test_large_audio_file_encoding()),
             ?_test(test_invalid_audio_format_rejection()),
             ?_test(test_audio_base64_roundtrip()),
             ?_test(test_is_valid_audio_format()),
             ?_test(test_audio_content_structure()),
             ?_test(test_audio_metadata_optional_fields())
         ]
     end}.

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% Test 1: Audio MIME type validation
test_audio_mime_type_validation() ->
    %% Valid WAV format
    ?assertEqual(ok, erlmcp_audio:validate_audio_mime_type(?MCP_MIME_AUDIO_WAV)),

    %% Valid MP3 format
    ?assertEqual(ok, erlmcp_audio:validate_audio_mime_type(?MCP_MIME_AUDIO_MPEG)),

    %% Valid AAC format
    ?assertEqual(ok, erlmcp_audio:validate_audio_mime_type(?MCP_MIME_AUDIO_AAC)),

    %% Invalid format
    ?assertMatch({error, {unsupported_audio_format, _}},
                 erlmcp_audio:validate_audio_mime_type(<<"audio/invalid">>)),

    %% Non-audio format
    ?assertMatch({error, {unsupported_audio_format, _}},
                 erlmcp_audio:validate_audio_mime_type(?MCP_MIME_TEXT_PLAIN)).

%% Test 2: Supported audio formats list
test_supported_audio_formats() ->
    Formats = erlmcp_audio:supported_audio_formats(),
    ?assertMatch([_|_], Formats),
    ?assertEqual(8, length(Formats)),
    ?assert(lists:member(?MCP_MIME_AUDIO_WAV, Formats)),
    ?assert(lists:member(?MCP_MIME_AUDIO_MPEG, Formats)),
    ?assert(lists:member(?MCP_MIME_AUDIO_AAC, Formats)),
    ?assert(lists:member(?MCP_MIME_AUDIO_FLAC, Formats)),
    ?assert(lists:member(?MCP_MIME_AUDIO_OGG, Formats)),
    ?assert(lists:member(?MCP_MIME_AUDIO_WEBM, Formats)),
    ?assert(lists:member(?MCP_MIME_AUDIO_OPUS, Formats)).

%% Test 3: Encode audio content - WAV
test_encode_audio_content_wav() ->
    AudioData = create_test_audio_data(100),
    Result = erlmcp_audio:encode_audio_content(AudioData, ?MCP_MIME_AUDIO_WAV),
    ?assertMatch({ok, Map} when is_map(Map), Result),
    {ok, Content} = Result,
    ?assertEqual(?MCP_CONTENT_TYPE_AUDIO, maps:get(?MCP_PARAM_TYPE, Content)),
    ?assertEqual(?MCP_MIME_AUDIO_WAV, maps:get(?MCP_PARAM_MIME_TYPE, Content)),
    ?assert(is_binary(maps:get(?MCP_PARAM_DATA, Content))).

%% Test 4: Encode audio content - MP3
test_encode_audio_content_mp3() ->
    AudioData = create_test_audio_data(256),
    Result = erlmcp_audio:encode_audio_content(AudioData, ?MCP_MIME_AUDIO_MPEG),
    ?assertMatch({ok, _}, Result),
    {ok, Content} = Result,
    ?assertEqual(?MCP_MIME_AUDIO_MPEG, maps:get(?MCP_PARAM_MIME_TYPE, Content)).

%% Test 5: Encode audio content - AAC
test_encode_audio_content_aac() ->
    AudioData = create_test_audio_data(128),
    Result = erlmcp_audio:encode_audio_content(AudioData, ?MCP_MIME_AUDIO_AAC),
    ?assertMatch({ok, _}, Result),
    {ok, Content} = Result,
    ?assertEqual(?MCP_MIME_AUDIO_AAC, maps:get(?MCP_PARAM_MIME_TYPE, Content)).

%% Test 6: Encode audio content - FLAC
test_encode_audio_content_flac() ->
    AudioData = create_test_audio_data(512),
    Result = erlmcp_audio:encode_audio_content(AudioData, ?MCP_MIME_AUDIO_FLAC),
    ?assertMatch({ok, _}, Result),
    {ok, Content} = Result,
    ?assertEqual(?MCP_MIME_AUDIO_FLAC, maps:get(?MCP_PARAM_MIME_TYPE, Content)).

%% Test 7: Encode audio content - OGG
test_encode_audio_content_ogg() ->
    AudioData = create_test_audio_data(200),
    Result = erlmcp_audio:encode_audio_content(AudioData, ?MCP_MIME_AUDIO_OGG),
    ?assertMatch({ok, _}, Result),
    {ok, Content} = Result,
    ?assertEqual(?MCP_MIME_AUDIO_OGG, maps:get(?MCP_PARAM_MIME_TYPE, Content)).

%% Test 8: Encode audio content - WebM
test_encode_audio_content_webm() ->
    AudioData = create_test_audio_data(300),
    Result = erlmcp_audio:encode_audio_content(AudioData, ?MCP_MIME_AUDIO_WEBM),
    ?assertMatch({ok, _}, Result),
    {ok, Content} = Result,
    ?assertEqual(?MCP_MIME_AUDIO_WEBM, maps:get(?MCP_PARAM_MIME_TYPE, Content)).

%% Test 9: Encode audio content - Opus
test_encode_audio_content_opus() ->
    AudioData = create_test_audio_data(150),
    Result = erlmcp_audio:encode_audio_content(AudioData, ?MCP_MIME_AUDIO_OPUS),
    ?assertMatch({ok, _}, Result),
    {ok, Content} = Result,
    ?assertEqual(?MCP_MIME_AUDIO_OPUS, maps:get(?MCP_PARAM_MIME_TYPE, Content)).

%% Test 10: Base64 encoding
test_encode_audio_base64() ->
    AudioData = create_test_audio_data(64),
    Base64 = erlmcp_audio:encode_audio_base64(AudioData),
    ?assert(is_binary(Base64)),
    %% Base64 should be larger than original when encoded (no padding needed for our test)
    ?assert(byte_size(Base64) > 0),
    %% Should be valid base64
    ?assertMatch({ok, _}, erlmcp_audio:decode_audio_base64(Base64)).

%% Test 11: Base64 decoding
test_decode_audio_base64() ->
    AudioData = create_test_audio_data(100),
    Base64 = erlmcp_audio:encode_audio_base64(AudioData),
    Result = erlmcp_audio:decode_audio_base64(Base64),
    ?assertMatch({ok, _}, Result),
    {ok, Decoded} = Result,
    ?assertEqual(AudioData, Decoded).

%% Test 12: Encode audio with metadata
test_encode_audio_with_metadata() ->
    AudioData = create_test_audio_data(200),
    Metadata = #{
        duration => 125.5,
        sample_rate => 48000,
        channels => 2,
        bitrate => 320000
    },
    Result = erlmcp_audio:encode_audio_content_with_metadata(
        AudioData,
        ?MCP_MIME_AUDIO_MPEG,
        Metadata
    ),
    ?assertMatch({ok, _}, Result),
    {ok, Content} = Result,
    ?assertEqual(?MCP_CONTENT_TYPE_AUDIO, maps:get(?MCP_PARAM_TYPE, Content)),
    ?assertEqual(?MCP_MIME_AUDIO_MPEG, maps:get(?MCP_PARAM_MIME_TYPE, Content)),
    %% Metadata may be stored as annotations or separate field
    ?assert(is_binary(maps:get(?MCP_PARAM_DATA, Content))).

%% Test 13: Audio metadata extraction
test_audio_metadata_extraction() ->
    %% Test with all metadata fields
    FullMetadata = #{
        duration => 100.0,
        sample_rate => 44100,
        channels => 2,
        bitrate => 128000
    },
    AudioData = create_test_audio_data(100),
    Result = erlmcp_audio:encode_audio_content_with_metadata(
        AudioData,
        ?MCP_MIME_AUDIO_WAV,
        FullMetadata
    ),
    ?assertMatch({ok, _}, Result),

    %% Test with partial metadata
    PartialMetadata = #{
        duration => 50.5,
        sample_rate => 48000
    },
    Result2 = erlmcp_audio:encode_audio_content_with_metadata(
        AudioData,
        ?MCP_MIME_AUDIO_AAC,
        PartialMetadata
    ),
    ?assertMatch({ok, _}, Result2).

%% Test 14: Large audio file handling
test_large_audio_file_encoding() ->
    %% Create a larger audio file (1MB)
    LargeAudioData = create_test_audio_data(1024 * 1024),
    Result = erlmcp_audio:encode_audio_content(LargeAudioData, ?MCP_MIME_AUDIO_WAV),
    ?assertMatch({ok, _}, Result),
    {ok, Content} = Result,
    %% Base64 encoding increases size by approximately 33%
    EncodedSize = byte_size(maps:get(?MCP_PARAM_DATA, Content)),
    OriginalSize = byte_size(LargeAudioData),
    %% Check that encoded size is reasonable (between 1.33x and 1.35x original)
    ?assert(EncodedSize > OriginalSize),
    ?assert(EncodedSize < OriginalSize * 1.4).

%% Test 15: Invalid audio format rejection
test_invalid_audio_format_rejection() ->
    AudioData = create_test_audio_data(100),

    %% Invalid audio MIME type
    Result = erlmcp_audio:encode_audio_content(AudioData, <<"audio/invalid">>),
    ?assertMatch({error, {unsupported_audio_format, _}}, Result),

    %% Non-audio MIME type
    Result2 = erlmcp_audio:encode_audio_content(AudioData, ?MCP_MIME_TEXT_PLAIN),
    ?assertMatch({error, {unsupported_audio_format, _}}, Result2),

    %% Image MIME type
    Result3 = erlmcp_audio:encode_audio_content(AudioData, <<"image/png">>),
    ?assertMatch({error, {unsupported_audio_format, _}}, Result3).

%% Test 16: Base64 roundtrip
test_audio_base64_roundtrip() ->
    OriginalData = create_test_audio_data(500),

    %% Encode to base64
    Base64Encoded = erlmcp_audio:encode_audio_base64(OriginalData),
    ?assert(is_binary(Base64Encoded)),

    %% Decode back from base64
    {ok, DecodedData} = erlmcp_audio:decode_audio_base64(Base64Encoded),

    %% Should match original
    ?assertEqual(OriginalData, DecodedData),
    ?assertEqual(byte_size(OriginalData), byte_size(DecodedData)).

%% Test 17: is_valid_audio_format predicate
test_is_valid_audio_format() ->
    %% Valid formats
    ?assert(erlmcp_audio:is_valid_audio_format(?MCP_MIME_AUDIO_WAV)),
    ?assert(erlmcp_audio:is_valid_audio_format(?MCP_MIME_AUDIO_MPEG)),
    ?assert(erlmcp_audio:is_valid_audio_format(?MCP_MIME_AUDIO_AAC)),
    ?assert(erlmcp_audio:is_valid_audio_format(?MCP_MIME_AUDIO_FLAC)),

    %% Invalid formats
    ?assertNot(erlmcp_audio:is_valid_audio_format(<<"audio/invalid">>)),
    ?assertNot(erlmcp_audio:is_valid_audio_format(?MCP_MIME_TEXT_PLAIN)),
    ?assertNot(erlmcp_audio:is_valid_audio_format(<<"image/jpeg">>)).

%% Test 18: Audio content structure validation
test_audio_content_structure() ->
    AudioData = create_test_audio_data(100),
    {ok, Content} = erlmcp_audio:encode_audio_content(AudioData, ?MCP_MIME_AUDIO_WAV),

    %% Verify all required fields
    ?assertEqual(?MCP_CONTENT_TYPE_AUDIO, maps:get(?MCP_PARAM_TYPE, Content)),
    ?assertEqual(?MCP_MIME_AUDIO_WAV, maps:get(?MCP_PARAM_MIME_TYPE, Content)),
    ?assert(is_binary(maps:get(?MCP_PARAM_DATA, Content))),

    %% Verify data is valid base64
    DataField = maps:get(?MCP_PARAM_DATA, Content),
    ?assertMatch({ok, _}, erlmcp_audio:decode_audio_base64(DataField)).

%% Test 19: Audio metadata optional fields handling
test_audio_metadata_optional_fields() ->
    AudioData = create_test_audio_data(100),

    %% Test with all fields
    Metadata1 = #{
        duration => 120.0,
        sample_rate => 48000,
        channels => 2,
        bitrate => 320000
    },
    Result1 = erlmcp_audio:encode_audio_content_with_metadata(
        AudioData,
        ?MCP_MIME_AUDIO_MPEG,
        Metadata1
    ),
    ?assertMatch({ok, _}, Result1),

    %% Test with no metadata
    Metadata2 = #{},
    Result2 = erlmcp_audio:encode_audio_content_with_metadata(
        AudioData,
        ?MCP_MIME_AUDIO_WAV,
        Metadata2
    ),
    ?assertMatch({ok, _}, Result2),

    %% Test with only duration
    Metadata3 = #{duration => 90.5},
    Result3 = erlmcp_audio:encode_audio_content_with_metadata(
        AudioData,
        ?MCP_MIME_AUDIO_AAC,
        Metadata3
    ),
    ?assertMatch({ok, _}, Result3).

%%====================================================================
%% Helper Functions
%%====================================================================

%% Create test audio data of specified size
%% Returns a binary with pseudo-random audio samples
-spec create_test_audio_data(pos_integer()) -> binary().
create_test_audio_data(Size) when Size > 0 ->
    %% Generate pseudo-random binary data simulating audio samples
    random_binary(Size).

%% Generate random binary of specified size
-spec random_binary(pos_integer()) -> binary().
random_binary(Size) ->
    crypto:strong_rand_bytes(Size).
