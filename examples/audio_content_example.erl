%%% @doc
%%% Example: Using Audio Content Type Support in erlmcp
%%%
%%% This example demonstrates how to:
%%% 1. Create audio content from raw binary data
%%% 2. Validate audio MIME types
%%% 3. Handle audio resources with erlmcp_server
%%% 4. Include audio metadata
%%%
%%% Gap #34: Audio Content Type Support - MCP 2025-11-25 Compliance
%%%
%%% @since 0.7.0
-module(audio_content_example).

-export([
    example_create_audio_content/0,
    example_validate_format/0,
    example_with_metadata/0,
    example_resource_handler/0,
    example_full_workflow/0
]).

%%====================================================================
%% Example 1: Create Audio Content
%%====================================================================

%%% @doc
%%% Example: Create audio content from raw binary data
%%% This is the basic use case for serving audio resources
%%% @end
example_create_audio_content() ->
    % Simulate loading audio file
    AudioData = load_sample_audio(256),

    % Encode as audio content for MCP transport
    {ok, Content} = erlmcp_audio:encode_audio_content(AudioData, <<"audio/wav">>),

    % Content structure:
    % #{
    %     <<"type">> => <<"audio">>,
    %     <<"data">> => <<"base64-encoded-data">>,
    %     <<"mimeType">> => <<"audio/wav">>
    % }

    io:format("Created audio content: type=~p, has_data=~p~n", [
        maps:get(<<"type">>, Content),
        maps:is_key(<<"data">>, Content)
    ]).

%%====================================================================
%% Example 2: Validate Audio Format
%%====================================================================

%%% @doc
%%% Example: Validate audio MIME types before encoding
%%% Demonstrates error handling for invalid formats
%%% @end
example_validate_format() ->
    % Valid format
    case erlmcp_audio:validate_audio_mime_type(<<"audio/mp3">>) of
        ok ->
            io:format("✓ MP3 format is supported~n");
        {error, Reason} ->
            io:format("✗ MP3 format error: ~p~n", [Reason])
    end,

    % Invalid format
    case erlmcp_audio:validate_audio_mime_type(<<"audio/invalid">>) of
        ok ->
            io:format("✗ Invalid format should not be supported~n");
        {error, {unsupported_audio_format, _}} ->
            io:format("✓ Invalid format correctly rejected~n")
    end,

    % List supported formats
    SupportedFormats = erlmcp_audio:supported_audio_formats(),
    io:format("Supported formats (~p): ~p~n", [length(SupportedFormats), SupportedFormats]).

%%====================================================================
%% Example 3: Audio Content with Metadata
%%====================================================================

%%% @doc
%%% Example: Create audio content with metadata
%%% Demonstrates optional metadata: duration, sample rate, channels, bitrate
%%% @end
example_with_metadata() ->
    AudioData = load_sample_audio(512),

    % Audio metadata
    Metadata = #{
        duration => 45.5,           % seconds
        sample_rate => 44100,        % Hz
        channels => 2,               % stereo
        bitrate => 192000            % bits per second
    },

    {ok, Content} = erlmcp_audio:encode_audio_content_with_metadata(
        AudioData,
        <<"audio/mp3">>,
        Metadata
    ),

    io:format("Created MP3 content with metadata:~n"),
    io:format("  Type: ~p~n", [maps:get(<<"type">>, Content)]),
    io:format("  MIME: ~p~n", [maps:get(<<"mimeType">>, Content)]),
    io:format("  Has data: ~p~n", [maps:is_key(<<"data">>, Content)]).

%%====================================================================
%% Example 4: Resource Handler Pattern
%%====================================================================

%%% @doc
%%% Example: Use audio content in erlmcp_server resource handlers
%%% Demonstrates how to serve audio resources dynamically
%%% @end
example_resource_handler() ->
    % Handler that returns audio content
    AudioHandler = fun(Uri) ->
        % In real code, you would load the audio file based on Uri
        AudioBinary = load_sample_audio(1024),

        % Create audio content for the resource
        {ok, Content} = erlmcp_audio:encode_audio_content(
            AudioBinary,
            <<"audio/wav">>
        ),
        Content
    end,

    % Would normally be used with erlmcp_server:add_resource/3
    % erlmcp_server:add_resource(Server, <<"audio://resource/sample">>, AudioHandler)

    % For demonstration, test the handler
    TestContent = AudioHandler(<<"audio://resource/sample">>),
    io:format("Resource handler returned audio content of type: ~p~n",
              [maps:get(<<"type">>, TestContent)]).

%%====================================================================
%% Example 5: Full Workflow
%%====================================================================

%%% @doc
%%% Example: Full workflow - validate, encode, and prepare for JSON-RPC
%%% @end
example_full_workflow() ->
    io:format("~n=== Full Audio Content Workflow ===~n~n"),

    % Step 1: Define audio source
    AudioFile = <<"recording.wav">>,
    MimeType = <<"audio/wav">>,
    io:format("Step 1: Audio source = ~p (~p)~n", [AudioFile, MimeType]),

    % Step 2: Validate format
    io:format("Step 2: Validating format... "),
    case erlmcp_audio:validate_audio_mime_type(MimeType) of
        ok ->
            io:format("✓ Valid~n");
        {error, R} ->
            io:format("✗ Error: ~p~n", [R]),
            halt()
    end,

    % Step 3: Load audio (simulated)
    io:format("Step 3: Loading audio file... "),
    AudioData = load_sample_audio(256),
    io:format("✓ Loaded (~p bytes)~n", [byte_size(AudioData)]),

    % Step 4: Encode as content
    io:format("Step 4: Encoding as audio content... "),
    {ok, Content} = erlmcp_audio:encode_audio_content(AudioData, MimeType),
    io:format("✓ Encoded~n"),

    % Step 5: Verify structure
    io:format("Step 5: Verifying content structure...~n"),
    ContentType = maps:get(<<"type">>, Content),
    ContentMime = maps:get(<<"mimeType">>, Content),
    ContentData = maps:get(<<"data">>, Content),
    io:format("  - type: ~p ✓~n", [ContentType]),
    io:format("  - mimeType: ~p ✓~n", [ContentMime]),
    io:format("  - data (base64): ~p bytes ✓~n", [byte_size(ContentData)]),

    % Step 6: Verify JSON serialization
    io:format("Step 6: JSON serialization compatible... "),
    Json = jsx:encode(Content),
    io:format("✓ (~p bytes JSON)~n~n", [byte_size(Json)]).

%%====================================================================
%% Helper Functions
%%====================================================================

%%% Generate test audio data
load_sample_audio(Size) ->
    crypto:strong_rand_bytes(Size).

%%====================================================================
%% Main Entry Point
%%====================================================================

% Run all examples
run_all() ->
    io:format("~n=== Audio Content Type Support Examples ===~n~n"),

    io:format("1. Creating audio content...~n"),
    example_create_audio_content(),
    io:format("~n"),

    io:format("2. Validating audio formats...~n"),
    example_validate_format(),
    io:format("~n"),

    io:format("3. Audio content with metadata...~n"),
    example_with_metadata(),
    io:format("~n"),

    io:format("4. Resource handler pattern...~n"),
    example_resource_handler(),
    io:format("~n"),

    example_full_workflow(),

    io:format("All examples completed successfully!~n").
