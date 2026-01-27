-module(erlmcp_icon_validator_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

icon_validator_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_validate_icon_https()),
            ?_test(test_validate_icon_data_uri()),
            ?_test(test_validate_icon_file()),
            ?_test(test_store_icon()),
            ?_test(test_get_icon()),
            ?_test(test_invalid_mime_type()),
            ?_test(test_data_uri_too_large()),
            ?_test(test_missing_src()),
            ?_test(test_unsupported_scheme()),
            ?_test(test_valid_mime_types())
        ]
    }.

%%====================================================================
%% Individual Tests
%%====================================================================

test_validate_icon_https() ->
    Icon = #{
        <<"src">> => <<"https://example.com/icon.png">>
    },

    {ok, ValidatedIcon} = erlmcp_icon_validator:validate_icon(Icon),
    ?assert(is_map(ValidatedIcon)),
    ?assert(maps:get(<<"src">>, ValidatedIcon) =:= <<"https://example.com/icon.png">>).

test_validate_icon_data_uri() ->
    Icon = #{
        <<"src">> => <<"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==">>
    },

    {ok, ValidatedIcon} = erlmcp_icon_validator:validate_icon(Icon),
    ?assert(is_map(ValidatedIcon)).

test_validate_icon_file() ->
    Icon = #{
        <<"src">> => <<"file:///Users/sac/icon.png">>
    },

    {ok, ValidatedIcon} = erlmcp_icon_validator:validate_icon(Icon),
    ?assert(is_map(ValidatedIcon)).

test_store_icon() ->
    IconId = <<"icon_test_1">>,
    Metadata = #{
        <<"src">> => <<"https://example.com/icon.png">>,
        <<"mimeType">> => <<"image/png">>
    },

    {ok, StoredId} = erlmcp_icon_validator:store_icon(IconId, Metadata),
    ?assert(StoredId =:= IconId).

test_get_icon() ->
    IconId = <<"icon_test_2">>,
    Metadata = #{
        <<"src">> => <<"https://example.com/icon.png">>,
        <<"mimeType">> => <<"image/png">>
    },

    {ok, _StoredId} = erlmcp_icon_validator:store_icon(IconId, Metadata),
    {ok, RetrievedMetadata} = erlmcp_icon_validator:get_icon(IconId),

    ?assert(is_map(RetrievedMetadata)),
    ?assert(maps:get(<<"src">>, RetrievedMetadata) =:= <<"https://example.com/icon.png">>).

test_invalid_mime_type() ->
    Icon = #{
        <<"src">> => <<"data:application/pdf;base64,JVBERi0xLjQK">>
    },

    {error, {unsupported_mime_type, _}} = erlmcp_icon_validator:validate_icon(Icon),

    ?assert(true).

test_data_uri_too_large() ->
    %% Create data URI larger than 100KB limit
    LargeData = binary:copy(<<"x">>, 102401),
    Icon = #{
        <<"src">> => <<"data:image/png;base64,", LargeData/binary>>
    },

    {error, data_uri_too_large} = erlmcp_icon_validator:validate_icon(Icon),

    ?assert(true).

test_missing_src() ->
    Icon = #{},

    {error, missing_src} = erlmcp_icon_validator:validate_icon(Icon),

    ?assert(true).

test_unsupported_scheme() ->
    Icon = #{
        <<"src">> => <<"ftp://example.com/icon.png">>
    },

    {error, {unsupported_scheme, _}} = erlmcp_icon_validator:validate_icon(Icon),

    ?assert(true).

test_valid_mime_types() ->
    ValidMimeTypes = [
        <<"image/png">>,
        <<"image/jpeg">>,
        <<"image/webp">>,
        <<"image/svg+xml">>,
        <<"image/gif">>,
        <<"image/x-icon">>
    ],

    Results = lists:map(
        fun(MimeType) ->
            erlmcp_icon_validator:is_valid_mime_type(MimeType)
        end,
        ValidMimeTypes
    ),

    ?assert(lists:all(fun(R) -> R =:= true end, Results)).
