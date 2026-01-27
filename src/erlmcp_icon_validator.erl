-module(erlmcp_icon_validator).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% API exports
-export([
    validate_icon/1,
    store_icon/2,
    get_icon/1,
    is_valid_mime_type/1
]).

%% Constants
-define(MAX_DATA_URI_SIZE, 102400). %% 100KB
-define(ICON_TABLE, icon_metadata).
-define(WHITELISTED_MIME_TYPES, [
    <<"image/png">>,
    <<"image/jpeg">>,
    <<"image/webp">>,
    <<"image/svg+xml">>,
    <<"image/gif">>,
    <<"image/x-icon">>
]).

%%====================================================================
%% API Functions
%%====================================================================

-spec validate_icon(map()) -> {ok, map()} | {error, term()}.
validate_icon(Icon) when is_map(Icon) ->
    SpanCtx = erlmcp_tracing:start_span(<<"icon_validator.validate">>),
    try
        Src = maps:get(<<"src">>, Icon, undefined),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"icon_src_scheme">> => get_scheme(Src)
        }),

        case validate_icon_src(Src) of
            {ok, ValidSrc} ->
                erlmcp_tracing:set_status(SpanCtx, ok),
                {ok, Icon#{<<"src">> => ValidSrc}};
            {error, ErrorReason} ->
                erlmcp_tracing:record_error_details(SpanCtx, validation_failed, ErrorReason),
                {error, ErrorReason}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

validate_icon(_) ->
    {error, invalid_icon_format}.

-spec store_icon(binary(), map()) -> {ok, binary()} | {error, term()}.
store_icon(IconId, Metadata) when is_binary(IconId), is_map(Metadata) ->
    SpanCtx = erlmcp_tracing:start_span(<<"icon_validator.store">>),
    try
        ets:new(?ICON_TABLE, [named_table, set, public]),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"icon_id">> => IconId
        }),

        ets:insert(?ICON_TABLE, {IconId, Metadata}),

        erlmcp_tracing:set_status(SpanCtx, ok),
        {ok, IconId}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            {error, {Class, Reason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

-spec get_icon(binary()) -> {ok, map()} | {error, not_found}.
get_icon(IconId) when is_binary(IconId) ->
    SpanCtx = erlmcp_tracing:start_span(<<"icon_validator.get">>),
    try
        case ets:lookup(?ICON_TABLE, IconId) of
            [{IconId, Metadata}] ->
                erlmcp_tracing:set_status(SpanCtx, ok),
                {ok, Metadata};
            [] ->
                erlmcp_tracing:record_error_details(SpanCtx, not_found, IconId),
                {error, not_found}
        end
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            {error, not_found}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

-spec is_valid_mime_type(binary()) -> boolean().
is_valid_mime_type(MimeType) when is_binary(MimeType) ->
    lists:member(MimeType, ?WHITELISTED_MIME_TYPES);

is_valid_mime_type(_) ->
    false.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec validate_icon_src(binary() | undefined) -> {ok, binary()} | {error, term()}.
validate_icon_src(undefined) ->
    {error, missing_src};

validate_icon_src(Src) when is_binary(Src) ->
    Scheme = get_scheme(Src),
    case Scheme of
        <<"data">> ->
            validate_data_uri(Src);
        <<"https">> ->
            validate_https_uri(Src);
        <<"file">> ->
            validate_file_uri(Src);
        _ ->
            {error, {unsupported_scheme, Scheme}}
    end;

validate_icon_src(_) ->
    {error, invalid_src_format}.

-spec get_scheme(binary() | undefined) -> binary().
get_scheme(undefined) ->
    <<"unknown">>;

get_scheme(Uri) when is_binary(Uri) ->
    case binary:split(Uri, <<"://">>) of
        [Scheme, _Rest] -> Scheme;
        _ -> case binary:split(Uri, <<":">>) of
            [Scheme, _] -> Scheme;
            _ -> <<"unknown">>
        end
    end;

get_scheme(_) ->
    <<"unknown">>.

-spec validate_data_uri(binary()) -> {ok, binary()} | {error, term()}.
validate_data_uri(Uri) ->
    case byte_size(Uri) > ?MAX_DATA_URI_SIZE of
        true ->
            {error, data_uri_too_large};
        false ->
            case extract_data_uri_mime_type(Uri) of
                {ok, MimeType} ->
                    case is_valid_mime_type(MimeType) of
                        true -> {ok, Uri};
                        false -> {error, {unsupported_mime_type, MimeType}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec validate_https_uri(binary()) -> {ok, binary()} | {error, term()}.
validate_https_uri(Uri) ->
    %% Basic HTTPS validation - just check it's a valid URI
    case uri_string:parse(Uri) of
        #{scheme := <<"https">>, host := _Host} ->
            {ok, Uri};
        #{scheme := <<"https">>} ->
            {error, invalid_https_uri};
        _ ->
            {error, not_https_uri}
    end.

-spec validate_file_uri(binary()) -> {ok, binary()} | {error, term()}.
validate_file_uri(Uri) ->
    %% File URI should be validated against roots if available
    case uri_string:parse(Uri) of
        #{scheme := <<"file">>, path := _Path} ->
            {ok, Uri};
        #{scheme := <<"file">>} ->
            {error, invalid_file_uri};
        _ ->
            {error, not_file_uri}
    end.

-spec extract_data_uri_mime_type(binary()) -> {ok, binary()} | {error, term()}.
extract_data_uri_mime_type(Uri) ->
    case binary:split(Uri, <<";">>) of
        [_DataPrefix, Rest] ->
            case binary:split(Rest, <<",">>) of
                [MimeType, _Data] ->
                    {ok, MimeType};
                _ ->
                    {error, invalid_data_uri_format}
            end;
        [_] ->
            %% No mime type specified, default to text/plain
            {ok, <<"image/png">>}
    end.
