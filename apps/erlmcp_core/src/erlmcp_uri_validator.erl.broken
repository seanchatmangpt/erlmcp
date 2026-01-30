-module(erlmcp_uri_validator).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    validate_resource_uri_on_registration/1,
    validate_uri_template/1,
    validate_uri/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%-----------------------------------------------------------------
%%% URI Validator for MCP Resources
%%%
%%% Implements RFC 3986 URI validation with MCP-specific constraints:
%%% - Scheme validation (file://, http://, https://, custom schemes)
%%% - Path traversal prevention (../)
%%% - Length limits (2048 characters default)
%%% - Character encoding validation
%%%
%%% Gap #41: URI validation on resource registration
%%%-----------------------------------------------------------------

-define(MAX_URI_LENGTH, 2048).
-define(MAX_URI_TEMPLATE_LENGTH, 4096).

-record(state, {
    cache :: ets:tid()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Validate resource URI on registration (Gap #41)
%% Returns: ok | {error, {ErrorType :: atom(), ErrorMsg :: binary()}}
-spec validate_resource_uri_on_registration(binary()) -> ok | {error, {atom(), binary()}}.
validate_resource_uri_on_registration(Uri) when is_binary(Uri) ->
    gen_server:call(?MODULE, {validate_resource_uri, Uri}).

%% @doc Validate URI template syntax (Gap #41)
%% Returns: ok | {error, {ErrorType :: atom(), ErrorMsg :: binary()}}
-spec validate_uri_template(binary()) -> ok | {error, {atom(), binary()}}.
validate_uri_template(UriTemplate) when is_binary(UriTemplate) ->
    gen_server:call(?MODULE, {validate_uri_template, UriTemplate}).

%% @doc Validate URI format (RFC 3986)
%% Returns: ok | {error, {ErrorType :: atom(), ErrorMsg :: binary()}}
-spec validate_uri(binary()) -> ok | {error, {atom(), binary()}}.
validate_uri(Uri) when is_binary(Uri) ->
    gen_server:call(?MODULE, {validate_uri, Uri}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Cache = ets:new(uri_validator_cache, [set, private, {read_concurrency, true}]),
    {ok, #state{cache = Cache}}.

handle_call({validate_resource_uri, Uri}, _From, State) ->
    Result = do_validate_resource_uri(Uri, State#state.cache),
    {reply, Result, State};

handle_call({validate_uri_template, UriTemplate}, _From, State) ->
    Result = do_validate_uri_template(UriTemplate),
    {reply, Result, State};

handle_call({validate_uri, Uri}, _From, State) ->
    Result = do_validate_uri(Uri),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions - URI Validation
%%%===================================================================

%% @private Validate resource URI for registration
do_validate_resource_uri(Uri, Cache) when is_binary(Uri) ->
    %% Check cache first
    case ets:lookup(Cache, Uri) of
        [{_, ok}] ->
            ok;
        [{_, Error}] ->
            Error;
        [] ->
            Result = validate_resource_uri_internal(Uri),
            ets:insert(Cache, {Uri, Result}),
            Result
    end.

%% @private Internal validation logic
validate_resource_uri_internal(Uri) ->
    %% 1. Length check
    case byte_size(Uri) of
        0 ->
            {error, {empty_uri, <<"URI cannot be empty">>}};
        Size when Size > ?MAX_URI_LENGTH ->
            MaxBin = integer_to_binary(?MAX_URI_LENGTH),
            Msg = <<"URI exceeds maximum length of ", MaxBin/binary>>,
            {error, {uri_too_long, Msg}};
        _ ->
            %% 2. Scheme check
            case extract_scheme(Uri) of
                {error, Reason} ->
                    {error, {invalid_scheme, Reason}};
                {ok, Scheme} ->
                    %% 3. Path validation
                    validate_uri_path(Uri, Scheme)
            end
    end.

%% @private Extract URI scheme
extract_scheme(<<>>) ->
    {error, <<"No scheme found">>};
extract_scheme(Uri) ->
    SchemeSep = <<"://">>,
    case binary:split(Uri, SchemeSep) of
        [Scheme, _Rest] when byte_size(Scheme) > 0 ->
            {ok, Scheme};
        _ ->
            %% No scheme, might be relative URI or custom format
            {ok, undefined}
    end.

%% @private Validate URI path for security
validate_uri_path(Uri, Scheme) ->
    %% Check for path traversal attempts
    ParentDirUnix = <<"../">>,
    case binary:match(Uri, ParentDirUnix) of
        {_, _} ->
            {error, {path_traversal,
                <<"URI contains path traversal sequence ../">>}};
        nomatch ->
            %% Check Windows path traversal
            ParentDirWin = <<"..\\">>,
            case binary:match(Uri, ParentDirWin) of
                {_, _} ->
                    {error, {path_traversal,
                        <<"URI contains Windows path traversal sequence">>}};
                nomatch ->
                    validate_scheme_specific_rules(Uri, Scheme)
            end
    end.

%% @private Scheme-specific validation rules
validate_scheme_specific_rules(Uri, undefined) ->
    %% Relative URI or custom format - allow basic validation
    validate_uri_characters(Uri);
validate_scheme_specific_rules(Uri, Scheme) ->
    case Scheme of
        <<"file">> ->
            validate_file_uri(Uri);
        <<"http">> ->
            validate_http_uri(Uri);
        <<"https">> ->
            validate_http_uri(Uri);
        <<"ftp">> ->
            validate_http_uri(Uri);
        _ ->
            %% Custom scheme - basic validation
            validate_uri_characters(Uri)
    end.

%% @private Validate file:// URIs
validate_file_uri(Uri) ->
    %% Must have file:// prefix
    case Uri of
        <<"file://", Rest/binary>> when byte_size(Rest) > 0 ->
            validate_uri_characters(Uri);
        _ ->
            {error, {invalid_file_uri,
                <<"file:// URIs must have a path component">>}}
    end.

%% @private Validate http:// and https:// URIs
validate_http_uri(Uri) ->
    %% Must have scheme://host format
    case Uri of
        <<"http://", Rest/binary>> when byte_size(Rest) > 0 ->
            validate_uri_characters(Uri);
        <<"https://", Rest/binary>> when byte_size(Rest) > 0 ->
            validate_uri_characters(Uri);
        <<"ftp://", Rest/binary>> when byte_size(Rest) > 0 ->
            validate_uri_characters(Uri);
        _ ->
            {error, {invalid_http_uri,
                <<"HTTP URIs must have scheme://host format">>}}
    end.

%% @private Validate URI characters (RFC 3986)
validate_uri_characters(Uri) ->
    try
        %% Check for null bytes
        NullByte = <<0>>,
        case binary:match(Uri, NullByte) of
            {_, _} ->
                {error, {invalid_uri,
                    <<"URI contains null byte">>}};
            nomatch ->
                %% Check for control characters
                case has_invalid_control_chars(Uri) of
                    true ->
                        {error, {invalid_uri,
                            <<"URI contains invalid control characters">>}};
                    false ->
                        ok
                end
        end
    catch
        _:_ ->
            {error, {invalid_uri,
                <<"URI validation failed">>}}
    end.

%% @private Check for invalid control characters
has_invalid_control_chars(<<>>) ->
    false;
has_invalid_control_chars(<<C, Rest/binary>>) when C >= 0, C =< 31, C /= 9, C /= 10, C /= 13 ->
    true;
has_invalid_control_chars(<<_, Rest/binary>>) ->
    has_invalid_control_chars(Rest).

%% @private Validate URI template syntax
do_validate_uri_template(UriTemplate) when is_binary(UriTemplate) ->
    %% 1. Length check
    case byte_size(UriTemplate) of
        0 ->
            {error, {empty_template, <<"URI template cannot be empty">>}};
        Size when Size > ?MAX_URI_TEMPLATE_LENGTH ->
            MaxBin = integer_to_binary(?MAX_URI_TEMPLATE_LENGTH),
            Msg = <<"URI template exceeds maximum length of ", MaxBin/binary>>,
            {error, {template_too_long, Msg}};
        _ ->
            %% 2. Template syntax validation
            validate_template_syntax(UriTemplate)
    end.

%% @private Validate template syntax (e.g., {variable})
validate_template_syntax(UriTemplate) ->
    %% Check for balanced braces
    case validate_template_braces(UriTemplate, 0, 0) of
        {error, Reason} ->
            {error, {invalid_template_syntax, Reason}};
        ok ->
            %% Validate URI characters
            validate_resource_uri_internal(UriTemplate)
    end.

%% @private Validate brace balancing in templates
validate_template_braces(<<>>, OpenBraces, _Depth) when OpenBraces =:= 0 ->
    ok;
validate_template_braces(<<>>, _OpenBraces, _Depth) ->
    {error, <<"Unbalanced braces in template">>};
validate_template_braces(<<"{", Rest/binary>>, OpenBraces, Depth) ->
    validate_template_braces(Rest, OpenBraces + 1, Depth + 1);
validate_template_braces(<<"}", Rest/binary>>, OpenBraces, Depth) when OpenBraces > 0 ->
    validate_template_braces(Rest, OpenBraces - 1, Depth - 1);
validate_template_braces(<<_Byte, Rest/binary>>, OpenBraces, Depth) ->
    validate_template_braces(Rest, OpenBraces, Depth).

%% @private Basic URI validation
do_validate_uri(Uri) when is_binary(Uri) ->
    validate_resource_uri_internal(Uri).
