-module(erlmcp_http_auth).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-behavior(gen_server).

%% API exports
-export([
    start_link/1,
    get_token/1,
    refresh_token/1,
    validate_token/1,
    inject_headers/2
]).

%% gen_server exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Constants
-define(TOKEN_TABLE, oauth_tokens).
-define(TOKEN_REFRESH_INTERVAL, 3600000). %% 1 hour
-define(HTTP_TIMEOUT, 5000).

-record(state, {
    client_id :: binary(),
    client_secret :: binary(),
    token_endpoint :: string(),
    resource_indicator :: string(),
    current_token :: binary() | undefined,
    token_expiry :: integer() | undefined,
    refresh_timer :: reference() | undefined
}).

-record(token_cache, {
    key :: binary(),
    token :: binary(),
    expiry :: integer(),
    scope :: binary()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-spec get_token(binary()) -> {ok, binary()} | {error, term()}.
get_token(ResourceIndicator) ->
    gen_server:call(?MODULE, {get_token, ResourceIndicator}, ?HTTP_TIMEOUT).

-spec refresh_token(binary()) -> {ok, binary()} | {error, term()}.
refresh_token(ResourceIndicator) ->
    gen_server:call(?MODULE, {refresh_token, ResourceIndicator}, ?HTTP_TIMEOUT).

-spec validate_token(binary()) -> boolean().
validate_token(Token) when is_binary(Token) ->
    SpanCtx = erlmcp_tracing:start_span(<<"http_auth.validate_token">>),
    try
        case ets:lookup(?TOKEN_TABLE, Token) of
            [#token_cache{expiry = Expiry}] ->
                Now = erlang:system_time(second),
                Result = Now < Expiry,
                erlmcp_tracing:set_status(SpanCtx, ok),
                Result;
            [] ->
                erlmcp_tracing:record_error_details(SpanCtx, not_found, Token),
                false
        end
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            false
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

validate_token(_) ->
    false.

-spec inject_headers(map(), binary()) -> {ok, map()} | {error, term()}.
inject_headers(Headers, ResourceIndicator) ->
    SpanCtx = erlmcp_tracing:start_span(<<"http_auth.inject_headers">>),
    try
        case get_token(ResourceIndicator) of
            {ok, Token} ->
                AuthHeader = <<"Bearer ", Token/binary>>,
                UpdatedHeaders = Headers#{
                    <<"authorization">> => AuthHeader,
                    <<"resource-indicator">> => ResourceIndicator
                },
                erlmcp_tracing:set_status(SpanCtx, ok),
                {ok, UpdatedHeaders};
            {error, Reason} ->
                erlmcp_tracing:record_error_details(SpanCtx, auth_failed, Reason),
                {error, Reason}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Config) ->
    erlmcp_tracing:start_span(<<"http_auth.init">>),

    ets:new(?TOKEN_TABLE, [named_table, set, public, {keypos, #token_cache.key}]),

    ClientId = maps:get(client_id, Config, <<>>),
    ClientSecret = maps:get(client_secret, Config, <<>>),
    TokenEndpoint = maps:get(token_endpoint, Config, ""),
    ResourceIndicator = maps:get(resource_indicator, Config, ""),

    %% Start initial token refresh
    {ok, RefreshRef} = timer:send_after(?TOKEN_REFRESH_INTERVAL, refresh_token_timer),

    State = #state{
        client_id = ClientId,
        client_secret = ClientSecret,
        token_endpoint = TokenEndpoint,
        resource_indicator = ResourceIndicator,
        refresh_timer = RefreshRef
    },

    {ok, State}.

handle_call({get_token, ResourceIndicator}, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"http_auth.get_token">>),
    try
        case State#state.current_token of
            undefined ->
                case request_new_token(State, ResourceIndicator) of
                    {ok, Token, Expiry} ->
                        erlmcp_tracing:set_status(SpanCtx, ok),
                        {reply, {ok, Token}, State#state{
                            current_token = Token,
                            token_expiry = Expiry
                        }};
                    {error, Reason} ->
                        erlmcp_tracing:record_error_details(SpanCtx, token_request_failed, Reason),
                        {reply, {error, Reason}, State}
                end;
            Token ->
                case is_token_valid(State#state.token_expiry) of
                    true ->
                        erlmcp_tracing:set_status(SpanCtx, ok),
                        {reply, {ok, Token}, State};
                    false ->
                        case request_new_token(State, ResourceIndicator) of
                            {ok, NewToken, Expiry} ->
                                erlmcp_tracing:set_status(SpanCtx, ok),
                                {reply, {ok, NewToken}, State#state{
                                    current_token = NewToken,
                                    token_expiry = Expiry
                                }};
                            {error, Reason} ->
                                erlmcp_tracing:record_error_details(SpanCtx, token_refresh_failed, Reason),
                                {reply, {error, Reason}, State}
                        end
                end
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {reply, {error, {Class, CaughtReason}}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_call({refresh_token, ResourceIndicator}, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"http_auth.refresh_token">>),
    try
        case request_new_token(State, ResourceIndicator) of
            {ok, Token, Expiry} ->
                erlmcp_tracing:set_status(SpanCtx, ok),
                {reply, {ok, Token}, State#state{
                    current_token = Token,
                    token_expiry = Expiry
                }};
            {error, Reason} ->
                erlmcp_tracing:record_error_details(SpanCtx, refresh_failed, Reason),
                {reply, {error, Reason}, State}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {reply, {error, {Class, CaughtReason}}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh_token_timer, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"http_auth.refresh_timer">>),
    try
        case request_new_token(State, State#state.resource_indicator) of
            {ok, Token, Expiry} ->
                erlmcp_tracing:set_status(SpanCtx, ok),
                {ok, NewRef1} = timer:send_after(?TOKEN_REFRESH_INTERVAL, refresh_token_timer),
                {noreply, State#state{
                    current_token = Token,
                    token_expiry = Expiry,
                    refresh_timer = NewRef1
                }};
            {error, TokenError} ->
                erlmcp_tracing:record_error_details(SpanCtx, auto_refresh_failed, TokenError),
                {ok, NewRef2} = timer:send_after(?TOKEN_REFRESH_INTERVAL, refresh_token_timer),
                {noreply, State#state{refresh_timer = NewRef2}}
        end
    catch
        Class:CaughtError:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtError, Stacktrace),
            {ok, NewRef3} = timer:send_after(?TOKEN_REFRESH_INTERVAL, refresh_token_timer),
            {noreply, State#state{refresh_timer = NewRef3}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.refresh_timer of
        undefined -> ok;
        Ref -> timer:cancel(Ref)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec request_new_token(#state{}, string()) -> {ok, binary(), integer()} | {error, term()}.
request_new_token(State, ResourceIndicator) ->
    Body = uri_string:quote_string(lists:flatten(io_lib:format(
        "grant_type=client_credentials&client_id=~s&client_secret=~s&resource=~s",
        [
            binary_to_list(State#state.client_id),
            binary_to_list(State#state.client_secret),
            ResourceIndicator
        ]
    ))),

    Headers = [
        {<<"content-type">>, <<"application/x-www-form-urlencoded">>}
    ],

    case httpc:request(post,
        {State#state.token_endpoint, Headers, "application/x-www-form-urlencoded", Body},
        [{timeout, ?HTTP_TIMEOUT}],
        []) of
        {ok, {{200, _}, _ResponseHeaders, ResponseBody}} ->
            case jsx:decode(ResponseBody) of
                {error, _} ->
                    {error, invalid_json_response};
                Response ->
                    Token = maps:get(<<"access_token">>, Response),
                    ExpiresIn = maps:get(<<"expires_in">>, Response, 3600),
                    Expiry = erlang:system_time(second) + ExpiresIn,

                    ets:insert(?TOKEN_TABLE, #token_cache{
                        key = Token,
                        token = Token,
                        expiry = Expiry,
                        scope = maps:get(<<"scope">>, Response, <<"">>)
                    }),

                    {ok, Token, Expiry}
            end;
        {ok, {{ErrorCode, _}, _ResponseHeaders, _ResponseBody}} ->
            {error, {http_error, ErrorCode}};
        {error, Reason} ->
            {error, {http_request_failed, Reason}}
    end.

-spec is_token_valid(integer() | undefined) -> boolean().
is_token_valid(undefined) ->
    false;

is_token_valid(Expiry) ->
    Now = erlang:system_time(second),
    %% Consider token valid if expires in more than 60 seconds
    Now + 60 < Expiry.
