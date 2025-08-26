%%%-------------------------------------------------------------------
%%% @doc
%%% Simplified HTTP Transport for Erlang MCP (Model Context Protocol)
%%%
%%% Major simplification of HTTP transport focusing on MCP request/response
%%% patterns with registry integration and proper error handling.
%%%
%%% Key Features:
%%% - Clean MCP HTTP client functionality
%%% - Registry integration for transport management
%%% - Proper error handling and timeouts
%%% - Simplified state management
%%% - Standard behavior callbacks
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_http).

-behaviour(gen_server).

-include("erlmcp.hrl").

-include_lib("kernel/include/logger.hrl").

%% Transport API (mimics behavior interface)
-export([init_transport/2, send/2, close/1, get_info/1, handle_transport_call/2]).
%% API
-export([start_link/2, stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Simplified state record (Phase 3 Enhancement) - aligned with behavior pattern
-record(state,
        {transport_id :: atom(),
         config :: map(),
         url :: string(),
         headers :: [{string(), string()}],
         http_options :: [term()],
         statistics :: map(),
         connection_state = connecting :: connecting | connected | disconnected | error,
         last_error :: term() | undefined}).

%% Default HTTP headers for MCP
-define(DEFAULT_HEADERS,
        [{"Content-Type", "application/json"},
         {"Accept", "application/json"},
         {"User-Agent", "erlmcp-http-transport/1.0"}]).
%% Default timeouts
-define(DEFAULT_TIMEOUT, 30000).
-define(DEFAULT_CONNECT_TIMEOUT, 10000).

%%====================================================================
%% Transport API (mimics behavior interface)
%%====================================================================

%% @doc Internal initialization function
-spec init_transport(atom(), map()) -> {ok, #state{}} | {error, term()}.
init_transport(TransportId, Config) ->
    ?LOG_DEBUG("Initializing HTTP transport ~p with config: ~p",
               [TransportId, maps:without([password, secret, token], Config)]),

    case validate_config(Config) of
        ok ->
            Url = ensure_string(maps:get(url, Config)),
            Headers = merge_headers(maps:get(headers, Config, [])),
            HttpOptions = build_http_options(Config),
            Statistics =
                #{messages_sent => 0,
                  messages_received => 0,
                  bytes_sent => 0,
                  bytes_received => 0,
                  errors => 0,
                  started_at => erlang:system_time(millisecond),
                  last_activity => erlang:system_time(millisecond)},

            State =
                #state{transport_id = TransportId,
                       config = Config,
                       url = Url,
                       headers = Headers,
                       http_options = HttpOptions,
                       statistics = Statistics,
                       connection_state = connected,
                       last_error = undefined},

            register_with_registry(State),
            ?LOG_INFO("HTTP transport ~p initialized for URL: ~s", [TransportId, Url]),
            {ok, State};
        {error, Reason} ->
            ?LOG_ERROR("Failed to initialize HTTP transport ~p: ~p", [TransportId, Reason]),
            {error, Reason}
    end.

%% @doc Send data via HTTP transport
-spec send(#state{}, iodata()) -> ok | {error, term()}.
send(State, Data) ->
    ?LOG_DEBUG("Sending HTTP request to ~s", [State#state.url]),

    try
        Body = iolist_to_binary(Data),

        case httpc:request(post,
                           {State#state.url, State#state.headers, "application/json", Body},
                           State#state.http_options,
                           [{body_format, binary}])
        of
            {ok, {{_, Status, _}, _ResponseHeaders, _ResponseBody}}
                when Status >= 200, Status < 300 ->
                ?LOG_DEBUG("HTTP request successful, status: ~p", [Status]),
                ok;
            {ok, {{_, Status, _}, _Headers, ResponseBody}} ->
                ?LOG_WARNING("HTTP request failed with status: ~p, body: ~p",
                             [Status, ResponseBody]),
                {error, {http_status, Status}};
            {error, Reason} ->
                ?LOG_ERROR("HTTP request failed: ~p", [Reason]),
                {error, {http_error, Reason}}
        end
    catch
        Class:ErrorReason:Stacktrace ->
            ?LOG_ERROR("HTTP request exception: ~p:~p~n~p", [Class, ErrorReason, Stacktrace]),
            {error, {request_exception, Class, ErrorReason}}
    end.

%% @doc Close the HTTP transport
-spec close(#state{}) -> ok.
close(State) ->
    unregister_from_registry(State),
    ?LOG_INFO("HTTP transport ~p closed", [State#state.transport_id]),
    ok.

%% @doc Get transport information - enhanced with state tracking
-spec get_info(#state{}) -> erlmcp_transport:transport_info().
get_info(State) ->
    Info =
        #{transport_id => State#state.transport_id,
          type => http,
          status => State#state.connection_state,
          peer => State#state.url,
          version => <<"1.0.0">>,
          capabilities => [client, json_rpc, ssl],
          connection_state => State#state.connection_state,
          statistics => State#state.statistics,
          started_at =>
              maps:get(started_at, State#state.statistics, erlang:system_time(millisecond)),
          last_activity =>
              maps:get(last_activity, State#state.statistics, erlang:system_time(millisecond)),
          config => maps:without([password, secret, token], State#state.config)},

    % Add last error if present
    case State#state.last_error of
        undefined ->
            Info;
        Error ->
            Info#{last_error => Error}
    end.

%% @doc Handle transport-specific calls
-spec handle_transport_call(term(), #state{}) ->
                               {reply, term(), #state{}} | {error, term()}.
handle_transport_call(get_url, State) ->
    {reply, {ok, State#state.url}, State};
handle_transport_call({set_headers, Headers}, State) when is_list(Headers) ->
    NewHeaders = merge_headers(Headers),
    NewState = State#state{headers = NewHeaders},
    {reply, ok, NewState};
handle_transport_call({set_timeout, Timeout}, State)
    when is_integer(Timeout), Timeout > 0 ->
    NewHttpOptions = update_http_timeout(State#state.http_options, Timeout),
    NewState = State#state{http_options = NewHttpOptions},
    {reply, ok, NewState};
handle_transport_call(_Request, _State) ->
    {error, unknown_request}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(TransportId, Config) ->
    gen_server:start_link(?MODULE, [TransportId, Config], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the gen_server process (gen_server callback)
%% Note: This shadows the behavior init/1 callback by design
init([TransportId, Config]) ->
    process_flag(trap_exit, true),
    ConfigWithId = Config#{transport_id => TransportId},
    case init_transport(TransportId, ConfigWithId) of
        {ok, State} ->
            {ok, State};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({send, Data}, _From, State) ->
    case send(State, Data) of
        ok ->
            NewStats = update_statistics(State#state.statistics, messages_sent, 1),
            NewState = State#state{statistics = NewStats, last_error = undefined},
            {reply, ok, NewState};
        {error, Reason} = Error ->
            NewStats = update_statistics(State#state.statistics, errors, 1),
            NewState =
                State#state{statistics = NewStats,
                            connection_state = error,
                            last_error = Reason},
            {reply, Error, NewState}
    end;
handle_call(close, _From, State) ->
    close(State),
    {stop, normal, ok, State};
handle_call(get_info, _From, State) ->
    Info = get_info(State),
    {reply, Info, State};
handle_call({transport_call, Request}, _From, State) ->
    case handle_transport_call(Request, State) of
        {reply, Reply, NewState} ->
            {reply, Reply, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, Reason}, State) ->
    ?LOG_WARNING("HTTP transport received EXIT signal: ~p", [Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    close(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Validate transport configuration using comprehensive schema
-spec validate_config(map()) -> ok | {error, term()}.
validate_config(Config) ->
    case erlmcp_config_validation:validate_transport_config(http, http, Config) of
        ok ->
            ok;
        {error, ValidationErrors} ->
            % Convert validation errors to legacy format for compatibility
            FirstError = hd(ValidationErrors),
            ErrorMessage = maps:get(message, FirstError, <<"Configuration validation failed">>),
            {error, {validation_failed, ErrorMessage, ValidationErrors}}
    end.

%% @private Legacy validation (kept for reference)
validate_config_legacy(Config) ->
    try
        case maps:get(url, Config, undefined) of
            undefined ->
                throw(missing_url);
            Url ->
                validate_url(Url)
        end,

        % Validate optional fields
        case maps:get(timeout, Config, undefined) of
            undefined ->
                ok;
            Timeout when is_integer(Timeout), Timeout > 0 ->
                ok;
            _ ->
                throw({invalid_timeout, maps:get(timeout, Config)})
        end,

        case maps:get(headers, Config, undefined) of
            undefined ->
                ok;
            Headers when is_list(Headers) ->
                ok;
            _ ->
                throw({invalid_headers, maps:get(headers, Config)})
        end,

        ok
    catch
        Reason ->
            {error, Reason};
        Class:Reason ->
            {error, {validation_error, Class, Reason}}
    end.

%% @private
%% Merge user headers with defaults
-spec merge_headers([{string(), string()}]) -> [{string(), string()}].
merge_headers(UserHeaders) ->
    % Convert to maps for easier merging
    DefaultMap = maps:from_list(?DEFAULT_HEADERS),
    UserMap = maps:from_list(UserHeaders),

    % User headers override defaults
    MergedMap = maps:merge(DefaultMap, UserMap),
    maps:to_list(MergedMap).

%% @private
%% Build HTTP options
-spec build_http_options(map()) -> [term()].
build_http_options(Config) ->
    BaseOpts =
        [{timeout, maps:get(timeout, Config, ?DEFAULT_TIMEOUT)},
         {connect_timeout, maps:get(connect_timeout, Config, ?DEFAULT_CONNECT_TIMEOUT)}],

    % Add SSL options if URL is HTTPS
    Url = maps:get(url, Config),
    case is_https_url(Url) of
        true ->
            SslOpts = maps:get(ssl_options, Config, []),
            [{ssl, SslOpts} | BaseOpts];
        false ->
            BaseOpts
    end.

%% @private
%% Check if URL is HTTPS
-spec is_https_url(binary() | string()) -> boolean().
is_https_url(Url) when is_binary(Url) ->
    is_https_url(binary_to_list(Url));
is_https_url(Url) when is_list(Url) ->
    case string:prefix(Url, "https://") of
        nomatch ->
            false;
        _ ->
            true
    end.

%% @private
%% Validate URL format
-spec validate_url(binary() | string()) -> ok | no_return().
validate_url(Url) when is_binary(Url) ->
    validate_url(binary_to_list(Url));
validate_url(Url) when is_list(Url) ->
    HasHttp =
        case string:prefix(Url, "http://") of
            nomatch ->
                false;
            _ ->
                true
        end,
    HasHttps =
        case string:prefix(Url, "https://") of
            nomatch ->
                false;
            _ ->
                true
        end,
    case HasHttp orelse HasHttps of
        false ->
            throw(invalid_url_format);
        true ->
            ok
    end;
validate_url(_) ->
    throw(invalid_url_type).

%% @private
%% Ensure value is string
-spec ensure_string(binary() | string()) -> string().
ensure_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
ensure_string(Value) when is_list(Value) ->
    Value.

%% @private
%% Update HTTP timeout in options
-spec update_http_timeout(list(), pos_integer()) -> list().
update_http_timeout(HttpOptions, NewTimeout) ->
    lists:keystore(timeout, 1, HttpOptions, {timeout, NewTimeout}).

%% @private
%% Update statistics counter
-spec update_statistics(map(), atom(), non_neg_integer()) -> map().
update_statistics(Stats, Key, Value) ->
    CurrentValue = maps:get(Key, Stats, 0),
    Stats#{Key => CurrentValue + Value, last_activity => erlang:system_time(millisecond)}.

%% @private
%% Register transport with the registry using standard behavior helper
-spec register_with_registry(#state{}) -> ok.
register_with_registry(#state{transport_id = TransportId, config = Config}) ->
    TransportConfig =
        Config#{type => http,
                url => maps:get(url, Config),
                timeout => maps:get(timeout, Config, ?DEFAULT_TIMEOUT),
                capabilities => [client, json_rpc, ssl],
                pid => self()},
    case erlmcp_transport_behavior:register_with_registry(TransportId,
                                                          self(),
                                                          TransportConfig)
    of
        ok ->
            ?LOG_DEBUG("Registered HTTP transport ~p with registry", [TransportId]),
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to register HTTP transport ~p: ~p", [TransportId, Reason]),
            ok  % Don't fail initialization if registry isn't available
    end.

%% @private
%% Unregister transport from the registry using standard behavior helper
-spec unregister_from_registry(#state{}) -> ok.
unregister_from_registry(#state{transport_id = TransportId}) ->
    erlmcp_transport_behavior:unregister_from_registry(TransportId),
    ?LOG_DEBUG("Unregistered HTTP transport ~p from registry", [TransportId]),
    ok.
