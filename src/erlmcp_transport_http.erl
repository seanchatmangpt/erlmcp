%%%-------------------------------------------------------------------
%%% @doc
%%% Simplified HTTP Transport for Erlang MCP (Model Context Protocol)
%%%
%%% This module provides a simplified HTTP client transport implementation
%%% for MCP. It focuses on the core use case: making HTTP requests to MCP
%%% servers and handling JSON-RPC 2.0 responses over HTTP.
%%%
%%% Features:
%%% - HTTP/HTTPS client functionality
%%% - JSON-RPC 2.0 over HTTP
%%% - Registry integration
%%% - Proper error handling and timeouts
%%% - SSL/TLS support
%%% - Connection pooling
%%% - Retry logic
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_http).

-behaviour(gen_server).

-include("erlmcp.hrl").

-include_lib("kernel/include/logger.hrl").

%% Transport API
-export([send/2, close/1, get_info/1, handle_transport_call/2]).
%% API
-export([start_link/2, start_link/3, stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Types
-type http_method() :: get | post | put | delete.
-type http_headers() :: [{string(), string()}].

%% Simplified state record focused on MCP use case
-record(state,
        {%% Core identification
         transport_id :: atom(),
         owner_pid :: pid(),
         registry_pid :: pid() | undefined,
         %% HTTP client configuration
         url :: binary(),
         method = post :: http_method(),
         headers = [] :: http_headers(),
         %% Connection settings
         timeout = 30000 :: timeout(),
         connect_timeout = 10000 :: timeout(),
         ssl_options = [] :: [ssl:tls_client_option()],
         %% Retry and reliability
         max_retries = 3 :: non_neg_integer(),
         retry_delay = 1000 :: pos_integer(),
         %% State tracking
         connected = false :: boolean(),
         error_count = 0 :: non_neg_integer(),
         last_error :: term() | undefined,
         %% Statistics
         requests_sent = 0 :: non_neg_integer(),
         responses_received = 0 :: non_neg_integer(),
         bytes_sent = 0 :: non_neg_integer(),
         bytes_received = 0 :: non_neg_integer(),
         %% Internal
         http_client_options :: map()}).

-type state() :: #state{}.

%% Default HTTP headers for MCP
-define(DEFAULT_HEADERS,
        [{"Content-Type", "application/json"},
         {"Accept", "application/json"},
         {"User-Agent", "erlmcp-http-transport/1.0"}]).

%%====================================================================
%% Transport API Implementation
%%====================================================================

-spec send(state(), iodata()) -> ok | {error, term()}.
send(State, Data) ->
    send_with_retry(State, Data, State#state.max_retries).

-spec close(state()) -> ok.
close(State) ->
    unregister_from_registry(State),
    ?LOG_INFO("HTTP transport closed for URL: ~s", [State#state.url]),
    ok.

-spec get_info(state()) -> erlmcp_transport:transport_info().
get_info(State) ->
    #{type => http,
      version => <<"1.0">>,
      capabilities => [client, json_rpc, ssl, retry],
      connection_state =>
          case State#state.connected of
              true ->
                  connected;
              false ->
                  disconnected
          end,
      statistics =>
          #{requests_sent => State#state.requests_sent,
            responses_received => State#state.responses_received,
            bytes_sent => State#state.bytes_sent,
            bytes_received => State#state.bytes_received,
            error_count => State#state.error_count},
      url => State#state.url,
      method => State#state.method,
      timeout => State#state.timeout,
      last_error => State#state.last_error}.

-spec handle_transport_call(term(), state()) ->
                               {reply, term(), state()} | {error, term()}.
handle_transport_call(get_url, State) ->
    {reply, State#state.url, State};
handle_transport_call({set_timeout, Timeout}, State)
    when is_integer(Timeout), Timeout > 0 ->
    NewState = State#state{timeout = Timeout},
    {reply, ok, NewState};
handle_transport_call({set_headers, Headers}, State) when is_list(Headers) ->
    NewState = State#state{headers = merge_headers(Headers)},
    {reply, ok, NewState};
handle_transport_call(reset_stats, State) ->
    NewState =
        State#state{requests_sent = 0,
                    responses_received = 0,
                    bytes_sent = 0,
                    bytes_received = 0,
                    error_count = 0,
                    last_error = undefined},
    {reply, ok, NewState};
handle_transport_call(reconnect, State) ->
    % For HTTP, reconnect means testing the connection
    case test_connection(State) of
        ok ->
            NewState = State#state{connected = true, last_error = undefined},
            {reply, ok, NewState};
        {error, Reason} ->
            NewState =
                State#state{connected = false,
                            last_error = Reason,
                            error_count = State#state.error_count + 1},
            {reply, {error, Reason}, NewState}
    end;
handle_transport_call(_Request, _State) ->
    {error, unknown_request}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(TransportId, Opts) ->
    gen_server:start_link(?MODULE, [TransportId, Opts], []).

-spec start_link(atom(), atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(TransportId, Name, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [TransportId, Opts], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([TransportId, Opts]) ->
    process_flag(trap_exit, true),

    ?LOG_DEBUG("Initializing HTTP transport ~p with opts: ~p", [TransportId, Opts]),

    case validate_http_opts(Opts) of
        ok ->
            Url = maps:get(url, Opts),
            OwnerPid = maps:get(owner, Opts),

            State =
                #state{transport_id = TransportId,
                       url = ensure_binary(Url),
                       owner_pid = OwnerPid,
                       method = maps:get(method, Opts, post),
                       headers = merge_headers(maps:get(headers, Opts, [])),
                       timeout = maps:get(timeout, Opts, 30000),
                       connect_timeout = maps:get(connect_timeout, Opts, 10000),
                       ssl_options = maps:get(ssl_options, Opts, []),
                       max_retries = maps:get(max_retries, Opts, 3),
                       retry_delay = maps:get(retry_delay, Opts, 1000),
                       http_client_options = build_client_options(Opts)},

            % Register with registry if available
            register_with_registry(State),

            % Test initial connection
            case test_connection(State) of
                ok ->
                    FinalState = State#state{connected = true},
                    ?LOG_INFO("HTTP transport ~p started successfully", [TransportId]),
                    {ok, FinalState};
                {error, Reason} ->
                    ?LOG_WARNING("HTTP transport ~p started but connection test failed: ~p",
                                 [TransportId, Reason]),
                    FinalState =
                        State#state{connected = false,
                                    last_error = Reason,
                                    error_count = 1},
                    {ok, FinalState}
            end;
        {error, Reason} ->
            ?LOG_ERROR("Failed to initialize HTTP transport ~p: ~p", [TransportId, Reason]),
            {stop, Reason}
    end.

handle_call({send, Data}, _From, State) ->
    case send(State, Data) of
        ok ->
            NewState =
                State#state{requests_sent = State#state.requests_sent + 1,
                            bytes_sent = State#state.bytes_sent + iolist_size(Data)},
            {reply, ok, NewState};
        {error, Reason} = Error ->
            NewState =
                State#state{error_count = State#state.error_count + 1,
                            last_error = Reason,
                            connected = false},
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

handle_info({http_response, Response}, State) ->
    % Handle asynchronous HTTP response if needed
    ?LOG_DEBUG("Received HTTP response: ~p", [Response]),
    NewState = State#state{responses_received = State#state.responses_received + 1},
    {noreply, NewState};
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
%% Validate HTTP transport options
-spec validate_http_opts(map()) -> ok | {error, term()}.
validate_http_opts(Opts) ->
    try
        % Check required fields
        case maps:get(url, Opts, undefined) of
            undefined ->
                throw(missing_url);
            Url ->
                validate_url(Url)
        end,

        case maps:get(owner, Opts, undefined) of
            undefined ->
                throw(missing_owner);
            Owner when is_pid(Owner) ->
                ok;
            _ ->
                throw(invalid_owner)
        end,

        validate_extended_http_opts(Opts)
    catch
        Reason ->
            {error, Reason};
        Class:Reason ->
            {error, {validation_error, Class, Reason}}
    end.

%% @private
%% duplicate older helper removed; canonical version exists below

%% @private
%% Validate extended HTTP options
-spec validate_extended_http_opts(map()) -> ok | {error, term()}.
validate_extended_http_opts(Opts) ->
    try
        % Validate method
        Method = maps:get(method, Opts, post),
        case lists:member(Method, [get, post, put, delete]) of
            true ->
                ok;
            false ->
                throw({invalid_method, Method})
        end,

        % Validate timeouts
        Timeout = maps:get(timeout, Opts, 30000),
        case is_integer(Timeout) andalso Timeout > 0 of
            true ->
                ok;
            false ->
                throw({invalid_timeout, Timeout})
        end,

        ConnectTimeout = maps:get(connect_timeout, Opts, 10000),
        case is_integer(ConnectTimeout) andalso ConnectTimeout > 0 of
            true ->
                ok;
            false ->
                throw({invalid_connect_timeout, ConnectTimeout})
        end,

        % Validate retries
        MaxRetries = maps:get(max_retries, Opts, 3),
        case is_integer(MaxRetries) andalso MaxRetries >= 0 of
            true ->
                ok;
            false ->
                throw({invalid_max_retries, MaxRetries})
        end,

        ok
    catch
        Reason ->
            {error, Reason};
        Class:Reason ->
            {error, {validation_error, Class, Reason}}
    end.

%% @private
%% Send data with retry logic
-spec send_with_retry(state(), iodata(), non_neg_integer()) -> ok | {error, term()}.
send_with_retry(_State, _Data, 0) ->
    {error, max_retries_exceeded};
send_with_retry(State, Data, RetriesLeft) ->
    case send_http_request(State, Data) of
        {ok, _Response} ->
            ok;
        {error, Reason} = Error ->
            case should_retry(Reason) of
                true when RetriesLeft > 1 ->
                    timer:sleep(State#state.retry_delay),
                    send_with_retry(State, Data, RetriesLeft - 1);
                _ ->
                    Error
            end
    end.

%% @private
%% Send actual HTTP request
-spec send_http_request(state(), iodata()) -> {ok, term()} | {error, term()}.
send_http_request(State, Data) ->
    #state{url = Url,
           method = Method,
           headers = Headers,
           timeout = _Timeout,
           ssl_options = _SSLOpts,
           http_client_options = _ClientOpts} =
        State,

    try
        % Ensure data is binary
        Body = iolist_to_binary(Data),

        % Build request options
        RequestOpts = build_request_options(State),

        % Make HTTP request using httpc
        case Method of
            get ->
                httpc:request(get, {binary_to_list(Url), Headers}, RequestOpts, []);
            post ->
                ContentType = proplists:get_value("Content-Type", Headers, "application/json"),
                httpc:request(post,
                              {binary_to_list(Url), Headers, ContentType, Body},
                              RequestOpts,
                              []);
            put ->
                ContentType = proplists:get_value("Content-Type", Headers, "application/json"),
                httpc:request(put,
                              {binary_to_list(Url), Headers, ContentType, Body},
                              RequestOpts,
                              []);
            delete ->
                httpc:request(delete, {binary_to_list(Url), Headers}, RequestOpts, [])
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("HTTP request failed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {error, {http_request_failed, Class, Reason}}
    end.

%% @private
%% Build HTTP request options
-spec build_request_options(state()) -> proplists:proplist().
build_request_options(State) ->
    BaseOpts =
        [{timeout, State#state.timeout}, {connect_timeout, State#state.connect_timeout}],

    % Add SSL options if URL is HTTPS
    case binary:match(State#state.url, <<"https://">>) of
        {0, _} ->
            [{ssl, State#state.ssl_options} | BaseOpts];
        nomatch ->
            BaseOpts
    end.

%% @private
%% Test connection to the HTTP endpoint
-spec test_connection(state()) -> ok | {error, term()}.
test_connection(State) ->
    % Simple HEAD request to test connectivity
    TestUrl = binary_to_list(State#state.url),
    RequestOpts = build_request_options(State#state{timeout = 5000}),

    case httpc:request(head, {TestUrl, []}, RequestOpts, []) of
        {ok, {{_, Status, _}, _, _}} when Status >= 200, Status < 400 ->
            ok;
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_status, Status}};
        {error, Reason} ->
            {error, {connection_test_failed, Reason}}
    end.

%% @private
%% Determine if an error should trigger a retry
-spec should_retry(term()) -> boolean().
should_retry({http_status, Status}) when Status >= 500 ->
    true;
should_retry({connection_test_failed, _}) ->
    true;
should_retry(timeout) ->
    true;
should_retry(connect_timeout) ->
    true;
should_retry({socket_error, _}) ->
    true;
should_retry(_) ->
    false.

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
%% Build HTTP client options
-spec build_client_options(map()) -> map().
build_client_options(Opts) ->
    #{pool_size => maps:get(pool_size, Opts, 10),
      max_sessions => maps:get(max_sessions, Opts, 20),
      keep_alive => maps:get(keep_alive, Opts, true)}.

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
%% Ensure value is binary
-spec ensure_binary(binary() | string()) -> binary().
ensure_binary(Value) when is_binary(Value) ->
    Value;
ensure_binary(Value) when is_list(Value) ->
    list_to_binary(Value).

%% @private
%% Register transport with the registry
-spec register_with_registry(state()) -> ok.
register_with_registry(#state{transport_id = TransportId} = State) ->
    case whereis(erlmcp_registry) of
        undefined ->
            ?LOG_WARNING("Registry not available for HTTP transport ~p", [TransportId]),
            ok;
        RegistryPid when is_pid(RegistryPid) ->
            Config =
                #{type => http,
                  url => State#state.url,
                  method => State#state.method,
                  timeout => State#state.timeout,
                  capabilities => [client, json_rpc, ssl, retry]},
            case erlmcp_registry:register_transport(TransportId, self(), Config) of
                ok ->
                    ?LOG_DEBUG("Registered HTTP transport ~p with registry", [TransportId]),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("Failed to register HTTP transport ~p: ~p", [TransportId, Reason]),
                    ok
            end
    end.

%% @private
%% Unregister transport from the registry
-spec unregister_from_registry(state()) -> ok.
unregister_from_registry(#state{transport_id = undefined}) ->
    ok;
unregister_from_registry(#state{transport_id = TransportId}) ->
    case whereis(erlmcp_registry) of
        undefined ->
            ok;
        RegistryPid when is_pid(RegistryPid) ->
            case erlmcp_registry:unregister_transport(TransportId) of
                ok ->
                    ?LOG_DEBUG("Unregistered HTTP transport ~p from registry", [TransportId]),
                    ok;
                {error, Reason} ->
                    ?LOG_WARNING("Failed to unregister HTTP transport ~p: ~p",
                                 [TransportId, Reason]),
                    ok
            end
    end.
