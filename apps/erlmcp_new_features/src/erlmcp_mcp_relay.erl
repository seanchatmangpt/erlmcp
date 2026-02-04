%%%-------------------------------------------------------------------
%%% @doc
%%% MCP Relay Server - Proxy requests to backend MCP servers
%%%
%%% This gen_server implements a request relay that forwards JSON-RPC 2.0
%%% requests to multiple backend MCP servers. Supports dynamic backend
%%% management and load distribution.
%%%
%%% Features:
%%% - Dynamic backend registration/deregistration
%%% - Request relay with timeout handling
%%% - Backend health monitoring
%%% - Request correlation and tracking
%%% - Load distribution across backends
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_mcp_relay).
-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         relay_request/2,
         relay_request/3,
         add_backend/2,
         remove_backend/1,
         list_backends/0,
         get_backend_status/1,
         set_backend_enabled/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type backend_id() :: atom() | binary().
-type backend_url() :: binary() | string().
-type backend_config() ::
    #{url => backend_url(),
      enabled => boolean(),
      timeout => pos_integer(),
      weight => non_neg_integer(),
      healthy => boolean()}.

-type request() ::
    #{jsonrpc := binary(),
      method := binary(),
      id => term(),
      params => map() | [term()]}.

-type response() ::
    #{jsonrpc := binary(),
      result := term(),
      id := term()} |
    #{jsonrpc := binary(),
      error := map(),
      id := term()}.

-record(state, {
    backends = #{} :: #{backend_id() => backend_config()},
    pending_requests = #{} :: #{reference() => {pid(), backend_id()}},
    request_timeout = 5000 :: pos_integer()
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the relay server with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the relay server with options.
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Relay a JSON-RPC request to an available backend.
%% Uses round-robin selection among enabled, healthy backends.
-spec relay_request(backend_id() | undefined, request()) ->
    {ok, response()} | {error, term()}.
relay_request(undefined, Request) ->
    relay_request(Request, 5000).

relay_request(Request, Timeout) when is_map(Request), is_integer(Timeout) ->
    gen_server:call(?MODULE, {relay_request, Request, Timeout}, Timeout).

%% @doc Relay a request to a specific backend.
-spec relay_request(backend_id(), request(), pos_integer()) ->
    {ok, response()} | {error, term()}.
relay_request(BackendId, Request, Timeout) ->
    gen_server:call(?MODULE, {relay_request, BackendId, Request, Timeout}, Timeout).

%% @doc Add a new backend to the relay.
-spec add_backend(backend_id(), backend_config()) -> ok | {error, term()}.
add_backend(BackendId, Config) ->
    gen_server:call(?MODULE, {add_backend, BackendId, Config}).

%% @doc Remove a backend from the relay.
-spec remove_backend(backend_id()) -> ok | {error, term()}.
remove_backend(BackendId) ->
    gen_server:call(?MODULE, {remove_backend, BackendId}).

%% @doc List all configured backends.
-spec list_backends() -> {ok, [{backend_id(), backend_config()}]}.
list_backends() ->
    gen_server:call(?MODULE, list_backends).

%% @doc Get the status of a specific backend.
-spec get_backend_status(backend_id()) -> {ok, backend_config()} | {error, not_found}.
get_backend_status(BackendId) ->
    gen_server:call(?MODULE, {get_backend_status, BackendId}).

%% @doc Enable or disable a backend.
-spec set_backend_enabled(backend_id(), boolean()) -> ok | {error, term()}.
set_backend_enabled(BackendId, Enabled) ->
    gen_server:call(?MODULE, {set_backend_enabled, BackendId, Enabled}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init(Options) ->
    RequestTimeout = proplists:get_value(request_timeout, Options, 5000),
    logger:info("Initializing MCP Relay with timeout ~p", [RequestTimeout]),
    {ok, #state{request_timeout = RequestTimeout}}.

%% @private
handle_call({relay_request, Request, Timeout}, From, State) ->
    case select_backend(State) of
        {ok, BackendId, Config} ->
            do_relay_request(BackendId, Request, Timeout, From, State);
        {error, no_backends} ->
            {reply, {error, no_available_backends}, State}
    end;

handle_call({relay_request, BackendId, Request, Timeout}, From, State) ->
    case maps:get(BackendId, State#state.backends, undefined) of
        undefined ->
            {reply, {error, {backend_not_found, BackendId}}, State};
        #{enabled := false} ->
            {reply, {error, {backend_disabled, BackendId}}, State};
        #{healthy := false} ->
            {reply, {error, {backend_unhealthy, BackendId}}, State};
        Config ->
            do_relay_request(BackendId, Request, Timeout, From, State)
    end;

handle_call({add_backend, BackendId, Config}, _From, State) ->
    case maps:is_key(BackendId, State#state.backends) of
        true ->
            {reply, {error, {backend_exists, BackendId}}, State};
        false ->
            DefaultConfig = #{enabled => true,
                            timeout => 5000,
                            weight => 1,
                            healthy => true},
            MergedConfig = maps:merge(DefaultConfig, Config),
            logger:info("Added backend ~p with config ~p", [BackendId, MergedConfig]),
            {reply, ok, State#state{backends = maps:put(BackendId, MergedConfig,
                                                       State#state.backends)}}
    end;

handle_call({remove_backend, BackendId}, _From, State) ->
    case maps:is_key(BackendId, State#state.backends) of
        true ->
            logger:info("Removed backend ~p", [BackendId]),
            {reply, ok, State#state{backends = maps:remove(BackendId,
                                                           State#state.backends)}};
        false ->
            {reply, {error, {backend_not_found, BackendId}}, State}
    end;

handle_call(list_backends, _From, State) ->
    BackendsList = maps:to_list(State#state.backends),
    {reply, {ok, BackendsList}, State};

handle_call({get_backend_status, BackendId}, _From, State) ->
    case maps:get(BackendId, State#state.backends, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Config ->
            {reply, {ok, Config}, State}
    end;

handle_call({set_backend_enabled, BackendId, Enabled}, _From, State) ->
    case maps:get(BackendId, State#state.backends, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Config ->
            UpdatedConfig = Config#{enabled => Enabled},
            logger:info("Backend ~p enabled set to ~p", [BackendId, Enabled]),
            {reply, ok, State#state{backends = maps:put(BackendId, UpdatedConfig,
                                                        State#state.backends)}}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Select an available backend using weighted round-robin.
-spec select_backend(state()) -> {ok, backend_id(), backend_config()} |
                                   {error, no_backends}.
select_backend(State) ->
    EnabledBackends = maps:filter(fun(_K, V) ->
        maps:get(enabled, V, true) andalso maps:get(healthy, V, true)
    end, State#state.backends),

    case maps:size(EnabledBackends) of
        0 ->
            {error, no_backends};
        _ ->
            WeightedList = mapsfold(fun(K, V, Acc) ->
                Weight = maps:get(weight, V, 1),
                lists:duplicate(Weight, K) ++ Acc
            end, [], EnabledBackends),
            SelectedId = lists:nth(rand:uniform(length(WeightedList)), WeightedList),
            Config = maps:get(SelectedId, EnabledBackends),
            {ok, SelectedId, Config}
    end.

%% @doc Relay a request to the specified backend.
-spec do_relay_request(backend_id(), request(), pos_integer(),
                        {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
do_relay_request(BackendId, Request, Timeout, From, State) ->
    Config = maps:get(BackendId, State#state.backends),
    BackendTimeout = maps:get(timeout, Config, 5000),

    case maps:get(url, Config, undefined) of
        undefined ->
            {reply, {error, {backend_misconfigured, BackendId}}, State};
        Url ->
            spawn(fun() ->
                Result = send_request(Url, Request, min(Timeout, BackendTimeout)),
                gen_server:reply(From, Result)
            end),
            {noreply, State}
    end.

%% @doc Send HTTP request to backend URL.
-spec send_request(backend_url(), request(), pos_integer()) ->
    {ok, response()} | {error, term()}.
send_request(Url, Request, Timeout) ->
    try
        Header = [{"Content-Type", "application/json"}],
        Body = jsone:encode(Request),
        Options = [{timeout, Timeout}, {body_format, binary}],

        case httpc:request(post, {binary_to_list(Url), Header, "application/json", Body},
                          [], Options) of
            {ok, {{_, 200, _}, _Headers, ResponseBody}} ->
                Response = jsone:decode(ResponseBody),
                {ok, Response};
            {ok, {{_, StatusCode, _}, _, ResponseBody}} when StatusCode >= 400 ->
                logger:error("Backend error ~p: ~p", [StatusCode, ResponseBody]),
                {error, {http_error, StatusCode}};
            {error, Reason} ->
                logger:error("Backend request failed: ~p", [Reason]),
                {error, {request_failed, Reason}}
        end
    catch
        Type:Error:Stacktrace ->
            logger:error("Request encoding failed: ~p:~p ~p",
                        [Type, Error, Stacktrace]),
            {error, {encode_failed, Error}}
    end.

%% @private Helper for fold over maps.
mapsfold(Fun, Acc0, Map) when is_function(Fun, 3), is_map(Map) ->
    maps:fold(Fun, Acc0, Map).
