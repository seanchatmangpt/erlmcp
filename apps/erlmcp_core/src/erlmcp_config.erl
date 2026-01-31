-module(erlmcp_config).
-behaviour(gen_server).

%% API exports - Fast zero-copy config access via persistent_term
-export([
    start_link/0,
    get/1,
    get/2,
    get_all/0,
    set/2,
    update/1,
    delete/1,
    reload/0,
    clear_cache/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Internal key macro
-define(KEY(Name), {erlmcp_config, Name}).

%% State record
-record(state, {
    defaults :: #{atom() => term()},
    loaded_at :: erlang:timestamp()
}).

-type state() :: #state{}.
-type config_key() :: atom().
-type config_value() :: term().

-export_type([config_key/0, config_value/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the config server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Get config value (fast zero-copy read from persistent_term)
%% Access time: ~10ns vs ~1μs for ETS
-spec get(config_key()) -> config_value().
get(Key) when is_atom(Key) ->
    persistent_term:get(?KEY(Key)).

%% @doc Get config value with default
-spec get(config_key(), config_value()) -> config_value().
get(Key, Default) when is_atom(Key) ->
    try
        persistent_term:get(?KEY(Key))
    catch
        error:badarg -> Default
    end.

%% @doc Get all config as map (snapshot)
-spec get_all() -> #{config_key() => config_value()}.
get_all() ->
    AllTerms = persistent_term:get(),
    ConfigTerms = [
        {K, V}
     || {{erlmcp_config, K}, V} <- AllTerms
    ],
    maps:from_list(ConfigTerms).

%% @doc Set config value (rare operation, triggers GC on readers)
%% WARNING: Updates trigger GC on all processes that accessed this key
%% Use update/1 for bulk updates to minimize GC events
-spec set(config_key(), config_value()) -> ok.
set(Key, Value) when is_atom(Key) ->
    gen_server:call(?MODULE, {set, Key, Value}).

%% @doc Bulk update (single GC event instead of N events)
%% Preferred over multiple set/2 calls for performance
-spec update(#{config_key() => config_value()}) -> ok.
update(Updates) when is_map(Updates) ->
    gen_server:call(?MODULE, {update, Updates}).

%% @doc Delete config key
-spec delete(config_key()) -> ok.
delete(Key) when is_atom(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

%% @doc Reload config from application environment
-spec reload() -> ok.
reload() ->
    gen_server:call(?MODULE, reload).

%% @doc Clear all cached config (for testing)
-spec clear_cache() -> ok.
clear_cache() ->
    gen_server:call(?MODULE, clear_cache).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    % Get defaults
    Defaults = defaults(),

    % Load config from application env and write to persistent_term
    load_config(Defaults),

    logger:info("erlmcp_config initialized with ~w keys using persistent_term", [
        maps:size(Defaults)
    ]),

    {ok, #state{defaults = Defaults, loaded_at = erlang:timestamp()}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call({set, Key, Value}, _From, State) ->
    persistent_term:put(?KEY(Key), Value),
    logger:debug("Config key ~p updated", [Key]),
    {reply, ok, State};
handle_call({update, Updates}, _From, State) ->
    maps:foreach(
        fun(K, V) ->
            persistent_term:put(?KEY(K), V)
        end,
        Updates
    ),
    logger:info("Bulk config update: ~w keys", [maps:size(Updates)]),
    {reply, ok, State};
handle_call({delete, Key}, _From, State) ->
    persistent_term:erase(?KEY(Key)),
    logger:debug("Config key ~p deleted", [Key]),
    {reply, ok, State};
handle_call(reload, _From, State) ->
    load_config(State#state.defaults),
    logger:info("Config reloaded from application environment", []),
    {reply, ok, State#state{loaded_at = erlang:timestamp()}};
handle_call(clear_cache, _From, State) ->
    % Clear all erlmcp_config keys
    AllTerms = persistent_term:get(),
    lists:foreach(
        fun
            ({Key, _}) when element(1, Key) =:= erlmcp_config ->
                persistent_term:erase(Key);
            (_) ->
                ok
        end,
        AllTerms
    ),
    logger:warning("Config cache cleared (testing only)", []),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    logger:info("erlmcp_config terminating", []),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Default configuration values
%% These are fallbacks if not set in application env
-spec defaults() -> #{config_key() => config_value()}.
defaults() ->
    #{
        % Message size limits
        max_message_size => 10485760,
        % 10MB
        max_batch_size => 100,

        % Timeouts
        request_timeout => 30000,
        % 30s
        idle_timeout => 300000,
        % 5min
        shutdown_timeout => 5000,
        % 5s

        % Connection limits
        max_connections => 10000,
        max_connections_per_ip => 100,

        % Transport defaults
        transport_defaults =>
            #{
                tcp => #{port => 8080, backlog => 1024, send_timeout => 5000},
                http => #{port => 8081, max_keepalive => 100},
                ws => #{port => 8082, ping_interval => 30000},
                stdio => #{buffer_size => 4096}
            },

        % Capabilities
        capabilities =>
            #{
                tools => true,
                resources => true,
                prompts => true,
                logging => true,
                sampling => true
            },

        % Rate limiting
        rate_limit_enabled => true,
        rate_limit_requests_per_second => 1000,
        rate_limit_burst => 100,

        % Circuit breaker
        circuit_breaker_enabled => true,
        circuit_breaker_threshold => 5,
        circuit_breaker_timeout => 60000,
        % 1min

        % Session management
        session_backend => erlmcp_session_ets,
        session_cleanup_interval => 60000,
        % 1min
        session_max_age => 3600000,
        % 1hr

        % Observability
        metrics_enabled => true,
        tracing_enabled => false,
        health_check_interval => 10000,
        % 10s

        % Performance tuning
        pool_size => 10,
        pool_max_overflow => 20,
        buffer_size => 4096,
        read_concurrency => true,
        write_concurrency => true
    }.

%% @doc Load config from application environment into persistent_term
%% Merges app env with defaults
-spec load_config(#{config_key() => config_value()}) -> ok.
load_config(Defaults) ->
    maps:foreach(
        fun(Key, DefaultValue) ->
            % Get from app env or use default
            Value = application:get_env(erlmcp_core, Key, DefaultValue),
            persistent_term:put(?KEY(Key), Value)
        end,
        Defaults
    ).
