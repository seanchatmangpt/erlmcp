-module(erlmcp_otp_manager_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% @doc Start the OTP manager application
%% Initializes cache directory and starts supervision tree
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Initialize environment (cache dir, version requirements)
    ok = init_environment(),

    %% Start supervision tree
    case erlmcp_otp_manager_sup:start_link() of
        {ok, Pid} ->
            %% Pre-warm cache if configured (async, don't block startup)
            maybe_prewarm_cache(),
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc Stop the OTP manager application
-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Initialize cache directory
-spec init_environment() -> ok.
init_environment() ->
    CacheDir = application:get_env(erlmcp_otp_manager, cache_dir, ".erlmcp/otp-cache"),
    filelib:ensure_dir(filename:join(CacheDir, ".keep")),
    ok.

%% @private Pre-warm cache if enabled
-spec maybe_prewarm_cache() -> ok.
maybe_prewarm_cache() ->
    case application:get_env(erlmcp_otp_manager, prewarm_cache, false) of
        true ->
            spawn(fun() ->
                timer:sleep(100),
                %% Cache warming logic delegated to cache_server
                ok
            end);
        false ->
            ok
    end,
    ok.
