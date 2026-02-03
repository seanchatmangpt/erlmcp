%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_transports_app - Transports Application Callback
%%%
%%% OTP application callback for the transports subsystem.
%%% Starts the transport supervision tree.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transports_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Application Callbacks
%%====================================================================

%% @doc Start the transports application
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    ?LOG_INFO("Starting erlmcp_transports application"),

    %% Start the supervisor
    case erlmcp_transport_sup:start_link() of
        {ok, Pid} ->
            ?LOG_INFO("erlmcp_transports started successfully"),
            {ok, Pid};
        {error, Reason} = Error ->
            ?LOG_ERROR("Failed to start erlmcp_transports: ~p", [Reason]),
            Error
    end.

%% @doc Stop the transports application
-spec stop(term()) -> ok.
stop(_State) ->
    ?LOG_INFO("Stopping erlmcp_transports application"),
    ok.

%% @doc Prepare for graceful shutdown
-spec prep_stop(term()) -> term().
prep_stop(State) ->
    ?LOG_INFO("Preparing erlmcp_transports for graceful shutdown"),
    %% Stop accepting new connections
    try
        Pid = whereis(erlmcp_transport_sup),
        case Pid of
            undefined ->
                ok;
            _ ->
                erlmcp_transport_sup:stop_accepting()
        end
    catch
        _:_ -> ok
    end,
    %% Drain existing connection pool
    try
        PoolPid = whereis(erlmcp_connection_pool),
        case PoolPid of
            undefined ->
                ok;
            _ ->
                erlmcp_connection_pool:drain()
        end
    catch
        _:_ -> ok
    end,
    State.
