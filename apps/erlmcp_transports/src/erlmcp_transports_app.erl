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
-export([start/2, stop/1]).

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
