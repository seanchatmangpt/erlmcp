%%%-----------------------------------------------------------------------------
%%% @doc TCPS Application Behavior
%%%
%%% OTP application entry point for Toyota Code Production System.
%%% Initializes the 8-layer supervision tree.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_erlmcp_app).
-behaviour(application).

-export([start/2, stop/1]).

%%==============================================================================
%% Application callbacks
%%==============================================================================

-spec start(StartType :: application:start_type(), StartArgs :: term()) ->
    {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    tcps_erlmcp_sup:start_link().

-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.
