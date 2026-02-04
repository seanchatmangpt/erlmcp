%%%-------------------------------------------------------------------
%%% @doc SwarmFlow OS Application
%%%
%%% Main application module for SwarmFlow OS autonomic workflow runtime.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(swarmflow_os_app).

-behaviour(application).

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc Start the SwarmFlow OS application
%% @end
%%--------------------------------------------------------------------
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    swarmflow_os_sup:start_link().

%%--------------------------------------------------------------------
%% @doc Stop the SwarmFlow OS application
%% @end
%%--------------------------------------------------------------------
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
