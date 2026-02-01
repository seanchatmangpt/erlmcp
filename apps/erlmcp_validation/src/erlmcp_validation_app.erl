-module(erlmcp_validation_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%====================================================================
%%% API
%%%====================================================================

start(_StartType, _StartArgs) ->
    erlmcp_validation_sup:start_link().

stop(_State) ->
    ok.
