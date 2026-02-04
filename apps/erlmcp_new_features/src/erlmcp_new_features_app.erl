-module(erlmcp_new_features_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    erlmcp_new_features_sup:start_link().

stop(_State) ->
    ok.
