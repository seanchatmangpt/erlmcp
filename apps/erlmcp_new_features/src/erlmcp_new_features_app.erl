-module(erlmcp_new_features_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlmcp_new_features_sup:start_link().

stop(_State) ->
    ok.
