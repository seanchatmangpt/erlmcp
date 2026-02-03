-module(erlmcp_validation_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).

%%%====================================================================
%%% API
%%%====================================================================

start(_StartType, _StartArgs) ->
    erlmcp_validation_sup:start_link().

stop(_State) ->
    ok.

prep_stop(State) ->
    logger:info("Preparing erlmcp_validation for graceful shutdown"),
    %% Allow in-flight validations to complete
    State.
