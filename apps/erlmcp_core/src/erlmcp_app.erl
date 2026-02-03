-module(erlmcp_app).

-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Start the supervision tree
    erlmcp_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

-spec prep_stop(term()) -> term().
prep_stop(State) ->
    logger:info("Preparing erlmcp_core for graceful shutdown"),
    %% Initiate graceful shutdown of registry connections
    try
        case whereis(erlmcp_registry) of
            undefined ->
                ok;
            _Pid ->
                erlmcp_registry:graceful_shutdown()
        end
    catch
        _:_ -> ok
    end,
    State.
