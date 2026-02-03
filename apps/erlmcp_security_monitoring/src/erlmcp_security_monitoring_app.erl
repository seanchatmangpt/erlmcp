-module(erlmcp_security_monitoring_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Start the security monitoring supervision tree
    erlmcp_security_monitoring_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

-spec prep_stop(term()) -> term().
prep_stop(State) ->
    logger:info("Preparing erlmcp_security_monitoring for graceful shutdown"),
    %% Flush security events before shutdown
    try
        erlmcp_security_monitoring_sup:flush_events()
    catch
        _:_ -> ok
    end,
    State.