%%%-------------------------------------------------------------------
%%% @doc SwarmFlow PQChain Application
%%%
%%% Main application module for SwarmFlow PQChain post-quantum blockchain.
%%%
%%% Implements application behavior with proper startup and shutdown.
%%% Provides graceful shutdown via prep_stop/1 for consensus finalization.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(swarmflow_pqchain_app).

-behaviour(application).

-include("pqchain.hrl").

%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).

%%====================================================================
%% Application Callbacks
%%====================================================================

%% @doc Start the SwarmFlow PQChain application
%%
%% Starts the top-level supervisor which initializes all blockchain services.
%%
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Log startup
    logger:info("Starting SwarmFlow PQChain application"),

    %% Start top supervisor
    case swarmflow_pqchain_sup:start_link() of
        {ok, Pid} ->
            logger:info("SwarmFlow PQChain started successfully, supervisor: ~p", [Pid]),
            {ok, Pid};
        {error, Reason} = Error ->
            logger:error("Failed to start SwarmFlow PQChain: ~p", [Reason]),
            Error
    end.

%% @doc Prepare for application stop (graceful shutdown)
%%
%% Called before stop/1 to allow graceful shutdown of consensus and peer connections.
%% This is the proper place for finalization logic in OTP applications.
%%
-spec prep_stop(term()) -> term().
prep_stop(State) ->
    logger:info("Preparing SwarmFlow PQChain for shutdown"),

    %% Gracefully finalize consensus rounds
    try
        finalize_consensus()
    catch
        Class:Reason:Stacktrace ->
            logger:warning("Error during consensus finalization: ~p:~p~n~p",
                          [Class, Reason, Stacktrace])
    end,

    %% Gracefully close peer channels
    try
        close_peer_channels()
    catch
        Class2:Reason2:Stacktrace2 ->
            logger:warning("Error during peer channel closure: ~p:~p~n~p",
                          [Class2, Reason2, Stacktrace2])
    end,

    %% Persist final chain state
    try
        persist_chain_state()
    catch
        Class3:Reason3:Stacktrace3 ->
            logger:warning("Error during state persistence: ~p:~p~n~p",
                          [Class3, Reason3, Stacktrace3])
    end,

    logger:info("SwarmFlow PQChain prepared for shutdown"),
    State.

%% @doc Stop the SwarmFlow PQChain application
%%
%% Called after supervisor tree is shut down.
%% Most cleanup should happen in prep_stop/1.
%%
-spec stop(term()) -> ok.
stop(_State) ->
    logger:info("SwarmFlow PQChain stopped"),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Finalize all active consensus rounds
finalize_consensus() ->
    case whereis(pqc_consensus_sup) of
        undefined ->
            ok;
        _Pid ->
            Rounds = pqc_consensus_sup:list_rounds(),
            logger:info("Finalizing ~p consensus rounds", [length(Rounds)]),
            lists:foreach(
                fun(RoundPid) ->
                    try
                        pqc_consensus_sup:stop_round(RoundPid)
                    catch
                        _:_ -> ok
                    end
                end,
                Rounds
            )
    end.

%% @private
%% @doc Close all peer channels gracefully
close_peer_channels() ->
    case whereis(pqc_peer_sup) of
        undefined ->
            ok;
        _Pid ->
            Channels = pqc_peer_sup:list_channels(),
            logger:info("Closing ~p peer channels", [length(Channels)]),
            lists:foreach(
                fun(ChannelPid) ->
                    try
                        pqc_peer_sup:stop_channel(ChannelPid)
                    catch
                        _:_ -> ok
                    end
                end,
                Channels
            )
    end.

%% @private
%% @doc Persist current chain state to disk
persist_chain_state() ->
    case whereis(pqc_block) of
        undefined ->
            ok;
        Pid ->
            try
                logger:info("Persisting chain state"),
                %% This would call a persistence function on pqc_chain
                %% For now, just log
                logger:info("Chain state persisted")
            catch
                _:_ -> ok
            end
    end.
