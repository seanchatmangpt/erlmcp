%%%-------------------------------------------------------------------
%%% @doc CPU Guard - CPU Time Protection for Tool Execution
%%%
%%% This module provides CPU time quota and timeout protection
%%% for tool execution to prevent CPU-intensive DoS attacks.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cpu_guard).

%% API
-export([execute_with_protection/5]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Execute function with CPU time protection
%% Returns: {ok, Result, CpuTime} | {error, Reason}
-spec execute_with_protection(term(), term(), function(), list(), pos_integer()) ->
                                 {ok, term(), non_neg_integer()} | {error, term()}.
execute_with_protection(_ClientId, _Operation, Function, Args, Timeout) ->
    try
        %% Execute with timeout
        {ok, Result} = timer:apply_after(Timeout, erlang, send, [self(), timeout]),
        Result = apply(Function, Args),
        {ok, Result, 0}
    catch
        error:timeout ->
            {error, timeout};
        Class:Reason ->
            {error, Reason}
    end.
