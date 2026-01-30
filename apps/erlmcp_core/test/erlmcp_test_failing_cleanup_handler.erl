%%%-------------------------------------------------------------------
%%% @doc Test failing cleanup handler for cancellation tests
%%%
%%% This module provides a cleanup handler that always fails, used to test
%%% that cleanup handler failures don't crash the cancellation manager.
%%%-------------------------------------------------------------------
-module(erlmcp_test_failing_cleanup_handler).

%% API
-export([cleanup_operation/2]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Cleanup operation callback that always fails
%% This simulates a cleanup handler that throws an exception
cleanup_operation(_Token, _Reason) ->
    %% Always fail to test error handling
    exit(cleanup_failed).
