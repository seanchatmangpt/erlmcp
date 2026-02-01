%%%===================================================================
%%% @doc Test Synchronization Helper Functions Header
%%%
%%% This header file exports all functions from erlmcp_test_sync module
%%% for easy inclusion in test files.
%%%
%%% Usage:
%%% ```
%%% -include_lib("erlmcp/include/erlmcp.hrl").
%%% -include_lib("erlmcp/include/erlmcp_test_sync.hrl").
%%%
%%% {ok, _} = poll_until(fun() -> ready() end, 100, 2000).
%%% ```
%%% @end
%%%===================================================================

%% Core polling functions
-export([poll_until/4, poll_until/5, wait_for_condition/3, wait_for_condition/4]).
%% Process monitoring functions
-export([wait_for_process_death/2, wait_for_process_death/3, assert_process_dead/1]).
%% Message waiting functions
-export([wait_for_message/2, wait_for_message/3, flush_messages/0, wait_for_message_type/2]).
%% ETS table monitoring functions
-export([wait_for_ets_insert/2, wait_for_ets_insert/3, wait_for_ets_delete/2, wait_for_ets_delete/3,
         wait_for_ets_match/3, wait_for_ets_counter/3]).
%% Gen_server state monitoring
-export([wait_for_state/3, wait_for_state/4, assert_state/3]).
%% Utility functions
-export([retry/3, retry_with_backoff/4, flush_mailbox/0, wait_for/2]).
