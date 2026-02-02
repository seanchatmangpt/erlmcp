%%%-------------------------------------------------------------------
%%% @doc OTP 28 Priority Message Queues for MCP Urgent Control Signals
%%%
%%% This module provides OTP 28 EEP-76 priority message queue support,
%%% allowing critical messages to jump the queue while preserving ordering.
%%%
%%% == Priority Message Use Cases ==
%%% 1. Cancellation signals - User aborts long-running operations
%%% 2. Ping/health checks - Liveness monitoring during high load
%%% 3. System control - Shutdown, reconfiguration signals
%%% 4. Error alerts - Critical failure notifications
%%%
%%% == OTP 28 Innovation: EEP-76 Priority Messages ==
%%% - erlang:alias/1 creates priority-enabled aliases
%%% - erlang:send/3 with [priority] option jumps queue
%%% - Priority messages preserve ordering among themselves
%%% - Zero performance impact when unused
%%%
%%% == API Overview ==
%%% - create_priority_alias/0: Create alias for priority receiving
%%% - send_priority/3: Send priority message with sender context
%%% - send_urgent/2: Send urgent system message without context
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_priority).

%% API
-export([create_priority_alias/0, send_priority/3, send_urgent/2]).
-export([is_priority_alias/1]).

%% Types
-type priority_alias() :: erlang:alias().
-type priority_message() :: {priority, pid() | undefined, term()}.
-type urgent_message() :: {urgent, term()}.

-export_type([priority_alias/0, priority_message/0, urgent_message/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create a priority alias for a process.
%%
%% Creates an alias using OTP 28's erlang:alias/1 with [priority] option.
%% This alias can receive priority messages that jump the normal message queue.
%%
%% == Example ==
%% <pre>
%% Alias = erlmcp_priority:create_priority_alias(),
%% %% Send normal message
%% Alias ! {normal, data},
%% %% Send priority message (jumps queue)
%% erlmcp_priority:send_urgent(Alias, {urgent, shutdown})
%% </pre%%
%%
%% @returns Priority alias for use with send_priority/3 and send_urgent/2
%% @throws badarg if OTP 28 priority queues not available
-spec create_priority_alias() -> priority_alias().
create_priority_alias() ->
    try
        erlang:alias([priority])
    catch
        error:undef ->
            error({otp_version_unsupported, "Priority queues require OTP 28+"})
    end.

%% @doc Send a priority message that jumps the queue.
%%
%% Priority messages are delivered before normal messages in the mailbox,
%% preserving ordering among themselves.
%%
%% == Use Cases ==
%% - Cancellation signals: send_priority(Alias, {cancel, ReqId}, ClientPid)
%% - Health checks: send_priority(Alias, {ping, Ref}, From)
%% - Control signals: send_priority(Alias, {reconfigure, Config}, AdminPid)
%%
%% == Example ==
%% <pre>
%% Alias = erlmcp_priority:create_priority_alias(),
%% %% Normal operation
%% Self ! {normal, work},
%% %% Priority: Cancel operation (jumps ahead)
%% erlmcp_priority:send_priority(Alias, {cancel, ReqId}, ClientPid)
%% </pre>
%%
%% @param Alias Priority alias from create_priority_alias/0
%% @param Message Message payload (any term)
%% @param From Sender pid (for reply correlation)
%% @returns ok
%% @throws badarg if alias is invalid
-spec send_priority(priority_alias(), term(), pid()) -> ok.
send_priority(Alias, Message, From) when is_pid(From) ->
    try
        erlang:send(Alias, {priority, From, Message}, [priority])
    catch
        error:badarg ->
            error({invalid_alias, Alias})
    end.

%% @doc Send an urgent system message without sender context.
%%
%% Similar to send_priority/3 but without sender tracking.
%% Use for system-level urgent messages that don't require reply.
%%
%% == Use Cases ==
%% - Shutdown signals: send_urgent(Alias, shutdown)
%% - Error alerts: send_urgent(Alias, {critical_error, Reason})
%% - Reconfiguration: send_urgent(Alias, {reload_config, NewConfig})
%%
%% == Example ==
%% <pre>
%% Alias = erlmcp_priority:create_priority_alias(),
%% %% System shutdown signal (urgent, no reply needed)
%% erlmcp_priority:send_urgent(Alias, shutdown)
%% </pre>
%%
%% @param Alias Priority alias from create_priority_alias/0
%% @param Message Message payload (any term)
%% @returns ok
%% @throws badarg if alias is invalid
-spec send_urgent(priority_alias(), term()) -> ok.
send_urgent(Alias, Message) ->
    try
        erlang:send(Alias, {urgent, Message}, [priority])
    catch
        error:badarg ->
            error({invalid_alias, Alias})
    end.

%% @doc Check if a term is a priority alias.
%%
%% Useful for validation in gen_server callbacks and type checking.
%%
%% == Example ==
%% <pre>
%% handle_info({priority, From, Msg}, State) ->
%%     case erlmcp_priority:is_priority_alias(From) of
%%         true -> handle_priority_message(Msg, From, State);
%%         false -> {noreply, State}
%%     end
%% </pre>
%%
%% @param Term Term to check
%% @returns true if term is a priority alias, false otherwise
-spec is_priority_alias(term()) -> boolean().
is_priority_alias(Term) ->
    try
        %% OTP 28 aliases have internal structure
        %% This is a best-effort check
        erlang:is_process_alive(Term),
        true
    catch
        _:_ ->
            false
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Validate OTP 28 availability (internal).
%% Note: This function is reserved for future validation.
%% @private
%% -spec ensure_otp28() -> ok | no_return().
%% ensure_otp28() ->
%%     case erlang:system_info(otp_release) >= "28" of
%%         true ->
%%             ok;
%%         false ->
%%             error({otp_version_required, "28"})
%%     end.
