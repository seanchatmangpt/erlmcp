%%%-------------------------------------------------------------------
%%% @doc
%%% Trace integration points for erlmcp core components
%%%
%%% Provides helper functions to add OTP 28 trace points to:
%%% - Tool invocation chains
%%% - Session lifecycle events
%%% - Message passing
%%%
%%% Usage:
%%%   Add trace_point() calls at critical paths in erlmcp_server,
%%%   erlmcp_client, and erlmcp_session_backend.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_trace_integration).

%% API
-export([trace_tool_call/3]).
-export([trace_tool_result/3]).
-export([trace_session_event/2]).
-export([trace_message/3]).
-export([trace_error/2]).

%% Optional: Enable/disable tracing at runtime
-export([is_tracing_enabled/0]).
-export([enable_tracing/0]).
-export([disable_tracing/0]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type tool_name() :: binary().
-type tool_args() :: map().
-type tool_result() :: map() | {error, term()}.
-type session_event() :: created | initialized | closed | timeout.
-type message_type() :: send | recv.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Trace a tool call invocation
%% Call this when a tool is about to be executed
-spec trace_tool_call(pid(), tool_name(), tool_args()) -> ok.
trace_tool_call(ServerPid, ToolName, Args) when is_pid(ServerPid) ->
    case is_tracing_enabled() of
        true ->
            ?LOG(debug,
                 "[TRACE] Tool call: ~p on ~p with args: ~p",
                 [ToolName, ServerPid, Args]),
            erlmcp_tracer:trace_tool_calls(),
            ok;
        false ->
            ok
    end.

%% @doc Trace a tool call result
%% Call this when a tool execution completes
-spec trace_tool_result(pid(), tool_name(), tool_result()) -> ok.
trace_tool_result(ServerPid, ToolName, Result) when is_pid(ServerPid) ->
    case is_tracing_enabled() of
        true ->
            ?LOG(debug,
                 "[TRACE] Tool result: ~p on ~p result: ~p",
                 [ToolName, ServerPid, Result]),
            ok;
        false ->
            ok
    end.

%% @doc Trace a session lifecycle event
%% Call this when a session state changes
-spec trace_session_event(binary(), session_event()) -> ok.
trace_session_event(SessionId, Event) when is_binary(SessionId) ->
    case is_tracing_enabled() of
        true ->
            ?LOG(debug,
                 "[TRACE] Session event: ~p for session ~p",
                 [Event, SessionId]),
            ok;
        false ->
            ok
    end.

%% @doc Trace a message send/receive
%% Call this when sending or receiving MCP messages
-spec trace_message(pid(), message_type(), term()) -> ok.
trace_message(Pid, Type, Message) when is_pid(Pid) ->
    case is_tracing_enabled() of
        true ->
            ?LOG(debug,
                 "[TRACE] Message ~p: ~p on ~p",
                 [Type, Message, Pid]),
            ok;
        false ->
            ok
    end.

%% @doc Trace an error condition
%% Call this when an error occurs during execution
-spec trace_error(pid(), term()) -> ok.
trace_error(Pid, Error) when is_pid(Pid) ->
    case is_tracing_enabled() of
        true ->
            ?LOG(error,
                 "[TRACE] Error on ~p: ~p",
                 [Pid, Error]),
            ok;
        false ->
            ok
    end.

%% @doc Check if tracing is enabled
-spec is_tracing_enabled() -> boolean().
is_tracing_enabled() ->
    case application:get_env(erlmcp_observability, tracing_enabled) of
        {ok, true} -> true;
        _ -> false
    end.

%% @doc Enable tracing at runtime
-spec enable_tracing() -> ok.
enable_tracing() ->
    application:set_env(erlmcp_observability, tracing_enabled, true),
    logger:info("Tracing enabled"),
    ok.

%% @doc Disable tracing at runtime
-spec disable_tracing() -> ok.
disable_tracing() ->
    application:set_env(erlmcp_observability, tracing_enabled, false),
    logger:info("Tracing disabled"),
    ok.
