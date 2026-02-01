%%%-------------------------------------------------------------------
%%% @doc
%%% Validator Integration Hooks for Reproducer System
%%%
%%% Provides hooks that validators can call to emit reproducers
%%% when failures are detected.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_validator_hooks).

%% API exports
-export([maybe_emit_reproducer/4, emit_protocol_failure/3, emit_transport_failure/3,
         emit_sse_failure/3, enable_reproducer_capture/0, disable_reproducer_capture/0,
         is_capture_enabled/0]).

-define(CAPTURE_ENABLED_KEY, erlmcp_reproducer_capture_enabled).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Maybe emit a reproducer if capture is enabled
-spec maybe_emit_reproducer(binary(), binary(), term(), term()) -> ok.
maybe_emit_reproducer(Category, RuleId, Expected, Actual) ->
    case is_capture_enabled() of
        true ->
            case Category of
                protocol ->
                    emit_protocol_failure(RuleId, Expected, Actual);
                transport ->
                    emit_transport_failure(RuleId, Expected, Actual);
                sse ->
                    emit_sse_failure(RuleId, Expected, Actual);
                _ ->
                    ok
            end;
        false ->
            ok
    end.

%% @doc Emit protocol validation failure reproducer
-spec emit_protocol_failure(binary(), term(), term()) -> ok | {error, term()}.
emit_protocol_failure(RuleId, Expected, Actual) ->
    case whereis(erlmcp_reproducer) of
        undefined ->
            %% Reproducer system not started - skip
            ok;
        _Pid ->
            Input = get_failure_context(),
            case erlmcp_reproducer:capture_protocol_failure(RuleId, Input, Expected, Actual) of
                {ok, _Id} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc Emit transport validation failure reproducer
-spec emit_transport_failure(binary(), term(), term()) -> ok | {error, term()}.
emit_transport_failure(RuleId, Expected, Actual) ->
    case whereis(erlmcp_reproducer) of
        undefined ->
            ok;
        _Pid ->
            Input = get_failure_context(),
            case erlmcp_reproducer:capture_transport_failure(RuleId, Input, Expected, Actual) of
                {ok, _Id} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc Emit SSE validation failure reproducer
-spec emit_sse_failure(binary(), term(), term()) -> ok | {error, term()}.
emit_sse_failure(RuleId, Expected, Actual) ->
    case whereis(erlmcp_reproducer) of
        undefined ->
            ok;
        _Pid ->
            Input = get_failure_context(),
            case erlmcp_reproducer:capture_sse_failure(RuleId, Input, Expected, Actual) of
                {ok, _Id} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc Enable reproducer capture globally
-spec enable_reproducer_capture() -> ok.
enable_reproducer_capture() ->
    application:set_env(erlmcp_core, ?CAPTURE_ENABLED_KEY, true),
    ok.

%% @doc Disable reproducer capture globally
-spec disable_reproducer_capture() -> ok.
disable_reproducer_capture() ->
    application:set_env(erlmcp_core, ?CAPTURE_ENABLED_KEY, false),
    ok.

%% @doc Check if reproducer capture is enabled
-spec is_capture_enabled() -> boolean().
is_capture_enabled() ->
    case application:get_env(erlmcp_core, ?CAPTURE_ENABLED_KEY, false) of
        true ->
            true;
        _ ->
            false
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Get current failure context from process dictionary
-spec get_failure_context() -> term().
get_failure_context() ->
    case get(erlmcp_validator_failure_context) of
        undefined ->
            undefined;
        Context ->
            Context
    end.
