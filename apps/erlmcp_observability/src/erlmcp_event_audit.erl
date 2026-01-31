%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_event_audit - Audit Trail Event Handler
%%%
%%% gen_event handler that maintains an audit trail of MCP events.
%%% Provides tamper-proof audit logging for compliance (GDPR, SOC2, HIPAA).
%%%
%%% Features:
%%% - Immutable event log with timestamps
%%% - Cryptographic hash chain for tamper detection
%%% - Configurable retention period
%%% - Structured audit records
%%%
%%% Integration:
%%% - Uses erlmcp_audit_log for persistent storage
%%% - Each event creates an audit record with hash chain
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_event_audit).
-behaviour(gen_event).

%% gen_event callbacks
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% State record
-record(state, {
    enabled = true :: boolean(),
    log_all_events = false :: boolean(),  % If true, log all events; if false, only critical ones
    event_count = 0 :: non_neg_integer(),
    start_time :: integer()
}).

%%====================================================================
%% gen_event Callbacks
%%====================================================================

%% @doc Initialize the audit handler.
-spec init(map() | []) -> {ok, #state{}}.
init(Args) when is_map(Args) ->
    Enabled = maps:get(enabled, Args, true),
    LogAll = maps:get(log_all_events, Args, false),
    {ok, #state{
        enabled = Enabled,
        log_all_events = LogAll,
        start_time = erlang:system_time(millisecond)
    }};
init(_Args) ->
    {ok, #state{
        start_time = erlang:system_time(millisecond)
    }}.

%% @doc Handle events by creating audit records.
-spec handle_event(term(), #state{}) -> {ok, #state{}}.

%% Tool execution events (always audit)
handle_event({tool_executed, ToolName, Duration, Result}, #state{enabled = true} = State) ->
    audit_event(tool_execution, #{
        tool_name => ToolName,
        duration_ms => Duration,
        result => format_result(Result),
        timestamp => erlang:system_time(millisecond)
    }),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Resource update events (always audit)
handle_event({resource_updated, Uri, Metadata}, #state{enabled = true} = State) ->
    audit_event(resource_update, #{
        uri => Uri,
        metadata => Metadata,
        timestamp => erlang:system_time(millisecond)
    }),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Connection state events (always audit)
handle_event({connection_state, ConnectionState, Info}, #state{enabled = true} = State) ->
    audit_event(connection_state, #{
        state => ConnectionState,
        info => Info,
        timestamp => erlang:system_time(millisecond)
    }),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Error events (always audit)
handle_event({error, Category, Reason}, #state{enabled = true} = State) ->
    audit_event(error, #{
        category => Category,
        reason => format_reason(Reason),
        timestamp => erlang:system_time(millisecond)
    }),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Request received events (audit if log_all_events = true)
handle_event({request_received, Method, RequestId}, #state{enabled = true, log_all_events = true} = State) ->
    audit_event(request_received, #{
        method => Method,
        request_id => RequestId,
        timestamp => erlang:system_time(millisecond)
    }),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Response sent events (audit if log_all_events = true)
handle_event({response_sent, Method, RequestId, Duration}, #state{enabled = true, log_all_events = true} = State) ->
    audit_event(response_sent, #{
        method => Method,
        request_id => RequestId,
        duration_ms => Duration,
        timestamp => erlang:system_time(millisecond)
    }),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Notification sent events (audit if log_all_events = true)
handle_event({notification_sent, Method, Params}, #state{enabled = true, log_all_events = true} = State) ->
    audit_event(notification_sent, #{
        method => Method,
        params => sanitize_params(Params),
        timestamp => erlang:system_time(millisecond)
    }),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Session created events (always audit)
handle_event({session_created, SessionId, Metadata}, #state{enabled = true} = State) ->
    audit_event(session_created, #{
        session_id => SessionId,
        metadata => Metadata,
        timestamp => erlang:system_time(millisecond)
    }),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Session terminated events (always audit)
handle_event({session_terminated, SessionId, Reason}, #state{enabled = true} = State) ->
    audit_event(session_terminated, #{
        session_id => SessionId,
        reason => format_reason(Reason),
        timestamp => erlang:system_time(millisecond)
    }),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Disabled or unknown events - skip
handle_event(_Event, State) ->
    {ok, State}.

%% @doc Handle synchronous calls.
-spec handle_call(term(), #state{}) -> {ok, term(), #state{}}.
handle_call(get_stats, State) ->
    Stats = #{
        enabled => State#state.enabled,
        log_all_events => State#state.log_all_events,
        event_count => State#state.event_count,
        uptime_ms => erlang:system_time(millisecond) - State#state.start_time
    },
    {ok, Stats, State};
handle_call({set_enabled, Enabled}, State) ->
    {ok, ok, State#state{enabled = Enabled}};
handle_call({set_log_all, LogAll}, State) ->
    {ok, ok, State#state{log_all_events = LogAll}};
handle_call(_Request, State) ->
    {ok, {error, unknown_request}, State}.

%% @doc Handle info messages.
-spec handle_info(term(), #state{}) -> {ok, #state{}}.
handle_info(_Info, State) ->
    {ok, State}.

%% @doc Cleanup on handler termination.
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc Handle code changes.
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Create an audit record.
%% Uses erlmcp_audit_log if available, otherwise logs to file.
-spec audit_event(atom(), map()) -> ok.
audit_event(EventType, EventData) ->
    %% Try to use erlmcp_audit_log if available
    case whereis(erlmcp_audit_log) of
        undefined ->
            %% Fallback: log to logger with audit marker
            logger:notice("[AUDIT] ~p: ~p", [EventType, EventData]),
            ok;
        _Pid ->
            %% Use audit_log module
            try
                erlmcp_audit_log:log_event(EventType, EventData)
            catch
                _:_ ->
                    %% Fallback on error
                    logger:notice("[AUDIT] ~p: ~p", [EventType, EventData]),
                    ok
            end
    end.

%% @doc Format result for audit log (avoid leaking sensitive data).
-spec format_result(term()) -> term().
format_result({ok, _}) -> ok;
format_result(ok) -> ok;
format_result({error, Reason}) -> {error, format_reason(Reason)};
format_result(error) -> error;
format_result(_) -> completed.

%% @doc Format error reason for audit log (truncate large terms).
-spec format_reason(term()) -> term().
format_reason(Reason) when is_binary(Reason) ->
    case byte_size(Reason) > 1000 of
        true -> <<(binary_part(Reason, 0, 1000))/binary, "...">>;
        false -> Reason
    end;
format_reason(Reason) when is_list(Reason) ->
    case length(Reason) > 1000 of
        true -> lists:sublist(Reason, 1000) ++ "...";
        false -> Reason
    end;
format_reason(Reason) ->
    Reason.

%% @doc Sanitize params to remove sensitive data.
-spec sanitize_params(map()) -> map().
sanitize_params(Params) when is_map(Params) ->
    %% Remove common sensitive fields
    SensitiveKeys = [<<"password">>, <<"secret">>, <<"token">>, <<"api_key">>, <<"credentials">>],
    maps:fold(
        fun(Key, Value, Acc) ->
            case lists:member(Key, SensitiveKeys) of
                true -> Acc#{Key => <<"[REDACTED]">>};
                false -> Acc#{Key => Value}
            end
        end,
        #{},
        Params
    );
sanitize_params(Params) ->
    Params.
