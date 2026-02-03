%% @doc Enterprise Audit Logger
%% Centralized audit logging for compliance and security
-module(erlmcp_enterprise_audit).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([log_event/1, get_events/1, get_events_by_user/1, get_events_by_time/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    audit_log :: ets:tid(),  % Audit events storage
    retention :: integer(),  % Retention period in milliseconds
    index :: ets:tid()  % User index
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-spec log_event(Event :: map()) -> ok.
log_event(Event) ->
    gen_server:call(?MODULE, {log_event, Event}).

-spec get_events(Query :: map()) -> {ok, [map()]} | {error, term()}.
get_events(Query) ->
    gen_server:call(?MODULE, {get_events, Query}).

-spec get_events_by_user(User :: binary()) -> {ok, [map()]} | {error, term()}.
get_events_by_user(User) ->
    gen_server:call(?MODULE, {get_events_by_user, User}).

-spec get_events_by_time(Start :: integer(), End :: integer()) -> {ok, [map()]} | {error, term()}.
get_events_by_time(Start, End) ->
    gen_server:call(?MODULE, {get_events_by_time, Start, End}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),

    %% Initialize audit log
    AuditLog = ets:new(audit_log, [
        set,
        public,
        {write_concurrency, true},
        {keypos, 2},  % Event timestamp
        {named_table, true}
    ]),

    %% Initialize user index
    UserIndex = ets:new(user_index, [
        set,
        public,
        {write_concurrency, true},
        {named_table, true}
    ]),

    %% Set retention period (90 days for audit logs)
    Retention = 7776000000,  % 90 days in milliseconds

    State = #state{
        audit_log = AuditLog,
        retention = Retention,
        index = UserIndex
    },

    %% Start cleanup timer
    erlang:send_after(Retention, self(), cleanup_old_events),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call({log_event, Event}, _From, State) ->
    %% Validate event
    case validate_audit_event(Event) of
        valid ->
            %% Add timestamp
            TimestampedEvent = Event#{timestamp => erlang:system_time(millisecond)},
            LogId = erlang:unique_integer(),

            %% Store event
            ets:insert(State#state.audit_log, {LogId, TimestampedEvent}),

            %% Update user index
            UserId = maps:get(user, TimestampedEvent, <<"unknown">>),
            ets:insert(State#state.index, {UserId, LogId}),

            %% Notify compliance monitor
            erlmcp_enterprise_bus:publish(audit_event, TimestampedEvent),

            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_events, Query}, _From, State) ->
    %% Parse query
    case parse_query(Query) of
        {ok, ParsedQuery} ->
            Events = query_audit_log(State, ParsedQuery),
            {reply, {ok, Events}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_events_by_user, User}, _From, State) ->
    %% Get events for specific user
    case ets:lookup(State#state.index, User) of
        [{User, _LogId}] ->
            %% Get all events for this user
            Events = get_user_events(State, User),
            {reply, {ok, Events}, State};
        _ ->
            {reply, {ok, []}, State}
    end;

handle_call({get_events_by_time, Start, End}, _From, State) ->
    %% Get events within time range
    Events = get_time_range_events(State, Start, End),
    {reply, {ok, Events}, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info(cleanup_old_events, State) ->
    %% Clean up old audit events
    CurrentTime = erlang:system_time(millisecond),
    Cutoff = CurrentTime - State#state.retention,

    %% Find old events
    OldEvents = ets:foldl(fun({_LogId, Event}, Acc) ->
        case Event#{timestamp} of
            Timestamp when Timestamp =< Cutoff ->
                [Event | Acc];
            _ ->
                Acc
        end
    end, [], State#state.audit_log),

    %% Remove old events and index entries
    lists:foreach(fun(Event) ->
        LogId = get_event_log_id(Event),
        ets:delete(State#state.audit_log, LogId),

        UserId = maps:get(user, Event, <<"unknown">>),
        ets:delete_object(State#state.index, {UserId, LogId})
    end, OldEvents),

    %% Schedule next cleanup
    erlang:send_after(State#state.retention, self(), cleanup_old_events),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    %% Cleanup ETS tables
    ets:delete(State#state.audit_log),
    ets:delete(State#state.index).

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec validate_audit_event(map()) -> valid | {error, term()}.
validate_audit_event(Event) ->
    RequiredFields = [timestamp, user, action, resource, result],
    case lists:all(fun(Field) -> maps:is_key(Field, Event) end, RequiredFields) of
        true -> valid;
        false -> {error, missing_required_fields}
    end.

-spec parse_query(map()) -> {ok, map()} | {error, term()}.
parse_query(Query) ->
    case Query of
        #{user := User, action := Action} ->
            {ok, #{user => User, action => Action}};
        #{start_time := Start, end_time := End} ->
            {ok, #{time_range => {Start, End}}};
        #{event_type := Type} ->
            {ok, #{event_type => Type}};
        _ ->
            {error, invalid_query}
    end.

-spec query_audit_log(#state{}, map()) -> [map()].
query_audit_log(State, ParsedQuery) ->
    case ParsedQuery of
        #{user := User, action := Action} ->
            %% Get events for user and action
            Events = get_user_events(State, User),
            lists:filter(fun(Event) -> maps:get(action, Event) =:= Action end, Events);
        #{time_range := {Start, End}} ->
            %% Get events in time range
            get_time_range_events(State, Start, End);
        #{event_type := Type} ->
            %% Get events of specific type
            ets:foldl(fun({_LogId, Event}, Acc) ->
                case maps:get(event_type, Event, undefined) of
                    Type -> [Event | Acc];
                    _ -> Acc
                end
            end, [], State#state.audit_log);
        _ ->
            %% Return all events
            ets:foldl(fun({_LogId, Event}, Acc) -> [Event | Acc] end, [], State#state.audit_log)
    end.

-spec get_user_events(#state{}, binary()) -> [map()].
get_user_events(State, User) ->
    case ets:lookup(State#state.index, User) of
        [{User, _LogId}] ->
            %% Get all log IDs for this user
            LogIds = ets:foldl(fun({UserId, LogId}, Acc) ->
                case UserId of
                    User -> [LogId | Acc];
                    _ -> Acc
                end
            end, [], State#state.index),

            %% Get events for these log IDs
            lists:foldl(fun(LogId, Acc) ->
                case ets:lookup(State#state.audit_log, LogId) of
                    [{LogId, Event}] -> [Event | Acc];
                    _ -> Acc
                end
            end, [], LogIds);
        _ ->
            []
    end.

-spec get_time_range_events(#state{}, integer(), integer()) -> [map()].
get_time_range_events(State, Start, End) ->
    ets:foldl(fun({_LogId, Event}, Acc) ->
        case Event#{timestamp} of
            Timestamp when Timestamp >= Start, Timestamp =< End ->
                [Event | Acc];
            _ ->
                Acc
        end
    end, [], State#state.audit_log).

-spec get_event_log_id(map()) -> integer().
get_event_log_id(Event) ->
    %% In real implementation, this would be stored with the event
    %% For now, generate from timestamp
    erlang:unique_integer().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

audit_test() ->
    %% Test audit logging
    {ok, _Pid} = start_link(),

    %% Test event logging
    Event = #{
        timestamp => erlang:system_time(millisecond),
        user => "test_user",
        action => "login",
        resource => "/api/login",
        result => "success"
    },
    ok = log_event(Event),

    %% Test event retrieval
    {ok, Events} = get_events(#{}),
    ?assert(length(Events) > 0),

    %% Test user-based retrieval
    {ok, UserEvents} = get_events_by_user("test_user"),
    ?assert(length(UserEvents) > 0),

    %% Cleanup
    stop(),
    ok.

-endif.