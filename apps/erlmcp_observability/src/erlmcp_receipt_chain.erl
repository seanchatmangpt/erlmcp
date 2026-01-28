%%% erlmcp_receipt_chain.erl
%%% Immutable event log and receipt chain management
-module(erlmcp_receipt_chain).

%% Public API
-export([
    add_event/1,
    get_events_by_type/1,
    get_event_by_id/1,
    get_all_events/0,
    restore_state/1
]).

-include("erlmcp.hrl").

%% ETS table for event storage
-define(EVENT_TABLE, erlmcp_receipt_chain_table).

%% === Initialization ===

%% Ensure ETS table exists
ensure_table() ->
    case ets:info(?EVENT_TABLE) of
        undefined ->
            ets:new(?EVENT_TABLE, [
                named_table,
                public,
                ordered_set,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        _ ->
            ok
    end.

%% === Public API ===

%% @doc Add event to receipt chain
-spec add_event(Event :: map()) -> ok.
add_event(Event) ->
    ensure_table(),
    EventId = erlang:system_time(microsecond),
    EventWithId = Event#{
        id => EventId,
        recorded_at => erlang:system_time(millisecond)
    },
    ets:insert(?EVENT_TABLE, {EventId, EventWithId}),
    ok.

%% @doc Get all events of a specific type
-spec get_events_by_type(Type :: atom()) -> {ok, [map()]} | {error, not_found}.
get_events_by_type(Type) ->
    ensure_table(),
    case ets:match_object(?EVENT_TABLE, {'_', #{type => Type, '_' => '_'}}) of
        [] ->
            {ok, []};
        Matches ->
            Events = [Event || {_, Event} <- Matches],
            {ok, lists:reverse(Events)}  %% Return in reverse chronological order
    end.

%% @doc Get specific event by ID
-spec get_event_by_id(EventId :: integer()) -> {ok, map()} | {error, not_found}.
get_event_by_id(EventId) ->
    ensure_table(),
    case ets:lookup(?EVENT_TABLE, EventId) of
        [{_, Event}] -> {ok, Event};
        [] -> {error, not_found}
    end.

%% @doc Get all events
-spec get_all_events() -> [map()].
get_all_events() ->
    ensure_table(),
    Entries = ets:tab2list(?EVENT_TABLE),
    Events = [Event || {_, Event} <- Entries],
    lists:reverse(Events).  %% Return in reverse chronological order

%% @doc Restore receipt chain state from snapshot
-spec restore_state(State :: map()) -> ok.
restore_state(State) ->
    ensure_table(),
    ets:delete_all_objects(?EVENT_TABLE),
    maps:fold(
        fun(EventId, Event, Acc) ->
            ets:insert(?EVENT_TABLE, {EventId, Event}),
            Acc
        end,
        ok,
        State
    ).
