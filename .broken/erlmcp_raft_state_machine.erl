%% @doc Raft State Machine for Distributed Configuration Store
%%
%% This module implements a distributed configuration store using the Raft
%% consensus algorithm. It provides:
%%   - Key-value configuration storage with strong consistency
%%   - Atomic read-modify-write operations
%%   - Configuration versioning for change tracking
%%   - Snapshot support for log compaction
%%
%% The state machine implements the erlmcp_raft state machine callback
%% behavior, allowing it to be used with the Raft consensus module.
-module(erlmcp_raft_state_machine).

-behaviour(gen_server).

-include("erlmcp_raft.hrl").
-include("erlmcp.hrl").

%% Callbacks for erlmcp_raft behavior
-export([init/1, apply/2, snapshot/1, restore/1, get_state/1]).

%% API for configuration operations
-export([start_link/0, start_link/1,
         get/1, set/2, delete/1,
         get_version/0, get_all/0,
         subscribe/1, unsubscribe/1,
         get_history/2]).

%% gen_server callbacks (for local standalone mode)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% State record
-type config_key() :: binary() | atom().
-type config_value() :: term().
-type config_version() :: pos_integer().

-record(config_entry,
        {key :: config_key(),
         value :: config_value(),
         version :: config_version(),
         updated_at :: integer(),
         updated_by :: raft_node_id() | undefined}).

-type config_entry() :: #config_entry{}.

-record(state_machine_state,
        {data :: #{config_key() => config_entry()},
         version :: config_version(),
         history :: [{config_version(), config_key(), term()}],  % Limited history
         subscribers :: sets:set(pid()),
         max_history :: pos_integer()}).

-type state() :: #state_machine_state{}.

%%%====================================================================
%%% erlmcp_raft State Machine Callbacks
%%%====================================================================

%% @doc Initialize the state machine
-spec init(Args :: term()) -> {ok, state()} | {error, term()}.
init(_Args) ->
   InitialState = #state_machine_state{
        data = #{},
        version = 0,
        history = [],
        subscribers = sets:new(),
        max_history = 1000
    },
    {ok, InitialState}.

%% @doc Apply a command to the state machine
-spec apply(Command :: term(), State :: state()) ->
    {ok, state(), term()} | {error, term()}.
apply({set, Key, Value}, State = #state_machine_state{data = Data, version = Version}) ->
    NewVersion = Version + 1,
    Entry = #config_entry{
        key = Key,
        value = Value,
        version = NewVersion,
        updated_at = erlang:monotonic_time(millisecond),
        updated_by = undefined  % Will be set by Raft node
    },
    NewData = maps:put(Key, Entry, Data),
    NewHistory = add_to_history(Key, NewVersion, State#state_machine_state.history,
                                 State#state_machine_state.max_history),
    NewState = State#state_machine_state{
        data = NewData,
        version = NewVersion,
        history = NewHistory
    },
    {ok, NewState, {ok, NewVersion}};

apply({set, Key, Value, Options}, State) ->
    %% Set with options (e.g., updated_by)
    case maps:get(updated_by, Options, undefined) of
        undefined ->
            apply({set, Key, Value}, State);
        UpdatedBy ->
            apply({set, Key, Value, UpdatedBy}, State)
    end;

apply({set, Key, Value, UpdatedBy}, State = #state_machine_state{data = Data, version = Version}) ->
    NewVersion = Version + 1,
    Entry = #config_entry{
        key = Key,
        value = Value,
        version = NewVersion,
        updated_at = erlang:monotonic_time(millisecond),
        updated_by = UpdatedBy
    },
    NewData = maps:put(Key, Entry, Data),
    NewHistory = add_to_history(Key, NewVersion, State#state_machine_state.history,
                                 State#state_machine_state.max_history),
    NewState = State#state_machine_state{
        data = NewData,
        version = NewVersion,
        history = NewHistory
    },
    {ok, NewState, {ok, NewVersion}};

apply({delete, Key}, State = #state_machine_state{data = Data, version = Version}) ->
    case maps:is_key(Key, Data) of
        true ->
            NewVersion = Version + 1,
            NewData = maps:remove(Key, Data),
            NewHistory = add_to_history(Key, NewVersion, State#state_machine_state.history,
                                         State#state_machine_state.max_history),
            NewState = State#state_machine_state{
                data = NewData,
                version = NewVersion,
                history = NewHistory
            },
            {ok, NewState, {ok, deleted}};
        false ->
            {ok, State, {error, not_found}}
    end;

apply({atomic, Operations}, State) when is_list(Operations) ->
    %% Apply a batch of operations atomically
    apply_atomic_batch(Operations, State);

apply({merge, Key, Value}, State = #state_machine_state{data = Data, version = Version}) ->
    %% Merge value with existing (for maps)
    NewVersion = Version + 1,
    ExistingValue = case maps:get(Key, Data, undefined) of
                       #config_entry{value = V} -> V;
                       undefined -> #{}
                   end,
    MergedValue = maps:merge(ExistingValue, Value),
    Entry = #config_entry{
        key = Key,
        value = MergedValue,
        version = NewVersion,
        updated_at = erlang:monotonic_time(millisecond),
        updated_by = undefined
    },
    NewData = maps:put(Key, Entry, Data),
    NewHistory = add_to_history(Key, NewVersion, State#state_machine_state.history,
                                 State#state_machine_state.max_history),
    NewState = State#state_machine_state{
        data = NewData,
        version = NewVersion,
        history = NewHistory
    },
    {ok, NewState, {ok, NewVersion}};

apply({config_change, ConfigChange}, State) ->
    %% Handle cluster membership changes
    {ok, State, {ok, config_change_applied}};

apply(Command, _State) ->
    {error, {unknown_command, Command}}.

%% @doc Create a snapshot of the current state
-spec snapshot(state()) -> {ok, binary()} | {error, term()}.
snapshot(State = #state_machine_state{data = Data, version = Version}) ->
    try
        SnapshotData = term_to_binary(State, [compressed]),
        {ok, SnapshotData}
    catch
        _:Reason ->
            {error, {snapshot_failed, Reason}}
    end.

%% @doc Restore state from a snapshot
-spec restore(binary()) -> {ok, state()} | {error, term()}.
restore(SnapshotData) ->
    try
        State = binary_to_term(SnapshotData),
        {ok, State}
    catch
        _:Reason ->
            {error, {restore_failed, Reason}}
    end.

%% @doc Get the current state as a map
-spec get_state(state()) -> map().
get_state(#state_machine_state{data = Data, version = Version}) ->
    #{config => Data, version => Version}.

%%%====================================================================
%%% API Functions - Standalone Mode
%%%====================================================================

%% @doc Start the configuration state machine
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(term()) -> {ok, pid()} | {error, term()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% @doc Get a configuration value
-spec get(config_key()) -> {ok, config_value()} | {error, not_found}.
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

%% @doc Set a configuration value
-spec set(config_key(), config_value()) -> {ok, config_version()} | {error, term()}.
set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).

%% @doc Delete a configuration value
-spec delete(config_key()) -> {ok, deleted} | {error, not_found}.
delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

%% @doc Get the current configuration version
-spec get_version() -> {ok, config_version()}.
get_version() ->
    gen_server:call(?MODULE, get_version).

%% @doc Get all configuration key-value pairs
-spec get_all() -> #{config_key() => config_value()}.
get_all() ->
    gen_server:call(?MODULE, get_all).

%% @doc Subscribe to configuration changes
-spec subscribe(pid()) -> ok.
subscribe(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {subscribe, Pid}).

%% @doc Unsubscribe from configuration changes
-spec unsubscribe(pid()) -> ok.
unsubscribe(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {unsubscribe, Pid}).

%% @doc Get change history
-spec get_history(pos_integer(), pos_integer()) -> [{config_version(), config_key(), integer()}].
get_history(Limit, Offset) ->
    gen_server:call(?MODULE, {get_history, Limit, Offset}).

%%%====================================================================
%%% gen_server Callbacks (Standalone Mode)
%%%====================================================================

-spec init(term()) -> {ok, state()}.
init(Args) ->
    {ok, InitialState} = init(Args),
    logger:info("Configuration state machine started"),
    {ok, InitialState}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.
handle_call({get, Key}, _From, State = #state_machine_state{data = Data}) ->
    case maps:get(Key, Data, undefined) of
        #config_entry{value = Value} ->
            {reply, {ok, Value}, State};
        undefined ->
            {reply, {error, not_found}, State}
    end;

handle_call({set, Key, Value}, _From, State) ->
    case apply({set, Key, Value}, State) of
        {ok, NewState, Result} ->
            notify_subscribers(Key, Value, State#state_machine_state.subscribers),
            {reply, Result, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({delete, Key}, _From, State) ->
    case apply({delete, Key}, State) of
        {ok, NewState, Result} ->
            notify_subscribers(Key, deleted, State#state_machine_state.subscribers),
            {reply, Result, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_version, _From, State = #state_machine_state{version = Version}) ->
    {reply, {ok, Version}, State};

handle_call(get_all, _From, State = #state_machine_state{data = Data}) ->
    %% Extract values from config entries
    ConfigMap = maps:map(fun(_K, #config_entry{value = V}) -> V end, Data),
    {reply, ConfigMap, State};

handle_call({subscribe, Pid}, _From, State = #state_machine_state{subscribers = Subs}) ->
    NewSubs = sets:add_element(Pid, Subs),
    erlang:monitor(process, Pid),
    {reply, ok, State#state_machine_state{subscribers = NewSubs}};

handle_call({unsubscribe, Pid}, _From, State = #state_machine_state{subscribers = Subs}) ->
    NewSubs = sets:del_element(Pid, Subs),
    erlang:demonitor(Pid),
    {reply, ok, State#state_machine_state{subscribers = NewSubs}};

handle_call({get_history, Limit, Offset}, _From, State = #state_machine_state{history = History}) ->
    %% Apply offset and limit
    Start = max(1, Offset + 1),
    Selected = lists:sublist(lists:reverse(History), Start, Limit),
    {reply, Selected, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State = #state_machine_state{subscribers = Subs}) ->
    %% Remove dead subscriber
    NewSubs = sets:del_element(Pid, Subs),
    {noreply, State#state_machine_state{subscribers = NewSubs}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    logger:info("Configuration state machine terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Apply a batch of operations atomically
-spec apply_atomic_batch([term()], state()) -> {ok, state(), term()} | {error, term()}.
apply_atomic_batch([], State) ->
    {ok, State, {ok, batch_applied}};
apply_atomic_batch([Op | Rest], State) ->
    case apply(Op, State) of
        {ok, NewState, _Result} ->
            apply_atomic_batch(Rest, NewState);
        {error, Reason} ->
            {error, {batch_failed, Reason, Op}}
    end.

%% @doc Add entry to history with size limit
-spec add_to_history(config_key(), config_version(), [{config_version(), config_key(), term()}],
                     pos_integer()) -> [{config_version(), config_key(), term()}].
add_to_history(Key, Version, History, MaxHistory) ->
    Entry = {Version, Key, erlang:monotonic_time(millisecond)},
    NewHistory = [Entry | History],
    case length(NewHistory) > MaxHistory of
        true ->
            lists:sublist(NewHistory, MaxHistory);
        false ->
            NewHistory
    end.

%% @doc Notify subscribers of configuration changes
-spec notify_subscribers(config_key(), config_value() | deleted, sets:set(pid()) -> ok.
notify_subscribers(Key, Value, Subscribers) ->
    Message = {config_change, Key, Value},
    sets:fold(fun(Pid, _Acc) ->
                      case is_process_alive(Pid) of
                          true ->
                              Pid ! Message;
                          false ->
                              ok
                      end,
                      ok
              end, ok, Subscribers).
