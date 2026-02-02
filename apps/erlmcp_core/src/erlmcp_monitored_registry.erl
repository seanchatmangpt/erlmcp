-module(erlmcp_monitored_registry).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([register/3, unregister/1, lookup/1, list/0]).
-export([spawn_monitored/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type key() :: term().
-type value() :: term().
-type tag() :: term().
-type entry() :: {key(), value(), pid(), reference()}.

-record(state,
        {registry :: #{key() => {value(), pid(), reference()}},
         tag_index :: #{tag() => [key()]}}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Register a process with a tagged monitor.
%% The tag is embedded in the DOWN message for automatic identification.
-spec register(key(), value(), tag()) -> {ok, reference()} | {error, term()}.
register(Key, Value, Tag) ->
    gen_server:call(?MODULE, {register, Key, Value, Tag}).

%% @doc Unregister a monitored process.
-spec unregister(key()) -> ok | {error, not_found}.
unregister(Key) ->
    gen_server:call(?MODULE, {unregister, Key}).

%% @doc Lookup a value by key.
-spec lookup(key()) -> {ok, value(), pid()} | {error, not_found}.
lookup(Key) ->
    gen_server:call(?MODULE, {lookup, Key}).

%% @doc List all registered keys.
-spec list() -> {ok, [key()]}.
list() ->
    gen_server:call(?MODULE, list).

%% @doc Spawn a process with a tagged monitor.
%% The DOWN message will contain the tag for easy identification.
-spec spawn_monitored(fun(), tag(), key()) -> {ok, pid(), reference()} | {error, term()}.
spawn_monitored(Fun, Tag, Key) ->
    gen_server:call(?MODULE, {spawn_monitored, Fun, Tag, Key}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(_Opts) ->
    {ok, #state{registry = #{}, tag_index = #{}}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {reply, term(), #state{}, hibernate}.
handle_call({register, Key, Value, Tag}, {Pid, _Ref}, State) ->
    case maps:is_key(Key, State#state.registry) of
        true ->
            {reply, {error, already_exists}, State};
        false ->
            %% OTP 27/28: Tag monitor - tag embedded in DOWN message
            MonRef = erlang:monitor(process, Pid, [{tag, Tag}]),
            NewRegistry = maps:put(Key, {Value, Pid, MonRef}, State#state.registry),
            NewTagIndex = maps:put(Tag, [Key | maps:get(Tag, State#state.tag_index, [])], State#state.tag_index),
            {reply, {ok, MonRef}, State#state{registry = NewRegistry, tag_index = NewTagIndex}, hibernate}
    end;

handle_call({spawn_monitored, Fun, Tag, Key}, _From, State) ->
    case maps:is_key(Key, State#state.registry) of
        true ->
            {reply, {error, already_exists}, State};
        false ->
            try
                %% Spawn process with initial monitor
                {Pid, InitialRef} = spawn_monitor(Fun),

                %% Replace with tagged monitor
                TaggedRef = erlang:monitor(process, Pid, [{tag, Tag}]),
                erlang:demonitor(InitialRef, [flush]),

                NewRegistry = maps:put(Key, {Pid, Pid, TaggedRef}, State#state.registry),
                NewTagIndex = maps:put(Tag, [Key | maps:get(Tag, State#state.tag_index, [])], State#state.tag_index),

                {reply, {ok, Pid, TaggedRef}, State#state{registry = NewRegistry, tag_index = NewTagIndex}, hibernate}
            catch
                _:Reason ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({unregister, Key}, _From, State) ->
    case maps:get(Key, State#state.registry, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        {_Value, _Pid, MonRef} ->
            erlang:demonitor(MonRef, [flush]),
            NewRegistry = maps:remove(Key, State#state.registry),
            %% Remove from tag index (requires filtering)
            NewTagIndex = remove_from_tag_index(Key, State#state.tag_index),
            {reply, ok, State#state{registry = NewRegistry, tag_index = NewTagIndex}, hibernate}
    end;

handle_call({lookup, Key}, _From, State) ->
    case maps:get(Key, State#state.registry, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        {Value, Pid, _MonRef} ->
            {reply, {ok, Value, Pid}, State, hibernate}
    end;

handle_call(list, _From, State) ->
    Keys = maps:keys(State#state.registry),
    {reply, {ok, Keys}, State, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}} | {noreply, #state{}, hibernate}.
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {noreply, #state{}, hibernate}.
%% OTP 27/28: DOWN message with tag embedded
%% Tag is in the first element of the reference
handle_info({'DOWN', Tag, process, Pid, Reason}, State) ->
    logger:warning("Monitored process died - Tag: ~p, Pid: ~p, Reason: ~p", [Tag, Pid, Reason]),

    %% Find all keys with this tag
    case maps:get(Tag, State#state.tag_index, []) of
        [] ->
            %% No keys found - shouldn't happen
            {noreply, State, hibernate};
        Keys ->
            %% Remove all keys with this tag
            NewRegistry = lists:foldl(
                fun(Key, Acc) ->
                    case maps:get(Key, Acc, undefined) of
                        {_Value, Pid, _MonRef} ->
                            logger:info("Removing monitored entry: ~p", [Key]),
                            maps:remove(Key, Acc);
                        _ ->
                            Acc
                    end
                end,
                State#state.registry,
                Keys
            ),

            %% Remove tag from index
            NewTagIndex = maps:remove(Tag, State#state.tag_index),

            {noreply, State#state{registry = NewRegistry, tag_index = NewTagIndex}, hibernate}
    end;

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Remove key from tag index.
%% @private
-spec remove_from_tag_index(key(), #{tag() => [key()]}) -> #{tag() => [key()]}.
remove_from_tag_index(Key, TagIndex) ->
    maps:map(
        fun(_Tag, Keys) ->
            lists:filter(fun(K) -> K =/= Key end, Keys)
        end,
        TagIndex
    ).
