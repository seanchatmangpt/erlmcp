%%%-------------------------------------------------------------------
%%% @doc erlmcp-flow Agent Registry (gproc-based)
%%% O(log N) lookup for agent discovery
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_registry).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    register_agent/3,
    unregister_agent/1,
    find_agent/1,
    find_agents_by_capability/1,
    query_agents/1,
    get_agent_load/1,
    increment_load/1,
    decrement_load/1,
    update_heartbeat/1,
    get_last_heartbeat/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    heartbeats = #{} :: #{binary() => erlang:timestamp()}
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_agent(AgentId, Pid, Capabilities) -> ok | {error, term()}
    when AgentId :: binary(),
         Pid :: pid(),
         Capabilities :: [binary()].
register_agent(AgentId, Pid, Capabilities) ->
    gen_server:call(?MODULE, {register_agent, AgentId, Pid, Capabilities}).

-spec unregister_agent(AgentId) -> ok
    when AgentId :: binary().
unregister_agent(AgentId) ->
    gen_server:call(?MODULE, {unregister_agent, AgentId}).

-spec find_agent(AgentId) -> {ok, pid()} | {error, not_found}
    when AgentId :: binary().
find_agent(AgentId) ->
    Key = {n, l, {flow_agent, AgentId}},
    case gproc:where(Key) of
        undefined -> {error, not_found};
        Pid -> {ok, Pid}
    end.

-spec find_agents_by_capability(Capability) -> [pid()]
    when Capability :: binary().
find_agents_by_capability(Capability) ->
    gproc:lookup_pids({p, l, {flow_capability, Capability}}).

-spec query_agents(Capabilities :: [binary()]) -> [binary()].
query_agents(Capabilities) ->
    % Find agents that have ALL required capabilities (AND logic)
    AgentSets = [sets:from_list([get_agent_id(Pid) ||
                                 Pid <- find_agents_by_capability(Cap)])
                 || Cap <- Capabilities],

    case AgentSets of
        [] -> [];
        [First | Rest] ->
            FinalSet = lists:foldl(fun sets:intersection/2, First, Rest),
            sets:to_list(FinalSet)
    end.

-spec get_agent_load(AgentId) -> non_neg_integer()
    when AgentId :: binary().
get_agent_load(AgentId) ->
    CounterKey = {c, l, {flow_load, AgentId}},
    case gproc:lookup_value(CounterKey) of
        undefined -> 0;
        Load -> Load
    end.

-spec increment_load(AgentId) -> non_neg_integer()
    when AgentId :: binary().
increment_load(AgentId) ->
    CounterKey = {c, l, {flow_load, AgentId}},
    gproc:update_counter(CounterKey, 1).

-spec decrement_load(AgentId) -> non_neg_integer()
    when AgentId :: binary().
decrement_load(AgentId) ->
    CounterKey = {c, l, {flow_load, AgentId}},
    gproc:update_counter(CounterKey, -1).

-spec update_heartbeat(AgentId) -> ok
    when AgentId :: binary().
update_heartbeat(AgentId) ->
    gen_server:cast(?MODULE, {update_heartbeat, AgentId}).

-spec get_last_heartbeat(AgentId) -> {ok, erlang:timestamp()} | {error, not_found}
    when AgentId :: binary().
get_last_heartbeat(AgentId) ->
    gen_server:call(?MODULE, {get_last_heartbeat, AgentId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("erlmcp-flow registry starting"),
    {ok, #state{}}.

handle_call({register_agent, AgentId, Pid, Capabilities}, _From, State) ->
    Key = {n, l, {flow_agent, AgentId}},

    case gproc:where(Key) of
        undefined ->
            try
                % Register name
                gproc:reg_other(Key, Pid, #{
                    capabilities => Capabilities,
                    registered_at => erlang:timestamp()
                }),
                gproc:monitor(Key),

                % Index by capabilities
                lists:foreach(fun(Cap) ->
                    PropKey = {p, l, {flow_capability, Cap}},
                    gproc:reg_other(PropKey, Pid, #{agent_id => AgentId})
                end, Capabilities),

                % Load counter
                CounterKey = {c, l, {flow_load, AgentId}},
                gproc:reg_other(CounterKey, Pid, 0),

                % Update heartbeat
                NewHeartbeats = maps:put(AgentId, erlang:timestamp(),
                                        State#state.heartbeats),

                ?LOG_INFO("Registered agent ~p with capabilities ~p",
                         [AgentId, Capabilities]),
                {reply, ok, State#state{heartbeats = NewHeartbeats}}
            catch
                error:badarg ->
                    {reply, {error, already_registered}, State}
            end;
        ExistingPid when ExistingPid =:= Pid ->
            {reply, ok, State};
        _ ->
            {reply, {error, already_registered}, State}
    end;

handle_call({unregister_agent, AgentId}, _From, State) ->
    Key = {n, l, {flow_agent, AgentId}},
    case gproc:where(Key) of
        undefined ->
            {reply, ok, State};
        Pid ->
            try gproc:unreg_other(Key, Pid)
            catch error:badarg -> ok
            end,
            NewHeartbeats = maps:remove(AgentId, State#state.heartbeats),
            {reply, ok, State#state{heartbeats = NewHeartbeats}}
    end;

handle_call({get_last_heartbeat, AgentId}, _From, State) ->
    case maps:get(AgentId, State#state.heartbeats, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Timestamp -> {reply, {ok, Timestamp}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({update_heartbeat, AgentId}, State) ->
    NewHeartbeats = maps:put(AgentId, erlang:timestamp(),
                            State#state.heartbeats),
    {noreply, State#state{heartbeats = NewHeartbeats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gproc, unreg, _Ref, {n, l, {flow_agent, AgentId}}}, State) ->
    ?LOG_WARNING("Agent ~p unregistered (process died)", [AgentId]),
    NewHeartbeats = maps:remove(AgentId, State#state.heartbeats),
    {noreply, State#state{heartbeats = NewHeartbeats}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_agent_id(Pid) ->
    % Get agent ID from gproc registration
    case gproc:select([{{{n, l, {flow_agent, '$1'}}, Pid, '_'}, [], ['$1']}]) of
        [AgentId] -> AgentId;
        _ -> undefined
    end.
