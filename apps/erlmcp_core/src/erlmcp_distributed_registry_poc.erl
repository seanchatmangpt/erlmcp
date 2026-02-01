%% @doc Distributed Registry POC
%%
%% Proof-of-concept distributed registry with name registration,
%% lookup, and automatic cleanup on process death.
%%
%% API:
%% - start_link/0: Start the registry
%% - register_name/2: Register a name for a process
%% - whereis_name/2: Lookup a process by name
%% - unregister_name/2: Unregister a name
%% - get_all_names/1: Get all registered names
%% - stop/1: Stop the registry
-module(erlmcp_distributed_registry_poc).

-behaviour(gen_server).

%% API
-export([start_link/0, register_name/3, whereis_name/2, unregister_name/2, get_all_names/1,
         stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,
        {registry :: map(),  %% Name -> Pid
         monitors :: map()}).   %% Pid -> Name (for cleanup)

%%====================================================================
%% API
%%====================================================================

%% @doc Start the registry
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Register a name for a process
-spec register_name(pid(), term(), pid()) -> ok | {error, already_registered}.
register_name(RegistryPid, Name, ProcessPid) ->
    gen_server:call(RegistryPid, {register_name, Name, ProcessPid}).

%% @doc Lookup a process by name
-spec whereis_name(pid(), term()) -> {ok, pid()} | {error, not_found}.
whereis_name(RegistryPid, Name) ->
    gen_server:call(RegistryPid, {whereis_name, Name}).

%% @doc Unregister a name
-spec unregister_name(pid(), term()) -> ok.
unregister_name(RegistryPid, Name) ->
    gen_server:call(RegistryPid, {unregister_name, Name}).

%% @doc Get all registered names
-spec get_all_names(pid()) -> {ok, [term()]}.
get_all_names(RegistryPid) ->
    gen_server:call(RegistryPid, get_all_names).

%% @doc Stop the registry
-spec stop(pid()) -> ok.
stop(RegistryPid) ->
    gen_server:stop(RegistryPid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{registry = #{}, monitors = #{}}}.

handle_call({register_name, Name, ProcessPid},
            _From,
            State = #state{registry = Registry, monitors = Monitors}) ->
    case maps:is_key(Name, Registry) of
        true ->
            {reply, {error, already_registered}, State};
        false ->
            %% Monitor the process
            MonitorRef = monitor(process, ProcessPid),
            NewRegistry = maps:put(Name, ProcessPid, Registry),
            NewMonitors = maps:put(ProcessPid, {Name, MonitorRef}, Monitors),
            {reply, ok, State#state{registry = NewRegistry, monitors = NewMonitors}}
    end;
handle_call({whereis_name, Name}, _From, State = #state{registry = Registry}) ->
    case maps:get(Name, Registry, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Pid ->
            {reply, {ok, Pid}, State}
    end;
handle_call({unregister_name, Name},
            _From,
            State = #state{registry = Registry, monitors = Monitors}) ->
    case maps:get(Name, Registry, undefined) of
        undefined ->
            {reply, ok, State};
        Pid ->
            %% Demonitor the process
            case maps:get(Pid, Monitors, undefined) of
                {_, MonitorRef} ->
                    demonitor(MonitorRef, [flush]);
                undefined ->
                    ok
            end,
            NewRegistry = maps:remove(Name, Registry),
            NewMonitors = maps:remove(Pid, Monitors),
            {reply, ok, State#state{registry = NewRegistry, monitors = NewMonitors}}
    end;
handle_call(get_all_names, _From, State = #state{registry = Registry}) ->
    Names = maps:keys(Registry),
    {reply, {ok, Names}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, _Reason},
            State = #state{registry = Registry, monitors = Monitors}) ->
    %% Process died - cleanup
    case maps:get(Pid, Monitors, undefined) of
        {Name, MonitorRef} ->
            NewRegistry = maps:remove(Name, Registry),
            NewMonitors = maps:remove(Pid, Monitors),
            {noreply, State#state{registry = NewRegistry, monitors = NewMonitors}};
        undefined ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
