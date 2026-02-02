-module(erlmcp_otp28_supervisor_enhancements).
-moduledoc """
OTP 28.3.1 Supervisor Enhancements

This module provides OTP 28-specific supervisor improvements:
- Enhanced child restart tracking
- Optimized process monitoring
- Auto-hibernation management
- Dynamic child lifecycle management
""" .

-export([
    %% Child management
    restart_child_with_tracking/2,
    bulk_restart_children/2,
    %% Process monitoring
    monitor_child_processes/1,
    get_child_process_info/2,
    %% Hibernation
    configure_hibernation/2,
    get_hibernation_config/1,
    %% Health monitoring
    supervisor_detailed_health/1,
    child_restart_history/0,
    %% Statistics
    get_restart_statistics/0,
    get_performance_metrics/1
]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

-type supervisor_ref() :: pid() | atom().
-type child_id() :: term().
-type restart_info() :: #{timestamp => erlang:timestamp(),
                         reason => term()}.
-type performance_metrics() :: #{total_children => non_neg_integer(),
                                 active_children => non_neg_integer(),
                                 restarting_children => non_neg_integer(),
                                 avg_memory => non_neg_integer(),
                                 avg_reductions => non_neg_integer()}.

-record(state, {
    restart_history :: #{child_id() => [restart_info()]},
    performance_stats :: #{supervisor_ref() => performance_metrics()},
    monitored_children :: #{pid() => child_id()}
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Restart child with enhanced tracking (OTP 28)
-spec restart_child_with_tracking(child_id(), supervisor_ref()) ->
    {ok, pid()} | {error, term()}.
restart_child_with_tracking(ChildId, Supervisor) ->
    try
        %% Get current PID before restart
        Children = supervisor:which_children(Supervisor),
        OldPid = case lists:keyfind(ChildId, 1, Children) of
            {ChildId, Pid, _, _} when is_pid(Pid) -> Pid;
            _ -> undefined
        end,

        %% Perform restart
        Result = case supervisor:terminate_child(Supervisor, ChildId) of
            ok ->
                case supervisor:restart_child(Supervisor, ChildId) of
                    {ok, Pid} = OK ->
                        %% Track restart
                        gen_server:cast(?MODULE, {track_restart, ChildId, OldPid, Pid}),
                        OK;
                    {error, already_present} ->
                        %% Already restarted
                        {ok, self()};
                    Error ->
                        Error
                end;
            {error, not_found} ->
                supervisor:restart_child(Supervisor, ChildId)
        end,

        Result
    catch
        _:Error ->
            ?LOG_ERROR("Failed to restart child ~p: ~p", [ChildId, Error]),
            {error, Error}
    end.

%% @doc Restart multiple children in bulk
-spec bulk_restart_children([child_id()], supervisor_ref()) ->
    {ok, [{child_id(), {ok, pid()} | {error, term()}}]}.
bulk_restart_children(ChildIds, Supervisor) when is_list(ChildIds) ->
    Results = [{ChildId, restart_child_with_tracking(ChildId, Supervisor)}
               || ChildId <- ChildIds],
    {ok, Results}.

%% @doc Monitor all child processes of a supervisor
-spec monitor_child_processes(supervisor_ref()) -> {ok, [reference()]}.
monitor_child_processes(Supervisor) ->
    try
        Children = supervisor:which_children(Supervisor),
        Refs = [monitor_child(Pid, Id) || {Id, Pid, _, _} <- Children, is_pid(Pid)],
        {ok, Refs}
    catch
        _:Error ->
            ?LOG_ERROR("Failed to monitor children: ~p", [Error]),
            {error, Error}
    end.

%% @doc Get detailed process info for a child
-spec get_child_process_info(supervisor_ref(), child_id()) ->
    {ok, map()} | {error, term()}.
get_child_process_info(Supervisor, ChildId) ->
    try
        Children = supervisor:which_children(Supervisor),
        case lists:keyfind(ChildId, 1, Children) of
            {ChildId, Pid, Type, Modules} when is_pid(Pid) ->
                Info = case is_process_alive(Pid) of
                    true ->
                        %% OTP 28: Use optimized process_info/2
                        process_info(Pid, [
                            message_queue_len,
                            memory,
                            heap_size,
                            total_heap_size,
                            stack_size,
                            reductions,
                            current_function,
                            status
                        ]);
                    false ->
                        [{status, dead}]
                end,
                {ok, #{
                    id => ChildId,
                    pid => Pid,
                    type => Type,
                    modules => ensure_list(Modules),
                    info => maps:from_list(Info)
                }};
            false ->
                {error, not_found}
        end
    catch
        _:Error ->
            {error, Error}
    end.

%% @doc Configure hibernation for supervisor (OTP 28)
-spec configure_hibernation(supervisor_ref(), pos_integer() | disabled) ->
    ok | {error, term()}.
configure_hibernation(_Supervisor, AfterMs) when is_integer(AfterMs), AfterMs > 0 ->
    %% OTP 28: Hibernation is configured in supervisor flags
    %% This is a placeholder for future configuration API
    ?LOG_INFO("Hibernation configuration requested: ~pms", [AfterMs]),
    ok;
configure_hibernation(_Supervisor, disabled) ->
    ok.

%% @doc Get hibernation configuration
-spec get_hibernation_config(supervisor_ref()) ->
    #{enabled => boolean(), after_ms => non_neg_integer() | undefined}.
get_hibernation_config(Supervisor) ->
    try
        %% OTP 28: Read supervisor flags
        case erlang:function_exported(supervisor, get_supervisor_flags, 1) of
            true ->
                Flags = supervisor:get_supervisor_flags(Supervisor),
                #{
                    enabled => maps:get(auto_hibernation, Flags, false) =/= false,
                    after_ms => case maps:get(auto_hibernation, Flags, undefined) of
                        Module when is_atom(Module) ->
                            try Module:hibernate_after() of
                                Ms -> Ms
                            catch
                                _:_ -> undefined
                            end;
                        _ -> undefined
                    end
                };
            false ->
                #{enabled => false, after_ms => undefined}
        end
    catch
        _:_ ->
            #{enabled => false, after_ms => undefined}
    end.

%% @doc Get detailed health information
-spec supervisor_detailed_health(supervisor_ref()) ->
    #{health => healthy | degraded | critical,
      total_children => non_neg_integer(),
      active_children => non_neg_integer(),
      restarting_children => non_neg_integer(),
      dead_children => non_neg_integer(),
      avg_memory => non_neg_integer(),
      avg_reductions => non_neg_integer()}.
supervisor_detailed_health(Supervisor) ->
    try
        Children = supervisor:which_children(Supervisor),
        Total = length(Children),

        {Active, Restarting, Dead, TotalMem, TotalRed} =
        lists:foldl(fun({_Id, Pid, _Type, _Modules}, {Act, Rst, Dead, Mem, Red}) ->
            case Pid of
                undefined -> {Act, Rst, Dead + 1, Mem, Red};
                Pid when is_pid(Pid) ->
                    case is_process_alive(Pid) of
                        true ->
                            Info = process_info(Pid, [memory, reductions]),
                            Mem = proplists:get_value(memory, Info, 0),
                            Red = proplists:get_value(reductions, Info, 0),
                            {Act + 1, Rst, Dead, Mem + Mem, Red + Red};
                        false ->
                            {Act, Rst + 1, Dead, Mem, Red}
                    end
            end
        end, {0, 0, 0, 0, 0}, Children),

        AvgMem = case Active of
            0 -> 0;
            _ -> TotalMem div Active
        end,
        AvgRed = case Active of
            0 -> 0;
            _ -> TotalRed div Active
        end,

        Health = case Dead of
            0 -> healthy;
            _ when Dead < Total div 4 -> degraded;
            _ -> critical
        end,

        #{
            health => Health,
            total_children => Total,
            active_children => Active,
            restarting_children => Restaring,
            dead_children => Dead,
            avg_memory => AvgMem,
            avg_reductions => AvgRed
        }
    catch
        _:Error ->
            ?LOG_ERROR("Failed to get detailed health: ~p", [Error]),
            #{health => critical, total_children => 0, active_children => 0,
              restarting_children => 0, dead_children => 0, avg_memory => 0,
              avg_reductions => 0}
    end.

%% @doc Get restart history for all children
-spec child_restart_history() -> #{child_id() => [restart_info()]}.
child_restart_history() ->
    gen_server:call(?MODULE, get_restart_history).

%% @doc Get restart statistics
-spec get_restart_statistics() -> map().
get_restart_statistics() ->
    gen_server:call(?MODULE, get_statistics).

%% @doc Get performance metrics for supervisor
-spec get_performance_metrics(supervisor_ref()) ->
    {ok, performance_metrics()} | {error, term()}.
get_performance_metrics(Supervisor) ->
    try
        Health = supervisor_detailed_health(Supervisor),
        Metrics = #{
            total_children => maps:get(total_children, Health, 0),
            active_children => maps:get(active_children, Health, 0),
            restarting_children => maps:get(restarting_children, Health, 0),
            avg_memory => maps:get(avg_memory, Health, 0),
            avg_reductions => maps:get(avg_reductions, Health, 0)
        },
        {ok, Metrics}
    catch
        _:Error ->
            {error, Error}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{
        restart_history = #{},
        performance_stats = #{},
        monitored_children = #{}
    }}.

handle_call(get_restart_history, _From, State) ->
    {reply, State#state.restart_history, State};

handle_call(get_statistics, _From, State) ->
    History = State#state.restart_history,
    Stats = maps:fold(fun(_ChildId, Restarts, Acc) ->
        Acc#{total_restarts => maps:get(total_restarts, Acc, 0) + length(Restarts)}
    end, #{total_restarts => 0}, History),
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({track_restart, ChildId, OldPid, NewPid}, State) ->
    Timestamp = erlang:timestamp(),
    Reason = case OldPid of
        undefined -> initial_start;
        _ -> restart
    end,

    RestartInfo = #{timestamp => Timestamp, reason => Reason, old_pid => OldPid, new_pid => NewPid},
    History = maps:get(ChildId, State#state.restart_history, []),
    NewHistory = [RestartInfo | History],

    %% Keep last 100 restarts per child
    LimitedHistory = case length(NewHistory) > 100 of
        true -> lists:sublist(NewHistory, 100);
        false -> NewHistory
    end,

    NewState = State#state{restart_history = maps:put(ChildId, LimitedHistory, State#state.restart_history)},
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    case maps:get(Pid, State#state.monitored_children, undefined) of
        undefined ->
            {noreply, State};
        ChildId ->
            ?LOG_INFO("Child ~p (~p) terminated: ~p", [ChildId, Pid, Reason]),
            {noreply, State#state{monitored_children = maps:remove(Pid, State#state.monitored_children)}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Monitor a child process
-spec monitor_child(pid(), child_id()) -> reference().
monitor_child(Pid, ChildId) ->
    Ref = monitor(process, Pid),
    gen_server:cast(?MODULE, {register_monitor, Ref, Pid, ChildId}),
    Ref.

%% @doc Ensure modules is a list
-spec ensure_list(term()) -> [module()].
ensure_list(Modules) when is_list(Modules) -> Modules;
ensure_list(Module) when is_atom(Module) -> [Module];
ensure_list(_) -> [].
