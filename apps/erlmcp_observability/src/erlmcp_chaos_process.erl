%%%-------------------------------------------------------------------
%%% @doc erlmcp_chaos_process - Process Chaos Primitives
%%%
%%% Process failure injection for chaos engineering:
%%% - Kill random processes
%%% - Kill specific process types (servers, workers)
%%% - Supervisor cascade failures
%%% - Clock skew injection
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_process).

-export([kill_random/1, kill_servers/1, kill_supervisor_tree/1, inject_clock_skew/1]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Kill random processes at specified rate
-spec kill_random(map()) -> ok.
kill_random(Config) ->
    Rate = maps:get(rate, Config, 0.1),
    Interval = maps:get(interval, Config, 30000),

    ?LOG_INFO("Killing random processes at ~.1f% rate every ~pms", [Rate * 100, Interval]),

    kill_random_loop(Rate, Interval).

-spec kill_random_loop(float(), pos_integer()) -> ok.
kill_random_loop(Rate, Interval) ->
    timer:sleep(Interval),

    case rand:uniform() < Rate of
        true ->
            case find_killable_process() of
                {ok, Pid} ->
                    ?LOG_INFO("Killing random process: ~p", [Pid]),
                    exit(Pid, kill);
                error ->
                    ?LOG_DEBUG("No killable process found", [])
            end;
        false ->
            ok
    end,

    kill_random_loop(Rate, Interval).

%% @doc Kill server processes of specific type
-spec kill_servers(map()) -> ok.
kill_servers(Config) ->
    Target = maps:get(target, Config),
    Rate = maps:get(rate, Config, 0.1),
    Interval = maps:get(interval, Config, 30000),

    ?LOG_INFO("Killing ~p servers at ~.1f% rate every ~pms", [Target, Rate * 100, Interval]),

    kill_servers_loop(Target, Rate, Interval).

-spec kill_servers_loop(atom(), float(), pos_integer()) -> ok.
kill_servers_loop(Target, Rate, Interval) ->
    timer:sleep(Interval),

    case rand:uniform() < Rate of
        true ->
            case find_target_processes(Target) of
                [] ->
                    ?LOG_DEBUG("No ~p processes found", [Target]);
                Processes ->
                    VictimCount = max(1, round(length(Processes) * Rate)),
                    Victims = lists:sublist(Processes, VictimCount),
                    lists:foreach(fun(Pid) ->
                                     ?LOG_INFO("Killing ~p process: ~p", [Target, Pid]),
                                     exit(Pid, kill)
                                  end,
                                  Victims)
            end;
        false ->
            ok
    end,

    kill_servers_loop(Target, Rate, Interval).

%% @doc Kill supervisor tree to test recovery
-spec kill_supervisor_tree(map()) -> ok.
kill_supervisor_tree(Config) ->
    Target = maps:get(target, Config),

    ?LOG_INFO("Killing supervisor tree: ~p", [Target]),

    case whereis(Target) of
        undefined ->
            ?LOG_WARNING("Supervisor ~p not found", [Target]),
            ok;
        Pid ->
            ?LOG_INFO("Killing supervisor: ~p (~p)", [Target, Pid]),
            exit(Pid, kill),
            timer:sleep(1000),  % Wait for restart
            ok
    end.

%% @doc Inject clock skew (time manipulation)
-spec inject_clock_skew(map()) -> ok.
inject_clock_skew(Config) ->
    SkewMs = maps:get(skew_ms, Config, 5000),
    Duration = maps:get(duration, Config, 60000),

    ?LOG_INFO("Injecting ~pms clock skew for ~pms", [SkewMs, Duration]),

    % In real implementation, would manipulate time-related functions
    % This is simulation only
    timer:sleep(Duration),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec find_killable_process() -> {ok, pid()} | error.
find_killable_process() ->
    Processes = erlang:processes(),
    KillableProcesses = lists:filter(fun is_killable/1, Processes),

    case KillableProcesses of
        [] ->
            error;
        List ->
            {ok,
             lists:nth(
                 rand:uniform(length(List)), List)}
    end.

-spec is_killable(pid()) -> boolean().
is_killable(Pid) ->
    case erlang:process_info(Pid, registered_name) of
        {registered_name, Name} ->
            NameStr = atom_to_list(Name),
            % Only kill erlmcp_server or erlmcp_client processes
            string:prefix(NameStr, "erlmcp_server") =/= nomatch
            orelse string:prefix(NameStr, "erlmcp_client") =/= nomatch;
        _ ->
            false
    end.

-spec find_target_processes(atom()) -> [pid()].
find_target_processes(Target) ->
    TargetStr = atom_to_list(Target),
    Processes = erlang:processes(),

    lists:filter(fun(Pid) ->
                    case erlang:process_info(Pid, registered_name) of
                        {registered_name, Name} ->
                            NameStr = atom_to_list(Name),
                            string:prefix(NameStr, TargetStr) =/= nomatch;
                        _ ->
                            false
                    end
                 end,
                 Processes).
