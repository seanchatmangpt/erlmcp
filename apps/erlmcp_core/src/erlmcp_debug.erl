-module(erlmcp_debug).
-compile({no_auto_import,[statistics/1]}).

%% Joe Armstrong's Philosophy: Make systems inspectable
%% This module provides simple debugging helpers for MCP gen_servers
%% using Erlang's built-in sys module.

-type server_id() :: term().

-export([
    get_state/1,
    get_status/1,
    trace/1,
    trace/2,
    untrace/1,
    suspend/1,
    resume/1,
    replace_state/2,
    statistics/1,
    log_to_file/2,
    install_all/1,
    remove_all/1,
    debug_all_servers/0,
    debug_all_clients/0,
    debug_registry/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Get the current state of any MCP process
%% Uses sys:get_state/1 which calls format_status/2 if defined
-spec get_state(atom() | pid()) -> term().
get_state(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> {error, not_found};
        Pid -> get_state(Pid)
    end;
get_state(Pid) when is_pid(Pid) ->
    try
        sys:get_state(Pid)
    catch
        exit:Reason -> {error, Reason}
    end.

%% @doc Get full status including state and message queue
%% More detailed than get_state/1
-spec get_status(atom() | pid()) -> term().
get_status(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> {error, not_found};
        Pid -> get_status(Pid)
    end;
get_status(Pid) when is_pid(Pid) ->
    try
        sys:get_status(Pid)
    catch
        exit:Reason -> {error, Reason}
    end.

%% @doc Enable tracing for a process (writes to stdout)
-spec trace(atom() | pid()) -> ok | {error, term()}.
trace(Name) ->
    trace(Name, true).

%% @doc Enable/disable tracing for a process
-spec trace(atom() | pid(), boolean()) -> ok | {error, term()}.
trace(Name, Flag) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> {error, not_found};
        Pid -> trace(Pid, Flag)
    end;
trace(Pid, Flag) when is_pid(Pid), is_boolean(Flag) ->
    try
        sys:trace(Pid, Flag),
        ok
    catch
        exit:Reason -> {error, Reason}
    end.

%% @doc Disable tracing for a process
-spec untrace(atom() | pid()) -> ok | {error, term()}.
untrace(Name) ->
    trace(Name, false).

%% @doc Suspend a process for inspection
%% CAUTION: This will block the process from handling messages!
%% Always resume when done.
-spec suspend(atom() | pid()) -> ok | {error, term()}.
suspend(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> {error, not_found};
        Pid -> suspend(Pid)
    end;
suspend(Pid) when is_pid(Pid) ->
    try
        sys:suspend(Pid),
        ok
    catch
        exit:Reason -> {error, Reason}
    end.

%% @doc Resume a suspended process
-spec resume(atom() | pid()) -> ok | {error, term()}.
resume(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> {error, not_found};
        Pid -> resume(Pid)
    end;
resume(Pid) when is_pid(Pid) ->
    try
        sys:resume(Pid),
        ok
    catch
        exit:Reason -> {error, Reason}
    end.

%% @doc Replace state with a transformation function
%% For hot debugging - change state without restarting
%% CAUTION: Can corrupt state if used incorrectly!
-spec replace_state(atom() | pid(), fun((term()) -> term())) -> term() | {error, term()}.
replace_state(Name, Fun) when is_atom(Name), is_function(Fun, 1) ->
    case whereis(Name) of
        undefined -> {error, not_found};
        Pid -> replace_state(Pid, Fun)
    end;
replace_state(Pid, Fun) when is_pid(Pid), is_function(Fun, 1) ->
    try
        sys:replace_state(Pid, Fun)
    catch
        exit:Reason -> {error, Reason}
    end.

%% @doc Get statistics for a process
-spec statistics(atom() | pid()) -> {ok, map()} | {error, term()}.
statistics(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> {error, not_found};
        Pid -> statistics(Pid)
    end;
statistics(Pid) when is_pid(Pid) ->
    try
        Stats = #{
            message_queue_len => element(2, erlang:process_info(Pid, message_queue_len)),
            heap_size => element(2, erlang:process_info(Pid, heap_size)),
            total_heap_size => element(2, erlang:process_info(Pid, total_heap_size)),
            stack_size => element(2, erlang:process_info(Pid, stack_size)),
            reductions => element(2, erlang:process_info(Pid, reductions)),
            memory => element(2, erlang:process_info(Pid, memory)),
            current_function => element(2, erlang:process_info(Pid, current_function)),
            status => element(2, erlang:process_info(Pid, status))
        },
        {ok, Stats}
    catch
        error:badarg -> {error, process_dead};
        exit:Reason -> {error, Reason}
    end.

%% @doc Log all sys events to a file
-spec log_to_file(atom() | pid(), string()) -> ok | {error, term()}.
log_to_file(Name, FilePath) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> {error, not_found};
        Pid -> log_to_file(Pid, FilePath)
    end;
log_to_file(Pid, FilePath) when is_pid(Pid) ->
    try
        sys:log_to_file(Pid, FilePath),
        ok
    catch
        exit:Reason -> {error, Reason}
    end.

%% @doc Install all debug functions (trace, stats, log)
-spec install_all(atom() | pid()) -> ok | {error, term()}.
install_all(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> {error, not_found};
        Pid -> install_all(Pid)
    end;
install_all(Pid) when is_pid(Pid) ->
    try
        sys:install(Pid, {fun debug_handler/3, []}),
        ok
    catch
        exit:Reason -> {error, Reason}
    end.

%% @doc Remove all debug functions
-spec remove_all(atom() | pid()) -> ok | {error, term()}.
remove_all(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> {error, not_found};
        Pid -> remove_all(Pid)
    end;
remove_all(Pid) when is_pid(Pid) ->
    try
        sys:remove(Pid, fun debug_handler/3),
        ok
    catch
        exit:Reason -> {error, Reason}
    end.

%%====================================================================
%% Convenience Functions for MCP Processes
%%====================================================================

%% @doc Debug all registered MCP servers
-spec debug_all_servers() -> [{server_id(), map()}].
debug_all_servers() ->
    Servers = erlmcp_registry:list_servers(),
    lists:map(
        fun({ServerId, {Pid, _Config}}) ->
            State = get_state(Pid),
            Stats = statistics(Pid),
            {ServerId, #{
                pid => Pid,
                state => State,
                stats => Stats
            }}
        end,
        Servers
    ).

%% @doc Debug all registered MCP clients
%% Note: Clients are not centrally registered, so this searches the process registry
-spec debug_all_clients() -> [{pid(), map()}].
debug_all_clients() ->
    %% Find all erlmcp_client processes
    Clients = [Pid || {_Name, Pid, _Type, [erlmcp_client]} <- supervisor:which_children(erlmcp_client_sup)],
    lists:map(
        fun(Pid) ->
            State = get_state(Pid),
            Stats = statistics(Pid),
            {Pid, #{
                state => State,
                stats => Stats
            }}
        end,
        Clients
    ).

%% @doc Debug the MCP registry
-spec debug_registry() -> map().
debug_registry() ->
    case whereis(erlmcp_registry) of
        undefined ->
            #{error => registry_not_running};
        Pid ->
            #{
                pid => Pid,
                state => get_state(Pid),
                stats => statistics(Pid),
                servers => length(erlmcp_registry:list_servers()),
                transports => length(erlmcp_registry:list_transports())
            }
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Custom debug handler for sys:install/2
-spec debug_handler(term(), term(), term()) -> term().
debug_handler(FuncState, Event, ProcState) ->
    logger:debug("[MCP DEBUG] Event: ~p, State: ~p", [Event, ProcState]),
    {ok, FuncState}.
