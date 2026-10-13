%%%-----------------------------------------------------------------------------
%%% @doc erlmcp_cli_debugger - CLI Commands for OTP 28 Experimental Debugger
%%%
%%% Provides escript and shell commands for interacting with the OTP 28 native
%%% debugger for MCP debugging workflows.
%%%
%%% ## Usage (escript)
%%%
%%% ```bash
%%% %% Start debugging session
%%% ./scripts/debug.escript start
%%%
%%% %% Set breakpoint
%%% ./scripts/debug.escript breakpoint erlmcp_server 127
%%%
%%% %% Continue process
%%% ./scripts/debug.escript continue <pid>
%%%
%%% %% Inspect process
%%% ./scripts/debug.escript inspect <pid>
%%%
%%% %% List breakpoints
%%% ./scripts/debug.escript list-breakpoints
%%%
%%% %% Stop debugging
%%% ./scripts/debug.escript stop
%%% ```
%%%
%%% ## Usage (Erlang Shell)
%%%
%%% ```erlang
%%% %% Start debugger
%%% erlmcp_cli_debugger:start_cmd().
%%%
%%% %% Set breakpoint
%%% erlmcp_cli_debugger:breakpoint_cmd("erlmcp_server", "127").
%%%
%%% %% Continue
%%% erlmcp_cli_debugger:continue_cmd("<0.123.0>").
%%%
%%% %% Inspect
%%% erlmcp_cli_debugger:inspect_cmd("<0.123.0>").
%%%
%%% %% List breakpoints
%%% erlmcp_cli_debugger:list_breakpoints_cmd().
%%% ```
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_cli_debugger).

%% CLI Commands
-export([start_cmd/0, stop_cmd/0]).
-export([breakpoint_cmd/2, clear_breakpoint_cmd/2]).
-export([list_breakpoints_cmd/0, list_module_breakpoints_cmd/1]).
-export([inspect_cmd/1, inspect_stack_cmd/1]).
-export([continue_cmd/1, variables_cmd/1]).
-export([load_module_cmd/1, unload_module_cmd/1]).
-export([status_cmd/0, help_cmd/0]).

-include_lib("kernel/include/logger.hrl").

%%%=============================================================================
%%% COMMAND: START/STOP
%%%=============================================================================

%% @doc Start the OTP 28 debugger
-spec start_cmd() -> ok | {error, term()}.
start_cmd() ->
    case erlmcp_otp_debugger:is_supported() of
        false ->
            io:format("Error: OTP 28 debugger not supported.~n"),
            io:format("       Start VM with: erl +D~n"),
            {error, not_supported};
        true ->
            case erlmcp_otp_debugger:whereis_debugger() of
                undefined ->
                    case erlmcp_otp_debugger:start() of
                        {ok, Session} ->
                            io:format("Debugger started: Session ~p~n", [Session]),
                            io:format("Instrumentations: ~p~n",
                                      [erlmcp_otp_debugger:get_instrumentations()]),
                            ok;
                        {error, Reason} ->
                            io:format("Error starting debugger: ~p~n", [Reason]),
                            {error, Reason}
                    end;
                Pid ->
                    io:format("Debugger already running: ~p~n", [Pid]),
                    {error, already_running}
            end
    end.

%% @doc Stop the OTP 28 debugger
-spec stop_cmd() -> ok | {error, term()}.
stop_cmd() ->
    case erlmcp_otp_debugger:whereis_debugger() of
        undefined ->
            io:format("Debugger not running~n"),
            {error, not_running};
        _Pid ->
            %% Get session before stopping
            {ok, Session} = get_current_session(),
            ok = erlmcp_otp_debugger:stop(Session),
            io:format("Debugger stopped~n"),
            ok
    end.

%%%=============================================================================
%%% COMMAND: BREAKPOINTS
%%%=============================================================================

%% @doc Set a breakpoint: breakpoint Module Line
-spec breakpoint_cmd(string(), string()) -> ok | {error, term()}.
breakpoint_cmd(ModuleStr, LineStr) ->
    Module = list_to_existing_atom(ModuleStr),
    Line = list_to_integer(LineStr),

    case erlmcp_otp_debugger:set_breakpoint(Module, Line) of
        ok ->
            io:format("Breakpoint set: ~p:~p~n", [Module, Line]),
            ok;
        {error, Reason} ->
            io:format("Error setting breakpoint: ~p~n", [Reason]),
            print_breakpoint_error(Reason),
            {error, Reason}
    end.

%% @doc Clear a breakpoint: clear-breakpoint Module Line
-spec clear_breakpoint_cmd(string(), string()) -> ok | {error, term()}.
clear_breakpoint_cmd(ModuleStr, LineStr) ->
    Module = list_to_existing_atom(ModuleStr),
    Line = list_to_integer(LineStr),

    case erlmcp_otp_debugger:clear_breakpoint(Module, Line) of
        ok ->
            io:format("Breakpoint cleared: ~p:~p~n", [Module, Line]),
            ok;
        {error, Reason} ->
            io:format("Error clearing breakpoint: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc List all breakpoints
-spec list_breakpoints_cmd() -> ok.
list_breakpoints_cmd() ->
    case erlmcp_otp_debugger:list_breakpoints() of
        {ok, []} ->
            io:format("No breakpoints set~n");
        {ok, Breakpoints} ->
            io:format("~n~30s ~10s ~10s~n", ["Module", "Line", "Enabled"]),
            io:format("~s~n", [lists:duplicate(52, $-)]),
            lists:foreach(fun(#{module := M, line := L, enabled := Enabled}) ->
                              io:format("~30w ~10w ~10w~n", [M, L, Enabled])
                          end, Breakpoints);
        {error, Reason} ->
            io:format("Error listing breakpoints: ~p~n", [Reason])
    end,
    ok.

%% @doc List breakpoints for a specific module
-spec list_module_breakpoints_cmd(string()) -> ok.
list_module_breakpoints_cmd(ModuleStr) ->
    Module = list_to_existing_atom(ModuleStr),

    case erlmcp_otp_debugger:list_breakpoints(Module) of
        {ok, BreakpointsMap} when map_size(BreakpointsMap) =:= 0 ->
            io:format("No breakpoints in ~p~n", [Module]);
        {ok, BreakpointsMap} ->
            io:format("~nBreakpoints in ~p:~n", [Module]),
            maps:foreach(fun({F, A}, LineMap) ->
                              io:format("  ~p/~p:~n", [F, A]),
                              maps:foreach(fun(Line, Enabled) ->
                                                io:format("    Line ~p: ~p~n",
                                                          [Line, Enabled])
                                            end, LineMap)
                          end, BreakpointsMap);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%%%=============================================================================
%%% COMMAND: PROCESS INSPECTION
%%%=============================================================================

%% @doc Inspect a process: inspect Pid
-spec inspect_cmd(string()) -> ok.
inspect_cmd(PidStr) ->
    Pid = parse_pid(PidStr),

    case erlmcp_otp_debugger:inspect_process(Pid) of
        {ok, #{stack_frames := Frames, xregs_count := XRegCount}} ->
            io:format("~nProcess: ~p~n", [Pid]),
            io:format("X Registers: ~p~n", [XRegCount]),
            io:format("~nStack Frames:~n"),
            print_stack_frames(Frames);
        {error, process_running} ->
            io:format("Process is running (not suspended at breakpoint)~n"),
            io:format("Hint: Set a breakpoint and wait for it to be hit~n");
        {error, Reason} ->
            io:format("Error inspecting process: ~p~n", [Reason])
    end,
    ok.

%% @doc Inspect stack frames: inspect-stack Pid
-spec inspect_stack_cmd(string()) -> ok.
inspect_stack_cmd(PidStr) ->
    Pid = parse_pid(PidStr),

    case erlmcp_otp_debugger:inspect_stack(Pid, 1000) of
        {ok, Frames} ->
            io:format("~nStack frames for ~p:~n", [Pid]),
            print_stack_frames(Frames);
        {error, process_running} ->
            io:format("Process is running (not suspended at breakpoint)~n");
        {error, Reason} ->
            io:format("Error inspecting stack: ~p~n", [Reason])
    end,
    ok.

%% @doc Inspect variables: variables Pid
-spec variables_cmd(string()) -> ok.
variables_cmd(PidStr) ->
    Pid = parse_pid(PidStr),

    case erlmcp_otp_debugger:inspect_stack(Pid, 1000) of
        {ok, Frames} ->
            io:format("~nVariables for ~p:~n", [Pid]),
            print_variables(Frames);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%%%=============================================================================
%%% COMMAND: CONTINUE
%%%=============================================================================

%% @doc Continue execution: continue Pid
-spec continue_cmd(string()) -> ok.
continue_cmd(PidStr) ->
    Pid = parse_pid(PidStr),

    case erlmcp_otp_debugger:continue(Pid) of
        ok ->
            io:format("Process ~p resumed~n", [Pid]);
        {error, not_suspended} ->
            io:format("Process is not suspended at a breakpoint~n");
        {error, Reason} ->
            io:format("Error continuing: ~p~n", [Reason])
    end,
    ok.

%%%=============================================================================
%%% COMMAND: MODULE LOADING
%%%=============================================================================

%% @doc Load module for debugging: load-module Module
-spec load_module_cmd(string()) -> ok.
load_module_cmd(ModuleStr) ->
    Module = list_to_existing_atom(ModuleStr),
    {ok, Session} = get_current_session(),

    case erlmcp_otp_debugger:load_module(Session, Module) of
        ok ->
            io:format("Module ~p loaded for debugging~n", [Module]),
            ok;
        {error, Reason} ->
            io:format("Error loading module: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Unload module: unload-module Module
-spec unload_module_cmd(string()) -> ok.
unload_module_cmd(ModuleStr) ->
    Module = list_to_existing_atom(ModuleStr),
    {ok, Session} = get_current_session(),

    case erlmcp_otp_debugger:unload_module(Session, Module) of
        ok ->
            io:format("Module ~p unloaded from debugging~n", [Module]),
            ok;
        {error, Reason} ->
            io:format("Error unloading module: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%=============================================================================
%%% COMMAND: STATUS
%%%=============================================================================

%% @doc Show debugger status
-spec status_cmd() -> ok.
status_cmd() ->
    case erlmcp_otp_debugger:is_supported() of
        false ->
            io:format("Debugger Status: NOT SUPPORTED~n"),
            io:format("  Requires: OTP 28.0+ with +D flag~n"),
            io:format("  Start VM with: erl +D~n");
        true ->
            case erlmcp_otp_debugger:whereis_debugger() of
                undefined ->
                    io:format("Debugger Status: STOPPED~n");
                Pid ->
                    io:format("Debugger Status: RUNNING~n"),
                    io:format("  Pid: ~p~n", [Pid]),
                    io:format("  Instrumentations: ~p~n",
                              [erlmcp_otp_debugger:get_instrumentations()]),
                    {ok, Breakpoints} = erlmcp_otp_debugger:list_breakpoints(),
                    io:format("  Breakpoints: ~p set~n", [length(Breakpoints)])
            end
    end,
    ok.

%% @doc Show help
-spec help_cmd() -> ok.
help_cmd() ->
    io:format("~nOTP 28 Experimental Debugger Commands~n"),
    io:format("~40s~n", [lists:duplicate(40, $-)]),
    io:format("~nLifecycle:~n"),
    io:format("  start                          Start debugger~n"),
    io:format("  stop                           Stop debugger~n"),
    io:format("  status                         Show debugger status~n"),
    io:format("~nBreakpoints:~n"),
    io:format("  breakpoint Module Line         Set breakpoint~n"),
    io:format("  clear-breakpoint Module Line   Clear breakpoint~n"),
    io:format("  list-breakpoints               List all breakpoints~n"),
    io:format("  list-breakpoints Module        List breakpoints for module~n"),
    io:format("~nInspection:~n"),
    io:format("  inspect Pid                    Inspect process~n"),
    io:format("  inspect-stack Pid              Inspect stack frames~n"),
    io:format("  variables Pid                  Show variables~n"),
    io:format("~nExecution:~n"),
    io:format("  continue Pid                   Continue execution~n"),
    io:format("~nModules:~n"),
    io:format("  load-module Module             Load module for debugging~n"),
    io:format("  unload-module Module           Unload module~n"),
    io:format("~nPrerequisites:~n"),
    io:format("  - Erlang/OTP 28.0+~n"),
    io:format("  - VM started with +D flag~n"),
    io:format("  - Modules compiled with debug_info~n"),
    io:format("~nExample:~n"),
    io:format("  1> erlmcp_cli_debugger:start_cmd().~n"),
    io:format("  2> erlmcp_cli_debugger:breakpoint_cmd(\"erlmcp_server\", \"127\").~n"),
    io:format("  3> erlmcp_cli_debugger:inspect_cmd(\"<0.123.0>\").~n"),
    io:format("  4> erlmcp_cli_debugger:continue_cmd(\"<0.123.0>\").~n"),
    io:format("~n"),
    ok.

%%%=============================================================================
%%% INTERNAL FUNCTIONS
%%%=============================================================================

%% @private Get current debugger session
-spec get_current_session() -> {ok, erlmcp_otp_debugger:session()} | {error, term()}.
get_current_session() ->
    case erlmcp_otp_debugger:whereis_debugger() of
        undefined ->
            {error, not_running};
        _Pid ->
            %% For now, we'll assume session is managed internally
            %% In a real implementation, we'd store this
            {ok, 0}
    end.

%% @private Parse PID string
-spec parse_pid(string()) -> pid().
parse_pid("<" ++ Rest) ->
    [PidStr, _] = string:split(Rest, ">"),
    case erlang:list_to_pid("<" ++ PidStr ++ ">") of
        Pid when is_pid(Pid) -> Pid;
        _ -> erlang:error(badarg)
    end;
parse_pid(PidStr) ->
    try
        list_to_pid(PidStr)
    catch
        _:_ ->
            erlang:error(badarg)
    end.

%% @private Print stack frames
-spec print_stack_frames([erlmcp_otp_debugger:stack_frame()]) -> ok.
print_stack_frames(Frames) ->
    lists:foreach(fun({FrameNo, FrameFun, Info}) ->
                      io:format("  Frame ~p: ~p~n", [FrameNo, FrameFun]),
                      print_frame_info(Info)
                  end, Frames),
    ok.

%% @private Print frame info
-spec print_frame_info(map()) -> ok.
print_frame_info(#{slots := Slots, code := CodeAddr}) ->
    io:format("    Code: 0x~.16B~n", [CodeAddr]),
    io:format("    Slots: ~p slots~n", [length(Slots)]),
    ok.

%% @private Print variables from stack frames
-spec print_variables([erlmcp_otp_debugger:stack_frame()]) -> ok.
print_variables(Frames) ->
    lists:foreach(fun({FrameNo, _FrameFun, #{slots := Slots}}) ->
                      io:format("~nFrame ~p:~n", [FrameNo]),
                      print_slots(Slots, 0)
                  end, Frames),
    ok.

%% @private Print stack frame slots
-spec print_slots([term()], non_neg_integer()) -> ok.
print_slots([], _N) ->
    ok;
print_slots([Slot | Rest], N) ->
    case Slot of
        {value, Value} ->
            io:format("  Y~p = ~p~n", [N, limit_term_size(Value, 100)]);
        {too_large, Size} ->
            io:format("  Y~p = <too large, ~p bytes>~n", [N, Size]);
        {'catch', Fun} ->
            io:format("  Y~p = catch ~p~n", [N, Fun])
    end,
    print_slots(Rest, N + 1).

%% @private Limit term size for display
-spec limit_term_size(term(), pos_integer()) -> term().
limit_term_size(Term, MaxSize) when is_binary(Term) ->
    case byte_size(Term) of
        Size when Size > MaxSize ->
            <<Restricted:MaxSize/binary, _/binary>> = Term,
            <<Restricted/binary, "...">>;
        _ ->
            Term
    end;
limit_term_size(Term, MaxSize) when is_list(Term) ->
    case iolist_size(Term) of
        Size when Size > MaxSize ->
            {lists:sublist(Term, MaxSize), '..."'};
        _ ->
            Term
    end;
limit_term_size(Term, _MaxSize) ->
    Term.

%% @private Print breakpoint error details
-spec print_breakpoint_error(term()) -> ok.
print_breakpoint_error({badkey, Module}) ->
    io:format("  Module ~p is not loaded~n", [Module]);
print_breakpoint_error({badkey, Line}) ->
    io:format("  Line ~p does not exist in the module~n", [Line]);
print_breakpoint_error({unsupported, Line}) ->
    io:format("  Line ~p does not support breakpoints (e.g., function head)~n", [Line]);
print_breakpoint_error({unsupported, Module}) ->
    io:format("  Module ~p was not compiled with line breakpoint support~n", [Module]);
print_breakpoint_error(Reason) ->
    io:format("  ~p~n", [Reason]).

%% @private Convert string to existing atom
-spec list_to_existing_atom(string()) -> atom().
list_to_existing_atom(Str) ->
    try
        list_to_existing_atom(Str)
    catch
        error:badarg ->
            io:format("Error: Module '~s' does not exist~n", [Str]),
            erlang:error(badarg)
    end.
