%%%-----------------------------------------------------------------------------
%%% @doc erlmcp_otp_debugger - OTP 28 Experimental Native Debugger API Wrapper
%%%
%%% **EXPERIMENTAL** - Requires OTP 28.0+ with +D emulator flag
%%%
%%% This module wraps the experimental erl_debugger API introduced in OTP 28.0
%%% to provide MCP-specific debugging capabilities for complex model context
%%% workflows.
%%%
%%% ## Prerequisites
%%%
%%% 1. Erlang/OTP 28.0+
%%% 2. VM started with +D flag: `erl +D`
%%% 3. Modules compiled with debug_info and bin_opt_info flags
%%%
%%% ## Features
%%%
%%% - Line-by-line breakpoints in MCP handlers
%%% - Process state inspection (stack frames, X/Y registers)
%%% - Step-through execution for complex workflows
%%% - Variable inspection at breakpoints
%%% - MCP context debugging (request/response flows)
%%%
%%% ## Usage
%%%
%%% ```erlang
%%% %% Start debugger (requires OTP 28 with +D)
%%% {ok, Session} = erlmcp_otp_debugger:start(),
%%%
%%% %% Load module for debugging
%%% ok = erlmcp_otp_debugger:load_module(Session, erlmcp_server),
%%%
%%% %% Set breakpoint on line 42 in erlmcp_server.erl
%%% ok = erlmcp_otp_debugger:set_breakpoint(erlmcp_server, 42),
%%%
%%% %% Inspect process state when hitting breakpoint
%%% {ok, Frames} = erlmcp_otp_debugger:inspect_stack(Pid),
%%%
%%% %% Get variable value from stack frame
%%% {ok, Value} = erlmcp_otp_debugger:get_variable(Pid, FrameNo, SlotNo),
%%%
%%% %% Continue execution
%%% ok = erlmcp_otp_debugger:continue(Pid),
%%%
%%% %% Stop debugger
%%% ok = erlmcp_otp_debugger:stop(Session).
%%% ```
%%%
%%% ## MCP-Specific Debugging
%%%
%%% ```erlang
%%% %% Debug tool invocation chain
%%% {ok, Session} = erlmcp_otp_debugger:start(),
%%% ok = erlmcp_otp_debugger:load_module(Session, erlmcp_tool),
%%% ok = erlmcp_otp_debugger:set_breakpoint(erlmcp_tool, 127),  % execute/4
%%%
%%% %% Trace through MCP context flow
%%% ok = erlmcp_otp_debugger:load_module(Session, erlmcp_json_rpc),
%%% ok = erlmcp_otp_debugger:set_breakpoint(erlmcp_json_rpc, 89),  % encode_request
%%%
%%% %% Inspect request state at breakpoint
%%% {ok, State} = erlmcp_otp_debugger:inspect_process(Pid),
%%% ```
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_otp_debugger).
-behaviour(gen_server).

%% API - Debugger lifecycle
-export([start/0, start/1, stop/1, is_supported/0]).
-export([register_debugger/0, unregister_debugger/1, whereis_debugger/0]).

%% API - Module loading
-export([load_module/2, unload_module/2, list_loaded_modules/1]).

%% API - Breakpoints
-export([set_breakpoint/2, clear_breakpoint/2, list_breakpoints/0]).
-export([list_breakpoints/1, list_breakpoints/3]).
-export([toggle_instrumentations/1, get_instrumentations/0]).

%% API - Process inspection
-export([inspect_stack/1, inspect_stack/2]).
-export([inspect_variable/4, inspect_xreg/3]).
-export([inspect_process/1]).
-export([continue/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type session() :: erl_debugger:session().
-type breakpoint() :: {module(), pos_integer()}.
-type stack_frame() :: erl_debugger:stack_frame().
-type variable_value() :: {value, term()} | {too_large, pos_integer()}.

-record(state,
        {session :: session() | undefined,
         debugger_pid :: pid() | undefined,
         event_handler :: pid() | undefined,
         breakpoints :: map(),
         loaded_modules :: [module()],
         suspended_processes :: map()}).

-type state() :: #state{}.

%%%=============================================================================
%%% API - DEBUGGER LIFECYCLE
%%%=============================================================================

%% @doc Check if OTP 28 native debugger is supported
%% Requires OTP 28.0+ with +D emulator flag
-spec is_supported() -> boolean().
is_supported() ->
    try
        erl_debugger:supported() andalso (erlang:system_info(emu_type) =:= jit)
    catch
        _:_ ->
            false
    end.

%% @doc Start the OTP 28 native debugger
%% Returns {error, not_supported} if:
%% - OTP version < 28.0
%% - VM not started with +D flag
%% - erl_debugger module not available
-spec start() -> {ok, session()} | {error, term()}.
start() ->
    start([]).

%% @doc Start the debugger with options
%% Options:
%% - {instrumentations, [line_breakpoint]} - Enable instrumentations
-spec start(proplists:proplist()) -> {ok, session()} | {error, term()}.
start(Opts) ->
    case is_supported() of
        false ->
            {error, not_supported};
        true ->
            case erl_debugger:whereis() of
                undefined ->
                    gen_server:start_link(?MODULE, Opts, []);
                Pid when is_pid(Pid) ->
                    {error, {already_registered, Pid}}
            end
    end.

%% @doc Stop the debugger session
-spec stop(session()) -> ok.
stop(Session) ->
    gen_server:stop(?MODULE, normal, 5000),
    case whereis_debugger() of
        undefined ->
            ok;
        Pid ->
            erl_debugger:unregister(Pid, Session)
    end.

%% @doc Register this process as the debugger
-spec register_debugger() -> {ok, session()} | {error, term()}.
register_debugger() ->
    case erl_debugger:register(self()) of
        {ok, Session} ->
            {ok, Session};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Unregister the debugger
-spec unregister_debugger(session()) -> ok.
unregister_debugger(Session) ->
    erl_debugger:unregister(self(), Session).

%% @doc Get the registered debugger process
-spec whereis_debugger() -> pid() | undefined.
whereis_debugger() ->
    erl_debugger:whereis().

%%%=============================================================================
%%% API - MODULE LOADING
%%%=============================================================================

%% @doc Load a module for debugging
%% Module must be compiled with debug_info and bin_opt_info
-spec load_module(session(), module()) -> ok | {error, term()}.
load_module(Session, Module) ->
    gen_server:call(?MODULE, {load_module, Session, Module}).

%% @doc Unload a module from debugging
-spec unload_module(session(), module()) -> ok | {error, term()}.
unload_module(Session, Module) ->
    gen_server:call(?MODULE, {unload_module, Session, Module}).

%% @doc List all loaded debuggable modules
-spec list_loaded_modules(session()) -> {ok, [module()]}.
list_loaded_modules(Session) ->
    gen_server:call(?MODULE, {list_loaded_modules, Session}).

%%%=============================================================================
%%% API - BREAKPOINTS
%%%=============================================================================

%% @doc Set a breakpoint on Module:Line
-spec set_breakpoint(module(), pos_integer()) -> ok | {error, term()}.
set_breakpoint(Module, Line) ->
    gen_server:call(?MODULE, {set_breakpoint, Module, Line}).

%% @doc Clear a breakpoint on Module:Line
-spec clear_breakpoint(module(), pos_integer()) -> ok | {error, term()}.
clear_breakpoint(Module, Line) ->
    gen_server:call(?MODULE, {clear_breakpoint, Module, Line}).

%% @doc List all breakpoints for all modules
-spec list_breakpoints() -> {ok, [#{module := module(),
                                    line := pos_integer(),
                                    enabled := boolean()}]}.
list_breakpoints() ->
    gen_server:call(?MODULE, list_breakpoints).

%% @doc List all breakpoints for a module
-spec list_breakpoints(module()) ->
        {ok, map()} |
        {error, term()}.
list_breakpoints(Module) ->
    case erl_debugger:breakpoints(Module) of
        {ok, Breakpoints} ->
            {ok, Breakpoints};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc List breakpoints for a specific function
-spec list_breakpoints(module(), atom(), arity()) ->
        {ok, #{pos_integer() := boolean()}} | {error, term()}.
list_breakpoints(Module, Function, Arity) ->
    case erl_debugger:breakpoints(Module, Function, Arity) of
        {ok, Breakpoints} ->
            {ok, Breakpoints};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Toggle debug instrumentations
-spec toggle_instrumentations(#{line_breakpoint := boolean()}) -> ok.
toggle_instrumentations(Toggle) ->
    erl_debugger:toggle_instrumentations(Toggle).

%% @doc Get current instrumentations
-spec get_instrumentations() -> #{line_breakpoint := boolean()}.
get_instrumentations() ->
    erl_debugger:instrumentations().

%%%=============================================================================
%%% API - PROCESS INSPECTION
%%%=============================================================================

%% @doc Inspect stack frames of suspended process (default max term size: 1000)
-spec inspect_stack(pid()) ->
        {ok, [stack_frame()]} | {error, term()}.
inspect_stack(Pid) ->
    inspect_stack(Pid, 1000).

%% @doc Inspect stack frames with custom max term size
-spec inspect_stack(pid(), non_neg_integer()) ->
        {ok, [stack_frame()]} | {error, term()}.
inspect_stack(Pid, MaxTermSize) ->
    case erl_debugger:stack_frames(Pid, MaxTermSize) of
        running ->
            {error, process_running};
        Frames when is_list(Frames) ->
            {ok, Frames}
    end.

%% @doc Get variable value from stack frame slot
-spec inspect_variable(pid(), pos_integer(), non_neg_integer(), non_neg_integer()) ->
        {ok, variable_value()} | {error, term()}.
inspect_variable(Pid, FrameNo, SlotNo, MaxSize) ->
    case erl_debugger:peek_stack_frame_slot(Pid, FrameNo, SlotNo, MaxSize) of
        running ->
            {error, process_running};
        undefined ->
            {error, undefined_slot};
        Value ->
            {ok, Value}
    end.

%% @doc Inspect X register value
-spec inspect_xreg(pid(), non_neg_integer(), non_neg_integer()) ->
        {ok, variable_value()} | {error, term()}.
inspect_xreg(Pid, Reg, MaxSize) ->
    case erl_debugger:peek_xreg(Pid, Reg, MaxSize) of
        running ->
            {error, process_running};
        undefined ->
            {error, undefined_register};
        Value ->
            {ok, Value}
    end.

%% @doc Comprehensive process inspection
-spec inspect_process(pid()) ->
        {ok, #{stack_frames := [stack_frame()],
               xregs_count := non_neg_integer()}} |
        {error, term()}.
inspect_process(Pid) ->
    case erl_debugger:stack_frames(Pid, 1000) of
        running ->
            {error, process_running};
        Frames ->
            XRegCount = erl_debugger:xregs_count(Pid),
            {ok, #{stack_frames => Frames,
                   xregs_count => XRegCount}}
    end.

%% @doc Continue execution of suspended process
-spec continue(pid()) -> ok | {error, term()}.
continue(Pid) ->
    gen_server:call(?MODULE, {continue, Pid}).

%%%=============================================================================
%%% GEN_SERVER CALLBACKS
%%%=============================================================================

%% @private
init(Opts) ->
    %% Register as debugger
    case register_debugger() of
        {ok, Session} ->
            %% Enable line breakpoint instrumentation
            Instrumentations = proplists:get_value(instrumentations, Opts,
                                                   #{line_breakpoint => true}),
            toggle_instrumentations(Instrumentations),

            %% Start event handler
            EventHandler = spawn_link(fun() -> event_handler(Session) end),

            {ok, #state{session = Session,
                        debugger_pid = self(),
                        event_handler = EventHandler,
                        breakpoints = maps:new(),
                        loaded_modules = [],
                        suspended_processes = maps:new()}};
        {error, Reason} ->
            {stop, Reason}
    end.

%% @private
handle_call({load_module, _Session, Module}, _From, State) ->
    %% Ensure module is loaded with debug info
    case code:is_loaded(Module) of
        false ->
            {reply, {error, module_not_loaded}, State};
        {file, _File} ->
            Loaded = State#state.loaded_modules,
            case lists:member(Module, Loaded) of
                true ->
                    {reply, {error, already_loaded}, State};
                false ->
                    {reply, ok, State#state{loaded_modules = [Module | Loaded]}}
            end
    end;

handle_call({unload_module, _Session, Module}, _From, State) ->
    Loaded = State#state.loaded_modules,
    case lists:member(Module, Loaded) of
        false ->
            {reply, {error, not_loaded}, State};
        true ->
            {reply, ok, State#state{loaded_modules = lists:delete(Module, Loaded)}}
    end;

handle_call({list_loaded_modules, _Session}, _From, State) ->
    {reply, {ok, State#state.loaded_modules}, State};

handle_call({set_breakpoint, Module, Line}, _From, State) ->
    case erl_debugger:breakpoint(Module, Line, true) of
        ok ->
            Breakpoints = State#state.breakpoints,
            Key = {Module, Line},
            {reply, ok, State#state{breakpoints = Breakpoints#{Key => true}}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({clear_breakpoint, Module, Line}, _From, State) ->
    case erl_debugger:breakpoint(Module, Line, false) of
        ok ->
            Breakpoints = State#state.breakpoints,
            Key = {Module, Line},
            {reply, ok, State#state{breakpoints = maps:remove(Key, Breakpoints)}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(list_breakpoints, _From, State) ->
    Breakpoints = maps:to_list(State#state.breakpoints),
    Result = [#{module => M, line => L, enabled => Enabled}
              || {{M, L}, Enabled} <- Breakpoints],
    {reply, {ok, Result}, State};

handle_call({continue, Pid}, _From, State) ->
    Suspended = State#state.suspended_processes,
    case maps:get(Pid, Suspended, undefined) of
        undefined ->
            {reply, {error, not_suspended}, State};
        ResumeFun ->
            ResumeFun(),
            {reply, ok, State#state{suspended_processes = maps:remove(Pid, Suspended)}}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({debugger_event, _Session, Event}, State) ->
    handle_debugger_event(Event, State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    %% Clear all breakpoints
    maps:foreach(fun({Module, Line}, _Enabled) ->
                    erl_debugger:breakpoint(Module, Line, false)
                 end, State#state.breakpoints),

    %% Unregister debugger
    Session = State#state.session,
    case Session of
        undefined -> ok;
        _ -> unregister_debugger(Session)
    end,
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% INTERNAL FUNCTIONS - EVENT HANDLING
%%%=============================================================================

%% @private Handle debugger events
-spec handle_debugger_event(erl_debugger:event(), state()) -> ok.
handle_debugger_event({breakpoint, Pid, MFA, Line, ResumeFun}, State) ->
    ?LOG_INFO("Breakpoint hit: ~p at ~p:~p", [Pid, MFA, Line]),

    %% Store resume function
    Suspended = State#state.suspended_processes,
    NewState = State#state{suspended_processes = Suspended#{Pid => ResumeFun}},

    %% Inspect process state automatically
    case inspect_stack(Pid, 1000) of
        {ok, Frames} ->
            ?LOG_INFO("Stack frames: ~p", [Frames]),
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to inspect stack: ~p", [Reason]),
            ok
    end,

    %% Notify event handler
    State#state.event_handler ! {breakpoint_hit, Pid, MFA, Line},
    ok.

%% @private Event handler loop
-spec event_handler(session()) -> no_return().
event_handler(Session) ->
    receive
        {debugger_event, Session, Event} ->
            ?LOG_DEBUG("Debugger event: ~p", [Event]),
            event_handler(Session);
        {'EXIT', _Pid, _Reason} ->
            exit(normal);
        _ ->
            event_handler(Session)
    end.
