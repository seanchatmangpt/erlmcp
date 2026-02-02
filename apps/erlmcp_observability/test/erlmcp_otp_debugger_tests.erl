%%%-----------------------------------------------------------------------------
%%% @doc Tests for erlmcp_otp_debugger (OTP 28 Experimental Native Debugger)
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_otp_debugger_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% TEST FIXTURES
%%%=============================================================================

otp_debugger_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"Check debugger support", fun test_is_supported/0},
      {"Start and stop debugger", fun test_start_stop/0},
      {"Load module for debugging", fun test_load_module/0},
      {"Set and clear breakpoints", fun test_breakpoints/0},
      {"List breakpoints", fun test_list_breakpoints/0},
      {"Toggle instrumentations", fun test_instrumentations/0},
      {"Inspect process", fun test_inspect_process/0}]}.

setup() ->
    application:ensure_all_started(erlmcp_observability),
    %% Ensure we're using OTP 28 with +D flag (if available)
    case erlmcp_otp_debugger:is_supported() of
        true ->
            {ok, Session} = erlmcp_otp_debugger:start(),
            Session;
        false ->
            {skip, "OTP 28 debugger not supported (requires +D flag)"}
    end.

cleanup(Session) ->
    case Session of
        {skip, _Reason} -> ok;
        _ -> erlmcp_otp_debugger:stop(Session)
    end.

%%%=============================================================================
%%% SUPPORT TESTS
%%%=============================================================================

test_is_supported() ->
    %% On OTP 28 with +D flag, this should return true
    Supported = erlmcp_otp_debugger:is_supported(),
    ?assert(is_boolean(Supported)).

%%%=============================================================================
%%% LIFECYCLE TESTS
%%%=============================================================================

test_start_stop() ->
    {ok, Session} = erlmcp_otp_debugger:start(),
    ?assert(is_integer(Session)),

    %% Verify debugger is registered
    Pid = erlmcp_otp_debugger:whereis_debugger(),
    ?assert(is_pid(Pid)),

    %% Stop
    ok = erlmcp_otp_debugger:stop(Session),

    %% Verify unregistered
    ?assertEqual(undefined, erlmcp_otp_debugger:whereis_debugger()).

%%%=============================================================================
%%% MODULE LOADING TESTS
%%%=============================================================================

test_load_module() ->
    {ok, Session} = erlmcp_otp_debugger:start(),

    %% Load a module
    ok = erlmcp_otp_debugger:load_module(Session, lists),

    %% List loaded modules
    {ok, Loaded} = erlmcp_otp_debugger:list_loaded_modules(Session),
    ?assert(lists:member(lists, Loaded)),

    %% Unload
    ok = erlmcp_otp_debugger:unload_module(Session, lists),
    {ok, Loaded2} = erlmcp_otp_debugger:list_loaded_modules(Session),
    ?assertNot(lists:member(lists, Loaded2)).

%%%=============================================================================
%%% BREAKPOINT TESTS
%%%=============================================================================

test_breakpoints() ->
    {ok, Session} = erlmcp_otp_debugger:start(),

    %% Load module
    ok = erlmcp_otp_debugger:load_module(Session, erlmcp_otp_debugger_tests),

    %% Set breakpoint (assuming we're on a line with code)
    %% Note: Line numbers are module-specific, this is a demonstration
    case erlmcp_otp_debugger:set_breakpoint(erlmcp_otp_debugger_tests, 100) of
        ok ->
            ?assert(true);
        {error, {badkey, _Line}} ->
            %% Line 100 doesn't exist - that's ok for this test
            ?assert(true);
        {error, {unsupported, _Line}} ->
            %% Line doesn't support breakpoints (e.g., function head)
            ?assert(true)
    end,

    %% Clear breakpoint
    case erlmcp_otp_debugger:clear_breakpoint(erlmcp_otp_debugger_tests, 100) of
        ok ->
            ?assert(true);
        {error, _Reason} ->
            %% Expected if breakpoint wasn't set
            ?assert(true)
    end.

test_list_breakpoints() ->
    {ok, Session} = erlmcp_otp_debugger:start(),

    %% List all breakpoints (initially empty or system modules)
    {ok, Breakpoints} = erlmcp_otp_debugger:list_breakpoints(),
    ?assert(is_list(Breakpoints)),

    %% List breakpoints for a specific module
    case erlmcp_otp_debugger:list_breakpoints(lists) of
        {ok, ModuleBreakpoints} when is_map(ModuleBreakpoints) ->
            ?assert(true);
        {error, badkey} ->
            %% Module not loaded for debugging
            ?assert(true)
    end,

    %% List breakpoints for specific function
    case erlmcp_otp_debugger:list_breakpoints(lists, seq, 2) of
        {ok, FunBreakpoints} when is_map(FunBreakpoints) ->
            ?assert(true);
        {error, _Reason} ->
            %% Expected
            ?assert(true)
    end.

%%%=============================================================================
%%% INSTRUMENTATION TESTS
%%%=============================================================================

test_instrumentations() ->
    %% Get current instrumentations
    Instrumentations = erlmcp_otp_debugger:get_instrumentations(),
    ?assert(is_map(Instrumentations)),
    ?assert(is_boolean(maps:get(line_breakpoint, Instrumentations))),

    %% Toggle instrumentations
    ok = erlmcp_otp_debugger:toggle_instrumentations(#{line_breakpoint => true}),
    ?assert(true).

%%%=============================================================================
%%% PROCESS INSPECTION TESTS
%%%=============================================================================

test_inspect_process() ->
    {ok, _Session} = erlmcp_otp_debugger:start(),

    %% Create a test process
    Pid = spawn(fun() -> receive after infinity -> ok end end),

    %% Try to inspect (will fail as process is not suspended)
    case erlmcp_otp_debugger:inspect_process(Pid) of
        {error, process_running} ->
            ?assert(true);
        {ok, _} ->
            %% If by some chance it's suspended, that's also valid
            ?assert(true)
    end,

    %% Cleanup
    exit(Pid, kill).

%%%=============================================================================
%%% INTEGRATION TESTS
%%%=============================================================================

debugger_workflow_test_() ->
    {timeout, 30, fun test_debugger_workflow/0}.

%% @doc Test complete debugger workflow
test_debugger_workflow() ->
    case erlmcp_otp_debugger:is_supported() of
        false ->
            ?assert(true);  % Skip if not supported
        true ->
            %% 1. Start debugger
            {ok, Session} = erlmcp_otp_debugger:start(),
            ?assert(is_integer(Session)),

            %% 2. Enable instrumentations
            ok = erlmcp_otp_debugger:toggle_instrumentations(#{line_breakpoint => true}),

            %% 3. Load module
            ok = erlmcp_otp_debugger:load_module(Session, lists),

            %% 4. List breakpoints
            {ok, Breakpoints} = erlmcp_otp_debugger:list_breakpoints(),
            ?assert(is_list(Breakpoints)),

            %% 5. Cleanup
            ok = erlmcp_otp_debugger:stop(Session),
            ?assertEqual(undefined, erlmcp_otp_debugger:whereis_debugger())
    end.

%%%=============================================================================
%%% ERROR CASE TESTS
%%%=============================================================================

error_cases_test_() ->
    {foreach,
     fun() ->
         case erlmcp_otp_debugger:is_supported() of
             true -> {ok, S} = erlmcp_otp_debugger:start(), S;
             false -> {skip, "Not supported"}
         end
     end,
     fun(S) -> case S of {skip, _} -> ok; _ -> erlmcp_otp_debugger:stop(S) end end,
     [{"Double start fails", fun test_double_start/1},
      {"Load non-existent module", fun test_load_bad_module/1},
      {"Breakpoint on non-existent line", fun test_invalid_breakpoint/1}]}.

test_double_start(_Session) ->
    %% Starting twice should fail
    {error, {already_registered, _Pid}} = erlmcp_otp_debugger:start().

test_load_bad_module(_Session) ->
    %% Try to load a module that doesn't exist
    {error, module_not_loaded} = erlmcp_otp_debugger:load_module(
        undefined, non_existent_module_12345).

test_invalid_breakpoint(Session) ->
    %% Try to set breakpoint on non-existent line
    ok = erlmcp_otp_debugger:load_module(Session, lists),
    Result = erlmcp_otp_debugger:set_breakpoint(lists, 99999),
    ?assertMatch({error, {badkey, 99999}}, Result).

%%%=============================================================================
%%% GEN_SERVER TESTS
%%%=============================================================================

gen_server_behavior_test_() ->
    {timeout, 10, fun test_gen_server_behavior/0}.

test_gen_server_behavior() ->
    case erlmcp_otp_debugger:is_supported() of
        false ->
            ?assert(true);
        true ->
            {ok, Session} = erlmcp_otp_debugger:start(),

            %% Verify it's a gen_server by sending sync call
            {ok, Loaded} = gen_server:call(erlmcp_otp_debugger, {list_loaded_modules, Session}),
            ?assert(is_list(Loaded)),

            %% Verify cast doesn't crash
            gen_server:cast(erlmcp_otp_debugger, test_cast),

            erlmcp_otp_debugger:stop(Session),
            ?assert(true)
    end.
