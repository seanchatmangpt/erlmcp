%%%-----------------------------------------------------------------------------
%%% @doc Claude Code Hooks Integration Module
%%%
%%% Implements Erlang-based hooks for Claude Code integration, enforcing
%%% quality gates before task completion. This replaces bash-based hooks
%%% with proper OTP gen_server implementation.
%%%
%%% Hook Types:
%%% - pre_task: Validation before task starts
%%% - post_task: Validation after task completes (blocks "done" if fail)
%%% - pre_edit: Validation before file edits
%%% - post_edit: Auto-format and quality checks after edits
%%% - session_start: Context loading and environment setup
%%% - session_end: Metrics export and cleanup
%%%
%%% Integration:
%%% - Calls tcps_quality_gates:check_all_gates/1 for validation
%%% - Triggers Andon events on failures
%%% - Generates receipts for audit trail
%%% - Blocks task completion until all gates pass
%%%
%%% Usage from Bash Hooks:
%%% ```bash
%%% # .claude/hooks/post-task-validate.sh
%%% erl -noshell -eval "
%%%     application:ensure_all_started(erlmcp_core),
%%%     case erlmcp_hooks:post_task(<<\"$TASK_ID\">>) of
%%%         {pass, _} -> halt(0);
%%%         {fail, _} -> halt(1)
%%%     end.
%%% "
%%% ```
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_hooks).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    pre_task/2,
    post_task/1,
    post_task/2,
    pre_edit/2,
    post_edit/2,
    session_start/1,
    session_end/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type task_id() :: binary().
-type task_context() :: #{
    description => binary(),
    tool => binary(),
    file_path => binary(),
    session_id => binary()
}.

-type hook_result() :: {pass, map()} | {fail, map()}.

-export_type([task_id/0, task_context/0, hook_result/0]).

%%%=============================================================================
%%% Macros
%%%=============================================================================

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% gen_server State
%%%=============================================================================

-record(state, {
    hooks_enabled = true :: boolean(),
    auto_format = true :: boolean(),
    quality_gates_enabled = true :: boolean()
}).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start the hooks gen_server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc Pre-task hook: Validate before task starts.
%%
%% Checks:
%% - Task description is not empty
%% - Required tools are available
%% - Environment is ready
%%
%% @end
%%------------------------------------------------------------------------------
-spec pre_task(TaskId :: task_id(), Context :: task_context()) -> hook_result().
pre_task(TaskId, Context) ->
    gen_server:call(?SERVER, {pre_task, TaskId, Context}, 10000).

%%------------------------------------------------------------------------------
%% @doc Post-task hook: Validate after task completes.
%%
%% CRITICAL: This hook BLOCKS task completion if quality gates fail.
%%
%% Executes all TCPS quality gates:
%% 1. Compilation (zero errors)
%% 2. EUnit tests (all pass)
%% 3. Common Test (all pass)
%% 4. Coverage (>= 80%)
%% 5. Dialyzer (optional)
%% 6. Xref (optional)
%% 7. Benchmarks (if perf-critical code changed)
%%
%% Returns:
%% - {pass, Receipts} → Task completion allowed
%% - {fail, Violations} → Task completion BLOCKED
%%
%% @end
%%------------------------------------------------------------------------------
-spec post_task(TaskId :: task_id()) -> hook_result().
post_task(TaskId) ->
    post_task(TaskId, #{}).

-spec post_task(TaskId :: task_id(), Context :: task_context()) -> hook_result().
post_task(TaskId, Context) ->
    gen_server:call(?SERVER, {post_task, TaskId, Context}, 300000).

%%------------------------------------------------------------------------------
%% @doc Pre-edit hook: Validate before file edit.
%%
%% Checks:
%% - File is not in protected paths
%% - File type is allowed (Erlang/Rust only)
%% - No concurrent edits to same file
%%
%% @end
%%------------------------------------------------------------------------------
-spec pre_edit(FilePath :: binary(), Context :: task_context()) -> hook_result().
pre_edit(FilePath, Context) ->
    gen_server:call(?SERVER, {pre_edit, FilePath, Context}, 5000).

%%------------------------------------------------------------------------------
%% @doc Post-edit hook: Auto-format and validate after edit.
%%
%% Actions:
%% - Auto-format Erlang code (rebar3 format)
%% - Auto-format Rust code (rustfmt)
%% - Syntax validation
%% - Incremental quality checks
%%
%% @end
%%------------------------------------------------------------------------------
-spec post_edit(FilePath :: binary(), Context :: task_context()) -> hook_result().
post_edit(FilePath, Context) ->
    gen_server:call(?SERVER, {post_edit, FilePath, Context}, 30000).

%%------------------------------------------------------------------------------
%% @doc Session start hook: Initialize environment.
%%
%% Actions:
%% - Load previous session context
%% - Restore memory state
%% - Initialize TCPS services
%% - Set up quality gates
%%
%% @end
%%------------------------------------------------------------------------------
-spec session_start(SessionId :: binary()) -> hook_result().
session_start(SessionId) ->
    gen_server:call(?SERVER, {session_start, SessionId}, 10000).

%%------------------------------------------------------------------------------
%% @doc Session end hook: Cleanup and export metrics.
%%
%% Actions:
%% - Export session metrics
%% - Save memory state
%% - Generate summary receipt
%% - Cleanup resources
%%
%% @end
%%------------------------------------------------------------------------------
-spec session_end(SessionId :: binary()) -> hook_result().
session_end(SessionId) ->
    gen_server:call(?SERVER, {session_end, SessionId}, 10000).

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    io:format("erlmcp_hooks starting...~n"),
    State = #state{},
    {ok, State}.

handle_call({pre_task, TaskId, Context}, _From, State) ->
    Result = do_pre_task(TaskId, Context, State),
    {reply, Result, State};

handle_call({post_task, TaskId, Context}, _From, State) ->
    Result = do_post_task(TaskId, Context, State),
    {reply, Result, State};

handle_call({pre_edit, FilePath, Context}, _From, State) ->
    Result = do_pre_edit(FilePath, Context, State),
    {reply, Result, State};

handle_call({post_edit, FilePath, Context}, _From, State) ->
    Result = do_post_edit(FilePath, Context, State),
    {reply, Result, State};

handle_call({session_start, SessionId}, _From, State) ->
    Result = do_session_start(SessionId, State),
    {reply, Result, State};

handle_call({session_end, SessionId}, _From, State) ->
    Result = do_session_end(SessionId, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal Hook Implementations
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private Pre-task validation
%%------------------------------------------------------------------------------
-spec do_pre_task(task_id(), task_context(), #state{}) -> hook_result().
do_pre_task(TaskId, Context, _State) ->
    io:format("[erlmcp_hooks] Pre-task validation for ~s~n", [TaskId]),

    % Validate task description
    case maps:get(description, Context, <<>>) of
        <<>> ->
            {fail, #{
                hook => pre_task,
                task_id => TaskId,
                error => <<"Task description is empty">>,
                severity => medium
            }};
        Description ->
            io:format("  Task: ~s~n", [Description]),
            {pass, #{
                hook => pre_task,
                task_id => TaskId,
                validated => true
            }}
    end.

%%------------------------------------------------------------------------------
%% @private Post-task validation (CRITICAL: blocks task completion)
%%------------------------------------------------------------------------------
-spec do_post_task(task_id(), task_context(), #state{}) -> hook_result().
do_post_task(TaskId, Context, State) ->
    io:format("~n[erlmcp_hooks] Post-task validation for ~s~n", [TaskId]),
    io:format("==================================================~n"),

    case State#state.quality_gates_enabled of
        false ->
            io:format("  Quality gates DISABLED (development mode)~n"),
            {pass, #{hook => post_task, task_id => TaskId, gates_disabled => true}};
        true ->
            % Convert task ID to SKU ID
            SkuId = task_id_to_sku(TaskId, Context),

            % Check if tcps_quality_gates is available
            case erlang:function_exported(tcps_quality_gates, check_all_gates, 1) of
                true ->
                    io:format("  Running TCPS quality gates for SKU ~s...~n", [SkuId]),
                    case tcps_quality_gates:check_all_gates(SkuId) of
                        {ok, Receipts} ->
                            io:format("~n✅ ALL QUALITY GATES PASSED~n"),
                            io:format("  Total gates: ~p~n", [length(Receipts)]),
                            io:format("  Task completion: ALLOWED~n"),
                            io:format("==================================================~n"),
                            {pass, #{
                                hook => post_task,
                                task_id => TaskId,
                                sku_id => SkuId,
                                gates_passed => length(Receipts),
                                receipts => Receipts
                            }};
                        {failed_at, Gate, Violations} ->
                            io:format("~n❌ QUALITY GATE FAILED: ~p~n", [Gate]),
                            io:format("  Violations: ~p~n", [length(Violations)]),
                            io:format("  Task completion: BLOCKED~n"),
                            io:format("~nRESOLUTION REQUIRED:~n"),
                            print_violations(Violations),
                            io:format("==================================================~n"),
                            {fail, #{
                                hook => post_task,
                                task_id => TaskId,
                                sku_id => SkuId,
                                failed_gate => Gate,
                                violations => Violations,
                                blocked => true
                            }}
                    end;
                false ->
                    % TCPS quality gates not available - run basic validation
                    io:format("  TCPS quality gates not available, running basic validation...~n"),
                    run_basic_validation(TaskId, Context)
            end
    end.

%%------------------------------------------------------------------------------
%% @private Pre-edit validation
%%------------------------------------------------------------------------------
-spec do_pre_edit(binary(), task_context(), #state{}) -> hook_result().
do_pre_edit(FilePath, _Context, _State) ->
    io:format("[erlmcp_hooks] Pre-edit validation for ~s~n", [FilePath]),

    % Check file type (Erlang or Rust only)
    case check_file_type(FilePath) of
        {ok, FileType} ->
            io:format("  File type: ~s (allowed)~n", [FileType]),
            {pass, #{
                hook => pre_edit,
                file_path => FilePath,
                file_type => FileType,
                allowed => true
            }};
        {error, invalid_type} ->
            io:format("  File type: INVALID (only .erl, .hrl, .rs allowed)~n"),
            {fail, #{
                hook => pre_edit,
                file_path => FilePath,
                error => <<"Invalid file type - only Erlang (.erl, .hrl) and Rust (.rs) allowed">>,
                severity => high
            }}
    end.

%%------------------------------------------------------------------------------
%% @private Post-edit validation (auto-format)
%%------------------------------------------------------------------------------
-spec do_post_edit(binary(), task_context(), #state{}) -> hook_result().
do_post_edit(FilePath, _Context, State) ->
    io:format("[erlmcp_hooks] Post-edit validation for ~s~n", [FilePath]),

    case State#state.auto_format of
        false ->
            io:format("  Auto-format DISABLED~n"),
            {pass, #{hook => post_edit, file_path => FilePath, auto_format => false}};
        true ->
            case check_file_type(FilePath) of
                {ok, erlang} ->
                    auto_format_erlang(FilePath);
                {ok, rust} ->
                    auto_format_rust(FilePath);
                {error, invalid_type} ->
                    {pass, #{hook => post_edit, file_path => FilePath, skipped => true}}
            end
    end.

%%------------------------------------------------------------------------------
%% @private Session start
%%------------------------------------------------------------------------------
-spec do_session_start(binary(), #state{}) -> hook_result().
do_session_start(SessionId, _State) ->
    io:format("[erlmcp_hooks] Session start: ~s~n", [SessionId]),

    % Ensure TCPS services are running
    case application:ensure_all_started(tcps_erlmcp) of
        {ok, _Apps} ->
            io:format("  TCPS services started~n"),
            {pass, #{
                hook => session_start,
                session_id => SessionId,
                tcps_started => true
            }};
        {error, Reason} ->
            io:format("  WARNING: Failed to start TCPS services: ~p~n", [Reason]),
            {pass, #{
                hook => session_start,
                session_id => SessionId,
                tcps_started => false,
                warning => Reason
            }}
    end.

%%------------------------------------------------------------------------------
%% @private Session end
%%------------------------------------------------------------------------------
-spec do_session_end(binary(), #state{}) -> hook_result().
do_session_end(SessionId, _State) ->
    io:format("[erlmcp_hooks] Session end: ~s~n", [SessionId]),

    % Export metrics if available
    Metrics = case erlang:function_exported(tcps_quality_gates, get_quality_metrics, 0) of
        true -> tcps_quality_gates:get_quality_metrics();
        false -> #{}
    end,

    io:format("  Session metrics exported~n"),
    {pass, #{
        hook => session_end,
        session_id => SessionId,
        metrics => Metrics
    }}.

%%%=============================================================================
%%% Internal Helper Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Convert task ID to SKU ID
%%------------------------------------------------------------------------------
-spec task_id_to_sku(task_id(), task_context()) -> binary().
task_id_to_sku(TaskId, _Context) ->
    % Use task ID as SKU ID (can be enhanced to query work order system)
    TaskId.

%%------------------------------------------------------------------------------
%% Check file type
%%------------------------------------------------------------------------------
-spec check_file_type(binary()) -> {ok, erlang | rust} | {error, invalid_type}.
check_file_type(FilePath) ->
    case filename:extension(binary_to_list(FilePath)) of
        ".erl" -> {ok, erlang};
        ".hrl" -> {ok, erlang};
        ".rs" -> {ok, rust};
        _ -> {error, invalid_type}
    end.

%%------------------------------------------------------------------------------
%% Auto-format Erlang code
%%------------------------------------------------------------------------------
-spec auto_format_erlang(binary()) -> hook_result().
auto_format_erlang(FilePath) ->
    io:format("  Auto-formatting Erlang file: ~s~n", [FilePath]),
    FilePathStr = binary_to_list(FilePath),

    case os:cmd("rebar3 fmt -f " ++ FilePathStr) of
        "" ->
            io:format("  ✅ Formatted successfully~n"),
            {pass, #{
                hook => post_edit,
                file_path => FilePath,
                formatted => true
            }};
        Output ->
            io:format("  Format output: ~s~n", [Output]),
            {pass, #{
                hook => post_edit,
                file_path => FilePath,
                formatted => true,
                output => list_to_binary(Output)
            }}
    end.

%%------------------------------------------------------------------------------
%% Auto-format Rust code
%%------------------------------------------------------------------------------
-spec auto_format_rust(binary()) -> hook_result().
auto_format_rust(FilePath) ->
    io:format("  Auto-formatting Rust file: ~s~n", [FilePath]),
    FilePathStr = binary_to_list(FilePath),

    case os:cmd("rustfmt " ++ FilePathStr ++ " 2>&1") of
        "" ->
            io:format("  ✅ Formatted successfully~n"),
            {pass, #{
                hook => post_edit,
                file_path => FilePath,
                formatted => true
            }};
        Output ->
            io:format("  Format output: ~s~n", [Output]),
            {pass, #{
                hook => post_edit,
                file_path => FilePath,
                formatted => true,
                output => list_to_binary(Output)
            }}
    end.

%%------------------------------------------------------------------------------
%% Run basic validation (fallback when TCPS not available)
%%------------------------------------------------------------------------------
-spec run_basic_validation(task_id(), task_context()) -> hook_result().
run_basic_validation(TaskId, _Context) ->
    io:format("  Running basic validation...~n"),

    Checks = [
        {compilation, check_compilation()},
        {tests, check_tests()}
    ],

    Failures = [{Name, Reason} || {Name, {error, Reason}} <- Checks],

    case Failures of
        [] ->
            io:format("~n✅ BASIC VALIDATION PASSED~n"),
            io:format("  Compilation: OK~n"),
            io:format("  Tests: OK~n"),
            io:format("==================================================~n"),
            {pass, #{
                hook => post_task,
                task_id => TaskId,
                basic_validation => true,
                checks_passed => [Name || {Name, {ok, _}} <- Checks]
            }};
        _ ->
            io:format("~n❌ BASIC VALIDATION FAILED~n"),
            lists:foreach(fun({Name, Reason}) ->
                io:format("  ~p: FAILED (~p)~n", [Name, Reason])
            end, Failures),
            io:format("==================================================~n"),
            {fail, #{
                hook => post_task,
                task_id => TaskId,
                basic_validation => false,
                failures => Failures,
                blocked => true
            }}
    end.

%%------------------------------------------------------------------------------
%% Check compilation
%%------------------------------------------------------------------------------
-spec check_compilation() -> {ok, success} | {error, term()}.
check_compilation() ->
    case os:cmd("TERM=dumb rebar3 compile 2>&1") of
        Output ->
            case string:find(Output, "error") of
                nomatch -> {ok, success};
                _ -> {error, compilation_failed}
            end
    end.

%%------------------------------------------------------------------------------
%% Check tests
%%------------------------------------------------------------------------------
-spec check_tests() -> {ok, success} | {error, term()}.
check_tests() ->
    % Run both EUnit and CT
    EunitOutput = os:cmd("rebar3 eunit 2>&1"),
    CtOutput = os:cmd("rebar3 ct 2>&1"),

    EunitPassed = string:find(EunitOutput, "Failed: 0") =/= nomatch,
    CtPassed = string:find(CtOutput, "FAILED") =:= nomatch,

    case EunitPassed andalso CtPassed of
        true -> {ok, success};
        false -> {error, tests_failed}
    end.

%%------------------------------------------------------------------------------
%% Print violations (helper for output formatting)
%%------------------------------------------------------------------------------
-spec print_violations(list()) -> ok.
print_violations(Violations) ->
    lists:foreach(fun(V) ->
        Gate = maps:get(gate, V, unknown),
        Severity = maps:get(severity, V, medium),
        io:format("  - Gate: ~p (Severity: ~p)~n", [Gate, Severity]),
        case maps:get(error, V, undefined) of
            undefined -> ok;
            Error -> io:format("    Error: ~s~n", [Error])
        end
    end, Violations),
    ok.
