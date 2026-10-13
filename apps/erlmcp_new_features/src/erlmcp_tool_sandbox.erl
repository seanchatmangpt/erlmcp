-module(erlmcp_tool_sandbox).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         execute_tool/3,
         add_permission/2,
         set_limits/2,
         get_permissions/1,
         get_limits/1,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp_new_features.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 30000).
-define(MAX_EXECUTION_TIME, 30000).

%% Permission atoms from ontology
-define(PERM_READ_FILE, 'ReadFile').
-define(PERM_WRITE_FILE, 'WriteFile').
-define(PERM_NETWORK_ACCESS, 'NetworkAccess').
-define(PERM_EXECUTE_COMMAND, 'ExecuteCommand').
-define(PERM_READ_ENV, 'ReadEnv').
-define(PERM_SUBPROCESS, 'Subprocess').

-record(state, {
    permissions = sets:new() :: sets:set(atom()),
    limits = #{
        max_cpu_percent => 50,
        max_memory_mb => 256,
        max_timeout_sec => 30,
        max_file_size_mb => 10
    } :: map(),
    audit_log = [] :: list(),
    execution_count = 0 :: non_neg_integer()
}).

-record(execution_context, {
    tool_name :: binary(),
    arguments :: map(),
    start_time :: erlang:timestamp(),
    execution_id :: binary()
}).

-type sandbox() :: pid().
-type permission() :: ?PERM_READ_FILE | ?PERM_WRITE_FILE | ?PERM_NETWORK_ACCESS |
                     ?PERM_EXECUTE_COMMAND | ?PERM_READ_ENV | ?PERM_SUBPROCESS.
-type limits() :: #{
    max_cpu_percent => pos_integer(),
    max_memory_mb => pos_integer(),
    max_timeout_sec => pos_integer(),
    max_file_size_mb => pos_integer()
}.
-type execution_result() :: {ok, term()} | {error, term()}.

-export_type([sandbox/0, permission/0, limits/0, execution_result/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

-spec execute_tool(pid(), binary(), map()) -> execution_result().
execute_tool(Sandbox, ToolName, Arguments) when is_binary(ToolName), is_map(Arguments) ->
    gen_server:call(Sandbox, {execute_tool, ToolName, Arguments}, ?MAX_EXECUTION_TIME).

-spec add_permission(pid(), permission() | [permission()]) -> ok.
add_permission(Sandbox, Permission) when is_atom(Permission) ->
    gen_server:call(Sandbox, {add_permission, Permission});
add_permission(Sandbox, Permissions) when is_list(Permissions) ->
    gen_server:call(Sandbox, {add_permissions, Permissions}).

-spec set_limits(pid(), limits()) -> ok | {error, term()}.
set_limits(Sandbox, Limits) when is_map(Limits) ->
    gen_server:call(Sandbox, {set_limits, Limits}).

-spec get_permissions(pid()) -> sets:set(permission()).
get_permissions(Sandbox) ->
    gen_server:call(Sandbox, get_permissions).

-spec get_limits(pid()) -> limits().
get_limits(Sandbox) ->
    gen_server:call(Sandbox, get_limits).

-spec stop(pid()) -> ok.
stop(Sandbox) ->
    gen_server:stop(Sandbox).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}};
init(Options) ->
    InitialPerms = proplists:get_value(permissions, Options, []),
    InitialLimits = proplists:get_value(limits, Options, #{}),
    State = #state{
        permissions = sets:from_list(InitialPerms),
        limits = merge_limits(InitialLimits)
    },
    {ok, State}.

handle_call({execute_tool, ToolName, Arguments}, From, State) ->
    ExecutionId = generate_execution_id(),
    Context = #execution_context{
        tool_name = ToolName,
        arguments = Arguments,
        start_time = erlang:timestamp(),
        execution_id = ExecutionId
    },
    %% Execute in spawned process to enforce timeout
    _ = spawn(fun() ->
        Result = do_execute_tool(Context, State),
        gen_server:reply(From, Result)
    end),
    {noreply, State};

handle_call({add_permission, Permission}, _From, State) ->
    case validate_permission(Permission) of
        true ->
            NewPerms = sets:add_element(Permission, State#state.permissions),
            {reply, ok, State#state{permissions = NewPerms}};
        false ->
            {reply, {error, {invalid_permission, Permission}}, State}
    end;

handle_call({add_permissions, Permissions}, _From, State) ->
    case validate_permissions(Permissions) of
        true ->
            NewPerms = sets:union(State#state.permissions, sets:from_list(Permissions)),
            {reply, ok, State#state{permissions = NewPerms}};
        false ->
            {reply, {error, invalid_permissions}, State}
    end;

handle_call({set_limits, Limits}, _From, State) ->
    case validate_limits(Limits) of
        ok ->
            MergedLimits = merge_limits(Limits, State#state.limits),
            {reply, ok, State#state{limits = MergedLimits}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_permissions, _From, State) ->
    {reply, State#state.permissions, State};

handle_call(get_limits, _From, State) ->
    {reply, State#state.limits, State};

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Execute tool with permission checks and resource limits
do_execute_tool(Context, State) ->
    #execution_context{
        tool_name = ToolName,
        arguments = Arguments,
        execution_id = ExecutionId
    } = Context,

    %% Check required permissions
    RequiredPerms = get_required_permissions(ToolName),
    HasPermissions = check_permissions(RequiredPerms, State#state.permissions),

    case HasPermissions of
        true ->
            %% Enforce resource limits
            TimeoutMs = maps:get(max_timeout_sec, State#state.limits, 30) * 1000,
            MaxMemory = maps:get(max_memory_mb, State#state.limits, 256),

            %% Create execution result
            Result = execute_with_limits(ToolName, Arguments, TimeoutMs, MaxMemory),

            %% Log execution
            AuditEntry = #{
                execution_id => ExecutionId,
                tool_name => ToolName,
                timestamp => erlang:system_time(millisecond),
                result => Result
            },
            _ = log_audit(AuditEntry, State),

            Result;
        false ->
            {error, {permission_denied, RequiredPerms}}
    end.

%% @private Execute with timeout and memory monitoring
execute_with_limits(ToolName, _Arguments, _TimeoutMs, _MaxMemory) ->
    %% For now, return a simulated execution result
    %% In production, this would use ports or os:cmd with proper isolation
    try
        %% Simulate tool execution
        case ToolName of
            <<"read_file">> ->
                {ok, #{content => <<>>}};
            <<"write_file">> ->
                {ok, #{written => true}};
            <<"network_request">> ->
                {ok, #{status => ok}};
            _ ->
                {ok, #{result => executed}}
        end
    catch
        Type:Reason:Stacktrace ->
            {error, {Type, Reason, Stacktrace}}
    end.

%% @private Get required permissions for a tool
get_required_permissions(ToolName) ->
    case ToolName of
        <<"read_file">> -> [?PERM_READ_FILE];
        <<"write_file">> -> [?PERM_WRITE_FILE];
        <<"network_request">> -> [?PERM_NETWORK_ACCESS];
        <<"execute_command">> -> [?PERM_EXECUTE_COMMAND];
        <<"read_env">> -> [?PERM_READ_ENV];
        <<"subprocess">> -> [?PERM_SUBPROCESS];
        _ -> []
    end.

%% @private Check if all required permissions are granted
check_permissions(Required, Granted) ->
    lists:all(fun(P) -> sets:is_element(P, Granted) end, Required).

%% @private Validate single permission
validate_permission(Perm) ->
    lists:member(Perm, [
        ?PERM_READ_FILE,
        ?PERM_WRITE_FILE,
        ?PERM_NETWORK_ACCESS,
        ?PERM_EXECUTE_COMMAND,
        ?PERM_READ_ENV,
        ?PERM_SUBPROCESS
    ]).

%% @private Validate list of permissions
validate_permissions(Perms) when is_list(Perms) ->
    lists:all(fun validate_permission/1, Perms);
validate_permissions(_) ->
    false.

%% @private Validate limits
validate_limits(Limits) when is_map(Limits) ->
    MaxCPU = maps:get(max_cpu_percent, Limits, 50),
    MaxMem = maps:get(max_memory_mb, Limits, 256),
    MaxTimeout = maps:get(max_timeout_sec, Limits, 30),
    MaxFileSize = maps:get(max_file_size_mb, Limits, 10),

    case {MaxCPU > 0 andalso MaxCPU =< 100,
          MaxMem > 0 andalso MaxMem =< 10240,
          MaxTimeout > 0 andalso MaxTimeout =< 300,
          MaxFileSize > 0 andalso MaxFileSize =< 1024} of
        {true, true, true, true} -> ok;
        _ -> {error, invalid_limits}
    end;
validate_limits(_) ->
    {error, invalid_limits}.

%% @private Merge limits with defaults
merge_limits(NewLimits) ->
    merge_limits(NewLimits, #{
        max_cpu_percent => 50,
        max_memory_mb => 256,
        max_timeout_sec => 30,
        max_file_size_mb => 10
    }).

merge_limits(NewLimits, CurrentLimits) ->
    maps:merge(CurrentLimits, NewLimits).

%% @private Log audit entry
log_audit(Entry, State) ->
    NewLog = [Entry | State#state.audit_log],
    %% Keep last 1000 entries
    TrimmedLog = lists:sublist(NewLog, 1000),
    State#state{audit_log = TrimmedLog}.

%% @private Generate unique execution ID
generate_execution_id() ->
    Bin = <<(erlang:unique_integer([positive]))>>,
    <<Bin/binary, (integer_to_binary(erlang:system_time(millisecond)))/binary>>.
