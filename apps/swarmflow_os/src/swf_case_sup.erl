%%%-------------------------------------------------------------------
%%% @doc SwarmFlow Case Supervisor
%%%
%%% Simple_one_for_one supervisor for dynamic workflow case spawning.
%%% Each workflow case runs as a supervised gen_statem process.
%%%
%%% Architecture:
%%% - Uses simple_one_for_one for dynamic child creation
%%% - Each child is a swf_case gen_statem process
%%% - Failures restart only the affected case (transient restart)
%%% - Clean shutdown with configurable timeout
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(swf_case_sup).

-behaviour(supervisor).

-include("swarmflow.hrl").

%% API
-export([start_link/0,
         start_case/2,
         start_case/3,
         stop_case/1,
         list_cases/0,
         count_cases/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Restart intensity limits
-define(MAX_RESTARTS, 10).
-define(RESTART_PERIOD, 60).  % seconds
-define(SHUTDOWN_TIMEOUT, 10000).  % 10 seconds for graceful shutdown

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the case supervisor
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start a new workflow case with net_id and initial variables
-spec start_case(binary(), map()) -> {ok, pid()} | {error, term()}.
start_case(NetId, InitialVars) ->
    start_case(NetId, InitialVars, #{}).

%% @doc Start a new workflow case with options
%%
%% Options can include:
%%   - parent_case_id: Parent case for sub-workflows
%%   - root_case_id: Root of case hierarchy
%%   - deadline: Unix timestamp ms for case deadline
%%   - priority: Case priority (default 0)
%%   - tenant_id: Multi-tenant isolation
%%   - context_id: A2A context reference
%%   - parent_pid: Parent process for notifications
%%   - correlation_id: Distributed tracing ID
%%   - metadata: Additional metadata map
%%
-spec start_case(binary(), map(), map()) ->
    {ok, pid()} | {error, term()}.
start_case(NetId, InitialVars, Options)
  when is_binary(NetId), is_map(InitialVars), is_map(Options) ->
    supervisor:start_child(?SERVER, [NetId, InitialVars, Options]).

%% @doc Stop a workflow case gracefully
%%
%% Sends cancel message to the case process and waits for clean shutdown.
%% Returns ok if case was stopped, {error, not_found} if case doesn't exist.
%%
-spec stop_case(pid() | binary()) -> ok | {error, not_found | term()}.
stop_case(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            %% Try graceful cancellation first
            try
                ok = swf_case:cancel(Pid, <<"supervisor_stop">>),
                %% Wait for process to terminate
                MRef = monitor(process, Pid),
                receive
                    {'DOWN', MRef, process, Pid, _Reason} ->
                        ok
                after ?SHUTDOWN_TIMEOUT ->
                    demonitor(MRef, [flush]),
                    %% Force terminate if graceful shutdown fails
                    supervisor:terminate_child(?SERVER, Pid)
                end
            catch
                _:_ ->
                    %% Process may have already terminated
                    ok
            end;
        false ->
            {error, not_found}
    end;
stop_case(CaseId) when is_binary(CaseId) ->
    case find_case_pid(CaseId) of
        {ok, Pid} ->
            stop_case(Pid);
        error ->
            {error, not_found}
    end.

%% @doc List all active case PIDs
%%
%% Returns a list of PIDs for all currently running workflow cases.
%% Does not include terminated or restarting children.
%%
-spec list_cases() -> [pid()].
list_cases() ->
    Children = supervisor:which_children(?SERVER),
    [Pid || {_Id, Pid, _Type, _Modules} <- Children,
            is_pid(Pid),
            is_process_alive(Pid)].

%% @doc Count active cases
-spec count_cases() -> non_neg_integer().
count_cases() ->
    length(list_cases()).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%% @private
%% @doc Initialize the supervisor with simple_one_for_one strategy
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => ?MAX_RESTARTS,
        period => ?RESTART_PERIOD
    },

    %% Child spec for swf_case gen_statem
    %% Args will be provided when start_child/2 is called
    CaseChild = #{
        id => swf_case,
        start => {swf_case, start_link, []},
        restart => transient,  % Restart only on abnormal termination
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [swf_case]
    },

    {ok, {SupFlags, [CaseChild]}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Find a case PID by case ID
-spec find_case_pid(binary()) -> {ok, pid()} | error.
find_case_pid(CaseId) ->
    %% Try gproc first if available
    case code:is_loaded(gproc) of
        false ->
            find_case_pid_by_scan(CaseId);
        _ ->
            try
                Pid = gproc:lookup_pid({n, l, {swf_case, CaseId}}),
                {ok, Pid}
            catch
                _:_ ->
                    find_case_pid_by_scan(CaseId)
            end
    end.

%% @private
%% @doc Find case PID by scanning all children
-spec find_case_pid_by_scan(binary()) -> {ok, pid()} | error.
find_case_pid_by_scan(CaseId) ->
    Cases = list_cases(),
    find_case_in_list(CaseId, Cases).

%% @private
find_case_in_list(_CaseId, []) ->
    error;
find_case_in_list(CaseId, [Pid | Rest]) ->
    try
        case swf_case:get_case_record(Pid) of
            {ok, #swf_case{id = CaseId}} ->
                {ok, Pid};
            _ ->
                find_case_in_list(CaseId, Rest)
        end
    catch
        _:_ ->
            find_case_in_list(CaseId, Rest)
    end.
