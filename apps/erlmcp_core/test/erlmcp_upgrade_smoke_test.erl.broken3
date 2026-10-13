-module(erlmcp_upgrade_smoke_test).
-behaviour(gen_server).

%% API
-export([run_post_upgrade_checks/0,
         run_post_downgrade_checks/0,
         verify_application_state/0,
         generate_report/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

-define(SERVER, ?MODULE).

-record(state, {
    test_start_time :: erlang:timestamp(),
    checks_passed = 0 :: non_neg_integer(),
    checks_failed = 0 :: non_neg_integer(),
    results = [] :: [{atom(), pass | fail, term()}]
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec run_post_upgrade_checks() -> {ok, map()}.
run_post_upgrade_checks() ->
    gen_server:call(?SERVER, {run_checks, upgrade}).

-spec run_post_downgrade_checks() -> {ok, map()}.
run_post_downgrade_checks() ->
    gen_server:call(?SERVER, {run_checks, downgrade}).

-spec verify_application_state() -> {ok, map()}.
verify_application_state() ->
    gen_server:call(?SERVER, verify_state).

-spec generate_report() -> {ok, binary()}.
generate_report() ->
    gen_server:call(?SERVER, generate_report).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{test_start_time = erlang:timestamp()}}.

handle_call({run_checks, Type}, _From, State) ->
    {Results, Passed, Failed} = run_all_checks(Type),
    Report = generate_report_map(Results, Passed, Failed, Type),
    {reply, {ok, Report}, State#state{results = Results, checks_passed = Passed, checks_failed = Failed}};

handle_call(verify_state, _From, State) ->
    Verification = verify_system_state(),
    {reply, {ok, Verification}, State};

handle_call(generate_report, _From, State) ->
    Report = format_report(State),
    {reply, {ok, Report}, State};

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

run_all_checks(UpgradeType) ->
    Checks = [
        {applications_started, fun check_applications_started/0},
        {supervisors_alive, fun check_supervisors_alive/0},
        {gen_servers_reachable, fun check_gen_servers_reachable/0},
        {ets_tables_intact, fun check_ets_tables_intact/0},
        {mnesia_consistent, fun check_mnesia_consistent/0},
        {connections_alive, fun check_connections_alive/0},
        {registry_functional, fun check_registry_functional/0},
        {version_match, fun() -> check_version_match(UpgradeType) end},
        {memory_stable, fun check_memory_stable/0},
        {process_count_stable, fun check_process_count_stable/0}
    ],

    Results = [{Name, run_check(Name, Fun)} || {Name, Fun} <- Checks],
    Passed = length([P || {_, pass} <- Results]),
    Failed = length([F || {_, fail} <- Results]),
    {Results, Passed, Failed}.

run_check(Name, Fun) ->
    try
        case Fun() of
            ok -> {Name, pass, ok};
            {ok, _} -> {Name, pass, ok};
            {error, Reason} -> {Name, fail, Reason};
            Error -> {Name, fail, Error}
        end
    catch
        _:Exception:Stacktrace ->
            logger:error("Check ~p failed: ~p~n~p", [Name, Exception, Stacktrace]),
            {Name, fail, {exception, Exception}}
    end.

%%% Check implementations

check_applications_started() ->
    RequiredApps = [erlmcp_core, erlmcp_transports, erlmcp_observability, erlmcp_validation],
    NotStarted = [App || App <- RequiredApps, not is_app_started(App)],
    case NotStarted of
        [] -> ok;
        _ -> {error, {apps_not_started, NotStarted}}
    end.

check_supervisors_alive() ->
    Supervisors = [erlmcp_sup, erlmcp_core_sup, erlmcp_server_sup],
    Dead = [Sup || Sup <- Supervisors, not is_process_alive(whereis(Sup))],
    case Dead of
        [] -> ok;
        _ -> {error, {supervisors_dead, Dead}}
    end.

check_gen_servers_reachable() ->
    Servers = [erlmcp_registry],
    Unreachable = [Srv || Srv <- Servers, not is_process_alive(whereis(Srv))],
    case Unreachable of
        [] -> ok;
        _ -> {error, {servers_unreachable, Unreachable}}
    end.

check_ets_tables_intact() ->
    %% Check critical ETS tables
    CriticalTables = [erlmcp_registry],
    Missing = [T || T <- CriticalTables, ets:info(T) =:= undefined],
    case Missing of
        [] -> ok;
        _ -> {error, {tables_missing, Missing}}
    end.

check_mnesia_consistent() ->
    case mnesia:system_info(is_running) of
        no -> ok;
        yes ->
            case mnesia:wait_for_tables(mnesia:system_info(tables), 5000) of
                ok -> ok;
                {timeout, Tables} -> {error, {mnesia_timeout, Tables}}
            end
    end.

check_connections_alive() ->
    %% Check if active transport connections exist
    case catch erlmcp_sup:list_transports() of
        Transports when is_list(Transports) -> ok;
        _ -> {error, registry_unavailable}
    end.

check_registry_functional() ->
    case catch erlmcp_registry:list_servers() of
        Servers when is_list(Servers) -> ok;
        _ -> {error, registry_not_functional}
    end.

check_version_match(upgrade) ->
    CurrentVsn = get_application_version(),
    ExpectedVsn = <<"3.0.0">>,
    case CurrentVsn of
        ExpectedVsn -> ok;
        _ -> {error, {version_mismatch, CurrentVsn, ExpectedVsn}}
    end;
check_version_match(downgrade) ->
    CurrentVsn = get_application_version(),
    ExpectedVsn = <<"2.1.0">>,
    case CurrentVsn of
        ExpectedVsn -> ok;
        _ -> {error, {version_mismatch, CurrentVsn, ExpectedVsn}}
    end.

check_memory_stable() ->
    %% Simple check - memory should not have increased drastically
    Memory = erlang:memory(total),
    {_, Total} = memsup:get_memory_data(),
    UsageRatio = Memory / Total,
    case UsageRatio < 0.95 of
        true -> ok;
        false -> {error, {memory_usage_high, UsageRatio}}
    end.

check_process_count_stable() ->
    ProcessCount = erlang:system_info(process_count),
    MaxProcesses = erlang:system_info(process_limit),
    UsageRatio = ProcessCount / MaxProcesses,
    case UsageRatio < 0.95 of
        true -> ok;
        false -> {error, {process_count_high, UsageRatio}}
    end.

%%% Verification helpers

verify_system_state() ->
    #{
        applications => [App || App <- application:which_applications(),
                               element(1, App) =:= erlmcp_core orelse
                               element(1, App) =:= erlmcp_transports orelse
                               element(1, App) =:= erlmcp_observability orelse
                               element(1, App) =:= erlmcp_validation],
        supervisors => [{Sup, is_process_alive(whereis(Sup))} || Sup <- [erlmcp_sup, erlmcp_core_sup]],
        memory_mb => erlang:memory(total) div 1024 div 1024,
        process_count => erlang:system_info(process_count),
        version => get_application_version()
    }.

%%% Report generation

generate_report_map(Results, Passed, Failed, Type) ->
    Duration = case Type of
        upgrade -> <<"upgrade">>;
        downgrade -> <<"downgrade">>
    end,
    #{
        type => Duration,
        timestamp => erlang:system_time(second),
        total_checks => length(Results),
        passed => Passed,
        failed => Failed,
        success_rate => Passed / length(Results),
        details => [{Name, Result} || {Name, Result, _} <- Results]
    }.

format_report(State) ->
    Results = State#state.results,
    Passed = State#state.checks_passed,
    Failed = State#state.checks_failed,

    Status = case Failed of
        0 -> <<"PASSED">>;
        _ -> <<"FAILED">>
    end,

    Report = io_lib:format(
        "~n=== Upgrade Smoke Test Report ===~n"
        "Status: ~s~n"
        "Total Checks: ~p~n"
        "Passed: ~p~n"
        "Failed: ~p~n"
        "Duration: ~p ms~n"
        "~n"
        "Check Results:~n~p~n",
        [Status, Passed + Failed, Passed, Failed, 0, Results]),

    iolist_to_binary(Report).

%%% Utility functions

get_application_version() ->
    case application:get_key(erlmcp_core, vsn) of
        {ok, Vsn} -> list_to_binary(Vsn);
        undefined -> <<"unknown">>
    end.

is_app_started(App) ->
    case application:get_application(App) of
        {ok, _} -> true;
        undefined -> false
    end.

is_process_alive(undefined) -> false;
is_process_alive(Pid) when is_pid(Pid) -> erlang:is_process_alive(Pid).
