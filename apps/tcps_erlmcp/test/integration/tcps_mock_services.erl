%%%-------------------------------------------------------------------
%%% @doc TCPS Mock Service Manager
%%%
%%% Comprehensive mock service infrastructure for integration testing:
%%% - GitHub API (work order pull signals, issues)
%%% - Marketplace API (feature requests, SKU publication)
%%% - CVE Advisory Service (security pull signals)
%%% - OTLP Collector (telemetry testing)
%%% - SPARQL Endpoint (ontology queries)
%%%
%%% All mocks use lightweight Erlang processes with ETS state storage
%%% for fast startup (<1 second) and easy reset between tests.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_mock_services).

%% Service lifecycle
-export([
    start_all/0,
    stop_all/1,
    reset_all/0
]).

%% Individual service control
-export([
    start_github_api/0,
    stop_github_api/1,
    start_marketplace_api/0,
    stop_marketplace_api/1,
    start_cve_advisory/0,
    stop_cve_advisory/1,
    start_otlp_collector/0,
    stop_otlp_collector/1,
    start_sparql_endpoint/0,
    stop_sparql_endpoint/1
]).

%% Mock data injection
-export([
    inject_github_issue/1,
    inject_marketplace_feature/1,
    inject_cve_advisory/1,
    inject_ontology_data/1
]).

%% Mock state verification
-export([
    get_github_calls/0,
    get_marketplace_calls/0,
    get_cve_calls/0,
    get_otlp_spans/0,
    get_sparql_queries/0,
    verify_call/2,
    clear_call_history/0
]).

%% Port management
-define(GITHUB_PORT, 9001).
-define(MARKETPLACE_PORT, 9002).
-define(CVE_PORT, 9003).
-define(OTLP_PORT, 9004).
-define(SPARQL_PORT, 9005).

%% ETS table names
-define(GITHUB_STATE, tcps_mock_github_state).
-define(MARKETPLACE_STATE, tcps_mock_marketplace_state).
-define(CVE_STATE, tcps_mock_cve_state).
-define(OTLP_STATE, tcps_mock_otlp_state).
-define(SPARQL_STATE, tcps_mock_sparql_state).
-define(CALL_HISTORY, tcps_mock_call_history).

%%%===================================================================
%%% Service Lifecycle
%%%===================================================================

%% @doc Start all mock services
-spec start_all() -> {ok, map()}.
start_all() ->
    %% Initialize ETS tables
    ok = init_ets_tables(),

    %% Start all services
    {ok, GitHubPid} = start_github_api(),
    {ok, MarketplacePid} = start_marketplace_api(),
    {ok, CvePid} = start_cve_advisory(),
    {ok, OtlpPid} = start_otlp_collector(),
    {ok, SparqlPid} = start_sparql_endpoint(),

    Services = #{
        github => GitHubPid,
        marketplace => MarketplacePid,
        cve => CvePid,
        otlp => OtlpPid,
        sparql => SparqlPid,
        ports => #{
            github => ?GITHUB_PORT,
            marketplace => ?MARKETPLACE_PORT,
            cve => ?CVE_PORT,
            otlp => ?OTLP_PORT,
            sparql => ?SPARQL_PORT
        }
    },

    {ok, Services}.

%% @doc Stop all mock services
-spec stop_all(map()) -> ok.
stop_all(#{github := GitHubPid,
           marketplace := MarketplacePid,
           cve := CvePid,
           otlp := OtlpPid,
           sparql := SparqlPid}) ->
    ok = stop_github_api(GitHubPid),
    ok = stop_marketplace_api(MarketplacePid),
    ok = stop_cve_advisory(CvePid),
    ok = stop_otlp_collector(OtlpPid),
    ok = stop_sparql_endpoint(SparqlPid),
    ok = cleanup_ets_tables(),
    ok.

%% @doc Reset all mock state (keep services running)
-spec reset_all() -> ok.
reset_all() ->
    lists:foreach(fun(Table) ->
        catch ets:delete_all_objects(Table)
    end, [?GITHUB_STATE, ?MARKETPLACE_STATE, ?CVE_STATE,
          ?OTLP_STATE, ?SPARQL_STATE, ?CALL_HISTORY]),
    ok.

%%%===================================================================
%%% GitHub API Mock
%%%===================================================================

%% @doc Start GitHub API mock server
-spec start_github_api() -> {ok, pid()}.
start_github_api() ->
    Pid = spawn_link(fun() -> github_server_loop() end),
    {ok, Pid}.

%% @doc Stop GitHub API mock server
-spec stop_github_api(pid()) -> ok.
stop_github_api(Pid) ->
    Pid ! stop,
    ok.

github_server_loop() ->
    receive
        {get_issues, From, Repo} ->
            log_call(github, get_issues, #{repo => Repo}),
            Issues = get_github_issues(Repo),
            From ! {issues, Issues},
            github_server_loop();

        {get_issue, From, Repo, Number} ->
            log_call(github, get_issue, #{repo => Repo, number => Number}),
            Issue = get_github_issue(Repo, Number),
            From ! {issue, Issue},
            github_server_loop();

        {create_work_order_comment, From, Repo, Number, Comment} ->
            log_call(github, create_comment, #{repo => Repo, number => Number, comment => Comment}),
            ok = store_github_comment(Repo, Number, Comment),
            From ! {ok, #{id => generate_id()}},
            github_server_loop();

        {update_issue_labels, From, Repo, Number, Labels} ->
            log_call(github, update_labels, #{repo => Repo, number => Number, labels => Labels}),
            ok = update_github_labels(Repo, Number, Labels),
            From ! ok,
            github_server_loop();

        stop ->
            ok;

        _ ->
            github_server_loop()
    end.

%%%===================================================================
%%% Marketplace API Mock
%%%===================================================================

%% @doc Start Marketplace API mock server
-spec start_marketplace_api() -> {ok, pid()}.
start_marketplace_api() ->
    Pid = spawn_link(fun() -> marketplace_server_loop() end),
    {ok, Pid}.

%% @doc Stop Marketplace API mock server
-spec stop_marketplace_api(pid()) -> ok.
stop_marketplace_api(Pid) ->
    Pid ! stop,
    ok.

marketplace_server_loop() ->
    receive
        {publish_sku, From, SkuData} ->
            log_call(marketplace, publish_sku, SkuData),
            SkuId = maps:get(sku_id, SkuData),
            ok = store_marketplace_sku(SkuId, SkuData),
            From ! {ok, #{sku_id => SkuId, published_at => erlang:system_time(millisecond)}},
            marketplace_server_loop();

        {get_sku, From, SkuId} ->
            log_call(marketplace, get_sku, #{sku_id => SkuId}),
            Result = get_marketplace_sku(SkuId),
            From ! {sku, Result},
            marketplace_server_loop();

        {query_features, From, Query} ->
            log_call(marketplace, query_features, Query),
            Features = query_marketplace_features(Query),
            From ! {features, Features},
            marketplace_server_loop();

        {get_feature_requests, From} ->
            log_call(marketplace, get_feature_requests, #{}),
            Requests = get_all_feature_requests(),
            From ! {feature_requests, Requests},
            marketplace_server_loop();

        stop ->
            ok;

        _ ->
            marketplace_server_loop()
    end.

%%%===================================================================
%%% CVE Advisory Service Mock
%%%===================================================================

%% @doc Start CVE advisory mock server
-spec start_cve_advisory() -> {ok, pid()}.
start_cve_advisory() ->
    Pid = spawn_link(fun() -> cve_server_loop() end),
    {ok, Pid}.

%% @doc Stop CVE advisory mock server
-spec stop_cve_advisory(pid()) -> ok.
stop_cve_advisory(Pid) ->
    Pid ! stop,
    ok.

cve_server_loop() ->
    receive
        {get_advisories, From, Filters} ->
            log_call(cve, get_advisories, Filters),
            Advisories = get_cve_advisories(Filters),
            From ! {advisories, Advisories},
            cve_server_loop();

        {get_advisory, From, CveId} ->
            log_call(cve, get_advisory, #{cve_id => CveId}),
            Advisory = get_cve_advisory(CveId),
            From ! {advisory, Advisory},
            cve_server_loop();

        {check_vulnerabilities, From, Dependencies} ->
            log_call(cve, check_vulnerabilities, Dependencies),
            Vulns = check_dependency_vulnerabilities(Dependencies),
            From ! {vulnerabilities, Vulns},
            cve_server_loop();

        stop ->
            ok;

        _ ->
            cve_server_loop()
    end.

%%%===================================================================
%%% OTLP Collector Mock
%%%===================================================================

%% @doc Start OTLP collector mock server
-spec start_otlp_collector() -> {ok, pid()}.
start_otlp_collector() ->
    Pid = spawn_link(fun() -> otlp_server_loop() end),
    {ok, Pid}.

%% @doc Stop OTLP collector mock server
-spec stop_otlp_collector(pid()) -> ok.
stop_otlp_collector(Pid) ->
    Pid ! stop,
    ok.

otlp_server_loop() ->
    receive
        {export_spans, From, Spans} ->
            log_call(otlp, export_spans, #{span_count => length(Spans)}),
            ok = store_otlp_spans(Spans),
            From ! {ok, #{accepted => length(Spans)}},
            otlp_server_loop();

        {export_metrics, From, Metrics} ->
            log_call(otlp, export_metrics, #{metric_count => length(Metrics)}),
            ok = store_otlp_metrics(Metrics),
            From ! {ok, #{accepted => length(Metrics)}},
            otlp_server_loop();

        {export_logs, From, Logs} ->
            log_call(otlp, export_logs, #{log_count => length(Logs)}),
            ok = store_otlp_logs(Logs),
            From ! {ok, #{accepted => length(Logs)}},
            otlp_server_loop();

        stop ->
            ok;

        _ ->
            otlp_server_loop()
    end.

%%%===================================================================
%%% SPARQL Endpoint Mock
%%%===================================================================

%% @doc Start SPARQL endpoint mock server
-spec start_sparql_endpoint() -> {ok, pid()}.
start_sparql_endpoint() ->
    Pid = spawn_link(fun() -> sparql_server_loop() end),
    {ok, Pid}.

%% @doc Stop SPARQL endpoint mock server
-spec stop_sparql_endpoint(pid()) -> ok.
stop_sparql_endpoint(Pid) ->
    Pid ! stop,
    ok.

sparql_server_loop() ->
    receive
        {query, From, QueryString} ->
            log_call(sparql, query, #{query => QueryString}),
            Results = execute_sparql_query(QueryString),
            From ! {results, Results},
            sparql_server_loop();

        {update, From, UpdateString} ->
            log_call(sparql, update, #{update => UpdateString}),
            ok = execute_sparql_update(UpdateString),
            From ! ok,
            sparql_server_loop();

        stop ->
            ok;

        _ ->
            sparql_server_loop()
    end.

%%%===================================================================
%%% Mock Data Injection
%%%===================================================================

%% @doc Inject a GitHub issue into mock state
-spec inject_github_issue(map()) -> ok.
inject_github_issue(Issue) ->
    Repo = maps:get(repo, Issue, <<"test/repo">>),
    Number = maps:get(number, Issue, generate_issue_number()),
    ets:insert(?GITHUB_STATE, {{issue, Repo, Number}, Issue}),
    ok.

%% @doc Inject a marketplace feature request
-spec inject_marketplace_feature(map()) -> ok.
inject_marketplace_feature(Feature) ->
    FeatureId = maps:get(id, Feature, generate_id()),
    ets:insert(?MARKETPLACE_STATE, {{feature, FeatureId}, Feature}),
    ok.

%% @doc Inject a CVE advisory
-spec inject_cve_advisory(map()) -> ok.
inject_cve_advisory(Advisory) ->
    CveId = maps:get(cve_id, Advisory),
    ets:insert(?CVE_STATE, {{cve, CveId}, Advisory}),
    ok.

%% @doc Inject ontology data into SPARQL endpoint
-spec inject_ontology_data(map()) -> ok.
inject_ontology_data(Data) ->
    TripleId = generate_id(),
    ets:insert(?SPARQL_STATE, {{triple, TripleId}, Data}),
    ok.

%%%===================================================================
%%% Mock State Verification
%%%===================================================================

%% @doc Get all GitHub API calls
-spec get_github_calls() -> [map()].
get_github_calls() ->
    get_service_calls(github).

%% @doc Get all Marketplace API calls
-spec get_marketplace_calls() -> [map()].
get_marketplace_calls() ->
    get_service_calls(marketplace).

%% @doc Get all CVE service calls
-spec get_cve_calls() -> [map()].
get_cve_calls() ->
    get_service_calls(cve).

%% @doc Get all OTLP spans received
-spec get_otlp_spans() -> [map()].
get_otlp_spans() ->
    case ets:lookup(?OTLP_STATE, spans) of
        [{spans, Spans}] -> Spans;
        [] -> []
    end.

%% @doc Get all SPARQL queries executed
-spec get_sparql_queries() -> [binary()].
get_sparql_queries() ->
    get_service_calls(sparql).

%% @doc Verify a specific call was made
-spec verify_call(atom(), atom()) -> boolean().
verify_call(Service, Operation) ->
    Calls = get_service_calls(Service),
    lists:any(fun(#{operation := Op}) -> Op =:= Operation end, Calls).

%% @doc Clear all call history
-spec clear_call_history() -> ok.
clear_call_history() ->
    ets:delete_all_objects(?CALL_HISTORY),
    ok.

%%%===================================================================
%%% Internal Helpers - ETS Management
%%%===================================================================

init_ets_tables() ->
    Tables = [
        {?GITHUB_STATE, [set, public, named_table]},
        {?MARKETPLACE_STATE, [set, public, named_table]},
        {?CVE_STATE, [set, public, named_table]},
        {?OTLP_STATE, [set, public, named_table]},
        {?SPARQL_STATE, [set, public, named_table]},
        {?CALL_HISTORY, [bag, public, named_table]}
    ],
    lists:foreach(fun({Name, Opts}) ->
        case ets:info(Name) of
            undefined -> ets:new(Name, Opts);
            _ -> ok
        end
    end, Tables),
    ok.

cleanup_ets_tables() ->
    Tables = [?GITHUB_STATE, ?MARKETPLACE_STATE, ?CVE_STATE,
              ?OTLP_STATE, ?SPARQL_STATE, ?CALL_HISTORY],
    lists:foreach(fun(Table) ->
        catch ets:delete(Table)
    end, Tables),
    ok.

%%%===================================================================
%%% Internal Helpers - GitHub
%%%===================================================================

get_github_issues(Repo) ->
    Pattern = {{issue, Repo, '_'}, '_'},
    Issues = ets:match_object(?GITHUB_STATE, Pattern),
    [{Number, Issue} || {{issue, _, Number}, Issue} <- Issues].

get_github_issue(Repo, Number) ->
    case ets:lookup(?GITHUB_STATE, {issue, Repo, Number}) of
        [{{issue, _, _}, Issue}] -> {ok, Issue};
        [] -> {error, not_found}
    end.

store_github_comment(Repo, Number, Comment) ->
    CommentId = generate_id(),
    CommentData = #{
        id => CommentId,
        repo => Repo,
        issue_number => Number,
        body => Comment,
        created_at => erlang:system_time(millisecond)
    },
    ets:insert(?GITHUB_STATE, {{comment, CommentId}, CommentData}),
    ok.

update_github_labels(Repo, Number, Labels) ->
    case get_github_issue(Repo, Number) of
        {ok, Issue} ->
            UpdatedIssue = Issue#{labels => Labels},
            ets:insert(?GITHUB_STATE, {{issue, Repo, Number}, UpdatedIssue}),
            ok;
        _ ->
            ok
    end.

generate_issue_number() ->
    rand:uniform(9999).

%%%===================================================================
%%% Internal Helpers - Marketplace
%%%===================================================================

store_marketplace_sku(SkuId, SkuData) ->
    ets:insert(?MARKETPLACE_STATE, {{sku, SkuId}, SkuData}),
    ok.

get_marketplace_sku(SkuId) ->
    case ets:lookup(?MARKETPLACE_STATE, {sku, SkuId}) of
        [{{sku, _}, SkuData}] -> {ok, SkuData};
        [] -> {error, not_found}
    end.

query_marketplace_features(_Query) ->
    Pattern = {{feature, '_'}, '_'},
    Features = ets:match_object(?MARKETPLACE_STATE, Pattern),
    [Feature || {{feature, _}, Feature} <- Features].

get_all_feature_requests() ->
    query_marketplace_features(#{}).

%%%===================================================================
%%% Internal Helpers - CVE
%%%===================================================================

get_cve_advisories(Filters) ->
    Pattern = {{cve, '_'}, '_'},
    Advisories = ets:match_object(?CVE_STATE, Pattern),
    AllAdvisories = [Advisory || {{cve, _}, Advisory} <- Advisories],

    %% Apply filters
    Severity = maps:get(severity, Filters, undefined),
    case Severity of
        undefined -> AllAdvisories;
        _ -> [A || A <- AllAdvisories, maps:get(severity, A) =:= Severity]
    end.

get_cve_advisory(CveId) ->
    case ets:lookup(?CVE_STATE, {cve, CveId}) of
        [{{cve, _}, Advisory}] -> {ok, Advisory};
        [] -> {error, not_found}
    end.

check_dependency_vulnerabilities(Dependencies) ->
    %% For testing, return empty list (no vulnerabilities)
    %% Can be extended to inject specific vulnerabilities
    #{
        total_dependencies => length(Dependencies),
        vulnerabilities => [],
        safe => true
    }.

%%%===================================================================
%%% Internal Helpers - OTLP
%%%===================================================================

store_otlp_spans(Spans) ->
    ExistingSpans = case ets:lookup(?OTLP_STATE, spans) of
        [{spans, S}] -> S;
        [] -> []
    end,
    ets:insert(?OTLP_STATE, {spans, ExistingSpans ++ Spans}),
    ok.

store_otlp_metrics(Metrics) ->
    ExistingMetrics = case ets:lookup(?OTLP_STATE, metrics) of
        [{metrics, M}] -> M;
        [] -> []
    end,
    ets:insert(?OTLP_STATE, {metrics, ExistingMetrics ++ Metrics}),
    ok.

store_otlp_logs(Logs) ->
    ExistingLogs = case ets:lookup(?OTLP_STATE, logs) of
        [{logs, L}] -> L;
        [] -> []
    end,
    ets:insert(?OTLP_STATE, {logs, ExistingLogs ++ Logs}),
    ok.

%%%===================================================================
%%% Internal Helpers - SPARQL
%%%===================================================================

execute_sparql_query(QueryString) ->
    %% Simple mock query execution
    %% Return all triples for SELECT * queries
    case binary:match(QueryString, <<"SELECT">>) of
        nomatch -> [];
        _ ->
            Pattern = {{triple, '_'}, '_'},
            Triples = ets:match_object(?SPARQL_STATE, Pattern),
            [Data || {{triple, _}, Data} <- Triples]
    end.

execute_sparql_update(UpdateString) ->
    %% Mock update - just log it
    ets:insert(?SPARQL_STATE, {{update, generate_id()}, #{
        update => UpdateString,
        timestamp => erlang:system_time(millisecond)
    }}),
    ok.

%%%===================================================================
%%% Internal Helpers - Call Logging
%%%===================================================================

log_call(Service, Operation, Args) ->
    CallData = #{
        service => Service,
        operation => Operation,
        args => Args,
        timestamp => erlang:system_time(millisecond)
    },
    ets:insert(?CALL_HISTORY, {Service, CallData}),
    ok.

get_service_calls(Service) ->
    Calls = ets:lookup(?CALL_HISTORY, Service),
    [CallData || {_, CallData} <- Calls].

%%%===================================================================
%%% Internal Helpers - Utilities
%%%===================================================================

generate_id() ->
    iolist_to_binary([
        "mock-",
        integer_to_list(erlang:unique_integer([positive]))
    ]).
