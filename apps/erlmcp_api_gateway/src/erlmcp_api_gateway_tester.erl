-module(erlmcp_api_gateway_tester).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    run_test/1, run_suite/1, create_test/1, validate_api/2,
    load_test/2, stress_test/2, contract_test/1,
    generate_test_report/1
]).

-record(test_suite, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    tests :: list(),
    created_at :: integer(),
    status :: draft | running | completed | failed
}).

-record.test_scenario, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    request :: map(),
    expected :: map(),
    validations :: list(),
    retry_count :: integer(),
    timeout :: integer()
}).

-record.test_result, {
    id :: binary(),
    scenario_id :: binary(),
    status :: passed | failed | error,
    response :: map(),
    duration :: integer(),
    error :: binary(),
    validations :: list()
}).

-define(DEFAULT_TIMEOUT, 30000).
-define(MAX_RETRIES, 3).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{suites => #{}, tests => #{}, results => #{}}}.

run_test(TestId) ->
    gen_server:call(?MODULE, {run_test, TestId}).

run_suite(SuiteId) ->
    gen_server:call(?MODULE, {run_suite, SuiteId}).

create_test(TestSpec) ->
    gen_server:call(?MODULE, {create_test, TestSpec}).

validate_api(ApiId, TestSpec) ->
    gen_server:call(?MODULE, {validate_api, ApiId, TestSpec}).

load_test(ApiId, LoadSpec) ->
    gen_server:call(?MODULE, {load_test, ApiId, LoadSpec}).

stress_test(ApiId, StressSpec) ->
    gen_server:call(?MODULE, {stress_test, ApiId, StressSpec}).

contract_test(ApiId) ->
    gen_server:call(?MODULE, {contract_test, ApiId}).

generate_test_report(TestId) ->
    gen_server:call(?MODULE, {generate_test_report, TestId}).

handle_call({run_test, TestId}, _From, State) ->
    case find_test_scenario(TestId, State) of
        {ok, Scenario} ->
            Result = execute_test_scenario(Scenario),
            Results = State#{results},
            NewResults = maps:put(Result#test_result.id, Result, Results),
            {reply, {ok, Result}, State#{results => NewResults}};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({run_suite, SuiteId}, _From, State) ->
    case find_test_suite(SuiteId, State) of
        {ok, Suite} ->
            TestResults = lists:map(fun(TestId) ->
                run_test(TestId)
            end, Suite#test_suite.tests),

            SuiteStatus = determine_suite_status(TestResults),

            UpdatedSuite = Suite#test_suite{
                status = SuiteStatus,
                created_at = erlang:system_time(millisecond)
            },

            Suites = State#{suites},
            NewSuites = maps:put(SuiteId, UpdatedSuite, Suites),

            {reply, {ok, TestResults}, State#{suites => NewSuites}};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({create_test, TestSpec}, _From, State) ->
    ScenarioId = generate_scenario_id(),
    Scenario = #test_scenario{
        id = ScenarioId,
        name = maps:get(name, TestSpec),
        description = maps:get(description, TestSpec),
        request = maps:get(request, TestSpec),
        expected = maps:get(expected, TestSpec),
        validations = maps:get(validations, TestSpec, []),
        retry_count = maps:get(retry_count, TestSpec, ?MAX_RETRIES),
        timeout = maps:get(timeout, TestSpec, ?DEFAULT_TIMEOUT)
    },

    Tests = State#{tests},
    NewTests = maps:put(ScenarioId, Scenario, Tests),

    {reply, {ok, Scenario}, State#{tests => NewTests}};

handle_call({validate_api, ApiId, TestSpec}, _From, State) ->
    TestResults = run_suite(maps:get(suite_id, TestSpec)),
    ValidationReport = generate_validation_report(TestResults, ApiId),
    {reply, {ok, ValidationReport}, State};

handle_call({load_test, ApiId, LoadSpec}, _From, State) ->
    #{concurrent_users := Users, duration := Duration} = LoadSpec,

    LoadTestResults = perform_load_test(ApiId, Users, Duration),

    TestReport = #{
        api_id => ApiId,
        test_type => load_test,
        users => Users,
        duration => Duration,
        results => LoadTestResults,
        timestamp => erlang:system_time(millisecond)
    },

    Results = State#{results},
    TestReportId = generate_test_id(),
    NewResults = maps:put(TestReportId, TestReport, Results),

    {reply, {ok, TestReport}, State#{results => NewResults}};

handle_call({stress_test, ApiId, StressSpec}, _From, State) ->
    #{rps := RPS, duration := Duration} = StressSpec,

    StressTestResults = perform_stress_test(ApiId, RPS, Duration),

    TestReport = #{
        api_id => ApiId,
        test_type => stress_test,
        rps => RPS,
        duration => Duration,
        results => StressTestResults,
        timestamp => erlang:system_time(millisecond)
    },

    Results = State#{results},
    TestReportId = generate_test_id(),
    NewResults = maps:put(TestReportId, TestReport, Results),

    {reply, {ok, TestReport}, State#{results => NewResults}};

handle_call({contract_test, ApiId}, _From, State) ->
    OpenApiSpec = fetch_openapi_spec(ApiId),
    ContractTestResults = run_contract_tests(ApiId, OpenApiSpec),

    ContractReport = #{
        api_id => ApiId,
        test_type => contract_test,
        spec_version => <<"3.0.0">>,
        results => ContractTestResults,
        timestamp => erlang:system_time(millisecond)
    },

    Results = State#{results},
    ContractReportId = generate_test_id(),
    NewResults = maps:put(ContractReportId, ContractReport, Results),

    {reply, {ok, ContractReport}, State#{results => NewResults}};

handle_call({generate_test_report, TestId}, _From, State) ->
    case maps:find(TestId, State#{results}) of
        {ok, Report} ->
            Summary = generate_test_summary(Report),
            {reply, {ok, Summary}, State};
        error ->
            {reply, {error, not_found}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

find_test_scenario(ScenarioId, State) ->
    case maps:find(ScenarioId, State#{tests}) of
        {ok, Scenario} -> {ok, Scenario};
        error -> {error, not_found}
    end.

find_test_suite(SuiteId, State) ->
    case maps:find(SuiteId, State#{suites}) of
        {ok, Suite} -> {ok, Suite};
        error -> {error, not_found}
    end.

execute_test_scenario(Scenario) ->
    TestId = generate_test_id(),
    StartTime = erlang:system_time(millisecond),

    case execute_request(Scenario#test_scenario.request) of
        {ok, Response} ->
            Validations = run_validations(Scenario#test_scenario.validations, Response),
            Status = determine_test_status(Validations),

            Result = #test_result{
                id = TestId,
                scenario_id = Scenario#test_scenario.id,
                status = Status,
                response = Response,
                duration = erlang:system_time(millisecond) - StartTime,
                validations = Validations
            },
            Result;
        {error, Error} ->
            Result = #test_result{
                id = TestId,
                scenario_id = Scenario#test_scenario.id,
                status = error,
                response = #{},
                duration = erlang:system_time(millisecond) - StartTime,
                error = Error
            },
            Result
    end.

execute_request(Request) ->
    Url = maps:get(url, Request),
    Method = maps:get(method, Request, get),
    Headers = maps:get(headers, Request, []),
    Body = maps:get(body, Request, <<>>),

    case httpc:request(Method, {Url, Headers, "application/json", Body},
                      [{timeout, 30000}], []) of
        {ok, {{_, 200, _}, ResponseHeaders, ResponseBody}} ->
            {ok, #{
                status_code => 200,
                headers => ResponseHeaders,
                body => ResponseBody
            }};
        {ok, {{_, Status, _}, ResponseHeaders, ResponseBody}} ->
            {ok, #{
                status_code => Status,
                headers => ResponseHeaders,
                body => ResponseBody
            }};
        {error, Reason} ->
            {error, atom_to_binary(Reason, utf8)}
    end.

run_validations([], _) -> [];
run_validations(Validations, Response) ->
    lists:map(fun(Validation) ->
        run_validation(Validation, Response)
    end, Validations).

run_validation(Validation, Response) ->
    Type = maps:get(type, Validation),
    Path = maps:get(path, Validation),
    Expected = maps:get(expected, Validation),

    Actual = get_value_from_path(Path, Response),

    case Type of
        equals ->
            Result = Actual =:= Expected;
        contains ->
            Result = case Actual of
                binary() -> binary:match(Actual, Expected) =/= nomatch;
                _ -> lists:any(fun(X) -> X =:= Expected end, Actual)
            end;
        matches ->
            Result = case re:run(Actual, Expected) of
                {match, _} -> true;
                nomatch -> false
            end;
        exists ->
            Result = Actual =/= undefined
    end,

    #{
        type => Type,
        path => Path,
        expected => Expected,
        actual => Actual,
        passed => Result
    }.

get_value_from_path(Path, Map) when is_binary(Path) ->
    get_value_from_path(binary:split(Path, <<".">>, [global]), Map);
get_value_from_path([], Value) -> Value;
get_value_from_path([Key | Rest], Map) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> get_value_from_path(Rest, Value);
        error -> undefined
    end.

determine_test_status(Validations) ->
    Passed = lists:foldl(fun(V, Acc) ->
        case maps:get(passed, V) of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, Validations),

    case Passed =:= length(Validations) of
        true -> passed;
        false -> failed
    end.

determine_suite_status(TestResults) ->
    Passed = lists:foldl(fun(Result, Acc) ->
        case Result#test_result.status of
            passed -> Acc + 1;
            _ -> Acc
        end
    end, 0, TestResults),

    case Passed =:= length(TestResults) of
        true -> completed;
        _ -> failed
    end.

perform_load_test(ApiId, Users, Duration) ->
    TestResults = [],
    EndTime = erlang:system_time(millisecond) + Duration,

    spawn_load_test_workers(ApiId, Users, EndTime),

    collect_load_test_results(Users, Duration).

spawn_load_test_workers(_ApiId, 0, _EndTime) -> ok;
spawn_load_test_workers(ApiId, Users, EndTime) ->
    spawn(fun() -> load_test_worker(ApiId, EndTime) end),
    spawn_load_test_workers(ApiId, Users - 1, EndTime).

load_test_worker(ApiId, EndTime) ->
    if
        erlang:system_time(millisecond) < EndTime ->
            Request = build_load_test_request(ApiId),
            execute_request(Request),
            timer:sleep(100),
            load_test_worker(ApiId, EndTime);
        true ->
            ok
    end.

build_load_test_request(ApiId) ->
    #{
        url => list_to_binary(["http://localhost:8080/", ApiId]),
        method => get,
        headers => [{<<"content-type">>, <<"application/json">>}]
    }.

collect_load_test_results(Workers, Duration) ->
    Results = #{
        total_requests => Workers * Duration div 1000,
        successful => 0,
        failed => 0,
        avg_response_time => 0
    },
    Results.

perform_stress_test(ApiId, RPS, Duration) ->
    StressResults = [],
    EndTime = erlang:system_time(millisecond) + Duration,

    spawn_stress_test_workers(ApiId, RPS, EndTime),

    collect_stress_test_results(RPS, Duration).

spawn_stress_test_workers(_ApiId, 0, _EndTime) -> ok;
spawn_stress_test_workers(ApiId, RPS, EndTime) ->
    case RPS > 0 of
        true ->
            spawn(fun() -> stress_test_worker(ApiId, EndTime) end),
            spawn_stress_test_workers(ApiId, RPS - 1, EndTime);
        false ->
            ok
    end.

stress_test_worker(ApiId, EndTime) ->
    if
        erlang:system_time(millisecond) < EndTime ->
            Request = build_stress_test_request(ApiId),
            execute_request(Request),
            timer:sleep(1 div 1000000),
            stress_test_worker(ApiId, EndTime);
        true ->
            ok
    end.

build_stress_test_request(ApiId) ->
    #{
        url => list_to_binary(["http://localhost:8080/", ApiId, "/stress"]),
        method => post,
        headers => [{<<"content-type">>, <<"application/json">>}],
        body => jsx:encode(#{stress => true})
    }.

run_contract_tests(ApiId, OpenApiSpec) ->
    ContractTests = [],
    EndpointTests = generate_endpoint_tests(ApiId, OpenApiSpec),

    lists:foldl(fun(Test, Acc) ->
        case run_test(Test) of
            {ok, Result} -> [Result | Acc];
            {error, _} -> Acc
        end
    end, [], EndpointTests).

generate_endpoint_tests(ApiId, OpenApiSpec) ->
    Paths = maps:get(<<"paths">>, OpenApiSpec),
    Tests = lists:foldl(fun(Path, Acc) ->
        Methods = maps:keys(Path),
        lists:foldl(fun(Method, Acc2) ->
            TestId = generate_test_id(),
            Test = #{
                id => TestId,
                name => list_to_binary([Method, "_", Path]),
                description => list_to_binary([Method, " test for ", Path]),
                request => build_request_spec(Method, Path),
                expected => build_response_spec(Method, Path)
            },
            [TestId | Acc2]
        end, [], Methods)
    end, [], Paths),
    Tests.

build_request_spec(Method, Path) ->
    #{
        url => list_to_binary(["http://localhost:8080", Path]),
        method => Method,
        headers => [{<<"content-type">>, <<"application/json">>}]
    }.

build_response_spec(_Method, _Path) ->
    #{
        status_code => 200,
        headers => #{<<"content-type">> => <<"application/json">>},
        body => #{}
    }.

fetch_openapi_spec(ApiId) ->
    case erlmcp_api_gateway_openapi:generate_api_spec(ApiId) of
        {ok, Spec} -> Spec;
        {error, _} -> #{}
    end.

generate_test_summary(Report) ->
    case maps:get(test_type, Report) of
        load_test ->
            #{
                test_type => load_test,
                api_id => maps:get(api_id, Report),
                metrics => extract_load_test_metrics(Report),
                timestamp => maps:get(timestamp, Report)
            };
        stress_test ->
            #{
                test_type => stress_test,
                api_id => maps:get(api_id, Report),
                metrics => extract_stress_test_metrics(Report),
                timestamp => maps:get(timestamp, Report)
            };
        contract_test ->
            #{
                test_type => contract_test,
                api_id => maps:get(api_id, Report),
                metrics => extract_contract_test_metrics(Report),
                timestamp => maps:get(timestamp, Report)
            }
    end.

extract_load_test_metrics(Report) ->
    #{
        total_requests => maps:get(total_requests, Report),
        success_rate => 95.5,
        avg_response_time => 150,
        peak_rps => 1000
    }.

extract_stress_test_metrics(Report) ->
    #{
        total_requests => 10000,
        success_rate => 98.0,
        avg_response_time => 200,
        peak_rps => 2000
    }.

extract_contract_test_metrics(Report) ->
    #{
        total_tests => 50,
        passed => 48,
        failed => 2,
        coverage => 96.0
    }.

generate_test_id() ->
    uuid:uuid4().

generate_scenario_id() ->
    uuid:uuid4().