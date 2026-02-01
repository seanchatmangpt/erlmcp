%%%-------------------------------------------------------------------
%%% @doc
%%% Armstrong-Style Failure Artifact System
%%%
%%% Creates self-contained reproducers for every interop failure.
%%% Each reproducer is a runnable test case that captures the exact
%%% failure scenario with inputs, outputs, and violated rules.
%%%
%%% == Philosophy ==
%%%
%%% "Make every failure a gift" - Joe Armstrong
%%%
%%% - Every failure produces a self-contained reproducer
%%% - Reproducers are executable Erlang modules
%%% - Reproducers capture exact inputs, expected outputs, actual outputs
%%% - Reproducers include violated rule IDs for traceability
%%% - Reproducers stored in test/reproducers/ directory
%%% - CI blocks merge until reproducers are fixed
%%%
%%% == Usage ==
%%%
%%% 1. Capture failure:
%%%    erlmcp_reproducer:capture(Scenario)
%%%
%%% 2. Run reproducer:
%%%    reproducer_20260201_001:run()
%%%
%%% 3. Get all reproducers:
%%%    erlmcp_reproducer:list_all()
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_reproducer).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    capture/1,
    capture_protocol_failure/4,
    capture_transport_failure/4,
    capture_sse_failure/4,
    list_all/0,
    list_unfixed/0,
    get_reproducer/1,
    replay/1,
    mark_fixed/1,
    generate_reproducer_module/1,
    audit_report/0
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

-define(SERVER, ?MODULE).
-define(REPRODUCER_DIR, "test/reproducers").

%% Reproducer record
-record(reproducer, {
    id :: binary(),
    timestamp :: integer(),
    rule_id :: binary(),
    description :: binary(),
    input :: [term()],
    expected :: term(),
    actual :: term(),
    stacktrace :: term() | undefined,
    system_state :: map(),
    fixed = false :: boolean(),
    fixed_at :: integer() | undefined
}).

-type reproducer() :: #reproducer{}.

%% State record
-record(state, {
    reproducers = #{} :: #{binary() => reproducer()},
    counter = 0 :: non_neg_integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Capture a failure scenario and generate a reproducer
-spec capture(map()) -> {ok, binary()} | {error, term()}.
capture(Scenario) ->
    gen_server:call(?SERVER, {capture, Scenario}).

%% @doc Capture protocol validation failure
-spec capture_protocol_failure(binary(), term(), term(), term()) ->
    {ok, binary()} | {error, term()}.
capture_protocol_failure(RuleId, Input, Expected, Actual) ->
    Scenario = #{
        rule_id => RuleId,
        description => <<"Protocol validation failure">>,
        category => protocol,
        input => [Input],
        expected => Expected,
        actual => Actual,
        stacktrace => try throw(capture_stacktrace) catch _:_ -> erlang:get_stacktrace() end,
        system_state => capture_system_state()
    },
    capture(Scenario).

%% @doc Capture transport failure
-spec capture_transport_failure(binary(), term(), term(), term()) ->
    {ok, binary()} | {error, term()}.
capture_transport_failure(RuleId, Input, Expected, Actual) ->
    Scenario = #{
        rule_id => RuleId,
        description => <<"Transport validation failure">>,
        category => transport,
        input => [Input],
        expected => Expected,
        actual => Actual,
        stacktrace => try throw(capture_stacktrace) catch _:_ -> erlang:get_stacktrace() end,
        system_state => capture_system_state()
    },
    capture(Scenario).

%% @doc Capture SSE-specific failure
-spec capture_sse_failure(binary(), term(), term(), term()) ->
    {ok, binary()} | {error, term()}.
capture_sse_failure(RuleId, Input, Expected, Actual) ->
    Scenario = #{
        rule_id => RuleId,
        description => <<"SSE transport failure">>,
        category => sse,
        input => [Input],
        expected => Expected,
        actual => Actual,
        stacktrace => try throw(capture_stacktrace) catch _:_ -> erlang:get_stacktrace() end,
        system_state => capture_system_state()
    },
    capture(Scenario).

%% @doc List all reproducers
-spec list_all() -> {ok, [reproducer()]}.
list_all() ->
    gen_server:call(?SERVER, list_all).

%% @doc List only unfixed reproducers
-spec list_unfixed() -> {ok, [reproducer()]}.
list_unfixed() ->
    gen_server:call(?SERVER, list_unfixed).

%% @doc Get specific reproducer by ID
-spec get_reproducer(binary()) -> {ok, reproducer()} | {error, not_found}.
get_reproducer(Id) ->
    gen_server:call(?SERVER, {get_reproducer, Id}).

%% @doc Replay a reproducer scenario
-spec replay(map()) -> {ok, term()} | {error, term()}.
replay(Scenario) ->
    Input = maps:get(input, Scenario),
    Expected = maps:get(expected, Scenario),

    %% Execute the input scenario
    Result = case maps:get(category, Scenario, unknown) of
        protocol ->
            replay_protocol(Input);
        transport ->
            replay_transport(Input);
        sse ->
            replay_sse(Input);
        _ ->
            {error, unknown_category}
    end,

    %% Compare result with expected
    case Result of
        Expected ->
            {ok, Result};
        _ ->
            {error, {mismatch, #{
                expected => Expected,
                actual => Result
            }}}
    end.

%% @doc Mark reproducer as fixed
-spec mark_fixed(binary()) -> ok | {error, not_found}.
mark_fixed(Id) ->
    gen_server:call(?SERVER, {mark_fixed, Id}).

%% @doc Generate reproducer module file
-spec generate_reproducer_module(reproducer()) -> {ok, file:filename()} | {error, term()}.
generate_reproducer_module(#reproducer{} = R) ->
    ModuleName = reproducer_module_name(R#reproducer.id),
    FilePath = filename:join([?REPRODUCER_DIR, ModuleName ++ ".erl"]),

    %% Ensure directory exists
    filelib:ensure_dir(FilePath),

    %% Generate module content
    Content = format_reproducer_module(R),

    %% Write file
    case file:write_file(FilePath, Content) of
        ok -> {ok, FilePath};
        Error -> Error
    end.

%% @doc Generate audit report
-spec audit_report() -> {ok, map()}.
audit_report() ->
    gen_server:call(?SERVER, audit_report).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Load existing reproducers from disk
    Reproducers = load_reproducers_from_disk(),
    Counter = map_size(Reproducers),
    {ok, #state{reproducers = Reproducers, counter = Counter}}.

handle_call({capture, Scenario}, _From, State) ->
    %% Generate unique ID
    Counter = State#state.counter + 1,
    Timestamp = erlang:system_time(millisecond),
    Id = generate_reproducer_id(Timestamp, Counter),

    %% Create reproducer record
    Reproducer = #reproducer{
        id = Id,
        timestamp = Timestamp,
        rule_id = maps:get(rule_id, Scenario),
        description = maps:get(description, Scenario),
        input = maps:get(input, Scenario),
        expected = maps:get(expected, Scenario),
        actual = maps:get(actual, Scenario),
        stacktrace = maps:get(stacktrace, Scenario, undefined),
        system_state = maps:get(system_state, Scenario, #{})
    },

    %% Generate module file
    {ok, _FilePath} = generate_reproducer_module(Reproducer),

    %% Store in state
    NewReproducers = maps:put(Id, Reproducer, State#state.reproducers),
    NewState = State#state{
        reproducers = NewReproducers,
        counter = Counter
    },

    {reply, {ok, Id}, NewState};

handle_call(list_all, _From, State) ->
    Reproducers = maps:values(State#state.reproducers),
    {reply, {ok, Reproducers}, State};

handle_call(list_unfixed, _From, State) ->
    Unfixed = [R || R <- maps:values(State#state.reproducers),
                    R#reproducer.fixed =:= false],
    {reply, {ok, Unfixed}, State};

handle_call({get_reproducer, Id}, _From, State) ->
    case maps:find(Id, State#state.reproducers) of
        {ok, Reproducer} ->
            {reply, {ok, Reproducer}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({mark_fixed, Id}, _From, State) ->
    case maps:find(Id, State#state.reproducers) of
        {ok, Reproducer} ->
            UpdatedReproducer = Reproducer#reproducer{
                fixed = true,
                fixed_at = erlang:system_time(millisecond)
            },
            NewReproducers = maps:put(Id, UpdatedReproducer, State#state.reproducers),
            NewState = State#state{reproducers = NewReproducers},
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(audit_report, _From, State) ->
    Total = map_size(State#state.reproducers),
    Unfixed = length([R || R <- maps:values(State#state.reproducers),
                           R#reproducer.fixed =:= false]),
    Fixed = Total - Unfixed,
    FixRate = case Total of
        0 -> 100.0;
        _ -> (Fixed / Total) * 100.0
    end,

    Report = #{
        timestamp => erlang:system_time(millisecond),
        total_reproducers => Total,
        fixed => Fixed,
        unfixed => Unfixed,
        fix_rate_percent => FixRate,
        reproducers_by_rule => group_by_rule(State#state.reproducers)
    },

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

%% @private Generate unique reproducer ID
-spec generate_reproducer_id(integer(), integer()) -> binary().
generate_reproducer_id(Timestamp, Counter) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:system_time_to_universal_time(Timestamp, millisecond),
    DateStr = io_lib:format("~4..0B~2..0B~2..0B", [Year, Month, Day]),
    TimeStr = io_lib:format("~2..0B~2..0B~2..0B", [Hour, Minute, Second]),
    CounterStr = io_lib:format("~3..0B", [Counter]),
    iolist_to_binary([DateStr, "_", TimeStr, "_", CounterStr]).

%% @private Get reproducer module name from ID
-spec reproducer_module_name(binary()) -> string().
reproducer_module_name(Id) ->
    "reproducer_" ++ binary_to_list(Id).

%% @private Format reproducer as Erlang module
-spec format_reproducer_module(reproducer()) -> binary().
format_reproducer_module(#reproducer{} = R) ->
    ModuleName = reproducer_module_name(R#reproducer.id),

    iolist_to_binary([
        "%% Automatically generated reproducer\n",
        "%% Created: ", format_timestamp(R#reproducer.timestamp), "\n",
        "%% Rule ID: ", R#reproducer.rule_id, "\n",
        "%% Description: ", R#reproducer.description, "\n\n",

        "-module(", ModuleName, ").\n",
        "-export([run/0, scenario/0]).\n\n",

        "-include_lib(\"eunit/include/eunit.hrl\").\n\n",

        "%% @doc Get scenario definition\n",
        "scenario() ->\n",
        "    #{", "\n",
        "        rule_id => <<\"", R#reproducer.rule_id, "\">>,\n",
        "        description => <<\"", R#reproducer.description, "\">>,\n",
        "        input => ", format_term(R#reproducer.input), ",\n",
        "        expected => ", format_term(R#reproducer.expected), ",\n",
        "        actual => ", format_term(R#reproducer.actual), "\n",
        "    }.\n\n",

        "%% @doc Run reproducer test\n",
        "run() ->\n",
        "    S = scenario(),\n",
        "    Input = maps:get(input, S),\n",
        "    Expected = maps:get(expected, S),\n",
        "    Actual = maps:get(actual, S),\n",
        "    \n",
        "    %% Replay scenario\n",
        "    Result = erlmcp_reproducer:replay(S),\n",
        "    \n",
        "    %% Verify expected behavior\n",
        "    case Result of\n",
        "        {ok, Expected} ->\n",
        "            {ok, fixed};\n",
        "        {error, {mismatch, _}} ->\n",
        "            {error, still_failing};\n",
        "        _ ->\n",
        "            {error, unexpected_result}\n",
        "    end.\n\n",

        "%% @doc EUnit test wrapper\n",
        "reproducer_test() ->\n",
        "    {ok, fixed} = run().\n"
    ]).

%% @private Format term for module generation
-spec format_term(term()) -> iolist().
format_term(Term) ->
    io_lib:format("~p", [Term]).

%% @private Format timestamp
-spec format_timestamp(integer()) -> binary().
format_timestamp(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:system_time_to_universal_time(Timestamp, millisecond),
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B UTC",
                      [Year, Month, Day, Hour, Minute, Second])
    ).

%% @private Capture current system state
-spec capture_system_state() -> map().
capture_system_state() ->
    #{
        otp_version => erlang:system_info(otp_release),
        erlang_version => erlang:system_info(version),
        process_count => erlang:system_info(process_count),
        memory => erlang:memory(),
        timestamp => erlang:system_time(millisecond)
    }.

%% @private Load reproducers from disk
-spec load_reproducers_from_disk() -> #{binary() => reproducer()}.
load_reproducers_from_disk() ->
    case filelib:wildcard(filename:join([?REPRODUCER_DIR, "reproducer_*.erl"])) of
        [] ->
            #{};
        Files ->
            lists:foldl(fun(File, Acc) ->
                case load_reproducer_from_file(File) of
                    {ok, Id, Reproducer} ->
                        maps:put(Id, Reproducer, Acc);
                    {error, _} ->
                        Acc
                end
            end, #{}, Files)
    end.

%% @private Load reproducer from file
-spec load_reproducer_from_file(file:filename()) ->
    {ok, binary(), reproducer()} | {error, term()}.
load_reproducer_from_file(File) ->
    Basename = filename:basename(File, ".erl"),
    case string:prefix(Basename, "reproducer_") of
        nomatch ->
            {error, invalid_filename};
        IdStr ->
            Id = list_to_binary(IdStr),
            %% For now, create minimal reproducer from filename
            %% In full implementation, would parse module to extract scenario
            Reproducer = #reproducer{
                id = Id,
                timestamp = 0,
                rule_id = <<"UNKNOWN">>,
                description = <<"Loaded from disk">>,
                input = [],
                expected = undefined,
                actual = undefined
            },
            {ok, Id, Reproducer}
    end.

%% @private Group reproducers by rule ID
-spec group_by_rule(#{binary() => reproducer()}) -> #{binary() => integer()}.
group_by_rule(Reproducers) ->
    lists:foldl(fun(#reproducer{rule_id = RuleId}, Acc) ->
        Count = maps:get(RuleId, Acc, 0),
        maps:put(RuleId, Count + 1, Acc)
    end, #{}, maps:values(Reproducers)).

%% @private Replay protocol scenario
-spec replay_protocol(term()) -> term().
replay_protocol([Json]) when is_binary(Json) ->
    erlmcp_protocol_validator:validate_json_rpc(Json);
replay_protocol(Other) ->
    {error, {invalid_protocol_input, Other}}.

%% @private Replay transport scenario
-spec replay_transport(term()) -> term().
replay_transport([{Transport, Data}]) ->
    erlmcp_transport_validator:validate_message_format(Transport, Data);
replay_transport(Other) ->
    {error, {invalid_transport_input, Other}}.

%% @private Replay SSE scenario
-spec replay_sse(term()) -> term().
replay_sse([{sse_connect, Opts}]) ->
    %% Simulate SSE connection
    {ok, connected};
replay_sse([{sse_message, Data}]) when is_map(Data) ->
    %% Validate SSE message format
    case maps:get(resume_id, Data, undefined) of
        undefined -> {ok, no_resume};
        ResumeId when is_binary(ResumeId) -> {ok, ResumeId};
        _ -> {error, invalid_resume_id}
    end;
replay_sse(Other) ->
    {error, {invalid_sse_input, Other}}.
