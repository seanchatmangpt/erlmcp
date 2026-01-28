%%%-------------------------------------------------------------------
%%% @doc TCPS 5 Whys Root Cause Analysis Framework
%%%
%%% Implements Toyota Production System's 5 Whys methodology for
%%% systematic root cause analysis of Andon events (test failures,
%%% compilation errors, non-determinism).
%%%
%%% Key Features:
%%% - Structured 5 Whys analysis with evidence chain
%%% - Automated prevention action generation
%%% - SHACL shape delta suggestions
%%% - Test case delta generation from failures
%%% - Receipt-based audit trail
%%% - Integration with TCPS ontology
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_root_cause).
-behaviour(gen_server).

-include("erlmcp.hrl").
-include("tcps_root_cause.hrl").

%% API exports
-export([
    start_link/0,
    start_analysis/2,
    add_why/3,
    finalize_analysis/3,
    generate_prevention_actions/1,
    generate_root_cause_receipt/1,
    get_analysis/1,
    list_analyses/0,
    get_analyses_by_andon/1,
    stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% Type Definitions
%%%===================================================================

-type analysis_id() :: binary().
-type andon_event_id() :: binary().
-type why_number() :: 1..5.
-type root_cause() :: binary().
-type prevention_action() :: binary().

%% Records are defined in include/tcps_root_cause.hrl

-record(state, {
    analyses = #{} :: #{analysis_id() => #five_whys{}},
    andon_index = #{} :: #{andon_event_id() => [analysis_id()]}
}).

-type state() :: #state{}.

-export_type([
    analysis_id/0,
    andon_event_id/0,
    why_number/0,
    root_cause/0,
    prevention_action/0
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the root cause analysis server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Start a new 5 Whys analysis for an Andon event
-spec start_analysis(AndonEventId :: andon_event_id(), Problem :: binary()) ->
    {ok, analysis_id()} | {error, term()}.
start_analysis(AndonEventId, Problem) when is_binary(AndonEventId), is_binary(Problem) ->
    gen_server:call(?MODULE, {start_analysis, AndonEventId, Problem}).

%% @doc Add a why answer to the analysis
-spec add_why(AnalysisId :: analysis_id(), WhyNumber :: why_number(), Answer :: binary()) ->
    ok | {error, term()}.
add_why(AnalysisId, WhyNumber, Answer) when is_binary(AnalysisId),
                                             is_integer(WhyNumber),
                                             WhyNumber >= 1, WhyNumber =< 5,
                                             is_binary(Answer) ->
    gen_server:call(?MODULE, {add_why, AnalysisId, WhyNumber, Answer}).

%% @doc Finalize the analysis with root cause and prevention actions
-spec finalize_analysis(AnalysisId :: analysis_id(), RootCause :: binary(), Prevention :: binary()) ->
    {ok, map()} | {error, term()}.
finalize_analysis(AnalysisId, RootCause, Prevention) when is_binary(AnalysisId),
                                                           is_binary(RootCause),
                                                           is_binary(Prevention) ->
    gen_server:call(?MODULE, {finalize_analysis, AnalysisId, RootCause, Prevention}).

%% @doc Generate automated prevention actions from root cause
-spec generate_prevention_actions(RootCause :: binary()) -> #prevention_delta{}.
generate_prevention_actions(RootCause) when is_binary(RootCause) ->
    generate_prevention_actions_impl(RootCause).

%% @doc Generate receipt for completed analysis
-spec generate_root_cause_receipt(Analysis :: #five_whys{}) -> map().
generate_root_cause_receipt(#five_whys{} = Analysis) ->
    generate_receipt_impl(Analysis).

%% @doc Get analysis by ID
-spec get_analysis(AnalysisId :: analysis_id()) -> {ok, #five_whys{}} | {error, not_found}.
get_analysis(AnalysisId) when is_binary(AnalysisId) ->
    gen_server:call(?MODULE, {get_analysis, AnalysisId}).

%% @doc List all analyses
-spec list_analyses() -> [#five_whys{}].
list_analyses() ->
    gen_server:call(?MODULE, list_analyses).

%% @doc Get all analyses for a specific Andon event
-spec get_analyses_by_andon(AndonEventId :: andon_event_id()) -> [#five_whys{}].
get_analyses_by_andon(AndonEventId) when is_binary(AndonEventId) ->
    gen_server:call(?MODULE, {get_analyses_by_andon, AndonEventId}).

%% @doc Stop the server
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.
handle_call({start_analysis, AndonEventId, Problem}, _From, State) ->
    AnalysisId = generate_analysis_id(),
    Now = erlang:system_time(millisecond),

    Analysis = #five_whys{
        analysis_id = AnalysisId,
        andon_event_id = AndonEventId,
        problem = Problem,
        created_at = Now,
        updated_at = Now,
        status = pending
    },

    NewAnalyses = maps:put(AnalysisId, Analysis, State#state.analyses),
    NewAndonIndex = add_to_andon_index(AndonEventId, AnalysisId, State#state.andon_index),

    NewState = State#state{
        analyses = NewAnalyses,
        andon_index = NewAndonIndex
    },

    {reply, {ok, AnalysisId}, NewState};

handle_call({add_why, AnalysisId, WhyNumber, Answer}, _From, State) ->
    case maps:find(AnalysisId, State#state.analyses) of
        {ok, Analysis} ->
            case update_why(Analysis, WhyNumber, Answer) of
                {ok, UpdatedAnalysis} ->
                    NewAnalyses = maps:put(AnalysisId, UpdatedAnalysis, State#state.analyses),
                    NewState = State#state{analyses = NewAnalyses},
                    {reply, ok, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({finalize_analysis, AnalysisId, RootCause, Prevention}, _From, State) ->
    case maps:find(AnalysisId, State#state.analyses) of
        {ok, Analysis} ->
            case validate_analysis(Analysis) of
                ok ->
                    Now = erlang:system_time(millisecond),
                    FinalizedAnalysis = Analysis#five_whys{
                        root_cause = RootCause,
                        prevention_action = Prevention,
                        finalized_at = Now,
                        updated_at = Now,
                        status = finalized
                    },

                    Receipt = generate_receipt_impl(FinalizedAnalysis),
                    PreventionDelta = generate_prevention_actions_impl(RootCause),

                    NewAnalyses = maps:put(AnalysisId, FinalizedAnalysis, State#state.analyses),
                    NewState = State#state{analyses = NewAnalyses},

                    Result = #{
                        receipt => Receipt,
                        prevention_delta => prevention_delta_to_map(PreventionDelta),
                        analysis => five_whys_to_map(FinalizedAnalysis)
                    },

                    {reply, {ok, Result}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_analysis, AnalysisId}, _From, State) ->
    case maps:find(AnalysisId, State#state.analyses) of
        {ok, Analysis} ->
            {reply, {ok, Analysis}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(list_analyses, _From, State) ->
    Analyses = maps:values(State#state.analyses),
    {reply, Analyses, State};

handle_call({get_analyses_by_andon, AndonEventId}, _From, State) ->
    case maps:find(AndonEventId, State#state.andon_index) of
        {ok, AnalysisIds} ->
            Analyses = lists:filtermap(
                fun(Id) ->
                    case maps:find(Id, State#state.analyses) of
                        {ok, A} -> {true, A};
                        error -> false
                    end
                end,
                AnalysisIds
            ),
            {reply, Analyses, State};
        error ->
            {reply, [], State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Generate unique analysis ID
-spec generate_analysis_id() -> binary().
generate_analysis_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(999999),
    iolist_to_binary(io_lib:format("analysis_~p_~p", [Timestamp, Random])).

%% @private Add analysis to Andon event index
-spec add_to_andon_index(andon_event_id(), analysis_id(), map()) -> map().
add_to_andon_index(AndonEventId, AnalysisId, Index) ->
    case maps:find(AndonEventId, Index) of
        {ok, Ids} ->
            maps:put(AndonEventId, [AnalysisId | Ids], Index);
        error ->
            maps:put(AndonEventId, [AnalysisId], Index)
    end.

%% @private Update a specific why in the analysis
-spec update_why(#five_whys{}, why_number(), binary()) -> {ok, #five_whys{}} | {error, term()}.
update_why(Analysis, 1, Answer) ->
    Now = erlang:system_time(millisecond),
    {ok, Analysis#five_whys{why_1 = Answer, updated_at = Now, status = in_progress}};
update_why(Analysis, 2, Answer) ->
    Now = erlang:system_time(millisecond),
    {ok, Analysis#five_whys{why_2 = Answer, updated_at = Now, status = in_progress}};
update_why(Analysis, 3, Answer) ->
    Now = erlang:system_time(millisecond),
    {ok, Analysis#five_whys{why_3 = Answer, updated_at = Now, status = in_progress}};
update_why(Analysis, 4, Answer) ->
    Now = erlang:system_time(millisecond),
    {ok, Analysis#five_whys{why_4 = Answer, updated_at = Now, status = in_progress}};
update_why(Analysis, 5, Answer) ->
    Now = erlang:system_time(millisecond),
    {ok, Analysis#five_whys{why_5 = Answer, updated_at = Now, status = in_progress}};
update_why(_Analysis, WhyNumber, _Answer) ->
    {error, {invalid_why_number, WhyNumber}}.

%% @private Validate analysis has all required whys
-spec validate_analysis(#five_whys{}) -> ok | {error, term()}.
validate_analysis(#five_whys{why_1 = undefined}) ->
    {error, missing_why_1};
validate_analysis(#five_whys{why_2 = undefined}) ->
    {error, missing_why_2};
validate_analysis(#five_whys{why_3 = undefined}) ->
    {error, missing_why_3};
validate_analysis(#five_whys{why_4 = undefined}) ->
    {error, missing_why_4};
validate_analysis(#five_whys{why_5 = undefined}) ->
    {error, missing_why_5};
validate_analysis(_Analysis) ->
    ok.

%% @private Generate prevention actions based on root cause patterns
-spec generate_prevention_actions_impl(binary()) -> #prevention_delta{}.
generate_prevention_actions_impl(RootCause) ->
    RootCauseLower = string:lowercase(binary_to_list(RootCause)),

    %% Pattern matching for common root causes
    ShaclAdditions = detect_shacl_needs(RootCauseLower),
    TestAdditions = detect_test_needs(RootCauseLower),
    TemplateImprovements = detect_template_needs(RootCauseLower),
    DependencyPins = detect_dependency_needs(RootCauseLower),

    #prevention_delta{
        shacl_additions = ShaclAdditions,
        test_additions = TestAdditions,
        template_improvements = TemplateImprovements,
        dependency_pins = DependencyPins
    }.

%% @private Detect SHACL shape additions needed
-spec detect_shacl_needs(string()) -> [binary()].
detect_shacl_needs(RootCause) ->
    Patterns = [
        {"shacl", <<"Add SHACL shape for validation">>},
        {"validation", <<"Add SHACL shape for required property validation">>},
        {"invalid data", <<"Add SHACL datatype constraint">>},
        {"missing property", <<"Add SHACL sh:minCount constraint">>},
        {"type mismatch", <<"Add SHACL sh:class or sh:datatype constraint">>},
        {"cardinality", <<"Add SHACL sh:minCount and sh:maxCount constraints">>},
        {"format", <<"Add SHACL sh:pattern constraint for format validation">>},
        {"range", <<"Add SHACL sh:minInclusive and sh:maxInclusive constraints">>}
    ],

    lists:filtermap(
        fun({Pattern, Shape}) ->
            case string:str(RootCause, Pattern) of
                0 -> false;
                _ -> {true, Shape}
            end
        end,
        Patterns
    ).

%% @private Detect test additions needed
-spec detect_test_needs(string()) -> [binary()].
detect_test_needs(RootCause) ->
    Patterns = [
        {"race condition", <<"Add concurrency test with multiple processes">>},
        {"edge case", <<"Add boundary value test cases">>},
        {"error handling", <<"Add negative test cases for error paths">>},
        {"timeout", <<"Add timeout handling test">>},
        {"memory leak", <<"Add memory usage monitoring test">>},
        {"non-deterministic", <<"Add property-based test with random inputs">>},
        {"integration", <<"Add integration test for component interaction">>}
    ],

    lists:filtermap(
        fun({Pattern, Test}) ->
            case string:str(RootCause, Pattern) of
                0 -> false;
                _ -> {true, Test}
            end
        end,
        Patterns
    ).

%% @private Detect template improvements needed
-spec detect_template_needs(string()) -> [binary()].
detect_template_needs(RootCause) ->
    Patterns = [
        {"boilerplate", <<"Add template helper for repeated code patterns">>},
        {"inconsistent", <<"Standardize template structure across modules">>},
        {"hard-coded", <<"Extract hard-coded values to template variables">>},
        {"duplicated", <<"Create reusable template component">>},
        {"complex", <<"Simplify template logic with better abstractions">>}
    ],

    lists:filtermap(
        fun({Pattern, Improvement}) ->
            case string:str(RootCause, Pattern) of
                0 -> false;
                _ -> {true, Improvement}
            end
        end,
        Patterns
    ).

%% @private Detect dependency pinning needs
-spec detect_dependency_needs(string()) -> #{binary() => binary()}.
detect_dependency_needs(RootCause) ->
    %% Pattern: "dependency X version Y caused..."
    case re:run(RootCause, "dependency ([a-z_]+) version ([0-9.]+)", [global, {capture, all_but_first, list}]) of
        {match, [[Dep, Version] | _]} ->
            #{list_to_binary(Dep) => list_to_binary(Version)};
        nomatch ->
            #{}
    end.

%% @private Generate receipt for root cause analysis
-spec generate_receipt_impl(#five_whys{}) -> map().
generate_receipt_impl(Analysis) ->
    #{
        receipt_type => <<"root_cause_analysis">>,
        receipt_version => <<"1.0.0">>,
        analysis_id => Analysis#five_whys.analysis_id,
        andon_event_id => Analysis#five_whys.andon_event_id,
        timestamp => iso8601_timestamp(Analysis#five_whys.finalized_at),

        problem_statement => Analysis#five_whys.problem,

        five_whys_chain => #{
            why_1 => #{
                question => <<"Why did this problem occur?">>,
                answer => Analysis#five_whys.why_1
            },
            why_2 => #{
                question => <<"Why did that happen?">>,
                answer => Analysis#five_whys.why_2
            },
            why_3 => #{
                question => <<"Why did that happen?">>,
                answer => Analysis#five_whys.why_3
            },
            why_4 => #{
                question => <<"Why did that happen?">>,
                answer => Analysis#five_whys.why_4
            },
            why_5 => #{
                question => <<"Why did that happen?">>,
                answer => Analysis#five_whys.why_5
            }
        },

        root_cause => Analysis#five_whys.root_cause,
        prevention_action => Analysis#five_whys.prevention_action,

        prevention_delta => prevention_delta_to_map(
            generate_prevention_actions_impl(Analysis#five_whys.root_cause)
        ),

        timeline => #{
            created_at => iso8601_timestamp(Analysis#five_whys.created_at),
            updated_at => iso8601_timestamp(Analysis#five_whys.updated_at),
            finalized_at => iso8601_timestamp(Analysis#five_whys.finalized_at),
            duration_ms => Analysis#five_whys.finalized_at - Analysis#five_whys.created_at
        },

        status => Analysis#five_whys.status,

        %% Link to TCPS ontology
        ontology_ref => <<"tcps:RootCauseAnalysis">>,
        andon_event_ref => iolist_to_binary([<<"tcps:AndonEvent/">>, Analysis#five_whys.andon_event_id])
    }.

%% @private Convert prevention delta to map
-spec prevention_delta_to_map(#prevention_delta{}) -> map().
prevention_delta_to_map(Delta) ->
    #{
        shacl_additions => Delta#prevention_delta.shacl_additions,
        test_additions => Delta#prevention_delta.test_additions,
        template_improvements => Delta#prevention_delta.template_improvements,
        dependency_pins => Delta#prevention_delta.dependency_pins
    }.

%% @private Convert five_whys record to map
-spec five_whys_to_map(#five_whys{}) -> map().
five_whys_to_map(Analysis) ->
    #{
        analysis_id => Analysis#five_whys.analysis_id,
        andon_event_id => Analysis#five_whys.andon_event_id,
        problem => Analysis#five_whys.problem,
        why_1 => Analysis#five_whys.why_1,
        why_2 => Analysis#five_whys.why_2,
        why_3 => Analysis#five_whys.why_3,
        why_4 => Analysis#five_whys.why_4,
        why_5 => Analysis#five_whys.why_5,
        root_cause => Analysis#five_whys.root_cause,
        prevention_action => Analysis#five_whys.prevention_action,
        created_at => Analysis#five_whys.created_at,
        updated_at => Analysis#five_whys.updated_at,
        finalized_at => Analysis#five_whys.finalized_at,
        status => Analysis#five_whys.status
    }.

%% @private Convert millisecond timestamp to ISO8601 format
-spec iso8601_timestamp(integer() | undefined) -> binary() | undefined.
iso8601_timestamp(undefined) ->
    undefined;
iso8601_timestamp(Milliseconds) ->
    Seconds = Milliseconds div 1000,
    DateTime = calendar:system_time_to_universal_time(Seconds, second),
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                      [Year, Month, Day, Hour, Minute, Second])
    ).
