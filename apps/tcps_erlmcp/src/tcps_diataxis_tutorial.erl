%%%-------------------------------------------------------------------
%%% @doc TCPS Diataxis Tutorial Engine
%%% Learning-oriented content that takes learners by the hand through
%%% a series of steps to complete a project and gain basic competence.
%%%
%%% This module provides the "Tutorial" quadrant of Diataxis:
%%% - Step-by-step guided learning experiences
%%% - Safe environment for learning by doing
%%% - Clear learning outcomes
%%% - Progressive skill building
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_diataxis_tutorial).

-export([
    get_tutorial/1,
    list_tutorials/0,
    list_by_level/1,
    get_next_step/2,
    get_previous_step/2,
    complete_step/3,
    get_progress/2,
    validate_tutorial/1,
    start_tutorial/2,
    reset_progress/2
]).

-type tutorial_id() :: binary().
-type step_id() :: pos_integer().
-type user_id() :: binary().
-type level() :: beginner | intermediate | advanced.
-type step() :: #{
    step_number := step_id(),
    title := binary(),
    description := binary(),
    instructions := [binary()],
    code_example => binary(),
    expected_output => binary(),
    hints => [binary()],
    common_mistakes => [binary()],
    estimated_minutes := pos_integer()
}.
-type tutorial() :: #{
    id := tutorial_id(),
    title := binary(),
    level := level(),
    description := binary(),
    prerequisites := [binary()],
    learning_outcomes := [binary()],
    steps := [step()],
    total_duration_minutes := pos_integer(),
    tags := [binary()]
}.
-type progress() :: #{
    tutorial_id := tutorial_id(),
    user_id := user_id(),
    completed_steps := [step_id()],
    current_step := step_id(),
    started_at := integer(),
    last_activity := integer()
}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Get a specific tutorial by ID
-spec get_tutorial(tutorial_id()) -> {ok, tutorial()} | {error, not_found}.
get_tutorial(Id) ->
    case maps:find(Id, get_all_tutorials()) of
        {ok, Tutorial} -> {ok, Tutorial};
        error -> {error, not_found}
    end.

%% @doc List all available tutorials
-spec list_tutorials() -> [tutorial()].
list_tutorials() ->
    maps:values(get_all_tutorials()).

%% @doc List tutorials by difficulty level
-spec list_by_level(level()) -> [tutorial()].
list_by_level(Level) ->
    [T || T <- list_tutorials(), maps:get(level, T) =:= Level].

%% @doc Get the next step in a tutorial
-spec get_next_step(tutorial_id(), step_id()) -> {ok, step()} | {error, term()}.
get_next_step(TutorialId, CurrentStep) ->
    case get_tutorial(TutorialId) of
        {ok, Tutorial} ->
            Steps = maps:get(steps, Tutorial),
            NextStepNum = CurrentStep + 1,
            case lists:keyfind(NextStepNum, 1, [{maps:get(step_number, S), S} || S <- Steps]) of
                {_, NextStep} -> {ok, NextStep};
                false -> {error, no_next_step}
            end;
        {error, _} = Error -> Error
    end.

%% @doc Get the previous step in a tutorial
-spec get_previous_step(tutorial_id(), step_id()) -> {ok, step()} | {error, term()}.
get_previous_step(TutorialId, CurrentStep) ->
    case get_tutorial(TutorialId) of
        {ok, Tutorial} ->
            Steps = maps:get(steps, Tutorial),
            PrevStepNum = CurrentStep - 1,
            if
                PrevStepNum < 1 -> {error, no_previous_step};
                true ->
                    case lists:keyfind(PrevStepNum, 1, [{maps:get(step_number, S), S} || S <- Steps]) of
                        {_, PrevStep} -> {ok, PrevStep};
                        false -> {error, step_not_found}
                    end
            end;
        {error, _} = Error -> Error
    end.

%% @doc Mark a step as completed for a user
-spec complete_step(tutorial_id(), user_id(), step_id()) -> ok | {error, term()}.
complete_step(TutorialId, UserId, StepId) ->
    case get_progress(TutorialId, UserId) of
        {ok, Progress} ->
            CompletedSteps = maps:get(completed_steps, Progress),
            NewCompleted = lists:usort([StepId | CompletedSteps]),
            NewProgress = Progress#{
                completed_steps := NewCompleted,
                current_step := StepId + 1,
                last_activity := erlang:system_time(second)
            },
            store_progress(NewProgress);
        {error, not_found} ->
            {error, tutorial_not_started}
    end.

%% @doc Get user's progress on a tutorial
-spec get_progress(tutorial_id(), user_id()) -> {ok, progress()} | {error, not_found}.
get_progress(TutorialId, UserId) ->
    % In production, this would query a database
    % For now, use ETS or process dictionary
    case persistent_term:get({progress, TutorialId, UserId}, undefined) of
        undefined -> {error, not_found};
        Progress -> {ok, Progress}
    end.

%% @doc Validate a tutorial structure
-spec validate_tutorial(tutorial()) -> ok | {error, term()}.
validate_tutorial(Tutorial) ->
    RequiredKeys = [id, title, level, description, prerequisites, learning_outcomes, steps, total_duration_minutes, tags],
    case lists:all(fun(Key) -> maps:is_key(Key, Tutorial) end, RequiredKeys) of
        true ->
            validate_steps(maps:get(steps, Tutorial));
        false ->
            {error, missing_required_keys}
    end.

%% @doc Start a tutorial for a user
-spec start_tutorial(tutorial_id(), user_id()) -> {ok, progress()} | {error, term()}.
start_tutorial(TutorialId, UserId) ->
    case get_tutorial(TutorialId) of
        {ok, _Tutorial} ->
            Now = erlang:system_time(second),
            Progress = #{
                tutorial_id => TutorialId,
                user_id => UserId,
                completed_steps => [],
                current_step => 1,
                started_at => Now,
                last_activity => Now
            },
            store_progress(Progress),
            {ok, Progress};
        {error, _} = Error -> Error
    end.

%% @doc Reset user's progress on a tutorial
-spec reset_progress(tutorial_id(), user_id()) -> ok.
reset_progress(TutorialId, UserId) ->
    persistent_term:erase({progress, TutorialId, UserId}),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
store_progress(Progress) ->
    TutorialId = maps:get(tutorial_id, Progress),
    UserId = maps:get(user_id, Progress),
    persistent_term:put({progress, TutorialId, UserId}, Progress),
    ok.

%% @private
validate_steps([]) -> ok;
validate_steps([Step | Rest]) ->
    RequiredKeys = [step_number, title, description, instructions, estimated_minutes],
    case lists:all(fun(Key) -> maps:is_key(Key, Step) end, RequiredKeys) of
        true -> validate_steps(Rest);
        false -> {error, invalid_step}
    end.

%% @private
get_all_tutorials() ->
    #{
        <<"getting_started_tcps">> => #{
            id => <<"getting_started_tcps">>,
            title => <<"Getting Started with TCPS">>,
            level => beginner,
            description => <<"Learn the basics of Toyota Continuous Production System for code">>,
            prerequisites => [<<"Basic Erlang knowledge">>, <<"Git familiarity">>],
            learning_outcomes => [
                <<"Understand TCPS core concepts">>,
                <<"Set up your first TCPS project">>,
                <<"Create and verify work orders">>,
                <<"Use quality gates effectively">>
            ],
            steps => [
                #{
                    step_number => 1,
                    title => <<"Install TCPS">>,
                    description => <<"Set up TCPS in your Erlang project">>,
                    instructions => [
                        <<"Add erlmcp to your rebar.config dependencies">>,
                        <<"Run 'rebar3 compile' to fetch and build">>,
                        <<"Verify installation with 'rebar3 tcps help'">>
                    ],
                    code_example => <<"{deps, [{erlmcp, {git, \"https://github.com/your/erlmcp.git\", {branch, \"main\"}}}]}.">>,
                    expected_output => <<"TCPS commands available...">>,
                    hints => [<<"Make sure you have Erlang 24+ installed">>],
                    common_mistakes => [<<"Forgetting to run rebar3 compile after adding dependency">>],
                    estimated_minutes => 5
                },
                #{
                    step_number => 2,
                    title => <<"Create Your First Work Order">>,
                    description => <<"Learn how to create a work order in TCPS">>,
                    instructions => [
                        <<"Open Erlang shell with 'rebar3 shell'">>,
                        <<"Create a work order: tcps_work_order:create(#{})">>
                    ],
                    estimated_minutes => 10
                },
                #{
                    step_number => 3,
                    title => <<"Set Up Quality Gates">>,
                    description => <<"Configure quality gates for your project">>,
                    instructions => [
                        <<"Create tcps.config in your project root">>,
                        <<"Define quality thresholds">>,
                        <<"Run 'rebar3 tcps quality check'">>
                    ],
                    estimated_minutes => 15
                }
            ],
            total_duration_minutes => 30,
            tags => [<<"beginner">>, <<"setup">>, <<"basics">>]
        },

        <<"mcp_integration">> => #{
            id => <<"mcp_integration">>,
            title => <<"Integrating TCPS with MCP Servers">>,
            level => intermediate,
            description => <<"Learn to expose TCPS functionality via MCP protocol">>,
            prerequisites => [<<"Completed Getting Started tutorial">>, <<"Understanding of MCP protocol">>],
            learning_outcomes => [
                <<"Set up an MCP server for TCPS">>,
                <<"Expose TCPS tools via MCP">>,
                <<"Connect AI agents to TCPS">>,
                <<"Monitor MCP interactions">>
            ],
            steps => [
                #{
                    step_number => 1,
                    title => <<"Start the MCP Server">>,
                    description => <<"Launch TCPS MCP server">>,
                    instructions => [
                        <<"Start server: tcps_mcp_server:start_link()">>,
                        <<"Verify server is listening">>,
                        <<"Test with mcp_client">>
                    ],
                    estimated_minutes => 10
                },
                #{
                    step_number => 2,
                    title => <<"Register TCPS Tools">>,
                    description => <<"Make TCPS operations available as MCP tools">>,
                    instructions => [
                        <<"Register work_order tool">>,
                        <<"Register quality_gate tool">>,
                        <<"Test tool discovery">>
                    ],
                    estimated_minutes => 15
                },
                #{
                    step_number => 3,
                    title => <<"Connect an AI Agent">>,
                    description => <<"Connect Claude or another AI to your MCP server">>,
                    instructions => [
                        <<"Configure AI agent with MCP endpoint">>,
                        <<"Test tool invocation">>,
                        <<"Review telemetry data">>
                    ],
                    estimated_minutes => 20
                }
            ],
            total_duration_minutes => 45,
            tags => [<<"mcp">>, <<"integration">>, <<"ai">>]
        },

        <<"simulator_scenarios">> => #{
            id => <<"simulator_scenarios">>,
            title => <<"TCPS Simulator: Practice Scenarios">>,
            level => advanced,
            description => <<"Master TCPS by practicing with realistic simulation scenarios">>,
            prerequisites => [
                <<"Completed MCP Integration tutorial">>,
                <<"Understanding of Lean manufacturing">>,
                <<"Experience with quality systems">>
            ],
            learning_outcomes => [
                <<"Run production-like simulations">>,
                <<"Handle defect scenarios">>,
                <<"Optimize throughput">>,
                <<"Analyze telemetry data">>
            ],
            steps => [
                #{
                    step_number => 1,
                    title => <<"Basic Production Run">>,
                    description => <<"Simulate a simple production scenario">>,
                    instructions => [
                        <<"Start simulator: tcps_simulator:start_scenario(basic_production)">>,
                        <<"Observe work orders flowing through pipeline">>,
                        <<"Check quality gates">>
                    ],
                    estimated_minutes => 15
                },
                #{
                    step_number => 2,
                    title => <<"Handling Defects">>,
                    description => <<"Simulate defect detection and andon response">>,
                    instructions => [
                        <<"Run defect scenario">>,
                        <<"Trigger andon alert">>,
                        <<"Perform root cause analysis">>,
                        <<"Verify corrective action">>
                    ],
                    estimated_minutes => 25
                },
                #{
                    step_number => 3,
                    title => <<"Performance Optimization">>,
                    description => <<"Optimize simulated production throughput">>,
                    instructions => [
                        <<"Run high-load scenario">>,
                        <<"Identify bottlenecks from telemetry">>,
                        <<"Adjust WIP limits">>,
                        <<"Measure improvement">>
                    ],
                    estimated_minutes => 30
                }
            ],
            total_duration_minutes => 70,
            tags => [<<"simulator">>, <<"advanced">>, <<"optimization">>]
        }
    }.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_tutorial_test() ->
    {ok, Tutorial} = get_tutorial(<<"getting_started_tcps">>),
    ?assertEqual(<<"Getting Started with TCPS">>, maps:get(title, Tutorial)),
    ?assertEqual(beginner, maps:get(level, Tutorial)),
    ?assertEqual(3, length(maps:get(steps, Tutorial))).

list_by_level_test() ->
    Beginner = list_by_level(beginner),
    ?assertEqual(1, length(Beginner)),
    Intermediate = list_by_level(intermediate),
    ?assertEqual(1, length(Intermediate)),
    Advanced = list_by_level(advanced),
    ?assertEqual(1, length(Advanced)).

get_next_step_test() ->
    {ok, Step2} = get_next_step(<<"getting_started_tcps">>, 1),
    ?assertEqual(2, maps:get(step_number, Step2)),
    ?assertEqual(<<"Create Your First Work Order">>, maps:get(title, Step2)).

get_previous_step_test() ->
    {ok, Step1} = get_previous_step(<<"getting_started_tcps">>, 2),
    ?assertEqual(1, maps:get(step_number, Step1)),
    {error, no_previous_step} = get_previous_step(<<"getting_started_tcps">>, 1).

start_tutorial_test() ->
    UserId = <<"user123">>,
    {ok, Progress} = start_tutorial(<<"getting_started_tcps">>, UserId),
    ?assertEqual(<<"getting_started_tcps">>, maps:get(tutorial_id, Progress)),
    ?assertEqual(UserId, maps:get(user_id, Progress)),
    ?assertEqual(1, maps:get(current_step, Progress)),
    ?assertEqual([], maps:get(completed_steps, Progress)).

complete_step_test() ->
    UserId = <<"user456">>,
    {ok, _} = start_tutorial(<<"getting_started_tcps">>, UserId),
    ok = complete_step(<<"getting_started_tcps">>, UserId, 1),
    {ok, Progress} = get_progress(<<"getting_started_tcps">>, UserId),
    ?assertEqual([1], maps:get(completed_steps, Progress)),
    ?assertEqual(2, maps:get(current_step, Progress)).

validate_tutorial_test() ->
    {ok, Tutorial} = get_tutorial(<<"getting_started_tcps">>),
    ?assertEqual(ok, validate_tutorial(Tutorial)).

reset_progress_test() ->
    UserId = <<"user789">>,
    {ok, _} = start_tutorial(<<"getting_started_tcps">>, UserId),
    ok = reset_progress(<<"getting_started_tcps">>, UserId),
    ?assertEqual({error, not_found}, get_progress(<<"getting_started_tcps">>, UserId)).

-endif.
