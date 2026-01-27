%%%-----------------------------------------------------------------------------
%%% @doc TCPS Tutorial Step-by-Step Execution Engine
%%%
%%% Implements step-by-step tutorial execution with real TCPS integration,
%%% validation, hints, and progress checkpoints. Each tutorial is broken down
%%% into discrete, executable steps with clear learning outcomes.
%%%
%%% Features:
%%% - Step definition and metadata management
%%% - Interactive step execution with real TCPS modules
%%% - Code snippet generation and explanation
%%% - Context-sensitive hints and guidance
%%% - Checkpoint creation for progress tracking
%%% - Error handling with educational feedback
%%%
%%% Step Types:
%%% - explanation: Concept introduction (read-only)
%%% - demonstration: Watch system execute
%%% - hands_on: User executes with validation
%%% - challenge: User solves problem independently
%%% - reflection: Review and understanding check
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_tutorial_steps).

%% API exports
-export([
    get_first_step/1,
    get_next_step/2,
    get_step_info/2,
    execute_step/3,
    get_hint/2,
    get_code_snippet/2,
    create_checkpoint/3
]).

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type tutorial_id() :: atom().
-type step_id() :: binary().
-type step_type() :: explanation | demonstration | hands_on | challenge | reflection.

-type step_metadata() :: #{
    step_id := step_id(),
    tutorial_id := tutorial_id(),
    step_number := pos_integer(),
    title := binary(),
    type := step_type(),
    description := binary(),
    learning_outcome := binary(),
    estimated_time := pos_integer(),  % seconds
    hints := [binary()],
    code_snippets := [code_snippet()]
}.

-type code_snippet() :: #{
    language := binary(),
    code := binary(),
    explanation := binary(),
    executable := boolean()
}.

-type step_result() :: #{
    success := boolean(),
    output := term(),
    feedback := binary(),
    next_step := step_id() | completed,
    checkpoint := map() | undefined
}.

-export_type([step_metadata/0, step_result/0, code_snippet/0]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%% @doc Get first step for tutorial
-spec get_first_step(tutorial_id()) -> step_id().
get_first_step(TutorialId) ->
    Steps = get_tutorial_steps(TutorialId),
    case Steps of
        [First | _] -> maps:get(step_id, First);
        [] -> <<"completed">>
    end.

%% @doc Get next step in sequence
-spec get_next_step(tutorial_id(), step_id()) -> step_id() | completed.
get_next_step(TutorialId, CurrentStepId) ->
    Steps = get_tutorial_steps(TutorialId),
    StepIds = [maps:get(step_id, S) || S <- Steps],

    case find_index(CurrentStepId, StepIds) of
        {ok, Index} when Index < length(StepIds) ->
            lists:nth(Index + 1, StepIds);
        _ ->
            completed
    end.

%% @doc Get step metadata and information
-spec get_step_info(tutorial_id(), step_id()) -> step_metadata().
get_step_info(TutorialId, StepId) ->
    Steps = get_tutorial_steps(TutorialId),
    case lists:keyfind(StepId, 2, [S || S <- Steps]) of
        false -> #{error => step_not_found};
        Step -> Step
    end.

%% @doc Execute tutorial step with user input
-spec execute_step(tutorial_id(), step_id(), map()) -> {ok, step_result()} | {error, term()}.
execute_step(TutorialId, StepId, Input) ->
    try
        Result = case TutorialId of
            quality_gate -> execute_quality_gate_step(StepId, Input);
            kanban_wip -> execute_kanban_wip_step(StepId, Input);
            andon_event -> execute_andon_event_step(StepId, Input);
            five_whys -> execute_five_whys_step(StepId, Input);
            complete_workflow -> execute_complete_workflow_step(StepId, Input);
            _ -> {error, unknown_tutorial}
        end,

        case Result of
            {ok, _} -> Result;
            {error, Reason} -> {error, Reason}
        end
    catch
        Error:Reason2:Stacktrace ->
            io:format("Step execution error: ~p:~p~n~p~n", [Error, Reason2, Stacktrace]),
            {error, {execution_failed, Reason2}}
    end.

%% @doc Get hint for current step
-spec get_hint(tutorial_id(), step_id()) -> binary().
get_hint(TutorialId, StepId) ->
    StepInfo = get_step_info(TutorialId, StepId),
    Hints = maps:get(hints, StepInfo, []),
    case Hints of
        [First | _] -> First;
        [] -> <<"No hints available for this step.">>
    end.

%% @doc Get code snippet for step
-spec get_code_snippet(tutorial_id(), step_id()) -> [code_snippet()].
get_code_snippet(TutorialId, StepId) ->
    StepInfo = get_step_info(TutorialId, StepId),
    maps:get(code_snippets, StepInfo, []).

%% @doc Create progress checkpoint
-spec create_checkpoint(tutorial_id(), step_id(), map()) -> map().
create_checkpoint(TutorialId, StepId, State) ->
    #{
        tutorial_id => TutorialId,
        step_id => StepId,
        timestamp => erlang:timestamp(),
        state => State,
        checkpoint_type => progress
    }.

%%%=============================================================================
%%% Tutorial Step Definitions
%%%=============================================================================

%% @private Get steps for quality_gate tutorial
get_tutorial_steps(quality_gate) ->
    [
        #{
            step_id => <<"qg_step_1">>,
            tutorial_id => quality_gate,
            step_number => 1,
            title => <<"Understanding Quality Gates">>,
            type => explanation,
            description => <<"Quality gates are automated checkpoints that ensure code meets standards before proceeding. In TCPS, quality gates implement Jidoka (built-in quality).">>,
            learning_outcome => <<"Understand the purpose and types of quality gates in TCPS">>,
            estimated_time => 120,
            hints => [<<"Quality gates prevent defects from progressing to later stages">>],
            code_snippets => []
        },
        #{
            step_id => <<"qg_step_2">>,
            tutorial_id => quality_gate,
            step_number => 2,
            title => <<"Start Quality Gates Service">>,
            type => demonstration,
            description => <<"Watch how the quality gates service starts and initializes">>,
            learning_outcome => <<"Understand quality gate service lifecycle">>,
            estimated_time => 60,
            hints => [<<"The service uses gen_server for state management">>],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"tcps_quality_gates:start_link()">>,
                explanation => <<"Starts the quality gates gen_server">>,
                executable => true
            }]
        },
        #{
            step_id => <<"qg_step_3">>,
            tutorial_id => quality_gate,
            step_number => 3,
            title => <<"Run Your First Quality Gate">>,
            type => hands_on,
            description => <<"Execute a compilation quality gate check">>,
            learning_outcome => <<"Successfully run and interpret quality gate results">>,
            estimated_time => 180,
            hints => [
                <<"Use tcps_quality_gates:check_gate/2">>,
                <<"SKU ID identifies the code unit being validated">>,
                <<"Gate returns {pass, Receipt} or {fail, Violations}">>
            ],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"Result = tcps_quality_gates:check_gate(compilation, <<\"sku_demo_001\">>)">>,
                explanation => <<"Check compilation gate for a demo SKU">>,
                executable => true
            }]
        },
        #{
            step_id => <<"qg_step_4">>,
            tutorial_id => quality_gate,
            step_number => 4,
            title => <<"Handle Gate Failures">>,
            type => hands_on,
            description => <<"Learn how to handle and respond to gate failures">>,
            learning_outcome => <<"Understand failure handling and remediation">>,
            estimated_time => 240,
            hints => [
                <<"Failed gates trigger Andon events">>,
                <<"Violations contain details about what failed">>
            ],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"case Result of\n    {pass, Receipt} -> io:format(\"Passed!~n\");\n    {fail, Violations} -> io:format(\"Failed: ~p~n\", [Violations])\nend">>,
                explanation => <<"Pattern match on gate result">>,
                executable => true
            }]
        },
        #{
            step_id => <<"qg_step_5">>,
            tutorial_id => quality_gate,
            step_number => 5,
            title => <<"Review and Reflect">>,
            type => reflection,
            description => <<"Review what you learned about quality gates">>,
            learning_outcome => <<"Consolidate understanding of quality gate concepts">>,
            estimated_time => 120,
            hints => [<<"Think about how quality gates prevent defects">>],
            code_snippets => []
        }
    ];

%% @private Get steps for kanban_wip tutorial
get_tutorial_steps(kanban_wip) ->
    [
        #{
            step_id => <<"kb_step_1">>,
            tutorial_id => kanban_wip,
            step_number => 1,
            title => <<"Kanban and WIP Limits Explained">>,
            type => explanation,
            description => <<"Kanban uses WIP (Work In Progress) limits to prevent overload. Each bucket (reliability, security, cost, compliance) has a WIP limit.">>,
            learning_outcome => <<"Understand Kanban WIP limit concepts">>,
            estimated_time => 180,
            hints => [<<"WIP limits prevent system overload and context switching">>],
            code_snippets => []
        },
        #{
            step_id => <<"kb_step_2">>,
            tutorial_id => kanban_wip,
            step_number => 2,
            title => <<"Start Kanban Service">>,
            type => demonstration,
            description => <<"Initialize the Kanban service with default WIP limits">>,
            learning_outcome => <<"Start and configure Kanban service">>,
            estimated_time => 90,
            hints => [<<"Default WIP limit is 5 per bucket">>],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"tcps_kanban:start_link()">>,
                explanation => <<"Start Kanban gen_server">>,
                executable => true
            }]
        },
        #{
            step_id => <<"kb_step_3">>,
            tutorial_id => kanban_wip,
            step_number => 3,
            title => <<"Check WIP Status">>,
            type => hands_on,
            description => <<"Check current WIP for reliability bucket">>,
            learning_outcome => <<"Query and interpret WIP status">>,
            estimated_time => 120,
            hints => [
                <<"Use tcps_kanban:get_wip_status/1">>,
                <<"Returns current, limit, available, and utilization">>
            ],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"Status = tcps_kanban:get_wip_status(reliability)">>,
                explanation => <<"Get WIP status for reliability bucket">>,
                executable => true
            }]
        },
        #{
            step_id => <<"kb_step_4">>,
            tutorial_id => kanban_wip,
            step_number => 4,
            title => <<"Set WIP Limits">>,
            type => hands_on,
            description => <<"Configure custom WIP limit for security bucket">>,
            learning_outcome => <<"Configure WIP limits dynamically">>,
            estimated_time => 150,
            hints => [
                <<"Use tcps_kanban:set_wip_limit/2">>,
                <<"Set security to 3 for stricter control">>
            ],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"ok = tcps_kanban:set_wip_limit(security, 3)">>,
                explanation => <<"Set security bucket WIP limit to 3">>,
                executable => true
            }]
        },
        #{
            step_id => <<"kb_step_5">>,
            tutorial_id => kanban_wip,
            step_number => 5,
            title => <<"Process Pull Signal">>,
            type => hands_on,
            description => <<"Send a pull signal and observe WIP limit enforcement">>,
            learning_outcome => <<"Process pull signals with WIP validation">>,
            estimated_time => 240,
            hints => [
                <<"Use tcps_kanban:process_pull_signal/1">>,
                <<"Pull signal includes bucket and payload">>
            ],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"Signal = #{bucket => reliability, priority => 5, payload => #{type => bug_fix}},\nResult = tcps_kanban:process_pull_signal(Signal)">>,
                explanation => <<"Process a reliability pull signal">>,
                executable => true
            }]
        },
        #{
            step_id => <<"kb_step_6">>,
            tutorial_id => kanban_wip,
            step_number => 6,
            title => <<"Handle WIP Limit Reached">>,
            type => challenge,
            description => <<"Fill the WIP limit and observe refusal behavior">>,
            learning_outcome => <<"Understand WIP limit enforcement">>,
            estimated_time => 300,
            hints => [
                <<"Send multiple pull signals to exceed limit">>,
                <<"System returns refusal receipt when limit reached">>
            ],
            code_snippets => []
        }
    ];

%% @private Get steps for andon_event tutorial
get_tutorial_steps(andon_event) ->
    [
        #{
            step_id => <<"ae_step_1">>,
            tutorial_id => andon_event,
            step_number => 1,
            title => <<"Andon Stop-the-Line Principles">>,
            type => explanation,
            description => <<"Andon empowers anyone to stop production when defects are detected. This prevents defects from propagating.">>,
            learning_outcome => <<"Understand Andon stop-the-line philosophy">>,
            estimated_time => 180,
            hints => [<<"Andon is about immediate problem visibility and resolution">>],
            code_snippets => []
        },
        #{
            step_id => <<"ae_step_2">>,
            tutorial_id => andon_event,
            step_number => 2,
            title => <<"Trigger Test Failure Andon">>,
            type => hands_on,
            description => <<"Trigger an Andon event for a test failure">>,
            learning_outcome => <<"Create Andon events programmatically">>,
            estimated_time => 240,
            hints => [
                <<"Use tcps_andon:trigger_andon/2">>,
                <<"Provide failure type and context">>
            ],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"Context = #{sku_id => <<\"sku_001\">>, stage => testing, details => #{test => \"unit_test_1\", error => \"assertion_failed\"}},\n{ok, EventId} = tcps_andon:trigger_andon(test_failure, Context)">>,
                explanation => <<"Trigger Andon for test failure">>,
                executable => true
            }]
        },
        #{
            step_id => <<"ae_step_3">>,
            tutorial_id => andon_event,
            step_number => 3,
            title => <<"Check Stage Blocking">>,
            type => hands_on,
            description => <<"Verify that subsequent stages are blocked">>,
            learning_outcome => <<"Understand stop-the-line enforcement">>,
            estimated_time => 180,
            hints => [
                <<"Use tcps_andon:can_proceed_to_stage/2">>,
                <<"Active Andon events block progression">>
            ],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"CanProceed = tcps_andon:can_proceed_to_stage(<<\"sku_001\">>, validation)">>,
                explanation => <<"Check if SKU can proceed to validation stage">>,
                executable => true
            }]
        },
        #{
            step_id => <<"ae_step_4">>,
            tutorial_id => andon_event,
            step_number => 4,
            title => <<"Resolve Andon Event">>,
            type => hands_on,
            description => <<"Resolve the Andon event with root cause">>,
            learning_outcome => <<"Execute Andon resolution workflow">>,
            estimated_time => 300,
            hints => [
                <<"Use tcps_andon:resolve_andon/2">>,
                <<"Provide resolution details and root cause">>
            ],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"Resolution = #{root_cause => <<\"Missing null check\">>, actions_taken => <<\"Added validation\">>, prevention => <<\"Add null check pattern to linter\">>},\nok = tcps_andon:resolve_andon(EventId, Resolution)">>,
                explanation => <<"Resolve Andon with comprehensive details">>,
                executable => true
            }]
        }
    ];

%% @private Get steps for five_whys tutorial
get_tutorial_steps(five_whys) ->
    [
        #{
            step_id => <<"fw_step_1">>,
            tutorial_id => five_whys,
            step_number => 1,
            title => <<"5 Whys Methodology">>,
            type => explanation,
            description => <<"The 5 Whys technique asks 'why' repeatedly (5 times) to drill down to the root cause of a problem.">>,
            learning_outcome => <<"Understand 5 Whys root cause analysis">>,
            estimated_time => 240,
            hints => [<<"Each 'why' should lead closer to the systemic root cause">>],
            code_snippets => []
        },
        #{
            step_id => <<"fw_step_2">>,
            tutorial_id => five_whys,
            step_number => 2,
            title => <<"Start Root Cause Analysis">>,
            type => hands_on,
            description => <<"Start 5 Whys analysis for an Andon event">>,
            learning_outcome => <<"Initialize root cause analysis">>,
            estimated_time => 180,
            hints => [
                <<"Use tcps_root_cause:start_analysis/2">>,
                <<"Provide Andon event ID and problem statement">>
            ],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"{ok, AnalysisId} = tcps_root_cause:start_analysis(EventId, <<\"Test failure in user authentication\">>)">>,
                explanation => <<"Start analysis with problem statement">>,
                executable => true
            }]
        },
        #{
            step_id => <<"fw_step_3">>,
            tutorial_id => five_whys,
            step_number => 3,
            title => <<"Answer the Five Whys">>,
            type => hands_on,
            description => <<"Progressively answer each 'why' question">>,
            learning_outcome => <<"Execute systematic root cause investigation">>,
            estimated_time => 600,
            hints => [
                <<"Use tcps_root_cause:add_why/3">>,
                <<"Each answer should lead to the next 'why'">>
            ],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"ok = tcps_root_cause:add_why(AnalysisId, 1, <<\"Authentication token was null\">>),\nok = tcps_root_cause:add_why(AnalysisId, 2, <<\"Token generation failed\">>),\nok = tcps_root_cause:add_why(AnalysisId, 3, <<\"Config file was missing\">>),\nok = tcps_root_cause:add_why(AnalysisId, 4, <<\"Deployment script didn't copy configs\">>),\nok = tcps_root_cause:add_why(AnalysisId, 5, <<\"No config deployment validation step\">>)">>,
                explanation => <<"Answer all five whys">>,
                executable => true
            }]
        },
        #{
            step_id => <<"fw_step_4">>,
            tutorial_id => five_whys,
            step_number => 4,
            title => <<"Finalize Root Cause">>,
            type => hands_on,
            description => <<"Document root cause and prevention actions">>,
            learning_outcome => <<"Complete analysis with actionable outcomes">>,
            estimated_time => 300,
            hints => [
                <<"Use tcps_root_cause:finalize_analysis/3">>,
                <<"Root cause should be systemic, prevention should be concrete">>
            ],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"{ok, Receipt} = tcps_root_cause:finalize_analysis(AnalysisId, <<\"Missing deployment validation\">>, <<\"Add config validation to deployment gate\">>)">>,
                explanation => <<"Finalize with root cause and prevention">>,
                executable => true
            }]
        }
    ];

%% @private Get steps for complete_workflow tutorial
get_tutorial_steps(complete_workflow) ->
    [
        #{
            step_id => <<"cw_step_1">>,
            tutorial_id => complete_workflow,
            step_number => 1,
            title => <<"End-to-End TCPS Workflow">>,
            type => explanation,
            description => <<"Complete workflow: Pull signal → Work order → Quality gates → Andon (if failure) → 5 Whys → Resolution → Completion">>,
            learning_outcome => <<"Understand complete TCPS production flow">>,
            estimated_time => 240,
            hints => [<<"Each step integrates multiple TCPS concepts">>],
            code_snippets => []
        },
        #{
            step_id => <<"cw_step_2">>,
            tutorial_id => complete_workflow,
            step_number => 2,
            title => <<"Create Work Order from Pull Signal">>,
            type => hands_on,
            description => <<"Process pull signal to create work order">>,
            learning_outcome => <<"Initiate work through demand signals">>,
            estimated_time => 240,
            hints => [<<"Use tcps_work_order:create_from_github/1">>],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"PullSignal = #{source => github, type => bug_report, priority => 7, payload => #{issue_id => 123}},\n{ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal)">>,
                explanation => <<"Create work order from GitHub issue">>,
                executable => true
            }]
        },
        #{
            step_id => <<"cw_step_3">>,
            tutorial_id => complete_workflow,
            step_number => 3,
            title => <<"Start Work Order">>,
            type => hands_on,
            description => <<"Begin work on the order">>,
            learning_outcome => <<"Transition work order through lifecycle">>,
            estimated_time => 180,
            hints => [<<"Use tcps_work_order:start_work_order/1">>],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"ok = tcps_work_order:start_work_order(WorkOrderId)">>,
                explanation => <<"Start work order execution">>,
                executable => true
            }]
        },
        #{
            step_id => <<"cw_step_4">>,
            tutorial_id => complete_workflow,
            step_number => 4,
            title => <<"Run Quality Gate Sequence">>,
            type => hands_on,
            description => <<"Execute all quality gates">>,
            learning_outcome => <<"Validate work through quality gates">>,
            estimated_time => 360,
            hints => [
                <<"Use tcps_quality_gates:check_all_gates/1">>,
                <<"All gates must pass for completion">>
            ],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"Results = tcps_quality_gates:check_all_gates(WorkOrderId)">>,
                explanation => <<"Run complete gate sequence">>,
                executable => true
            }]
        },
        #{
            step_id => <<"cw_step_5">>,
            tutorial_id => complete_workflow,
            step_number => 5,
            title => <<"Handle Failures with Andon + 5 Whys">>,
            type => challenge,
            description => <<"If gates fail, trigger Andon and perform 5 Whys">>,
            learning_outcome => <<"Integrate problem resolution workflow">>,
            estimated_time => 600,
            hints => [
                <<"Failed gates auto-trigger Andon">>,
                <<"Complete 5 Whys before resolving Andon">>
            ],
            code_snippets => []
        },
        #{
            step_id => <<"cw_step_6">>,
            tutorial_id => complete_workflow,
            step_number => 6,
            title => <<"Complete Work Order">>,
            type => hands_on,
            description => <<"Finalize work order with completion receipt">>,
            learning_outcome => <<"Complete end-to-end workflow">>,
            estimated_time => 240,
            hints => [<<"Use tcps_work_order:complete_work_order/2">>],
            code_snippets => [#{
                language => <<"erlang">>,
                code => <<"Deliverables = #{artifacts => [<<\"fixed_module.erl\">>], tests => [<<\"unit_tests.erl\">>]},\n{ok, Receipt} = tcps_work_order:complete_work_order(WorkOrderId, Deliverables)">>,
                explanation => <<"Complete work order with deliverables">>,
                executable => true
            }]
        }
    ];

get_tutorial_steps(_) ->
    [].

%%%=============================================================================
%%% Step Execution Functions
%%%=============================================================================

%% @private Execute quality_gate tutorial step
execute_quality_gate_step(<<"qg_step_1">>, _Input) ->
    {ok, #{
        success => true,
        output => <<"Concept understood">>,
        feedback => <<"Great! You now understand quality gate basics. Let's see them in action.">>,
        next_step => <<"qg_step_2">>,
        checkpoint => undefined
    }};

execute_quality_gate_step(<<"qg_step_2">>, _Input) ->
    case tcps_quality_gates:start_link() of
        {ok, _Pid} ->
            {ok, #{
                success => true,
                output => <<"Service started">>,
                feedback => <<"Quality gates service is now running!">>,
                next_step => <<"qg_step_3">>,
                checkpoint => #{service_started => true}
            }};
        {error, {already_started, _}} ->
            {ok, #{
                success => true,
                output => <<"Service already running">>,
                feedback => <<"Quality gates service is already active.">>,
                next_step => <<"qg_step_3">>,
                checkpoint => #{service_started => true}
            }};
        {error, Reason} ->
            {error, {service_start_failed, Reason}}
    end;

execute_quality_gate_step(<<"qg_step_3">>, Input) ->
    SkuId = maps:get(sku_id, Input, <<"tutorial_sku_001">>),
    Result = tcps_quality_gates:check_gate(compilation, SkuId),

    case Result of
        {pass, Receipt} ->
            {ok, #{
                success => true,
                output => Receipt,
                feedback => <<"Excellent! Your quality gate passed. Review the receipt for details.">>,
                next_step => <<"qg_step_4">>,
                checkpoint => #{gate_passed => true, receipt => Receipt}
            }};
        {fail, Violations} ->
            {ok, #{
                success => false,
                output => Violations,
                feedback => <<"Quality gate failed. This is expected for learning - let's handle it in the next step.">>,
                next_step => <<"qg_step_4">>,
                checkpoint => #{gate_failed => true, violations => Violations}
            }}
    end;

execute_quality_gate_step(<<"qg_step_4">>, Input) ->
    Handled = maps:get(failure_handled, Input, false),
    case Handled of
        true ->
            {ok, #{
                success => true,
                output => <<"Failure handling demonstrated">>,
                feedback => <<"Perfect! You understand how to handle gate failures.">>,
                next_step => <<"qg_step_5">>,
                checkpoint => undefined
            }};
        false ->
            {ok, #{
                success => false,
                output => <<"Pattern matching needed">>,
                feedback => <<"Use pattern matching on {pass, Receipt} or {fail, Violations}">>,
                next_step => <<"qg_step_4">>,
                checkpoint => undefined
            }}
    end;

execute_quality_gate_step(<<"qg_step_5">>, _Input) ->
    {ok, #{
        success => true,
        output => <<"Tutorial completed">>,
        feedback => <<"Congratulations! You've completed the Quality Gate tutorial.">>,
        next_step => completed,
        checkpoint => #{tutorial_completed => true}
    }};

execute_quality_gate_step(_, _) ->
    {error, unknown_step}.

%% @private Execute kanban_wip tutorial step
execute_kanban_wip_step(<<"kb_step_1">>, _Input) ->
    {ok, #{
        success => true,
        output => <<"Concept understood">>,
        feedback => <<"Great! WIP limits prevent system overload.">>,
        next_step => <<"kb_step_2">>,
        checkpoint => undefined
    }};

execute_kanban_wip_step(<<"kb_step_2">>, _Input) ->
    case tcps_kanban:start_link() of
        {ok, _Pid} ->
            {ok, #{
                success => true,
                output => <<"Kanban service started">>,
                feedback => <<"Kanban service is running with default WIP limits.">>,
                next_step => <<"kb_step_3">>,
                checkpoint => #{service_started => true}
            }};
        {error, {already_started, _}} ->
            {ok, #{
                success => true,
                output => <<"Service already running">>,
                feedback => <<"Kanban service is already active.">>,
                next_step => <<"kb_step_3">>,
                checkpoint => #{service_started => true}
            }};
        {error, Reason} ->
            {error, {service_start_failed, Reason}}
    end;

execute_kanban_wip_step(<<"kb_step_3">>, _Input) ->
    Status = tcps_kanban:get_wip_status(reliability),
    {ok, #{
        success => true,
        output => Status,
        feedback => io_lib:format("WIP Status: ~p/~p used (~.1f% utilization)",
            [maps:get(current, Status), maps:get(limit, Status), maps:get(utilization, Status) * 100]),
        next_step => <<"kb_step_4">>,
        checkpoint => #{wip_checked => true}
    }};

execute_kanban_wip_step(<<"kb_step_4">>, Input) ->
    Limit = maps:get(limit, Input, 3),
    ok = tcps_kanban:set_wip_limit(security, Limit),
    {ok, #{
        success => true,
        output => <<"Limit set">>,
        feedback => io_lib:format("Security WIP limit set to ~p", [Limit]),
        next_step => <<"kb_step_5">>,
        checkpoint => #{limit_set => true, new_limit => Limit}
    }};

execute_kanban_wip_step(<<"kb_step_5">>, Input) ->
    Signal = maps:get(pull_signal, Input, #{bucket => reliability, priority => 5, payload => #{type => bug_fix}}),
    case tcps_kanban:process_pull_signal(Signal) of
        {ok, WorkOrder} ->
            {ok, #{
                success => true,
                output => WorkOrder,
                feedback => <<"Pull signal processed! Work order created.">>,
                next_step => <<"kb_step_6">>,
                checkpoint => #{signal_processed => true, work_order => WorkOrder}
            }};
        {error, wip_limit_reached} ->
            {ok, #{
                success => true,
                output => <<"WIP limit reached">>,
                feedback => <<"WIP limit prevented new work - this is expected behavior!">>,
                next_step => <<"kb_step_6">>,
                checkpoint => #{wip_limit_enforced => true}
            }}
    end;

execute_kanban_wip_step(<<"kb_step_6">>, Input) ->
    LimitReached = maps:get(limit_reached, Input, false),
    case LimitReached of
        true ->
            {ok, #{
                success => true,
                output => <<"Challenge completed">>,
                feedback => <<"Excellent! You've seen WIP limits in action.">>,
                next_step => completed,
                checkpoint => #{tutorial_completed => true}
            }};
        false ->
            {ok, #{
                success => false,
                output => <<"Keep sending pull signals">>,
                feedback => <<"Send more pull signals until WIP limit is reached.">>,
                next_step => <<"kb_step_6">>,
                checkpoint => undefined
            }}
    end;

execute_kanban_wip_step(_, _) ->
    {error, unknown_step}.

%% @private Execute andon_event tutorial step
execute_andon_event_step(<<"ae_step_1">>, _Input) ->
    {ok, #{
        success => true,
        output => <<"Concept understood">>,
        feedback => <<"Andon empowers immediate problem resolution!">>,
        next_step => <<"ae_step_2">>,
        checkpoint => undefined
    }};

execute_andon_event_step(<<"ae_step_2">>, Input) ->
    Context = maps:get(context, Input, #{
        sku_id => <<"tutorial_sku_andon">>,
        stage => testing,
        details => #{test => <<"unit_test_demo">>, error => <<"assertion_failed">>}
    }),

    case tcps_andon:trigger_andon(test_failure, Context) of
        {ok, EventId} ->
            {ok, #{
                success => true,
                output => EventId,
                feedback => <<"Andon event triggered! Production is now stopped for this SKU.">>,
                next_step => <<"ae_step_3">>,
                checkpoint => #{andon_triggered => true, event_id => EventId}
            }};
        {error, Reason} ->
            {error, {andon_trigger_failed, Reason}}
    end;

execute_andon_event_step(<<"ae_step_3">>, Input) ->
    SkuId = maps:get(sku_id, Input, <<"tutorial_sku_andon">>),
    CanProceed = tcps_andon:can_proceed_to_stage(SkuId, validation),

    {ok, #{
        success => not CanProceed,
        output => #{can_proceed => CanProceed},
        feedback => case CanProceed of
            false -> <<"Correct! Stage is blocked due to active Andon event.">>;
            true -> <<"No active Andon - try triggering one first.">>
        end,
        next_step => <<"ae_step_4">>,
        checkpoint => #{blocking_verified => true}
    }};

execute_andon_event_step(<<"ae_step_4">>, Input) ->
    EventId = maps:get(event_id, Input),
    Resolution = maps:get(resolution, Input, #{
        root_cause => <<"Missing null check">>,
        actions_taken => <<"Added validation">>,
        prevention => <<"Add pattern to linter">>
    }),

    case tcps_andon:resolve_andon(EventId, Resolution) of
        ok ->
            {ok, #{
                success => true,
                output => <<"Andon resolved">>,
                feedback => <<"Perfect! Andon resolved. Production can resume.">>,
                next_step => completed,
                checkpoint => #{andon_resolved => true, tutorial_completed => true}
            }};
        {error, Reason} ->
            {error, {resolution_failed, Reason}}
    end;

execute_andon_event_step(_, _) ->
    {error, unknown_step}.

%% @private Execute five_whys tutorial step
execute_five_whys_step(<<"fw_step_1">>, _Input) ->
    {ok, #{
        success => true,
        output => <<"Concept understood">>,
        feedback => <<"The 5 Whys drill down to systemic root causes!">>,
        next_step => <<"fw_step_2">>,
        checkpoint => undefined
    }};

execute_five_whys_step(<<"fw_step_2">>, Input) ->
    AndonEventId = maps:get(andon_event_id, Input, <<"andon_tutorial_001">>),
    Problem = maps:get(problem, Input, <<"Test failure in authentication">>),

    case tcps_root_cause:start_analysis(AndonEventId, Problem) of
        {ok, AnalysisId} ->
            {ok, #{
                success => true,
                output => AnalysisId,
                feedback => <<"5 Whys analysis started. Now answer each 'why'.">>,
                next_step => <<"fw_step_3">>,
                checkpoint => #{analysis_started => true, analysis_id => AnalysisId}
            }};
        {error, Reason} ->
            {error, {analysis_start_failed, Reason}}
    end;

execute_five_whys_step(<<"fw_step_3">>, Input) ->
    AnalysisId = maps:get(analysis_id, Input),
    Whys = maps:get(whys, Input, []),

    case length(Whys) of
        5 ->
            lists:foreach(fun({N, Answer}) ->
                tcps_root_cause:add_why(AnalysisId, N, Answer)
            end, lists:zip(lists:seq(1, 5), Whys)),

            {ok, #{
                success => true,
                output => <<"All whys answered">>,
                feedback => <<"Great! You've drilled down through all 5 levels.">>,
                next_step => <<"fw_step_4">>,
                checkpoint => #{whys_completed => true, whys => Whys}
            }};
        N when N < 5 ->
            {ok, #{
                success => false,
                output => io_lib:format("~p whys answered", [N]),
                feedback => io_lib:format("Keep going - you need ~p more whys", [5 - N]),
                next_step => <<"fw_step_3">>,
                checkpoint => undefined
            }};
        _ ->
            {error, too_many_whys}
    end;

execute_five_whys_step(<<"fw_step_4">>, Input) ->
    AnalysisId = maps:get(analysis_id, Input),
    RootCause = maps:get(root_cause, Input, <<"Missing deployment validation">>),
    Prevention = maps:get(prevention, Input, <<"Add config validation to deployment gate">>),

    case tcps_root_cause:finalize_analysis(AnalysisId, RootCause, Prevention) of
        {ok, Receipt} ->
            {ok, #{
                success => true,
                output => Receipt,
                feedback => <<"Perfect! Root cause analysis complete with prevention actions.">>,
                next_step => completed,
                checkpoint => #{analysis_completed => true, tutorial_completed => true}
            }};
        {error, Reason} ->
            {error, {finalization_failed, Reason}}
    end;

execute_five_whys_step(_, _) ->
    {error, unknown_step}.

%% @private Execute complete_workflow tutorial step
execute_complete_workflow_step(<<"cw_step_1">>, _Input) ->
    {ok, #{
        success => true,
        output => <<"Workflow overview understood">>,
        feedback => <<"Let's execute a complete TCPS workflow!">>,
        next_step => <<"cw_step_2">>,
        checkpoint => undefined
    }};

execute_complete_workflow_step(<<"cw_step_2">>, Input) ->
    PullSignal = maps:get(pull_signal, Input, #{
        source => github,
        type => bug_report,
        priority => 7,
        payload => #{issue_id => 123, title => <<"Authentication bug">>}
    }),

    case tcps_work_order:create_work_order(PullSignal) of
        {ok, WorkOrderId} ->
            {ok, #{
                success => true,
                output => WorkOrderId,
                feedback => <<"Work order created from pull signal!">>,
                next_step => <<"cw_step_3">>,
                checkpoint => #{work_order_created => true, work_order_id => WorkOrderId}
            }};
        {error, Reason} ->
            {error, {work_order_creation_failed, Reason}}
    end;

execute_complete_workflow_step(<<"cw_step_3">>, Input) ->
    WorkOrderId = maps:get(work_order_id, Input),

    case tcps_work_order:start_work_order(WorkOrderId) of
        ok ->
            {ok, #{
                success => true,
                output => <<"Work started">>,
                feedback => <<"Work order in progress!">>,
                next_step => <<"cw_step_4">>,
                checkpoint => #{work_started => true}
            }};
        {error, Reason} ->
            {error, {work_start_failed, Reason}}
    end;

execute_complete_workflow_step(<<"cw_step_4">>, Input) ->
    WorkOrderId = maps:get(work_order_id, Input),

    Results = tcps_quality_gates:check_all_gates(WorkOrderId),
    AllPassed = lists:all(fun({_, {Result, _}}) -> Result =:= pass end, Results),

    {ok, #{
        success => true,
        output => Results,
        feedback => case AllPassed of
            true -> <<"All quality gates passed! Ready to complete.">>;
            false -> <<"Some gates failed - proceed to Andon resolution in next step.">>
        end,
        next_step => <<"cw_step_5">>,
        checkpoint => #{gates_run => true, results => Results, all_passed => AllPassed}
    }};

execute_complete_workflow_step(<<"cw_step_5">>, Input) ->
    AllPassed = maps:get(all_passed, Input, false),
    AndonResolved = maps:get(andon_resolved, Input, false),

    case {AllPassed, AndonResolved} of
        {true, _} ->
            {ok, #{
                success => true,
                output => <<"No failures - skip to completion">>,
                feedback => <<"All gates passed - no Andon needed.">>,
                next_step => <<"cw_step_6">>,
                checkpoint => undefined
            }};
        {false, true} ->
            {ok, #{
                success => true,
                output => <<"Andon resolved">>,
                feedback => <<"Great! Andon and 5 Whys completed.">>,
                next_step => <<"cw_step_6">>,
                checkpoint => #{andon_workflow_complete => true}
            }};
        {false, false} ->
            {ok, #{
                success => false,
                output => <<"Resolve Andon first">>,
                feedback => <<"Complete Andon event and 5 Whys analysis before proceeding.">>,
                next_step => <<"cw_step_5">>,
                checkpoint => undefined
            }}
    end;

execute_complete_workflow_step(<<"cw_step_6">>, Input) ->
    WorkOrderId = maps:get(work_order_id, Input),
    Deliverables = maps:get(deliverables, Input, #{
        artifacts => [<<"fixed_module.erl">>],
        tests => [<<"unit_tests.erl">>]
    }),

    case tcps_work_order:complete_work_order(WorkOrderId, Deliverables) of
        {ok, Receipt} ->
            {ok, #{
                success => true,
                output => Receipt,
                feedback => <<"Congratulations! Complete TCPS workflow executed successfully!">>,
                next_step => completed,
                checkpoint => #{workflow_completed => true, tutorial_completed => true}
            }};
        {error, Reason} ->
            {error, {completion_failed, Reason}}
    end;

execute_complete_workflow_step(_, _) ->
    {error, unknown_step}.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% @private Find index of element in list
find_index(Element, List) ->
    find_index(Element, List, 1).

find_index(_, [], _) ->
    not_found;
find_index(Element, [Element | _], Index) ->
    {ok, Index};
find_index(Element, [_ | Rest], Index) ->
    find_index(Element, Rest, Index + 1).
