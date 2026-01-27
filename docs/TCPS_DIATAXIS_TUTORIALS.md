# TCPS Diataxis Tutorial System

## Overview

The TCPS Diataxis Tutorial System provides **interactive, hands-on learning experiences** for the Toyota Cyber Production System (TCPS). Following the Diataxis framework's learning-oriented quadrant, these tutorials guide users through practical exercises with real TCPS integration.

### Key Features

- **5 Progressive Tutorials**: From beginner to advanced TCPS concepts
- **Interactive Execution**: Real TCPS module integration with live feedback
- **Progress Tracking**: Checkpoints, achievements, and learning paths
- **Validation System**: Automated step validation with hints and guidance
- **Multi-User Support**: Isolated sessions for concurrent learners

### Architecture

```
tcps_diataxis_tutorial (Orchestration)
    ├── Session Management
    ├── Progress Tracking
    └── Achievement System

tcps_tutorial_steps (Step Execution)
    ├── Step Definitions
    ├── Code Snippets
    └── Checkpoint Creation

tcps_tutorial_validation (Validation)
    ├── Step Validation
    ├── Prerequisite Checks
    └── Code Quality Assessment
```

---

## Available Tutorials

### 1. Your First Quality Gate (Beginner)

**Tutorial ID**: `quality_gate`
**Duration**: 15 minutes
**Prerequisites**: None

#### Learning Objectives

- Understand quality gate concepts and Jidoka principles
- Start and interact with quality gates service
- Execute quality gate checks
- Handle gate failures appropriately

#### Tutorial Steps

1. **Understanding Quality Gates** (Explanation)
   - Introduction to built-in quality (Jidoka)
   - Quality gate types and purposes

2. **Start Quality Gates Service** (Demonstration)
   - Watch service initialization
   - Understand gen_server lifecycle

3. **Run Your First Quality Gate** (Hands-on)
   - Execute compilation gate check
   - Interpret gate results

4. **Handle Gate Failures** (Hands-on)
   - Pattern match on success/failure
   - Respond to violations

5. **Review and Reflect** (Reflection)
   - Consolidate understanding

#### Example Usage

```erlang
%% Start tutorial
{ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(<<"user_001">>, quality_gate),

%% Get current step
{ok, StepInfo} = tcps_diataxis_tutorial:get_current_step(SessionId),

%% Execute step
{ok, Result} = tcps_diataxis_tutorial:execute_step(SessionId, #{}),

%% Get hint if needed
{ok, Hint} = tcps_diataxis_tutorial:get_hint(SessionId),

%% Validate step completion
{ok, ValidationResult} = tcps_diataxis_tutorial:validate_step(SessionId, #{
    gate_result => {pass, #{receipt_id => <<"test">>}},
    sku_id => <<"tutorial_sku_001">>
}),

%% Complete tutorial
ok = tcps_diataxis_tutorial:complete_tutorial(SessionId).
```

---

### 2. Kanban WIP Limits in Action (Beginner)

**Tutorial ID**: `kanban_wip`
**Duration**: 20 minutes
**Prerequisites**: None

#### Learning Objectives

- Understand Kanban WIP limit concepts
- Configure WIP limits for different buckets
- Process pull signals with WIP validation
- Handle WIP limit violations

#### Tutorial Steps

1. **Kanban and WIP Limits Explained** (Explanation)
2. **Start Kanban Service** (Demonstration)
3. **Check WIP Status** (Hands-on)
4. **Set WIP Limits** (Hands-on)
5. **Process Pull Signal** (Hands-on)
6. **Handle WIP Limit Reached** (Challenge)

#### Example Usage

```erlang
%% Start tutorial
{ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(<<"user_002">>, kanban_wip),

%% Execute step 3: Check WIP status
{ok, Result} = tcps_diataxis_tutorial:execute_step(SessionId, #{}),
#{
    success := true,
    output := #{current := 0, limit := 5, available := 5, utilization := 0.0}
} = Result,

%% Execute step 4: Set WIP limit
{ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{limit => 3}),

%% Execute step 5: Process pull signal
{ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{
    pull_signal => #{
        bucket => reliability,
        priority => 5,
        payload => #{type => bug_fix}
    }
}).
```

---

### 3. Triggering an Andon Event (Intermediate)

**Tutorial ID**: `andon_event`
**Duration**: 25 minutes
**Prerequisites**: `quality_gate`

#### Learning Objectives

- Understand Andon stop-the-line principles
- Trigger Andon events for failures
- Verify stage blocking enforcement
- Execute resolution workflow

#### Tutorial Steps

1. **Andon Stop-the-Line Principles** (Explanation)
2. **Trigger Test Failure Andon** (Hands-on)
3. **Check Stage Blocking** (Hands-on)
4. **Resolve Andon Event** (Hands-on)

#### Example Usage

```erlang
%% Start tutorial
{ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(<<"user_003">>, andon_event),

%% Skip to step 2: Trigger Andon
{ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{}),  % Step 1

{ok, Result2} = tcps_diataxis_tutorial:execute_step(SessionId, #{
    context => #{
        sku_id => <<"tutorial_sku_andon">>,
        stage => testing,
        details => #{
            test => <<"unit_test_demo">>,
            error => <<"assertion_failed">>
        }
    }
}),

EventId = maps:get(output, Result2),

%% Step 3: Verify blocking
{ok, Result3} = tcps_diataxis_tutorial:execute_step(SessionId, #{
    sku_id => <<"tutorial_sku_andon">>
}),

%% Step 4: Resolve Andon
{ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{
    event_id => EventId,
    resolution => #{
        root_cause => <<"Missing null check">>,
        actions_taken => <<"Added validation">>,
        prevention => <<"Add pattern to linter">>
    }
}).
```

---

### 4. 5 Whys Root Cause Analysis (Intermediate)

**Tutorial ID**: `five_whys`
**Duration**: 30 minutes
**Prerequisites**: `andon_event`

#### Learning Objectives

- Master 5 Whys methodology
- Identify true root causes
- Generate prevention actions
- Document analysis findings

#### Tutorial Steps

1. **5 Whys Methodology** (Explanation)
2. **Start Root Cause Analysis** (Hands-on)
3. **Answer the Five Whys** (Hands-on)
4. **Finalize Root Cause** (Hands-on)

#### Example Usage

```erlang
%% Start tutorial
{ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(<<"user_004">>, five_whys),

%% Execute through explanation
{ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{}),

%% Step 2: Start analysis
{ok, Result2} = tcps_diataxis_tutorial:execute_step(SessionId, #{
    andon_event_id => <<"andon_tutorial_001">>,
    problem => <<"Test failure in authentication">>
}),

AnalysisId = maps:get(output, Result2),

%% Step 3: Answer all 5 whys
{ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{
    analysis_id => AnalysisId,
    whys => [
        <<"Authentication token was null">>,
        <<"Token generation failed">>,
        <<"Config file was missing">>,
        <<"Deployment script didn't copy configs">>,
        <<"No config deployment validation step">>
    ]
}),

%% Step 4: Finalize
{ok, Receipt} = tcps_diataxis_tutorial:execute_step(SessionId, #{
    analysis_id => AnalysisId,
    root_cause => <<"Missing deployment validation">>,
    prevention => <<"Add config validation to deployment gate">>
}).
```

---

### 5. Complete TCPS Workflow (Advanced)

**Tutorial ID**: `complete_workflow`
**Duration**: 45 minutes
**Prerequisites**: `kanban_wip`, `five_whys`

#### Learning Objectives

- Execute complete TCPS workflow end-to-end
- Integrate all TCPS concepts
- Handle complex scenarios
- Achieve production readiness

#### Tutorial Steps

1. **End-to-End TCPS Workflow** (Explanation)
2. **Create Work Order from Pull Signal** (Hands-on)
3. **Start Work Order** (Hands-on)
4. **Run Quality Gate Sequence** (Hands-on)
5. **Handle Failures with Andon + 5 Whys** (Challenge)
6. **Complete Work Order** (Hands-on)

#### Example Usage

```erlang
%% Start tutorial
{ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(<<"user_005">>, complete_workflow),

%% Step 1: Explanation
{ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{}),

%% Step 2: Create work order
{ok, Result2} = tcps_diataxis_tutorial:execute_step(SessionId, #{
    pull_signal => #{
        source => github,
        type => bug_report,
        priority => 7,
        payload => #{issue_id => 123, title => <<"Auth bug">>}
    }
}),

WorkOrderId = maps:get(output, Result2),

%% Step 3: Start work
{ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{
    work_order_id => WorkOrderId
}),

%% Step 4: Run quality gates
{ok, Result4} = tcps_diataxis_tutorial:execute_step(SessionId, #{
    work_order_id => WorkOrderId
}),

GateResults = maps:get(output, Result4),
AllPassed = lists:all(fun({_, {Result, _}}) -> Result =:= pass end, GateResults),

%% Step 5: Handle failures if needed
{ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{
    all_passed => AllPassed,
    andon_resolved => true  % If failures occurred and were resolved
}),

%% Step 6: Complete work order
{ok, Receipt} = tcps_diataxis_tutorial:execute_step(SessionId, #{
    work_order_id => WorkOrderId,
    deliverables => #{
        artifacts => [<<"fixed_module.erl">>],
        tests => [<<"unit_tests.erl">>]
    }
}).
```

---

## API Reference

### Tutorial Orchestration API

#### `tcps_diataxis_tutorial:start_link/0`

Start the tutorial orchestration server.

**Returns**: `{ok, Pid} | {error, Reason}`

```erlang
{ok, Pid} = tcps_diataxis_tutorial:start_link().
```

#### `tcps_diataxis_tutorial:list_tutorials/0`

List all available tutorials with metadata.

**Returns**: `{ok, [TutorialMetadata]} | {error, Reason}`

```erlang
{ok, Tutorials} = tcps_diataxis_tutorial:list_tutorials(),
[FirstTutorial | _] = Tutorials,
#{
    id := quality_gate,
    title := <<"Your First Quality Gate">>,
    difficulty := beginner,
    estimated_time := 15,
    prerequisites := [],
    learning_objectives := [...]
} = FirstTutorial.
```

#### `tcps_diataxis_tutorial:get_tutorial_info/1`

Get detailed information about a specific tutorial.

**Parameters**: `TutorialId :: tutorial_id()`
**Returns**: `{ok, TutorialMetadata} | {error, not_found}`

```erlang
{ok, Info} = tcps_diataxis_tutorial:get_tutorial_info(quality_gate).
```

#### `tcps_diataxis_tutorial:start_tutorial/2`

Start a new tutorial session for a user.

**Parameters**:
- `UserId :: binary()`
- `TutorialId :: tutorial_id()`

**Returns**: `{ok, SessionId} | {error, Reason}`

```erlang
UserId = <<"user_001">>,
{ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate).
```

#### `tcps_diataxis_tutorial:get_tutorial_progress/1`

Get current progress for a tutorial session.

**Parameters**: `SessionId :: binary()`
**Returns**: `{ok, TutorialSession} | {error, not_found}`

```erlang
{ok, Progress} = tcps_diataxis_tutorial:get_tutorial_progress(SessionId),
#{
    session_id := _,
    tutorial_id := quality_gate,
    user_id := <<"user_001">>,
    current_step := <<"qg_step_2">>,
    completed_steps := [<<"qg_step_1">>],
    status := in_progress,
    hints_used := 0
} = Progress.
```

#### `tcps_diataxis_tutorial:execute_step/2`

Execute a tutorial step with user input.

**Parameters**:
- `SessionId :: binary()`
- `Input :: map()`

**Returns**: `{ok, StepResult} | {error, Reason}`

```erlang
{ok, Result} = tcps_diataxis_tutorial:execute_step(SessionId, #{
    sku_id => <<"tutorial_sku_001">>
}),

#{
    success := true,
    output := _,
    feedback := <<"Excellent! Your quality gate passed.">>,
    next_step := <<"qg_step_4">>,
    checkpoint := #{gate_passed => true}
} = Result.
```

#### `tcps_diataxis_tutorial:validate_step/2`

Validate step completion.

**Parameters**:
- `SessionId :: binary()`
- `ValidationData :: map()`

**Returns**: `{ok, ValidationResult} | {error, Reason}`

```erlang
{ok, Validation} = tcps_diataxis_tutorial:validate_step(SessionId, #{
    gate_result => {pass, #{}},
    sku_id => <<"tutorial_sku_001">>
}),

#{
    valid := true,
    errors := [],
    warnings := [],
    suggestions := [],
    score := 1.0
} = Validation.
```

#### `tcps_diataxis_tutorial:get_hint/1`

Get hint for current step.

**Parameters**: `SessionId :: binary()`
**Returns**: `{ok, Hint :: binary()} | {error, Reason}`

```erlang
{ok, Hint} = tcps_diataxis_tutorial:get_hint(SessionId),
<<"Use tcps_quality_gates:check_gate/2">> = Hint.
```

#### `tcps_diataxis_tutorial:complete_tutorial/1`

Mark tutorial as completed.

**Parameters**: `SessionId :: binary()`
**Returns**: `ok | {error, Reason}`

```erlang
ok = tcps_diataxis_tutorial:complete_tutorial(SessionId).
```

#### `tcps_diataxis_tutorial:get_user_progress/1`

Get overall progress for a user across all tutorials.

**Parameters**: `UserId :: binary()`
**Returns**: `{ok, ProgressReport} | {error, not_found}`

```erlang
{ok, Progress} = tcps_diataxis_tutorial:get_user_progress(<<"user_001">>),
#{
    user_id := <<"user_001">>,
    completed_tutorials := [quality_gate, kanban_wip],
    in_progress_tutorials := [andon_event],
    total_time_spent := 45,  % minutes
    achievements := [<<"First Quality Gate">>, <<"Kanban Master">>],
    skill_level := intermediate
} = Progress.
```

#### `tcps_diataxis_tutorial:get_learning_path/1`

Get recommended learning path for user.

**Parameters**: `UserId :: binary()`
**Returns**: `{ok, [tutorial_id()]} | {error, Reason}`

```erlang
{ok, Path} = tcps_diataxis_tutorial:get_learning_path(<<"user_001">>),
[andon_event, five_whys, complete_workflow] = Path.
```

---

### Step Execution API

#### `tcps_tutorial_steps:get_first_step/1`

Get first step for tutorial.

**Parameters**: `TutorialId :: tutorial_id()`
**Returns**: `StepId :: binary()`

```erlang
<<"qg_step_1">> = tcps_tutorial_steps:get_first_step(quality_gate).
```

#### `tcps_tutorial_steps:get_next_step/2`

Get next step in sequence.

**Parameters**:
- `TutorialId :: tutorial_id()`
- `CurrentStepId :: binary()`

**Returns**: `StepId :: binary() | completed`

```erlang
<<"qg_step_2">> = tcps_tutorial_steps:get_next_step(quality_gate, <<"qg_step_1">>).
```

#### `tcps_tutorial_steps:get_step_info/2`

Get step metadata and information.

**Parameters**:
- `TutorialId :: tutorial_id()`
- `StepId :: binary()`

**Returns**: `StepMetadata :: map()`

```erlang
StepInfo = tcps_tutorial_steps:get_step_info(quality_gate, <<"qg_step_3">>),
#{
    step_id := <<"qg_step_3">>,
    title := <<"Run Your First Quality Gate">>,
    type := hands_on,
    description := _,
    learning_outcome := _,
    estimated_time := 180,
    hints := [_|_],
    code_snippets := [_|_]
} = StepInfo.
```

#### `tcps_tutorial_steps:get_code_snippet/2`

Get code snippets for step.

**Parameters**:
- `TutorialId :: tutorial_id()`
- `StepId :: binary()`

**Returns**: `[CodeSnippet]`

```erlang
Snippets = tcps_tutorial_steps:get_code_snippet(quality_gate, <<"qg_step_3">>),
[#{
    language := <<"erlang">>,
    code := <<"Result = tcps_quality_gates:check_gate(compilation, <<\"sku_001\">>)">>,
    explanation := <<"Check compilation gate for a demo SKU">>,
    executable := true
}] = Snippets.
```

---

### Validation API

#### `tcps_tutorial_validation:validate_step/3`

Validate step completion with custom validation data.

**Parameters**:
- `TutorialId :: tutorial_id()`
- `StepId :: binary()`
- `ValidationData :: map()`

**Returns**: `ValidationResult :: map()`

```erlang
Result = tcps_tutorial_validation:validate_step(
    quality_gate,
    <<"qg_step_3">>,
    #{
        gate_result => {pass, #{}},
        sku_id => <<"tutorial_sku_001">>
    }
),

#{
    valid := true,
    errors := [],
    warnings := [],
    suggestions := [],
    score := 1.0
} = Result.
```

#### `tcps_tutorial_validation:validate_prerequisites/2`

Validate prerequisites for a tutorial.

**Parameters**:
- `TutorialId :: tutorial_id()`
- `UserProgress :: map()`

**Returns**: `{ok, valid} | {error, [UnmetPrerequisites]}`

```erlang
UserProgress = #{completed_tutorials => [quality_gate]},
{ok, valid} = tcps_tutorial_validation:validate_prerequisites(andon_event, UserProgress).
```

#### `tcps_tutorial_validation:verify_checkpoint/2`

Verify checkpoint state is valid.

**Parameters**:
- `StepId :: binary()`
- `CheckpointState :: map()`

**Returns**: `{ok, valid} | {error, {missing_keys, [atom()]}}`

```erlang
Checkpoint = #{service_started => true},
{ok, valid} = tcps_tutorial_validation:verify_checkpoint(<<"qg_step_2">>, Checkpoint).
```

#### `tcps_tutorial_validation:validate_code_quality/1`

Validate code quality for hands-on exercises.

**Parameters**: `Code :: binary()`
**Returns**: `ValidationResult :: map()`

```erlang
Code = <<"Result = tcps_quality_gates:check_gate(compilation, <<\"sku_001\">>).">>,
Result = tcps_tutorial_validation:validate_code_quality(Code),
#{
    valid := true,
    errors := [],
    warnings := [],
    suggestions := [],
    score := Score
} = Result,
Score >= 0.7.
```

---

## Progress Tracking

### Checkpoints

Checkpoints capture critical state at key tutorial moments:

```erlang
%% Checkpoint automatically created after important steps
{ok, Result} = tcps_diataxis_tutorial:execute_step(SessionId, Input),
#{checkpoint := Checkpoint} = Result,

%% Checkpoint structure
#{
    service_started => true,
    gate_passed => true,
    receipt => #{receipt_id => <<"...">>}
} = Checkpoint.
```

### Achievements

Achievements are earned through tutorial completion:

- **First Quality Gate**: Complete quality_gate tutorial
- **Kanban Master**: Complete kanban_wip tutorial
- **Stop the Line Champion**: Complete andon_event tutorial
- **Root Cause Detective**: Complete five_whys tutorial
- **TCPS Expert**: Complete complete_workflow tutorial
- **No Hints Used**: Complete any tutorial without hints

```erlang
{ok, Achievements} = tcps_diataxis_tutorial:get_achievements(UserId),
[<<"First Quality Gate">>, <<"No Hints Used">>] = Achievements.
```

### Skill Levels

Skill level is calculated based on completed tutorials:

- **Beginner**: 0 tutorials completed
- **Intermediate**: 1-2 tutorials completed
- **Advanced**: 3-4 tutorials completed
- **Expert**: All 5 tutorials completed

---

## Testing

### Running Tests

```bash
# Run all tutorial tests
rebar3 eunit --module=tcps_diataxis_tutorial_tests
rebar3 eunit --module=tcps_tutorial_steps_tests
rebar3 eunit --module=tcps_tutorial_validation_tests

# Run with coverage
rebar3 cover --reset
rebar3 eunit
rebar3 cover --verbose
```

### Test Coverage Goals

- **Tutorial Orchestration**: 80%+ coverage
- **Step Execution**: 85%+ coverage
- **Validation System**: 90%+ coverage

### Example Test

```erlang
test_execute_quality_gate_tutorial() ->
    UserId = <<"test_user_001">>,

    %% Start tutorial
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),

    %% Execute all steps
    lists:foldl(fun(_, _) ->
        {ok, Result} = tcps_diataxis_tutorial:execute_step(SessionId, #{
            failure_handled => true,
            sku_id => <<"test_sku">>
        }),
        maps:get(next_step, Result)
    end, <<"qg_step_1">>, lists:seq(1, 5)),

    %% Complete tutorial
    ok = tcps_diataxis_tutorial:complete_tutorial(SessionId),

    %% Verify achievement
    {ok, Achievements} = tcps_diataxis_tutorial:get_achievements(UserId),
    ?assert(lists:member(<<"First Quality Gate">>, Achievements)).
```

---

## Integration with TCPS Modules

The tutorial system integrates with these TCPS modules:

- **tcps_quality_gates**: Quality gate execution
- **tcps_kanban**: WIP limit management
- **tcps_andon**: Stop-the-line events
- **tcps_root_cause**: 5 Whys analysis
- **tcps_work_order**: Work order lifecycle

### Service Dependencies

Start required TCPS services before tutorials:

```erlang
%% Start TCPS services
application:ensure_all_started(tcps_quality_gates),
application:ensure_all_started(tcps_kanban),
application:ensure_all_started(tcps_andon),
application:ensure_all_started(tcps_root_cause),
application:ensure_all_started(tcps_work_order),

%% Start tutorial system
{ok, _} = tcps_diataxis_tutorial:start_link().
```

---

## Best Practices

### For Tutorial Creators

1. **Clear Learning Objectives**: Each step should have explicit outcomes
2. **Progressive Difficulty**: Build complexity gradually
3. **Real Integration**: Use actual TCPS modules, not mocks
4. **Immediate Feedback**: Provide feedback after every action
5. **Error Guidance**: Help users recover from mistakes

### For Tutorial Users

1. **Follow Prerequisites**: Complete prerequisite tutorials first
2. **Read Carefully**: Understand step descriptions before executing
3. **Experiment**: Try different inputs to understand behavior
4. **Use Hints Sparingly**: Challenge yourself before requesting hints
5. **Reflect**: Complete reflection steps to consolidate learning

---

## Troubleshooting

### Common Issues

#### Tutorial Won't Start

```erlang
%% Error: prerequisites_not_met
%% Solution: Complete prerequisite tutorials first
{ok, Info} = tcps_diataxis_tutorial:get_tutorial_info(five_whys),
Prerequisites = maps:get(prerequisites, Info),
%% Complete these tutorials first: [andon_event]
```

#### Step Validation Fails

```erlang
%% Check validation errors
{ok, Validation} = tcps_diataxis_tutorial:validate_step(SessionId, Data),
Errors = maps:get(errors, Validation),
Suggestions = maps:get(suggestions, Validation),
%% Follow suggestions to fix validation issues
```

#### Service Not Started

```erlang
%% Error: service_not_running
%% Solution: Start required TCPS service
tcps_quality_gates:start_link(),  % or appropriate service
```

---

## Future Enhancements

- **Interactive Code Editor**: In-browser Erlang code execution
- **Video Walkthroughs**: Recorded demonstrations for each tutorial
- **Peer Learning**: Share progress and solutions with other learners
- **Advanced Challenges**: Optional bonus exercises for experts
- **Custom Tutorials**: User-created tutorial templates

---

## References

- **Diataxis Framework**: https://diataxis.fr/
- **Toyota Production System**: Lean manufacturing principles
- **TCPS Documentation**: See `docs/TCPS_*.md` files
- **TCPS Quality Gates**: `docs/TCPS_QUALITY_GATES.md`
- **TCPS Kanban**: Source code documentation in `src/tcps_kanban.erl`

---

## Support

For questions or issues with the tutorial system:

1. Check tutorial hints and suggestions
2. Review API documentation above
3. Examine test files for usage examples
4. Consult TCPS module documentation

---

**Generated**: 2026-01-26
**Version**: 1.0.0
**Maintainer**: TCPS Development Team
