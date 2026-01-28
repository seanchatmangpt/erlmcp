%%%-----------------------------------------------------------------------------
%%% @doc TCPS Diataxis MCP Prompts
%%%
%%% Implements MCP prompts for guided learning and task completion in the
%%% TCPS Diataxis simulator. Prompts provide contextual guidance aligned with
%%% the Diataxis documentation framework quadrants.
%%%
%%% Prompt Categories:
%%% - Tutorial Completion: Step-by-step completion prompts
%%% - How-to Recipes: Task-oriented suggestions for specific goals
%%% - Explanation Clarifications: Understanding-oriented concept prompts
%%% - Reference Lookups: Quick information-oriented prompts
%%%
%%% All prompts include:
%%% - Argument validation
%%% - Context-aware suggestions
%%% - Progressive disclosure
%%% - Error handling
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_mcp_prompts).

-include("erlmcp.hrl").

%% API exports
-export([
    get_all_prompts/0
]).

%% Prompt handler exports
-export([
    handle_tutorial_completion/1,
    handle_howto_recipe/1,
    handle_explanation_clarify/1,
    handle_reference_lookup/1,
    handle_quality_gate_guidance/1,
    handle_andon_response_guide/1
]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%% @doc Get all prompt definitions with arguments and handlers.
-spec get_all_prompts() -> [{binary(), [#mcp_prompt_argument{}], fun((map()) -> [map()])}].
get_all_prompts() ->
    [
        {<<"tutorial_completion">>, tutorial_completion_args(), fun handle_tutorial_completion/1},
        {<<"howto_recipe">>, howto_recipe_args(), fun handle_howto_recipe/1},
        {<<"explanation_clarify">>, explanation_clarify_args(), fun handle_explanation_clarify/1},
        {<<"reference_lookup">>, reference_lookup_args(), fun handle_reference_lookup/1},
        {<<"quality_gate_guidance">>, quality_gate_guidance_args(), fun handle_quality_gate_guidance/1},
        {<<"andon_response_guide">>, andon_response_guide_args(), fun handle_andon_response_guide/1}
    ].

%%%=============================================================================
%%% Prompt Arguments
%%%=============================================================================

%% @private
-spec tutorial_completion_args() -> [#mcp_prompt_argument{}].
tutorial_completion_args() ->
    [
        #mcp_prompt_argument{
            name = <<"step">>,
            description = <<"Current tutorial step number (1-5)">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"context">>,
            description = <<"Additional context about user's progress">>,
            required = false
        }
    ].

%% @private
-spec howto_recipe_args() -> [#mcp_prompt_argument{}].
howto_recipe_args() ->
    [
        #mcp_prompt_argument{
            name = <<"task">>,
            description = <<"Task to accomplish (e.g., 'create_work_order', 'respond_to_andon')">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"difficulty">>,
            description = <<"Difficulty level: beginner, intermediate, advanced">>,
            required = false
        }
    ].

%% @private
-spec explanation_clarify_args() -> [#mcp_prompt_argument{}].
explanation_clarify_args() ->
    [
        #mcp_prompt_argument{
            name = <<"concept">>,
            description = <<"TCPS concept to clarify (e.g., 'andon', 'kanban', 'quality_gates')">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"confusion_point">>,
            description = <<"Specific point of confusion or question">>,
            required = false
        }
    ].

%% @private
-spec reference_lookup_args() -> [#mcp_prompt_argument{}].
reference_lookup_args() ->
    [
        #mcp_prompt_argument{
            name = <<"item">>,
            description = <<"Item to look up (tool name, concept, API method)">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"type">>,
            description = <<"Lookup type: tool, concept, api, schema">>,
            required = false
        }
    ].

%% @private
-spec quality_gate_guidance_args() -> [#mcp_prompt_argument{}].
quality_gate_guidance_args() ->
    [
        #mcp_prompt_argument{
            name = <<"gate_type">>,
            description = <<"Type of quality gate: test_pass_rate, coverage, shacl, deterministic">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"current_value">>,
            description = <<"Current value of the metric">>,
            required = false
        }
    ].

%% @private
-spec andon_response_guide_args() -> [#mcp_prompt_argument{}].
andon_response_guide_args() ->
    [
        #mcp_prompt_argument{
            name = <<"failure_type">>,
            description = <<"Type of failure that triggered Andon">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"stage">>,
            description = <<"Stage where failure occurred">>,
            required = true
        }
    ].

%%%=============================================================================
%%% Prompt Handlers
%%%=============================================================================

%% @doc Handle tutorial_completion prompt.
-spec handle_tutorial_completion(map()) -> [map()].
handle_tutorial_completion(Args) ->
    Step = binary_to_integer(maps:get(<<"step">>, Args, <<"1">>)),
    Context = maps:get(<<"context">>, Args, <<>>),

    Messages = case Step of
        1 ->
            [
                user_message(<<"You're on step 1: Starting a TCPS simulation.">>),
                assistant_message(generate_step_1_prompt(Context))
            ];
        2 ->
            [
                user_message(<<"You're on step 2: Creating your first work order.">>),
                assistant_message(generate_step_2_prompt(Context))
            ];
        3 ->
            [
                user_message(<<"You're on step 3: Running tests and checking quality gates.">>),
                assistant_message(generate_step_3_prompt(Context))
            ];
        4 ->
            [
                user_message(<<"You're on step 4: Understanding Kanban WIP limits.">>),
                assistant_message(generate_step_4_prompt(Context))
            ];
        5 ->
            [
                user_message(<<"You're on step 5: Triggering and responding to Andon events.">>),
                assistant_message(generate_step_5_prompt(Context))
            ];
        _ ->
            [
                user_message(<<"Tutorial step out of range (1-5)">>),
                assistant_message(<<"Please choose a step between 1 and 5.">>)
            ]
    end,

    Messages.

%% @doc Handle howto_recipe prompt.
-spec handle_howto_recipe(map()) -> [map()].
handle_howto_recipe(Args) ->
    Task = maps:get(<<"task">>, Args),
    Difficulty = maps:get(<<"difficulty">>, Args, <<"beginner">>),

    Recipe = case Task of
        <<"create_work_order">> ->
            generate_create_work_order_recipe(Difficulty);
        <<"respond_to_andon">> ->
            generate_andon_response_recipe(Difficulty);
        <<"manage_wip_limits">> ->
            generate_wip_limits_recipe(Difficulty);
        <<"check_quality_gates">> ->
            generate_quality_gates_recipe(Difficulty);
        _ ->
            <<"Unknown task. Available tasks: create_work_order, respond_to_andon, manage_wip_limits, check_quality_gates">>
    end,

    [
        user_message(<<"How do I ", Task/binary, "?">>),
        assistant_message(Recipe)
    ].

%% @doc Handle explanation_clarify prompt.
-spec handle_explanation_clarify(map()) -> [map()].
handle_explanation_clarify(Args) ->
    Concept = maps:get(<<"concept">>, Args),
    ConfusionPoint = maps:get(<<"confusion_point">>, Args, <<>>),

    Explanation = case Concept of
        <<"andon">> ->
            generate_andon_explanation(ConfusionPoint);
        <<"kanban">> ->
            generate_kanban_explanation(ConfusionPoint);
        <<"quality_gates">> ->
            generate_quality_gates_explanation(ConfusionPoint);
        <<"heijunka">> ->
            generate_heijunka_explanation(ConfusionPoint);
        <<"jidoka">> ->
            generate_jidoka_explanation(ConfusionPoint);
        _ ->
            <<"Unknown concept. Available concepts: andon, kanban, quality_gates, heijunka, jidoka">>
    end,

    Message = case ConfusionPoint of
        <<>> -> <<"I need clarification on ", Concept/binary>>;
        _ -> <<"I need clarification on ", Concept/binary, ": ", ConfusionPoint/binary>>
    end,
    [
        user_message(Message),
        assistant_message(Explanation)
    ].

%% @doc Handle reference_lookup prompt.
-spec handle_reference_lookup(map()) -> [map()].
handle_reference_lookup(Args) ->
    Item = maps:get(<<"item">>, Args),
    Type = maps:get(<<"type">>, Args, <<"auto">>),

    Reference = case Type of
        <<"tool">> -> lookup_tool_reference(Item);
        <<"concept">> -> lookup_concept_reference(Item);
        <<"api">> -> lookup_api_reference(Item);
        <<"schema">> -> lookup_schema_reference(Item);
        <<"auto">> -> auto_lookup_reference(Item);
        _ -> <<"Unknown reference type">>
    end,

    [
        user_message(<<"Reference lookup: ", Item/binary>>),
        assistant_message(Reference)
    ].

%% @doc Handle quality_gate_guidance prompt.
-spec handle_quality_gate_guidance(map()) -> [map()].
handle_quality_gate_guidance(Args) ->
    GateType = maps:get(<<"gate_type">>, Args),
    CurrentValue = maps:get(<<"current_value">>, Args, <<>>),

    Guidance = case GateType of
        <<"test_pass_rate">> ->
            generate_test_pass_rate_guidance(CurrentValue);
        <<"coverage">> ->
            generate_coverage_guidance(CurrentValue);
        <<"shacl">> ->
            generate_shacl_guidance(CurrentValue);
        <<"deterministic">> ->
            generate_deterministic_guidance(CurrentValue);
        _ ->
            <<"Unknown gate type. Available: test_pass_rate, coverage, shacl, deterministic">>
    end,

    [
        user_message(<<"I need guidance on ", GateType/binary, " quality gate">>),
        assistant_message(Guidance)
    ].

%% @doc Handle andon_response_guide prompt.
-spec handle_andon_response_guide(map()) -> [map()].
handle_andon_response_guide(Args) ->
    FailureType = maps:get(<<"failure_type">>, Args),
    Stage = maps:get(<<"stage">>, Args),

    Guide = generate_andon_response_guide(FailureType, Stage),

    [
        user_message(<<"Andon triggered for ", FailureType/binary, " at ", Stage/binary, " stage">>),
        assistant_message(Guide)
    ].

%%%=============================================================================
%%% Message Helpers
%%%=============================================================================

%% @private
-spec user_message(binary()) -> map().
user_message(Text) ->
    #{
        ?MCP_PARAM_ROLE => ?MCP_ROLE_USER,
        ?MCP_PARAM_CONTENT => #{
            ?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT,
            ?MCP_PARAM_TEXT => Text
        }
    }.

%% @private
-spec assistant_message(binary()) -> map().
assistant_message(Text) ->
    #{
        ?MCP_PARAM_ROLE => ?MCP_ROLE_ASSISTANT,
        ?MCP_PARAM_CONTENT => #{
            ?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT,
            ?MCP_PARAM_TEXT => Text
        }
    }.

%%%=============================================================================
%%% Tutorial Step Prompts
%%%=============================================================================

%% @private
-spec generate_step_1_prompt(binary()) -> binary().
generate_step_1_prompt(_Context) ->
    <<"
**Step 1: Starting a TCPS Simulation**

To begin your TCPS journey, you'll start a simulation session:

```
Use the simulator_start tool with:
{
  \"config\": {
    \"max_steps\": 100,
    \"initial_quadrant\": \"tutorial\",
    \"enable_telemetry\": true
  }
}
```

This will:
- Create a unique simulation session
- Initialize all TCPS components (Kanban, Quality Gates, Andon)
- Set up telemetry for observability
- Place you in the tutorial quadrant

**Expected Output:**
- Session ID for tracking
- Initial state confirmation
- Next steps guidance

**Try it now!** Use the simulator_start tool to begin.
    ">>.

%% @private
-spec generate_step_2_prompt(binary()) -> binary().
generate_step_2_prompt(_Context) ->
    <<"
**Step 2: Creating Your First Work Order**

Work orders are the fundamental unit of work in TCPS Kanban:

```
Use simulator_step with:
{
  \"action\": \"create_work_order\",
  \"params\": {
    \"bucket\": \"reliability\",
    \"priority\": 1
  }
}
```

**Kanban Buckets:**
- reliability: Code quality, bug fixes
- security: Security patches, vulnerabilities
- cost: Performance optimizations
- compliance: Regulatory requirements

**WIP Limits:**
Each bucket has a WIP (Work In Progress) limit of 5.
Creating work orders beyond this limit will be blocked!

**Try different buckets** to see how Kanban manages flow.
    ">>.

%% @private
-spec generate_step_3_prompt(binary()) -> binary().
generate_step_3_prompt(_Context) ->
    <<"
**Step 3: Running Tests and Quality Gates**

TCPS enforces quality through automated gates:

```
Use simulator_step with:
{
  \"action\": \"run_tests\",
  \"params\": {}
}
```

**Quality Gates:**
- Test Pass Rate: ≥ 80% (Lean Six Sigma standard)
- Code Coverage: ≥ 80%
- SHACL Validation: Must pass
- Deterministic Build: Must be reproducible

**Andon Triggers:**
If any gate fails, Andon stops the pipeline!

**Try forcing a failure** with quality_gate_simulate tool.
    ">>.

%% @private
-spec generate_step_4_prompt(binary()) -> binary().
generate_step_4_prompt(_Context) ->
    <<"
**Step 4: Understanding Kanban WIP Limits**

Visualize your Kanban board:

```
Use kanban_visualize with:
{
  \"format\": \"markdown\",
  \"include_metrics\": true
}
```

**WIP Limit Benefits:**
- Prevents work overload
- Maintains flow
- Highlights bottlenecks
- Enforces focus

**Try creating multiple work orders** in the same bucket to hit the WIP limit!

**Experiment:**
1. Create 3 work orders in reliability
2. Try creating 2 more (WIP = 5/5)
3. Try creating one more (BLOCKED!)
    ">>.

%% @private
-spec generate_step_5_prompt(binary()) -> binary().
generate_step_5_prompt(_Context) ->
    <<"
**Step 5: Triggering Andon Events**

Experience the stop-the-line system:

```
Use andon_trigger with:
{
  \"failure_type\": \"test_failure\",
  \"stage\": \"testing\",
  \"details\": {
    \"description\": \"Test pass rate below 80%\"
  }
}
```

**Andon Response Protocol:**
1. Production STOPS immediately
2. Root cause analysis required
3. Countermeasures documented
4. Prevention measures implemented
5. Quality gates updated

**This is Jidoka** - autonomation with human intelligence!

**Tutorial Complete!**
Explore how-to recipes and explanations to deepen your understanding.
    ">>.

%%%=============================================================================
%%% How-to Recipe Generators
%%%=============================================================================

%% @private
-spec generate_create_work_order_recipe(binary()) -> binary().
generate_create_work_order_recipe(<<"beginner">>) ->
    <<"
**Recipe: Create a Work Order**

**Goal:** Add a new work item to the Kanban board

**Steps:**
1. Choose a bucket (reliability, security, cost, compliance)
2. Set priority (1 = highest)
3. Call simulator_step with create_work_order action

**Example:**
```json
{
  \"action\": \"create_work_order\",
  \"params\": {
    \"bucket\": \"reliability\",
    \"priority\": 1
  }
}
```

**Success Criteria:**
- Work order ID returned
- WIP count increased
- Kanban board updated
    ">>;
generate_create_work_order_recipe(_) ->
    <<"Advanced work order creation recipe...">>. % Abbreviated for brevity

%% @private
-spec generate_andon_response_recipe(binary()) -> binary().
generate_andon_response_recipe(_Difficulty) ->
    <<"
**Recipe: Respond to Andon Event**

**Goal:** Properly handle a stop-the-line event

**Steps:**
1. Identify root cause (use tcps_explain tool)
2. Document findings
3. Implement countermeasures
4. Update quality gates to prevent recurrence
5. Resume production

**Example:**
```
1. andon_trigger: Test failure at testing stage
2. tcps_explain: Understand why test_pass_rate gate failed
3. Document: Tests need refactoring
4. Countermeasure: Add missing test cases
5. Prevention: Increase coverage threshold
```

**This embodies Kaizen** - continuous improvement!
    ">>.

%% @private
-spec generate_wip_limits_recipe(binary()) -> binary().
generate_wip_limits_recipe(_Difficulty) ->
    <<"Recipe for managing WIP limits...">>.

%% @private
-spec generate_quality_gates_recipe(binary()) -> binary().
generate_quality_gates_recipe(_Difficulty) ->
    <<"Recipe for quality gate management...">>.

%%%=============================================================================
%%% Concept Explanation Generators
%%%=============================================================================

%% @private
-spec generate_andon_explanation(binary()) -> binary().
generate_andon_explanation(<<>>) ->
    <<"
**Andon: Stop-the-Line Quality System**

Andon (Japanese: アンドン) is a manufacturing term from Toyota Production System
that refers to a system to notify management, maintenance, and other workers
of a quality or process problem.

**Core Principle:**
Anyone can and should stop production when a defect is detected.

**In Software:**
- SHACL violations stop compilation
- Test failures stop deployment
- Non-deterministic builds stop release
- Missing receipts stop validation

**Why Stop?**
Passing defects downstream multiplies their cost 10x at each stage!
Stopping immediately enables faster, cheaper fixes.

**Related:** Jidoka (autonomation), Quality Gates, Root Cause Analysis
    ">>;
generate_andon_explanation(_ConfusionPoint) ->
    <<"Andon explanation for specific confusion point...">>.

%% @private
-spec generate_kanban_explanation(binary()) -> binary().
generate_kanban_explanation(_ConfusionPoint) ->
    <<"
**Kanban: Visual Work Management System**

Kanban (Japanese: 看板, signboard) is a lean method to manage and improve work
across human systems by visualizing work and limiting work-in-progress (WIP).

**Key Concepts:**
- **Visual Board:** See all work at a glance
- **WIP Limits:** Prevent overload (5 items per bucket)
- **Pull System:** Work is pulled when capacity exists
- **Flow:** Optimize for smooth, continuous delivery

**TCPS Buckets:**
1. Reliability (quality, bugs)
2. Security (vulnerabilities)
3. Cost (performance)
4. Compliance (regulatory)

**Benefits:**
- Identifies bottlenecks
- Prevents context switching
- Improves focus and flow
- Enables predictable delivery
    ">>.

%% @private
-spec generate_quality_gates_explanation(binary()) -> binary().
generate_quality_gates_explanation(_ConfusionPoint) ->
    <<"Quality gates explanation...">>.

%% @private
-spec generate_heijunka_explanation(binary()) -> binary().
generate_heijunka_explanation(_ConfusionPoint) ->
    <<"Heijunka (production leveling) explanation...">>.

%% @private
-spec generate_jidoka_explanation(binary()) -> binary().
generate_jidoka_explanation(_ConfusionPoint) ->
    <<"Jidoka (autonomation) explanation...">>.

%%%=============================================================================
%%% Reference Lookup Functions
%%%=============================================================================

%% @private
-spec lookup_tool_reference(binary()) -> binary().
lookup_tool_reference(<<"simulator_start">>) ->
    <<"
**Tool: simulator_start**

**Purpose:** Start a new TCPS simulation session

**Input Schema:**
```json
{
  \"config\": {
    \"max_steps\": integer (1-1000),
    \"initial_quadrant\": \"tutorial\" | \"howto\" | \"explanation\" | \"reference\",
    \"enable_telemetry\": boolean
  }
}
```

**Output:** Session ID and initial state

**Example:** See tutorial step 1
    ">>;
lookup_tool_reference(_) ->
    <<"Tool reference not found">>.

%% @private
-spec lookup_concept_reference(binary()) -> binary().
lookup_concept_reference(_Item) ->
    <<"Concept reference...">>.

%% @private
-spec lookup_api_reference(binary()) -> binary().
lookup_api_reference(_Item) ->
    <<"API reference...">>.

%% @private
-spec lookup_schema_reference(binary()) -> binary().
lookup_schema_reference(_Item) ->
    <<"Schema reference...">>.

%% @private
-spec auto_lookup_reference(binary()) -> binary().
auto_lookup_reference(Item) ->
    % Try tool first, then concept
    case is_tool(Item) of
        true -> lookup_tool_reference(Item);
        false -> lookup_concept_reference(Item)
    end.

%% @private
-spec is_tool(binary()) -> boolean().
is_tool(<<"simulator_start">>) -> true;
is_tool(<<"simulator_step">>) -> true;
is_tool(<<"simulator_query">>) -> true;
is_tool(<<"diataxis_navigate">>) -> true;
is_tool(<<"tcps_explain">>) -> true;
is_tool(<<"quality_gate_simulate">>) -> true;
is_tool(<<"andon_trigger">>) -> true;
is_tool(<<"kanban_visualize">>) -> true;
is_tool(_) -> false.

%%%=============================================================================
%%% Quality Gate Guidance Generators
%%%=============================================================================

%% @private
-spec generate_test_pass_rate_guidance(binary()) -> binary().
generate_test_pass_rate_guidance(<<>>) ->
    <<"
**Quality Gate: Test Pass Rate**

**Threshold:** ≥ 80% (Lean Six Sigma standard)

**Why 80%?**
Based on Toyota Production System's 99.99966% defect-free target.
80% pass rate ensures 3.4 defects per million opportunities (DPMO).

**Improving Pass Rate:**
1. Fix failing tests first
2. Review test quality (are they meaningful?)
3. Refactor brittle tests
4. Add missing edge cases
5. Use TDD for new code

**Andon Trigger:**
Pass rate < 80% triggers Andon and stops pipeline!
    ">>;
generate_test_pass_rate_guidance(_CurrentValue) ->
    <<"Test pass rate guidance with current value...">>.

%% @private
-spec generate_coverage_guidance(binary()) -> binary().
generate_coverage_guidance(_CurrentValue) ->
    <<"Coverage guidance...">>.

%% @private
-spec generate_shacl_guidance(binary()) -> binary().
generate_shacl_guidance(_CurrentValue) ->
    <<"SHACL validation guidance...">>.

%% @private
-spec generate_deterministic_guidance(binary()) -> binary().
generate_deterministic_guidance(_CurrentValue) ->
    <<"Deterministic build guidance...">>.

%%%=============================================================================
%%% Andon Response Guide Generator
%%%=============================================================================

%% @private
-spec generate_andon_response_guide(binary(), binary()) -> binary().
generate_andon_response_guide(FailureType, Stage) ->
    <<"
**Andon Response Guide**

**Failure Type:** ", FailureType/binary, "
**Stage:** ", Stage/binary, "

**Immediate Actions:**
1. STOP all downstream work
2. Gather failure data
3. Notify team
4. Begin root cause analysis

**Root Cause Analysis (5 Whys):**
Ask 'why' 5 times to find root cause:
- Why did the failure occur?
- Why did that happen?
- Why wasn't it caught earlier?
- Why did the process allow it?
- Why isn't there a safeguard?

**Countermeasures:**
- Fix: Address immediate issue
- Prevent: Update quality gates
- Verify: Test the fix
- Document: Update procedures

**Resume Criteria:**
✓ Root cause identified
✓ Countermeasure implemented
✓ Prevention mechanism added
✓ Quality gates updated
✓ Receipt generated

**This is Kaizen in action!**
    ">>.
