# TCPS Diataxis Explanation Module Documentation

## Overview

The TCPS Diataxis Explanation module provides **understanding-oriented** content that clarifies concepts, explains design decisions, and helps learners understand the "why" behind TCPS (Toyota Code Production System).

This is the third quadrant of the Diataxis framework:

```
                Learning-Oriented        Goal-Oriented
                      ↓                       ↓
            ┌─────────────────┬─────────────────────┐
            │   TUTORIALS     │    HOW-TO GUIDES    │
            │  (Learning)     │     (Tasks)         │
Practical   ├─────────────────┼─────────────────────┤
            │  EXPLANATION    │    REFERENCE        │
            │ (Understanding) │  (Information)      │
            └─────────────────┴─────────────────────┘
Theoretical                      ↑
                          This module
```

## Architecture

The explanation module consists of three Erlang modules:

### 1. `tcps_diataxis_explain.erl` - Explanation Engine

Main interface for accessing explanations:

```erlang
-module(tcps_diataxis_explain).

% Core API
-export([
    get_explanation/1,         % Get explanation by ID
    list_explanations/0,       % List all explanations
    list_by_category/1,        % Filter by category
    search_explanations/1,     % Search by keyword
    get_related_explanations/1,% Get related content
    get_explanation_path/1,    % Get learning path
    validate_explanation/1     % Validate structure
]).
```

**Key Features:**
- **Content retrieval** - Access explanations by ID or category
- **Search functionality** - Find explanations by keyword
- **Learning paths** - Discover related content and progression
- **Validation** - Ensure explanation structure integrity

### 2. `tcps_concepts.erl` - Core Concept Explanations

Provides **5 core concept explanations** drawn from Toyota Production System philosophy:

| ID | Title | Difficulty | Reading Time |
|----|-------|------------|--------------|
| `why_tcps` | Why TCPS? Understanding Toyota Production System for Code | Beginner | 8 min |
| `jidoka_philosophy` | The Philosophy of Jidoka (Built-in Quality) | Intermediate | 10 min |
| `pull_vs_push` | Pull vs Push: Understanding Kanban Pull Systems | Intermediate | 9 min |
| `andon_thinking` | Andon: The Power of Stop-the-Line Thinking | Intermediate | 10 min |
| `heijunka_leveling` | Heijunka: Leveling Production Load | Intermediate | 9 min |

**What Core Concepts Cover:**
- **Manufacturing-software parallels** - How TPS principles apply to code
- **Fundamental philosophies** - Jidoka, pull systems, Andon, Heijunka
- **Historical context** - Why these principles emerged and succeeded
- **Conceptual frameworks** - Mental models for understanding TCPS

### 3. `tcps_principles.erl` - Design Decisions & Comparisons

Provides **10 explanations** covering design rationale and comparative analysis:

**Design Decisions (5 explanations):**

| ID | Title | Difficulty | Reading Time |
|----|-------|------------|--------------|
| `receipts_not_commits` | Why Receipts Instead of Git Commits? | Intermediate | 10 min |
| `quality_gates_vs_ci` | Why Quality Gates vs Traditional CI/CD? | Intermediate | 11 min |
| `wip_limits_matter` | Why WIP Limits Matter in Software Development | Intermediate | 10 min |
| `dual_storage` | Why TCPS Uses Dual Storage (JSON + RDF) | Advanced | 10 min |
| `mcp_integration` | Why MCP Integration for TCPS? | Advanced | 11 min |

**Comparisons (5 explanations):**

| ID | Title | Difficulty | Reading Time |
|----|-------|------------|--------------|
| `tcps_vs_devops` | TCPS vs Traditional DevOps | Intermediate | 9 min |
| `tcps_vs_lean` | TCPS vs Lean Software Development | Intermediate | 10 min |
| `tcps_vs_agile` | TCPS vs Agile/Scrum | Intermediate | 11 min |
| `quality_gates_vs_static` | Quality Gates vs Static Analysis | Intermediate | 10 min |
| `andon_vs_monitoring` | Andon vs Error Monitoring | Intermediate | 10 min |

## Explanation Structure

Each explanation follows a rich, comprehensive structure:

```erlang
-type explanation() :: #{
    id := binary(),                    % Unique identifier
    title := binary(),                 % Human-readable title
    category := category(),            % core_concepts | design_decisions | comparisons
    summary := binary(),               % One-sentence summary

    % Rich content sections
    sections := [section()],           % Multiple content sections

    % Learning aids
    analogies := [binary()],           % Real-world comparisons
    trade_offs := #{                   % Honest pros/cons
        pros := [binary()],
        cons := [binary()]
    },

    % Navigation
    related := [explanation_id()],     % Related explanations
    tags := [binary()],                % Searchable tags

    % Metadata
    difficulty := beginner | intermediate | advanced,
    reading_time_minutes := pos_integer()
}.
```

### Section Structure

Each explanation contains multiple sections with:

```erlang
-type section() :: #{
    title := binary(),           % Section heading
    content := binary(),         % Rich markdown content

    % Optional enhancements
    examples => [binary()],      % Concrete examples
    diagrams => [binary()],      % ASCII diagrams or descriptions
    key_points => [binary()]     % Bullet point summaries
}.
```

## Content Philosophy

### Understanding-Oriented (Not Task-Oriented)

Explanations focus on **understanding**, not doing:

- ❌ **Not**: "How to configure quality gates" (that's a how-to)
- ✅ **Yes**: "Why quality gates enforce higher standards than traditional CI/CD"

- ❌ **Not**: "Steps to set WIP limits" (that's a how-to)
- ✅ **Yes**: "Why WIP limits matter: The science and math of flow"

### Clarity Through Context

Explanations provide context and background:

- **Historical context** - Why did Toyota develop these principles?
- **Problem context** - What problems do they solve?
- **Comparative context** - How does this compare to alternatives?
- **Cultural context** - What mindset shift is needed?

### Rich, Multi-Dimensional Content

Each explanation includes:

1. **Conceptual explanation** - The core idea and its significance
2. **Analogies** - Real-world parallels to aid understanding
3. **Examples** - Concrete scenarios demonstrating the concept
4. **Diagrams** - Visual representations of relationships
5. **Trade-offs** - Honest pros and cons (builds trust)
6. **Related content** - Links to deepen understanding

### Progressive Disclosure

Content is organized for progressive learning:

- **Beginner explanations** - Start with "Why TCPS?"
- **Intermediate explanations** - Dive into specific concepts (Jidoka, Andon)
- **Advanced explanations** - Technical decisions (dual storage, MCP)

## Usage Examples

### Basic Retrieval

```erlang
% Get a specific explanation
{ok, Explanation} = tcps_diataxis_explain:get_explanation(<<"why_tcps">>).

% Access content
Title = maps:get(title, Explanation),
Sections = maps:get(sections, Explanation),
Analogies = maps:get(analogies, Explanation).
```

### Browsing by Category

```erlang
% Get all core concept explanations
CoreConcepts = tcps_diataxis_explain:list_by_category(core_concepts),
% Returns: 5 explanations (why_tcps, jidoka_philosophy, pull_vs_push, andon_thinking, heijunka_leveling)

% Get all design decision explanations
DesignDecisions = tcps_diataxis_explain:list_by_category(design_decisions),
% Returns: 5 explanations (receipts_not_commits, quality_gates_vs_ci, etc.)

% Get all comparison explanations
Comparisons = tcps_diataxis_explain:list_by_category(comparisons).
% Returns: 5 explanations (tcps_vs_devops, tcps_vs_lean, etc.)
```

### Searching

```erlang
% Search for explanations about quality
QualityExplanations = tcps_diataxis_explain:search_explanations(<<"quality">>),
% Returns explanations with "quality" in title, summary, or tags

% Search for Kanban-related content
KanbanExplanations = tcps_diataxis_explain:search_explanations(<<"kanban">>).
```

### Learning Paths

```erlang
% Get related explanations
{ok, JidokaExpl} = tcps_diataxis_explain:get_explanation(<<"jidoka_philosophy">>),
RelatedIds = maps:get(related, JidokaExpl),
% Returns: [<<"why_tcps">>, <<"andon_thinking">>, <<"quality_gates_vs_ci">>, ...]

% Get full learning path for a topic
LearningPath = tcps_diataxis_explain:get_explanation_path(<<"jidoka_philosophy">>),
% Returns explanations in progressive order (beginner → intermediate → advanced)
```

### Validation

```erlang
% Validate explanation structure
{ok, Explanation} = tcps_diataxis_explain:get_explanation(<<"why_tcps">>),
ok = tcps_diataxis_explain:validate_explanation(Explanation).
```

## MCP Integration

The explanation module integrates with Model Context Protocol (MCP) for AI-assisted learning:

### MCP Endpoints

```javascript
// Get explanation via MCP
const explanation = await mcp.call('tcps_diataxis_explain', 'get_explanation', {
  id: 'jidoka_philosophy'
});

// Search via MCP
const results = await mcp.call('tcps_diataxis_explain', 'search_explanations', {
  keyword: 'quality gates'
});

// Get learning path via MCP
const path = await mcp.call('tcps_diataxis_explain', 'get_explanation_path', {
  id: 'andon_thinking'
});
```

### AI-Powered Learning Scenarios

**Scenario 1: Developer asks "Why do we need quality gates?"**

```
Developer → AI: "Why do we need quality gates instead of just running tests in CI?"

AI (via MCP):
  1. Calls tcps_diataxis_explain:get_explanation(<<"quality_gates_vs_ci">>)
  2. Retrieves comprehensive explanation
  3. Adapts to developer's context
  4. Responds with personalized summary + key points + examples
```

**Scenario 2: Developer confused about Jidoka**

```
Developer → AI: "I don't understand what Jidoka means"

AI (via MCP):
  1. Calls tcps_diataxis_explain:get_explanation(<<"jidoka_philosophy">>)
  2. Retrieves explanation with analogies
  3. Calls get_related_explanations(<<"jidoka_philosophy">>)
  4. Responds with:
     - Core explanation in conversational tone
     - Helpful analogies (spell-check, circuit breakers)
     - Related topics to explore (Andon, quality gates)
```

**Scenario 3: Learning path guidance**

```
Developer → AI: "I'm new to TCPS, where should I start?"

AI (via MCP):
  1. Calls list_by_category(core_concepts)
  2. Filters by difficulty (beginner)
  3. Calls get_explanation(<<"why_tcps">>)
  4. Responds with:
     - "Start with 'Why TCPS?' (8 min read)"
     - Summary of what they'll learn
     - Next recommended topics (Jidoka, Pull systems)
```

## Content Coverage

### Core Concepts (5 explanations)

**1. Why TCPS?** (`why_tcps`)
- Manufacturing-software parallels
- Core problem: Quality at speed
- Five TPS principles adapted for software
- Why TCPS is relevant today (complexity, velocity, cost, DX, AI)

**2. Jidoka Philosophy** (`jidoka_philosophy`)
- What is Jidoka (built-in quality)
- Four stages of Jidoka (detect, stop, fix, prevent)
- Jidoka in software (quality gates)
- The human element (autonomation)

**3. Pull vs Push** (`pull_vs_push`)
- Push systems and their problems (overload, inventory, context switching)
- Pull systems and their benefits (capacity-based, WIP limits, visual bottlenecks)
- TCPS pull implementation (Kanban, receipts, on-demand resources)
- When pull works better than push

**4. Andon Thinking** (`andon_thinking`)
- What is Andon (stop-the-line authority)
- Counterintuitive power of stopping (faster overall)
- Andon in software (quality gates at multiple stages)
- Cultural shift required (stop is not failure)

**5. Heijunka Leveling** (`heijunka_leveling`)
- What is Heijunka (production leveling)
- Two dimensions (volume + variety leveling)
- Heijunka in software (sprint planning, work type ratios, daily balancing)
- Benefits of leveling (sustainable pace, predictability, quality)

### Design Decisions (5 explanations)

**1. Receipts Not Commits** (`receipts_not_commits`)
- Limitations of Git commits (no quality metadata)
- What are TCPS receipts (comprehensive work records)
- How receipts enable quality (gates, visibility, Andon, value tracking)
- Receipts and Git are complementary

**2. Quality Gates vs CI** (`quality_gates_vs_ci`)
- Traditional CI/CD best effort model
- TCPS quality gates (non-negotiable standards)
- The ratchet effect (quality only goes up)
- Cultural shift required

**3. WIP Limits Matter** (`wip_limits_matter`)
- The multitasking myth (context switching cost)
- Little's Law (mathematical basis for WIP limits)
- Quality degrades with high WIP
- How to set WIP limits (conservative, per-column, tune based on data)

**4. Dual Storage** (`dual_storage`)
- Storage trade-offs (JSON vs RDF)
- JSON for operational layer (fast, simple)
- RDF for semantic layer (rich, queryable)
- Synchronization pattern (best of both worlds)

**5. MCP Integration** (`mcp_integration`)
- What is MCP (Model Context Protocol)
- TCPS + MCP: AI-assisted quality
- Educational dimension (Diataxis via MCP)
- Vision: AI-native development workflow

### Comparisons (5 explanations)

**1. TCPS vs DevOps** (`tcps_vs_devops`)
- Core philosophy differences (delivery vs quality)
- Where they overlap (automation, CI/CD)
- Where they differ (pace, standards, culture, work units)
- When to use what (integration pattern)

**2. TCPS vs Lean** (`tcps_vs_lean`)
- Lean Software foundation (7 principles)
- TCPS as concrete implementation of Lean
- What TCPS adds beyond Lean (TPS concepts, quality rigor, receipts, dual storage)
- When Lean is enough, when TCPS helps

**3. TCPS vs Agile** (`tcps_vs_agile`)
- Philosophical common ground (iteration, feedback)
- Key differences (quality vs velocity)
- Work management (Scrum vs Kanban vs TCPS Kanban)
- Can you use both? (Yes, integration patterns)

**4. Quality Gates vs Static Analysis** (`quality_gates_vs_static`)
- Static analysis: good but not enough (optional, fragmented, no trends)
- Quality gates: comprehensive, enforced (mandatory, multi-dimensional, ratchet)
- Static analysis inside quality gates (tools vs system)
- When to use what (migration path)

**5. Andon vs Monitoring** (`andon_vs_monitoring`)
- Error monitoring: reactive response (production-focused)
- Andon: proactive prevention (pre-production)
- Cost difference: exponential (1x vs 100x vs 1000x)
- Use both: defense in depth (prevention + safety net)

## Reading Time Estimates

Total reading time for all 15 explanations: **149 minutes** (~2.5 hours)

By category:
- **Core Concepts**: 46 minutes (5 explanations × 8-10 min avg)
- **Design Decisions**: 52 minutes (5 explanations × 10-11 min avg)
- **Comparisons**: 51 minutes (5 explanations × 9-11 min avg)

Recommended reading order:

**Week 1 - Foundations (46 min)**
1. Why TCPS? (8 min) - Start here
2. Jidoka Philosophy (10 min)
3. Pull vs Push (9 min)
4. Andon Thinking (10 min)
5. Heijunka Leveling (9 min)

**Week 2 - Design Rationale (52 min)**
6. Receipts Not Commits (10 min)
7. Quality Gates vs CI (11 min)
8. WIP Limits Matter (10 min)
9. Dual Storage (10 min)
10. MCP Integration (11 min)

**Week 3 - Context & Comparisons (51 min)**
11. TCPS vs DevOps (9 min)
12. TCPS vs Lean (10 min)
13. TCPS vs Agile (11 min)
14. Quality Gates vs Static Analysis (10 min)
15. Andon vs Monitoring (10 min)

## Quality Standards

All explanations adhere to high quality standards:

### Content Quality

✓ **Comprehensive** - Multiple sections covering different angles
✓ **Contextualized** - Historical, cultural, technical context provided
✓ **Honest** - Trade-offs section acknowledges both pros and cons
✓ **Accessible** - Analogies and examples for clarity
✓ **Connected** - Related explanations for deeper learning

### Structural Quality

✓ **Well-organized** - Clear section hierarchy
✓ **Scannable** - Key points, examples, diagrams
✓ **Progressive** - Beginner → Intermediate → Advanced
✓ **Validated** - Structure validation ensures consistency

### Writing Quality

✓ **Clear** - Conversational, jargon-explained
✓ **Engaging** - Analogies, examples, diagrams
✓ **Respectful** - Acknowledges alternative approaches
✓ **Educational** - Focuses on understanding, not just information

## Testing

The module includes comprehensive EUnit tests:

```erlang
% Structure validation
get_explanation_test() ->
    {ok, Explanation} = get_explanation(<<"why_tcps">>),
    ?assertEqual(<<"Why TCPS? Understanding Toyota Production System for Code">>,
                 maps:get(title, Explanation)).

% Category filtering
list_by_category_test() ->
    CoreConcepts = list_by_category(core_concepts),
    ?assertEqual(5, length(CoreConcepts)).

% Search functionality
search_explanations_test() ->
    Results = search_explanations(<<"quality">>),
    ?assert(length(Results) > 0).

% Validation
validate_explanation_test() ->
    {ok, Explanation} = get_explanation(<<"why_tcps">>),
    ?assertEqual(ok, validate_explanation(Explanation)).
```

Run tests:

```bash
rebar3 eunit --module=tcps_diataxis_explain
rebar3 eunit --module=tcps_concepts
rebar3 eunit --module=tcps_principles
```

## Implementation Notes

### Performance Considerations

- **In-memory storage** - All explanations loaded at module startup
- **Fast retrieval** - O(1) map lookup by ID
- **Efficient search** - Linear scan with early termination
- **Lazy loading** - Related explanations fetched on-demand

### Extensibility

Adding new explanations:

1. **Choose category** - core_concepts | design_decisions | comparisons
2. **Create function** - Return explanation map
3. **Follow structure** - Use explanation() type
4. **Add to get_all_explanations/0** - Register in map
5. **Write tests** - Validate structure
6. **Update docs** - Add to this file

Example:

```erlang
% In tcps_concepts.erl or tcps_principles.erl
new_concept() ->
    #{
        id => <<"new_concept">>,
        title => <<"New Concept Title">>,
        category => core_concepts,
        summary => <<"One-sentence summary">>,
        sections => [
            #{title => <<"Section 1">>, content => <<"...">>}
        ],
        analogies => [<<"Analogy 1">>, <<"Analogy 2">>],
        trade_offs => #{pros => [...], cons => [...]},
        related => [<<"related_id_1">>, <<"related_id_2">>],
        tags => [<<"tag1">>, <<"tag2">>],
        difficulty => intermediate,
        reading_time_minutes => 10
    }.

% In tcps_diataxis_explain.erl
get_all_explanations() ->
    #{
        % ... existing explanations ...
        <<"new_concept">> => tcps_concepts:new_concept()
    }.
```

### Future Enhancements

Potential improvements:

1. **Rich media** - Embed images, videos, interactive diagrams
2. **Localization** - Multi-language support
3. **Personalization** - Adapt content based on reader's role/experience
4. **Progress tracking** - Remember what user has read
5. **Quiz integration** - Check understanding after reading
6. **Community contributions** - Allow user-submitted explanations
7. **Version history** - Track explanation evolution over time

## References

- **Diataxis Framework**: https://diataxis.fr/
- **Toyota Production System**: Taiichi Ohno, "Toyota Production System: Beyond Large-Scale Production"
- **Lean Software Development**: Mary & Tom Poppendieck
- **The Goal**: Eliyahu Goldratt (Theory of Constraints)
- **Accelerate**: Nicole Forsgren, Jez Humble, Gene Kim

## License

Part of the TCPS (Toyota Code Production System) project.

---

**Remember**: Explanations are about **understanding**, not doing. They clarify the "why" behind TCPS, provide context, and help learners build mental models. For practical steps, see the How-To Guide module.
