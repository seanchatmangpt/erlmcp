# TCPS Diataxis How-to Guides Documentation

## Overview

The TCPS How-to Guides module provides task-oriented documentation following the Diataxis framework's how-to quadrant. These guides help users accomplish specific goals with step-by-step instructions, code examples, and verification steps.

## Architecture

### Module Structure

```
src/tcps_mcp_diataxis/
├── tcps_diataxis_howto.erl    - How-to guide engine (gen_server)
├── tcps_howto_recipes.erl     - Recipe library (12 comprehensive guides)
└── howto/                     - Additional guide storage
```

### Core Components

#### 1. How-to Guide Engine (`tcps_diataxis_howto.erl`)

The engine is a gen_server that:
- Manages a library of how-to guides
- Provides search and discovery capabilities
- Supports step-by-step execution
- Formats guides for MCP tool integration
- Tracks guide relationships and categories

**Key Features:**
- **Category indexing**: Quick access by category (quality_gates, kanban, andon, etc.)
- **Full-text search**: Token-based search across titles, problems, and tags
- **Step execution**: Retrieve commands and verification steps
- **MCP integration**: Format guides for consumption by MCP tools
- **Related guides**: Discover related how-to guides

#### 2. Recipe Library (`tcps_howto_recipes.erl`)

Contains 12 production-ready how-to guides:

1. **How to Set Up Quality Gates** (beginner, 20 min)
2. **How to Configure Kanban Buckets and WIP Limits** (beginner, 15 min)
3. **How to Respond to an Andon Alert** (beginner, 10 min)
4. **How to Conduct a 5 Whys Analysis** (intermediate, 30 min)
5. **How to Implement Heijunka Production Leveling** (advanced, 45 min)
6. **How to Generate and Verify Receipt Chains** (intermediate, 25 min)
7. **How to Monitor TCPS Metrics** (beginner, 20 min)
8. **How to Integrate TCPS with CI/CD** (intermediate, 35 min)
9. **How to Debug Quality Gate Failures** (intermediate, 25 min)
10. **How to Optimize TCPS Performance** (advanced, 40 min)
11. **How to Calculate and Set Takt Time** (intermediate, 30 min)
12. **How to Implement Jidoka (Automation with Human Touch)** (advanced, 50 min)

## Guide Structure

Each how-to guide follows this standardized format:

```erlang
#{
    id => guide_id,                    % Unique identifier
    title => <<"Guide Title">>,        % Clear, action-oriented title
    category => category_atom,         % Categorization
    difficulty => beginner | intermediate | advanced,
    duration => minutes,               % Expected completion time
    problem => <<"Problem description">>,  % What problem this solves
    prerequisites => [<<"Prereq 1">>, ...],  % What's needed before starting
    steps => [Step1, Step2, ...],      % Ordered step list
    verification => <<"Success criteria">>,  % How to verify completion
    common_pitfalls => [<<"Pitfall 1">>, ...],  % What to avoid
    related_guides => [guide_id_1, ...],  % Related guides
    tags => [<<"tag1">>, ...]          % Searchable tags
}
```

### Step Structure

Each step provides complete execution guidance:

```erlang
#{
    number => N,                           % Step sequence number
    description => <<"What to do">>,       % Clear description
    command => <<"Code to run">> | undefined,  % Executable command
    expected_output => <<"Expected result">>,  % What success looks like
    verification => <<"How to verify">>,   % Verification method
    notes => [<<"Note 1">>, ...]          % Important considerations
}
```

## API Reference

### Starting the Engine

```erlang
%% Start the how-to guide engine
{ok, Pid} = tcps_diataxis_howto:start_link().
```

### Discovering Guides

```erlang
%% List all available guides
{ok, AllGuides} = tcps_diataxis_howto:list_guides().

%% Get a specific guide
{ok, Guide} = tcps_diataxis_howto:get_guide(quality_gates).

%% Search guides by keyword
{ok, Results} = tcps_diataxis_howto:search_guides(<<"kanban workflow">>).

%% Get guides by category
{ok, QualityGuides} = tcps_diataxis_howto:get_guide_by_category(quality_gates).

%% Find related guides
{ok, Related} = tcps_diataxis_howto:get_related_guides(quality_gates).
```

### Executing Guides

```erlang
%% Get a specific step's execution details
{ok, StepInfo} = tcps_diataxis_howto:execute_guide_step(quality_gates, 1).
%% Returns: #{command => <<"...">>, expected => <<"...">>, verification => <<"...">>}

%% Verify step completion
{ok, verified, Message} = tcps_diataxis_howto:verify_step_completion(quality_gates, 1).
```

### Troubleshooting

```erlang
%% Get common pitfalls for a guide
{ok, Pitfalls} = tcps_diataxis_howto:get_troubleshooting(quality_gates).
```

### MCP Integration

```erlang
%% Format guide for MCP tool consumption
{ok, Guide} = tcps_diataxis_howto:get_guide(quality_gates),
MCPFormat = tcps_diataxis_howto:format_for_mcp(Guide).
%% Returns JSON-compatible map with binary keys
```

## Categories

### Quality Gates
Guides for setting up and managing quality controls:
- Setting up quality gates
- Debugging gate failures
- Jidoka automation

### Kanban
Workflow management and WIP control:
- Configuring Kanban buckets and WIP limits

### Andon
Alert response and incident management:
- Responding to Andon alerts

### Analysis
Root cause analysis and problem-solving:
- Conducting 5 Whys analysis

### Production
Production planning and leveling:
- Implementing Heijunka
- Calculating and setting takt time

### Receipts
Cryptographic audit trails:
- Generating and verifying receipt chains

### Monitoring
System observability and metrics:
- Monitoring TCPS metrics

### Integration
Connecting TCPS to external systems:
- Integrating TCPS with CI/CD

### Performance
Optimization and tuning:
- Optimizing TCPS performance

## Usage Patterns

### Pattern 1: New User Getting Started

```erlang
%% 1. Start with beginner guides
{ok, BeginnerGuides} = tcps_diataxis_howto:list_guides(),
Beginner = lists:filter(
    fun(#{difficulty := D}) -> D =:= beginner end,
    BeginnerGuides
).

%% 2. Begin with quality gates
{ok, QGGuide} = tcps_diataxis_howto:get_guide(quality_gates).

%% 3. Execute each step
lists:foreach(
    fun(StepNum) ->
        {ok, StepInfo} = tcps_diataxis_howto:execute_guide_step(quality_gates, StepNum),
        %% Execute command
        %% Verify output
        {ok, verified, _} = tcps_diataxis_howto:verify_step_completion(quality_gates, StepNum)
    end,
    lists:seq(1, length(maps:get(steps, QGGuide)))
).
```

### Pattern 2: Troubleshooting an Issue

```erlang
%% 1. Search for relevant guides
{ok, DebugGuides} = tcps_diataxis_howto:search_guides(<<"gate failure debug">>).

%% 2. Get the debugging guide
{ok, DebugGuide} = tcps_diataxis_howto:get_guide(debugging).

%% 3. Check common pitfalls
{ok, Pitfalls} = tcps_diataxis_howto:get_troubleshooting(debugging).

%% 4. Follow the steps
%% ... execute debugging steps ...
```

### Pattern 3: Building CI/CD Integration

```erlang
%% 1. Get the integration guide
{ok, CICDGuide} = tcps_diataxis_howto:get_guide(cicd_integration).

%% 2. Check prerequisites
Prerequisites = maps:get(prerequisites, CICDGuide).

%% 3. Follow steps sequentially
Steps = maps:get(steps, CICDGuide),
lists:foreach(
    fun(#{number := N, command := Cmd, verification := Verify}) ->
        io:format("Step ~p: ~s~n", [N, Cmd]),
        %% Execute command
        io:format("Verify: ~s~n", [Verify])
    end,
    Steps
).

%% 4. Explore related guides
{ok, Related} = tcps_diataxis_howto:get_related_guides(cicd_integration).
%% Returns: [quality_gates, monitoring, receipt_chain, debugging]
```

### Pattern 4: MCP Tool Integration

```erlang
%% MCP tool requests a how-to guide
handle_mcp_request(<<"get_howto_guide">>, #{<<"id">> := GuideId}) ->
    {ok, Guide} = tcps_diataxis_howto:get_guide(binary_to_atom(GuideId)),
    MCPFormat = tcps_diataxis_howto:format_for_mcp(Guide),
    {ok, MCPFormat}.

%% MCP tool searches guides
handle_mcp_request(<<"search_guides">>, #{<<"query">> := Query}) ->
    {ok, Results} = tcps_diataxis_howto:search_guides(Query),
    FormattedResults = [tcps_diataxis_howto:format_for_mcp(G) || G <- Results],
    {ok, FormattedResults}.
```

## Guide Summaries

### 1. How to Set Up Quality Gates

**Problem**: Need to enforce quality standards and prevent defects from entering codebase.

**Key Steps**:
1. Define quality criteria (coverage, lint, types, security)
2. Configure enforcement points (commit, PR, merge)
3. Set up automatic quality checks
4. Configure notifications
5. Test with sample code

**Outcome**: Automated quality enforcement that blocks defective code.

---

### 2. How to Configure Kanban Buckets and WIP Limits

**Problem**: Bottlenecks and context switching due to unmanaged workflow.

**Key Steps**:
1. Define workflow buckets matching your process
2. Calculate WIP limits (team size × 1.5)
3. Apply limits to buckets
4. Configure enforcement policies
5. Test transitions
6. Monitor flow metrics

**Outcome**: Balanced workflow with predictable throughput.

---

### 3. How to Respond to an Andon Alert

**Problem**: Quality or process issue detected, need systematic response.

**Key Steps**:
1. Acknowledge alert immediately (within 5 min)
2. Retrieve alert details and context
3. Stop the line if critical
4. Investigate root cause
5. Implement immediate fix
6. Verify and resume
7. Schedule RCA

**Outcome**: Quick, systematic incident response that prevents defect propagation.

---

### 4. How to Conduct a 5 Whys Analysis

**Problem**: Need to identify true root cause of recurring problem.

**Key Steps**:
1. Define problem clearly with data
2. Ask Why #1: What caused this?
3. Ask Why #2-5: Drill deeper each time
4. Identify systemic root cause
5. Define corrective actions
6. Track implementation

**Outcome**: Root cause identification and preventive actions.

---

### 5. How to Implement Heijunka Production Leveling

**Problem**: Unpredictable work arrival causing bottlenecks and idle time.

**Key Steps**:
1. Analyze historical demand patterns
2. Calculate target daily production
3. Design heijunka box (scheduling board)
4. Level work mix across time
5. Implement pitch intervals
6. Configure buffer management
7. Simulate schedule
8. Launch and monitor

**Outcome**: Smooth, predictable production flow aligned with demand.

---

### 6. How to Generate and Verify Receipt Chains

**Problem**: Need cryptographic proof and tamper-evident audit trail.

**Key Steps**:
1. Initialize receipt chain with genesis block
2. Generate receipts for work items
3. Verify individual receipt integrity
4. Verify full chain continuity
5. Query receipt history
6. Export for archival
7. Detect tampering
8. Generate external proofs

**Outcome**: Tamper-proof, verifiable audit trail.

---

### 7. How to Monitor TCPS Metrics

**Problem**: Need visibility into system health and performance.

**Key Steps**:
1. Configure metrics collection
2. View current metrics
3. Set up alerts
4. Create dashboard
5. Analyze trends
6. Generate reports
7. Export to external systems

**Outcome**: Comprehensive observability and proactive issue detection.

---

### 8. How to Integrate TCPS with CI/CD

**Problem**: Pipeline lacks quality gates and lean principles.

**Key Steps**:
1. Install TCPS in CI environment
2. Configure quality gates in pipeline
3. Add metrics collection
4. Generate build receipts
5. Implement Andon alerts
6. Integrate Kanban board
7. Configure deployment gates
8. Generate reports

**Outcome**: CI/CD pipeline with automated quality enforcement and lean practices.

---

### 9. How to Debug Quality Gate Failures

**Problem**: Quality gate failing but cause unclear.

**Key Steps**:
1. Identify which gate failed
2. Get detailed execution logs
3. Identify uncovered code sections
4. Check test execution
5. Verify tool configuration
6. Compare with baseline
7. Get fix recommendations
8. Verify fix locally

**Outcome**: Root cause identified and resolved efficiently.

---

### 10. How to Optimize TCPS Performance

**Problem**: TCPS operations slower than expected.

**Key Steps**:
1. Establish performance baseline
2. Identify slowest operations
3. Analyze operation traces
4. Enable caching
5. Parallelize independent operations
6. Optimize receipt verification
7. Tune database queries
8. Implement incremental operations
9. Benchmark optimizations
10. Monitor continuously

**Outcome**: Significant performance improvements (50%+ in critical paths).

---

### 11. How to Calculate and Set Takt Time

**Problem**: Production pace doesn't match customer demand.

**Key Steps**:
1. Calculate available production time
2. Determine customer demand rate
3. Calculate takt time (available ÷ demand)
4. Compare to cycle time
5. Configure takt pacing
6. Set up monitoring
7. Analyze adherence
8. Adjust for demand changes

**Outcome**: Production pace aligned with customer demand.

---

### 12. How to Implement Jidoka

**Problem**: Automated processes lack intelligent error detection and judgment.

**Key Steps**:
1. Identify processes for jidoka
2. Define normal vs abnormal conditions
3. Implement automatic error detection
4. Configure auto-stop mechanism
5. Build in source quality checks
6. Enable intelligent error analysis
7. Configure human intervention points
8. Test with simulated errors
9. Implement visual management
10. Monitor effectiveness

**Outcome**: Smart automation that builds quality at source and stops on abnormalities.

## Search and Discovery

### Search Algorithm

The search system uses tokenization and inverted indexing:

1. **Indexing**: Guides are indexed at startup
   - Extract tokens from title, problem, and tags
   - Build inverted index: token → [guide_ids]
   - Filter short words and common words

2. **Querying**: Search queries are processed similarly
   - Tokenize query
   - Look up each token in index
   - Union matching guide IDs
   - Return corresponding guides

3. **Optimization**: Search is O(1) for index lookup, O(n) for result assembly

### Category Taxonomy

```
quality_gates    - Quality control and enforcement
kanban           - Workflow management
andon            - Alert and incident response
analysis         - Root cause analysis
production       - Production planning and leveling
receipts         - Cryptographic audit trails
monitoring       - Observability and metrics
integration      - External system integration
debugging        - Troubleshooting and problem resolution
performance      - Optimization and tuning
```

## Common Pitfalls (Across All Guides)

### General Pitfalls

1. **Skipping prerequisites**: Leads to confusion and failure
2. **Not verifying each step**: Issues compound
3. **Ignoring common pitfalls**: Repeating known mistakes
4. **No baseline metrics**: Can't measure improvement
5. **Poor documentation**: Loses learning opportunities
6. **Insufficient testing**: Discovers problems in production
7. **Not following up**: Issues recur

### Quality-Specific Pitfalls

1. **Setting thresholds too high initially**: Unrealistic, discourages adoption
2. **Setting thresholds too low**: Defeats the purpose
3. **No escape hatch for emergencies**: Blocks critical fixes
4. **Skipping gates under pressure**: Undermines the system

### Process-Specific Pitfalls

1. **Too many process steps**: Adds complexity, reduces agility
2. **WIP limits too high/low**: Either ineffective or causes frustration
3. **No monitoring**: Can't improve what you don't measure
4. **Reverting to old habits**: Requires discipline to maintain

### Technical-Specific Pitfalls

1. **Premature optimization**: Optimize the wrong things
2. **No caching strategy**: Repeated expensive work
3. **Missing database indexes**: Query performance degrades
4. **No continuous monitoring**: Regressions go unnoticed

## Integration with Diataxis Framework

### How-to vs Other Quadrants

| Aspect | How-to Guides | Tutorials | Explanations | Reference |
|--------|---------------|-----------|--------------|-----------|
| **Purpose** | Solve problems | Learn by doing | Understand | Look up info |
| **Focus** | Goals | Learning | Understanding | Information |
| **Form** | Steps | Lessons | Discussion | Description |
| **Analogy** | Recipe | Cooking lesson | Food science | Encyclopedia |

### When to Use How-to Guides

Use how-to guides when users need to:
- Accomplish a specific task
- Solve a known problem
- Configure or set up a feature
- Troubleshoot an issue
- Integrate with external systems
- Optimize performance

### When NOT to Use How-to Guides

Don't use how-to guides for:
- Teaching concepts (use Tutorials)
- Explaining why/how things work (use Explanations)
- Looking up API details (use Reference)

## Extending the Guide Library

### Adding New Guides

```erlang
%% 1. Create guide map following the structure
new_guide() ->
    #{
        id => my_new_guide,
        title => <<"How to Do Something">>,
        category => appropriate_category,
        difficulty => beginner | intermediate | advanced,
        duration => estimated_minutes,
        problem => <<"Clear problem statement">>,
        prerequisites => [<<"Prereq 1">>, <<"Prereq 2">>],
        steps => [
            #{
                number => 1,
                description => <<"First step">>,
                command => <<"command to run">>,
                expected_output => <<"what you should see">>,
                verification => <<"how to verify">>,
                notes => [<<"Important note">>]
            }
            %% ... more steps
        ],
        verification => <<"Overall completion criteria">>,
        common_pitfalls => [<<"Pitfall 1">>, <<"Pitfall 2">>],
        related_guides => [related_guide_1, related_guide_2],
        tags => [<<"tag1">>, <<"tag2">>]
    }.

%% 2. Add to tcps_howto_recipes.erl
-export([get_my_new_guide/0]).

get_my_new_guide() ->
    new_guide().

%% 3. Update load_all_guides/0
load_all_guides() ->
    #{
        %% ... existing guides
        my_new_guide => get_my_new_guide()
    }.
```

### Guide Quality Checklist

- [ ] Clear, actionable title starting with "How to"
- [ ] Specific problem statement
- [ ] Complete prerequisites list
- [ ] 5-10 well-defined steps
- [ ] Executable commands for each step
- [ ] Expected output examples
- [ ] Verification steps
- [ ] At least 5 common pitfalls
- [ ] Related guides identified
- [ ] Appropriate tags
- [ ] Realistic difficulty and duration

## Performance Considerations

### Indexing Performance

- **Initial load**: O(n × m) where n = guides, m = avg tokens per guide
- **Search**: O(k + r) where k = query tokens, r = result guides
- **Category lookup**: O(1) hash map lookup
- **Memory**: ~1KB per guide in indexes

### Optimization Opportunities

1. **Lazy loading**: Load guide details on demand
2. **Caching**: Cache frequently accessed guides
3. **Incremental indexing**: Update index on guide changes
4. **Compression**: Compress guide text in storage

## Testing

### Unit Testing

```erlang
%% Test guide retrieval
test_get_guide() ->
    {ok, Guide} = tcps_diataxis_howto:get_guide(quality_gates),
    ?assertEqual(quality_gates, maps:get(id, Guide)),
    ?assertEqual(beginner, maps:get(difficulty, Guide)).

%% Test search
test_search() ->
    {ok, Results} = tcps_diataxis_howto:search_guides(<<"quality gates">>),
    ?assert(length(Results) > 0),
    Ids = [maps:get(id, G) || G <- Results],
    ?assert(lists:member(quality_gates, Ids)).

%% Test step execution
test_step_execution() ->
    {ok, StepInfo} = tcps_diataxis_howto:execute_guide_step(quality_gates, 1),
    ?assert(maps:is_key(command, StepInfo)),
    ?assert(maps:is_key(expected, StepInfo)).
```

### Integration Testing

```erlang
%% Test MCP integration
test_mcp_format() ->
    {ok, Guide} = tcps_diataxis_howto:get_guide(quality_gates),
    MCPFormat = tcps_diataxis_howto:format_for_mcp(Guide),
    ?assertEqual(<<"how_to_guide">>, maps:get(type, MCPFormat)),
    ?assert(is_binary(maps:get(title, MCPFormat))).
```

## Troubleshooting

### Common Issues

**Issue**: Guide not found
- **Cause**: Incorrect guide ID
- **Solution**: Use `list_guides()` to see available IDs

**Issue**: Search returns no results
- **Cause**: Query doesn't match indexed tokens
- **Solution**: Try different keywords, check tags

**Issue**: Step execution fails
- **Cause**: Prerequisites not met
- **Solution**: Verify prerequisites before starting

**Issue**: MCP formatting fails
- **Cause**: Guide structure incomplete
- **Solution**: Validate guide against structure spec

## Best Practices

### For Guide Authors

1. **Start with the problem**: Make it relatable
2. **Be specific**: "Set test coverage to 80%" not "improve quality"
3. **Show, don't tell**: Include actual commands and outputs
4. **Verify everything**: Each step should be verifiable
5. **Document pitfalls**: Save users from common mistakes
6. **Link related content**: Help users discover more
7. **Test the guide**: Actually follow the steps yourself

### For Guide Users

1. **Read the whole guide first**: Understand the full picture
2. **Check prerequisites**: Don't skip this step
3. **Follow steps in order**: They build on each other
4. **Verify each step**: Don't proceed if verification fails
5. **Note common pitfalls**: Avoid known issues
6. **Explore related guides**: Deepen your knowledge
7. **Provide feedback**: Help improve the guides

### For Integrators

1. **Use category indexes**: Faster than linear search
2. **Cache frequently accessed guides**: Improve performance
3. **Format once**: Reuse MCP-formatted guides
4. **Handle errors gracefully**: Guide might not exist
5. **Provide context**: Show related guides
6. **Track usage**: Know which guides are popular
7. **Update regularly**: Keep guides current

## Metrics and Analytics

### Useful Metrics to Track

```erlang
%% Guide usage
track_guide_usage(GuideId) ->
    %% Increment usage counter
    %% Track completion rate
    %% Measure time to complete
    ok.

%% Search effectiveness
track_search_effectiveness(Query, ResultCount, GuideOpened) ->
    %% Log search queries
    %% Track result relevance
    %% Measure search-to-guide conversion
    ok.

%% Step success rate
track_step_success(GuideId, StepNumber, Success) ->
    %% Track which steps fail most
    %% Identify guides needing improvement
    ok.
```

### Analytics Queries

```erlang
%% Most popular guides
get_popular_guides(TopN) ->
    %% Return top N most accessed guides
    ok.

%% Guides with low completion rate
get_problematic_guides(ThresholdPercent) ->
    %% Return guides with completion < threshold
    ok.

%% Common search terms
get_common_searches(TopN) ->
    %% Return top N search queries
    ok.
```

## Future Enhancements

### Planned Features

1. **Interactive execution**: Execute commands directly from guide
2. **Progress tracking**: Save user progress through guides
3. **Video supplements**: Add video walkthroughs
4. **Version tracking**: Track guide versions over time
5. **User contributions**: Allow guide submissions
6. **Localization**: Multi-language support
7. **Adaptive difficulty**: Adjust based on user skill
8. **Prerequisite checking**: Auto-verify prerequisites

### Enhancement Opportunities

1. **AI assistance**: Generate guide content from templates
2. **Auto-verification**: Programmatically verify step completion
3. **Context awareness**: Suggest guides based on current task
4. **Personalization**: Recommend guides based on user history
5. **Collaboration**: Multi-user guide walkthroughs
6. **Gamification**: Badges for completing guide series

## Conclusion

The TCPS How-to Guides module provides a comprehensive, task-oriented documentation system that helps users accomplish specific goals efficiently. With 12 production-ready guides covering all major TCPS features, robust search and discovery, and seamless MCP integration, the module serves as an essential resource for both new and experienced TCPS users.

The guides follow the Diataxis framework's how-to quadrant principles:
- ✅ Task-oriented (solve specific problems)
- ✅ Goal-focused (accomplish real objectives)
- ✅ Practical (step-by-step instructions)
- ✅ Verifiable (clear success criteria)
- ✅ Safe (common pitfalls documented)

## Additional Resources

- **Diataxis Framework**: https://diataxis.fr/how-to-guides/
- **TCPS Tutorial Module**: For learning-oriented content
- **TCPS Explanation Module**: For understanding-oriented content
- **TCPS Reference Module**: For information-oriented content

---

**Module Version**: 1.0.0
**Last Updated**: 2024-01-26
**Maintainer**: TCPS Documentation Team
