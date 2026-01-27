%%%-------------------------------------------------------------------
%%% @doc TCPS Design Principles and Comparisons
%%% Explanation-oriented content that clarifies design decisions,
%%% trade-offs, and compares TCPS to alternative approaches.
%%%
%%% This module provides:
%%% - Design decision explanations (why TCPS works this way)
%%% - Comparative analysis (TCPS vs other methodologies)
%%% - Trade-off discussions
%%% - Context for architectural choices
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_principles).

-export([
    % Design Decisions
    receipts_not_commits/0,
    quality_gates_vs_ci/0,
    wip_limits_matter/0,
    dual_storage/0,
    mcp_integration/0,

    % Comparisons
    tcps_vs_devops/0,
    tcps_vs_lean/0,
    tcps_vs_agile/0,
    quality_gates_vs_static/0,
    andon_vs_monitoring/0
]).

%%%===================================================================
%%% Design Decision Explanations
%%%===================================================================

%% @doc Why Receipts Instead of Git Commits?
receipts_not_commits() ->
    #{
        id => <<"receipts_not_commits">>,
        title => <<"Why Receipts Instead of Git Commits?">>,
        category => design_decisions,
        summary => <<"Understand why TCPS uses receipts as the unit of work instead of relying solely on Git commits, and how this enables better quality tracking.">>,

        sections => [
            #{
                title => <<"The Limitations of Git Commits">>,
                content => <<
                    "Git commits are excellent for version control, but they have limitations\n"
                    "as units of work tracking:\n\n"

                    "**Problem 1: Commits Don't Capture Quality**\n"
                    "A Git commit tells you:\n"
                    "• What changed (diff)\n"
                    "• When it changed (timestamp)\n"
                    "• Who changed it (author)\n"
                    "• Why it changed (message, if written well)\n\n"

                    "But Git commits DON'T tell you:\n"
                    "• Did tests pass?\n"
                    "• What was test coverage?\n"
                    "• Were there type errors?\n"
                    "• Did linting pass?\n"
                    "• Were security scans clean?\n"
                    "• How long did CI take?\n\n"

                    "**Problem 2: Commits Don't Track Work Units**\n"
                    "• One feature might span 10 commits\n"
                    "• One commit might touch 3 features\n"
                    "• WIP commits clutter history\n"
                    "• Rebase/squash loses intermediate state\n"
                    "• No clear 'unit of delivery'\n\n"

                    "**Problem 3: Commits Don't Model Process**\n"
                    "Git commit flow:\n"
                    "  Write code → Commit → Push → Pray CI passes\n\n"

                    "No representation of:\n"
                    "• Quality gates\n"
                    "• Review process\n"
                    "• Testing stages\n"
                    "• Deployment workflow\n\n"

                    "Git is a technical tool (version control).\n"
                    "We need a business/process tool (work tracking)."
                >>,
                key_points => [
                    <<"Git commits lack quality metadata">>,
                    <<"Commits don't map to work units">>,
                    <<"No process modeling in Git">>,
                    <<"Version control ≠ work tracking">>
                ]
            },

            #{
                title => <<"What Are TCPS Receipts?">>,
                content => <<
                    "A **Receipt** in TCPS is a comprehensive record of completed work:\n\n"

                    "```erlang\n"
                    "-record(receipt, {\n"
                    "    id :: binary(),              % Unique identifier\n"
                    "    task_id :: binary(),         % Related task/story\n"
                    "    developer :: binary(),       % Who did the work\n"
                    "    timestamp :: integer(),      % When completed\n"
                    "    \n"
                    "    % Code changes\n"
                    "    files_changed :: [binary()],\n"
                    "    lines_added :: integer(),\n"
                    "    lines_removed :: integer(),\n"
                    "    git_commits :: [binary()],   % References to commits\n"
                    "    \n"
                    "    % Quality metrics\n"
                    "    tests_passed :: boolean(),\n"
                    "    test_coverage :: float(),\n"
                    "    type_check :: boolean(),\n"
                    "    lint_clean :: boolean(),\n"
                    "    security_clean :: boolean(),\n"
                    "    \n"
                    "    % Process tracking\n"
                    "    quality_gate_results :: [gate_result()],\n"
                    "    review_status :: approved | pending | rejected,\n"
                    "    wip_duration :: integer(),   % Time in progress\n"
                    "    \n"
                    "    % Value tracking\n"
                    "    value_points :: integer(),   % Business value\n"
                    "    complexity :: simple | medium | complex,\n"
                    "    category :: feature | bug | tech_debt | docs\n"
                    "}).\n"
                    "```\n\n"

                    "Receipts capture BOTH the technical change (like commits)\n"
                    "AND the quality/process metadata (unlike commits)."
                >>,
                examples => [
                    <<"Receipt #R-1234: Feature 'User Login' - 5 files, 247 lines, 95% coverage, all gates passed">>,
                    <<"Receipt #R-1235: Bug fix - 1 file, 12 lines, 100% coverage, security scan clean">>,
                    <<"Receipt links to git commits abc123, def456 for version history">>
                ],
                key_points => [
                    <<"Receipts = commits + quality + process">>,
                    <<"Comprehensive work record">>,
                    <<"Links to git commits (not replacement)">>,
                    <<"Enables quality tracking and analytics">>
                ]
            },

            #{
                title => <<"How Receipts Enable Quality">>,
                content => <<
                    "**1. Quality Gates Validate Receipts**\n"
                    "Before a receipt is 'issued', work must pass quality gates:\n"
                    "• Type checking: 100% type coverage\n"
                    "• Testing: ≥80% code coverage, all tests pass\n"
                    "• Linting: All 400+ Ruff rules enforced\n"
                    "• Security: Bandit scan clean\n"
                    "• Documentation: All public APIs documented\n\n"

                    "Receipt status:\n"
                    "• draft: Work in progress\n"
                    "• pending: Awaiting quality validation\n"
                    "• validated: All gates passed\n"
                    "• rejected: Gates failed, needs rework\n"
                    "• deployed: In production\n\n"

                    "**2. Receipts Make Quality Visible**\n"
                    "At any time, you can query:\n"
                    "• How many receipts passed all gates this sprint?\n"
                    "• What's the average test coverage across receipts?\n"
                    "• Which quality gates fail most often?\n"
                    "• How long does quality validation take?\n"
                    "• What's the trend in quality metrics?\n\n"

                    "**3. Receipts Enable Andon (Stop-the-Line)**\n"
                    "When quality gate fails:\n"
                    "• Receipt marked 'rejected'\n"
                    "• Work cannot move to next stage\n"
                    "• Developer notified with specific failures\n"
                    "• Team metrics show quality blockage\n"
                    "• Process stops until quality restored\n\n"

                    "**4. Receipts Track Value Delivery**\n"
                    "Link receipts to business outcomes:\n"
                    "• Which features (receipts) drove revenue?\n"
                    "• What's the quality profile of high-value work?\n"
                    "• Are we delivering value sustainably (quality + speed)?\n"
                    "• ROI of quality investment (prevented bugs)"
                >>,
                diagrams => [
                    <<"Receipt Lifecycle: Draft → Pending → [Quality Gates] → Validated → Deployed">>,
                    <<"Quality gates block receipt validation until standards met">>
                ],
                key_points => [
                    <<"Gates validate receipts before acceptance">>,
                    <<"Quality metrics visible and queryable">>,
                    <<"Enables Andon stop-the-line">>,
                    <<"Links quality to business value">>
                ]
            },

            #{
                title => <<"Receipts and Git: Complementary">>,
                content => <<
                    "TCPS doesn't replace Git—it complements it:\n\n"

                    "**Git provides:**\n"
                    "• Version control (what changed, when, by whom)\n"
                    "• Branching and merging (parallel development)\n"
                    "• History and blame (code archaeology)\n"
                    "• Collaboration (distributed teams)\n\n"

                    "**Receipts provide:**\n"
                    "• Quality tracking (did it meet standards?)\n"
                    "• Work unit tracking (feature/bug/task completion)\n"
                    "• Process modeling (gates, reviews, deployment)\n"
                    "• Business value (link code to outcomes)\n\n"

                    "**Together:**\n"
                    "• Receipt references git commits (technical history)\n"
                    "• Git commit messages reference receipts (work context)\n"
                    "• Receipt = 'logical unit', Commits = 'technical steps'\n"
                    "• Receipt persists even if commits squashed/rebased\n\n"

                    "**Example:**\n"
                    "Feature: User Authentication (Receipt #R-1234)\n"
                    "• Git commit abc123: 'Add login endpoint'\n"
                    "• Git commit def456: 'Add password hashing'\n"
                    "• Git commit ghi789: 'Add session management'\n"
                    "• Receipt R-1234: 'User Authentication feature - 3 commits, 95% coverage, all gates passed'\n\n"

                    "Git tracks how you built it.\n"
                    "Receipt tracks what you built and whether it's quality."
                >>,
                key_points => [
                    <<"Git = version control, Receipts = work tracking">>,
                    <<"Complementary, not competing">>,
                    <<"Receipts reference git commits">>,
                    <<"Different abstraction levels (logical vs technical)">>
                ]
            }
        ],

        analogies => [
            <<"Git commits = individual transactions; Receipts = daily sales report with quality metrics">>,
            <<"Git = tool marks on wood; Receipt = finished furniture with quality certificate">>,
            <<"Git = ingredients list; Receipt = recipe with nutrition facts and taste rating">>
        ],

        trade_offs => #{
            pros => [
                <<"Comprehensive quality tracking">>,
                <<"Clear work units (not just commits)">>,
                <<"Process modeling (gates, stages)">>,
                <<"Business value linkage">>,
                <<"Andon enablement (stop on quality failure)">>,
                <<"Analytics and continuous improvement">>
            ],
            cons => [
                <<"Additional bookkeeping overhead">>,
                <<"Learning curve (new concept)">>,
                <<"Requires tooling (TCPS system)">>,
                <<"Possible duplication with issue trackers">>,
                <<"Need to maintain receipt-commit mapping">>
            ]
        },

        related => [
            <<"quality_gates_vs_ci">>,
            <<"jidoka_philosophy">>,
            <<"andon_thinking">>,
            <<"dual_storage">>
        ],

        tags => [
            <<"receipts">>,
            <<"git">>,
            <<"quality">>,
            <<"tracking">>,
            <<"design">>,
            <<"work units">>
        ],

        difficulty => intermediate,
        reading_time_minutes => 10
    }.

%% @doc Why Quality Gates vs Traditional CI/CD?
quality_gates_vs_ci() ->
    #{
        id => <<"quality_gates_vs_ci">>,
        title => <<"Why Quality Gates vs Traditional CI/CD?">>,
        category => design_decisions,
        summary => <<"Understand the difference between TCPS quality gates and traditional CI/CD pipelines, and why gates enforce higher standards.">>,

        sections => [
            #{
                title => <<"Traditional CI/CD: Best Effort">>,
                content => <<
                    "Most CI/CD pipelines operate on a 'best effort' model:\n\n"

                    "**Typical CI/CD Pipeline:**\n"
                    "1. Developer pushes code\n"
                    "2. CI runs tests\n"
                    "3. If tests pass → merge allowed\n"
                    "4. If tests fail → merge blocked\n\n"

                    "Sounds good, but there are problems:\n\n"

                    "**Problem 1: Coverage Theater**\n"
                    "• 'All tests pass!' (but only 30% coverage)\n"
                    "• New code completely untested\n"
                    "• CI says green, quality is red\n\n"

                    "**Problem 2: Quality Decay**\n"
                    "• Coverage drops from 80% → 75% → 70%\n"
                    "• Each commit slightly worse than last\n"
                    "• No enforcement of quality trends\n"
                    "• Technical debt accumulates silently\n\n"

                    "**Problem 3: Binary Pass/Fail**\n"
                    "• Tests pass? Ship it!\n"
                    "• No measurement of HOW WELL it passed\n"
                    "• Missing: coverage, complexity, duplication\n"
                    "• False sense of security\n\n"

                    "**Problem 4: Optional Quality Checks**\n"
                    "• Linting: Warning only (merge anyway)\n"
                    "• Type checking: Gradual/optional\n"
                    "• Security scanning: Run monthly (not per-commit)\n"
                    "• Documentation: 'Nice to have'\n\n"

                    "CI/CD often becomes a checkbox exercise:\n"
                    "'Tests pass, ship it!' without asking 'Is this actually quality code?'"
                >>,
                key_points => [
                    <<"CI/CD often just checks tests pass">>,
                    <<"No coverage enforcement">>,
                    <<"Quality can decay gradually">>,
                    <<"Many quality checks optional">>
                ]
            },

            #{
                title => <<"TCPS Quality Gates: Non-Negotiable Standards">>,
                content => <<
                    "Quality Gates in TCPS enforce **non-negotiable standards**:\n\n"

                    "**Gate 1: Type Safety (100% Required)**\n"
                    "• Every function has type annotations\n"
                    "• Mypy runs in --strict mode\n"
                    "• NO Any types without justification\n"
                    "• NO type: ignore without documented reason\n"
                    "→ Receipt rejected if ANY type error\n\n"

                    "**Gate 2: Test Coverage (≥80% Required)**\n"
                    "• Line coverage measured automatically\n"
                    "• Branch coverage tracked\n"
                    "• Coverage cannot decrease\n"
                    "• New code must meet minimum coverage\n"
                    "→ Receipt rejected if coverage < 80%\n\n"

                    "**Gate 3: Code Quality (ALL 400+ Rules)**\n"
                    "• Ruff enforces ALL rules (no subsets)\n"
                    "• NO noqa comments without justification\n"
                    "• Complexity limits enforced\n"
                    "• Import organization required\n"
                    "→ Receipt rejected if ANY violation\n\n"

                    "**Gate 4: Security (Zero Vulnerabilities)**\n"
                    "• Bandit security scanner\n"
                    "• Dependency vulnerability checks\n"
                    "• No hardcoded secrets\n"
                    "• SQL injection prevention\n"
                    "→ Receipt rejected if ANY security issue\n\n"

                    "**Gate 5: Documentation (All Public APIs)**\n"
                    "• NumPy-style docstrings required\n"
                    "• Parameter descriptions mandatory\n"
                    "• Return types documented\n"
                    "• Examples for complex functions\n"
                    "→ Receipt rejected if missing docstrings\n\n"

                    "**Key Difference:**\n"
                    "Traditional CI: 'Nice if tests pass'\n"
                    "TCPS Gates: 'MUST pass ALL gates, no exceptions'"
                >>,
                examples => [
                    <<"Gate blocks merge: 'Type coverage 98% (requires 100%)'">>,
                    <<"Gate blocks merge: 'Test coverage 78% (requires ≥80%)'">>,
                    <<"Gate blocks merge: 'Ruff rule F401 violated (unused import)'">>,
                    <<"Gate blocks merge: 'Bandit B105: Hardcoded password detected'">>
                ],
                key_points => [
                    <<"Five mandatory quality gates">>,
                    <<"100% type coverage required">>,
                    <<"80%+ test coverage required">>,
                    <<"ALL quality rules enforced">>,
                    <<"Zero tolerance for violations">>
                ]
            },

            #{
                title => <<"The Ratchet Effect: Quality Only Goes Up">>,
                content => <<
                    "TCPS gates implement a 'quality ratchet':\n\n"

                    "**How it works:**\n"
                    "• Measure current quality baseline\n"
                    "• New work must meet or exceed baseline\n"
                    "• If new work improves quality, update baseline\n"
                    "• Baseline can only stay same or increase (never decrease)\n\n"

                    "**Example: Test Coverage Ratchet**\n"
                    "Day 1: Project at 75% coverage\n"
                    "• Gate threshold: ≥75% (current baseline)\n"
                    "• New PR adds code, coverage → 78%\n"
                    "• Gate passes (78% ≥ 75%)\n"
                    "• New baseline: 78%\n\n"

                    "Day 2: Project at 78% coverage\n"
                    "• Gate threshold: ≥78% (updated baseline)\n"
                    "• New PR adds code, coverage → 76%\n"
                    "• Gate FAILS (76% < 78%)\n"
                    "• Developer must add tests to reach ≥78%\n\n"

                    "Day 3: Project at 78% coverage\n"
                    "• New PR adds code, coverage → 82%\n"
                    "• Gate passes\n"
                    "• New baseline: 82%\n\n"

                    "Over time:\n"
                    "75% → 78% → 78% → 82% → 85% → 87% → ...\n\n"

                    "Quality trends upward through continuous enforcement.\n"
                    "No 'one step forward, two steps back' quality decay."
                >>,
                diagrams => [
                    <<"Quality Ratchet: ↗ (up or flat), never ↘ (down)">>,
                    <<"Coverage: 75% → 78% → 78% → 82% → 85% (monotonically increasing)">>
                ],
                key_points => [
                    <<"Baseline = current quality level">>,
                    <<"New work must meet baseline">>,
                    <<"Improvements update baseline">>,
                    <<"Quality only increases, never decreases">>,
                    <<"Gradual, sustainable improvement">>
                ]
            },

            #{
                title => <<"Cultural Shift: Quality is Non-Negotiable">>,
                content => <<
                    "The biggest difference isn't technical—it's cultural.\n\n"

                    "**Traditional CI/CD Culture:**\n"
                    "• 'Tests pass, good enough!'\n"
                    "• Quality checks are suggestions\n"
                    "• Ship fast, fix later\n"
                    "• Warnings ignored if tests green\n"
                    "• Coverage decay accepted\n\n"

                    "**TCPS Quality Gate Culture:**\n"
                    "• 'All gates pass, then we discuss shipping'\n"
                    "• Quality checks are requirements\n"
                    "• Quality first, speed follows\n"
                    "• NO warnings tolerated\n"
                    "• Quality improvement celebrated\n\n"

                    "This requires buy-in:\n\n"

                    "**Developers must:**\n"
                    "• Accept that quality takes time (upfront)\n"
                    "• Learn to write tests FIRST (TDD)\n"
                    "• Value type safety and documentation\n"
                    "• See gates as helpful, not obstacles\n\n"

                    "**Management must:**\n"
                    "• Accept that initial velocity may slow\n"
                    "• Invest in quality infrastructure\n"
                    "• Celebrate quality improvements\n"
                    "• Not pressure teams to skip gates\n\n"

                    "**The Payoff:**\n"
                    "After 2-3 months of strict gates:\n"
                    "• Technical debt drops dramatically\n"
                    "• Bug rates decrease (fewer prod incidents)\n"
                    "• Velocity INCREASES (less rework)\n"
                    "• Developer confidence grows\n"
                    "• Codebase becomes pleasure to work in\n\n"

                    "Quality gates are training wheels that become rocket boosters."
                >>,
                key_points => [
                    <<"Quality as requirement, not suggestion">>,
                    <<"Cultural shift required">>,
                    <<"Short-term slowdown, long-term speedup">>,
                    <<"Payoff in 2-3 months">>,
                    <<"Sustainable quality culture">>
                ]
            }
        ],

        analogies => [
            <<"Traditional CI = 'Did you spell-check?' Gates = 'Grammar, style, clarity, accuracy ALL required'">>,
            <<"CI = 'Car starts?' Gates = 'Safety inspection, emissions, insurance ALL current'">>,
            <<"CI = 'Building stands?' Gates = 'Building code, fire safety, accessibility ALL compliant'">>
        ],

        trade_offs => #{
            pros => [
                <<"Non-negotiable quality standards">>,
                <<"Prevents quality decay">>,
                <<"Ratchet effect (quality trends up)">>,
                <<"Comprehensive coverage (5+ dimensions)">>,
                <<"Clear, objective criteria">>,
                <<"Training effect (team learns quality)">>
            ],
            cons => [
                <<"Slower initial commits (more checks)">>,
                <<"Requires quality infrastructure investment">>,
                <<"Can feel restrictive at first">>,
                <<"Cultural shift needed">>,
                <<"May block urgent fixes (use escape hatch sparingly)">>,
                <<"Needs continuous tuning of thresholds">>
            ]
        },

        related => [
            <<"jidoka_philosophy">>,
            <<"andon_thinking">>,
            <<"receipts_not_commits">>,
            <<"quality_gates_vs_static">>
        ],

        tags => [
            <<"quality gates">>,
            <<"ci/cd">>,
            <<"standards">>,
            <<"jidoka">>,
            <<"culture">>,
            <<"ratchet">>
        ],

        difficulty => intermediate,
        reading_time_minutes => 11
    }.

%% @doc Why WIP Limits Matter in Software Development
wip_limits_matter() ->
    #{
        id => <<"wip_limits_matter">>,
        title => <<"Why WIP Limits Matter in Software Development">>,
        category => design_decisions,
        summary => <<"Understand why limiting work-in-progress is critical for flow, quality, and sustainable pace in software teams.">>,

        sections => [
            #{
                title => <<"The Multitasking Myth">>,
                content => <<
                    "**Cultural Belief:** More tasks in progress = more productivity\n\n"

                    "Manager: 'You're working on 10 things at once? Impressive!'\n"
                    "Reality: You're making slow progress on 10 things, fast progress on 0.\n\n"

                    "**The Science:**\n"
                    "Research shows context switching costs 20-80% productivity:\n"
                    "• 10 minutes to regain focus after switch\n"
                    "• Mental residue from previous task\n"
                    "• Increased error rates\n"
                    "• Decision fatigue\n\n"

                    "**Example:**\n"
                    "Developer juggling 10 tasks:\n"
                    "• 8am-9am: Task A (coding)\n"
                    "• 9am-10am: Task B review requested\n"
                    "• 10am-11am: Task C urgent bug\n"
                    "• 11am-12pm: Back to Task A (lost context, re-read code)\n"
                    "• 1pm-2pm: Task D meeting\n"
                    "• 2pm-3pm: Task E blocked, start Task F\n"
                    "• 3pm-4pm: Task A again (lost context again)\n"
                    "• 4pm-5pm: Task C escalated\n\n"

                    "End of day: 10 tasks started, 0 completed.\n"
                    "High activity, zero delivery.\n\n"

                    "**With WIP Limit of 3:**\n"
                    "• 8am-12pm: Task A (deep focus, completed)\n"
                    "• 1pm-3pm: Task B (deep focus, completed)\n"
                    "• 3pm-5pm: Task C (deep focus, completed)\n\n"

                    "End of day: 3 tasks completed.\n"
                    "Lower activity, high delivery."
                >>,
                key_points => [
                    <<"Multitasking is a productivity myth">>,
                    <<"Context switching costs 20-80% efficiency">>,
                    <<"More WIP = more activity, less delivery">>,
                    <<"Limiting WIP enables deep focus">>
                ]
            },

            #{
                title => <<"Little's Law: The Math of Flow">>,
                content => <<
                    "**Little's Law** (proven mathematical theorem):\n\n"

                    "  Lead Time = Work in Progress / Throughput\n\n"

                    "Or rearranged:\n\n"

                    "  Throughput = Work in Progress / Lead Time\n\n"

                    "**What this means:**\n\n"

                    "If you want faster delivery (lower lead time), you must:\n"
                    "• Reduce work in progress (WIP), OR\n"
                    "• Increase throughput (finish rate)\n\n"

                    "But throughput is constrained by team capacity.\n"
                    "You can't just 'work faster' sustainably.\n\n"

                    "The ONLY sustainable lever is reducing WIP.\n\n"

                    "**Example:**\n\n"

                    "Team A: WIP = 20 tasks, Throughput = 5 tasks/week\n"
                    "Lead Time = 20 / 5 = 4 weeks per task\n\n"

                    "Team B: WIP = 10 tasks, Throughput = 5 tasks/week\n"
                    "Lead Time = 10 / 5 = 2 weeks per task\n\n"

                    "Same team capacity (5 tasks/week).\n"
                    "Half the WIP = Half the lead time.\n\n"

                    "**But wait, won't limiting WIP reduce throughput?**\n\n"

                    "Counterintuitively, NO:\n"
                    "• Lower WIP → Less context switching\n"
                    "• Less context switching → More focus\n"
                    "• More focus → Higher quality\n"
                    "• Higher quality → Less rework\n"
                    "• Less rework → HIGHER throughput\n\n"

                    "Empirically, teams that cut WIP in half often see:\n"
                    "• Lead time drop 50%+\n"
                    "• Throughput INCREASE 20-30%\n"
                    "• Quality improve significantly\n\n"

                    "It's not magic—it's math + psychology."
                >>,
                examples => [
                    <<"Team with WIP=20: 4 week lead time">>,
                    <<"Same team with WIP=10: 2 week lead time">>,
                    <<"Same team with WIP=5: 1 week lead time">>,
                    <<"Throughput often increases as WIP decreases">>
                ],
                key_points => [
                    <<"Little's Law: Lead Time = WIP / Throughput">>,
                    <<"Lower WIP = faster delivery">>,
                    <<"Can't sustainably increase throughput">>,
                    <<"Reducing WIP is the key lever">>,
                    <<"Lower WIP often increases throughput">>
                ]
            },

            #{
                title => <<"Quality Degrades with High WIP">>,
                content => <<
                    "High WIP kills quality:\n\n"

                    "**1. Shallow Work**\n"
                    "When juggling many tasks:\n"
                    "• No deep focus on any single task\n"
                    "• Quick hacks instead of thoughtful solutions\n"
                    "• Copy-paste instead of refactoring\n"
                    "• 'Good enough' mentality\n\n"

                    "**2. Testing Shortcuts**\n"
                    "Overwhelmed by tasks:\n"
                    "• 'I'll add tests later' (never happens)\n"
                    "• Manual testing instead of automated\n"
                    "• Happy path only, no edge cases\n"
                    "• Coverage drops gradually\n\n"

                    "**3. Documentation Neglect**\n"
                    "Too many tasks:\n"
                    "• No time for docstrings\n"
                    "• No time for README updates\n"
                    "• No time for API docs\n"
                    "• Future maintainers suffer\n\n"

                    "**4. Technical Debt Accumulation**\n"
                    "Rushing between tasks:\n"
                    "• 'TODO: refactor this' comments pile up\n"
                    "• Code duplication unchecked\n"
                    "• Architecture shortcuts\n"
                    "• Complexity grows unchecked\n\n"

                    "**The Vicious Cycle:**\n"
                    "High WIP → Poor quality → More bugs → More urgent tasks →\n"
                    "Even higher WIP → Even worse quality → ...\n\n"

                    "**Breaking the Cycle:**\n"
                    "Low WIP → Deep focus → High quality → Fewer bugs →\n"
                    "Sustainable WIP → Maintained quality → ...\n\n"

                    "WIP limits create virtuous cycles."
                >>,
                key_points => [
                    <<"High WIP prevents deep work">>,
                    <<"Quality shortcuts become habit">>,
                    <<"Technical debt accumulates">>,
                    <<"Vicious cycle of WIP and quality">>,
                    <<"Low WIP creates virtuous cycles">>
                ]
            },

            #{
                title => <<"How to Set WIP Limits">>,
                content => <<
                    "**Start Conservative:**\n"
                    "Better to start too low than too high.\n"
                    "Rule of thumb: Team size ÷ 2\n"
                    "• 4-person team → Start with WIP limit of 2\n"
                    "• 6-person team → Start with WIP limit of 3\n\n"

                    "**Per-Column Limits (Kanban):**\n"
                    "Set limits for each workflow stage:\n"
                    "• To Do: Unlimited (prioritized backlog)\n"
                    "• In Progress: Team size ÷ 2 (e.g., 3 for 6-person team)\n"
                    "• Review: 2 (prevent review bottleneck)\n"
                    "• Testing: 2 (balance test load)\n"
                    "• Done: Unlimited (completed work)\n\n"

                    "**Per-Person Limits:**\n"
                    "Individual WIP limits:\n"
                    "• Coding: 1-2 tasks max\n"
                    "• Review: 1-2 PRs max\n"
                    "• Total: 3 tasks max across all types\n\n"

                    "**Tune Based on Data:**\n"
                    "After 2-4 weeks, measure:\n"
                    "• Average lead time (lower is better)\n"
                    "• Throughput (tasks completed per week)\n"
                    "• Blocked time (work waiting for others)\n"
                    "• Quality metrics (test coverage, bug rate)\n\n"

                    "Adjust limits:\n"
                    "• If often blocked → Slightly increase limit\n"
                    "• If quality suffering → Decrease limit\n"
                    "• If lead time too high → Decrease limit\n"
                    "• If idle time frequent → Slightly increase limit\n\n"

                    "**Golden Rule:**\n"
                    "When in doubt, lower the limit.\n"
                    "It's easier to loosen constraints than tighten them."
                >>,
                examples => [
                    <<"4-person team: WIP limit of 2 in Progress">>,
                    <<"6-person team: WIP limit of 3 in Progress, 2 in Review">>,
                    <<"Individual: Max 1 coding task, 1 review, 1 support issue">>,
                    <<"Tune monthly based on lead time and quality metrics">>
                ],
                key_points => [
                    <<"Start with team size ÷ 2">>,
                    <<"Set per-column limits (Kanban)">>,
                    <<"Set per-person limits (3 total max)">>,
                    <<"Tune based on data (lead time, quality)">>,
                    <<"When in doubt, lower the limit">>
                ]
            }
        ],

        analogies => [
            <<"Juggling: 3 balls = impressive; 10 balls = everything hits the floor">>,
            <<"Restaurant kitchen: Cook 3 dishes well vs. start 10 dishes poorly">>,
            <<"Highway traffic: Moderate traffic flows; too many cars = gridlock">>
        ],

        trade_offs => #{
            pros => [
                <<"Faster lead times (tasks complete sooner)">>,
                <<"Higher quality (deep focus enables thoroughness)">>,
                <<"Less context switching (more productive time)">>,
                <<"Better morale (sense of completion)">>,
                <<"Predictable flow (easier planning)">>,
                <<"Often increases throughput (less rework)">>
            ],
            cons => [
                <<"May feel slow at first (fewer tasks started)">>,
                <<"Requires discipline (resist starting new work)">>,
                <<"Can create idle time if limits too conservative">>,
                <<"Cultural resistance ('looks less busy')">>,
                <<"Needs visible management (Kanban board)">>
            ]
        },

        related => [
            <<"pull_vs_push">>,
            <<"heijunka_leveling">>,
            <<"why_tcps">>
        ],

        tags => [
            <<"wip limits">>,
            <<"flow">>,
            <<"kanban">>,
            <<"productivity">>,
            <<"quality">>,
            <<"little's law">>
        ],

        difficulty => intermediate,
        reading_time_minutes => 10
    }.

%% @doc Why TCPS Uses Dual Storage (JSON + RDF)
dual_storage() ->
    #{
        id => <<"dual_storage">>,
        title => <<"Why TCPS Uses Dual Storage (JSON + RDF)">>,
        category => design_decisions,
        summary => <<"Understand the rationale for dual storage: JSON for operational efficiency and RDF for semantic richness and knowledge integration.">>,

        sections => [
            #{
                title => <<"The Storage Trade-Off">>,
                content => <<
                    "Every storage format makes trade-offs:\n\n"

                    "**JSON/Document Storage:**\n"
                    "Pros:\n"
                    "• Fast read/write operations\n"
                    "• Simple, intuitive structure\n"
                    "• Easy to debug (human-readable)\n"
                    "• Great for operational systems\n"
                    "• Wide language support\n\n"

                    "Cons:\n"
                    "• No built-in semantics (meaning)\n"
                    "• Limited query capabilities\n"
                    "• Difficult to integrate external knowledge\n"
                    "• Schema evolution can be painful\n"
                    "• No inferencing or reasoning\n\n"

                    "**RDF/Semantic Storage:**\n"
                    "Pros:\n"
                    "• Rich semantic expressiveness\n"
                    "• Powerful query language (SPARQL)\n"
                    "• Easy integration with ontologies\n"
                    "• Schema-less (flexible evolution)\n"
                    "• Reasoning and inferencing\n"
                    "• Links to external knowledge graphs\n\n"

                    "Cons:\n"
                    "• Slower for operational read/write\n"
                    "• Steeper learning curve\n"
                    "• More complex debugging\n"
                    "• Overhead for simple operations\n"
                    "• Less widespread adoption\n\n"

                    "TCPS needs BOTH sets of benefits.\n"
                    "Solution: Use both, optimized for different purposes."
                >>,
                key_points => [
                    <<"JSON fast for operations, limited semantics">>,
                    <<"RDF rich semantics, slower operations">>,
                    <<"Different strengths for different needs">>,
                    <<"Dual storage gets best of both">>
                ]
            },

            #{
                title => <<"JSON: The Operational Layer">>,
                content => <<
                    "JSON handles day-to-day operations:\n\n"

                    "**Use Cases:**\n"
                    "• Receipt creation (fast write)\n"
                    "• Task status updates (frequent changes)\n"
                    "• Kanban board state (real-time UI)\n"
                    "• Quality gate results (quick validation)\n"
                    "• Developer queries (fast reads)\n\n"

                    "**Example: Receipt Storage (JSON)**\n"
                    "```json\n"
                    "{\n"
                    "  \"id\": \"R-1234\",\n"
                    "  \"task_id\": \"TASK-567\",\n"
                    "  \"developer\": \"alice@example.com\",\n"
                    "  \"timestamp\": 1706284800,\n"
                    "  \"quality_gates\": {\n"
                    "    \"type_check\": {\"passed\": true, \"coverage\": 100},\n"
                    "    \"test_coverage\": {\"passed\": true, \"coverage\": 87.5},\n"
                    "    \"lint\": {\"passed\": true, \"violations\": 0}\n"
                    "  }\n"
                    "}\n"
                    "```\n\n"

                    "**Why JSON works here:**\n"
                    "• Developer creates receipt → immediate write (< 10ms)\n"
                    "• Dashboard queries receipts → fast read (< 20ms)\n"
                    "• Quality gate updates status → quick update\n"
                    "• Simple structure matches operational needs\n\n"

                    "JSON is the 'hot' storage—active, frequently accessed data."
                >>,
                examples => [
                    <<"Receipt creation: 8ms write to JSON">>,
                    <<"Dashboard load: 15ms read from JSON">>,
                    <<"Quality gate update: 5ms JSON update">>,
                    <<"Real-time Kanban board: JSON state">>
                ],
                key_points => [
                    <<"JSON for fast operations">>,
                    <<"Day-to-day development workflow">>,
                    <<"Real-time dashboard queries">>,
                    <<"'Hot' operational storage">>
                ]
            },

            #{
                title => <<"RDF: The Semantic Layer">>,
                content => <<
                    "RDF adds semantic richness and knowledge integration:\n\n"

                    "**Use Cases:**\n"
                    "• Link receipts to TPS ontology (Jidoka, Heijunka concepts)\n"
                    "• Integrate with project knowledge graphs\n"
                    "• Complex analytics queries (SPARQL)\n"
                    "• Reasoning about patterns and trends\n"
                    "• Cross-project learning and insights\n\n"

                    "**Example: Receipt as RDF (Turtle)**\n"
                    "```turtle\n"
                    "@prefix tcps: <http://tcps.example.org/ontology#> .\n"
                    "@prefix tps: <http://tps.example.org/ontology#> .\n\n"

                    "<receipt/R-1234> a tcps:Receipt ;\n"
                    "  tcps:taskId <task/TASK-567> ;\n"
                    "  tcps:developer <developer/alice> ;\n"
                    "  tcps:timestamp \"2024-01-26T12:00:00Z\"^^xsd:dateTime ;\n"
                    "  tcps:embodiesJidoka tps:BuiltInQuality ;\n"
                    "  tcps:passedQualityGate tcps:TypeCheckGate ;\n"
                    "  tcps:passedQualityGate tcps:TestCoverageGate ;\n"
                    "  tcps:testCoverage \"87.5\"^^xsd:float ;\n"
                    "  tcps:contributesToHeijunka <sprint/Sprint-42> .\n"
                    "```\n\n"

                    "**Why RDF works here:**\n"
                    "• Links receipt to TPS concepts (Jidoka, Heijunka)\n"
                    "• Enables queries like: 'Show all receipts embodying Jidoka'\n"
                    "• Reasoning: Infer quality trends from ontology rules\n"
                    "• Integration: Connect to external knowledge graphs\n\n"

                    "**Example Query (SPARQL):**\n"
                    "```sparql\n"
                    "# Find all developers who consistently embody Jidoka\n"
                    "SELECT ?developer (AVG(?coverage) as ?avgCoverage)\n"
                    "WHERE {\n"
                    "  ?receipt a tcps:Receipt ;\n"
                    "    tcps:developer ?developer ;\n"
                    "    tcps:embodiesJidoka tps:BuiltInQuality ;\n"
                    "    tcps:testCoverage ?coverage .\n"
                    "}\n"
                    "GROUP BY ?developer\n"
                    "HAVING (AVG(?coverage) > 85)\n"
                    "```\n\n"

                    "RDF is the 'cold' storage—analytical, knowledge-oriented data."
                >>,
                examples => [
                    <<"Link receipt to TPS ontology concepts">>,
                    <<"SPARQL query: 'Developers embodying Jidoka'">>,
                    <<"Reasoning: Infer quality patterns">>,
                    <<"Integration with external knowledge graphs">>
                ],
                key_points => [
                    <<"RDF for semantic richness">>,
                    <<"Analytics and knowledge queries">>,
                    <<"Ontology integration (TPS concepts)">>,
                    <<"'Cold' analytical storage">>
                ]
            },

            #{
                title => <<"Synchronization: Best of Both Worlds">>,
                content => <<
                    "How dual storage works in practice:\n\n"

                    "**Write Path:**\n"
                    "1. Developer completes work\n"
                    "2. System creates receipt → Write to JSON (fast, 8ms)\n"
                    "3. Background process → Convert to RDF (async, 500ms)\n"
                    "4. Store in RDF triplestore (semantic layer)\n\n"

                    "Result:\n"
                    "• Developer sees immediate confirmation (JSON)\n"
                    "• Semantic data available shortly after (RDF)\n"
                    "• No blocking on slow RDF operations\n\n"

                    "**Read Path (Operational):**\n"
                    "1. Dashboard requests recent receipts\n"
                    "2. Query JSON storage (fast, 15ms)\n"
                    "3. Return results to UI\n\n"

                    "**Read Path (Analytical):**\n"
                    "1. Data scientist queries patterns\n"
                    "2. Run SPARQL query on RDF (semantic, 2s)\n"
                    "3. Return rich, contextualized insights\n\n"

                    "**Consistency:**\n"
                    "• JSON is source of truth for operational data\n"
                    "• RDF is eventual consistency (background sync)\n"
                    "• Reconciliation process ensures alignment\n"
                    "• Typical lag: < 1 second for RDF availability\n\n"

                    "**Failure Handling:**\n"
                    "• JSON write fails → Receipt creation fails (immediate feedback)\n"
                    "• RDF sync fails → Retry queue (eventual consistency)\n"
                    "• Monitoring alerts if RDF lag exceeds threshold\n"
                    "• Reconciliation job runs nightly to catch any gaps"
                >>,
                diagrams => [
                    <<"Write: Receipt → JSON (8ms) → RDF (async 500ms)">>,
                    <<"Read Operational: Query → JSON (15ms) → UI">>,
                    <<"Read Analytical: Query → RDF (2s) → Insights">>
                ],
                key_points => [
                    <<"JSON written immediately (fast)">>,
                    <<"RDF synced asynchronously (semantic)">>,
                    <<"Operational queries use JSON">>,
                    <<"Analytical queries use RDF">>,
                    <<"Eventual consistency with reconciliation">>
                ]
            }
        ],

        analogies => [
            <<"JSON = Cash register (fast transactions); RDF = Accounting system (deep analysis)">>,
            <<"JSON = Day planner (quick reference); RDF = Personal knowledge graph (connections)">>,
            <<"JSON = Speedometer (current speed); RDF = Trip computer (patterns, insights)">>
        ],

        trade_offs => #{
            pros => [
                <<"Fast operations (JSON) + Rich semantics (RDF)">>,
                <<"Operational efficiency + Analytical power">>,
                <<"Simple debugging (JSON) + Deep queries (SPARQL)">>,
                <<"Immediate feedback (JSON) + Knowledge integration (RDF)">>,
                <<"Best tool for each job">>
            ],
            cons => [
                <<"Storage duplication (2x disk space)">>,
                <<"Synchronization complexity">>,
                <<"Potential consistency lag (eventual)">>,
                <<"Need to maintain both systems">>,
                <<"Learning curve for RDF/SPARQL">>
            ]
        },

        related => [
            <<"receipts_not_commits">>,
            <<"mcp_integration">>
        ],

        tags => [
            <<"storage">>,
            <<"json">>,
            <<"rdf">>,
            <<"semantics">>,
            <<"architecture">>,
            <<"dual storage">>
        ],

        difficulty => advanced,
        reading_time_minutes => 10
    }.

%% @doc Why MCP Integration for TCPS?
mcp_integration() ->
    #{
        id => <<"mcp_integration">>,
        title => <<"Why MCP Integration for TCPS?">>,
        category => design_decisions,
        summary => <<"Understand why TCPS integrates with Model Context Protocol (MCP) and how this enables AI-assisted quality and process automation.">>,

        sections => [
            #{
                title => <<"What is MCP?">>,
                content => <<
                    "**Model Context Protocol (MCP)** is Anthropic's standard for connecting\n"
                    "AI assistants (like Claude) to external tools and data sources.\n\n"

                    "Think of MCP as an 'API for AI':\n"
                    "• Defines standard way for AI to call tools\n"
                    "• Provides context from external systems\n"
                    "• Enables structured, type-safe interactions\n"
                    "• Allows AI to read and write application state\n\n"

                    "**Before MCP:**\n"
                    "AI: 'I need to check test coverage'\n"
                    "Developer: *Manually runs pytest --cov, copies output, pastes to AI*\n"
                    "AI: *Parses unstructured text, may misunderstand*\n\n"

                    "**With MCP:**\n"
                    "AI: *Calls tcps_quality.get_coverage() via MCP*\n"
                    "TCPS: *Returns structured JSON: {coverage: 87.5, status: 'passing'}*\n"
                    "AI: *Receives structured data, understands perfectly*\n\n"

                    "MCP makes AI assistants 'first-class citizens' in development workflows."
                >>,
                key_points => [
                    <<"MCP = standard API for AI assistants">>,
                    <<"Structured, type-safe interactions">>,
                    <<"AI can read/write application state">>,
                    <<"Eliminates manual copy-paste to AI">>
                ]
            },

            #{
                title => <<"TCPS + MCP: AI-Assisted Quality">>,
                content => <<
                    "TCPS exposes quality and process data via MCP:\n\n"

                    "**Use Case 1: Quality Gate Analysis**\n"
                    "Developer: 'Claude, why did my quality gate fail?'\n"
                    "Claude:\n"
                    "  1. *Calls tcps_quality.get_gate_results(receipt_id) via MCP*\n"
                    "  2. *Receives: {type_check: pass, coverage: fail (78% < 80%), lint: pass}*\n"
                    "  3. *Calls tcps_quality.get_uncovered_lines(receipt_id)*\n"
                    "  4. *Receives: ['src/utils.py:42-45', 'src/auth.py:67-72']*\n"
                    "  5. Responds: 'Your coverage gate failed (78% < 80%). Missing tests for:\n"
                    "     - src/utils.py lines 42-45 (format_response function)\n"
                    "     - src/auth.py lines 67-72 (validate_token error handling)\n"
                    "     Add tests for these paths to pass the gate.'\n\n"

                    "**Use Case 2: Work Planning**\n"
                    "Developer: 'Claude, what should I work on next?'\n"
                    "Claude:\n"
                    "  1. *Calls tcps_kanban.get_board_state() via MCP*\n"
                    "  2. *Receives WIP limits, available tasks, priorities*\n"
                    "  3. *Calls tcps_kanban.can_pull_task() via MCP*\n"
                    "  4. *Checks: In Progress (2/3), can pull 1 more task*\n"
                    "  5. Responds: 'You can pull 1 more task (WIP: 2/3). Highest priority:\n"
                    "     TASK-567: Implement password reset (8 points, feature)'\n\n"

                    "**Use Case 3: Receipt Generation**\n"
                    "Developer: 'Claude, create a receipt for my completed work'\n"
                    "Claude:\n"
                    "  1. *Calls tcps_git.get_recent_commits() via MCP*\n"
                    "  2. *Calls tcps_quality.run_all_gates() via MCP*\n"
                    "  3. *Receives quality results*\n"
                    "  4. *Calls tcps_receipt.create({...}) via MCP*\n"
                    "  5. Responds: 'Receipt R-1234 created:\n"
                    "     - 3 commits, 5 files changed, +247/-45 lines\n"
                    "     - Coverage: 89.2% (✓ passed)\n"
                    "     - Type check: 100% (✓ passed)\n"
                    "     - All gates: ✓ PASSED'\n\n"

                    "AI becomes an intelligent interface to TCPS."
                >>,
                examples => [
                    <<"'Why did my gate fail?' → AI analyzes gate results">>,
                    <<"'What should I work on?' → AI checks Kanban + WIP limits">>,
                    <<"'Create a receipt' → AI runs gates + generates receipt">>,
                    <<"'How's our sprint quality?' → AI queries RDF + analyzes trends">>
                ],
                key_points => [
                    <<"AI analyzes quality gate failures">>,
                    <<"AI suggests next work (respecting WIP limits)">>,
                    <<"AI automates receipt creation">>,
                    <<"AI provides quality insights">>
                ]
            },

            #{
                title => <<"Educational Dimension: Diataxis via MCP">>,
                content => <<
                    "TCPS uses MCP to expose Diataxis documentation:\n\n"

                    "**Diataxis Framework:**\n"
                    "• Tutorials: Learning-oriented (step-by-step)\n"
                    "• How-To Guides: Task-oriented (practical steps)\n"
                    "• Explanation: Understanding-oriented (concepts, 'why')\n"
                    "• Reference: Information-oriented (technical specs)\n\n"

                    "**MCP Endpoints:**\n"
                    "```erlang\n"
                    "% Tutorials\n"
                    "tcps_mcp:get_tutorial(\"getting_started\")\n"
                    "tcps_mcp:get_tutorial(\"first_quality_gate\")\n\n"

                    "% How-To Guides\n"
                    "tcps_mcp:get_howto(\"create_receipt\")\n"
                    "tcps_mcp:get_howto(\"configure_quality_gates\")\n\n"

                    "% Explanations\n"
                    "tcps_mcp:get_explanation(\"why_tcps\")\n"
                    "tcps_mcp:get_explanation(\"jidoka_philosophy\")\n\n"

                    "% Reference\n"
                    "tcps_mcp:get_reference(\"quality_gate_api\")\n"
                    "tcps_mcp:get_reference(\"receipt_schema\")\n"
                    "```\n\n"

                    "**AI-Powered Learning:**\n"
                    "Developer: 'Claude, I don't understand why WIP limits help'\n"
                    "Claude:\n"
                    "  1. *Calls tcps_mcp:get_explanation(\"wip_limits_matter\") via MCP*\n"
                    "  2. *Receives full explanation with analogies, examples*\n"
                    "  3. *Summarizes key points in conversational tone*\n"
                    "  4. Responds with contextualized explanation\n\n"

                    "Developer: 'How do I set up my first quality gate?'\n"
                    "Claude:\n"
                    "  1. *Calls tcps_mcp:get_tutorial(\"first_quality_gate\") via MCP*\n"
                    "  2. *Receives step-by-step tutorial*\n"
                    "  3. *Adapts to developer's current project context*\n"
                    "  4. Responds with personalized, actionable steps\n\n"

                    "MCP makes documentation conversational and context-aware."
                >>,
                key_points => [
                    <<"Diataxis docs exposed via MCP">>,
                    <<"AI retrieves and adapts documentation">>,
                    <<"Context-aware learning (personalized)">>,
                    <<"Conversational documentation access">>
                ]
            },

            #{
                title => <<"The Vision: AI-Native Development">>,
                content => <<
                    "MCP integration enables 'AI-native' development workflow:\n\n"

                    "**Traditional Workflow:**\n"
                    "1. Developer writes code\n"
                    "2. Runs tests manually\n"
                    "3. Checks coverage manually\n"
                    "4. Fixes issues\n"
                    "5. Commits\n"
                    "6. Waits for CI\n"
                    "7. CI fails, back to step 4\n\n"

                    "**AI-Native Workflow (with MCP):**\n"
                    "1. Developer: 'Claude, I need to implement password reset'\n"
                    "2. Claude:\n"
                    "   - *Checks Kanban WIP limits via MCP*\n"
                    "   - *Reviews related receipts via MCP*\n"
                    "   - *Suggests implementation approach*\n"
                    "3. Developer writes code with Claude's assistance\n"
                    "4. Developer: 'Claude, run quality checks'\n"
                    "5. Claude:\n"
                    "   - *Runs all quality gates via MCP*\n"
                    "   - *Reports: 'Coverage 78%, need tests for error handling'*\n"
                    "6. Developer adds tests (with Claude's help)\n"
                    "7. Claude:\n"
                    "   - *Re-runs gates via MCP*\n"
                    "   - *Reports: 'All gates passed!'*\n"
                    "8. Developer: 'Claude, create receipt'\n"
                    "9. Claude:\n"
                    "   - *Creates receipt via MCP*\n"
                    "   - *Updates Kanban via MCP*\n"
                    "   - *Reports: 'Receipt R-1234 created, task moved to Done'*\n\n"

                    "**Benefits:**\n"
                    "• Immediate feedback (no waiting for CI)\n"
                    "• Guided quality (AI suggests improvements)\n"
                    "• Automated process (receipt creation, Kanban updates)\n"
                    "• Learning integrated (AI explains concepts as you work)\n"
                    "• Higher quality (gates enforced interactively)\n\n"

                    "AI becomes a quality-aware pair programmer."
                >>,
                key_points => [
                    <<"AI-native workflow via MCP">>,
                    <<"Immediate quality feedback">>,
                    <<"Guided improvements">>,
                    <<"Automated process steps">>,
                    <<"Quality-aware pair programming">>
                ]
            }
        ],

        analogies => [
            <<"MCP = USB standard (universal connection between AI and apps)">>,
            <<"AI via MCP = Expert consultant with direct system access (not just verbal advice)">>,
            <<"MCP docs = Interactive encyclopedia (AI retrieves and explains)">>
        ],

        trade_offs => #{
            pros => [
                <<"AI-assisted quality and process">>,
                <<"Immediate feedback (no CI wait)">>,
                <<"Conversational documentation">>,
                <<"Automated routine tasks">>,
                <<"Learning integrated into workflow">>,
                <<"Context-aware assistance">>
            ],
            cons => [
                <<"Requires MCP server setup">>,
                <<"Dependency on AI assistant (Claude)">>,
                <<"Learning curve (MCP concepts)">>,
                <<"Potential over-reliance on AI">>,
                <<"Security considerations (AI access to data)">>,
                <<"Requires network connection for AI">>
            ]
        },

        related => [
            <<"quality_gates_vs_ci">>,
            <<"receipts_not_commits">>,
            <<"dual_storage">>
        ],

        tags => [
            <<"mcp">>,
            <<"ai">>,
            <<"claude">>,
            <<"automation">>,
            <<"diataxis">>,
            <<"integration">>
        ],

        difficulty => advanced,
        reading_time_minutes => 11
    }.

%%%===================================================================
%%% Comparison Explanations
%%%===================================================================

%% @doc TCPS vs Traditional DevOps
tcps_vs_devops() ->
    #{
        id => <<"tcps_vs_devops">>,
        title => <<"TCPS vs Traditional DevOps">>,
        category => comparisons,
        summary => <<"Compare TCPS to traditional DevOps practices, highlighting philosophical differences and complementary strengths.">>,

        sections => [
            #{
                title => <<"Core Philosophy Differences">>,
                content => <<
                    "**DevOps Philosophy:**\n"
                    "• Break down silos between Dev and Ops\n"
                    "• Automate deployment pipelines (CI/CD)\n"
                    "• Monitor production systems\n"
                    "• Faster delivery through automation\n"
                    "• 'Move fast, infrastructure as code'\n\n"

                    "**TCPS Philosophy:**\n"
                    "• Build quality into every step (Jidoka)\n"
                    "• Pull-based work flow (Kanban)\n"
                    "• Stop and fix when quality at risk (Andon)\n"
                    "• Sustainable pace through leveling (Heijunka)\n"
                    "• 'Quality first, speed follows'\n\n"

                    "**Key Difference:**\n"
                    "DevOps focuses on DELIVERY (automation, pipelines, deployment).\n"
                    "TCPS focuses on QUALITY (built-in standards, prevention, sustainable pace).\n\n"

                    "They're not competing—they're complementary.\n"
                    "DevOps asks: 'How do we ship fast?'\n"
                    "TCPS asks: 'How do we ensure what we ship is quality?'"
                >>,
                key_points => [
                    <<"DevOps = delivery focus">>,
                    <<"TCPS = quality focus">>,
                    <<"Complementary, not competing">>,
                    <<"Different philosophical roots">>
                ]
            },

            #{
                title => <<"Where They Overlap">>,
                content => <<
                    "**Both Value:**\n"
                    "• Automation (reduce manual toil)\n"
                    "• Fast feedback loops\n"
                    "• Continuous improvement\n"
                    "• Measuring and optimizing\n"
                    "• Team collaboration\n\n"

                    "**Both Use:**\n"
                    "• CI/CD pipelines\n"
                    "• Automated testing\n"
                    "• Infrastructure as code\n"
                    "• Monitoring and observability\n"
                    "• Version control (Git)\n\n"

                    "**Integration Example:**\n"
                    "TCPS quality gates run IN DevOps pipelines:\n\n"

                    "```yaml\n"
                    "# .github/workflows/ci.yml (DevOps)\n"
                    "- name: Run TCPS Quality Gates\n"
                    "  run: tcps quality-gate --all\n"
                    "  # Type check, coverage, lint, security (TCPS)\n\n"

                    "- name: Deploy (if gates pass)\n"
                    "  run: kubectl apply -f deploy.yml\n"
                    "  # Deployment automation (DevOps)\n"
                    "```\n\n"

                    "DevOps provides the automation infrastructure.\n"
                    "TCPS provides the quality standards enforced in that infrastructure."
                >>,
                examples => [
                    <<"GitHub Actions (DevOps) runs TCPS quality gates">>,
                    <<"Kubernetes deployment (DevOps) blocked by TCPS Andon">>,
                    <<"Terraform (DevOps) + TCPS quality checks for IaC">>,
                    <<"Prometheus monitoring (DevOps) + TCPS quality metrics">>
                ],
                key_points => [
                    <<"Both value automation and feedback">>,
                    <<"TCPS gates run in DevOps pipelines">>,
                    <<"DevOps = infrastructure, TCPS = standards">>,
                    <<"Integrated, not separate">>
                ]
            },

            #{
                title => <<"Where They Differ">>,
                content => <<
                    "**Pace and Sustainability:**\n"
                    "DevOps: 'Deploy 10x per day!'\n"
                    "TCPS: 'Deploy when quality standards met, at sustainable pace'\n\n"

                    "DevOps can encourage unsustainable speed.\n"
                    "TCPS enforces Heijunka (leveling) for long-term health.\n\n"

                    "**Quality Standards:**\n"
                    "DevOps: 'Run tests in CI'\n"
                    "TCPS: '100% type coverage, 80%+ test coverage, ALL lint rules, zero security issues'\n\n"

                    "DevOps defines the 'how' (pipelines).\n"
                    "TCPS defines the 'what' (specific quality thresholds).\n\n"

                    "**Stop-the-Line Authority:**\n"
                    "DevOps: Deployment blocked if tests fail\n"
                    "TCPS: ANYONE can pull Andon cord, culture of quality empowerment\n\n"

                    "DevOps is technical (automated blocking).\n"
                    "TCPS is cultural (empowered quality ownership).\n\n"

                    "**Work Management:**\n"
                    "DevOps: Not opinionated about work process\n"
                    "TCPS: WIP limits, Kanban pull, Heijunka leveling\n\n"

                    "DevOps doesn't dictate HOW teams organize work.\n"
                    "TCPS provides specific work management practices (from TPS).\n\n"

                    "**Receipts vs Commits:**\n"
                    "DevOps: Git commits are units of work\n"
                    "TCPS: Receipts (commits + quality metadata) are units of work\n\n"

                    "DevOps tracks technical changes.\n"
                    "TCPS tracks quality-validated work units."
                >>,
                key_points => [
                    <<"DevOps: speed; TCPS: sustainable pace">>,
                    <<"DevOps: pipelines; TCPS: quality thresholds">>,
                    <<"DevOps: technical; TCPS: cultural">>,
                    <<"DevOps: commits; TCPS: receipts">>,
                    <<"Different emphases, both valuable">>
                ]
            },

            #{
                title => <<"When to Use What">>,
                content => <<
                    "**Use DevOps When:**\n"
                    "• You need deployment automation\n"
                    "• Infrastructure management is key\n"
                    "• Scaling deployment frequency\n"
                    "• Breaking down Dev/Ops silos\n"
                    "• Cloud-native architectures\n\n"

                    "**Use TCPS When:**\n"
                    "• Quality is non-negotiable (safety-critical, financial, healthcare)\n"
                    "• Technical debt is a major problem\n"
                    "• Team experiencing burnout from firefighting\n"
                    "• Need sustainable, predictable pace\n"
                    "• Want manufacturing-grade quality standards\n\n"

                    "**Use Both When:**\n"
                    "• You want fast delivery AND high quality\n"
                    "• You need automation AND standards\n"
                    "• You value speed AND sustainability\n\n"

                    "**Integration Pattern:**\n"
                    "1. Adopt DevOps for automation infrastructure (CI/CD, IaC, monitoring)\n"
                    "2. Layer TCPS quality gates into DevOps pipelines\n"
                    "3. Use TCPS work management (Kanban, WIP limits, Heijunka)\n"
                    "4. Combine DevOps delivery speed with TCPS quality culture\n\n"

                    "Result: Fast, automated delivery of high-quality software at sustainable pace."
                >>,
                key_points => [
                    <<"DevOps for automation infrastructure">>,
                    <<"TCPS for quality standards">>,
                    <<"Best together (speed + quality)">>,
                    <<"Layer TCPS into DevOps pipelines">>
                ]
            }
        ],

        analogies => [
            <<"DevOps = Highway system (fast transportation); TCPS = Traffic rules + safety standards">>,
            <<"DevOps = Fast food kitchen (speed, automation); TCPS = Michelin star standards (quality, process)">>,
            <<"DevOps = Race car (performance); TCPS = Safety engineering (quality, sustainability)">>
        ],

        trade_offs => #{
            pros => [
                <<"Complementary strengths (automation + quality)">>,
                <<"Can integrate (TCPS gates in DevOps pipelines)">>,
                <<"Different focus areas (delivery vs quality)">>,
                <<"Both improve software development">>
            ],
            cons => [
                <<"TCPS may slow initial DevOps velocity">>,
                <<"DevOps can encourage unsustainable pace">>,
                <<"Cultural tension (speed vs quality)">>,
                <<"Need discipline to balance both">>
            ]
        },

        related => [
            <<"why_tcps">>,
            <<"tcps_vs_lean">>,
            <<"tcps_vs_agile">>,
            <<"quality_gates_vs_ci">>
        ],

        tags => [
            <<"comparison">>,
            <<"devops">>,
            <<"ci/cd">>,
            <<"quality">>,
            <<"automation">>
        ],

        difficulty => intermediate,
        reading_time_minutes => 9
    }.

%% @doc TCPS vs Lean Software Development
tcps_vs_lean() ->
    #{
        id => <<"tcps_vs_lean">>,
        title => <<"TCPS vs Lean Software Development">>,
        category => comparisons,
        summary => <<"Understand the relationship between TCPS and Lean Software Development, and how TCPS is a specific implementation of Lean principles.">>,

        sections => [
            #{
                title => <<"Lean Software: The Foundation">>,
                content => <<
                    "**Lean Software Development** (Mary and Tom Poppendieck, 2003)\n"
                    "adapted Lean manufacturing principles to software:\n\n"

                    "**Seven Lean Principles:**\n"
                    "1. Eliminate Waste\n"
                    "2. Build Quality In\n"
                    "3. Create Knowledge\n"
                    "4. Defer Commitment\n"
                    "5. Deliver Fast\n"
                    "6. Respect People\n"
                    "7. Optimize the Whole\n\n"

                    "These are high-level philosophical principles.\n"
                    "They tell you WHAT to value, not HOW to implement.\n\n"

                    "**Example:**\n"
                    "'Build Quality In' is a principle.\n"
                    "But HOW do you build quality in?\n"
                    "• Automated testing?\n"
                    "• Code reviews?\n"
                    "• Type systems?\n"
                    "• Quality gates?\n"
                    "• TDD?\n\n"

                    "Lean Software gives you the compass (direction).\n"
                    "TCPS gives you the map (specific practices)."
                >>,
                key_points => [
                    <<"Lean Software = high-level principles">>,
                    <<"7 principles from manufacturing">>,
                    <<"Philosophy, not implementation">>,
                    <<"'What' but not 'how'">>
                ]
            },

            #{
                title => <<"TCPS: Lean Made Concrete">>,
                content => <<
                    "TCPS is a specific, opinionated implementation of Lean principles:\n\n"

                    "**Lean Principle → TCPS Implementation:**\n\n"

                    "**1. Eliminate Waste**\n"
                    "Lean: 'Remove activities that don't add value'\n"
                    "TCPS:\n"
                    "• WIP limits (eliminate task-thrashing waste)\n"
                    "• Pull systems (eliminate inventory waste)\n"
                    "• Quality gates (eliminate rework waste)\n"
                    "• Receipts (eliminate tracking waste)\n\n"

                    "**2. Build Quality In**\n"
                    "Lean: 'Quality should be built into the process'\n"
                    "TCPS:\n"
                    "• Jidoka philosophy (automatic quality detection)\n"
                    "• 5 mandatory quality gates (type, test, lint, security, docs)\n"
                    "• Andon cord (stop-the-line on quality failure)\n"
                    "• Quality ratchet (quality only improves)\n\n"

                    "**3. Create Knowledge**\n"
                    "Lean: 'Learn and improve continuously'\n"
                    "TCPS:\n"
                    "• RDF semantic layer (knowledge capture)\n"
                    "• Receipt analytics (learning from data)\n"
                    "• Diataxis documentation (structured learning)\n"
                    "• MCP integration (AI-assisted learning)\n\n"

                    "**4. Defer Commitment**\n"
                    "Lean: 'Delay decisions until the last responsible moment'\n"
                    "TCPS:\n"
                    "• Pull-based task selection (commit when ready)\n"
                    "• WIP limits (don't commit to too much)\n"
                    "• Kanban board (visualize options)\n\n"

                    "**5. Deliver Fast**\n"
                    "Lean: 'Short cycle times'\n"
                    "TCPS:\n"
                    "• Pull systems (reduce lead time)\n"
                    "• WIP limits (Little's Law: lower WIP = faster delivery)\n"
                    "• Quality gates (prevent rework delays)\n\n"

                    "**6. Respect People**\n"
                    "Lean: 'Empower team members'\n"
                    "TCPS:\n"
                    "• Andon cord (anyone can stop for quality)\n"
                    "• Heijunka (sustainable, humane pace)\n"
                    "• WIP limits (prevent burnout)\n\n"

                    "**7. Optimize the Whole**\n"
                    "Lean: 'System thinking, not local optimization'\n"
                    "TCPS:\n"
                    "• Dual storage (operational + analytical optimization)\n"
                    "• Receipt tracking (end-to-end visibility)\n"
                    "• Quality gates (prevent local 'fast commits' that slow overall delivery)\n\n"

                    "TCPS takes abstract Lean principles and gives you concrete tools."
                >>,
                examples => [
                    <<"Lean: 'Build quality in' → TCPS: 5 quality gates + Jidoka">>,
                    <<"Lean: 'Eliminate waste' → TCPS: WIP limits + pull systems">>,
                    <<"Lean: 'Deliver fast' → TCPS: Pull + WIP → lower lead time">>,
                    <<"Lean: 'Respect people' → TCPS: Andon + Heijunka">>
                ],
                key_points => [
                    <<"TCPS = concrete implementation of Lean">>,
                    <<"Each Lean principle → specific TCPS tools">>,
                    <<"Philosophy made actionable">>,
                    <<"'How' in addition to 'what'">>
                ]
            },

            #{
                title => <<"What TCPS Adds Beyond Lean">>,
                content => <<
                    "TCPS brings specific tools from Toyota Production System:\n\n"

                    "**1. Japanese TPS Concepts**\n"
                    "Lean Software mentions these, but TCPS implements them:\n"
                    "• Jidoka (built-in quality automation)\n"
                    "• Heijunka (production leveling)\n"
                    "• Andon (stop-the-line authority)\n"
                    "• Kanban (visual pull system)\n"
                    "• Kaizen (continuous improvement)\n\n"

                    "**2. Quality Rigor**\n"
                    "Lean says 'build quality in'.\n"
                    "TCPS specifies EXACTLY what quality means:\n"
                    "• 100% type coverage (not 'some types')\n"
                    "• ≥80% test coverage (specific threshold)\n"
                    "• ALL 400+ lint rules (not 'some linting')\n"
                    "• Zero security issues (not 'few vulnerabilities')\n"
                    "• Complete documentation (not 'some docs')\n\n"

                    "**3. Receipts as Work Units**\n"
                    "Lean doesn't specify how to track work.\n"
                    "TCPS creates receipts (work + quality metadata).\n\n"

                    "**4. Dual Storage (JSON + RDF)**\n"
                    "Lean doesn't prescribe data architecture.\n"
                    "TCPS uses dual storage for operational + semantic needs.\n\n"

                    "**5. MCP Integration**\n"
                    "Lean predates AI assistants.\n"
                    "TCPS integrates AI (Claude via MCP) for quality assistance.\n\n"

                    "**6. Diataxis Documentation**\n"
                    "Lean values knowledge creation.\n"
                    "TCPS implements structured documentation framework (Diataxis).\n\n"

                    "TCPS is 'Lean Software Development: The Complete Implementation Kit'."
                >>,
                key_points => [
                    <<"TCPS implements TPS concepts concretely">>,
                    <<"Specific quality thresholds (not abstract)">>,
                    <<"Receipts for work tracking">>,
                    <<"Modern additions (AI, semantics)">>,
                    <<"Implementation kit, not just philosophy">>
                ]
            },

            #{
                title => <<"When Lean Is Enough, When TCPS Helps">>,
                content => <<
                    "**Use Lean Software Principles When:**\n"
                    "• You want philosophical guidance\n"
                    "• You're designing your own practices\n"
                    "• You need flexibility to adapt\n"
                    "• Your context is unique\n"
                    "• You're starting a cultural shift\n\n"

                    "Lean gives you freedom to implement as you see fit.\n\n"

                    "**Use TCPS When:**\n"
                    "• You want proven, specific practices\n"
                    "• You don't want to invent your own system\n"
                    "• You need manufacturing-grade quality\n"
                    "• You want concrete tools (gates, receipts, Kanban)\n"
                    "• You value TPS heritage and rigor\n\n"

                    "TCPS gives you a complete, opinionated system.\n\n"

                    "**Migration Path:**\n"
                    "Many teams start with Lean principles, then adopt TCPS:\n"
                    "1. Read Lean Software Development (understand philosophy)\n"
                    "2. Try implementing Lean principles (struggle with specifics)\n"
                    "3. Discover TCPS (concrete implementation)\n"
                    "4. Adopt TCPS practices (proven tools)\n"
                    "5. Customize TCPS to context (informed by Lean principles)\n\n"

                    "Lean philosophy + TCPS implementation = Powerful combination."
                >>,
                key_points => [
                    <<"Lean for philosophy and flexibility">>,
                    <<"TCPS for concrete, proven practices">>,
                    <<"Lean → TCPS common migration path">>,
                    <<"Combine philosophy + implementation">>
                ]
            }
        ],

        analogies => [
            <<"Lean = Architecture principles; TCPS = Detailed blueprints and building codes">>,
            <<"Lean = 'Eat healthy'; TCPS = Specific meal plans with nutrition targets">>,
            <<"Lean = Music theory; TCPS = Complete sheet music with performance notes">>
        ],

        trade_offs => #{
            pros => [
                <<"TCPS = concrete implementation of Lean">>,
                <<"Specific tools, not just philosophy">>,
                <<"Proven practices from TPS">>,
                <<"Can customize based on Lean principles">>,
                <<"Complements Lean (not competes)">>
            ],
            cons => [
                <<"TCPS more opinionated than Lean">>,
                <<"Less flexibility (specific practices)">>,
                <<"May not fit all contexts">>,
                <<"Learning curve (TPS concepts)">>
            ]
        },

        related => [
            <<"why_tcps">>,
            <<"jidoka_philosophy">>,
            <<"tcps_vs_devops">>,
            <<"tcps_vs_agile">>
        ],

        tags => [
            <<"comparison">>,
            <<"lean">>,
            <<"philosophy">>,
            <<"tps">>,
            <<"implementation">>
        ],

        difficulty => intermediate,
        reading_time_minutes => 10
    }.

%% @doc TCPS vs Agile/Scrum
tcps_vs_agile() ->
    #{
        id => <<"tcps_vs_agile">>,
        title => <<"TCPS vs Agile/Scrum">>,
        category => comparisons,
        summary => <<"Compare TCPS to Agile methodologies (Scrum, Kanban), highlighting differences in quality focus, flow, and work management.">>,

        sections => [
            #{
                title => <<"Philosophical Common Ground">>,
                content => <<
                    "TCPS and Agile share several values:\n\n"

                    "**Both Value:**\n"
                    "• Iterative development (small batches)\n"
                    "• Fast feedback loops\n"
                    "• Team collaboration and communication\n"
                    "• Continuous improvement (retrospectives/kaizen)\n"
                    "• Customer value delivery\n"
                    "• Responding to change\n\n"

                    "**Both Reject:**\n"
                    "• Big upfront design (waterfall)\n"
                    "• Rigid, unchangeable plans\n"
                    "• Silos and handoffs\n"
                    "• Delayed feedback\n"
                    "• Documentation over working software\n\n"

                    "They're aligned in spirit: adaptive, collaborative, iterative.\n\n"

                    "The differences are in EMPHASIS and SPECIFIC PRACTICES."
                >>,
                key_points => [
                    <<"Shared values: iteration, feedback, collaboration">>,
                    <<"Both reject waterfall rigidity">>,
                    <<"Aligned in spirit">>,
                    <<"Differences in emphasis and practices">>
                ]
            },

            #{
                title => <<"Key Differences: Quality vs Velocity">>,
                content => <<
                    "**Agile/Scrum Emphasis: Velocity**\n"
                    "• Sprint goals: Deliver X story points\n"
                    "• Velocity trending: Are we speeding up?\n"
                    "• Burndown charts: Are we on track to finish?\n"
                    "• Definition of Done: Team-defined (varies)\n"
                    "• Quality: Important, but measured informally\n\n"

                    "In practice, Agile teams often prioritize:\n"
                    "• Completing sprint commitments\n"
                    "• Increasing story point throughput\n"
                    "• Stakeholder demo (working features)\n\n"

                    "Quality can become secondary pressure:\n"
                    "'We committed to 30 points, let's get it done!' →\n"
                    "Shortcuts on tests, docs, refactoring.\n\n"

                    "**TCPS Emphasis: Quality + Sustainable Flow**\n"
                    "• Quality gates: Non-negotiable standards\n"
                    "• Jidoka: Built-in quality, stop on defect\n"
                    "• Andon: Anyone can halt for quality\n"
                    "• Heijunka: Level workload (sustainable pace)\n"
                    "• Definition of Done: Explicit quality gates (100% types, 80%+ coverage, etc.)\n\n"

                    "TCPS teams prioritize:\n"
                    "• Passing ALL quality gates\n"
                    "• Sustainable pace (no crunch sprints)\n"
                    "• Quality ratchet (never decrease quality)\n\n"

                    "Velocity is outcome, not goal:\n"
                    "'Quality first, speed follows from less rework.'\n\n"

                    "**The Tension:**\n"
                    "Agile: 'Did we deliver the sprint commitment?'\n"
                    "TCPS: 'Did we deliver quality work at sustainable pace?'\n\n"

                    "These can conflict when velocity pressure builds."
                >>,
                examples => [
                    <<"Agile: 'We're behind, skip some tests to finish sprint'">>,
                    <<"TCPS: 'Quality gate failed, we stop and fix (even if sprint misses)'">>,
                    <<"Agile: 'Velocity increased 20% this sprint!'">>,
                    <<"TCPS: 'Quality ratchet increased coverage to 87%, velocity stable'">>
                ],
                key_points => [
                    <<"Agile emphasizes velocity">>,
                    <<"TCPS emphasizes quality + sustainable flow">>,
                    <<"Can conflict under velocity pressure">>,
                    <<"TCPS: quality is non-negotiable">>
                ]
            },

            #{
                title => <<"Work Management: Scrum vs Kanban vs TCPS Kanban">>,
                content => <<
                    "**Scrum (Sprint-Based):**\n"
                    "• 2-week sprints (timeboxed)\n"
                    "• Sprint planning (commit to work upfront)\n"
                    "• Sprint review/retro (batch feedback)\n"
                    "• Fixed-length iterations\n"
                    "• No mid-sprint changes (ideally)\n\n"

                    "Pros:\n"
                    "• Predictable rhythm\n"
                    "• Regular stakeholder demos\n"
                    "• Forcing function (sprint deadline)\n\n"

                    "Cons:\n"
                    "• Encourages overcommitment (sprint goal pressure)\n"
                    "• Batches work (doesn't optimize flow)\n"
                    "• Can feel rigid (no mid-sprint changes)\n"
                    "• Velocity focus can compromise quality\n\n"

                    "**Kanban (Agile Flavor):**\n"
                    "• Continuous flow (no sprints)\n"
                    "• WIP limits (sometimes)\n"
                    "• Pull when ready\n"
                    "• Metrics: Lead time, throughput\n\n"

                    "Pros:\n"
                    "• Flexible (start anytime)\n"
                    "• Flow-optimized\n"
                    "• Responds to urgent work\n\n"

                    "Cons:\n"
                    "• Can lack quality rigor (depends on team)\n"
                    "• WIP limits often informal/unenforced\n"
                    "• No built-in quality gates\n\n"

                    "**TCPS Kanban (TPS-Derived):**\n"
                    "• Continuous flow (like Agile Kanban)\n"
                    "• WIP limits (STRICT, enforced by system)\n"
                    "• Pull with quality gates (can't pull if WIP full OR gates fail)\n"
                    "• Heijunka leveling (balance work types 70/20/10)\n"
                    "• Andon integration (stop-the-line on quality failure)\n"
                    "• Metrics: Lead time, quality, sustainability\n\n"

                    "Pros:\n"
                    "• Flow optimization + quality rigor\n"
                    "• Enforced WIP limits (sustainable pace)\n"
                    "• Quality gates integrated\n"
                    "• Balanced work (features + tech debt + bugs)\n\n"

                    "**Key Difference:**\n"
                    "Agile Kanban: Flow focus\n"
                    "TCPS Kanban: Flow + Quality + Sustainability focus"
                >>,
                key_points => [
                    <<"Scrum: sprint-based, velocity focus">>,
                    <<"Agile Kanban: flow-based, flexible">>,
                    <<"TCPS Kanban: flow + quality + sustainability">>,
                    <<"TCPS Kanban most rigorous">>
                ]
            },

            #{
                title => <<"Can You Use Both? (Yes!)">>,
                content => <<
                    "TCPS and Agile are not mutually exclusive.\n"
                    "Many teams combine them:\n\n"

                    "**Pattern 1: Agile + TCPS Quality Gates**\n"
                    "• Keep Scrum ceremonies (planning, retro, demo)\n"
                    "• Keep sprint cadence (2-week iterations)\n"
                    "• ADD TCPS quality gates (definition of done)\n"
                    "• ADD Andon culture (anyone can stop for quality)\n\n"

                    "Result: Agile's rhythm + TCPS's quality rigor\n\n"

                    "**Pattern 2: Kanban + TCPS Practices**\n"
                    "• Use continuous flow (Kanban board)\n"
                    "• ADD strict WIP limits (TCPS)\n"
                    "• ADD quality gates per workflow stage (TCPS)\n"
                    "• ADD Heijunka leveling (TCPS)\n\n"

                    "Result: Flow optimization + built-in quality + sustainable pace\n\n"

                    "**Pattern 3: TCPS with Agile Values**\n"
                    "• Use TCPS system (receipts, gates, Kanban, Andon)\n"
                    "• Maintain Agile values (collaboration, iteration, feedback)\n"
                    "• Keep Agile ceremonies for team communication\n"
                    "• Use TCPS for quality and work management\n\n"

                    "Result: Best of both worlds\n\n"

                    "**Migration Path:**\n"
                    "1. Start with Agile/Scrum (learn iteration, collaboration)\n"
                    "2. Notice quality or sustainability issues\n"
                    "3. Add TCPS quality gates (improve definition of done)\n"
                    "4. Add WIP limits (improve flow)\n"
                    "5. Adopt Heijunka leveling (sustainable pace)\n"
                    "6. Full TCPS system (receipts, RDF, MCP) for mature teams\n\n"

                    "You don't have to choose—you can integrate."
                >>,
                examples => [
                    <<"Scrum team adds TCPS quality gates to 'Definition of Done'">>,
                    <<"Kanban team adds strict WIP limits + Heijunka from TCPS">>,
                    <<"Agile team keeps retros, adds Andon culture from TCPS">>,
                    <<"TCPS team keeps Agile values (collaboration, adaptation)">>
                ],
                key_points => [
                    <<"TCPS and Agile can integrate">>,
                    <<"Add TCPS quality to Agile flow">>,
                    <<"Multiple integration patterns">>,
                    <<"Migration path: Agile → Agile+TCPS">>
                ]
            }
        ],

        analogies => [
            <<"Agile = Jazz improvisation (adaptive); TCPS = Classical performance (quality standards)">>,
            <<"Scrum = Sprint training (velocity); TCPS = Marathon training (sustainable pace)">>,
            <<"Agile = Iterative design; TCPS = Design + Manufacturing quality control">>
        ],

        trade_offs => #{
            pros => [
                <<"Can integrate (Agile + TCPS quality)">>,
                <<"Shared values (iteration, collaboration)">>,
                <<"Complementary strengths (flow + quality)">>,
                <<"Multiple integration patterns">>
            ],
            cons => [
                <<"Velocity focus can conflict with quality focus">>,
                <<"TCPS more prescriptive than Agile">>,
                <<"Cultural shift needed (stop-the-line mindset)">>,
                <<"May slow initial sprint velocity">>
            ]
        },

        related => [
            <<"why_tcps">>,
            <<"pull_vs_push">>,
            <<"wip_limits_matter">>,
            <<"heijunka_leveling">>,
            <<"tcps_vs_lean">>
        ],

        tags => [
            <<"comparison">>,
            <<"agile">>,
            <<"scrum">>,
            <<"kanban">>,
            <<"velocity">>,
            <<"quality">>
        ],

        difficulty => intermediate,
        reading_time_minutes => 11
    }.

%% @doc Quality Gates vs Static Analysis
quality_gates_vs_static() ->
    #{
        id => <<"quality_gates_vs_static">>,
        title => <<"Quality Gates vs Static Analysis">>,
        category => comparisons,
        summary => <<"Understand how TCPS quality gates differ from traditional static analysis tools, and why gates enforce higher standards.">>,

        sections => [
            #{
                title => <<"Static Analysis: Good, But Not Enough">>,
                content => <<
                    "**Traditional Static Analysis Tools:**\n"
                    "• Linters (ESLint, Ruff, Pylint)\n"
                    "• Type checkers (TypeScript, Mypy)\n"
                    "• Security scanners (Bandit, Snyk)\n"
                    "• Complexity analyzers (SonarQube)\n\n"

                    "These are valuable tools, but they have limitations:\n\n"

                    "**Problem 1: Optional Enforcement**\n"
                    "Most static analysis is advisory:\n"
                    "• ESLint finds issues → Developer ignores (ships anyway)\n"
                    "• Mypy reports type errors → Use 'type: ignore' comment\n"
                    "• Bandit finds security issue → 'noqa' comment skips it\n\n"

                    "Tools suggest, but don't enforce.\n\n"

                    "**Problem 2: No Comprehensive Standards**\n"
                    "Each tool checks ONE dimension:\n"
                    "• Ruff checks style\n"
                    "• Mypy checks types\n"
                    "• Pytest checks tests pass\n"
                    "• None check coverage, test quality, documentation together\n\n"

                    "Fragmented view of quality.\n\n"

                    "**Problem 3: No Quality Trends**\n"
                    "Static analysis is point-in-time:\n"
                    "• 'Code has 12 lint errors today'\n"
                    "• Yesterday it had 10 errors\n"
                    "• No enforcement that it improves (or even stays same)\n\n"

                    "Quality can decay gradually.\n\n"

                    "**Problem 4: No Process Integration**\n"
                    "Static analysis tools don't understand workflow:\n"
                    "• When should they run? (Pre-commit? PR? Deploy?)\n"
                    "• What happens if they fail? (Block? Warn?)\n"
                    "• How do they relate to work units?\n\n"

                    "Static analysis is a tool, not a system."
                >>,
                key_points => [
                    <<"Static analysis often optional">>,
                    <<"Each tool checks one dimension">>,
                    <<"No quality trend enforcement">>,
                    <<"Not integrated into workflow">>
                ]
            },

            #{
                title => <<"Quality Gates: Comprehensive, Enforced">>,
                content => <<
                    "TCPS Quality Gates build on static analysis but add:\n\n"

                    "**1. Mandatory Enforcement (Non-Negotiable)**\n"
                    "Gates block work from progressing:\n"
                    "• Type gate fails → Receipt rejected (can't merge)\n"
                    "• Coverage gate fails → Receipt rejected\n"
                    "• Lint gate fails → Receipt rejected\n"
                    "• Security gate fails → Receipt rejected\n"
                    "• Documentation gate fails → Receipt rejected\n\n"

                    "No suppression comments, no 'skip this time'.\n\n"

                    "**2. Multi-Dimensional Standards**\n"
                    "Gates check ALL quality dimensions together:\n\n"

                    "```erlang\n"
                    "-record(quality_gate_results, {\n"
                    "    type_coverage :: {pass | fail, float()},    % 100% required\n"
                    "    test_coverage :: {pass | fail, float()},    % ≥80% required\n"
                    "    lint_quality :: {pass | fail, integer()},   % 0 violations\n"
                    "    security_scan :: {pass | fail, [issue()]},  % 0 issues\n"
                    "    documentation :: {pass | fail, [missing()]} % 0 missing\n"
                    "}).\n"
                    "```\n\n"

                    "ALL must pass for receipt to be validated.\n\n"

                    "**3. Quality Ratchet (Trend Enforcement)**\n"
                    "Gates track and enforce quality trends:\n"
                    "• Baseline: 85% coverage\n"
                    "• New work must be ≥85%\n"
                    "• If new work achieves 88%, baseline → 88%\n"
                    "• Quality can only go up (or stay flat), never down\n\n"

                    "Prevents gradual quality decay.\n\n"

                    "**4. Process Integration (Workflow-Aware)**\n"
                    "Gates understand workflow stages:\n"
                    "• Pre-commit gates (fast checks before commit)\n"
                    "• Pre-merge gates (comprehensive before integration)\n"
                    "• Pre-deploy gates (final validation before production)\n"
                    "• Post-deploy monitoring (production quality)\n\n"

                    "Each stage has appropriate checks.\n\n"

                    "**5. Receipt Association (Work Unit Tracking)**\n"
                    "Gate results attached to receipts:\n"
                    "• Receipt R-1234: All gates passed (validated)\n"
                    "• Receipt R-1235: Coverage gate failed (rejected)\n\n"

                    "Quality tied to work units, not just code."
                >>,
                examples => [
                    <<"Gate blocks merge: 'Type coverage 98% (requires 100%)'">>,
                    <<"Gate tracks trend: 'Coverage 85% → 88% → 90% (improving)'">>,
                    <<"Pre-commit gate: Fast checks (types, lint) in 5s">>,
                    <<"Pre-merge gate: Comprehensive (all 5 gates) in 2min">>,
                    <<"Receipt shows: '5/5 gates passed, validated'">>
                ],
                key_points => [
                    <<"Mandatory enforcement (no skipping)">>,
                    <<"Multi-dimensional (5 dimensions)">>,
                    <<"Quality ratchet (trends up)">>,
                    <<"Workflow-integrated (staged gates)">>,
                    <<"Receipt-associated (work units)">>
                ]
            },

            #{
                title => <<"Static Analysis Inside Quality Gates">>,
                content => <<
                    "Quality gates USE static analysis tools:\n\n"

                    "**Gate Implementation:**\n"
                    "```erlang\n"
                    "% Type Coverage Gate\n"
                    "run_type_gate(Receipt) ->\n"
                    "    % Uses Mypy (static analysis)\n"
                    "    Result = os:cmd(\"mypy --strict src/\"),\n"
                    "    Coverage = parse_type_coverage(Result),\n"
                    "    \n"
                    "    % Gate logic (enforcement)\n"
                    "    case Coverage of\n"
                    "        100.0 -> {pass, Coverage};\n"
                    "        Less -> {fail, Less, \"Type coverage must be 100%\"}\n"
                    "    end.\n\n"

                    "% Lint Quality Gate\n"
                    "run_lint_gate(Receipt) ->\n"
                    "    % Uses Ruff (static analysis)\n"
                    "    Result = os:cmd(\"ruff check src/ --output-format json\"),\n"
                    "    Violations = parse_ruff_violations(Result),\n"
                    "    \n"
                    "    % Gate logic (enforcement)\n"
                    "    case length(Violations) of\n"
                    "        0 -> {pass, 0};\n"
                    "        N -> {fail, N, Violations}\n"
                    "    end.\n"
                    "```\n\n"

                    "**The Relationship:**\n"
                    "• Static analysis tools = Measurement instruments\n"
                    "• Quality gates = Manufacturing quality control system\n\n"

                    "Gates use tools (Mypy, Ruff, Bandit) but add:\n"
                    "• Enforcement logic (pass/fail criteria)\n"
                    "• Multi-tool integration (comprehensive view)\n"
                    "• Workflow integration (when to run, what to block)\n"
                    "• Receipt tracking (quality history)\n"
                    "• Trend analysis (quality over time)\n\n"

                    "Static analysis provides data.\n"
                    "Quality gates provide decisions and enforcement."
                >>,
                key_points => [
                    <<"Gates use static analysis tools">>,
                    <<"Tools = measurement, Gates = enforcement">>,
                    <<"Gates add workflow integration">>,
                    <<"Gates add trend tracking">>,
                    <<"Gates add decision logic">>
                ]
            },

            #{
                title => <<"When to Use What">>,
                content => <<
                    "**Use Static Analysis Alone When:**\n"
                    "• Learning/exploring (not enforcing yet)\n"
                    "• Gradual adoption (team building habits)\n"
                    "• Legacy codebase (too many violations to fix)\n"
                    "• Advisory mode (suggest, don't block)\n\n"

                    "Static analysis is a good starting point.\n\n"

                    "**Use Quality Gates When:**\n"
                    "• Quality is non-negotiable (safety, finance, healthcare)\n"
                    "• Technical debt is a problem\n"
                    "• Need to prevent quality decay\n"
                    "• Team ready for discipline\n"
                    "• Want manufacturing-grade standards\n\n"

                    "Quality gates are for serious quality commitment.\n\n"

                    "**Migration Path:**\n"
                    "1. **Week 1-2:** Introduce static analysis tools\n"
                    "   - Install Mypy, Ruff, Bandit, Pytest\n"
                    "   - Run in advisory mode (warnings only)\n"
                    "   - Team learns what violations look like\n\n"

                    "2. **Week 3-4:** Fix existing violations\n"
                    "   - Clean up type errors\n"
                    "   - Fix lint issues\n"
                    "   - Resolve security problems\n"
                    "   - Add missing tests\n\n"

                    "3. **Week 5-6:** Implement pre-commit gates\n"
                    "   - Light gates (type check, basic lint)\n"
                    "   - Fast feedback (< 10s)\n"
                    "   - Prevent new violations\n\n"

                    "4. **Week 7-8:** Implement pre-merge gates\n"
                    "   - Comprehensive gates (all 5 dimensions)\n"
                    "   - Block merges on failures\n"
                    "   - Enforce quality standards\n\n"

                    "5. **Week 9+:** Full TCPS system\n"
                    "   - Receipt tracking\n"
                    "   - Quality ratchet\n"
                    "   - RDF analytics\n"
                    "   - Continuous improvement\n\n"

                    "Gradual transition from advisory to enforced quality."
                >>,
                key_points => [
                    <<"Static analysis = starting point">>,
                    <<"Quality gates = serious commitment">>,
                    <<"Gradual migration path (8 weeks)">>,
                    <<"Advisory → Light gates → Full gates">>
                ]
            }
        ],

        analogies => [
            <<"Static analysis = Thermometer (measures temperature); Quality gates = Thermostat (enforces temperature)">>,
            <<"Static analysis = Spell checker (suggests fixes); Quality gates = Editor (blocks publication if errors)">>,
            <<"Static analysis = Speedometer (shows speed); Quality gates = Speed limiter (enforces limit)">>
        ],

        trade_offs => #{
            pros => [
                <<"Gates enforce, tools suggest">>,
                <<"Multi-dimensional comprehensive view">>,
                <<"Quality trends enforced (ratchet)">>,
                <<"Workflow integration">>,
                <<"Uses existing tools (Mypy, Ruff, etc.)">>
            ],
            cons => [
                <<"More overhead than advisory tools">>,
                <<"Can block work (by design)">>,
                <<"Requires discipline and buy-in">>,
                <<"Cultural shift needed">>,
                <<"Initial velocity may slow">>
            ]
        },

        related => [
            <<"quality_gates_vs_ci">>,
            <<"jidoka_philosophy">>,
            <<"andon_thinking">>
        ],

        tags => [
            <<"comparison">>,
            <<"static analysis">>,
            <<"quality gates">>,
            <<"enforcement">>,
            <<"tools">>
        ],

        difficulty => intermediate,
        reading_time_minutes => 10
    }.

%% @doc Andon vs Error Monitoring
andon_vs_monitoring() ->
    #{
        id => <<"andon_vs_monitoring">>,
        title => <<"Andon vs Error Monitoring">>,
        category => comparisons,
        summary => <<"Compare TCPS Andon (stop-the-line) to traditional error monitoring systems, and understand when each is appropriate.">>,

        sections => [
            #{
                title => <<"Error Monitoring: Reactive Response">>,
                content => <<
                    "**Traditional Error Monitoring** (Sentry, Datadog, New Relic):\n\n"

                    "**How it works:**\n"
                    "1. Code deployed to production\n"
                    "2. Errors occur in production\n"
                    "3. Monitoring system captures errors\n"
                    "4. Alerts sent to team\n"
                    "5. Team investigates and fixes\n"
                    "6. Hotfix deployed\n\n"

                    "**Characteristics:**\n"
                    "• **Reactive:** Responds after error occurs\n"
                    "• **Production-focused:** Monitors live systems\n"
                    "• **Detection:** Finds errors users encounter\n"
                    "• **Alerting:** Notifies team of problems\n"
                    "• **Debugging:** Provides stack traces, context\n\n"

                    "**Strengths:**\n"
                    "• Catches errors in real-world conditions\n"
                    "• Provides production visibility\n"
                    "• Helps debug complex issues\n"
                    "• Tracks error trends over time\n"
                    "• Essential for production systems\n\n"

                    "**Weaknesses:**\n"
                    "• Errors already impacted users (too late)\n"
                    "• Costly to fix (production issues expensive)\n"
                    "• Reactive (not preventive)\n"
                    "• No quality enforcement before deploy\n"
                    "• Can normalize 'acceptable error rates'\n\n"

                    "Monitoring is essential, but it's a safety net—not prevention."
                >>,
                key_points => [
                    <<"Error monitoring is reactive">>,
                    <<"Detects production issues">>,
                    <<"Helps debug and fix">>,
                    <<"Essential but not preventive">>,
                    <<"Errors already impacted users">>
                ]
            },

            #{
                title => <<"Andon: Proactive Prevention">>,
                content => <<
                    "**TCPS Andon** (Stop-the-Line Quality Control):\n\n"

                    "**How it works:**\n"
                    "1. Developer writes code\n"
                    "2. Quality gates run automatically\n"
                    "3. Gate detects issue (type error, missing test, security vulnerability)\n"
                    "4. System 'pulls Andon cord' (blocks merge/deploy)\n"
                    "5. Developer fixes issue immediately\n"
                    "6. Code never reaches production with known issues\n\n"

                    "**Characteristics:**\n"
                    "• **Proactive:** Prevents errors before deploy\n"
                    "• **Pre-production:** Catches issues in development\n"
                    "• **Prevention:** Stops defects at source\n"
                    "• **Blocking:** Halts delivery until fixed\n"
                    "• **Early feedback:** Developer fixes while in context\n\n"

                    "**Strengths:**\n"
                    "• Prevents defects from reaching production\n"
                    "• Cheaper to fix (caught early)\n"
                    "• Developer still in context (easy fix)\n"
                    "• No user impact (blocked before deploy)\n"
                    "• Creates quality culture (built-in quality)\n\n"

                    "**Weaknesses:**\n"
                    "• Can't catch all errors (some only appear in production)\n"
                    "• May slow delivery (by design, to ensure quality)\n"
                    "• Requires quality infrastructure (gates)\n"
                    "• Cultural shift needed (stop-the-line mindset)\n\n"

                    "Andon is prevention—catching issues before they become problems."
                >>,
                examples => [
                    <<"Andon blocks merge: 'Security vulnerability B105 detected in auth.py:67'">>,
                    <<"Andon blocks merge: 'Test coverage dropped to 76% (requires ≥80%)'">>,
                    <<"Andon blocks merge: 'Type error in utils.py:42 (str vs int)'">>,
                    <<"Developer fixes immediately (still in context), re-runs gates, merges">>
                ],
                key_points => [
                    <<"Andon is proactive prevention">>,
                    <<"Catches issues pre-production">>,
                    <<"Blocks delivery until fixed">>,
                    <<"Developer fixes while in context">>,
                    <<"No user impact (blocked before deploy)">>
                ]
            },

            #{
                title => <<"The Cost Difference: Exponential">>,
                content => <<
                    "**Cost of Fixing Defects:**\n\n"

                    "Industry research (IBM, NIST) shows exponential cost increase:\n\n"

                    "**Fix During Development (Andon):**\n"
                    "• Caught by quality gate: 1x cost\n"
                    "• Developer still in context\n"
                    "• Fix time: 5-15 minutes\n"
                    "• Impact: Zero (not deployed)\n"
                    "• Example: Type error caught by Mypy gate\n\n"

                    "**Fix During Testing:**\n"
                    "• Caught by QA: 10x cost\n"
                    "• Developer switched to other work\n"
                    "• Fix time: 1-2 hours (context switch)\n"
                    "• Impact: Testing delay\n"
                    "• Example: Bug found in staging\n\n"

                    "**Fix in Production (Monitoring):**\n"
                    "• Caught by error monitoring: 100x cost\n"
                    "• Users impacted\n"
                    "• Fix time: 4-8 hours (investigation + hotfix + deploy)\n"
                    "• Impact: User frustration, potential revenue loss\n"
                    "• Example: Production bug found by Sentry\n\n"

                    "**Major Incident:**\n"
                    "• Critical production issue: 1000x cost\n"
                    "• Many users impacted\n"
                    "• Fix time: Days (all-hands, rollback, patch, communicate)\n"
                    "• Impact: Reputation damage, SLA breach, potential legal\n"
                    "• Example: Security breach, data loss\n\n"

                    "**The Math:**\n"
                    "Andon preventing 100 defects (5 min each):\n"
                    "  Cost: 100 × 5 min = 500 minutes (8.3 hours)\n\n"

                    "Same 100 defects in production (4 hours each):\n"
                    "  Cost: 100 × 4 hours = 400 hours\n\n"

                    "Andon saves 400 - 8.3 = 391.7 hours.\n\n"

                    "Prevention is vastly cheaper than reaction."
                >>,
                diagrams => [
                    <<"Cost to Fix: Development (1x) → Testing (10x) → Production (100x) → Incident (1000x)">>,
                    <<"Andon catches at 1x, Monitoring catches at 100x+">>
                ],
                key_points => [
                    <<"Defect cost increases exponentially">>,
                    <<"Development fix: 1x cost">>,
                    <<"Production fix: 100x cost">>,
                    <<"Andon prevents expensive fixes">>,
                    <<"Monitoring catches what Andon misses">>
                ]
            },

            #{
                title => <<"Use Both: Defense in Depth">>,
                content => <<
                    "Andon and Monitoring are complementary (not competitive):\n\n"

                    "**Layer 1: Andon (Pre-Production Prevention)**\n"
                    "Catches:\n"
                    "• Type errors (Mypy gate)\n"
                    "• Missing tests (coverage gate)\n"
                    "• Lint violations (Ruff gate)\n"
                    "• Known security vulnerabilities (Bandit gate)\n"
                    "• Missing documentation (docstring gate)\n\n"

                    "Goal: Prevent known issues from deploying\n\n"

                    "**Layer 2: Error Monitoring (Production Safety Net)**\n"
                    "Catches:\n"
                    "• Runtime errors (unexpected conditions)\n"
                    "• Integration failures (third-party services)\n"
                    "• Performance regressions (slow queries)\n"
                    "• Edge cases (rare combinations)\n"
                    "• Production-only issues (scale, load)\n\n"

                    "Goal: Detect and respond to production issues quickly\n\n"

                    "**Together:**\n"
                    "• Andon catches ~80% of issues (pre-production)\n"
                    "• Monitoring catches remaining ~20% (production)\n"
                    "• Overall defect rate dramatically reduced\n"
                    "• User impact minimized\n"
                    "• Fix costs optimized (most caught early)\n\n"

                    "**Feedback Loop:**\n"
                    "When monitoring catches an issue:\n"
                    "1. Fix the immediate problem (hotfix)\n"
                    "2. Ask: 'Could Andon have caught this?'\n"
                    "3. If yes: Add new quality gate (prevent recurrence)\n"
                    "4. If no: Improve monitoring (detect faster next time)\n\n"

                    "**Example:**\n"
                    "Sentry reports: 'NoneType has no attribute \"name\"'\n"
                    "Root cause: Missing null check\n\n"

                    "Immediate: Hotfix (add null check)\n"
                    "Prevent: Add Mypy strict optional gate (catches similar issues)\n\n"

                    "Over time, Andon gets smarter (fewer production errors)."
                >>,
                examples => [
                    <<"Andon catches: Type errors, missing tests, lint issues (80%)">>,
                    <<"Monitoring catches: Runtime edge cases, integration failures (20%)">>,
                    <<"Production error → Hotfix → New Andon gate (prevent recurrence)">>,
                    <<"Defense in depth: Multiple layers of protection">>
                ],
                key_points => [
                    <<"Use both (complementary layers)">>,
                    <<"Andon = prevention (~80%)">>,
                    <<"Monitoring = safety net (~20%)">>,
                    <<"Feedback loop: Production errors → New gates">>,
                    <<"Continuous improvement">>
                ]
            }
        ],

        analogies => [
            <<"Andon = Airbag prevention (seatbelt, safe driving); Monitoring = Airbag deployment (accident already happened)">>,
            <<"Andon = Food safety inspection (prevent contamination); Monitoring = Food poisoning reports (detect after consumers affected)">>,
            <<"Andon = Building code compliance (prevent unsafe construction); Monitoring = Fire alarms (detect emergencies)">>
        ],

        trade_offs => #{
            pros => [
                <<"Andon prevents (cheaper), Monitoring detects (safety net)">>,
                <<"Complementary, not competing">>,
                <<"Andon catches ~80%, Monitoring ~20%">>,
                <<"Feedback loop improves both over time">>,
                <<"Defense in depth">>
            ],
            cons => [
                <<"Andon can't catch all errors (production-only issues)">>,
                <<"Monitoring is reactive (users already impacted)">>,
                <<"Need both systems (investment)">>,
                <<"Andon may slow delivery (by design)">>
            ]
        },

        related => [
            <<"andon_thinking">>,
            <<"jidoka_philosophy">>,
            <<"quality_gates_vs_ci">>
        ],

        tags => [
            <<"comparison">>,
            <<"andon">>,
            <<"monitoring">>,
            <<"prevention">>,
            <<"safety net">>,
            <<"cost">>
        ],

        difficulty => intermediate,
        reading_time_minutes => 10
    }.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

receipts_not_commits_structure_test() ->
    Explanation = receipts_not_commits(),
    ?assertEqual(<<"receipts_not_commits">>, maps:get(id, Explanation)),
    ?assertEqual(design_decisions, maps:get(category, Explanation)),
    ?assert(length(maps:get(sections, Explanation)) >= 3).

quality_gates_vs_ci_structure_test() ->
    Explanation = quality_gates_vs_ci(),
    ?assertEqual(<<"quality_gates_vs_ci">>, maps:get(id, Explanation)),
    ?assert(maps:is_key(trade_offs, Explanation)).

wip_limits_matter_structure_test() ->
    Explanation = wip_limits_matter(),
    ?assertEqual(<<"wip_limits_matter">>, maps:get(id, Explanation)),
    ?assert(is_integer(maps:get(reading_time_minutes, Explanation))).

dual_storage_structure_test() ->
    Explanation = dual_storage(),
    ?assertEqual(<<"dual_storage">>, maps:get(id, Explanation)),
    ?assertEqual(advanced, maps:get(difficulty, Explanation)).

mcp_integration_structure_test() ->
    Explanation = mcp_integration(),
    ?assertEqual(<<"mcp_integration">>, maps:get(id, Explanation)),
    ?assert(length(maps:get(tags, Explanation)) > 0).

tcps_vs_devops_structure_test() ->
    Explanation = tcps_vs_devops(),
    ?assertEqual(<<"tcps_vs_devops">>, maps:get(id, Explanation)),
    ?assertEqual(comparisons, maps:get(category, Explanation)).

tcps_vs_lean_structure_test() ->
    Explanation = tcps_vs_lean(),
    ?assertEqual(<<"tcps_vs_lean">>, maps:get(id, Explanation)).

tcps_vs_agile_structure_test() ->
    Explanation = tcps_vs_agile(),
    ?assertEqual(<<"tcps_vs_agile">>, maps:get(id, Explanation)).

quality_gates_vs_static_structure_test() ->
    Explanation = quality_gates_vs_static(),
    ?assertEqual(<<"quality_gates_vs_static">>, maps:get(id, Explanation)).

andon_vs_monitoring_structure_test() ->
    Explanation = andon_vs_monitoring(),
    ?assertEqual(<<"andon_vs_monitoring">>, maps:get(id, Explanation)).

-endif.
