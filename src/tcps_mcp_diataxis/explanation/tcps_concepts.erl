%%%-------------------------------------------------------------------
%%% @doc TCPS Core Concepts
%%% Understanding-oriented explanations of fundamental TCPS concepts
%%% drawn from Toyota Production System philosophy.
%%%
%%% This module provides rich, contextual explanations of:
%%% - Why TCPS exists and what problems it solves
%%% - Jidoka (built-in quality) philosophy
%%% - Pull vs Push systems (Kanban)
%%% - Andon (stop-the-line) thinking
%%% - Heijunka (production leveling)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_concepts).

-export([
    why_tcps/0,
    jidoka_philosophy/0,
    pull_vs_push/0,
    andon_thinking/0,
    heijunka_leveling/0
]).

%%%===================================================================
%%% Core Concept Explanations
%%%===================================================================

%% @doc Why TCPS? Understanding Toyota Production System for Code
why_tcps() ->
    #{
        id => <<"why_tcps">>,
        title => <<"Why TCPS? Understanding Toyota Production System for Code">>,
        category => core_concepts,
        summary => <<"Understand why software development needs manufacturing-grade quality systems and how TCPS adapts Toyota's proven principles for code.">>,

        sections => [
            #{
                title => <<"The Manufacturing-Software Parallel">>,
                content => <<
                    "In 1950s Japan, Toyota faced a critical challenge: produce high-quality cars with "
                    "limited resources while competing against established American manufacturers. "
                    "Their solution—the Toyota Production System (TPS)—revolutionized manufacturing.\n\n"

                    "Today, software development faces remarkably similar challenges:\n"
                    "• How do we ensure quality when moving fast?\n"
                    "• How do we prevent defects rather than just detect them?\n"
                    "• How do we optimize flow without sacrificing thoroughness?\n"
                    "• How do we empower teams to stop and fix problems?\n\n"

                    "TCPS (Toyota Code Production System) adapts TPS principles that solved these "
                    "problems in manufacturing to the domain of software development."
                >>,
                key_points => [
                    <<"TPS solved quality+speed challenges in manufacturing">>,
                    <<"Software faces similar challenges today">>,
                    <<"TCPS adapts proven TPS principles for code">>,
                    <<"Focus on preventing defects, not just detecting them">>
                ]
            },

            #{
                title => <<"The Core Problem: Quality at Speed">>,
                content => <<
                    "Traditional software development treats quality as a trade-off:\n"
                    "• Fast delivery OR high quality (pick one)\n"
                    "• Automated testing OR manual thoroughness\n"
                    "• Developer velocity OR code reviews\n\n"

                    "TPS proved this is a false dichotomy. Toyota achieved BOTH:\n"
                    "• Highest quality cars in the world\n"
                    "• Fastest production times\n"
                    "• Lowest costs\n\n"

                    "The key insight: Quality and speed are not enemies—they're allies.\n"
                    "When you build quality in from the start (Jidoka), you eliminate:\n"
                    "• Rework cycles that slow delivery\n"
                    "• Technical debt that accumulates interest\n"
                    "• Firefighting that disrupts flow\n"
                    "• Context switching between 'building' and 'fixing' modes"
                >>,
                diagrams => [
                    <<"Traditional: Fast XOR Quality">>,
                    <<"TPS/TCPS: Fast AND Quality (through built-in quality)">>
                ],
                key_points => [
                    <<"Quality vs speed is a false dichotomy">>,
                    <<"Built-in quality enables both">>,
                    <<"Prevention eliminates rework">>,
                    <<"Flow improves when defects stop">>
                ]
            },

            #{
                title => <<"What TCPS Provides">>,
                content => <<
                    "TCPS translates five core TPS principles to software:\n\n"

                    "1. **Jidoka (Built-in Quality)**\n"
                    "   Manufacturing: Machines stop automatically when defects occur\n"
                    "   Software: Quality gates stop delivery when standards violated\n\n"

                    "2. **Just-in-Time (Pull Systems)**\n"
                    "   Manufacturing: Produce only what's needed, when it's needed\n"
                    "   Software: WIP limits, task pull, resource on-demand\n\n"

                    "3. **Andon (Stop and Fix)**\n"
                    "   Manufacturing: Workers pull cord to stop assembly line\n"
                    "   Software: Developers trigger quality gates, halt delivery\n\n"

                    "4. **Heijunka (Level Loading)**\n"
                    "   Manufacturing: Smooth production volume and variety\n"
                    "   Software: Balance feature work, bugs, tech debt, learning\n\n"

                    "5. **Kaizen (Continuous Improvement)**\n"
                    "   Manufacturing: Small daily improvements by everyone\n"
                    "   Software: Retrospectives, process tuning, skill building"
                >>,
                examples => [
                    <<"Quality Gate stops merge when coverage < 80%">>,
                    <<"WIP limit of 3 prevents task thrashing">>,
                    <<"Andon alert on security vulnerability">>,
                    <<"Heijunka balances 70% features, 20% tech debt, 10% learning">>
                ],
                key_points => [
                    <<"Five TPS principles adapted for software">>,
                    <<"Each principle addresses specific pain points">>,
                    <<"Principles work together synergistically">>,
                    <<"Results: higher quality + faster delivery">>
                ]
            },

            #{
                title => <<"Why Now? The Context for TCPS">>,
                content => <<
                    "Several forces make TCPS particularly relevant today:\n\n"

                    "**1. Complexity Crisis**\n"
                    "Modern software systems are exponentially more complex than 10 years ago.\n"
                    "Microservices, distributed systems, cloud infrastructure, multiple languages,\n"
                    "numerous integrations—traditional quality approaches don't scale.\n\n"

                    "**2. Velocity Pressure**\n"
                    "Markets demand faster delivery: daily deploys, feature flags, A/B testing.\n"
                    "Teams need systems that enable speed WITHOUT sacrificing quality.\n\n"

                    "**3. Cost of Defects**\n"
                    "Software defects are expensive:\n"
                    "• Production incidents cost $5,600/minute (Gartner)\n"
                    "• Security breaches average $4.45M per incident (IBM)\n"
                    "• Technical debt costs 23-42% of engineering capacity (Stripe)\n\n"

                    "**4. Developer Experience**\n"
                    "Firefighting, rework, and quality issues cause burnout.\n"
                    "TCPS creates sustainable, humane development practices.\n\n"

                    "**5. AI/Automation Era**\n"
                    "As AI writes more code, quality systems become MORE important.\n"
                    "We need robust gates to validate AI-generated code at scale."
                >>,
                key_points => [
                    <<"Software complexity requires systematic quality">>,
                    <<"Velocity pressure demands speed+quality together">>,
                    <<"Defect costs justify prevention investment">>,
                    <<"Developer experience improves with good systems">>,
                    <<"AI era needs stronger quality validation">>
                ]
            }
        ],

        analogies => [
            <<"Like Toyota transforming car manufacturing, TCPS transforms software development">>,
            <<"Just as assembly line stops prevent defective cars from shipping, quality gates prevent defective code from deploying">>,
            <<"Think of TCPS as manufacturing quality control adapted for the software factory">>
        ],

        trade_offs => #{
            pros => [
                <<"Higher code quality with measurable improvement">>,
                <<"Faster overall delivery (less rework)">>,
                <<"Lower technical debt accumulation">>,
                <<"Better developer experience and morale">>,
                <<"Reduced production incidents">>,
                <<"Predictable, sustainable pace">>
            ],
            cons => [
                <<"Initial setup overhead (gates, processes)">>,
                <<"Requires cultural shift (stop-the-line mindset)">>,
                <<"May slow individual commits (but speeds overall delivery)">>,
                <<"Needs team discipline and buy-in">>,
                <<"Learning curve for TPS concepts">>
            ]
        },

        related => [
            <<"jidoka_philosophy">>,
            <<"pull_vs_push">>,
            <<"andon_thinking">>,
            <<"tcps_vs_devops">>,
            <<"tcps_vs_lean">>
        ],

        tags => [
            <<"philosophy">>,
            <<"toyota">>,
            <<"quality">>,
            <<"tps">>,
            <<"introduction">>,
            <<"context">>
        ],

        difficulty => beginner,
        reading_time_minutes => 8
    }.

%% @doc The Philosophy of Jidoka (Built-in Quality)
jidoka_philosophy() ->
    #{
        id => <<"jidoka_philosophy">>,
        title => <<"The Philosophy of Jidoka (Built-in Quality)">>,
        category => core_concepts,
        summary => <<"Understand Jidoka—the principle of building quality into every step rather than inspecting it in at the end.">>,

        sections => [
            #{
                title => <<"What is Jidoka?">>,
                content => <<
                    "Jidoka (自働化) literally means 'automation with a human touch' in Japanese.\n"
                    "It's one of the two pillars of Toyota Production System (alongside Just-in-Time).\n\n"

                    "The core principle: **Build quality into the process itself**\n\n"

                    "Traditional approach (Inspection):\n"
                    "Build → Build → Build → Inspect → Find defects → Rework → Re-inspect\n\n"

                    "Jidoka approach (Built-in Quality):\n"
                    "Build ✓ → Build ✓ → Build ✓ (quality verified at each step)\n\n"

                    "The difference:\n"
                    "• Inspection finds defects AFTER they're made\n"
                    "• Jidoka PREVENTS defects from being made\n\n"

                    "In Toyota factories, machines automatically stop when they detect abnormalities.\n"
                    "This prevents defective parts from moving downstream where they'd cause\n"
                    "cascading problems and exponentially higher costs to fix."
                >>,
                key_points => [
                    <<"Build quality in, don't inspect it in">>,
                    <<"Prevent defects rather than detect them">>,
                    <<"Stop automatically when abnormalities occur">>,
                    <<"Early detection = lower fix costs">>
                ]
            },

            #{
                title => <<"The Four Stages of Jidoka">>,
                content => <<
                    "Toyota implements Jidoka through four stages:\n\n"

                    "**1. Detect the Abnormality**\n"
                    "Systems must recognize when something is wrong.\n"
                    "In TCPS: Quality gates detect rule violations (coverage drop, type errors, security issues)\n\n"

                    "**2. Stop**\n"
                    "Immediately halt the process when abnormality detected.\n"
                    "In TCPS: Block merge/deploy when quality gates fail\n\n"

                    "**3. Fix or Correct the Immediate Condition**\n"
                    "Address the specific problem right away.\n"
                    "In TCPS: Developer fixes the failing test, adds missing types, resolves security issue\n\n"

                    "**4. Investigate Root Cause and Install Countermeasures**\n"
                    "Understand WHY it happened and prevent recurrence.\n"
                    "In TCPS: Team retrospective, process improvement, new quality gate\n\n"

                    "This cycle ensures problems don't just get fixed—they get prevented from recurring."
                >>,
                examples => [
                    <<"Stage 1: Quality gate detects test coverage dropped from 85% to 72%">>,
                    <<"Stage 2: Gate blocks merge, prevents deployment">>,
                    <<"Stage 3: Developer adds missing tests to restore coverage">>,
                    <<"Stage 4: Team adds pre-commit hook to catch this earlier">>
                ],
                key_points => [
                    <<"Detect → Stop → Fix → Prevent">>,
                    <<"Each stage is essential">>,
                    <<"Focus on root cause, not symptoms">>,
                    <<"Continuous improvement prevents recurrence">>
                ]
            },

            #{
                title => <<"Jidoka in Software Development">>,
                content => <<
                    "TCPS implements Jidoka through Quality Gates:\n\n"

                    "**Type Safety (Detect type errors)**\n"
                    "• Traditional: Runtime errors in production\n"
                    "• Jidoka: Compile-time type checking prevents errors\n\n"

                    "**Test Coverage (Detect untested code)**\n"
                    "• Traditional: Bugs found by users\n"
                    "• Jidoka: Coverage gates ensure tests exist\n\n"

                    "**Security Scanning (Detect vulnerabilities)**\n"
                    "• Traditional: Security breach in production\n"
                    "• Jidoka: Bandit/SAST catches issues before deploy\n\n"

                    "**Code Quality (Detect maintainability issues)**\n"
                    "• Traditional: Tech debt accumulates until crisis\n"
                    "• Jidoka: Ruff/linters enforce standards continuously\n\n"

                    "**Documentation (Detect missing context)**\n"
                    "• Traditional: Undocumented code becomes legacy\n"
                    "• Jidoka: Docstring checks ensure API documentation\n\n"

                    "Each gate embodies Jidoka: automatic detection, immediate stop, mandatory fix."
                >>,
                key_points => [
                    <<"Quality gates = Jidoka for software">>,
                    <<"Automatic detection at every step">>,
                    <<"Prevention over reaction">>,
                    <<"Multiple dimensions of quality">>
                ]
            },

            #{
                title => <<"The Human Element: Autonomation">>,
                content => <<
                    "Jidoka is sometimes called 'autonomation'—automation with human intelligence.\n\n"

                    "The key insight: Humans shouldn't be quality inspectors.\n"
                    "Machines/systems should do automatic detection.\n"
                    "Humans should do root cause analysis and improvement.\n\n"

                    "**Bad division of labor:**\n"
                    "• Humans: Manually check every line of code for errors\n"
                    "• Machines: Just compile and run\n\n"

                    "**Good division of labor:**\n"
                    "• Machines: Automatically check types, coverage, security, style\n"
                    "• Humans: Design solutions, improve processes, handle exceptions\n\n"

                    "This frees developers from tedious checking and lets them focus on:\n"
                    "• Creative problem solving\n"
                    "• System design\n"
                    "• Understanding user needs\n"
                    "• Improving the development process\n\n"

                    "Jidoka doesn't replace human judgment—it amplifies it by automating\n"
                    "the routine checks so humans can focus on the hard problems."
                >>,
                key_points => [
                    <<"Automate detection, preserve human judgment">>,
                    <<"Machines check, humans improve">>,
                    <<"Frees developers for creative work">>,
                    <<"Amplifies rather than replaces intelligence">>
                ]
            }
        ],

        analogies => [
            <<"Like spell-check in word processors: catches errors automatically so you can focus on writing">>,
            <<"Like sensors in a car engine: detect problems and alert driver before catastrophic failure">>,
            <<"Like circuit breakers in electrical systems: automatically stop flow when danger detected">>
        ],

        trade_offs => #{
            pros => [
                <<"Prevents defects rather than detecting them late">>,
                <<"Lower overall cost (early detection = cheap fixes)">>,
                <<"Frees developers from manual checking">>,
                <<"Builds quality muscle memory in team">>,
                <<"Reduces production incidents dramatically">>,
                <<"Creates sustainable development pace">>
            ],
            cons => [
                <<"Requires upfront investment in gates/automation">>,
                <<"May slow initial commits (but speeds overall delivery)">>,
                <<"Can feel restrictive initially">>,
                <<"Requires cultural shift to stop-and-fix mindset">>,
                <<"Need to balance thoroughness with pragmatism">>
            ]
        },

        related => [
            <<"why_tcps">>,
            <<"andon_thinking">>,
            <<"quality_gates_vs_ci">>,
            <<"quality_gates_vs_static">>
        ],

        tags => [
            <<"jidoka">>,
            <<"quality">>,
            <<"toyota">>,
            <<"philosophy">>,
            <<"prevention">>,
            <<"automation">>
        ],

        difficulty => intermediate,
        reading_time_minutes => 10
    }.

%% @doc Pull vs Push: Understanding Kanban Pull Systems
pull_vs_push() ->
    #{
        id => <<"pull_vs_push">>,
        title => <<"Pull vs Push: Understanding Kanban Pull Systems">>,
        category => core_concepts,
        summary => <<"Learn the difference between push and pull systems, and why pull systems create better flow in software development.">>,

        sections => [
            #{
                title => <<"Push Systems: The Traditional Approach">>,
                content => <<
                    "In a **push system**, work is pushed to the next stage based on upstream completion:\n\n"

                    "Product Manager → Pushes requirements to → Developers\n"
                    "Developers → Push code to → QA\n"
                    "QA → Pushes tested features to → Deployment\n\n"

                    "Problems with push systems:\n\n"

                    "**1. Overload**\n"
                    "Downstream stages get overwhelmed. QA has 50 features to test,\n"
                    "developers juggle 10 concurrent tasks, ops drowns in deploy requests.\n\n"

                    "**2. Inventory Buildup**\n"
                    "Work piles up between stages. Features 'done' but not deployed,\n"
                    "code reviewed but not merged, tests written but not run.\n\n"

                    "**3. Context Switching**\n"
                    "People constantly switch between tasks as new work arrives,\n"
                    "losing focus and productivity.\n\n"

                    "**4. Hidden Bottlenecks**\n"
                    "You don't see problems until work stacks up.\n"
                    "By then, you have a crisis.\n\n"

                    "This is like a factory where each station produces as fast as possible,\n"
                    "regardless of downstream capacity. Result: massive inventory piles,\n"
                    "long lead times, and waste."
                >>,
                diagrams => [
                    <<"Push System: PM → [50 tasks] → Dev → [30 tasks] → QA → [20 tasks] → Deploy">>,
                    <<"Work accumulates at bottlenecks, creates waste">>
                ],
                key_points => [
                    <<"Push = upstream pushes work downstream">>,
                    <<"Creates overload and inventory buildup">>,
                    <<"Hides bottlenecks until crisis">>,
                    <<"Forces constant context switching">>
                ]
            },

            #{
                title => <<"Pull Systems: The Kanban Way">>,
                content => <<
                    "In a **pull system**, downstream stages pull work when they have capacity:\n\n"

                    "Backlog ← Developer pulls task (when ready)\n"
                    "In Progress ← QA pulls feature (when they can test it)\n"
                    "Testing ← Deploy pulls release (when system ready)\n\n"

                    "Benefits of pull systems:\n\n"

                    "**1. Capacity-Based Flow**\n"
                    "Work only moves when there's capacity to handle it.\n"
                    "No overload, no overwhelming queues.\n\n"

                    "**2. WIP Limits**\n"
                    "Each stage has a maximum work-in-progress limit.\n"
                    "Example: Dev can work on max 3 tasks, QA max 2 features.\n\n"

                    "**3. Visual Bottlenecks**\n"
                    "When a column fills up, you immediately see the problem.\n"
                    "Blocked work is visible, not hidden in someone's backlog.\n\n"

                    "**4. Focus and Flow**\n"
                    "People finish what they started before pulling new work.\n"
                    "Deep work instead of constant context switching.\n\n"

                    "**5. Team-Based Optimization**\n"
                    "When bottleneck appears, team can swarm to clear it.\n"
                    "System optimizes for overall flow, not local efficiency."
                >>,
                examples => [
                    <<"Kanban board: To Do | In Progress (WIP: 3) | Review (WIP: 2) | Done">>,
                    <<"Developer finishes task, pulls next one from To Do">>,
                    <<"Review column full? Developers help with reviews before starting new work">>,
                    <<"Deploy ready? Pull next release from Testing">>
                ],
                key_points => [
                    <<"Pull = downstream pulls when ready">>,
                    <<"WIP limits prevent overload">>,
                    <<"Bottlenecks become visible immediately">>,
                    <<"Enables focus and flow">>,
                    <<"Team optimizes system, not individuals">>
                ]
            },

            #{
                title => <<"TCPS Pull Implementation">>,
                content => <<
                    "TCPS implements pull systems through several mechanisms:\n\n"

                    "**1. Kanban Board (tcps_kanban.erl)**\n"
                    "Visual board with WIP limits per column.\n"
                    "Developers pull tasks from backlog when ready.\n"
                    "System prevents exceeding WIP limits.\n\n"

                    "**2. Receipt-Based Flow**\n"
                    "Instead of pushing code to CI/CD:\n"
                    "• Developer completes work, creates receipt\n"
                    "• Receipt enters quality gate queue\n"
                    "• Quality system pulls receipt when ready to validate\n"
                    "• Deploy system pulls validated receipt when ready to ship\n\n"

                    "**3. Resource On-Demand**\n"
                    "Instead of pre-allocating resources:\n"
                    "• Tests run when triggered (not continuously)\n"
                    "• Containers spin up when needed (not always-on)\n"
                    "• Documentation generates when requested (not on every commit)\n\n"

                    "**4. Priority Queues**\n"
                    "Backlog organized by priority, but pulled in order.\n"
                    "High priority doesn't push out in-progress work.\n"
                    "Finish what you started (respect WIP limits).\n\n"

                    "**5. Explicit Capacity**\n"
                    "Teams set their WIP limits based on:\n"
                    "• Team size and skills\n"
                    "• Nature of work (complexity, uncertainty)\n"
                    "• Historical data (what worked before)\n"
                    "• Regular tuning (adjust limits over time)"
                >>,
                key_points => [
                    <<"Kanban board with WIP limits">>,
                    <<"Receipt-based task flow">>,
                    <<"On-demand resource allocation">>,
                    <<"Priority doesn't violate WIP limits">>,
                    <<"Capacity-based work pull">>
                ]
            },

            #{
                title => <<"When Pull Works Better Than Push">>,
                content => <<
                    "Pull systems excel when:\n\n"

                    "**Variable Work Items**\n"
                    "Tasks have different sizes and complexities.\n"
                    "Pull adapts naturally to variation.\n\n"

                    "**Uncertain Capacity**\n"
                    "Team availability fluctuates (meetings, learning, support).\n"
                    "Pull adjusts to actual capacity.\n\n"

                    "**Quality Over Speed**\n"
                    "Better to finish fewer things well than start many things poorly.\n"
                    "Pull enforces completing before starting.\n\n"

                    "**Knowledge Work**\n"
                    "Software development requires focus and deep thinking.\n"
                    "Pull minimizes context switching.\n\n"

                    "**Continuous Improvement**\n"
                    "Pull systems make problems visible quickly.\n"
                    "Team can experiment with WIP limits and optimize.\n\n"

                    "Push systems can work when:\n"
                    "• Work items are uniform (same size/complexity)\n"
                    "• Capacity is predictable and stable\n"
                    "• Speed matters more than quality\n"
                    "• Work is routine/mechanical\n\n"

                    "Software development rarely has these characteristics,\n"
                    "which is why pull systems typically work better."
                >>,
                key_points => [
                    <<"Pull excels with variable, complex work">>,
                    <<"Software development is ideal for pull">>,
                    <<"Push works for uniform, predictable work">>,
                    <<"Pull enables continuous improvement">>
                ]
            }
        ],

        analogies => [
            <<"Push = Flooding a pipe with water; Pull = Opening a tap when you need water">>,
            <<"Push = Restaurant kitchen cooking everything; Pull = Cooking when customer orders">>,
            <<"Push = Email overload; Pull = Checking email when you're ready to respond">>
        ],

        trade_offs => #{
            pros => [
                <<"Prevents overload and context switching">>,
                <<"Makes bottlenecks visible immediately">>,
                <<"Enables focus and deep work">>,
                <<"Adapts to actual capacity">>,
                <<"Reduces waste (unfinished work)">>,
                <<"Improves quality through focus">>
            ],
            cons => [
                <<"May feel slower initially (limiting WIP)">>,
                <<"Requires discipline to respect limits">>,
                <<"Can create idle time if limits too conservative">>,
                <<"Needs cultural shift from 'busy' to 'effective'">>,
                <<"Requires visual management (Kanban board)">>
            ]
        },

        related => [
            <<"why_tcps">>,
            <<"wip_limits_matter">>,
            <<"heijunka_leveling">>
        ],

        tags => [
            <<"pull systems">>,
            <<"push systems">>,
            <<"kanban">>,
            <<"wip limits">>,
            <<"flow">>,
            <<"toyota">>
        ],

        difficulty => intermediate,
        reading_time_minutes => 9
    }.

%% @doc Andon: The Power of Stop-the-Line Thinking
andon_thinking() ->
    #{
        id => <<"andon_thinking">>,
        title => <<"Andon: The Power of Stop-the-Line Thinking">>,
        category => core_concepts,
        summary => <<"Understand the Andon cord principle—empowering anyone to stop work when quality is at risk, and why it's essential for software excellence.">>,

        sections => [
            #{
                title => <<"What is Andon?">>,
                content => <<
                    "**Andon** (行灯) is a Japanese term for a paper lantern.\n"
                    "In Toyota factories, it refers to the visual management system that\n"
                    "alerts everyone when there's a problem on the production line.\n\n"

                    "The most famous element: **The Andon Cord**\n\n"

                    "A cord running along the entire assembly line that ANY worker can pull\n"
                    "to stop production when they detect a quality problem.\n\n"

                    "This was revolutionary in manufacturing:\n\n"

                    "**Traditional factories:**\n"
                    "• Workers keep line moving no matter what\n"
                    "• Quality inspectors catch defects at end\n"
                    "• Defective products get reworked or scrapped\n"
                    "• Workers feel powerless to improve quality\n\n"

                    "**Toyota with Andon:**\n"
                    "• ANY worker can stop the line\n"
                    "• Problems fixed immediately, on the spot\n"
                    "• Defects never move downstream\n"
                    "• Workers empowered to ensure quality\n\n"

                    "The psychological impact is profound: Quality becomes EVERYONE'S responsibility,\n"
                    "not just management's or QA's. Workers take ownership and pride in the product."
                >>,
                key_points => [
                    <<"Anyone can stop production for quality">>,
                    <<"Problems fixed immediately, not deferred">>,
                    <<"Empowerment creates ownership">>,
                    <<"Quality is everyone's responsibility">>
                ]
            },

            #{
                title => <<"The Counterintuitive Power of Stopping">>,
                content => <<
                    "Stopping production seems counterproductive:\n"
                    "• Isn't the goal to keep moving?\n"
                    "• Won't stopping slow us down?\n"
                    "• Shouldn't we fix problems later?\n\n"

                    "Toyota discovered the opposite is true:\n\n"

                    "**Stopping SPEEDS UP overall production:**\n\n"

                    "Without Andon:\n"
                    "• Defect created at Station 1\n"
                    "• Continues through Stations 2-10\n"
                    "• Detected at final inspection\n"
                    "• Product scrapped or sent back to Station 1\n"
                    "• Rework takes 10x longer than initial fix\n"
                    "• Root cause never addressed (keeps recurring)\n\n"

                    "With Andon:\n"
                    "• Defect detected at Station 1\n"
                    "• Worker pulls cord, line stops (5 seconds)\n"
                    "• Team swarms, fixes root cause (2 minutes)\n"
                    "• Line resumes, problem never recurs\n"
                    "• Total time: 2 minutes vs. 30 minutes rework\n\n"

                    "The math is clear: Early stopping and fixing is FASTER than letting\n"
                    "defects propagate. Plus, you eliminate recurring defects.\n\n"

                    "**The Exponential Cost of Downstream Defects:**\n"
                    "• Fix at source: 1x effort\n"
                    "• Fix one stage later: 10x effort\n"
                    "• Fix at end of line: 100x effort\n"
                    "• Fix in production: 1000x effort"
                >>,
                diagrams => [
                    <<"Cost of Fix: Design (1x) → Build (10x) → Test (100x) → Production (1000x)">>,
                    <<"Andon stops at source, prevents exponential cost increase">>
                ],
                key_points => [
                    <<"Stopping early = faster overall">>,
                    <<"Prevention cheaper than rework">>,
                    <<"Defect cost increases exponentially">>,
                    <<"Fix at source, not downstream">>
                ]
            },

            #{
                title => <<"Andon in Software: Quality Gates">>,
                content => <<
                    "TCPS implements Andon through Quality Gates that automatically\n"
                    "'pull the cord' when quality standards are violated:\n\n"

                    "**1. Pre-Commit Gates (Earliest Stop)**\n"
                    "Catch issues before code enters repository:\n"
                    "• Type errors, lint violations, missing tests\n"
                    "• Developer fixes immediately (1 minute)\n"
                    "• Cost: Minimal (still in developer's head)\n\n"

                    "**2. Merge Gates (Stop Before Integration)**\n"
                    "Catch issues before code joins main branch:\n"
                    "• Test failures, coverage drops, security issues\n"
                    "• Developer fixes in branch (30 minutes)\n"
                    "• Cost: Low (code still isolated)\n\n"

                    "**3. Deployment Gates (Stop Before Production)**\n"
                    "Catch issues before code reaches users:\n"
                    "• Integration failures, performance regressions\n"
                    "• Team fixes in staging (2 hours)\n"
                    "• Cost: Medium (requires coordination)\n\n"

                    "**4. Monitoring/Andon Alerts (Production Safety Net)**\n"
                    "Catch issues in production quickly:\n"
                    "• Error rate spikes, performance degradation\n"
                    "• Team triggers rollback/hotfix (1 day)\n"
                    "• Cost: High (impacts users, reputation)\n\n"

                    "Each gate is an Andon cord. The earlier you pull it, the cheaper the fix.\n\n"

                    "**Developer Empowerment:**\n"
                    "Just like factory workers, developers can 'pull the Andon cord':\n"
                    "• Block their own PR if quality isn't ready\n"
                    "• Request help from team to fix issues\n"
                    "• Suggest new gates when gaps discovered\n"
                    "• Stop deployment if monitoring shows problems"
                >>,
                examples => [
                    <<"Pre-commit: 'Type error in utils.py, fix before commit'">>,
                    <<"Merge gate: 'Coverage dropped 85%→78%, add tests before merge'">>,
                    <<"Deploy gate: 'Security vulnerability found, block deploy'">>,
                    <<"Andon alert: 'Error rate 0.1%→5%, rollback immediately'">>
                ],
                key_points => [
                    <<"Multiple Andon cords at different stages">>,
                    <<"Earlier stop = cheaper fix">>,
                    <<"Developers empowered to stop delivery">>,
                    <<"Quality gates = automatic Andon">>
                ]
            },

            #{
                title => <<"Cultural Shift: Stop is Not Failure">>,
                content => <<
                    "The hardest part of Andon isn't technical—it's cultural.\n\n"

                    "**Traditional software culture:**\n"
                    "• 'Keep shipping no matter what'\n"
                    "• 'We'll fix bugs later'\n"
                    "• 'Don't block the release'\n"
                    "• Stopping = failure, punishment, blame\n\n"

                    "**Andon/TCPS culture:**\n"
                    "• 'Stop and fix is normal'\n"
                    "• 'Quality first, then speed'\n"
                    "• 'Better to stop now than break prod'\n"
                    "• Stopping = responsibility, improvement, care\n\n"

                    "Psychological safety is essential:\n\n"

                    "**Workers won't pull Andon cord if:**\n"
                    "• They fear blame or punishment\n"
                    "• Management pressures them to keep moving\n"
                    "• Stopping is seen as personal failure\n"
                    "• Past stops led to negative consequences\n\n"

                    "**Workers confidently pull Andon cord when:**\n"
                    "• Organization celebrates quality catches\n"
                    "• Team swarms to help (not blame)\n"
                    "• Stopping is seen as system improvement\n"
                    "• Past stops led to better processes\n\n"

                    "Toyota learned: The best factory is one where workers pull\n"
                    "the Andon cord OFTEN (early problem detection) and\n"
                    "resolve quickly (effective teamwork).\n\n"

                    "High Andon pulls = high quality awareness.\n"
                    "Low Andon pulls = either perfect (rare) or fear-based (dangerous)."
                >>,
                key_points => [
                    <<"Stopping is normal, not failure">>,
                    <<"Psychological safety required">>,
                    <<"Celebrate quality catches">>,
                    <<"Team swarms to help, not blame">>,
                    <<"High Andon usage = healthy culture">>
                ]
            }
        ],

        analogies => [
            <<"Like emergency stop button on machinery—everyone knows where it is and when to use it">>,
            <<"Like circuit breaker in home—automatically stops electrical flow when danger detected">>,
            <<"Like 'Stop the Line' at Toyota—anyone empowered to prevent defects from moving forward">>
        ],

        trade_offs => #{
            pros => [
                <<"Catches defects at source (cheapest fix)">>,
                <<"Empowers developers to own quality">>,
                <<"Prevents cascading failures">>,
                <<"Creates quality-first culture">>,
                <<"Reduces rework and technical debt">>,
                <<"Faster overall delivery (less firefighting)">>
            ],
            cons => [
                <<"Requires cultural shift and psychological safety">>,
                <<"May slow individual commits (but speeds delivery)">>,
                <<"Needs team discipline and support">>,
                <<"Can feel uncomfortable initially (stopping feels wrong)">>,
                <<"Requires investment in gates and automation">>
            ]
        },

        related => [
            <<"jidoka_philosophy">>,
            <<"quality_gates_vs_ci">>,
            <<"andon_vs_monitoring">>
        ],

        tags => [
            <<"andon">>,
            <<"quality">>,
            <<"empowerment">>,
            <<"stop-the-line">>,
            <<"toyota">>,
            <<"culture">>
        ],

        difficulty => intermediate,
        reading_time_minutes => 10
    }.

%% @doc Heijunka: Leveling Production Load
heijunka_leveling() ->
    #{
        id => <<"heijunka_leveling">>,
        title => <<"Heijunka: Leveling Production Load">>,
        category => core_concepts,
        summary => <<"Learn how Heijunka (production leveling) balances workload to create sustainable pace and predictable delivery in software development.">>,

        sections => [
            #{
                title => <<"What is Heijunka?">>,
                content => <<
                    "**Heijunka** (平準化) means 'leveling' or 'smoothing' in Japanese.\n"
                    "It's the practice of balancing production volume and variety over time\n"
                    "to create predictable, sustainable flow.\n\n"

                    "**The Problem Heijunka Solves:**\n\n"

                    "Without leveling:\n"
                    "• Week 1: Build 100 red cars\n"
                    "• Week 2: Build 100 blue cars\n"
                    "• Week 3: Build 50 red, 50 blue cars\n"
                    "• Week 4: Rush order! Build 200 blue cars!\n\n"

                    "Result:\n"
                    "• Red paint station idle Week 2\n"
                    "• Blue paint station overwhelmed Week 4\n"
                    "• Workers stressed by constant variation\n"
                    "• Inventory swings wildly\n"
                    "• Quality suffers during rush periods\n\n"

                    "With Heijunka:\n"
                    "• Every day: Build mix of red and blue cars\n"
                    "• Every week: Consistent volume (100 total)\n"
                    "• Smooth pattern: R-B-R-B-R-B repeating\n\n"

                    "Result:\n"
                    "• All stations have consistent workload\n"
                    "• Workers develop steady rhythm\n"
                    "• Inventory stays balanced\n"
                    "• Quality remains high\n"
                    "• Predictable delivery times"
                >>,
                key_points => [
                    <<"Level both volume and variety">>,
                    <<"Create predictable, smooth flow">>,
                    <<"Avoid feast-or-famine cycles">>,
                    <<"Sustainable pace for team">>
                ]
            },

            #{
                title => <<"Two Dimensions of Leveling">>,
                content => <<
                    "Heijunka balances TWO dimensions:\n\n"

                    "**1. Volume Leveling (平準化量)**\n"
                    "Smooth out the AMOUNT of work over time.\n\n"

                    "Without volume leveling:\n"
                    "• Sprint 1: 20 story points\n"
                    "• Sprint 2: 45 story points (crunch!)\n"
                    "• Sprint 3: 10 story points (recovery)\n"
                    "• Sprint 4: 50 story points (death march!)\n\n"

                    "With volume leveling:\n"
                    "• Sprint 1: 30 story points\n"
                    "• Sprint 2: 32 story points\n"
                    "• Sprint 3: 28 story points\n"
                    "• Sprint 4: 31 story points\n\n"

                    "Consistent velocity = predictable delivery = sustainable pace.\n\n"

                    "**2. Variety Leveling (平準化種)**\n"
                    "Smooth out the TYPE of work over time.\n\n"

                    "Without variety leveling:\n"
                    "• Sprint 1: All feature development\n"
                    "• Sprint 2: All bug fixes\n"
                    "• Sprint 3: All technical debt\n"
                    "• Sprint 4: All documentation\n\n"

                    "With variety leveling (every sprint):\n"
                    "• 70% feature development\n"
                    "• 20% technical debt/refactoring\n"
                    "• 10% bug fixes/learning\n\n"

                    "Balanced work types = well-rounded progress = healthy codebase."
                >>,
                examples => [
                    <<"Volume: Target 30 points/sprint ±10% (27-33 points)">>,
                    <<"Variety: 7 feature tasks, 2 tech debt, 1 bug fix each sprint">>,
                    <<"Daily: Mix of coding, reviewing, testing, documenting">>,
                    <<"Weekly: Balance new features with maintenance">>
                ],
                key_points => [
                    <<"Volume leveling = consistent amount">>,
                    <<"Variety leveling = consistent mix">>,
                    <<"Both needed for sustainability">>,
                    <<"Predictability emerges from leveling">>
                ]
            },

            #{
                title => <<"Heijunka in Software Development">>,
                content => <<
                    "TCPS applies Heijunka through several practices:\n\n"

                    "**1. Sprint/Iteration Planning**\n"
                    "Use historical data to set consistent capacity:\n"
                    "• Team velocity: 30 points/sprint (track over 6 sprints)\n"
                    "• Don't overcommit (45 points) or undercommit (15 points)\n"
                    "• Aim for small variation: ±10% is healthy, ±50% is chaos\n\n"

                    "**2. Work Type Ratios**\n"
                    "Maintain consistent mix of work types:\n"
                    "• Features: 70% (new capabilities, user value)\n"
                    "• Technical debt: 20% (refactoring, upgrades, cleanup)\n"
                    "• Bugs/Support: 10% (fixes, customer issues)\n"
                    "• Learning: Built into feature work (not separate)\n\n"

                    "**3. Daily Balancing**\n"
                    "Within each day, mix activities:\n"
                    "• Morning: Deep work (coding, design)\n"
                    "• Afternoon: Collaborative work (reviews, pairing)\n"
                    "• Throughout: Testing, documentation\n"
                    "• Avoid: All-day coding marathons, all-day meeting days\n\n"

                    "**4. Heijunka Box (Kanban Board)**\n"
                    "Visual tool showing balanced work:\n"
                    "• Columns: To Do, In Progress, Review, Done\n"
                    "• Swim lanes: Features, Tech Debt, Bugs\n"
                    "• WIP limits: 3 features, 1 tech debt, 1 bug\n"
                    "• Visual balance: Not all work in one swim lane\n\n"

                    "**5. Avoiding Batching**\n"
                    "Don't batch similar work (defeats leveling):\n"
                    "• ❌ Bad: Code for 2 weeks, then test for 2 weeks\n"
                    "• ✅ Good: Code + test each feature as you go\n"
                    "• ❌ Bad: Build features for 3 sprints, then fix tech debt\n"
                    "• ✅ Good: Include tech debt in every sprint"
                >>,
                key_points => [
                    <<"Consistent sprint capacity">>,
                    <<"Balanced work type ratios (70/20/10)">>,
                    <<"Daily activity mixing">>,
                    <<"Visual heijunka box (Kanban)">>,
                    <<"Avoid batching work">>
                ]
            },

            #{
                title => <<"Benefits of Leveling">>,
                content => <<
                    "**1. Sustainable Pace**\n"
                    "No more crunch weeks followed by recovery weeks.\n"
                    "Team maintains consistent, humane work rhythm.\n"
                    "Lower burnout, higher retention.\n\n"

                    "**2. Predictable Delivery**\n"
                    "When velocity is consistent, forecasting is accurate.\n"
                    "Stakeholders get reliable commitments.\n"
                    "Planning becomes easier.\n\n"

                    "**3. Better Quality**\n"
                    "Rushed work (from unleveled spikes) creates defects.\n"
                    "Steady pace allows time for thoroughness.\n"
                    "Quality gates work better with consistent flow.\n\n"

                    "**4. Skill Balance**\n"
                    "Variety leveling keeps all skills active:\n"
                    "• Don't forget how to refactor (if only coding features)\n"
                    "• Don't lose feature velocity (if only fixing debt)\n"
                    "• Maintain well-rounded capabilities\n\n"

                    "**5. Reduced Waste**\n"
                    "Unleveled work creates waste:\n"
                    "• Idle time when one type of work dries up\n"
                    "• Overtime when another type surges\n"
                    "• Context switching between extreme modes\n"
                    "• Inventory buildup (unfinished work between phases)\n\n"

                    "**6. Improved Morale**\n"
                    "Teams prefer predictable, balanced work over chaos.\n"
                    "Autonomy to manage own pace (within leveled constraints).\n"
                    "Pride in consistent, quality delivery."
                >>,
                key_points => [
                    <<"Sustainable pace prevents burnout">>,
                    <<"Predictability enables planning">>,
                    <<"Quality improves with steady flow">>,
                    <<"Skills stay balanced">>,
                    <<"Waste reduced, morale improved">>
                ]
            }
        ],

        analogies => [
            <<"Like pacing yourself in a marathon—steady pace beats sprinting and resting">>,
            <<"Like eating balanced meals—protein, carbs, vegetables every day (not all protein one day, all carbs next)">>,
            <<"Like training schedule—mix of strength, cardio, flexibility (not all one type)">>
        ],

        trade_offs => #{
            pros => [
                <<"Sustainable, humane pace for team">>,
                <<"Predictable delivery and planning">>,
                <<"Higher quality through consistency">>,
                <<"Reduced waste and context switching">>,
                <<"Better morale and retention">>,
                <<"Balanced skill development">>
            ],
            cons => [
                <<"May feel slow when 'urgent' work appears">>,
                <<"Requires discipline to maintain ratios">>,
                <<"Can't easily handle true emergencies (use sparingly)">>,
                <<"Needs accurate historical data (takes time to build)">>,
                <<"Stakeholders may resist 'saying no' to surges">>
            ]
        },

        related => [
            <<"pull_vs_push">>,
            <<"wip_limits_matter">>,
            <<"why_tcps">>
        ],

        tags => [
            <<"heijunka">>,
            <<"leveling">>,
            <<"sustainability">>,
            <<"flow">>,
            <<"toyota">>,
            <<"pace">>
        ],

        difficulty => intermediate,
        reading_time_minutes => 9
    }.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

why_tcps_structure_test() ->
    Explanation = why_tcps(),
    ?assertEqual(<<"why_tcps">>, maps:get(id, Explanation)),
    ?assertEqual(core_concepts, maps:get(category, Explanation)),
    ?assertEqual(4, length(maps:get(sections, Explanation))),
    ?assert(length(maps:get(analogies, Explanation)) > 0).

jidoka_philosophy_structure_test() ->
    Explanation = jidoka_philosophy(),
    ?assertEqual(<<"jidoka_philosophy">>, maps:get(id, Explanation)),
    ?assertEqual(core_concepts, maps:get(category, Explanation)),
    ?assertEqual(intermediate, maps:get(difficulty, Explanation)).

pull_vs_push_structure_test() ->
    Explanation = pull_vs_push(),
    ?assertEqual(<<"pull_vs_push">>, maps:get(id, Explanation)),
    ?assert(length(maps:get(sections, Explanation)) >= 3).

andon_thinking_structure_test() ->
    Explanation = andon_thinking(),
    ?assertEqual(<<"andon_thinking">>, maps:get(id, Explanation)),
    ?assert(maps:is_key(trade_offs, Explanation)).

heijunka_leveling_structure_test() ->
    Explanation = heijunka_leveling(),
    ?assertEqual(<<"heijunka_leveling">>, maps:get(id, Explanation)),
    ?assert(is_integer(maps:get(reading_time_minutes, Explanation))).

-endif.
