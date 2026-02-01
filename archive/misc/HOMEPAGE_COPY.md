# Homepage Copy: The 80/20 Cloud
**Never mentions MCP+. Pulls customers in through pain → simplicity → proof.**

---

## HERO SECTION

### Headline
**You're paying for 100%. You're using 20%.**

### Subheadline
Cloud tools are bloated, expensive, and built for enterprises you'll never become. We sell the 20% you actually use—for 5× less.

### Primary CTA
**Start Free** → (no credit card, deploys in 60 seconds)

### Secondary CTA
**See the Savings** → (cost calculator)

### Hero Visual
Split-screen comparison:
- **LEFT**: Datadog dashboard with 47 unused features grayed out, "$2,800/mo" in red
- **RIGHT**: Our observability dashboard with 6 core metrics highlighted, "$99/mo" in green
- **Caption**: "Same insights. 28× cheaper."

---

## PROBLEM SECTION

### Headline
**Enterprise software is built for problems you don't have.**

### 3-Column Pain Points

#### Column 1: Overpriced
**What they charge:**
- Datadog: $99/host/month
- Vanta: $20K/year
- Temporal: $2K/month minimum

**What you need:**
- System metrics and error logs
- SOC 2 documentation
- Retry logic for failed jobs

**The gap:**
You're funding features for Fortune 500 companies.

#### Column 2: Overcomplicated
**What they sell:**
- 300-page admin guides
- 6-week onboarding programs
- Dedicated CSM required

**What you need:**
- Install
- Configure
- Done

**The gap:**
Complexity is a business model, not a feature.

#### Column 3: Unverifiable
**What they give you:**
- "Trust our dashboard"
- CSV exports you hope are accurate
- Logs that might be complete

**What you need:**
- Cryptographic proof
- Tamper-evident audit trails
- Evidence auditors accept

**The gap:**
Hope is not a compliance strategy.

---

## SOLUTION SECTION

### Headline
**The 80/20 of everything. Built from scratch.**

### Subheadline
We didn't fork their code. We rebuilt each tool to do one thing well—and prove it happened.

### 4-Point Value Prop

#### 1. Radically Simple
No enterprise bloat. No unused features. No 47-page setup guides.

**Example**: Our agent runtime has 6 config options. LangChain has 143.

#### 2. Radically Cheaper
We charge for what you use, not what we built.

**Example**: Our workflow engine costs $99/mo. Temporal starts at $2,000/mo.

#### 3. Radically Transparent
Every action generates a cryptographic receipt. Your auditors will love you.

**Example**: Our ETL proves data lineage. Fivetran asks you to trust their logs.

#### 4. Radically Fast
Self-host in 60 seconds. Cloud deploy in 5 minutes. No sales calls.

**Example**: From signup to production in the time it takes to read Datadog's pricing page.

---

## SKU SHOWCASE: "Start Here"

### Headline
**Pick your pain point. We'll solve it this week.**

### SKU Cards (First 5 Launch SKUs)

#### SKU #1: Agent Runtime
**Replace**: LangChain Cloud ($15K/yr), LangSmith ($5K/yr)

**What it does**:
- Run AI agents with any LLM
- 5 built-in tools (shell, browser, files, search, API)
- Structured logging with request/response receipts
- stdio and SSE transports

**What it doesn't do**:
- Support 50+ LLMs you'll never use
- Complex multi-agent orchestration
- Enterprise SSO (use our Access Control SKU instead)

**Pricing**:
- Free (self-host, unlimited)
- $49/mo (cloud, 10K messages)
- $999/mo (cloud, unlimited)

**Migration time**: 2 hours

[Start Free] [See Docs]

---

#### SKU #2: Workflow Engine
**Replace**: Temporal Cloud ($2K/mo+), AWS Step Functions ($0.025/transition)

**What it does**:
- Retry failed jobs automatically
- Track workflow state with receipts
- Schedule recurring tasks
- Simple YAML configuration

**What it doesn't do**:
- Distributed sagas across 47 microservices
- Workflow versioning (ship v2, don't run both)
- Complex compensation logic

**Pricing**:
- Free (self-host, unlimited)
- $99/mo (cloud, 1K workflows/mo)
- $2,999/mo (cloud, unlimited)

**Migration time**: 4 hours

[Start Free] [See Docs]

---

#### SKU #3: Compliance Docs
**Replace**: Vanta ($20K/yr), Drata ($30K/yr)

**What it does**:
- Generate SOC 2 Type 2 documentation
- Auto-collect evidence from your receipts
- Map controls to NIST 800-53, ISO 27001
- Export audit-ready PDFs

**What it doesn't do**:
- Continuous vendor risk monitoring (use our Vuln Scanner SKU)
- Automated evidence collection from 50+ SaaS tools (we collect from *our* SKUs)
- Audit management platform

**Pricing**:
- Free (policy templates)
- $499/mo (SOC 2 Type 2)
- $3,999/mo (SOC 2 + ISO + NIST)

**Migration time**: 1 day (first report in 2 weeks)

[Start Free] [See Docs]

---

#### SKU #4: Policy-as-Code
**Replace**: Open Policy Agent Enterprise ($30K/yr), Styra DAS ($50K/yr)

**What it does**:
- Validate requests against policies
- Enforce access control rules
- Generate policy violation receipts
- Integrate with any app via API

**What it doesn't do**:
- Learn a new language (policies are SHACL, which is RDF)
- Distributed policy bundles (self-host or use our cloud)
- Real-time policy updates (10-second refresh)

**Pricing**:
- Free (self-host, unlimited)
- $99/mo (cloud, 100 policies)
- $1,999/mo (cloud, unlimited)

**Migration time**: 6 hours

[Start Free] [See Docs]

---

#### SKU #5: Observability
**Replace**: Datadog ($99/host/mo), New Relic ($99/user/mo)

**What it does**:
- Collect system metrics (CPU, memory, disk, network)
- Aggregate error logs
- Trace requests across services
- Receipt-based distributed tracing

**What it doesn't do**:
- APM with code-level profiling (use language-native tools)
- 200+ integrations (we have 12 common ones)
- Anomaly detection ML (we have threshold alerts)

**Pricing**:
- Free (self-host, unlimited)
- $9/host/mo (cloud)
- $99/host/mo (cloud, unlimited retention)

**Migration time**: 3 hours

[Start Free] [See Docs]

---

### Browse All 20 Tools →

---

## HOW IT WORKS (Without Revealing MCP+)

### Headline
**Three principles. Zero compromises.**

#### Principle 1: Receipts, Not Dashboards
Every action generates a cryptographic receipt: who did what, when, with what data.

**Why this matters**:
- Auditors accept receipts, not screenshots
- You can prove compliance, not claim it
- Receipts are portable—export and verify anywhere

**Example**: Our ETL gives you a receipt chain proving data lineage from source to destination. Fivetran gives you a log file you hope is complete.

#### Principle 2: Data You Own
Your receipts live in **your** infrastructure. SQLite for self-host, S3-compatible storage for cloud.

**Why this matters**:
- No vendor lock-in (receipts are open standard)
- No data residency concerns (it never leaves your VPC)
- No "trust us" (receipts are cryptographically verifiable)

**Example**: Migrate from our Workflow Engine to yours in 30 minutes. Migrate from Temporal? Good luck.

#### Principle 3: One Factory, Infinite Tools
We built a code generation system that turns requirements into tools. Adding SKU #6 costs us 10% of what it cost to build SKU #1.

**Why this matters**:
- We can price at 5-10× discount (20% cost = 80% margin)
- We ship new tools in weeks, not quarters
- You get better tools for less money

**Example**: Our competitors raise $100M to build 5 products. We built 20 products with 4 engineers.

---

## PRICING PHILOSOPHY

### Headline
**Free to start. Paid to scale. Enterprise when you're ready.**

### 3-Tier Explanation

#### Free (Self-Host)
- All 20 tools, unlimited usage
- Receipts stored locally (SQLite)
- Community support (Discord, GitHub)
- No credit card required

**Best for**: Startups, side projects, learning

#### Paid (Cloud)
- Managed hosting (99.9% SLA)
- Receipts stored in your S3 bucket
- Email support (24-hour response)
- Usage-based pricing (see each SKU)

**Best for**: Growing teams, production workloads

#### Enterprise (Custom)
- Air-gapped deployment
- Custom compliance profiles (FedRAMP, HIPAA, PCI)
- Dedicated support (Slack Connect)
- Volume discounts

**Best for**: Regulated industries, large orgs

### Pricing Comparison Calculator

**Interactive tool**:
- User selects tools they use (Datadog, Temporal, Vanta, etc.)
- We show current annual cost
- We show our cost for equivalent 80/20 SKUs
- We show 3-year savings

**Example output**:
> You're currently spending **$84,000/year** on:
> - Datadog: $35,400
> - Temporal: $24,000
> - Vanta: $20,000
> - LaunchDarkly: $4,600
>
> Switch to our SKUs and pay **$16,788/year**.
>
> **3-year savings: $201,636**
>
> [Start Free Trial]

---

## SOCIAL PROOF SECTION

### Headline
**Built by engineers who got tired of overpaying.**

### 3 Testimonial Cards (Hypothetical, for launch)

#### Card 1: Startup Founder
> "We were spending $4K/month on Datadog, Temporal, and LaunchDarkly. We're now spending $400/month and getting 90% of the value. The receipts alone saved us 40 hours during our SOC 2 audit."
>
> — **Alex Chen**, CTO, Cascade AI (YC S24)

#### Card 2: Mid-Market Engineering Lead
> "Temporal's sales team wanted $30K/year minimum. We self-hosted the Workflow Engine in 20 minutes and it's been rock-solid for 6 months. When we need enterprise features, we'll upgrade—but we're not there yet."
>
> — **Jordan Rivera**, VP Engineering, Flexport competitor

#### Card 3: Security Engineer
> "Vanta wanted $25K for SOC 2. We generated the exact same documentation from our receipts for $499/month. Our auditor said it was the most thorough evidence package they'd seen."
>
> — **Sam Park**, Security Lead, Series B fintech

### Customer Logo Bar
[Show 12-16 logos of hypothetical early customers]

---

## FAQ SECTION

### Headline
**Questions we get asked 47 times a day.**

#### Q: Is this open source?
**A**: The runtime is Apache 2.0. The cloud hosting is paid. Think: Postgres (free) vs. RDS (managed).

#### Q: What's a "receipt"?
**A**: A cryptographic proof that an action happened. Like a git commit hash, but for API calls, workflow runs, policy decisions, etc. Your auditors can verify them independently.

#### Q: Can I mix your tools with others?
**A**: Yes. Our tools integrate via standard APIs. Use our Observability SKU with your existing Temporal workflows, no problem.

#### Q: How is this so cheap?
**A**: We don't have enterprise sales teams, complex onboarding, or feature bloat. We built a code generator that turns requirements into tools—so our cost per SKU is 10% of what incumbents pay.

#### Q: What if I need enterprise features?
**A**: Upgrade to our Enterprise tier (custom compliance profiles, air-gap deployment, dedicated support). Still 3-5× cheaper than incumbents.

#### Q: How do you make money if free tier is unlimited?
**A**: 80% of users self-host (free). 15% upgrade to cloud (paid). 5% need enterprise (high-margin). This is the open-source SaaS model.

#### Q: Is this secure?
**A**: Receipts use NIST-approved post-quantum cryptography. You can verify them yourself (receipts are signed, timestamped, hash-chained). We don't ask you to "trust our logs."

#### Q: What happens if you shut down?
**A**: You have all your receipts locally. Self-host the runtime (Apache 2.0). Migrate to another tool in hours, not months.

#### Q: Can you prove this works?
**A**: Every receipt has a hash. Run `sha256sum` on the receipt file—if the hash matches, the data is intact. That's cryptographic proof, not a dashboard screenshot.

---

## COMPARISON TABLE

### Headline
**How we stack up against the $100M+ incumbents.**

| Feature | Incumbents | Us |
|---------|-----------|-----|
| **Pricing** | $2K-$30K/year per tool | $99-$2,999/year per tool |
| **Onboarding** | 4-6 weeks, requires CSM | 1-4 hours, self-serve |
| **Evidence** | Logs you export | Cryptographic receipts |
| **Lock-in** | Proprietary data formats | Open standard receipts |
| **Self-host** | Enterprise-only | Free, unlimited |
| **Vendor risk** | Trust their infrastructure | Receipts in your S3 |
| **Complexity** | 100+ features you don't use | 20% you actually need |
| **Support** | Sales calls, ticket systems | Docs, Discord, email |
| **Migration time** | 3-6 months | 2-48 hours |

---

## TRUST INDICATORS

### Headline
**Show, don't tell.**

#### Security
- ✅ NIST FIPS 203/204/205 (post-quantum cryptography)
- ✅ SOC 2 Type 2 compliant (generated with our own tool)
- ✅ Open source runtime (audit the code yourself)
- ✅ Zero-knowledge architecture (we can't see your data)

#### Reliability
- ✅ 99.9% uptime SLA (cloud tier)
- ✅ Self-host option (no single point of failure)
- ✅ Multi-region deployments (AWS, GCP, Azure)
- ✅ Automated backups (hourly snapshots)

#### Transparency
- ✅ Public roadmap (GitHub Projects)
- ✅ Open pricing (no "contact sales")
- ✅ Incident reports (status page with receipts)
- ✅ Open compliance docs (see our SOC 2 report)

---

## FINAL CTA SECTION

### Headline
**Stop overpaying. Start today.**

### Subheadline
Deploy your first tool in 60 seconds. No credit card. No sales call. No regrets.

### Two-Path CTA

#### Path 1: Self-Host (Free)
```bash
curl -sSL https://get.80-20.cloud | sh
```
[Copy command] [View install docs]

**What you get**:
- All 20 tools, unlimited usage
- Receipts stored locally (SQLite)
- Community support

#### Path 2: Cloud (Free Trial)
[Start 14-Day Trial] → (no credit card, full access)

**What you get**:
- Managed hosting, 99.9% SLA
- 10K requests/month per tool
- Email support

### Below the Fold
**Migrating from an incumbent?** We'll help you switch for free. [Book a migration call →]

**Need enterprise features?** Custom compliance, air-gap, Slack support. [Talk to our team →]

**Just want to learn?** Join 2,000+ engineers in our Discord. [Join the community →]

---

## FOOTER

### Column 1: Product
- [All Tools](#) (browse 20 SKUs)
- [Pricing](#)
- [Docs](#)
- [API Reference](#)
- [GitHub](#) (runtime source code)

### Column 2: Solutions
- [Compliance Bundle](#) (SOC 2 in 2 weeks)
- [Developer Tools](#) (agents, workflows, policies)
- [Data Tools](#) (ETL, catalog, quality)
- [Enterprise](#) (FedRAMP, HIPAA, air-gap)

### Column 3: Resources
- [Blog](#) (how receipts work)
- [Comparison Guides](#) (vs. Datadog, vs. Vanta, etc.)
- [Case Studies](#)
- [Migration Guides](#)
- [Security](#) (SOC 2 report, pen test results)

### Column 4: Company
- [About](#) (we're 4 engineers)
- [Careers](#) (we're hiring)
- [Discord](#) (community)
- [Contact](#) (hello@80-20.cloud)

---

## MESSAGING PRINCIPLES (Internal, Not Published)

### What We Never Say:
- ❌ "MCP+" or "Model Context Protocol"
- ❌ "Erlang" or "Rust" or any implementation details
- ❌ "Ontology-driven" or "SPARQL queries"
- ❌ "Semantic web" or "RDF" or "knowledge graphs"
- ❌ "ggen" or "code generation system" (just say "built from scratch")

### What We Always Say:
- ✅ "Receipts" (not "cryptographic proofs" until FAQ)
- ✅ "80/20 of [incumbent]" (frame as simplification, not limitation)
- ✅ "Built from scratch" (implies we control the stack)
- ✅ "Open standard" (receipts are portable, but don't explain JSON-LD)
- ✅ "Self-host or cloud" (open source moat)

### Positioning Hierarchy:
1. **Primary**: You're overpaying for features you don't use
2. **Secondary**: We rebuilt each tool to do one thing well
3. **Tertiary**: Receipts give you proof, not promises
4. **Hidden**: MCP+ is the factory, ontology is the blueprint, Erlang is the engine

### Competitive Framing:
- **vs. Datadog**: 28× cheaper for 90% of the value
- **vs. Vanta**: $499 vs. $20K for the same SOC 2 docs
- **vs. Temporal**: Self-host in 20 minutes vs. $30K sales call
- **vs. Open Source**: We're open source too—but with managed cloud option

### Fear, Uncertainty, Doubt (FUD) to Address:
- **"Too good to be true"** → Show the receipts (literally)
- **"What if you shut down?"** → Self-host option + open source
- **"Can't be as good as [incumbent]"** → It's not. It's 80/20. That's the point.
- **"Security risk?"** → Post-quantum crypto + you own the data

---

## A/B TEST VARIATIONS

### Hero Headline Options:
1. **You're paying for 100%. You're using 20%.** (current)
2. Stop funding enterprise features you'll never use.
3. Cloud tools at 80% off. Because you don't need the other 80%.
4. The 80/20 of everything. 5× cheaper.

### Primary CTA Options:
1. **Start Free** (current)
2. Deploy in 60 seconds
3. See what you'll save
4. Try for free, upgrade when ready

### Value Prop Angle Options:
1. **Radically Simple / Cheaper / Transparent / Fast** (current, feature-focused)
2. **Less bloat / Less cost / Less trust / Less time** (problem-focused)
3. **Built for you / Priced for you / Proves it to you / Ships today** (customer-focused)

---

## LAUNCH CHECKLIST

### Pre-Launch:
- [ ] Domain: 80-20.cloud (or similar)
- [ ] Landing page (this copy)
- [ ] 5 SKU docs pages (one per launch SKU)
- [ ] Pricing calculator (interactive)
- [ ] Install script (curl | sh)
- [ ] Cloud trial signup (no CC)
- [ ] Discord community
- [ ] GitHub repos (runtime + docs)

### Launch Day:
- [ ] Post to Hacker News ("Show HN: The 80/20 of cloud tools, built from scratch")
- [ ] Post to r/programming, r/devops, r/selfhosted
- [ ] Tweet thread (receipts, pricing, self-host)
- [ ] Email to 100-person beta list

### Post-Launch:
- [ ] Comparison guides (vs. Datadog, vs. Vanta, vs. Temporal)
- [ ] Case study: "SOC 2 in 2 weeks with receipts"
- [ ] Blog: "How receipts work (and why your auditor will love them)"
- [ ] Podcast tour: Changelog, Software Engineering Daily, DevOps Chat

---

**The homepage is the storefront. The SKUs are the products. MCP+ is the factory in the back. Nobody sees the factory.**
