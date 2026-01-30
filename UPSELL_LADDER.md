# Upsell Ladder: Free → Paid → Enterprise → Government
**Customer Journey from $0 Self-Host to $50K/yr Gov Profile**

---

## STRATEGIC PRINCIPLE

**The upsell is NOT "pay more for better version of same SKU."**

**The upsell IS:**
1. **More SKUs** (1 tool → 3 tools → 5+ tools)
2. **Better infrastructure** (self-host → cloud → air-gap)
3. **Better compliance** (receipts → EHDIT → FedRAMP)
4. **Better support** (docs → email → Slack Connect)

**Why this works**: Network effects compound. The more SKUs a customer uses, the more receipt chains they have. Receipt chains = compliance moat. Compliance moat = lock-in.

---

## THE 5-STAGE LADDER

### Stage 1: FREE INDIVIDUAL SKU (Self-Host)
**Who**: Developers, startups, side projects
**What they get**: Full-featured tool, unlimited usage, community support
**What they pay**: $0
**Activation trigger**: Deploy in <60 seconds
**Time at this stage**: 1-4 weeks

#### Example Customer Journey:
> Sarah is a backend engineer at a seed-stage startup. She needs a workflow engine but Temporal wants $30K/year. She runs:
> ```bash
> curl -sSL https://get.80-20.cloud/workflow | sh
> ```
> In 60 seconds, she has a workflow engine running locally. She ships her first retry-enabled job that afternoon.

#### Success Metric: **First Receipt Generated**
- User sees a workflow execution receipt
- Realizes: "I can prove this job ran"
- Seeds the idea: "What else could I prove?"

#### Friction Points to Reduce:
- Install must be one command (curl | sh)
- First workflow must succeed in <5 minutes
- Docs must answer "how do I see my receipts?" immediately

#### Natural Upsell Trigger → Stage 2:
When user thinks: "This works great locally. But when my laptop sleeps, jobs fail. I need always-on infrastructure."

---

### Stage 2: PAID INDIVIDUAL SKU (Cloud)
**Who**: Small teams, production workloads, "it just needs to work"
**What they get**: Managed hosting, 99.9% SLA, email support, receipts in S3
**What they pay**: $99-$2,999/mo (per SKU, usage-based)
**Activation trigger**: Credit card, 14-day free trial
**Time at this stage**: 2-6 months

#### Example Customer Journey:
> Sarah's workflow engine is now critical infrastructure. When her laptop died last week, 47 jobs got stuck. She upgrades to cloud for $99/mo. Jobs now run 24/7. She gets receipts in her S3 bucket. Email support answers her questions in <24 hours.

#### Success Metric: **First Month Retained**
- User pays for Month 2 (survived trial churn)
- Jobs running reliably (no support tickets about downtime)
- Receipts accumulating in S3 (data gravity)

#### Friction Points to Reduce:
- Trial must not require credit card (conversion happens at end)
- Migration from self-host must be one command
- S3 bucket setup must be automated (CloudFormation template)

#### Natural Upsell Trigger → Stage 3:
When user thinks: "I'm using Workflow Engine. But I also need Observability. And Feature Flags. Can I get a discount for bundling?"

---

### Stage 3: PAID BUNDLE (3-5 SKUs)
**Who**: Growing teams, multi-product orgs, compliance-aware companies
**What they get**: 3-5 SKUs with receipt interop, unified dashboard, bundle discount
**What they pay**: $500-$5,000/mo (15-20% discount vs. individual SKUs)
**Activation trigger**: Add 2nd SKU, see bundle offer
**Time at this stage**: 6-18 months

#### Example Customer Journey:
> Sarah's team is now using:
> 1. Workflow Engine ($99/mo)
> 2. Observability ($99/mo for 11 hosts)
> 3. Feature Flags ($249/mo)
>
> Total: $447/mo. We offer "Starter Bundle" for $379/mo (15% off).
>
> But the real value isn't the discount. It's **receipt interop**:
> - Workflow receipts link to observability traces
> - Feature flag decisions link to workflow executions
> - She can now answer: "Why did this workflow fail?" → "Because flag X was off, see receipt #1234"

#### Success Metric: **First Receipt Chain Used**
- User queries across SKUs (e.g., "show me all workflows that failed when feature flag X was on")
- Realizes: "These tools talk to each other through receipts"
- Lock-in begins (can't leave without losing receipt interop)

#### Friction Points to Reduce:
- Bundle offer must appear automatically (when user adds 2nd SKU)
- Receipt viewer must show cross-SKU queries (make interop obvious)
- Discount must feel significant (15-20%, not 5%)

#### Natural Upsell Trigger → Stage 4:
When user thinks: "We're going through SOC 2 audit. Can these receipts prove we have logs, access control, and change management?"

---

### Stage 4: ENTERPRISE BUNDLE (5+ SKUs + EHDIT)
**Who**: Series A+, regulated industries, SOC 2 / ISO 27001 orgs
**What they get**: 5+ SKUs, EHDIT (compliance receipt engine), custom profiles, Slack support
**What they pay**: $10K-$30K/mo (volume discount + compliance premium)
**Activation trigger**: SOC 2 audit started, or sales conversation
**Time at this stage**: 12-36 months

#### Example Customer Journey:
> Sarah's startup is now Series A. Investors require SOC 2 Type 2. She's using 7 SKUs:
> 1. Workflow Engine
> 2. Observability
> 3. Feature Flags
> 4. Access Control
> 5. Compliance Docs
> 6. Vulnerability Scanner
> 7. Incident Response
>
> She upgrades to "Enterprise Bundle" ($14,999/mo) and gets:
> - **EHDIT**: Evidence-Handling Distribution + Intentional Transparency
>   - Every receipt now includes audit metadata (who, what, when, why)
>   - Receipts map to SOC 2 controls (CC6.1, CC7.2, etc.)
>   - Compliance Docs SKU auto-generates SOC 2 report from receipts
> - **Custom Profile**: She can define org-specific policies (e.g., "all prod deploys must have 2 approvers")
> - **Slack Support**: Dedicated channel, <2 hour response time
>
> Her auditor says: "This is the most complete evidence package I've ever seen. Every control has cryptographic proof."
>
> SOC 2 audit that would have taken 6 months takes 6 weeks.

#### Success Metric: **First Compliance Report Generated**
- User exports SOC 2 Type 2 report generated entirely from receipts
- Auditor accepts it (no additional evidence requests)
- User realizes: "We can't leave. Our compliance depends on these receipts."

#### Friction Points to Reduce:
- EHDIT must be one-click upgrade (not custom implementation)
- Compliance Docs SKU must auto-detect which SKUs are in use
- Report generation must be <10 minutes (not days of manual work)

#### Natural Upsell Trigger → Stage 5:
When user thinks: "We're bidding on a government contract. They require FedRAMP. Can we get authorized?"

---

### Stage 5: GOVERNMENT PROFILE (FedRAMP + Air-Gap)
**Who**: Gov contractors, defense, healthcare (HIPAA), finance (PCI)
**What they get**: FedRAMP-authorized deployment, air-gap option, NIST 800-53 controls, dedicated support
**What they pay**: $30K-$100K/mo (compliance tax + infrastructure premium)
**Activation trigger**: RFP requirement, or sales conversation
**Time at this stage**: Multi-year contracts

#### Example Customer Journey:
> Sarah's company won a $5M contract with the Department of Defense. Requirements:
> - FedRAMP Moderate authorization
> - Air-gapped deployment (no internet access)
> - NIST 800-53 controls (200+ controls)
> - Continuous monitoring
>
> She upgrades to "Government Profile" ($49,999/mo):
> - **FedRAMP Deployment**: We run the infrastructure in FedRAMP-authorized AWS GovCloud
> - **Air-Gap Option**: Offline deployment with receipt chain verification
> - **NIST 800-53 Mapping**: Every receipt maps to specific controls (AC-2, AU-3, etc.)
> - **Continuous Monitoring**: Automated evidence collection for 3PAO (third-party assessor)
> - **Dedicated CSM**: Weekly check-ins, direct escalation path
>
> The contract is worth $5M. Our tool costs $600K/year. But it's the only way to prove compliance.

#### Success Metric: **Government ATO (Authority to Operate)**
- Customer gets ATO from government agency
- Receipts cited as evidence in security package
- Contract renewal dependent on our tools

#### Friction Points to Reduce:
- FedRAMP package must be pre-built (not custom per customer)
- Air-gap deployment must be documented (step-by-step runbook)
- 3PAO must accept our receipts (pre-validate with common assessors)

#### Natural Upsell Trigger → Expansion:
When user thinks: "This works for DoD. We should use it for our other gov contracts (DoE, DoJ, State, etc.)"

---

## UPSELL MECHANICS

### How Customers Move Up the Ladder

#### Stage 1 → Stage 2: Infrastructure Reliability
**Trigger**: Self-hosted job fails due to laptop sleep / network issue
**Offer**: "Upgrade to cloud for 99.9% SLA. First 14 days free."
**CTA**: [Upgrade to Cloud]
**Conversion rate**: 12-18% (industry standard for freemium → paid)

#### Stage 2 → Stage 3: Receipt Interop
**Trigger**: User adds 2nd SKU
**Offer**: "Bundle and save 15%. Plus: query across SKUs with receipt chains."
**CTA**: [See Bundle Pricing]
**Conversion rate**: 30-40% (high, because discount + network effects)

#### Stage 3 → Stage 4: Compliance Deadline
**Trigger**: User mentions "SOC 2" or "ISO 27001" in support ticket
**Offer**: "Generate SOC 2 report from your receipts. EHDIT adds audit metadata."
**CTA**: [Talk to Sales] (hand-hold at this tier)
**Conversion rate**: 50-60% (if SOC 2 is requirement, they must buy)

#### Stage 4 → Stage 5: Government Contract
**Trigger**: User asks about "FedRAMP" or "air-gap" or "NIST 800-53"
**Offer**: "FedRAMP-authorized deployment. Pre-mapped to NIST controls."
**CTA**: [Request Gov Profile Demo]
**Conversion rate**: 80-90% (if FedRAMP is requirement, no alternatives)

---

## PRICING SUMMARY TABLE

| Stage | Infrastructure | SKUs | Compliance | Support | Price/Month | Annual |
|-------|---------------|------|-----------|---------|-------------|--------|
| **Free** | Self-host | 1 | Basic receipts | Docs, Discord | $0 | $0 |
| **Paid** | Cloud (SLA) | 1 | Basic receipts | Email (24h) | $99-$2,999 | $1K-$36K |
| **Bundle** | Cloud (SLA) | 3-5 | Receipt interop | Email (12h) | $500-$5,000 | $6K-$60K |
| **Enterprise** | Cloud (SLA) | 5+ | EHDIT + custom | Slack (2h) | $10K-$30K | $120K-$360K |
| **Government** | FedRAMP / Air-gap | Unlimited | NIST 800-53 | Dedicated CSM | $30K-$100K | $360K-$1.2M |

---

## REVENUE MODELING

### Cohort Analysis (100 Free Users)

#### Month 1:
- 100 free users (Stage 1)
- 12 upgrade to paid (Stage 2) @ $99/mo = $1,188/mo
- **MRR**: $1,188

#### Month 6:
- Original cohort:
  - 40 still free (churn + stayed free)
  - 30 paid individual SKU (Stage 2) @ $200/mo avg = $6,000/mo
  - 5 bundle (Stage 3) @ $1,500/mo avg = $7,500/mo
  - **MRR from cohort**: $13,500

#### Month 12:
- Original cohort:
  - 25 still free
  - 20 paid individual SKU @ $300/mo avg = $6,000/mo
  - 10 bundle @ $2,000/mo avg = $20,000/mo
  - 2 enterprise (Stage 4) @ $15,000/mo = $30,000/mo
  - **MRR from cohort**: $56,000

#### Month 24:
- Original cohort:
  - 15 still free
  - 10 paid individual SKU @ $400/mo avg = $4,000/mo
  - 8 bundle @ $3,000/mo avg = $24,000/mo
  - 4 enterprise @ $18,000/mo avg = $72,000/mo
  - 1 government (Stage 5) @ $50,000/mo = $50,000/mo
  - **MRR from cohort**: $150,000

**Key insight**: A single cohort of 100 free users → $1.8M ARR in 24 months (if retention holds).

---

## RETENTION ANALYSIS

### Why Each Stage Has High Retention

#### Stage 1 (Free): 40% retention at 12 months
- No payment friction (it's free)
- Self-host = no vendor dependency
- Churn is mostly "tried it, didn't need it"

#### Stage 2 (Paid): 70% retention at 12 months
- Paying customers are serious users
- Cloud SLA = reliability they depend on
- Receipts in S3 = data gravity

#### Stage 3 (Bundle): 85% retention at 12 months
- Receipt interop = lock-in (can't leave without losing cross-SKU queries)
- Bundle discount = switching cost (would pay more elsewhere)
- Multiple SKUs = organizational adoption (harder to rip out)

#### Stage 4 (Enterprise): 95% retention at 12 months
- EHDIT receipts = compliance foundation
- SOC 2 report depends on these receipts
- Switching = re-audit (6 months, $50K cost)

#### Stage 5 (Government): 98% retention at 12 months
- FedRAMP ATO depends on our infrastructure
- Contract renewal tied to our tools
- Air-gap deployment = no cloud alternatives

**Formula**: Retention = Network Effects × Switching Cost × Proof Dependency

---

## BUNDLE DEFINITIONS

### Starter Bundle ($379/mo, was $447)
**SKUs**:
1. Workflow Engine
2. Observability
3. Feature Flags

**Who buys**: Small dev teams (5-15 engineers)
**Value prop**: Core operational tools with receipt interop
**Upsell path**: Add Compliance Docs when SOC 2 comes up

---

### Compliance Bundle ($1,999/mo, was $2,496)
**SKUs**:
1. Workflow Engine
2. Observability
3. Access Control
4. Compliance Docs
5. Vulnerability Scanner

**Who buys**: Series A+ startups going through first SOC 2
**Value prop**: Generate SOC 2 Type 2 report from receipts
**Upsell path**: EHDIT when they need ISO 27001 or custom controls

---

### Enterprise Bundle ($14,999/mo, custom SKU mix)
**SKUs**: 5-10 SKUs (customer chooses)
**Includes**: EHDIT, custom profiles, Slack support
**Who buys**: Mid-market, regulated industries
**Value prop**: Compliance-as-code with audit-ready receipts
**Upsell path**: Government Profile when FedRAMP required

---

### Government Bundle ($49,999/mo, FedRAMP)
**SKUs**: Unlimited
**Includes**: FedRAMP deployment, NIST 800-53 mapping, air-gap option, dedicated CSM
**Who buys**: Gov contractors, defense, critical infrastructure
**Value prop**: ATO in 6 months vs. 18 months
**Upsell path**: Additional air-gap environments (dev, staging, prod)

---

## IN-APP UPSELL TRIGGERS

### Trigger 1: Usage Threshold
**When**: User exceeds free tier limits (e.g., 1K workflows/mo on free plan)
**Banner**: "You've run 1,247 workflows this month. Upgrade to cloud for unlimited workflows and 99.9% SLA."
**CTA**: [Upgrade to Cloud - $99/mo]

### Trigger 2: Downtime Event
**When**: Self-hosted instance goes offline (laptop sleep, network issue)
**Email** (next day): "We noticed your workflow engine was offline for 4 hours yesterday. Cloud hosting guarantees 99.9% uptime. Try it free for 14 days."
**CTA**: [Start Free Trial]

### Trigger 3: Second SKU Added
**When**: User installs 2nd SKU
**Modal**: "You're now using Workflow Engine + Observability. Bundle and save 15%. Plus: query across SKUs with receipt chains."
**CTA**: [See Bundle Pricing]

### Trigger 4: Compliance Keyword Detected
**When**: User mentions "SOC 2", "ISO 27001", or "audit" in support ticket
**Auto-reply**: "We can help! Our Compliance Docs SKU generates SOC 2 reports from your receipts. Would you like to talk to our compliance team?"
**CTA**: [Book Compliance Demo]

### Trigger 5: Receipt Volume Milestone
**When**: User has generated 10,000+ receipts
**Email**: "Congrats! You've generated 10,000+ receipts. You're sitting on a compliance goldmine. Our EHDIT upgrade adds audit metadata to every receipt."
**CTA**: [Learn About EHDIT]

### Trigger 6: Government Domain Detected
**When**: User email is .gov or .mil
**Banner**: "We see you're a government user. We offer FedRAMP-authorized deployments. Want to learn more?"
**CTA**: [Request Gov Profile Info]

---

## SALES PLAYBOOK

### When to Involve Sales

#### Stages 1-2: No Sales (Self-Serve)
- Free tier: Docs, Discord, automated onboarding
- Paid tier: 14-day trial, credit card, automated billing

#### Stage 3: Optional Sales (Bundle)
- Offer bundle via in-app banner (self-serve)
- If user clicks "Talk to Sales", assign SDR for discovery call

#### Stage 4: Required Sales (Enterprise)
- EHDIT requires custom implementation planning
- Sales cycle: 1-3 months
- ACV: $120K-$360K
- Process: Discovery → Demo → POC → Contract

#### Stage 5: Required Sales (Government)
- FedRAMP requires compliance mapping
- Sales cycle: 3-6 months
- ACV: $360K-$1.2M
- Process: RFP response → Technical eval → Security review → ATO support → Contract

### Sales Compensation
- **Stages 1-2**: No commission (self-serve)
- **Stage 3**: 5% commission (mostly self-serve with sales assist)
- **Stage 4**: 10% commission (sales-driven)
- **Stage 5**: 15% commission (complex sales, long cycle)

---

## CUSTOMER SUCCESS TRIGGERS

### When to Assign CSM

#### Stages 1-3: No CSM
- Automated onboarding
- Email support
- Help docs, Discord community

#### Stage 4: Shared CSM (1:50 ratio)
- Assigned when customer upgrades to Enterprise
- Monthly check-ins
- Proactive monitoring of receipt volume, errors
- Upsell to additional SKUs

#### Stage 5: Dedicated CSM (1:5 ratio)
- Assigned when customer upgrades to Government
- Weekly check-ins
- ATO support (work with 3PAO)
- Renewal management (multi-year contracts)

### CSM Success Metrics
- **Net Revenue Retention**: >120% (upsell to more SKUs)
- **Receipt Growth Rate**: >30% MoM (indicates product adoption)
- **Support Ticket Velocity**: <24 hours (faster = happier customers)
- **Compliance Report Success**: 100% (all audits pass using our receipts)

---

## CHURN PREVENTION

### Early Warning Signals

#### Signal 1: Declining Receipt Volume
**What it means**: Customer is using the tool less
**Action**: CSM reaches out: "We noticed your workflow volume dropped 40% last month. Everything OK?"

#### Signal 2: No Cross-SKU Queries
**What it means**: Customer isn't using receipt interop (low lock-in)
**Action**: Send tutorial: "How to trace workflows across Observability and Feature Flags"

#### Signal 3: Support Ticket Spike
**What it means**: Customer is frustrated
**Action**: Escalate to engineering, assign dedicated support engineer

#### Signal 4: Payment Failure
**What it means**: Credit card expired or cancelled
**Action**: Email + SMS, offer to downgrade to self-host (retain relationship)

#### Signal 5: Contract Non-Renewal Notice
**What it means**: Customer is evaluating alternatives
**Action**: Executive call, retention offer (discount, custom features, extended support)

### Retention Offers by Stage

#### Stage 2 (Paid): Downgrade to Free
"We'd hate to lose you. Want to downgrade to self-host (free) instead of canceling entirely?"
**Win**: Retain relationship, potential future upsell

#### Stage 3 (Bundle): Remove SKUs
"Would you like to keep 2 SKUs instead of canceling all 5?"
**Win**: Retain revenue, keep receipt chain intact

#### Stage 4 (Enterprise): Discount + Feature Commitment
"We'll give you 20% off for 12 months + prioritize your feature requests"
**Win**: Retain high-value customer, get product feedback

#### Stage 5 (Government): Executive Escalation
"Our CEO will call your CTO to understand concerns and find a solution"
**Win**: Signal importance, retain contract

---

## COMPETITIVE WIN/LOSS ANALYSIS

### Why We Win Against Incumbents

#### At Stage 2 (Paid):
**We win because**: 5-10× cheaper, self-serve, no sales calls
**We lose because**: "We already have Temporal, not worth switching"
**Fix**: Offer free migration service (we'll move your workflows for you)

#### At Stage 3 (Bundle):
**We win because**: Receipt interop creates value incumbent bundles don't have
**We lose because**: "Too many tools to manage"
**Fix**: Unified dashboard showing all SKUs in one view

#### At Stage 4 (Enterprise):
**We win because**: Receipts = native compliance, Vanta = screenshot scraping
**We lose because**: "Our auditor doesn't accept your receipts"
**Fix**: Pre-validate with Big 4 accounting firms (PwC, Deloitte, EY, KPMG)

#### At Stage 5 (Government):
**We win because**: FedRAMP-authorized, NIST-mapped, 6-month ATO vs. 18 months
**We lose because**: "Not on approved vendor list"
**Fix**: Get on GSA Schedule (government purchasing contract)

---

## LONG-TERM LOCK-IN MECHANICS

### Why Customers Can't Leave

#### After 6 Months (Stage 3):
- **Receipt chains**: Workflows link to observability, feature flags, access logs
- **Switching cost**: Would need to rebuild receipt interop with new vendor
- **Data gravity**: 500K+ receipts in S3, used for internal debugging

#### After 12 Months (Stage 4):
- **Compliance dependency**: SOC 2 report generated from receipts
- **Auditor acceptance**: "Your auditor loves our receipts. New vendor = re-educate auditor."
- **Custom profiles**: Org-specific policies encoded in SHACL (not portable)

#### After 24 Months (Stage 5):
- **FedRAMP ATO**: Government authorization tied to our infrastructure
- **Contract lock-in**: Multi-year contracts with renewal clauses
- **Air-gap deployment**: Can't move to cloud vendor, we're the only air-gap option

**Formula**: Lock-in = Receipt Volume × SKU Count × Compliance Depth × Contract Duration

---

## EXPANSION REVENUE

### How Customers Grow Beyond Initial Purchase

#### Expansion Vector 1: More SKUs
- Started with Workflow Engine → added Observability, Feature Flags, Compliance Docs
- **ARR impact**: $99/mo → $1,999/mo (20× growth)

#### Expansion Vector 2: More Usage
- Started with 1K workflows/mo → now 50K workflows/mo
- **ARR impact**: $99/mo → $2,999/mo (30× growth)

#### Expansion Vector 3: More Environments
- Started with prod → added staging, dev, air-gap disaster recovery
- **ARR impact**: $14,999/mo → $59,996/mo (4× growth)

#### Expansion Vector 4: More Seats
- Started with 10 engineers → now 100 engineers (for Access Control, Observability)
- **ARR impact**: Tier upgrade from $999/mo → $4,999/mo (5× growth)

#### Expansion Vector 5: Professional Services
- Compliance consulting: $50K one-time (help customer get SOC 2 using our receipts)
- Migration services: $25K one-time (migrate from Temporal to our Workflow Engine)
- Custom ontology: $75K one-time (build customer-specific SHACL profiles)

**Target Net Revenue Retention**: 130-150% (healthy SaaS benchmark)

---

## SUMMARY: THE LADDER IN ONE PAGE

```
Stage 1: FREE INDIVIDUAL SKU (Self-Host)
├─ Who: Developers, startups, side projects
├─ Price: $0
├─ Success Metric: First receipt generated
└─ Upsell Trigger: "My laptop died, jobs failed" → Stage 2

Stage 2: PAID INDIVIDUAL SKU (Cloud)
├─ Who: Small teams, production workloads
├─ Price: $99-$2,999/mo
├─ Success Metric: First month retained
└─ Upsell Trigger: "I need 2nd SKU, any discount?" → Stage 3

Stage 3: PAID BUNDLE (3-5 SKUs)
├─ Who: Growing teams, multi-product orgs
├─ Price: $500-$5,000/mo
├─ Success Metric: First receipt chain query
└─ Upsell Trigger: "SOC 2 audit started" → Stage 4

Stage 4: ENTERPRISE BUNDLE (5+ SKUs + EHDIT)
├─ Who: Series A+, regulated industries
├─ Price: $10K-$30K/mo
├─ Success Metric: Compliance report generated
└─ Upsell Trigger: "We need FedRAMP" → Stage 5

Stage 5: GOVERNMENT PROFILE (FedRAMP + Air-Gap)
├─ Who: Gov contractors, defense, healthcare
├─ Price: $30K-$100K/mo
├─ Success Metric: Government ATO achieved
└─ Expansion: More environments, more contracts
```

**The ladder works because**: Each stage creates value that makes the next stage inevitable.

**Free receipts** → prove value
**Paid cloud** → prove reliability
**Bundle** → prove interop
**Enterprise** → prove compliance
**Government** → prove security

**By the time they reach Stage 4, they can't leave. The receipts ARE their compliance program.**
