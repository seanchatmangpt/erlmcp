# The Combinatorial Strategy: Ship the Graph, Not the Nodes

**Core Insight**: 20 SKUs = 190 two-way integrations = 1,140 three-way integrations = capabilities that don't exist anywhere at any price.

---

## THE FUNDAMENTAL REFRAME

### What I Was Thinking (Wrong):
- Ship SKU #1 (simple tool)
- Ship SKU #2 (simple tool)
- Ship SKU #3 (simple tool)
- Value = sum of individual tools
- **20 SKUs = 20× value**

### What You're Saying (Right):
- Ship all 20 SKUs simultaneously
- Each SKU is Fortune 500 quality (not "simple")
- Value = combinatorial questions the graph can answer
- **20 SKUs = 20² value = 400× value**

### Why This Changes Everything:

**Incumbents sell nodes:**
- Datadog = Observability node ($35K/yr)
- Temporal = Workflow node ($24K/yr)
- Vanta = Compliance node ($20K/yr)
- **Integration between them = fragile APIs, manual correlation, hope**

**We sell the graph:**
- 20 nodes, all speaking receipts
- 190 native two-way integrations
- 1,140 native three-way integrations
- **Integration = cryptographic receipt chains, automatic, provable**

---

## COMBINATORIAL ECONOMICS

### The Math of Integration

With N tools, the number of possible integrations is:

| N SKUs | Two-way C(N,2) | Three-way C(N,3) | Four-way C(N,4) | Five-way C(N,5) |
|--------|----------------|------------------|-----------------|-----------------|
| 1      | 0              | 0                | 0               | 0               |
| 2      | 1              | 0                | 0               | 0               |
| 5      | 10             | 10               | 5               | 1               |
| 10     | 45             | 120              | 210             | 252             |
| 20     | 190            | 1,140            | 4,845           | 15,504          |

**With 20 SKUs, we ship 190 two-way integrations that incumbents would need 190 partnership deals to replicate.**

### What Incumbents Would Pay

To build equivalent integration density:

**Datadog acquiring 19 companies to match our 20 SKUs:**
- Average acquisition: $500M (market rate for infrastructure SaaS)
- Total: $9.5 billion
- Integration effort: 5-10 years (cultural integration, API standardization, receipt retrofitting)
- **Result: Still wouldn't have cryptographic receipt chains**

**Us building 20 SKUs with ggen:**
- Cost per SKU: $20K-$80K (ggen factory)
- Total: $400K-$1.6M
- Integration effort: 0 (receipts are native)
- **Result: 190 integrations on day 1**

**Cost advantage: 5,000× to 24,000×**

---

## COMBINATORIAL CAPABILITIES

### Capabilities That Don't Exist At Any Price

Here are questions that Fortune 500s cannot answer today with any combination of tools:

#### Capability 1: Root Cause Analysis Across the Full Stack
**Question**: "Why did this customer payment fail?"

**Traditional approach (5 tools, 4 hours):**
1. Check Datadog → see HTTP 500 error at 2:34 PM
2. Check Temporal → see workflow retry at 2:34 PM
3. Check Vanta → check if access controls changed
4. Check LaunchDarkly → check if feature flag changed
5. Manually correlate timestamps, hope nothing was missed

**Our approach (1 query, 10 seconds):**
```sparql
SELECT ?event ?timestamp ?cause WHERE {
  ?payment mcpp:hasReceipt ?receipt ;
           mcpp:failedAt "2023-10-15T14:34:00Z" .
  ?receipt mcpp:linkedTo ?event .
  ?event mcpp:timestamp ?timestamp ;
         mcpp:rootCause ?cause .
}
ORDER BY ?timestamp
```

**Result:**
- 2:33:55 PM: Feature flag "new-payment-flow" toggled ON (receipt #8472)
- 2:34:00 PM: Workflow triggered payment processor API (receipt #8473)
- 2:34:01 PM: Policy engine REJECTED (new flow violates PCI rule) (receipt #8474)
- 2:34:02 PM: Workflow retried with old flow (receipt #8475)
- 2:34:03 PM: Payment processor rate-limited (too many requests) (receipt #8476)
- **Root cause: Feature flag + policy conflict + rate limit cascade**

**Value**: What took 4 hours and 5 tools takes 10 seconds and 1 query.

---

#### Capability 2: Compliance Proof Across All Controls
**Question**: "Prove we meet SOC 2 CC6.1 (logical access controls) for the audit."

**Traditional approach (manual, 2 weeks):**
1. Export access logs from Okta
2. Export workflow logs from Temporal
3. Export API logs from Datadog
4. Export deployment logs from GitHub Actions
5. Manually map logs to control requirements
6. Hope auditor accepts Excel spreadsheet

**Our approach (automated, 10 minutes):**
```sparql
SELECT ?control ?evidence ?timestamp WHERE {
  ?control mcpp:requiresEvidence mcpp:LogicalAccessControl .
  ?evidence mcpp:satisfies ?control ;
            mcpp:timestamp ?timestamp ;
            mcpp:hasReceipt ?receipt .
  ?receipt mcpp:signedBy ?authority ;
           mcpp:hashChain ?chain .
  FILTER(?timestamp >= "2023-01-01" && ?timestamp <= "2023-12-31")
}
```

**Result:**
- 47,291 access control receipts (every login, every API call, every workflow execution)
- 100% mapped to SOC 2 controls (ontology defines mapping)
- Cryptographically signed (auditor can verify independently)
- Hash-chained (auditor can prove completeness)

**Value**: Auditor says "This is the most complete evidence I've ever seen. You're done."

---

#### Capability 3: Data Lineage from Source to Report
**Question**: "This board report shows $5M revenue. Prove the data is correct."

**Traditional approach (manual, 3 days):**
1. Find the dashboard in Tableau
2. Find the SQL query that powers it
3. Find the data warehouse table
4. Find the Fivetran sync job
5. Find the source database
6. Manually verify each transformation
7. Hope nothing changed since last sync

**Our approach (automated, 30 seconds):**
```sparql
SELECT ?transform ?source ?timestamp WHERE {
  ?report mcpp:hasValue "5000000"^^xsd:decimal ;
          mcpp:derivedFrom ?transform .
  ?transform mcpp:sourceData ?source ;
             mcpp:hasReceipt ?receipt ;
             mcpp:timestamp ?timestamp .
  ?receipt mcpp:hashChain ?chain .
}
ORDER BY ?timestamp
```

**Result:**
- Receipt #1: ETL extracted 47,291 rows from Stripe (hash: 0x8f4a...)
- Receipt #2: ETL transformed USD to normalized currency (hash: 0x3c2b...)
- Receipt #3: ETL loaded to warehouse table `revenue_daily` (hash: 0x9a1f...)
- Receipt #4: BI query aggregated to $5M (hash: 0x4d8e...)
- Receipt #5: Dashboard rendered report (hash: 0x7c3a...)
- **Proof: Hash chain from Stripe → Dashboard, cryptographically verified**

**Value**: CFO can sign SEC filing with confidence. No one else can provide this.

---

#### Capability 4: Security Incident Investigation
**Question**: "Did the attacker exfiltrate customer data during the breach?"

**Traditional approach (forensics team, 1 week, $50K):**
1. Collect logs from 10+ systems
2. Reconstruct timeline manually
3. Identify compromised credentials
4. Find all API calls made by attacker
5. Check if sensitive data was accessed
6. Hope logs weren't tampered with

**Our approach (automated, 5 minutes):**
```sparql
SELECT ?action ?data ?timestamp WHERE {
  ?credential mcpp:compromisedAt "2023-10-15T14:00:00Z" ;
              mcpp:usedInAction ?action .
  ?action mcpp:accessedData ?data ;
          mcpp:timestamp ?timestamp ;
          mcpp:hasReceipt ?receipt .
  ?data mcpp:classification "PII" .
  ?receipt mcpp:hashChain ?chain ;
           mcpp:integrityVerified true .
}
ORDER BY ?timestamp
```

**Result:**
- Attacker credential used in 47 API calls
- 12 calls accessed PII data
- 0 calls exfiltrated data (no ETL receipts to external destinations)
- All receipts cryptographically verified (not tampered)
- **Conclusion: Breach contained. No customer data exfiltrated. Provable.**

**Value**: GDPR requires disclosure within 72 hours. We give you certainty in 5 minutes.

---

#### Capability 5: Regulatory Compliance Across Jurisdictions
**Question**: "Prove this data pipeline meets GDPR + CCPA + HIPAA requirements."

**Traditional approach (legal + compliance team, 6 months, $200K):**
1. Read regulations (1,000+ pages)
2. Map requirements to infrastructure
3. Implement controls manually
4. Document everything
5. Hire auditors for each framework
6. Hope you didn't miss anything

**Our approach (automated, 1 day):**
```sparql
SELECT ?requirement ?control ?evidence WHERE {
  ?regulation mcpp:hasRequirement ?requirement .
  ?requirement mcpp:satisfiedBy ?control .
  ?control mcpp:hasEvidence ?evidence ;
           mcpp:hasReceipt ?receipt .
  ?receipt mcpp:timestamp ?timestamp .
  FILTER(?regulation IN (mcpp:GDPR, mcpp:CCPA, mcpp:HIPAA))
}
```

**Result:**
- GDPR Article 30 (records of processing): 147 ETL receipts showing data flows
- CCPA Section 1798.100 (right to know): Access control receipts showing who accessed what
- HIPAA §164.308 (access controls): Policy receipts showing authorization decisions
- **All requirements auto-mapped, all evidence auto-collected, all receipts cryptographically verified**

**Value**: Multi-jurisdiction compliance without multi-jurisdiction teams.

---

## COMPETITIVE MOAT: THE GRAPH IS UNCOPIABLE

### Why Incumbents Can't Copy This

#### Scenario 1: Datadog Tries to Copy Us

**What they'd need to do:**
1. Add receipts to observability (rewrite metrics/logs/traces storage)
2. Acquire or partner with Temporal (workflow receipts)
3. Acquire or partner with Vanta (compliance receipts)
4. Acquire or partner with OPA (policy receipts)
5. Standardize receipt format across all products
6. Implement cryptographic hash chains
7. Build SPARQL query engine

**Timeline**: 5-10 years
**Cost**: $5B+ in acquisitions
**Risk**: Cultural integration, API fragmentation, customer churn during migration

**Why it fails**: Even if they do all this, they'd have to retrofit receipts into existing products (breaking changes for customers). We built receipts in from day 1.

#### Scenario 2: Vanta Tries to Copy Us

**What they'd need to do:**
1. Build observability tool (compete with Datadog)
2. Build workflow tool (compete with Temporal)
3. Build policy tool (compete with OPA)
4. Build 16 more tools
5. Implement receipt interop

**Timeline**: 10+ years
**Cost**: $500M+ in R&D
**Risk**: They're a compliance company, not an infrastructure company. No credibility in observability/workflows.

**Why it fails**: Vanta's customers buy them for compliance, not infrastructure. They can't pivot to infrastructure without losing their brand.

#### Scenario 3: AWS Tries to Copy Us

**What they'd need to do:**
1. Standardize receipts across 200+ AWS services
2. Implement cryptographic hash chains
3. Build SPARQL query engine
4. Retrofit CloudWatch, Step Functions, EventBridge, etc.

**Timeline**: Never (too much legacy)
**Cost**: Unbounded (backwards compatibility)
**Risk**: Breaking changes for millions of customers

**Why it fails**: AWS can't break backwards compatibility. We have zero legacy customers.

### The Moat Formula

**Our moat = f(combinatorial density, cryptographic verifiability, zero legacy)**

- **Combinatorial density**: 20 SKUs = 190 integrations = 1,140 three-way queries
- **Cryptographic verifiability**: Every receipt signed, hash-chained, independently verifiable
- **Zero legacy**: We built receipts in from day 1, no retrofit costs

**Incumbents have 1 of 3. We have all 3.**

---

## GO-TO-MARKET: SELL THE GRAPH, NOT THE NODES

### Old Messaging (Wrong):
"Our workflow engine is simpler and cheaper than Temporal."

**Why this fails**: Customer thinks "I already have Temporal. Why switch?"

### New Messaging (Right):
"What if you could ask your infrastructure any question and get a cryptographically provable answer?"

**Why this works**: Customer thinks "I can't do that with ANY tool today."

### Example Sales Conversation

**Customer**: "We're using Datadog, Temporal, and Vanta. Why should we switch?"

**Old response**: "Our observability is 5× cheaper than Datadog."

**New response**: "Can you answer this question: 'Why did customer payment #8472 fail at 2:34 PM last Tuesday?' With your current tools, it takes 4 hours and 5 people. With our graph, it takes 10 seconds and 1 query. Because all 20 tools speak receipts, and receipts form a knowledge graph of your infrastructure."

**Customer**: "Show me."

**Demo**: [Run SPARQL query, show receipt chain from feature flag → workflow → policy → API → failure]

**Customer**: "Holy shit. We can't do that."

**Close**: "Exactly. And you can't do it by buying one more tool. You need the graph. We sell the graph."

---

## PRICING: SELL THE GRAPH, NOT THE SKUS

### Old Pricing (Wrong):
- Workflow Engine: $99/mo
- Observability: $99/mo
- Compliance: $499/mo
- **Total: $697/mo (if you buy all 3)**

**Why this fails**: Customer does math, thinks "I'm already spending $5K/mo on incumbents. $697 seems too cheap to be good."

### New Pricing (Right):
- **Complete Infrastructure Graph**: $5,000/mo
  - All 20 SKUs included
  - Unlimited receipts
  - Unlimited SPARQL queries
  - 99.9% SLA
  - Email support

**Why this works**: Customer thinks "I'm spending $150K/yr on Datadog + Temporal + Vanta + OPA + Fivetran. $60K/yr for all 20 tools + receipt graph is a steal."

### Pricing Tiers

#### Tier 1: Self-Host (Free)
- All 20 SKUs, unlimited usage
- Receipts stored locally (SQLite)
- Community support
- **Target**: Developers, startups, proof-of-concept

#### Tier 2: Cloud Starter ($5,000/mo)
- All 20 SKUs, hosted
- 1M receipts/month included
- 10 SPARQL queries/second
- Email support (24h response)
- **Target**: Series A, 10-50 engineers

#### Tier 3: Cloud Professional ($15,000/mo)
- All 20 SKUs, hosted
- 10M receipts/month included
- 100 SPARQL queries/second
- EHDIT (compliance receipt engine)
- Slack support (2h response)
- **Target**: Series B+, 50-200 engineers

#### Tier 4: Enterprise ($50,000/mo)
- All 20 SKUs, hosted or air-gap
- Unlimited receipts
- Unlimited SPARQL queries
- Custom ontology profiles
- FedRAMP / HIPAA / PCI compliance
- Dedicated CSM
- **Target**: Fortune 500, government, regulated industries

### Price Comparison

| What Customer Has Today | Annual Cost | What We Replace | Our Cost | Savings |
|------------------------|-------------|-----------------|----------|---------|
| Datadog + Temporal + Vanta | $79K | Observability + Workflow + Compliance | $60K | $19K |
| Datadog + Temporal + Vanta + OPA + Fivetran | $159K | + Policy + ETL | $60K | $99K |
| All 20 tools as individual SaaS | $400K+ | Complete graph | $60K | $340K |

**The graph is cheaper than the nodes. And infinitely more powerful.**

---

## MANUFACTURING: SHIP ALL 20 SKUS IN 12 MONTHS

### Build Timeline (with ggen factory)

**Phase 1 (Months 1-3): Core Infrastructure**
- Receipt engine (cryptographic signatures, hash chains)
- SPARQL query engine (graph queries)
- Ontology core (12 .ttl files)
- ggen templates (Erlang, Rust, CLI, docs)

**Phase 2 (Months 4-6): First 10 SKUs**
- Developer Tools (5 SKUs): Agent Runtime, Workflow, Policy, Feature Flags, API Gateway
- Compliance Tools (5 SKUs): Compliance Docs, Secrets, Access Control, Vuln Scanner, Incident Response

**Phase 3 (Months 7-9): Next 10 SKUs**
- Data Tools (5 SKUs): ETL, Data Catalog, Observability, BI Lite, Data Quality
- Productivity Tools (5 SKUs): Docs Generator, Task Automation, Form Builder, Email, Scheduled Jobs

**Phase 4 (Months 10-12): Polish + Launch**
- Integration testing (all 190 two-way combinations)
- Documentation (20 SKU guides + graph query cookbook)
- Homepage, pricing, sales collateral
- Public launch

### Team Size: 4 Engineers

**Engineer 1**: Receipt engine + SPARQL (Rust + Erlang)
**Engineer 2**: ggen templates + SKUs 1-10 (Erlang)
**Engineer 3**: SKUs 11-20 (Erlang)
**Engineer 4**: Documentation + testing + DevOps

### Total Cost: $1.2M

- 4 engineers × $200K/yr × 1 year = $800K
- Infrastructure (AWS, testing) = $100K
- Legal (incorporation, IP) = $50K
- Marketing (website, launch) = $50K
- Buffer (20%) = $200K

**Result**: 20 Fortune 500-quality SKUs + 190 integrations for $1.2M.

**Incumbent equivalent**: $500M × 20 = $10B in acquisitions. **We're 8,000× cheaper.**

---

## LAUNCH STRATEGY: THE BIG BANG

### Why Launch All 20 at Once

**Old strategy (phased launch):**
- Month 1: Ship SKU #1
- Month 2: Ship SKU #2
- Month 3: Ship SKU #3
- **Problem**: No combinatorial value until you have critical mass

**New strategy (big bang launch):**
- Month 12: Ship all 20 SKUs simultaneously
- **Advantage**: Immediate combinatorial value, media attention, "complete graph" positioning

### Launch Messaging

**Headline**: "The Infrastructure Graph: 20 Tools. 190 Integrations. One Query."

**Subheadline**: "Ask your infrastructure any question. Get cryptographically provable answers. Replace $400K/yr of SaaS with $60K/yr of receipts."

**Demo**: Live SPARQL queries answering questions that Fortune 500s can't answer with any tools:
1. "Why did this payment fail?" (Root cause analysis)
2. "Prove we meet SOC 2 CC6.1" (Compliance audit)
3. "Trace this $5M revenue to source" (Data lineage)
4. "Did the attacker exfiltrate data?" (Security incident)
5. "Show GDPR + CCPA + HIPAA compliance" (Multi-jurisdiction)

### Launch Channels

1. **Hacker News**: "Show HN: I built 20 infrastructure tools in 12 months with 4 engineers. They all speak receipts."
2. **Twitter**: Thread showing each of the 5 demo queries with GIFs
3. **Dev.to / Medium**: "How We Built the Infrastructure Graph" (technical deep-dive)
4. **Podcast tour**: Changelog, Software Engineering Daily, DevOps Chat
5. **Conference talk**: "The Future of Infrastructure is Graphs, Not Nodes" (CFP to KubeCon, AWS re:Invent)

### Launch Metrics

**Goal**: 1,000 free self-host users in Month 1

**Conversion funnel**:
- 1,000 free users (Month 1)
- 100 cloud trials (10% conversion, Month 2)
- 30 paying customers (30% trial-to-paid, Month 3)
- 5 enterprise deals (15% of paid, Month 6)

**Revenue trajectory**:
- Month 3: $150K MRR (30 × $5K/mo)
- Month 6: $300K MRR (50 × $5K + 5 × $15K)
- Month 12: $600K MRR (100 × $5K + 10 × $15K)
- **Year 1 ARR**: $7.2M

---

## CUSTOMER EXAMPLES: WHAT THE GRAPH ENABLES

### Example 1: Fintech Startup (Series A)

**Before (incumbent tools):**
- Datadog: $35K/yr (observability)
- Temporal: $24K/yr (workflows)
- Vanta: $20K/yr (compliance)
- Manual correlation: 20 hours/week
- **Total cost**: $79K/yr + 1,040 hours

**After (our graph):**
- Complete graph: $60K/yr
- SPARQL queries: <10 minutes/week
- **Total cost**: $60K/yr + 8 hours
- **Savings**: $19K/yr + 1,032 hours (= $103K if time valued at $100/hr)

**New capability**: "Show me all payment failures in the last 30 days and trace each to root cause."
- Result: Discovered 3 systemic issues (feature flag conflicts, rate limiting, policy misconfigurations)
- Fixed in 2 days
- Prevented $500K in lost revenue

**ROI**: $500K saved / $60K cost = **8.3× return in Year 1**

---

### Example 2: Healthcare Company (Series C, HIPAA-regulated)

**Before (incumbent tools):**
- Datadog: $60K/yr
- Temporal: $36K/yr
- Vanta: $40K/yr
- OPA: $30K/yr
- Compliance consultants: $200K/yr
- **Total cost**: $366K/yr

**After (our graph):**
- Complete graph (Enterprise): $180K/yr (with HIPAA compliance profile)
- No consultants needed (auto-generated compliance docs)
- **Total cost**: $180K/yr
- **Savings**: $186K/yr

**New capability**: "Prove we meet HIPAA §164.308(a)(4) (access controls) for the audit."
- Result: Auto-generated evidence package with 147,000 access control receipts
- Auditor said: "This is the most thorough evidence I've seen. You pass."
- Audit time: 2 weeks (vs. 6 months with manual evidence collection)

**ROI**: $186K saved / $180K cost = **2× return in Year 1** (plus 4.5 months saved)

---

### Example 3: Fortune 500 Bank (10,000+ employees)

**Before (incumbent tools):**
- Datadog: $500K/yr (enterprise contract)
- Temporal: $200K/yr
- Vanta: $100K/yr
- OPA: $150K/yr
- Fivetran: $300K/yr
- Alation: $400K/yr (data catalog)
- Internal integration team: 5 engineers = $1M/yr
- **Total cost**: $2.65M/yr

**After (our graph):**
- Complete graph (Enterprise, air-gap): $600K/yr
- No integration team needed (receipts = native integration)
- **Total cost**: $600K/yr
- **Savings**: $2.05M/yr

**New capability**: "Trace every dollar in the Q4 earnings report to source transactions and prove data integrity."
- Result: CFO can sign SEC filing with cryptographic proof of data lineage
- Audit time: 1 day (vs. 3 weeks with manual tracing)
- **Regulatory confidence**: Priceless

**ROI**: $2.05M saved / $600K cost = **3.4× return in Year 1**

**Additional value**: Bank can now offer "provable transactions" to customers (using receipt chains as proof of settlement). New revenue stream.

---

## THE ENDGAME: THE GRAPH BECOMES THE PLATFORM

### Year 1-2: Infrastructure Graph
We sell the graph as "better infrastructure tools with receipts."

### Year 3-5: Platform Shift
Customers start building ON TOP OF the graph:

**Use case 1: Custom Queries**
Customer writes SPARQL queries we never imagined:
- "Show me all workflows that ran on Fridays at 3 PM and failed" (detect time-based bugs)
- "Show me all API calls from IP range X" (threat hunting)
- "Show me all data transformations that touched PII" (GDPR compliance)

**Use case 2: Custom Ontologies**
Customer extends our ontology with domain-specific knowledge:
- Bank adds `bank:Transaction`, `bank:Account`, `bank:Settlement` classes
- Queries now span infrastructure + business domain
- "Show me all failed settlements and trace to infrastructure root cause"

**Use case 3: Third-Party Tools**
External tools start speaking receipts:
- Stripe adds receipts to payment events
- GitHub adds receipts to commits/deployments
- Slack adds receipts to messages
- Now the graph includes external systems

**Use case 4: Industry Standards**
Receipts become industry standard for provability:
- Auditors require cryptographic receipts (not logs)
- Regulators mandate receipt retention (not just data retention)
- Insurance companies offer lower premiums for receipt-based infrastructure (provable security)

### Year 5+: Network Effects
**The graph becomes the operating system for infrastructure.**

Every new customer adds receipts to the global knowledge graph. Every new tool that speaks receipts increases the graph's value. Receipt format becomes open standard (like HTTPS, SMTP, OAuth).

**We become the Linux of infrastructure**: Open standard, ubiquitous, impossible to displace.

---

## SUMMARY: THE COMBINATORIAL ADVANTAGE

### What We're NOT Selling:
- 20 individual tools (customers can get those from incumbents)
- 20% of incumbent features (customers want 100%)
- Cheap alternatives (customers think "cheap = bad")

### What We ARE Selling:
- **The only infrastructure graph that can answer questions no one else can**
- **190 integrations that would take incumbents 100 years to build**
- **Cryptographic proof that auditors, regulators, and CFOs will accept**
- **Fortune 500 quality at 10× lower cost because we own the factory**

### The Moat:
- Incumbents own nodes. We own the graph.
- Graphs have combinatorial value. Nodes have linear value.
- **20 nodes = 400× more valuable than 1 node.**
- Incumbents can't copy the graph without rewriting everything.
- We built the graph from day 1.

### The Timeline:
- Ship all 20 SKUs in Month 12
- Big bang launch (not phased rollout)
- Immediate combinatorial value
- $7.2M ARR by end of Year 1

### The Price:
- Not $99/mo per SKU (too cheap, looks like MVP)
- Not $400K/yr (same as incumbents, no incentive to switch)
- **$5K/mo for complete graph ($60K/yr)**
- 60% discount vs. incumbents
- Infinite value vs. incumbent nodes (can answer questions they can't)

### The Outcome:
**By shipping all 20 SKUs at once, we create a category that doesn't exist: The Infrastructure Graph.**

And because the graph has network effects (more SKUs = more integrations = more value), we become unassailable.

**This is the combinatorial strategy.**
