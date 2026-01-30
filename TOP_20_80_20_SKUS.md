# Top 20 SKUs: 80/20 Cloud & SaaS Clones

**Strategic Principle**: Incumbents sell 100% capability at premium prices. Customers use 20%. We sell the 20% they actually use at 5-10× lower cost, built from the same MCP+ factory.

**Manufacturing Advantage**: One ggen factory → 20 SKUs → near-zero marginal cost per SKU.

---

## CATEGORY 1: Developer Infrastructure (6 SKUs)

### SKU #1: Simple Agent Runtime
**Incumbent**: LangChain Cloud ($15K/yr), LangSmith ($5K/yr)
**They Sell**: Multi-LLM routing, complex chains, 50+ integrations, evaluation suite
**Customers Use**: Single LLM, 3-5 tools, basic logging
**Our 80/20**: MCP-native agent runtime with stdio/SSE transports, 5 built-in tools, structured logging
- **Pricing**: Free (self-host) → $49/mo (cloud, 10K msgs) → $499/mo (100K msgs) → $999/mo (unlimited)
- **Build Effort**: 60-80 hours (already 70% done in erlmcp)
- **Moat**: Receipts = audit trail. LangChain has no proof of execution.

### SKU #2: Policy-as-Code Engine
**Incumbent**: Open Policy Agent Enterprise ($30K/yr), Styra DAS ($50K/yr)
**They Sell**: Rego language, distributed deployment, IDE, compliance packs
**Customers Use**: 10-20 simple policies, webhook validation, basic audit
**Our 80/20**: SHACL-based policy validation with MCP tool integration
- **Pricing**: Free (self-host) → $99/mo (100 policies) → $999/mo (1K policies) → $1,999/mo (unlimited)
- **Build Effort**: 92-128 hours
- **Moat**: Policies are ontology. OPA policies are strings. We generate proofs.

### SKU #3: Evidence-First ETL
**Incumbent**: Fivetran ($1-5 per million rows), Airbyte Cloud ($2.50/credit)
**They Sell**: 300+ connectors, incremental sync, transformation layer
**Customers Use**: 3-5 connectors, full refresh, no transformations
**Our 80/20**: MCP-to-MCP data movement with receipt chains (provenance audit)
- **Pricing**: Free (self-host) → $199/mo (10M rows) → $2,499/mo (100M rows) → $4,999/mo (1B rows)
- **Build Effort**: 212-310 hours
- **Moat**: Every row has a receipt. Fivetran = trust us. We = verify everything.

### SKU #4: Workflow Engine
**Incumbent**: Temporal Cloud ($2K/mo+), AWS Step Functions ($0.025/transition)
**They Sell**: Complex sagas, versioning, distributed transactions, replay
**Customers Use**: Linear workflows, retry logic, basic state machine
**Our 80/20**: TCPS Kanban-based workflow with stage receipts
- **Pricing**: Free (self-host) → $99/mo (1K workflows) → $1,499/mo (50K workflows) → $2,999/mo (unlimited)
- **Build Effort**: 88-124 hours
- **Moat**: Workflows = production receipts. Temporal = black box state.

### SKU #5: Feature Flag Service
**Incumbent**: LaunchDarkly ($99-$1,500/mo), Split.io ($50-$2K/mo)
**They Sell**: Targeting, A/B testing, gradual rollouts, analytics
**Customers Use**: Boolean flags, percentage rollouts, basic targeting
**Our 80/20**: MCP tool that evaluates flags from ontology with decision receipts
- **Pricing**: Free (self-host) → $29/mo (25 flags) → $249/mo (500 flags) → $999/mo (unlimited)
- **Build Effort**: 56-76 hours
- **Moat**: Flag decisions = receipts. LaunchDarkly = telemetry only.

### SKU #6: API Gateway
**Incumbent**: Kong Enterprise ($30K/yr), Apigee ($3K-$30K/mo)
**They Sell**: Rate limiting, authentication, analytics, monetization, dev portal
**Customers Use**: JWT auth, basic rate limits, request logging
**Our 80/20**: MCP transport with OAuth2, rate limiting, request/response receipts
- **Pricing**: Free (self-host) → $99/mo (1M requests) → $999/mo (50M requests) → $4,999/mo (unlimited)
- **Build Effort**: 148-208 hours
- **Moat**: Every API call = receipt. Kong = logs you parse.

---

## CATEGORY 2: Compliance & Security (5 SKUs)

### SKU #7: Compliance Docs Generator
**Incumbent**: Vanta ($8K-$40K/yr), Drata ($12K-$60K/yr)
**They Sell**: SOC 2, ISO 27001, automated evidence collection, vendor risk
**Customers Use**: SOC 2 Type 2, policy templates, manual evidence upload
**Our 80/20**: Generate SOC 2 docs from TCPS receipts with SPARQL queries
- **Pricing**: Free (templates) → $499/mo (SOC 2 docs) → $1,999/mo (SOC 2 + ISO) → $3,999/mo (all frameworks)
- **Build Effort**: 116-164 hours
- **Moat**: Receipts = native evidence. Vanta = screenshot scraping.

### SKU #8: Secrets Manager
**Incumbent**: HashiCorp Vault ($0.03/hr = $262/mo), AWS Secrets Manager ($0.40/secret/mo)
**They Sell**: Dynamic secrets, encryption as a service, leasing, PKI
**Customers Use**: Static secrets, basic rotation, read/write API
**Our 80/20**: MCP tool for secret storage with access receipts and ML-DSA encryption
- **Pricing**: Free (self-host) → $19/mo (50 secrets) → $99/mo (500 secrets) → $499/mo (unlimited)
- **Build Effort**: 72-96 hours
- **Moat**: Secret access = receipt. Vault = audit log you query.

### SKU #9: Access Control as Data
**Incumbent**: Okta Workforce ($5-$15/user/mo), Auth0 ($23-$240/mo base)
**They Sell**: SSO, MFA, user provisioning, lifecycle management, adaptive auth
**Customers Use**: SAML/OIDC SSO, TOTP MFA, manual provisioning
**Our 80/20**: SHACL-based RBAC with MCP auth tool and login receipts
- **Pricing**: Free (100 users) → $2/user/mo (SSO + MFA) → $5/user/mo (+ provisioning) → $10/user/mo (+ adaptive)
- **Build Effort**: 168-236 hours
- **Moat**: Access decisions = ontology + receipts. Okta = proprietary graph.

### SKU #10: Vulnerability Scanner
**Incumbent**: Snyk ($98-$649/mo), Checkmarx ($50K-$200K/yr)
**They Sell**: SAST, SCA, container scanning, auto-fix, prioritization
**Customers Use**: Dependency scanning, basic SAST, CVE reports
**Our 80/20**: MCP tool that scans deps with receipt chain (scan → finding → fix)
- **Pricing**: Free (public repos) → $49/mo (5 private repos) → $299/mo (50 repos) → $999/mo (unlimited)
- **Build Effort**: 132-184 hours
- **Moat**: Scan results = receipts. Snyk = dashboard you can't prove.

### SKU #11: Incident Response Tracker
**Incumbent**: PagerDuty ($21-$51/user/mo), Opsgenie ($9-$29/user/mo)
**They Sell**: Alerting, on-call scheduling, escalations, postmortems, analytics
**Customers Use**: Alert routing, basic on-call, incident timeline
**Our 80/20**: TCPS Andon system with incident receipts and root cause ontology
- **Pricing**: Free (5 users) → $9/user/mo (unlimited incidents) → $19/user/mo (+ postmortems) → $39/user/mo (+ analytics)
- **Build Effort**: 96-132 hours
- **Moat**: Incidents = Andon events. PagerDuty = timeline you screenshot.

---

## CATEGORY 3: Data & Analytics (4 SKUs)

### SKU #12: Semantic Data Catalog
**Incumbent**: Alation ($100K-$500K/yr), Collibra ($150K-$1M/yr)
**They Sell**: Data discovery, lineage, governance, stewardship, business glossary
**Customers Use**: Table/column search, basic lineage, manual tagging
**Our 80/20**: RDF catalog with SPARQL search and receipt-based lineage
- **Pricing**: Free (self-host) → $499/mo (100 tables) → $2,499/mo (1K tables) → $9,999/mo (unlimited)
- **Build Effort**: 188-264 hours
- **Moat**: Lineage = receipt chains. Alation = metadata you enter.

### SKU #13: Observability Stack
**Incumbent**: Datadog ($15-$35/host/mo), New Relic ($49-$349/user/mo)
**They Sell**: Metrics, logs, traces, APM, profiling, dashboards, alerts
**Customers Use**: System metrics, error logs, basic dashboards
**Our 80/20**: TCPS Kanban metrics with receipt-based tracing
- **Pricing**: Free (self-host) → $9/host/mo (metrics + logs) → $29/host/mo (+ traces) → $99/host/mo (unlimited)
- **Build Effort**: 236-328 hours
- **Moat**: Traces = receipt chains. Datadog = correlated guesses.

### SKU #14: Business Intelligence Lite
**Incumbent**: Tableau ($70-$150/user/mo), Looker ($3K-$5K/mo base)
**They Sell**: Drag-drop viz, data modeling, governance, embedded analytics
**Customers Use**: 5-10 dashboards, basic charts, scheduled reports
**Our 80/20**: SPARQL queries → charts with receipt-stamped data snapshots
- **Pricing**: Free (5 dashboards) → $49/user/mo (unlimited dashboards) → $199/user/mo (+ embedded) → $999/mo (white-label)
- **Build Effort**: 164-228 hours
- **Moat**: Report data = receipt snapshot. Tableau = live query you can't verify.

### SKU #15: Data Quality Monitor
**Incumbent**: Monte Carlo ($50K-$200K/yr), Bigeye ($30K-$150K/yr)
**They Sell**: Anomaly detection, lineage, incident management, root cause
**Customers Use**: Freshness checks, null rate monitoring, basic alerts
**Our 80/20**: SHACL constraints as data quality rules with violation receipts
- **Pricing**: Free (self-host) → $299/mo (50 tables) → $1,499/mo (500 tables) → $4,999/mo (unlimited)
- **Build Effort**: 124-172 hours
- **Moat**: Quality checks = SHACL + receipts. Monte Carlo = ML black box.

---

## CATEGORY 4: Productivity & Ops (5 SKUs)

### SKU #16: Documentation Generator
**Incumbent**: ReadMe ($99-$799/mo), GitBook ($0-$125/user/mo)
**They Sell**: Interactive docs, API explorer, versioning, analytics, search
**Customers Use**: Markdown docs, basic search, manual updates
**Our 80/20**: Generate docs from ontology with ggen templates
- **Pricing**: Free (open source) → $29/mo (custom domain) → $199/mo (+ API explorer) → $999/mo (white-label)
- **Build Effort**: 68-92 hours
- **Moat**: Docs = generated from ontology. ReadMe = manually written.

### SKU #17: Task Automation (Zapier Clone)
**Incumbent**: Zapier ($29-$599/mo), Make ($9-$299/mo)
**They Sell**: 5K+ app integrations, multi-step workflows, filters, webhooks
**Customers Use**: 3-5 apps, linear workflows, basic triggers
**Our 80/20**: MCP tool chaining with receipt-based workflow execution
- **Pricing**: Free (100 tasks/mo) → $19/mo (1K tasks) → $99/mo (10K tasks) → $499/mo (unlimited)
- **Build Effort**: 144-200 hours
- **Moat**: Workflow executions = receipts. Zapier = run history you trust.

### SKU #18: Form Builder
**Incumbent**: Typeform ($29-$99/user/mo), JotForm ($34-$99/mo)
**They Sell**: Conditional logic, payment integration, HIPAA, analytics, A/B testing
**Customers Use**: Simple forms, basic validation, email notifications
**Our 80/20**: SHACL-based form validation with submission receipts
- **Pricing**: Free (3 forms, 100 submissions) → $19/mo (unlimited forms, 1K submissions) → $99/mo (10K submissions) → $299/mo (unlimited)
- **Build Effort**: 84-116 hours
- **Moat**: Form submissions = receipts. Typeform = stored responses you trust.

### SKU #19: Email Campaign Manager
**Incumbent**: Mailchimp ($13-$350/mo), SendGrid ($19.95-$89.95/mo)
**They Sell**: Automation, segmentation, A/B testing, analytics, CRM integration
**Customers Use**: Bulk email, basic templates, open/click tracking
**Our 80/20**: MCP email tool with send receipts and delivery verification
- **Pricing**: Free (500 contacts) → $19/mo (5K contacts) → $99/mo (50K contacts) → $499/mo (unlimited)
- **Build Effort**: 112-156 hours
- **Moat**: Email sends = receipts. Mailchimp = reporting you export.

### SKU #20: Scheduled Jobs Service
**Incumbent**: AWS EventBridge Scheduler ($1/million invocations), Cronitor ($7-$99/mo)
**They Sell**: Complex schedules, retries, monitoring, distributed execution
**Customers Use**: Cron expressions, basic retries, uptime monitoring
**Our 80/20**: TCPS Kanban scheduler with job execution receipts
- **Pricing**: Free (self-host) → $9/mo (100 jobs) → $49/mo (1K jobs) → $199/mo (unlimited)
- **Build Effort**: 76-104 hours
- **Moat**: Job runs = receipts. EventBridge = CloudWatch logs you query.

---

## PORTFOLIO ECONOMICS

### Year 1 (5 SKUs in market)
- **Assumption**: 10 customers per SKU, 60% free tier, 30% paid, 10% enterprise
- **Revenue per SKU**: ~$23K ARR
- **Total ARR**: $115K (5 SKUs × $23K)
- **Factory Cost**: $80K (one-time ggen investment)
- **Gross Margin**: Already positive by SKU #4

### Year 2 (15 SKUs in market)
- **Assumption**: 25 customers per SKU, same tier distribution
- **Revenue per SKU**: ~$58K ARR
- **Total ARR**: $870K (15 SKUs × $58K)
- **Bundle Revenue**: +$130K (20% of customers buy 3+ SKUs)
- **Total ARR**: $1M

### Year 3 (20 SKUs in market)
- **Assumption**: 40 customers per SKU, improving conversion (50/35/15)
- **Revenue per SKU**: ~$92K ARR
- **Total ARR**: $1.84M (20 SKUs × $92K)
- **Bundle Revenue**: +$550K (30% of customers buy 5+ SKUs)
- **Compliance Bundle**: +$200K (Gov profile upsells)
- **Total ARR**: $2.6M

### Manufacturing Advantage Over Time
| Year | Our Cost/SKU | Incumbent Cost/SKU | Cost Advantage | Time Advantage |
|------|--------------|-------------------|----------------|----------------|
| 1    | $16K         | $1.5M            | 94× cheaper    | 12× faster     |
| 2    | $8K          | $2M              | 250× cheaper   | 18× faster     |
| 3    | $5K          | $3M              | 600× cheaper   | 24× faster     |

**Why the moat compounds**:
1. **Network Effects**: More SKUs → more receipts → compliance bundle value ↑
2. **Learning**: Each SKU improves ggen templates (shared factory)
3. **Lock-in**: Customers using 3+ SKUs can't leave (receipt interop)
4. **Pricing Power**: As receipt network grows, we can raise prices

---

## STRATEGIC SEQUENCING

### Phase 1: Foundation (Q1 2026)
Launch SKUs that create receipt density:
1. **Simple Agent Runtime** (every AI app needs this)
2. **Workflow Engine** (creates receipt chains)
3. **Feature Flags** (decision receipts everywhere)

**Goal**: 500+ receipts/day across customer base

### Phase 2: Compliance Flywheel (Q2 2026)
Launch SKUs that consume receipts:
4. **Compliance Docs Generator** (turns receipts into SOC 2)
5. **Access Control as Data** (login receipts)
6. **Vulnerability Scanner** (scan receipts)

**Goal**: First SOC 2 report generated entirely from receipts

### Phase 3: Data Moat (Q3 2026)
Launch SKUs that create ontology lock-in:
7. **Semantic Data Catalog** (RDF lineage)
8. **Data Quality Monitor** (SHACL rules)
9. **Evidence-First ETL** (provenance chains)

**Goal**: Customers can't leave without losing their knowledge graph

### Phase 4: Platform Play (Q4 2026)
Launch SKUs that create ecosystem:
10-20. **Productivity tools** (docs, forms, email, etc.)

**Goal**: Every workflow generates receipts → compliance bundle inevitable

---

## COMPETITIVE DYNAMICS

### Why Incumbents Can't Respond

**1. Cost Structure**
- Datadog: $3M to build new feature, 18-month cycle
- Us: $5K to build new SKU, 6-week cycle
- **They can't price-match without destroying margins**

**2. Architectural Lock-in**
- Their products = monoliths (can't extract 20%)
- Our products = ontology projections (trivial to subset)
- **They can't ship 80/20 versions without cannibalizing 100% SKUs**

**3. Stealth Advantage**
- They think we're a "simple alternative" (Erlang invisible)
- By the time they reverse-engineer receipts, we have 20 SKUs
- **Cognitive + computational hardness = 2-year moat per SKU**

### Why Customers Switch

**1. Price**
- 5-10× cheaper than incumbent
- ROI payback in 2-3 months

**2. Simplicity**
- 20% of features = 0% of complexity
- Self-serve onboarding in <1 hour

**3. Trust**
- Receipts = cryptographic proof
- Incumbents = "trust our dashboard"

**4. Lock-in Reversal**
- Receipts are portable (open standard)
- Incumbent data is proprietary

---

## MESSAGING MATRIX

| Audience | Pain Point | Our 80/20 Solution | Incumbent Problem |
|----------|-----------|-------------------|-------------------|
| Startups | "Datadog costs more than AWS" | Observability for $9/host | $99/host minimum |
| Mid-Market | "SOC 2 costs $50K" | Compliance docs for $499/mo | Vanta at $20K/yr |
| Enterprise | "Can't prove API calls happened" | Receipts for everything | Logs you hope weren't tampered |
| Government | "NIST 800-53 requires evidence" | Native compliance receipts | Manual evidence collection |

---

## NEXT STEPS

1. **Build Priority**: Start with SKU #1 (Agent Runtime) - 60 hours, highest market pull
2. **Go-to-Market**: Homepage copy that leads with pain ("Stop paying for features you don't use")
3. **Pricing Strategy**: Free tier to build receipt network, paid tier at 5× discount to incumbent
4. **Bundle Definition**: "Compliance Bundle" = 5+ SKUs with unified receipt viewer

**The factory is ready. Time to ship SKUs.**
