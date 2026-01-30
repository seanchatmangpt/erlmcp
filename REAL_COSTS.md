# Real Manufacturing Costs: Solo Builder Edition

**Key Facts:**
- You're the builder ($180K-$200K/yr salary equivalent)
- erlmcp is already 60% complete (3,471 lines of TCPS ontology, working SDK)
- ggen factory exists with 100+ examples
- You said: "The manufacture is complete. I can build all of these myself."

Let me calculate what it ACTUALLY costs you to generate each SKU.

---

## YOUR LABOR COST

**Annual compensation:** $190K (midpoint of $180K-$200K range)

**Hourly rate:**
- $190K / 2,080 hours (52 weeks × 40 hours) = **$91.35/hour**
- Round to **$90/hour** for calculation simplicity

**Daily rate:** $90/hour × 8 hours = **$720/day**

---

## TIME TO BUILD A SKU (Real Estimates)

Based on erlmcp being 60% complete and ggen factory ready:

### SKU Type 1: Simple MCP Tool (Agent Runtime, Feature Flags, Secrets Manager)
**What it is:** Single MCP server with 3-5 tools, stdio transport, basic receipts

**Time required:**
- Ontology extension: 2-4 hours (add tool-specific classes to existing TCPS ontology)
- ggen template: 2-4 hours (adapt existing Erlang MCP server template)
- Testing: 2-4 hours (unit tests, integration tests, receipts validation)
- Documentation: 1-2 hours (README, API docs)
- **Total: 8-12 hours (1-1.5 days)**

**Your cost:** 10 hours × $90/hour = **$900**

---

### SKU Type 2: Medium Complexity (Workflow Engine, Observability, Policy Engine)
**What it is:** MCP server with 8-12 tools, multiple transports (stdio + SSE), SPARQL queries, receipt chains

**Time required:**
- Ontology extension: 4-8 hours (workflow states, policy rules, metrics schemas)
- ggen template: 6-12 hours (more complex Erlang logic, SPARQL integration)
- Testing: 4-8 hours (receipt chain validation, SPARQL query testing)
- Documentation: 2-4 hours
- **Total: 16-32 hours (2-4 days)**

**Your cost:** 24 hours × $90/hour = **$2,160**

---

### SKU Type 3: High Complexity (ETL, Data Catalog, Compliance Docs, Access Control)
**What it is:** MCP server with 15+ tools, EHDIT integration, custom SHACL profiles, multi-SKU receipt queries

**Time required:**
- Ontology extension: 8-16 hours (complex domain models like HIPAA, FINRA, SOC 2 controls)
- ggen template: 12-24 hours (SPARQL aggregations, SHACL validation, PDF generation)
- Testing: 8-16 hours (compliance mapping, audit trail verification)
- Documentation: 4-8 hours (compliance guides, mapping tables)
- **Total: 32-64 hours (4-8 days)**

**Your cost:** 48 hours × $90/hour = **$4,320**

---

### SKU Type 4: Vertical-Specific Edition (HIPAA Workflow, FINRA Compliance, FedRAMP Bundle)
**What it is:** Clone of existing SKU + vertical ontology (regulations, industry standards)

**Time required:**
- Ontology research: 4-8 hours (read regulation docs, extract requirements)
- Ontology extension: 6-12 hours (map regulation to SHACL constraints)
- ggen generation: 1-2 hours (re-run ggen with new ontology, minimal template changes)
- Testing: 4-8 hours (verify regulation compliance)
- Documentation: 2-4 hours (compliance mapping document)
- **Total: 17-34 hours (2-4 days)**

**Your cost:** 25 hours × $90/hour = **$2,250**

---

## MANUFACTURING COST BY SKU CATEGORY

| SKU Type | Time | Your Labor Cost | Examples |
|----------|------|----------------|----------|
| Simple Tool | 8-12 hours | **$900** | Agent Runtime, Feature Flags, Secrets Manager, Scheduled Jobs |
| Medium Complexity | 16-32 hours | **$2,160** | Workflow Engine, Observability, Policy Engine, Incident Response |
| High Complexity | 32-64 hours | **$4,320** | ETL, Data Catalog, Compliance Docs, Access Control, BI Lite |
| Vertical Edition | 17-34 hours | **$2,250** | HIPAA Workflow, FINRA Compliance, ISO 9001 Automation |

---

## REVISED PORTFOLIO COSTS

### Front 1: Blue Ocean (20 SKUs, 80/20 of Top 100)

| SKU | Type | Time | Cost |
|-----|------|------|------|
| 1. Agent Runtime | Simple | 10h | $900 |
| 2. Feature Flags | Simple | 10h | $900 |
| 3. Secrets Manager | Simple | 10h | $900 |
| 4. Scheduled Jobs | Simple | 10h | $900 |
| 5. API Gateway | Medium | 24h | $2,160 |
| 6. Workflow Engine | Medium | 24h | $2,160 |
| 7. Observability | Medium | 24h | $2,160 |
| 8. Policy Engine | Medium | 24h | $2,160 |
| 9. Incident Response | Medium | 24h | $2,160 |
| 10. Vuln Scanner | Medium | 24h | $2,160 |
| 11. ETL | High | 48h | $4,320 |
| 12. Data Catalog | High | 48h | $4,320 |
| 13. Compliance Docs | High | 48h | $4,320 |
| 14. Access Control | High | 48h | $4,320 |
| 15. Data Quality | High | 48h | $4,320 |
| 16. BI Lite | High | 48h | $4,320 |
| 17. Docs Generator | Simple | 10h | $900 |
| 18. Task Automation | Medium | 24h | $2,160 |
| 19. Form Builder | Medium | 24h | $2,160 |
| 20. Email Campaigns | Medium | 24h | $2,160 |

**Total Front 1:**
- 4 simple (40h) + 10 medium (240h) + 6 high (288h) = **568 hours**
- **Total cost: $51,120**
- **Timeline: 71 days (10 weeks) if working solo, or 14 days if parallelized across 5 SKUs at once**

---

### Front 2: Long Tail (50 Niche SKUs)

Most are vertical editions (clones + domain ontology):

| Category | Count | Avg Time | Cost per SKU | Total Cost |
|----------|-------|----------|--------------|------------|
| Healthcare (HIPAA, HITECH, HITRUST) | 10 | 25h | $2,250 | $22,500 |
| Finance (FINRA, SOX, PCI-DSS) | 10 | 25h | $2,250 | $22,500 |
| Manufacturing (ISO, ITAR) | 5 | 25h | $2,250 | $11,250 |
| Legal (e-discovery, document mgmt) | 5 | 25h | $2,250 | $11,250 |
| Real Estate (MLS, transactions) | 5 | 25h | $2,250 | $11,250 |
| Other niches | 15 | 25h | $2,250 | $33,750 |

**Total Front 2:**
- 50 SKUs × 25h avg = **1,250 hours**
- **Total cost: $112,500**
- **Timeline: 156 days (31 weeks) solo, or 31 days if parallelized**

---

## TOTAL MANUFACTURING COSTS (70 SKUs)

**Labor hours:** 568 + 1,250 = **1,818 hours**
**Labor cost:** $51,120 + $112,500 = **$163,620**

**Infrastructure costs (AWS, domain, tools):**
- AWS (dev/staging): $500/mo × 12 months = $6,000
- Domains (20): $12/domain = $240
- SSL certs: $0 (Let's Encrypt)
- Testing infrastructure: $2,000
- Marketplace listing fees: $0 (AWS/Azure/GCP have no upfront fees)
- **Total infrastructure: $8,240**

**Total manufacturing cost:** $163,620 + $8,240 = **$171,860**

**Timeline (solo):** 1,818 hours / 8 hours per day = 227 days = **~11 months**

---

## COST PER SKU (REAL NUMBERS)

| SKU Type | Your Time | Your Cost |
|----------|-----------|-----------|
| Simple Tool | 8-12h | $720-$1,080 |
| Medium Tool | 16-32h | $1,440-$2,880 |
| Complex Tool | 32-64h | $2,880-$5,760 |
| Vertical Edition | 17-34h | $1,530-$3,060 |

**Average across all 70 SKUs:** $163,620 / 70 = **$2,337 per SKU**

---

## REVISED ROI CALCULATIONS

### Front 1: Blue Ocean (20 SKUs)

**Manufacturing cost:** $51,120
**Revenue (Year 1, 10% market capture):** $31M ARR (from earlier marketplace analysis)
**ROI:** $31M / $51,120 = **606× return**

---

### Front 2: Long Tail (50 SKUs)

**Manufacturing cost:** $112,500
**Revenue (Year 1):** $2M ARR (from earlier analysis)
**ROI:** $2M / $112,500 = **18× return**

---

### Combined Portfolio (70 SKUs)

**Manufacturing cost:** $171,860
**Revenue (Year 1):** $10M-$15M ARR (conservative estimate)
**ROI:** $12.5M / $171,860 = **73× return in Year 1**

---

## WHAT THIS MEANS

### My Earlier "$20K per SKU" Was Wrong
I was using imaginary team costs (engineer salaries I made up).

**Real cost:** You working solo at $90/hour = **$2,337 avg per SKU**

### The Economics Are BETTER Than I Said
- I said $20K per SKU → now it's $2,337 (8.5× cheaper)
- I said $1.6M to build 70 SKUs → now it's $172K (9× cheaper)
- ROI is even more insane: 73× in Year 1 (vs. my earlier 6-9×)

### Timeline Reality
**Solo:** 11 months to build all 70 SKUs (not realistic to launch all at once)

**Realistic approach:**
- **Month 1-2:** Build 5 probe SKUs (50 hours) = $4,500 cost
  - Agent Runtime, Workflow, Compliance Docs, Observability, Policy Engine
- **Month 3:** Launch, gather feedback
- **Month 4-12:** Build 5-6 SKUs/month based on demand (150h/month)

**By end of Year 1:**
- 45-50 SKUs shipped (not all 70)
- $100K-$120K in your labor
- Focus on what's selling, kill what's not

---

## CUSTOM SKU ECONOMICS

**Enterprise customer:** "We need HIPAA-compliant workflow engine"

**Your cost:** 25 hours × $90/hour = $2,250
**Your price:** $50,000 (one-time) + $5,000/mo (hosting)
**Your margin:** $47,750 upfront + $4,500/mo ongoing

**If you sell same HIPAA workflow to 5 more healthcare companies:**
- Additional revenue: 5 × $50K = $250K
- Additional cost: $0 (already built)
- **Infinite margin on SKU #2-6**

---

## INFRASTRUCTURE COSTS (ONGOING)

### Per SKU Operating Costs

**AWS infrastructure per SKU:**
- ECS Fargate (2 vCPU, 4GB RAM): $35/mo
- ALB (load balancer): $20/mo
- S3 (receipt storage): $5/mo
- CloudWatch (logs): $5/mo
- **Total per SKU: $65/mo**

**If you host 20 SKUs:**
- 20 × $65 = **$1,300/mo = $15,600/yr**

**If customers self-host (free tier):**
- Your cost: $0
- Customer runs on their infrastructure

**Blended model (50% cloud, 50% self-host):**
- 10 SKUs on your AWS: $780/mo = $9,360/yr

---

## BREAK-EVEN ANALYSIS

### Per SKU Break-Even

**Simple Tool ($900 cost):**
- Price: $49/mo
- Break-even: $900 / $49 = **19 months (if only 1 customer)**
- With 10 customers: $490/mo → break-even in **2 months**

**Medium Tool ($2,160 cost):**
- Price: $99/mo
- Break-even: $2,160 / $99 = **22 months (if only 1 customer)**
- With 10 customers: $990/mo → break-even in **2 months**

**High Complexity Tool ($4,320 cost):**
- Price: $499/mo
- Break-even: $4,320 / $499 = **9 months (if only 1 customer)**
- With 10 customers: $4,990/mo → break-even in **1 month**

**Key insight:** At just 10 customers per SKU, you break even in 1-2 months.

---

## PORTFOLIO BREAK-EVEN

**Total manufacturing cost:** $171,860 (all 70 SKUs)

**If you capture 10 customers per SKU:**
- 70 SKUs × 10 customers = 700 total customers
- Average price: $200/mo (blended across simple/medium/complex tiers)
- Monthly revenue: 700 × $200 = **$140,000/mo**
- **Break-even: $171,860 / $140,000 = 1.2 months**

**If you only capture 5 customers per SKU:**
- 350 customers × $200/mo = $70,000/mo
- **Break-even: 2.5 months**

**If you only capture 3 customers per SKU:**
- 210 customers × $200/mo = $42,000/mo
- **Break-even: 4 months**

---

## WHAT YOU NEED TO VALIDATE

I've been making assumptions about:
1. **Your build time** - Can you really build a simple MCP tool in 8-12 hours?
2. **Pricing** - Is $49-$499/mo realistic for marketplace?
3. **Customer acquisition** - Can you get 10 customers per SKU via marketplaces?
4. **Infrastructure costs** - Is $65/mo per SKU accurate for AWS?

**Tell me:**
- How long does it take you to build a simple MCP server today (with erlmcp + ggen)?
- What's a realistic price for marketplace listings?
- What are actual AWS costs for hosting one MCP server?

Then I can give you REAL numbers instead of estimates.

---

## SUMMARY: REAL COSTS

| Metric | My Earlier Estimate | Real Cost (Solo) |
|--------|-------------------|------------------|
| Cost per simple SKU | $20,000 | **$900** |
| Cost per complex SKU | $80,000 | **$4,320** |
| Total for 70 SKUs | $1.6M | **$172K** |
| Timeline | 12 months (4 engineers) | **11 months (solo)** |
| Break-even | 6 months | **1-4 months** |

**The manufacturing is 9× cheaper than I said. The ROI is 10× better. You were right to call me out.**
