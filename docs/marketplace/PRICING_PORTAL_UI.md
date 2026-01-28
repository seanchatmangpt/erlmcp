# erlmcp Pricing Portal UI Layout

## Portal Structure

The erlmcp pricing portal is a single-page application (SPA) that presents three pricing tiers in an interactive, responsive layout. The portal fetches all pricing data from `dist/marketplace/plans.json` at runtime, ensuring data is always current and auto-generated from plan specifications.

### Header Section

```
┌────────────────────────────────────────────────────────────┐
│                    erlmcp Pricing                          │
│  Simple, transparent, flat-rate pricing.                   │
│  No metering, no surprises.                               │
└────────────────────────────────────────────────────────────┘
```

- **Title**: "erlmcp Pricing"
- **Tagline**: Emphasizes simplicity and transparency
- **Background**: Purple gradient (667eea → 764ba2)
- **Typography**: White, high contrast
- **Responsive**: Centered and scales on mobile

### Plan Cards Grid

Three equally-sized plan cards displayed in a responsive grid:

```
┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│   TEAM       │  │  ENTERPRISE  │  │  GOVERNMENT  │
│   Tier       │  │   Tier       │  │   Tier       │
│              │  │              │  │              │
│ Description  │  │ Description  │  │ Description  │
│              │  │              │  │              │
│ Throughput   │  │ Throughput   │  │ Throughput   │
│   450 req/s  │  │  1500 req/s  │  │   900 req/s  │
│              │  │              │  │              │
│ Concurrent   │  │ Concurrent   │  │ Concurrent   │
│     128      │  │     512      │  │     256      │
│              │  │              │  │              │
│ P99 Latency  │  │ P99 Latency  │  │ P99 Latency  │
│    250ms     │  │    100ms     │  │    150ms     │
│              │  │              │  │              │
│ Failover     │  │ Failover     │  │ Failover     │
│   30 sec     │  │   10 sec     │  │   15 sec     │
│              │  │              │  │              │
│ [View Deets] │  │ [View Deets] │  │ [View Deets] │
│ [Evidence]   │  │ [Evidence]   │  │ [Evidence]   │
└──────────────┘  └──────────────┘  └──────────────┘
```

#### Card Components

Each plan card contains:

1. **Plan Name** (bold, 24px)
   - "Team Tier - Startups, POCs, Low-Scale"
   - "Enterprise Tier - Production Applications"
   - "Government Tier - FIPS-140-2, Audit Logging"

2. **Description** (14px, gray)
   - One sentence explaining use case
   - Example: "Perfect for startups, proof-of-concepts, hobby projects"

3. **Envelope Summary Box** (light gray background)
   - Four-item list with labels and values
   - **Throughput**: throughput_req_s from plan spec
   - **Concurrent Connections**: concurrent_connections from plan spec
   - **P99 Latency**: p99_latency_ms from plan spec (in ms)
   - **Failover SLA**: failover_sla_seconds from plan spec (in seconds)
   - Formatted with thousand separators for large numbers

4. **Action Buttons**
   - **View Details** (blue, primary): Links to plan-specific documentation
   - **Evidence** (gray, secondary): Links to evidence bundle browser

### Comparison Section

Below the three plan cards is a detailed comparison matrix showing all tiers:

```
┌─────────────────────────────────────────────────────┐
│            Plan Comparison Matrix                   │
├─────────────────────────────────────────────────────┤
│ Metric              │ Team      │ Enterprise │ Gov │
├─────────────────────────────────────────────────────┤
│ Throughput (req/s)  │ 450       │ 1500       │ 900 │
│ Concurrent Conns    │ 128       │ 512        │ 256 │
│ P99 Latency (ms)    │ 250       │ 100        │ 150 │
│ Failover SLA (sec)  │ 30        │ 10         │ 15  │
│ Availability %      │ 99.0%     │ 99.95%     │ ... │
│ Audit Logging       │ No        │ Yes        │ Yes │
│ FIPS 140-2          │ No        │ No         │ Yes │
│ High Availability   │ No        │ Yes        │ Yes │
│ Pricing Model       │ Flat-Rate │ Flat-Rate  │ ... │
└─────────────────────────────────────────────────────┘
```

- Rows: Metric names (left column)
- Columns: Team, Enterprise, Government
- Data: Auto-generated from `plan-comparison.md`
- Highlights key differences between tiers

### Upgrade Path Visualization

Below the comparison table:

```
┌─────────────────────────────────────────────────────┐
│              Upgrade Paths                          │
├─────────────────────────────────────────────────────┤
│                                                     │
│ Team → Enterprise:                                  │
│ Unlock production-grade features, load balancing,  │
│ connection pooling, and high availability.         │
│                                                     │
│ Enterprise → Government:                            │
│ Add FIPS 140-2 compliance, comprehensive audit     │
│ logging, encryption enforcement, and security.     │
│                                                     │
└─────────────────────────────────────────────────────┘
```

- Text-based descriptions
- Shows feature progression
- Links to upgrade guides (if available)

## Design System

### Colors
- **Primary**: #667eea (Purple)
- **Secondary**: #764ba2 (Purple-darker)
- **Background**: White (#ffffff)
- **Text**: #333333 (Dark gray)
- **Borders**: #e0e0e0 (Light gray)
- **Hover**: #f5f5f5 (Very light gray)

### Typography
- **Headers**: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif
- **Body**: Same system font stack
- **Sizes**:
  - Page title: 48px
  - Section title: 32px
  - Plan name: 24px
  - Labels: 14px
  - Fine print: 13px

### Spacing
- **Page padding**: 40px top/bottom, 20px left/right
- **Card padding**: 40px
- **Card gap**: 30px
- **Button gap**: 10px
- **Line height**: 1.6 for readability

### Responsive Design

#### Desktop (1200px+)
- 3-column grid for plan cards
- Full comparison matrix visible
- All details in single viewport

#### Tablet (768px - 1199px)
- 2-column grid for plan cards (3rd card wraps)
- Comparison matrix scrollable
- Responsive font sizes

#### Mobile (< 768px)
- Single column for plan cards
- Stack vertically
- Comparison table horizontal scroll
- Optimized touch targets (min 44px)

## Interactive Elements

### Plan Card Interactions
1. **Hover**: Card slides up 10px with shadow enhancement
2. **Click Details**: Routes to plan documentation page
3. **Click Evidence**: Routes to evidence bundle browser for that plan

### Buttons
- **Primary (View Details)**:
  - Background: #667eea
  - Hover: #5568d3 (darker)
  - Transition: 0.3s ease

- **Secondary (Evidence)**:
  - Background: #f0f0f0
  - Border: 1px #e0e0e0
  - Hover: #e8e8e8

### Comparison Matrix
- **Rows hover**: Light gray highlight (#f9f9f9)
- **Sortable**: Can click column headers to sort (future enhancement)

## Data Flow

```
dist/marketplace/plans.json
        ↓
   (fetch at runtime)
        ↓
   Portal Page
        ↓
   Parse & Render
        ↓
   Plan Cards Grid
   Comparison Table
   Upgrade Paths
```

All data is deterministic - the same plan specs always generate identical portal output.

## Evidence Browser Integration

Each plan card's "Evidence" button links to evidence bundles:

```
/dist/evidence/<version>/<plan>/
  ├── sbom.json
  ├── provenance.json
  ├── chaos-report.md
  ├── benchmark-report.md
  └── audit-schema.json (Enterprise/Gov only)
```

## SLA Metrics Dashboard

Portal includes links to live SLA metrics:

```
/metrics/sla/<plan>/
  ├── Current availability %
  ├── Uptime in past 24h/7d/30d
  ├── P99 latency trends
  ├── Failover event logs
  └── Incident history
```

## Accessibility

- **ARIA labels**: All interactive elements labeled
- **Semantic HTML**: <button> tags for buttons, proper heading hierarchy
- **Keyboard navigation**: Tab through all interactive elements
- **Color contrast**: WCAG AA compliant (4.5:1 minimum)
- **Focus states**: Clear visual indicators on keyboard focus

## Performance

- **Page load**: < 1 second (HTML + CSS inline)
- **Data fetch**: < 200ms (plans.json from CDN)
- **Rendering**: < 300ms (client-side React/vanilla JS)
- **Total Time to Interactive**: < 1.5 seconds

## Portal Generation

Portal is auto-generated from plan specs:

```bash
make generate-portal
```

This:
1. Reads plans/team.plan.json, plans/enterprise.plan.json, plans/gov.plan.json
2. Generates dist/marketplace/plans.json (flattened, market-ready format)
3. Generates dist/marketplace/plan-comparison.md (markdown table)
4. Generates dist/marketplace/portal-metadata.json (generation metadata)
5. Generates templates/pricing_portal.html (portal template)

All output is deterministic - running the command multiple times produces identical output.

## Deployment

Portal is deployed as:
1. **HTML file**: templates/pricing_portal.html (served by web server)
2. **Data file**: dist/marketplace/plans.json (served by CDN or web server)
3. **Documentation**: dist/marketplace/plan-comparison.md (GitHub/documentation site)
4. **Metadata**: dist/marketplace/portal-metadata.json (audit trail)

Portal is validated before release:

```bash
make verify-portal
```

This ensures:
- plans.json is valid JSON
- All evidence files exist
- Comparison matrix is valid markdown
- HTML portal is well-formed
- Portal is ready for marketplace website integration
