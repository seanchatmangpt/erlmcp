# Supply Chain Verification for erlmcp v1.4.0

This guide explains how to generate, verify, and upload supply chain artifacts (SBOM, provenance, vulnerability scans, and VEX policies) for erlmcp releases.

## Overview

The supply chain automation generates production-ready artifacts that establish trust and transparency in erlmcp's dependencies and build process:

- **SBOM (Software Bill of Materials)**: Complete list of all dependencies in CycloneDX and SPDX formats
- **Provenance**: SLSA-compliant reproducible build manifest with build environment details
- **Vulnerability Scans**: Comprehensive CVE assessment for all dependencies
- **VEX (Vulnerability Exploitability Exchange)**: Policy document for vulnerability disclosure

## Quick Start

### Generate All Supply Chain Artifacts

```bash
# Generate SBOM, provenance, vulnerabilities, and VEX in one command
make evidence

# Artifacts generated at: dist/evidence/v1.4.0/
ls -lh dist/evidence/v1.4.0/
```

Expected output:
```
erlmcp-1.4.0.sbom.json            (3.3K) - CycloneDX JSON SBOM
erlmcp-1.4.0.spdx.json            (5.9K) - SPDX JSON SBOM
erlmcp-1.4.0.provenance.json      (2.6K) - SLSA build manifest
erlmcp-1.4.0.build_manifest.txt   (5.1K) - Human-readable build info
erlmcp-1.4.0.vulnerabilities.json (3.0K) - CVE scan report
erlmcp-1.4.0.vex.json             (2.5K) - VEX policy document
erlmcp-1.4.0.vex_policy.txt       (8.8K) - VEX policy explanation
erlmcp-1.4.0.security_summary.txt (4.6K) - Security assessment summary
```

### Generate Individual Artifacts

```bash
# Generate just SBOM
make evidence-sbom

# Generate just provenance
make evidence-provenance

# Generate vulnerabilities
make evidence-vulnerabilities

# Generate VEX policy
make evidence-vex
```

## Verifying Artifacts

### Validate JSON Structure

```bash
# Verify all JSON files are valid
for f in dist/evidence/v1.4.0/*.json; do
  echo -n "$(basename $f): "
  python3 -m json.tool "$f" > /dev/null 2>&1 && echo "VALID" || echo "INVALID"
done
```

### Validate SBOM Completeness

```bash
# Check all 13 dependencies are present
python3 << 'EOF'
import json

with open('dist/evidence/v1.4.0/erlmcp-1.4.0.sbom.json') as f:
    sbom = json.load(f)
    components = sbom.get('components', [])
    names = [c['name'] for c in components]

    expected = ['jsx', 'jesse', 'gproc', 'gun', 'ranch', 'poolboy',
                'bbmustache', 'cowboy', 'opentelemetry_api', 'opentelemetry',
                'opentelemetry_exporter', 'jobs', 'fs']

    missing = [d for d in expected if d not in names]
    print(f"Dependencies scanned: {len(names)}")
    print(f"Missing: {missing if missing else 'None'}")
    print("Status: COMPLETE" if not missing else "Status: INCOMPLETE")
EOF
```

### Verify Provenance Reproducibility

```bash
# Check build is marked reproducible
python3 << 'EOF'
import json

with open('dist/evidence/v1.4.0/erlmcp-1.4.0.provenance.json') as f:
    prov = json.load(f)
    reproducible = prov.get('metadata', {}).get('reproducible', False)
    materials = prov.get('materials', [])

    print(f"Reproducible: {reproducible}")
    print(f"Materials recorded: {len(materials)}")
    for m in materials:
        print(f"  - {m.get('uri')}: {m.get('digest', {}).get('sha256', '')[:16]}...")
EOF
```

### Check Vulnerability Scan

```bash
# View vulnerability assessment
python3 << 'EOF'
import json

with open('dist/evidence/v1.4.0/erlmcp-1.4.0.vulnerabilities.json') as f:
    data = json.load(f)
    scan = data.get('scanResults', {})

    print(f"Packages scanned: {scan.get('packagesScanned', 0)}")
    print(f"Vulnerabilities found: {scan.get('vulnerabilitiesFound', 0)}")
    print(f"Critical: {scan.get('criticalCount', 0)}")
    print(f"High: {scan.get('highCount', 0)}")
    print(f"Medium: {scan.get('mediumCount', 0)}")
    print(f"Low: {scan.get('lowCount', 0)}")
EOF
```

### Verify VEX Policy

```bash
# Check VEX policy is complete
python3 << 'EOF'
import json

with open('dist/evidence/v1.4.0/erlmcp-1.4.0.vex.json') as f:
    vex = json.load(f)

    print(f"VEX Statements: {len(vex.get('statements', []))}")
    for stmt in vex.get('statements', []):
        print(f"  - {stmt.get('vulnerability', {}).get('name')}")
        print(f"    Status: {stmt.get('status')}")
EOF
```

## Running Tests

```bash
# Run supply chain test suite
rebar3 ct --suite erlmcp_supply_chain_SUITE

# Expected output: All tests PASS
#   ✓ SBOM artifacts exist
#   ✓ SBOM JSON valid
#   ✓ SBOM contains all dependencies
#   ✓ Provenance reproducible
#   ✓ Vulnerabilities scanned completely
#   ✓ VEX policy complete
```

## CI/CD Integration

### GitHub Actions

Add to `.github/workflows/release.yml`:

```yaml
- name: Generate Supply Chain Artifacts
  run: |
    make evidence

- name: Verify Artifacts
  run: |
    test -f dist/evidence/v1.4.0/erlmcp-1.4.0.sbom.json
    test -f dist/evidence/v1.4.0/erlmcp-1.4.0.provenance.json
    test -f dist/evidence/v1.4.0/erlmcp-1.4.0.vulnerabilities.json
    test -f dist/evidence/v1.4.0/erlmcp-1.4.0.vex.json

- name: Upload to Release
  uses: softprops/action-gh-release@v1
  with:
    files: dist/evidence/v1.4.0/*
```

### Manual Release Process

```bash
# 1. Generate artifacts
make evidence

# 2. Verify all artifacts
ls -la dist/evidence/v1.4.0/

# 3. Create release
gh release create v1.4.0 \
  dist/evidence/v1.4.0/erlmcp-1.4.0.sbom.json \
  dist/evidence/v1.4.0/erlmcp-1.4.0.spdx.json \
  dist/evidence/v1.4.0/erlmcp-1.4.0.provenance.json \
  dist/evidence/v1.4.0/erlmcp-1.4.0.vulnerabilities.json \
  dist/evidence/v1.4.0/erlmcp-1.4.0.vex.json
```

## Uploading to GCP Marketplace

### Prerequisites

```bash
# Install gcloud
curl https://sdk.cloud.google.com | bash

# Authenticate
gcloud auth login
gcloud config set project YOUR_GCP_PROJECT
```

### Upload SBOM to GCP

```bash
# Create a GCS bucket for supply chain evidence
gsutil mb gs://erlmcp-supply-chain/

# Upload SBOM
gsutil cp dist/evidence/v1.4.0/erlmcp-1.4.0.sbom.json \
  gs://erlmcp-supply-chain/v1.4.0/

# Upload SPDX format
gsutil cp dist/evidence/v1.4.0/erlmcp-1.4.0.spdx.json \
  gs://erlmcp-supply-chain/v1.4.0/

# Make publicly readable (optional)
gsutil acl ch -u AllUsers:R gs://erlmcp-supply-chain/v1.4.0/erlmcp-1.4.0.sbom.json

# Verify upload
gsutil ls -la gs://erlmcp-supply-chain/v1.4.0/
```

### Register with Marketplace

1. Go to [GCP Cloud Marketplace](https://console.cloud.google.com/marketplace)
2. Create new product
3. Add SBOM URL: `https://storage.googleapis.com/erlmcp-supply-chain/v1.4.0/erlmcp-1.4.0.sbom.json`
4. Add provenance URL for build verification
5. Upload VEX policy for vulnerability disclosure

## Artifact Specifications

### CycloneDX SBOM Format

File: `erlmcp-1.4.0.sbom.json`

Structure:
```json
{
  "bomFormat": "CycloneDX",
  "specVersion": "1.4",
  "serialNumber": "urn:uuid:erlmcp-sbom-1.4.0",
  "metadata": {
    "component": {
      "type": "application",
      "name": "erlmcp",
      "version": "1.4.0"
    }
  },
  "components": [
    {"name": "jsx", "version": "3.1.0", "type": "library"},
    ...
  ]
}
```

### SPDX Format

File: `erlmcp-1.4.0.spdx.json`

Includes detailed package metadata, licenses, and relationships.

### SLSA Provenance

File: `erlmcp-1.4.0.provenance.json`

Conforms to [SLSA Level 3](https://slsa.dev/) standard:
- Build environment captured (Erlang/OTP, rebar3 versions)
- Source materials hashed (SHA256)
- Reproducibility verified
- Build ID for traceability

### Vulnerability Report

File: `erlmcp-1.4.0.vulnerabilities.json`

Format: OpenVEX compatible
- All 13 dependencies assessed
- No known vulnerabilities found
- Scan timestamp and tooling recorded
- Compliance status: PASS

### VEX Policy

File: `erlmcp-1.4.0.vex.json` and `.txt`

OpenVEX standard format documenting:
- Vulnerability assessment methodology
- Known CVE coverage
- Justification for each vulnerability status
- Policy effective date and review schedule

## Troubleshooting

### Artifacts Not Generated

```bash
# Check make target exists
grep -n "^evidence:" Makefile

# Check scripts are executable
ls -la scripts/release/*.sh

# Run manually
bash scripts/release/generate_sbom.sh 1.4.0
bash scripts/release/generate_provenance.sh 1.4.0
bash scripts/release/scan_vulnerabilities.sh 1.4.0
bash scripts/release/generate_vex.sh 1.4.0
```

### JSON Validation Fails

```bash
# Install python3 if needed
python3 --version

# Validate specific file
python3 -m json.tool dist/evidence/v1.4.0/erlmcp-1.4.0.sbom.json

# Check for control characters
od -c dist/evidence/v1.4.0/erlmcp-1.4.0.sbom.json | grep -v '   ' | head -20
```

### Version Mismatch

```bash
# Update scripts to new version
sed -i 's/1.4.0/1.5.0/g' scripts/release/*.sh

# Verify updates
grep "VERSION" scripts/release/*.sh | head -4
```

## Known Limitations

1. **Erlang SBOM Tools**: Limited SBOM generation tools available for Erlang ecosystem
   - Solution: Community-maintained generators and manual dependency tracking

2. **CVE Database**: Erlang packages may not have comprehensive CVE coverage
   - Solution: Regular manual security audits and dependency updates

3. **Build Reproducibility**: Depends on exact toolchain versions
   - Solution: Record build environment in provenance

## Next Steps

- [ ] Integrate with SBOM-based compliance scanning
- [ ] Set up automated vulnerability monitoring
- [ ] Create marketplace product with supply chain evidence
- [ ] Establish quarterly security review schedule
- [ ] Document dependency license compliance

## References

- [CycloneDX Specification](https://cyclonedx.org/)
- [SPDX Format](https://spdx.dev/)
- [SLSA Framework](https://slsa.dev/)
- [OpenVEX Standard](https://openvex.dev/)
- [GCP Marketplace Guidelines](https://cloud.google.com/marketplace/docs)

## Support

For issues or questions:

1. Check logs: `dist/evidence/v1.4.0/`
2. Review build manifest: `erlmcp-1.4.0.build_manifest.txt`
3. File issue: https://github.com/banyan-platform/erlmcp/issues
