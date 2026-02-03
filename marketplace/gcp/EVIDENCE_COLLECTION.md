# Marketplace Review Evidence Collection System

Automated evidence collection and archiving for GCP Marketplace review process.

## Overview

This system automatically captures, validates, and archives all evidence required for GCP Marketplace reviewer validation. Every test generates cryptographically verifiable receipts with SHA-256 hashes.

## Quick Start

```bash
# Run complete evidence collection
cd /Users/sac/erlmcp
./marketplace/gcp/scripts/collect-evidence.sh

# This will:
# 1. Create timestamped evidence directory
# 2. Run all deployment tests
# 3. Capture observability data
# 4. Perform security audit
# 5. Test failure scenarios
# 6. Benchmark performance
# 7. Generate hash manifests
# 8. Create distributable archive
```

## Evidence Archive Structure

```
REVIEW_EVIDENCE/YYYYMMDD-HHMMSS/
├── 00-bootstrap.log                    # Project bootstrap
├── 01-schema-validation.log            # Schema validation
├── 02-vm-deployment.log                # Compute Engine deployment
├── 03-gke-deployment.log               # GKE deployment
├── 04-cloudrun-deployment.log          # Cloud Run deployment
├── 05-secrets-rotation.log             # Secret Manager tests
├── 06-cloud-logging.log                # Cloud Logging queries
├── 07-cloud-monitoring.log             # Cloud Monitoring metrics
├── 08-error-reporting.log              # Error Reporting data
├── 09-cloud-debugger.log               # Cloud Debugger status
├── 10-cloud-trace.log                  # Cloud Trace summary
├── 11-iam-policies.log                 # IAM policy dump
├── 12-service-accounts.log             # Service account list
├── 13-firewall-rules.log               # Firewall rules
├── 14-network-tiers.log                # Network configuration
├── 15-security-scan.log                # Security audit results
├── 16-kill-test.log                    # VM termination test
├── 17-restart-test.log                 # Restart verification
├── 18-recovery-test.log                # Disaster recovery
├── 19-load-test.log                    # Load testing
├── 20-benchmark.log                    # Performance benchmarks
├── 21-latency.log                      # Latency measurements
├── 22-destruction.log                  # Cleanup logs
├── screenshots/                        # Console screenshots
│   ├── monitoring-dashboard.png
│   ├── logging-viewer.png
│   └── error-reporting.png
├── manifests/                          # Hash manifests
│   ├── sha256sum.txt                   # SHA-256 hashes
│   ├── sha512sum.txt                   # SHA-512 hashes
│   └── file-list.json                  # Detailed metadata
└── FINAL_REPORT.md                     # Summary report
```

## Individual Test Scripts

### Evidence Collection
- **collect-evidence.sh** - Main orchestration script (runs everything)

### Deployment Tests
- **bootstrap.sh** - Project bootstrap and GCP configuration
- **validate-schema.sh** - Marketplace schema validation
- **deploy-vm.sh** - Compute Engine deployment
- **deploy-gke.sh** - GKE cluster deployment
- **deploy-cloudrun.sh** - Cloud Run deployment
- **destroy.sh** - Resource cleanup

### Operational Tests
- **test-kill.sh** - VM termination and recovery
- **test-restart.sh** - Service restart verification
- **test-recovery.sh** - Disaster recovery procedures
- **test-secrets-rotation.sh** - Secret Manager rotation

### Performance Tests
- **load-test.sh** - Load testing with Apache Bench
- **measure-latency.sh** - Latency measurements
- **benchmark.sh** - Comprehensive performance benchmarks

### Security Tests
- **security-scan.sh** - Security audit (IAM, firewall, encryption)

## Evidence Categories

### 1. Deployment Evidence
Validates all deployment mechanisms:
- Compute Engine (VM)
- Google Kubernetes Engine (GKE)
- Cloud Run (serverless)
- Secret Manager integration

### 2. Observability Evidence
Captures monitoring and logging:
- Cloud Logging queries
- Cloud Monitoring metrics
- Error Reporting
- Cloud Debugger status
- Cloud Trace data
- Console screenshots (if Chrome available)

### 3. Security Evidence
Validates security posture:
- IAM policies
- Service accounts
- Firewall rules
- Network configuration
- Disk encryption
- Shielded VM features
- Secret access controls

### 4. Failure Testing
Validates resilience:
- VM termination (kill test)
- Service restart
- Disaster recovery
- Snapshot recovery

### 5. Performance Evidence
Meets performance SLAs:
- Load testing (concurrent connections)
- Response time distribution
- Latency percentiles
- Throughput benchmarks
- Resource utilization

### 6. Destruction Evidence
Validates cleanup:
- Resource deletion
- No orphaned resources
- Clean state restoration

## Hash Verification

All evidence files include cryptographic hashes for verification:

```bash
# Verify SHA-256 hashes
cd REVIEW_EVIDENCE/YYYYMMDD-HHMMSS/
sha256sum -c manifests/sha256sum.txt

# Verify SHA-512 hashes
sha512sum -c manifests/sha512sum.txt

# View detailed file metadata
cat manifests/file-list.json | jq .
```

## Archive Creation

Evidence is automatically packaged as a distributable archive:

```bash
# Archive created automatically
marketplace-evidence-YYYYMMDD-HHMMSS.tar.gz
marketplace-evidence-YYYYMMDD-HHMMSS.tar.gz.sha256

# Verify archive integrity
sha256sum -c marketplace-evidence-YYYYMMDD-HHMMSS.tar.gz.sha256

# Extract archive
tar -xzf marketplace-evidence-YYYYMMDD-HHMMSS.tar.gz
```

## Final Report

The `FINAL_REPORT.md` includes:

1. **Executive Summary**
   - Total commands executed
   - Success/failure counts
   - Success rate percentage
   - Total duration

2. **Evidence Categories**
   - Checklist of captured evidence
   - Category completeness

3. **Command Results**
   - Per-command exit codes
   - Execution times
   - Detailed log references

4. **File Manifest**
   - Total file count
   - Total archive size
   - Hash verification instructions

5. **Screenshots**
   - Console screenshot list
   - File sizes

6. **Archive Integrity**
   - Manifest hash
   - Creation timestamp
   - Evidence ID

## Requirements

### Google Cloud SDK
```bash
gcloud components install
gcloud init
```

### Required Tools
- `gcloud` - Google Cloud CLI
- `jq` - JSON processor
- `sha256sum` / `sha512sum` - Hash utilities
- `tar` - Archive utility
- `curl` - HTTP client
- `gsutil` - Storage CLI (for bucket tests)

### Optional Tools (for enhanced features)
- `google-chrome` or `chromium` - Console screenshots
- `ab` (Apache Bench) - Load testing
- `ping` - Network latency testing

## Running Individual Tests

You can run individual test scripts for targeted testing:

```bash
# Just deployment
./marketplace/gcp/scripts/deploy-vm.sh

# Just security
./marketplace/gcp/scripts/security-scan.sh

# Just performance
./marketplace/gcp/scripts/load-test.sh
```

## Integration with CI/CD

Add to CI/CD pipeline:

```yaml
# .github/workflows/marketplace-review.yml
steps:
  - name: Collect Evidence
    run: ./marketplace/gcp/scripts/collect-evidence.sh

  - name: Upload Evidence
    uses: actions/upload-artifact@v3
    with:
      name: marketplace-evidence
      path: REVIEW_EVIDENCE/marketplace-evidence-*.tar.gz*
```

## Troubleshooting

### Chrome Screenshots Not Captured
Install Chrome for console screenshots:
```bash
# macOS
brew install --cask google-chrome

# Ubuntu/Debian
sudo apt-get install google-chrome-stable
```

### Apache Bench Not Available
Install for load testing:
```bash
# Ubuntu/Debian
sudo apt-get install apache2-utils

# macOS
brew install httpd
```

### Permission Errors
Ensure proper IAM roles:
```bash
gcloud projects add-iam-policy-binding PROJECT_ID \
  --member='user:YOUR_EMAIL' \
  --role='roles/owner'
```

## Evidence Retention

Evidence archives should be retained for:
- Marketplace review period (minimum 30 days)
- Audit trail requirements
- Historical performance analysis

## Security Considerations

1. **Sensitive Data**: Evidence may contain:
   - IAM policies (redact if needed)
   - Service account emails (non-sensitive)
   - Internal IPs (non-sensitive)

2. **Secrets**: Script automatically:
   - Redacts secret values in logs
   - Uses test-only secrets
   - Cleans up test resources

3. **Access Control**: Archive contains:
   - No actual secrets (only references)
   - No private keys
   - No passwords

## Support

For issues or questions:
1. Check individual log files
2. Verify IAM permissions
3. Ensure GCP APIs enabled
4. Check quota limits

## License

Same as parent project.
