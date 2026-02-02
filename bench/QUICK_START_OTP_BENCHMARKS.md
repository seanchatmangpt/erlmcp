# Quick Start: OTP Performance Benchmarks

## 5-Minute Setup

```bash
# 1. Navigate to project
cd /Users/sac/erlmcp

# 2. Run full benchmark suite
./bench/run_otp_comparison.sh

# 3. View results
cat bench/results/otp_comparison/otp_comparison_*.json | jq '.summary'
```

## Understanding Results

### Good Grade (A)

```json
{
  "message_throughput_m": 5.23,     // >5.0 ✓
  "max_connections": 48234,         // >40K ✓
  "avg_memory_kb": 4.5,            // <10 ✓
  "registry_throughput_k": 587.2,   // >553 ✓
  "queue_throughput_m": 42.3,       // >40 ✓
  "otel_overhead": 2.13            // <5% ✓
}
```

### Needs Improvement (C)

```json
{
  "message_throughput_m": 3.1,      // <5.0 ❌
  "max_connections": 28500,         // <40K ❌
  "avg_memory_kb": 12.8,           // >10 ❌
  "registry_throughput_k": 423.1,   // <553 ❌
  "queue_throughput_m": 31.2,       // <40 ❌
  "otel_overhead": 8.5             // >5% ❌
}
```

## Common Issues

### Issue: Low Message Throughput

**Fix**: Adjust scheduler count
```bash
# Edit vm.args
echo "+SP 8" >> vm.args  # Set to CPU core count

# Re-run benchmark
./bench/run_otp_comparison.sh
```

### Issue: Low Connection Capacity

**Fix**: Increase file descriptor limit
```bash
# Check current limit
ulimit -n

# Increase limit
ulimit -n 65535

# Re-run benchmark
./bench/run_otp_comparison.sh
```

### Issue: High Memory Usage

**Fix**: Enable binary optimization
```bash
# Edit vm.args
echo "+MBas aobf" >> vm.args

# Re-run benchmark
./bench/run_otp_comparison.sh
```

## Comparing OTP Versions

### Install kerl (OTP Version Manager)

```bash
# macOS
brew install kerl

# Linux
git clone https://github.com/kerl/kerl.git
chmod +x kerl/kerl
sudo mv kerl/kerl /usr/local/bin/
```

### Build OTP Versions

```bash
# Update available releases
kerl update releases

# Build OTP 28.3.1
kerl build 28.3.1
kerl install 28.3.1 $HOME/.kerl/installs/28.3.1

# Build OTP 27.3.2
kerl build 27.3.2
kerl install 27.3.2 $HOME/.kerl/installs/27.3.2
```

### Run Comparison

```bash
# Compare all installed versions
./bench/compare_otp_versions.sh

# View comparison table
cat bench/results/otp_comparison/comparison_table.md
```

## Integration with CI/CD

### GitHub Actions Example

```yaml
- name: Run OTP Benchmarks
  run: ./bench/run_otp_comparison.sh
  
- name: Check for Regression
  run: |
    python3 bench/scripts/analyze_otp_comparison.py \
      bench/results/otp_comparison/ \
      28
```

### Pre-commit Hook

```bash
# .git/hooks/pre-commit
#!/bin/bash

# Run quick benchmark (OTP 28 baseline)
./bench/run_otp_comparison.sh > /dev/null 2>&1

# Check for regression
python3 bench/scripts/analyze_otp_comparison.py \
  bench/results/otp_comparison/ \
  28

if [ $? -ne 0 ]; then
  echo "Performance regression detected!"
  echo "Run './bench/run_otp_comparison.sh' for details"
  exit 1
fi
```

## Performance Targets Summary

| Metric | Target (A) | Acceptable (B) | Poor (C) |
|--------|------------|----------------|----------|
| Message Throughput | >5M msg/s | >3M msg/s | <3M msg/s |
| Max Connections | >50K | >40K | <40K |
| Memory/Process | <5 KB | <10 KB | >10 KB |
| Registry | >553K ops/s | >400K ops/s | <400K ops/s |
| Queue | >40M ops/s | >30M ops/s | <30M ops/s |
| OTEL Overhead | <5% | <10% | >10% |

## Next Steps

1. **Establish Baseline**: Run on OTP 28.3.1 to create baseline
2. **Monitor Trends**: Run weekly and track performance over time
3. **Automate**: Integrate into CI/CD pipeline
4. **Investigate**: Address any regressions immediately
5. **Document**: Record VM args and configuration changes

## Getting Help

- Full Guide: `bench/OTP_COMPARISON_GUIDE.md`
- README: `bench/OTP_COMPARISON_README.md`
- Issues: https://github.com/seanchatmangpt/erlmcp/issues

## Key Files

- `bench/erlmcp_bench_otp_comparison.erl` - Main benchmark module
- `bench/run_otp_comparison.sh` - Run current OTP
- `bench/compare_otp_versions.sh` - Compare multiple OTP versions
- `bench/scripts/analyze_otp_comparison.py` - Analyze results
- `bench/results/otp_comparison/` - Result files
