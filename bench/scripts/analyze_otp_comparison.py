#!/usr/bin/env python3
"""
OTP Comparison Benchmark Analyzer

Analyzes benchmark results across OTP versions and detects regressions.
"""

import json
import sys
from pathlib import Path
from typing import Dict, List, Any
from dataclasses import dataclass


@dataclass
class BenchmarkResult:
    """Individual benchmark result"""
    otp_release: str
    erts_version: str
    timestamp: int
    
    message_throughput: float  # M msg/sec
    max_connections: int
    avg_memory_kb: float
    registry_throughput: float  # K msg/sec
    queue_throughput: float  # M ops/sec
    otel_overhead: float  # percent
    
    grade: str


def load_results(results_dir: Path) -> List[BenchmarkResult]:
    """Load all benchmark results from directory"""
    results = []
    
    for json_file in results_dir.glob("otp_comparison_*.json"):
        try:
            with open(json_file, 'r') as f:
                data = json.load(f)
            
            summary = data.get('summary', {})
            
            result = BenchmarkResult(
                otp_release=summary.get('otp_release', 'unknown'),
                erts_version=data.get('erts_version', 'unknown'),
                timestamp=data.get('timestamp', 0),
                
                message_throughput=summary.get('message_throughput_m', 0.0),
                max_connections=int(summary.get('max_connections', 0)),
                avg_memory_kb=summary.get('avg_memory_kb', 0.0),
                registry_throughput=summary.get('registry_throughput_k', 0.0),
                queue_throughput=summary.get('queue_throughput_m', 0.0),
                otel_overhead=summary.get('otel_overhead', 0.0),
                
                grade=data.get('grade', {}).get('overall', 'C')
            )
            
            results.append(result)
            
        except (json.JSONDecodeError, KeyError) as e:
            print(f"Warning: Failed to load {json_file}: {e}", file=sys.stderr)
    
    return results


def compare_versions(results: List[BenchmarkResult]) -> Dict[str, Any]:
    """Compare performance across OTP versions"""
    
    if not results:
        return {"error": "No results to compare"}
    
    # Group by OTP version
    by_version: Dict[str, List[BenchmarkResult]] = {}
    for result in results:
        version = result.otp_release
        if version not in by_version:
            by_version[version] = []
        by_version[version].append(result)
    
    # Calculate averages per version
    version_stats = {}
    for version, version_results in by_version.items():
        count = len(version_results)
        version_stats[version] = {
            'count': count,
            'message_throughput': sum(r.message_throughput for r in version_results) / count,
            'max_connections': sum(r.max_connections for r in version_results) // count,
            'avg_memory_kb': sum(r.avg_memory_kb for r in version_results) / count,
            'registry_throughput': sum(r.registry_throughput for r in version_results) / count,
            'queue_throughput': sum(r.queue_throughput for r in version_results) / count,
            'otel_overhead': sum(r.otel_overhead for r in version_results) / count,
            'grade': most_common_grade(version_results)
        }
    
    return version_stats


def detect_regression(current: BenchmarkResult, baseline: BenchmarkResult, 
                     threshold: float = 10.0) -> Dict[str, Any]:
    """Detect performance regression against baseline"""
    
    def check_regression(current_val: float, baseline_val: float, 
                        name: str, higher_is_better: bool = True) -> Dict[str, Any]:
        """Check individual metric for regression"""
        if baseline_val == 0:
            return {"metric": name, "status": "skip", "reason": "baseline is zero"}
        
        change = ((current_val - baseline_val) / baseline_val) * 100
        
        if higher_is_better:
            regressed = change < -threshold
            improved = change > threshold
        else:
            regressed = change > threshold
            improved = change < -threshold
        
        status = "regression" if regressed else ("improvement" if improved else "ok")
        
        return {
            "metric": name,
            "status": status,
            "change_percent": change,
            "current": current_val,
            "baseline": baseline_val
        }
    
    # Check all metrics
    checks = [
        check_regression(
            current.message_throughput,
            baseline.message_throughput,
            "Message Throughput",
            higher_is_better=True
        ),
        check_regression(
            current.max_connections,
            baseline.max_connections,
            "Max Connections",
            higher_is_better=True
        ),
        check_regression(
            current.avg_memory_kb,
            baseline.avg_memory_kb,
            "Memory (KB)",
            higher_is_better=False  # Lower is better
        ),
        check_regression(
            current.registry_throughput,
            baseline.registry_throughput,
            "Registry Throughput",
            higher_is_better=True
        ),
        check_regression(
            current.queue_throughput,
            baseline.queue_throughput,
            "Queue Throughput",
            higher_is_better=True
        ),
        check_regression(
            current.otel_overhead,
            baseline.otel_overhead,
            "OTEL Overhead",
            higher_is_better=False  # Lower is better
        )
    ]
    
    # Overall status
    has_regression = any(c["status"] == "regression" for c in checks)
    
    return {
        "has_regression": has_regression,
        "checks": checks,
        "threshold": threshold
    }


def print_comparison(version_stats: Dict[str, Any]):
    """Print version comparison table"""
    
    if not version_stats or "error" in version_stats:
        print("No comparison data available")
        return
    
    print("\n" + "=" * 100)
    print("OTP Version Comparison")
    print("=" * 100)
    
    # Header
    versions = sorted(version_stats.keys())
    print(f"\n{'Metric':<25} " + " ".join(f"{v:>15}" for v in versions))
    print("-" * 100)
    
    # Message Throughput
    print(f"{'Message Throughput (M/s)':<25} " + 
          " ".join(f"{version_stats[v]['message_throughput']:>15.2f}" for v in versions))
    
    # Max Connections
    print(f"{'Max Connections':<25} " + 
          " ".join(f"{version_stats[v]['max_connections']:>15}" for v in versions))
    
    # Memory
    print(f"{'Memory/Process (KB)':<25} " + 
          " ".join(f"{version_stats[v]['avg_memory_kb']:>15.2f}" for v in versions))
    
    # Registry Throughput
    print(f"{'Registry Throughput (K/s)':<25} " + 
          " ".join(f"{version_stats[v]['registry_throughput']:>15.1f}" for v in versions))
    
    # Queue Throughput
    print(f"{'Queue Throughput (M/s)':<25} " + 
          " ".join(f"{version_stats[v]['queue_throughput']:>15.2f}" for v in versions))
    
    # OTEL Overhead
    print(f"{'OTEL Overhead (%)':<25} " + 
          " ".join(f"{version_stats[v]['otel_overhead']:>15.2f}" for v in versions))
    
    # Grade
    print(f"{'Overall Grade':<25} " + 
          " ".join(f"{version_stats[v]['grade']:>15}" for v in versions))
    
    print("=" * 100)


def print_regression(regression: Dict[str, Any]):
    """Print regression report"""
    
    if not regression.get("has_regression", False):
        print("\nNo regression detected")
        return
    
    print("\n" + "!" * 50)
    print("PERFORMANCE REGRESSION DETECTED")
    print("!" * 50)
    print(f"\nThreshold: {regression['threshold']}%")
    print()
    
    for check in regression["checks"]:
        if check["status"] == "regression":
            print(f"❌ {check['metric']}")
            print(f"   Baseline: {check['baseline']:.2f}")
            print(f"   Current:  {check['current']:.2f}")
            print(f"   Change:   {check['change_percent']:.2f}%")
            print()


def most_common_grade(results: List[BenchmarkResult]) -> str:
    """Find most common grade"""
    from collections import Counter
    grades = [r.grade for r in results]
    return Counter(grades).most_common(1)[0][0]


def main():
    """Main entry point"""
    
    if len(sys.argv) < 2:
        print("Usage: analyze_otp_comparison.py <results_dir> [baseline_version]")
        print("  results_dir: Directory containing OTP comparison results")
        print("  baseline_version: OTP version to use as baseline (default: oldest)")
        sys.exit(1)
    
    results_dir = Path(sys.argv[1])
    
    if not results_dir.exists():
        print(f"Error: Results directory does not exist: {results_dir}")
        sys.exit(1)
    
    # Load results
    results = load_results(results_dir)
    
    if not results:
        print(f"Error: No benchmark results found in {results_dir}")
        sys.exit(1)
    
    print(f"Loaded {len(results)} benchmark results")
    
    # Compare versions
    version_stats = compare_versions(results)
    print_comparison(version_stats)
    
    # Detect regression (use oldest version as baseline if not specified)
    if len(sys.argv) >= 3:
        baseline_version = sys.argv[2]
    else:
        # Use oldest version as baseline
        baseline_version = min(version_stats.keys())
        print(f"\nUsing OTP {baseline_version} as baseline")
    
    # Find baseline result (first result from baseline version)
    baseline_result = next((r for r in results if r.otp_release == baseline_version), None)
    current_result = max(results, key=lambda r: r.timestamp)  # Use most recent
    
    if baseline_result and current_result:
        regression = detect_regression(current_result, baseline_result)
        print_regression(regression)
        
        # Exit with error code on regression
        if regression.get("has_regression", False):
            sys.exit(1)


if __name__ == "__main__":
    main()
