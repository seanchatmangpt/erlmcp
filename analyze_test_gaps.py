#!/usr/bin/env python3
"""
Test Coverage Gap Analysis for erlmcp
Identifies modules without corresponding test files
"""

import os
import re
from pathlib import Path
from typing import Dict, List, Set, Tuple

def get_source_modules(base_path: str) -> Dict[str, List[str]]:
    """Get all source modules grouped by application"""
    modules = {}
    apps_dir = Path(base_path) / "apps"

    for app_dir in apps_dir.iterdir():
        if not app_dir.is_dir():
            continue

        src_dir = app_dir / "src"
        if not src_dir.exists():
            continue

        app_modules = []
        for erl_file in src_dir.rglob("*.erl"):
            # Get module name without path and extension
            module_name = erl_file.stem
            # Skip .app.src files
            if not module_name.endswith("_app") or erl_file.name == module_name + ".erl":
                app_modules.append(module_name)

        modules[app_dir.name] = sorted(app_modules)

    return modules

def get_test_modules(base_path: str) -> Dict[str, List[str]]:
    """Get all test modules grouped by application"""
    test_modules = {}
    apps_dir = Path(base_path) / "apps"

    for app_dir in apps_dir.iterdir():
        if not app_dir.is_dir():
            continue

        test_dir = app_dir / "test"
        if not test_dir.exists():
            continue

        app_tests = []
        for test_file in test_dir.rglob("*.erl"):
            # Get test module name
            test_name = test_file.stem
            app_tests.append(test_name)

        test_modules[app_dir.name] = sorted(app_tests)

    return test_modules

def identify_missing_tests(source_modules: Dict[str, List[str]],
                          test_modules: Dict[str, List[str]]) -> Dict[str, List[str]]:
    """Identify source modules without corresponding test files"""
    missing_tests = {}

    for app_name, modules in source_modules.items():
        app_tests = test_modules.get(app_name, [])

        # Extract base names from test files (remove _tests, _SUITE suffixes)
        tested_modules = set()
        for test_name in app_tests:
            # Handle various test naming patterns
            if test_name.endswith("_tests"):
                tested_modules.add(test_name[:-6])
            elif test_name.endswith("_SUITE"):
                tested_modules.add(test_name[:-6])
            elif "_tests" in test_name:
                # Handle names like erlmcp_auth_api_tests
                parts = test_name.split("_tests")[0]
                tested_modules.add(parts)

        # Find modules without tests
        missing = []
        for module in modules:
            # Skip supervisors, apps, and behaviors (often don't need dedicated tests)
            if module.endswith("_sup") or module.endswith("_app") or module.endswith("_behavior"):
                continue

            # Check if module has a corresponding test
            has_test = False
            for tested in tested_modules:
                if module.startswith(tested) or tested.startswith(module):
                    has_test = True
                    break

            if not has_test:
                missing.append(module)

        if missing:
            missing_tests[app_name] = sorted(missing)

    return missing_tests

def categorize_modules(modules: List[str]) -> Dict[str, List[str]]:
    """Categorize modules by type for prioritization"""
    categories = {
        "Core Protocol": [],
        "Session Management": [],
        "Security": [],
        "Transport": [],
        "Validation": [],
        "Observability": [],
        "Utilities": [],
        "Pricing/TCPS": [],
    }

    for module in modules:
        if any(x in module for x in ["client", "server", "registry", "json_rpc", "protocol"]):
            categories["Core Protocol"].append(module)
        elif any(x in module for x in ["session", "failover", "replicator"]):
            categories["Session Management"].append(module)
        elif any(x in module for x in ["auth", "secrets", "mtls", "security", "validator"]):
            categories["Security"].append(module)
        elif any(x in module for x in ["transport", "http", "tcp", "ws", "sse", "stdio"]):
            categories["Transport"].append(module)
        elif any(x in module for x in ["validator", "compliance", "spec_parser"]):
            categories["Validation"].append(module)
        elif any(x in module for x in ["metrics", "otel", "tracing", "chaos", "dashboard", "health"]):
            categories["Observability"].append(module)
        elif any(x in module for x in ["pricing", "tcps", "poka_yoke", "sla"]):
            categories["Pricing/TCPS"].append(module)
        else:
            categories["Utilities"].append(module)

    # Remove empty categories
    return {k: v for k, v in categories.items() if v}

def analyze_ct_suites(test_modules: Dict[str, List[str]]) -> Dict[str, int]:
    """Count Common Test suites per application"""
    ct_counts = {}
    for app_name, tests in test_modules.items():
        ct_count = sum(1 for t in tests if t.endswith("_SUITE"))
        ct_counts[app_name] = ct_count
    return ct_counts

def main():
    base_path = "/home/user/erlmcp"

    print("=" * 80)
    print("ERLMCP TEST COVERAGE GAP ANALYSIS")
    print("=" * 80)
    print()

    # Get all modules
    source_modules = get_source_modules(base_path)
    test_modules = get_test_modules(base_path)

    # Print summary
    print("MODULE SUMMARY BY APPLICATION")
    print("-" * 80)
    for app_name in sorted(source_modules.keys()):
        src_count = len(source_modules.get(app_name, []))
        test_count = len(test_modules.get(app_name, []))
        print(f"{app_name:30s} {src_count:3d} source modules | {test_count:3d} test files")
    print()

    # Identify missing tests
    missing_tests = identify_missing_tests(source_modules, test_modules)

    print("=" * 80)
    print("CRITICAL: MODULES WITHOUT CORRESPONDING TEST FILES")
    print("=" * 80)
    print()

    total_missing = 0
    for app_name in sorted(missing_tests.keys()):
        modules = missing_tests[app_name]
        total_missing += len(modules)

        print(f"\n{app_name.upper()}")
        print("-" * 80)

        # Categorize by type
        categorized = categorize_modules(modules)
        for category, mods in categorized.items():
            if mods:
                print(f"\n  {category}:")
                for mod in mods:
                    print(f"    - {mod}")

    print()
    print("=" * 80)
    print(f"TOTAL MODULES WITHOUT TESTS: {total_missing}")
    print("=" * 80)
    print()

    # Common Test analysis
    print("=" * 80)
    print("COMMON TEST SUITE ANALYSIS")
    print("=" * 80)
    print()

    ct_counts = analyze_ct_suites(test_modules)
    for app_name in sorted(ct_counts.keys()):
        count = ct_counts[app_name]
        print(f"{app_name:30s} {count:3d} Common Test suites")
    print()

    # Priority recommendations
    print("=" * 80)
    print("PRIORITY TESTING RECOMMENDATIONS (Chicago School TDD)")
    print("=" * 80)
    print()
    print("PRIORITY 1 (CRITICAL - Core Protocol, Security):")
    print("-" * 80)

    priority1_modules = []
    for app_name, modules in missing_tests.items():
        categorized = categorize_modules(modules)
        for category in ["Core Protocol", "Security", "Session Management"]:
            if category in categorized:
                for mod in categorized[category]:
                    priority1_modules.append(f"{app_name}/{mod}")

    for mod in sorted(priority1_modules):
        print(f"  - {mod}")

    print()
    print("PRIORITY 2 (HIGH - Transport, Validation):")
    print("-" * 80)

    priority2_modules = []
    for app_name, modules in missing_tests.items():
        categorized = categorize_modules(modules)
        for category in ["Transport", "Validation"]:
            if category in categorized:
                for mod in categorized[category]:
                    priority2_modules.append(f"{app_name}/{mod}")

    for mod in sorted(priority2_modules):
        print(f"  - {mod}")

    print()
    print("PRIORITY 3 (MEDIUM - Observability, Utilities):")
    print("-" * 80)

    priority3_modules = []
    for app_name, modules in missing_tests.items():
        categorized = categorize_modules(modules)
        for category in ["Observability", "Utilities"]:
            if category in categorized:
                for mod in categorized[category]:
                    priority3_modules.append(f"{app_name}/{mod}")

    for mod in sorted(priority3_modules):
        print(f"  - {mod}")

    print()
    print("=" * 80)
    print("ADDITIONAL TESTING GAPS")
    print("=" * 80)
    print()
    print("1. Missing Property-Based Tests (Proper):")
    print("   - Transport protocol encoding/decoding")
    print("   - Session state transitions")
    print("   - Registry concurrent operations")
    print("   - JSON-RPC roundtrip properties")
    print()
    print("2. Missing Integration Tests (Common Test):")
    print("   - Multi-transport coordination scenarios")
    print("   - Cluster failover and split-brain handling")
    print("   - End-to-end MCP protocol workflows")
    print("   - Chaos engineering with recovery validation")
    print()
    print("3. Missing Error Path Tests:")
    print("   - Network partition recovery")
    print("   - Memory exhaustion scenarios")
    print("   - Malformed message handling")
    print("   - Timeout and deadline handling")
    print()
    print("=" * 80)
    print("CHICAGO SCHOOL TDD COMPLIANCE CHECK")
    print("=" * 80)
    print()
    print("Requirements:")
    print("  ✓ Use real gen_servers (no mocks)")
    print("  ✓ Test all interfaces (JSON-RPC, stdio, HTTP, WebSocket, TCP)")
    print("  ✓ State-based verification (observable behavior)")
    print("  ✓ Real process spawning and coordination")
    print("  ✓ 80%+ coverage minimum (85%+ for core modules)")
    print()

if __name__ == "__main__":
    main()
