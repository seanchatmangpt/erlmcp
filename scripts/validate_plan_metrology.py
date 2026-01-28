#!/usr/bin/env python3
"""
Validate erlmcp Plan Metrology v1.5.0
Validates all plan files against canonical metrology standards
"""

import json
import os
import sys
from pathlib import Path
from typing import Dict, List, Tuple

# Colors
RED = '\033[0;31m'
GREEN = '\033[0;32m'
YELLOW = '\033[1;33m'
NC = '\033[0m'

SCRIPT_DIR = Path(__file__).parent
PROJECT_ROOT = SCRIPT_DIR.parent
PLANS_DIR = PROJECT_ROOT / "plans"
WORKLOADS_DIR = PROJECT_ROOT / "bench" / "workloads"
ENVIRONMENTS_DIR = PROJECT_ROOT / "bench" / "environments"
GLOSSARY = PROJECT_ROOT / "docs" / "metrology" / "METRICS_GLOSSARY.md"

violations = 0

def log_error(msg: str):
    print(f"    {RED}✗ VIOLATION: {msg}{NC}")
    global violations
    violations += 1

def log_success(msg: str):
    print(f"    {GREEN}✓ {msg}{NC}")

def log_warning(msg: str):
    print(f"    {YELLOW}⚠ WARNING: {msg}{NC}")

def validate_prohibited_patterns(plan_file: Path, data: Dict) -> bool:
    """Check for prohibited ambiguous patterns"""
    print("  Checking prohibited patterns...")
    has_violations = False
    
    plan_str = plan_file.read_text()
    
    # Check for ambiguous patterns in string
    if '"req/s"' in plan_str or '"req_s"' in plan_str or 'throughput_req_s' in plan_str:
        log_error("Ambiguous 'req/s' or 'req_s' found (use msg_per_s)")
        has_violations = True
    
    if '"MiB/conn"' in plan_str or '"MB/conn"' in plan_str:
        log_error("Ambiguous 'MiB/conn' found (specify: heap, state, or RSS)")
        has_violations = True
    
    if not has_violations:
        log_success("No prohibited patterns found")
    
    return not has_violations

def get_all_workload_ids(data: Dict) -> List[str]:
    """Recursively extract all workload_id values"""
    workload_ids = []
    
    def extract(obj):
        if isinstance(obj, dict):
            if 'workload_id' in obj:
                workload_ids.append(obj['workload_id'])
            for value in obj.values():
                extract(value)
        elif isinstance(obj, list):
            for item in obj:
                extract(item)
    
    extract(data)
    return list(set(workload_ids))

def validate_workload_refs(plan_file: Path, data: Dict) -> bool:
    """Validate all workload_id references exist"""
    print("  Checking workload references...")
    has_violations = False
    
    workload_ids = get_all_workload_ids(data)
    
    if not workload_ids:
        log_error("No workload_id references found")
        return False
    
    for wid in workload_ids:
        workload_file = WORKLOADS_DIR / f"{wid}.json"
        if not workload_file.exists():
            log_error(f"workload_id '{wid}' not found: {workload_file}")
            has_violations = True
        else:
            log_success(f"workload_id '{wid}' exists")
    
    if not has_violations:
        log_success("All workload references valid")
    
    return not has_violations

def validate_metrology_fields(plan_file: Path, data: Dict) -> bool:
    """Validate required metrology fields"""
    print("  Checking required metrology fields...")
    has_violations = False
    
    # Check metrology_compliance section
    if 'metrology_compliance' not in data:
        log_error("Missing metrology_compliance section")
        return False
    
    mc = data['metrology_compliance']
    version = mc.get('version', '')
    if version != 'v1.5.0':
        log_error(f"metrology_compliance.version must be 'v1.5.0' (found: '{version}')")
        has_violations = True
    else:
        log_success("metrology_compliance.version = v1.5.0")
    
    # Check throughput structure
    if 'envelope' in data and 'throughput' in data['envelope']:
        tp = data['envelope']['throughput']
        if isinstance(tp, dict):
            if tp.get('unit') != 'msg_per_s':
                log_error("throughput.unit must be 'msg_per_s'")
                has_violations = True
            else:
                log_success("throughput uses msg_per_s")
            
            if 'workload_id' not in tp:
                log_error("throughput missing workload_id")
                has_violations = True
            
            if 'transport' not in tp:
                log_error("throughput missing transport")
                has_violations = True
    
    # Check memory structure
    if 'memory' in data:
        mem = data['memory']
        if 'per_connection_heap_mib' not in mem:
            log_error("memory missing per_connection_heap_mib")
            has_violations = True
        
        if 'per_node_total_rss_mib' not in mem:
            log_error("memory missing per_node_total_rss_mib")
            has_violations = True
        
        if 'workload_id' not in mem:
            log_error("memory missing workload_id")
            has_violations = True
    
    if not has_violations:
        log_success("All required metrology fields present")
    
    return not has_violations

def validate_plan(plan_file: Path) -> bool:
    """Validate a single plan file"""
    print("─" * 63)
    print(f"Validating: {plan_file.name}")
    print("─" * 63)
    
    try:
        with open(plan_file) as f:
            data = json.load(f)
    except json.JSONDecodeError as e:
        print(f"{RED}✗ INVALID JSON: {e}{NC}")
        global violations
        violations += 1
        return False
    
    passed = True
    passed &= validate_prohibited_patterns(plan_file, data)
    passed &= validate_workload_refs(plan_file, data)
    passed &= validate_metrology_fields(plan_file, data)
    
    print()
    return passed

def main():
    print("═" * 63)
    print("  erlmcp Plan Metrology Validator v1.5.0")
    print("═" * 63)
    print()
    
    # Check prerequisites
    if not GLOSSARY.exists():
        print(f"{RED}✗ ERROR: Metrics glossary not found: {GLOSSARY}{NC}")
        return 1
    
    print("✓ Prerequisites OK")
    print()
    print(f"Validating plan files in: {PLANS_DIR}")
    print()
    
    # Validate each plan file
    all_passed = True
    for plan_file in sorted(PLANS_DIR.glob("*.plan.json")):
        all_passed &= validate_plan(plan_file)
    
    # Summary
    print("═" * 63)
    print("  VALIDATION SUMMARY")
    print("═" * 63)
    print()
    
    if violations == 0:
        print(f"{GREEN}✓✓✓ ALL VALIDATIONS PASSED ✓✓✓{NC}")
        print()
        print("Plans are compliant with metrology v1.5.0")
        print("Total violations: 0")
        return 0
    else:
        print(f"{RED}✗✗✗ VALIDATION FAILED ✗✗✗{NC}")
        print()
        print(f"Total violations: {violations}")
        print()
        print("Please fix violations and re-run validation.")
        print("See: docs/metrology/METRICS_GLOSSARY.md")
        return 1

if __name__ == '__main__':
    sys.exit(main())
