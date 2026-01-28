#!/usr/bin/env python3
"""
Module Inventory Extractor for erlmcp v2
Enumerates all modules, detects duplicates/broken files, classifies, extracts exports
Output: inventory.json
"""

import os
import re
import json
from pathlib import Path
from typing import Dict, List, Tuple, Set
from collections import defaultdict
from datetime import datetime


class ModuleInventory:
    """Extract and classify Erlang modules from src/ directory."""

    FAMILY_PATTERNS = {
        "tcps": r"^tcps",
        "transport": r"erlmcp_transport_",
        "cli": r"erlmcp_cli_",
        "metrics": r"erlmcp_metrics",
        "monitoring": r"erlmcp_monitor",
        "pricing": r"erlmcp_pricing",
        "configuration": r"erlmcp_config",
        "server": r"erlmcp_server",
        "client": r"erlmcp_client",
        "resilience": r"erlmcp_(circuit|backpressure|graceful|rate_limiter)",
        "http": r"erlmcp_http",
        "health": r"erlmcp_health",
        "pooling": r"erlmcp_(pool|buffer)",
        "memory": r"erlmcp_memory",
        "profiling": r"erlmcp_profile",
        "benchmarking": r"erlmcp_bench",
        "queuing": r"erlmcp_queue",
        "receipts": r"erlmcp_receipt",
        "registry": r"erlmcp_registry",
        "sessions": r"erlmcp_session",
        "routing": r"erlmcp_routing",
        "json_rpc": r"erlmcp_json_rpc",
        "logging": r"erlmcp_logging",
        "tracing": r"erlmcp_trace",
        "reporting": r"erlmcp_report",
        "planning": r"erlmcp_plan",
        "supervision": r"erlmcp_sup",
    }

    def __init__(self, src_dir: str = "src"):
        self.src_dir = src_dir
        self.active_files: List[str] = []
        self.broken_files: List[str] = []
        self.duplicate_files: List[str] = []

    def classify_file(self, filepath: str) -> str:
        """Classify file as active, broken, or duplicate."""
        basename = os.path.basename(filepath)

        # Check for broken marker
        if basename.endswith(".broken"):
            return "broken"

        # Check for backup/duplicate markers
        if basename.endswith((".backup", ".bak")):
            return "duplicate"

        # Check for variant names
        if any(
            variant in basename for variant in ["_new", "_refactored", "_copy", "_old"]
        ):
            return "duplicate"

        return "active"

    def scan_modules(self) -> None:
        """Scan src/ directory and categorize all files."""
        # Scan for both .erl and .erl.* files
        all_files = []
        for pattern in ["*.erl", "*.erl.*"]:
            all_files.extend(Path(self.src_dir).rglob(pattern))

        # Remove duplicates and convert to strings
        all_files = sorted(set(str(f) for f in all_files))

        for file_str in all_files:
            classification = self.classify_file(file_str)

            if classification == "active":
                self.active_files.append(file_str)
            elif classification == "broken":
                self.broken_files.append(file_str)
            else:  # duplicate
                self.duplicate_files.append(file_str)

        # Sort for consistency
        self.active_files.sort()
        self.broken_files.sort()
        self.duplicate_files.sort()

    def extract_family(self, module_name: str) -> str:
        """Determine family for a module based on naming pattern."""
        if module_name == "erlmcp":
            return "core"
        if module_name == "rdf_utils":
            return "utilities"

        for family, pattern in self.FAMILY_PATTERNS.items():
            if re.search(pattern, module_name):
                return family

        return "utilities"

    def extract_exports(self, filepath: str) -> List[str]:
        """Extract function exports from .erl file."""
        try:
            with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
                content = f.read()
        except Exception:
            return []

        exports = []

        # Find -export([...]) declarations
        export_pattern = r"-export\(\[([^\]]+)\]\)"
        matches = re.findall(export_pattern, content)

        for match in matches:
            # Split by comma and extract function names
            functions = [f.strip() for f in match.split(",")]
            exports.extend([f for f in functions if f])

        return exports

    def analyze_module(self, filepath: str) -> Dict:
        """Extract metadata for a single module."""
        module_name = os.path.basename(filepath).replace(".erl", "")
        exports = self.extract_exports(filepath)
        size_bytes = os.path.getsize(filepath)
        family = self.extract_family(module_name)

        return {
            "file": filepath,
            "module": module_name,
            "family": family,
            "exports": exports,
            "num_exports": len(exports),
            "size_bytes": size_bytes,
        }

    def build_inventory(self) -> Dict:
        """Build complete inventory structure."""
        # Analyze active modules
        modules = [self.analyze_module(f) for f in self.active_files]

        # Group by family
        families = defaultdict(list)
        for module in modules:
            families[module["family"]].append(module)

        # Convert defaultdict to regular dict
        families = dict(families)

        # Add broken file metadata
        broken_modules = [
            {
                "file": f,
                "module": os.path.basename(f).replace(".erl", ""),
                "family": "BROKEN",
                "exports": [],
                "size_bytes": os.path.getsize(f),
            }
            for f in self.broken_files
        ]

        # Add duplicate file metadata
        duplicate_modules = [
            {
                "file": f,
                "module": os.path.basename(f).replace(".erl", ""),
                "family": "DUPLICATE",
                "exports": [],
                "size_bytes": os.path.getsize(f),
            }
            for f in self.duplicate_files
        ]

        return {
            "timestamp": int(datetime.now().timestamp()),
            "src_directory": self.src_dir,
            "summary": {
                "total_modules": len(self.active_files),
                "num_families": len(families),
                "num_broken": len(self.broken_files),
                "num_duplicates": len(self.duplicate_files),
            },
            "families": families,
            "broken": broken_modules,
            "duplicates": duplicate_modules,
            "all_modules": modules,
        }

    def write_json(self, output_file: str) -> bool:
        """Write inventory to JSON file."""
        try:
            inventory = self.build_inventory()
            with open(output_file, "w") as f:
                json.dump(inventory, f, indent=2)
            return True
        except Exception as e:
            print(f"Error writing {output_file}: {e}")
            return False

    def report_summary(self, inventory: Dict) -> None:
        """Print summary report."""
        summary = inventory["summary"]
        print(f"✓ Inventory written to inventory.json")
        print(f"  Total modules: {summary['total_modules']}")
        print(f"  Families: {summary['num_families']}")
        print(f"  Duplicates: {summary['num_duplicates']}")
        print(f"  Broken: {summary['num_broken']}")


def main():
    """Main entry point."""
    extractor = ModuleInventory("src")
    extractor.scan_modules()

    if extractor.write_json("inventory.json"):
        inventory = extractor.build_inventory()
        extractor.report_summary(inventory)
    else:
        exit(1)


if __name__ == "__main__":
    main()
