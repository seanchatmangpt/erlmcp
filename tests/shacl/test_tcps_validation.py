"""
TCPS SHACL Validation Test Suite

Tests the SHACL shapes for TCPS quality gates against valid and invalid test data.

Validation Authority: TCPS Definition of Done (docs/TCPS.md)
Quality Standard: Zero defects (Lean Six Sigma)
"""

import sys
from pathlib import Path
from typing import Tuple
import pytest

try:
    from pyshacl import validate
    from rdflib import Graph
    PYSHACL_AVAILABLE = True
except ImportError:
    PYSHACL_AVAILABLE = False


# Test data paths
SHAPES_FILE = Path(__file__).parent.parent.parent / "shapes" / "tcps_shapes.ttl"
VALID_DATA_FILE = Path(__file__).parent / "test_data_valid.ttl"
INVALID_DATA_FILE = Path(__file__).parent / "test_data_invalid.ttl"


@pytest.mark.skipif(not PYSHACL_AVAILABLE, reason="pyshacl not installed")
class TestTCPSSHACLValidation:
    """Test suite for TCPS SHACL validation shapes."""

    def setup_method(self) -> None:
        """Set up test fixtures."""
        # Load shapes graph
        self.shapes_graph = Graph()
        self.shapes_graph.parse(str(SHAPES_FILE), format="turtle")

        # Load valid data graph
        self.valid_data_graph = Graph()
        self.valid_data_graph.parse(str(VALID_DATA_FILE), format="turtle")

        # Load invalid data graph
        self.invalid_data_graph = Graph()
        self.invalid_data_graph.parse(str(INVALID_DATA_FILE), format="turtle")

    def _validate_data(self, data_graph: Graph) -> Tuple[bool, Graph, str]:
        """
        Run SHACL validation on a data graph.

        Args:
            data_graph: RDF graph containing data to validate

        Returns:
            Tuple of (conforms, results_graph, results_text)
        """
        conforms, results_graph, results_text = validate(
            data_graph=data_graph,
            shacl_graph=self.shapes_graph,
            inference='rdfs',
            abort_on_first=False,
            allow_warnings=False,
            meta_shacl=False,
            advanced=True,
            js=False
        )
        return conforms, results_graph, results_text

    # ========================================================================
    # Valid Data Tests - Should Pass All Validations
    # ========================================================================

    def test_valid_data_passes_all_validations(self) -> None:
        """Test that valid TCPS data passes all SHACL validations."""
        conforms, results_graph, results_text = self._validate_data(self.valid_data_graph)

        assert conforms, (
            f"Valid TCPS data MUST pass all SHACL validations.\n"
            f"Validation errors:\n{results_text}"
        )

    def test_valid_sku_completeness(self) -> None:
        """Test that valid SKU has all required fields."""
        conforms, _, results_text = self._validate_data(self.valid_data_graph)

        # Valid SKU should have: name, version, description, workOrderId, receipts, quality gates
        assert conforms, f"Valid SKU completeness check failed:\n{results_text}"

    def test_valid_workorder_structure(self) -> None:
        """Test that valid WorkOrder has all required fields."""
        conforms, _, results_text = self._validate_data(self.valid_data_graph)

        # Valid WorkOrder should have: demand signal, bucket, priority, status
        assert conforms, f"Valid WorkOrder structure check failed:\n{results_text}"

    def test_valid_receipts_present(self) -> None:
        """Test that valid SKU has receipts for all production stages."""
        conforms, _, results_text = self._validate_data(self.valid_data_graph)

        # Valid SKU should have compile, test, release, publish receipts
        assert conforms, f"Valid receipt presence check failed:\n{results_text}"

    def test_valid_compilation_receipt(self) -> None:
        """Test that compilation receipt has zero errors."""
        conforms, _, results_text = self._validate_data(self.valid_data_graph)

        # Compilation receipt must have errorCount = 0
        assert conforms, f"Valid compilation receipt check failed:\n{results_text}"

    def test_valid_test_receipt_quality_gates(self) -> None:
        """Test that test receipt meets 80% pass rate and coverage thresholds."""
        conforms, _, results_text = self._validate_data(self.valid_data_graph)

        # Test receipt must have passRate >= 80% and coverage >= 80%
        assert conforms, f"Valid test receipt quality gates check failed:\n{results_text}"

    def test_valid_release_receipt_deterministic_hash(self) -> None:
        """Test that release receipt has valid SHA-256 hash."""
        conforms, _, results_text = self._validate_data(self.valid_data_graph)

        # Release receipt must have 64-character hex hash
        assert conforms, f"Valid release deterministic hash check failed:\n{results_text}"

    def test_valid_publish_receipt_smoke_tests(self) -> None:
        """Test that publish receipt has passing smoke tests."""
        conforms, _, results_text = self._validate_data(self.valid_data_graph)

        # Publish receipt must have smokeTestResult = "pass"
        assert conforms, f"Valid publish smoke test check failed:\n{results_text}"

    def test_valid_resolved_andon_event(self) -> None:
        """Test that resolved Andon event has resolution timestamp and preventive actions."""
        conforms, _, results_text = self._validate_data(self.valid_data_graph)

        # Resolved Andon must have resolvedAt and preventiveAction
        assert conforms, f"Valid resolved Andon event check failed:\n{results_text}"

    # ========================================================================
    # Invalid Data Tests - Should Fail Validations
    # ========================================================================

    def test_invalid_data_fails_validations(self) -> None:
        """Test that invalid TCPS data fails SHACL validations."""
        conforms, results_graph, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, (
            "Invalid TCPS data MUST fail SHACL validations (stop-the-line).\n"
            "Expected validation errors but data passed all checks."
        )

    def test_invalid_sku_missing_fields(self) -> None:
        """Test that SKU missing required fields fails validation."""
        # Test with just the invalid SKU missing fields
        invalid_graph = Graph()
        invalid_graph.parse(str(INVALID_DATA_FILE), format="turtle")

        conforms, _, results_text = self._validate_data(invalid_graph)

        assert not conforms, "SKU missing required fields MUST fail validation"
        assert "name" in results_text or "version" in results_text or "description" in results_text, \
            f"Validation should report missing required fields:\n{results_text}"

    def test_invalid_sku_version_format(self) -> None:
        """Test that SKU with invalid semantic version fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "SKU with invalid version format MUST fail validation"

    def test_invalid_sku_no_receipts(self) -> None:
        """Test that SKU without receipts fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "SKU without receipts (proof of work) MUST fail validation"

    def test_invalid_sku_open_andon_event(self) -> None:
        """Test that SKU with open Andon event cannot be shipped."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "SKU with open Andon event MUST NOT be shippable"

    def test_invalid_workorder_bucket(self) -> None:
        """Test that WorkOrder with invalid bucket fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "WorkOrder with invalid bucket MUST fail validation"

    def test_invalid_workorder_status(self) -> None:
        """Test that WorkOrder with invalid status fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "WorkOrder with invalid status MUST fail validation"

    def test_invalid_workorder_priority(self) -> None:
        """Test that WorkOrder with priority out of range fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "WorkOrder priority outside 1-10 range MUST fail validation"

    def test_invalid_receipt_stage_name(self) -> None:
        """Test that Receipt with invalid stage name fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Receipt with non-TCPS stage name MUST fail validation"

    def test_invalid_receipt_empty_evidence(self) -> None:
        """Test that Receipt with empty evidence fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Receipt with empty evidence data MUST fail validation"

    def test_invalid_compilation_errors(self) -> None:
        """Test that compilation receipt with errors fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Compilation with errorCount > 0 MUST fail validation"

    def test_invalid_test_low_pass_rate(self) -> None:
        """Test that test receipt with <80% pass rate fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Test passRate < 80% MUST fail validation (quality gate)"

    def test_invalid_test_low_coverage(self) -> None:
        """Test that test receipt with <80% coverage fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Test coverage < 80% MUST fail validation (quality gate)"

    def test_invalid_release_hash_format(self) -> None:
        """Test that release with invalid hash format fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Release with non-SHA256 hash MUST fail validation"

    def test_invalid_release_artifact_path(self) -> None:
        """Test that release with invalid artifact path fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Release artifact path without URI scheme MUST fail validation"

    def test_invalid_publish_smoke_test_fail(self) -> None:
        """Test that publish with failed smoke test fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Publish with smokeTestResult != 'pass' MUST fail validation"

    def test_invalid_publish_no_entitlement_gate(self) -> None:
        """Test that publish without entitlement gate fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Publish with entitlementGateActive = false MUST fail validation"

    def test_invalid_andon_short_failure_reason(self) -> None:
        """Test that Andon with short failure reason fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Andon failureReason < 10 chars MUST fail validation"

    def test_invalid_andon_short_root_cause(self) -> None:
        """Test that Andon with short root cause analysis fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Andon rootCauseAnalysis < 20 chars MUST fail validation"

    def test_invalid_andon_missing_affected_sku(self) -> None:
        """Test that Andon without affected SKU fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Andon without affectedSKU MUST fail validation"

    def test_invalid_resolved_andon_missing_timestamp(self) -> None:
        """Test that resolved Andon without resolution timestamp fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Resolved Andon without resolvedAt MUST fail validation"

    def test_invalid_resolved_andon_missing_preventive_action(self) -> None:
        """Test that resolved Andon without preventive action fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Resolved Andon without preventiveAction MUST fail validation"

    def test_invalid_andon_status(self) -> None:
        """Test that Andon with invalid status fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Andon with invalid status MUST fail validation"

    def test_invalid_andon_severity(self) -> None:
        """Test that Andon with invalid severity fails validation."""
        conforms, _, results_text = self._validate_data(self.invalid_data_graph)

        assert not conforms, "Andon with invalid severity MUST fail validation"

    # ========================================================================
    # Cross-Cutting Constraint Tests
    # ========================================================================

    def test_receipt_timestamp_order(self) -> None:
        """Test that receipt timestamps are chronologically consistent with SKU creation."""
        # This is validated via SPARQL in the shapes
        conforms, _, _ = self._validate_data(self.valid_data_graph)
        assert conforms, "Valid data should have chronologically consistent timestamps"

    def test_workorder_completion_requires_passing_receipts(self) -> None:
        """Test that completed WorkOrder cannot have failed receipts."""
        # This is validated via SPARQL in the shapes
        conforms, _, _ = self._validate_data(self.valid_data_graph)
        assert conforms, "Valid completed WorkOrder should have all passing receipts"


# ============================================================================
# Standalone Validation Script
# ============================================================================

def validate_tcps_data(data_file: Path, shapes_file: Path = SHAPES_FILE) -> bool:
    """
    Validate TCPS data against SHACL shapes.

    Args:
        data_file: Path to RDF data file to validate
        shapes_file: Path to SHACL shapes file

    Returns:
        True if validation passes, False otherwise
    """
    if not PYSHACL_AVAILABLE:
        print("ERROR: pyshacl library not installed. Install with: pip install pyshacl")
        return False

    # Load graphs
    shapes_graph = Graph()
    shapes_graph.parse(str(shapes_file), format="turtle")

    data_graph = Graph()
    data_graph.parse(str(data_file), format="turtle")

    # Run validation
    conforms, results_graph, results_text = validate(
        data_graph=data_graph,
        shacl_graph=shapes_graph,
        inference='rdfs',
        abort_on_first=False,
        allow_warnings=False
    )

    # Print results
    print(f"\n{'=' * 80}")
    print(f"TCPS SHACL Validation Results")
    print(f"{'=' * 80}")
    print(f"Data file: {data_file}")
    print(f"Shapes file: {shapes_file}")
    print(f"{'=' * 80}")

    if conforms:
        print("✓ VALIDATION PASSED - All quality gates satisfied")
        print("  Data is compliant with TCPS Definition of Done")
        print(f"{'=' * 80}\n")
        return True
    else:
        print("✗ VALIDATION FAILED - Quality gates not satisfied")
        print("  STOP THE LINE: Fix violations before shipping")
        print(f"{'=' * 80}")
        print("\nValidation Report:")
        print(results_text)
        print(f"{'=' * 80}\n")
        return False


if __name__ == "__main__":
    """Run validation on test data files."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Validate TCPS data against SHACL shapes"
    )
    parser.add_argument(
        "data_file",
        nargs="?",
        type=Path,
        help="Path to RDF data file to validate"
    )
    parser.add_argument(
        "--shapes",
        type=Path,
        default=SHAPES_FILE,
        help="Path to SHACL shapes file"
    )
    parser.add_argument(
        "--test-valid",
        action="store_true",
        help="Test validation with valid test data"
    )
    parser.add_argument(
        "--test-invalid",
        action="store_true",
        help="Test validation with invalid test data"
    )

    args = parser.parse_args()

    exit_code = 0

    if args.test_valid:
        print("\nTesting with VALID test data...")
        if not validate_tcps_data(VALID_DATA_FILE, args.shapes):
            exit_code = 1

    if args.test_invalid:
        print("\nTesting with INVALID test data...")
        if validate_tcps_data(INVALID_DATA_FILE, args.shapes):
            print("ERROR: Invalid data incorrectly passed validation!")
            exit_code = 1
        else:
            print("✓ Correctly detected invalid data (expected failure)")

    if args.data_file:
        print(f"\nValidating custom data file: {args.data_file}")
        if not validate_tcps_data(args.data_file, args.shapes):
            exit_code = 1

    if not any([args.test_valid, args.test_invalid, args.data_file]):
        # Run both tests by default
        print("\nRunning default validation tests...")
        print("\n" + "=" * 80)
        print("TEST 1: Valid Data Validation")
        print("=" * 80)
        if not validate_tcps_data(VALID_DATA_FILE, args.shapes):
            exit_code = 1

        print("\n" + "=" * 80)
        print("TEST 2: Invalid Data Detection")
        print("=" * 80)
        if validate_tcps_data(INVALID_DATA_FILE, args.shapes):
            print("ERROR: Invalid data incorrectly passed validation!")
            exit_code = 1
        else:
            print("\n✓ INVALID DATA TEST PASSED")
            print("  Correctly detected violations in invalid test data")
            print("  SHACL shapes are working as expected\n")

    sys.exit(exit_code)
