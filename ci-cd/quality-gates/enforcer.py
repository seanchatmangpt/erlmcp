#!/usr/bin/env python3
"""
Quality Gate Enforcer for erlmcp v3
Enforces quality policies across the CI/CD pipeline
"""

import argparse
import json
import os
import sys
import yaml
import subprocess
from pathlib import Path
from typing import Dict, List, Any, Optional
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class QualityGateEnforcer:
    """Main class for enforcing quality gates"""

    def __init__(self, config_file: str):
        """Initialize with configuration"""
        self.config = self.load_config(config_file)
        self.results = {}
        self.passed = True

    def load_config(self, config_file: str) -> Dict[str, Any]:
        """Load configuration from YAML file"""
        try:
            with open(config_file, 'r') as f:
                return yaml.safe_load(f)
        except FileNotFoundError:
            logger.error(f"Configuration file not found: {config_file}")
            sys.exit(1)

    def check_compilation(self) -> Dict[str, Any]:
        """Check compilation status"""
        logger.info("Checking compilation...")
        result = {
            "category": "compilation",
            "passed": False,
            "details": {}
        }

        try:
            # Run rebar3 compile
            output = subprocess.run(
                ["rebar3", "compile"],
                capture_output=True,
                text=True,
                check=False
            )

            if output.returncode == 0:
                result["passed"] = True
                result["details"] = {"status": "success"}
            else:
                result["details"] = {
                    "status": "failed",
                    "errors": output.stderr
                }

        except Exception as e:
            result["details"] = {
                "status": "error",
                "message": str(e)
            }

        self.results["compilation"] = result
        return result

    def check_tests(self) -> Dict[str, Any]:
        """Check test results"""
        logger.info("Checking tests...")
        result = {
            "category": "testing",
            "passed": False,
            "details": {
                "eunit": {},
                "ct": {},
                "coverage": {}
            }
        }

        # Check EUnit tests
        try:
            output = subprocess.run(
                ["rebar3", "eunit"],
                capture_output=True,
                text=True,
                check=False
            )

            if output.returncode == 0:
                result["details"]["eunit"]["passed"] = True
                result["details"]["eunit"]["coverage"] = self._check_eunit_coverage()
            else:
                result["details"]["eunit"]["passed"] = False
                result["details"]["eunit"]["errors"] = output.stderr
        except Exception as e:
            result["details"]["eunit"]["error"] = str(e)

        # Check CT tests
        try:
            output = subprocess.run(
                ["rebar3", "ct"],
                capture_output=True,
                text=True,
                check=False
            )

            if output.returncode == 0:
                result["details"]["ct"]["passed"] = True
                result["details"]["ct"]["coverage"] = self._check_ct_coverage()
            else:
                result["details"]["ct"]["passed"] = False
                result["details"]["ct"]["errors"] = output.stderr
        except Exception as e:
            result["details"]["ct"]["error"] = str(e)

        # Overall test status
        eunit_passed = result["details"]["eunit"].get("passed", False)
        ct_passed = result["details"]["ct"].get("passed", False)

        result["passed"] = eunit_passed and ct_passed

        self.results["testing"] = result
        return result

    def _check_eunit_coverage(self) -> float:
        """Check EUnit coverage"""
        try:
            # Parse coverage report
            output = subprocess.run(
                ["rebar3", "cover"],
                capture_output=True,
                text=True,
                check=False
            )

            # Extract coverage percentage (simplified)
            if "Covered" in output.stdout:
                # Extract coverage percentage
                lines = output.stdout.split('\n')
                for line in lines:
                    if "Covered" in line and "%" in line:
                        coverage = float(line.split('%')[0].split()[-1])
                        return coverage

            return 0.0
        except:
            return 0.0

    def _check_ct_coverage(self) -> float:
        """Check CT coverage"""
        try:
            # Similar implementation as EUnit coverage check
            output = subprocess.run(
                ["rebar3", "cover"],
                capture_output=True,
                text=True,
                check=False
            )

            # Parse CT coverage report
            if "Covered" in output.stdout:
                lines = output.stdout.split('\n')
                for line in lines:
                    if "Covered" in line and "%" in line:
                        coverage = float(line.split('%')[0].split()[-1])
                        return coverage

            return 0.0
        except:
            return 0.0

    def check_security(self) -> Dict[str, Any]:
        """Check security scans"""
        logger.info("Checking security...")
        result = {
            "category": "security",
            "passed": False,
            "details": {
                "vulnerabilities": {},
                "secrets": {},
                "licenses": {}
            }
        }

        # Run vulnerability scan
        try:
            output = subprocess.run(
                ["trivy", ".", "--format", "json"],
                capture_output=True,
                text=True,
                check=False
            )

            # Parse Trivy output
            try:
                trivy_results = json.loads(output.stdout)
                vulnerabilities = self._analyze_vulnerabilities(trivy_results)
                result["details"]["vulnerabilities"] = vulnerabilities
            except json.JSONDecodeError:
                result["details"]["vulnerabilities"]["error"] = "Failed to parse Trivy output"
        except Exception as e:
            result["details"]["vulnerabilities"]["error"] = str(e)

        # Check for secrets
        result["details"]["secrets"] = self._check_secrets()

        # Check licenses
        result["details"]["licenses"] = self._check_licenses()

        # Overall security status
        vuln_critical = result["details"]["vulnerabilities"].get("critical", 0)
        vuln_high = result["details"]["vulnerabilities"].get("high", 0)
        secrets_found = len(result["details"]["secrets"].get("found", [])) > 0

        # Check against thresholds
        thresholds = self.config["security"]["thresholds"]

        security_passed = (
            vuln_critical <= thresholds["critical"] and
            vuln_high <= thresholds["high"] and
            not secrets_found
        )

        result["passed"] = security_passed

        self.results["security"] = result
        return result

    def _analyze_vulnerabilities(self, trivy_results: Dict) -> Dict[str, int]:
        """Analyze vulnerability results"""
        vulnerabilities = {"critical": 0, "high": 0, "medium": 0, "low": 0}

        for result in trivy_results.get("Results", []):
            for vuln in result.get("Vulnerabilities", []):
                severity = vuln.get("Severity", "UNKNOWN").lower()
                if severity in vulnerabilities:
                    vulnerabilities[severity] += 1

        return vulnerabilities

    def _check_secrets(self) -> Dict[str, Any]:
        """Check for hardcoded secrets"""
        result = {"found": [], "patterns": []}

        # Check common secret patterns
        secret_patterns = self.config["security"]["secrets"]["patterns"]

        for pattern in secret_patterns:
            try:
                output = subprocess.run(
                    ["grep", "-r", pattern, "."],
                    capture_output=True,
                    text=True,
                    check=False
                )

                if output.returncode == 0:
                    result["found"].append({
                        "pattern": pattern,
                        "files": output.stdout.strip().split('\n')
                    })
                    result["patterns"].append(pattern)
            except Exception as e:
                result["error"] = str(e)

        return result

    def _check_licenses(self) -> Dict[str, Any]:
        """Check license compliance"""
        result = {"status": "unknown", "violations": []}

        try:
            # Run license check (simplified)
            output = subprocess.run(
                ["mix", "deps.audit"],
                capture_output=True,
                text=True,
                check=False
            )

            if output.returncode == 0:
                result["status"] = "compliant"
            else:
                result["status"] = "non-compliant"
                result["violations"] = output.stderr.split('\n')

        except Exception as e:
            result["error"] = str(e)

        return result

    def check_performance(self) -> Dict[str, Any]:
        """Check performance metrics"""
        logger.info("Checking performance...")
        result = {
            "category": "performance",
            "passed": False,
            "details": {}
        }

        try:
            # Run performance benchmarks
            output = subprocess.run(
                ["make", "benchmark-quick"],
                capture_output=True,
                text=True,
                check=False
            )

            if output.returncode == 0:
                # Parse benchmark results
                perf_data = self._parse_performance_results(output.stdout)
                result["details"] = perf_data

                # Check against thresholds
                thresholds = self.config["performance"]["thresholds"]

                perf_passed = (
                    perf_data.get("response_time", 0) <= thresholds["response_time"] and
                    perf_data.get("throughput", 0) >= thresholds["throughput"]
                )

                result["passed"] = perf_passed
            else:
                result["details"]["error"] = output.stderr

        except Exception as e:
            result["details"]["error"] = str(e)

        self.results["performance"] = result
        return result

    def _parse_performance_results(self, output: str) -> Dict[str, Any]:
        """Parse performance benchmark output"""
        results = {}

        # Example parsing (adjust based on actual output format)
        lines = output.split('\n')
        for line in lines:
            if "Response time" in line:
                results["response_time"] = float(line.split(':')[1].strip().split()[0])
            elif "Throughput" in line:
                results["throughput"] = float(line.split(':')[1].strip().split()[0])
            elif "Memory usage" in line:
                results["memory_usage"] = float(line.split(':')[1].strip().split()[0])

        return results

    def check_code_quality(self) -> Dict[str, Any]:
        """Check code quality metrics"""
        logger.info("Checking code quality...")
        result = {
            "category": "code_quality",
            "passed": False,
            "details": {}
        }

        # Run Dialyzer
        try:
            output = subprocess.run(
                ["rebar3", "dialyzer"],
                capture_output=True,
                text=True,
                check=False
            )

            dialyzer_passed = output.returncode == 0
            result["details"]["dialyzer"] = {
                "passed": dialyzer_passed,
                "warnings": 0 if dialyzer_passed else len(output.stdout.split('\n'))
            }
        except Exception as e:
            result["details"]["dialyzer"] = {"error": str(e)}

        # Run Xref
        try:
            output = subprocess.run(
                ["rebar3", "xref"],
                capture_output=True,
                text=True,
                check=False
            )

            xref_passed = output.returncode == 0
            result["details"]["xref"] = {
                "passed": xref_passed,
                "undefined": 0 if xref_passed else len(output.stdout.split('\n'))
            }
        except Exception as e:
            result["details"]["xref"] = {"error": str(e)}

        # Overall code quality
        dialyzer_ok = result["details"]["dialyzer"].get("passed", False)
        xref_ok = result["details"]["xref"].get("passed", False)

        result["passed"] = dialyzer_ok and xref_ok

        self.results["code_quality"] = result
        return result

    def enforce_policies(self, environment: str = "development") -> bool:
        """Enforce policies for specific environment"""
        logger.info(f"Enforcing policies for environment: {environment}")

        env_policies = self.config["environments"].get(environment, {})
        required_categories = env_policies.get("required_categories", [])
        blocking_categories = env_policies.get("blocking_categories", [])

        # Run all checks
        if "compilation" in required_categories:
            self.check_compilation()

        if "testing" in required_categories:
            self.check_tests()

        if "security" in required_categories:
            self.check_security()

        if "performance" in required_categories:
            self.check_performance()

        if "code_quality" in required_categories:
            self.check_code_quality()

        # Determine overall status
        overall_passed = True
        for category, result in self.results.items():
            if category in blocking_categories and not result["passed"]:
                overall_passed = False
                logger.error(f"Blocking category failed: {category}")

        self.passed = overall_passed
        return overall_passed

    def generate_report(self) -> Dict[str, Any]:
        """Generate quality gate report"""
        report = {
            "timestamp": datetime.datetime.now().isoformat(),
            "environment": self.config.get("default_environment", "development"),
            "overall_passed": self.passed,
            "categories": self.results,
            "summary": {
                "total_categories": len(self.results),
                "passed_categories": sum(1 for r in self.results.values() if r["passed"]),
                "failed_categories": sum(1 for r in self.results.values() if not r["passed"])
            }
        }

        return report

    def send_notifications(self, report: Dict[str, Any]):
        """Send notifications for quality gate results"""
        if self.config["notifications"]["slack"]["enabled"]:
            self._send_slack_notification(report)

        if self.config["notifications"]["email"]["enabled"]:
            self._send_email_notification(report)

    def _send_slack_notification(self, report: Dict[str, Any]):
        """Send Slack notification"""
        # Implementation would use Slack webhook
        logger.info("Sending Slack notification...")

    def _send_email_notification(self, report: Dict[str, Any]):
        """Send email notification"""
        # Implementation would use email service
        logger.info("Sending email notification...")

    def save_report(self, report: Dict[str, Any], output_file: str):
        """Save quality gate report"""
        with open(output_file, 'w') as f:
            json.dump(report, f, indent=2)
        logger.info(f"Report saved to: {output_file}")


def main():
    parser = argparse.ArgumentParser(description="Quality Gate Enforcer for erlmcp v3")
    parser.add_argument("--config", "-c", required=True, help="Configuration file path")
    parser.add_argument("--environment", "-e", default="development",
                       help="Environment to enforce policies for")
    parser.add_argument("--output", "-o", help="Output file for report")

    args = parser.parse_args()

    # Initialize enforcer
    enforcer = QualityGateEnforcer(args.config)

    # Enforce policies
    success = enforcer.enforce_policies(args.environment)

    # Generate and save report
    report = enforcer.generate_report()

    if args.output:
        enforcer.save_report(report, args.output)

    # Send notifications
    if not success:
        enforcer.send_notifications(report)

    # Exit with appropriate code
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()