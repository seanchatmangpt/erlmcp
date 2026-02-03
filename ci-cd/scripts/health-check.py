#!/usr/bin/env python3
"""
Health Check Script for erlmcp v3
Performs comprehensive health checks for deployed instances
"""

import argparse
import json
import os
import sys
import subprocess
import time
import requests
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass
from datetime import datetime, timedelta
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

@dataclass
class HealthConfig:
    """Health check configuration"""
    endpoint: str
    timeout: int = 30
    expected_status: int = 200
    health_path: str = "/health"
    readiness_path: str = "/ready"
    metrics_path: str = "/metrics"
    max_failures: int = 3
    check_interval: int = 10
    retry_count: int = 3

    def validate(self):
        """Validate configuration"""
        if self.timeout < 5:
            raise ValueError("Timeout must be at least 5 seconds")
        if self.max_failures < 1:
            raise ValueError("Max failures must be at least 1")
        if self.check_interval < 1:
            raise ValueError("Check interval must be at least 1 second")

class HealthChecker:
    """Health check implementation"""

    def __init__(self, config: HealthConfig):
        self.config = config
        self.setup_logging()
        self.failure_count = 0
        self.last_success = None

    def setup_logging(self):
        """Setup logging"""
        handler = logging.StreamHandler()
        handler.setLevel(logging.INFO)
        formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
        handler.setFormatter(formatter)
        logger.addHandler(handler)

    def check_endpoint(self, endpoint: str, expected_status: int) -> Tuple[bool, Dict]:
        """Check HTTP endpoint"""
        try:
            response = requests.get(
                endpoint,
                timeout=self.config.timeout,
                verify=False  # Consider verifying in production
            )

            if response.status_code == expected_status:
                try:
                    return True, response.json()
                except:
                    return True, {"status": "ok", "message": "Healthy"}
            else:
                return False, {
                    "status": "error",
                    "code": response.status_code,
                    "message": f"Expected {expected_status}, got {response.status_code}"
                }

        except requests.exceptions.Timeout:
            return False, {
                "status": "timeout",
                "message": f"Request timed out after {self.config.timeout} seconds"
            }
        except requests.exceptions.ConnectionError:
            return False, {
                "status": "connection_error",
                "message": "Failed to connect to endpoint"
            }
        except Exception as e:
            return False, {
                "status": "error",
                "message": str(e)
            }

    def check_readiness(self) -> bool:
        """Check application readiness"""
        endpoint = f"{self.config.endpoint}{self.config.readiness_path}"
        success, data = self.check_endpoint(endpoint, 200)

        if success:
            if isinstance(data, dict) and "ready" in data:
                return data["ready"]
            return True

        logger.error(f"Readiness check failed: {data}")
        return False

    def check_liveness(self) -> bool:
        """Check application liveness"""
        endpoint = f"{self.config.endpoint}{self.config.health_path}"
        success, data = self.check_endpoint(endpoint, 200)

        if success:
            if isinstance(data, dict) and "status" in data:
                return data["status"] == "ok"
            return True

        logger.error(f"Liveness check failed: {data}")
        return False

    def check_metrics(self) -> bool:
        """Check metrics endpoint"""
        endpoint = f"{self.config.endpoint}{self.config.metrics_path}"
        success, data = self.check_endpoint(endpoint, 200)

        if success:
            if isinstance(data, dict):
                # Check for expected metrics
                required_metrics = [
                    "erlmcp_requests_total",
                    "erlmcp_response_time_seconds",
                    "erlmcp_errors_total"
                ]

                for metric in required_metrics:
                    if metric not in data:
                        logger.warning(f"Metric {metric} not found")

                return True
            return True

        logger.error(f"Metrics check failed: {data}")
        return False

    def check_performance(self) -> Tuple[bool, Dict]:
        """Check performance metrics"""
        try:
            # Get metrics
            endpoint = f"{self.config.endpoint}{self.config.metrics_path}"
            response = requests.get(endpoint, timeout=self.config.timeout)

            if response.status_code == 200:
                metrics = response.text

                # Parse metrics (simplified)
                performance_data = {
                    "response_time": self._extract_metric(metrics, "erlmcp_response_time_seconds_bucket"),
                    "request_rate": self._extract_metric(metrics, "erlmcp_requests_total"),
                    "error_rate": self._extract_rate(metrics, "erlmcp_errors_total"),
                    "active_connections": self._extract_metric(metrics, "erlmcp_connections_active")
                }

                # Check thresholds
                thresholds = {
                    "max_response_time": 0.1,  # 100ms
                    "max_error_rate": 0.01,    # 1%
                    "min_throughput": 100      # requests per second
                }

                performance_ok = True
                for metric, threshold in thresholds.items():
                    value = performance_data.get(metric, 0)
                    if metric in ["max_response_time", "max_error_rate"] and value > threshold:
                        performance_ok = False
                        logger.warning(f"{metric}: {value} > {threshold}")
                    elif metric == "min_throughput" and value < threshold:
                        performance_ok = False
                        logger.warning(f"{metric}: {value} < {threshold}")

                return performance_ok, performance_data

            return False, {"error": "Failed to get metrics"}

        except Exception as e:
            return False, {"error": str(e)}

    def _extract_metric(self, metrics: str, name: str) -> float:
        """Extract metric value from Prometheus text format"""
        lines = metrics.split('\n')
        for line in lines:
            if line.startswith(name) and '{' in line:
                # Extract value from metric line
                parts = line.split(' ')
                if len(parts) > 1:
                    try:
                        return float(parts[1])
                    except:
                        return 0.0
        return 0.0

    def _extract_rate(self, metrics: str, name: str) -> float:
        """Extract rate from metrics"""
        total = self._extract_metric(metrics, f"{name}_total")
        sum_seconds = self._extract_metric(metrics, f"{name}_seconds_sum")

        if sum_seconds > 0:
            return total / sum_seconds
        return 0.0

    def check_database(self) -> bool:
        """Check database connectivity"""
        try:
            # Execute health query
            result = subprocess.run([
                'curl', '-s', f"{self.config.endpoint}/health/database"],
                timeout=self.config.timeout,
                capture_output=True,
                text=True
            )

            if result.returncode == 0:
                health_data = json.loads(result.stdout)
                return health_data.get("database", {}).get("status") == "healthy"

            return False

        except Exception as e:
            logger.error(f"Database check failed: {e}")
            return False

    def check_redis(self) -> bool:
        """Check Redis connectivity"""
        try:
            result = subprocess.run([
                'curl', '-s', f"{self.config.endpoint}/health/redis"],
                timeout=self.config.timeout,
                capture_output=True,
                text=True
            )

            if result.returncode == 0:
                health_data = json.loads(result.stdout)
                return health_data.get("redis", {}).get("status") == "healthy"

            return False

        except Exception as e:
            logger.error(f"Redis check failed: {e}")
            return False

    def run_checks(self) -> Dict:
        """Run all health checks"""
        results = {
            "timestamp": datetime.now().isoformat(),
            "endpoint": self.config.endpoint,
            "checks": {},
            "overall": True
        }

        # Liveness check
        liveness_ok = self.check_liveness()
        results["checks"]["liveness"] = {
            "status": "pass" if liveness_ok else "fail",
            "timestamp": datetime.now().isoformat()
        }

        # Readiness check
        readiness_ok = self.check_readiness()
        results["checks"]["readiness"] = {
            "status": "pass" if readiness_ok else "fail",
            "timestamp": datetime.now().isoformat()
        }

        # Metrics check
        metrics_ok = self.check_metrics()
        results["checks"]["metrics"] = {
            "status": "pass" if metrics_ok else "fail",
            "timestamp": datetime.now().isoformat()
        }

        # Performance check
        perf_ok, perf_data = self.check_performance()
        results["checks"]["performance"] = {
            "status": "pass" if perf_ok else "fail",
            "data": perf_data,
            "timestamp": datetime.now().isoformat()
        }

        # Database check
        db_ok = self.check_database()
        results["checks"]["database"] = {
            "status": "pass" if db_ok else "fail",
            "timestamp": datetime.now().isoformat()
        }

        # Redis check
        redis_ok = self.check_redis()
        results["checks"]["redis"] = {
            "status": "pass" if redis_ok else "fail",
            "timestamp": datetime.now().isoformat()
        }

        # Overall status
        overall_ok = all([
            liveness_ok,
            readiness_ok,
            metrics_ok,
            perf_ok,
            db_ok,
            redis_ok
        ])

        results["overall"] = overall_ok

        # Update failure count
        if not overall_ok:
            self.failure_count += 1
        else:
            self.failure_count = 0
            self.last_success = datetime.now()

        return results

    def monitor(self, duration: int = 300):
        """Monitor endpoint for specified duration"""
        logger.info(f"Starting health monitoring for {duration} seconds...")

        start_time = datetime.now()
        end_time = start_time + timedelta(seconds=duration)

        while datetime.now() < end_time:
            results = self.run_checks()

            if not results["overall"]:
                logger.error("Health check failed!")
                if self.failure_count >= self.config.max_failures:
                    logger.error(f"Maximum failures ({self.config.max_failures}) reached!")
                    return False
            else:
                logger.info("All health checks passing")

            # Wait for next check
            time.sleep(self.config.check_interval)

        logger.info("Health monitoring completed")
        return True

def main():
    parser = argparse.ArgumentParser(description="Health Check for erlmcp v3")
    parser.add_argument("--endpoint", "-e", required=True,
                       help="Endpoint to check (e.g., http://localhost:8080)")
    parser.add_argument("--timeout", "-t", type=int, default=30,
                       help="Request timeout in seconds (30)")
    parser.add_argument("--max-failures", "-f", type=int, default=3,
                       help="Maximum failures before failure (3)")
    parser.add_argument("--check-interval", "-i", type=int, default=10,
                       help="Check interval in seconds (10)")
    parser.add_argument("--duration", "-d", type=int, default=300,
                       help="Monitoring duration in seconds (300)")
    parser.add_argument("--retry", "-r", type=int, default=3,
                       help="Number of retries per check (3)")
    parser.add_argument("--json", action="store_true",
                       help="Output results in JSON format")

    args = parser.parse_args()

    # Create configuration
    config = HealthConfig(
        endpoint=args.endpoint,
        timeout=args.timeout,
        max_failures=args.max_failures,
        check_interval=args.check_interval,
        retry_count=args.retry
    )

    # Validate configuration
    config.validate()

    # Create health checker
    checker = HealthChecker(config)

    if args.json:
        # Run single check and output JSON
        results = checker.run_checks()
        print(json.dumps(results, indent=2))
        sys.exit(0 if results["overall"] else 1)
    else:
        # Run monitoring
        success = checker.monitor(args.duration)
        sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()