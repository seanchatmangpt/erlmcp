#!/usr/bin/env python3
"""
Blue-Green Deployment System for erlmcp v3
Implements zero-downtime deployments
"""

import argparse
import json
import os
import sys
import subprocess
import time
import logging
from typing import Dict, List, Optional
from dataclasses import dataclass
from pathlib import Path

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

@dataclass
class BlueGreenConfig:
    """Configuration for blue-green deployment"""
    namespace: str
    image: str
    timeout: int = 900
    health_threshold: float = 95.0
    readiness_threshold: float = 95.0
    traffic_grace_period: int = 30
    blue_service: str = "erlmcp-blue"
    green_service: str = "erlmcp-green"
    active_service: str = "erlmcp"

    def validate(self):
        """Validate configuration"""
        if self.timeout < 300:
            raise ValueError("timeout must be at least 300 seconds")
        if not 0 <= self.health_threshold <= 100:
            raise ValueError("health_threshold must be between 0 and 100")

class BlueGreenDeployer:
    """Main blue-green deployment class"""

    def __init__(self, config: BlueGreenConfig):
        self.config = config
        self.setup_logging()

    def setup_logging(self):
        """Setup logging"""
        handler = logging.StreamHandler()
        handler.setLevel(logging.INFO)
        formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
        handler.setFormatter(formatter)
        logger.addHandler(handler)

    def validate_prerequisites(self):
        """Validate deployment prerequisites"""
        logger.info("Validating prerequisites...")

        # Check kubectl
        try:
            subprocess.run(['kubectl', 'version', '--client'],
                         check=True, capture_output=True)
        except (subprocess.CalledProcessError, FileNotFoundError):
            logger.error("kubectl not found or not configured")
            sys.exit(1)

        # Check namespace exists
        try:
            result = subprocess.run(
                ['kubectl', 'get', 'namespace', self.config.namespace],
                check=True, capture_output=True
            )
        except subprocess.CalledProcessError:
            logger.error(f"Namespace {self.config.namespace} not found")
            sys.exit(1)

        logger.info("Prerequisites validated")

    def get_active_color(self) -> str:
        """Get currently active color (blue or green)"""
        try:
            result = subprocess.run(
                ['kubectl', 'get', 'service', self.config.active_service,
                 '-n', self.config.namespace,
                 '-o', 'jsonpath={.spec.selector.color}'],
                check=True, capture_output=True, text=True
            )
            return result.stdout.strip()
        except subprocess.CalledProcessError:
            logger.warning("Active service selector not found, assuming blue")
            return "blue"

    def prepare_deployment(self) -> str:
        """Prepare new deployment"""
        logger.info("Preparing deployment...")

        # Determine which color to deploy
        active_color = self.get_active_color()
        new_color = "green" if active_color == "blue" else "blue"
        logger.info(f"Current active: {active_color}, deploying: {new_color}")

        # Create deployment manifest
        deployment_manifest = self._create_deployment_manifest(new_color)
        manifest_file = f'/tmp/deployment-{new_color}.yaml'

        with open(manifest_file, 'w') as f:
            json.dump(deployment_manifest, f, indent=2)

        try:
            subprocess.run(
                ['kubectl', 'apply', '-f', manifest_file],
                check=True
            )
            logger.info(f"{new_color.capitalize()} deployment created")
            return new_color
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to create {new_color} deployment: {e}")
            raise

    def _create_deployment_manifest(self, color: str) -> Dict:
        """Create deployment manifest for specified color"""
        return {
            "apiVersion": "apps/v1",
            "kind": "Deployment",
            "metadata": {
                "name": f"erlmcp-{color}",
                "namespace": self.config.namespace,
                "labels": {
                    "app": "erlmcp",
                    "color": color
                }
            },
            "spec": {
                "replicas": 3,
                "selector": {
                    "matchLabels": {
                        "app": "erlmcp",
                        "color": color
                    }
                },
                "template": {
                    "metadata": {
                        "labels": {
                            "app": "erlmcp",
                            "color": color
                        },
                        "annotations": {
                            "prometheus.io/scrape": "true",
                            "prometheus.io/port": "8080"
                        }
                    },
                    "spec": {
                        "containers": [{
                            "name": "erlmcp",
                            "image": self.config.image,
                            "ports": [{
                                "containerPort": 8080,
                                "name": "http"
                            }],
                            "env": [
                                {"name": "ENVIRONMENT", "value": self.config.namespace},
                                {"name": "COLOR", "value": color}
                            ],
                            "livenessProbe": {
                                "httpGet": {
                                    "path": "/health",
                                    "port": 8080
                                },
                                "initialDelaySeconds": 30,
                                "periodSeconds": 10
                            },
                            "readinessProbe": {
                                "httpGet": {
                                    "path": "/ready",
                                    "port": 8080
                                },
                                "initialDelaySeconds": 5,
                                "periodSeconds": 5
                            },
                            "resources": {
                                "requests": {
                                    "memory": "256Mi",
                                    "cpu": "250m"
                                },
                                "limits": {
                                    "memory": "512Mi",
                                    "cpu": "500m"
                                }
                            }
                        }],
                        "imagePullSecrets": [{
                            "name": "regcred"
                        }],
                        "terminationGracePeriodSeconds": 60
                    }
                },
                "strategy": {
                    "type": "RollingUpdate",
                    "rollingUpdate": {
                        "maxSurge": "25%",
                        "maxUnavailable": "25%"
                    }
                }
            }
        }

    def wait_for_deployment(self, color: str) -> bool:
        """Wait for deployment to be ready"""
        logger.info(f"Waiting for {color} deployment to be ready...")

        start_time = time.time()
        while time.time() - start_time < self.config.timeout:
            try:
                result = subprocess.run(
                    ['kubectl', ' rollout', 'status',
                     f'deployment/erlmcp-{color}',
                     '-n', self.config.namespace,
                     '--timeout=30s'],
                    check=True,
                    capture_output=True,
                    text=True
                )

                if "successfully rolled out" in result.stdout:
                    logger.info(f"{color.capitalize()} deployment is ready")
                    return True

            except subprocess.CalledProcessError as e:
                if "timed out waiting for condition" in str(e):
                    logger.info(f"Still waiting for {color} deployment...")
                else:
                    logger.error(f"Error checking {color} deployment: {e}")
                    return False

            time.sleep(10)

        logger.error(f"{color.capitalize()} deployment timeout")
        return False

    def create_service(self, color: str):
        """Create service for specified color"""
        logger.info(f"Creating {color} service...")

        service_manifest = {
            "apiVersion": "v1",
            "kind": "Service",
            "metadata": {
                "name": f"erlmcp-{color}",
                "namespace": self.config.namespace,
                "labels": {
                    "app": "erlmcp",
                    "color": color
                }
            },
            "spec": {
                "selector": {
                    "app": "erlmcp",
                    "color": color
                },
                "ports": [{
                    "port": 80,
                    "targetPort": 8080,
                    "name": "http"
                }],
                "type": "ClusterIP"
            }
        }

        service_file = f'/tmp/service-{color}.yaml'
        with open(service_file, 'w') as f:
            json.dump(service_manifest, f, indent=2)

        try:
            subprocess.run(
                ['kubectl', 'apply', '-f', service_file],
                check=True
            )
            logger.info(f"{color.capitalize()} service created")
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to create {color} service: {e}")
            raise

    def validate_deployment(self, color: str) -> bool:
        """Validate deployment health"""
        logger.info(f"Validating {color} deployment...")

        # Check pod health
        try:
            result = subprocess.run(
                ['kubectl', 'get', 'pods',
                 '-n', self.config.namespace,
                 '-l', f'app=erlmcp,color={color}',
                 '-o', 'jsonpath={.items[*].status.containerStatuses[*].ready}'],
                check=True, capture_output=True, text=True
            )

            ready_statuses = result.stdout.strip().split()
            ready_count = sum(1 for status in ready_statuses if status.lower() == 'true')
            total_count = len(ready_statuses)

            health_percentage = (ready_count / total_count) * 100 if total_count > 0 else 0

            logger.info(f"{color.capitalize()} health: {health_percentage:.1f}% ({ready_count}/{total_count} ready)")

            if health_percentage < self.config.health_threshold:
                logger.error(f"{color.capitalize()} health below threshold")
                return False

        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to check {color} health: {e}")
            return False

        # Check endpoint readiness
        try:
            result = subprocess.run(
                ['kubectl', 'get', 'endpoints', f'erlmcp-{color}',
                 '-n', self.config.namespace',
                 '-o', 'jsonpath={.subsets[*].addresses}'],
                check=True, capture_output=True, text=True
            )

            if result.stdout.strip():
                logger.info(f"{color.capitalize()} endpoints are ready")
            else:
                logger.error(f"{color.capitalize()} endpoints not ready")
                return False

        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to check {color} endpoints: {e}")
            return False

        # Run readiness check
        try:
            service_ip = subprocess.run(
                ['kubectl', 'get', 'service', f'erlmcp-{color}',
                 '-n', self.config.namespace',
                 '-o', 'jsonpath={.spec.clusterIP}'],
                check=True, capture_output=True, text=True
            ).stdout.strip()

            if service_ip:
                response = subprocess.run(
                    ['curl', '-f', '-s', '--max-time', '10',
                     f'http://{service_ip}:8080/ready'],
                    check=True,
                    capture_output=True
                )
                logger.info(f"{color.capitalize()} readiness check passed")
            else:
                logger.error(f"{color.capitalize()} service IP not found")
                return False

        except subprocess.CalledProcessError as e:
            logger.error(f"{color.capitalize()} readiness check failed: {e}")
            return False

        return True

    def switch_traffic(self, new_color: str):
        """Switch traffic to new color"""
        logger.info(f"Switching traffic to {new_color}...")

        # Update active service selector
        try:
            subprocess.run(
                ['kubectl', 'patch', 'service', self.config.active_service,
                 '-n', self.config.namespace,
                 '--patch', json.dumps({
                     "spec": {
                         "selector": {
                             "color": new_color
                         }
                     }
                 }),
                 '--type=merge'],
                check=True
            )
            logger.info(f"Traffic switched to {new_color}")

        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to switch traffic: {e}")
            raise

        # Wait for traffic to switch
        logger.info(f"Waiting {self.config.traffic_grace_period}s for traffic to stabilize...")
        time.sleep(self.config.traffic_grace_period)

    def verify_traffic_switch(self, new_color: str) -> bool:
        """Verify traffic has switched correctly"""
        logger.info("Verifying traffic switch...")

        try:
            # Check active service endpoint
            result = subprocess.run(
                ['kubectl', 'get', 'endpoints', self.config.active_service,
                 '-n', self.config.namespace',
                 '-o', 'jsonpath={.subsets[*].addresses[*].ip}'],
                check=True, capture_output=True, text=True
            )

            endpoint_ips = result.stdout.strip().split()
            logger.info(f"Active service endpoints: {endpoint_ips}")

            # Verify endpoints belong to new color
            if endpoint_ips:
                # Get pods for new color
                pod_result = subprocess.run(
                    ['kubectl', 'get', 'pods',
                     '-n', self.config.namespace,
                     '-l', f'app=erlmcp,color={new_color}',
                     '-o', 'jsonpath={.items[*].status.podIP}'],
                    check=True, capture_output=True, text=True
                )

                new_color_ips = pod_result.stdout.strip().split()

                # Check if all endpoints match new color pods
                if set(endpoint_ips).issubset(set(new_color_ips)):
                    logger.info("✅ Traffic switch verified successfully")
                    return True
                else:
                    logger.error("❌ Traffic switch verification failed")
                    return False
            else:
                logger.error("No endpoints found for active service")
                return False

        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to verify traffic switch: {e}")
            return False

    def cleanup_deployment(self, old_color: str):
        """Clean up old deployment"""
        logger.info(f"Cleaning up {old_color} deployment...")

        try:
            # Scale down old deployment
            subprocess.run(
                ['kubectl', 'scale', 'deployment',
                 f'erlmcp-{old_color}',
                 '--replicas=0',
                 '-n', self.config.namespace],
                check=True
            )

            # Wait for pods to terminate
            time.sleep(30)

            # Delete old deployment
            subprocess.run(
                ['kubectl', 'delete', 'deployment',
                 f'erlmcp-{old_color}',
                 '-n', self.config.namespace',
                 '--ignore-not-found=True'],
                check=True
            )

            # Delete old service
            subprocess.run(
                ['kubectl', 'delete', 'service',
                 f'erlmcp-{old_color}',
                 '-n', self.config.namespace',
                 '--ignore-not-found=True'],
                check=True
            )

            logger.info(f"✅ {old_color.capitalize()} deployment cleaned up")

        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to clean up {old_color}: {e}")

    def deploy(self):
        """Execute full blue-green deployment"""
        try:
            self.validate_prerequisites()

            # Prepare new deployment
            new_color = self.prepare_deployment()

            # Wait for deployment
            if not self.wait_for_deployment(new_color):
                logger.error(f"{new_color} deployment not ready")
                sys.exit(1)

            # Create service
            self.create_service(new_color)

            # Validate deployment
            if not self.validate_deployment(new_color):
                logger.error(f"{new_color} validation failed")
                sys.exit(1)

            # Switch traffic
            self.switch_traffic(new_color)

            # Verify traffic switch
            if not self.verify_traffic_switch(new_color):
                logger.error("Traffic switch verification failed")
                sys.exit(1)

            # Clean up old deployment
            old_color = "blue" if new_color == "green" else "green"
            self.cleanup_deployment(old_color)

            logger.info("✅ Blue-green deployment completed successfully")

        except Exception as e:
            logger.error(f"❌ Blue-green deployment failed: {e}")
            sys.exit(1)

def main():
    parser = argparse.ArgumentParser(description="Blue-Green Deployment for erlmcp v3")
    parser.add_argument("--namespace", "-n", required=True,
                       help="Kubernetes namespace")
    parser.add_argument("--image", "-i", required=True,
                       help="Image to deploy")
    parser.add_argument("--timeout", "-t", type=int, default=900,
                       help="Timeout in seconds (900)")
    parser.add_argument("--health-threshold", type=float, default=95.0,
                       help="Health threshold percentage (95.0)")
    parser.add_argument("--readiness-threshold", type=float, default=95.0,
                       help="Readiness threshold percentage (95.0)")
    parser.add_argument("--grace-period", type=int, default=30,
                       help="Traffic switch grace period in seconds (30)")
    parser.add_argument("--dry-run", action="store_true",
                       help="Show what would be done")

    args = parser.parse_args()

    # Create configuration
    config = BlueGreenConfig(
        namespace=args.namespace,
        image=args.image,
        timeout=args.timeout,
        health_threshold=args.health_threshold,
        readiness_threshold=args.readiness_threshold,
        traffic_grace_period=args.grace_period
    )

    # Validate configuration
    config.validate()

    if args.dry_run:
        logger.info("Dry run mode - showing deployment plan:")
        logger.info(f"- Namespace: {config.namespace}")
        logger.info(f"- Image: {config.image}")
        logger.info(f"- Timeout: {config.timeout}s")
        logger.info(f"- Health threshold: {config.health_threshold}%")
    else:
        # Execute deployment
        deployer = BlueGreenDeployer(config)
        deployer.deploy()

if __name__ == "__main__":
    main()