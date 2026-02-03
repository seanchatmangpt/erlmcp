#!/usr/bin/env python3
"""
Canary Deployment System for erlmcp v3
Implements progressive deployment with automated rollback
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
class CanaryConfig:
    """Configuration for canary deployment"""
    namespace: str
    image: str
    canary_percent: int = 10
    timeout: int = 300
    health_threshold: float = 95.0
    error_threshold: int = 1
    monitoring_interval: int = 30
    metrics_path: str = "/tmp/metrics"

    def validate(self):
        """Validate configuration"""
        if not 0 < self.canary_percent < 100:
            raise ValueError("canary_percent must be between 1 and 99")
        if self.timeout < 60:
            raise ValueError("timeout must be at least 60 seconds")
        if not 0 <= self.health_threshold <= 100:
            raise ValueError("health_threshold must be between 0 and 100")

class CanaryDeployer:
    """Main canary deployment class"""

    def __init__(self, config: CanaryConfig):
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

    def get_current_deployment(self) -> Dict:
        """Get current deployment status"""
        try:
            result = subprocess.run(
                ['kubectl', 'get', 'deployment', 'erlmcp',
                 '-n', self.config.namespace,
                 '-o', 'json'],
                check=True, capture_output=True, text=True
            )
            return json.loads(result.stdout)
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to get deployment: {e}")
            raise

    def create_canary_deployment(self) -> str:
        """Create canary deployment"""
        logger.info("Creating canary deployment...")

        # Get current deployment
        current_deployment = self.get_current_deployment()
        current_replicas = current_deployment['spec']['replicas']
        canary_replicas = max(1, int(current_replicas * self.config.canary_percent / 100))

        # Create canary deployment manifest
        canary_manifest = {
            "apiVersion": "apps/v1",
            "kind": "Deployment",
            "metadata": {
                "name": "erlmcp-canary",
                "namespace": self.config.namespace,
                "labels": {
                    "app": "erlmcp",
                    "version": "canary",
                    "deployment-type": "canary"
                },
                "annotations": {
                    "canary.percentage": str(self.config.canary_percent),
                    "canary.original-image": current_deployment['spec']['template']['spec']['containers'][0]['image']
                }
            },
            "spec": {
                "replicas": canary_replicas,
                "selector": {
                    "matchLabels": {
                        "app": "erlmcp",
                        "version": "canary"
                    }
                },
                "template": current_deployment['spec']['template'].copy(),
                "strategy": {
                    "type": "RollingUpdate",
                    "rollingUpdate": {
                        "maxSurge": "25%",
                        "maxUnavailable": "25%"
                    }
                }
            }
        }

        # Update image in canary template
        canary_manifest['spec']['template']['spec']['containers'][0]['image'] = self.config.image
        canary_manifest['spec']['template']['spec']['containers'][0]['name'] = 'erlmcp-canary'
        canary_manifest['spec']['template']['metadata']['labels']['version'] = 'canary'

        # Apply canary deployment
        manifest_file = '/tmp/canary-deployment.yaml'
        with open(manifest_file, 'w') as f:
            json.dump(canary_manifest, f, indent=2)

        try:
            subprocess.run(
                ['kubectl', 'apply', '-f', manifest_file],
                check=True
            )
            logger.info(f"Canary deployment created with {canary_replicas} replicas")
            return manifest_file
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to create canary deployment: {e}")
            raise

    def create_service_split(self):
        """Create service split configuration"""
        logger.info("Creating service split...")

        # Get current service
        try:
            result = subprocess.run(
                ['kubectl', 'get', 'service', 'erlmcp',
                 '-n', self.config.namespace,
                 '-o', 'json'],
                check=True, capture_output=True, text=True
            )
            service = json.loads(result.stdout)
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to get service: {e}")
            raise

        # Create canary service
        canary_service = {
            "apiVersion": "v1",
            "kind": "Service",
            "metadata": {
                "name": "erlmcp-canary",
                "namespace": self.config.namespace,
                "labels": {
                    "app": "erlmcp",
                    "version": "canary"
                }
            },
            "spec": {
                "selector": {
                    "app": "erlmcp",
                    "version": "canary"
                },
                "ports": service['spec']['ports'],
                "type": "ClusterIP"
            }
        }

        service_file = '/tmp/canary-service.yaml'
        with open(service_file, 'w') as f:
            json.dump(canary_service, f, indent=2)

        try:
            subprocess.run(
                ['kubectl', 'apply', '-f', service_file],
                check=True
            )
            logger.info("Canary service created")
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to create canary service: {e}")
            raise

    def configure_traffic_split(self):
        """Configure traffic split using Istio or Ingress"""
        logger.info("Configuring traffic split...")

        # Check if Istio is available
        try:
            subprocess.run(['istioctl', 'version'],
                         check=True, capture_output=True)
            self._use_istio_traffic_split()
        except (subprocess.CalledProcessError, FileNotFoundError):
            logger.info("Istio not found, using nginx ingress")
            self._use_nginx_traffic_split()

    def _use_istio_traffic_split(self):
        """Use Istio for traffic splitting"""
        canary_rule = {
            "apiVersion": "networking.istio.io/v1alpha3",
            "kind": "VirtualService",
            "metadata": {
                "name": "erlmcp",
                "namespace": self.config.namespace
            },
            "spec": {
                "hosts": ["erlmcp.example.com"],
                "http": [{
                    "route": [
                        {
                            "destination": {
                                "host": "erlmcp.default.svc.cluster.local",
                                "subset": "stable"
                            },
                            "weight": 100 - self.config.canary_percent
                        },
                        {
                            "destination": {
                                "host": "erlmcp.default.svc.cluster.local",
                                "subset": "canary"
                            },
                            "weight": self.config.canary_percent
                        }
                    ]
                }]
            }
        }

        rule_file = '/tmp/istio-virtualservice.yaml'
        with open(rule_file, 'w') as f:
            json.dump(canary_rule, f, indent=2)

        try:
            subprocess.run(
                ['kubectl', 'apply', '-f', rule_file],
                check=True
            )
            logger.info("Istio traffic split configured")
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to configure Istio traffic: {e}")
            raise

    def _use_nginx_traffic_split(self):
        """Use nginx ingress for traffic splitting"""
        # This would require nginx ingress annotations
        logger.warning("Nginx traffic split not fully implemented")

    def monitor_canary(self) -> bool:
        """Monitor canary deployment health"""
        logger.info(f"Monitoring canary for {self.config.timeout} seconds...")

        start_time = time.time()
        error_count = 0

        while time.time() - start_time < self.config.timeout:
            # Check canary pods
            try:
                result = subprocess.run(
                    ['kubectl', 'get', 'pods',
                     '-n', self.config.namespace,
                     '-l', 'app=erlmcp,version=canary',
                     '-o', 'json'],
                    check=True, capture_output=True, text=True
                )
                pods = json.loads(result.stdout)

                # Get pod statuses
                ready_pods = 0
                for pod in pods['items']:
                    for container in pod['status']['containerStatuses']:
                        if container['ready']:
                            ready_pods += 1

                total_pods = len(pods['items'])
                if total_pods > 0:
                    health_percent = (ready_pods / total_pods) * 100
                else:
                    health_percent = 0

                logger.info(f"Canary health: {health_percent:.1f}% ({ready_pods}/{total_pods} pods ready)")

                # Check health threshold
                if health_percent < self.config.health_threshold:
                    logger.error(f"Canary health below threshold: {health_percent:.1f}% < {self.config.health_threshold}%")
                    error_count += 1
                else:
                    error_count = 0

                # Check error threshold
                if error_count >= self.config.error_threshold:
                    logger.error(f"Error threshold exceeded: {error_count} errors")
                    return False

            except subprocess.CalledProcessError as e:
                logger.error(f"Failed to check canary pods: {e}")
                error_count += 1

            time.sleep(self.config.monitoring_interval)

        logger.info("Canary monitoring completed successfully")
        return True

    def run_performance_test(self) -> bool:
        """Run performance test against canary"""
        logger.info("Running performance test against canary...")

        try:
            # Get canary service endpoint
            result = subprocess.run(
                ['kubectl', 'get', 'service', 'erlmcp-canary',
                 '-n', self.config.namespace,
                 '-o', 'jsonpath={.spec.clusterIP}'],
                check=True, capture_output=True, text=True
            )
            canary_ip = result.stdout.strip()

            # Run performance test
            cmd = [
                'wrk', '-t4', '-c100', '-d30s',
                f'http://{canary_ip}:8080/health'
            ]

            result = subprocess.run(
                cmd,
                check=True,
                capture_output=True,
                text=True
            )

            # Parse results
            if 'requests/sec' in result.stdout:
                throughput = float(result.stdout.split('requests/sec')[0].split()[-1])
                logger.info(f"Canary throughput: {throughput:.2f} req/s")

                # Compare with baseline (simplified)
                baseline = 1000  # Baseline throughput
                if throughput < baseline * 0.8:  # 20% degradation
                    logger.error(f"Performance degraded: {throughput:.2f} < {baseline * 0.8}")
                    return False

            return True

        except subprocess.CalledProcessError as e:
            logger.error(f"Performance test failed: {e}")
            return False

    def promote_canary(self):
        """Promote canary to production"""
        logger.info("Promoting canary to production...")

        # Update stable deployment
        try:
            result = subprocess.run(
                ['kubectl', 'get', 'deployment', 'erlmcp-canary',
                 '-n', self.config.namespace,
                 '-o', 'json'],
                check=True, capture_output=True, text=True
            )
            canary_deployment = json.loads(result.stdout)

            # Create stable deployment from canary
            stable_manifest = canary_deployment.copy()
            stable_manifest['metadata']['name'] = 'erlmcp'
            stable_manifest['metadata']['labels']['version'] = 'stable'
            stable_manifest['spec']['template']['spec']['containers'][0]['name'] = 'erlmcp'
            stable_manifest['spec']['template']['metadata']['labels']['version'] = 'stable'

            manifest_file = '/tmp/stable-deployment.yaml'
            with open(manifest_file, 'w') as f:
                json.dump(stable_manifest, f, indent=2)

            # Apply stable deployment
            subprocess.run(
                ['kubectl', 'apply', '-f', manifest_file],
                check=True
            )

            logger.info("Canary promoted to production")

        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to promote canary: {e}")
            raise

    def rollback_canary(self):
        """Rollback canary deployment"""
        logger.info("Rolling back canary deployment...")

        # Delete canary resources
        try:
            subprocess.run(
                ['kubectl', 'delete', 'deployment', 'erlmcp-canary',
                 '-n', self.config.namespace'],
                check=False
            )

            subprocess.run(
                ['kubectl', 'delete', 'service', 'erlmcp-canary',
                 '-n', self.config.namespace'],
                check=False
            )

            logger.info("Canary deployment rolled back")

        except subprocess.CalledProcessError as e:
            logger.error(f"Rollback failed: {e}")

    def deploy(self):
        """Execute full canary deployment"""
        try:
            self.validate_prerequisites()

            # Create canary deployment
            self.create_canary_deployment()

            # Create service split
            self.create_service_split()

            # Configure traffic
            self.configure_traffic_split()

            # Monitor health
            if not self.monitor_canary():
                logger.error("Canary health check failed, rolling back")
                self.rollback_canary()
                sys.exit(1)

            # Run performance test
            if not self.run_performance_test():
                logger.error("Performance test failed, rolling back")
                self.rollback_canary()
                sys.exit(1)

            # Promote to production
            self.promote_canary()

            # Cleanup canary resources
            self.rollback_canary()

            logger.info("✅ Canary deployment completed successfully")

        except Exception as e:
            logger.error(f"❌ Canary deployment failed: {e}")
            self.rollback_canary()
            sys.exit(1)

def main():
    parser = argparse.ArgumentParser(description="Canary Deployment for erlmcp v3")
    parser.add_argument("--namespace", "-n", required=True,
                       help="Kubernetes namespace")
    parser.add_argument("--image", "-i", required=True,
                       help="Canary image to deploy")
    parser.add_argument("--canary-percent", "-p", type=int, default=10,
                       help="Percentage of traffic for canary (10)")
    parser.add_argument("--timeout", "-t", type=int, default=300,
                       help="Monitoring timeout in seconds (300)")
    parser.add_argument("--health-threshold", type=float, default=95.0,
                       help="Health threshold percentage (95.0)")
    parser.add_argument("--error-threshold", type=int, default=1,
                       help="Error threshold before rollback (1)")
    parser.add_argument("--dry-run", action="store_true",
                       help="Show what would be done")

    args = parser.parse_args()

    # Create configuration
    config = CanaryConfig(
        namespace=args.namespace,
        image=args.image,
        canary_percent=args.canary_percent,
        timeout=args.timeout,
        health_threshold=args.health_threshold,
        error_threshold=args.error_threshold
    )

    # Validate configuration
    config.validate()

    if args.dry_run:
        logger.info("Dry run mode - showing deployment plan:")
        logger.info(f"- Namespace: {config.namespace}")
        logger.info(f"- Image: {config.image}")
        logger.info(f"- Canary percentage: {config.canary_percent}%")
        logger.info(f"- Timeout: {config.timeout}s")
        logger.info(f"- Health threshold: {config.health_threshold}%")
    else:
        # Execute deployment
        deployer = CanaryDeployer(config)
        deployer.deploy()

if __name__ == "__main__":
    main()