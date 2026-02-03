#!/usr/bin/env python3
"""
Capacity Planner for erlmcp v3
Analyzes usage patterns and recommends scaling strategies
"""

import json
import argparse
from datetime import datetime, timedelta
from typing import Dict, List, Optional
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from dataclasses import dataclass


@dataclass
class CapacityMetrics:
    """Container for capacity metrics"""
    timestamp: datetime
    sessions_active: int
    requests_per_second: float
    cpu_usage: float
    memory_usage: float
    storage_usage: float
    network_io: float
    error_rate: float


@dataclass
class ScalingRecommendation:
    """Container for scaling recommendations"""
    action: str  # 'scale_up', 'scale_out', 'optimize', 'monitor'
    region: str
    node_count: int
    resource_type: str  # 'cpu', 'memory', 'network', 'storage'
    current_load: float
    projected_load: float
    confidence: float
    timeline: str
    estimated_cost: float


class CapacityPlanner:
    """Capacity planning engine for erlmcp"""

    def __init__(self, config_path: str = "config/capacity-config.json"):
        self.config = self._load_config(config_path)
        self.metrics_history: List[CapacityMetrics] = []
        self.scaling_history: List[ScalingRecommendation] = []

    def _load_config(self, config_path: str) -> Dict:
        """Load configuration from JSON file"""
        try:
            with open(config_path, 'r') as f:
                return json.load(f)
        except FileNotFoundError:
            # Default configuration
            return {
                "regions": {
                    "us-east-1": {"node_count": 3, "instance_type": "c6i.8xlarge"},
                    "eu-west-1": {"node_count": 3, "instance_type": "c6i.8xlarge"},
                    "ap-southeast-1": {"node_count": 2, "instance_type": "c6i.4xlarge"}
                },
                "scaling_thresholds": {
                    "cpu_high": 80.0,
                    "memory_high": 85.0,
                    "network_high": 80.0,
                    "sessions_per_node_high": 20000
                },
                "growth_rates": {
                    "sessions": 1.15,  # 15% growth per quarter
                    "requests": 1.20,  # 20% growth per quarter
                    "storage": 1.10    # 10% growth per quarter
                }
            }

    def load_metrics_data(self, metrics_file: str):
        """Load metrics data from JSON file"""
        try:
            with open(metrics_file, 'r') as f:
                data = json.load(f)

            self.metrics_history = [
                CapacityMetrics(
                    timestamp=datetime.fromisoformat(m['timestamp']),
                    sessions_active=m['sessions_active'],
                    requests_per_second=m['requests_per_second'],
                    cpu_usage=m['cpu_usage'],
                    memory_usage=m['memory_usage'],
                    storage_usage=m['storage_usage'],
                    network_io=m['network_io'],
                    error_rate=m['error_rate']
                )
                for m in data['metrics']
            ]

            print(f"Loaded {len(self.metrics_history)} metrics records")

        except FileNotFoundError:
            print(f"Metrics file {metrics_file} not found. Using sample data.")
            self._generate_sample_data()

    def _generate_sample_data(self):
        """Generate sample metrics data for demonstration"""
        base_time = datetime.now() - timedelta(days=90)

        for i in range(90):
            timestamp = base_time + timedelta(days=i)

            # Simulate growth patterns
            sessions = 10000 + (i * 150) + np.random.normal(0, 500)
            rps = 50000 + (i * 750) + np.random.normal(0, 2500)
            cpu = 40 + (i * 0.3) + np.random.normal(0, 5)
            memory = 45 + (i * 0.25) + np.random.normal(0, 3)
            storage = 100 + (i * 1.2)
            network = 200 + (i * 2.5) + np.random.normal(0, 10)
            error_rate = 0.001 + np.random.exponential(0.0005)

            self.metrics_history.append(
                CapacityMetrics(
                    timestamp=timestamp,
                    sessions_active=int(sessions),
                    requests_per_second=rps,
                    cpu_usage=min(100, cpu),
                    memory_usage=min(100, memory),
                    storage_usage=storage,
                    network_io=network,
                    error_rate=min(0.1, error_rate)
                )
            )

    def analyze_current_capacity(self) -> Dict:
        """Analyze current capacity utilization"""
        if not self.metrics_history:
            return {}

        latest = self.metrics_history[-1]
        thresholds = self.config['scaling_thresholds']

        analysis = {
            'timestamp': latest.timestamp.isoformat(),
            'sessions': {
                'current': latest.sessions_active,
                'threshold': 20000 * self.config['regions']['us-east-1']['node_count'],
                'utilization': (latest.sessions_active / (20000 * self.config['regions']['us-east-1']['node_count'])) * 100,
                'trend': self._calculate_trend('sessions_active')
            },
            'cpu': {
                'current': latest.cpu_usage,
                'threshold': thresholds['cpu_high'],
                'utilization': latest.cpu_usage,
                'trend': self._calculate_trend('cpu_usage')
            },
            'memory': {
                'current': latest.memory_usage,
                'threshold': thresholds['memory_high'],
                'utilization': latest.memory_usage,
                'trend': self._calculate_trend('memory_usage')
            },
            'network': {
                'current': latest.network_io,
                'threshold': thresholds['network_high'],
                'utilization': (latest.network_io / 1000) * 100,
                'trend': self._calculate_trend('network_io')
            },
            'storage': {
                'current': latest.storage_usage,
                'growth_rate': self.config['growth_rates']['storage'],
                'projected': latest.storage_usage * (self.config['growth_rates']['storage'] ** 4)  # 4 quarters
            }
        }

        return analysis

    def _calculate_trend(self, metric_name: str) -> str:
        """Calculate trend direction for a metric"""
        if len(self.metrics_history) < 7:
            return 'stable'

        values = [getattr(m, metric_name) for m in self.metrics_history[-7:]]
        slope = np.polyfit(range(len(values)), values, 1)[0]

        if slope > 0.1:
            return 'increasing'
        elif slope < -0.1:
            return 'decreasing'
        else:
            return 'stable'

    def generate_scaling_recommendations(self) -> List[ScalingRecommendation]:
        """Generate scaling recommendations based on current trends"""
        analysis = self.analyze_current_capacity()
        recommendations = []

        # Check CPU
        if analysis['cpu']['utilization'] > self.config['scaling_thresholds']['cpu_high'] * 0.8:
            rec = ScalingRecommendation(
                action='scale_up',
                region='us-east-1',
                node_count=self.config['regions']['us-east-1']['node_count'] + 1,
                resource_type='cpu',
                current_load=analysis['cpu']['utilization'],
                projected_load=analysis['cpu']['trend'],
                confidence=0.9,
                timeline='2 weeks',
                estimated_cost=5000  # Monthly cost estimate
            )
            recommendations.append(rec)

        # Check memory
        if analysis['memory']['utilization'] > self.config['scaling_thresholds']['memory_high'] * 0.8:
            rec = ScalingRecommendation(
                action='scale_out',
                region='us-east-1',
                node_count=self.config['regions']['us-east-1']['node_count'] + 1,
                resource_type='memory',
                current_load=analysis['memory']['utilization'],
                projected_load=analysis['memory']['trend'],
                confidence=0.85,
                timeline='1 month',
                estimated_cost=6000
            )
            recommendations.append(rec)

        # Check session capacity
        sessions_per_node = analysis['sessions']['current'] / self.config['regions']['us-east-1']['node_count']
        if sessions_per_node > 15000:  # 75% of 20K
            rec = ScalingRecommendation(
                action='scale_out',
                region='us-east-1',
                node_count=round(sessions_per_node / 20000) + 1,
                resource_type='sessions',
                current_load=sessions_per_node,
                projected_load=sessions_per_node * 1.15,
                confidence=0.95,
                timeline='1 month',
                estimated_cost=7000
            )
            recommendations.append(rec)

        # Check storage growth
        if analysis['storage']['projected'] > 1000:  # Projected storage threshold
            rec = ScalingRecommendation(
                action='optimize',
                region='all',
                node_count=0,
                resource_type='storage',
                current_load=analysis['storage']['current'],
                projected_load=analysis['storage']['projected'],
                confidence=0.8,
                timeline='3 months',
                estimated_cost=2000
            )
            recommendations.append(rec)

        return recommendations

    def generate_capacity_report(self, output_file: str = None):
        """Generate comprehensive capacity report"""
        analysis = self.analyze_current_capacity()
        recommendations = self.generate_scaling_recommendations()

        # Create visualizations
        self._create_visualizations()

        # Generate report
        report = {
            'generated_at': datetime.now().isoformat(),
            'analysis': analysis,
            'recommendations': [
                {
                    'action': r.action,
                    'region': r.region,
                    'resource_type': r.resource_type,
                    'current_load': r.current_load,
                    'projected_load': r.projected_load,
                    'timeline': r.timeline,
                    'estimated_cost': r.estimated_cost,
                    'confidence': r.confidence
                }
                for r in recommendations
            ],
            'projections': self._generate_projections()
        }

        # Save report
        if output_file:
            with open(output_file, 'w') as f:
                json.dump(report, f, indent=2)
            print(f"Capacity report saved to {output_file}")

        # Print summary
        self._print_summary(report)

        return report

    def _generate_projections(self) -> Dict:
        """Generate future capacity projections"""
        if not self.metrics_history:
            return {}

        latest = self.metrics_history[-1]
        growth_rates = self.config['growth_rates']

        projections = {}

        for quarter in [1, 2, 3, 4]:
            projections[f'Q{quarter}'] = {
                'sessions': int(latest.sessions_active * (growth_rates['sessions'] ** quarter)),
                'requests_per_second': int(latest.requests_per_second * (growth_rates['requests'] ** quarter)),
                'storage': latest.storage_usage * (growth_rates['storage'] ** quarter),
                'required_nodes': int(
                    (latest.sessions_active * (growth_rates['sessions'] ** quarter)) / 20000
                )
            }

        return projections

    def _create_visualizations(self):
        """Create capacity visualization charts"""
        if not self.metrics_history:
            return

        # Prepare data
        df = pd.DataFrame([
            {
                'date': m.timestamp,
                'sessions': m.sessions_active,
                'cpu': m.cpu_usage,
                'memory': m.memory_usage,
                'rps': m.requests_per_second
            }
            for m in self.metrics_history
        ])

        # Create plots
        fig, axes = plt.subplots(2, 2, figsize=(15, 10))

        # Sessions trend
        axes[0, 0].plot(df['date'], df['sessions'], 'b-', linewidth=2)
        axes[0, 0].set_title('Active Sessions Trend')
        axes[0, 0].set_xlabel('Date')
        axes[0, 0].set_ylabel('Sessions')
        axes[0, 0].grid(True, alpha=0.3)

        # CPU utilization
        axes[0, 1].plot(df['date'], df['cpu'], 'r-', linewidth=2)
        axes[0, 1].axhline(y=self.config['scaling_thresholds']['cpu_high'], color='r', linestyle='--', label='CPU Threshold')
        axes[0, 1].set_title('CPU Utilization')
        axes[0, 1].set_xlabel('Date')
        axes[0, 1].set_ylabel('CPU %')
        axes[0, 1].legend()
        axes[0, 1].grid(True, alpha=0.3)

        # Memory utilization
        axes[1, 0].plot(df['date'], df['memory'], 'g-', linewidth=2)
        axes[1, 0].axhline(y=self.config['scaling_thresholds']['memory_high'], color='g', linestyle='--', label='Memory Threshold')
        axes[1, 0].set_title('Memory Utilization')
        axes[1, 0].set_xlabel('Date')
        axes[1, 0].set_ylabel('Memory %')
        axes[1, 0].legend()
        axes[1, 0].grid(True, alpha=0.3)

        # Requests per second
        axes[1, 1].plot(df['date'], df['rps'], 'm-', linewidth=2)
        axes[1, 1].set_title('Requests Per Second')
        axes[1, 1].set_xlabel('Date')
        axes[1, 1].set_ylabel('RPS')
        axes[1, 1].grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig('/var/log/erlmcp/capacity-analysis.png', dpi=300, bbox_inches='tight')
        plt.close()

        print("Capacity analysis visualization saved")

    def _print_summary(self, report: Dict):
        """Print summary of capacity analysis"""
        print("\n" + "="*60)
        print("CAPACITY PLANNING SUMMARY")
        print("="*60)

        print(f"\nGenerated at: {report['generated_at']}")

        if report['recommendations']:
            print("\n🚨 SCALING RECOMMENDATIONS:")
            for rec in report['recommendations']:
                confidence_emoji = "🔴" if rec['confidence'] > 0.9 else "🟡" if rec['confidence'] > 0.7 else "🟢"
                print(f"\n{confidence_emoji} {rec['action'].upper()} in {rec['region']}")
                print(f"   Resource: {rec['resource_type']}")
                print(f"   Timeline: {rec['timeline']}")
                print(f"   Estimated cost: ${rec['estimated_cost']}/month")
                print(f"   Confidence: {rec['confidence']*100:.0f}%")
        else:
            print("\n✅ No immediate scaling recommendations. System is within capacity limits.")

        if report['projections']:
            print("\n📈 4-QUARTER PROJECTIONS:")
            for quarter, proj in report['projections'].items():
                print(f"\n   {quarter}:")
                print(f"     Sessions: {proj['sessions']:,}")
                print(f"     Requests/sec: {proj['requests_per_second']:,}")
                print(f"     Storage: {proj['storage']:.1f} GB")
                print(f"     Required nodes: {proj['required_nodes']}")


def main():
    parser = argparse.ArgumentParser(description='erlmcp Capacity Planner')
    parser.add_argument('--metrics', type=str, default='/var/log/erlmcp/capacity-metrics.json',
                       help='Path to metrics JSON file')
    parser.add_argument('--output', type=str, default='/var/log/erlmcp/capacity-report.json',
                       help='Path to output report file')
    parser.add_argument('--sample', action='store_true',
                       help='Generate sample data for demonstration')

    args = parser.parse_args()

    # Initialize capacity planner
    planner = CapacityPlanner()

    # Load metrics data
    if args.sample:
        planner._generate_sample_data()
    else:
        planner.load_metrics_data(args.metrics)

    # Generate report
    report = planner.generate_capacity_report(args.output)

    return report


if __name__ == '__main__':
    main()