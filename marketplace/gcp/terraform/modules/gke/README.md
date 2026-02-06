# GKE Terraform Module for erlmcp

Enterprise-grade Google Kubernetes Engine (GKE) Terraform module with support for both Standard and Autopilot clusters, optimized for production workloads.

## Features

### Cluster Modes
- **GKE Autopilot**: Fully managed, hands-off Kubernetes with Google-managed nodes
- **GKE Standard**: Full control over node configuration and management

### Security
- **GKE Security Posture**: Automated vulnerability scanning and security recommendations
- **Binary Authorization**: Container image attestation and policy enforcement
- **Shielded GKE Nodes**: Secure Boot and Integrity Monitoring
- **Confidential GKE Nodes**: Encrypted memory for sensitive workloads
- **Workload Identity**: IAM-based authentication for GCP services
- **Private Clusters**: Private control plane and node endpoints
- **Network Policy**: Pod-level network segmentation with Calico or Dataplane V2

### Networking
- **Dataplane V2**: Advanced networking with eBPF for improved performance and security
- **Gateway API**: Next-generation ingress with advanced traffic management
- **GKE Network Policy**: Built-in network segmentation
- **Multi-network support**: Multiple network interfaces per pod
- **Image Streaming (GCFS)**: Faster pod startup times
- **gVNIC**: High-performance virtual NIC

### Observability
- **Google Cloud Managed Service for Prometheus**: Fully managed Prometheus
- **Cloud Operations for GKE**: Integrated logging and monitoring
- **Advanced Datapath Observability**: Deep network insights with Dataplane V2
- **Workload metrics**: Automatic Kubernetes metrics collection

### Cost Optimization
- **Spot VMs**: Up to 91% cost savings for fault-tolerant workloads
- **GKE Cost Allocation**: Namespace and label-based cost tracking
- **Cluster Autoscaling**: Automatic node provisioning based on workload demand
- **Node Auto-provisioning**: Automatic node pool creation for optimal resource utilization

### High Availability
- **Regional Clusters**: 99.95% SLA with multi-zone control plane
- **Blue-Green Node Upgrades**: Zero-downtime rolling updates
- **Node Auto-repair**: Automatic node health monitoring and repair
- **Maintenance Windows**: Scheduled maintenance with exclusion periods

### Additional Features
- **GCS FUSE CSI Driver**: Mount Cloud Storage buckets as volumes
- **Filestore CSI Driver**: Fully managed NFS for stateful workloads
- **GKE Backup**: Built-in disaster recovery
- **Config Connector**: Manage GCP resources via Kubernetes
- **Fleet Management**: Multi-cluster management and policies
- **Deletion Protection**: Prevent accidental cluster deletion

## Usage

### Basic Autopilot Cluster

```hcl
module "gke_autopilot" {
  source = "./modules/gke"

  project_id   = "my-project"
  region       = "us-central1"
  cluster_name = "erlmcp-autopilot"

  # Enable Autopilot mode
  enable_autopilot = true

  # Network configuration
  network = "projects/my-project/global/networks/my-vpc"
  subnet  = "projects/my-project/regions/us-central1/subnetworks/my-subnet"

  # IP allocation policy (required for Autopilot)
  ip_allocation_policy = {
    cluster_secondary_range_name  = "pods"
    services_secondary_range_name = "services"
  }

  # Security
  enable_security_posture          = true
  security_posture_mode            = "BASIC"
  enable_private_endpoint          = false  # Autopilot requires public endpoint initially
  enable_private_nodes             = true

  # Monitoring
  enable_managed_prometheus = true
}
```

### Standard Cluster with Custom Node Pools

```hcl
module "gke_standard" {
  source = "./modules/gke"

  project_id   = "my-project"
  region       = "us-central1"
  cluster_name = "erlmcp-standard"

  # Standard mode (default)
  enable_autopilot = false

  # Network configuration
  network = "my-vpc"
  subnet  = "my-subnet"

  # Private cluster
  enable_private_endpoint = true
  enable_private_nodes    = true
  master_ipv4_cidr_block  = "172.16.0.0/28"

  # Create custom node pools
  create_custom_node_pools = true
  create_spot_node_pool    = true

  # Primary node pool configuration
  node_pools = {
    primary = {
      machine_type                = "e2-standard-4"
      disk_size_gb                = 100
      disk_type                   = "pd-balanced"
      min_count                   = 3
      max_count                   = 10
      enable_secure_boot          = true
      enable_integrity_monitoring = true
      enable_gvnic                = true
      enable_confidential_nodes   = false
      local_ssd_count             = 0

      # Advanced features
      kubelet_config = {
        cpu_manager_policy = "static"
        pod_pids_limit     = 4096
      }

      linux_node_config = {
        sysctls = {
          "net.core.somaxconn"  = "32768"
          "net.ipv4.tcp_tw_reuse" = "1"
        }
      }
    }

    # Spot node pool for cost optimization
    spot = {
      machine_type  = "e2-standard-4"
      disk_size_gb  = 100
      disk_type     = "pd-standard"
      min_count     = 0
      max_count     = 20
      spot          = true
      enable_gvnic  = true

      taints = [{
        key    = "workload-type"
        value  = "batch"
        effect = "NO_SCHEDULE"
      }]
    }
  }

  # Security features
  enable_security_posture              = true
  security_posture_mode                = "ENTERPRISE"
  security_posture_vulnerability_mode  = "VULNERABILITY_ENTERPRISE"
  binauthz_evaluation_mode             = "PROJECT_SINGLETON_POLICY_ENFORCE"
  enable_workload_vulnerability_scanning = true

  # Networking
  datapath_provider   = "ADVANCED_DATAPATH"  # Dataplane V2
  enable_gateway_api  = true
  gateway_api_channel = "CHANNEL_STANDARD"

  # Monitoring and observability
  enable_managed_prometheus               = true
  enable_advanced_datapath_observability  = true

  # Advanced node features
  enable_image_streaming = true
  enable_fast_socket     = false

  # Cost management
  enable_cost_allocation = true
  cost_center            = "engineering"

  # Autoscaling
  enable_cluster_autoscaling  = true
  cluster_autoscaling_profile = "OPTIMIZE_UTILIZATION"
  enable_node_autoprovisioning = true

  cluster_resource_limits = [
    {
      resource_type = "cpu"
      minimum       = 12
      maximum       = 200
    },
    {
      resource_type = "memory"
      minimum       = 48
      maximum       = 800
    }
  ]

  # Fleet management
  enable_fleet = true

  # Protection
  enable_deletion_protection = true
}
```

### Production-Ready Cluster with All Features

```hcl
module "gke_production" {
  source = "./modules/gke"

  project_id   = "my-project"
  region       = "us-central1"
  cluster_name = "erlmcp-prod"

  # Kubernetes version
  kubernetes_version = "1.30."
  release_channel    = "STABLE"

  # Network
  network = "projects/my-project/global/networks/prod-vpc"
  subnet  = "projects/my-project/regions/us-central1/subnetworks/gke-subnet"

  ip_allocation_policy = {
    cluster_secondary_range_name  = "gke-pods"
    services_secondary_range_name = "gke-services"
  }

  # Private cluster
  enable_private_endpoint = true
  enable_private_nodes    = true
  master_ipv4_cidr_block  = "172.16.0.0/28"
  master_global_access    = true

  # Authorized networks for control plane access
  authorized_networks = [
    {
      cidr_block   = "10.0.0.0/8"
      display_name = "corporate-network"
    }
  ]

  # Node pools
  create_custom_node_pools = true
  create_spot_node_pool    = true

  node_pools = {
    primary = {
      machine_type                = "n2-standard-8"
      image_type                  = "COS_CONTAINERD"
      disk_size_gb                = 200
      disk_type                   = "pd-ssd"
      min_count                   = 3
      max_count                   = 20
      location_policy             = "BALANCED"
      enable_secure_boot          = true
      enable_integrity_monitoring = true
      enable_gvnic                = true
      enable_confidential_nodes   = false

      labels = {
        workload-type = "general"
        tier          = "production"
      }

      kubelet_config = {
        cpu_manager_policy   = "static"
        cpu_cfs_quota        = true
        cpu_cfs_quota_period = "100ms"
        pod_pids_limit       = 8192
      }

      linux_node_config = {
        sysctls = {
          "net.core.somaxconn"       = "65535"
          "net.ipv4.tcp_tw_reuse"    = "1"
          "net.ipv4.ip_local_port_range" = "32768 65535"
        }
        cgroup_mode = "CGROUP_MODE_V2"
      }
    }

    spot = {
      machine_type  = "n2-standard-8"
      disk_size_gb  = 200
      disk_type     = "pd-balanced"
      min_count     = 0
      max_count     = 50
      spot          = true
      enable_gvnic  = true

      labels = {
        workload-type     = "batch"
        cost-optimization = "true"
      }

      taints = [{
        key    = "workload-type"
        value  = "batch"
        effect = "NO_SCHEDULE"
      }]
    }
  }

  # Security
  enable_security_posture              = true
  security_posture_mode                = "ENTERPRISE"
  security_posture_vulnerability_mode  = "VULNERABILITY_ENTERPRISE"
  binauthz_evaluation_mode             = "PROJECT_SINGLETON_POLICY_ENFORCE"
  enable_workload_vulnerability_scanning = true
  enable_secure_boot                   = true
  enable_integrity_monitoring          = true

  # Networking
  datapath_provider    = "ADVANCED_DATAPATH"
  enable_network_policy = true
  enable_gateway_api   = true
  gateway_api_channel  = "CHANNEL_STANDARD"
  enable_gcs_fuse_csi  = true
  enable_filestore_csi = true
  enable_dns_cache     = true

  # Monitoring
  enable_managed_prometheus              = true
  enable_advanced_datapath_observability = true
  enable_datapath_observability_relay    = true

  logging_components = [
    "SYSTEM_COMPONENTS",
    "WORKLOADS",
    "APISERVER",
    "CONTROLLER_MANAGER",
    "SCHEDULER"
  ]

  monitoring_components = [
    "SYSTEM_COMPONENTS",
    "WORKLOADS",
    "APISERVER",
    "CONTROLLER_MANAGER",
    "SCHEDULER"
  ]

  # Advanced node features
  enable_image_streaming = true
  node_logging_variant   = "MAX_THROUGHPUT"

  # Autoscaling
  enable_cluster_autoscaling  = true
  cluster_autoscaling_profile = "OPTIMIZE_UTILIZATION"
  enable_node_autoprovisioning = true

  cluster_resource_limits = [
    {
      resource_type = "cpu"
      minimum       = 24
      maximum       = 500
    },
    {
      resource_type = "memory"
      minimum       = 96
      maximum       = 2000
    }
  ]

  # Node pool upgrades
  node_pool_upgrade_strategy = "BLUE_GREEN"
  blue_green_soak_duration   = "7200s"
  blue_green_batch_percentage = 0.33

  # Maintenance
  maintenance_window_start = "03:00"
  maintenance_exclusions = [
    {
      start_time = "2025-11-24T00:00:00Z"
      end_time   = "2025-11-30T23:59:59Z"
      recurrence = "FREQ=YEARLY"  # Black Friday week
    }
  ]

  # Cost management
  enable_cost_allocation = true
  cost_center            = "engineering"

  # Fleet and backup
  enable_fleet        = true
  enable_backup_agent = true

  # Notifications
  enable_notification_config = true
  notification_pubsub_topic  = "projects/my-project/topics/gke-notifications"

  # Protection
  enable_deletion_protection = true

  # Labels
  resource_labels = {
    environment     = "production"
    application     = "erlmcp"
    cost-center     = "engineering"
    compliance      = "pci-dss"
    disaster-recovery = "tier-1"
  }
}
```

## Requirements

| Name | Version |
|------|---------|
| terraform | >= 1.8.0 |
| google | >= 6.0.0 |
| google-beta | >= 6.0.0 |
| kubernetes | >= 2.33.0 |
| helm | >= 2.15.0 |

## Providers

| Name | Version |
|------|---------|
| google | >= 6.0.0 |
| google-beta | >= 6.0.0 |
| kubernetes | >= 2.33.0 |
| helm | >= 2.15.0 |

## Inputs

See [variables.tf](./variables.tf) for a complete list of inputs.

### Key Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| project_id | GCP project ID | `string` | n/a | yes |
| region | GCP region for regional cluster | `string` | `"us-central1"` | no |
| cluster_name | GKE cluster name | `string` | `"erlmcp-cluster"` | no |
| enable_autopilot | Enable GKE Autopilot mode | `bool` | `false` | no |
| kubernetes_version | Kubernetes version prefix | `string` | `""` | no |
| release_channel | GKE release channel (RAPID, REGULAR, STABLE) | `string` | `"REGULAR"` | no |
| enable_private_nodes | Enable private nodes | `bool` | `true` | no |
| enable_security_posture | Enable GKE Security Posture | `bool` | `true` | no |
| enable_managed_prometheus | Enable Managed Prometheus | `bool` | `true` | no |
| datapath_provider | Datapath provider (ADVANCED_DATAPATH or LEGACY_DATAPATH) | `string` | `"ADVANCED_DATAPATH"` | no |

## Outputs

See [outputs.tf](./outputs.tf) for a complete list of outputs.

### Key Outputs

| Name | Description |
|------|-------------|
| cluster_name | GKE cluster name |
| cluster_endpoint | GKE cluster endpoint |
| cluster_ca_certificate | GKE cluster CA certificate |
| kubectl_config_command | Command to configure kubectl |
| workload_identity_pool | Workload Identity pool |
| cluster_features | Summary of enabled features |
| console_links | Google Cloud Console links |

## Security Best Practices

1. **Enable Private Clusters**: Set `enable_private_nodes = true` and `enable_private_endpoint = true`
2. **Use Workload Identity**: Enabled by default for secure GCP API access
3. **Enable Security Posture**: Set `enable_security_posture = true` with `security_posture_mode = "ENTERPRISE"`
4. **Enable Binary Authorization**: Set `binauthz_evaluation_mode = "PROJECT_SINGLETON_POLICY_ENFORCE"`
5. **Use Shielded Nodes**: Enabled by default with Secure Boot and Integrity Monitoring
6. **Enable Network Policy**: Set `enable_network_policy = true` with Dataplane V2
7. **Limit Control Plane Access**: Configure `authorized_networks` for master endpoint
8. **Enable Vulnerability Scanning**: Set `enable_workload_vulnerability_scanning = true`

## Cost Optimization

1. **Use Autopilot**: Lower management overhead and pay-per-pod pricing
2. **Enable Spot VMs**: Set `create_spot_node_pool = true` for up to 91% savings
3. **Use Cluster Autoscaling**: Set `enable_cluster_autoscaling = true`
4. **Enable Cost Allocation**: Track costs by namespace with `enable_cost_allocation = true`
5. **Right-size Node Pools**: Use `e2` machine types for general workloads
6. **Use Committed Use Discounts**: For predictable workloads, use GCP CUDs
7. **Enable Node Auto-provisioning**: Automatic optimal node selection

## High Availability

1. **Use Regional Clusters**: Deploy across multiple zones for 99.95% SLA
2. **Configure Node Auto-repair**: Enabled by default
3. **Use Blue-Green Upgrades**: Set `node_pool_upgrade_strategy = "BLUE_GREEN"`
4. **Set Appropriate PDBs**: Configure PodDisruptionBudgets for applications
5. **Use Multiple Replicas**: Deploy workloads with replicas > 1
6. **Configure Backup**: Enable `enable_backup_agent = true`

## Monitoring and Observability

1. **Enable Managed Prometheus**: Set `enable_managed_prometheus = true`
2. **Configure Cloud Operations**: Logs and metrics automatically exported
3. **Enable Datapath Observability**: For network insights with Dataplane V2
4. **Set Up Alerts**: Configure notification channels for critical events
5. **Use Cost Allocation**: Track resource usage by namespace

## Upgrading

### Cluster Version Upgrades

Clusters in release channels are automatically upgraded. To control versions:

```hcl
release_channel    = "STABLE"
kubernetes_version = "1.30."  # Pin to specific minor version
```

### Node Pool Upgrades

For zero-downtime upgrades, use Blue-Green strategy:

```hcl
node_pool_upgrade_strategy = "BLUE_GREEN"
blue_green_soak_duration   = "3600s"
blue_green_batch_percentage = 0.5
```

## Troubleshooting

### Private Cluster Access

If you can't access the private cluster:

1. Verify you're connecting from an authorized network
2. Use Cloud Shell or a bastion host in the VPC
3. Enable `master_global_access = true` for VPN/Interconnect access
4. Check firewall rules allow port 443 to master CIDR

### Node Pool Creation Failures

Common issues:

1. **Quota exceeded**: Check GCP quotas for CPUs, IPs, and disks
2. **IP address exhaustion**: Ensure subnet has enough secondary ranges
3. **Service account permissions**: Verify node SA has required roles

### Workload Identity Issues

If pods can't access GCP services:

1. Verify KSA is annotated with GSA email
2. Check IAM binding: `gcloud iam service-accounts add-iam-policy-binding`
3. Ensure Workload Identity is enabled on node pools

## Examples

See the [examples](../../examples/) directory for complete working examples:

- [Basic GKE deployment](../../examples/gke-deployment/)
- [Autopilot cluster](../../examples/autopilot-cluster/)
- [Multi-region setup](../../examples/multi-region/)

## License

Apache 2.0. See [LICENSE](../../../../LICENSE) for details.

## Support

For issues and questions:
- GitHub Issues: https://github.com/yourusername/erlmcp/issues
- Documentation: https://cloud.google.com/kubernetes-engine/docs

## Changelog

### v3.0.0 (2026-02)
- Added GKE Autopilot support
- Updated to latest GKE API (v1beta1)
- Added Security Posture management
- Implemented Gateway API support
- Added Dataplane V2 advanced features
- Enhanced node pool configurations
- Added comprehensive cost management
- Improved observability with Managed Prometheus
- Added Fleet management support
- Implemented blue-green upgrade strategy
- Added deletion protection
- Enhanced security with vulnerability scanning
