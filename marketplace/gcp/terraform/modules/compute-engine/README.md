# Google Compute Engine Module for erlmcp (2026 Edition)

## Overview

This module deploys erlmcp on Google Compute Engine with enterprise-grade features including the latest machine types, hyperdisk storage, advanced security configurations, and automated management capabilities.

## Latest Features (2026)

### 1. Modern Machine Types

The module now supports all latest GCE machine families:

#### General Purpose
- **E2** (cost-optimized): `e2-micro`, `e2-small`, `e2-medium`, `e2-standard-2/4/8/16/32`
- **N2** (balanced Intel): `n2-standard-2/4/8/16/32/48/64/80/96/128`
- **N2D** (balanced AMD): `n2d-standard-2/4/8/16/32/48/64/80/96/128/224`
- **T2D** (cost-optimized AMD): `t2d-standard-1/2/4/8/16/32/48/60`
- **T2A** (Arm-based): `t2a-standard-1/2/4/8/16/32/48`

#### Compute-Optimized
- **C2** (Intel): `c2-standard-4/8/16/30/60`
- **C2D** (AMD): `c2d-standard-2/4/8/16/32/56/112`
- **C3** (4th Gen Intel Sapphire Rapids): `c3-standard-4/8/22/44/88/176`

#### Memory-Optimized
- **M3**: `m3-megamem-64/128`, `m3-ultramem-32/64/128`

### 2. Next-Generation Disk Types

#### Persistent Disks
- **pd-standard**: Standard HDD persistent disk
- **pd-balanced**: Balanced SSD (good price/performance)
- **pd-ssd**: High-performance SSD
- **pd-extreme**: Extreme performance (specific machine types required)

#### Hyperdisk (Latest)
- **hyperdisk-balanced**: Next-gen balanced performance (default)
- **hyperdisk-throughput**: Optimized for high throughput workloads
- **hyperdisk-extreme**: Highest performance with configurable IOPS/throughput

### 3. Advanced Security Features

#### Shielded VM (Enabled by Default)
- **Secure Boot**: Ensures only authenticated OS software runs
- **vTPM**: Virtual Trusted Platform Module
- **Integrity Monitoring**: Monitors boot integrity

#### Confidential Computing
- **AMD SEV**: Standard Secure Encrypted Virtualization
- **AMD SEV-SNP**: Latest Secure Nested Paging (enhanced security)

#### Access Control
- **OS Login**: IAM-based SSH key management (enabled by default)
- **Block Project SSH Keys**: Prevents project-wide SSH keys (security best practice)

### 4. Advanced Compute Features

#### GPU Support
```hcl
guest_accelerators = [
  {
    type  = "nvidia-l4"      # Cost-effective inference
    count = 1
  }
]
```

Available GPU types:
- `nvidia-tesla-t4`: T4 GPUs (general purpose)
- `nvidia-tesla-v100`: V100 GPUs (training/inference)
- `nvidia-tesla-a100`: A100 GPUs (highest performance)
- `nvidia-l4`: L4 GPUs (cost-effective inference)
- `nvidia-a100-80gb`: A100 80GB (large models)

#### Local SSD Disks
```hcl
local_ssds = [
  {
    device_name = "local-ssd-0"
    interface   = "NVME"  # or "SCSI"
  }
]
```

#### Advanced Machine Features
```hcl
advanced_machine_features = {
  enable_nested_virtualization = true
  threads_per_core             = 1     # Disable hyperthreading
  visible_core_count           = 4     # Limit visible cores
}
```

#### CPU Platform Selection
```hcl
min_cpu_platform = "Intel Sapphire Rapids"  # 4th Gen Intel
# or "AMD Genoa", "AMD Milan", "Ampere Altra"
```

### 5. Automated Management

#### Snapshot Scheduling
```hcl
enable_snapshot_schedule = true
snapshot_schedule_name   = "daily-snapshot"
snapshot_retention_days  = 14
snapshot_start_time      = "03:00"  # UTC
```

#### Instance Scheduling (Cost Optimization)
```hcl
instance_schedule = {
  enabled    = true
  start_time = "09:00"
  stop_time  = "18:00"
  timezone   = "America/New_York"
}
```

### 6. Network Performance

```hcl
network_performance_config = {
  total_egress_bandwidth_tier = "TIER_1"  # Highest bandwidth
}
```

### 7. Reservation Affinity

```hcl
reservation_affinity = {
  type = "SPECIFIC_RESERVATION"
  specific_reservations = [
    {
      key    = "compute.googleapis.com/reservation-name"
      values = ["my-reservation"]
    }
  ]
}
```

## Usage Examples

### Basic Deployment

```hcl
module "erlmcp_compute" {
  source = "./modules/compute-engine"

  project_id     = "my-project"
  zone           = "us-central1-a"
  instance_name  = "erlmcp-server"
  machine_type   = "n2-standard-4"

  # Use latest hyperdisk
  disk_type    = "hyperdisk-balanced"
  disk_size_gb = 50

  # Enhanced security
  enable_secure_boot           = true
  enable_os_login              = true
  block_project_ssh_keys       = true
}
```

### High-Performance Deployment

```hcl
module "erlmcp_compute" {
  source = "./modules/compute-engine"

  project_id     = "my-project"
  zone           = "us-central1-a"
  machine_type   = "c3-standard-22"  # 4th Gen Intel

  # Hyperdisk Extreme for maximum performance
  disk_type    = "hyperdisk-extreme"
  disk_size_gb = 100

  # Add local SSDs for even higher I/O
  local_ssds = [
    { device_name = "local-ssd-0", interface = "NVME" },
    { device_name = "local-ssd-1", interface = "NVME" }
  ]

  # Optimize CPU
  min_cpu_platform = "Intel Sapphire Rapids"
  advanced_machine_features = {
    enable_nested_virtualization = false
    threads_per_core             = 2
    visible_core_count           = 0
  }
}
```

### GPU-Enabled Deployment

```hcl
module "erlmcp_compute" {
  source = "./modules/compute-engine"

  project_id     = "my-project"
  zone           = "us-central1-a"
  machine_type   = "n1-standard-8"

  # Add GPUs
  guest_accelerators = [
    {
      type  = "nvidia-l4"
      count = 1
    }
  ]
  enable_display = true

  # GPU workloads benefit from high-performance storage
  disk_type = "hyperdisk-extreme"
}
```

### Cost-Optimized with Spot VMs

```hcl
module "erlmcp_compute" {
  source = "./modules/compute-engine"

  project_id          = "my-project"
  zone                = "us-central1-a"
  machine_type        = "t2d-standard-4"  # AMD cost-optimized
  provisioning_model  = "SPOT"
  preemptible         = false  # Use SPOT instead

  # Automated scheduling for further savings
  instance_schedule = {
    enabled    = true
    start_time = "08:00"
    stop_time  = "20:00"
    timezone   = "UTC"
  }
}
```

### Production with Auto-Scaling

```hcl
module "erlmcp_compute" {
  source = "./modules/compute-engine"

  project_id     = "my-project"
  zone           = "us-central1-a"
  machine_type   = "n2-standard-4"

  # Managed Instance Group
  create_instance_template = true
  create_instance_group    = true

  # Auto-scaling configuration
  enable_autoscaling      = true
  min_replicas            = 2
  max_replicas            = 10
  cpu_utilization_target  = 0.75

  # Auto-healing
  enable_auto_healing           = true
  auto_healing_initial_delay    = 300

  # Automated snapshots
  enable_snapshot_schedule = true
  snapshot_retention_days  = 30
  snapshot_start_time      = "02:00"
}
```

## Security Best Practices (2026)

1. **Enable OS Login**: Use IAM for SSH key management
   ```hcl
   enable_os_login = true
   block_project_ssh_keys = true
   ```

2. **Use Shielded VMs**: Enable all Shielded VM features
   ```hcl
   enable_secure_boot          = true
   enable_vtpm                 = true
   enable_integrity_monitoring = true
   ```

3. **Confidential Computing**: For sensitive workloads
   ```hcl
   enable_confidential_compute         = true
   enable_confidential_compute_sev_snp = true  # Latest AMD SEV-SNP
   ```

4. **Minimal IAM Scopes**: Use least privilege
   ```hcl
   instance_scopes = [
     "https://www.googleapis.com/auth/logging.write",
     "https://www.googleapis.com/auth/monitoring.write",
     "https://www.googleapis.com/auth/secretmanager.secretAccessor"
   ]
   ```

5. **Disable IP Forwarding**: Unless specifically needed
   ```hcl
   can_ip_forward = false
   ```

## Performance Optimization

### For Compute-Intensive Workloads
- Use C3 (Intel Sapphire Rapids) or C2D (AMD) machine types
- Disable hyperthreading: `threads_per_core = 1`
- Use hyperdisk-extreme or local SSDs

### For Memory-Intensive Workloads
- Use M3 memory-optimized machine types
- Consider using hyperdisk-throughput for data-intensive operations

### For Cost Optimization
- Use T2D (AMD) or E2 machine types
- Enable instance scheduling for dev/test environments
- Use SPOT provisioning model for fault-tolerant workloads

### For Network-Intensive Workloads
- Use TIER_1 network bandwidth
- Consider using GVNIC (Google Virtual NIC)
- Use Premium tier networking

## Migration Guide

### From E2 to N2
```hcl
# Old
machine_type = "e2-medium"

# New (better performance, same cost tier)
machine_type = "n2-standard-2"
```

### From pd-balanced to hyperdisk-balanced
```hcl
# Old
disk_type = "pd-balanced"

# New (better performance)
disk_type = "hyperdisk-balanced"
```

### From Ubuntu 22.04 to 24.04
```hcl
# Old
source_image = "projects/ubuntu-os-cloud/global/images/family/ubuntu-2204-lts"

# New
source_image = "projects/ubuntu-os-cloud/global/images/family/ubuntu-2404-lts-amd64"
```

## Variables Reference

See [variables.tf](./variables.tf) for complete variable documentation.

## Outputs Reference

See [outputs.tf](./outputs.tf) for complete output documentation.

## Requirements

- Terraform >= 1.5.0
- Google Provider >= 5.0.0

## Architecture Decisions

### Why Hyperdisk as Default?
Hyperdisk Balanced offers better performance than pd-balanced at similar cost, with the flexibility to adjust IOPS and throughput independently.

### Why N2 as Default Machine Type?
N2 provides the best balance of performance and cost for most workloads, with better performance than E2 and lower cost than C2/C3.

### Why OS Login Enabled by Default?
OS Login provides centralized SSH key management through IAM, improving security and compliance.

### Why Block Project SSH Keys?
Blocking project-wide SSH keys enforces the principle of least privilege and prevents unauthorized access through shared keys.

## Troubleshooting

### Hyperdisk Not Available
If hyperdisk is not available in your zone, fall back to pd-balanced:
```hcl
disk_type = "pd-balanced"
```

### GPU Not Available
Check GPU availability by zone: `gcloud compute accelerator-types list`

### Confidential VM Not Supported
Not all machine types support Confidential VMs. Use N2D or C2D for AMD SEV support.

## Support and Contribution

For issues and contributions, see the main project repository.

## License

See LICENSE file in the repository root.
