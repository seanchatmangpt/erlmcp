# Development Environment Configuration
# Optimized for cost and rapid iteration
# Updated: 2026-02-06

environment            = "dev"
project_id             = "taiea-dev"
region                 = "us-central1"
gke_zones              = ["us-central1-a"]
gke_cluster_name       = "erlmcp-dev-cluster"
gke_node_pool_size     = 1
gke_machine_type       = "n2-standard-2"
cloud_sql_tier         = "db-f1-micro"
backup_retention_days  = 7
firestore_enabled      = false
enable_monitoring      = true

# New variables
alert_email_addresses          = []
monthly_budget_limit           = 500
log_retention_days             = 7
security_log_retention_days    = 30
artifact_retention_count       = 5
enable_cross_region_backup     = false
enable_workload_identity       = true
enable_binary_authorization    = false
enable_vulnerability_scanning  = true
enable_ddos_protection         = false
enable_cdn                     = false
enable_data_residency          = false
compliance_framework           = "none"
enable_audit_logs              = false
enable_organization_policies   = false
enable_vpc_service_controls    = false
terraform_state_encryption     = false
