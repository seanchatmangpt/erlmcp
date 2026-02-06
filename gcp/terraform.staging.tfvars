# Staging Environment Configuration
# Production-like setup for testing and validation
# Updated: 2026-02-06

environment            = "staging"
project_id             = "taiea-staging"
region                 = "us-central1"
gke_zones              = ["us-central1-a", "us-central1-b", "us-central1-c"]
gke_cluster_name       = "erlmcp-staging-cluster"
gke_node_pool_size     = 2
gke_machine_type       = "n2-standard-4"
cloud_sql_tier         = "db-g1-small"
backup_retention_days  = 14
firestore_enabled      = true
enable_monitoring      = true

# New variables
alert_email_addresses          = ["devops@example.com"]
monthly_budget_limit           = 1500
log_retention_days             = 30
security_log_retention_days    = 90
artifact_retention_count       = 10
enable_cross_region_backup     = true
enable_workload_identity       = true
enable_binary_authorization    = true
enable_vulnerability_scanning  = true
enable_ddos_protection         = false
enable_cdn                     = false
enable_data_residency          = false
compliance_framework           = "sox"
enable_audit_logs              = true
enable_organization_policies   = false
enable_vpc_service_controls    = false
terraform_state_encryption     = true
