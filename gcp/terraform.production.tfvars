# Production Environment Configuration
# High availability and enterprise-grade security
# Updated: 2026-02-06

environment            = "production"
project_id             = "taiea-prod"
region                 = "us-central1"
gke_zones              = ["us-central1-a", "us-central1-b", "us-central1-c"]
gke_cluster_name       = "erlmcp-prod-cluster"
gke_node_pool_size     = 3
gke_machine_type       = "n2-standard-8"
cloud_sql_tier         = "db-custom-4-16384"
backup_retention_days  = 30
firestore_enabled      = true
enable_monitoring      = true

# New variables
alert_email_addresses          = ["devops@example.com", "oncall@example.com"]
monthly_budget_limit           = 5000
log_retention_days             = 90
security_log_retention_days    = 365
artifact_retention_count       = 20
enable_cross_region_backup     = true
enable_workload_identity       = true
enable_binary_authorization    = true
enable_vulnerability_scanning  = true
enable_ddos_protection         = true
enable_cdn                     = true
enable_data_residency          = true
compliance_framework           = "sox"
enable_audit_logs              = true
enable_organization_policies   = true
enable_vpc_service_controls    = true
terraform_state_encryption     = true
