# Joe Armstrong AGI MCP - Existing Cloud Spend Configuration
# Use this tfvars with existing GCP project that has billing enabled
#
# Usage:
#   terraform init
#   terraform plan -var-file=terraform.existing-spend.tfvars
#   terraform apply -var-file=terraform.existing-spend.tfvars
#
# No new billing account required - uses existing project quotas

environment            = "production"
# Set your existing project ID here
project_id             = "YOUR_EXISTING_PROJECT"
region                 = "us-central1"

# Cloud Run (serverless - pay per request)
# Perfect for variable traffic, uses existing spend efficiently
cloudrun_enabled       = true
cloudrun_min_instances = 1
cloudrun_max_instances = 100
cloudrun_cpu           = "2"
cloudrun_memory        = "2Gi"

# GKE disabled by default (enable for dedicated infrastructure)
gke_enabled            = false
gke_zones              = ["us-central1-a", "us-central1-b", "us-central1-c"]
gke_cluster_name       = "erlmcp-agi-cluster"
gke_node_pool_size     = 3
gke_machine_type       = "n2-standard-4"

# Cloud SQL disabled (use in-memory by default)
# Enable for persistent sessions
cloud_sql_enabled      = false
cloud_sql_tier         = "db-f1-micro"

# Monitoring uses existing GCP operations
enable_monitoring      = true
enable_logging         = true
enable_tracing         = true

# Cost controls
budget_alert_threshold = 100
enable_budget_alerts   = true
