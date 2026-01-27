# Staging Environment Terraform Variables
# Used for testing in GCP dev project with production-like setup

environment            = "staging"
project_id             = "taiea-dev"
region                 = "us-central1"
gke_zones              = ["us-central1-a", "us-central1-b", "us-central1-c"]
gke_cluster_name       = "erlmcp-staging-cluster"
gke_node_pool_size     = 2
gke_machine_type       = "n2-standard-4"
cloud_sql_tier         = "db-g1-small"
backup_retention_days  = 14
firestore_enabled      = true
enable_monitoring      = true
