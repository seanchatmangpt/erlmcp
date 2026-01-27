# Development Environment Terraform Variables
# Used for local development in GCP dev project

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
