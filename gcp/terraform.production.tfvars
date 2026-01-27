# Production Environment Terraform Variables
# Used for customer-facing service in GCP prod project

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
