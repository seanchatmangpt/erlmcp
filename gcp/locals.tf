# ==============================================================================
# Local Values
# ==============================================================================
# Common values used across multiple resources.
# ==============================================================================

locals {
  # Common labels applied to all resources
  common_labels = {
    app         = var.app_name
    environment = var.environment
    managed_by  = "terraform"
    project     = var.project_id
  }

  # Computed resource names
  service_name = "${var.app_name}-service"
  db_name      = "${var.app_name}-db"

  # Region shortcode for naming
  region_short = {
    "us-central1"   = "usc1"
    "us-east1"      = "use1"
    "us-west1"      = "usw1"
    "europe-west1"  = "euw1"
    "asia-east1"    = "ase1"
  }
}
