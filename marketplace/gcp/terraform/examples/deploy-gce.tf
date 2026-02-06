# erlmcp GCE VM Deployment Example
# This example shows how to deploy erlmcp on Google Compute Engine

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
  }
}

# Configure the provider
provider "google" {
  project = var.project_id
  region  = var.region
  zone    = var.zone
}

# Use the GCE VM module
module "erlmcp_gce" {
  source              = "../../modules/gce-vm"
  project_id          = var.project_id
  region              = var.region
  zone                = var.zone
  instance_name       = var.instance_name
  machine_type        = var.machine_type
  min_instances       = var.min_instances
  max_instances       = var.max_instances
  target_cpu_utilization = var.target_cpu_utilization
  image_name          = var.image_name
  vpc_network         = var.vpc_network
  vpc_subnetwork      = var.vpc_subnetwork
  enable_http         = var.enable_http
  enable_https        = var.enable_https
  ssl_certificate      = var.ssl_certificate
  service_account_email = var.service_account_email
  service_account_roles = var.service_account_roles
  labels              = var.labels
  tags                = var.tags
}

# Variables for this deployment
variable "project_id" {
  type        = string
  description = "The GCP project ID"
}

variable "region" {
  type        = string
  default     = "us-central1"
  description = "The region to deploy in"
}

variable "zone" {
  type        = string
  default     = "us-central1-a"
  description = "The zone to deploy in"
}

variable "instance_name" {
  type        = string
  default     = "erlmcp-vm"
  description = "Base name for VM instances"
}

variable "machine_type" {
  type        = string
  default     = "n2-standard-4"
  description = <<-EOT
    Machine type for VM instances (2026):
    - n2-standard-4: Intel balanced (recommended)
    - n2d-standard-4: AMD balanced
    - c3-standard-4: 4th gen Intel compute-optimized
    - e2-standard-4: Cost-optimized
  EOT
}

variable "min_instances" {
  type        = number
  default     = 2
  description = "Minimum number of instances"
}

variable "max_instances" {
  type        = number
  default     = 10
  description = "Maximum number of instances"
}

variable "target_cpu_utilization" {
  type        = number
  default     = 0.7
  description = "Target CPU utilization for auto-scaling"
}

variable "image_name" {
  type        = string
  default     = "erlmcp-marketplace-v1"
  description = "Name of the custom VM image to use"
}

variable "vpc_network" {
  type        = string
  default     = "default"
  description = "VPC network name"
}

variable "vpc_subnetwork" {
  type        = string
  default     = "default"
  description = "VPC subnetwork name"
}

variable "enable_http" {
  type        = bool
  default     = true
  description = "Enable HTTP health checks"
}

variable "enable_https" {
  type        = bool
  default     = true
  description = "Enable HTTPS health checks"
}

variable "ssl_certificate" {
  type        = string
  default     = ""
  description = "SSL certificate name for HTTPS"
}

variable "service_account_email" {
  type        = string
  default     = ""
  description = "Service account email for instances"
}

variable "service_account_roles" {
  type        = list(string)
  default     = ["roles/secretmanager.secretAccessor", "roles/logging.logWriter", "roles/monitoring.metricWriter"]
  description = "IAM roles for service account"
}

variable "labels" {
  type        = map(string)
  default     = {
    app = "erlmcp"
    role = "server"
    type = "marketplace"
  }
  description = "Labels to apply to resources"
}

variable "tags" {
  type        = list(string)
  default     = ["erlmcp", "http-server", "https-server"]
  description = "Network tags to apply to instances"
}

# Outputs
output "service_account_email" {
  value = module.erlmcp_gce.service_account_email
}

output "load_balancer_ip" {
  value = module.erlmcp_gce.load_balancer_ip
}

output "instance_group_manager" {
  value = module.erlmcp_gce.instance_group_manager
}

output "health_check_endpoint" {
  value = module.erlmcp_gce.health_check_endpoint
}

output "metrics_endpoint" {
  value = module.erlmcp_gce.metrics_endpoint
}

output "api_endpoint" {
  value = module.erlmcp_gce.api_endpoint
}