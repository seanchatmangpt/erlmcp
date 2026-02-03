# erlmcp Cloud Run Deployment Example
# This example shows how to deploy erlmcp on Google Cloud Run

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.5"
    }
  }
}

# Configure the provider
provider "google" {
  project = var.project_id
  region  = var.region
}

# Use the Cloud Run module
module "erlmcp_cloudrun" {
  source               = "../../modules/cloud-run"
  project_id           = var.project_id
  region               = var.region
  service_name         = var.service_name
  container_image      = var.container_image
  secrets              = var.secrets
  environment_variables = var.environment_variables
  cpu                  = var.cpu
  memory               = var.memory
  max_instances        = var.max_instances
  min_instances        = var.min_instances
  concurrency         = var.concurrency
  timeout              = var.timeout
  enable_traffic_splitting = var.enable_traffic_splitting
  traffic_split_percent   = var.traffic_split_percent
  ingress_settings   = var.ingress_settings
  service_account_email = var.service_account_email
  labels              = var.labels
  annotations         = var.annotations
  domain_mapping      = var.domain_mapping
}

# Variables for this deployment
variable "project_id" {
  type        = string
  description = "The GCP project ID"
}

variable "region" {
  type        = string
  default     = "us-central1"
  description = "The region to deploy Cloud Run service in"
}

variable "service_name" {
  type        = string
  default     = "erlmcp"
  description = "Name of the Cloud Run service"
}

variable "container_image" {
  type        = string
  description = "Container image to deploy (must be in Artifact Registry)"
}

variable "secrets" {
  type        = map(string)
  default     = {
    "ERLMCP_ERLANG_COOKIE" = "erlmcp-erlang-cookie"
    "ERLMCP_DATABASE_URL" = "erlmcp-database-url"
    "ERLMCP_JWT_SECRET" = "erlmcp-jwt-secret"
  }
  description = "Map of secret names to Cloud Run secret environment variables"
}

variable "environment_variables" {
  type        = map(string)
  default     = {
    "GOOGLE_CLOUD_PROJECT" = var.project_id
    "ENVIRONMENT" = "production"
    "LOG_LEVEL" = "info"
    "ENABLE_METRICS" = "true"
    "ENABLE_TRACING" = "true"
  }
  description = "Environment variables to set on the Cloud Run service"
}

variable "cpu" {
  type        = number
  default     = 1
  description = "CPU allocation for the Cloud Run service"
}

variable "memory" {
  type        = number
  default     = 512
  description = "Memory allocation in MiB for the Cloud Run service"
}

variable "max_instances" {
  type        = number
  default     = 100
  description = "Maximum number of instances"
}

variable "min_instances" {
  type        = number
  default     = 0
  description = "Minimum number of instances"
}

variable "concurrency" {
  type        = number
  default     = 80
  description = "Concurrency level per instance"
}

variable "timeout" {
  type        = number
  default     = 300
  description = "Request timeout in seconds"
}

variable "enable_traffic_splitting" {
  type        = bool
  default     = false
  description = "Enable traffic splitting for blue-green deployments"
}

variable "traffic_split_percent" {
  type        = number
  default     = 100
  description = "Traffic percentage for current revision (when splitting enabled)"
}

variable "ingress_settings" {
  type        = string
  default     = "ALL"
  description = "Ingress settings: 'ALL', 'INTERNAL', 'INTERNAL_AND Cloud_LB'"
}

variable "service_account_email" {
  type        = string
  default     = ""
  description = "Service account for Cloud Run service"
}

variable "labels" {
  type        = map(string)
  default     = {
    app = "erlmcp"
    type = "marketplace"
  }
  description = "Labels to apply to the service"
}

variable "annotations" {
  type        = map(string)
  default     = {
    "marketplace.cloud.google.com/deployment-type" = "cloudrun"
  }
  description = "Additional annotations for the Cloud Run service"
}

variable "domain_mapping" {
  type        = string
  default     = ""
  description = "Custom domain mapping (e.g., 'erlmcp.example.com')"
}

# Outputs
output "service_url" {
  value = module.erlmcp_cloudrun.service_url
}

output "service_name" {
  value = module.erlmcp_cloudrun.service_name
}

output "location" {
  value = module.erlmcp_cloudrun.location
}

output "service_account" {
  value = module.erlmcp_cloudrun.service_account
}

output "domain_url" {
  value = module.erlmcp_cloudrun.domain_url
}

output "secrets" {
  value = module.erlmcp_cloudrun.secrets
}

output "revision" {
  value = module.erlmcp_cloudrun.revision
}

output "ingress" {
  value = module.erlmcp_cloudrun.ingress
}