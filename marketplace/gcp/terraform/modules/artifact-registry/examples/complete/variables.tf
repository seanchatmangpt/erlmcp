variable "project_id" {
  description = "GCP Project ID"
  type        = string
}

variable "region" {
  description = "Primary GCP region"
  type        = string
  default     = "us-central1"
}
