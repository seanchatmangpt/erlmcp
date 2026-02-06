variable "project_id" {
  description = "GCP Project ID"
  type        = string
}

variable "location" {
  description = "Primary location for the repository"
  type        = string
}

variable "repository_id" {
  description = "Repository ID"
  type        = string
}

variable "description" {
  description = "Description of the repository"
  type        = string
  default     = ""
}

variable "format" {
  description = "Repository format (DOCKER, MAVEN, NPM, PYTHON, APT, YUM, GO, GOOGET, KFP)"
  type        = string
  default     = "DOCKER"

  validation {
    condition     = contains(["DOCKER", "MAVEN", "NPM", "PYTHON", "APT", "YUM", "GO", "GOOGET", "KFP"], var.format)
    error_message = "Format must be one of: DOCKER, MAVEN, NPM, PYTHON, APT, YUM, GO, GOOGET, KFP"
  }
}

variable "mode" {
  description = "Repository mode (STANDARD_REPOSITORY, VIRTUAL_REPOSITORY, REMOTE_REPOSITORY)"
  type        = string
  default     = "STANDARD_REPOSITORY"

  validation {
    condition     = contains(["STANDARD_REPOSITORY", "VIRTUAL_REPOSITORY", "REMOTE_REPOSITORY"], var.mode)
    error_message = "Mode must be one of: STANDARD_REPOSITORY, VIRTUAL_REPOSITORY, REMOTE_REPOSITORY"
  }
}

variable "kms_key_name" {
  description = "KMS key name for CMEK encryption (format: projects/{project}/locations/{location}/keyRings/{keyring}/cryptoKeys/{key})"
  type        = string
  default     = null
}

variable "replica_locations" {
  description = "List of additional locations for multi-region replication"
  type        = list(string)
  default     = []
}

variable "cleanup_policies" {
  description = "List of cleanup policies for the repository"
  type = list(object({
    id     = string
    action = string
    condition = optional(object({
      tag_state             = optional(string)
      tag_prefixes          = optional(list(string))
      version_name_prefixes = optional(list(string))
      package_name_prefixes = optional(list(string))
      older_than            = optional(string)
      newer_than            = optional(string)
    }))
    most_recent_versions = optional(object({
      package_name_prefixes = optional(list(string))
      keep_count            = optional(number)
    }))
  }))
  default = [
    {
      id     = "delete-untagged"
      action = "DELETE"
      condition = {
        tag_state  = "UNTAGGED"
        older_than = "2592000s" # 30 days
      }
      most_recent_versions = null
    },
    {
      id     = "keep-recent-versions"
      action = "KEEP"
      condition = null
      most_recent_versions = {
        keep_count = 10
      }
    }
  ]
}

variable "docker_immutable_tags" {
  description = "Enable immutable tags for Docker repositories"
  type        = bool
  default     = false
}

variable "maven_allow_snapshot_overwrites" {
  description = "Allow snapshot overwrites for Maven repositories"
  type        = bool
  default     = null
}

variable "maven_version_policy" {
  description = "Version policy for Maven repositories (RELEASE, SNAPSHOT)"
  type        = string
  default     = null

  validation {
    condition     = var.maven_version_policy == null || contains(["RELEASE", "SNAPSHOT"], var.maven_version_policy)
    error_message = "Maven version policy must be RELEASE or SNAPSHOT"
  }
}

variable "upstream_repositories" {
  description = "Upstream repositories for virtual repository mode"
  type = list(object({
    id         = string
    repository = string
    priority   = number
  }))
  default = []
}

variable "remote_repository_config" {
  description = "Configuration for remote repository mode"
  type = object({
    description = optional(string)
    docker_repository = optional(object({
      public_repository = optional(string)
      custom_repository = optional(object({
        uri = string
      }))
    }))
    maven_repository = optional(object({
      public_repository = optional(string)
      custom_repository = optional(object({
        uri = string
      }))
    }))
    npm_repository = optional(object({
      public_repository = optional(string)
      custom_repository = optional(object({
        uri = string
      }))
    }))
    python_repository = optional(object({
      public_repository = optional(string)
      custom_repository = optional(object({
        uri = string
      }))
    }))
    apt_repository = optional(object({
      public_repository = object({
        repository_base = string
        repository_path = string
      })
    }))
    yum_repository = optional(object({
      public_repository = object({
        repository_base = string
        repository_path = string
      })
    }))
    upstream_credentials = optional(object({
      username_password_credentials = optional(object({
        username                = string
        password_secret_version = string
      }))
    }))
  })
  default = null
}

variable "iam_bindings" {
  description = "IAM role bindings for the repository"
  type        = map(list(string))
  default = {
    "roles/artifactregistry.reader" = []
    "roles/artifactregistry.writer" = []
  }
}

variable "labels" {
  description = "Labels to apply to the repository"
  type        = map(string)
  default     = {}
}
