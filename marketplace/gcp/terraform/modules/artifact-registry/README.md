# GCP Artifact Registry Terraform Module

Enterprise-grade Terraform module for GCP Artifact Registry with multi-region replication, cleanup policies, and customer-managed encryption keys (CMEK).

## Features

- **Multi-Region Replication**: Automatic replication across specified GCP regions
- **Cleanup Policies**: Configurable retention and deletion policies for artifacts
- **CMEK Encryption**: Customer-managed encryption keys via Cloud KMS
- **Format Support**: Docker, Maven, NPM, Python, APT, YUM, Go, KFP
- **Repository Modes**: Standard, Virtual, Remote repositories
- **IAM Management**: Automated IAM bindings for primary and replica repositories
- **Immutable Tags**: Optional Docker tag immutability

## Usage

### Basic Docker Repository

```hcl
module "docker_registry" {
  source = "./modules/artifact-registry"

  project_id    = "my-project"
  location      = "us-central1"
  repository_id = "docker-repo"
  description   = "Production Docker images"
  format        = "DOCKER"

  labels = {
    environment = "production"
    managed-by  = "terraform"
  }
}
```

### Multi-Region with CMEK Encryption

```hcl
module "secure_registry" {
  source = "./modules/artifact-registry"

  project_id    = "my-project"
  location      = "us-central1"
  repository_id = "secure-docker"
  format        = "DOCKER"

  # Multi-region replication
  replica_locations = [
    "us-east1",
    "europe-west1",
    "asia-southeast1"
  ]

  # Customer-managed encryption
  kms_key_name = "projects/my-project/locations/us-central1/keyRings/artifact-registry/cryptoKeys/docker-encryption"

  # IAM bindings
  iam_bindings = {
    "roles/artifactregistry.reader" = [
      "serviceAccount:ci-cd@my-project.iam.gserviceaccount.com",
      "group:developers@example.com"
    ]
    "roles/artifactregistry.writer" = [
      "serviceAccount:ci-cd@my-project.iam.gserviceaccount.com"
    ]
  }
}
```

### Advanced Cleanup Policies

```hcl
module "registry_with_cleanup" {
  source = "./modules/artifact-registry"

  project_id    = "my-project"
  location      = "us-central1"
  repository_id = "app-images"
  format        = "DOCKER"

  cleanup_policies = [
    {
      id     = "delete-old-untagged"
      action = "DELETE"
      condition = {
        tag_state  = "UNTAGGED"
        older_than = "604800s" # 7 days
      }
      most_recent_versions = null
    },
    {
      id     = "delete-dev-images"
      action = "DELETE"
      condition = {
        tag_prefixes = ["dev-", "feature-"]
        older_than   = "2592000s" # 30 days
      }
      most_recent_versions = null
    },
    {
      id     = "keep-production-versions"
      action = "KEEP"
      condition = {
        tag_prefixes = ["prod-", "release-"]
      }
      most_recent_versions = {
        keep_count = 50
      }
    },
    {
      id     = "keep-recent-any"
      action = "KEEP"
      condition = null
      most_recent_versions = {
        keep_count = 10
      }
    }
  ]

  docker_immutable_tags = true
}
```

### Virtual Repository

```hcl
module "virtual_registry" {
  source = "./modules/artifact-registry"

  project_id    = "my-project"
  location      = "us-central1"
  repository_id = "virtual-docker"
  format        = "DOCKER"
  mode          = "VIRTUAL_REPOSITORY"

  upstream_repositories = [
    {
      id         = "primary"
      repository = "projects/my-project/locations/us-central1/repositories/docker-repo"
      priority   = 100
    },
    {
      id         = "cache"
      repository = "projects/my-project/locations/us-central1/repositories/docker-cache"
      priority   = 50
    }
  ]
}
```

### Remote Repository (Proxy)

```hcl
module "dockerhub_proxy" {
  source = "./modules/artifact-registry"

  project_id    = "my-project"
  location      = "us-central1"
  repository_id = "dockerhub-proxy"
  format        = "DOCKER"
  mode          = "REMOTE_REPOSITORY"

  remote_repository_config = {
    description = "Docker Hub proxy for faster pulls"
    docker_repository = {
      public_repository = "DOCKER_HUB"
    }
  }
}
```

## Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|----------|
| project_id | GCP Project ID | `string` | n/a | yes |
| location | Primary location for the repository | `string` | n/a | yes |
| repository_id | Repository ID | `string` | n/a | yes |
| format | Repository format | `string` | `"DOCKER"` | no |
| mode | Repository mode | `string` | `"STANDARD_REPOSITORY"` | no |
| kms_key_name | KMS key for CMEK encryption | `string` | `null` | no |
| replica_locations | Additional locations for replication | `list(string)` | `[]` | no |
| cleanup_policies | List of cleanup policies | `list(object)` | See variables.tf | no |
| docker_immutable_tags | Enable immutable tags | `bool` | `false` | no |
| iam_bindings | IAM role bindings | `map(list(string))` | `{}` | no |
| labels | Resource labels | `map(string)` | `{}` | no |

## Outputs

| Name | Description |
|------|-------------|
| repository_id | The ID of the primary repository |
| repository_name | The full resource name |
| repository_url | The URL for pushing/pulling artifacts |
| replica_repositories | Map of replica repositories by location |
| all_repository_urls | All repository URLs (primary + replicas) |
| kms_key_name | KMS key used for encryption |

## Cleanup Policy Examples

### Delete untagged images older than 30 days
```hcl
{
  id     = "delete-untagged"
  action = "DELETE"
  condition = {
    tag_state  = "UNTAGGED"
    older_than = "2592000s"
  }
}
```

### Keep only the 10 most recent versions
```hcl
{
  id     = "keep-recent"
  action = "KEEP"
  most_recent_versions = {
    keep_count = 10
  }
}
```

### Delete feature branch images older than 7 days
```hcl
{
  id     = "delete-feature-branches"
  action = "DELETE"
  condition = {
    tag_prefixes = ["feature-", "dev-"]
    older_than   = "604800s"
  }
}
```

## Security

- **CMEK Encryption**: All data encrypted at rest with customer-managed keys
- **IAM Least Privilege**: Granular role bindings per repository
- **Immutable Tags**: Prevent tag overwrites for production images
- **Audit Logging**: All operations logged to Cloud Audit Logs

## Requirements

- Terraform >= 1.5
- Google Provider >= 5.0
- GCP Project with Artifact Registry API enabled
- KMS API enabled (if using CMEK)
