# GCP DNS + CDN Terraform Module

Enterprise-grade Terraform module for deploying Cloud DNS, Cloud CDN, and SSL certificates on Google Cloud Platform.

## Features

- **Cloud DNS**: Managed DNS zone with DNSSEC support
- **Cloud CDN**: Global content delivery with caching policies
- **SSL/TLS**: Google-managed or self-managed certificates
- **Load Balancing**: Global HTTPS/HTTP load balancer
- **Security**: Cloud Armor integration for DDoS protection
- **Monitoring**: Backend logging and metrics

## Architecture

```
Internet → Cloud DNS → Global Load Balancer → Cloud CDN → Backend (Bucket/Service)
                             ↓
                        SSL Certificate
                             ↓
                        Cloud Armor (optional)
```

## Usage

### Basic Example (Cloud Storage Backend)

```hcl
module "cdn" {
  source = "./modules/dns-cdn"

  project_id   = "my-project-id"
  name_prefix  = "myapp"
  dns_zone_name = "myapp-zone"
  dns_domain    = "example.com."

  ssl_certificate_type = "google_managed"
  ssl_domains          = ["example.com", "www.example.com"]

  backend_type = "bucket"

  labels = {
    environment = "production"
    managed_by  = "terraform"
  }
}
```

### Advanced Example (Backend Service)

```hcl
module "cdn" {
  source = "./modules/dns-cdn"

  project_id   = "my-project-id"
  name_prefix  = "myapp"
  dns_zone_name = "myapp-zone"
  dns_domain    = "example.com."

  ssl_certificate_type = "google_managed"
  ssl_domains          = ["example.com"]
  ssl_min_tls_version  = "TLS_1_3"

  backend_type     = "service"
  backend_protocol = "HTTPS"
  backend_groups = [
    {
      group           = google_compute_instance_group.backend.id
      balancing_mode  = "UTILIZATION"
      capacity_scaler = 1.0
      max_utilization = 0.8
    }
  ]
  backend_health_checks = [google_compute_health_check.default.id]

  cdn_cache_mode  = "CACHE_ALL_STATIC"
  cdn_default_ttl = 3600
  cdn_max_ttl     = 86400

  create_security_policy = true
  security_policy_rules = [
    {
      action   = "deny(403)"
      priority = 1000
      match = {
        versioned_expr = "SRC_IPS_V1"
        config = {
          src_ip_ranges = ["203.0.113.0/24"]
        }
      }
      description = "Block malicious IPs"
    },
    {
      action   = "allow"
      priority = 2147483647
      match = {
        versioned_expr = "SRC_IPS_V1"
        config = {
          src_ip_ranges = ["*"]
        }
      }
      description = "Default allow rule"
    }
  ]

  labels = {
    environment = "production"
  }
}
```

### Self-Managed SSL Certificate

```hcl
module "cdn" {
  source = "./modules/dns-cdn"

  project_id   = "my-project-id"
  name_prefix  = "myapp"
  dns_zone_name = "myapp-zone"
  dns_domain    = "example.com."

  ssl_certificate_type = "self_managed"
  ssl_private_key      = file("certs/private-key.pem")
  ssl_certificate      = file("certs/certificate.pem")

  backend_type = "bucket"
}
```

## Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|----------|
| project_id | GCP project ID | string | - | yes |
| name_prefix | Prefix for all resource names | string | - | yes |
| dns_zone_name | Name of the Cloud DNS managed zone | string | - | yes |
| dns_domain | DNS domain name (must end with dot) | string | - | yes |
| ssl_certificate_type | Type of SSL certificate (google_managed, self_managed, existing) | string | "google_managed" | no |
| backend_type | Type of backend (bucket or service) | string | "bucket" | no |
| cdn_cache_mode | CDN cache mode | string | "CACHE_ALL_STATIC" | no |
| create_http_redirect | Create HTTP to HTTPS redirect | bool | true | no |

See [variables.tf](./variables.tf) for complete list of inputs.

## Outputs

| Name | Description |
|------|-------------|
| dns_zone_name_servers | Name servers for DNS delegation |
| cdn_ip_address | Global static IP address |
| ssl_certificate_status | Status of managed SSL certificate |
| endpoints | CDN endpoints (HTTPS/HTTP) |

See [outputs.tf](./outputs.tf) for complete list of outputs.

## DNS Configuration

After deployment, configure your domain registrar with the name servers:

```bash
terraform output dns_zone_name_servers
```

## SSL Certificate Provisioning

Google-managed SSL certificates require DNS validation and may take 10-60 minutes to provision.

Monitor status:

```bash
gcloud compute ssl-certificates list
```

## CDN Cache Invalidation

```bash
gcloud compute url-maps invalidate-cdn-cache URL_MAP_NAME \
  --path "/*" \
  --async
```

## Requirements

- Terraform >= 1.0
- Google Cloud Provider >= 4.0
- GCP Project with APIs enabled:
  - Cloud DNS API
  - Compute Engine API
  - Cloud Storage API (if using bucket backend)

## Security Considerations

- Enable DNSSEC for DNS zone
- Use TLS 1.2 or higher
- Implement Cloud Armor rules for DDoS protection
- Enable backend logging for audit trails
- Use IAM least privilege for service accounts

## Cost Optimization

- Use appropriate CDN cache TTLs
- Leverage Cloud Storage lifecycle policies
- Monitor egress traffic
- Use signed URLs for private content

## License

Apache 2.0
