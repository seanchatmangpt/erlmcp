variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "name_prefix" {
  description = "Prefix for all resource names"
  type        = string
}

variable "labels" {
  description = "Labels to apply to all resources"
  type        = map(string)
  default     = {}
}

# DNS Variables
variable "dns_zone_name" {
  description = "Name of the Cloud DNS managed zone"
  type        = string
}

variable "dns_domain" {
  description = "DNS domain name (must end with dot)"
  type        = string
}

variable "dns_zone_description" {
  description = "Description of the DNS zone"
  type        = string
  default     = "Managed by Terraform"
}

variable "dnssec_enabled" {
  description = "Enable DNSSEC for the DNS zone"
  type        = bool
  default     = false
}

variable "dns_zone_visibility" {
  description = "Visibility of the DNS zone (public or private)"
  type        = string
  default     = "public"
}

variable "private_visibility_networks" {
  description = "List of VPC network URLs for private DNS zone visibility"
  type        = list(string)
  default     = []
}

variable "create_a_record" {
  description = "Create A record for root domain"
  type        = bool
  default     = true
}

variable "create_www_cname" {
  description = "Create CNAME record for www subdomain"
  type        = bool
  default     = true
}

variable "dns_ttl" {
  description = "TTL for DNS records"
  type        = number
  default     = 300
}

variable "custom_dns_records" {
  description = "Map of custom DNS records to create"
  type = map(object({
    name    = string
    type    = string
    ttl     = number
    rrdatas = list(string)
  }))
  default = {}
}

# SSL Certificate Variables
variable "ssl_certificate_type" {
  description = "Type of SSL certificate (google_managed, self_managed, or existing)"
  type        = string
  default     = "google_managed"
  validation {
    condition     = contains(["google_managed", "self_managed", "existing"], var.ssl_certificate_type)
    error_message = "ssl_certificate_type must be google_managed, self_managed, or existing"
  }
}

variable "ssl_domains" {
  description = "List of domains for managed SSL certificate"
  type        = list(string)
  default     = []
}

variable "ssl_private_key" {
  description = "Private key for self-managed SSL certificate"
  type        = string
  default     = ""
  sensitive   = true
}

variable "ssl_certificate" {
  description = "Certificate for self-managed SSL certificate"
  type        = string
  default     = ""
  sensitive   = true
}

variable "existing_ssl_certificates" {
  description = "List of existing SSL certificate IDs to use"
  type        = list(string)
  default     = []
}

variable "create_ssl_policy" {
  description = "Create SSL policy for the load balancer"
  type        = bool
  default     = true
}

variable "ssl_policy_profile" {
  description = "SSL policy profile (COMPATIBLE, MODERN, RESTRICTED, CUSTOM)"
  type        = string
  default     = "MODERN"
}

variable "ssl_min_tls_version" {
  description = "Minimum TLS version (TLS_1_0, TLS_1_1, TLS_1_2, TLS_1_3)"
  type        = string
  default     = "TLS_1_2"
}

variable "existing_ssl_policy" {
  description = "Existing SSL policy ID to use"
  type        = string
  default     = null
}

# Backend Variables
variable "backend_type" {
  description = "Type of backend (bucket or service)"
  type        = string
  default     = "bucket"
  validation {
    condition     = contains(["bucket", "service"], var.backend_type)
    error_message = "backend_type must be bucket or service"
  }
}

# Cloud Storage Bucket Variables
variable "cdn_bucket_name" {
  description = "Name of the Cloud Storage bucket for CDN (auto-generated if empty)"
  type        = string
  default     = ""
}

variable "cdn_bucket_location" {
  description = "Location of the CDN bucket"
  type        = string
  default     = "US"
}

variable "cdn_bucket_storage_class" {
  description = "Storage class of the CDN bucket"
  type        = string
  default     = "STANDARD"
}

variable "cdn_bucket_force_destroy" {
  description = "Allow bucket deletion even if not empty"
  type        = bool
  default     = false
}

variable "cdn_bucket_versioning" {
  description = "Enable versioning for the CDN bucket"
  type        = bool
  default     = false
}

variable "cdn_bucket_lifecycle_rules" {
  description = "Lifecycle rules for the CDN bucket"
  type = list(object({
    action = object({
      type          = string
      storage_class = optional(string)
    })
    condition = object({
      age                   = optional(number)
      created_before        = optional(string)
      with_state            = optional(string)
      matches_storage_class = optional(list(string))
      num_newer_versions    = optional(number)
    })
  }))
  default = []
}

variable "cdn_bucket_cors_origins" {
  description = "CORS origins for the CDN bucket"
  type        = list(string)
  default     = ["*"]
}

variable "cdn_bucket_cors_methods" {
  description = "CORS methods for the CDN bucket"
  type        = list(string)
  default     = ["GET", "HEAD"]
}

variable "cdn_bucket_cors_response_headers" {
  description = "CORS response headers for the CDN bucket"
  type        = list(string)
  default     = ["*"]
}

variable "cdn_bucket_cors_max_age" {
  description = "CORS max age in seconds for the CDN bucket"
  type        = number
  default     = 3600
}

# Backend Service Variables
variable "backend_protocol" {
  description = "Protocol for backend service"
  type        = string
  default     = "HTTP"
}

variable "backend_port_name" {
  description = "Named port for backend service"
  type        = string
  default     = "http"
}

variable "backend_timeout_sec" {
  description = "Timeout for backend service in seconds"
  type        = number
  default     = 30
}

variable "backend_connection_draining_timeout" {
  description = "Connection draining timeout in seconds"
  type        = number
  default     = 300
}

variable "backend_groups" {
  description = "List of backend instance groups"
  type = list(object({
    group           = string
    balancing_mode  = string
    capacity_scaler = number
    max_utilization = optional(number)
  }))
  default = []
}

variable "backend_health_checks" {
  description = "List of health check IDs for backend service"
  type        = list(string)
  default     = []
}

variable "backend_iap_config" {
  description = "IAP configuration for backend service"
  type = object({
    oauth2_client_id     = string
    oauth2_client_secret = string
  })
  default   = null
  sensitive = true
}

variable "backend_security_policy" {
  description = "Security policy ID for backend service"
  type        = string
  default     = null
}

variable "backend_enable_logging" {
  description = "Enable logging for backend service"
  type        = bool
  default     = true
}

variable "backend_log_sample_rate" {
  description = "Sample rate for backend service logging (0.0 to 1.0)"
  type        = number
  default     = 1.0
}

# CDN Cache Policy Variables
variable "cdn_cache_mode" {
  description = "CDN cache mode (CACHE_ALL_STATIC, USE_ORIGIN_HEADERS, FORCE_CACHE_ALL)"
  type        = string
  default     = "CACHE_ALL_STATIC"
}

variable "cdn_client_ttl" {
  description = "CDN client TTL in seconds"
  type        = number
  default     = 3600
}

variable "cdn_default_ttl" {
  description = "CDN default TTL in seconds"
  type        = number
  default     = 3600
}

variable "cdn_max_ttl" {
  description = "CDN max TTL in seconds"
  type        = number
  default     = 86400
}

variable "cdn_negative_caching" {
  description = "Enable negative caching for CDN"
  type        = bool
  default     = false
}

variable "cdn_serve_while_stale" {
  description = "Serve stale content while revalidating in seconds"
  type        = number
  default     = 0
}

variable "cdn_signed_url_cache_max_age" {
  description = "Max age for signed URL cache in seconds"
  type        = number
  default     = 0
}

variable "cdn_negative_caching_policy" {
  description = "Negative caching policy for specific HTTP status codes"
  type = list(object({
    code = number
    ttl  = number
  }))
  default = []
}

variable "cdn_cache_key_policy" {
  description = "Cache key policy for CDN"
  type = object({
    include_host           = optional(bool)
    include_protocol       = optional(bool)
    include_query_string   = optional(bool)
    query_string_whitelist = optional(list(string))
    query_string_blacklist = optional(list(string))
    include_http_headers   = optional(list(string))
  })
  default = null
}

variable "cdn_custom_response_headers" {
  description = "Custom response headers for CDN"
  type        = map(string)
  default     = {}
}

# URL Map Variables
variable "url_map_host_rules" {
  description = "Host rules for URL map"
  type = list(object({
    hosts        = list(string)
    path_matcher = string
  }))
  default = []
}

variable "url_map_path_matchers" {
  description = "Path matchers for URL map"
  type = list(object({
    name            = string
    default_service = string
    path_rules = list(object({
      paths   = list(string)
      service = string
    }))
  }))
  default = []
}

# Load Balancer Variables
variable "create_http_redirect" {
  description = "Create HTTP to HTTPS redirect"
  type        = bool
  default     = true
}

variable "quic_override" {
  description = "QUIC override (NONE, ENABLE, DISABLE)"
  type        = string
  default     = "NONE"
}

variable "http_keep_alive_timeout" {
  description = "HTTP keep-alive timeout in seconds"
  type        = number
  default     = 610
}

# Cloud Armor Variables
variable "create_security_policy" {
  description = "Create Cloud Armor security policy"
  type        = bool
  default     = false
}

variable "security_policy_rules" {
  description = "Cloud Armor security policy rules"
  type = list(object({
    action      = string
    priority    = number
    description = optional(string)
    preview     = optional(bool)
    match = object({
      versioned_expr = optional(string)
      config = optional(object({
        src_ip_ranges = list(string)
      }))
      expr = optional(object({
        expression = string
      }))
    })
    rate_limit_options = optional(object({
      conform_action = string
      exceed_action  = string
      threshold = object({
        count        = number
        interval_sec = number
      })
      enforce_on_key = optional(string)
    }))
  }))
  default = []
}

variable "security_policy_adaptive_protection" {
  description = "Enable adaptive protection for Cloud Armor"
  type        = bool
  default     = false
}
