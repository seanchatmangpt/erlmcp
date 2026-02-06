# Example 1: Static Website with Cloud Storage Backend
module "static_website_cdn" {
  source = "./modules/dns-cdn"

  project_id    = "my-project-123"
  name_prefix   = "static-web"
  dns_zone_name = "static-web-zone"
  dns_domain    = "mystaticsite.com."

  ssl_certificate_type = "google_managed"
  ssl_domains          = ["mystaticsite.com", "www.mystaticsite.com"]
  ssl_min_tls_version  = "TLS_1_2"

  backend_type              = "bucket"
  cdn_bucket_location       = "US"
  cdn_bucket_storage_class  = "STANDARD"
  cdn_bucket_versioning     = true

  cdn_cache_mode  = "CACHE_ALL_STATIC"
  cdn_client_ttl  = 3600
  cdn_default_ttl = 3600
  cdn_max_ttl     = 86400

  cdn_custom_response_headers = {
    "X-Frame-Options"        = "DENY"
    "X-Content-Type-Options" = "nosniff"
    "Strict-Transport-Security" = "max-age=31536000; includeSubDomains"
  }

  create_http_redirect = true
  create_a_record      = true
  create_www_cname     = true

  labels = {
    environment = "production"
    application = "static-website"
    cost_center = "marketing"
  }
}

# Example 2: API Gateway with Backend Service and Rate Limiting
module "api_cdn" {
  source = "./modules/dns-cdn"

  project_id    = "my-project-123"
  name_prefix   = "api-gateway"
  dns_zone_name = "api-zone"
  dns_domain    = "api.mycompany.com."

  ssl_certificate_type = "google_managed"
  ssl_domains          = ["api.mycompany.com"]
  ssl_policy_profile   = "MODERN"
  ssl_min_tls_version  = "TLS_1_3"

  backend_type        = "service"
  backend_protocol    = "HTTPS"
  backend_port_name   = "https"
  backend_timeout_sec = 30

  backend_groups = [
    {
      group           = "projects/my-project-123/zones/us-central1-a/instanceGroups/api-backend"
      balancing_mode  = "UTILIZATION"
      capacity_scaler = 0.8
      max_utilization = 0.7
    }
  ]

  backend_health_checks    = ["projects/my-project-123/global/healthChecks/api-health-check"]
  backend_enable_logging   = true
  backend_log_sample_rate  = 1.0

  cdn_cache_mode  = "USE_ORIGIN_HEADERS"
  cdn_default_ttl = 0
  cdn_max_ttl     = 3600

  create_security_policy = true
  security_policy_rules = [
    {
      action   = "rate_based_ban"
      priority = 100
      match = {
        versioned_expr = "SRC_IPS_V1"
        config = {
          src_ip_ranges = ["*"]
        }
      }
      rate_limit_options = {
        conform_action = "allow"
        exceed_action  = "deny(429)"
        threshold = {
          count        = 100
          interval_sec = 60
        }
        enforce_on_key = "IP"
      }
      description = "Rate limit to 100 requests per minute per IP"
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
      description = "Default allow"
    }
  ]

  security_policy_adaptive_protection = true

  create_http_redirect = true
  quic_override        = "ENABLE"

  labels = {
    environment = "production"
    application = "api-gateway"
    tier        = "frontend"
  }
}

# Example 3: Multi-Region CDN with Custom Cache Policies
module "global_cdn" {
  source = "./modules/dns-cdn"

  project_id    = "my-project-123"
  name_prefix   = "global-cdn"
  dns_zone_name = "global-zone"
  dns_domain    = "cdn.example.com."

  ssl_certificate_type = "google_managed"
  ssl_domains          = ["cdn.example.com", "*.cdn.example.com"]

  backend_type = "bucket"
  cdn_bucket_location = "US"

  cdn_bucket_lifecycle_rules = [
    {
      action = {
        type          = "SetStorageClass"
        storage_class = "NEARLINE"
      }
      condition = {
        age            = 30
        matches_storage_class = ["STANDARD"]
      }
    },
    {
      action = {
        type = "Delete"
      }
      condition = {
        age        = 365
        with_state = "ARCHIVED"
      }
    }
  ]

  cdn_cache_mode  = "CACHE_ALL_STATIC"
  cdn_client_ttl  = 7200
  cdn_default_ttl = 7200
  cdn_max_ttl     = 604800

  cdn_negative_caching = true
  cdn_negative_caching_policy = [
    { code = 404, ttl = 120 },
    { code = 403, ttl = 60 },
    { code = 500, ttl = 0 }
  ]

  cdn_cache_key_policy = {
    include_host         = true
    include_protocol     = true
    include_query_string = true
    query_string_whitelist = ["v", "format"]
  }

  cdn_serve_while_stale = 86400

  custom_dns_records = {
    status = {
      name    = "status.cdn.example.com."
      type    = "A"
      ttl     = 60
      rrdatas = ["10.0.0.10"]
    }
    txt_verification = {
      name    = "cdn.example.com."
      type    = "TXT"
      ttl     = 300
      rrdatas = ["google-site-verification=abc123"]
    }
  }

  labels = {
    environment = "production"
    application = "global-cdn"
    scope       = "multi-region"
  }
}

# Example 4: Private CDN with VPC Integration
module "private_cdn" {
  source = "./modules/dns-cdn"

  project_id    = "my-project-123"
  name_prefix   = "private-cdn"
  dns_zone_name = "private-zone"
  dns_domain    = "internal.example.com."

  dns_zone_visibility = "private"
  private_visibility_networks = [
    "projects/my-project-123/global/networks/vpc-main",
    "projects/my-project-123/global/networks/vpc-backup"
  ]

  ssl_certificate_type = "self_managed"
  ssl_private_key      = file("${path.module}/certs/internal-key.pem")
  ssl_certificate      = file("${path.module}/certs/internal-cert.pem")

  backend_type     = "service"
  backend_protocol = "HTTPS"

  backend_groups = [
    {
      group           = "projects/my-project-123/zones/us-central1-a/instanceGroups/internal-backend"
      balancing_mode  = "RATE"
      capacity_scaler = 1.0
    }
  ]

  backend_health_checks = ["projects/my-project-123/global/healthChecks/internal-health"]

  backend_iap_config = {
    oauth2_client_id     = "123456789.apps.googleusercontent.com"
    oauth2_client_secret = var.iap_client_secret
  }

  cdn_cache_mode = "USE_ORIGIN_HEADERS"

  create_http_redirect = false
  create_a_record      = true
  create_www_cname     = false

  labels = {
    environment = "production"
    application = "internal-cdn"
    visibility  = "private"
  }
}

# Example 5: E-Commerce with Advanced URL Routing
module "ecommerce_cdn" {
  source = "./modules/dns-cdn"

  project_id    = "my-project-123"
  name_prefix   = "ecommerce"
  dns_zone_name = "ecommerce-zone"
  dns_domain    = "shop.example.com."

  ssl_certificate_type = "google_managed"
  ssl_domains          = ["shop.example.com"]

  backend_type = "service"

  backend_groups = [
    {
      group           = "projects/my-project-123/zones/us-central1-a/instanceGroups/shop-backend"
      balancing_mode  = "UTILIZATION"
      capacity_scaler = 1.0
      max_utilization = 0.8
    }
  ]

  backend_health_checks = ["projects/my-project-123/global/healthChecks/shop-health"]

  url_map_host_rules = [
    {
      hosts        = ["shop.example.com"]
      path_matcher = "shop-paths"
    }
  ]

  url_map_path_matchers = [
    {
      name            = "shop-paths"
      default_service = "projects/my-project-123/global/backendServices/shop-default"
      path_rules = [
        {
          paths   = ["/api/*"]
          service = "projects/my-project-123/global/backendServices/shop-api"
        },
        {
          paths   = ["/static/*", "/images/*"]
          service = "projects/my-project-123/global/backendBuckets/shop-static"
        }
      ]
    }
  ]

  cdn_cache_mode = "CACHE_ALL_STATIC"

  create_security_policy = true
  security_policy_rules = [
    {
      action   = "deny(403)"
      priority = 100
      match = {
        expr = {
          expression = "origin.region_code == 'CN'"
        }
      }
      description = "Block traffic from specific regions"
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
      description = "Default allow"
    }
  ]

  labels = {
    environment = "production"
    application = "ecommerce"
    vertical    = "retail"
  }
}
