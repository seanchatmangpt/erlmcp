terraform {
  required_version = ">= 1.0"
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 4.0"
    }
  }
}

# Cloud DNS Managed Zone
resource "google_dns_managed_zone" "main" {
  name        = var.dns_zone_name
  dns_name    = var.dns_domain
  description = var.dns_zone_description
  project     = var.project_id

  dnssec_config {
    state         = var.dnssec_enabled ? "on" : "off"
    non_existence = var.dnssec_enabled ? "nsec3" : null
  }

  visibility = var.dns_zone_visibility

  dynamic "private_visibility_config" {
    for_each = var.dns_zone_visibility == "private" ? [1] : []
    content {
      dynamic "networks" {
        for_each = var.private_visibility_networks
        content {
          network_url = networks.value
        }
      }
    }
  }

  labels = var.labels
}

# DNS Records
resource "google_dns_record_set" "a_record" {
  count        = var.create_a_record ? 1 : 0
  name         = var.dns_domain
  type         = "A"
  ttl          = var.dns_ttl
  managed_zone = google_dns_managed_zone.main.name
  project      = var.project_id
  rrdatas      = [google_compute_global_address.cdn_ip.address]
}

resource "google_dns_record_set" "www_cname" {
  count        = var.create_www_cname ? 1 : 0
  name         = "www.${var.dns_domain}"
  type         = "CNAME"
  ttl          = var.dns_ttl
  managed_zone = google_dns_managed_zone.main.name
  project      = var.project_id
  rrdatas      = [var.dns_domain]
}

resource "google_dns_record_set" "custom_records" {
  for_each     = var.custom_dns_records
  name         = each.value.name
  type         = each.value.type
  ttl          = each.value.ttl
  managed_zone = google_dns_managed_zone.main.name
  project      = var.project_id
  rrdatas      = each.value.rrdatas
}

# Global Static IP Address
resource "google_compute_global_address" "cdn_ip" {
  name         = "${var.name_prefix}-cdn-ip"
  project      = var.project_id
  address_type = "EXTERNAL"
  ip_version   = "IPV4"
}

# SSL Certificate - Google Managed
resource "google_compute_managed_ssl_certificate" "default" {
  count   = var.ssl_certificate_type == "google_managed" ? 1 : 0
  name    = "${var.name_prefix}-ssl-cert"
  project = var.project_id

  managed {
    domains = var.ssl_domains
  }

  lifecycle {
    create_before_destroy = true
  }
}

# SSL Certificate - Self Managed
resource "google_compute_ssl_certificate" "self_managed" {
  count       = var.ssl_certificate_type == "self_managed" ? 1 : 0
  name        = "${var.name_prefix}-ssl-cert"
  project     = var.project_id
  private_key = var.ssl_private_key
  certificate = var.ssl_certificate

  lifecycle {
    create_before_destroy = true
  }
}

# SSL Policy
resource "google_compute_ssl_policy" "default" {
  count           = var.create_ssl_policy ? 1 : 0
  name            = "${var.name_prefix}-ssl-policy"
  project         = var.project_id
  profile         = var.ssl_policy_profile
  min_tls_version = var.ssl_min_tls_version
}

# Cloud Storage Bucket for CDN (if backend type is bucket)
resource "google_storage_bucket" "cdn_bucket" {
  count         = var.backend_type == "bucket" ? 1 : 0
  name          = var.cdn_bucket_name != "" ? var.cdn_bucket_name : "${var.name_prefix}-cdn-bucket"
  project       = var.project_id
  location      = var.cdn_bucket_location
  storage_class = var.cdn_bucket_storage_class
  force_destroy = var.cdn_bucket_force_destroy

  uniform_bucket_level_access = true

  versioning {
    enabled = var.cdn_bucket_versioning
  }

  dynamic "lifecycle_rule" {
    for_each = var.cdn_bucket_lifecycle_rules
    content {
      action {
        type          = lifecycle_rule.value.action.type
        storage_class = lookup(lifecycle_rule.value.action, "storage_class", null)
      }
      condition {
        age                   = lookup(lifecycle_rule.value.condition, "age", null)
        created_before        = lookup(lifecycle_rule.value.condition, "created_before", null)
        with_state            = lookup(lifecycle_rule.value.condition, "with_state", null)
        matches_storage_class = lookup(lifecycle_rule.value.condition, "matches_storage_class", null)
        num_newer_versions    = lookup(lifecycle_rule.value.condition, "num_newer_versions", null)
      }
    }
  }

  cors {
    origin          = var.cdn_bucket_cors_origins
    method          = var.cdn_bucket_cors_methods
    response_header = var.cdn_bucket_cors_response_headers
    max_age_seconds = var.cdn_bucket_cors_max_age
  }

  labels = var.labels
}

# Backend Bucket for CDN
resource "google_compute_backend_bucket" "cdn_backend" {
  count       = var.backend_type == "bucket" ? 1 : 0
  name        = "${var.name_prefix}-cdn-backend"
  project     = var.project_id
  bucket_name = google_storage_bucket.cdn_bucket[0].name
  enable_cdn  = true

  cdn_policy {
    cache_mode        = var.cdn_cache_mode
    client_ttl        = var.cdn_client_ttl
    default_ttl       = var.cdn_default_ttl
    max_ttl           = var.cdn_max_ttl
    negative_caching  = var.cdn_negative_caching
    serve_while_stale = var.cdn_serve_while_stale

    dynamic "negative_caching_policy" {
      for_each = var.cdn_negative_caching_policy
      content {
        code = negative_caching_policy.value.code
        ttl  = negative_caching_policy.value.ttl
      }
    }

    dynamic "cache_key_policy" {
      for_each = var.cdn_cache_key_policy != null ? [var.cdn_cache_key_policy] : []
      content {
        query_string_whitelist = lookup(cache_key_policy.value, "query_string_whitelist", null)
      }
    }
  }

  dynamic "custom_response_headers" {
    for_each = var.cdn_custom_response_headers
    content {
      header_name  = custom_response_headers.key
      header_value = custom_response_headers.value
    }
  }
}

# Backend Service for CDN (if backend type is service)
resource "google_compute_backend_service" "cdn_backend" {
  count                           = var.backend_type == "service" ? 1 : 0
  name                            = "${var.name_prefix}-cdn-backend-service"
  project                         = var.project_id
  protocol                        = var.backend_protocol
  port_name                       = var.backend_port_name
  timeout_sec                     = var.backend_timeout_sec
  enable_cdn                      = true
  connection_draining_timeout_sec = var.backend_connection_draining_timeout

  dynamic "backend" {
    for_each = var.backend_groups
    content {
      group           = backend.value.group
      balancing_mode  = backend.value.balancing_mode
      capacity_scaler = backend.value.capacity_scaler
      max_utilization = lookup(backend.value, "max_utilization", null)
    }
  }

  health_checks = var.backend_health_checks

  cdn_policy {
    cache_mode        = var.cdn_cache_mode
    client_ttl        = var.cdn_client_ttl
    default_ttl       = var.cdn_default_ttl
    max_ttl           = var.cdn_max_ttl
    negative_caching  = var.cdn_negative_caching
    serve_while_stale = var.cdn_serve_while_stale
    signed_url_cache_max_age_sec = var.cdn_signed_url_cache_max_age

    dynamic "negative_caching_policy" {
      for_each = var.cdn_negative_caching_policy
      content {
        code = negative_caching_policy.value.code
        ttl  = negative_caching_policy.value.ttl
      }
    }

    dynamic "cache_key_policy" {
      for_each = var.cdn_cache_key_policy != null ? [var.cdn_cache_key_policy] : []
      content {
        include_host           = lookup(cache_key_policy.value, "include_host", true)
        include_protocol       = lookup(cache_key_policy.value, "include_protocol", true)
        include_query_string   = lookup(cache_key_policy.value, "include_query_string", true)
        query_string_whitelist = lookup(cache_key_policy.value, "query_string_whitelist", null)
        query_string_blacklist = lookup(cache_key_policy.value, "query_string_blacklist", null)
        include_http_headers   = lookup(cache_key_policy.value, "include_http_headers", null)
      }
    }
  }

  dynamic "iap" {
    for_each = var.backend_iap_config != null ? [var.backend_iap_config] : []
    content {
      oauth2_client_id     = iap.value.oauth2_client_id
      oauth2_client_secret = iap.value.oauth2_client_secret
    }
  }

  security_policy = var.backend_security_policy

  dynamic "log_config" {
    for_each = var.backend_enable_logging ? [1] : []
    content {
      enable      = true
      sample_rate = var.backend_log_sample_rate
    }
  }
}

# URL Map
resource "google_compute_url_map" "default" {
  name            = "${var.name_prefix}-url-map"
  project         = var.project_id
  default_service = var.backend_type == "bucket" ? google_compute_backend_bucket.cdn_backend[0].id : google_compute_backend_service.cdn_backend[0].id

  dynamic "host_rule" {
    for_each = var.url_map_host_rules
    content {
      hosts        = host_rule.value.hosts
      path_matcher = host_rule.value.path_matcher
    }
  }

  dynamic "path_matcher" {
    for_each = var.url_map_path_matchers
    content {
      name            = path_matcher.value.name
      default_service = path_matcher.value.default_service

      dynamic "path_rule" {
        for_each = path_matcher.value.path_rules
        content {
          paths   = path_rule.value.paths
          service = path_rule.value.service
        }
      }
    }
  }
}

# Target HTTPS Proxy
resource "google_compute_target_https_proxy" "default" {
  name    = "${var.name_prefix}-https-proxy"
  project = var.project_id
  url_map = google_compute_url_map.default.id

  ssl_certificates = var.ssl_certificate_type == "google_managed" ? [
    google_compute_managed_ssl_certificate.default[0].id
    ] : var.ssl_certificate_type == "self_managed" ? [
    google_compute_ssl_certificate.self_managed[0].id
  ] : var.existing_ssl_certificates

  ssl_policy      = var.create_ssl_policy ? google_compute_ssl_policy.default[0].id : var.existing_ssl_policy
  quic_override   = var.quic_override
  http_keep_alive_timeout_sec = var.http_keep_alive_timeout
}

# Target HTTP Proxy (for redirect)
resource "google_compute_target_http_proxy" "default" {
  count   = var.create_http_redirect ? 1 : 0
  name    = "${var.name_prefix}-http-proxy"
  project = var.project_id
  url_map = google_compute_url_map.http_redirect[0].id
}

# URL Map for HTTP to HTTPS redirect
resource "google_compute_url_map" "http_redirect" {
  count   = var.create_http_redirect ? 1 : 0
  name    = "${var.name_prefix}-http-redirect"
  project = var.project_id

  default_url_redirect {
    https_redirect         = true
    redirect_response_code = "MOVED_PERMANENTLY_DEFAULT"
    strip_query            = false
  }
}

# Global Forwarding Rule - HTTPS
resource "google_compute_global_forwarding_rule" "https" {
  name                  = "${var.name_prefix}-https-forwarding-rule"
  project               = var.project_id
  ip_protocol           = "TCP"
  load_balancing_scheme = "EXTERNAL"
  port_range            = "443"
  target                = google_compute_target_https_proxy.default.id
  ip_address            = google_compute_global_address.cdn_ip.id
  labels                = var.labels
}

# Global Forwarding Rule - HTTP (for redirect)
resource "google_compute_global_forwarding_rule" "http" {
  count                 = var.create_http_redirect ? 1 : 0
  name                  = "${var.name_prefix}-http-forwarding-rule"
  project               = var.project_id
  ip_protocol           = "TCP"
  load_balancing_scheme = "EXTERNAL"
  port_range            = "80"
  target                = google_compute_target_http_proxy.default[0].id
  ip_address            = google_compute_global_address.cdn_ip.id
  labels                = var.labels
}

# Cloud Armor Security Policy (optional)
resource "google_compute_security_policy" "cdn_security_policy" {
  count   = var.create_security_policy ? 1 : 0
  name    = "${var.name_prefix}-security-policy"
  project = var.project_id

  dynamic "rule" {
    for_each = var.security_policy_rules
    content {
      action   = rule.value.action
      priority = rule.value.priority

      match {
        versioned_expr = lookup(rule.value.match, "versioned_expr", null)

        dynamic "config" {
          for_each = lookup(rule.value.match, "config", null) != null ? [rule.value.match.config] : []
          content {
            src_ip_ranges = lookup(config.value, "src_ip_ranges", null)
          }
        }

        dynamic "expr" {
          for_each = lookup(rule.value.match, "expr", null) != null ? [rule.value.match.expr] : []
          content {
            expression = expr.value.expression
          }
        }
      }

      description = lookup(rule.value, "description", null)
      preview     = lookup(rule.value, "preview", false)

      dynamic "rate_limit_options" {
        for_each = lookup(rule.value, "rate_limit_options", null) != null ? [rule.value.rate_limit_options] : []
        content {
          conform_action = rate_limit_options.value.conform_action
          exceed_action  = rate_limit_options.value.exceed_action

          rate_limit_threshold {
            count        = rate_limit_options.value.threshold.count
            interval_sec = rate_limit_options.value.threshold.interval_sec
          }

          enforce_on_key = lookup(rate_limit_options.value, "enforce_on_key", null)
        }
      }
    }
  }

  adaptive_protection_config {
    layer_7_ddos_defense_config {
      enable = var.security_policy_adaptive_protection
    }
  }
}
