terraform {
  required_version = ">= 1.5"
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = "~> 5.0"
    }
  }
}

locals {
  mesh_id = "proj-${var.project_number}"
  asm_labels = merge(
    var.labels,
    {
      mesh_id = local.mesh_id
      managed_by = "terraform"
    }
  )
}

resource "google_gke_hub_feature" "mesh" {
  provider = google-beta
  project  = var.project_id
  location = "global"
  name     = "servicemesh"

  fleet_default_member_config {
    mesh {
      management = var.mesh_management
    }
  }

  labels = local.asm_labels
}

resource "google_gke_hub_feature_membership" "mesh" {
  provider = google-beta
  for_each = toset(var.cluster_memberships)

  project  = var.project_id
  location = "global"
  feature  = google_gke_hub_feature.mesh.name
  membership = each.value

  mesh {
    management = var.mesh_management
  }

  configmanagement {
    version = var.config_sync_version
  }
}

resource "google_service_mesh_endpoint_policy" "default" {
  provider = google-beta
  project  = var.project_id
  name     = "${var.name_prefix}-endpoint-policy"

  type               = "GRPC_SERVER"
  server_tls_policy  = google_network_security_server_tls_policy.mtls.id
  endpoint_matcher {
    metadata_label_matcher {
      metadata_label_match_criteria = "MATCH_ALL"
      metadata_labels {
        label_name  = "app"
        label_value = var.app_selector
      }
    }
  }

  labels = local.asm_labels
}

resource "google_network_security_server_tls_policy" "mtls" {
  provider = google-beta
  project  = var.project_id
  name     = "${var.name_prefix}-mtls-policy"
  location = var.region

  description = "mTLS policy for Anthos Service Mesh"

  mtls_policy {
    client_validation_mode = var.mtls_mode
    client_validation_ca {
      grpc_endpoint {
        target_uri = var.ca_pool_endpoint
      }
    }
  }

  server_certificate {
    certificate_provider_instance {
      plugin_instance = var.certificate_provider
    }
  }

  labels = local.asm_labels
}

resource "google_network_security_client_tls_policy" "default" {
  provider = google-beta
  project  = var.project_id
  name     = "${var.name_prefix}-client-tls-policy"
  location = var.region

  description = "Client TLS policy for mTLS"

  client_certificate {
    certificate_provider_instance {
      plugin_instance = var.certificate_provider
    }
  }

  server_validation_ca {
    grpc_endpoint {
      target_uri = var.ca_pool_endpoint
    }
  }

  labels = local.asm_labels
}

resource "google_network_services_gateway" "asm_gateway" {
  provider = google-beta
  project  = var.project_id
  name     = "${var.name_prefix}-gateway"
  location = var.region

  type        = var.gateway_type
  ports       = var.gateway_ports
  scope       = var.gateway_scope

  addresses = var.gateway_addresses

  server_tls_policy = var.enable_mtls ? google_network_security_server_tls_policy.mtls.id : null

  labels = local.asm_labels

  depends_on = [google_gke_hub_feature.mesh]
}

resource "google_network_services_http_route" "default" {
  provider = google-beta
  for_each = var.http_routes

  project  = var.project_id
  name     = "${var.name_prefix}-${each.key}-route"
  location = var.region

  hostnames = each.value.hostnames
  gateways  = [google_network_services_gateway.asm_gateway.id]

  rules {
    matches {
      dynamic "query_parameters" {
        for_each = lookup(each.value, "query_parameters", [])
        content {
          query_parameter = query_parameters.value.name
          exact_match     = lookup(query_parameters.value, "exact_match", null)
          regex_match     = lookup(query_parameters.value, "regex_match", null)
        }
      }

      dynamic "headers" {
        for_each = lookup(each.value, "headers", [])
        content {
          header      = headers.value.name
          exact_match = lookup(headers.value, "exact_match", null)
          regex_match = lookup(headers.value, "regex_match", null)
        }
      }
    }

    action {
      dynamic "destinations" {
        for_each = each.value.destinations
        content {
          service_name = destinations.value.service
          weight       = lookup(destinations.value, "weight", 100)
        }
      }

      dynamic "retry_policy" {
        for_each = lookup(each.value, "retry_policy", null) != null ? [each.value.retry_policy] : []
        content {
          num_retries      = retry_policy.value.num_retries
          retry_conditions = lookup(retry_policy.value, "retry_conditions", ["5xx"])
          per_try_timeout  = lookup(retry_policy.value, "per_try_timeout", "30s")
        }
      }

      timeout = lookup(each.value, "timeout", "30s")

      dynamic "fault_injection_policy" {
        for_each = lookup(each.value, "fault_injection", null) != null ? [each.value.fault_injection] : []
        content {
          dynamic "delay" {
            for_each = lookup(fault_injection_policy.value, "delay", null) != null ? [fault_injection_policy.value.delay] : []
            content {
              fixed_delay = delay.value.fixed_delay
              percentage  = lookup(delay.value, "percentage", 100)
            }
          }

          dynamic "abort" {
            for_each = lookup(fault_injection_policy.value, "abort", null) != null ? [fault_injection_policy.value.abort] : []
            content {
              http_status = abort.value.http_status
              percentage  = lookup(abort.value, "percentage", 100)
            }
          }
        }
      }
    }
  }

  labels = local.asm_labels
}

resource "google_network_services_grpc_route" "default" {
  provider = google-beta
  for_each = var.grpc_routes

  project  = var.project_id
  name     = "${var.name_prefix}-${each.key}-grpc-route"
  location = var.region

  hostnames = each.value.hostnames
  gateways  = [google_network_services_gateway.asm_gateway.id]

  rules {
    matches {
      method {
        grpc_service = each.value.grpc_service
        grpc_method  = lookup(each.value, "grpc_method", null)
      }

      dynamic "headers" {
        for_each = lookup(each.value, "headers", [])
        content {
          key   = headers.value.name
          value = headers.value.value
        }
      }
    }

    action {
      dynamic "destinations" {
        for_each = each.value.destinations
        content {
          service_name = destinations.value.service
          weight       = lookup(destinations.value, "weight", 100)
        }
      }

      timeout = lookup(each.value, "timeout", "30s")

      dynamic "retry_policy" {
        for_each = lookup(each.value, "retry_policy", null) != null ? [each.value.retry_policy] : []
        content {
          num_retries      = retry_policy.value.num_retries
          retry_conditions = lookup(retry_policy.value, "retry_conditions", ["unavailable"])
        }
      }

      dynamic "fault_injection_policy" {
        for_each = lookup(each.value, "fault_injection", null) != null ? [each.value.fault_injection] : []
        content {
          dynamic "delay" {
            for_each = lookup(fault_injection_policy.value, "delay", null) != null ? [fault_injection_policy.value.delay] : []
            content {
              fixed_delay = delay.value.fixed_delay
              percentage  = lookup(delay.value, "percentage", 100)
            }
          }

          dynamic "abort" {
            for_each = lookup(fault_injection_policy.value, "abort", null) != null ? [fault_injection_policy.value.abort] : []
            content {
              http_status = abort.value.http_status
              percentage  = lookup(abort.value, "percentage", 100)
            }
          }
        }
      }
    }
  }

  labels = local.asm_labels
}

resource "google_network_services_tcp_route" "default" {
  provider = google-beta
  for_each = var.tcp_routes

  project  = var.project_id
  name     = "${var.name_prefix}-${each.key}-tcp-route"
  location = var.region

  gateways = [google_network_services_gateway.asm_gateway.id]

  rules {
    matches {
      address = each.value.address
      port    = each.value.port
    }

    action {
      dynamic "destinations" {
        for_each = each.value.destinations
        content {
          service_name = destinations.value.service
          weight       = lookup(destinations.value, "weight", 100)
        }
      }
    }
  }

  labels = local.asm_labels
}

resource "google_network_security_authorization_policy" "default" {
  provider = google-beta
  for_each = var.authz_policies

  project  = var.project_id
  name     = "${var.name_prefix}-${each.key}-authz"
  location = var.region

  action = each.value.action

  dynamic "rules" {
    for_each = each.value.rules
    content {
      dynamic "sources" {
        for_each = lookup(rules.value, "sources", [])
        content {
          principals          = lookup(sources.value, "principals", null)
          ip_blocks           = lookup(sources.value, "ip_blocks", null)
        }
      }

      dynamic "destinations" {
        for_each = lookup(rules.value, "destinations", [])
        content {
          hosts   = lookup(destinations.value, "hosts", null)
          ports   = lookup(destinations.value, "ports", null)
          methods = lookup(destinations.value, "methods", null)

          dynamic "http_header_match" {
            for_each = lookup(destinations.value, "http_headers", [])
            content {
              header_name = http_header_match.value.name
              regex_match = http_header_match.value.regex
            }
          }
        }
      }
    }
  }

  labels = local.asm_labels
}

resource "google_compute_network_endpoint_group" "mesh_neg" {
  provider = google-beta
  for_each = var.network_endpoint_groups

  project               = var.project_id
  name                  = "${var.name_prefix}-${each.key}-neg"
  network               = each.value.network
  subnetwork            = each.value.subnetwork
  zone                  = each.value.zone
  network_endpoint_type = "GCE_VM_IP_PORT"
  default_port          = lookup(each.value, "default_port", null)
}

resource "google_monitoring_alert_policy" "mesh_alerts" {
  provider = google-beta
  for_each = var.enable_monitoring ? toset([
    "high-error-rate",
    "high-latency",
    "low-success-rate"
  ]) : []

  project      = var.project_id
  display_name = "${var.name_prefix}-${each.key}"
  combiner     = "OR"

  conditions {
    display_name = "${each.key} condition"

    condition_threshold {
      filter          = "resource.type=\"k8s_container\" AND metric.type=\"istio.io/service/server/response_latencies\""
      duration        = "60s"
      comparison      = "COMPARISON_GT"
      threshold_value = each.key == "high-latency" ? var.latency_threshold_ms : var.error_rate_threshold

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_RATE"
      }
    }
  }

  notification_channels = var.notification_channels

  alert_strategy {
    auto_close = "1800s"
  }

  documentation {
    content   = "Service Mesh ${each.key} alert triggered"
    mime_type = "text/markdown"
  }
}

resource "google_logging_metric" "mesh_metrics" {
  provider = google-beta
  for_each = var.enable_logging ? toset([
    "request-count",
    "error-count",
    "latency"
  ]) : []

  project = var.project_id
  name    = "${var.name_prefix}_${replace(each.key, "-", "_")}"
  filter  = "resource.type=\"k8s_container\" AND labels.\"istio.io/rev\"!=\"\""

  metric_descriptor {
    metric_kind = "DELTA"
    value_type  = "INT64"
    unit        = each.key == "latency" ? "ms" : "1"

    labels {
      key         = "service_name"
      value_type  = "STRING"
      description = "Service name"
    }

    labels {
      key         = "response_code"
      value_type  = "STRING"
      description = "HTTP response code"
    }
  }

  label_extractors = {
    "service_name"  = "EXTRACT(labels.\"app\")"
    "response_code" = "EXTRACT(labels.\"response_code\")"
  }
}

resource "google_service_directory_namespace" "mesh" {
  count    = var.enable_service_directory ? 1 : 0
  provider = google-beta

  project      = var.project_id
  namespace_id = "${var.name_prefix}-mesh"
  location     = var.region

  labels = local.asm_labels
}

resource "google_service_directory_service" "mesh_services" {
  provider = google-beta
  for_each = var.enable_service_directory ? var.service_directory_services : {}

  project     = var.project_id
  service_id  = each.key
  namespace   = google_service_directory_namespace.mesh[0].id

  metadata = each.value.metadata

  annotations = merge(
    local.asm_labels,
    lookup(each.value, "annotations", {})
  )
}

resource "google_binary_authorization_policy" "mesh_policy" {
  count    = var.enable_binary_authorization ? 1 : 0
  provider = google-beta

  project = var.project_id

  admission_whitelist_patterns {
    name_pattern = "gcr.io/${var.project_id}/*"
  }

  default_admission_rule {
    evaluation_mode  = "REQUIRE_ATTESTATION"
    enforcement_mode = var.binary_authz_enforcement_mode
    require_attestations_by = var.binary_authz_attestors
  }

  global_policy_evaluation_mode = "ENABLE"

  cluster_admission_rules {
    cluster                 = "*"
    evaluation_mode         = "REQUIRE_ATTESTATION"
    enforcement_mode        = var.binary_authz_enforcement_mode
    require_attestations_by = var.binary_authz_attestors
  }
}

resource "google_compute_region_network_endpoint_group" "serverless_neg" {
  provider = google-beta
  for_each = var.serverless_negs

  project               = var.project_id
  name                  = "${var.name_prefix}-${each.key}-serverless-neg"
  region                = var.region
  network_endpoint_type = each.value.type

  dynamic "cloud_run" {
    for_each = each.value.type == "SERVERLESS" && lookup(each.value, "cloud_run_service", null) != null ? [1] : []
    content {
      service = each.value.cloud_run_service
    }
  }

  dynamic "cloud_function" {
    for_each = each.value.type == "SERVERLESS" && lookup(each.value, "cloud_function", null) != null ? [1] : []
    content {
      function = each.value.cloud_function
    }
  }

  dynamic "app_engine" {
    for_each = each.value.type == "SERVERLESS" && lookup(each.value, "app_engine_service", null) != null ? [1] : []
    content {
      service = each.value.app_engine_service
      version = lookup(each.value, "app_engine_version", null)
    }
  }
}

resource "google_iap_brand" "mesh_iap" {
  count             = var.enable_iap ? 1 : 0
  provider          = google-beta
  project           = var.project_id
  support_email     = var.iap_support_email
  application_title = "${var.name_prefix} Service Mesh"
}

resource "google_iap_web_iam_binding" "mesh_iap_users" {
  count   = var.enable_iap ? 1 : 0
  provider = google-beta
  project = var.project_id
  role    = "roles/iap.httpsResourceAccessor"
  members = var.iap_members
}

resource "google_compute_security_policy" "mesh_policy" {
  count    = var.enable_cloud_armor ? 1 : 0
  provider = google-beta

  project = var.project_id
  name    = "${var.name_prefix}-mesh-security-policy"

  dynamic "rule" {
    for_each = var.cloud_armor_rules
    content {
      action      = rule.value.action
      priority    = rule.value.priority
      description = lookup(rule.value, "description", null)

      match {
        versioned_expr = lookup(rule.value, "versioned_expr", "SRC_IPS_V1")

        dynamic "config" {
          for_each = lookup(rule.value, "src_ip_ranges", null) != null ? [1] : []
          content {
            src_ip_ranges = rule.value.src_ip_ranges
          }
        }

        dynamic "expr" {
          for_each = lookup(rule.value, "expression", null) != null ? [1] : []
          content {
            expression = rule.value.expression
          }
        }
      }

      dynamic "rate_limit_options" {
        for_each = lookup(rule.value, "rate_limit", null) != null ? [rule.value.rate_limit] : []
        content {
          conform_action = "allow"
          exceed_action  = "deny(429)"

          rate_limit_threshold {
            count        = rate_limit_options.value.count
            interval_sec = rate_limit_options.value.interval_sec
          }

          enforce_on_key = lookup(rate_limit_options.value, "enforce_on_key", "IP")
        }
      }
    }
  }

  rule {
    action   = "allow"
    priority = "2147483647"
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = ["*"]
      }
    }
    description = "Default allow rule"
  }

  adaptive_protection_config {
    layer_7_ddos_defense_config {
      enable = var.enable_adaptive_protection
    }
  }
}
