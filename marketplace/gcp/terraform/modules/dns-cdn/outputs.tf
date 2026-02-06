output "dns_zone_id" {
  description = "ID of the Cloud DNS managed zone"
  value       = google_dns_managed_zone.main.id
}

output "dns_zone_name" {
  description = "Name of the Cloud DNS managed zone"
  value       = google_dns_managed_zone.main.name
}

output "dns_zone_name_servers" {
  description = "List of name servers for the DNS zone"
  value       = google_dns_managed_zone.main.name_servers
}

output "dns_domain" {
  description = "DNS domain name"
  value       = google_dns_managed_zone.main.dns_name
}

output "cdn_ip_address" {
  description = "Global static IP address for CDN"
  value       = google_compute_global_address.cdn_ip.address
}

output "cdn_ip_id" {
  description = "ID of the global static IP address"
  value       = google_compute_global_address.cdn_ip.id
}

output "ssl_certificate_id" {
  description = "ID of the SSL certificate"
  value = var.ssl_certificate_type == "google_managed" ? (
    length(google_compute_managed_ssl_certificate.default) > 0 ? google_compute_managed_ssl_certificate.default[0].id : null
    ) : var.ssl_certificate_type == "self_managed" ? (
    length(google_compute_ssl_certificate.self_managed) > 0 ? google_compute_ssl_certificate.self_managed[0].id : null
  ) : null
}

output "ssl_certificate_status" {
  description = "Status of the managed SSL certificate"
  value = var.ssl_certificate_type == "google_managed" && length(google_compute_managed_ssl_certificate.default) > 0 ? {
    domains        = google_compute_managed_ssl_certificate.default[0].managed[0].domains
    domain_status  = google_compute_managed_ssl_certificate.default[0].managed[0].domain_status
    certificate_id = google_compute_managed_ssl_certificate.default[0].certificate_id
  } : null
}

output "ssl_policy_id" {
  description = "ID of the SSL policy"
  value       = var.create_ssl_policy && length(google_compute_ssl_policy.default) > 0 ? google_compute_ssl_policy.default[0].id : null
}

output "cdn_bucket_name" {
  description = "Name of the CDN Cloud Storage bucket"
  value       = var.backend_type == "bucket" && length(google_storage_bucket.cdn_bucket) > 0 ? google_storage_bucket.cdn_bucket[0].name : null
}

output "cdn_bucket_url" {
  description = "URL of the CDN Cloud Storage bucket"
  value       = var.backend_type == "bucket" && length(google_storage_bucket.cdn_bucket) > 0 ? google_storage_bucket.cdn_bucket[0].url : null
}

output "backend_bucket_id" {
  description = "ID of the backend bucket"
  value       = var.backend_type == "bucket" && length(google_compute_backend_bucket.cdn_backend) > 0 ? google_compute_backend_bucket.cdn_backend[0].id : null
}

output "backend_service_id" {
  description = "ID of the backend service"
  value       = var.backend_type == "service" && length(google_compute_backend_service.cdn_backend) > 0 ? google_compute_backend_service.cdn_backend[0].id : null
}

output "url_map_id" {
  description = "ID of the URL map"
  value       = google_compute_url_map.default.id
}

output "target_https_proxy_id" {
  description = "ID of the target HTTPS proxy"
  value       = google_compute_target_https_proxy.default.id
}

output "target_http_proxy_id" {
  description = "ID of the target HTTP proxy"
  value       = var.create_http_redirect && length(google_compute_target_http_proxy.default) > 0 ? google_compute_target_http_proxy.default[0].id : null
}

output "https_forwarding_rule_id" {
  description = "ID of the HTTPS forwarding rule"
  value       = google_compute_global_forwarding_rule.https.id
}

output "http_forwarding_rule_id" {
  description = "ID of the HTTP forwarding rule"
  value       = var.create_http_redirect && length(google_compute_global_forwarding_rule.http) > 0 ? google_compute_global_forwarding_rule.http[0].id : null
}

output "security_policy_id" {
  description = "ID of the Cloud Armor security policy"
  value       = var.create_security_policy && length(google_compute_security_policy.cdn_security_policy) > 0 ? google_compute_security_policy.cdn_security_policy[0].id : null
}

output "load_balancer_ip" {
  description = "Load balancer IP address (same as cdn_ip_address)"
  value       = google_compute_global_address.cdn_ip.address
}

output "cdn_enabled" {
  description = "Whether CDN is enabled"
  value       = true
}

output "backend_type" {
  description = "Type of backend used (bucket or service)"
  value       = var.backend_type
}

output "endpoints" {
  description = "CDN endpoints for accessing the service"
  value = {
    https = "https://${trimsuffix(var.dns_domain, ".")}"
    http  = var.create_http_redirect ? "http://${trimsuffix(var.dns_domain, ".")}" : null
  }
}
