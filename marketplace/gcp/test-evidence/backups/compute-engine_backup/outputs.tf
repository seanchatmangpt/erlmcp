# ============================================================================
# Compute Engine Module Outputs
# ============================================================================

output "instance_names" {
  description = "Names of created instances"
  value       = google_compute_instance.erlmcp[*].name
}

output "instance_ids" {
  description = "IDs of created instances"
  value       = google_compute_instance.erlmcp[*].id
}

output "instance_self_links" {
  description = "Self-links of created instances"
  value       = google_compute_instance.erlmcp[*].self_link
}

output "instance_internal_ips" {
  description = "Internal IP addresses"
  value       = google_compute_instance.erlmcp[*].network_interface[0].network_ip
}

output "instance_external_ips" {
  description = "External/public IP addresses"
  value = compact([
    for iface in google_compute_instance.erlmcp[*].network_interface[0] :
    iface.access_config[0].nat_ip
    if length(iface.access_config) > 0
  ])
}

output "instance_zones" {
  description = "Zones of created instances"
  value       = google_compute_instance.erlmcp[*].zone
}

output "service_account_email" {
  description = "Service account email"
  value       = var.create_service_account ? google_service_account.erlmcp[0].email : null
}

output "instance_group_name" {
  description = "Name of managed instance group (if created)"
  value       = var.create_instance_group ? google_compute_instance_group_manager.erlmcp[0].name : null
}

output "instance_group_url" {
  description = "URL of managed instance group (if created)"
  value       = var.create_instance_group ? google_compute_instance_group_manager.erlmcp[0].self_link : null
}

output "instance_template_name" {
  description = "Name of instance template (if created)"
  value       = var.create_instance_template ? google_compute_instance_template.erlmcp[0].name : null
}

output "health_check_name" {
  description = "Name of health check (if created)"
  value       = var.create_health_check ? google_compute_health_check.erlmcp[0].name : null
}

output "health_check_self_link" {
  description = "Self-link of health check (if created)"
  value       = var.create_health_check ? google_compute_health_check.erlmcp[0].self_link : null
}

output "static_ip_address" {
  description = "Static IP address (if created)"
  value       = var.create_static_ip ? google_compute_address.erlmcp[0].address : null
}

output "firewall_rule_names" {
  description = "Names of firewall rules (if created)"
  value = var.create_firewall_rules ? [
    google_compute_firewall.erlmcp-http[0].name,
    google_compute_firewall.erlmcp-metrics[0].name,
    google_compute_firewall.erlmcp-distribution[0].name
  ] : []
}

output "ssh_command" {
  description = "SSH command to connect to the first instance"
  value = length(google_compute_instance.erlmcp) > 0 ? format(
    "gcloud compute ssh %s --zone=%s --project=%s",
    google_compute_instance.erlmcp[0].name,
    var.zone,
    var.project_id
  ) : null
}

output "health_check_url" {
  description = "Health check URL (first instance)"
  value = length(google_compute_instance.erlmcp) > 0 && length(google_compute_instance.erlmcp[0].network_interface[0].access_config) > 0 ? format(
    "http://%s%s",
    google_compute_instance.erlmcp[0].network_interface[0].access_config[0].nat_ip,
    var.health_check_path
  ) : null
}
