packer {
  required_version = ">= 1.8.0"

  required_plugins {
    google = {
      version = ">= 1.1.1"
      source  = "github.com/hashicorp/google"
    }
  }
}

variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "image_family" {
  type        = string
  default     = "erlmcp-marketplace"
  description = "Image family name"
}

variable "image_name" {
  type        = string
  default     = "erlmcp-marketplace-v1"
  description = "Image name"
}

variable "zone" {
  type        = string
  default     = "us-central1-a"
  description = "GCP zone for building"
}

source "googlecompute" "erlmcp-vm" {
  project_id          = var.project_id
  zone               = var.zone
  machine_type       = "e2-standard-4"
  image_family       = "debian-12"
  image_name         = "debian-12-bookworm-v20240117"
  ssh_username       = "packer"
  use_internal_ip    = false

  // Network configuration
  network           = "default"
  subnetwork        = "default"

  // Disk configuration
  disk_size         = 50
  disk_type         = "pd-balanced"

  // Metadata for startup
  metadata = {
    gce-container-declaration = file("docker-container.yaml")
    enable-guest-agent       = "true"
  }

  // Enable Shielded VM features
  shielded_vm_config {
    enable_secure_boot          = true
    enable_vtpm                 = true
    enable_integrity_monitoring = true
  }

  // Tags for firewall rules
  tags = ["erlmcp-marketplace", "http-server", "https-server"]
}

build {
  name    = "erlmcp-marketplace-vm"
  sources = ["source.googlecompute.erlmcp-vm"]

  provisioner "file" {
    source      = "scripts/install-ops-agent.sh"
    destination = "/tmp/install-ops-agent.sh"
    direction   = "upload"
  }

  provisioner "file" {
    source      = "scripts/start-erlmcp.sh"
    destination = "/tmp/start-erlmcp.sh"
    direction   = "upload"
  }

  provisioner "shell" {
    inline = [
      "sudo mv /tmp/install-ops-agent.sh /usr/local/bin/",
      "sudo chmod +x /usr/local/bin/install-ops-agent.sh",
      "sudo mv /tmp/start-erlmcp.sh /usr/local/bin/",
      "sudo chmod +x /usr/local/bin/start-erlmcp.sh",

      "sudo apt-get update",
      "sudo apt-get install -y docker.io docker-compose",
      "sudo systemctl start docker",
      "sudo systemctl enable docker",

      "sudo /usr/local/bin/install-ops-agent.sh",

      "sudo mkdir -p /etc/systemd/system",
      "sudo tee /etc/systemd/system/erlmcp.service > /dev/null <<'EOF'",
      "[Unit]",
      "Description=erlmcp Marketplace Service",
      "After=network.target docker.service",
      "Requires=docker.service",
      "",
      "[Service]",
      "Type=oneshot",
      "RemainAfterExit=yes",
      "ExecStart=/usr/local/bin/start-erlmcp.sh",
      "TimeoutStartSec=300",
      "Restart=on-failure",
      "RestartSec=10",
      "",
      "[Install]",
      "WantedBy=multi-user.target",
      "EOF",

      "sudo systemctl daemon-reload",
      "sudo systemctl enable erlmcp",

      "sudo mkdir -p /var/log/erlmcp",
      "sudo chown -R packer:packer /var/log/erlmcp",

      "sudo apt-get install -y syft trivy",
      "syft ${var.project_id}/us-central1-docker.pkg.dev/${var.project_id}/erlmcp-marketplace/erlmcp-marketplace:${var.image_name} -o cyclonedx-json=erlmcp-sbom.json",
      "sudo mv erlmcp-sbom.json /etc/",

      "trivy image --severity CRITICAL,HIGH --format json --output trivy-report.json ${var.project_id}/us-central1-docker.pkg.dev/${var.project_id}/erlmcp-marketplace/erlmcp-marketplace:${var.image_name}",
      "sudo mv trivy-report.json /etc/",
    ]

    inline_shebang = "/bin/bash -e"
  }

  post-processor {
    type          = "googlecompute"
    name          = "rename-image"
    force_delete  = true
    description   = "Rename the image to include version timestamp"

    template {
      destination = "projects/${var.project_id}/global/images/{{user `image_name`}}-{{timestamp}}"
    }
  }
}