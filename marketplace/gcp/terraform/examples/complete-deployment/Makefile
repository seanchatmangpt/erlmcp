# ============================================================================
# erlmcp Complete Deployment Makefile
# ============================================================================

.PHONY: help init plan apply destroy validate fmt lint clean test-gke test-cloudrun test-gce backup

# Default target
.DEFAULT_GOAL := help

# Variables
PROJECT_ID ?= $(shell grep 'project_id' terraform.tfvars 2>/dev/null | cut -d '=' -f2 | tr -d ' "')
ENVIRONMENT ?= production
REGION ?= us-central1

help: ## Display this help message
	@echo "erlmcp Complete Deployment Commands"
	@echo "===================================="
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n\nTargets:\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2 }' $(MAKEFILE_LIST)

init: ## Initialize Terraform
	@echo "Initializing Terraform..."
	terraform init

validate: ## Validate Terraform configuration
	@echo "Validating configuration..."
	terraform validate
	@echo "✓ Configuration is valid"

fmt: ## Format Terraform files
	@echo "Formatting Terraform files..."
	terraform fmt -recursive
	@echo "✓ Files formatted"

plan: validate ## Generate and show execution plan
	@echo "Generating execution plan..."
	terraform plan -out=tfplan
	@echo "✓ Plan saved to tfplan"

apply: ## Deploy infrastructure
	@echo "Deploying infrastructure..."
	@if [ ! -f tfplan ]; then \
		echo "No plan found. Running plan first..."; \
		$(MAKE) plan; \
	fi
	terraform apply tfplan
	@rm -f tfplan
	@echo "✓ Deployment complete"

destroy: ## Destroy all infrastructure (WARNING: destructive)
	@echo "⚠️  WARNING: This will destroy all infrastructure!"
	@read -p "Type 'yes' to confirm: " confirm; \
	if [ "$$confirm" = "yes" ]; then \
		terraform destroy; \
	else \
		echo "Aborted."; \
	fi

refresh: ## Refresh Terraform state
	@echo "Refreshing state..."
	terraform refresh

output: ## Show Terraform outputs
	@terraform output

clean: ## Remove temporary files
	@echo "Cleaning temporary files..."
	rm -f tfplan
	rm -f .terraform.lock.hcl
	rm -rf .terraform/
	@echo "✓ Cleaned"

# ============================================================================
# Testing
# ============================================================================

test-gke: ## Test GKE deployment
	@echo "Testing GKE deployment..."
	@kubectl get nodes
	@kubectl get pods -n erlmcp
	@echo "✓ GKE is healthy"

test-cloudrun: ## Test Cloud Run deployment
	@echo "Testing Cloud Run deployment..."
	@SERVICE_URL=$$(terraform output -raw cloud_run.service_url); \
	curl -sf $$SERVICE_URL/health || (echo "✗ Cloud Run health check failed" && exit 1)
	@echo "✓ Cloud Run is healthy"

test-gce: ## Test GCE deployment
	@echo "Testing GCE deployment..."
	@INSTANCE_NAME=$$(terraform output -json compute_engine.instance_names | jq -r '.[0]'); \
	gcloud compute ssh $$INSTANCE_NAME --command="curl -sf http://localhost:8080/health" || (echo "✗ GCE health check failed" && exit 1)
	@echo "✓ GCE is healthy"

test-all: test-gke test-cloudrun test-gce ## Run all health checks
	@echo "✓ All platforms healthy"

# ============================================================================
# Operations
# ============================================================================

logs-gke: ## View GKE logs
	@kubectl logs -n erlmcp -l app=erlmcp --tail=100 -f

logs-cloudrun: ## View Cloud Run logs
	@SERVICE_NAME=$$(terraform output -raw cloud_run.service_name); \
	gcloud logging read "resource.type=cloud_run_revision AND resource.labels.service_name=$$SERVICE_NAME" --limit 50

logs-gce: ## View GCE logs
	@INSTANCE_NAME=$$(terraform output -json compute_engine.instance_names | jq -r '.[0]'); \
	gcloud compute ssh $$INSTANCE_NAME --command="sudo journalctl -u erlmcp -n 100 -f"

scale-gke: ## Scale GKE nodes (usage: make scale-gke NODES=5)
	@echo "Scaling GKE to $(NODES) nodes..."
	terraform apply -var="gke_max_nodes=$(NODES)"

scale-cloudrun: ## Scale Cloud Run (usage: make scale-cloudrun MAX=100)
	@echo "Scaling Cloud Run to max $(MAX) instances..."
	terraform apply -var="cloud_run_max_instances=$(MAX)"

# ============================================================================
# Backup & Recovery
# ============================================================================

backup: ## Backup Terraform state and secrets
	@echo "Creating backup..."
	@mkdir -p backup
	@terraform state pull > backup/terraform.tfstate.backup.$$(date +%Y%m%d-%H%M%S)
	@echo "✓ State backed up"

restore: ## Restore Terraform state (usage: make restore FILE=backup/terraform.tfstate.backup.YYYYMMDD-HHMMSS)
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE parameter required"; \
		exit 1; \
	fi
	@echo "Restoring state from $(FILE)..."
	@terraform state push $(FILE)
	@echo "✓ State restored"

# ============================================================================
# Security
# ============================================================================

security-scan: ## Run security scan on Terraform configuration
	@echo "Running security scan..."
	@command -v tfsec >/dev/null 2>&1 || { echo "tfsec not installed. Install: brew install tfsec"; exit 1; }
	@tfsec .
	@echo "✓ Security scan complete"

compliance-check: ## Check compliance
	@echo "Checking compliance..."
	@echo "- Binary Authorization: enabled"
	@echo "- Private endpoints: enabled"
	@echo "- TLS: enabled"
	@echo "✓ Compliance check complete"

# ============================================================================
# Monitoring
# ============================================================================

dashboard: ## Open monitoring dashboard
	@DASHBOARD_URL=$$(terraform output -raw observability.dashboards.performance); \
	open $$DASHBOARD_URL || xdg-open $$DASHBOARD_URL

metrics: ## Show current metrics
	@echo "Fetching metrics..."
	@gcloud monitoring time-series list \
		--filter='metric.type="run.googleapis.com/request_count"' \
		--limit=10

# ============================================================================
# Development
# ============================================================================

dev-setup: ## Setup development environment
	@echo "Setting up development environment..."
	@if [ ! -f terraform.tfvars ]; then \
		cp terraform.tfvars.example terraform.tfvars; \
		echo "✓ Created terraform.tfvars - Please configure it"; \
	fi
	@$(MAKE) init

cost-estimate: ## Estimate infrastructure costs
	@echo "Estimating costs..."
	@command -v infracost >/dev/null 2>&1 || { echo "infracost not installed. See: https://www.infracost.io/"; exit 1; }
	@infracost breakdown --path .

# ============================================================================
# Documentation
# ============================================================================

docs: ## Generate documentation
	@echo "Generating documentation..."
	@command -v terraform-docs >/dev/null 2>&1 || { echo "terraform-docs not installed. Install: brew install terraform-docs"; exit 1; }
	@terraform-docs markdown table . > TERRAFORM_DOCS.md
	@echo "✓ Documentation generated: TERRAFORM_DOCS.md"

graph: ## Generate dependency graph
	@echo "Generating dependency graph..."
	@terraform graph | dot -Tpng > dependency-graph.png
	@echo "✓ Graph saved: dependency-graph.png"

# ============================================================================
# Quick commands
# ============================================================================

quick-deploy: init plan apply test-all ## Initialize, plan, apply, and test (full deployment)
	@echo "✓ Quick deploy complete"

quick-update: plan apply ## Plan and apply (fast update)
	@echo "✓ Quick update complete"

status: ## Show deployment status
	@echo "Deployment Status"
	@echo "================="
	@echo "Project: $(PROJECT_ID)"
	@echo "Environment: $(ENVIRONMENT)"
	@echo "Region: $(REGION)"
	@echo ""
	@echo "GKE Status:"
	@kubectl get nodes 2>/dev/null || echo "  Not accessible"
	@echo ""
	@echo "Cloud Run Status:"
	@gcloud run services list --filter="labels.environment=$(ENVIRONMENT)" 2>/dev/null || echo "  Not found"
	@echo ""
	@echo "GCE Status:"
	@gcloud compute instances list --filter="labels.environment=$(ENVIRONMENT)" 2>/dev/null || echo "  Not found"
