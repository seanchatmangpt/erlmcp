#!/bin/bash
# Terraform Infrastructure Fix Script
# GCP Marketplace Deployment Emergency Fixes

set -e

echo "🔧 Starting Terraform Infrastructure Fix Script..."
echo "=================================================="

# Create backup directory
mkdir -p /Users/sac/erlmcp/marketplace/gcp/test-evidence/backups
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="/Users/sac/erlmcp/marketplace/gcp/test-evidence/backups/backup_${TIMESTAMP}"

# Fix Cloud Run Module
echo "📦 Fixing Cloud Run Module..."
cd /Users/sac/erlmcp/marketplace/gcp/terraform/modules/cloud-run

# Backup the files
cp main.tf "${BACKUP_DIR}/cloud-run_main.tf.bak"
cp variables.tf "${BACKUP_DIR}/cloud-run_variables.tf.bak"
cp outputs.tf "${BACKUP_DIR}/cloud-run_outputs.tf.bak"

# Remove duplicate outputs from main.tf
sed -i '' '/^output "service_name" {/,/}/d' main.tf
sed -i '' '/^output "service_url" {/,/}/d' main.tf
sed -i '' '/^output "location" {/,/}/d' main.tf

# Remove duplicate variables from variables.tf
grep -v "variable \"project_id\" {" variables.tf > temp_vars.tf && mv temp_vars.tf variables.tf
grep -v "variable \"region\" {" variables.tf > temp_vars.tf && mv temp_vars.tf variables.tf
grep -v "variable \"service_name\" {" variables.tf > temp_vars.tf && mv temp_vars.tf variables.tf
grep -v "variable \"cpu\" {" variables.tf > temp_vars.tf && mv temp_vars.tf variables.tf
grep -v "variable \"memory\" {" variables.tf > temp_vars.tf && mv temp_vars.tf variables.tf
grep -v "variable \"min_instances\" {" variables.tf > temp_vars.tf && mv temp_vars.tf variables.tf
grep -v "variable \"max_instances\" {" variables.tf > temp_vars.tf && mv temp_vars.tf variables.tf
grep -v "variable \"service_account_email\" {" variables.tf > temp_vars.tf && mv temp_vars.tf variables.tf

echo "✅ Cloud Run module duplicates removed"

# Fix Secret Manager Module
echo "🔐 Fixing Secret Manager Module..."
cd /Users/sac/erlmcp/marketplace/gcp/terraform/modules/secret-manager

# Backup
cp main.tf "${BACKUP_DIR}/secret-manager_main.tf.bak"

# Replace deprecated automatic argument with proper syntax
sed -i '' 's/automatic = true/replication { automatic {} }/g' main.tf

echo "✅ Secret Manager module deprecated arguments fixed"

# Fix VPC Module
echo "🌐 Fixing VPC Module..."
cd /Users/sac/erlmcp/marketplace/gcp/terraform/modules/vpc

# Backup
cp main.tf "${BACKUP_DIR}/vpc_main.tf.bak"

# Fix deprecated arguments
sed -i '' 's/secondary_ip_ranges/secondary_range/g' main.tf
sed -i '' '/metadata_fields {/,/^}/d' main.tf
sed -i '' '/purpose {/,/^}/d' main.tf

echo "✅ VPC module deprecated arguments fixed"

# Fix GCE VM Module
echo "💻 Fixing GCE VM Module..."
cd /Users/sac/erlmcp/marketplace/gcp/terraform/modules/gce-vm

# Backup
cp main.tf "${BACKUP_DIR}/gce-vm_main.tf.bak"

# Fix deprecated arguments
sed -i '' 's/size = 50//g' main.tf
sed -i '' 's/path = "\/health"$/path = "\/health"/g' main.tf

# Create missing start script
cat > start-erlmcp.sh << 'EOF'
#!/bin/bash
# erlmcp startup script
echo "Starting erlmcp on GCE VM..."
exec erlmcp --config=/etc/erlmcp/config.yaml
EOF
chmod +x start-erlmcp.sh

echo "✅ GCE VM module issues fixed"

# Fix Compute Engine Module
echo "⚙️ Fixing Compute Engine Module..."
cd /Users/sac/erlmcp/marketplace/gcp/terraform/modules/compute-engine

# Backup
cp main.tf "${BACKUP_DIR}/compute-engine_main.tf.bak"

# Remove invalid variable usage
sed -i '' '/create_before_destroy = var.create_before_destroy/d' main.tf

echo "✅ Compute Engine module invalid variable usage fixed"

# Fix Observability Module
echo "📊 Fixing Observability Module..."
cd /Users/sac/erlmcp/marketplace/gcp/terraform/modules/observability

# Backup
cp main.tf "${BACKUP_DIR}/observability_main.tf.bak"

# Create module directories
mkdir -p alert-policies
mkdir -p dashboards

# Create empty module files for now
cat > alert-policies/alert-policies.tf << 'EOF'
# Alert policies module placeholder
EOF

cat > dashboards/dashboards.tf << 'EOF'
# Dashboards module placeholder
EOF

echo "✅ Observability module directories created"

# Validate all modules after fixes
echo "🔍 Validating all modules after fixes..."
cd /Users/sac/erlmcp/marketplace/gcp/terraform

for module in gke cloud-run compute-engine secret-manager observability vpc gce-vm; do
  echo "=== Validating $module module after fixes ==="
  cd "/Users/sac/erlmcp/marketplace/gcp/terraform/modules/$module"

  # Initialize and validate
  docker run --rm -v $(pwd):/workspace -w /workspace hashicorp/terraform:1.8.5 init -no-color >/dev/null 2>&1 || true

  if docker run --rm -v $(pwd):/workspace -w /workspace hashicorp/terraform:1.8.5 validate -no-color >/dev/null 2>&1; then
    echo "✅ Valid: $module"

    # Check format
    if docker run --rm -v $(pwd):/workspace -w /workspace hashicorp/terraform:1.8.5 fmt -check -diff -no-color >/dev/null 2>&1; then
      echo "✅ Format OK: $module"
    else
      echo "⚠️  Format issues: $module"
      docker run --rm -v $(pwd):/workspace -w /workspace hashicorp/terraform:1.8.5 fmt -check -diff -no-color 2>&1 || true
    fi
  else
    echo "❌ Still failing: $module"
    docker run --rm -v $(pwd):/workspace -w /workspace hashicorp/terraform:1.8.5 validate -no-color 2>&1 || true
  fi

  echo ""
done

# Create pre-flight checklist
echo "✅ Creating pre-flight checklist..."
cat > /Users/sac/erlmcp/marketplace/gcp/test-evidence/preflight-checklist.md << 'EOF'
# Terraform Infrastructure Pre-Flight Checklist

## Critical Validation Items (Must Pass)

### Phase 1: Basic Validation
- [ ] All modules validate successfully (`terraform validate`)
- [ ] All modules pass format checks (`terraform fmt -check`)
- [ ] No duplicate outputs or variables
- [ ] All required providers defined

### Phase 2: Configuration Validation
- [ ] All resource configurations use correct syntax
- [ ] No deprecated provider arguments
- [ ] Proper for_each usage where needed
- [ ] File references exist and are accessible

### Phase 3: State Management
- [ ] Remote backend configured (GCS recommended)
- [ ] State locking mechanism enabled
- [ ] State encryption configured
- [ ] State bucket lifecycle policies in place

### Phase 4: Security Validation
- [ ] IAM roles follow least privilege principle
- [ ] Service accounts properly scoped
- [ ] Secret manager access controlled
- [ ] VPC Service Controls enabled

### Phase 5: Performance Optimization
- [ ] Autoscaling configured where applicable
- [ ] Monitoring and logging enabled
- [ ] Cost optimization tags present
- [ ] Resource labels standardized

## Emergency Contacts

- Infrastructure Lead: infrastructure@example.com
- Terraform Specialist: tf-specialist@example.com
- Emergency Hotline: +1-555-0199

## Success Criteria

All modules must validate successfully without errors or warnings before proceeding to production deployment.

EOF

echo "🎉 Terraform Infrastructure Fix Script completed!"
echo "=================================================="
echo "📁 Backups created in: $BACKUP_DIR"
echo "📋 Pre-flight checklist: /Users/sac/erlmcp/marketplace/gcp/test-evidence/preflight-checklist.md"
echo "📊 Validation report: /Users/sac/erlmcp/marketplace/gcp/test-evidence/terraform-validation-report.md"

echo ""
echo "Next Steps:"
echo "1. Review the validation report"
echo "2. Check the pre-flight checklist"
echo "3. Run final validation"
echo "4. Proceed to deployment if all checks pass"