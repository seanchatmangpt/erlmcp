#!/bin/bash
# erlmcp v3 Network Infrastructure Deployment Script
set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
VPC_ID="vpc-1234567890"
REGION="us-east-1"
PROFILE="erlmcp-prod"
CLUSTER_NAME="erlmcp-cluster"

log() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check prerequisites
check_prerequisites() {
    log "Checking prerequisites..."

    # Check AWS CLI
    if ! command -v aws &> /dev/null; then
        error "AWS CLI not installed"
        exit 1
    fi

    # Check Terraform
    if ! command -v terraform &> /dev/null; then
        error "Terraform not installed"
        exit 1
    fi

    # Check kubectl
    if ! command -v kubectl &> /dev/null; then
        error "kubectl not installed"
        exit 1
    fi

    log "All prerequisites installed"
}

# Deploy network infrastructure
deploy_network() {
    log "Deploying network infrastructure..."

    # Initialize Terraform
    cd terraform/network
    terraform init

    # Plan infrastructure
    terraform plan -var="vpc_id=$VPC_ID" -var="region=$REGION" -out="network.tfplan"

    # Apply infrastructure
    terraform apply "network.tfplan"

    cd ../..
    log "Network infrastructure deployed"
}

# Deploy load balancers
deploy_load_balancers() {
    log "Deploying load balancers..."

    # Initialize Terraform
    cd terraform/load-balancer
    terraform init

    # Plan infrastructure
    terraform plan -var="vpc_id=$VPC_ID" -var="region=$REGION" -out="lb.tfplan"

    # Apply infrastructure
    terraform apply "lb.tfplan"

    cd ../..
    log "Load balancers deployed"
}

# Deploy security groups
deploy_security_groups() {
    log "Deploying security groups..."

    # Initialize Terraform
    cd terraform/security
    terraform init

    # Plan infrastructure
    terraform plan -var="vpc_id=$VPC_ID" -var="region=$REGION" -out="security.tfplan"

    # Apply infrastructure
    terraform apply "security.tfplan"

    cd ../..
    log "Security groups deployed"
}

# Deploy monitoring
deploy_monitoring() {
    log "Deploying monitoring infrastructure..."

    # Initialize Terraform
    cd terraform/monitoring
    terraform init

    # Plan infrastructure
    terraform plan -var="region=$REGION" -out="monitoring.tfplan"

    # Apply infrastructure
    terraform apply "monitoring.tfplan"

    cd ../..
    log "Monitoring infrastructure deployed"
}

# Deploy Kubernetes cluster
deploy_cluster() {
    log "Deploying Kubernetes cluster..."

    # Initialize Terraform
    cd terraform/cluster
    terraform init

    # Plan infrastructure
    terraform plan -var="cluster_name=$CLUSTER_NAME" -var="region=$REGION" -out="cluster.tfplan"

    # Apply infrastructure
    terraform apply "cluster.tfplan"

    cd ../..
    log "Kubernetes cluster deployed"
}

# Configure networking
configure_networking() {
    log "Configuring networking..."

    # Get load balancer DNS name
    LB_DNS=$(aws elbv2 describe-load-balancers --names erlmcp-lb --query 'LoadBalancers[0].DNSName' --output text)

    if [ -z "$LB_DNS" ]; then
        error "Failed to get load balancer DNS name"
        exit 1
    fi

    # Update DNS records
    aws route53 change-resource-record-sets \
        --hosted-zone-id Z1234567890 \
        --change-batch '{
            "Comment": "Update erlmcp DNS",
            "Changes": [
                {
                    "Action": "UPSERT",
                    "ResourceRecordSet": {
                        "Name": "erlmcp.example.com",
                        "Type": "A",
                        "TTL": 60,
                        "ResourceRecords": [
                            {
                                "Value": "'$LB_DNS'"
                            }
                        ]
                    }
                }
            ]
        }'

    log "DNS configuration updated"
}

# Apply Kubernetes manifests
apply_manifests() {
    log "Applying Kubernetes manifests..."

    # Get kubeconfig
    aws eks update-kubeconfig --name $CLUSTER_NAME --region $REGION --profile $PROFILE

    # Create namespace
    kubectl create namespace erlmcp --dry-run=client -o yaml | kubectl apply -f -

    # Apply manifests
    kubectl apply -f k8s/namespace.yaml
    kubectl apply -f k8s/network-policy.yaml
    kubectl apply -f k8s/configmap.yaml
    kubectl apply -f k8s/deployment.yaml
    kubectl apply -f k8s/service.yaml
    kubectl apply -f k8s/ingress.yaml

    log "Kubernetes manifests applied"
}

# Verify deployment
verify_deployment() {
    log "Verifying deployment..."

    # Check pod status
    kubectl get pods -n erlmcp

    # Check service status
    kubectl get svc -n erlmcp

    # Check ingress status
    kubectl get ingress -n erlmcp

    log "Deployment verified"
}

# Main execution
main() {
    log "Starting erlmcp v3 network infrastructure deployment..."

    check_prerequisites
    deploy_network
    deploy_load_balancers
    deploy_security_groups
    deploy_monitoring
    deploy_cluster
    configure_networking
    apply_manifests
    verify_deployment

    log "Deployment completed successfully!"
}

# Run main function
main "$@"