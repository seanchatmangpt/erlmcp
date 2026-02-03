#!/bin/bash
# erlmcp v3 Firewall Configuration Script
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
INSTANCE_ID="i-1234567890"
SECURITY_GROUP_ID="sg-1234567890"
WAF_WEB_ACL_ID="12345678-1234-1234-1234-123456789012"

log() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Validate configuration
validate_config() {
    log "Validating firewall configuration..."

    # Check required variables
    if [ -z "$VPC_ID" ] || [ -z "$REGION" ] || [ -z "$PROFILE" ]; then
        error "Required configuration variables not set"
        exit 1
    fi

    # Check if VPC exists
    if ! aws ec2 describe-vpcs --vpc-ids $VPC_ID --query 'Vpcs[0].VpcId' --output text > /dev/null; then
        error "VPC $VPC_ID not found"
        exit 1
    fi

    log "Configuration validated"
}

# Create security group rules
create_security_groups() {
    log "Creating security groups..."

    # Create application security group
    SG_APP=$(aws ec2 create-security-group \
        --group-name erlmcp-app-sg \
        --description "erlmcp application security group" \
        --vpc-id $VPC_ID \
        --query 'GroupId' \
        --output text)

    if [ -z "$SG_APP" ]; then
        error "Failed to create application security group"
        exit 1
    fi

    # Create database security group
    SG_DB=$(aws ec2 create-security-group \
        --group-name erlmcp-db-sg \
        --description "erlmcp database security group" \
        --vpc-id $VPC_ID \
        --query 'GroupId' \
        --output text)

    if [ -z "$SG_DB" ]; then
        error "Failed to create database security group"
        exit 1
    fi

    # Create management security group
    SG_MGMT=$(aws ec2 create-security-group \
        --group-name erlmcp-mgmt-sg \
        --description "erlmcp management security group" \
        --vpc-id $VPC_ID \
        --query 'GroupId' \
        --output text)

    if [ -z "$SG_MGMT" ]; then
        error "Failed to create management security group"
        exit 1
    fi

    log "Security groups created: $SG_APP, $SG_DB, $SG_MGMT"
}

# Configure application security group
configure_app_sg() {
    log "Configuring application security group..."

    # Allow HTTPS
    aws ec2 authorize-security-group-ingress \
        --group-id $SECURITY_GROUP_ID \
        --protocol tcp \
        --port 443 \
        --cidr 0.0.0.0/0

    # Allow HTTP
    aws ec2 authorize-security-group-ingress \
        --group-id $SECURITY_GROUP_ID \
        --protocol tcp \
        --port 80 \
        --cidr 0.0.0.0/0

    # Allow health checks
    aws ec2 authorize-security-group-ingress \
        --group-id $SECURITY_GROUP_ID \
        --protocol tcp \
        --port 8080 \
        --cidr 10.10.0.0/22

    # Allow SSH from management
    aws ec2 authorize-security-group-ingress \
        --group-id $SECURITY_GROUP_ID \
        --protocol tcp \
        --port 22 \
        --cidr 10.40.0.0/24

    # Deny all other inbound traffic
    aws ec2 revoke-security-group-ingress \
        --group-id $SECURITY_GROUP_ID \
        --protocol all \
        --cidr 0.0.0.0/0

    # Allow all outbound traffic
    aws ec2 authorize-security-group-egress \
        --group-id $SECURITY_GROUP_ID \
        --protocol all \
        --cidr 0.0.0.0/0

    log "Application security group configured"
}

# Configure database security group
configure_db_sg() {
    log "Configuring database security group..."

    # Allow PostgreSQL from application
    aws ec2 authorize-security-group-ingress \
        --group-id $SECURITY_GROUP_ID \
        --protocol tcp \
        --port 5432 \
        --source-group $SECURITY_GROUP_ID

    # Allow MySQL from application
    aws ec2 authorize-security-group-ingress \
        --group-id $SECURITY_GROUP_ID \
        --protocol tcp \
        --port 3306 \
        --source-group $SECURITY_GROUP_ID

    # Allow database admin
    aws ec2 authorize-security-group-ingress \
        --group-id $SECURITY_GROUP_ID \
        --protocol tcp \
        --port 5432 \
        --cidr 10.40.0.0/24

    # Deny all other inbound traffic
    aws ec2 revoke-security-group-ingress \
        --group-id $SECURITY_GROUP_ID \
        --protocol all \
        --cidr 0.0.0.0/0

    # Allow all outbound traffic
    aws ec2 authorize-security-group-egress \
        --group-id $SECURITY_GROUP_ID \
        --protocol all \
        --cidr 0.0.0.0/0

    log "Database security group configured"
}

# Configure management security group
configure_mgmt_sg() {
    log "Configuring management security group..."

    # Allow SSH from management
    aws ec2 authorize-security-group-ingress \
        --group-id $SECURITY_GROUP_ID \
        --protocol tcp \
        --port 22 \
        --cidr 10.40.0.0/24

    # Allow web management
    aws ec2 authorize-security-group-ingress \
        --group-id $SECURITY_GROUP_ID \
        --protocol tcp \
        --port 8443 \
        --cidr 10.40.0.0/24

    # Allow monitoring
    aws ec2 authorize-security-group-ingress \
        --group-id $SECURITY_GROUP_ID \
        --protocol tcp \
        --port 9100 \
        --cidr 10.40.0.0/24

    # Deny all other inbound traffic
    aws ec2 revoke-security-group-ingress \
        --group-id $SECURITY_GROUP_ID \
        --protocol all \
        --cidr 0.0.0.0/0

    # Allow all outbound traffic
    aws ec2 authorize-security-group-egress \
        --group-id $SECURITY_GROUP_ID \
        --protocol all \
        --cidr 0.0.0.0/0

    log "Management security group configured"
}

# Configure network ACLs
configure_nacl() {
    log "Configuring network ACLs..."

    # Get default NACL
    NACL_ID=$(aws ec2 describe-network-acls \
        --filters "Name=vpc-id,Values=$VPC_ID" "Name=default,Values=true" \
        --query 'NetworkAcls[0].NetworkAclId' \
        --output text)

    if [ -z "$NACL_ID" ]; then
        error "Failed to get default NACL"
        exit 1
    fi

    # Clear existing rules
    aws ec2 delete-network-acl-entry \
        --network-acl-id $NACL_ID \
        --rule-number 100

    # Allow inbound HTTPS
    aws ec2 create-network-acl-entry \
        --network-acl-id $NACL_ID \
        --rule-number 100 \
        --protocol tcp \
        --port-range From=443,To=443 \
        --rule-action allow \
        --egress

    # Allow outbound DNS
    aws ec2 create-network-acl-entry \
        --network-acl-id $NACL_ID \
        --rule-number 200 \
        --protocol udp \
        --port-range From=53,To=53 \
        --rule-action allow \
        --egress

    # Allow outbound NTP
    aws ec2 create-network-acl-entry \
        --network-acl-id $NACL_ID \
        --rule-number 300 \
        --protocol udp \
        --port-range From=123,To=123 \
        --rule-action allow \
        --egress

    # Deny all other traffic
    aws ec2 create-network-acl-entry \
        --network-acl-id $NACL_ID \
        --rule-number 400 \
        --protocol all \
        --rule-action deny \
        --egress

    log "Network ACLs configured"
}

# Configure WAF
configure_waf() {
    log "Configuring WAF..."

    # Create WAF web ACL
    WEB_ACL_ID=$(aws wafv2 create-web-acl \
        --name erlmcp-waf \
        --scope regional \
        --region $REGION \
        --default-action Allow={} \
        --rules \
        "Type='REGULAR',Name='SQLI',Priority=1,Action={Type='BLOCK'},Statement={ByteMatchStatement={TextTransformation='NONE',FieldToMatch={Data='query_string',Type='QUERY_STRING'},PositionalConstraint='CONTAINS',MatchedData='union|insert|delete|update|select'},RegexBasedMatchStatement={RegexString='(?i)(union.*select|insert.*into|delete.*from|'.*or.*'|'.*and.*|'.*exec|'.*execute)',RegexTransformation='NONE'}}" \
        --visibility-config \
        "SampledRequestsEnabled=false,CloudWatchMetricsEnabled=true,MetricName=erlmcp-waf" \
        --query 'Summary.WebACLId' \
        --output text)

    if [ -z "$WEB_ACL_ID" ]; then
        error "Failed to create WAF web ACL"
        exit 1
    fi

    # Associate WAF with load balancer
    aws wafv2 associate-web-acl \
        --web-acl-arn arn:aws:wafv2:$REGION:123456789012:regional/webacl/erlmcp-waf/$WEB_ACL_ID \
        --resource-arn arn:aws:elasticloadbalancing:$REGION:123456789012:loadbalancer/app/erlmcp-lb/1234567890123456

    log "WAF configured and associated"
}

# Configure firewall rules on instance
configure_instance_firewall() {
    log "Configuring firewall rules on instance..."

    # Install iptables-persistent
    apt-get update
    apt-get install -y iptables-persistent

    # Create iptables rules
    iptables -F
    iptables -X

    # Default policies
    iptables -P INPUT DROP
    iptables -P FORWARD DROP
    iptables -P OUTPUT ACCEPT

    # Allow established connections
    iptables -A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT

    # Allow loopback
    iptables -A INPUT -i lo -j ACCEPT

    # Allow SSH from management
    iptables -A INPUT -p tcp --dport 22 -s 10.40.0.0/24 -j ACCEPT

    # Allow erlmcp application
    iptables -A INPUT -p tcp --dport 8080 -s 10.20.0.0/16 -j ACCEPT
    iptables -A INPUT -p tcp --dport 8443 -s 10.20.0.0/16 -j ACCEPT

    # Allow database
    iptables -A INPUT -p tcp --dport 5432 -s 10.30.0.0/22 -j ACCEPT
    iptables -A INPUT -p tcp --dport 3306 -s 10.30.0.0/22 -j ACCEPT

    # Allow monitoring
    iptables -A INPUT -p tcp --dport 9100 -s 10.40.0.0/24 -j ACCEPT

    # Log dropped packets
    iptables -A INPUT -m limit --limit 5/min -j LOG --log-prefix "iptables-dropped: " --log-level 4

    # Save rules
    iptables-save > /etc/iptables/rules.v4

    log "Instance firewall configured"
}

# Configure IPtables logging
configure_logging() {
    log "Configuring firewall logging..."

    # Create logging directory
    mkdir -p /var/log/firewall

    # Create logrotate configuration
    cat > /etc/logrotate.d/firewall << EOF
/var/log/firewall/*
    daily
    rotate 30
    compress
    delaycompress
    missingok
    notifempty
    create 0640 root adm
EOF

    # Create log processing script
    cat > /usr/local/bin/process-iptables-logs.sh << 'EOF'
#!/bin/bash

# Process firewall logs
LOG_FILE="/var/log/firewall/iptables.log"
SUMMARY_FILE="/var/log/firewall/summary.log"

# Create summary
echo "=== Firewall Log Summary ===" > $SUMMARY_FILE
echo "Timestamp: $(date)" >> $SUMMARY_FILE
echo "" >> $SUMMARY_FILE

# Count blocked connections
echo "Blocked Connections:" >> $SUMMARY_FILE
grep "iptables-dropped" /var/log/syslog | tail -1000 | awk '{print $6}' | sort | uniq -c | sort -nr >> $SUMMARY_FILE

# Top source IPs
echo "" >> $SUMMARY_FILE
echo "Top Source IPs:" >> $SUMMARY_FILE
grep "iptables-dropped" /var/log/syslog | tail -1000 | awk '{print $11}' | cut -d'=' -f2 | sort | uniq -c | sort -nr | head -10 >> $SUMMARY_FILE

# Alert on excessive blocking
BLOCKED_COUNT=$(grep "iptables-dropped" /var/log/syslog | tail -60 | wc -l)
if [ $BLOCKED_COUNT -gt 50 ]; then
    echo "High blocking rate detected: $BLOCKED_COUNT in last minute" | mail -s "Firewall Alert" security@example.com
fi

# Rotate logs
mv /var/log/syslog /var/log/firewall/syslog.$(date +%Y%m%d-%H%M%S)
kill -USR1 $(cat /var/run/syslogd.pid)
EOF

    chmod +x /usr/local/bin/process-iptables-logs.sh

    # Add cron job
    (crontab -l 2>/dev/null; echo "*/5 * * * * /usr/local/bin/process-iptables-logs.sh") | crontab -

    log "Firewall logging configured"
}

# Main execution
main() {
    log "Starting erlmcp v3 firewall configuration..."

    validate_config
    create_security_groups
    configure_app_sg
    configure_db_sg
    configure_mgmt_sg
    configure_nacl
    configure_waf
    configure_instance_firewall
    configure_logging

    log "Firewall configuration completed!"
}

# Run main function
main "$@"