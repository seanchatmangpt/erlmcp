#!/bin/bash
# erlmcp v3 VPN Configuration Script
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
CUSTOMER_GATEWAY_ID="cgw-1234567890"
TRANSIT_GATEWAY_ID="tgw-1234567890"
VPN_CONNECTION_ID="vpn-1234567890"
TUNNEL1_OUTSIDE_IP="203.0.113.1"
TUNNEL2_OUTSIDE_IP="203.0.113.2"

log() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Validate VPN configuration
validate_config() {
    log "Validating VPN configuration..."

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

# Create VPN connection
create_vpn_connection() {
    log "Creating VPN connection..."

    # Create VPN connection
    VPN_CONNECTION_ID=$(aws ec2 create-vpn-connection \
        --type "ipsec.1" \
        --customer-gateway-id "$CUSTOMER_GATEWAY_ID" \
        --transit-gateway-id "$TRANSIT_GATEWAY_ID" \
        --tag-specifications "ResourceType=vpn-connection,Tags=[{Key=Name,Value=erlmcp-vpn}]" \
        --query 'VpnConnection.VpnConnectionId' \
        --output text)

    if [ -z "$VPN_CONNECTION_ID" ]; then
        error "Failed to create VPN connection"
        exit 1
    fi

    log "VPN connection created with ID: $VPN_CONNECTION_ID"
}

# Configure VPN tunnels
configure_tunnels() {
    log "Configuring VPN tunnels..."

    # Tunnel 1 configuration
    cat > /tmp/tunnel1.conf << EOF
phase1=ikev2
phase1hash=sha256
phase1encryption=aes-256-gcm-128
ikeversion=2
keylife=28800
rekeymargin=540
rekeyfuzz=100
ikelifetime=36000
keyexchange=dhgroup14
authmethod=secret
lifetime_seconds=28800
dpd_delay=10
dpd_timeout=30
mobike=no
left=10.0.0.1
leftsubnet=10.0.0.0/8
leftid=vpn-erlmcp-primary
right=$TUNNEL1_OUTSIDE_IP
rightsubnet=10.60.0.0/16
rightid=vpn-customer-primary
rightsendcert=never
auto=start
EOF

    # Tunnel 2 configuration
    cat > /tmp/tunnel2.conf << EOF
phase1=ikev2
phase1hash=sha256
phase1encryption=aes-256-gcm-128
ikeversion=2
keylife=28800
rekeymargin=540
rekeyfuzz=100
ikelifetime=36000
keyexchange=dhgroup14
authmethod=secret
lifetime_seconds=28800
dpd_delay=10
dpd_timeout=30
mobike=no
left=10.0.0.2
leftsubnet=10.0.0.0/8
leftid=vpn-erlmcp-secondary
right=$TUNNEL2_OUTSIDE_IP
rightsubnet=10.60.0.0/16
rightid=vpn-customer-secondary
rightsendcert=never
auto=start
EOF

    log "Tunnel configurations created"
}

# Configure IPsec
configure_ipsec() {
    log "Configuring IPsec..."

    # Create IPsec configuration
    cat > /etc/ipsec.conf << EOF
config erlmcp-vpn
    # Tunnel 1
    ikev2 erlmcp-tunnel1
    # Tunnel 2
    ikev2 erlmcp-tunnel2
EOF

    # Copy tunnel configurations
    cp /tmp/tunnel1.conf /etc/ipsec.d/erlmcp-tunnel1.conf
    cp /tmp/tunnel2.conf /etc/ipsec.d/erlmcp-tunnel2.conf

    # Create IPsec secrets
    cat > /etc/ipsec.secrets << EOF
%any : PSK "your-pre-shared-key"
EOF

    log "IPsec configuration completed"
}

# Configure routing
configure_routing() {
    log "Configuring routing..."

    # Get transit gateway attachment ID
    ATTACHMENT_ID=$(aws ec2 describe-transit-gateway-vpc-attachments \
        --transit-gateway-ids $TRANSIT_GATEWAY_ID \
        --filters "Name=vpc-id,Values=$VPC_ID" \
        --query 'TransitGatewayVpcAttachments[0].TransitGatewayVpcAttachmentId' \
        --output text)

    if [ -z "$ATTACHMENT_ID" ]; then
        error "Failed to get transit gateway attachment"
        exit 1
    fi

    # Update route table for customer traffic
    ROUTE_TABLE_ID=$(aws ec2 describe-route-tables \
        --filters "Name=vpc-id,Values=$VPC_ID" "Name=association.main,Values=true" \
        --query 'RouteTables[0].RouteTableId' \
        --output text)

    if [ -z "$ROUTE_TABLE_ID" ]; then
        error "Failed to get route table ID"
        exit 1
    fi

    # Add route for customer network
    aws ec2 create-route \
        --route-table-id $ROUTE_TABLE_ID \
        --destination-cidr-block 10.60.0.0/16 \
        --transit-gateway-id $TRANSIT_GATEWAY_ID

    log "Routing configuration completed"
}

# Start IPsec services
start_ipsec() {
    log "Starting IPsec services..."

    # Start StrongSwan
    systemctl start strongswan
    systemctl enable strongswan

    # Start IPsec
    ipsec start

    # Verify connections
    sleep 30
    ipsec status

    log "IPsec services started"
}

# Monitor VPN status
monitor_vpn() {
    log "Monitoring VPN status..."

    # Check tunnel status
    while true; do
        echo "=== VPN Status ==="
        date
        ipsec status | grep "STATE:"
        echo ""

        # Check BGP routes
        if command -v vtysh &> /dev/null; then
            echo "=== BGP Routes ==="
            vtysh -c "show bgp summary"
            vtysh -c "show bgp neighbors"
            echo ""
        fi

        # Check IPsec status
        echo "=== IPsec Status ==="
        ipsec statusall | grep "established"
        echo ""

        sleep 60
    done
}

# Main execution
main() {
    log "Starting erlmcp v3 VPN configuration..."

    validate_config
    create_vpn_connection
    configure_tunnels
    configure_ipsec
    configure_routing
    start_ipsec
    monitor_vpn
}

# Run main function
main "$@"