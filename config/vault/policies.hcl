# Vault Policies for erlmcp v3
# Define fine-grained access control policies for different components

# ============================================================================
# ERLMCP ADMIN POLICY - Full access to erlmcp secrets
# ============================================================================
path "erlmcp/data/*" {
  capabilities = ["create", "read", "update", "delete", "list"]
}

path "erlmcp/metadata/*" {
  capabilities = ["list", "read", "delete"]
}

path "erlmcp/config/*" {
  capabilities = ["create", "read", "update", "delete", "list"]
}

path "erlmcp/tls/*" {
  capabilities = ["create", "read", "update", "delete", "list"]
}

path "erlmcp/auth/*" {
  capabilities = ["create", "read", "update", "delete", "list"]
}

path "erlmcp/database/*" {
  capabilities = ["create", "read", "update", "delete", "list"]
}

# Database secrets engine
path "database/creds/*" {
  capabilities = ["create", "read", "update", "delete"]
}

# PKI certificates
path "pki/issue/*" {
  capabilities = ["create", "update"]
}

path "pki/sign/*" {
  capabilities = ["create", "update"]
}

# Transit encryption
path "transit/encrypt/*" {
  capabilities = ["create", "update"]
}

path "transit/decrypt/*" {
  capabilities = ["create", "update"]
}

path "transit/keys/*" {
  capabilities = ["create", "read", "update", "delete"]
}

# ============================================================================
# ERLMCP DATABASE POLICY - Database credentials only
# ============================================================================
path "erlmcp/database/*" {
  capabilities = ["read"]
}

path "database/creds/erlmcp-app" {
  capabilities = ["read"]
}

path "database/creds/erlmcp-replica" {
  capabilities = ["read"]
}

# ============================================================================
# ERLMCP TLS POLICY - Certificate management
# ============================================================================
path "erlmcp/tls/*" {
  capabilities = ["read"]
}

path "pki/cert/ca" {
  capabilities = ["read"]
}

path "pki/cert/public" {
  capabilities = ["read"]
}

path "pki/issue/erlmcp-server" {
  capabilities = ["create"]
}

path "pki/issue/erlmcp-client" {
  capabilities = ["create"]
}

# ============================================================================
# ERLMCP CONFIG POLICY - Application configuration
# ============================================================================
path "erlmcp/config/*" {
  capabilities = ["read", "list"]
}

path "erlmcp/metadata/*" {
  capabilities = ["read", "list"]
}

# ============================================================================
# ERLMCP AUTH POLICY - Authentication secrets
# ============================================================================
path "erlmcp/auth/*" {
  capabilities = ["read"]
}

# ============================================================================
# ERLMCP MONITORING POLICY - Metrics and observability
# ============================================================================
path "erlmcp/monitoring/*" {
  capabilities = ["read", "list"]
}

# ============================================================================
# ERLMCP CI/CD POLICY - For deployment automation
# ============================================================================
path "erlmcp/cicd/*" {
  capabilities = ["create", "read", "update", "delete"]
}

# Allowed to create temporary tokens
path "auth/token/create" {
  capabilities = ["create", "update"]
}

# ============================================================================
# ERLMCP AUDIT POLICY - For security audit logging
# ============================================================================
path "sys/audit" {
  capabilities = ["read", "list"]
}

path "sys/audit-hash/*" {
  capabilities = ["read"]
}

# ============================================================================
# ERLMCP REPLICATION POLICY - For multi-cluster replication
# ============================================================================
path "sys/replication/status" {
  capabilities = ["read", "list"]
}

path "sys/replication/primary/secondary-token" {
  capabilities = ["create", "update"]
}

# ============================================================================
# ERLMCP OPERATOR POLICY - Limited operational access
# ============================================================================
# Can read configuration
path "erlmcp/config/*" {
  capabilities = ["read", "list"]
}

# Can list but not read secrets
path "erlmcp/data/*" {
  capabilities = ["list"]
}

# Can restart services
path "sys/leases/*" {
  capabilities = ["list", "read"]
}

# Can check health
path "sys/health" {
  capabilities = ["read", "update"]
}

# ============================================================================
# SECRET ROTATION POLICY - For automated secret rotation
# ============================================================================
path "erlmcp/data/*" {
  capabilities = ["create", "read", "update", "list"]
}

path "erlmcp/metadata/*" {
  capabilities = ["read", "update", "list"]
}

# Can rotate database credentials
path "database/rotate/*" {
  capabilities = ["create", "update"]
}

# Can rotate PKI certificates
path "pki/issue/*" {
  capabilities = ["create", "update"]
}

# ============================================================================
# ERLMCP BACKUP POLICY - For backup and restore operations
# ============================================================================
path "erlmcp/*" {
  capabilities = ["read", "list"]
}

path "sys/storage/raft/snapshot" {
  capabilities = ["create"]
}

# ============================================================================
# ERLMCP READONLY POLICY - For monitoring and debugging
# ============================================================================
path "erlmcp/data/*" {
  capabilities = ["read", "list"]
}

path "erlmcp/metadata/*" {
  capabilities = ["read", "list"]
}

path "sys/leases/*" {
  capabilities = ["list", "read"]
}

# ============================================================================
# ERLMCP DENY POLICY - Block all access (for compromised tokens)
# ============================================================================
path "*" {
  capabilities = ["deny"]
}
