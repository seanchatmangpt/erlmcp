# ============================================================================
# Secret Manager Simulation - Vault Policy for ErlMCP
# ============================================================================
# Simulates GCP Secret Manager and Cloud KMS access policies

# ErlMCP secrets path - read only
path "erlmcp/*" {
  capabilities = ["read", "list"]
}

# Allow creating and managing secrets (for CI/CD)
path "erlmcp/data/*" {
  capabilities = ["create", "update", "read", "delete", "list"]
}

# Allow reading secret metadata
path "erlmcp/metadata/*" {
  capabilities = ["read", "list"]
}

# Transit engine for encryption (simulates Cloud KMS)
path "transit/encrypt/erlmcp-app-key" {
  capabilities = ["update"]
}

path "transit/decrypt/erlmcp-app-key" {
  capabilities = ["update"]
}

path "transit/keys/erlmcp-app-key" {
  capabilities = ["read"]
}

# Allow key rotation (simulates KMS key rotation)
path "transit/keys/erlmcp-app-key/rotate" {
  capabilities = ["update"]
}

# Auth token renewal
path "auth/token/renew-self" {
  capabilities = ["update"]
}

path "auth/token/lookup-self" {
  capabilities = ["read"]
}

# System health check
path "sys/health" {
  capabilities = ["read", "sudo"]
}
