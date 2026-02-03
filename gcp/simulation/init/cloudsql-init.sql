-- Cloud SQL Simulation - Database Initialization
-- ============================================================================
-- This script initializes the PostgreSQL database to simulate Cloud SQL
-- with typical erlmcp application schema.
-- ============================================================================

-- Enable required extensions (common in Cloud SQL)
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pg_stat_statements";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-- Create application schema
CREATE SCHEMA IF NOT EXISTS erlmcp;
SET search_path TO erlmcp, public;

-- ============================================================================
-- MCP Sessions Table
-- Tracks active MCP client sessions
-- ============================================================================
CREATE TABLE IF NOT EXISTS mcp_sessions (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    client_id VARCHAR(255) NOT NULL,
    transport_type VARCHAR(50) NOT NULL,
    protocol_version VARCHAR(20) NOT NULL DEFAULT '2024-11-05',
    capabilities JSONB DEFAULT '{}',
    server_info JSONB DEFAULT '{}',
    connected_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_activity_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    disconnected_at TIMESTAMPTZ,
    status VARCHAR(20) NOT NULL DEFAULT 'active',
    metadata JSONB DEFAULT '{}',
    CONSTRAINT valid_status CHECK (status IN ('active', 'idle', 'disconnected', 'error'))
);

CREATE INDEX idx_mcp_sessions_client ON mcp_sessions(client_id);
CREATE INDEX idx_mcp_sessions_status ON mcp_sessions(status);
CREATE INDEX idx_mcp_sessions_connected ON mcp_sessions(connected_at);

-- ============================================================================
-- MCP Requests Table
-- Logs all JSON-RPC requests for auditing
-- ============================================================================
CREATE TABLE IF NOT EXISTS mcp_requests (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    session_id UUID REFERENCES mcp_sessions(id) ON DELETE CASCADE,
    request_id VARCHAR(255),
    method VARCHAR(255) NOT NULL,
    params JSONB,
    received_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    completed_at TIMESTAMPTZ,
    response JSONB,
    error JSONB,
    duration_ms INTEGER,
    metadata JSONB DEFAULT '{}'
);

CREATE INDEX idx_mcp_requests_session ON mcp_requests(session_id);
CREATE INDEX idx_mcp_requests_method ON mcp_requests(method);
CREATE INDEX idx_mcp_requests_received ON mcp_requests(received_at);

-- ============================================================================
-- MCP Resources Table
-- Registered resources available via MCP
-- ============================================================================
CREATE TABLE IF NOT EXISTS mcp_resources (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    uri VARCHAR(2048) NOT NULL UNIQUE,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    mime_type VARCHAR(255),
    resource_type VARCHAR(50) NOT NULL DEFAULT 'static',
    handler_module VARCHAR(255),
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT valid_resource_type CHECK (resource_type IN ('static', 'dynamic', 'template'))
);

CREATE INDEX idx_mcp_resources_uri ON mcp_resources(uri);
CREATE INDEX idx_mcp_resources_type ON mcp_resources(resource_type);

-- ============================================================================
-- MCP Tools Table
-- Registered tools available via MCP
-- ============================================================================
CREATE TABLE IF NOT EXISTS mcp_tools (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    name VARCHAR(255) NOT NULL UNIQUE,
    description TEXT,
    input_schema JSONB NOT NULL,
    handler_module VARCHAR(255) NOT NULL,
    handler_function VARCHAR(255) NOT NULL DEFAULT 'handle',
    requires_confirmation BOOLEAN DEFAULT FALSE,
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_mcp_tools_name ON mcp_tools(name);

-- ============================================================================
-- MCP Prompts Table
-- Registered prompts available via MCP
-- ============================================================================
CREATE TABLE IF NOT EXISTS mcp_prompts (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    name VARCHAR(255) NOT NULL UNIQUE,
    description TEXT,
    arguments JSONB DEFAULT '[]',
    template TEXT NOT NULL,
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_mcp_prompts_name ON mcp_prompts(name);

-- ============================================================================
-- Audit Log Table
-- Security audit trail
-- ============================================================================
CREATE TABLE IF NOT EXISTS audit_log (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    event_type VARCHAR(100) NOT NULL,
    actor VARCHAR(255),
    resource_type VARCHAR(100),
    resource_id VARCHAR(255),
    action VARCHAR(100) NOT NULL,
    details JSONB DEFAULT '{}',
    ip_address INET,
    user_agent TEXT,
    success BOOLEAN NOT NULL DEFAULT TRUE
);

CREATE INDEX idx_audit_log_timestamp ON audit_log(timestamp);
CREATE INDEX idx_audit_log_event ON audit_log(event_type);
CREATE INDEX idx_audit_log_actor ON audit_log(actor);

-- ============================================================================
-- Metrics Table
-- Performance metrics storage
-- ============================================================================
CREATE TABLE IF NOT EXISTS metrics (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    metric_name VARCHAR(255) NOT NULL,
    metric_type VARCHAR(50) NOT NULL,
    value DOUBLE PRECISION NOT NULL,
    labels JSONB DEFAULT '{}',
    CONSTRAINT valid_metric_type CHECK (metric_type IN ('counter', 'gauge', 'histogram', 'summary'))
);

CREATE INDEX idx_metrics_timestamp ON metrics(timestamp);
CREATE INDEX idx_metrics_name ON metrics(metric_name);

-- Hypertable-like partitioning (simplified for PostgreSQL)
-- In production Cloud SQL, consider using partitioning
CREATE INDEX idx_metrics_time_name ON metrics(timestamp, metric_name);

-- ============================================================================
-- Insert sample data
-- ============================================================================

-- Sample resources
INSERT INTO mcp_resources (uri, name, description, mime_type, resource_type) VALUES
    ('file:///etc/erlmcp/config.json', 'Configuration', 'Application configuration', 'application/json', 'static'),
    ('erlmcp://status', 'System Status', 'Current system status', 'application/json', 'dynamic'),
    ('erlmcp://metrics', 'Metrics', 'Application metrics', 'application/json', 'dynamic')
ON CONFLICT (uri) DO NOTHING;

-- Sample tools
INSERT INTO mcp_tools (name, description, input_schema, handler_module, handler_function) VALUES
    ('echo', 'Echo back the input', '{"type": "object", "properties": {"message": {"type": "string"}}, "required": ["message"]}', 'erlmcp_tools', 'echo'),
    ('get_time', 'Get current server time', '{"type": "object", "properties": {}}', 'erlmcp_tools', 'get_time'),
    ('calculate', 'Perform calculation', '{"type": "object", "properties": {"expression": {"type": "string"}}, "required": ["expression"]}', 'erlmcp_tools', 'calculate')
ON CONFLICT (name) DO NOTHING;

-- Sample prompts
INSERT INTO mcp_prompts (name, description, arguments, template) VALUES
    ('greeting', 'Generate a greeting', '[{"name": "name", "description": "Name to greet", "required": true}]', 'Hello, {{name}}! Welcome to erlmcp.'),
    ('summarize', 'Summarize text', '[{"name": "text", "description": "Text to summarize", "required": true}]', 'Please summarize the following: {{text}}')
ON CONFLICT (name) DO NOTHING;

-- ============================================================================
-- Create read-only user for monitoring
-- ============================================================================
DO $$
BEGIN
    IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'erlmcp_readonly') THEN
        CREATE ROLE erlmcp_readonly WITH LOGIN PASSWORD 'readonly_password';
    END IF;
END
$$;

GRANT CONNECT ON DATABASE erlmcp TO erlmcp_readonly;
GRANT USAGE ON SCHEMA erlmcp TO erlmcp_readonly;
GRANT SELECT ON ALL TABLES IN SCHEMA erlmcp TO erlmcp_readonly;
ALTER DEFAULT PRIVILEGES IN SCHEMA erlmcp GRANT SELECT ON TABLES TO erlmcp_readonly;

-- ============================================================================
-- Create function for updated_at trigger
-- ============================================================================
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Apply triggers
CREATE TRIGGER update_mcp_resources_updated_at
    BEFORE UPDATE ON mcp_resources
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_mcp_tools_updated_at
    BEFORE UPDATE ON mcp_tools
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_mcp_prompts_updated_at
    BEFORE UPDATE ON mcp_prompts
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

-- ============================================================================
-- Database statistics view
-- ============================================================================
CREATE OR REPLACE VIEW database_stats AS
SELECT
    (SELECT COUNT(*) FROM mcp_sessions WHERE status = 'active') AS active_sessions,
    (SELECT COUNT(*) FROM mcp_requests WHERE received_at > NOW() - INTERVAL '1 hour') AS requests_last_hour,
    (SELECT COUNT(*) FROM mcp_resources) AS total_resources,
    (SELECT COUNT(*) FROM mcp_tools) AS total_tools,
    (SELECT COUNT(*) FROM mcp_prompts) AS total_prompts,
    (SELECT AVG(duration_ms) FROM mcp_requests WHERE completed_at > NOW() - INTERVAL '1 hour') AS avg_response_time_ms;

-- Grant view access
GRANT SELECT ON database_stats TO erlmcp_readonly;

COMMIT;
