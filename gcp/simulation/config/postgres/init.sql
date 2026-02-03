-- ============================================================================
-- Cloud SQL Simulation - PostgreSQL Initialization
-- ============================================================================
-- Simulates GCP Cloud SQL environment with proper extensions and schemas

-- Enable required extensions (available in Cloud SQL)
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pg_stat_statements";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-- Create application schema
CREATE SCHEMA IF NOT EXISTS erlmcp;

-- Create additional databases for different environments
CREATE DATABASE erlmcp_staging;
CREATE DATABASE erlmcp_prod;

-- Grant privileges
GRANT ALL PRIVILEGES ON DATABASE erlmcp_dev TO erlmcp;
GRANT ALL PRIVILEGES ON DATABASE erlmcp_staging TO erlmcp;
GRANT ALL PRIVILEGES ON DATABASE erlmcp_prod TO erlmcp;

-- Create audit log table (simulates Cloud SQL audit logging)
CREATE TABLE IF NOT EXISTS erlmcp.audit_log (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    action VARCHAR(50) NOT NULL,
    table_name VARCHAR(100),
    record_id VARCHAR(255),
    old_data JSONB,
    new_data JSONB,
    user_id VARCHAR(255),
    ip_address INET,
    user_agent TEXT
);

CREATE INDEX idx_audit_log_timestamp ON erlmcp.audit_log(timestamp);
CREATE INDEX idx_audit_log_action ON erlmcp.audit_log(action);

-- Create sessions table (for distributed Erlang coordination)
CREATE TABLE IF NOT EXISTS erlmcp.sessions (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    node_name VARCHAR(255) NOT NULL,
    started_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_heartbeat TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    metadata JSONB DEFAULT '{}',
    status VARCHAR(20) NOT NULL DEFAULT 'active'
);

CREATE INDEX idx_sessions_node ON erlmcp.sessions(node_name);
CREATE INDEX idx_sessions_status ON erlmcp.sessions(status);

-- Create metrics table (for persistence)
CREATE TABLE IF NOT EXISTS erlmcp.metrics (
    id BIGSERIAL PRIMARY KEY,
    timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    metric_name VARCHAR(255) NOT NULL,
    metric_type VARCHAR(50) NOT NULL,
    value DOUBLE PRECISION NOT NULL,
    labels JSONB DEFAULT '{}'
);

CREATE INDEX idx_metrics_timestamp ON erlmcp.metrics(timestamp);
CREATE INDEX idx_metrics_name ON erlmcp.metrics(metric_name);

-- Partition metrics table by time (simulates Cloud SQL best practices)
-- In production, you'd use native partitioning

-- Create function for audit logging
CREATE OR REPLACE FUNCTION erlmcp.audit_trigger_func()
RETURNS TRIGGER AS $$
BEGIN
    INSERT INTO erlmcp.audit_log (action, table_name, record_id, old_data, new_data)
    VALUES (
        TG_OP,
        TG_TABLE_NAME,
        CASE
            WHEN TG_OP = 'DELETE' THEN OLD.id::text
            ELSE NEW.id::text
        END,
        CASE WHEN TG_OP != 'INSERT' THEN row_to_json(OLD) END,
        CASE WHEN TG_OP != 'DELETE' THEN row_to_json(NEW) END
    );
    RETURN COALESCE(NEW, OLD);
END;
$$ LANGUAGE plpgsql;

-- Log connection info
DO $$
BEGIN
    RAISE NOTICE 'GCP Cloud SQL Simulation initialized';
    RAISE NOTICE 'Database: erlmcp_dev';
    RAISE NOTICE 'Extensions: uuid-ossp, pg_stat_statements, pgcrypto';
END $$;
