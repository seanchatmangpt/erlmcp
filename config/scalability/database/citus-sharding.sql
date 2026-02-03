-- Citus Database Sharding Configuration for erlmcp v3
-- This script sets up distributed tables and sharding strategies

-- Enable Citus extension
CREATE EXTENSION citus;

-- Create distributed tables for erlmcp v3
CREATE TABLE erlmcp_sessions (
    session_id UUID PRIMARY KEY,
    user_id UUID NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_accessed TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    expires_at TIMESTAMPTZ NOT NULL,
    data JSONB NOT NULL,
    metadata JSONB,
    status VARCHAR(20) NOT NULL DEFAULT 'active',
    shard_id INT NOT NULL,
    CONSTRAINT valid_status CHECK (status IN ('active', 'expired', 'terminated'))
);

CREATE TABLE erlmcp_tools (
    tool_id UUID PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    version VARCHAR(50) NOT NULL,
    description TEXT,
    schema JSONB NOT NULL,
    implementation BYTEA,
    timeout INT NOT NULL DEFAULT 30000,
    max_concurrent INT NOT NULL DEFAULT 100,
    enabled BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    shard_id INT NOT NULL,
    CONSTRAINT valid_timeout CHECK (timeout BETWEEN 1000 AND 300000),
    CONSTRAINT valid_max_concurrent CHECK (max_concurrent BETWEEN 1 AND 1000)
);

CREATE TABLE erlmcp_resources (
    resource_id UUID PRIMARY KEY,
    type VARCHAR(50) NOT NULL,
    name VARCHAR(255) NOT NULL,
    content BYTEA,
    metadata JSONB,
    size BIGINT NOT NULL,
    checksum VARCHAR(64),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    expires_at TIMESTAMPTZ,
    access_count INT NOT NULL DEFAULT 0,
    last_accessed TIMESTAMPTZ,
    shard_id INT NOT NULL,
    CONSTRAINT valid_size CHECK (size >= 0)
);

CREATE TABLE erlmcp_tool_executions (
    execution_id UUID PRIMARY KEY,
    session_id UUID NOT NULL REFERENCES erlmcp_sessions(session_id),
    tool_id UUID NOT NULL REFERENCES erlmcp_tools(tool_id),
    input JSONB NOT NULL,
    output JSONB,
    status VARCHAR(20) NOT NULL DEFAULT 'pending',
    started_at TIMESTAMPTZ,
    completed_at TIMESTAMPTZ,
    error_message TEXT,
    execution_time BIGINT,
    shard_id INT NOT NULL,
    CONSTRAINT valid_status CHECK (status IN ('pending', 'running', 'completed', 'failed', 'cancelled'))
);

CREATE TABLE erlmcp_user_metrics (
    metric_id UUID PRIMARY KEY,
    user_id UUID NOT NULL,
    metric_type VARCHAR(50) NOT NULL,
    metric_value DOUBLE PRECISION NOT NULL,
    timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    metadata JSONB,
    shard_id INT NOT NULL
);

-- Create distributed indexes
CREATE INDEX idx_sessions_user ON erlmcp_sessions USING hash (user_id);
CREATE INDEX idx_sessions_status ON erlmcp_sessions USING hash (status);
CREATE INDEX idx_sessions_last_accessed ON erlmcp_sessions (last_accessed);
CREATE INDEX idx_tools_name ON erlmcp_tools USING hash (name);
CREATE INDEX idx_tools_enabled ON erlmcp_tools USING hash (enabled);
CREATE INDEX idx_resources_type ON erlmcp_resources USING hash (type);
CREATE INDEX idx_executions_session ON erlmcp_tool_executions USING hash (session_id);
CREATE INDEX idx_executions_status ON erlmcp_tool_executions USING hash (status);
CREATE INDEX idx_metrics_user ON erlmcp_user_metrics USING hash (user_id);
CREATE INDEX idx_metrics_type ON erlmcp_user_metrics USING hash (metric_type);
CREATE INDEX idx_metrics_timestamp ON erlmcp_user_metrics (timestamp);

-- Shard the tables by appropriate columns
SELECT create_distributed_table('erlmcp_sessions', 'user_id');
SELECT create_distributed_table('erlmcp_tools', 'tool_id');
SELECT create_distributed_table('erlmcp_resources', 'resource_id');
SELECT create_distributed_table('erlmcp_tool_executions', 'session_id');
SELECT create_distributed_table('erlmcp_user_metrics', 'user_id');

-- Create shard placement groups for better data locality
SELECT create_placement_group('sessions_group', 3);
SELECT create_placement_group('tools_group', 3);
SELECT create_placement_group('resources_group', 3);

-- Create worker tables for specific shard
-- This would be run on each worker node
CREATE TABLE erlmcp_sessions_distributed (
    LIKE erlmcp_sessions INCLUDING ALL
);

CREATE TABLE erlmcp_tools_distributed (
    LIKE erlmcp_tools INCLUDING ALL
);

CREATE TABLE erlmcp_resources_distributed (
    LIKE erlmcp_resources INCLUDING ALL
);

CREATE TABLE erlmcp_tool_executions_distributed (
    LIKE erlmcp_tool_executions INCLUDING ALL
);

CREATE TABLE erlmcp_user_metrics_distributed (
    LIKE erlmcp_user_metrics INCLUDING ALL
);

-- Create views for easier querying
CREATE VIEW erlmcp_active_sessions AS
SELECT * FROM erlmcp_sessions
WHERE status = 'active' AND expires_at > NOW();

CREATE VIEW erlmcp_pending_executions AS
SELECT * FROM erlmcp_tool_executions
WHERE status = 'pending';

CREATE VIEW erlmcp_resource_stats AS
SELECT
    type,
    COUNT(*) as resource_count,
    SUM(size) as total_size,
    AVG(access_count) as avg_access_count
FROM erlmcp_resources
GROUP BY type;

-- Create partitioned tables for time-series data
CREATE TABLE erlmcp_executions_history (
    execution_id UUID,
    session_id UUID NOT NULL,
    tool_id UUID NOT NULL,
    input JSONB NOT NULL,
    output JSONB,
    status VARCHAR(20) NOT NULL,
    started_at TIMESTAMPTZ,
    completed_at TIMESTAMPTZ,
    error_message TEXT,
    execution_time BIGINT,
    shard_id INT NOT NULL
) PARTITION BY RANGE (started_at);

-- Create time-based partitions for better query performance
CREATE TABLE erlmcp_executions_history_2024_01 PARTITION OF erlmcp_executions_history
    FOR VALUES FROM ('2024-01-01') TO ('2024-02-01');

CREATE TABLE erlmcp_executions_history_2024_02 PARTITION OF erlmcp_executions_history
    FOR VALUES FROM ('2024-02-01') TO ('2024-03-01');

CREATE TABLE erlmcp_executions_history_2024_03 PARTITION OF erlmcp_executions_history
    FOR VALUES FROM ('2024-03-01') TO ('2024-04-01');

-- Create constraints for partitioned table
ALTER TABLE erlmcp_executions_history ADD PRIMARY KEY (execution_id);
ALTER TABLE erlmcp_executions_history ADD CONSTRAINT fk_session_id FOREIGN KEY (session_id) REFERENCES erlmcp_sessions(session_id);
ALTER TABLE erlmcp_executions_history ADD CONSTRAINT fk_tool_id FOREIGN KEY (tool_id) REFERENCES erlmcp_tools(tool_id);

-- Create distributed function for cross-shard queries
CREATE OR REPLACE FUNCTION erlmcp_get_user_sessions(user_id UUID)
RETURNS TABLE(
    session_id UUID,
    created_at TIMESTAMPTZ,
    last_accessed TIMESTAMPTZ,
    expires_at TIMESTAMPTZ,
    status VARCHAR(20),
    data JSONB
) AS $$
BEGIN
    RETURN QUERY SELECT
        session_id, created_at, last_accessed, expires_at, status, data
    FROM erlmcp_sessions
    WHERE user_id = user_id
    ORDER BY last_accessed DESC;
END;
$$ LANGUAGE plpgsql;

-- Create function for session cleanup
CREATE OR REPLACE FUNCTION erlmcp_cleanup_expired_sessions()
RETURNS INTEGER AS $$
DECLARE
    expired_count INTEGER;
BEGIN
    DELETE FROM erlmcp_sessions
    WHERE expires_at <= NOW();

    GET DIAGNOSTICS expired_count = ROW_COUNT;
    RETURN expired_count;
END;
$$ LANGUAGE plpgsql;

-- Create scheduler job for cleanup
SELECT cron.schedule('0 * * * *', $$SELECT erlmcp_cleanup_expired_sessions()$$);