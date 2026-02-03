-- ============================================================================
-- erlmcp Integration Test Database Initialization
-- ============================================================================
-- Creates test schema and data for PostgreSQL-based integration tests.

-- Create test schema
CREATE SCHEMA IF NOT EXISTS erlmcp_test;

-- Create sessions table for session persistence testing
CREATE TABLE IF NOT EXISTS erlmcp_test.sessions (
    id VARCHAR(64) PRIMARY KEY,
    client_id VARCHAR(128) NOT NULL,
    state JSONB NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP,
    metadata JSONB DEFAULT '{}'::jsonb
);

-- Create resources table for resource access testing
CREATE TABLE IF NOT EXISTS erlmcp_test.resources (
    uri VARCHAR(512) PRIMARY KEY,
    name VARCHAR(256) NOT NULL,
    description TEXT,
    mime_type VARCHAR(128),
    content BYTEA,
    metadata JSONB DEFAULT '{}'::jsonb,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create prompts table for prompt template testing
CREATE TABLE IF NOT EXISTS erlmcp_test.prompts (
    name VARCHAR(256) PRIMARY KEY,
    description TEXT,
    template TEXT NOT NULL,
    arguments JSONB DEFAULT '[]'::jsonb,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create tools table for tool invocation testing
CREATE TABLE IF NOT EXISTS erlmcp_test.tools (
    name VARCHAR(256) PRIMARY KEY,
    description TEXT NOT NULL,
    input_schema JSONB NOT NULL,
    metadata JSONB DEFAULT '{}'::jsonb,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create subscriptions table for subscription testing
CREATE TABLE IF NOT EXISTS erlmcp_test.subscriptions (
    id SERIAL PRIMARY KEY,
    uri VARCHAR(512) NOT NULL,
    subscriber_pid VARCHAR(128) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(uri, subscriber_pid)
);

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_sessions_client_id ON erlmcp_test.sessions(client_id);
CREATE INDEX IF NOT EXISTS idx_sessions_expires_at ON erlmcp_test.sessions(expires_at);
CREATE INDEX IF NOT EXISTS idx_subscriptions_uri ON erlmcp_test.subscriptions(uri);

-- Insert test data
INSERT INTO erlmcp_test.resources (uri, name, description, mime_type, content, metadata) VALUES
    ('test://resource/1', 'Test Resource 1', 'First test resource', 'text/plain', 'Hello, World!', '{"type": "static"}'::jsonb),
    ('test://resource/2', 'Test Resource 2', 'Second test resource', 'application/json', '{"key": "value"}'::jsonb, '{"type": "dynamic"}'::jsonb),
    ('test://resource/3', 'Markdown Resource', 'Markdown documentation', 'text/markdown', '# Test Document\n\nThis is a test.', '{"type": "doc"}'::jsonb)
ON CONFLICT (uri) DO NOTHING;

INSERT INTO erlmcp_test.prompts (name, description, template, arguments) VALUES
    ('test_prompt', 'A test prompt template', 'Hello, {{name}}! Welcome to {{app}}.', '[{"name": "name", "description": "User name", "required": true}, {"name": "app", "description": "Application name", "required": false}]'::jsonb),
    ('summarize', 'Summarize content', 'Please summarize: {{content}}', '[{"name": "content", "description": "Content to summarize", "required": true}]'::jsonb)
ON CONFLICT (name) DO NOTHING;

INSERT INTO erlmcp_test.tools (name, description, input_schema, metadata) VALUES
    ('echo', 'Echo input back', '{"type": "object", "properties": {"message": {"type": "string"}}, "required": ["message"]}'::jsonb, '{"category": "test"}'::jsonb),
    ('calculate', 'Perform calculation', '{"type": "object", "properties": {"x": {"type": "number"}, "y": {"type": "number"}, "operation": {"type": "string", "enum": ["add", "subtract", "multiply", "divide"]}}, "required": ["x", "y", "operation"]}'::jsonb, '{"category": "math"}'::jsonb)
ON CONFLICT (name) DO NOTHING;

-- Grant permissions
GRANT ALL PRIVILEGES ON SCHEMA erlmcp_test TO erlmcp_test;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA erlmcp_test TO erlmcp_test;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA erlmcp_test TO erlmcp_test;

ANALYZE;
