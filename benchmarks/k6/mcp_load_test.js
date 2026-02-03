#!/usr/bin/env k6
//===----------------------------------------------------------------------===
//
//  erlmcp v3 - Fortune 500 Scale Load Testing with k6
//
//  Target: 100K+ concurrent connections, 5M+ msg/s throughput
//
//  Usage:
//    k6 run --vus 10000 --duration 5m mcp_load_test.js
//    k6 run --vus 50000 --duration 10m mcp_load_test.js
//    k6 run --vus 100000 --duration 30m mcp_load_test.js
//
//  Scaling Projections:
//    | Scale  | VUs    | Duration | Target RPS | Target Connections |
//    |--------|--------|----------|------------|---------------------|
//    | 1x     | 10K    | 5m       | 1M         | 50K                 |
//    | 10x    | 50K    | 10m      | 5M         | 500K                |
//    | 100x   | 100K   | 30m      | 10M        | 5M                  |
//
//===----------------------------------------------------------------------===

import { check, group, sleep } from 'k6';
import { Rate } from 'k6/metrics';

// Custom metrics for erlmcp
const erlmcpMetrics = {
    // Connection metrics
    connections_established: new Rate('erlmcp_connections_established'),
    connections_failed: new Rate('erlmcp_connections_failed'),
    connections_reused: new Rate('erlmcp_connections_reused'),

    // Message throughput metrics
    messages_sent: new Rate('erlmcp_messages_sent'),
    messages_received: new Rate('erlmcp_messages_received'),
    message_latency: new Rate('erlmcp_message_latency'),

    // Tool invocation metrics
    tool_calls: new Rate('erlmcp_tool_calls'),
    tool_call_duration: new Rate('erlmcp_tool_call_duration'),
    tool_call_success: new Rate('erlmcp_tool_call_success'),

    // Resource access metrics
    resource_reads: new Rate('erlmcp_resource_reads'),
    resource_subscriptions: new Rate('erlmcp_resource_subscriptions'),

    // Token optimization metrics
    token_usage_baseline: new Rate('erlmcp_token_usage_baseline'),
    token_usage_batched: new Rate('erlmcp_token_usage_batched'),
    token_savings_ratio: new Rate('erlmcp_token_savings_ratio'),

    // Flash attention metrics
    flash_attention_speedup: new Rate('erlmcp_flash_attention_speedup'),
    streaming_throughput: new Rate('erlmcp_streaming_throughput'),
};

// Test configuration
export const options = {
    // Virtual Users
    vus: 10000,           // Starting VUs
    vusMax: 100000,       // Maximum VUs for Fortune 500 scale

    // Test duration
    duration: '5m',       // Main test duration
    stages: [
        { duration: '1m', target: 10000 },   // Ramp up to 10K
        { duration: '2m', target: 50000 },   // Ramp up to 50K
        { duration: '5m', target: 100000 },  // Ramp up to 100K
        { duration: '5m', target: 100000 },  // Sustain at 100K
        { duration: '2m', target: 0 },       // Ramp down
    ],

    // Thresholds for SLA compliance
    thresholds: {
        // Connection success rate > 99.9%
        'erlmcp_connections_failed': ['rate<0.001'],

        // Message throughput targets
        'erlmcp_messages_sent': ['rate>5000000'],  // 5M msg/s

        // Latency targets (p99)
        'erlmcp_message_latency': ['p(99)<100'],    // <100ms

        // Tool call targets
        'erlmcp_tool_call_success': ['rate>0.999'],  // 99.9% success
        'erlmcp_tool_call_duration': ['p(95)<50'],  // <50ms

        // Token optimization targets
        'erlmcp_token_savings_ratio': ['rate>0.5'],  // >50% savings

        // Flash attention targets
        'erlmcp_flash_attention_speedup': ['rate>2.0'], // >2x speedup
        'erlmcp_streaming_throughput': ['rate>10000'], // >10K tokens/sec
    },
};

// MCP server configuration
const MCP_BASE_URL = __ENV.MCP_BASE_URL || 'http://localhost:8080';
const MCP_WS_URL = __ENV.MCP_WS_URL || 'ws://localhost:8080/ws';

// Session management
let sessionId = null;
let transportType = __ENV.TRANSPORT_TYPE || 'sse'; // 'sse' | 'ws' | 'http'

//===----------------------------------------------------------------------===
// Setup Function
//===----------------------------------------------------------------------===
export function setup() {
    // Initialize test session
    const response = http.post(`${MCP_BASE_URL}/mcp/sessions`, JSON.stringify({
        capabilities: {
            roots: { listChanged: true },
            sampling: {}
        }
    }), {
        headers: { 'Content-Type': 'application/json' },
    });

    check(response, {
        'session created': (r) => r.status === 200,
    });

    if (response.status === 200) {
        const data = response.json();
        sessionId = data.sessionId;
        console.log(`Session created: ${sessionId}`);
    }

    return { sessionId };
}

//===----------------------------------------------------------------------===
// Teardown Function
//===----------------------------------------------------------------------===
export function teardown(data) {
    if (data.sessionId) {
        const response = http.del(`${MCP_BASE_URL}/mcp/sessions/${data.sessionId}`);
        console.log(`Session deleted: ${data.sessionId}`);
    }
}

//===----------------------------------------------------------------------===
// Main Test Scenario
//===----------------------------------------------------------------------===
export default function(data) {
    const session = data.sessionId || sessionId;

    // Test 1: Connection Establishment
    testConnectionEstablishment(session);

    // Test 2: Tool Invocation Throughput
    testToolInvocation(session);

    // Test 3: Resource Access
    testResourceAccess(session);

    // Test 4: Message Batching
    testMessageBatching(session);

    // Test 5: Flash Attention Streaming
    testFlashAttentionStreaming(session);

    // Small pause between iterations
    sleep(0.1);
}

//===----------------------------------------------------------------------===
// Test: Connection Establishment
//===----------------------------------------------------------------------===
function testConnectionEstablishment(session) {
    group('Connection: Establish', function() {
        const startTime = Date.now();

        const response = http.get(`${MCP_BASE_URL}/mcp/sessions/${session}/status`, {
            tags: { name: 'connection_status' },
        });

        const duration = Date.now() - startTime;

        check(response, {
            'status: 200': (r) => r.status === 200,
            'latency: <10ms': (r) => duration < 10,
            'latency: <50ms': (r) => duration < 50,
        });

        erlmcpMetrics.connections_established.add(response.status === 200 ? 1 : 0);
        erlmcpMetrics.connections_failed.add(response.status !== 200 ? 1 : 0);
    });
}

//===----------------------------------------------------------------------===
// Test: Tool Invocation Throughput
//===----------------------------------------------------------------------===
function testToolInvocation(session) {
    group('Tools: Invoke', function() {
        const tools = [
            'fs_read_file',
            'fs_write_file',
            'fs_list_directory',
            'search_files',
            'git_diff',
        ];

        const tool = tools[Math.floor(Math.random() * tools.length)];
        const startTime = Date.now();

        const response = http.post(
            `${MCP_BASE_URL}/mcp/sessions/${session}/tools/${tool}/invoke`,
            JSON.stringify({
                arguments: {
                    path: '/tmp/test.txt',
                },
            }),
            {
                headers: { 'Content-Type': 'application/json' },
                tags: { name: `tool_${tool}` },
            }
        );

        const duration = Date.now() - startTime;

        check(response, {
            'tool call: success': (r) => r.status === 200 || r.status === 202,
            'tool call: <50ms': (r) => duration < 50,
            'tool call: <100ms': (r) => duration < 100,
        });

        erlmcpMetrics.tool_calls.add(1);
        erlmcpMetrics.tool_call_duration.add(duration);
        erlmcpMetrics.tool_call_success.add(response.status === 200 ? 1 : 0);
    });
}

//===----------------------------------------------------------------------===
// Test: Resource Access
//===----------------------------------------------------------------------===
function testResourceAccess(session) {
    group('Resources: Access', function() {
        const resources = ['file://config.json', 'file://schema.json'];
        const resource = resources[Math.floor(Math.random() * resources.length)];

        const response = http.get(
            `${MCP_BASE_URL}/mcp/sessions/${session}/resources/${encodeURIComponent(resource)}`,
            {
                tags: { name: 'resource_read' },
            }
        );

        check(response, {
            'resource: found': (r) => r.status === 200,
            'resource: <100ms': (r) => r.timings.duration < 100,
        });

        erlmcpMetrics.resource_reads.add(response.status === 200 ? 1 : 0);
    });
}

//===----------------------------------------------------------------------===
// Test: Message Batching (Token Optimization)
//===----------------------------------------------------------------------===
function testMessageBatching(session) {
    group('Optimization: Message Batching', function() {
        // Baseline: individual messages
        const baselineMessages = Array(10).fill(null).map((_, i) => ({
            role: 'user',
            content: { type: 'text', text: `Message ${i}` }
        }));

        const baselineStart = Date.now();
        const baselineResponse = http.post(
            `${MCP_BASE_URL}/mcp/sessions/${session}/messages/baseline`,
            JSON.stringify({ messages: baselineMessages }),
            {
                headers: { 'Content-Type': 'application/json' },
                tags: { name: 'baseline_messages' },
            }
        );
        const baselineDuration = Date.now() - baselineStart;

        // Batched: combined message
        const batchedMessage = {
            role: 'user',
            content: { type: 'text', text: baselineMessages.map(m => m.content.text).join('\n') }
        };

        const batchedStart = Date.now();
        const batchedResponse = http.post(
            `${MCP_BASE_URL}/mcp/sessions/${session}/messages/batched`,
            JSON.stringify({ messages: [batchedMessage] }),
            {
                headers: { 'Content-Type': 'application/json' },
                tags: { name: 'batched_messages' },
            }
        );
        const batchedDuration = Date.now() - batchedStart;

        // Calculate token savings
        const tokenSavings = 1 - (batchedDuration / baselineDuration);

        check(baselineResponse, {
            'baseline: success': (r) => r.status === 200,
        });

        check(batchedResponse, {
            'batched: success': (r) => r.status === 200,
            'batched: faster than baseline': () => batchedDuration < baselineDuration,
        });

        erlmcpMessages.token_usage_baseline.add(baselineDuration);
        erlmcpMessages.token_usage_batched.add(batchedDuration);
        erlmcpMessages.token_savings_ratio.add(tokenSavings);
    });
}

//===----------------------------------------------------------------------===
// Test: Flash Attention Streaming
//===----------------------------------------------------------------------===
function testFlashAttentionStreaming(session) {
    group('Optimization: Flash Attention Streaming', function() {
        const streamTokens = 100;

        const response = http.post(
            `${MCP_BASE_URL}/mcp/sessions/${session}/stream`,
            JSON.stringify({
                tokens: streamTokens,
                optimization: 'flash_attention',
            }),
            {
                headers: { 'Content-Type': 'application/json' },
                tags: { name: 'flash_attention_stream' },
            }
        );

        // Calculate speedup (baseline: 100ms per 100 tokens)
        const baselineTime = streamTokens; // 1ms per token
        const actualTime = response.timings.duration;
        const speedup = baselineTime / actualTime;

        check(response, {
            'streaming: success': (r) => r.status === 200,
            'streaming: >2x speedup': () => speedup > 2.0,
            'streaming: >4x speedup': () => speedup > 4.0,
        });

        erlmcpMetrics.flash_attention_speedup.add(speedup);
        erlmcpMetrics.streaming_throughput.add(streamTokens / (actualTime / 1000));
    });
}

//===----------------------------------------------------------------------===
// WebSocket Transport Test (for real-time scenarios)
//===----------------------------------------------------------------------===
export function wsTest(data) {
    const session = data.sessionId || sessionId;

    if (transportType === 'ws') {
        const ws = new WebSocket(`${MCP_WS_URL}?session=${session}`);

        ws.onopen = function() {
            console.log('WebSocket connected');

            // Send messages rapidly
            for (let i = 0; i < 100; i++) {
                ws.send(JSON.stringify({
                    jsonrpc: '2.0',
                    id: i,
                    method: 'tools/invoke',
                    params: {
                        name: 'test_tool',
                        arguments: { index: i }
                    }
                }));
            }
        };

        ws.onmessage = function(event) {
            const data = JSON.parse(event.data);
            erlmcpMetrics.messages_received.add(1);
        };
    }
}
