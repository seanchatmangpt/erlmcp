#!/usr/bin/env k6
/**
 * erlmcp Ramp-up Load Test
 *
 * Purpose: Gradually increase load from 0 to 1000 RPS to measure
 *          system behavior under increasing traffic patterns.
 *
 * Test Phases:
 *   1. Ramp-up: 0 -> 1000 RPS over 5 minutes
 *   2. Sustained: Hold at 1000 RPS for 2 minutes
 *   3. Ramp-down: 1000 -> 0 RPS over 1 minute
 *
 * SLA Thresholds:
 *   - P95 latency < 200ms
 *   - P99 latency < 500ms
 *   - Error rate < 1%
 *   - Success rate > 99%
 *
 * @module ramp-up-test
 */

import http from 'k6/http';
import { check, sleep } from 'k6';
import { Rate, Trend, Counter } from 'k6/metrics';
import { SharedArray } from 'k6/data';

// ============================================================================
// CONFIGURATION
// ============================================================================

export const options = {
  // Ramp-up stages
  stages: [
    { duration: '1m', target: 100 },   // Warmup: 0 -> 100 RPS
    { duration: '2m', target: 300 },   // Ramp: 100 -> 300 RPS
    { duration: '2m', target: 600 },   // Ramp: 300 -> 600 RPS
    { duration: '2m', target: 1000 },  // Ramp: 600 -> 1000 RPS
    { duration: '2m', target: 1000 },  // Sustain at 1000 RPS
    { duration: '1m', target: 0 },     // Cooldown: 1000 -> 0 RPS
  ],

  // Thresholds for automated pass/fail
  thresholds: {
    // HTTP response times (milliseconds)
    'http_req_duration': [
      { threshold: 'p(95)<200', abortOnFail: false, delayAbortEval: '30s' },
      { threshold: 'p(99)<500', abortOnFail: false, delayAbortEval: '30s' },
      { threshold: 'avg<100', abortOnFail: false, delayAbortEval: '30s' },
    ],

    // HTTP errors
    'http_req_failed': [
      { threshold: 'rate<0.01', abortOnFail: true, delayAbortEval: '1m' },
    ],

    // Requests per second
    'http_reqs': [
      { threshold: 'rate>900', abortOnFail: false },
    ],

    // Custom error rate
    'errors': [
      { threshold: 'rate<0.01', abortOnFail: true },
    ],
  },

  // Graceful stop settings
  gracefulStop: '30s',

  // No default connection timeout
  noConnectionReuse: false,
  noVUConnectionReuse: false,

  // User agent
  userAgent: 'k6/erlmcp-rampup-test',

  // Batch settings
  batch: 20,
  batchPerHost: 10,
};

// ============================================================================
// CUSTOM METRICS
// ============================================================================

const errorRate = new Rate('errors');
const latency = new Trend('latency', true);
const throughputCounter = new Counter('throughput');
const customChecks = {
  success: new Rate('check_success'),
  latency_ok: new Rate('check_latency_ok'),
  response_valid: new Rate('check_response_valid'),
};

// ============================================================================
// TEST DATA - Production-like payloads
// ============================================================================

const mcpRequests = new SharedArray('MCP Requests', function () {
  return [
    // Initialize requests
    {
      jsonrpc: '2.0',
      id: 1,
      method: 'initialize',
      params: {
        protocolVersion: '2024-11-05',
        capabilities: {
          tools: {},
          resources: {},
          prompts: {},
        },
        clientInfo: {
          name: 'k6-load-test',
          version: '1.0.0',
        },
      },
    },
    // List tools requests
    {
      jsonrpc: '2.0',
      id: 2,
      method: 'tools/list',
      params: {},
    },
    // List resources requests
    {
      jsonrpc: '2.0',
      id: 3,
      method: 'resources/list',
      params: {},
    },
    // List prompts requests
    {
      jsonrpc: '2.0',
      id: 4,
      method: 'prompts/list',
      params: {},
    },
    // Health check requests
    {
      jsonrpc: '2.0',
      id: 5,
      method: 'health',
      params: {},
    },
    // Tool call requests (simulated)
    {
      jsonrpc: '2.0',
      id: 6,
      method: 'tools/call',
      params: {
        name: 'example_tool',
        arguments: {
          query: 'test data',
          limit: 10,
        },
      },
    },
    // Resource read requests
    {
      jsonrpc: '2.0',
      id: 7,
      method: 'resources/read',
      params: {
        uri: 'example://data/test',
      },
    },
  ];
});

// ============================================================================
// ENVIRONMENT VARIABLES
// ============================================================================

const BASE_URL = __ENV.BASE_URL || 'http://localhost:8080';
const API_ENDPOINT = `${BASE_URL}/mcp`;
const HEALTH_ENDPOINT = `${BASE_URL}/health`;

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/**
 * Select a random MCP request from the test data
 */
function getRandomRequest() {
  const request = mcpRequests[Math.floor(Math.random() * mcpRequests.length)];
  // Generate unique ID for each request
  return JSON.stringify({
    ...request,
    id: Math.floor(Math.random() * 1000000),
  });
}

/**
 * Validate JSON-RPC response
 */
function validateResponse(response) {
  try {
    const body = JSON.parse(response.body);

    // Must have jsonrpc field
    if (body.jsonrpc !== '2.0') {
      return false;
    }

    // Must have either result or error
    if (!body.result && !body.error) {
      return false;
    }

    // If error exists, it should have code and message
    if (body.error && (!body.error.code || !body.error.message)) {
      return false;
    }

    return true;
  } catch (e) {
    return false;
  }
}

/**
 * Extract latency from response timing
 */
function extractLatency(response) {
  return response.timings.duration;
}

// ============================================================================
// SETUP
// ============================================================================

export function setup() {
  console.log(`Starting ramp-up test against: ${BASE_URL}`);
  console.log(`API Endpoint: ${API_ENDPOINT}`);

  // Pre-warm the connection
  const healthRes = http.get(HEALTH_ENDPOINT, {
    tags: { name: 'setup-health-check' },
    timeout: '10s',
  });

  if (healthRes.status !== 200) {
    console.warn(`Health check returned status ${healthRes.status}`);
  }

  return {
    startTime: new Date().toISOString(),
    targetUrl: BASE_URL,
  };
}

// ============================================================================
// TEARDOWN
// ============================================================================

export function teardown(data) {
  console.log(`Ramp-up test completed. Started at: ${data.startTime}`);
  console.log(`Test ended at: ${new Date().toISOString()}`);
}

// ============================================================================
// MAIN TEST FUNCTION
// ============================================================================

export default function (data) {
  // Randomize request selection for realistic traffic pattern
  const payload = getRandomRequest();

  // Add unique request ID for correlation
  const requestId = `req_${__VU}_${__ITER}_${Date.now()}`;

  // Make the request
  const params = {
    headers: {
      'Content-Type': 'application/json',
      'X-Request-ID': requestId,
      'User-Agent': 'k6-rampup-test/1.0',
    },
    tags: {
      name: 'mcp-request',
      test_type: 'ramp-up',
    },
    timeout: '30s',
  };

  const response = http.post(API_ENDPOINT, payload, params);

  // Record metrics
  const responseTime = extractLatency(response);
  latency.add(responseTime);
  throughputCounter.add(1);

  // Check success criteria
  const success = check(response, {
    'status is 200 or 201': (r) => r.status >= 200 && r.status < 300,
    'response time < 200ms': (r) => r.timings.duration < 200,
    'response time < 500ms': (r) => r.timings.duration < 500,
    'has valid JSON-RPC response': (r) => validateResponse(r),
    'response body is not empty': (r) => r.body.length > 0,
  });

  // Update custom metrics
  customChecks.success.add(success);
  customChecks.latency_ok.add(response.timings.duration < 200);
  customChecks.response_valid.add(validateResponse(response));

  // Track errors
  if (!success || response.status >= 400) {
    errorRate.add(1);
    console.error(`Request failed: ${response.status} - ${response.body.substring(0, 200)}`);
  } else {
    errorRate.add(0);
  }

  // Small sleep to pace requests
  sleep(Math.random() * 0.1 + 0.05); // 50-150ms think time
}
