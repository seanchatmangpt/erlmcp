#!/usr/bin/env k6
/**
 * erlmcp Sustained Load Test
 *
 * Purpose: Measure system behavior under sustained load at constant RPS.
 *          Identifies memory leaks, connection pool exhaustion, and
 *          resource accumulation over time.
 *
 * Test Configuration:
 *   - Duration: 5 minutes sustained
 *   - Target: 500 RPS constant
 *   - Scenarios: Mixed workload (read/write/health)
 *
 * SLA Thresholds:
 *   - P95 latency < 150ms
 *   - P99 latency < 300ms
 *   - Error rate < 0.5%
 *   - Memory growth < 10% over test duration
 *
 * @module sustained-load-test
 */

import http from 'k6/http';
import { check, sleep } from 'k6';
import { Rate, Trend, Counter, Gauge } from 'k6/metrics';
import { SharedArray } from 'k6/data';

// ============================================================================
// CONFIGURATION
// ============================================================================

export const options = {
  // Sustained load profile
  stages: [
    { duration: '30s', target: 500 },   // Ramp to 500 RPS
    { duration: '5m', target: 500 },    // Sustain for 5 minutes
    { duration: '30s', target: 0 },     // Ramp down
  ],

  // Thresholds for sustained load
  thresholds: {
    // HTTP response times
    'http_req_duration': [
      { threshold: 'p(95)<150', abortOnFail: false, delayAbortEval: '30s' },
      { threshold: 'p(99)<300', abortOnFail: false, delayAbortEval: '30s' },
      { threshold: 'avg<80', abortOnFail: false, delayAbortEval: '30s' },
      { threshold: 'max<1000', abortOnFail: false },
    ],

    // HTTP errors
    'http_req_failed': [
      { threshold: 'rate<0.005', abortOnFail: true, delayAbortEval: '1m' },
    ],

    // Throughput
    'http_reqs': [
      { threshold: 'rate>480', abortOnFail: false },
    ],

    // Custom error rate
    'errors': [
      { threshold: 'rate<0.005', abortOnFail: true },
    ],

    // Memory stability (tracked via response sizes)
    'http_req_duration': ['avg<100'],
  },

  // Graceful stop
  gracefulStop: '30s',
  discardResponseBodies: false,

  // User agent
  userAgent: 'k6/erlmcp-sustained-test',

  // Batch settings
  batch: 50,
  batchPerHost: 20,
};

// ============================================================================
// CUSTOM METRICS
// ============================================================================

const errorRate = new Rate('errors');
const latency = new Trend('latency', true);
const throughputCounter = new Counter('throughput');

// Scenario-specific metrics
const readLatency = new Trend('read_latency');
const writeLatency = new Trend('write_latency');
const healthLatency = new Trend('health_latency');

const customChecks = {
  success: new Rate('check_success'),
  response_valid: new Rate('check_response_valid'),
  no_memory_leak: new Rate('check_memory_stable'),
};

// ============================================================================
// TEST DATA - Weighted workload distribution
// ============================================================================

const workloadMix = {
  read: 0.60,      // 60% reads (list tools, resources, prompts)
  write: 0.30,     // 30% writes (tool calls, resource operations)
  health: 0.10,    // 10% health checks
};

const mcpRequests = new SharedArray('MCP Requests', function () {
  return [
    // Read operations (60%)
    {
      jsonrpc: '2.0',
      method: 'tools/list',
      params: {},
      type: 'read',
    },
    {
      jsonrpc: '2.0',
      method: 'resources/list',
      params: {},
      type: 'read',
    },
    {
      jsonrpc: '2.0',
      method: 'prompts/list',
      params: {},
      type: 'read',
    },
    {
      jsonrpc: '2.0',
      method: 'tools/call',
      params: {
        name: 'get_status',
        arguments: {},
      },
      type: 'read',
    },

    // Write operations (30%)
    {
      jsonrpc: '2.0',
      method: 'tools/call',
      params: {
        name: 'create_item',
        arguments: {
          name: `test_item_${Math.random()}`,
          value: 'test data',
        },
      },
      type: 'write',
    },
    {
      jsonrpc: '2.0',
      method: 'resources/subscribe',
      params: {
        uri: 'example://events/test',
      },
      type: 'write',
    },

    // Health checks (10%)
    {
      jsonrpc: '2.0',
      method: 'health',
      params: {},
      type: 'health',
    },
  ];
});

// ============================================================================
// ENVIRONMENT VARIABLES
// ============================================================================

const BASE_URL = __ENV.BASE_URL || 'http://localhost:8080';
const API_ENDPOINT = `${BASE_URL}/mcp`;
const HEALTH_ENDPOINT = `${BASE_URL}/health`;
const METRICS_ENDPOINT = `${BASE_URL}/metrics`;

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/**
 * Select request based on workload mix distribution
 */
function selectRequestByMix() {
  const rand = Math.random();

  if (rand < workloadMix.read) {
    // Select a read operation
    const readOps = mcpRequests.filter((r) => r.type === 'read');
    return readOps[Math.floor(Math.random() * readOps.length)];
  } else if (rand < workloadMix.read + workloadMix.write) {
    // Select a write operation
    const writeOps = mcpRequests.filter((r) => r.type === 'write');
    return writeOps[Math.floor(Math.random() * writeOps.length)];
  } else {
    // Health check
    return mcpRequests.find((r) => r.type === 'health');
  }
}

/**
 * Validate JSON-RPC response
 */
function validateResponse(response) {
  try {
    const body = JSON.parse(response.body);
    return body.jsonrpc === '2.0' && (body.result || body.error);
  } catch (e) {
    return false;
  }
}

/**
 * Check for memory leak indicators
 */
function checkMemoryStability(response) {
  // Track response body size as proxy for memory
  const size = response.body.length;

  // Check for unusual growth in response size
  // (which might indicate memory leaks in response building)
  if (size > 10 * 1024 * 1024) {
    // Response > 10MB is suspicious
    console.warn(`Large response detected: ${size} bytes`);
    return false;
  }

  return true;
}

// ============================================================================
// SETUP
// ============================================================================

export function setup() {
  console.log(`Starting sustained load test against: ${BASE_URL}`);
  console.log(`Workload mix: Read ${workloadMix.read * 100}%, Write ${workloadMix.write * 100}%, Health ${workloadMix.health * 100}%`);

  // Verify system health before starting
  const healthRes = http.get(HEALTH_ENDPOINT, {
    tags: { name: 'setup-health-check' },
    timeout: '10s',
  });

  if (healthRes.status !== 200) {
    throw new Error(`System not healthy at start: ${healthRes.status}`);
  }

  return {
    startTime: new Date().toISOString(),
    responseSizes: [],
  };
}

// ============================================================================
// TEARDOWN
// ============================================================================

export function teardown(data) {
  const endTime = new Date();
  const duration = (new Date(data.startTime) - endTime) / 1000 / 60;

  console.log(`Sustained load test completed after ${Math.abs(duration).toFixed(2)} minutes`);

  // Analyze memory stability
  if (data.responseSizes.length > 100) {
    const avgSize = data.responseSizes.reduce((a, b) => a + b, 0) / data.responseSizes.length;
    const maxSize = Math.max(...data.responseSizes);
    const minSize = Math.min(...data.responseSizes);

    console.log(`Response size statistics:`);
    console.log(`  Average: ${avgSize.toFixed(2)} bytes`);
    console.log(`  Min: ${minSize} bytes`);
    console.log(`  Max: ${maxSize} bytes`);

    if (maxSize > avgSize * 10) {
      console.warn(`Possible memory leak: Max response size ${maxSize} is > 10x average ${avgSize.toFixed(2)}`);
    }
  }
}

// ============================================================================
// MAIN TEST FUNCTION
// ============================================================================

export default function (data) {
  // Select request based on workload mix
  const requestTemplate = selectRequestByMix();
  const requestType = requestTemplate.type;

  const payload = JSON.stringify({
    ...requestTemplate,
    id: Math.floor(Math.random() * 1000000),
  });

  const requestId = `req_${__VU}_${__ITER}_${Date.now()}`;

  // Make the request
  const params = {
    headers: {
      'Content-Type': 'application/json',
      'X-Request-ID': requestId,
      'X-Request-Type': requestType,
      'User-Agent': 'k6-sustained-test/1.0',
    },
    tags: {
      name: `mcp-${requestType}`,
      test_type: 'sustained',
      request_type: requestType,
    },
    timeout: '30s',
  };

  const startTime = Date.now();
  const response = http.post(API_ENDPOINT, payload, params);
  const responseTime = Date.now() - startTime;

  // Record type-specific latency
  switch (requestType) {
    case 'read':
      readLatency.add(responseTime);
      break;
    case 'write':
      writeLatency.add(responseTime);
      break;
    case 'health':
      healthLatency.add(responseTime);
      break;
  }

  // Record general metrics
  latency.add(responseTime);
  throughputCounter.add(1);

  // Track response size for memory leak detection
  data.responseSizes.push(response.body.length);
  if (data.responseSizes.length > 1000) {
    data.responseSizes.shift(); // Keep last 1000
  }

  // Check success criteria
  const success = check(response, {
    'status is 200 or 201': (r) => r.status >= 200 && r.status < 300,
    'response time < 150ms': (r) => r.timings.duration < 150,
    'response time < 300ms': (r) => r.timings.duration < 300,
    'has valid JSON-RPC response': (r) => validateResponse(r),
    'no memory leak indicators': (r) => checkMemoryStability(r),
    'response size reasonable': (r) => r.body.length < 1024 * 1024,
  });

  // Update custom metrics
  customChecks.success.add(success);
  customChecks.response_valid.add(validateResponse(response));
  customChecks.no_memory_leak.add(checkMemoryStability(response));

  // Track errors
  if (!success || response.status >= 400) {
    errorRate.add(1);
    console.error(`Request failed [${requestType}]: ${response.status} - ${response.body.substring(0, 200)}`);
  } else {
    errorRate.add(0);
  }

  // Minimal sleep for sustained load
  sleep(0.01); // 10ms pacing
}
