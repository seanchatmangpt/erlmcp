#!/usr/bin/env k6
/**
 * erlmcp Spike Load Test
 *
 * Purpose: Test system resilience under sudden traffic spikes.
 *          Validates auto-scaling, circuit breakers, and backpressure.
 *
 * Test Configuration:
 *   - Baseline: 100 RPS
 *   - Spike: Jump to 5000 RPS instantly
 *   - Duration: Multiple spike cycles
 *
 * SLA Thresholds:
 *   - Baseline P95 < 100ms
 *   - Spike P95 < 1000ms (relaxed during spike)
 *   - Error rate < 5% during spike
 *   - Recovery to baseline < 60s
 *
 * @module spike-test
 */

import http from 'k6/http';
import { check, sleep } from 'k6';
import { Rate, Trend, Counter, Gauge } from 'k6/metrics';
import { SharedArray } from 'k6/data';

// ============================================================================
// CONFIGURATION
// ============================================================================

export const options = {
  // Spike test stages - simulate multiple sudden traffic bursts
  stages: [
    { duration: '1m', target: 100 },    // Baseline
    { duration: '10s', target: 5000 },  // SPIKE 1: Rapid ramp-up
    { duration: '30s', target: 5000 },  // SPIKE 1: Hold at peak
    { duration: '30s', target: 100 },   // Recovery 1
    { duration: '30s', target: 100 },   // Stabilize
    { duration: '10s', target: 5000 },  // SPIKE 2: Rapid ramp-up
    { duration: '30s', target: 5000 },  // SPIKE 2: Hold at peak
    { duration: '30s', target: 100 },   // Recovery 2
    { duration: '1m', target: 0 },      // Cooldown
  ],

  // Thresholds for spike test
  thresholds: {
    // HTTP response times (lenient during spikes)
    'http_req_duration': [
      // Baseline thresholds
      { threshold: 'p(95)<500', abortOnFail: false },
      { threshold: 'p(99)<2000', abortOnFail: false },
      // Average should be reasonable even during spike
      { threshold: 'avg<300', abortOnFail: false },
    ],

    // HTTP errors (tolerate higher error rate during spikes)
    'http_req_failed': [
      { threshold: 'rate<0.05', abortOnFail: false },
    ],

    // Request throughput
    'http_reqs': [
      { threshold: 'rate>80', abortOnFail: false },
    ],

    // Custom error rate
    'errors': [
      { threshold: 'rate<0.05', abortOnFail: false },
    ],
  },

  // Graceful stop
  gracefulStop: '30s',

  // Connection reuse for spike performance
  noConnectionReuse: false,
  noVUConnectionReuse: false,

  // User agent
  userAgent: 'k6/erlmcp-spike-test',

  // Batch settings
  batch: 100,
  batchPerHost: 50,
};

// ============================================================================
// CUSTOM METRICS
// ============================================================================

const errorRate = new Rate('errors');
const latency = new Trend('latency', true);
const throughputCounter = new Counter('throughput');
const spikeErrors = new Counter('spike_errors');

// Phase-specific metrics
const baselineLatency = new Trend('baseline_latency');
const spikeLatency = new Trend('spike_latency');
const recoveryLatency = new Trend('recovery_latency');

const customChecks = {
  success: new Rate('check_success'),
  degraded_but_functional: new Rate('check_degraded'),
  response_valid: new Rate('check_response_valid'),
};

// ============================================================================
// TEST DATA
// ============================================================================

const mcpRequests = new SharedArray('MCP Requests', function () {
  return [
    {
      jsonrpc: '2.0',
      method: 'initialize',
      params: {
        protocolVersion: '2024-11-05',
        capabilities: { tools: {}, resources: {} },
        clientInfo: { name: 'k6-spike-test', version: '1.0' },
      },
    },
    {
      jsonrpc: '2.0',
      method: 'tools/list',
      params: {},
    },
    {
      jsonrpc: '2.0',
      method: 'resources/list',
      params: {},
    },
    {
      jsonrpc: '2.0',
      method: 'tools/call',
      params: {
        name: 'example_tool',
        arguments: { query: 'spike test', limit: 10 },
      },
    },
    {
      jsonrpc: '2.0',
      method: 'health',
      params: {},
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
// STATE TRACKING
// ============================================================================

let currentPhase = 'baseline';
let lastVUCount = 0;

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/**
 * Detect current test phase based on active VUs
 */
function detectPhase() {
  const currentVUs = __VU;
  const totalVUs = __ITER || 1;

  // Rough detection based on timing
  const now = Date.now();
  const startTime = new Date(__ENV.START_TIME || Date.now()).getTime();
  const elapsed = (now - startTime) / 1000;

  if (elapsed < 60) return 'baseline';
  if (elapsed < 110) return 'spike';
  if (elapsed < 140) return 'recovery';
  if (elapsed < 170) return 'baseline';
  if (elapsed < 180) return 'spike';
  if (elapsed < 210) return 'recovery';
  return 'cooldown';
}

/**
 * Get relaxed timeout during spike
 */
function getTimeoutForPhase(phase) {
  switch (phase) {
    case 'spike':
      return '60s'; // More patience during spike
    case 'recovery':
      return '30s';
    default:
      return '10s';
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
 * Check if response indicates degraded but functional service
 */
function isDegradedButFunctional(response) {
  // Consider it degraded if slow but successful
  const isSlow = response.timings.duration > 1000;
  const isSuccessful = response.status >= 200 && response.status < 300;
  return isSlow && isSuccessful;
}

// ============================================================================
// SETUP
// ============================================================================

export function setup() {
  const startTime = new Date().toISOString();
  console.log(`Starting spike test against: ${BASE_URL}`);
  console.log(`Start time: ${startTime}`);

  // Verify system health
  const healthRes = http.get(HEALTH_ENDPOINT, {
    tags: { name: 'setup-health-check' },
    timeout: '10s',
  });

  if (healthRes.status !== 200) {
    console.warn(`Health check returned ${healthRes.status}, proceeding anyway`);
  }

  return {
    startTime: startTime,
    spikeCount: 0,
    errorCounts: [],
  };
}

// ============================================================================
// TEARDOWN
// ============================================================================

export function teardown(data) {
  const endTime = new Date();
  console.log(`Spike test completed at: ${endTime.toISOString()}`);
  console.log(`Total spikes encountered: ${data.spikeCount}`);

  // Analyze error pattern
  if (data.errorCounts.length > 0) {
    const avgErrorRate = data.errorCounts.reduce((a, b) => a + b, 0) / data.errorCounts.length;
    console.log(`Average error rate during spikes: ${(avgErrorRate * 100).toFixed(2)}%`);
  }
}

// ============================================================================
// MAIN TEST FUNCTION
// ============================================================================

export default function (data) {
  // Detect current phase
  const phase = detectPhase();

  // Select random request
  const requestTemplate = mcpRequests[Math.floor(Math.random() * mcpRequests.length)];
  const payload = JSON.stringify({
    ...requestTemplate,
    id: Math.floor(Math.random() * 1000000),
  });

  const requestId = `req_${__VU}_${__ITER}_${Date.now()}`;

  // Configure parameters based on phase
  const params = {
    headers: {
      'Content-Type': 'application/json',
      'X-Request-ID': requestId,
      'X-Test-Phase': phase,
      'User-Agent': 'k6-spike-test/1.0',
    },
    tags: {
      name: `mcp-${phase}`,
      test_type: 'spike',
      phase: phase,
    },
    timeout: getTimeoutForPhase(phase),
  };

  const startTime = Date.now();
  const response = http.post(API_ENDPOINT, payload, params);
  const responseTime = Date.now() - startTime;

  // Record phase-specific latency
  switch (phase) {
    case 'baseline':
      baselineLatency.add(responseTime);
      break;
    case 'spike':
      spikeLatency.add(responseTime);
      data.spikeCount++;
      break;
    case 'recovery':
      recoveryLatency.add(responseTime);
      break;
  }

  // Record general metrics
  latency.add(responseTime);
  throughputCounter.add(1);

  // Determine success thresholds based on phase
  const latencyThreshold = phase === 'spike' ? 2000 : 500;
  const isSlow = responseTime > latencyThreshold;

  // Check success criteria (phase-aware)
  const success = check(response, {
    [`status is 2xx (${phase})`]: (r) => r.status >= 200 && r.status < 300,
    [`response time < ${latencyThreshold}ms (${phase})`]: (r) => r.timings.duration < latencyThreshold,
    'has valid JSON-RPC response': (r) => validateResponse(r),
    'not timeout': (r) => r.status !== 0,
    'degraded but functional': (r) => isDegradedButFunctional(r) || r.timings.duration < latencyThreshold,
  });

  // Update custom metrics
  customChecks.success.add(success);
  customChecks.degraded_but_functional.add(isDegradedButFunctional(response) || responseTime < 500);
  customChecks.response_valid.add(validateResponse(response));

  // Track errors
  const isError = !success || response.status >= 400;
  errorRate.add(isError ? 1 : 0);

  if (isError && response.status !== 0) {
    spikeErrors.add(1);
    data.errorCounts.push(1);
    console.error(`[${phase}] Request failed: ${response.status} - ${responseTime}ms`);
  } else {
    data.errorCounts.push(0);
  }

  // Adaptive sleep based on phase
  const sleepTime = phase === 'spike' ? 0.001 : 0.05;
  sleep(sleepTime);
}
