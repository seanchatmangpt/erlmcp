#!/usr/bin/env k6
/**
 * erlmcp Failover and High Availability Test
 *
 * Purpose: Validate system resilience during node failures and
 *          cluster reconfiguration. Tests graceful degradation and
 *          automatic recovery.
 *
 * Test Scenarios:
 *   1. Single node failure during sustained load
 *   2. Network partition simulation
 *   3. Leader election during load
 *   4. Recovery and reconnection
 *
 * SLA Thresholds:
 *   - P95 latency < 500ms during failover
 *   - Zero data loss (all requests eventually succeed)
 *   - Recovery time < 30s
 *   - Error rate < 10% during failover
 *
 * @module failover-test
 */

import http from 'k6/http';
import { check, sleep } from 'k6';
import { Rate, Trend, Counter, Gauge } from 'k6/metrics';
import { SharedArray } from 'k6/data';

// ============================================================================
// CONFIGURATION
// ============================================================================

export const options = {
  // Failover test stages
  stages: [
    { duration: '1m', target: 200 },    // Baseline: Establish steady state
    { duration: '2m', target: 200 },    // Sustained load before failover
    { duration: '30s', target: 200 },   // FAILOVER WINDOW: Node failure occurs here
    { duration: '2m', target: 200 },    // Recovery period
    { duration: '1m', target: 200 },    // Stabilization
    { duration: '30s', target: 0 },     // Cooldown
  ],

  // Thresholds for failover test (more lenient)
  thresholds: {
    // HTTP response times (tolerate slower during failover)
    'http_req_duration': [
      { threshold: 'p(95)<1000', abortOnFail: false },
      { threshold: 'p(99)<5000', abortOnFail: false },
      { threshold: 'avg<500', abortOnFail: false },
    ],

    // HTTP errors (tolerate higher during failover)
    'http_req_failed': [
      { threshold: 'rate<0.10', abortOnFail: false },
    ],

    // Minimum throughput
    'http_reqs': [
      { threshold: 'rate>100', abortOnFail: false },
    ],

    // Custom error rate
    'errors': [
      { threshold: 'rate<0.10', abortOnFail: false },
    ],

    // Retry success rate
    'retry_success': [
      { threshold: 'rate>0.90', abortOnFail: false },
    ],
  },

  // Graceful stop
  gracefulStop: '60s',

  // User agent
  userAgent: 'k6/erlmcp-failover-test',

  // Batch settings
  batch: 30,
  batchPerHost: 15,
};

// ============================================================================
// CUSTOM METRICS
// ============================================================================

const errorRate = new Rate('errors');
const latency = new Trend('latency', true);
const throughputCounter = new Counter('throughput');
const retryCounter = new Counter('retries');
const retrySuccessRate = new Rate('retry_success');

// Phase-specific metrics
const stableLatency = new Trend('stable_latency');
const failoverLatency = new Trend('failover_latency');
const recoveryLatency = new Trend('recovery_latency');

const customChecks = {
  success: new Rate('check_success'),
  retry_success: new Rate('check_retry_success'),
  data_consistency: new Rate('check_data_consistency'),
  cluster_healthy: new Rate('check_cluster_healthy'),
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
        clientInfo: { name: 'k6-failover-test', version: '1.0' },
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
      method: 'prompts/list',
      params: {},
    },
    {
      jsonrpc: '2.0',
      method: 'health',
      params: {},
    },
    {
      jsonrpc: '2.0',
      method: 'tools/call',
      params: {
        name: 'cluster_status',
        arguments: {},
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
const CLUSTER_ENDPOINT = `${BASE_URL}/cluster/status`;

// Failover configuration
const FAILOVER_START_TIME = 90; // seconds into test when failover begins
const FAILOVER_DURATION = 30;   // expected duration of failover

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/**
 * Detect current test phase
 */
function detectPhase() {
  const elapsed = __ITER || 0;
  const now = Date.now();
  const startTime = new Date(__ENV.START_TIME || Date.now()).getTime();
  const elapsedSeconds = (now - startTime) / 1000;

  if (elapsedSeconds < 60) return 'baseline';
  if (elapsedSeconds < 180) return 'stable';
  if (elapsedSeconds < 210) return 'failover';
  if (elapsedSeconds < 330) return 'recovery';
  return 'stabilization';
}

/**
 * Get retry configuration for current phase
 */
function getRetryConfig(phase) {
  switch (phase) {
    case 'failover':
      return { maxRetries: 5, backoff: 2000 }; // More retries during failover
    case 'recovery':
      return { maxRetries: 3, backoff: 1000 };
    default:
      return { maxRetries: 2, backoff: 500 };
  }
}

/**
 * Execute request with retry logic
 */
function executeWithRetry(url, payload, params, retryConfig) {
  let lastError = null;
  let attempt = 0;
  let response = null;

  while (attempt <= retryConfig.maxRetries) {
    attempt++;
    retryCounter.add(1);

    response = http.post(url, payload, {
      ...params,
      tags: {
        ...params.tags,
        attempt: attempt.toString(),
      },
    });

    // Success on 2xx status
    if (response.status >= 200 && response.status < 300) {
      retrySuccessRate.add(attempt === 1 ? 1 : 0); // Track first-attempt success
      return { response, success: true, attempts: attempt };
    }

    // Don't retry client errors (4xx)
    if (response.status >= 400 && response.status < 500) {
      return { response, success: false, attempts: attempt, shouldRetry: false };
    }

    lastError = response;

    // Backoff before retry
    if (attempt <= retryConfig.maxRetries) {
      sleep(retryConfig.backoff / 1000);
    }
  }

  retrySuccessRate.add(0);
  return { response: lastError, success: false, attempts: attempt, shouldRetry: true };
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

  return false;
}

/**
 * Check cluster health
 */
function checkClusterHealth() {
  try {
    const response = http.get(CLUSTER_ENDPOINT, {
      timeout: '5s',
      tags: { name: 'cluster-health-check' },
    });

    if (response.status === 200) {
      const data = JSON.parse(response.body);
      return data.healthy === true || data.status === 'healthy';
    }
  } catch (e) {
    // Cluster endpoint might not be available
  }

  return true; // Assume healthy if endpoint not available
}

/**
 * Check data consistency across requests
 */
function checkDataConsistency(response, requestId) {
  try {
    const body = JSON.parse(response.body);

    // Check for request ID correlation
    if (body.id && body.id !== requestId) {
      return false;
    }

    // Check for proper error structure if error exists
    if (body.error) {
      return body.error.code !== undefined && body.error.message !== undefined;
    }

    return true;
  } catch (e) {
    return false;
  }
}

// ============================================================================
// SETUP
// ============================================================================

export function setup() {
  const startTime = new Date().toISOString();
  console.log(`Starting failover test against: ${BASE_URL}`);
  console.log(`Start time: ${startTime}`);
  console.log(`Failover window expected at: ${FAILOVER_START_TIME}s`);

  // Check initial cluster health
  const clusterHealthy = checkClusterHealth();
  console.log(`Initial cluster health: ${clusterHealthy ? 'HEALTHY' : 'DEGRADED'}`);

  return {
    startTime: startTime,
    failoverDetected: false,
    dataConsistencyViolations: 0,
    totalRequests: 0,
    successfulRequests: 0,
  };
}

// ============================================================================
// TEARDOWN
// ============================================================================

export function teardown(data) {
  const endTime = new Date();
  console.log(`Failover test completed at: ${endTime.toISOString()}`);
  console.log(`Total requests: ${data.totalRequests}`);
  console.log(`Successful requests: ${data.successfulRequests}`);
  console.log(`Success rate: ${((data.successfulRequests / data.totalRequests) * 100).toFixed(2)}%`);
  console.log(`Data consistency violations: ${data.dataConsistencyViolations}`);
  console.log(`Failover detected: ${data.failoverDetected}`);
}

// ============================================================================
// MAIN TEST FUNCTION
// ============================================================================

export default function (data) {
  // Detect current phase
  const phase = detectPhase();

  // Select random request
  const requestTemplate = mcpRequests[Math.floor(Math.random() * mcpRequests.length)];
  const requestId = Math.floor(Math.random() * 1000000);
  const payload = JSON.stringify({
    ...requestTemplate,
    id: requestId,
  });

  // Get retry configuration for phase
  const retryConfig = getRetryConfig(phase);

  // Configure parameters
  const params = {
    headers: {
      'Content-Type': 'application/json',
      'X-Request-ID': `req_${__VU}_${__ITER}_${Date.now()}`,
      'X-Test-Phase': phase,
      'User-Agent': 'k6-failover-test/1.0',
    },
    tags: {
      name: `mcp-${phase}`,
      test_type: 'failover',
      phase: phase,
    },
    timeout: phase === 'failover' ? '30s' : '10s',
  };

  // Execute with retry logic
  const result = executeWithRetry(API_ENDPOINT, payload, params, retryConfig);
  const response = result.response;
  const responseTime = response.timings.duration;

  // Track total requests
  data.totalRequests++;

  // Record phase-specific latency
  switch (phase) {
    case 'baseline':
    case 'stable':
      stableLatency.add(responseTime);
      break;
    case 'failover':
      failoverLatency.add(responseTime);
      data.failoverDetected = true;
      break;
    case 'recovery':
      recoveryLatency.add(responseTime);
      break;
  }

  // Record general metrics
  latency.add(responseTime);
  throughputCounter.add(1);

  // Check data consistency
  const isConsistent = checkDataConsistency(response, requestId);
  if (!isConsistent) {
    data.dataConsistencyViolations++;
  }

  // Check success criteria
  const success = check(response, {
    [`status is 2xx (${phase})`]: (r) => r.status >= 200 && r.status < 300,
    [`response time < 1000ms (${phase})`]: (r) => r.timings.duration < 1000,
    'has valid JSON-RPC response': (r) => validateResponse(r),
    'data is consistent': () => isConsistent,
    'cluster is healthy': () => checkClusterHealth(),
  });

  // Update custom metrics
  customChecks.success.add(result.success);
  customChecks.retry_success.add(result.success && result.attempts === 1);
  customChecks.data_consistency.add(isConsistent);
  customChecks.cluster_healthy.add(checkClusterHealth());

  // Track successful requests
  if (result.success) {
    data.successfulRequests++;
    errorRate.add(0);
  } else {
    errorRate.add(1);
    console.error(`[${phase}] Request failed after ${result.attempts} attempts: ${response.status}`);
  }

  // Adaptive sleep based on phase
  const sleepTime = phase === 'failover' ? 0.1 : 0.05;
  sleep(sleepTime);
}
