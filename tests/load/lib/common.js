/**
 * erlmcp Load Testing Library
 *
 * Common utilities and helpers for k6 load tests.
 *
 * @module common
 */

/**
 * Default configuration for erlmcp load tests
 */
export const DEFAULT_CONFIG = {
  baseURL: 'http://localhost:8080',
  apiEndpoint: '/mcp',
  healthEndpoint: '/health',
  timeout: '10s',
  headers: {
    'Content-Type': 'application/json',
    'User-Agent': 'k6-erlmcp-test',
  },
};

/**
 * Standard JSON-RPC request templates
 */
export const REQUEST_TEMPLATES = {
  initialize: {
    jsonrpc: '2.0',
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

  tools_list: {
    jsonrpc: '2.0',
    method: 'tools/list',
    params: {},
  },

  resources_list: {
    jsonrpc: '2.0',
    method: 'resources/list',
    params: {},
  },

  prompts_list: {
    jsonrpc: '2.0',
    method: 'prompts/list',
    params: {},
  },

  tool_call: {
    jsonrpc: '2.0',
    method: 'tools/call',
    params: {
      name: 'example_tool',
      arguments: {},
    },
  },

  resource_read: {
    jsonrpc: '2.0',
    method: 'resources/read',
    params: {
      uri: 'example://data/test',
    },
  },

  health: {
    jsonrpc: '2.0',
    method: 'health',
    params: {},
  },
};

/**
 * Validate a JSON-RPC response
 *
 * @param {Response} response - The k6 HTTP response object
 * @returns {boolean} True if response is valid JSON-RPC
 */
export function validateJsonRpc(response) {
  try {
    const body = JSON.parse(response.body);

    // Must have jsonrpc version
    if (body.jsonrpc !== '2.0') {
      return false;
    }

    // Must have either result or error
    if (body.result === undefined && body.error === undefined) {
      return false;
    }

    // If error exists, validate structure
    if (body.error !== undefined) {
      if (body.error.code === undefined || body.error.message === undefined) {
        return false;
      }
    }

    return true;
  } catch (e) {
    return false;
  }
}

/**
 * Create a JSON-RPC request with a unique ID
 *
 * @param {Object} template - The request template
 * @param {number} id - The unique request ID
 * @returns {string} JSON stringified request
 */
export function createRequest(template, id) {
  return JSON.stringify({
    ...template,
    id: id,
  });
}

/**
 * Execute a request with retry logic
 *
 * @param {string} url - The target URL
 * @param {string} payload - JSON request payload
 * @param {Object} params - k6 HTTP params
 * @param {Object} retryConfig - Retry configuration
 * @returns {Object} Result with response and success status
 */
export function executeWithRetry(url, payload, params, retryConfig) {
  const maxRetries = retryConfig.maxRetries || 3;
  const backoff = retryConfig.backoff || 500;
  const backoffMultiplier = retryConfig.backoffMultiplier || 2;

  let lastResponse = null;
  let attempt = 0;

  while (attempt <= maxRetries) {
    attempt++;

    const response = http.post(url, payload, {
      ...params,
      tags: {
        ...params.tags,
        attempt: attempt.toString(),
      },
    });

    // Success on 2xx status
    if (response.status >= 200 && response.status < 300) {
      return {
        response: response,
        success: true,
        attempts: attempt,
      };
    }

    // Don't retry client errors (4xx)
    if (response.status >= 400 && response.status < 500) {
      return {
        response: response,
        success: false,
        attempts: attempt,
        shouldRetry: false,
      };
    }

    lastResponse = response;

    // Backoff before retry
    if (attempt <= maxRetries) {
      const sleepTime = (backoff * Math.pow(backoffMultiplier, attempt - 1)) / 1000;
      sleep(sleepTime);
    }
  }

  return {
    response: lastResponse,
    success: false,
    attempts: attempt,
    shouldRetry: true,
  };
}

/**
 * Calculate throughput metrics
 *
 * @param {Array} measurements - Array of timing measurements
 * @returns {Object} Throughput statistics
 */
export function calculateThroughput(measurements) {
  if (!measurements || measurements.length === 0) {
    return {
      avg: 0,
      min: 0,
      max: 0,
      p50: 0,
      p95: 0,
      p99: 0,
    };
  }

  const sorted = [...measurements].sort((a, b) => a - b);
  const len = sorted.length;

  return {
    avg: measurements.reduce((a, b) => a + b, 0) / measurements.length,
    min: sorted[0],
    max: sorted[len - 1],
    p50: sorted[Math.floor(len * 0.5)],
    p95: sorted[Math.floor(len * 0.95)],
    p99: sorted[Math.floor(len * 0.99)],
  };
}

/**
 * Create a unique request ID
 *
 * @param {number} vu - Virtual user number
 * @param {number} iter - Iteration number
 * @returns {string} Unique request ID
 */
export function createRequestId(vu, iter) {
  return `req_${vu}_${iter}_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
}

/**
 * Detect test phase based on elapsed time
 *
 * @param {number} startTime - Test start timestamp
 * @param {Array} stages - Test stage definitions
 * @returns {string} Current phase name
 */
export function detectPhase(startTime, stages) {
  const now = Date.now();
  const elapsed = (now - startTime) / 1000;

  let timeSoFar = 0;
  for (const stage of stages) {
    const duration = parseDuration(stage.duration);
    timeSoFar += duration;

    if (elapsed < timeSoFar) {
      return stage.name || 'unknown';
    }
  }

  return 'complete';
}

/**
 * Parse duration string to seconds
 *
 * @param {string} duration - Duration string (e.g., '1m', '30s', '2h')
 * @returns {number} Duration in seconds
 */
export function parseDuration(duration) {
  const match = duration.match(/^(\d+)([smh])$/);
  if (!match) {
    return parseInt(duration, 10) || 0;
  }

  const value = parseInt(match[1], 10);
  const unit = match[2];

  switch (unit) {
    case 's':
      return value;
    case 'm':
      return value * 60;
    case 'h':
      return value * 3600;
    default:
      return value;
  }
}

/**
 * Export metrics to Prometheus format
 *
 * @param {Object} metrics - k6 metrics object
 * @param {string} testName - Name of the test
 * @returns {string} Prometheus-formatted metrics
 */
export function exportPrometheusMetrics(metrics, testName) {
  const lines = [];
  const timestamp = Date.now() * 1000; // Prometheus expects milliseconds

  for (const [key, value] of Object.entries(metrics)) {
    lines.push(`# HELP erlmcp_${key} ${key} metric`);
    lines.push(`# TYPE erlmcp_${key} gauge`);
    lines.push(`erlmcp_${key}{test="${testName}"} ${value} ${timestamp}`);
  }

  return lines.join('\n');
}

/**
 * Log test summary
 *
 * @param {Object} data - Test data object
 * @param {Object} metrics - Custom metrics
 */
export function logSummary(data, metrics) {
  console.log('=== Test Summary ===');
  console.log(`Total Requests: ${data.totalRequests || 0}`);
  console.log(`Successful: ${data.successfulRequests || 0}`);
  console.log(`Failed: ${data.failedRequests || 0}`);
  console.log(`Success Rate: ${((data.successfulRequests / data.totalRequests) * 100 || 0).toFixed(2)}%`);

  if (metrics) {
    console.log(`Average Latency: ${metrics.latency?.avg || 0}ms`);
    console.log(`P95 Latency: ${metrics.latency?.p95 || 0}ms`);
    console.log(`P99 Latency: ${metrics.latency?.p99 || 0}ms`);
  }

  console.log('==================');
}
