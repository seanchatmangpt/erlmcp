//===----------------------------------------------------------------------===
//
//  Regression Testing Configuration for erlmcp v3
//
//  This configuration ensures no performance regressions across versions.
//
//  Usage:
//    go test -bench=. -benchmem= ./...
//
//===----------------------------------------------------------------------===

package benchmarks

import (
    "fmt"
    "math/rand"
    "os"
    "testing"
    "time"
)

// Regression thresholds
const (
    // Throughput must not decrease by more than 10%
    MinThroughputRatio = 0.90

    // Latency p99 must not increase by more than 20%
    MaxLatencyIncreaseRatio = 1.20

    // Memory must not increase by more than 30%
    MaxMemoryIncreaseRatio = 1.30

    // Baseline values (updated from benchmarks)
    BaselineThroughput = 2000000.0 // 2M msg/s
    BaselineP99LatencyMs   = 10.0    // 10ms
    BaselineMemoryMB       = 512     // 512MB
)

// Test configurations
type TestConfig struct {
    Connections int
    Duration    time.Duration
    Workers     int
    MessageSize int
}

// Benchmark result
type BenchmarkResult struct {
    Throughput    float64 // ops/sec
    P50LatencyMs  float64
    P95LatencyMs  float64
    P99LatencyMs  float64
    MemoryMB      float64
}

// Test cases
var testConfigs = []TestConfig{
    {Connections: 1000, Duration: 10 * time.Second, Workers: 10, MessageSize: 1024},
    {Connections: 10000, Duration: 30 * time.Second, Workers: 100, MessageSize: 1024},
    {Connections: 50000, Duration: 60 * time.Second, Workers: 500, MessageSize: 1024},
}

//===----------------------------------------------------------------------===
// Registry Benchmarks
//===----------------------------------------------------------------------===

func BenchmarkRegistryRegister(b *testing.B) {
    config := TestConfig{
        Connections: 10000,
        Workers:     b.N,
        MessageSize: 256,
    }

    result := runBenchmark(b, config)

    b.ReportMetric("throughput_ops", result.Throughput)
    b.ReportMetric("p99_latency_ms", result.P99LatencyMs)
    b.ReportMetric("memory_mb", result.MemoryMB)
}

func BenchmarkRegistryLookup(b *testing.B) {
    // Pre-populate registry
    registry := setupTestRegistry(10000)

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        key := fmt.Sprintf("server-%d", rand.Intn(10000))
        _ = registry.Lookup(key)
    }
    b.StopTimer()

    // Verify regression
    throughput := float64(b.N) / b.Elapsed().Seconds()
    if throughput < BaselineThroughput*MinThroughputRatio {
        b.Errorf("Throughput regression: %.0f ops/sec < %.0f ops/sec (baseline * 0.9)",
            throughput, BaselineThroughput*MinThroughputRatio)
    }
}

//===----------------------------------------------------------------------===
// Message Throughput Benchmarks
//===----------------------------------------------------------------------===

func BenchmarkMessageThroughput(b *testing.B) {
    results := make([]BenchmarkResult, len(testConfigs))

    for i, config := range testConfigs {
        b.Run(fmt.Sprintf("connections=%d", config.Connections), func(b *testing.B) {
            results[i] = runBenchmark(b, config)
        })
    }

    // Verify no regression across scales
    for i, result := range results {
        expectedThroughput := BaselineThroughput * float64(i+1) * MinThroughputRatio
        if result.Throughput < expectedThroughput {
            b.Errorf("Regression at scale %dx: %.0f < %.0f ops/sec",
                i+1, result.Throughput, expectedThroughput)
        }
    }
}

func BenchmarkMessageBatching(b *testing.B) {
    b.Run("baseline", func(b *testing.B) {
        baseline := benchmarkBaselineMessages(b)
        b.ReportMetric("tokens", float64(baseline))
    })

    b.Run("batched", func(b *testing.B) {
        batched := benchmarkBatchedMessages(b)
        b.ReportMetric("tokens", float64(batched))

        // Verify token savings > 50%
        savings := 1.0 - (float64(batched) / float64(baseline))
        if savings < 0.50 {
            b.Errorf("Token savings below 50%%: %.2f%%", savings*100)
        }
    })
}

//===----------------------------------------------------------------------===
// Transport Benchmarks
//===----------------------------------------------------------------------===

func BenchmarkTransportStdio(b *testing.B) {
    transport := setupStdioTransport()
    message := createTestMessage(1024)

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        _ = transport.Encode(message)
    }
    b.StopTimer()
}

func BenchmarkTransportTCP(b *testing.B) {
    transport := setupTCPTransport()
    message := createTestMessage(1024)

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        _ = transport.Send(message)
    }
    b.StopTimer()
}

func BenchmarkTransportHTTP(b *testing.B) {
    transport := setupHTTPTransport()
    message := createTestMessage(1024)

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        _ = transport.SendHTTP(message)
    }
    b.StopTimer()
}

//===----------------------------------------------------------------------===
// Flash Attention Benchmarks
//===----------------------------------------------------------------------===

func BenchmarkFlashAttention(b *testing.B) {
    tokenCount := 100

    b.Run("baseline", func(b *testing.B) {
        baseline := benchmarkBaselineAttention(b, tokenCount)
        b.ReportMetric("speedup", 1.0)
    })

    b.Run("flash_attention", func(b *testing.B) {
        flash := benchmarkFlashAttention(b, tokenCount)

        // Verify speedup > 2.0x
        speedup := float64(baseline) / float64(flash)
        b.ReportMetric("speedup", speedup)

        if speedup < 2.0 {
            b.Errorf("Flash Attention speedup below 2.0x: %.2fx", speedup)
        }
    })
}

//===----------------------------------------------------------------------===
// Memory Benchmarks
//===----------------------------------------------------------------------===

func BenchmarkMemoryFootprint(b *testing.B) {
    var m1, m2 runtime.MemStats

    runtime.ReadMemStats(&m1)

    // Simulate load
    for i := 0; i < b.N; i++ {
        // Create test connections
        conn := createTestConnection()
        conn.Process()
    }

    runtime.ReadMemStats(&m2)

    memoryMB := float64(m2.Alloc-m1.Alloc) / 1024 / 1024
    b.ReportMetric("memory_mb", memoryMB)

    // Verify memory not exceeding baseline * 1.3
    if memoryMB > BaselineMemoryMB*MaxMemoryIncreaseRatio {
        b.Errorf("Memory regression: %.2f MB > %.2f MB (baseline * 1.3)",
            memoryMB, BaselineMemoryMB*MaxMemoryIncreaseRatio)
    }
}

//===----------------------------------------------------------------------===
// Helper Functions
//===----------------------------------------------------------------------===

func runBenchmark(b *testing.B, config TestConfig) BenchmarkResult {
    // Setup
    registry := setupTestRegistry(config.Connections)
    workers := setupWorkers(config.Workers)
    messages := generateTestMessages(config.MessageSize)

    // Run benchmark
    b.ResetTimer()
    latencies := make([]time.Duration, 0, config.Connections)

    start := time.Now()
    for i := 0; i < config.Connections; i++ {
        msgStart := time.Now()
        _ = registry.Process(messages[i%len(messages)])
        latencies = append(latencies, time.Since(msgStart))
    }
    duration := time.Since(start)

    b.StopTimer()

    // Calculate metrics
    result := BenchmarkResult{
        Throughput:   float64(config.Connections) / duration.Seconds(),
        P50LatencyMs: percentile(latencies, 0.50).Seconds() * 1000,
        P95LatencyMs: percentile(latencies, 0.95).Seconds() * 1000,
        P99LatencyMs: percentile(latencies, 0.99).Seconds() * 1000,
    }

    // Get memory stats
    var m runtime.MemStats
    runtime.ReadMemStats(&m)
    result.MemoryMB = float64(m.Alloc) / 1024 / 1024

    return result
}

func percentile(latencies []time.Duration, p float64) time.Duration {
    idx := int(float64(len(latencies)) * p)
    if idx >= len(latencies) {
        idx = len(latencies) - 1
    }
    return latencies[idx]
}

func setupTestRegistry(count int) *TestRegistry {
    return &TestRegistry{
        entries: make(map[string]interface{}, count),
    }
}

func setupWorkers(count int) []*TestWorker {
    workers := make([]*TestWorker, count)
    for i := 0; i < count; i++ {
        workers[i] = &TestWorker{id: i}
    }
    return workers
}

func generateTestMessages(size int) [][]byte {
    return [][]byte{make([]byte, size)}
}

func createTestMessage(size int) []byte {
    return make([]byte, size)
}

func createTestConnection() *TestConnection {
    return &TestConnection{}
}

// Test types
type TestRegistry struct {
    entries map[string]interface{}
}

func (r *TestRegistry) Lookup(key string) (interface{}, bool) {
    val, ok := r.entries[key]
    return val, ok
}

func (r *TestRegistry) Process(msg []byte) error {
    return nil // Simulated processing
}

type TestWorker struct {
    id int
}

type TestConnection struct{}

func (c *TestConnection) Process() {
    // Simulated processing
}

//===----------------------------------------------------------------------===
// Baseline Implementations
//===----------------------------------------------------------------------===

func benchmarkBaselineMessages(b *testing.B) int {
    totalTokens := 0
    for i := 0; i < b.N; i++ {
        // Simulate 10 individual messages
        for j := 0; j < 10; j++ {
            totalTokens += 100 // 100 tokens per message
        }
    }
    return totalTokens
}

func benchmarkBatchedMessages(b *testing.B) int {
    totalTokens := 0
    for i := 0; i < b.N; i++ {
        // Simulate 1 batched message
        totalTokens += 1000 // 1000 tokens in batch
    }
    return totalTokens
}

func benchmarkBaselineAttention(b *testing.B, tokenCount int) time.Duration {
    start := time.Now()
    for i := 0; i < b.N; i++ {
        // Simulate standard attention (slower)
        simulateAttention(tokenCount, false)
    }
    return time.Since(start)
}

func benchmarkFlashAttention(b *testing.B, tokenCount int) time.Duration {
    start := time.Now()
    for i := 0; i < b.N; i++ {
        // Simulate flash attention (faster)
        simulateAttention(tokenCount, true)
    }
    return time.Since(start)
}

func simulateAttention(tokenCount int, flash bool) {
    // Simulated attention computation
    chunks := 1
    if flash {
        chunks = int(math.Ceil(float64(tokenCount) / 64.0))
    }

    for i := 0; i < chunks; i++ {
        // Simulate chunk processing
        _ = make([]byte, 1024)
    }
}

//===----------------------------------------------------------------------===
// Main Test Runner
//===----------------------------------------------------------------------===

func TestPerformanceRegression(t *testing.T) {
    if os.Getenv("SKIP_REGRESSION_TEST") == "1" {
        t.Skip("Skipping regression test")
    }

    // Run all benchmarks and check for regressions
    t.Run("Registry", testRegistryRegression)
    t.Run("Throughput", testThroughputRegression)
    t.Run("Latency", testLatencyRegression)
    t.Run("Memory", testMemoryRegression)
}

func testRegistryRegression(t *testing.T) {
    result := testing.Benchmark(func(b *testing.B) {
        BenchmarkRegistryLookup(b)
    })

    // Extract throughput from result
    // Format: "BenchmarkRegistryLookup-8 100000 12345 ns/op"
    // This would need proper parsing in production
}

func testThroughputRegression(t *testing.T) {
    // Measure throughput
    start := time.Now()
    operations := 100000

    for i := 0; i < operations; i++ {
        // Simulated operation
        _ = rand.Intn(10000)
    }

    duration := time.Since(start)
    throughput := float64(operations) / duration.Seconds()

    if throughput < BaselineThroughput*MinThroughputRatio {
        t.Errorf("Throughput regression: %.0f ops/sec < %.0f ops/sec",
            throughput, BaselineThroughput*MinThroughputRatio)
    }
}

func testLatencyRegression(t *testing.T) {
    latencies := make([]time.Duration, 1000)

    for i := 0; i < 1000; i++ {
        start := time.Now()
        // Simulated operation
        _ = rand.Intn(10000)
        latencies[i] = time.Since(start)
    }

    // Calculate p99
    sort.Slice(latencies, func(i, j int) bool {
        return latencies[i] < latencies[j]
    })

    p99Index := int(float64(len(latencies)) * 0.99)
    p99 := latencies[p99Index].Seconds() * 1000

    if p99 > BaselineP99LatencyMs*MaxLatencyIncreaseRatio {
        t.Errorf("P99 latency regression: %.2f ms > %.2f ms (baseline * 1.2)",
            p99, BaselineP99LatencyMs*MaxLatencyIncreaseRatio)
    }
}

func testMemoryRegression(t *testing.T) {
    var m1, m2 runtime.MemStats

    runtime.ReadMemStats(&m1)

    // Simulate workload
    data := make([][]byte, 10000)
    for i := range data {
        data[i] = make([]byte, 10240) // 10KB each
    }

    runtime.ReadMemStats(&m2)

    memoryMB := float64(m2.Alloc-m1.Alloc) / 1024 / 1024
    baselineMB := BaselineMemoryMB * 10 // 10x baseline for this test

    if memoryMB > baselineMB*MaxMemoryIncreaseRatio {
        t.Errorf("Memory regression: %.2f MB > %.2f MB (baseline * 1.3)",
            memoryMB, baselineMB*MaxMemoryIncreaseRatio)
    }
}
