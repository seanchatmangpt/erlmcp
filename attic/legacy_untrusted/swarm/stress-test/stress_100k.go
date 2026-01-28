package main

import (
	"crypto/tls"
	"flag"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"os/signal"
	"sync"
	"sync/atomic"
	"syscall"
	"time"
)

type TestMetrics struct {
	requestsSent       int64
	requestsSuccess    int64
	requestsFailed     int64
	totalLatency       int64
	minLatency         int64
	maxLatency         int64
	p50Latency         int64
	p95Latency         int64
	p99Latency         int64
	errors             int64
	connectionErrors   int64
	timeoutErrors      int64
	statusCodeErrors   int64
	bytesReceived      int64
	bytesSent          int64
	latencies          []int64
	latenciesMutex     sync.Mutex
}

type Config struct {
	URL         string
	Connections int
	Duration    time.Duration
	BatchSize   int
	Timeout     time.Duration
	Verbose     bool
}

func main() {
	url := flag.String("url", "http://localhost:8080", "Target URL")
	connections := flag.Int("connections", 100000, "Number of concurrent connections")
	duration := flag.Int("duration", 5, "Test duration in minutes")
	batchSize := flag.Int("batch", 5000, "Connections per second")
	timeout := flag.Duration("timeout", 5*time.Second, "Request timeout")
	verbose := flag.Bool("v", false, "Verbose output")
	flag.Parse()

	config := Config{
		URL:         *url,
		Connections: *connections,
		Duration:    time.Duration(*duration) * time.Minute,
		BatchSize:   *batchSize,
		Timeout:     *timeout,
		Verbose:     *verbose,
	}

	runStressTest(config)
}

func runStressTest(config Config) {
	startTime := time.Now()
	endTime := startTime.Add(config.Duration)
	metrics := &TestMetrics{
		minLatency: 999999999,
		latencies:  make([]int64, 0, config.Connections),
	}

	printHeader(config, startTime)

	// Setup HTTP client with connection pooling
	client := createHTTPClient(config)

	// Setup signal handling for graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	// Progress monitoring
	progressDone := make(chan struct{})
	go monitorProgress(metrics, startTime, endTime, progressDone)

	// Rate limiter
	ticker := time.NewTicker(time.Second / time.Duration(config.BatchSize))
	defer ticker.Stop()

	var wg sync.WaitGroup
	connectionLimiter := make(chan struct{}, config.BatchSize*2)

	// Connection launcher
	launched := 0
	for launched < config.Connections {
		select {
		case <-sigChan:
			fmt.Println("\n[INFO] Received shutdown signal, waiting for in-flight requests...")
			break
		case <-ticker.C:
			if launched < config.Connections && time.Now().Before(endTime) {
				wg.Add(1)
				go func(connID int) {
					defer wg.Done()
					connectionLimiter <- struct{}{}
					defer func() { <-connectionLimiter }()

					makeRequest(client, config.URL, metrics, connID)
				}(launched)
				launched++
			}
		}
	}

	// Wait for all requests to complete
	wg.Wait()
	close(progressDone)

	// Finalize and print report
	printFinalReport(metrics, startTime, time.Now(), config.Connections)
}

func createHTTPClient(config Config) *http.Client {
	transport := &http.Transport{
		Dial: (&net.Dialer{
			Timeout:   config.Timeout,
			KeepAlive: 30 * time.Second,
		}).Dial,
		TLSClientConfig: &tls.Config{
			InsecureSkipVerify: true,
		},
		MaxIdleConns:        config.Connections,
		MaxIdleConnsPerHost: 256,
		MaxConnsPerHost:     config.Connections,
		DisableKeepAlives:   false,
		DisableCompression:  true,
		IdleConnTimeout:     time.Hour,
		ResponseHeaderTimeout: config.Timeout,
	}

	return &http.Client{
		Transport: transport,
		Timeout:   config.Timeout,
	}
}

func makeRequest(client *http.Client, url string, metrics *TestMetrics, connID int) {
	atomic.AddInt64(&metrics.requestsSent, 1)

	start := time.Now()
	resp, err := client.Get(url + "/health")
	latency := time.Since(start).Milliseconds()

	// Record latency
	metrics.latenciesMutex.Lock()
	metrics.latencies = append(metrics.latencies, latency)
	metrics.latenciesMutex.Unlock()

	if err != nil {
		atomic.AddInt64(&metrics.requestsFailed, 1)
		atomic.AddInt64(&metrics.errors, 1)

		if netErr, ok := err.(net.Error); ok {
			if netErr.Timeout() {
				atomic.AddInt64(&metrics.timeoutErrors, 1)
			} else {
				atomic.AddInt64(&metrics.connectionErrors, 1)
			}
		}
		return
	}

	defer resp.Body.Close()

	bodyBytes, _ := io.ReadAll(resp.Body)
	atomic.AddInt64(&metrics.bytesReceived, int64(len(bodyBytes)))
	atomic.AddInt64(&metrics.bytesSent, 200) // Approximate

	if resp.StatusCode != http.StatusOK {
		atomic.AddInt64(&metrics.statusCodeErrors, 1)
		atomic.AddInt64(&metrics.errors, 1)
		return
	}

	atomic.AddInt64(&metrics.requestsSuccess, 1)
	atomic.AddInt64(&metrics.totalLatency, latency)

	// Update min/max latency
	for {
		minL := atomic.LoadInt64(&metrics.minLatency)
		if latency < minL && atomic.CompareAndSwapInt64(&metrics.minLatency, minL, latency) {
			break
		} else if latency >= minL {
			break
		}
	}

	for {
		maxL := atomic.LoadInt64(&metrics.maxLatency)
		if latency > maxL && atomic.CompareAndSwapInt64(&metrics.maxLatency, maxL, latency) {
			break
		} else if latency <= maxL {
			break
		}
	}
}

func monitorProgress(metrics *TestMetrics, start, end time.Time, totalConns int) {
	ticker := time.NewTicker(time.Second)
	defer ticker.Stop()

	for range ticker.C {
		now := time.Now()
		if now.After(end) {
			break
		}

		elapsed := int(now.Sub(start).Seconds())
		sent := atomic.LoadInt64(&metrics.requestsSent)
		success := atomic.LoadInt64(&metrics.requestsSuccess)
		failed := atomic.LoadInt64(&metrics.requestsFailed)
		totalLat := atomic.LoadInt64(&metrics.totalLatency)

		var avgLat int64
		if success > 0 {
			avgLat = totalLat / success
		}

		throughput := int64(0)
		if elapsed > 0 {
			throughput = success / int64(elapsed)
		}

		fmt.Printf("[%3ds] Active: %d | Sent: %d | Success: %d | Failed: %d | Throughput: %d/s | Avg Lat: %dms\n",
			elapsed, totalConns, sent, success, failed, throughput, avgLat)
	}
}

func calculatePercentiles(latencies []int64) (p50, p95, p99 int64) {
	if len(latencies) == 0 {
		return 0, 0, 0
	}

	// Simple bubble sort for small sample or use quicksort for large
	sorted := make([]int64, len(latencies))
	copy(sorted, latencies)

	// Partition for percentiles
	quickSort(sorted, 0, len(sorted)-1)

	p50Idx := (len(sorted) * 50) / 100
	p95Idx := (len(sorted) * 95) / 100
	p99Idx := (len(sorted) * 99) / 100

	if p50Idx < len(sorted) {
		p50 = sorted[p50Idx]
	}
	if p95Idx < len(sorted) {
		p95 = sorted[p95Idx]
	}
	if p99Idx < len(sorted) {
		p99 = sorted[p99Idx]
	}

	return
}

func quickSort(arr []int64, low, high int) {
	if low < high {
		p := partition(arr, low, high)
		quickSort(arr, low, p-1)
		quickSort(arr, p+1, high)
	}
}

func partition(arr []int64, low, high int) int {
	pivot := arr[high]
	i := low - 1

	for j := low; j < high; j++ {
		if arr[j] < pivot {
			i++
			arr[i], arr[j] = arr[j], arr[i]
		}
	}
	arr[i+1], arr[high] = arr[high], arr[i+1]
	return i + 1
}

func printHeader(config Config, startTime time.Time) {
	fmt.Println()
	fmt.Println("╔═══════════════════════════════════════════════════════════════╗")
	fmt.Println("║   ErlMCP 100K Concurrent Connection Stress Test              ║")
	fmt.Println("╚═══════════════════════════════════════════════════════════════╝")
	fmt.Println()
	fmt.Printf("Target URL:        %s\n", config.URL)
	fmt.Printf("Connections:       %d\n", config.Connections)
	fmt.Printf("Duration:          %d minutes\n", int(config.Duration.Minutes()))
	fmt.Printf("Batch Size:        %d conn/sec\n", config.BatchSize)
	fmt.Printf("Request Timeout:   %v\n", config.Timeout)
	fmt.Printf("Start Time:        %s\n", startTime.Format(time.RFC3339))
	fmt.Println()
}

func printFinalReport(metrics *TestMetrics, start, end time.Time, totalConns int) {
	elapsed := end.Sub(start)

	sent := atomic.LoadInt64(&metrics.requestsSent)
	success := atomic.LoadInt64(&metrics.requestsSuccess)
	failed := atomic.LoadInt64(&metrics.requestsFailed)
	totalLat := atomic.LoadInt64(&metrics.totalLatency)
	errors := atomic.LoadInt64(&metrics.errors)
	minLat := atomic.LoadInt64(&metrics.minLatency)
	maxLat := atomic.LoadInt64(&metrics.maxLatency)
	bytesRx := atomic.LoadInt64(&metrics.bytesReceived)
	bytesTx := atomic.LoadInt64(&metrics.bytesSent)

	var avgLat, throughput, successRate, errorRate int64
	if success > 0 {
		avgLat = totalLat / success
	}
	if elapsed.Seconds() > 0 {
		throughput = int64(float64(success) / elapsed.Seconds())
	}
	if sent > 0 {
		successRate = (success * 100) / sent
		errorRate = (errors * 100) / sent
	}

	// Calculate percentiles
	p50, p95, p99 := calculatePercentiles(metrics.latencies)

	fmt.Printf("\n\n╔═══════════════════════════════════════════════════════════════╗\n")
	fmt.Printf("║                  FINAL TEST REPORT                          ║\n")
	fmt.Printf("╚═══════════════════════════════════════════════════════════════╝\n\n")

	fmt.Printf("Test Duration:             %.2f seconds\n", elapsed.Seconds())
	fmt.Printf("Total Connections:         %d\n", totalConns)

	fmt.Printf("\n--- REQUEST METRICS ---\n")
	fmt.Printf("Requests Sent:             %d\n", sent)
	fmt.Printf("Successful Requests:       %d\n", success)
	fmt.Printf("Failed Requests:           %d\n", failed)
	fmt.Printf("Success Rate:              %d%%\n", successRate)

	fmt.Printf("\n--- LATENCY METRICS (ms) ---\n")
	fmt.Printf("Min Latency:               %d\n", minLat)
	fmt.Printf("P50 (Median):              %d\n", p50)
	fmt.Printf("P95 (95th percentile):     %d\n", p95)
	fmt.Printf("P99 (99th percentile):     %d\n", p99)
	fmt.Printf("Avg Latency:               %d\n", avgLat)
	fmt.Printf("Max Latency:               %d\n", maxLat)

	fmt.Printf("\n--- THROUGHPUT ---\n")
	fmt.Printf("Requests/Second:           %d\n", throughput)
	if sent > 0 {
		fmt.Printf("MB/Second:                 %.2f\n", float64(bytesTx)/elapsed.Seconds()/1024/1024)
	}

	fmt.Printf("\n--- DATA TRANSFER ---\n")
	fmt.Printf("Bytes Sent:                %d\n", bytesTx)
	fmt.Printf("Bytes Received:            %d\n", bytesRx)

	fmt.Printf("\n--- ERROR BREAKDOWN ---\n")
	fmt.Printf("Total Errors:              %d\n", errors)
	fmt.Printf("Error Rate:                %d%%\n", errorRate)
	fmt.Printf("Connection Errors:         %d\n", atomic.LoadInt64(&metrics.connectionErrors))
	fmt.Printf("Timeout Errors:            %d\n", atomic.LoadInt64(&metrics.timeoutErrors))
	fmt.Printf("Status Code Errors:        %d\n", atomic.LoadInt64(&metrics.statusCodeErrors))

	fmt.Printf("\n╔═══════════════════════════════════════════════════════════════╗\n")
	if successRate >= 95 {
		fmt.Printf("║  ✓ TEST PASSED (Success rate: %d%%)                    ║\n", successRate)
	} else {
		fmt.Printf("║  ✗ TEST FAILED (Success rate: %d%%)                    ║\n", successRate)
	}
	fmt.Printf("╚═══════════════════════════════════════════════════════════════╝\n\n")

	// Exit with proper code
	if successRate >= 95 {
		os.Exit(0)
	} else {
		os.Exit(1)
	}
}
