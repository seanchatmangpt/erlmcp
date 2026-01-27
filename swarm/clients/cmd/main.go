package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"math"
	"math/rand"
	"net"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"sync"
	"sync/atomic"
	"syscall"
	"time"

	"github.com/gorilla/websocket"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"golang.org/x/time/rate"
)

var (
	clientType      = os.Getenv("CLIENT_TYPE")
	targetServers   = os.Getenv("TARGET_SERVERS")
	clientCountStr  = os.Getenv("CLIENT_COUNT")
	messageRateStr  = os.Getenv("MESSAGE_RATE")
	durationStr     = os.Getenv("DURATION_SECONDS")
	metricsPort     = os.Getenv("METRICS_PORT")
	metricsEnabled  = os.Getenv("METRICS_ENABLED")

	// Prometheus metrics
	requestCounter = prometheus.NewCounterVec(
		prometheus.CounterOpts{
			Name: "mcp_client_requests_total",
			Help: "Total number of MCP requests sent",
		},
		[]string{"client_type", "method", "status"},
	)

	requestDuration = prometheus.NewHistogramVec(
		prometheus.HistogramOpts{
			Name:    "mcp_client_request_duration_ms",
			Help:    "Request duration in milliseconds",
			Buckets: prometheus.ExponentialBuckets(10, 2, 10),
		},
		[]string{"client_type", "method"},
	)

	connectionCounter = prometheus.NewGaugeVec(
		prometheus.GaugeOpts{
			Name: "mcp_client_connections_active",
			Help: "Number of active connections",
		},
		[]string{"client_type"},
	)

	messagesSent = prometheus.NewCounterVec(
		prometheus.CounterOpts{
			Name: "mcp_client_messages_sent_total",
			Help: "Total messages sent by client type",
		},
		[]string{"client_type", "message_type"},
	)

	messagesErrors = prometheus.NewCounterVec(
		prometheus.CounterOpts{
			Name: "mcp_client_messages_errors_total",
			Help: "Total message errors",
		},
		[]string{"client_type", "error_type"},
	)
)

func init() {
	prometheus.MustRegister(
		requestCounter,
		requestDuration,
		connectionCounter,
		messagesSent,
		messagesErrors,
	)

	if clientType == "" {
		clientType = "normal"
	}
	if targetServers == "" {
		targetServers = "localhost:5555"
	}
	if clientCountStr == "" {
		clientCountStr = "25"
	}
	if messageRateStr == "" {
		messageRateStr = "100"
	}
	if durationStr == "" {
		durationStr = "3600"
	}
	if metricsPort == "" {
		metricsPort = "8888"
	}
	if metricsEnabled == "" {
		metricsEnabled = "true"
	}
}

type MCPMessage struct {
	Jsonrpc string            `json:"jsonrpc"`
	ID      int64             `json:"id"`
	Method  string            `json:"method"`
	Params  map[string]interface{} `json:"params,omitempty"`
	Result  interface{}       `json:"result,omitempty"`
	Error   *ErrorObject      `json:"error,omitempty"`
}

type ErrorObject struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
	Data    interface{} `json:"data,omitempty"`
}

type ClientProfile struct {
	Name           string
	ClientCount    int
	MessageRate    int
	MessageSize    int
	Timeout        time.Duration
	ConnectionTTL  time.Duration
	ReconnectDelay time.Duration
	Methods        []string
}

type MCPClient struct {
	id       string
	profile  *ClientProfile
	conn     *websocket.Conn
	limiter  *rate.Limiter
	ctx      context.Context
	cancel   context.CancelFunc
	stats    *ClientStats
	mu       sync.Mutex
	closed   bool
}

type ClientStats struct {
	MessagesSent      int64
	MessagesReceived  int64
	Errors            int64
	ConnectionTime    time.Duration
	AverageLatency    int64
	MaxLatency        int64
	MinLatency        int64
	LastMessageTime   time.Time
}

func getProfile(typeName string) *ClientProfile {
	profiles := map[string]*ClientProfile{
		"high-load": {
			Name:           "high-load",
			ClientCount:    50,
			MessageRate:    1000,
			MessageSize:    2048,
			Timeout:        5 * time.Second,
			ConnectionTTL:  5 * time.Minute,
			ReconnectDelay: 1 * time.Second,
			Methods:        []string{"tools/call", "resources/read", "prompts/get"},
		},
		"normal": {
			Name:           "normal",
			ClientCount:    25,
			MessageRate:    100,
			MessageSize:    512,
			Timeout:        10 * time.Second,
			ConnectionTTL:  10 * time.Minute,
			ReconnectDelay: 5 * time.Second,
			Methods:        []string{"tools/call", "resources/list", "resources/read"},
		},
		"low-load": {
			Name:           "low-load",
			ClientCount:    10,
			MessageRate:    10,
			MessageSize:    256,
			Timeout:        30 * time.Second,
			ConnectionTTL:  30 * time.Minute,
			ReconnectDelay: 10 * time.Second,
			Methods:        []string{"tools/list", "resources/list"},
		},
		"slow-client": {
			Name:           "slow-client",
			ClientCount:    5,
			MessageRate:    1,
			MessageSize:    1024,
			Timeout:        60 * time.Second,
			ConnectionTTL:  15 * time.Minute,
			ReconnectDelay: 30 * time.Second,
			Methods:        []string{"resources/read"},
		},
		"message-bomber": {
			Name:           "message-bomber",
			ClientCount:    10,
			MessageRate:    10000,
			MessageSize:    128,
			Timeout:        2 * time.Second,
			ConnectionTTL:   2 * time.Minute,
			ReconnectDelay: 500 * time.Millisecond,
			Methods:        []string{"ping"},
		},
	}

	if profile, ok := profiles[typeName]; ok {
		return profile
	}
	return profiles["normal"]
}

func (c *MCPClient) Connect(serverAddr string) error {
	c.mu.Lock()
	defer c.mu.Unlock()

	dialer := websocket.Dialer{
		HandshakeTimeout: 10 * time.Second,
		DialContext: func(ctx context.Context, network, addr string) (net.Conn, error) {
			return net.DialTimeout(network, addr, 10*time.Second)
		},
	}

	url := fmt.Sprintf("ws://%s/mcp", serverAddr)
	conn, _, err := dialer.DialContext(c.ctx, url, nil)
	if err != nil {
		messagesErrors.WithLabelValues(c.profile.Name, "connection_failed").Inc()
		return fmt.Errorf("connection failed: %w", err)
	}

	c.conn = conn
	conn.SetReadDeadline(time.Now().Add(c.profile.Timeout))
	connectionCounter.WithLabelValues(c.profile.Name).Inc()

	return nil
}

func (c *MCPClient) SendMessage(methodName string) error {
	c.mu.Lock()
	if c.conn == nil || c.closed {
		c.mu.Unlock()
		return fmt.Errorf("connection not available")
	}
	c.mu.Unlock()

	// Rate limiting
	if !c.limiter.Allow() {
		messagesErrors.WithLabelValues(c.profile.Name, "rate_limited").Inc()
		return fmt.Errorf("rate limit exceeded")
	}

	msg := MCPMessage{
		Jsonrpc: "2.0",
		ID:      time.Now().UnixNano(),
		Method:  methodName,
		Params: map[string]interface{}{
			"client_id": c.id,
			"timestamp": time.Now().Unix(),
		},
	}

	if c.profile.MessageSize > 512 {
		msg.Params["payload"] = strings.Repeat("x", c.profile.MessageSize-512)
	}

	start := time.Now()

	c.mu.Lock()
	if err := c.conn.WriteJSON(msg); err != nil {
		c.mu.Unlock()
		messagesErrors.WithLabelValues(c.profile.Name, "write_failed").Inc()
		return fmt.Errorf("write failed: %w", err)
	}
	c.mu.Unlock()

	// Attempt to read response (non-blocking)
	c.conn.SetReadDeadline(time.Now().Add(c.profile.Timeout))

	var resp MCPMessage
	if err := c.conn.ReadJSON(&resp); err != nil {
		if websocket.IsUnexpectedCloseError(err, websocket.CloseGoingAway, websocket.CloseAbnormalClosure) {
			messagesErrors.WithLabelValues(c.profile.Name, "connection_closed").Inc()
		}
		// Non-fatal: continue without response
	}

	duration := time.Since(start)
	durationMs := duration.Milliseconds()

	requestDuration.WithLabelValues(c.profile.Name, methodName).Observe(float64(durationMs))
	messagesSent.WithLabelValues(c.profile.Name, methodName).Inc()

	atomic.AddInt64(&c.stats.MessagesSent, 1)
	atomic.AddInt64(&c.stats.AverageLatency, durationMs)

	if durationMs > atomic.LoadInt64(&c.stats.MaxLatency) {
		atomic.StoreInt64(&c.stats.MaxLatency, durationMs)
	}

	if c.stats.MinLatency == 0 || durationMs < atomic.LoadInt64(&c.stats.MinLatency) {
		atomic.StoreInt64(&c.stats.MinLatency, durationMs)
	}

	c.stats.LastMessageTime = time.Now()

	return nil
}

func (c *MCPClient) Run(serverAddr string) {
	defer func() {
		c.Close()
		connectionCounter.WithLabelValues(c.profile.Name).Dec()
	}()

	// Initial connection
	if err := c.Connect(serverAddr); err != nil {
		log.Printf("[%s] Failed to connect: %v", c.id, err)
		return
	}

	ticker := time.NewTicker(time.Second / time.Duration(c.profile.MessageRate))
	defer ticker.Stop()

	connectionTimer := time.NewTimer(c.profile.ConnectionTTL)
	defer connectionTimer.Stop()

	for {
		select {
		case <-c.ctx.Done():
			return

		case <-connectionTimer.C:
			// Recreate connection periodically
			c.Close()
			connectionTimer.Reset(c.profile.ConnectionTTL)
			if err := c.Connect(serverAddr); err != nil {
				log.Printf("[%s] Reconnection failed: %v", c.id, err)
				time.Sleep(c.profile.ReconnectDelay)
				connectionTimer.Reset(c.profile.ConnectionTTL)
			}

		case <-ticker.C:
			methodIdx := rand.Intn(len(c.profile.Methods))
			method := c.profile.Methods[methodIdx]

			if err := c.SendMessage(method); err != nil && !strings.Contains(err.Error(), "rate limit") {
				log.Printf("[%s] Send error: %v", c.id, err)
				atomic.AddInt64(&c.stats.Errors, 1)
			}
		}
	}
}

func (c *MCPClient) Close() error {
	c.mu.Lock()
	defer c.mu.Unlock()

	if c.closed || c.conn == nil {
		return nil
	}

	c.closed = true
	connectionCounter.WithLabelValues(c.profile.Name).Dec()
	return c.conn.Close()
}

func (c *MCPClient) GetStats() map[string]interface{} {
	return map[string]interface{}{
		"client_id":          c.id,
		"messages_sent":      atomic.LoadInt64(&c.stats.MessagesSent),
		"errors":             atomic.LoadInt64(&c.stats.Errors),
		"average_latency_ms": float64(atomic.LoadInt64(&c.stats.AverageLatency)) / math.Max(1, float64(atomic.LoadInt64(&c.stats.MessagesSent))),
		"max_latency_ms":     atomic.LoadInt64(&c.stats.MaxLatency),
		"min_latency_ms":     atomic.LoadInt64(&c.stats.MinLatency),
	}
}

func parseClientCount() int {
	count := 25
	if n, err := fmt.Sscanf(clientCountStr, "%d", &count); err == nil && n == 1 {
		return count
	}
	return count
}

func parseMessageRate() int {
	rate := 100
	if n, err := fmt.Sscanf(messageRateStr, "%d", &rate); err == nil && n == 1 {
		return rate
	}
	return rate
}

func parseDuration() time.Duration {
	seconds := 3600
	if n, err := fmt.Sscanf(durationStr, "%d", &seconds); err == nil && n == 1 {
		return time.Duration(seconds) * time.Second
	}
	return time.Duration(seconds) * time.Second
}

func main() {
	flag.Parse()

	profile := getProfile(clientType)

	// Override from environment
	if count := parseClientCount(); count > 0 {
		profile.ClientCount = count
	}
	if rate := parseMessageRate(); rate > 0 {
		profile.MessageRate = rate
	}

	duration := parseDuration()

	log.Printf("Starting MCP Client Simulator")
	log.Printf("  Type: %s", profile.Name)
	log.Printf("  Clients: %d", profile.ClientCount)
	log.Printf("  Message Rate: %d/sec", profile.MessageRate)
	log.Printf("  Target Servers: %s", targetServers)
	log.Printf("  Duration: %v", duration)

	// Start metrics server
	if metricsEnabled == "true" {
		go func() {
			http.Handle("/metrics", promhttp.Handler())
			http.HandleFunc("/health", func(w http.ResponseWriter, r *http.Request) {
				w.WriteHeader(http.StatusOK)
				w.Write([]byte("OK"))
			})
			port := ":" + metricsPort
			log.Printf("Metrics server listening on %s", port)
			if err := http.ListenAndServe(port, nil); err != nil {
				log.Fatalf("Metrics server error: %v", err)
			}
		}()
	}

	ctx, cancel := context.WithTimeout(context.Background(), duration)
	defer cancel()

	var wg sync.WaitGroup
	clients := make([]*MCPClient, profile.ClientCount)

	// Create and start clients
	for i := 0; i < profile.ClientCount; i++ {
		clientCtx, clientCancel := context.WithCancel(ctx)
		client := &MCPClient{
			id:      fmt.Sprintf("%s-client-%d", profile.Name, i),
			profile: profile,
			limiter: rate.NewLimiter(rate.Every(time.Second/time.Duration(profile.MessageRate)), 1),
			ctx:     clientCtx,
			cancel:  clientCancel,
			stats: &ClientStats{
				MinLatency: math.MaxInt64,
			},
		}

		clients[i] = client
		wg.Add(1)

		go func(c *MCPClient) {
			defer wg.Done()
			c.Run(targetServers)
		}(client)

		// Stagger connections
		time.Sleep(time.Duration(i) * time.Millisecond)
	}

	// Metrics reporting goroutine
	go func() {
		ticker := time.NewTicker(30 * time.Second)
		defer ticker.Stop()

		for range ticker.C {
			totalSent := int64(0)
			totalErrors := int64(0)

			for _, c := range clients {
				totalSent += atomic.LoadInt64(&c.stats.MessagesSent)
				totalErrors += atomic.LoadInt64(&c.stats.Errors)
			}

			rate := float64(totalSent) / 30.0
			log.Printf("Stats - Messages Sent: %d, Errors: %d, Rate: %.2f/sec", totalSent, totalErrors, rate)
		}
	}()

	// Graceful shutdown handling
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		<-sigChan
		log.Println("Shutdown signal received, closing clients...")
		cancel()
	}()

	// Wait for all clients
	wg.Wait()

	// Final stats
	log.Println("\n=== Final Statistics ===")
	for i, c := range clients {
		if i < 5 { // Log first 5 clients
			stats := c.GetStats()
			statsJSON, _ := json.MarshalIndent(stats, "", "  ")
			log.Printf("%s: %s", c.id, string(statsJSON))
		}
	}

	log.Println("All clients closed successfully")
}
