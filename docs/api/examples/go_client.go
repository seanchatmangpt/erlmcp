// erlmcp Go Client Example
//
// This example demonstrates how to connect to an erlmcp server
// and interact with tools, resources, and prompts.
//
// Build: go build go_client.go
// Run: ./go_client [url]
//
// Default URL: http://localhost:8765/mcp

package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"sync"
)

// MCPError represents an MCP error response
type MCPError struct {
	Code    int             `json:"code"`
	Message string          `json:"message"`
	Data    json.RawMessage `json:"data,omitempty"`
}

func (e MCPError) Error() string {
	return fmt.Sprintf("%d: %s", e.Code, e.Message)
}

// JSONRPCRequest represents a JSON-RPC request
type JSONRPCRequest struct {
	JSONRPC string      `json:"jsonrpc"`
	ID      int         `json:"id"`
	Method  string      `json:"method"`
	Params  interface{} `json:"params,omitempty"`
}

// JSONRPCResponse represents a JSON-RPC response
type JSONRPCResponse struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      int             `json:"id"`
	Result  json.RawMessage `json:"result,omitempty"`
	Error   *MCPError       `json:"error,omitempty"`
}

// ServerInfo represents server information
type ServerInfo struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

// InitializeResult represents the initialize response
type InitializeResult struct {
	ProtocolVersion string               `json:"protocolVersion"`
	Capabilities   map[string]interface{} `json:"capabilities"`
	ServerInfo     ServerInfo            `json:"serverInfo"`
}

// Tool represents a tool definition
type Tool struct {
	Name        string          `json:"name"`
	Description string          `json:"description"`
	InputSchema json.RawMessage `json:"inputSchema,omitempty"`
}

// ToolListResult represents the tools/list response
type ToolListResult struct {
	Tools []Tool `json:"tools"`
}

// Resource represents a resource
type Resource struct {
	URI         string          `json:"uri"`
	Name        string          `json:"name"`
	Description string          `json:"description,omitempty"`
	MimeType    string          `json:"mimeType,omitempty"`
}

// ResourceListResult represents the resources/list response
type ResourceListResult struct {
	Resources []Resource `json:"resources"`
}

// Prompt represents a prompt template
type Prompt struct {
	Name        string `json:"name"`
	Description string `json:"description,omitempty"`
}

// PromptListResult represents the prompts/list response
type PromptListResult struct {
	Prompts []Prompt `json:"prompts"`
}

// McpClient is an MCP client
type McpClient struct {
	baseURL    string
	httpClient *http.Client
	mu         sync.Mutex
	messageID  int
}

// NewMcpClient creates a new MCP client
func NewMcpClient(baseURL string) *McpClient {
	return &McpClient{
		baseURL:    baseURL,
		httpClient: &http.Client{},
		messageID:  0,
	}
}

// sendRequest sends a JSON-RPC request
func (c *McpClient) sendRequest(method string, params interface{}) (json.RawMessage, error) {
	c.mu.Lock()
	c.messageID++
	id := c.messageID
	c.mu.Unlock()

	reqBody := JSONRPCRequest{
		JSONRPC: "2.0",
		ID:      id,
		Method:  method,
		Params:  params,
	}

	jsonData, err := json.Marshal(reqBody)
	if err != nil {
		return nil, err
	}

	resp, err := c.httpClient.Post(c.baseURL, "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	var rpcResp JSONRPCResponse
	if err := json.Unmarshal(body, &rpcResp); err != nil {
		return nil, err
	}

	if rpcResp.Error != nil {
		return nil, rpcResp.Error
	}

	return rpcResp.Result, nil
}

// Initialize initializes the connection
func (c *McpClient) Initialize(clientName, clientVersion string) (*InitializeResult, error) {
	params := map[string]interface{}{
		"protocolVersion": "2025-11-25",
		"capabilities": map[string]interface{}{
			"roots":    map[string]interface{}{},
			"sampling": map[string]interface{}{},
		},
		"clientInfo": map[string]string{
			"name":    clientName,
			"version": clientVersion,
		},
	}

	result, err := c.sendRequest("initialize", params)
	if err != nil {
		return nil, err
	}

	var initResult InitializeResult
	if err := json.Unmarshal(result, &initResult); err != nil {
		return nil, err
	}

	return &initResult, nil
}

// ListTools lists available tools
func (c *McpClient) ListTools() (*ToolListResult, error) {
	result, err := c.sendRequest("tools/list", nil)
	if err != nil {
		return nil, err
	}

	var toolResult ToolListResult
	if err := json.Unmarshal(result, &toolResult); err != nil {
		return nil, err
	}

	return &toolResult, nil
}

// CallTool executes a tool
func (c *McpClient) CallTool(name string, arguments map[string]interface{}) (json.RawMessage, error) {
	params := map[string]interface{}{
		"name": name,
	}
	if arguments != nil {
		params["arguments"] = arguments
	}

	return c.sendRequest("tools/call", params)
}

// ListResources lists available resources
func (c *McpClient) ListResources() (*ResourceListResult, error) {
	result, err := c.sendRequest("resources/list", nil)
	if err != nil {
		return nil, err
	}

	var resourceResult ResourceListResult
	if err := json.Unmarshal(result, &resourceResult); err != nil {
		return nil, err
	}

	return &resourceResult, nil
}

// ReadResource reads a resource
func (c *McpClient) ReadResource(uri string) (json.RawMessage, error) {
	return c.sendRequest("resources/read", map[string]interface{}{
		"uri": uri,
	})
}

// ListPrompts lists available prompts
func (c *McpClient) ListPrompts() (*PromptListResult, error) {
	result, err := c.sendRequest("prompts/list", nil)
	if err != nil {
		return nil, err
	}

	var promptResult PromptListResult
	if err := json.Unmarshal(result, &promptResult); err != nil {
		return nil, err
	}

	return &promptResult, nil
}

// Ping checks server liveness
func (c *McpClient) Ping() error {
	_, err := c.sendRequest("ping", nil)
	return err
}

func main() {
	url := "http://localhost:8765/mcp"
	if len(os.Args) > 1 {
		url = os.Args[1]
	}

	fmt.Println("==================================================")
	fmt.Println("erlmcp Go Client Example")
	fmt.Println("==================================================")

	client := NewMcpClient(url)

	// Initialize
	fmt.Println("\n1. Initializing connection...")
	initResult, err := client.Initialize("go-client", "1.0.0")
	if err != nil {
		fmt.Printf("   Error: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("   Connected to %s v%s\n", initResult.ServerInfo.Name, initResult.ServerInfo.Version)

	// Ping
	fmt.Println("\n2. Pinging server...")
	if err := client.Ping(); err != nil {
		fmt.Printf("   Ping failed: %v\n", err)
	} else {
		fmt.Println("   Server alive: true")
	}

	// List tools
	fmt.Println("\n3. Listing available tools...")
	tools, err := client.ListTools()
	if err != nil {
		fmt.Printf("   Error: %v\n", err)
	} else {
		for _, tool := range tools {
			fmt.Printf("   - %s: %s\n", tool.Name, tool.Description)
		}
	}

	// Call a tool
	if tools != nil && len(tools) > 0 {
		fmt.Printf("\n4. Calling tool '%s'...\n", tools[0].Name)
		result, err := client.CallTool(tools[0].Name, map[string]interface{}{})
		if err != nil {
			fmt.Printf("   Error: %v\n", err)
		} else {
			fmt.Printf("   Result: %s\n", string(result))
		}
	}

	// List resources
	fmt.Println("\n5. Listing available resources...")
	resources, err := client.ListResources()
	if err != nil {
		fmt.Printf("   Error: %v\n", err)
	} else {
		for _, resource := range resources {
			fmt.Printf("   - %s: %s\n", resource.URI, resource.Name)
		}
	}

	// Read a resource
	if resources != nil && len(resources) > 0 {
		fmt.Printf("\n6. Reading resource '%s'...\n", resources[0].URI)
		result, err := client.ReadResource(resources[0].URI)
		if err != nil {
			fmt.Printf("   Error: %v\n", err)
		} else {
			if len(result) > 100 {
				fmt.Printf("   Content: %s...\n", string(result[:100]))
			} else {
				fmt.Printf("   Content: %s\n", string(result))
			}
		}
	}

	// List prompts
	fmt.Println("\n7. Listing available prompts...")
	prompts, err := client.ListPrompts()
	if err != nil {
		fmt.Printf("   Error: %v\n", err)
	} else {
		for _, prompt := range prompts {
			fmt.Printf("   - %s: %s\n", prompt.Name, prompt.Description)
		}
	}

	fmt.Println("\n==================================================")
	fmt.Println("Example complete!")
	fmt.Println("==================================================")
}
