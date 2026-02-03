#!/usr/bin/env node
/**
 * erlmcp JavaScript/Node.js Client Example
 *
 * This example demonstrates how to connect to an erlmcp server
 * and interact with tools, resources, and prompts.
 *
 * Run: node javascript_client.mjs [url]
 * Default URL: http://localhost:8765/mcp
 */

import fetch from 'node-fetch';

const DEFAULT_URL = 'http://localhost:8765/mcp';

class McpError extends Error {
    constructor(code, message, data) {
        super(`${code}: ${message}`);
        this.code = code;
        this.message = message;
        this.data = data;
    }
}

class McpClient {
    constructor(baseUrl) {
        this.baseUrl = baseUrl;
        this.messageId = 0;
        this.serverCapabilities = {};
        this.serverInfo = {};
    }

    async _sendRequest(method, params = null) {
        this.messageId++;
        const payload = {
            jsonrpc: '2.0',
            id: this.messageId,
            method: method
        };
        if (params !== null) {
            payload.params = params;
        }

        const response = await fetch(this.baseUrl, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(payload)
        });

        const data = await response.json();

        if (data.error) {
            throw new McpError(
                data.error.code,
                data.error.message,
                data.error.data
            );
        }

        return data.result || {};
    }

    async initialize(clientName = 'javascript-client', clientVersion = '1.0.0') {
        const result = await this._sendRequest('initialize', {
            protocolVersion: '2025-11-25',
            capabilities: {
                roots: {},
                sampling: {}
            },
            clientInfo: {
                name: clientName,
                version: clientVersion
            }
        });

        this.serverInfo = result.serverInfo || {};
        this.serverCapabilities = result.capabilities || {};
        console.log(`Connected to ${this.serverInfo.name} v${this.serverInfo.version}`);
        return result;
    }

    async listTools() {
        const result = await this._sendRequest('tools/list');
        return result.tools || [];
    }

    async callTool(name, arguments = {}) {
        return await this._sendRequest('tools/call', {
            name,
            arguments
        });
    }

    async listResources() {
        const result = await this._sendRequest('resources/list');
        return result.resources || [];
    }

    async readResource(uri) {
        return await this._sendRequest('resources/read', { uri });
    }

    async subscribeResource(uri) {
        return await this._sendRequest('resources/subscribe', { uri });
    }

    async listPrompts() {
        const result = await this._sendRequest('prompts/list');
        return result.prompts || [];
    }

    async getPrompt(name, arguments = {}) {
        return await this._sendRequest('prompts/get', {
            name,
            arguments
        });
    }

    async ping() {
        try {
            await this._sendRequest('ping');
            return true;
        } catch (error) {
            return false;
        }
    }
}

async function main() {
    const url = process.argv[2] || DEFAULT_URL;

    console.log('==================================================');
    console.log('erlmcp JavaScript Client Example');
    console.log('==================================================');

    const client = new McpClient(url);

    try {
        // Initialize
        console.log('\n1. Initializing connection...');
        await client.initialize();

        // Ping
        console.log('\n2. Pinging server...');
        const alive = await client.ping();
        console.log(`   Server alive: ${alive}`);

        // List tools
        console.log('\n3. Listing available tools...');
        const tools = await client.listTools();
        for (const tool of tools) {
            console.log(`   - ${tool.name}: ${tool.description || 'No description'}`);
        }

        // Call a tool
        if (tools.length > 0) {
            console.log(`\n4. Calling tool '${tools[0].name}'...`);
            try {
                const result = await client.callTool(tools[0].name, {});
                if (result.content) {
                    for (const content of result.content) {
                        if (content.type === 'text') {
                            console.log(`   Result: ${content.text}`);
                        }
                    }
                }
            } catch (error) {
                console.log(`   Error: ${error.message}`);
            }
        }

        // List resources
        console.log('\n5. Listing available resources...');
        const resources = await client.listResources();
        for (const resource of resources) {
            console.log(`   - ${resource.uri}: ${resource.name || 'Unnamed'}`);
        }

        // Read a resource
        if (resources.length > 0) {
            console.log(`\n6. Reading resource '${resources[0].uri}'...`);
            try {
                const result = await client.readResource(resources[0].uri);
                if (result.contents) {
                    for (const content of result.contents) {
                        if (content.type === 'text') {
                            const text = content.text.substring(0, 100);
                            const suffix = content.text.length > 100 ? '...' : '';
                            console.log(`   Content: ${text}${suffix}`);
                        }
                    }
                }
            } catch (error) {
                console.log(`   Error: ${error.message}`);
            }
        }

        // List prompts
        console.log('\n7. Listing available prompts...');
        const prompts = await client.listPrompts();
        for (const prompt of prompts) {
            console.log(`   - ${prompt.name}: ${prompt.description || 'No description'}`);
        }

        console.log('\n==================================================');
        console.log('Example complete!');
        console.log('==================================================');

    } catch (error) {
        console.error(`\nError: ${error.message}`);
        process.exit(1);
    }
}

main();
