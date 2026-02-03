#!/usr/bin/env python3
"""
erlmcp Python Client Example

This example demonstrates how to connect to an erlmcp server
and interact with tools, resources, and prompts.

Requirements:
    pip install asyncio aiohttp
"""

import asyncio
import json
import sys
from typing import Any, Optional

import aiohttp


class McpError(Exception):
    """Base MCP error"""
    def __init__(self, code: int, message: str, data: Any = None):
        self.code = code
        self.message = message
        self.data = data
        super().__init__(f"{code}: {message}")


class McpClient:
    """MCP Client for erlmcp servers"""

    def __init__(self, transport_url: str):
        self.transport_url = transport_url
        self.session: Optional[aiohttp.ClientSession] = None
        self.message_id = 0
        self.server_capabilities = {}
        self.server_info = {}

    async def __aenter__(self):
        self.session = aiohttp.ClientSession()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        if self.session:
            await self.session.close()

    async def _send_request(self, method: str, params: dict = None) -> dict:
        """Send a JSON-RPC request"""
        self.message_id += 1
        payload = {
            "jsonrpc": "2.0",
            "id": self.message_id,
            "method": method,
        }
        if params:
            payload["params"] = params

        async with self.session.post(
            self.transport_url,
            json=payload,
            headers={"Content-Type": "application/json"}
        ) as response:
            data = await response.json()

        if "error" in data:
            error = data["error"]
            raise McpError(error["code"], error["message"], error.get("data"))

        return data.get("result", {})

    async def initialize(self, client_name: str = "python-client", client_version: str = "1.0.0") -> dict:
        """Initialize the connection"""
        result = await self._send_request("initialize", {
            "protocolVersion": "2025-11-25",
            "capabilities": {
                "roots": {},
                "sampling": {}
            },
            "clientInfo": {
                "name": client_name,
                "version": client_version
            }
        })
        self.server_info = result.get("serverInfo", {})
        self.server_capabilities = result.get("capabilities", {})
        print(f"Connected to {self.server_info.get('name')} v{self.server_info.get('version')}")
        return result

    async def list_tools(self) -> list:
        """List available tools"""
        result = await self._send_request("tools/list")
        return result.get("tools", [])

    async def call_tool(self, name: str, arguments: dict = None) -> dict:
        """Execute a tool"""
        params = {"name": name}
        if arguments:
            params["arguments"] = arguments

        result = await self._send_request("tools/call", params)
        return result

    async def list_resources(self) -> list:
        """List available resources"""
        result = await self._send_request("resources/list")
        return result.get("resources", [])

    async def read_resource(self, uri: str) -> dict:
        """Read a resource"""
        result = await self._send_request("resources/read", {"uri": uri})
        return result

    async def subscribe_resource(self, uri: str) -> None:
        """Subscribe to resource updates"""
        await self._send_request("resources/subscribe", {"uri": uri})

    async def list_prompts(self) -> list:
        """List available prompts"""
        result = await self._send_request("prompts/list")
        return result.get("prompts", [])

    async def get_prompt(self, name: str, arguments: dict = None) -> dict:
        """Get a prompt template with arguments"""
        params = {"name": name}
        if arguments:
            params["arguments"] = arguments

        result = await self._send_request("prompts/get", params)
        return result

    async def ping(self) -> bool:
        """Check server liveness"""
        try:
            await self._send_request("ping")
            return True
        except Exception:
            return False


async def main():
    """Main example"""

    # Configure server URL
    url = sys.argv[1] if len(sys.argv) > 1 else "http://localhost:8765/mcp"

    async with McpClient(url) as client:
        print("=" * 50)
        print("erlmcp Python Client Example")
        print("=" * 50)

        # Initialize
        print("\n1. Initializing connection...")
        await client.initialize()

        # Ping
        print("\n2. Pinging server...")
        alive = await client.ping()
        print(f"   Server alive: {alive}")

        # List tools
        print("\n3. Listing available tools...")
        tools = await client.list_tools()
        for tool in tools:
            print(f"   - {tool['name']}: {tool.get('description', 'No description')}")

        # Call a tool
        if tools:
            print(f"\n4. Calling tool '{tools[0]['name']}'...")
            try:
                result = await client.call_tool(tools[0]['name'], {})
                if 'content' in result:
                    for content in result['content']:
                        if content.get('type') == 'text':
                            print(f"   Result: {content['text']}")
            except McpError as e:
                print(f"   Error: {e}")

        # List resources
        print("\n5. Listing available resources...")
        resources = await client.list_resources()
        for resource in resources:
            print(f"   - {resource['uri']}: {resource.get('name', 'Unnamed')}")

        # Read a resource
        if resources:
            print(f"\n6. Reading resource '{resources[0]['uri']}'...")
            try:
                result = await client.read_resource(resources[0]['uri'])
                if 'contents' in result:
                    for content in result['contents']:
                        if content.get('type') == 'text':
                            text = content['text'][:100]
                            print(f"   Content: {text}{'...' if len(content['text']) > 100 else ''}")
            except McpError as e:
                print(f"   Error: {e}")

        # List prompts
        print("\n7. Listing available prompts...")
        prompts = await client.list_prompts()
        for prompt in prompts:
            print(f"   - {prompt['name']}: {prompt.get('description', 'No description')}")

        print("\n" + "=" * 50)
        print("Example complete!")
        print("=" * 50)


if __name__ == "__main__":
    asyncio.run(main())
