# Calculator MCP Server Guide

## Overview

This is an updated calculator MCP server built with erlmcp 0.3.1 that provides mathematical operations through the Model Context Protocol. The server offers basic arithmetic operations, advanced math functions, and educational features.

## Features

### Tools
- **add**: Add two numbers
- **subtract**: Subtract two numbers  
- **multiply**: Multiply two numbers
- **divide**: Divide two numbers (with zero-division protection)
- **power**: Calculate power (a^b)
- **sqrt**: Calculate square root
- **factorial**: Calculate factorial
- **calculate**: Evaluate mathematical expressions

### Resources
- **calculator://history**: View calculation history
- **calculator://help**: Get help on using the calculator

### Prompts
- **math_problem**: Generate math problems with configurable difficulty and type

## Building and Running

### Prerequisites
- Erlang/OTP 24 or later
- Rebar3 build tool

### Build the Project
```bash
# Clone the erlmcp repository
git clone <repository-url>
cd erlmcp

# Compile the project
rebar3 compile
```

### Testing the Server
```bash
# Run the test directly
erl -pa _build/default/lib/erlmcp/ebin -noshell -eval "calculator_test:run(), halt()."
```

## Claude Desktop Integration

### 1. Configuration File Location
Create or edit the `mcp.json` file in your Claude Desktop configuration directory:

- **macOS**: `~/Library/Application Support/Claude/mcp.json`
- **Windows**: `%APPDATA%\Claude\mcp.json`
- **Linux**: `~/.config/Claude/mcp.json`

### 2. Configuration
```json
{
  "mcpServers": {
    "calculator": {
      "command": "erl",
      "args": [
        "-noshell",
        "-pa", "/path/to/erlmcp/_build/default/lib/erlmcp/ebin",
        "-eval", "calculator_server_stdio:start()."
      ]
    }
  }
}
```

**Important**: Replace `/path/to/erlmcp/_build/default/lib/erlmcp/ebin` with the actual absolute path to your compiled erlmcp library.

### 3. Restart Claude Desktop
After editing the configuration, completely restart Claude Desktop for the changes to take effect.

## Usage Examples

### Basic Arithmetic
```
User: Use the add tool to calculate 25 + 17

Claude: I'll use the add tool to calculate 25 + 17.

[Claude calls the add tool with {"a": 25, "b": 17}]

The result is 42.
```

### Advanced Operations
```
User: Use the power tool to calculate 2^10

Claude: I'll calculate 2 raised to the power of 10.

[Claude calls the power tool with {"a": 2, "b": 10}]

The result is 1024.
```

### Expression Evaluation
```
User: Use the calculate tool to evaluate "(15 + 8) * 3 - 12"

Claude: I'll evaluate that mathematical expression for you.

[Claude calls the calculate tool with {"expression": "(15 + 8) * 3 - 12"}]

The result is 57.
```

### Getting Help
```
User: Read the calculator://help resource

Claude: I'll read the calculator help resource for you.

[Claude reads the calculator://help resource]

The help resource shows the available operations and usage examples for the calculator server.
```

### Generating Math Problems
```
User: Use the math_problem prompt to create a hard algebra problem

Claude: I'll generate a hard algebra problem for you.

[Claude uses the math_problem prompt with {"difficulty": "hard", "type": "algebra"}]

Here's an algebra problem: Solve for x: 3x + 7 = 22
```

## Error Handling

The calculator server includes comprehensive error handling:

- **Division by zero**: Returns helpful error message
- **Invalid expressions**: Provides parsing error details
- **Negative square roots**: Explains mathematical limitation
- **Invalid factorial inputs**: Checks for integer and positive values
- **Number overflow**: Limits factorial calculations to prevent crashes

## Implementation Details

### Key Changes from Original
1. **Updated API**: Uses the current erlmcp 0.3.1 API
2. **Improved Error Handling**: Better error messages and validation
3. **Schema Validation**: Includes JSON schema for tool parameters
4. **Resource Support**: Adds helpful resources
5. **Prompt Integration**: Provides educational prompts
6. **Expression Evaluator**: Simple mathematical expression parser

### Architecture
- **stdio Transport**: Uses standard input/output for communication
- **JSON-RPC**: Follows MCP protocol specification
- **Stateless Design**: Each calculation is independent
- **Error Recovery**: Graceful handling of invalid inputs

## Extending the Calculator

To add new mathematical functions:

1. Add a new tool using `erlmcp_stdio:add_tool/4`
2. Include proper JSON schema for validation
3. Implement error handling for edge cases
4. Update the help resource with new functionality

Example:
```erlang
ok = erlmcp_stdio:add_tool(<<"sin">>, <<"Calculate sine of angle in radians">>,
    fun(#{<<"angle">> := Angle}) ->
        Result = math:sin(Angle),
        format_number(Result)
    end,
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"angle">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Angle in radians">>}
        },
        <<"required">> => [<<"angle">>]
    }),
```

## Troubleshooting

### Common Issues

1. **Server not starting**: Check that erlmcp is properly compiled and the path in mcp.json is correct
2. **Tool not found**: Verify the tool name matches exactly (case-sensitive)
3. **Invalid parameters**: Check that the JSON schema requirements are met
4. **Connection errors**: Ensure Claude Desktop was restarted after configuration changes

### Debug Mode
Enable debug logging by modifying the logger configuration in `main/1`:

```erlang
logger:set_primary_config(level, debug),
```

### Manual Testing
Test the server manually by running:
```bash
erl -pa _build/default/lib/erlmcp/ebin -eval "calculator_server_stdio:start()."
```

Then send JSON-RPC messages to test individual tools.

## License

This calculator server is built on the erlmcp library and follows the same licensing terms.