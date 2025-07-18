# Weather MCP Server Guide

## Overview

This is a comprehensive weather MCP server built with erlmcp that provides weather information, forecasts, and temperature conversion through the Model Context Protocol. The server offers current weather conditions, 5-day forecasts, weather alerts, and temperature unit conversions.

## Features

### Tools

- **get_weather**: Get current weather conditions for any location
- **get_forecast**: Get 5-day weather forecast for any location
- **get_weather_alerts**: Get active weather alerts and warnings
- **convert_temperature**: Convert temperatures between Celsius, Fahrenheit, and Kelvin

### Resources

- **weather://locations**: List of supported location formats and examples
- **weather://help**: Comprehensive help documentation for all weather tools
- **weather://status**: Weather service status and statistics

### Prompts

- **weather_report**: Generate prompts for weather reports (current, forecast, or detailed)
- **weather_advice**: Generate prompts for weather-based activity advice

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
# Run the comprehensive test suite
erl -pa _build/default/lib/erlmcp/ebin -noshell -eval "weather_test:run_all_tests(), halt()."

# Run specific test groups
erl -pa _build/default/lib/erlmcp/ebin -noshell -eval "weather_test:run_weather_tests(), halt()."
erl -pa _build/default/lib/erlmcp/ebin -noshell -eval "weather_test:run_conversion_tests(), halt()."
```

## Claude Desktop Integration

### 1. Locate the Configuration File

Claude Desktop uses a `claude_desktop_config.json` configuration file. The location depends on your operating system:

- **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Windows**: `%APPDATA%\Claude\claude_desktop_config.json`
- **Linux**: `~/.config/Claude/claude_desktop_config.json`

### 2. Create or Edit claude_desktop_config.json

Add the weather server configuration to your `claude_desktop_config.json` file:

```json
{
  "mcpServers": {
    "erlmcp-weather": {
        "command": "erl",
        "args": [
            "-pa",
            "<ABS PATH>/erlsci/erlmcp/_build/weather/lib/erlmcp/ebin",
            "-pa",
            "<ABS PATH>/erlsci/erlmcp/_build/weather/lib/jsx/ebin",
            "-pa",
            "<ABS PATH>/erlsci/erlmcp/_build/weather/lib/jesse/ebin",
            "-eval",
            "code:load_file(weather_server_stdio), weather_server_stdio:start()",
            "-noshell"
        ],
        "cwd": "'$(pwd)'"
    }
  }
}
```

**Important**: Replace `<ABS PATH>` with the actual absolute path to your compiled erlmcp library.

### 3. Restart Claude Desktop

After editing the configuration, completely restart Claude Desktop for the changes to take effect.

## Usage Examples

### Current Weather

```
User: Use the get_weather tool to get current conditions in New York, NY

Claude: I'll get the current weather conditions for New York, NY.

[Claude calls the get_weather tool with {"location": "New York, NY"}]

The current weather in New York, NY shows:
- Temperature: 22.0°C (71.6°F)
- Condition: Partly Cloudy
- Humidity: 65%
- Wind: NE 12 km/h
- Last updated: 2024-01-15 14:30:00
```

### 5-Day Forecast

```
User: Use the get_forecast tool to get a 5-day forecast for London, UK

Claude: I'll get the 5-day weather forecast for London, UK.

[Claude calls the get_forecast tool with {"location": "London, UK"}]

Here's the 5-day forecast for London, UK:
- Today: 18.0°C (64.4°F) - Cloudy
- Tomorrow: 20.0°C (68.0°F) - Sunny
- Wednesday: 16.0°C (60.8°F) - Light Rain
- Thursday: 19.0°C (66.2°F) - Partly Cloudy
- Friday: 17.0°C (62.6°F) - Thunderstorm
```

### Temperature Conversion

```
User: Use the convert_temperature tool to convert 25°C to Fahrenheit

Claude: I'll convert 25°C to Fahrenheit for you.

[Claude calls the convert_temperature tool with {"temperature": 25, "from_unit": "celsius", "to_unit": "fahrenheit"}]

25.00°CELSIUS = 77.00°FAHRENHEIT
```

### Weather Alerts

```
User: Use the get_weather_alerts tool to check for alerts in Miami, FL

Claude: I'll check for active weather alerts in Miami, FL.

[Claude calls the get_weather_alerts tool with {"location": "Miami, FL"}]

Active Weather Alerts for Miami, FL:

⚠️  Heat Warning
Extreme heat expected. Stay hydrated and avoid outdoor activities.
```

### Reading Resources

```
User: Read the weather://help resource

Claude: I'll read the weather help resource for you.

[Claude reads the weather://help resource]

The weather help resource provides comprehensive information about:
- Available tools: get_weather, get_forecast, get_weather_alerts, convert_temperature
- Usage examples and parameter formats
- Supported temperature units and location formats
```

### Using Prompts

```
User: Use the weather_report prompt to create a forecast report for Seattle

Claude: I'll generate a weather report prompt for Seattle.

[Claude uses the weather_report prompt with {"location": "Seattle", "type": "forecast"}]

Please provide a 5-day weather forecast for Seattle. Include temperature, humidity, wind conditions, and any relevant weather alerts.
```

```
User: Use the weather_advice prompt for hiking in Denver

Claude: I'll generate a weather advice prompt for hiking in Denver.

[Claude uses the weather_advice prompt with {"location": "Denver", "activity": "hiking"}]

Based on the current weather conditions in Denver, please provide advice for hiking. Include recommendations for clothing, timing, and any precautions.
```

## Supported Location Formats

The weather server supports various location formats:

- **City names**: "New York", "London", "Tokyo"
- **City, State**: "Miami, FL", "Seattle, WA"
- **City, Country**: "London, UK", "Sydney, Australia"
- **Coordinates**: "40.7128,-74.0060" (latitude, longitude)
- **ZIP codes**: "10001", "90210"
- **International postal codes**: "SW1A 1AA, UK"

## Temperature Units

Supported temperature units for conversion:
- **celsius** (C)
- **fahrenheit** (F)
- **kelvin** (K)

## Mock Data System

The weather server uses a sophisticated mock data system for demonstration purposes:

### Consistent Data Generation
- Uses location-based hashing for consistent results
- Same location always returns same weather data
- Different locations return different weather patterns

### Realistic Weather Patterns
- Temperature ranges: 10-40°C (50-104°F)
- Varied weather conditions: Sunny, Cloudy, Rainy, Snowy, etc.
- Realistic humidity levels: 30-90%
- Wind speeds and directions
- Seasonal and geographical variations

### Alert System
- Heat warnings for certain locations
- Thunderstorm watches
- Winter storm advisories
- Location-based alert probability

## Error Handling

The weather server includes comprehensive error handling:

- **Invalid locations**: Gracefully handles empty or invalid location strings
- **Invalid temperature units**: Clear error messages for unsupported units
- **Conversion errors**: Detailed error reporting for temperature conversions
- **Service errors**: Robust error handling for all API calls

## Implementation Details

### Architecture

- **stdio Transport**: Uses standard input/output for MCP communication
- **JSON-RPC**: Follows MCP protocol specification
- **Modular Design**: Separate modules for different weather functions
- **Extensible**: Easy to add new weather tools and data sources

### Key Features

1. **Real-time Mock Data**: Generates consistent, realistic weather data
2. **Multi-format Support**: Handles various location and temperature formats
3. **Educational Resources**: Comprehensive help and documentation
4. **Prompt Templates**: Ready-to-use prompts for common weather tasks
5. **Error Recovery**: Graceful handling of all error conditions

### Data Sources

Currently uses mock data generators, but the architecture supports:
- Real weather API integration
- Historical weather data
- Satellite imagery
- Radar data
- Weather station feeds

## Extending the Weather Server

To add new weather functionality:

1. **New Weather Tools**: Add additional weather-related tools
2. **Real API Integration**: Replace mock data with real weather APIs
3. **Advanced Features**: Add UV index, air quality, astronomical data
4. **Localization**: Support for different languages and units
5. **Historical Data**: Add historical weather queries

Example of adding a new tool:

```erlang
ok = erlmcp_stdio:add_tool(<<"get_air_quality">>, <<"Get air quality index">>,
    fun(#{<<"location">> := Location}) ->
        get_air_quality(Location)
    end,
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"location">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Location name">>}
        },
        <<"required">> => [<<"location">>]
    }),
```

## Troubleshooting

### Common Issues

1. **Server not starting**: Check that erlmcp is properly compiled and paths are correct
2. **Tool not found**: Verify tool names match exactly (case-sensitive)
3. **Invalid parameters**: Check that JSON schema requirements are met
4. **Connection errors**: Ensure Claude Desktop was restarted after configuration

### Debug Mode

Enable debug logging by modifying the logger configuration:

```erlang
logger:set_primary_config(level, debug),
```

### Testing Individual Components

Test weather functions manually:

```bash
erl -pa _build/default/lib/erlmcp/ebin
1> weather_test:test_get_weather().
2> weather_test:test_convert_temperature().
```

## Real Weather API Integration

To integrate with real weather APIs (OpenWeatherMap, WeatherAPI, etc.):

1. **API Key Configuration**: Add API key management
2. **HTTP Client**: Add HTTP client for API calls
3. **Rate Limiting**: Implement API rate limiting
4. **Caching**: Add response caching for efficiency
5. **Error Handling**: Handle API errors and fallbacks

Example API integration:

```erlang
get_real_weather(Location) ->
    ApiKey = get_api_key(),
    Url = build_weather_url(Location, ApiKey),
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            parse_weather_response(Body);
        {error, Reason} ->
            {error, Reason}
    end.
```

## Performance Considerations

- **Caching**: Implement weather data caching
- **Batch Requests**: Support multiple location queries
- **Async Operations**: Use async calls for better performance
- **Memory Management**: Efficient data structures for large datasets

## License

This weather server is built on the erlmcp library and follows the same licensing terms.
