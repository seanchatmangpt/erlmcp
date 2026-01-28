-module(weather_server_stdio).
-export([start/0, main/1]).

start() ->
    main([]).

main(_Args) ->
    %% Start the erlmcp applications first (v2.0 umbrella structure)
    case application:ensure_all_started(erlmcp_core) of
        {ok, _StartedCore} ->
            logger:info("Successfully started erlmcp_core application~n");
        {error, Reason} ->
            logger:error("Failed to start erlmcp_core application: ~p~n", [Reason]),
            halt(1)
    end,
    case application:ensure_all_started(erlmcp_transports) of
        {ok, _StartedTransports} ->
            logger:info("Successfully started erlmcp_transports application~n");
        {error, TransportReason} ->
            logger:error("Failed to start erlmcp_transports application: ~p~n", [TransportReason]),
            halt(1)
    end,

    %% Enable debug logging
    logger:remove_handler(default),
    logger:add_handler(stderr_handler, logger_std_h, #{
        level => info,
        config => #{type => standard_error}
    }),
    logger:set_primary_config(level, info),

    logger:info("Starting Weather MCP server...~n"),

    %% Start the stdio MCP server
    case erlmcp_stdio:start() of
        ok ->
            logger:info("Successfully started stdio server~n"),
            setup_weather_server(),
            logger:info("Weather server setup complete, waiting for shutdown...~n"),
            wait_for_shutdown();
        {error, StartErrReason} ->
            logger:error("Failed to start stdio server: ~p", [StartErrReason]),
            halt(1)
    end.

setup_weather_server() ->
    %% Add weather tools
    ok = erlmcp_stdio:add_tool(<<"get_weather">>, <<"Get current weather for a location">>,
        fun(#{<<"location">> := Location}) ->
            get_weather(Location)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"location">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Location name (city, state/country)">>}
            },
            <<"required">> => [<<"location">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"get_forecast">>, <<"Get 5-day weather forecast for a location">>,
        fun(#{<<"location">> := Location}) ->
            get_forecast(Location)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"location">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Location name (city, state/country)">>}
            },
            <<"required">> => [<<"location">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"get_weather_alerts">>, <<"Get weather alerts for a location">>,
        fun(#{<<"location">> := Location}) ->
            get_weather_alerts(Location)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"location">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Location name (city, state/country)">>}
            },
            <<"required">> => [<<"location">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"convert_temperature">>, <<"Convert temperature between units">>,
        fun(#{<<"temperature">> := Temp, <<"from_unit">> := FromUnit, <<"to_unit">> := ToUnit}) ->
            convert_temperature(Temp, FromUnit, ToUnit)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"temperature">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Temperature value">>},
                <<"from_unit">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Source unit: celsius, fahrenheit, kelvin">>},
                <<"to_unit">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Target unit: celsius, fahrenheit, kelvin">>}
            },
            <<"required">> => [<<"temperature">>, <<"from_unit">>, <<"to_unit">>]
        }),

    %% Add weather resources
    ok = erlmcp_stdio:add_resource(<<"weather://locations">>, <<"Supported Weather Locations">>,
        fun(_Uri) ->
            <<"Supported locations include:\n"
              "- Major cities: New York, London, Tokyo, Sydney, Mumbai\n"
              "- US states: California, Texas, Florida, New York\n"
              "- Countries: United States, United Kingdom, Canada, Australia\n"
              "- Coordinates: lat,lon format (e.g., 40.7128,-74.0060)\n"
              "- Zip codes: US zip codes (e.g., 10001, 90210)\n"
              "- International postal codes with country (e.g., SW1A 1AA, UK)\n">>
        end,
        <<"text/plain">>),

    ok = erlmcp_stdio:add_resource(<<"weather://help">>, <<"Weather API Help">>,
        fun(_Uri) ->
            <<"Weather MCP Server Help\n\n"
              "Available Tools:\n"
              "- get_weather: Get current weather conditions\n"
              "- get_forecast: Get 5-day weather forecast\n"
              "- get_weather_alerts: Get active weather alerts\n"
              "- convert_temperature: Convert between temperature units\n\n"
              "Usage Examples:\n"
              "- Current weather: {\"location\": \"New York, NY\"}\n"
              "- Temperature conversion: {\"temperature\": 25, \"from_unit\": \"celsius\", \"to_unit\": \"fahrenheit\"}\n"
              "- Weather alerts: {\"location\": \"Miami, FL\"}\n\n"
              "Supported temperature units: celsius, fahrenheit, kelvin\n"
              "Location formats: City names, coordinates, zip codes\n">>
        end,
        <<"text/plain">>),

    ok = erlmcp_stdio:add_resource(<<"weather://status">>, <<"Weather Service Status">>,
        fun(_Uri) ->
            Status = get_service_status(),
            iolist_to_binary(io_lib:format("Weather Service Status: ~s~n"
                                           "Last Updated: ~s~n"
                                           "API Calls Today: ~p~n"
                                           "Service Uptime: ~s~n",
                                           [Status, get_current_time(), get_api_calls_today(), get_uptime()]))
        end,
        <<"text/plain">>),

    %% Add weather prompts
    ok = erlmcp_stdio:add_prompt(<<"weather_report">>, <<"Generate a weather report prompt">>,
        fun(Args) ->
            Location = maps:get(<<"location">>, Args, <<"your location">>),
            Type = maps:get(<<"type">>, Args, <<"current">>),
            
            ReportType = case Type of
                <<"current">> -> <<"current weather conditions">>;
                <<"forecast">> -> <<"5-day weather forecast">>;
                <<"detailed">> -> <<"detailed weather analysis">>;
                _ -> <<"weather information">>
            end,
            
            [#{
                <<"role">> => <<"user">>,
                <<"content">> => #{
                    <<"type">> => <<"text">>,
                    <<"text">> => <<"Please provide a ", ReportType/binary, " for ", Location/binary, 
                                   ". Include temperature, humidity, wind conditions, and any relevant weather alerts.">>
                }
            }]
        end,
        [
            #{<<"name">> => <<"location">>, <<"description">> => <<"Location for weather report">>, <<"required">> => true},
            #{<<"name">> => <<"type">>, <<"description">> => <<"Report type: current, forecast, detailed">>, <<"required">> => false}
        ]),

    ok = erlmcp_stdio:add_prompt(<<"weather_advice">>, <<"Generate weather advice prompt">>,
        fun(Args) ->
            Location = maps:get(<<"location">>, Args, <<"your area">>),
            Activity = maps:get(<<"activity">>, Args, <<"outdoor activities">>),
            
            [#{
                <<"role">> => <<"user">>,
                <<"content">> => #{
                    <<"type">> => <<"text">>,
                    <<"text">> => <<"Based on the current weather conditions in ", Location/binary, 
                                   ", please provide advice for ", Activity/binary, 
                                   ". Include recommendations for clothing, timing, and any precautions.">>
                }
            }]
        end,
        [
            #{<<"name">> => <<"location">>, <<"description">> => <<"Location for weather advice">>, <<"required">> => true},
            #{<<"name">> => <<"activity">>, <<"description">> => <<"Planned activity">>, <<"required">> => false}
        ]),

    logger:info("Weather server configured with tools, resources, and prompts~n").

%% Weather API Functions (Mock implementations for demo)
get_weather(Location) ->
    LocationStr = binary_to_list(Location),
    % Mock weather data based on location
    {Temp, Condition, Humidity, Wind} = generate_mock_weather(LocationStr),
    
    iolist_to_binary(io_lib:format(
        "Current weather in ~s:~n"
        "Temperature: ~.1f°C (~.1f°F)~n"
        "Condition: ~s~n"
        "Humidity: ~p%~n"
        "Wind: ~s~n"
        "Last updated: ~s~n",
        [LocationStr, Temp, celsius_to_fahrenheit(Temp), Condition, Humidity, Wind, get_current_time()]
    )).

get_forecast(Location) ->
    LocationStr = binary_to_list(Location),
    Forecast = generate_mock_forecast(LocationStr),
    
    Header = io_lib:format("5-Day Weather Forecast for ~s:~n~n", [LocationStr]),
    ForecastText = [format_forecast_day(Day) || Day <- Forecast],
    
    iolist_to_binary([Header, ForecastText]).

get_weather_alerts(Location) ->
    LocationStr = binary_to_list(Location),
    Alerts = generate_mock_alerts(LocationStr),
    
    case Alerts of
        [] ->
            iolist_to_binary(io_lib:format("No active weather alerts for ~s~n", [LocationStr]));
        _ ->
            Header = io_lib:format("Active Weather Alerts for ~s:~n~n", [LocationStr]),
            AlertsText = [format_alert(Alert) || Alert <- Alerts],
            iolist_to_binary([Header, AlertsText])
    end.

convert_temperature(Temp, FromUnit, ToUnit) ->
    FromUnitStr = binary_to_list(string:lowercase(FromUnit)),
    ToUnitStr = binary_to_list(string:lowercase(ToUnit)),
    
    try
        % Convert to Celsius first
        TempC = case FromUnitStr of
            "celsius" -> Temp;
            "fahrenheit" -> (Temp - 32) * 5/9;
            "kelvin" -> Temp - 273.15;
            _ -> throw({invalid_unit, FromUnit})
        end,
        
        % Convert from Celsius to target unit
        Result = case ToUnitStr of
            "celsius" -> TempC;
            "fahrenheit" -> TempC * 9/5 + 32;
            "kelvin" -> TempC + 273.15;
            _ -> throw({invalid_unit, ToUnit})
        end,
        
        % Use simple formatting without special characters
        FromUnitUpper = string:to_upper(FromUnitStr),
        ToUnitUpper = string:to_upper(ToUnitStr),
        
        iolist_to_binary(io_lib:format("~.2f ~s = ~.2f ~s~n", 
            [float(Temp), FromUnitUpper, float(Result), ToUnitUpper]))
    catch
        throw:{invalid_unit, Unit} ->
            iolist_to_binary(io_lib:format("Error: Invalid temperature unit '~s'. "
                                           "Supported units: celsius, fahrenheit, kelvin~n", [Unit]));
        _:_ ->
            <<"Error: Invalid temperature conversion parameters">>
    end.

%% Helper functions for mock data generation
generate_mock_weather(Location) ->
    % Simple hash-based mock data for consistent results
    Hash = erlang:phash2(Location, 1000),
    Temp = 10 + (Hash rem 30),  % 10-40°C
    Conditions = ["Sunny", "Partly Cloudy", "Cloudy", "Light Rain", "Heavy Rain", "Snow", "Thunderstorm"],
    Condition = lists:nth((Hash rem length(Conditions)) + 1, Conditions),
    Humidity = 30 + (Hash rem 60),  % 30-90%
    WindSpeed = Hash rem 25,  % 0-25 km/h
    WindDir = ["N", "NE", "E", "SE", "S", "SW", "W", "NW"],
    Wind = lists:nth((Hash rem length(WindDir)) + 1, WindDir) ++ io_lib:format(" ~p km/h", [WindSpeed]),
    
    {float(Temp), Condition, Humidity, Wind}.

generate_mock_forecast(Location) ->
    Days = ["Today", "Tomorrow", "Wednesday", "Thursday", "Friday"],
    [begin
        DayHash = erlang:phash2({Location, Day}, 1000),
        Temp = 8 + (DayHash rem 25),  % 8-33°C
        Conditions = ["Sunny", "Partly Cloudy", "Cloudy", "Light Rain", "Thunderstorm"],
        Condition = lists:nth((DayHash rem length(Conditions)) + 1, Conditions),
        {Day, float(Temp), Condition}
    end || Day <- Days].

generate_mock_alerts(Location) ->
    Hash = erlang:phash2(Location, 100),
    % Generate alerts based on location hash
    case Hash rem 4 of
        0 -> [];  % No alerts
        1 -> [{"Heat Warning", "Extreme heat expected. Stay hydrated and avoid outdoor activities."}];
        2 -> [{"Severe Thunderstorm Watch", "Severe thunderstorms possible. Stay indoors."}];
        3 -> [{"Winter Storm Advisory", "Snow and ice expected. Travel may be hazardous."}]
    end.

format_forecast_day({Day, Temp, Condition}) ->
    io_lib:format("~s: ~.1f°C (~.1f°F) - ~s~n", 
        [Day, Temp, celsius_to_fahrenheit(Temp), Condition]).

format_alert({Type, Description}) ->
    io_lib:format("WARNING: ~s~n~s~n~n", [Type, Description]).

celsius_to_fahrenheit(C) ->
    C * 9/5 + 32.

get_current_time() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", 
        [Year, Month, Day, Hour, Minute, Second]).

get_service_status() ->
    "Active".

get_api_calls_today() ->
    {{Year, Month, Day}, _Time} = calendar:local_time(),
    erlang:phash2({Year, Month, Day}, 1000).

get_uptime() ->
    "99.9%".

wait_for_shutdown() ->
    %% Monitor the stdio server process to know when it's done
    case whereis(erlmcp_stdio_server) of
        undefined ->
            logger:warn("Stdio server not found, exiting~n");
        Pid ->
            monitor(process, Pid),
            receive
                {'DOWN', _Ref, process, Pid, _Reason} ->
                    logger:info("Stdio server terminated, exiting~n")
            end
    end.