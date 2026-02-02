-module(erlmcp_tool_weather).

%% Weather tool for erlmcp_tool_router
%% Fetches weather information for cities

-export([handle/2]).

%% @doc Handle weather tool invocation
-spec handle(binary(), map()) -> {ok, map()} | {error, term()}.
handle(_Args, Captures) ->
    City = maps:get(<<"city">>, Captures, <<"Unknown">>),
    try
        %% Mock weather data
        WeatherData = get_mock_weather(City),
        {ok, #{<<"city">> => City,
                <<"temperature">> => proplists:get_value(temperature, WeatherData),
                <<"conditions">> => proplists:get_value(conditions, WeatherData),
                <<"humidity">> => proplists:get_value(humidity, WeatherData)}}
    catch
        _:_:Stacktrace ->
            {error, {weather_failed, Stacktrace}}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Mock weather data (in production, integrate with weather API)
get_mock_weather(<<"San Francisco">>) ->
    [{temperature, 65}, {conditions, <<"Partly Cloudy">>}, {humidity, 75}];
get_mock_weather(<<"New York">>) ->
    [{temperature, 45}, {conditions, <<"Sunny">>}, {humidity, 60}];
get_mock_weather(<<"Tokyo">>) ->
    [{temperature, 70}, {conditions, <<"Rainy">>}, {humidity, 85}];
get_mock_weather(City) ->
    [{temperature, 20}, {conditions, <<"Unknown">>}, {humidity, 50},
     {message, list_to_binary(["Weather data not available for ", City])}].
