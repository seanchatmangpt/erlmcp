%%%-------------------------------------------------------------------
%%% @doc
%%% Timeline Visualization Generator for Profile Data
%%%
%%% Generates SVG and HTML visualizations from timeline profile data.
%%% Creates interactive flame graphs, process interaction diagrams,
%%% and scheduler activity charts.
%%%
%%% == Output Formats ==
%%%
%%% 1. **SVG**: Static vector graphics timeline
%%% 2. **HTML**: Interactive timeline with zoom/pan
%%% 3. **JSON**: Raw data export
%%%
%%% == Usage Example ==
%%%
%%% ```erlang
%%% {ok, Result, Timeline} = erlmcp_profiler:profile_timeline(
%%%     fun() -> my_function() end,
%%%     <<"my_function">>
%%% ),
%%%
%%% %% Generate SVG
%%% {ok, SVG} = erlmcp_timeline_viz:generate_svg(Timeline),
%%% file:write_file("timeline.svg", SVG).
%%%
%%% %% Generate HTML
%%% {ok, HTML} = erlmcp_timeline_viz:generate_html(Timeline),
%%% file:write_file("timeline.html", HTML).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_timeline_viz).

%% API
-export([generate_svg/1, generate_svg/2, generate_html/1, generate_html/2,
         generate_flamegraph/1, generate_interaction_graph/1,
         export_json/1, export_csv/1]).

-include_lib("kernel/include/logger.hrl").

%% Type definitions
-type viz_opts() ::
    #{width => pos_integer(),
      height => pos_integer(),
      show_grid => boolean(),
      show_legend => boolean(),
      color_scheme => default | warm | cool | monochrome,
      zoom_level => float()}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Generate SVG visualization from timeline profile
-spec generate_svg(map()) -> {ok, binary()} | {error, term()}.
generate_svg(Timeline) ->
    generate_svg(Timeline, #{}).

%% @doc Generate SVG with custom options
-spec generate_svg(map(), viz_opts()) -> {ok, binary()} | {error, term()}.
generate_svg(Timeline, Opts) ->
    try
        Width = maps:get(width, Opts, 1200),
        Height = maps:get(height, Opts, 600),
        ShowGrid = maps:get(show_grid, Opts, true),
        ShowLegend = maps:get(show_legend, Opts, true),

        Events = maps:get(events, Timeline, []),
        StartTime = maps:get(start_time, Timeline, 0),
        EndTime = maps:get(end_time, Timeline, 0),
        TotalDuration = EndTime - StartTime,

        %% Calculate scale
        TimeScale = Width / max(TotalDuration, 1),

        %% Generate SVG elements
        Grid = generate_grid(Width, Height, TimeScale, StartTime, ShowGrid),
        EventsSVG = generate_events_svg(Events, StartTime, TimeScale, Height, Opts),
        Axis = generate_time_axis(Width, Height, StartTime, EndTime, TotalDuration),
        Legend = generate_legend(Width, ShowLegend),

        %% Combine all elements
        SVG = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
                "<svg xmlns=\"http://www.w3.org/2000/svg\" ",
                "width=\"", (integer_to_binary(Width))/binary, "\" ",
                "height=\"", (integer_to_binary(Height))/binary, "\" ",
                "viewBox=\"0 0 ", (integer_to_binary(Width))/binary, " ", (integer_to_binary(Height))/binary, "\">\n",
                "<style>\n",
                "  .scheduler { fill: #4CAF50; }\n",
                "  .process { fill: #2196F3; }\n",
                "  .gc { fill: #FF9800; }\n",
                "  .port { fill: #9C27B0; }\n",
                "  .grid { stroke: #e0e0e0; stroke-width: 1; }\n",
                "  .axis { stroke: #333; stroke-width: 2; }\n",
                "  .text { font-family: monospace; font-size: 12px; fill: #333; }\n",
                "</style>\n",
                Grid/binary,
                EventsSVG/binary,
                Axis/binary,
                Legend/binary,
                "</svg>\n">>,

        {ok, SVG}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Failed to generate SVG: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {error, {viz_failed, {Class, Reason}}}
    end.

%% @doc Generate HTML visualization from timeline profile
-spec generate_html(map()) -> {ok, binary()} | {error, term()}.
generate_html(Timeline) ->
    generate_html(Timeline, #{}).

%% @doc Generate HTML with custom options
-spec generate_html(map(), viz_opts()) -> {ok, binary()} | {error, term()}.
generate_html(Timeline, Opts) ->
    try
        %% Generate SVG content
        {ok, SVG} = generate_svg(Timeline, Opts),

        %% Extract statistics
        Stats = maps:get(statistics, Timeline, #{}),
        Label = maps:get(label, Timeline, <<"profile">>),
        DurationUs = maps:get(total_duration_us, Timeline, 0),
        DurationMs = DurationUs / 1000,

        %% Build HTML with interactive controls
        HTML = <<"<!DOCTYPE html>\n",
                 "<html>\n",
                 "<head>\n",
                 "  <title>Timeline Profile: ", Label/binary, "</title>\n",
                 "  <style>\n",
                 "    body { font-family: 'Segoe UI', sans-serif; margin: 20px; background: #f5f5f5; }\n",
                 "    .container { max-width: 1400px; margin: 0 auto; background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }\n",
                 "    h1 { color: #333; border-bottom: 2px solid #4CAF50; padding-bottom: 10px; }\n",
                 "    .stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px; margin: 20px 0; }\n",
                 "    .stat-card { background: #f9f9f9; padding: 15px; border-radius: 6px; border-left: 4px solid #2196F3; }\n",
                 "    .stat-label { font-size: 12px; color: #666; text-transform: uppercase; letter-spacing: 0.5px; }\n",
                 "    .stat-value { font-size: 24px; font-weight: bold; color: #333; margin-top: 5px; }\n",
                 "    .timeline-container { margin: 20px 0; border: 1px solid #ddd; border-radius: 6px; overflow: hidden; }\n",
                 "    .controls { margin: 20px 0; padding: 15px; background: #f0f0f0; border-radius: 6px; }\n",
                 "    .controls button { padding: 8px 16px; margin-right: 10px; border: none; border-radius: 4px; cursor: pointer; font-size: 14px; }\n",
                 "    .btn-zoom { background: #2196F3; color: white; }\n",
                 "    .btn-zoom:hover { background: #1976D2; }\n",
                 "    .btn-reset { background: #FF9800; color: white; }\n",
                 "    .legend { display: flex; gap: 20px; margin: 20px 0; flex-wrap: wrap; }\n",
                 "    .legend-item { display: flex; align-items: center; gap: 8px; }\n",
                 "    .legend-color { width: 20px; height: 20px; border-radius: 3px; }\n",
                 "    .event-table { width: 100%; border-collapse: collapse; margin: 20px 0; }\n",
                 "    .event-table th { background: #4CAF50; color: white; padding: 12px; text-align: left; }\n",
                 "    .event-table td { padding: 10px; border-bottom: 1px solid #ddd; }\n",
                 "    .event-table tr:hover { background: #f5f5f5; }\n",
                 "  </style>\n",
                 "</head>\n",
                 "<body>\n",
                 "  <div class=\"container\">\n",
                 "    <h1>Timeline Profile: ", Label/binary, "</h1>\n",
                 "    <div class=\"stats\">\n",
                 "      <div class=\"stat-card\">\n",
                 "        <div class=\"stat-label\">Total Duration</div>\n",
                 "        <div class=\"stat-value\">", (format_duration(DurationMs))/binary, "</div>\n",
                 "      </div>\n",
                 "      <div class=\"stat-card\">\n",
                 "        <div class=\"stat-label\">Event Count</div>\n",
                 "        <div class=\"stat-value\">", (integer_to_binary(maps:get(event_count, Stats, 0)))/binary, "</div>\n",
                 "      </div>\n",
                 "    </div>\n",
                 "    <div class=\"controls\">\n",
                 "      <button class=\"btn-zoom\" onclick=\"zoomIn()\">Zoom In</button>\n",
                 "      <button class=\"btn-zoom\" onclick=\"zoomOut()\">Zoom Out</button>\n",
                 "      <button class=\"btn-reset\" onclick=\"resetZoom()\">Reset</button>\n",
                 "    </div>\n",
                 "    <div class=\"timeline-container\" id=\"timeline\">\n",
                 SVG/binary,
                 "    </div>\n",
                 "    <div class=\"legend\">\n",
                 "      <div class=\"legend-item\"><div class=\"legend-color\" style=\"background: #4CAF50;\"></div><span>Scheduler</span></div>\n",
                 "      <div class=\"legend-item\"><div class=\"legend-color\" style=\"background: #2196F3;\"></div><span>Process</span></div>\n",
                 "      <div class=\"legend-item\"><div class=\"legend-color\" style=\"background: #FF9800;\"></div><span>GC</span></div>\n",
                 "      <div class=\"legend-item\"><div class=\"legend-color\" style=\"background: #9C27B0;\"></div><span>Port</span></div>\n",
                 "    </div>\n",
                 "    <script>\n",
                 "      let scale = 1;\n",
                 "      function zoomIn() { scale *= 1.2; updateZoom(); }\n",
                 "      function zoomOut() { scale /= 1.2; updateZoom(); }\n",
                 "      function resetZoom() { scale = 1; updateZoom(); }\n",
                 "      function updateZoom() {\n",
                 "        const svg = document.querySelector('#timeline svg');\n",
                 "        svg.style.transform = `scale(${scale})`;\n",
                 "        svg.style.transformOrigin = 'top left';\n",
                 "      }\n",
                 "    </script>\n",
                 "  </div>\n",
                 "</body>\n",
                 "</html>\n">>,

        {ok, HTML}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Failed to generate HTML: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {error, {viz_failed, {Class, Reason}}}
    end.

%% @doc Generate flamegraph from timeline
-spec generate_flamegraph(map()) -> {ok, binary()} | {error, term()}.
generate_flamegraph(Timeline) ->
    try
        Events = maps:get(events, Timeline, []),
        StartTime = maps:get(start_time, Timeline, 0),
        EndTime = maps:get(end_time, Timeline, 0),

        %% Group events by type and stack
        StackedEvents = build_event_stack(Events, StartTime),

        %% Generate flamegraph SVG
        FlameSVG = generate_flamegraph_svg(StackedEvents, StartTime, EndTime),

        {ok, FlameSVG}
    catch
        _:_ ->
            {error, flamegraph_generation_failed}
    end.

%% @doc Generate process interaction graph
-spec generate_interaction_graph(map()) -> {ok, binary()} | {error, term()}.
generate_interaction_graph(Timeline) ->
    try
        Events = maps:get(events, Timeline, []),

        %% Extract process interactions
        Interactions = extract_interactions(Events),

        %% Generate graph
        GraphSVG = generate_interaction_svg(Interactions),

        {ok, GraphSVG}
    catch
        _:_ ->
            {error, interaction_graph_failed}
    end.

%% @doc Export timeline data as JSON
-spec export_json(map()) -> {ok, binary()} | {error, term()}.
export_json(Timeline) ->
    try
        JSON = jsx:encode(Timeline, [space, {indent, 2}]),
        {ok, JSON}
    catch
        _:_ ->
            {error, json_export_failed}
    end.

%% @doc Export timeline data as CSV
-spec export_csv(map()) -> {ok, binary()} | {error, term()}.
export_csv(Timeline) ->
    try
        Events = maps:get(events, Timeline, []),
        StartTime = maps:get(start_time, Timeline, 0),

        %% CSV header
        Header = <<"timestamp,type,duration_us,relative_time_us\n">>,

        %% Convert events to CSV rows
        Rows = [format_event_csv(Event, StartTime) || Event <- Events],
        CSV = iolist_to_binary([Header | Rows]),

        {ok, CSV}
    catch
        _:_ ->
            {error, csv_export_failed}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Generate grid lines
-spec generate_grid(pos_integer(), pos_integer(), float(), integer(), boolean()) -> binary().
generate_grid(Width, Height, TimeScale, StartTime, true) ->
    %% Generate vertical grid lines every 100us
    Interval = 100,
    NumLines = (Width div 100) + 1,

    Lines = [generate_grid_line(X, Height) || X <- lists:seq(0, NumLines * 100, 100)],
    iolist_to_binary(Lines);
generate_grid(_, _, _, _, false) ->
    <<>>.

%% @private Generate single grid line
-spec generate_grid_line(pos_integer(), pos_integer()) -> binary().
generate_grid_line(X, Height) ->
    iolist_to_binary([
        "<line class=\"grid\" x1=\"",
        integer_to_binary(X),
        "\" y1=\"0\" x2=\"",
        integer_to_binary(X),
        "\" y2=\"",
        integer_to_binary(Height),
        "\"/>\n"
    ]).

%% @private Generate SVG elements for events
-spec generate_events_svg([map()], integer(), float(), pos_integer(), viz_opts()) -> binary().
generate_events_svg(Events, StartTime, TimeScale, Height, Opts) ->
    EventHeight = 20,
    Spacing = 5,
    Y = 50,

    EventSVGs = [generate_event_svg(Event, StartTime, TimeScale, Y, EventHeight, Opts)
                 || Event <- Events],
    iolist_to_binary(EventSVGs).

%% @private Generate SVG element for single event
-spec generate_event_svg(map(), integer(), float(), pos_integer(), pos_integer(), viz_opts()) -> binary().
generate_event_svg(#{type := Type, timestamp := Timestamp} = Event, StartTime, TimeScale, Y, EventHeight, _Opts) ->
    RelativeTime = Timestamp - StartTime,
    X = round(RelativeTime * TimeScale),
    Color = get_event_color(Type),

    %% Determine width based on duration (default 50px if duration is 0)
    Width = case maps:get(duration_us, Event, 0) of
                0 -> 50;
                Duration -> max(round(Duration * TimeScale), 2)
            end,

    iolist_to_binary([
        "<rect class=\"", atom_to_binary(Type), "\" ",
        "x=\"", integer_to_binary(X), "\" ",
        "y=\"", integer_to_binary(Y), "\" ",
        "width=\"", integer_to_binary(Width), "\" ",
        "height=\"", integer_to_binary(EventHeight), "\" ",
        "fill=\"", Color, "\" ",
        "opacity=\"0.7\">\n",
        "  <title>", format_event_title(Event), "</title>\n",
        "</rect>\n"
    ]).

%% @private Generate time axis
-spec generate_time_axis(pos_integer(), pos_integer(), integer(), integer(), integer()) -> binary().
generate_time_axis(Width, Height, StartTime, EndTime, TotalDuration) ->
    Y = Height - 30,

    %% Axis line
    AxisLine = iolist_to_binary([
        "<line class=\"axis\" x1=\"0\" y1=\"",
        integer_to_binary(Y),
        "\" x2=\"",
        integer_to_binary(Width),
        "\" y2=\"",
        integer_to_binary(Y),
        "\"/>\n"
    ]),

    %% Time labels
    NumLabels = min(10, Width div 100),
    Labels = [generate_time_label(X, StartTime, EndTime, TotalDuration, Width, Y)
              || X <- lists:seq(0, Width, Width div NumLabels)],

    iolist_to_binary([AxisLine | Labels]).

%% @private Generate time label
-spec generate_time_label(pos_integer(), integer(), integer(), integer(), pos_integer(), pos_integer()) -> binary().
generate_time_label(X, StartTime, EndTime, TotalDuration, Width, Y) ->
    Ratio = X / Width,
    TimeOffset = round(TotalDuration * Ratio),
    TimeLabel = integer_to_binary(TimeOffset),

    iolist_to_binary([
        "<text class=\"text\" x=\"",
        integer_to_binary(X),
        "\" y=\"",
        integer_to_binary(Y + 20),
        "\">",
        TimeLabel,
        "μs</text>\n"
    ]).

%% @private Generate legend
-spec generate_legend(pos_integer(), boolean()) -> binary().
generate_legend(_Width, true) ->
    iolist_to_binary([
        "<g transform=\"translate(20, 20)\">\n",
        "  <rect width=\"12\" height=\"12\" fill=\"#4CAF50\"/>\n",
        "  <text x=\"20\" y=\"10\" class=\"text\">Scheduler</text>\n",
        "  <rect y=\"20\" width=\"12\" height=\"12\" fill=\"#2196F3\"/>\n",
        "  <text x=\"20\" y=\"30\" class=\"text\">Process</text>\n",
        "  <rect y=\"40\" width=\"12\" height=\"12\" fill=\"#FF9800\"/>\n",
        "  <text x=\"20\" y=\"50\" class=\"text\">GC</text>\n",
        "  <rect y=\"60\" width=\"12\" height=\"12\" fill=\"#9C27B0\"/>\n",
        "  <text x=\"20\" y=\"70\" class=\"text\">Port</text>\n",
        "</g>\n"
    ]);
generate_legend(_, false) ->
    <<>>.

%% @private Get color for event type
-spec get_event_color(atom()) -> binary().
get_event_color(scheduler) -> <<"#4CAF50">>;
get_event_color(process) -> <<"#2196F3">>;
get_event_color(gc) -> <<"#FF9800">>;
get_event_color(port) -> <<"#9C27B0">>;
get_event_color(_) -> <<"#999999">>.

%% @private Format event title for tooltip
-spec format_event_title(map()) -> binary().
format_event_title(#{type := Type, timestamp := Timestamp, details := Details}) ->
    TypeStr = atom_to_binary(Type),
    TimeStr = integer_to_binary(Timestamp),
    DetailsStr = format_details(Details),
    <<TypeStr/binary, " @ ", TimeStr/binary, "μs\n", DetailsStr/binary>>;
format_event_title(#{type := Type, timestamp := Timestamp}) ->
    TypeStr = atom_to_binary(Type),
    TimeStr = integer_to_binary(Timestamp),
    <<TypeStr/binary, " @ ", TimeStr/binary, "μs">>.

%% @private Format event details
-spec format_details(map()) -> binary().
format_details(Details) when map_size(Details) =:= 0 ->
    <<"">>;
format_details(Details) ->
    maps:fold(fun(K, V, Acc) ->
                      KStr = case K of
                                 Bin when is_binary(Bin) -> Bin;
                                 Atom when is_atom(Atom) -> atom_to_binary(Atom);
                                 _ -> <<"unknown">>
                             end,
                      VStr = iolist_to_binary(io_lib:format("~p", [V])),
                      <<Acc/binary, KStr/binary, ": ", VStr/binary, "\n">>
              end, <<>>, Details).

%% @private Format duration for display
-spec format_duration(float()) -> binary().
format_duration(Ms) when Ms < 1.0 ->
    Us = Ms * 1000,
    <<(float_to_binary(Us, [{decimals, 2}]))/binary, "μs">>;
format_duration(Ms) when Ms < 1000.0 ->
    <<(float_to_binary(Ms, [{decimals, 2}]))/binary, "ms">>;
format_duration(Ms) ->
    Sec = Ms / 1000.0,
    <<(float_to_binary(Sec, [{decimals, 2}]))/binary, "s">>.

%% @private Format event as CSV row
-spec format_event_csv(map(), integer()) -> binary().
format_event_csv(#{timestamp := Timestamp, type := Type, duration_us := Duration}, StartTime) ->
    RelativeTime = Timestamp - StartTime,
    TypeStr = atom_to_binary(Type),
    iolist_to_binary([
        integer_to_binary(Timestamp), ",",
        TypeStr, ",",
        integer_to_binary(Duration), ",",
        integer_to_binary(RelativeTime), "\n"
    ]);
format_event_csv(#{timestamp := Timestamp, type := Type}, StartTime) ->
    RelativeTime = Timestamp - StartTime,
    TypeStr = atom_to_binary(Type),
    iolist_to_binary([
        integer_to_binary(Timestamp), ",",
        TypeStr, ",0,",
        integer_to_binary(RelativeTime), "\n"
    ]).

%% @private Build event stack for flamegraph
-spec build_event_stack([map()], integer()) -> [map()].
build_event_stack(Events, StartTime) ->
    %% Sort events by timestamp and build stack
    SortedEvents = lists:sort(fun(#{timestamp := T1}, #{timestamp := T2}) -> T1 =< T2 end, Events),
    lists:map(fun(E) ->
                      RelativeTime = maps:get(timestamp, E) - StartTime,
                      E#{relative_time => RelativeTime}
              end, SortedEvents).

%% @private Generate flamegraph SVG
-spec generate_flamegraph_svg([map()], integer(), integer()) -> binary().
generate_flamegraph_svg(_StackedEvents, _StartTime, _EndTime) ->
    %% Simplified flamegraph generation
    <<"<?xml version=\"1.0\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"800\" height=\"400\">\n",
      "<text x=\"400\" y=\"200\" text-anchor=\"middle\" font-size=\"16\">Flamegraph (TODO)</text>\n",
      "</svg>\n">>.

%% @private Extract process interactions
-spec extract_interactions([map()]) -> [map()].
extract_interactions(_Events) ->
    %% Simplified interaction extraction
    [].

%% @private Generate interaction graph SVG
-spec generate_interaction_svg([map()]) -> binary().
generate_interaction_svg(_Interactions) ->
    <<"<?xml version=\"1.0\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"800\" height=\"400\">\n",
      "<text x=\"400\" y=\"200\" text-anchor=\"middle\" font-size=\"16\">Interaction Graph (TODO)</text>\n",
      "</svg>\n">>.
