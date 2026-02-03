%%%-------------------------------------------------------------------
%%% @doc
%%% Trace visualization module for erlmcp distributed tracing
%%%
%%% Converts OTP 28 trace events into human-readable formats:
%%% - Text timeline: Chronological view of events
%%% - Call graphs: Tool invocation chains
%%% - Message flow: Inter-process communication
%%% - System events: GC, scheduling, port activity
%%%
%%% == Output Formats ==
%%% - text: Plain text timeline
%%% - json: Structured data for dashboards
%%% - html: Interactive timeline visualization
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_trace_visualizer).

%% API
-export([format_timeline/1, format_timeline/2]).
-export([format_call_graph/1]).
-export([format_message_flow/1]).
-export([format_system_events/1]).
-export([export_trace/2]).
-export([generate_report/1]).
-export([analyze_performance/1]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type trace_event() :: #{timestamp := integer(),
                          event_type := atom(),
                          data := term()}.
-type visualization_format() :: text | json | html.
-type visualization_options() ::
    #{format := visualization_format(),
      detailed := boolean(),
      filter := fun((trace_event()) -> boolean())}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Format trace events as a readable timeline
-spec format_timeline([trace_event()]) -> iolist().
format_timeline(Events) ->
    format_timeline(Events, #{format => text, detailed => true}).

%% @doc Format trace events with custom options
-spec format_timeline([trace_event()], visualization_options()) -> iolist().
format_timeline(Events, Options) ->
    Format = maps:get(format, Options, text),
    SortedEvents = sort_events_chronologically(Events),
    FilteredEvents = filter_events(SortedEvents, Options),

    case Format of
        text ->
            format_text_timeline(FilteredEvents, Options);
        json ->
            format_json_timeline(FilteredEvents, Options);
        html ->
            format_html_timeline(FilteredEvents, Options)
    end.

%% @doc Generate call graph from trace events
%% Shows tool invocation chains and parent-child relationships
-spec format_call_graph([trace_event()]) -> iolist().
format_call_graph(Events) ->
    CallEvents = filter_by_type(Events, call),
    build_call_graph(CallEvents).

%% @doc Visualize message flow between processes
-spec format_message_flow([trace_event()]) -> iolist().
format_message_flow(Events) ->
    SendEvents = filter_by_type(Events, send),
    RecvEvents = filter_by_type(Events, 'receive'),
    build_message_flow(SendEvents, RecvEvents).

%% @doc Format system event monitoring results
%% Shows GC, scheduling, and port activity
-spec format_system_events([trace_event()]) -> iolist().
format_system_events(Events) ->
    SystemEvents = filter_by_type(Events, system_monitor),
    format_system_monitoring(SystemEvents).

%% @doc Export trace data to file
-spec export_trace([trace_event()], file:filename()) -> ok | {error, term()}.
export_trace(Events, Filename) ->
    Format = filename:extension(Filename),
    Output =
        case Format of
            ".json" ->
                format_timeline(Events, #{format => json, detailed => true});
            ".html" ->
                format_timeline(Events, #{format => html, detailed => true});
            _ ->
                format_timeline(Events, #{format => text, detailed => true})
        end,
    file:write_file(Filename, iolist_to_binary(Output)).

%% @doc Generate comprehensive trace analysis report
-spec generate_report([trace_event()]) -> map().
generate_report(Events) ->
    #{timeline => format_text_timeline(Events, #{}),
     call_graph => build_call_graph(filter_by_type(Events, call)),
     message_flow => build_message_flow(filter_by_type(Events, send),
                                        filter_by_type(Events, 'receive')),
     system_events => format_system_monitoring(filter_by_type(Events,
                                                             system_monitor)),
     statistics => calculate_statistics(Events),
     performance => analyze_performance(Events)}.

%% @doc Analyze performance metrics from trace
-spec analyze_performance([trace_event()]) -> map().
analyze_performance(Events) ->
    CallEvents = filter_by_type(Events, call),
    SystemEvents = filter_by_type(Events, system_monitor),

    %% Calculate call statistics
    CallCounts = count_calls(CallEvents),
    AvgCallTime = calculate_avg_call_duration(CallEvents),

    %% System event analysis
    GCTime = sum_gc_time(SystemEvents),
    ScheduleTime = sum_schedule_time(SystemEvents),

    #{total_events => length(Events),
     call_counts => CallCounts,
     avg_call_duration_us => AvgCallTime,
     total_gc_time_ms => GCTime,
     total_schedule_time_ms => ScheduleTime,
     hotspots => identify_hotspots(CallEvents)}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Sort events chronologically
-spec sort_events_chronologically([trace_event()]) -> [trace_event()].
sort_events_chronologically(Events) ->
    lists:sort(fun(#{timestamp := TS1}, #{timestamp := TS2}) -> TS1 =< TS2 end,
               Events).

%% @private
%% Filter events based on options
-spec filter_events([trace_event()], visualization_options()) -> [trace_event()].
filter_events(Events, Options) ->
    Filter = maps:get(filter, Options, fun(_) -> true end),
    lists:filter(Filter, Events).

%% @private
%% Filter events by type
-spec filter_by_type([trace_event()], atom()) -> [trace_event()].
filter_by_type(Events, Type) ->
    lists:filter(fun(#{event_type := ET}) -> ET =:= Type end, Events).

%% @private
%% Format as text timeline
-spec format_text_timeline([trace_event()], visualization_options()) -> iolist().
format_text_timeline(Events, Options) ->
    Detailed = maps:get(detailed, Options, true),

    Header = ["\n",
              "========================================\n",
              "MCP Trace Timeline\n",
              "========================================\n",
              io_lib:fwrite("Total Events: ~p~n", [length(Events)]),
              "========================================\n\n"],

    EventLines =
        [format_text_event(Event, Detailed) || Event <- Events],

    [Header, EventLines].

%% @private
%% Format single event as text
-spec format_text_event(trace_event(), boolean()) -> iolist().
format_text_event(#{timestamp := TS,
                     event_type := Type,
                     data := Data},
                   Detailed) ->
    TimeStr = format_timestamp(TS),
    Base = io_lib:fwrite("[~s] ~s", [TimeStr, Type]),

    Details =
        case Detailed of
            true ->
                format_event_details(Type, Data);
            false ->
                ""
        end,

    [Base, " ", Details, "\n"].

%% @private
%% Format event type-specific details
-spec format_event_details(atom(), term()) -> iolist().
format_event_details(call, #{module := M,
                             function := F,
                             arity := A,
                             arguments := Args}) ->
    io_lib:fwrite("~p:~p/~p Args: ~p", [M, F, A, Args]);

format_event_details(send, #{message := Msg, to := To}) ->
    io_lib:fwrite("To: ~p Msg: ~p", [To, Msg]);

format_event_details('receive', #{message := Msg}) ->
    io_lib:fwrite("Msg: ~p", [Msg]);

format_event_details(system_monitor,
                      #{system_event := Event, info := Info}) ->
    io_lib:fwrite("~p Info: ~p", [Event, Info]);

format_event_details(_Type, _Data) ->
    "".

%% @private
%% Format timestamp for display
-spec format_timestamp(integer()) -> string().
format_timestamp(Microseconds) ->
    {{Y, M, D}, {H, Min, S}} = calendar:system_time_to_universal_time(Microseconds div 1000,
                                                                       microsecond),
    Ms = (Microseconds rem 1000000) div 1000,
    io_lib:fwrite("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~3..0B",
                  [Y, M, D, H, Min, S, Ms]).

%% @private
%% Format as JSON timeline
-spec format_json_timeline([trace_event()], visualization_options()) -> iolist().
format_json_timeline(Events, _Options) ->
    JSON = #{events => lists:map(fun event_to_json_map/1, Events),
             count => length(Events),
             start_time => get_start_time(Events),
             end_time => get_end_time(Events)},
    jsx:encode(JSON).

%% @private
%% Convert event to JSON-compatible map
-spec event_to_json_map(trace_event()) -> map().
event_to_json_map(#{timestamp := TS,
                     event_type := Type,
                     data := Data}) ->
    #{timestamp => TS,
      event_type => Type,
      data => format_data_for_json(Data)}.

%% @private
%% Format data for JSON output
-spec format_data_for_json(term()) -> term().
format_data_for_json(Data) when is_map(Data) ->
    maps:map(fun(_K, V) -> format_value_for_json(V) end, Data);
format_data_for_json(Data) ->
    format_value_for_json(Data).

%% @private
%% Format individual values for JSON
-spec format_value_for_json(term()) -> term().
format_value_for_json(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
format_value_for_json(Ref) when is_reference(Ref) ->
    list_to_binary(io_lib:format("~p", [Ref]));
format_value_for_json(Port) when is_port(Port) ->
    list_to_binary(erlang:port_to_list(Port));
format_value_for_json(Fun) when is_function(Fun) ->
    <<"function">>;
format_value_for_json(Term) ->
    Term.

%% @private
%% Format as HTML timeline
-spec format_html_timeline([trace_event()], visualization_options()) -> iolist().
format_html_timeline(Events, _Options) ->
    Header = ["<!DOCTYPE html>\n",
              "<html>\n<head>\n",
              "<title>MCP Trace Timeline</title>\n",
              "<style>\n",
              "  body { font-family: monospace; background: #1e1e1e; color: #d4d4d4; }\n",
              "  .event { padding: 8px; margin: 4px 0; border-left: 3px solid #007acc; }\n",
              "  .timestamp { color: #569cd6; }\n",
              "  .event-type { color: #4ec9b0; font-weight: bold; }\n",
              "  .details { color: #ce9178; }\n",
              "  .call { border-color: #4ec9b0; }\n",
              "  .send { border-color: #dcdcaa; }\n",
              "  .'receive' { border-color: #b5cea8; }\n",
              "  .system { border-color: #f48771; }\n",
              "</style>\n",
              "</head>\n<body>\n",
              "<h1>MCP Trace Timeline</h1>\n",
              io_lib:fwrite("<p>Total Events: ~p</p>\n", [length(Events)]),
              "<div class='timeline'>\n"],

    EventDivs = [format_html_event(Event) || Event <- Events],

    Footer = ["</div>\n</body>\n</html>"],

    [Header, EventDivs, Footer].

%% @private
%% Format single event as HTML
-spec format_html_event(trace_event()) -> iolist().
format_html_event(#{timestamp := TS,
                     event_type := Type,
                     data := Data}) ->
    TimeStr = format_timestamp(TS),
    TypeClass = atom_to_list(Type),
    DetailsStr = format_event_details(Type, Data),

    ["<div class='event ", TypeClass, "'>\n",
     "  <span class='timestamp'>", TimeStr, "</span>\n",
     "  <span class='event-type'>", atom_to_list(Type), "</span>\n",
     "  <span class='details'>", DetailsStr, "</span>\n",
     "</div>\n"].

%% @private
%% Build call graph from call events
-spec build_call_graph([trace_event()]) -> iolist().
build_call_graph(CallEvents) ->
    %% Group calls by process
    ByPid = lists:foldl(fun(#{data := #{pid := Pid} = Data}, Acc) ->
                               maps:update_with(Pid,
                                               fun(Calls) -> [Data | Calls] end,
                                               [Data],
                                               Acc)
                       end,
                       #{},
                       CallEvents),

    %% Format call graph
    ["Call Graph:\n",
     "========================================\n",
     format_call_graph_by_pid(ByPid)].

%% @private
%% Format call graph grouped by PID
-spec format_call_graph_by_pid(#{pid() => [map()]}) -> iolist().
format_call_graph_by_pid(ByPid) ->
    maps:fold(fun(Pid, Calls, Acc) ->
                  [io_lib:fwrite("~nProcess: ~p~n  Calls: ~p~n",
                                [Pid, length(Calls)]),
                   format_calls(Calls),
                   Acc]
              end,
              [],
              ByPid).

%% @private
%% Format list of calls
-spec format_calls([map()]) -> iolist().
format_calls(Calls) ->
    [io_lib:fwrite("  - ~p:~p/~p~n",
                   [maps:get(module, C),
                    maps:get(function, C),
                    maps:get(arity, C)])
     || C <- Calls].

%% @private
%% Build message flow visualization
-spec build_message_flow([trace_event()], [trace_event()]) -> iolist().
build_message_flow(SendEvents, RecvEvents) ->
    %% Pair send and 'receive' events
    Flows = pair_messages(SendEvents, RecvEvents),

    ["Message Flow:\n",
     "========================================\n",
     io_lib:fwrite("Total Messages: ~p~n", [length(Flows)]),
     format_message_flows(Flows)].

%% @private
%% Pair send and 'receive' events
-spec pair_messages([trace_event()], [trace_event()]) -> [map()].
pair_messages(SendEvents, RecvEvents) ->
    %% Simple pairing by timestamp proximity
    %% TODO: Implement proper correlation by message ID
    lists:zipwith(fun(Send, Recv) ->
                     #{send => maps:get(data, Send),
                       'receive' => maps:get(data, Recv)}
                  end,
                  SendEvents,
                  RecvEvents).

%% @private
%% Format message flows
-spec format_message_flows([map()]) -> iolist().
format_message_flows(Flows) ->
    [format_single_flow(Flow) || Flow <- Flows].

%% @private
%% Format single message flow
-spec format_single_flow(map()) -> iolist().
format_single_flow(#{send := Send, 'receive' := Recv}) ->
    io_lib:fwrite("~p -> ~p: ~p~n",
                  [maps:get(pid, Send),
                   maps:get(to, Send),
                   maps:get(message, Send)]).

%% @private
%% Format system monitoring events
-spec format_system_monitoring([trace_event()]) -> iolist().
format_system_monitoring(SystemEvents) ->
    ["System Events:\n",
     "========================================\n",
     io_lib:fwrite("Total System Events: ~p~n", [length(SystemEvents)]),
     format_system_events_by_type(SystemEvents)].

%% @private
%% Group and format system events by type
-spec format_system_events_by_type([trace_event()]) -> iolist().
format_system_events_by_type(Events) ->
    ByType = lists:foldl(fun(#{data := #{system_event := Type} = Data}, Acc) ->
                                maps:update_with(Type,
                                                fun(Evs) -> [Data | Evs] end,
                                                [Data],
                                                Acc)
                        end,
                        #{},
                        Events),

    maps:fold(fun(Type, Evs, Acc) ->
                  [io_lib:fwrite("~n~p: ~p events~n", [Type, length(Evs)]),
                   format_system_events_list(Evs),
                   Acc]
              end,
              [],
              ByType).

%% @private
%% Format list of system events
-spec format_system_events_list([map()]) -> iolist().
format_system_events_list(Events) ->
    [io_lib:fwrite("  - Pid: ~p, Info: ~p~n",
                   [maps:get(pid, E), maps:get(info, E)])
     || E <- Events].

%% @private
%% Calculate trace statistics
-spec calculate_statistics([trace_event()]) -> map().
calculate_statistics(Events) ->
    Types = [Type || #{event_type := Type} <- Events],
    CountByType = lists:foldl(fun(Type, Acc) ->
                                      maps:update_with(Type,
                                                      fun(C) -> C + 1 end,
                                                      1,
                                                      Acc)
                              end,
                              #{},
                              Types),

    #{total => length(Events),
      by_type => CountByType,
      duration_ms => calculate_duration(Events)}.

%% @private
%% Calculate total trace duration
-spec calculate_duration([trace_event()]) -> non_neg_integer().
calculate_duration([]) ->
    0;
calculate_duration(Events) ->
    Start = get_start_time(Events),
    End = get_end_time(Events),
    (End - Start) div 1000.

%% @private
%% Get trace start time
-spec get_start_time([trace_event()]) -> integer().
get_start_time([]) ->
    0;
get_start_time([#{timestamp := TS} | _]) ->
    TS.

%% @private
%% Get trace end time
-spec get_end_time([trace_event()]) -> integer().
get_end_time([]) ->
    0;
get_end_time(Events) ->
    #{timestamp := TS} = lists:last(Events),
    TS.

%% @private
%% Count calls by function
-spec count_calls([trace_event()]) -> map().
count_calls(CallEvents) ->
    lists:foldl(fun(#{data := #{module := M,
                                function := F,
                                arity := A}},
                    Acc) ->
                       Key = {M, F, A},
                       maps:update_with(Key,
                                       fun(C) -> C + 1 end,
                                       1,
                                       Acc)
               end,
               #{},
               CallEvents).

%% @private
%% Calculate average call duration
-spec calculate_avg_call_duration([trace_event()]) -> float().
calculate_avg_call_duration(_CallEvents) ->
    %% TODO: Implement duration tracking using return_from events
    0.0.

%% @private
%% Sum garbage collection time
-spec sum_gc_time([trace_event()]) -> non_neg_integer().
sum_gc_time(SystemEvents) ->
    LongGC = [E || E <- SystemEvents,
                  maps:get(system_event, maps:get(data, E, #{})) =:= long_gc],

    lists:foldl(fun(#{data := #{info := Info}}, Acc) ->
                       case lists:keyfind(timeout, 1, Info) of
                           {timeout, Time} ->
                               Acc + Time;
                           false ->
                               Acc
                       end
               end,
               0,
               LongGC).

%% @private
%% Sum schedule time
-spec sum_schedule_time([trace_event()]) -> non_neg_integer().
sum_schedule_time(SystemEvents) ->
    LongSched = [E || E <- SystemEvents,
                     maps:get(system_event, maps:get(data, E, #{})) =:= long_schedule],

    lists:foldl(fun(#{data := #{info := Info}}, Acc) ->
                       case lists:keyfind(timeout, 1, Info) of
                           {timeout, Time} ->
                               Acc + Time;
                           false ->
                               Acc
                       end
               end,
               0,
               LongSched).

%% @private
%% Identify performance hotspots
-spec identify_hotspots([trace_event()]) -> [{term(), non_neg_integer()}].
identify_hotspots(CallEvents) ->
    CallCounts = count_calls(CallEvents),

    %% Sort by count descending
    Sorted = lists:sort(fun({_K1, C1}, {_K2, C2}) -> C1 > C2 end,
                        maps:to_list(CallCounts)),

    %% Return top 10
    lists:sublist(Sorted, 10).
