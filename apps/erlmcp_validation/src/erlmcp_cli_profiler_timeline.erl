%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Commands for Timeline Profiling
%%%
%%% Provides command-line interface for OTP 26-27 timeline profiling:
%%% - erlmcp profile:tool ToolName
%%% - erlmcp profile:session SessionId
%%% - erlmcp profile:transport TransportType
%%%
%%% == Usage Examples ==
%%%
%%% ```bash
%%% # Profile a tool call
%%% erlmcp profile:tool fs read_file '{"path": "/tmp/test.txt"}'
%%%
%%% # Profile a session
%%% erlmcp profile:session abc123def456
%%%
%%% # Profile transport
%%% erlmcp profile:transport stdio 1024
%%%
%%% # Generate visualization
%%% erlmcp profile:visualize timeline.json output.svg
%%%
%%% # Compare profiles
%%% erlmcp profile:compare baseline.json current.json
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_profiler_timeline).

%% CLI exports
-export([command/2, main/1]).
%% Profile commands
-export([profile_tool/3, profile_session/1, profile_transport/2,
         visualize_profile/2, compare_profiles/2, list_profiles/0]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% CLI Command Handler
%%====================================================================

%% @doc Handle profile commands from CLI
-spec command([string()], map()) -> ok | {error, term()}.
command(["profile:tool", Server, Tool | Args], _Opts) ->
    Arguments = parse_arguments(Args),
    profile_tool(list_to_binary(Server), list_to_binary(Tool), Arguments);

command(["profile:session", SessionId], _Opts) ->
    profile_session(list_to_binary(SessionId));

command(["profile:transport", TransportType, MessageSizeStr], _Opts) ->
    MessageSize = list_to_integer(MessageSizeStr),
    profile_transport(list_to_binary(TransportType), MessageSize);

command(["profile:visualize", InputFile, OutputFile], _Opts) ->
    visualize_profile(InputFile, OutputFile);

command(["profile:compare", BaselineFile, ComparisonFile], _Opts) ->
    compare_profiles(BaselineFile, ComparisonFile);

command(["profile:list"], _Opts) ->
    list_profiles();

command(["profile" | _], _Opts) ->
    print_profile_help();

command(_Unknown, _Opts) ->
    {error, unknown_command}.

%% @doc Main entry point for escript
-spec main([string()]) -> ok.
main(Args) ->
    case command(Args, #{}) of
        ok ->
            ok;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            halt(1)
    end.

%%====================================================================
%% Profile Commands
%%====================================================================

%% @doc Profile a tool invocation
-spec profile_tool(binary(), binary(), map()) -> ok.
profile_tool(Server, Tool, Arguments) ->
    io:format("Profiling tool: ~p:~p~n", [Server, Tool]),
    io:format("Arguments: ~p~n", [Arguments]),

    case whereis(erlmcp_profiler) of
        undefined ->
            io:format("Starting profiler...~n"),
            {ok, _Pid} = erlmcp_profiler:start_link();
        _ ->
            ok
    end,

    Opts = get_default_opts(),
    Label = <<"tool.", Server/binary, ".", Tool/binary>>,

    %% Execute profile
    case erlmcp_profiler:profile_tool_call(Server, Tool, Arguments, Opts) of
        {ok, Result, Timeline} ->
            io:format("~nProfile completed successfully~n"),
            io:format("Result: ~p~n", [Result]),
            print_timeline_summary(Timeline),

            %% Save profile data
            ProfileId = maps:get(profile_id, Timeline),
            Filename = <<"profile_", ProfileId/binary, ".json">>,
            save_profile(Timeline, Filename),
            io:format("~nProfile saved to: ~s~n", [Filename]);
        {error, Reason} ->
            io:format("Profile failed: ~p~n", [Reason]),
            halt(1)
    end.

%% @doc Profile a session request
-spec profile_session(binary()) -> ok.
profile_session(SessionId) ->
    io:format("Profiling session: ~s~n", [SessionId]),

    Request = #{action => profile, session_id => SessionId},

    case erlmcp_profiler:profile_session_request(SessionId, Request) of
        {ok, Result, Timeline} ->
            io:format("~nProfile completed successfully~n"),
            io:format("Result: ~p~n", [Result]),
            print_timeline_summary(Timeline),

            %% Save profile data
            ProfileId = maps:get(profile_id, Timeline),
            Filename = <<"profile_session_", ProfileId/binary, ".json">>,
            save_profile(Timeline, Filename),
            io:format("~nProfile saved to: ~s~n", [Filename]);
        {error, Reason} ->
            io:format("Profile failed: ~p~n", [Reason]),
            halt(1)
    end.

%% @doc Profile transport performance
-spec profile_transport(binary(), pos_integer()) -> ok.
profile_transport(TransportType, MessageSize) ->
    io:format("Profiling transport: ~s with message size: ~p bytes~n", [TransportType, MessageSize]),

    case erlmcp_profiler:profile_transport(TransportType, MessageSize) of
        {ok, Result, Timeline} ->
            io:format("~nProfile completed successfully~n"),
            io:format("Result: ~p~n", [Result]),
            print_timeline_summary(Timeline),

            %% Save profile data
            ProfileId = maps:get(profile_id, Timeline),
            Filename = <<"profile_transport_", TransportType/binary, "_", ProfileId/binary, ".json">>,
            save_profile(Timeline, Filename),
            io:format("~nProfile saved to: ~s~n", [Filename]);
        {error, Reason} ->
            io:format("Profile failed: ~p~n", [Reason]),
            halt(1)
    end.

%% @doc Generate visualization from profile
-spec visualize_profile(string(), string()) -> ok.
visualize_profile(InputFile, OutputFile) ->
    io:format("Generating visualization from ~s~n", [InputFile]),

    case file:read_file(InputFile) of
        {ok, JSON} ->
            try jsx:decode(JSON, [return_maps]) of
                Timeline ->
                    Extension = filename:extension(OutputFile),
                    OutputFileBin = list_to_binary(OutputFile),

                    case generate_viz(Timeline, Extension, OutputFileBin) of
                        ok ->
                            io:format("Visualization saved to: ~s~n", [OutputFile]);
                        {error, Reason} ->
                            io:format("Visualization failed: ~p~n", [Reason]),
                            halt(1)
                    end
            catch
                _:_ ->
                    io:format("Failed to parse profile data~n"),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("Failed to read profile file: ~p~n", [Reason]),
            halt(1)
    end.

%% @doc Compare two profiles
-spec compare_profiles(string(), string()) -> ok.
compare_profiles(BaselineFile, ComparisonFile) ->
    io:format("Comparing profiles~n"),
    io:format("Baseline: ~s~n", [BaselineFile]),
    io:format("Comparison: ~s~n", [ComparisonFile]),

    case load_profile(BaselineFile) of
        {ok, Baseline} ->
            case load_profile(ComparisonFile) of
                {ok, Comparison} ->
                    Diff = erlmcp_profiler:compare_profiles(Baseline, Comparison),
                    print_comparison(Diff);
                {error, Reason} ->
                    io:format("Failed to load comparison: ~p~n", [Reason]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("Failed to load baseline: ~p~n", [Reason]),
            halt(1)
    end.

%% @doc List all saved profiles
-spec list_profiles() -> ok.
list_profiles() ->
    io:format("Saved profiles:~n"),

    case file:list_dir(".") of
        {ok, Files} ->
            Profiles = [F || F <- Files, lists:prefix("profile_", F), lists:suffix(".json", F)],
            case Profiles of
                [] ->
                    io:format("  No profiles found~n");
                _ ->
                    lists:foreach(fun(F) ->
                                          io:format("  ~s~n", [F])
                                  end, Profiles)
            end;
        {error, Reason} ->
            io:format("Failed to list profiles: ~p~n", [Reason])
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Parse command-line arguments to map
-spec parse_arguments([string()]) -> map().
parse_arguments([]) ->
    #{};
parse_arguments([Arg | Rest]) ->
    case string:split(Arg, "=") of
        [Key, Value] ->
            KeyBin = list_to_binary(Key),
            ValueBin = list_to_binary(Value),
            maps:put(KeyBin, ValueBin, parse_arguments(Rest));
        _ ->
            parse_arguments(Rest)
    end.

%% @private Get default profiling options
-spec get_default_opts() -> map().
get_default_opts() ->
    #{include_scheduler => true,
      include_gc => true,
      include_ports => true,
      sample_rate => 1.0,
      max_events => 10000,
      processor_id => 0}.

%% @private Print timeline summary
-spec print_timeline_summary(map()) -> ok.
print_timeline_summary(Timeline) ->
    DurationUs = maps:get(total_duration_us, Timeline, 0),
    DurationMs = DurationUs / 1000,
    EventCount = maps:get(event_count, maps:get(statistics, Timeline, #{}), 0),

    io:format("~n=== Timeline Summary ===~n"),
    io:format("Duration: ~.2f ms~n", [DurationMs]),
    io:format("Events: ~p~n", [EventCount]),
    io:format("Profile ID: ~s~n", [maps:get(profile_id, Timeline)]),

    %% Print event counts
    Stats = maps:get(statistics, Timeline, #{}),
    case maps:get(event_counts, Stats, undefined) of
        undefined -> ok;
        Counts ->
            io:format("~nEvent Breakdown:~n"),
            maps:fold(fun(Type, Count, _Acc) ->
                              io:format("  ~p: ~p~n", [Type, Count])
                      end, ok, Counts)
    end.

%% @private Save profile to file
-spec save_profile(map(), binary()) -> ok.
save_profile(Timeline, Filename) ->
    JSON = jsx:encode(Timeline, [space, {indent, 2}]),
    file:write_file(Filename, JSON).

%% @private Load profile from file
-spec load_profile(string()) -> {ok, map()} | {error, term()}.
load_profile(Filename) ->
    case file:read_file(Filename) of
        {ok, JSON} ->
            try
                Timeline = jsx:decode(JSON, [return_maps]),
                {ok, Timeline}
            catch
                _:_ ->
                    {error, invalid_json}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Generate visualization based on extension
-spec generate_viz(map(), string(), binary()) -> ok | {error, term()}.
generate_viz(Timeline, ".svg", OutputFile) ->
    case erlmcp_timeline_viz:generate_svg(Timeline) of
        {ok, SVG} ->
            file:write_file(OutputFile, SVG);
        {error, Reason} ->
            {error, Reason}
    end;
generate_viz(Timeline, ".html", OutputFile) ->
    case erlmcp_timeline_viz:generate_html(Timeline) of
        {ok, HTML} ->
            file:write_file(OutputFile, HTML);
        {error, Reason} ->
            {error, Reason}
    end;
generate_viz(_Timeline, Extension, _OutputFile) ->
    {error, {unsupported_format, Extension}}.

%% @private Print comparison results
-spec print_comparison(map()) -> ok.
print_comparison(#{baseline := Baseline, comparison := Comparison, diff := Diff}) ->
    BaselineDuration = maps:get(total_duration_us, Baseline, 0) / 1000,
    ComparisonDuration = maps:get(total_duration_us, Comparison, 0) / 1000,
    DurationDiff = maps:get(duration_us_diff, Diff, 0) / 1000,
    PercentChange = maps:get(duration_percent_change, Diff, 0.0),
    Faster = maps:get(faster, Diff, false),

    io:format("~n=== Profile Comparison ===~n"),
    io:format("Baseline: ~.2f ms~n", [BaselineDuration]),
    io:format("Comparison: ~.2f ms~n", [ComparisonDuration]),
    io:format("Difference: ~.2f ms (~.1f%)~n", [DurationDiff, PercentChange]),

    case Faster of
        true -> io:format("Result: Comparison is FASTER ✓~n");
        false -> io:format("Result: Comparison is SLOWER ✗~n")
    end.

%% @private Print profile help
-spec print_profile_help() -> ok.
print_profile_help() ->
    io:format("~nTimeline Profiler Commands:~n"),
    io:format("~n  erlmcp profile:tool <server> <tool> [args]~n"),
    io:format("      Profile a tool invocation~n"),
    io:format("      Example: erlmcp profile:tool fs read_file path=/tmp/test.txt~n"),
    io:format("~n  erlmcp profile:session <session_id>~n"),
    io:format("      Profile a session request~n"),
    io:format("      Example: erlmcp profile:session abc123def456~n"),
    io:format("~n  erlmcp profile:transport <type> <size>~n"),
    io:format("      Profile transport performance~n"),
    io:format("      Example: erlmcp profile:transport stdio 1024~n"),
    io:format("~n  erlmcp profile:visualize <input> <output>~n"),
    io:format("      Generate visualization from profile~n"),
    io:format("      Example: erlmcp profile:visualize profile.json timeline.svg~n"),
    io:format("~n  erlmcp profile:compare <baseline> <comparison>~n"),
    io:format("      Compare two profiles~n"),
    io:format("      Example: erlmcp profile:compare baseline.json current.json~n"),
    io:format("~n  erlmcp profile:list~n"),
    io:format("      List all saved profiles~n"),
    io:format("~n").
