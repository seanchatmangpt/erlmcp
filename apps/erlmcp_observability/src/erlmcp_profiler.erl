%%%-------------------------------------------------------------------
%%% @doc
%%% Timeline Profiler for OTP 26-27 Performance Analysis
%%%
%%% This module provides timeline-based profiling using OTP 26's system_profile
%%% and OTP 27's enhanced fprof capabilities. It visualizes process execution
%%% timelines, context switches, and scheduler behavior for MCP operations.
%%%
%%% == Key Features ==
%%%
%%% 1. **Timeline Profiling**: Visualize process execution over time
%%% 2. **Context Switching**: Track scheduler migrations and process state changes
%%% 3. **Tool Invocation Analysis**: Profile complete tool call lifecycle
%%% 4. **Session Flow Analysis**: Trace request-response patterns
%%% 5. **Transport Profiling**: Analyze transport layer performance
%%%
%%% == Usage Example ==
%%%
%%% ```erlang
%%% %% Profile a tool invocation
%%% {ok, Result, Timeline} = erlmcp_profiler:profile_timeline(
%%%     fun() -> erlmcp_server:call_tool(<<"fs">>, <<"read_file">>, Args) end,
%%%     <<"tool.read_file">>
%%% ),
%%%
%%% %% Generate SVG visualization
%%% {ok, SVG} = erlmcp_timeline_viz:generate_svg(Timeline),
%%%     file:write_file("tool_timeline.svg", SVG).
%%%
%%% %% Profile with CLI
%%% erlmcp_profiler_cli:profile_tool(<<"fs">>, <<"read_file">>).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_profiler).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, profile_timeline/2, profile_function/3,
         profile_tool_call/4, profile_session_request/2, profile_transport/2,
         get_profile_data/1, stop_profile/1, aggregate_profiles/1, compare_profiles/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type profile_id() :: binary().
-type timestamp() :: integer().
-type duration_us() :: non_neg_integer().

-type timeline_event() ::
    #{type := scheduler | process | port | gc,
      timestamp := timestamp(),
      duration_us := duration_us(),
      details => map()}.

-type timeline_profile() ::
    #{profile_id := profile_id(),
      label := binary(),
      start_time := timestamp(),
      end_time := timestamp(),
      total_duration_us := duration_us(),
      events := [timeline_event()],
      statistics := map()}.

-type profile_opts() ::
    #{include_scheduler => boolean(),
      include_gc => boolean(),
      include_ports => boolean(),
      sample_rate => float(),
      max_events => non_neg_integer()}.

-type profile_state() ::
    #{profile_id := profile_id(),
      label := binary(),
      opts := profile_opts(),
      start_time := timestamp(),
      events := [timeline_event()],
      pid := pid(),
      profiler_pid := pid() | undefined,
      status := running | completed | stopped}.

-record(state,
        {active_profiles = #{} :: #{profile_id() := profile_state()},
         profile_counter = 0 :: non_neg_integer(),
         aggregator_pid :: pid() | undefined}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the profiler with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the profiler with custom options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Profile a function execution with timeline tracking
-spec profile_timeline(fun(() -> term()), binary()) ->
                           {ok, term(), timeline_profile()} | {error, term()}.
profile_timeline(Fun, Label) when is_function(Fun, 0), is_binary(Label) ->
    Opts = get_default_opts(),
    profile_timeline(Fun, Label, Opts).

%% @doc Profile a function with custom options
-spec profile_function(fun(() -> term()), binary(), profile_opts()) ->
                           {ok, term(), timeline_profile()} | {error, term()}.
profile_function(Fun, Label, Opts) when is_function(Fun, 0), is_binary(Label), is_map(Opts) ->
    ProfileId = generate_profile_id(),
    StartTime = erlang:system_time(microsecond),

    %% Start system profiling
    {ok, ProfilerPid} = start_system_profile(Opts),

    %% Execute function
    try
        Result = Fun(),
        EndTime = erlang:system_time(microsecond),

        %% Stop profiling and collect data
        Events = collect_profile_data(ProfilerPid, Opts),
        Timeline = build_timeline(ProfileId, Label, StartTime, EndTime, Events),

        {ok, Result, Timeline}
    catch
        Class:Reason:Stacktrace ->
            EndTime = erlang:system_time(microsecond),
            stop_system_profile(ProfilerPid),

            %% Create error timeline
            Events = [],
            Timeline = build_timeline(ProfileId, Label, StartTime, EndTime, Events),
            ErrorTimeline = Timeline#{error => #{class => Class,
                                                reason => Reason,
                                                stacktrace => Stacktrace}},

            erlang:raise(Class, Reason, Stacktrace)
    after
        stop_system_profile(ProfilerPid)
    end.

%% @doc Profile a complete tool call
-spec profile_tool_call(binary(), binary(), map(), profile_opts()) ->
                               {ok, term(), timeline_profile()} | {error, term()}.
profile_tool_call(Server, Tool, Arguments, Opts) when is_binary(Server),
                                                       is_binary(Tool),
                                                       is_map(Arguments),
                                                       is_map(Opts) ->
    Label = <<"tool.", Server/binary, ".", Tool/binary>>,
    Fun = fun() ->
                  case whereis(erlmcp_server) of
                      undefined -> {error, server_not_started};
                      Pid -> gen_server:call(Pid, {call_tool, Server, Tool, Arguments}, 30000)
                  end
          end,
    profile_function(Fun, Label, Opts).

%% @doc Profile a session request-response cycle
-spec profile_session_request(binary(), map()) ->
                                     {ok, term(), timeline_profile()} | {error, term()}.
profile_session_request(SessionId, Request) when is_binary(SessionId), is_map(Request) ->
    Label = <<"session.", SessionId/binary>>,
    Fun = fun() ->
                  case erlmcp_session_manager:get_session(SessionId) of
                      {ok, _SessionData} ->
                          %% Simulate request processing
                          {ok, #{status => processed}};
                      {error, not_found} ->
                          {error, session_not_found}
                  end
          end,
    profile_function(Fun, Label, get_default_opts()).

%% @doc Profile transport layer performance
-spec profile_transport(binary(), pos_integer()) ->
                               {ok, term(), timeline_profile()} | {error, term()}.
profile_transport(TransportType, MessageSize) when is_binary(TransportType) ->
    Label = <<"transport.", TransportType/binary>>,
    TestData = crypto:strong_rand_bytes(MessageSize),

    Fun = fun() ->
                  %% Simulate transport send/receive
                  StartTime = erlang:monotonic_time(microsecond),
                  %% Encode
                  Encoded = erlmcp_json_rpc:encode_request(1, <<"test/method">>, #{}),
                  %% Decode
                  case erlmcp_json_rpc:decode_message(Encoded) of
                      {ok, _Decoded} ->
                          EndTime = erlang:monotonic_time(microsecond),
                          {ok, EndTime - StartTime};
                      Error ->
                          Error
                  end
          end,
    profile_function(Fun, Label, get_default_opts()).

%% @doc Get stored profile data
-spec get_profile_data(profile_id()) -> {ok, timeline_profile()} | {error, not_found}.
get_profile_data(ProfileId) ->
    gen_server:call(?MODULE, {get_profile_data, ProfileId}).

%% @doc Stop a running profile
-spec stop_profile(profile_id()) -> ok | {error, not_found}.
stop_profile(ProfileId) ->
    gen_server:call(?MODULE, {stop_profile, ProfileId}).

%% @doc Aggregate multiple profiles
-spec aggregate_profiles([timeline_profile()]) ->
                                {ok, timeline_profile()} | {error, term()}.
aggregate_profiles([]) ->
    {error, no_profiles};
aggregate_profiles([Profile | _] = Profiles) ->
    try
        %% Merge all events into a single timeline
        AllEvents = lists:flatmap(fun(P) -> maps:get(events, P, []) end, Profiles),

        %% Calculate aggregate statistics
        TotalDuration = lists:foldl(fun(P, Acc) ->
                                            maps:get(total_duration_us, P, 0) + Acc
                                    end, 0, Profiles),

        StartTime = lists:foldl(fun(P, Min) ->
                                        min(maps:get(start_time, P), Min)
                                end, maps:get(start_time, Profile), Profiles),

        EndTime = lists:foldl(fun(P, Max) ->
                                      max(maps:get(end_time, P), Max)
                              end, maps:get(end_time, Profile), Profiles),

        AggregateLabel = <<"aggregate_", (integer_to_binary(length(Profiles)))/binary, "_profiles">>,

        {ok, #{profile_id => generate_profile_id(),
                label => AggregateLabel,
                start_time => StartTime,
                end_time => EndTime,
                total_duration_us => TotalDuration,
                events => sort_events_by_timestamp(AllEvents),
                statistics => calculate_aggregate_stats(Profiles)}}
    catch
        _:_ ->
            {error, aggregation_failed}
    end.

%% @doc Compare two profiles
-spec compare_profiles(timeline_profile(), timeline_profile()) ->
                              #{baseline := timeline_profile(),
                                comparison := timeline_profile(),
                                diff := map()}.
compare_profiles(Profile1, Profile2) ->
    Duration1 = maps:get(total_duration_us, Profile1, 0),
    Duration2 = maps:get(total_duration_us, Profile2, 0),
    DurationDiff = Duration2 - Duration1,

    EventCount1 = length(maps:get(events, Profile1, [])),
    EventCount2 = length(maps:get(events, Profile2, [])),
    EventCountDiff = EventCount2 - EventCount1,

    #{baseline => Profile1,
      comparison => Profile2,
      diff => #{duration_us_diff => DurationDiff,
                duration_percent_change => percent_change(Duration1, Duration2),
                event_count_diff => EventCountDiff,
                faster => Duration2 < Duration1}}.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(_Opts) ->
    ?LOG_INFO("Starting timeline profiler"),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
                         {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({get_profile_data, ProfileId}, _From, State) ->
    case maps:get(ProfileId, State#state.active_profiles, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ProfileState ->
            Timeline = build_timeline_from_state(ProfileState),
            {reply, {ok, Timeline}, State}
    end;
handle_call({stop_profile, ProfileId}, _From, State) ->
    case maps:get(ProfileId, State#state.active_profiles, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ProfileState ->
            stop_system_profile(maps:get(profiler_pid, ProfileState)),
            UpdatedProfile = ProfileState#{status => stopped},
            ActiveProfiles = maps:put(ProfileId, UpdatedProfile, State#state.active_profiles),
            {reply, ok, State#state{active_profiles = ActiveProfiles}}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({profile_data, ProfileId, Data}, State) ->
    case maps:get(ProfileId, State#state.active_profiles, undefined) of
        undefined ->
            {noreply, State};
        ProfileState ->
            ExistingEvents = maps:get(events, ProfileState, []),
            UpdatedProfile = ProfileState#{events => ExistingEvents ++ Data},
            ActiveProfiles = maps:put(ProfileId, UpdatedProfile, State#state.active_profiles),
            {noreply, State#state{active_profiles = ActiveProfiles}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Profile timeline with options
-spec profile_timeline(fun(() -> term()), binary(), profile_opts()) ->
                               {ok, term(), timeline_profile()} | {error, term()}.
profile_timeline(Fun, Label, Opts) ->
    profile_function(Fun, Label, Opts).

%% @private Start system profiling
-spec start_system_profile(profile_opts()) -> {ok, pid()} | {error, term()}.
start_system_profile(Opts) ->
    ProfilePid = self(),
    Flags = build_profile_flags(Opts),

    try
        %% Start system profiler (OTP 26+)
        case erlang:system_profile(ProfilePid, Flags) of
            {ok, _ProfilerPid} ->
                {ok, ProfilePid};
            {error, Reason} ->
                {error, {profile_start_failed, Reason}}
        end
    catch
        error:badarg ->
            %% Fallback for older OTP versions
            ?LOG_WARNING("system_profile not available, using basic profiling"),
            {ok, ProfilePid}
    end.

%% @private Stop system profiling
-spec stop_system_profile(pid()) -> ok.
stop_system_profile(Pid) ->
    try
        erlang:system_profile(Pid, false),
        ok
    catch
        _:_ ->
            ok
    end.

%% @private Build profile flags from options
-spec build_profile_flags(profile_opts()) -> [atom() | {atom(), term()}].
build_profile_flags(Opts) ->
    Flags0 = [scheduler, timestamp, exclusive],

    Flags1 = case maps:get(include_scheduler, Opts, true) of
                 true -> [scheduler | Flags0];
                 false -> Flags0
             end,

    Flags2 = case maps:get(include_ports, Opts, true) of
                 true -> [port | Flags1];
                 false -> Flags1
             end,

    %% Add processor option for OTP 26+
    case maps:get(processor_id, Opts, 0) of
        ProcessorId when is_integer(ProcessorId) ->
            [{processor, ProcessorId} | Flags2];
        _ ->
            Flags2
    end.

%% @private Collect profile data from system messages
-spec collect_profile_data(pid(), profile_opts()) -> [timeline_event()].
collect_profile_data(ProfilerPid, Opts) ->
    MaxEvents = maps:get(max_events, Opts, 10000),
    Timeout = 5000,

    collect_events(ProfilerPid, MaxEvents, Timeout, []).

%% @private Collect events from system profile
-spec collect_events(pid(), non_neg_integer(), timeout(), [timeline_event()]) ->
                            [timeline_event()].
collect_events(_Pid, 0, _Timeout, Acc) ->
    lists:reverse(Acc);
collect_events(_Pid, Max, Timeout, Acc) ->
    receive
        {profile, Data} when is_list(Data) ->
            Events = [parse_profile_event(E) || E <- Data],
            collect_events(_Pid, Max - length(Events), Timeout, Events ++ Acc);
        {profile, Event} ->
            EventMap = parse_profile_event(Event),
            collect_events(_Pid, Max - 1, Timeout, [EventMap | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

%% @private Parse profile event from system_profile data
-spec parse_profile_event(term()) -> timeline_event().
parse_profile_event({scheduler, _Pid, _State, Timestamp, Mfa}) ->
    #{type => scheduler,
      timestamp => normalize_timestamp(Timestamp),
      duration_us => 0,
      details => #{mfa => format_mfa(Mfa)}};
parse_profile_event({process, _Pid, _State, Timestamp, Mfa}) ->
    #{type => process,
      timestamp => normalize_timestamp(Timestamp),
      duration_us => 0,
      details => #{mfa => format_mfa(Mfa)}};
parse_profile_event({gc, _Pid, _Info, Timestamp}) ->
    #{type => gc,
      timestamp => normalize_timestamp(Timestamp),
      duration_us => 0,
      details => #{reason => gc}};
parse_profile_event({port, _Port, _Action, Timestamp}) ->
    #{type => port,
      timestamp => normalize_timestamp(Timestamp),
      duration_us => 0,
      details => #{action => port_operation}};
parse_profile_event(Event) ->
    #{type => unknown,
      timestamp => erlang:system_time(microsecond),
      duration_us => 0,
      details => #{raw => Event}}.

%% @private Build timeline from collected data
-spec build_timeline(profile_id(), binary(), timestamp(), timestamp(), [timeline_event()]) ->
                         timeline_profile().
build_timeline(ProfileId, Label, StartTime, EndTime, Events) ->
    TotalDuration = EndTime - StartTime,
    Statistics = calculate_timeline_stats(Events, StartTime, EndTime),

    #{profile_id => ProfileId,
      label => Label,
      start_time => StartTime,
      end_time => EndTime,
      total_duration_us => TotalDuration,
      events => sort_events_by_timestamp(Events),
      statistics => Statistics}.

%% @private Build timeline from profile state
-spec build_timeline_from_state(profile_state()) -> timeline_profile().
build_timeline_from_state(ProfileState) ->
    #{profile_id => maps:get(profile_id, ProfileState),
      label => maps:get(label, ProfileState),
      start_time => maps:get(start_time, ProfileState),
      end_time => erlang:system_time(microsecond),
      total_duration_us => 0,
      events => maps:get(events, ProfileState, []),
      statistics => #{}}.

%% @private Calculate timeline statistics
-spec calculate_timeline_stats([timeline_event()], timestamp(), timestamp()) -> map().
calculate_timeline_stats(Events, StartTime, EndTime) ->
    TotalDuration = EndTime - StartTime,

    EventCounts =
        lists:foldl(fun(#{type := Type}, Acc) ->
                           maps:update_with(Type, fun(Count) -> Count + 1 end, 1, Acc)
                    end, #{}, Events),

    EventByType = group_events_by_type(Events),

    #{total_duration_us => TotalDuration,
      event_counts => EventCounts,
      events_by_type => EventByType,
      event_count => length(Events)}.

%% @private Calculate aggregate statistics
-spec calculate_aggregate_stats([timeline_profile()]) -> map().
calculate_aggregate_stats(Profiles) ->
    TotalProfiles = length(Profiles),
    AvgDuration = lists:foldl(fun(P, Acc) ->
                                      maps:get(total_duration_us, P, 0) + Acc
                              end, 0, Profiles) div TotalProfiles,

    TotalEvents = lists:foldl(fun(P, Acc) ->
                                      length(maps:get(events, P, [])) + Acc
                              end, 0, Profiles),

    #{profile_count => TotalProfiles,
      avg_duration_us => AvgDuration,
      total_events => TotalEvents,
      avg_events_per_profile => TotalEvents div TotalProfiles}.

%% @private Group events by type
-spec group_events_by_type([timeline_event()]) -> map().
group_events_by_type(Events) ->
    lists:foldl(fun(#{type := Type} = Event, Acc) ->
                   maps:update_with(Type, fun(List) -> [Event | List] end, [Event], Acc)
                end, #{}, Events).

%% @private Sort events by timestamp
-spec sort_events_by_timestamp([timeline_event()]) -> [timeline_event()].
sort_events_by_timestamp(Events) ->
    lists:sort(fun(#{timestamp := T1}, #{timestamp := T2}) -> T1 =< T2 end, Events).

%% @private Normalize timestamp to microseconds
-spec normalize_timestamp(term()) -> timestamp().
normalize_timestamp(Timestamp) when is_integer(Timestamp) ->
    Timestamp;
normalize_timestamp({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs;
normalize_timestamp(_Other) ->
    erlang:system_time(microsecond).

%% @private Format MFA tuple to binary
-spec format_mfa({module(), Function, Arity} | {module(), Function, Arity, term()}) -> binary().
format_mfa({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
    iolist_to_binary(io_lib:format("~p:~p/~p", [M, F, A]));
format_mfa({M, F, A, _Location}) when is_atom(M), is_atom(F), is_integer(A) ->
    iolist_to_binary(io_lib:format("~p:~p/~p", [M, F, A]));
format_mfa(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

%% @private Calculate percentage change
-spec percent_change(number(), number()) -> float().
percent_change(Old, New) when Old =:= 0 ->
    0.0;
percent_change(Old, New) ->
    ((New - Old) / Old) * 100.0.

%% @private Generate unique profile ID
-spec generate_profile_id() -> profile_id().
generate_profile_id() ->
    <<ID:64>> = crypto:strong_rand_bytes(8),
    iolist_to_binary(io_lib:format("profile_~16.16.0b", [ID])).

%% @private Get default options
-spec get_default_opts() -> profile_opts().
get_default_opts() ->
    #{include_scheduler => true,
      include_gc => true,
      include_ports => true,
      sample_rate => 1.0,
      max_events => 10000,
      processor_id => 0}.
