%%%-----------------------------------------------------------------------------
%%% @doc TCPS Rebar3 Andon Management Provider
%%%
%%% Command-line interface for managing Andon events in rebar3 build pipeline:
%%% - List open Andon events
%%% - Resolve Andon with root cause analysis
%%% - View Andon details and receipts
%%% - Check build blocking status
%%%
%%% Usage:
%%%   rebar3 tcps andon list              - List all open Andon events
%%%   rebar3 tcps andon resolve <id>      - Resolve Andon interactively
%%%   rebar3 tcps andon show <id>         - Show Andon details
%%%   rebar3 tcps andon check             - Check if build is blocked
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_rebar3_andon).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, andon).
-define(NAMESPACE, tcps).
-define(DEPS, []).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, ?NAMESPACE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 tcps andon list"},
        {short_desc, "Manage TCPS Andon events"},
        {desc, "Interactive Andon event management for TCPS build pipeline. "
               "List, resolve, and inspect Andon stop-the-line events."},
        {opts, []}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% Ensure Andon system is started
    ensure_andon_started(),

    %% Get command args
    {_Opts, Args} = rebar_state:command_parsed_args(State),

    case Args of
        [] ->
            %% Default: list open Andons
            do_list(State);
        ["list" | _] ->
            do_list(State);
        ["resolve", AndonId | _] ->
            do_resolve(AndonId, State);
        ["show", AndonId | _] ->
            do_show(AndonId, State);
        ["check" | _] ->
            do_check(State);
        [Unknown | _] ->
            {error, io_lib:format("Unknown andon command: ~s~n"
                                  "Use: list, resolve <id>, show <id>, or check", [Unknown])}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("TCPS Andon Error: ~p", [Reason]).

%%%=============================================================================
%%% Internal Functions - Commands
%%%=============================================================================

%% @private List all open Andon events
do_list(State) ->
    rebar_api:info("~n=== TCPS Open Andon Events ===~n", []),

    %% Get all events from ETS
    AllEvents = ets:tab2list(tcps_andon_events),
    OpenEvents = [{Id, Event} || {Id, Event} <- AllEvents,
                                 is_map(Event),
                                 maps:get(status, Event, undefined) =:= open],

    case OpenEvents of
        [] ->
            rebar_api:info("No open Andon events. Build pipeline clear.~n~n", []),
            {ok, State};
        Events ->
            rebar_api:warn("~p open Andon event(s) blocking pipeline:~n~n", [length(Events)]),

            lists:foreach(fun({Id, Event}) ->
                FailureType = maps:get(failure_type, Event),
                SkuId = maps:get(sku_id, Event),
                Stage = maps:get(stage, Event),
                Timestamp = maps:get(timestamp, Event),
                Details = maps:get(details, Event, #{}),

                rebar_api:warn("Andon ID: ~s~n", [Id]),
                rebar_api:warn("  Type: ~s~n", [FailureType]),
                rebar_api:warn("  SKU: ~s~n", [SkuId]),
                rebar_api:warn("  Stage: ~s~n", [Stage]),
                rebar_api:warn("  Time: ~s~n", [format_timestamp(Timestamp)]),
                rebar_api:warn("  Details: ~p~n~n", [Details])
            end, Events),

            rebar_api:warn("To resolve: rebar3 tcps andon resolve <andon-id>~n~n", []),
            {ok, State}
    end.

%% @private Resolve an Andon event with root cause analysis
do_resolve(AndonId, State) ->
    rebar_api:info("~n=== Resolving Andon: ~s ===~n", [AndonId]),

    AndonIdBin = ensure_binary(AndonId),

    %% Get Andon event
    Event = tcps_andon:get_andon_event(AndonIdBin),

    case maps:get(status, Event, not_found) of
        not_found ->
            {error, io_lib:format("Andon event not found: ~s", [AndonId])};
        resolved ->
            rebar_api:info("Andon ~s is already resolved.~n", [AndonId]),
            {ok, State};
        open ->
            %% Display event details
            display_andon_details(Event),

            %% Interactive resolution
            rebar_api:info("~nStarting 5 Whys root cause analysis...~n", []),

            %% Prompt for root cause
            io:format("~nWhat was the root cause? "),
            {ok, RootCause} = io:get_line(""),
            RootCauseBin = string:trim(RootCause),

            io:format("What fix was applied? "),
            {ok, Fix} = io:get_line(""),
            FixBin = string:trim(Fix),

            io:format("What prevention was added? "),
            {ok, Prevention} = io:get_line(""),
            PreventionBin = string:trim(Prevention),

            %% Validate inputs
            case {byte_size(list_to_binary(RootCauseBin)) > 0,
                  byte_size(list_to_binary(FixBin)) > 0,
                  byte_size(list_to_binary(PreventionBin)) > 0} of
                {true, true, true} ->
                    %% Create resolution
                    Resolution = #{
                        root_cause => list_to_binary(RootCauseBin),
                        fix_applied => list_to_binary(FixBin),
                        prevention_added => list_to_binary(PreventionBin),
                        resolver => <<"rebar3_cli">>,
                        resolution_time_minutes => 5
                    },

                    %% Resolve Andon
                    case tcps_andon:resolve_andon(AndonIdBin, Resolution) of
                        ok ->
                            rebar_api:info("~n✓ Andon ~s resolved successfully!~n", [AndonId]),
                            rebar_api:info("Pipeline unblocked. You may proceed with build.~n~n", []),
                            {ok, State};
                        {error, Reason} ->
                            {error, io_lib:format("Failed to resolve Andon: ~p", [Reason])}
                    end;
                {_, _, _} ->
                    {error, "All fields (root cause, fix, prevention) are required"}
            end
    end.

%% @private Show detailed Andon information
do_show(AndonId, State) ->
    rebar_api:info("~n=== Andon Details: ~s ===~n", [AndonId]),

    AndonIdBin = ensure_binary(AndonId),
    Event = tcps_andon:get_andon_event(AndonIdBin),

    case maps:get(status, Event, not_found) of
        not_found ->
            {error, io_lib:format("Andon event not found: ~s", [AndonId])};
        _ ->
            display_andon_details(Event),

            %% Show receipts
            rebar_api:info("~nAssociated Receipts:~n", []),
            Receipts = tcps_andon:list_receipts_for_andon(AndonIdBin),

            case Receipts of
                [] ->
                    rebar_api:info("  No receipts found.~n", []);
                _ ->
                    lists:foreach(fun(Receipt) ->
                        ReceiptId = maps:get(<<"receipt_id">>, Receipt),
                        ReceiptType = maps:get(<<"receipt_type">>, Receipt),
                        rebar_api:info("  - ~s (~s)~n", [ReceiptId, ReceiptType])
                    end, Receipts)
            end,

            rebar_api:info("~n", []),
            {ok, State}
    end.

%% @private Check if build is blocked by Andon
do_check(State) ->
    rebar_api:info("~n=== TCPS Build Status Check ===~n", []),

    %% Get all open Andons
    AllEvents = ets:tab2list(tcps_andon_events),
    OpenEvents = [{Id, Event} || {Id, Event} <- AllEvents,
                                 is_map(Event),
                                 maps:get(status, Event, undefined) =:= open],

    case OpenEvents of
        [] ->
            rebar_api:info("✓ Build pipeline clear - no blocking Andon events~n~n", []),
            {ok, State};
        Events ->
            rebar_api:warn("⚠️  Build BLOCKED by ~p open Andon event(s):~n", [length(Events)]),

            lists:foreach(fun({Id, _Event}) ->
                rebar_api:warn("  - ~s~n", [Id])
            end, Events),

            rebar_api:warn("~nResolve Andons before proceeding with build.~n~n", []),
            {error, "Build blocked by open Andon events"}
    end.

%%%=============================================================================
%%% Internal Functions - Utilities
%%%=============================================================================

%% @private Ensure Andon system is started
ensure_andon_started() ->
    case whereis(tcps_andon) of
        undefined ->
            %% Start ETS table if not running
            tcps_andon:start(),
            ok;
        _Pid ->
            ok
    end.

%% @private Display Andon event details
display_andon_details(Event) ->
    rebar_api:info("Event ID: ~s~n", [maps:get(event_id, Event)]),
    rebar_api:info("Failure Type: ~s~n", [maps:get(failure_type, Event)]),
    rebar_api:info("SKU ID: ~s~n", [maps:get(sku_id, Event)]),
    rebar_api:info("Stage: ~s~n", [maps:get(stage, Event)]),
    rebar_api:info("Status: ~s~n", [maps:get(status, Event)]),
    rebar_api:info("Timestamp: ~s~n", [format_timestamp(maps:get(timestamp, Event))]),

    Details = maps:get(details, Event, #{}),
    rebar_api:info("Details: ~p~n", [Details]),

    case maps:get(resolution, Event, undefined) of
        undefined ->
            ok;
        Resolution ->
            rebar_api:info("~nResolution:~n", []),
            rebar_api:info("  Root Cause: ~s~n", [maps:get(root_cause, Resolution)]),
            rebar_api:info("  Fix Applied: ~s~n", [maps:get(fix_applied, Resolution)]),
            rebar_api:info("  Prevention: ~s~n", [maps:get(prevention_added, Resolution)]),
            rebar_api:info("  Resolved By: ~s~n", [maps:get(resolver, Resolution, <<"unknown">>)])
    end.

%% @private Format millisecond timestamp
format_timestamp(Milliseconds) ->
    Seconds = Milliseconds div 1000,
    DateTime = calendar:system_time_to_universal_time(Seconds, second),
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B UTC",
                  [Year, Month, Day, Hour, Minute, Second]).

%% @private Ensure value is binary
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) -> iolist_to_binary(io_lib:format("~p", [Value])).
