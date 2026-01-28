%%%-----------------------------------------------------------------------------
%%% @doc TCPS CLI Andon Management
%%%
%%% Handles all Andon stop-the-line related CLI commands.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_cli_andon).

-export([run/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec run([string()]) -> no_return().
run(["trigger" | Args]) ->
    trigger_andon(parse_args(Args));

run(["list" | Args]) ->
    list_andons(parse_args(Args));

run(["show", AndonId | _]) ->
    show_andon(list_to_binary(AndonId));

run(["resolve", AndonId | Args]) ->
    resolve_andon(list_to_binary(AndonId), parse_args(Args));

run(["status", SkuId | _]) ->
    check_status(list_to_binary(SkuId));

run([]) ->
    tcps_cli_format:error("Missing subcommand. Use: trigger, list, show, resolve, status"),
    halt(1);

run([Unknown | _]) ->
    tcps_cli_format:error("Unknown subcommand: ~s", [Unknown]),
    halt(1).

%%%=============================================================================
%%% Command Implementations
%%%=============================================================================

trigger_andon(Args) ->
    ensure_andon_running(),

    case {maps:get(type, Args, undefined), maps:get(sku, Args, undefined)} of
        {undefined, _} ->
            tcps_cli_format:error("Missing required argument: --type"),
            halt(1);
        {_, undefined} ->
            tcps_cli_format:error("Missing required argument: --sku"),
            halt(1);
        {TypeStr, SkuId} ->
            % Validate type
            case parse_failure_type(TypeStr) of
                {ok, Type} ->
                    Stage = case maps:get(stage, Args, undefined) of
                        undefined -> infer_stage(Type);
                        S -> list_to_existing_atom(S)
                    end,
                    Message = maps:get(message, Args, <<"Failure detected">>),

                    Context = #{
                        sku_id => list_to_binary(SkuId),
                        stage => Stage,
                        details => #{
                            message => Message,
                            triggered_by => <<"cli">>
                        }
                    },

                    case tcps_andon:trigger_andon(Type, Context) of
                        {ok, AndonId} ->
                            tcps_cli_format:success("Andon triggered: ~s", [AndonId]),
                            tcps_cli_format:info("Type: ~s", [Type]),
                            tcps_cli_format:info("SKU: ~s", [SkuId]),
                            tcps_cli_format:info("Stage: ~s", [Stage]),
                            halt(0);
                        {error, Reason} ->
                            tcps_cli_format:error("Failed to trigger Andon: ~p", [Reason]),
                            halt(1)
                    end;
                {error, Reason} ->
                    tcps_cli_format:error("~s", [Reason]),
                    halt(1)
            end
    end.

list_andons(Args) ->
    ensure_andon_running(),

    ShowAll = maps:get(all, Args, false),
    SkuFilter = maps:get(sku, Args, undefined),

    % Get all events from ETS
    AllEvents = case SkuFilter of
        undefined ->
            ets:tab2list(tcps_andon_events),
            [{Id, Event} || {Id, Event} <- ets:tab2list(tcps_andon_events),
                           is_map(Event)];
        SkuId ->
            SkuBin = list_to_binary(SkuId),
            [{Id, Event} || {Id, Event} <- ets:tab2list(tcps_andon_events),
                           is_map(Event),
                           maps:get(sku_id, Event, undefined) =:= SkuBin]
    end,

    % Filter by status
    Filtered = case ShowAll of
        true -> AllEvents;
        false ->
            [{Id, Event} || {Id, Event} <- AllEvents,
                           maps:get(status, Event) =:= open]
    end,

    % Sort by timestamp (newest first)
    Sorted = lists:reverse(lists:keysort(2, [{Id, maps:get(timestamp, Event, 0), Event}
                                              || {Id, Event} <- Filtered])),

    % Format output
    case Sorted of
        [] ->
            io:format("No Andon events found.~n"),
            halt(0);
        _ ->
            Format = tcps_cli_config:get(output_format, table),
            Headers = [event_id, failure_type, sku_id, stage, status, timestamp],
            FormattedEvents = [{Id, E} || {Id, _Ts, E} <- Sorted],
            FormattedMaps = [format_andon(Id, Event) || {Id, Event} <- FormattedEvents],
            tcps_cli_format:output(FormattedMaps, Format, #{headers => Headers}),
            halt(0)
    end.

show_andon(AndonId) ->
    ensure_andon_running(),

    Event = tcps_andon:get_andon_event(AndonId),

    case maps:get(status, Event, not_found) of
        not_found ->
            tcps_cli_format:error("Andon event not found: ~s", [AndonId]),
            halt(1);
        _ ->
            Format = tcps_cli_config:get(output_format, table),
            case Format of
                json ->
                    tcps_cli_format:json(Event);
                _ ->
                    print_andon_details(Event)
            end,
            halt(0)
    end.

resolve_andon(AndonId, Args) ->
    ensure_andon_running(),

    case {maps:get(root_cause, Args, undefined),
          maps:get(fix, Args, undefined),
          maps:get(prevention, Args, undefined)} of
        {undefined, _, _} ->
            tcps_cli_format:error("Missing required argument: --root-cause"),
            halt(1);
        {_, undefined, _} ->
            tcps_cli_format:error("Missing required argument: --fix"),
            halt(1);
        {_, _, undefined} ->
            tcps_cli_format:error("Missing required argument: --prevention"),
            halt(1);
        {RootCause, Fix, Prevention} ->
            Resolution = #{
                root_cause => list_to_binary(RootCause),
                fix_applied => list_to_binary(Fix),
                prevention_added => list_to_binary(Prevention),
                resolver => <<"cli">>,
                resolution_time_minutes => 0
            },

            case tcps_andon:resolve_andon(AndonId, Resolution) of
                ok ->
                    tcps_cli_format:success("Andon resolved: ~s", [AndonId]),
                    halt(0);
                {error, {andon_not_found, _}} ->
                    tcps_cli_format:error("Andon event not found: ~s", [AndonId]),
                    halt(1);
                {error, {already_resolved, _}} ->
                    tcps_cli_format:error("Andon already resolved: ~s", [AndonId]),
                    halt(1);
                {error, Reason} ->
                    tcps_cli_format:error("Failed to resolve Andon: ~p", [Reason]),
                    halt(1)
            end
    end.

check_status(SkuId) ->
    ensure_andon_running(),

    case tcps_andon:is_blocked(SkuId) of
        true ->
            case tcps_andon:can_proceed_to_stage(SkuId, execution) of
                {blocked, BlockingIds} ->
                    tcps_cli_format:error("SKU ~s is BLOCKED by Andon events:", [SkuId]),
                    lists:foreach(fun(Id) ->
                        io:format("  - ~s~n", [Id])
                    end, BlockingIds),
                    halt(1);
                {ok, proceed} ->
                    tcps_cli_format:success("SKU ~s is clear to proceed", [SkuId]),
                    halt(0)
            end;
        false ->
            tcps_cli_format:success("SKU ~s is clear to proceed", [SkuId]),
            halt(0)
    end.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

parse_args(Args) ->
    parse_args(Args, #{}).

parse_args([], Acc) ->
    Acc;
parse_args(["--type", Type | Rest], Acc) ->
    parse_args(Rest, Acc#{type => Type});
parse_args(["--sku", Sku | Rest], Acc) ->
    parse_args(Rest, Acc#{sku => Sku});
parse_args(["--stage", Stage | Rest], Acc) ->
    parse_args(Rest, Acc#{stage => Stage});
parse_args(["--message", Msg | Rest], Acc) ->
    parse_args(Rest, Acc#{message => list_to_binary(Msg)});
parse_args(["--root-cause", RC | Rest], Acc) ->
    parse_args(Rest, Acc#{root_cause => RC});
parse_args(["--fix", Fix | Rest], Acc) ->
    parse_args(Rest, Acc#{fix => Fix});
parse_args(["--prevention", Prev | Rest], Acc) ->
    parse_args(Rest, Acc#{prevention => Prev});
parse_args(["--open" | Rest], Acc) ->
    parse_args(Rest, Acc#{all => false});
parse_args(["--all" | Rest], Acc) ->
    parse_args(Rest, Acc#{all => true});
parse_args([Unknown | Rest], Acc) ->
    tcps_cli_format:warning("Unknown argument: ~s", [Unknown]),
    parse_args(Rest, Acc).

parse_failure_type("test_failure") -> {ok, test_failure};
parse_failure_type("shacl_violation") -> {ok, shacl_violation};
parse_failure_type("compilation_failure") -> {ok, compilation_failure};
parse_failure_type("non_determinism") -> {ok, non_determinism};
parse_failure_type("missing_receipt") -> {ok, missing_receipt};
parse_failure_type(Unknown) ->
    {error, io_lib:format("Invalid failure type: ~s. Use: test_failure, "
                          "shacl_violation, compilation_failure, "
                          "non_determinism, missing_receipt", [Unknown])}.

infer_stage(test_failure) -> testing;
infer_stage(shacl_violation) -> validation;
infer_stage(compilation_failure) -> compilation;
infer_stage(non_determinism) -> testing;
infer_stage(missing_receipt) -> validation.

ensure_andon_running() ->
    tcps_andon:start().

format_andon(Id, Event) ->
    #{
        event_id => Id,
        failure_type => maps:get(failure_type, Event),
        sku_id => maps:get(sku_id, Event),
        stage => maps:get(stage, Event),
        status => maps:get(status, Event),
        timestamp => tcps_cli_format:format_timestamp(maps:get(timestamp, Event))
    }.

print_andon_details(Event) ->
    io:format("~n"),
    io:format("Andon Event Details:~n"),
    io:format("====================~n"),
    io:format("Event ID:      ~s~n", [maps:get(event_id, Event)]),
    io:format("Failure Type:  ~s~n", [maps:get(failure_type, Event)]),
    io:format("SKU ID:        ~s~n", [maps:get(sku_id, Event)]),
    io:format("Stage:         ~s~n", [maps:get(stage, Event)]),
    io:format("Status:        ~s~n", [maps:get(status, Event)]),
    io:format("Timestamp:     ~s~n", [tcps_cli_format:format_timestamp(
                                        maps:get(timestamp, Event))]),

    case maps:get(details, Event, undefined) of
        undefined -> ok;
        Details ->
            io:format("~nDetails:~n"),
            maps:foreach(fun(K, V) ->
                io:format("  ~s: ~p~n", [K, V])
            end, Details)
    end,

    case maps:get(resolution, Event, undefined) of
        undefined -> ok;
        Resolution ->
            io:format("~nResolution:~n"),
            io:format("  Root Cause:  ~s~n", [maps:get(root_cause, Resolution)]),
            io:format("  Fix Applied: ~s~n", [maps:get(fix_applied, Resolution)]),
            io:format("  Prevention:  ~s~n", [maps:get(prevention_added, Resolution)])
    end,
    io:format("~n").
