%%%-----------------------------------------------------------------------------
%%% @doc TCPS CLI Kanban Management
%%%
%%% Handles all Kanban WIP and scheduling related CLI commands.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_cli_kanban).

-export([run/1]).

-spec run([string()]) -> no_return().

%%%=============================================================================
%%% API
%%%=============================================================================

run(["status" | Args]) ->
    show_status(parse_args(Args));

run(["schedule" | _]) ->
    show_schedule();

run(["set-limit", Bucket, Limit | _]) ->
    set_wip_limit(Bucket, list_to_integer(Limit));

run(["pull" | Args]) ->
    process_pull(parse_args(Args));

run([]) ->
    tcps_cli_format:error("Missing subcommand. Use: status, schedule, set-limit, pull"),
    halt(1);

run([Unknown | _]) ->
    tcps_cli_format:error("Unknown subcommand: ~s", [Unknown]),
    halt(1).

%%%=============================================================================
%%% Command Implementations
%%%=============================================================================

show_status(Args) ->
    ensure_kanban_running(),

    Buckets = case maps:get(bucket, Args, undefined) of
        undefined -> [reliability, security, cost, compliance];
        B -> [list_to_existing_atom(B)]
    end,

    StatusData = [begin
        Status = tcps_kanban:get_wip_status(Bucket),
        Status#{bucket => Bucket}
    end || Bucket <- Buckets],

    Format = tcps_cli_config:get(output_format, table),
    Headers = [bucket, current, limit, available, utilization],
    FormattedData = [format_wip_status(S) || S <- StatusData],

    tcps_cli_format:output(FormattedData, Format, #{headers => Headers}),
    halt(0).

show_schedule() ->
    tcps_cli_format:error("Schedule display not yet implemented"),
    halt(1).

set_wip_limit(BucketStr, Limit) ->
    ensure_kanban_running(),

    try list_to_existing_atom(BucketStr) of
        Bucket ->
            case lists:member(Bucket, [reliability, security, cost, compliance]) of
                true ->
                    ok = tcps_kanban:set_wip_limit(Bucket, Limit),
                    tcps_cli_format:success("WIP limit set: ~s = ~p", [Bucket, Limit]),
                    halt(0);
                false ->
                    tcps_cli_format:error("Invalid bucket: ~s", [BucketStr]),
                    halt(1)
            end
    catch
        _:_ ->
            tcps_cli_format:error("Invalid bucket: ~s", [BucketStr]),
            halt(1)
    end.

process_pull(Args) ->
    ensure_kanban_running(),

    case maps:get(bucket, Args, undefined) of
        undefined ->
            tcps_cli_format:error("Missing required argument: --bucket"),
            halt(1);
        BucketStr ->
            try list_to_existing_atom(BucketStr) of
                Bucket ->
                    Priority = maps:get(priority, Args, 50),
                    Signal = #{
                        bucket => Bucket,
                        priority => Priority,
                        payload => #{
                            triggered_by => <<"cli">>
                        }
                    },

                    case tcps_kanban:process_pull_signal(Signal) of
                        {ok, OrderId} ->
                            tcps_cli_format:success("Pull signal processed: ~s", [OrderId]),
                            halt(0);
                        {error, limit_reached} ->
                            tcps_cli_format:error("WIP limit reached for ~s", [Bucket]),
                            halt(1);
                        {error, Reason} ->
                            tcps_cli_format:error("Pull signal failed: ~p", [Reason]),
                            halt(1)
                    end
            catch
                _:_ ->
                    tcps_cli_format:error("Invalid bucket: ~s", [BucketStr]),
                    halt(1)
            end
    end.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

parse_args(Args) ->
    parse_args(Args, #{}).

parse_args([], Acc) ->
    Acc;
parse_args(["--bucket", Bucket | Rest], Acc) ->
    parse_args(Rest, Acc#{bucket => Bucket});
parse_args(["--priority", Priority | Rest], Acc) ->
    parse_args(Rest, Acc#{priority => list_to_integer(Priority)});
parse_args([Unknown | Rest], Acc) ->
    tcps_cli_format:warning("Unknown argument: ~s", [Unknown]),
    parse_args(Rest, Acc).

ensure_kanban_running() ->
    case whereis(tcps_kanban) of
        undefined ->
            case tcps_kanban:start_link() of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok;
                {error, Reason} ->
                    tcps_cli_format:error("Failed to start Kanban system: ~p", [Reason]),
                    halt(1)
            end;
        _Pid ->
            ok
    end.

format_wip_status(Status) ->
    Utilization = case maps:get(utilization, Status) of
        U when is_float(U) -> tcps_cli_format:format_percentage(U * 100);
        _ -> <<"N/A">>
    end,

    Available = case maps:get(available, Status) of
        infinity -> <<"∞">>;
        A -> integer_to_binary(A)
    end,

    Limit = case maps:get(limit, Status) of
        infinity -> <<"∞">>;
        L -> integer_to_binary(L)
    end,

    #{
        bucket => maps:get(bucket, Status),
        current => maps:get(current, Status),
        limit => Limit,
        available => Available,
        utilization => Utilization
    }.
