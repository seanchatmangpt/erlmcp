%%%-----------------------------------------------------------------------------
%%% @doc TCPS CLI Work Order Management
%%%
%%% Handles all work-order related CLI commands.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_cli_work_order).

-export([run/1]).

-spec run([string()]) -> no_return().

%%%=============================================================================
%%% API
%%%=============================================================================

run(["create" | Args]) ->
    create_work_order(parse_args(Args));

run(["list" | Args]) ->
    list_work_orders(parse_args(Args));

run(["show", OrderId | _]) ->
    show_work_order(list_to_binary(OrderId));

run(["complete", OrderId | _]) ->
    complete_work_order(list_to_binary(OrderId));

run(["delete", OrderId | _]) ->
    delete_work_order(list_to_binary(OrderId));

run([]) ->
    tcps_cli_format:error("Missing subcommand. Use: create, list, show, complete, delete"),
    halt(1);

run([Unknown | _]) ->
    tcps_cli_format:error("Unknown subcommand: ~s", [Unknown]),
    halt(1).

%%%=============================================================================
%%% Command Implementations
%%%=============================================================================

create_work_order(Args) ->
    % Validate required arguments
    case {maps:get(bucket, Args, undefined), maps:get(title, Args, undefined)} of
        {undefined, _} ->
            tcps_cli_format:error("Missing required argument: --bucket"),
            halt(1);
        {_, undefined} ->
            tcps_cli_format:error("Missing required argument: --title"),
            halt(1);
        {Bucket, Title} ->
            % Start Kanban server if not running
            ensure_kanban_running(),

            % Validate bucket
            case validate_bucket(Bucket) of
                {ok, BucketAtom} ->
                    % Create pull signal
                    Priority = maps:get(priority, Args, 50),
                    Description = maps:get(description, Args, <<"">>),

                    Signal = #{
                        bucket => BucketAtom,
                        priority => Priority,
                        payload => #{
                            title => list_to_binary(Title),
                            description => Description,
                            created_by => <<"cli">>,
                            created_at => erlang:system_time(millisecond)
                        }
                    },

                    % Process pull signal
                    case tcps_kanban:process_pull_signal(Signal) of
                        {ok, WorkOrderId} ->
                            tcps_cli_format:success("Work order created: ~s",
                                                   [WorkOrderId]),
                            tcps_cli_format:info("Bucket: ~s", [Bucket]),
                            tcps_cli_format:info("Priority: ~p", [Priority]),
                            halt(0);
                        {error, limit_reached} ->
                            tcps_cli_format:error(
                                "WIP limit reached for bucket ~s. Complete existing work first.",
                                [Bucket]),
                            halt(1);
                        {error, Reason} ->
                            tcps_cli_format:error("Failed to create work order: ~p", [Reason]),
                            halt(1)
                    end;
                {error, Reason} ->
                    tcps_cli_format:error("~s", [Reason]),
                    halt(1)
            end
    end.

list_work_orders(Args) ->
    ensure_kanban_running(),

    % Get filters
    Status = maps:get(status, Args, undefined),
    Bucket = case maps:get(bucket, Args, undefined) of
        undefined -> undefined;
        BucketStr -> list_to_existing_atom(BucketStr)
    end,
    Limit = maps:get(limit, Args, 50),

    % Get work orders from all buckets
    Buckets = case Bucket of
        undefined -> [reliability, security, cost, compliance];
        BucketAtom -> [BucketAtom]
    end,

    % Collect work orders
    AllOrders = lists:flatten([
        get_bucket_orders(Bkt) || Bkt <- Buckets
    ]),

    % Apply filters
    Filtered = case Status of
        undefined -> AllOrders;
        S ->
            StatusAtom = list_to_existing_atom(S),
            [O || O <- AllOrders, maps:get(status, O) =:= StatusAtom]
    end,

    % Limit results
    Limited = lists:sublist(Filtered, Limit),

    % Format output
    case Limited of
        [] ->
            io:format("No work orders found.~n"),
            halt(0);
        _ ->
            Format = tcps_cli_config:get(output_format, table),
            Headers = [id, bucket, priority, status, created_at],
            FormattedOrders = [format_work_order(O) || O <- Limited],
            tcps_cli_format:output(FormattedOrders, Format, #{headers => Headers}),
            halt(0)
    end.

show_work_order(OrderId) ->
    tcps_cli_format:error("Work order details not yet implemented: ~s", [OrderId]),
    halt(1).

complete_work_order(OrderId) ->
    ensure_kanban_running(),

    % Need to find which bucket this order belongs to
    Buckets = [reliability, security, cost, compliance],
    case find_order_bucket(OrderId, Buckets) of
        {ok, Bucket} ->
            case tcps_kanban:complete_work_order(Bucket, OrderId) of
                ok ->
                    tcps_cli_format:success("Work order completed: ~s", [OrderId]),
                    halt(0);
                {error, not_found} ->
                    tcps_cli_format:error("Work order not found: ~s", [OrderId]),
                    halt(1);
                {error, Reason} ->
                    tcps_cli_format:error("Failed to complete work order: ~p", [Reason]),
                    halt(1)
            end;
        {error, not_found} ->
            tcps_cli_format:error("Work order not found: ~s", [OrderId]),
            halt(1)
    end.

delete_work_order(OrderId) ->
    tcps_cli_format:error("Work order deletion not yet implemented: ~s", [OrderId]),
    halt(1).

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
parse_args(["--title", Title | Rest], Acc) ->
    parse_args(Rest, Acc#{title => Title});
parse_args(["--description", Desc | Rest], Acc) ->
    parse_args(Rest, Acc#{description => list_to_binary(Desc)});
parse_args(["--status", Status | Rest], Acc) ->
    parse_args(Rest, Acc#{status => Status});
parse_args(["--limit", Limit | Rest], Acc) ->
    parse_args(Rest, Acc#{limit => list_to_integer(Limit)});
parse_args([Unknown | Rest], Acc) ->
    tcps_cli_format:warning("Unknown argument: ~s", [Unknown]),
    parse_args(Rest, Acc).

validate_bucket(Bucket) when is_list(Bucket) ->
    try list_to_existing_atom(Bucket) of
        BucketAtom ->
            case lists:member(BucketAtom, [reliability, security, cost, compliance]) of
                true -> {ok, BucketAtom};
                false -> {error, "Invalid bucket. Use: reliability, security, cost, compliance"}
            end
    catch
        _:_ -> {error, "Invalid bucket. Use: reliability, security, cost, compliance"}
    end.

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

get_bucket_orders(Bucket) ->
    % This is a simplified version - in production, would query persistent storage
    case tcps_kanban:get_wip_status(Bucket) of
        #{current := Current} when Current > 0 ->
            % For now, return placeholder orders
            [];
        _ ->
            []
    end.

find_order_bucket(_OrderId, []) ->
    {error, not_found};
find_order_bucket(OrderId, [Bucket | Rest]) ->
    % Simplified - would check actual order storage
    case get_bucket_orders(Bucket) of
        [] ->
            find_order_bucket(OrderId, Rest);
        Orders ->
            case lists:any(fun(O) -> maps:get(id, O) =:= OrderId end, Orders) of
                true -> {ok, Bucket};
                false -> find_order_bucket(OrderId, Rest)
            end
    end.

format_work_order(Order) ->
    #{
        id => maps:get(id, Order),
        bucket => maps:get(bucket, Order),
        priority => maps:get(priority, Order, 0),
        status => maps:get(status, Order, pending),
        created_at => tcps_cli_format:format_timestamp(
            maps:get(created_at, Order, erlang:system_time(millisecond)))
    }.
